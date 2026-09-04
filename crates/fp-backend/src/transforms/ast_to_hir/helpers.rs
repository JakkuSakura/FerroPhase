use super::*;
use fp_core::hir::Res;

impl AstToHirLowerer {
    /// Resolve the expression node that owns a type reference before
    /// inspecting any expression wrapper around it.  Type syntax can be
    /// represented as `Ty::Expr(Value::Expr(..))`; the frontend's resolver
    /// records the namespace result on the owning expression node, and
    /// dropping that node in favour of the nested expression loses the
    /// result before HIR construction.  This is the AST-to-HIR equivalent of
    /// rustc carrying the resolved `Res` on the path node rather than
    /// reconstructing it from the spelling later.
    pub(super) fn resolved_type_path(&mut self, expr: &ast::Expr) -> Result<Option<hir::QPath>> {
        match expr.kind() {
            ast::ExprKind::Name(name) if name.qself.is_some() => Ok(Some(
                self.ast_expr_to_hir_path(expr, PathResolutionScope::Type, ParamMode::Explicit)?,
            )),
            ast::ExprKind::Name(name) => {
                let result = self.local_resolver.resolve_parsed_path(
                    &self.package_id,
                    &self.module_path,
                    &name.path,
                    fp_core::hir::resolve::Namespace::Type,
                );
                if !matches!(result, fp_core::hir::resolve::ResolutionResult::Found(_))
                    && !(name.path.prefix == fp_core::ast::path::PathPrefix::Plain
                        && name
                            .path
                            .segments
                            .first()
                            .is_some_and(|segment| is_primitive_type_name(segment.as_str())))
                {
                    return Ok(None);
                }
                Ok(Some(self.ast_expr_to_hir_path(
                    expr,
                    PathResolutionScope::Type,
                    ParamMode::Explicit,
                )?))
            }
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Expr(inner) => self.resolved_type_path(inner),
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn name_segment_args(&mut self, name: &Name) -> Result<Vec<Option<hir::GenericArgs>>> {
        match name {
            Name { path, .. } => path
                .segments
                .iter()
                .map(|segment| {
                    segment
                        .args
                        .as_deref()
                        .map(|arguments| self.convert_path_arguments(arguments))
                        .transpose()
                })
                .collect(),
        }
    }

    pub(super) fn convert_path_arguments(
        &mut self,
        arguments: &ast::GenericArgs,
    ) -> Result<hir::GenericArgs> {
        let mut span_ext = arguments.span();
        let mut parenthesized = hir::GenericArgsParentheses::No;
        let mut hir_args = Vec::new();
        let mut constraints = Vec::new();
        let args: &[ast::AngleBracketedArg] = match arguments {
            ast::GenericArgs::AngleBracketed(args) => &args.args,
            ast::GenericArgs::Parenthesized(ast::ParenthesizedArgs {
                span: _,
                inputs,
                inputs_span,
                output,
            }) => {
                let input_types = inputs
                    .iter()
                    .map(|input| self.transform_type_to_hir(input))
                    .collect::<Result<Vec<_>>>()?;
                let input_tuple = hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Tuple(input_types.into_iter().map(Box::new).collect()),
                    *inputs_span,
                );
                let output_ty = match output {
                    ast::FnRetTy::Ty(output) => self.transform_type_to_hir(output)?,
                    // Rustc gives the synthesized `()` output the span carried
                    // by `FnRetTy::Default` (normally the zero-width position
                    // immediately after `)`) rather than the full argument
                    // list. Keeping that distinction lets HIR->AST recover
                    // `Trait(T)` instead of inventing `Trait(T) -> ()`.
                    ast::FnRetTy::Default(default_span) => hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Tuple(Vec::new()),
                        *default_span,
                    ),
                };
                hir_args.push(hir::GenericArg::Type(Box::new(input_tuple)));
                constraints.push(hir::AssocItemConstraint {
                    hir_id: self.next_id(),
                    ident: hir::Symbol::new("Output"),
                    gen_args: hir::GenericArgs::default(),
                    kind: hir::AssocItemConstraintKind::Equality {
                        term: hir::Term::Ty(Box::new(output_ty.clone())),
                    },
                    span: output_ty.span(),
                });
                parenthesized = hir::GenericArgsParentheses::ParenSugar;
                // rustc uses the `(A, B)` span for HIR `GenericArgs`; the
                // enclosing `-> Output` belongs to the synthesized Output
                // constraint instead.
                span_ext = *inputs_span;
                &[]
            }
            ast::GenericArgs::ParenthesizedElided(_) => {
                parenthesized = hir::GenericArgsParentheses::ReturnTypeNotation;
                &[]
            }
        };
        for arg in args {
            let ast::AngleBracketedArg::Arg(arg) = arg else {
                let ast::AngleBracketedArg::Constraint(constraint) = arg else {
                    continue;
                };
                match constraint {
                    ast::AssocItemConstraint {
                        ident,
                        gen_args,
                        kind: ast::AssocItemConstraintKind::Equality { term },
                        span: constraint_span,
                    } => {
                        let term = match term {
                            ast::Term::Ty(ty) => {
                                hir::Term::Ty(Box::new(self.transform_type_to_hir(ty.as_ref())?))
                            }
                            ast::Term::Const(expr) => hir::Term::Const(Box::new(
                                self.transform_expr_to_hir(expr.as_ref())?,
                            )),
                        };
                        let gen_args = gen_args
                            .as_ref()
                            .map(|args| self.convert_path_arguments(args))
                            .transpose()?
                            .unwrap_or_default();
                        constraints.push(hir::AssocItemConstraint {
                            hir_id: self.next_id(),
                            ident: ident.clone().into(),
                            gen_args,
                            kind: hir::AssocItemConstraintKind::Equality { term },
                            span: *constraint_span,
                        });
                    }
                    ast::AssocItemConstraint {
                        ident,
                        gen_args,
                        kind: ast::AssocItemConstraintKind::Bound { bounds },
                        span: constraint_span,
                    } => {
                        let gen_args = gen_args
                            .as_ref()
                            .map(|args| self.convert_path_arguments(args))
                            .transpose()?
                            .unwrap_or_default();
                        constraints.push(hir::AssocItemConstraint {
                            hir_id: self.next_id(),
                            ident: ident.clone().into(),
                            gen_args,
                            kind: hir::AssocItemConstraintKind::Bound {
                                bounds: bounds
                                    .iter()
                                    .map(|bound| self.transform_type_to_hir(bound))
                                    .collect::<Result<Vec<_>>>()?,
                            },
                            span: *constraint_span,
                        });
                    }
                }
                continue;
            };
            match arg {
                ast::GenericArg::Lifetime(name) => {
                    hir_args.push(hir::GenericArg::Lifetime(hir::Lifetime::from_name(
                        name.as_str(),
                        self.next_id(),
                        name.span,
                    )))
                }
                ast::GenericArg::Type(ty) => {
                    if matches!(ty.as_ref(), ast::Ty::Wildcard(_)) {
                        hir_args.push(hir::GenericArg::Infer(hir::InferArg {
                            hir_id: self.next_id(),
                            span: ty.span(),
                            kind: hir::InferArgKind::TypeOrConst,
                        }));
                        continue;
                    }
                    // Rust keeps a path-shaped generic argument ambiguous in
                    // the AST and disambiguates it against the declaration's
                    // generic parameter.  Our HIR has separate type/const
                    // variants, so mirror rustc's disambiguation rule: use
                    // the value namespace only when the type namespace does
                    // not resolve the same single-segment path. Ordinary
                    // path resolution remains the resolver's responsibility.
                    if let ast::Ty::Expr(expr) = ty.as_ref()
                        && let ast::ExprKind::Name(name) = expr.kind()
                        && name.qself.is_none()
                        && name.path.prefix == fp_core::ast::path::PathPrefix::Plain
                        && name.path.segments.len() == 1
                        && !matches!(
                            self.local_resolver.resolve_parsed_path(
                                &self.package_id,
                                &self.module_path,
                                &name.path,
                                fp_core::hir::resolve::Namespace::Type,
                            ),
                            fp_core::hir::resolve::ResolutionResult::Found(_)
                        )
                        && matches!(
                            self.local_resolver.resolve_parsed_path(
                                &self.package_id,
                                &self.module_path,
                                &name.path,
                                fp_core::hir::resolve::Namespace::Value,
                            ),
                            fp_core::hir::resolve::ResolutionResult::Found(_)
                        )
                    {
                        hir_args.push(hir::GenericArg::Const(Box::new(
                            self.transform_expr_to_hir(expr)?,
                        )));
                    } else {
                        hir_args.push(hir::GenericArg::Type(Box::new(
                            self.transform_type_to_hir(ty.as_ref())?,
                        )));
                    }
                }
                ast::GenericArg::Const(expr) => {
                    let is_infer = match expr.kind() {
                        ast::ExprKind::Name(name) => {
                            name.as_ident().is_some_and(|ident| ident.as_str() == "_")
                        }
                        ast::ExprKind::Block(block) => block.last_expr().is_some_and(|inner| {
                            matches!(
                                inner.kind(),
                                ast::ExprKind::Name(name)
                                    if name
                                        .as_ident()
                                        .is_some_and(|ident| ident.as_str() == "_")
                            )
                        }),
                        _ => false,
                    };
                    if is_infer {
                        hir_args.push(hir::GenericArg::Infer(hir::InferArg {
                            hir_id: self.next_id(),
                            span: expr.span(),
                            kind: hir::InferArgKind::Const,
                        }));
                    } else {
                        hir_args.push(hir::GenericArg::Const(Box::new(
                            self.transform_expr_to_hir(expr.as_ref())?,
                        )));
                    }
                }
            }
        }
        Ok(hir::GenericArgs {
            args: hir_args,
            constraints,
            parenthesized,
            span_ext,
        })
    }

    pub(super) fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            if matches!(arg, ast::Ty::Wildcard(_)) {
                hir_args.push(hir::GenericArg::Infer(hir::InferArg {
                    hir_id: self.next_id(),
                    span: arg.span(),
                    kind: hir::InferArgKind::TypeOrConst,
                }));
                continue;
            }
            // An explicit associated-type binding (`Iterator<Item = U>` —
            // fp-lang's path-argument parser turns `Item = U` into a
            // `Ty::Expr(Assign { target: Item, value: U })` entry among a
            // `Path` segment's own `args`, per this same crate's
            // `items.rs`' `explicit_bindings` extraction, which already
            // handles this shape on its own dedicated path) is not an
            // ordinary positional type argument — passing it through to
            // `transform_type_to_hir`/`ast_expr_to_hir_path` here (which
            // has no notion of a binding, only plain type references)
            // always fails as "not path-like" and produces a synthetic
            // `__fp_error` placeholder. Every real trait-bound-with-
            // binding reaches here as one of `args`, so skip it — the
            // binding itself is recovered separately by whichever caller
            // already extracts `explicit_bindings`.
            if let ast::Ty::Expr(expr) = arg {
                if matches!(expr.kind(), ast::ExprKind::Assign(_)) {
                    continue;
                }
                // Lifetimes are not type expressions, but HIR keeps them as
                // first-class generic arguments just like rustc. Preserve a
                // lifetime supplied to a method turbofish instead of sending
                // it through ordinary type-path resolution as the name `'a`.
                if let ast::ExprKind::Name(name) = expr.kind()
                    && name.path.segments.len() == 1
                    && name.path.segments[0].as_str().starts_with('\'')
                {
                    hir_args.push(hir::GenericArg::Lifetime(hir::Lifetime::from_name(
                        name.path.segments[0].as_str(),
                        self.next_id(),
                        name.path.span(),
                    )));
                    continue;
                }
                // A const generic argument (`Simd<f32, 4>`, `[T; N]`'s own
                // `N` reused as a generic arg elsewhere, ...) parses as a
                // plain integer-literal `Ty::Expr`, not a type at all —
                // passing it to `transform_type_to_hir`/`ast_expr_to_hir_path`
                // (which only knows how to build a *type* path) always
                // fails as "not path-like", producing a `__fp_error`
                // placeholder that then cascades into unrelated
                // "unresolved type path" noise downstream. `hir::
                // GenericArg` already has a dedicated `Const` variant for
                // exactly this shape (see `fp-typing`'s `check_type_expr`,
                // which already reports a clean, accurate "const generic
                // arguments are not supported" for it) — route it there
                // instead of forcing it through the type-path builder.
                if matches!(
                    expr.kind(),
                    ast::ExprKind::Value(value)
                        if matches!(value.as_ref(), ast::Value::Int(_) | ast::Value::UInt(_))
                ) {
                    let hir_expr = self.transform_expr_to_hir(expr)?;
                    hir_args.push(hir::GenericArg::Const(Box::new(hir_expr)));
                    continue;
                }
                if let ast::ExprKind::Name(name) = expr.kind()
                    && name.qself.is_none()
                    && name.path.prefix == fp_core::ast::path::PathPrefix::Plain
                    && name.path.segments.len() == 1
                    && !matches!(
                        self.local_resolver.resolve_parsed_path(
                            &self.package_id,
                            &self.module_path,
                            &name.path,
                            fp_core::hir::resolve::Namespace::Type,
                        ),
                        fp_core::hir::resolve::ResolutionResult::Found(_)
                    )
                    && matches!(
                        self.local_resolver.resolve_parsed_path(
                            &self.package_id,
                            &self.module_path,
                            &name.path,
                            fp_core::hir::resolve::Namespace::Value,
                        ),
                        fp_core::hir::resolve::ResolutionResult::Found(_)
                    )
                {
                    let hir_expr = self.transform_expr_to_hir(expr)?;
                    hir_args.push(hir::GenericArg::Const(Box::new(hir_expr)));
                    continue;
                }
            }
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs {
            args: hir_args,
            constraints: Vec::new(),
            parenthesized: hir::GenericArgsParentheses::No,
            span_ext: Span::null(),
        })
    }

    pub(super) fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
        param_mode: ParamMode,
    ) -> Result<hir::QPath> {
        match expr.kind() {
            ast::ExprKind::Name(name) => {
                if let Some(qself) = &name.qself {
                    let base_ty = self.transform_type_to_hir(&qself.ty)?;
                    let args = self.name_segment_args(name)?;
                    let trait_count = qself.position;
                    if trait_count > 0 {
                        let trait_path = ast::Path::new(
                            name.path.prefix,
                            name.path
                                .segments
                                .iter()
                                .take(trait_count)
                                .cloned()
                                .collect(),
                        );
                        let trait_expr = ast::Expr::new(ast::ExprKind::Name(ast::Name {
                            qself: None,
                            path: trait_path,
                        }));
                        let trait_qpath = self.ast_expr_to_hir_path(
                            &trait_expr,
                            PathResolutionScope::Type,
                            ParamMode::Explicit,
                        )?;
                        let hir::QPath::Resolved(_, trait_path) = trait_qpath else {
                            return Err(
                                "trait qualification did not resolve to an ordinary path".into()
                            );
                        };
                        let mut associated = name
                            .path
                            .segments
                            .iter()
                            .skip(trait_count)
                            .zip(args.into_iter().skip(trait_count));
                        let Some((first_segment, first_args)) = associated.next() else {
                            return Err("qualified path has no associated segment".into());
                        };
                        let first_segment =
                            self.make_path_segment(first_segment.as_str(), first_args, param_mode);
                        let trait_res = trait_path.res;
                        let mut resolved_segments = trait_path.segments;
                        resolved_segments.push(first_segment.clone());
                        // Rustc keeps an explicitly qualified trait path in
                        // `QPath::Resolved(Some(Self), Path)`.  Only a later
                        // associated suffix is represented as a nested
                        // `QPath::TypeRelative` node.
                        let mut qpath = hir::QPath::Resolved(
                            Some(Box::new(base_ty)),
                            hir::Path {
                                span: name.path.span(),
                                res: trait_res,
                                segments: resolved_segments,
                            },
                        );
                        for (segment, args) in associated {
                            let receiver = hir::TypeExpr::new(
                                self.next_id(),
                                hir::TypeExprKind::Path(qpath),
                                expr.span(),
                            );
                            qpath = hir::QPath::type_relative(
                                receiver,
                                self.make_path_segment(segment.as_str(), args, param_mode),
                            );
                        }
                        return Ok(qpath);
                    }

                    // `<T>::Assoc` has no trait path to resolve.  Build the
                    // same nested type-relative chain rustc uses for
                    // `T::Assoc::Nested`.
                    let mut receiver = base_ty;
                    let mut qpath = None;
                    for (segment, args) in name.path.segments.iter().zip(args.into_iter()) {
                        let path = hir::QPath::type_relative(
                            receiver,
                            self.make_path_segment(segment.as_str(), args, param_mode),
                        );
                        qpath = Some(path.clone());
                        receiver = hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Path(path),
                            expr.span(),
                        );
                    }
                    return qpath.ok_or_else(|| "qualified path has no associated segment".into());
                }
                let namespace = match scope {
                    PathResolutionScope::Type => fp_core::hir::resolve::Namespace::Type,
                    PathResolutionScope::Value => fp_core::hir::resolve::Namespace::Value,
                    PathResolutionScope::Trait => fp_core::hir::resolve::Namespace::Type,
                };
                let parsed = name.path.clone();
                let resolution_namespace = namespace;
                let resolved = match self.local_resolver.resolve_parsed_path(
                    &self.package_id,
                    &self.module_path,
                    &parsed,
                    resolution_namespace,
                ) {
                    fp_core::hir::resolve::ResolutionResult::Found(path) => {
                        let mut path = path;
                        let args = self.name_segment_args(name)?;
                        path.span = expr.span();
                        for (segment, args) in path.segments.iter_mut().zip(args.iter()) {
                            segment.hir_id = self.next_id();
                            segment.infer_args = Self::infer_path_segment_args(args, param_mode);
                            segment.args = args.clone();
                        }
                        // The resolver returns only the resolved base path.
                        // The remaining source segments are associated
                        // extensions and must become nested TypeRelative
                        // nodes rather than Error-bearing ordinary segments.
                        let consumed = path.segments.len();
                        if consumed < parsed.segments.len() {
                            let mut qpath = hir::QPath::resolved(path);
                            for (segment, args) in parsed
                                .segments
                                .iter()
                                .skip(consumed)
                                .zip(args.into_iter().skip(consumed))
                            {
                                let receiver = hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(qpath),
                                    expr.span(),
                                );
                                qpath = hir::QPath::type_relative(
                                    receiver,
                                    self.make_path_segment(segment.as_str(), args, param_mode),
                                );
                            }
                            return Ok(qpath);
                        }
                        return Ok(hir::QPath::resolved(path));
                    }
                    fp_core::hir::resolve::ResolutionResult::NotFound(reason) => {
                        // Primitive names are language items rather than
                        // module-data declarations. Resolve them only after
                        // ordinary lookup so a user declaration named `u8`
                        // still shadows the builtin, matching Rust's name
                        // resolution behavior.
                        if parsed.prefix == fp_core::ast::path::PathPrefix::Plain
                            && parsed
                                .segments
                                .first()
                                .is_some_and(|segment| is_primitive_type_name(segment.as_str()))
                        {
                            // The primitive is the resolved base. Any
                            // trailing segments (for example `f128::MAX`)
                            // remain for type checking as associated items.
                            let primitive = parsed.segments[0].as_str().to_owned();
                            let args = self.name_segment_args(name)?;
                            let mut segments: Vec<_> = parsed
                                .segments
                                .iter()
                                .zip(args.into_iter())
                                .map(|(segment, args)| {
                                    self.make_path_segment(segment.as_str(), args, param_mode)
                                })
                                .collect();
                            if segments.len() > 1 {
                                let base = segments.remove(0);
                                let mut qpath = hir::QPath::resolved(hir::Path {
                                    span: expr.span(),
                                    res: Res::Builtin(hir::BuiltinSelfType::Primitive(
                                        primitive.clone(),
                                    )),
                                    segments: vec![base],
                                });
                                for segment in segments {
                                    let receiver = hir::TypeExpr::new(
                                        self.next_id(),
                                        hir::TypeExprKind::Path(qpath),
                                        expr.span(),
                                    );
                                    qpath = hir::QPath::type_relative(receiver, segment);
                                }
                                return Ok(qpath);
                            }
                            return Ok(hir::QPath::resolved(hir::Path {
                                span: expr.span(),
                                res: Res::Builtin(hir::BuiltinSelfType::Primitive(primitive)),
                                segments,
                            }));
                        }
                        if std::env::var_os("FP_TRACE_PATHS").is_some() {
                            eprintln!(
                                "ast_to_hir path miss: package={} module={:?} owner={:?} scope={scope:?} path={parsed} reason={reason:?}",
                                self.package_id, self.module_path, self.current_owner,
                            );
                        }
                        self.add_error(
                            fp_core::diagnostics::Diagnostic::error(format!(
                                "unresolved {scope:?} path `{parsed}`: {reason:?}"
                            ))
                            .with_span(expr.span()),
                        );
                        hir::Res::Error
                    }
                    fp_core::hir::resolve::ResolutionResult::Ambiguous => {
                        if std::env::var_os("FP_TRACE_PATHS").is_some() {
                            eprintln!(
                                "ast_to_hir path ambiguous: package={} module={:?} owner={:?} scope={scope:?} path={parsed}",
                                self.package_id, self.module_path, self.current_owner,
                            );
                        }
                        self.add_error(
                            fp_core::diagnostics::Diagnostic::error(format!(
                                "ambiguous {scope:?} path `{parsed}`"
                            ))
                            .with_span(expr.span()),
                        );
                        hir::Res::Error
                    }
                };
                let segment_args = self.name_segment_args(name)?;
                let names: Vec<&str> = name
                    .path
                    .segments
                    .iter()
                    .map(|segment| segment.as_str())
                    .collect();
                Ok(hir::QPath::resolved(hir::Path {
                    span: expr.span(),
                    res: resolved,
                    segments: names
                        .into_iter()
                        .zip(segment_args)
                        .map(|(name, args)| self.make_path_segment(name, args, param_mode))
                        .collect(),
                }))
            }
            ast::ExprKind::FieldAccess(select) => {
                // `T::ASSOC` is a type-relative path. Resolve its base in
                // the type namespace, as rustc does for a qualified path,
                // even when the surrounding expression is in value scope.
                // This applies to associated functions as well as constants:
                // `Vec::from` must resolve `Vec` as a type, never as a value
                // constructor or a same-named lexical binding. Keep a value
                // lookup only as the module-qualified constant fallback below.
                let base_scope = match select.obj.kind() {
                    ast::ExprKind::Name(name)
                        if matches!(name.path.prefix, fp_core::ast::path::PathPrefix::Plain) =>
                    {
                        let symbol = name
                            .path
                            .segments
                            .first()
                            .map(|segment| segment.as_str())
                            .unwrap_or_default();
                        match self
                            .local_resolver
                            .resolve_local(symbol, fp_core::hir::resolve::Namespace::Type)
                        {
                            fp_core::hir::resolve::ResolutionResult::Found(path)
                                if matches!(
                                    path.res,
                                    hir::Res::Def(_)
                                        | hir::Res::Generic(_)
                                        | hir::Res::SelfTy
                                        | hir::Res::Builtin(_)
                                ) =>
                            {
                                PathResolutionScope::Type
                            }
                            _ => match self
                                .local_resolver
                                .resolve_local(symbol, fp_core::hir::resolve::Namespace::Value)
                            {
                                fp_core::hir::resolve::ResolutionResult::Found(_) => {
                                    PathResolutionScope::Value
                                }
                                _ => PathResolutionScope::Type,
                            },
                        }
                    }
                    _ => PathResolutionScope::Type,
                };
                let type_base = self.ast_expr_to_hir_path(&select.obj, base_scope, param_mode)?;
                let member_args = select
                    .generic_args
                    .as_ref()
                    .map(|args| self.convert_path_arguments(args))
                    .transpose()?;
                let seg = self.make_path_segment(&select.field.name, member_args, param_mode);
                if matches!(base_scope, PathResolutionScope::Type) {
                    let receiver =
                        self.transform_type_to_hir(&ast::Ty::expr((*select.obj).clone()))?;
                    return Ok(hir::QPath::type_relative(receiver, seg));
                }
                match type_base {
                    hir::QPath::Resolved(qself, mut path) => {
                        path.segments.push(seg);
                        Ok(hir::QPath::Resolved(qself, path))
                    }
                    hir::QPath::TypeRelative(receiver, previous) => {
                        let base = hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Path(hir::QPath::type_relative(*receiver, previous)),
                            select.obj.span(),
                        );
                        Ok(hir::QPath::type_relative(base, seg))
                    }
                }
            }
            ast::ExprKind::Invoke(invoke) => {
                let mut base = match &invoke.target {
                    ast::ExprInvokeTarget::Function(name) => {
                        let expr = ast::Expr::new(ast::ExprKind::Name(name.clone()));
                        self.ast_expr_to_hir_path(&expr, scope, param_mode)?
                    }
                    ast::ExprInvokeTarget::Expr(expr) => {
                        self.ast_expr_to_hir_path(expr.as_ref(), scope, param_mode)?
                    }
                    // A generic-argumented reference to a *type* target
                    // (e.g. a qualified path's base type, or a bare type
                    // reused as a callable-position expression) parses its
                    // base as `ExprInvokeTarget::Type(ty)` rather than
                    // `Function(name)` — previously fell straight through
                    // to the generic "not path-like" error below and got
                    // replaced with a `__fp_error` placeholder path, even
                    // when the type itself resolves to a perfectly real
                    // path (the overwhelmingly common real case). Lower it
                    // the same way any other type reference is lowered,
                    // reusing its own already-resolved path when it has
                    // one; only genuinely non-path-shaped types (a tuple,
                    // a slice, `dyn Trait`, ...) still fall through.
                    ast::ExprInvokeTarget::Type(ty) => match ty {
                        // A type target is already the path head of this
                        // invoke. Resolve that head directly so lowering its
                        // generic arguments cannot re-enter this same
                        // `ExprInvokeTarget::Type` through `transform_type_to_hir`.
                        ast::Ty::Struct(struct_ty) => {
                            let expr = ast::Expr::new(ast::ExprKind::Name(ast::Name::ident(
                                struct_ty.name.clone(),
                            )));
                            self.ast_expr_to_hir_path(&expr, PathResolutionScope::Type, param_mode)?
                        }
                        ast::Ty::Expr(type_expr) => match type_expr.kind() {
                            ast::ExprKind::Name(name) => self.ast_expr_to_hir_path(
                                type_expr,
                                PathResolutionScope::Type,
                                param_mode,
                            )?,
                            ast::ExprKind::Value(value) => match value.as_ref() {
                                ast::Value::Type(inner) => match inner {
                                    ast::Ty::Struct(struct_ty) => {
                                        let expr = ast::Expr::new(ast::ExprKind::Name(
                                            ast::Name::ident(struct_ty.name.clone()),
                                        ));
                                        self.ast_expr_to_hir_path(
                                            &expr,
                                            PathResolutionScope::Type,
                                            param_mode,
                                        )?
                                    }
                                    ast::Ty::Expr(inner_expr) => match inner_expr.kind() {
                                        ast::ExprKind::Name(name) => self.ast_expr_to_hir_path(
                                            inner_expr,
                                            PathResolutionScope::Type,
                                            param_mode,
                                        )?,
                                        _ => {
                                            self.add_error(
                                                Diagnostic::error(
                                                    "expected a path-like type target".to_string(),
                                                )
                                                .with_source_context(DIAGNOSTIC_CONTEXT)
                                                .with_span(expr.span()),
                                            );
                                            hir::QPath::resolved(hir::Path {
                                                span: Default::default(),
                                                segments: vec![self.make_path_segment(
                                                    "__fp_error",
                                                    None,
                                                    param_mode,
                                                )],
                                                res: Res::Error,
                                            })
                                        }
                                    },
                                    _ => {
                                        self.add_error(
                                            Diagnostic::error(
                                                "expected a path-like type target".to_string(),
                                            )
                                            .with_source_context(DIAGNOSTIC_CONTEXT)
                                            .with_span(expr.span()),
                                        );
                                        hir::QPath::resolved(hir::Path {
                                            span: Default::default(),
                                            segments: vec![self.make_path_segment(
                                                "__fp_error",
                                                None,
                                                param_mode,
                                            )],
                                            res: Res::Error,
                                        })
                                    }
                                },
                                _ => {
                                    self.add_error(
                                        Diagnostic::error(
                                            "expected a path-like type target".to_string(),
                                        )
                                        .with_source_context(DIAGNOSTIC_CONTEXT)
                                        .with_span(expr.span()),
                                    );
                                    hir::QPath::resolved(hir::Path {
                                        span: Default::default(),
                                        segments: vec![self.make_path_segment(
                                            "__fp_error",
                                            None,
                                            param_mode,
                                        )],
                                        res: Res::Error,
                                    })
                                }
                            },
                            _ => {
                                self.add_error(
                                    Diagnostic::error(
                                        "expected a path-like type target".to_string(),
                                    )
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                                );
                                hir::QPath::resolved(hir::Path {
                                    span: Default::default(),
                                    segments: vec![self.make_path_segment(
                                        "__fp_error",
                                        None,
                                        param_mode,
                                    )],
                                    res: Res::Error,
                                })
                            }
                        },
                        _ => {
                            self.add_error(
                                Diagnostic::error("expected a path-like type target".to_string())
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                            );
                            hir::QPath::resolved(hir::Path {
                                span: Default::default(),
                                segments: vec![self.make_path_segment(
                                    "__fp_error",
                                    None,
                                    param_mode,
                                )],
                                res: Res::Error,
                            })
                        }
                    },
                    ast::ExprInvokeTarget::Method(select) => {
                        let base = self.ast_expr_to_hir_path(&select.obj, scope, param_mode)?;
                        let member_args = select
                            .generic_args
                            .as_ref()
                            .map(|args| self.convert_path_arguments(args))
                            .transpose()?;
                        let seg =
                            self.make_path_segment(&select.field.name, member_args, param_mode);
                        match base {
                            hir::QPath::Resolved(qself, mut path) => {
                                path.segments.push(seg);
                                hir::QPath::Resolved(qself, path)
                            }
                            hir::QPath::TypeRelative(receiver, previous) => {
                                let base = hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(hir::QPath::type_relative(
                                        *receiver, previous,
                                    )),
                                    select.obj.span(),
                                );
                                hir::QPath::type_relative(base, seg)
                            }
                        }
                    }
                    other => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "expected path-like expression for type path, found {:?}",
                                other
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(expr.span()),
                        );
                        hir::QPath::resolved(hir::Path {
                            span: Default::default(),
                            segments: vec![self.make_path_segment("__fp_error", None, param_mode)],
                            res: Res::Error,
                        })
                    }
                };

                if !invoke.args.is_empty() {
                    let args: Vec<ast::Ty> = invoke
                        .args
                        .iter()
                        .map(|arg| match arg.kind() {
                            ast::ExprKind::Value(value) => match value.as_ref() {
                                ast::Value::Type(ty) => ty.clone(),
                                _ => ast::Ty::expr(arg.clone()),
                            },
                            _ => ast::Ty::expr(arg.clone()),
                        })
                        .collect();
                    let hir_args = self.convert_generic_args(&args)?;
                    if let Some(last) = base.segments_mut().last_mut() {
                        if last.args.is_none() {
                            last.infer_args = false;
                            last.args = Some(hir_args);
                        }
                    } else if let Some(first) = base.segments_mut().first_mut() {
                        first.infer_args = false;
                        first.args = Some(hir_args);
                    }
                }

                Ok(base)
            }
            // A self-type like `&'a str`/`[T]`/`[T; N]` parses as a plain
            // `Ty` (not path-like at all — no `Name`/`Select`/`Invoke`
            // shape exists for it), wrapped as `Value::Type` by
            // `fp_lang::ast::type_to_expr`. These aren't nameable the way
            // typed impl identity expects — real rustc doesn't register
            // their impls under a module path either, it keys them by a
            // structural `SimplifiedType` bucket. Mirror that: tag the
            // path with `Res::Builtin(BuiltinSelfType)` (a typed shape
            // tag) instead of relying on the segment name; see
            // matching `Res::Builtin` check.
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Type(ast::Ty::Reference(reference)) => {
                    let kind = hir::BuiltinSelfType::Reference {
                        mutable: reference.mutability.unwrap_or(false),
                    };
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Slice(_)) => {
                    let kind = hir::BuiltinSelfType::Slice;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Array(_)) => {
                    let kind = hir::BuiltinSelfType::Array;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::RawPtr(ptr)) => {
                    let kind = hir::BuiltinSelfType::RawPtr {
                        mutable: ptr.mutability.unwrap_or(false),
                    };
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Nothing(_)) => {
                    let kind = hir::BuiltinSelfType::Never;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Unit(_)) => {
                    let kind = hir::BuiltinSelfType::Unit;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Tuple(_)) => {
                    let kind = hir::BuiltinSelfType::Tuple;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                ast::Value::Type(ast::Ty::Function(_)) => {
                    let kind = hir::BuiltinSelfType::Function;
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment(kind.bucket_key(), None, param_mode)],
                        res: hir::Res::Builtin(kind),
                    }))
                }
                // A multi-bound trait-object/`impl` type used in
                // expression position (`Box<dyn Fn(..) -> X + Send>`,
                // a closure cast, ...) — `+` (`TypeBinaryOpKind::Add`) is
                // the same token this compiler's struct-composition `+`
                // uses, just with no structural fields to merge here
                // either (see `fp-typing`'s own identical `TypeBinaryOp`
                // handling in `check_type_expr` for the type-position
                // counterpart of this exact shape/rationale). No
                // multi-trait `dyn`/`impl` representation exists to
                // build a real path for regardless, so approximate it as
                // its first bound rather than falling through to the
                // generic "not path-like" `__fp_error` placeholder below.
                ast::Value::Type(ast::Ty::TypeBinaryOp(op))
                    if op.kind == fp_core::ast::TypeBinaryOpKind::Add =>
                {
                    let lhs = ast::Expr::value(ast::Value::Type((*op.lhs).clone()));
                    self.ast_expr_to_hir_path(&lhs, scope, param_mode)
                }
                _ => {
                    self.add_error(
                        Diagnostic::error(format!(
                            "expected path-like expression for type path, found {:?}",
                            value
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(expr.span()),
                    );
                    Ok(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![self.make_path_segment("__fp_error", None, param_mode)],
                        res: Res::Error,
                    }))
                }
            },
            other => {
                self.add_error(
                    Diagnostic::error(format!(
                        "expected path-like expression for type path, found {:?}",
                        other
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(expr.span()),
                );
                Ok(hir::QPath::resolved(hir::Path {
                    span: Default::default(),
                    segments: vec![self.make_path_segment("__fp_error", None, param_mode)],
                    res: Res::Error,
                }))
            }
        }
    }

    pub(super) fn make_path_segment(
        &mut self,
        name: &str,
        args: Option<hir::GenericArgs>,
        param_mode: ParamMode,
    ) -> hir::PathSegment {
        let infer_args = Self::infer_path_segment_args(&args, param_mode);
        hir::PathSegment {
            ident: hir::Symbol::new(name),
            hir_id: self.next_id(),
            args,
            infer_args,
            res: hir::Res::Error,
        }
    }

    fn infer_path_segment_args(args: &Option<hir::GenericArgs>, param_mode: ParamMode) -> bool {
        if param_mode != ParamMode::Optional {
            return false;
        }
        match args {
            None => true,
            // Rustc's `has_non_lt_args` ignores associated-item constraints
            // and is false for an empty argument list, so both `Trait<>`
            // and `Trait<Item = T>` keep omitted type arguments inferable in
            // optional-parameter mode.
            Some(args) => matches!(args.parenthesized, hir::GenericArgsParentheses::No)
                && args
                    .args
                    .iter()
                    .all(|arg| matches!(arg, hir::GenericArg::Lifetime(_))),
        }
    }
}

/// Every primitive scalar name real Rust reserves — mirrors `fp-typing`'s
/// own `primitive_path_ty` name list (kept in sync deliberately; that one
/// maps the name to a `Ty`, this one only needs to recognize the name at
/// HIR-lowering time, before any `Ty` exists).
fn is_primitive_type_name(name: &str) -> bool {
    matches!(
        name,
        "bool"
            | "char"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f16"
            | "f32"
            | "f64"
            | "f128"
            | "str"
    )
}
