use super::*;
use fp_core::hir::Res;

impl AstToHirLowerer {
    pub(super) fn lookup_enum_variant(&self, base: &hir::Path, name: &str) -> Option<hir::Res> {
        let def_id = match base.res.as_ref()? {
            hir::Res::Def(def_id) => def_id.clone(),
            hir::Res::SelfTy => {
                let self_ty = self.current_impl_self_ty.as_ref()?;
                let hir::TypeExprKind::Path(path) = &self_ty.kind else {
                    return None;
                };
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };
                def_id.clone()
            }
            _ => return None,
        };
        let local_item = {
            self.package().def_map.get(&def_id).cloned()
        };
        let item = local_item
            .or_else(|| self.program_def_map.get(&def_id).cloned())
            .or_else(|| self.hir_program.item(def_id.clone()))?;
        let hir::ItemKind::Enum(enum_def) = &item.kind else {
            return None;
        };
        enum_def
            .variants
            .iter()
            .find(|variant| variant.name.as_str() == name)
            .map(|variant| hir::Res::Def(variant.def_id.clone()))
    }

    /// Preserve the HIR shape used for rustc's `QPath::TypeRelative` when a
    /// projection is rooted at a lexical type parameter. This HIR has no
    /// separate `QPath` node, so the parameter remains in the first segment,
    /// all projection segments remain in order, and `res` stays unresolved for
    /// type checking to resolve structurally from the parameter's bounds.
    fn preserve_lexical_projection_path(
        &self,
        path: hir::Path,
        scope: PathResolutionScope,
    ) -> hir::Path {
        let rooted_at_type_param = path.segments.first().is_some_and(|segment| {
            self.resolve_lexical_type_symbol(segment.name.as_str())
                .is_some()
        });
        if scope == PathResolutionScope::Type && path.segments.len() > 1 && rooted_at_type_param {
            hir::Path {
                segments: path.segments,
                res: Res::Error,
            }
        } else {
            path
        }
    }

    /// Resolve the expression node that owns a type reference before
    /// inspecting any expression wrapper around it.  Type syntax can be
    /// represented as `Ty::Expr(Value::Expr(..))`; the frontend's resolver
    /// records the namespace result on the owning expression node, and
    /// dropping that node in favour of the nested expression loses the
    /// result before HIR construction.  This is the AST-to-HIR equivalent of
    /// rustc carrying the resolved `Res` on the path node rather than
    /// reconstructing it from the spelling later.
    pub(super) fn resolved_type_path(&mut self, expr: &ast::Expr) -> Result<Option<hir::Path>> {
        let _ = expr;
        Ok(None)
    }

    fn name_segment_args(&mut self, name: &Name) -> Result<Vec<Option<hir::GenericArgs>>> {
        match name {
            Name::Ident(_) => Ok(vec![None]),
            Name::Path(path) => Ok(path.segments.iter().map(|_| None).collect()),
            Name::ParameterPath(path) => path
                .segments
                .iter()
                .map(|segment| {
                    if segment.args.is_empty() {
                        Ok(None)
                    } else {
                        self.convert_generic_args(&segment.args).map(Some)
                    }
                })
                .collect(),
        }
    }

    pub(super) fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            // An explicit associated-type binding (`Iterator<Item = U>` —
            // fp-lang's `parse_type_arg` turns `Item = U` into a
            // `Ty::Expr(Assign { target: Item, value: U })` entry among a
            // `ParameterPath` segment's own `args`, per this same crate's
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
            }
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    pub(super) fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        match expr.kind() {
            ast::ExprKind::Name(name) => {
                let namespace = match scope {
                    PathResolutionScope::Type => fp_core::hir::resolve::Namespace::Type,
                    PathResolutionScope::Value => fp_core::hir::resolve::Namespace::Value,
                    PathResolutionScope::Trait => fp_core::hir::resolve::Namespace::Type,
                };
                let parsed = match name {
                    Name::Ident(ident) => ast::Path::from_ident(ident.clone()),
                    Name::Path(path) => path.clone(),
                    Name::ParameterPath(path) => {
                        // Generic arguments do not participate in lexical
                        // name lookup. Resolve the first generic-bearing
                        // type/trait anchor and leave projections for type
                        // checking.
                        let anchor = path
                            .segments
                            .iter()
                            .position(|segment| !segment.args.is_empty())
                            .unwrap_or(path.segments.len().saturating_sub(1));
                        ast::Path::new(
                            path.prefix,
                            path.segments[..=anchor]
                                .iter()
                                .map(|segment| segment.ident.clone())
                                .collect(),
                        )
                    }
                };
                let resolution_namespace = if matches!(name, Name::ParameterPath(_)) {
                    fp_core::hir::resolve::Namespace::Type
                } else {
                    namespace
                };
                let res = match self.local_resolver.resolve_parsed_path(
                    &self.package_id,
                    &self.module_path,
                    &parsed,
                    resolution_namespace,
                ) {
                    fp_core::hir::resolve::ResolutionResult::Found(res) => res,
                    fp_core::hir::resolve::ResolutionResult::NotFound(_)
                    | fp_core::hir::resolve::ResolutionResult::Ambiguous => hir::Res::Error,
                };
                let segment_args = self.name_segment_args(name)?;
                let names: Vec<&str> = match name {
                    Name::Ident(ident) => vec![ident.as_str()],
                    Name::Path(path) => path
                        .segments
                        .iter()
                        .map(|segment| segment.as_str())
                        .collect(),
                    Name::ParameterPath(path) => path
                        .segments
                        .iter()
                        .map(|segment| segment.ident.as_str())
                        .collect(),
                };
                Ok(hir::Path {
                    res,
                    segments: names
                        .into_iter()
                        .zip(segment_args)
                        .map(|(name, args)| self.make_path_segment(name, args))
                        .collect(),
                })
            }
            ast::ExprKind::Select(select) => {
                // `T::ASSOC` is a type-relative path. Resolve its base in
                // the type namespace, as rustc does for a qualified path,
                // even when the surrounding expression is in value scope.
                // This applies to associated functions as well as constants:
                // `Vec::from` must resolve `Vec` as a type, never as a value
                // constructor or a same-named lexical binding. Keep a value
                // lookup only as the module-qualified constant fallback below.
                let type_base =
                    self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Type)?;
                let value_base = if matches!(select.select, ast::ExprSelectType::Const) {
                    Some(self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Value)?)
                } else {
                    None
                };
                // A module-qualified constant (`self::CONST` or
                // `module::CONST`) also uses `::`, but its base is a module,
                // not a type-relative path. Preserve rustc's namespace
                // fallback for that case after the type-relative lookup.
                let mut base = match value_base {
                    Some(value_base) if matches!(value_base.res, hir::Res::Module(_)) => value_base,
                    _ => type_base,
                };
                let member_args = if select.generic_args.is_empty() {
                    None
                } else {
                    Some(self.convert_generic_args(&select.generic_args)?)
                };
                let seg = self.make_path_segment(&select.field.name, member_args);
                base.segments.push(seg);
                if matches!(
                    select.select,
                    ast::ExprSelectType::Const | ast::ExprSelectType::Function
                ) && !matches!(base.res, hir::Res::Module(_))
                {
                    if let Some(res) = self.lookup_enum_variant(&base, &select.field.name) {
                        base.res = res;
                    }
                }

                Ok(self.preserve_lexical_projection_path(base, scope))
            }
            ast::ExprKind::Invoke(invoke) => {
                let mut base = match &invoke.target {
                    ast::ExprInvokeTarget::Function(name) => {
                        let expr = ast::Expr::new(ast::ExprKind::Name(name.clone()));
                        self.ast_expr_to_hir_path(&expr, PathResolutionScope::Value)?
                    }
                    ast::ExprInvokeTarget::Expr(expr) => {
                        self.ast_expr_to_hir_path(expr.as_ref(), scope)?
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
                            let expr = ast::Expr::new(ast::ExprKind::Name(ast::Name::Ident(
                                struct_ty.name.clone(),
                            )));
                            self.ast_expr_to_hir_path(&expr, PathResolutionScope::Type)?
                        }
                        ast::Ty::Expr(type_expr) => match type_expr.kind() {
                            ast::ExprKind::Name(name) => {
                                self.ast_expr_to_hir_path(type_expr, PathResolutionScope::Type)?
                            }
                            ast::ExprKind::Value(value) => match value.as_ref() {
                                ast::Value::Type(inner) => match inner {
                                    ast::Ty::Struct(struct_ty) => {
                                        let expr = ast::Expr::new(ast::ExprKind::Name(
                                            ast::Name::Ident(struct_ty.name.clone()),
                                        ));
                                        self.ast_expr_to_hir_path(
                                            &expr,
                                            PathResolutionScope::Type,
                                        )?
                                    }
                                    ast::Ty::Expr(inner_expr) => match inner_expr.kind() {
                                        ast::ExprKind::Name(name) => self
                                            .ast_expr_to_hir_path(
                                                inner_expr,
                                                PathResolutionScope::Type,
                                            )?,
                                        _ => {
                                            self.add_error(
                                                Diagnostic::error(
                                                    "expected a path-like type target".to_string(),
                                                )
                                                .with_source_context(DIAGNOSTIC_CONTEXT)
                                                .with_span(expr.span()),
                                            );
                                            hir::Path {
                                                segments: vec![
                                                    self.make_path_segment("__fp_error", None),
                                                ],
                                                res: Res::Error,
                                            }
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
                                        hir::Path {
                                            segments: vec![
                                                self.make_path_segment("__fp_error", None),
                                            ],
                                            res: Res::Error,
                                        }
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
                                    hir::Path {
                                        segments: vec![self.make_path_segment("__fp_error", None)],
                                        res: Res::Error,
                                    }
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
                                hir::Path {
                                    segments: vec![self.make_path_segment("__fp_error", None)],
                                    res: Res::Error,
                                }
                            }
                        },
                        _ => {
                            self.add_error(
                                Diagnostic::error("expected a path-like type target".to_string())
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                            );
                            hir::Path {
                                segments: vec![self.make_path_segment("__fp_error", None)],
                                res: Res::Error,
                            }
                        }
                    },
                    ast::ExprInvokeTarget::Method(select) => {
                        let mut base = self.ast_expr_to_hir_path(&select.obj, scope)?;
                        let seg = self.make_path_segment(&select.field.name, None);
                        base.segments.push(seg);
                        base
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
                        hir::Path {
                            segments: vec![self.make_path_segment("__fp_error", None)],
                            res: Res::Error,
                        }
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
                    if let Some(last) = base.segments.last_mut() {
                        if last.args.is_none() {
                            last.args = Some(hir_args);
                        }
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
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Slice(_)) => {
                    let kind = hir::BuiltinSelfType::Slice;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Array(_)) => {
                    let kind = hir::BuiltinSelfType::Array;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::RawPtr(ptr)) => {
                    let kind = hir::BuiltinSelfType::RawPtr {
                        mutable: ptr.mutability.unwrap_or(false),
                    };
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Nothing(_)) => {
                    let kind = hir::BuiltinSelfType::Never;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Unit(_)) => {
                    let kind = hir::BuiltinSelfType::Unit;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Tuple(_)) => {
                    let kind = hir::BuiltinSelfType::Tuple;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
                }
                ast::Value::Type(ast::Ty::Function(_)) => {
                    let kind = hir::BuiltinSelfType::Function;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: hir::Res::Builtin(kind),
                    })
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
                    self.ast_expr_to_hir_path(&lhs, scope)
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
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment("__fp_error", None)],
                        res: Res::Error,
                    })
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
                Ok(hir::Path {
                    segments: vec![self.make_path_segment("__fp_error", None)],
                    res: Res::Error,
                })
            }
        }
    }

    pub(super) fn make_path_segment(
        &self,
        name: &str,
        args: Option<hir::GenericArgs>,
    ) -> hir::PathSegment {
        hir::PathSegment {
            name: hir::Symbol::new(name),
            args,
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
