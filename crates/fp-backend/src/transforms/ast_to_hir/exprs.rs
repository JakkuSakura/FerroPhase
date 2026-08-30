use super::*;
use fp_core::ast::path::{PathPrefix, QualifiedPath};
use fp_core::intrinsics::CallKind;
use fp_core::query::lower_fp_expr_to_query;

mod loops;
use loops::*;
mod literal_values;
use literal_values::*;

impl AstToHirLowerer {
    /// Resolve an enum variant through a type alias in value position.
    ///
    /// Rust keeps the alias and the nominal enum as distinct definitions, but
    /// `Alias::Variant` is resolved against the enum's variant namespace. The
    /// HIR value table stores the variant under the nominal enum path, so the
    /// resolver has to follow the alias target before doing the final member
    /// lookup. This is especially important for `ascii::Char`, which is a
    /// public alias of `ascii_char::AsciiChar`.
    pub(super) fn enum_variant_through_type_path(
        &self,
        type_path: &fp_core::ast::path::QualifiedPath,
        variant_name: &str,
    ) -> Option<hir::Res> {
        // A bare nominal prefix is resolved relative to the current module
        // by the ordinary type-name resolver. `lookup_global_res` consumes a
        // fully qualified path, so asking it about `DiffRange` alone would
        // skip that lexical/module-relative tier and lose `DiffRange::Commit`.
        // Resolve the prefix through the same namespace-aware path first,
        // then use the published item identity to inspect its variants.
        let type_res = if type_path.segments.len() == 1 {
            self.resolve_type_symbol(type_path.segments[0].as_str())
                .or_else(|| self.lookup_global_res(type_path, PathResolutionScope::Type))
        } else {
            self.lookup_global_res(type_path, PathResolutionScope::Type)
        }?;
        let mut def_id = match type_res {
            hir::Res::Def(def_id) => def_id,
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

        for _ in 0..32 {
            let item = self
                .package
                .def_map
                .get(&def_id)
                .cloned()
                .or_else(|| self.hir_program.item(def_id.clone()))?;
            match &item.kind {
                hir::ItemKind::Enum(enum_def) => {
                    return enum_def
                        .variants
                        .iter()
                        .find(|variant| variant.name.as_str() == variant_name)
                        .map(|variant| hir::Res::Def(variant.def_id.clone()));
                }
                hir::ItemKind::Struct(_) => return None,
                _ => {
                    let target = self
                        .package
                        .type_alias_targets
                        .get(&def_id)
                        .cloned()
                        .or_else(|| self.hir_program.type_alias_target(def_id.clone()))?;
                    let hir::TypeExprKind::Path(path) = &target.kind else {
                        return None;
                    };
                    def_id = match &path.res {
                        Some(hir::Res::Def(target_def_id)) => target_def_id.clone(),
                        _ => self
                            .lookup_global_res(
                                &QualifiedPath::new(
                                    path.segments
                                        .iter()
                                        .map(|segment| segment.name.as_str().to_string())
                                        .collect(),
                                ),
                                PathResolutionScope::Type,
                            )
                            .and_then(|res| match res {
                                hir::Res::Def(target_def_id) => Some(target_def_id),
                                _ => None,
                            })?,
                    };
                }
            }
        }
        None
    }

    /// Resolve an external path from the dependency package that owns it.
    /// The consumer's module tree contains copied bindings for normal imports,
    /// but the dependency tree is authoritative for an extern-prelude root
    /// and for paths whose canonical spelling was published by the provider
    /// (`alloc::vec::Vec`, for example). This preserves the dependency's
    /// namespace and `Res` instead of reconstructing either from a name.
    pub(super) fn lookup_dependency_module_tree(
        &self,
        path: &QualifiedPath,
        scope: PathResolutionScope,
    ) -> Option<hir::Res> {
        let leaf = path.segments.last()?.as_str();
        let prefix = QualifiedPath::new(
            path.segments[..path.segments.len() - 1]
                .iter()
                .cloned()
                .collect(),
        );
        let namespace = scope.namespace();
        let mut packages: Vec<_> = self.hir_program.packages.values().collect();
        packages.sort_by(|left, right| left.borrow().id.cmp(&right.borrow().id));

        for package in packages {
            let package = package.borrow();
            let external_root = hir::HirProgram::external_crate_name(&package.id);

            // Follow public module re-exports before consulting physical
            // module paths. Bundled sysroot crates publish paths such as
            // `std::fmt` as aliases to `alloc::fmt`; rustc resolves the
            // alias first and then performs the final lookup in its target
            // module. A suffix scan alone cannot see that relationship.
            // Resolve against actual module nodes rather than trying a list
            // of package-relative spellings. A bundled dependency can expose
            // an extern-prelude root below its package root (for example, a
            // requested `alloc::vec` is stored as `std::alloc::vec`). The
            // suffix relation is structural and keeps the final binding
            // lookup in the requested namespace.
            let mut module_paths = vec![prefix.clone()];
            if prefix.segments.first().map(String::as_str) != Some(external_root.as_str()) {
                let mut rooted = vec![external_root.clone()];
                rooted.extend(prefix.segments.iter().cloned());
                module_paths.push(QualifiedPath::new(rooted));
            }
            if prefix.segments.first().map(String::as_str) == Some(external_root.as_str()) {
                let mut bundled = vec![external_root.clone(), external_root.clone()];
                bundled.extend(prefix.segments.iter().skip(1).cloned());
                module_paths.push(QualifiedPath::new(bundled));
            }

            for module_path in module_paths {
                let Some(module) = package.module_tree.module_id(&module_path) else {
                    continue;
                };
                if let Some(entry) = package.module_tree.lookup(module, namespace, leaf) {
                    if entry.export.can_access(&self.module_path.segments) {
                        return Some(entry.res.clone());
                    }
                }
            }
        }
        None
    }

    /// Records a diagnostic for an AST construct that can't be lowered to
    /// HIR (an unhandled shape, an unnormalized macro, etc.) and returns an
    /// empty-block placeholder in its place — lets HIR generation for the
    /// rest of the package continue past isolated gaps instead of
    /// aborting entirely on the first one (which previously forced a
    /// whole-package fallback to the untyped pipeline over a single
    /// unsupported construct anywhere in it). Mirrors the pre-existing
    /// closure-lowering-not-implemented precedent.
    fn error_placeholder_expr_kind(&mut self, message: String, error_span: Span) -> hir::ExprKind {
        self.add_error(
            Diagnostic::error(message)
                .with_source_context(DIAGNOSTIC_CONTEXT)
                .with_span(error_span),
        );
        hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts: Vec::new(),
            expr: None,
        })
    }

    /// Transform an AST expression to HIR expression
    pub(super) fn transform_expr_to_hir(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        let Some(normalizer) = self.intrinsic_normalizer.as_ref() else {
            return self.transform_expr_to_hir_inner(ast_expr);
        };

        // `Invoke` (plain function/method calls) is deliberately NOT
        // normalized here — by name, at all. Method calls need the
        // receiver's real resolved type to disambiguate safely from a
        // same-named user method (see `HirToAstLifter`'s post-typecheck
        // reclassification). Bare function-name calls are recognized in
        // `transform_invoke_to_hir`'s `Function` arm instead, purely from
        // the callee's *resolved* `DefId` and that declaration's own
        // `#[op]`/`#[intrinsic]` attribute (`hir::HirPackage::op_defs`/
        // `intrinsic_defs`) — never by name/path-matching the call site,
        // which can't tell a builtin apart from a same-named real user
        // function (e.g. `std::json`'s own `print`, called from within
        // `json` itself, is not the builtin print intrinsic).
        let needs_normalization = matches!(
            ast_expr.kind(),
            ast::ExprKind::Macro(_)
                | ast::ExprKind::IntrinsicCall(_)
                | ast::ExprKind::IntrinsicContainer(_)
                | ast::ExprKind::Struct(_)
                | ast::ExprKind::Structural(_)
        );
        if !needs_normalization {
            return self.transform_expr_to_hir_inner(ast_expr);
        }

        let expr_span = ast_expr.span();
        let mut normalized = match normalizer.normalize_expr(ast_expr.clone()) {
            Ok(n) => n.into_inner(),
            // A malformed macro/format construct (e.g. an unsupported format
            // spec) shouldn't fail type-checking for the *entire* package —
            // degrade just this one expression via the same
            // diagnostic-manager-backed placeholder every other unlowerable
            // construct in this file already uses.
            Err(e) => {
                let span = self.create_span(1);
                let hir_id = self.next_id();
                let kind = self.error_placeholder_expr_kind(e.to_string(), expr_span);
                return Ok(hir::Expr { hir_id, span, kind });
            }
        };
        if matches!(ast_expr.kind(), ast::ExprKind::Macro(_))
            && matches!(normalized.kind(), ast::ExprKind::IntrinsicCall(_))
        {
            normalized = match normalizer.normalize_expr(normalized) {
                Ok(n) => n.into_inner(),
                Err(e) => {
                    let span = self.create_span(1);
                    let hir_id = self.next_id();
                    let kind = self.error_placeholder_expr_kind(e.to_string(), expr_span);
                    return Ok(hir::Expr { hir_id, span, kind });
                }
            };
        }
        self.transform_expr_to_hir_inner(&normalized)
    }

    fn transform_expr_to_hir_inner(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        use ast::ExprKind;

        let expr_span = ast_expr.span();

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        if let Some(document) = lower_fp_expr_to_query(ast_expr, None) {
            let ir = self.resolve_query_ir(&document)?;
            return Ok(hir::Expr {
                hir_id,
                kind: hir::ExprKind::Query(hir::Query {
                    origin: super::query_origin(&document),
                    ir,
                    span: expr_span,
                }),
                span,
            });
        }

        let kind = match ast_expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Bytes(bytes) => {
                    let ty = fp_core::ast::resolved_expr_type(ast_expr.id());
                    Self::transform_bytes_value_to_hir(bytes, ty.as_ref())
                }
                ast::Value::Int(_)
                | ast::Value::UInt(_)
                | ast::Value::BigInt(_)
                | ast::Value::Decimal(_)
                | ast::Value::BigDecimal(_)
                    if fp_core::ast::resolved_expr_type(ast_expr.id()).is_some() =>
                {
                    let target = fp_core::ast::resolved_expr_type(ast_expr.id())
                        .expect("numeric literal type checked above");
                    let value = hir::Expr {
                        hir_id: self.next_id(),
                        kind: self.transform_value_to_hir(value)?,
                        span,
                    };
                    hir::ExprKind::Cast(
                        Box::new(value),
                        Box::new(self.transform_type_to_hir(&target)?),
                    )
                }
                _ => self.transform_value_to_hir(value)?,
            },
            ExprKind::Id(expr_id) => self.error_placeholder_expr_kind(
                format!("unresolved expression id {expr_id} during AST→HIR lowering"),
                expr_span,
            ),
            ExprKind::Name(_) => hir::ExprKind::Path(
                self.ast_expr_to_hir_path(ast_expr, PathResolutionScope::Value)?,
            ),
            ExprKind::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            ExprKind::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            ExprKind::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            ExprKind::Select(select) => self.transform_select_to_hir(select)?,
            ExprKind::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            ExprKind::Block(block) => {
                hir::ExprKind::Block(self.transform_block_node_to_hir(block)?)
            }
            ExprKind::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            ExprKind::Match(match_expr) => self.transform_match_to_hir(match_expr)?,
            ExprKind::Loop(loop_expr) => self.transform_loop_to_hir(loop_expr)?,
            ExprKind::While(while_expr) => self.transform_while_to_hir(while_expr)?,
            ExprKind::With(expr_with) => {
                let context = self.transform_expr_to_hir(expr_with.context.as_ref())?;
                let body = self.transform_expr_to_hir(expr_with.body.as_ref())?;
                hir::ExprKind::With(Box::new(context), Box::new(body))
            }
            ExprKind::Assign(assign) => self.transform_assign_to_hir(assign)?,
            ExprKind::Paren(paren) => self.transform_paren_to_hir(paren)?,
            ExprKind::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            ExprKind::Array(array_expr) => self.transform_array_to_hir(array_expr)?,
            ExprKind::ArrayRepeat(array_repeat) => {
                self.transform_array_repeat_to_hir(array_repeat)?
            }
            ExprKind::Tuple(tuple_expr) => {
                let values = tuple_expr
                    .values
                    .iter()
                    .map(|value| self.transform_expr_to_hir(value))
                    .collect::<Result<Vec<_>>>()?;
                hir::ExprKind::Tuple(values)
            }
            ExprKind::Range(range) => self.transform_range_to_hir(range)?,
            ExprKind::Index(index_expr) => {
                if let ast::ExprKind::Range(range) = index_expr.index.kind() {
                    if range.step.is_some() {
                        self.add_error(
                            Diagnostic::warning(
                                "range steps are not supported in slicing; ignoring step"
                                    .to_string(),
                            )
                            .with_span(expr_span),
                        );
                    }
                    let base_expr = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let start_expr = range
                        .start
                        .as_ref()
                        .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                        .transpose()?
                        .map(Box::new);
                    let end_expr = range
                        .end
                        .as_ref()
                        .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                        .transpose()?
                        .map(Box::new);
                    let inclusive = matches!(range.limit, ast::ExprRangeLimit::Inclusive);
                    hir::ExprKind::Slice(hir::SliceExpr {
                        hir_id: self.next_id(),
                        base: Box::new(base_expr),
                        start: start_expr,
                        end: end_expr,
                        inclusive,
                    })
                } else {
                    let base = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let index = self.transform_expr_to_hir(index_expr.index.as_ref())?;
                    hir::ExprKind::Index(Box::new(base), Box::new(index))
                }
            }
            ExprKind::Quote(_quote) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::Splice(_splice) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::SplicePending(_pending) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::Try(expr_try) => {
                let body = Box::new(self.transform_expr_to_hir(expr_try.expr.as_ref())?);
                let mut catches = Vec::with_capacity(expr_try.catches.len());
                for catch in &expr_try.catches {
                    let pat = catch
                        .pat
                        .as_ref()
                        .map(|pat| self.transform_pattern(pat.as_ref()))
                        .transpose()?;
                    catches.push(hir::TryCatch {
                        hir_id: self.next_id(),
                        pat,
                        body: self.transform_expr_to_hir(catch.body.as_ref())?,
                    });
                }
                let elze = expr_try
                    .elze
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                let finally = expr_try
                    .finally
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Try(hir::TryExpr {
                    expr: body,
                    catches,
                    elze,
                    finally,
                })
            }
            ExprKind::Await(expr_await) => {
                let inner_expr = self.transform_expr_to_hir(expr_await.base.as_ref())?;
                return Ok(hir::Expr {
                    hir_id,
                    kind: inner_expr.kind,
                    span,
                });
            }
            ExprKind::Async(async_expr) => {
                let inner_expr = self.transform_expr_to_hir(async_expr.expr.as_ref())?;
                return Ok(hir::Expr {
                    hir_id,
                    kind: inner_expr.kind,
                    span,
                });
            }
            ExprKind::For(for_expr) => {
                let kind = self.transform_for_to_hir(for_expr)?;
                return Ok(hir::Expr { hir_id, kind, span });
            }
            ExprKind::Closure(closure) => {
                // `capabilities.first_class_closures` (set by the driver
                // per target, see `fp_core::capabilities::
                // LanguageCapabilities`) means the closure hasn't already
                // been defunctionalized by `ClosureLowering` — lower it as
                // a real, first-class HIR node so `HirTypeChecker` can
                // resolve its signature via ordinary expected-type
                // propagation from its call site (see `hir_typeck.rs`'s
                // `Closure` arm). Every other pipeline (Native, needing
                // MIR) still runs the pre-pass, so a closure never reaches
                // here in the first place for those.
                if self.lowering_config.capabilities.first_class_closures {
                    self.push_value_scope();
                    let params = closure
                        .params
                        .iter()
                        .map(|pat| -> Result<hir::Param> {
                            let hir_pat = self.transform_pattern(pat)?;
                            self.register_pattern_bindings(&hir_pat);
                            Ok(hir::Param {
                                hir_id: self.next_id(),
                                pat: hir_pat,
                                // No source-level annotation for a bare
                                // closure pattern — `HirTypeChecker`'s
                                // `Closure` arm resolves the real type from
                                // the call site's expected-type hint,
                                // falling back to this placeholder only
                                // when none is available.
                                ty: hir::TypeExpr {
                                    hir_id: self.next_id(),
                                    kind: hir::TypeExprKind::Infer,
                                    span,
                                },
                                is_context: false,
                                as_tuple: false,
                                as_dict: false,
                                default: None,
                            })
                        })
                        .collect::<Result<Vec<_>>>();
                    let params = match params {
                        Ok(params) => params,
                        Err(error) => {
                            self.pop_value_scope();
                            return Err(error);
                        }
                    };
                    let body = self.transform_expr_to_hir(closure.body.as_ref());
                    self.pop_value_scope();
                    return Ok(hir::Expr {
                        hir_id,
                        kind: hir::ExprKind::Closure(hir::ExprClosure {
                            params,
                            body: Box::new(body?),
                        }),
                        span,
                    });
                }
                self.add_error(
                    Diagnostic::error("closure lowering not implemented".to_string())
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(expr_span),
                );
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                return Ok(hir::Expr {
                    hir_id,
                    kind: hir::ExprKind::Block(block),
                    span,
                });
            }
            ExprKind::Cast(cast_expr) => {
                let operand = self.transform_expr_to_hir(cast_expr.expr.as_ref())?;
                let ty = self.transform_type_to_hir(&cast_expr.ty)?;
                hir::ExprKind::Cast(Box::new(operand), Box::new(ty))
            }
            ExprKind::Macro(mac) => self.error_placeholder_expr_kind(
                format!(
                    "macro `{}` was not lowered during normalization",
                    mac.invocation.path
                ),
                expr_span,
            ),
            ExprKind::FormatString(format_str) => {
                self.transform_format_string_to_hir(format_str)?
            }
            ExprKind::Return(ret) => {
                let value = ret
                    .value
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Return(value)
            }
            ExprKind::Break(brk) => {
                let value = brk
                    .value
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Break(value)
            }
            ExprKind::Continue(_) => hir::ExprKind::Continue,
            ExprKind::ConstBlock(const_block) => {
                self.transform_const_block_to_hir(hir_id.clone(), const_block)?
            }
            ExprKind::IntrinsicContainer(container) => {
                self.transform_intrinsic_container_to_hir(container)?
            }
            ExprKind::IntrinsicCall(call) => self.transform_intrinsic_call_to_hir(call)?,
            ExprKind::Reference(reference) => {
                let inner = self.transform_expr_to_hir(reference.referee.as_ref())?;
                let mutable = match reference.mutable {
                    Some(true) => hir::ty::Mutability::Mut,
                    _ => hir::ty::Mutability::Not,
                };
                hir::ExprKind::Reference(hir::ExprReference {
                    hir_id: self.next_id(),
                    mutable,
                    raw: reference.raw,
                    expr: Box::new(inner),
                })
            }
            ExprKind::Dereference(deref) => {
                let inner = self.transform_expr_to_hir(deref.referee.as_ref())?;
                hir::ExprKind::Unary(hir::UnOp::Deref, Box::new(inner))
            }
            _ => self.error_placeholder_expr_kind(
                format!(
                    "unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                ),
                expr_span,
            ),
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    fn transform_const_block_to_hir(
        &mut self,
        hir_id: hir::HirId,
        const_block: &ast::ExprConstBlock,
    ) -> Result<hir::ExprKind> {
        let body = Box::new(self.transform_expr_to_hir(const_block.expr.as_ref())?);
        let def_id = self.next_def_id();
        // Recorded once, unconditionally, right here — not lazily by the
        // type checker each time it happens to encounter this node (see
        // `hir::HirPackage::const_block_defs`'s doc comment).
        self.package.record_const_block_def(
            def_id.clone(),
            hir::Block {
                hir_id,
                stmts: Vec::new(),
                expr: Some(body.clone()),
            },
        );
        Ok(hir::ExprKind::ConstBlock(hir::ExprConstBlock {
            def_id,
            body,
        }))
    }

    // create_main_function moved to items.rs

    /// Generate next HIR ID, scoped to the current owner (see
    /// `AstToHirLowerer::current_owner`). Falls back to the package-root
    /// owner when no item-like definition is currently being lowered.
    pub(super) fn next_id(&mut self) -> hir::HirId {
        let owner = self
            .current_owner
            .clone()
            .map(hir::OwnerId)
            .unwrap_or_else(|| hir::OwnerId::root(self.package_id.clone()));
        let local_id = self.local_id;
        self.local_id += 1;
        hir::HirId::new(owner, local_id)
    }

    /// Generate next definition ID
    pub(super) fn next_def_id(&mut self) -> hir::DefId {
        self.package.next_def_id()
    }

    // transform_function moved to items.rs

    // transform_params moved to items.rs

    // transform_generics moved to items.rs

    // wrap_ref_type moved to items.rs

    // make_self_param moved to items.rs

    // transform_impl moved to items.rs

    /// Transform AST value to HIR expression kind
    pub(super) fn transform_value_to_hir(&mut self, value: &ast::BValue) -> Result<hir::ExprKind> {
        use ast::Value;

        match value.as_ref() {
            Value::Int(i) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(i.value))),
            Value::UInt(u) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(u.value as i64))),
            // `hir::Lit::Integer` is `i64`-only — no arbitrary-precision HIR
            // literal exists. Best-effort narrow (saturating on overflow,
            // matching `fp-kotlin`'s own `Value::BigInt` rendering, which
            // already accepts the same imprecision for values this large).
            Value::BigInt(b) => {
                let narrowed = b.value.to_string().parse::<i64>().unwrap_or(i64::MAX);
                Ok(hir::ExprKind::Literal(hir::Lit::Integer(narrowed)))
            }
            Value::Bool(b) => Ok(hir::ExprKind::Literal(hir::Lit::Bool(b.value))),
            Value::String(s) => Ok(hir::ExprKind::Literal(hir::Lit::Str(s.value.clone()))),
            Value::Bytes(bytes) => {
                if let Some(text) = Self::borrowed_string_from_bytes(bytes) {
                    Ok(hir::ExprKind::Literal(hir::Lit::Str(text)))
                } else {
                    Ok(self.error_placeholder_expr_kind(
                        "byte values are not supported in AST→HIR expression lowering".to_string(),
                        value.span(),
                    ))
                }
            }
            Value::Decimal(d) => Ok(hir::ExprKind::Literal(hir::Lit::Float(d.value))),
            Value::Char(ch) => Ok(hir::ExprKind::Literal(hir::Lit::Char(ch.value))),
            Value::Unit(_) => {
                let block_id = self.next_id();
                Ok(hir::ExprKind::Block(hir::Block {
                    hir_id: block_id,
                    stmts: Vec::new(),
                    expr: None,
                }))
            }
            Value::Null(_) | Value::None(_) => Ok(hir::ExprKind::Literal(hir::Lit::Null)),
            Value::Struct(struct_val) => {
                let struct_name = struct_val.ty.name.name.as_str();
                let mut segments = Vec::new();
                segments.push(self.make_path_segment(struct_name, None));
                let res = self.resolve_type_symbol(struct_name);

                let path = hir::Path { segments, res };

                let mut fields = Vec::with_capacity(struct_val.structural.fields.len());
                for field in &struct_val.structural.fields {
                    let field_expr_kind =
                        self.transform_value_to_hir(&Box::new(field.value.clone()))?;
                    let field_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: field_expr_kind,
                        span: self.create_span(1),
                    };

                    fields.push(hir::StructExprField {
                        hir_id: self.next_id(),
                        name: field.name.clone().into(),
                        expr: field_expr,
                    });
                }

                Ok(hir::ExprKind::Struct(path, fields))
            }
            Value::Structural(structural) => {
                let def = self.materialize_structural_value_def(structural)?;
                let path = self.path_for_structural_def(&def);

                let mut fields = Vec::with_capacity(structural.fields.len());
                for field in &structural.fields {
                    let field_expr_kind =
                        self.transform_value_to_hir(&Box::new(field.value.clone()))?;
                    let field_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: field_expr_kind,
                        span: self.create_span(1),
                    };

                    fields.push(hir::StructExprField {
                        hir_id: self.next_id(),
                        name: field.name.clone().into(),
                        expr: field_expr,
                    });
                }

                Ok(hir::ExprKind::Struct(path, fields))
            }
            Value::List(list) => {
                let mut elements = Vec::with_capacity(list.values.len());
                for value in &list.values {
                    let expr_kind = self.transform_value_to_hir(&Box::new(value.clone()))?;
                    elements.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: expr_kind,
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(elements))
            }
            Value::Map(map) => {
                let mut entries = Vec::with_capacity(map.entries.len());
                for entry in &map.entries {
                    let key_kind = self.transform_value_to_hir(&Box::new(entry.key.clone()))?;
                    let value_kind = self.transform_value_to_hir(&Box::new(entry.value.clone()))?;
                    let key_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: key_kind,
                        span: self.create_span(1),
                    };
                    let value_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: value_kind,
                        span: self.create_span(1),
                    };
                    let entry = hir::ExprKind::Array(vec![key_expr, value_expr]);
                    entries.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: entry,
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(entries))
            }
            Value::Expr(expr) => self.transform_expr_to_hir(expr).map(|e| e.kind),
            // Deferred const-block types (Escaped) — placeholder until retry
            Value::Escaped(_) => {
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new("__fp_escaped"),
                        args: None,
                    }],
                    res: None,
                };
                Ok(hir::ExprKind::Path(path))
            }
            // A type value the *parser* already constant-folded (e.g. a
            // call argument like `&'static str` that fails plain-
            // expression grammar and falls back to `parse_type_expr`,
            // `fp-lang/src/ast/expr.rs`'s `parse_expr_or_type_value`) —
            // reflect it at runtime via the same `std::intrinsics::
            // primitive_type` intrinsic a bare `i64`/etc. value reference
            // now resolves to through its real `std::meta` prelude
            // `const` (ordinary name resolution, no special-casing here).
            Value::Type(ty) => match ty.primitive_type_value_name() {
                Some(name) => Ok(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: CallKind::PrimitiveType,
                    callargs: vec![hir::CallArg {
                        name: hir::Symbol::new("arg0"),
                        value: hir::Expr {
                            hir_id: self.next_id(),
                            kind: hir::ExprKind::Literal(hir::Lit::Str(name)),
                            span: value.span(),
                        },
                    }],
                })),
                None => Ok(self.error_placeholder_expr_kind(
                    format!("unsupported type value in expression position during AST→HIR: {ty:?}"),
                    value.span(),
                )),
            },
            Value::Function(func) => {
                let name = func.sig.name.clone().unwrap_or_else(|| {
                    self.add_error(
                        Diagnostic::error(
                            "function value must have a name for HIR lowering".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(value.span()),
                    );
                    ast::Ident::new("__fp_error".to_string())
                });
                let name = Name::Ident(name);
                let path = self.name_to_hir_path_with_scope(&name, PathResolutionScope::Value)?;
                Ok(hir::ExprKind::Path(path))
            }
            _ => Ok(self.error_placeholder_expr_kind(
                format!(
                    "unimplemented AST value type for HIR transformation: {:?}",
                    std::mem::discriminant(value.as_ref())
                ),
                value.span(),
            )),
        }
    }

    fn transform_array_to_hir(&mut self, array: &ast::ExprArray) -> Result<hir::ExprKind> {
        let mut elements = Vec::with_capacity(array.values.len());
        for value in &array.values {
            elements.push(self.transform_expr_to_hir(value)?);
        }
        Ok(hir::ExprKind::Array(elements))
    }

    fn transform_array_repeat_to_hir(
        &mut self,
        repeat: &ast::ExprArrayRepeat,
    ) -> Result<hir::ExprKind> {
        let elem = Box::new(self.transform_expr_to_hir(repeat.elem.as_ref())?);
        let len = Box::new(self.transform_expr_to_hir(repeat.len.as_ref())?);
        Ok(hir::ExprKind::ArrayRepeat { elem, len })
    }

    fn transform_intrinsic_container_to_hir(
        &mut self,
        container: &ast::ExprIntrinsicContainer,
    ) -> Result<hir::ExprKind> {
        match container {
            ast::ExprIntrinsicContainer::VecElements { elements } => {
                let mut items = Vec::with_capacity(elements.len());
                for element in elements {
                    items.push(self.transform_expr_to_hir(element)?);
                }
                Ok(hir::ExprKind::Array(items))
            }
            ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let elem = Box::new(self.transform_expr_to_hir(elem.as_ref())?);
                let len = Box::new(self.transform_expr_to_hir(len.as_ref())?);
                Ok(hir::ExprKind::ArrayRepeat { elem, len })
            }
            ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                let mut items = Vec::with_capacity(entries.len());
                for entry in entries {
                    let key = self.transform_expr_to_hir(&entry.key)?;
                    let value = self.transform_expr_to_hir(&entry.value)?;
                    items.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Array(vec![key, value]),
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(items))
            }
        }
    }

    /// Transform binary operation to HIR
    pub(super) fn transform_binop_to_hir(
        &mut self,
        binop: &ast::ExprBinOp,
    ) -> Result<hir::ExprKind> {
        let left = Box::new(self.transform_expr_to_hir(&binop.lhs)?);
        let right = Box::new(self.transform_expr_to_hir(&binop.rhs)?);
        let op = self.convert_binop_kind(&binop.kind);

        Ok(hir::ExprKind::Binary(op, left, right))
    }

    /// Transform unary operation to HIR
    pub(super) fn transform_unop_to_hir(&mut self, unop: &ast::ExprUnOp) -> Result<hir::ExprKind> {
        let operand = Box::new(self.transform_expr_to_hir(&unop.val)?);
        let op = self.convert_unop_kind(&unop.op, unop.span())?;

        Ok(hir::ExprKind::Unary(op, operand))
    }

    /// Transform function call/invoke to HIR
    pub(super) fn transform_invoke_to_hir(
        &mut self,
        invoke: &ast::ExprInvoke,
    ) -> Result<hir::ExprKind> {
        match &invoke.target {
            ast::ExprInvokeTarget::Method(select) => {
                let receiver_can_be_type_path = match select.obj.kind() {
                    ast::ExprKind::Name(_) | ast::ExprKind::Select(_) => true,
                    ast::ExprKind::Invoke(receiver) => {
                        matches!(receiver.target, ast::ExprInvokeTarget::Type(_))
                    }
                    _ => false,
                };
                if receiver_can_be_type_path
                    && let Some(segments) = self.path_segments_from_expr(&select.obj)
                {
                    let root_is_runtime_value = segments.first().is_some_and(|segment| {
                        segment.name.as_str() == "self"
                            || self.resolve_lexical_value_symbol(&segment.name).is_some()
                    });
                    if !root_is_runtime_value {
                        // Lower the original expression instead of rebuilding
                        // a name from `path_segments_from_expr`. The latter is
                        // intentionally only a shape probe and cannot retain
                        // generic arguments on `ParameterPath` segments. Rustc
                        // keeps those arguments on the resolved QPath head,
                        // and type-directed associated-item lookup needs them
                        // for `Vec::<T>::from`, `Arc::<T>::new`, and the like.
                        let base_path =
                            self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Type)?;
                        if matches!(
                            base_path.res,
                            Some(hir::Res::Def(_))
                                | Some(hir::Res::Builtin(_))
                                | Some(hir::Res::SelfTy)
                        ) {
                            // This is rustc's `QPath::TypeRelative` shape:
                            // the resolver has established the nominal/type
                            // base, while associated-item selection remains
                            // type-directed. Re-resolving the joined spelling
                            // as a value path can lose `Vec`/`String`'s type
                            // `Res` (or bind a same-named module), which then
                            // makes the type checker report an unresolved
                            // value path instead of selecting the impl item.
                            let mut path = base_path;
                            let member_args = if select.generic_args.is_empty() {
                                None
                            } else {
                                Some(self.convert_generic_args(&select.generic_args)?)
                            };
                            path.segments
                                .push(self.make_path_segment(&select.field.name, member_args));
                            if let Some(res) = self.lookup_enum_variant(&path, &select.field.name) {
                                path.res = Some(res);
                            }
                            let func_expr = hir::Expr {
                                hir_id: self.next_id(),
                                kind: hir::ExprKind::Path(path),
                                span: self.create_span(1),
                            };
                            let args = self.transform_call_args_strict(&invoke.args)?;
                            return Ok(hir::ExprKind::Call(Box::new(func_expr), args));
                        }
                    }
                }
                let receiver = self.transform_expr_to_hir(&select.obj)?;
                let generic_args = if select.generic_args.is_empty() {
                    None
                } else {
                    Some(hir::GenericArgs {
                        args: select
                            .generic_args
                            .iter()
                            .map(|ty| {
                                self.transform_type_to_hir(ty)
                                    .map(|ty| hir::GenericArg::Type(Box::new(ty)))
                            })
                            .collect::<Result<Vec<_>>>()?,
                    })
                };
                let args = self.transform_call_args_strict(&invoke.args)?;
                Ok(hir::ExprKind::MethodCall(
                    Box::new(receiver),
                    select.field.clone().into(),
                    generic_args,
                    args,
                ))
            }
            ast::ExprInvokeTarget::Function(name) => {
                if let Some(ident) = name.as_ident() {
                    if ident.as_str() == "import" {
                        return Ok(self.error_placeholder_expr_kind(
                            "dynamic import is only supported in interpret mode".to_string(),
                            invoke.span(),
                        ));
                    }
                    // `type(X)` — the reflection query producing `std::meta::
                    // TypeDescriptor`. Unlike ordinary calls (see the comment
                    // below), this can *never* be resolved by a real
                    // declaration's `DefId`: `type` is a reserved keyword, so
                    // no user or stdlib function can ever exist for it to
                    // resolve to. This used to be recognized post-parse by
                    // `fp-lang`'s own `resolve_lang_intrinsic` (`["type"] => ...
                    // CallKind::TypeOf`), but that whole pass only ever ran
                    // during AST normalization, which `needs_normalization`
                    // (this file) deliberately stopped running for `Invoke`
                    // expressions — leaving `type(X)` an unresolved plain call
                    // with no path forward. Recognize it here instead, the
                    // same structural way `import` just above is.
                    if ident.as_str() == "type"
                        && invoke.args.len() == 1
                        && invoke.kwargs.is_empty()
                    {
                        let value = self.transform_expr_to_hir(&invoke.args[0])?;
                        return Ok(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                            kind: CallKind::TypeOf,
                            callargs: vec![hir::CallArg {
                                name: hir::Symbol::new("arg0"),
                                value,
                            }],
                        }));
                    }
                }

                let mut path =
                    self.name_to_hir_path_with_scope(name, PathResolutionScope::Value)?;
                // A function-position qualified enum constructor must carry
                // the variant's constructor identity into HIR.  The value
                // resolver may intentionally retain the type head for a
                // type-relative path; resolve the final segment in the enum
                // variant namespace before type checking sees the callee.
                if path.res.is_some() && path.segments.len() > 1 {
                    let prefix = hir::Path {
                        segments: path.segments[..path.segments.len() - 1].to_vec(),
                        res: path.res.clone(),
                    };
                    if let Some(res) = self.lookup_enum_variant(
                        &prefix,
                        path.segments
                            .last()
                            .map(|segment| segment.name.as_str())
                            .unwrap_or(""),
                    ) {
                        path.res = Some(res);
                    }
                }
                if path.res.is_none() {
                    let base_name = match name {
                        ast::Name::Path(source) if source.segments.len() > 1 => {
                            let mut base = source.clone();
                            base.segments.pop();
                            Some(ast::Name::Path(base))
                        }
                        ast::Name::ParameterPath(source) if source.segments.len() > 1 => {
                            let mut base = source.clone();
                            base.segments.pop();
                            Some(ast::Name::ParameterPath(base))
                        }
                        _ => None,
                    };
                    if let Some(base_name) = base_name {
                        path.res = self
                            .name_to_hir_path_with_scope(&base_name, PathResolutionScope::Type)?
                            .res;
                    }
                }

                // A call's callee is only ever a compiler intrinsic/portable
                // op because its *own resolved declaration* was tagged
                // `#[intrinsic = "..."]`/`#[op(func = "...")]` — e.g.
                // `catch_unwind`'s real (stub-bodied) declaration, or
                // `std::time::now`'s. But recognizing this HERE, pre-
                // typecheck, forces every match through the low-level
                // `transform_intrinsic_call_to_hir` path, which can only
                // represent a genuine `IntrinsicKind` — many `#[op(...)]`s
                // (`Vec::new`, `Iter`, `AsRef`, `OptionSome`, `ResultOk`, ...)
                // have no such equivalent and exist purely for POST-typecheck,
                // backend-specific materialization. Always lower as an
                // ordinary `Call` here; reclassification (by this same real
                // `DefId`, never by re-deriving it from the call site's own
                // name/path) happens post-typecheck instead — see
                // `hir_to_mir::expr::lower_call` (`Native`) and
                // `HirToAstLifter::try_lift_call_as_intrinsic`
                // (`Transpile`), both consulting the same
                // `program.op_defs`/`intrinsic_defs` tables via
                // `transforms::resolve_call_kind`.
                let func_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(path),
                    span: self.create_span(1),
                };
                let args =
                    self.transform_call_args_bound(&invoke.args, &invoke.kwargs, Some(&func_expr))?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let func_expr = self.transform_expr_to_hir(expr)?;
                let args = self.transform_call_args_strict(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }

            _ => Ok(self.error_placeholder_expr_kind(
                format!(
                    "unimplemented invoke target type for HIR transformation: {:?}",
                    invoke.target
                ),
                invoke.span(),
            )),
        }
    }

    /// Transform field selection to HIR
    pub(super) fn transform_select_to_hir(
        &mut self,
        select: &ast::ExprSelect,
    ) -> Result<hir::ExprKind> {
        // A `::name` select (`u8::MAX`, `Map::SOME_CONST`) — syntactically
        // identical to `.name` in this parser (both fold into `ExprSelect`;
        // see `Postfix::ConstField`'s doc comment), but semantically a
        // *path* continuation, never a runtime field access. Build it the
        // same way a call's callee/a struct literal's name already does
        // (`ast_expr_to_hir_path`), rather than always lowering to
        // `FieldAccess` — the previous unconditional `FieldAccess` here
        // left every non-call, non-struct use of `Type::CONST` (an
        // ordinary value read, not immediately called) permanently
        // unresolvable, since a plain runtime field access has no notion
        // of a type-relative base at all.
        if matches!(select.select, ast::ExprSelectType::Const) {
            let type_path = self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Type)?;
            let value_path = self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Value)?;
            let mut path = if matches!(value_path.res, Some(hir::Res::Module(_))) {
                value_path
            } else {
                type_path
            };
            let seg = self.make_path_segment(&select.field.name, None);
            path.segments.push(seg);
            // Resolve the completed path after appending the selected item.
            // The base (`ascii::Char`) may be a re-exported type alias; its
            // variant namespace belongs to the nominal enum (`AsciiChar`),
            // so resolving only the base leaves `Char::Null` without a Res.
            let full_path = QualifiedPath::new(
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str().to_string())
                    .collect(),
            );
            path.res = self
                .lookup_global_res(&full_path, PathResolutionScope::Value)
                .or(path.res);
            return Ok(hir::ExprKind::Path(path));
        }
        let expr = Box::new(self.transform_expr_to_hir(&select.obj)?);
        let field = select.field.clone().into();

        Ok(hir::ExprKind::FieldAccess(expr, field))
    }

    /// Transform struct construction to HIR
    pub(super) fn transform_struct_to_hir(
        &mut self,
        struct_expr: &ast::ExprStruct,
    ) -> Result<hir::ExprKind> {
        let path =
            self.ast_expr_to_hir_path(struct_expr.name.as_ref(), PathResolutionScope::Value)?;
        let struct_span = struct_expr.span();

        let mut explicit_names = std::collections::HashSet::new();
        let fields = struct_expr
            .fields
            .iter()
            .map(|field| {
                let expr = if let Some(value) = field.value.as_ref() {
                    self.transform_expr_to_hir(value)?
                } else {
                    // Shorthand - reference local with same name.
                    let res = self.resolve_value_symbol(&field.name.name);
                    hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Path(hir::Path {
                            segments: vec![hir::PathSegment {
                                name: field.name.clone().into(),
                                args: None,
                            }],
                            res,
                        }),
                        span: self.create_span(1),
                    }
                };

                explicit_names.insert(field.name.name.clone());
                Ok(hir::StructExprField {
                    hir_id: self.next_id(),
                    name: field.name.clone().into(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let Some(update_expr) = struct_expr.update.as_ref() else {
            return Ok(hir::ExprKind::Struct(path, fields));
        };

        // Lower `Foo { ..base, field: value }` into a block that binds `base`
        // once and then fills missing fields from it, so later MIR lowering
        // only sees a plain struct literal.
        let struct_fields = match path.res {
            Some(hir::Res::Def(ref def_id)) => {
                if let Some(fields) = self.struct_field_defs.get(&def_id).cloned() {
                    fields
                } else {
                    self.add_error(
                        Diagnostic::error(
                            "struct update requires a known struct field layout".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(struct_span),
                    );
                    return Ok(hir::ExprKind::Struct(path, fields));
                }
            }
            _ => {
                let segments = path
                    .segments
                    .iter()
                    .map(|seg| seg.name.as_str().to_string())
                    .collect::<Vec<_>>();
                let Some(alias) = self.lookup_type_alias(&segments) else {
                    self.add_error(
                        Diagnostic::error(
                            "struct update requires a resolved struct definition".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(struct_span),
                    );
                    return Ok(hir::ExprKind::Struct(path, fields));
                };
                self.struct_fields_from_type(&alias, struct_span)?
            }
        };

        let base_expr = self.transform_expr_to_hir(update_expr.as_ref())?;
        let base_name = format!("__struct_update_{}", self.next_id());
        let base_symbol = hir::Symbol::new(base_name.clone());
        let base_pat_id = self.next_id();
        let base_pat = hir::Pat {
            hir_id: base_pat_id.clone(),
            kind: hir::PatKind::Binding {
                name: base_symbol.clone(),
                mutable: false,
            },
        };
        let local = hir::Local {
            hir_id: self.next_id(),
            pat: base_pat,
            ty: None,
            init: Some(base_expr),
        };
        let local_stmt = hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(local),
        };

        let base_path = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: base_symbol,
                    args: None,
                }],
                res: Some(hir::Res::Local(base_pat_id)),
            }),
            span: self.create_span(1),
        };

        let mut merged_fields = fields;
        for field in struct_fields {
            if explicit_names.contains(field.name.name.as_str()) {
                continue;
            }
            let access = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::FieldAccess(
                    Box::new(base_path.clone()),
                    hir::Symbol::new(field.name.name.clone()),
                ),
                span: self.create_span(1),
            };
            merged_fields.push(hir::StructExprField {
                hir_id: self.next_id(),
                name: hir::Symbol::new(field.name.name.clone()),
                expr: access,
            });
        }

        let struct_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Struct(path, merged_fields),
            span: self.create_span(1),
        };
        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts: vec![local_stmt],
            expr: Some(Box::new(struct_expr)),
        }))
    }

    fn transform_range_to_hir(&mut self, range: &ast::ExprRange) -> Result<hir::ExprKind> {
        if range.step.is_some() {
            return Ok(self.error_placeholder_expr_kind(
                "range steps are not supported outside for loops and slicing".to_string(),
                range.span(),
            ));
        }

        let (name, fields) = match (&range.start, &range.end, range.limit.clone()) {
            (None, None, ast::ExprRangeLimit::Exclusive) => ("RangeFull", Vec::new()),
            (Some(start), None, ast::ExprRangeLimit::Exclusive) => (
                "RangeFrom",
                vec![("start", self.transform_expr_to_hir(start)?)],
            ),
            (None, Some(end), ast::ExprRangeLimit::Exclusive) => {
                ("RangeTo", vec![("end", self.transform_expr_to_hir(end)?)])
            }
            (None, Some(end), ast::ExprRangeLimit::Inclusive) => (
                "RangeToInclusive",
                vec![("end", self.transform_expr_to_hir(end)?)],
            ),
            (Some(start), Some(end), ast::ExprRangeLimit::Exclusive) => (
                "Range",
                vec![
                    ("start", self.transform_expr_to_hir(start)?),
                    ("end", self.transform_expr_to_hir(end)?),
                ],
            ),
            (Some(start), Some(end), ast::ExprRangeLimit::Inclusive) => (
                "RangeInclusive",
                vec![
                    ("start", self.transform_expr_to_hir(start)?),
                    ("end", self.transform_expr_to_hir(end)?),
                ],
            ),
            (None, None, ast::ExprRangeLimit::Inclusive)
            | (Some(_), None, ast::ExprRangeLimit::Inclusive) => {
                return Err(eyre::eyre!("inclusive range requires an end bound").into());
            }
        };
        let path = self.name_to_hir_path_with_scope(
            &ast::Name::path(ast::Path {
                prefix: PathPrefix::Crate,
                segments: vec![ast::Ident::new("ops"), ast::Ident::new(name)],
            }),
            PathResolutionScope::Value,
        )?;
        let fields = fields
            .into_iter()
            .map(|(name, expr)| hir::StructExprField {
                hir_id: self.next_id(),
                name: hir::Symbol::new(name),
                expr,
            })
            .collect();
        Ok(hir::ExprKind::Struct(path, fields))
    }

    /// Transform a block node to HIR without wrapping it in an expression.
    pub(super) fn transform_block_node_to_hir(
        &mut self,
        block: &ast::ExprBlock,
    ) -> Result<hir::Block> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let last_expr_index = block
                .last_expr()
                .and_then(|_| block.stmts.len().checked_sub(1));
            let stmts = block
                .stmts
                .iter()
                .enumerate()
                .filter_map(|(idx, stmt)| {
                    if Some(idx) == last_expr_index {
                        return None;
                    }
                    Some(self.transform_block_stmt_to_hir(stmt))
                })
                .collect::<Result<Vec<_>>>()?;

            // Preserve the value of the final expression without duplicating it as a statement.
            let expr = last_expr_index
                .and_then(|idx| block.stmts.get(idx))
                .and_then(|stmt| match stmt {
                    ast::BlockStmt::Expr(expr) if expr.has_value() => {
                        Some(self.transform_expr_to_hir(expr.expr.as_ref()))
                    }
                    _ => None,
                })
                .transpose()?
                .map(Box::new);

            Ok(hir::Block {
                hir_id: self.next_id(),
                stmts,
                expr,
            })
        })();
        self.pop_value_scope();
        self.pop_type_scope();
        result
    }

    /// Transform if expression to HIR
    pub(super) fn transform_if_to_hir(&mut self, if_expr: &ast::ExprIf) -> Result<hir::ExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&if_expr.cond)?);
        let then_branch = Box::new(self.transform_expr_to_hir(&if_expr.then)?);
        let else_branch = if let Some(else_expr) = if_expr.elze.as_ref() {
            Some(Box::new(self.transform_expr_to_hir(else_expr)?))
        } else {
            None
        };

        Ok(hir::ExprKind::If(cond, then_branch, else_branch))
    }

    pub(super) fn transform_match_to_hir(
        &mut self,
        match_expr: &ast::ExprMatch,
    ) -> Result<hir::ExprKind> {
        let scrutinee = match_expr
            .scrutinee
            .as_ref()
            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
            .transpose()?;

        let scrutinee = scrutinee.ok_or_else(|| {
            fp_core::error::Error::from("match expressions without scrutinee are not supported")
        })?;

        let mut arms = Vec::with_capacity(match_expr.cases.len());
        for case in &match_expr.cases {
            let pat = if let Some(pat) = case.pat.as_ref() {
                self.transform_pattern(pat.as_ref())?
            } else {
                hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Wild,
                }
            };
            self.register_pattern_bindings(&pat);

            let guard = case
                .guard
                .as_ref()
                .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                .transpose()?;
            let body = self.transform_expr_to_hir(case.body.as_ref())?;

            arms.push(hir::MatchArm {
                hir_id: self.next_id(),
                pat,
                guard,
                body,
            });
        }

        Ok(hir::ExprKind::Match(Box::new(scrutinee), arms))
    }

    /// Transform loop to HIR
    pub(super) fn transform_loop_to_hir(
        &mut self,
        loop_expr: &ast::ExprLoop,
    ) -> Result<hir::ExprKind> {
        let body_expr = self.transform_expr_to_hir(&loop_expr.body.get())?;
        let body_block = if let hir::ExprKind::Block(block) = body_expr.kind {
            block
        } else {
            // If the body is not a block, wrap it in one
            hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: Some(Box::new(body_expr)),
            }
        };

        Ok(hir::ExprKind::Loop(body_block))
    }

    /// Transform while loop to HIR
    pub(super) fn transform_while_to_hir(
        &mut self,
        while_expr: &ast::ExprWhile,
    ) -> Result<hir::ExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&while_expr.cond.get())?);
        let body_expr = self.transform_expr_to_hir(&while_expr.body.get())?;
        let body_block = if let hir::ExprKind::Block(block) = body_expr.kind {
            block
        } else {
            // If the body is not a block, wrap it in one
            hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: Some(Box::new(body_expr)),
            }
        };

        Ok(hir::ExprKind::While(cond, body_block))
    }

    pub(super) fn transform_for_to_hir(
        &mut self,
        for_expr: &ast::ExprFor,
    ) -> Result<hir::ExprKind> {
        // A target with `LanguageCapabilities::first_class_for_loops` set
        // (Kotlin, currently) has its own native `for`/`foreach` plus real
        // collection methods (`.take(n)`, `.drop(n)`, ...) — lower the loop
        // as a real, un-desugared `hir::ExprKind::For` instead of eagerly
        // decomposing it into an index-based `while` loop here. `iter` is
        // lowered as an ordinary expression (no special-case shape
        // detection needed: `list.iter().take(n)` becomes a perfectly
        // normal method-call chain; `HirToAstLifter::lift_expr`'s `For` arm
        // and the existing generic `Op(Iter)` promotion handle it from
        // there), which is what avoids the whole class of "unrecognized
        // surface shape" bugs the index-loop extraction below is prone to
        // (see `extract_iter_loop_spec`'s doc comments for the specific
        // bugs this replaced). Every other pipeline (in particular
        // `PipelineMode::Native`, whose MIR has no iterator-protocol
        // concept) still falls through to the unchanged desugaring below.
        if self.lowering_config.capabilities.first_class_for_loops {
            let (pat, _ty, _) = self.transform_pattern_with_metadata(&for_expr.pat)?;
            self.register_pattern_bindings(&pat);
            let iter = self.transform_expr_to_hir(&for_expr.iter)?;
            let hir::ExprKind::Block(body) = self.transform_expr_to_hir(&for_expr.body)?.kind
            else {
                unreachable!("for-loop body is always a block expression")
            };
            return Ok(hir::ExprKind::For(Box::new(pat), Box::new(iter), body));
        }

        let mut stmts = Vec::new();

        if !matches!(for_expr.iter.kind(), ast::ExprKind::Range(_)) {
            if let Some(enum_spec) = self.extract_enumerate_loop_spec(for_expr)? {
                return self.lower_enumerate_for_loop(for_expr, enum_spec);
            }
            if let Some(iter_spec) = self.extract_iter_loop_spec(for_expr)? {
                return self.lower_iter_for_loop(for_expr, iter_spec);
            }
            if for_expr.pat.as_ident().is_some() {
                // Last resort: an arbitrary expression that isn't a range,
                // `.iter()`, or `.enumerate()` call but is nonetheless
                // Vec/slice-typed (e.g. `type(source).fields`, a
                // reflection intrinsic result) — index over it directly
                // rather than hard-erroring just because it lacks an
                // explicit `.iter()` suffix.
                return self.lower_bare_iter_for_loop(for_expr);
            }
            return Ok(self.error_placeholder_expr_kind(
                "`for` loop lowering only supports range iterators, iter(), and enumerate()"
                    .to_string(),
                for_expr.span(),
            ));
        }

        let (mut pat, _ty, _) = self.transform_pattern_with_metadata(&for_expr.pat)?;
        let (loop_name, loop_res) = match &mut pat.kind {
            hir::PatKind::Binding { name, .. } => {
                (name.clone(), Some(hir::Res::Local(pat.hir_id.clone())))
            }
            _ => {
                return Ok(self.error_placeholder_expr_kind(
                    "`for` loop pattern must be a simple binding".to_string(),
                    for_expr.span(),
                ));
            }
        };
        if let hir::PatKind::Binding { mutable, .. } = &mut pat.kind {
            *mutable = true;
        }

        let (start_expr, end_expr, step_expr, inclusive) = match for_expr.iter.kind() {
            ast::ExprKind::Range(range) => {
                let start = range
                    .start
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let end = range
                    .end
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let step = range
                    .step
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let inclusive = matches!(range.limit, ast::ExprRangeLimit::Inclusive);
                (start, end, step, inclusive)
            }
            _ => {
                return Ok(self.error_placeholder_expr_kind(
                    "`for` loop lowering currently only supports range iterators".to_string(),
                    for_expr.span(),
                ));
            }
        };

        let init_expr = start_expr.unwrap_or_else(|| hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
            span: Span::new(self.current_file, 0, 0),
        });

        let local = hir::Local {
            hir_id: self.next_id(),
            pat: pat.clone(),
            ty: None,
            init: Some(init_expr),
        };
        self.register_pattern_bindings(&local.pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(local),
        });

        let loop_var = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: loop_name.clone(),
                    args: None,
                }],
                res: loop_res,
            }),
            span: Span::new(self.current_file, 0, 0),
        };

        let end_expr = match end_expr {
            Some(expr) => expr,
            None => {
                self.add_error(
                    Diagnostic::error("`for` loop range missing end expression".to_string())
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(for_expr.span()),
                );
                hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
                    span: Span::new(self.current_file, 0, 0),
                }
            }
        };

        let cmp_op = if inclusive {
            hir::BinOp::Le
        } else {
            hir::BinOp::Lt
        };
        let cond_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Binary(cmp_op, Box::new(loop_var.clone()), Box::new(end_expr)),
            span: Span::new(self.current_file, 0, 0),
        };

        let step_expr = step_expr.unwrap_or_else(|| hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
            span: Span::new(self.current_file, 0, 0),
        });
        let increment = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Assign(
                Box::new(loop_var.clone()),
                Box::new(hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Binary(
                        hir::BinOp::Add,
                        Box::new(loop_var.clone()),
                        Box::new(step_expr),
                    ),
                    span: Span::new(self.current_file, 0, 0),
                }),
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let body_expr = self.transform_expr_to_hir(for_expr.body.as_ref())?;
        let mut body_stmts = Vec::new();
        if let hir::ExprKind::Block(block) = &body_expr.kind {
            body_stmts.extend(block.stmts.clone());
            if let Some(expr) = &block.expr {
                body_stmts.push(hir::Stmt {
                    hir_id: self.next_id(),
                    kind: hir::StmtKind::Semi(*expr.clone()),
                });
            }
        } else {
            body_stmts.push(hir::Stmt {
                hir_id: self.next_id(),
                kind: hir::StmtKind::Semi(body_expr),
            });
        }

        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Semi(increment),
        });

        let while_block = hir::Block {
            hir_id: self.next_id(),
            stmts: body_stmts,
            expr: None,
        };

        let while_expr = hir::ExprKind::While(Box::new(cond_expr), while_block);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Expr(hir::Expr {
                hir_id: self.next_id(),
                kind: while_expr,
                span: Span::new(self.current_file, 0, 0),
            }),
        });

        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts,
            expr: None,
        }))
    }

    fn path_segments_from_expr(&self, expr: &ast::Expr) -> Option<Vec<ast::Ident>> {
        match expr.kind() {
            ast::ExprKind::Name(name) => match name {
                ast::Name::Path(path) => Some(path.segments.clone()),
                ast::Name::Ident(ident) => Some(vec![ident.clone()]),
                ast::Name::ParameterPath(path) => {
                    Some(path.segments.iter().map(|seg| seg.ident.clone()).collect())
                }
            },
            ast::ExprKind::Invoke(invoke) => {
                // Permit no-arg method chains like `xs.iter().enumerate()` to be treated as a path.
                // This is used by enumerate() lowering to recover the base path segments.
                if !invoke.args.is_empty() {
                    return None;
                }
                match &invoke.target {
                    ast::ExprInvokeTarget::Function(name) => match name {
                        ast::Name::Path(path) => Some(path.segments.clone()),
                        ast::Name::Ident(ident) => Some(vec![ident.clone()]),
                        ast::Name::ParameterPath(path) => {
                            Some(path.segments.iter().map(|seg| seg.ident.clone()).collect())
                        }
                    },
                    ast::ExprInvokeTarget::Method(select) => {
                        let mut base = self.path_segments_from_expr(&select.obj)?;
                        base.push(select.field.clone());
                        Some(base)
                    }
                    ast::ExprInvokeTarget::Expr(expr) => self.path_segments_from_expr(expr),
                    _ => None,
                }
            }
            ast::ExprKind::Select(select) => {
                let mut base = self.path_segments_from_expr(&select.obj)?;
                base.push(select.field.clone());
                Some(base)
            }
            _ => None,
        }
    }

    /// Transform assignment to HIR
    pub(super) fn transform_assign_to_hir(
        &mut self,
        assign: &ast::ExprAssign,
    ) -> Result<hir::ExprKind> {
        let lhs = Box::new(self.transform_expr_to_hir(&assign.target)?);
        let rhs = Box::new(self.transform_expr_to_hir(&assign.value)?);

        Ok(hir::ExprKind::Assign(lhs, rhs))
    }

    /// Transform block statement to HIR (using actual AST types)
    pub(super) fn transform_block_stmt_to_hir(
        &mut self,
        stmt: &ast::BlockStmt,
    ) -> Result<hir::Stmt> {
        let kind = match stmt {
            ast::BlockStmt::Expr(expr_stmt) => {
                if let ast::ExprKind::Let(let_expr) = expr_stmt.expr.kind() {
                    let (pat, explicit_ty, _) =
                        self.transform_pattern_with_metadata(&let_expr.pat)?;
                    self.register_pattern_bindings(&pat);
                    let init = self.transform_expr_to_hir(&let_expr.expr)?;
                    let local = hir::Local {
                        hir_id: self.next_id(),
                        pat,
                        ty: explicit_ty,
                        init: Some(init),
                    };
                    hir::StmtKind::Local(local)
                } else {
                    let expr = self.transform_expr_to_hir(&expr_stmt.expr)?;
                    if expr_stmt.has_value() {
                        hir::StmtKind::Expr(expr)
                    } else {
                        hir::StmtKind::Semi(expr)
                    }
                }
            }
            ast::BlockStmt::Let(let_stmt) => {
                let (pat, explicit_ty, _) = self.transform_pattern_with_metadata(&let_stmt.pat)?;
                let init = let_stmt
                    .init
                    .as_ref()
                    .map(|v| self.transform_expr_to_hir(v))
                    .transpose()?;

                let local = hir::Local {
                    hir_id: self.next_id(),
                    pat,
                    ty: explicit_ty,
                    init,
                };

                self.register_pattern_bindings(&local.pat);

                hir::StmtKind::Local(local)
            }
            ast::BlockStmt::Item(item) => {
                // Transform items (struct definitions, const declarations, etc.)
                self.transform_item_to_hir_stmt(item)?
            }
            _ => {
                let stmt_span = self.create_span(1);
                let placeholder = self.error_placeholder_expr_kind(
                    format!(
                        "unimplemented block statement type for HIR transformation: {:?}",
                        stmt
                    ),
                    stmt_span,
                );
                hir::StmtKind::Semi(hir::Expr {
                    hir_id: self.next_id(),
                    kind: placeholder,
                    span: stmt_span,
                })
            }
        };

        Ok(hir::Stmt {
            hir_id: self.next_id(),
            kind,
        })
    }

    /// Convert AST binary operator to HIR
    pub(super) fn convert_binop_kind(&self, op: &BinOpKind) -> hir::BinOp {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => hir::BinOp::Add,
            BinOpKind::Sub => hir::BinOp::Sub,
            BinOpKind::Mul => hir::BinOp::Mul,
            BinOpKind::Div => hir::BinOp::Div,
            BinOpKind::Mod => hir::BinOp::Rem,
            BinOpKind::Shl => hir::BinOp::Shl,
            BinOpKind::Shr => hir::BinOp::Shr,
            BinOpKind::Eq => hir::BinOp::Eq,
            BinOpKind::Ne => hir::BinOp::Ne,
            BinOpKind::Lt => hir::BinOp::Lt,
            BinOpKind::Le => hir::BinOp::Le,
            BinOpKind::Gt => hir::BinOp::Gt,
            BinOpKind::Ge => hir::BinOp::Ge,
            BinOpKind::And => hir::BinOp::And,
            BinOpKind::Or => hir::BinOp::Or,
            BinOpKind::BitOr => hir::BinOp::BitOr,
            BinOpKind::BitAnd => hir::BinOp::BitAnd,
            BinOpKind::BitXor => hir::BinOp::BitXor,
        }
    }

    /// Convert AST unary operator to HIR
    pub(super) fn convert_unop_kind(&mut self, op: &UnOpKind, span: Span) -> Result<hir::UnOp> {
        match op {
            UnOpKind::Neg => Ok(hir::UnOp::Neg),
            UnOpKind::Not => Ok(hir::UnOp::Not),
            UnOpKind::Deref => Ok(hir::UnOp::Deref),
            UnOpKind::Any(kind) => {
                if kind.as_str() == "box" {
                    return Ok(hir::UnOp::Box);
                }
                self.add_error(
                    Diagnostic::error(format!(
                        "Unsupported unary operator variant encountered during AST→HIR lowering: {:?}",
                        kind
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(hir::UnOp::Not)
            }
        }
    }

    /// Transform parentheses expression to HIR (just unwrap the inner expression)
    pub(super) fn transform_paren_to_hir(
        &mut self,
        paren: &ast::ExprParen,
    ) -> Result<hir::ExprKind> {
        // Parentheses don't change semantics, just unwrap the inner expression
        let inner_expr = self.transform_expr_to_hir(&paren.expr)?;
        Ok(inner_expr.kind)
    }

    /// Transform format string to HIR - keep it as FormatString for later const evaluation
    pub(super) fn transform_format_string_to_hir(
        &mut self,
        format_str: &ast::ExprStringTemplate,
    ) -> Result<hir::ExprKind> {
        let parts = format_str
            .parts
            .iter()
            .map(|part| match part {
                ast::FormatTemplatePart::Literal(text) => {
                    hir::FormatTemplatePart::Literal(text.clone())
                }
                ast::FormatTemplatePart::Placeholder(ph) => {
                    let arg_ref = match &ph.arg_ref {
                        ast::FormatArgRef::Implicit => hir::FormatArgRef::Implicit,
                        ast::FormatArgRef::Positional(idx) => hir::FormatArgRef::Positional(*idx),
                        ast::FormatArgRef::Named(name) => hir::FormatArgRef::Named(name.clone()),
                    };
                    hir::FormatTemplatePart::Placeholder(hir::FormatPlaceholder {
                        arg_ref,
                        format_spec: ph.format_spec.clone(),
                    })
                }
            })
            .collect();

        Ok(hir::ExprKind::FormatString(hir::FormatString { parts }))
    }

    pub(super) fn transform_intrinsic_call_to_hir(
        &mut self,
        call: &ast::ExprIntrinsicCall,
    ) -> Result<hir::ExprKind> {
        let mut callargs = Vec::with_capacity(call.args.len() + call.kwargs.len());
        for (index, arg) in call.args.iter().enumerate() {
            callargs.push(hir::CallArg {
                name: hir::Symbol::new(format!("arg{}", index)),
                value: self.transform_expr_to_hir(arg)?,
            });
        }
        for kwarg in &call.kwargs {
            callargs.push(hir::CallArg {
                name: kwarg.name.clone().into(),
                value: self.transform_expr_to_hir(&kwarg.value)?,
            });
        }

        // `len` and `slice` are language operations, not compiler
        // intrinsics. Lower them to their ordinary HIR forms here so
        // typeck resolves the receiver and the backend sees the same
        // operations as source-written `.len()` and range slicing. Keeping
        // these as IntrinsicCall nodes would make every type checker and
        // backend carry a second, weaker type rule for operations that the
        // language already defines.
        match call.kind {
            CallKind::Len => {
                let [receiver] = callargs.as_slice() else {
                    return Err(fp_core::error::Error::from(
                        "len expects exactly one argument",
                    ));
                };
                return Ok(hir::ExprKind::MethodCall(
                    Box::new(receiver.value.clone()),
                    hir::Symbol::new("len"),
                    None,
                    Vec::new(),
                ));
            }
            CallKind::Slice => {
                let [base, start, end] = callargs.as_slice() else {
                    return Err(fp_core::error::Error::from(
                        "slice expects base, start, and end arguments",
                    ));
                };
                return Ok(hir::ExprKind::Slice(hir::SliceExpr {
                    hir_id: self.next_id(),
                    base: Box::new(base.value.clone()),
                    start: Some(Box::new(start.value.clone())),
                    end: Some(Box::new(end.value.clone())),
                    inclusive: false,
                }));
            }
            _ => {}
        }

        if matches!(
            call.kind,
            CallKind::Print | CallKind::Println | CallKind::Format
        ) {
            let mut existing = callargs
                .iter()
                .map(|arg| arg.name.as_str().to_string())
                .collect::<std::collections::HashSet<_>>();

            let captured_names = callargs
                .first()
                .and_then(|first| match &first.value.kind {
                    hir::ExprKind::FormatString(template) => Some(
                        template
                            .parts
                            .iter()
                            .filter_map(|part| match part {
                                hir::FormatTemplatePart::Placeholder(placeholder) => {
                                    match &placeholder.arg_ref {
                                        hir::FormatArgRef::Named(name) => {
                                            Some(name.as_str().to_string())
                                        }
                                        _ => None,
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>(),
                    ),
                    _ => None,
                })
                .unwrap_or_default();

            for name in captured_names {
                if existing.contains(&name) {
                    continue;
                }

                let path = self.name_to_hir_path_with_scope(
                    &Name::Ident(ast::Ident::new(name.as_str())),
                    PathResolutionScope::Value,
                )?;
                let value = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(path),
                    span: self.create_span(1),
                };

                callargs.push(hir::CallArg {
                    name: hir::Symbol::new(name.clone()),
                    value,
                });
                existing.insert(name);
            }
        }

        Ok(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
            kind: call.kind.clone(),
            callargs,
        }))
    }

    pub(super) fn transform_call_args_bound(
        &mut self,
        args: &[ast::Expr],
        kwargs: &[ast::ExprKwArg],
        callee: Option<&hir::Expr>,
    ) -> Result<Vec<hir::CallArg>> {
        let mut values = Vec::with_capacity(args.len() + kwargs.len());
        for arg in args {
            values.push((
                hir::Symbol::new(format!("arg{}", values.len())),
                self.transform_expr_to_hir(arg)?,
            ));
        }
        for kwarg in kwargs {
            values.push((
                kwarg.name.clone().into(),
                self.transform_expr_to_hir(&kwarg.value)?,
            ));
        }
        let Some((param_names, is_variadic)) = callee
            .and_then(|expr| match &expr.kind {
                hir::ExprKind::Path(path) => path.res.as_ref(),
                _ => None,
            })
            .and_then(|res| match res {
                hir::Res::Def(def_id) => Some(def_id.clone()),
                _ => None,
            })
            .and_then(|def_id| self.program_def_param_info(def_id))
        else {
            return Ok(values
                .into_iter()
                .map(|(name, value)| hir::CallArg { name, value })
                .collect());
        };

        if values.len() != param_names.len() {
            if is_variadic {
                let required = param_names.len().saturating_sub(1);
                if values.len() >= required {
                    return Ok(values
                        .into_iter()
                        .map(|(name, value)| hir::CallArg { name, value })
                        .collect());
                }
            }
            let span = args
                .first()
                .map(|arg| arg.span())
                .unwrap_or_else(Span::null);
            self.add_error(
                Diagnostic::error(
                    "call arguments do not match function parameter count".to_string(),
                )
                .with_source_context(DIAGNOSTIC_CONTEXT)
                .with_span(span),
            );
            return Ok(values
                .into_iter()
                .map(|(name, value)| hir::CallArg { name, value })
                .collect());
        }

        let mut ordered = vec![None; param_names.len()];
        for (index, (name, value)) in values.into_iter().enumerate() {
            let target = name
                .as_str()
                .strip_prefix("arg")
                .and_then(|index| index.parse::<usize>().ok())
                .filter(|target| *target == index)
                .or_else(|| {
                    param_names
                        .iter()
                        .position(|param| param.as_str() == name.as_str())
                });

            let Some(target) = target else {
                self.add_error(
                    Diagnostic::error(format!("unknown named argument `{name}` in call"))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(
                            args.first()
                                .map(|arg| arg.span())
                                .unwrap_or_else(Span::null),
                        ),
                );
                return Ok(Vec::new());
            };
            if ordered[target].is_some() {
                self.add_error(
                    Diagnostic::error(format!("duplicate argument `{name}` in call"))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(
                            args.first()
                                .map(|arg| arg.span())
                                .unwrap_or_else(Span::null),
                        ),
                );
                return Ok(Vec::new());
            }
            ordered[target] = Some(value);
        }

        Ok(ordered
            .into_iter()
            .enumerate()
            .map(|(index, value)| hir::CallArg {
                name: param_names[index].clone(),
                value: value.expect("argument count was checked against the function signature"),
            })
            .collect())
    }

    pub(super) fn transform_call_args_strict(
        &mut self,
        args: &[ast::Expr],
    ) -> Result<Vec<hir::CallArg>> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.transform_expr_to_hir(arg)?);
        }
        Ok(values
            .into_iter()
            .enumerate()
            .map(|(index, value)| hir::CallArg {
                name: hir::Symbol::new(format!("arg{}", index)),
                value,
            })
            .collect())
    }

    pub(super) fn program_def_param_info(
        &self,
        def_id: hir::DefId,
    ) -> Option<(Vec<hir::Symbol>, bool)> {
        let item = self
            .program_def_map
            .get(&def_id)
            .cloned()
            .or_else(|| self.hir_program.item(def_id.clone()));
        let Some(item) = item else {
            return None;
        };
        match &item.kind {
            hir::ItemKind::Function(function) => {
                let mut names = Vec::with_capacity(function.sig.inputs.len());
                for param in &function.sig.inputs {
                    match &param.pat.kind {
                        hir::PatKind::Binding { name, .. } => names.push(name.clone()),
                        _ => return None,
                    }
                }
                let is_variadic = function
                    .sig
                    .inputs
                    .last()
                    .map(|param| matches!(param.ty.kind, hir::TypeExprKind::Infer))
                    .unwrap_or(false);
                Some((names, is_variadic))
            }
            _ => None,
        }
    }

    // name_to_hir_path_with_scope moved to helpers.rs

    // ast_expr_to_hir_path moved to helpers.rs

    // convert_generic_args moved to helpers.rs

    // canonicalize_segments moved to helpers.rs

    pub(super) fn lookup_global_res(
        &self,
        path: &fp_core::ast::path::QualifiedPath,
        scope: PathResolutionScope,
    ) -> Option<hir::Res> {
        if path.segments.is_empty() {
            return None;
        }
        let key = path.to_key();

        // A variant is looked up in the namespace of the nominal enum behind
        // a transparent type alias (`ascii::Char::Null`, for example), not
        // as a child of the alias's module-tree binding. Keep this before the
        // ordinary value lookup so the alias remains transparent without
        // creating duplicate declarations or typechecker exceptions.
        if scope == PathResolutionScope::Value && path.segments.len() > 1 {
            let prefix = QualifiedPath::new(
                path.segments[..path.segments.len() - 1]
                    .iter()
                    .cloned()
                    .collect(),
            );
            if let Some(res) =
                self.enum_variant_through_type_path(&prefix, path.segments.last()?.as_str())
            {
                return Some(res);
            }
        }

        let local = self.lookup_symbol(&key, scope.namespace());
        // A cross-package export (e.g. `libc::macos::getenv`) is looked up
        // lazily against the workspace on a local-lookup miss, instead of
        // being eagerly copied into the module tree's own bindings up
        // front. The exported binding stays in its owning package.
        local
            .or_else(|| self.hir_program.find_export(&key))
            .or_else(|| {
                if scope == PathResolutionScope::Value && path.segments.len() > 1 {
                    self.lookup_symbol(&key, hir::Namespace::Type)
                } else {
                    None
                }
            })
    }

    // make_path_segment moved to helpers.rs

    pub(super) fn primitive_type_to_hir(&mut self, prim: ast::TypePrimitive) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Primitive(prim),
            Span::new(self.current_file, 0, 0),
        )
    }

    // transform_pattern_with_metadata moved to patterns.rs

    // transform_pattern moved to patterns.rs

    // register_pattern_bindings moved to patterns.rs

    /// Transform let expression to HIR
    pub(super) fn transform_let_to_hir(
        &mut self,
        let_expr: &ast::ExprLet,
    ) -> Result<hir::ExprKind> {
        let (pat, explicit_ty, _) = self.transform_pattern_with_metadata(&let_expr.pat)?;
        self.register_pattern_bindings(&pat);
        let init = self.transform_expr_to_hir(&let_expr.expr)?;
        let ty = explicit_ty.unwrap_or_else(|| self.create_unit_type());

        Ok(hir::ExprKind::Let(pat, Box::new(ty), Some(Box::new(init))))
    }
}
