use crate::*;
use fp_core::hir::*;
use fp_core::error::Result;
use fp_core::module::path::*;
use fp_core::span::Span;

impl HirTypeInferencer {
    pub(crate) fn infer_item_inner<'a>(&self, item: &'a mut Item) -> BoxFuture<'a, Result<()>> {
        let this = self.clone();
        Box::pin(async move {
            let span = item.span();
            let previous = this.inner.borrow().current_span;
            let active = this.span_or_previous(span, previous);
            this.inner.borrow_mut().current_span = active;
            let result = this.infer_item_inner_body(item).await;
            this.inner.borrow_mut().current_span = previous;
            result.map_err(|err| this.error_with_span(err, active))
        })
    }

    /// Split out of `infer_item_inner` so the span save/restore around it
    /// (which must run even on error) doesn't itself need to live inside a
    /// plain (sync) closure -- a sync closure can't contain `.await`, so
    /// this replaces the old IIFE-closure trick.
    pub(crate) fn infer_item_inner_body<'a>(
        &self,
        item: &'a mut Item,
    ) -> BoxFuture<'a, Result<()>> {
        let this = self.clone();
        Box::pin(async move {
            let ty = match item.kind_mut() {
                ItemKind::DefStruct(def) => {
                    this.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    this.insert_struct_def(&def.name, def.value.clone());
                    let ty = Ty::Struct(def.value.clone());
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder)
                        .await?;
                    ty
                }
                ItemKind::DefStructural(def) => {
                    this.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    let struct_ty = TypeStruct {
                        name: def.name.clone(),
                        generics_params: Vec::new(),
                        repr: ReprOptions::default(),
                        fields: def.value.fields.clone(),
                    };
                    this.insert_struct_def(&def.name, struct_ty.clone());
                    let ty = Ty::Struct(struct_ty);
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder)
                        .await?;
                    ty
                }
                ItemKind::DefType(def) => {
                    // Resolve the RHS to a concrete type; if it is structural, materialize it as a
                    // named struct so that later term-level syntax like `Foo { ... }` can type-check.
                    let placeholder = this.symbol_var(&def.name).await;

                    // Fast path: a const-block type alias already resolved to
                    // a concrete struct by comptime evaluation in a prior
                    // pass — use it directly instead of re-deriving it via
                    // structural inference alone, which can't determine the
                    // shape of a conditionally-built type (e.g. a builder
                    // chain inside `if`). Mirrors `DefConst`'s
                    // `resolved_consts` fast path (see below in this match).
                    let cached = if matches!(&def.value, Ty::ConstBlock(_)) {
                        this.typing_ctx
                            .resolved_types
                            .borrow()
                            .get(def.name.as_str())
                            .cloned()
                    } else {
                        None
                    };

                    let normalized = if let Some(struct_def) = cached {
                        this.insert_struct_def(&def.name, struct_def.clone());
                        Ty::Struct(struct_def)
                    } else if let Ty::ConstBlock(ref mut block) = def.value {
                        // Type the block body first (structural inference
                        // alone — it doesn't need the comptime result), then
                        // try to resolve its compile-time value now: the hook
                        // needs a concretely-typed expression to lower.
                        // Structural inference tolerates unresolved names by
                        // binding them to an error type rather than
                        // hard-failing, so its result is discarded here.
                        let _ = this.infer_expr_inner(block.expr.as_mut()).await;

                        let expr_id = this.expr_id(&block.expr);
                        let key = format!("__fp_expr_{expr_id}");
                        this.best_effort_resolve_comptime(&key, &block.expr).await;

                        let resolved_struct = this
                            .typing_ctx
                            .resolved_types
                            .borrow()
                            .get(def.name.as_str())
                            .cloned();
                        match resolved_struct {
                            Some(struct_def) => {
                                this.insert_struct_def(&def.name, struct_def.clone());
                                Ty::Struct(struct_def)
                            }
                            None => {
                                // The hook resolved *something* but it
                                // wasn't a struct under this name — a
                                // real error, not a silent placeholder.
                                this.emit_error(format!(
                                    "`type {} = const {{ ... }}` did not resolve to a struct type",
                                    def.name
                                ));
                                Ty::Unknown(TypeUnknown)
                            }
                        }
                    } else {
                        let value_var = this.type_from_ast_ty(&def.value).await?;
                        let resolved = this.resolve_to_ty(value_var).await?;
                        this.normalize_deftype_value(&def.name, resolved).await
                    };

                    let var = this.type_from_ast_ty(&normalized).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder)
                        .await?;
                    normalized
                }
                ItemKind::DefEnum(def) => {
                    this.enter_scope();
                    if !def.value.generics_params.is_empty() {
                        for param in &def.value.generics_params {
                            let var = this.register_generic_param(param.name.as_str());
                            let bounds = Self::extract_trait_bounds(&param.bounds);
                            if !bounds.is_empty() {
                                this.inner
                                    .borrow_mut()
                                    .generic_trait_bounds
                                    .insert(var, bounds);
                            }
                        }
                    }

                    this.insert_enum_def(&def.name, def.value.clone());
                    let ty = Ty::Enum(def.value.clone());
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder)
                        .await?;

                    let enum_name = this
                        .qualified_name(def.name.as_str())
                        .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                    // Extracted to an owned local before the `if let`: its
                    // body awaits repeatedly, and matching directly on
                    // `this.inner.borrow()...` would extend the guard's
                    // scope across those `.await`s.
                    let variant_keys_opt =
                        this.inner.borrow().enum_variants.get(&enum_name).cloned();
                    if let Some(variant_keys) = variant_keys_opt {
                        let enum_var = placeholder;
                        for (variant, qualified) in
                            def.value.variants.iter().zip(variant_keys.into_iter())
                        {
                            if let Some(variant_var) =
                                this.lookup_env_var(&qualified.to_key()).await
                            {
                                let variant_type_var = if matches!(variant.value, Ty::Unit(_)) {
                                    enum_var
                                } else if let Ty::Tuple(tuple) = &variant.value {
                                    let mut param_vars = Vec::new();
                                    for elem in &tuple.types {
                                        param_vars.push(this.type_from_ast_ty(elem).await?);
                                    }
                                    let fn_var = this.fresh_type_var();
                                    this.bind_function_term(fn_var, param_vars, enum_var);
                                    fn_var
                                } else {
                                    let payload_var = this.type_from_ast_ty(&variant.value).await?;
                                    let fn_var = this.fresh_type_var();
                                    this.bind_function_term(fn_var, vec![payload_var], enum_var);
                                    fn_var
                                };
                                let _ = this.unify(variant_var, variant_type_var).await;
                                let _ = this
                                    .generalize_symbol(&qualified.to_key(), variant_var)
                                    .await;
                            }
                        }
                    }

                    this.exit_scope();

                    ty
                }
                ItemKind::DefConst(def) => {
                    let name = def.name.as_str().to_string();
                    let resolved = this.typing_ctx.resolved_consts.borrow().get(&name).cloned();
                    if let Some(resolved) = resolved {
                        // Already evaluated in a prior pass — bind the
                        // symbol directly and skip comptime re-request. Keep
                        // its declared type: `Value::List` cannot retain an
                        // array's length on its own.
                        let placeholder = this.symbol_var(&def.name).await;
                        let ty = def
                            .ty
                            .clone()
                            .or_else(|| def.ty_annotation.clone())
                            .unwrap_or_else(|| crate::runtime::type_from_value(&resolved));
                        let ty_var = this.type_from_ast_ty(&ty).await?;
                        this.unify(placeholder, ty_var).await?;
                        def.ty_annotation = Some(ty.clone());
                        def.ty.get_or_insert(ty.clone());
                        this.generalize_symbol(def.name.as_str(), placeholder)
                            .await?;
                        ty
                    } else {
                        let placeholder = this.symbol_var(&def.name).await;
                        if let Some(annot) = def.ty.as_ref() {
                            def.value.set_ty(annot.clone());
                        }
                        // Type the value first (structural inference alone —
                        // it doesn't need the comptime result), *then* try to
                        // resolve its compile-time value: the hook needs a
                        // concretely-typed expression to lower.
                        let expr_var = {
                            let mut value = def.value.as_mut();
                            this.infer_expr_inner(&mut value).await?
                        };

                        if let Some(annot) = &def.ty {
                            let annot_var = this.type_from_ast_ty(annot).await?;
                            this.unify(expr_var, annot_var).await?;
                        }

                        this.unify(placeholder, expr_var).await?;
                        let ty = this.resolve_to_ty(expr_var).await?;
                        def.ty_annotation = Some(ty.clone());
                        def.ty.get_or_insert(ty.clone());
                        this.generalize_symbol(def.name.as_str(), placeholder)
                            .await?;

                        // If the value is itself a `const { ... }` block, it
                        // may already have resolved via its own hook call
                        // (recorded in `expr_resolutions`, keyed by that
                        // block's own expr id) earlier in this same pass —
                        // in which case there's nothing left to request, only
                        // to copy over under this item's name.
                        let already_resolved_inner = this
                            .typing_ctx
                            .expr_resolutions
                            .borrow()
                            .resolved_value(this.expr_id(&def.value))
                            .cloned();
                        if let Some(value) = already_resolved_inner {
                            this.typing_ctx
                                .resolved_consts
                                .borrow_mut()
                                .insert(name.clone(), value);
                            this.typing_ctx.wake_comptime(&name);
                        } else {
                            this.best_effort_resolve_comptime(&name, &def.value).await;
                        }
                        ty
                    }
                }
                ItemKind::DefStatic(def) => {
                    let placeholder = this.symbol_var(&def.name).await;
                    let expr_var = {
                        let mut value = def.value.as_mut();
                        this.infer_expr_inner(&mut value).await?
                    };
                    let ty_var = this.type_from_ast_ty(&def.ty).await?;
                    this.unify(expr_var, ty_var).await?;
                    this.unify(placeholder, expr_var).await?;
                    let ty = this.resolve_to_ty(expr_var).await?;
                    def.ty_annotation = Some(ty.clone());
                    this.generalize_symbol(def.name.as_str(), placeholder)
                        .await?;
                    ty
                }
                ItemKind::DefFunction(func) => this.infer_function(func).await?,
                ItemKind::DeclConst(decl) => {
                    // An external const declaration has no body to evaluate
                    // here at all -- nothing to await.
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclStatic(decl) => {
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclType(decl) => {
                    let ty = Ty::TypeBounds(decl.bounds.clone());
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclFunction(decl) => {
                    this.validate_extern_c_signature(&decl.sig);
                    let ty = this.ty_from_function_signature(&decl.sig)?;
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::Module(module) => {
                    this.push_module_path(module.name.as_str());
                    this.enter_scope();
                    // Read `env.len()` before taking the
                    // `module_scope_depths` write borrow -- see the same
                    // pattern in `predeclare_item`'s `Module` arm.
                    let env_len = this.inner.borrow().env.len();
                    this.inner
                        .borrow_mut()
                        .module_scope_depths
                        .push(env_len.saturating_sub(1));
                    this.predeclare_scope_items(&module.collected_items).await;
                    for child in &mut module.items {
                        this.infer_item_inner(child).await?;
                    }
                    this.exit_scope();
                    this.inner.borrow_mut().module_scope_depths.pop();
                    this.pop_module_path();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Import(import) => {
                    this.register_import_aliases(import).await;
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Macro(_) => {
                    if this.inner.borrow().lossy_mode {
                        Ty::Unit(TypeUnit)
                    } else {
                        this.emit_error("macro items are not yet supported");
                        Ty::Unknown(TypeUnknown)
                    }
                }
                ItemKind::DefTrait(trait_def) => {
                    let trait_name = trait_def.name.as_str().to_string();
                    this.enter_scope();
                    this.predeclare_scope_items(&trait_def.collected_items)
                        .await;

                    // Provide `Self` inside trait methods as a generic parameter
                    // bounded by the trait itself.
                    let self_var = this.register_generic_param("Self");
                    this.inner
                        .borrow_mut()
                        .generic_trait_bounds
                        .insert(self_var, vec![trait_name.clone()]);

                    for member in &mut trait_def.items {
                        match member.kind_mut() {
                            ItemKind::DeclFunction(decl) => {
                                let ty = this.ty_from_function_signature(&decl.sig)?;
                                decl.ty_annotation = Some(ty);
                            }
                            ItemKind::DefFunction(func) => {
                                this.infer_trait_method(func).await?;
                            }
                            _ => {}
                        }
                    }

                    this.exit_scope();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Impl(impl_block) => {
                    let ctx = this
                        .resolve_impl_context(&impl_block.self_ty, &impl_block.generics_params)
                        .await;

                    if let (Some(ctx), Some(trait_ty)) =
                        (ctx.as_ref(), impl_block.trait_ty.as_ref())
                    {
                        let trait_name = trait_ty.to_string();
                        this.inner
                            .borrow_mut()
                            .impl_traits
                            .entry(ctx.struct_name.clone())
                            .or_default()
                            .insert(trait_name.clone());

                        // No `.await` anywhere in this `if let`'s body, so a
                        // borrow taken directly as its scrutinee (and
                        // extended by Rust across the whole body) is fine.
                        if let Some(methods) = this
                            .inner
                            .borrow()
                            .trait_method_sigs
                            .get(&trait_name)
                            .cloned()
                        {
                            for (method_name, sig) in methods {
                                if sig.receiver.is_none() {
                                    continue;
                                }
                                // Ensure default trait methods are callable as inherent methods
                                // on this concrete receiver type.
                                for candidate in this.struct_name_variants_for_path(
                                    &ctx.struct_name,
                                    ctx.struct_name.segments.len() == 1,
                                ) {
                                    let mut method_sigs = this.own_method_sigs_mut();
                                    let entry = method_sigs.entry(candidate).or_default();
                                    if entry.iter().any(|(n, _)| n == &method_name) {
                                        continue;
                                    }
                                    entry.push((
                                        method_name.clone(),
                                        MethodSignature {
                                            sig: sig.clone(),
                                            impl_generics_params: ctx.impl_generics_params.clone(),
                                            self_ty: ctx.self_ty.clone(),
                                        },
                                    ));
                                }
                            }
                        }
                    }

                    this.inner.borrow_mut().impl_stack.push(ctx.clone());
                    this.enter_scope();
                    for param in &impl_block.generics_params {
                        let var = this.register_generic_param(param.name.as_str());
                        let bounds = Self::extract_trait_bounds(&param.bounds);
                        if !bounds.is_empty() {
                            this.inner
                                .borrow_mut()
                                .generic_trait_bounds
                                .insert(var, bounds);
                        }
                    }
                    this.predeclare_scope_items(&impl_block.collected_items)
                        .await;
                    for child in &mut impl_block.items {
                        this.infer_item_inner(child).await?;
                    }
                    this.exit_scope();
                    this.inner.borrow_mut().impl_stack.pop();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Expr(expr) => {
                    if let ExprKind::Splice(splice) = expr.kind_mut() {
                        let token_var = this.infer_expr_inner(splice.token.as_mut()).await?;
                        let token_ty = this.resolve_to_ty(token_var).await?;
                        if !this.is_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    this.emit_error(format!(
                                        "splice in item position requires item token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => this.emit_error("splice expects a quote token expression"),
                            }
                        }
                        Ty::Unit(TypeUnit)
                    } else if let ExprKind::SplicePending(pending) = expr.kind_mut() {
                        let token_var = this.infer_expr_inner(pending.token.as_mut()).await?;
                        let token_ty = this.resolve_to_ty(token_var).await?;
                        if !this.is_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    this.emit_error(format!(
                                        "splice in item position requires item token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => this.emit_error("splice expects a quote token expression"),
                            }
                        }
                        Ty::Unit(TypeUnit)
                    } else {
                        let var = this.infer_expr_inner(expr).await?;
                        this.resolve_to_ty(var).await?
                    }
                }
                _ => {
                    this.emit_error("type inference for item not implemented");
                    Ty::Unknown(TypeUnknown)
                }
            };

            item.set_ty(ty);
            Ok(())
        })
    }

    pub(crate) async fn infer_function(&self, func: &mut ItemDefFunction) -> Result<Ty> {
        self.validate_extern_c_signature(&func.sig);
        let is_lang_item = func.attrs.find_by_name("lang").is_some();
        let impl_ctx = self.inner.borrow().impl_stack.last().cloned().flatten();
        let fn_key = impl_ctx
            .as_ref()
            .map(|ctx| ctx.struct_name.with_segment(func.name.as_str().to_string()));
        let fn_var = if let Some(key) = fn_key.as_ref() {
            let key_str = key.to_key();
            if let Some(var) = self.lookup_env_var(&key_str).await {
                var
            } else {
                let var = self.fresh_type_var();
                self.insert_env(key_str, EnvEntry::Mono(var));
                var
            }
        } else {
            self.symbol_var(&func.name).await
        };
        let param_count = func.sig.params.len();
        let body_is_async_expr = matches!(func.body.kind(), ExprKind::Async(_));
        let is_async_fn = body_is_async_expr
            || func
                .sig
                .ret_ty
                .as_ref()
                .map(is_std_task_future_ty)
                .unwrap_or(false);
        let existing_signature = if is_async_fn {
            None
        } else {
            let root = self.find(fn_var);
            // Extracted to an owned local first: the `Bound` arm awaits,
            // and matching directly on `self.inner.borrow()...` would
            // extend the guard's scope across that `.await`.
            let root_kind = self.inner.borrow().type_vars[root].kind.clone();
            match root_kind {
                TypeVarKind::Bound(ty) => {
                    if let Some(func_term) = self.function_term_from_ty(&ty).await {
                        if func_term.params.len() == param_count {
                            Some(func_term)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        };

        let exception_policy = self.exception_policy_for_ret(func.sig.ret_ty.as_ref());
        let _exception_guard = self.push_exception_context(exception_policy);

        self.enter_scope();

        let _receiver_ty: Option<Ty> = None;
        if let Some(receiver) = func.sig.receiver.as_ref() {
            if let Some(ctx) = impl_ctx.as_ref() {
                let receiver_type = self.ty_for_receiver(ctx, receiver);
                let self_var = self.fresh_type_var();
                let expected = self.type_from_ast_ty(&receiver_type).await?;
                self.unify(self_var, expected).await?;
                self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
            } else {
                self.emit_error(format!(
                    "method {} defined without an impl context",
                    func.name
                ));
            }
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.inner
                        .borrow_mut()
                        .generic_trait_bounds
                        .insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for (idx, param) in func.sig.params.iter_mut().enumerate() {
            let var = existing_signature
                .as_ref()
                .and_then(|sig| sig.params.get(idx).cloned())
                .unwrap_or_else(|| self.fresh_type_var());
            let annot_var = self.type_from_ast_ty(&param.ty).await?;
            self.unify(var, annot_var).await?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var).await?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = if is_lang_item {
            if let Some(ret) = &func.sig.ret_ty {
                self.type_from_ast_ty(ret).await?
            } else {
                self.fresh_type_var()
            }
        } else if let Some(kind) = func.sig.quote_kind {
            let body_block = func.body.as_ref().clone().into_block();
            let mut quote_expr = Expr::from(ExprKind::Quote(ExprQuote {
                span: Span::null(),
                collected_items: body_block.collected_items.clone(),
                block: body_block,
                kind: Some(kind),
            }));
            self.infer_expr_inner(&mut quote_expr).await?
        } else {
            let mut body = func.body.as_mut();
            self.infer_expr_inner(&mut body).await?
        };

        let ret_var = if matches!(exception_policy, ExceptionReturnPolicy::AutoResult) {
            let body_ty = self.resolve_to_ty(body_var).await?;
            let inner_ty = if is_async_fn {
                std_task_future_inner_ty(&body_ty).unwrap_or(body_ty)
            } else {
                body_ty
            };
            let result_ty = make_std_result_ty(inner_ty, std_error_ty());
            let final_ret_ty = if is_async_fn {
                make_std_task_future_ty(result_ty)
            } else {
                result_ty
            };
            let result_var = self.type_from_ast_ty(&final_ret_ty).await?;
            if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
                self.unify(existing, result_var).await?;
                existing
            } else {
                result_var
            }
        } else if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
            if !is_async_fn || body_is_async_expr {
                self.unify(existing, body_var).await?;
            }
            if let Some(ret) = &func.sig.ret_ty {
                if is_async_fn {
                    let future_ty = if is_std_task_future_ty(ret) {
                        ret.clone()
                    } else {
                        make_std_task_future_ty(ret.clone())
                    };
                    let future_var = self.type_from_ast_ty(&future_ty).await?;
                    self.unify(existing, future_var).await?;

                    if !body_is_async_expr {
                        let body_ty = self.resolve_to_ty(body_var).await?;
                        if is_future_like_ty(&body_ty) {
                            self.unify(body_var, future_var).await?;
                        } else if let Some(inner_ty) = std_task_future_inner_ty(&future_ty) {
                            let inner_var = self.type_from_ast_ty(&inner_ty).await?;
                            self.unify(body_var, inner_var).await?;
                        }
                    }
                } else {
                    let annot_var = self.type_from_ast_ty(ret).await?;
                    self.unify(existing, annot_var).await?;
                }
            }
            existing
        } else if let Some(ret) = &func.sig.ret_ty {
            if is_async_fn {
                let future_ty = if is_std_task_future_ty(ret) {
                    ret.clone()
                } else {
                    make_std_task_future_ty(ret.clone())
                };
                let future_var = self.type_from_ast_ty(&future_ty).await?;
                if body_is_async_expr {
                    self.unify(body_var, future_var).await?;
                } else {
                    let body_ty = self.resolve_to_ty(body_var).await?;
                    if is_future_like_ty(&body_ty) {
                        self.unify(body_var, future_var).await?;
                    } else if let Some(inner_ty) = std_task_future_inner_ty(&future_ty) {
                        let inner_var = self.type_from_ast_ty(&inner_ty).await?;
                        self.unify(body_var, inner_var).await?;
                    }
                }
                future_var
            } else {
                let annot_var = self.type_from_ast_ty(ret).await?;
                self.unify(body_var, annot_var).await?;
                annot_var
            }
        } else {
            body_var
        };

        let ret_ty = self.resolve_to_ty(ret_var.clone()).await?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        self.exit_scope();

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var).await?);
        }

        self.bind_function_term(fn_var, param_vars.clone(), ret_var);

        let scheme = self.generalize(fn_var).await?;
        let scheme_env = scheme.clone();
        if let Some(key) = fn_key.as_ref() {
            let key_str = key.to_key();
            self.replace_env_entry(&key_str, EnvEntry::Poly(scheme_env));
        } else {
            self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme_env));
        }

        if let Some(ctx) = impl_ctx.as_ref() {
            for candidate in self.struct_name_variants_for_path(
                &ctx.struct_name,
                ctx.struct_name.segments.len() == 1,
            ) {
                let mut method_sigs = self.own_method_sigs_mut();
                let entry = method_sigs.entry(candidate).or_default();
                if !entry.iter().any(|(n, _)| n == func.name.as_str()) {
                    entry.push((
                        func.name.as_str().to_string(),
                        MethodSignature {
                            sig: func.sig.clone(),
                            impl_generics_params: ctx.impl_generics_params.clone(),
                            self_ty: ctx.self_ty.clone(),
                        },
                    ));
                }
            }
        }

        let func_ty = TypeFunction {
            params: param_tys.clone(),
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty.clone())),
        };

        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    pub(crate) async fn infer_trait_method(&self, func: &mut ItemDefFunction) -> Result<Ty> {
        let fn_var = self.symbol_var(&func.name).await;

        let exception_policy = self.exception_policy_for_ret(func.sig.ret_ty.as_ref());
        let _exception_guard = self.push_exception_context(exception_policy);

        self.enter_scope();

        if let Some(receiver) = func.sig.receiver.as_ref() {
            let self_ty = Ty::name(Name::ident("Self"));
            let receiver_type = match receiver {
                FunctionParamReceiver::Implicit
                | FunctionParamReceiver::Value
                | FunctionParamReceiver::MutValue => self_ty,
                FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                    TypeReference {
                        ty: Box::new(self_ty),
                        mutability: Some(false),
                        lifetime: None,
                    }
                    .into(),
                ),
                FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => {
                    Ty::Reference(
                        TypeReference {
                            ty: Box::new(self_ty),
                            mutability: Some(true),
                            lifetime: None,
                        }
                        .into(),
                    )
                }
            };

            let self_var = self.fresh_type_var();
            let expected = self.type_from_ast_ty(&receiver_type).await?;
            self.unify(self_var, expected).await?;
            self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.inner
                        .borrow_mut()
                        .generic_trait_bounds
                        .insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for param in func.sig.params.iter_mut() {
            let var = self.fresh_type_var();
            let annot_var = self.type_from_ast_ty(&param.ty).await?;
            self.unify(var, annot_var).await?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var).await?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = {
            let mut body = func.body.as_mut();
            self.infer_expr_inner(&mut body).await?
        };

        let ret_var = if matches!(exception_policy, ExceptionReturnPolicy::AutoResult) {
            let body_ty = self.resolve_to_ty(body_var).await?;
            let result_ty = make_std_result_ty(body_ty, std_error_ty());
            self.type_from_ast_ty(&result_ty).await?
        } else if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret).await?;
            self.unify(body_var, annot_var).await?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        self.bind_function_term(fn_var, param_vars.clone(), ret_var);

        let scheme = self.generalize(fn_var).await?;
        self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme));

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var).await?);
        }
        let ret_ty = self.resolve_to_ty(ret_var).await?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        let func_ty = TypeFunction {
            params: param_tys,
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        };
        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    pub(crate) async fn apply_pattern_generalization(&self, info: &PatternInfo) -> Result<()> {
        for binding in &info.bindings {
            let scheme = self.generalize(binding.var).await?;
            self.replace_env_entry(&binding.name, EnvEntry::Poly(scheme));
        }
        Ok(())
    }

    pub(crate) async fn scheme_from_method_signature(&self, sig: &FunctionSignature) -> Result<Ty> {
        let fn_var = self.fresh_type_var();
        let mut param_vars = Vec::new();
        for param in &sig.params {
            param_vars.push(self.type_from_ast_ty(&param.ty).await?);
        }
        let ret_var = if let Some(ret) = sig.ret_ty.as_ref() {
            self.type_from_ast_ty(ret).await?
        } else {
            self.unit_type_var()
        };
        self.bind_function_term(fn_var, param_vars, ret_var);
        self.generalize(fn_var).await
    }

    pub(crate) async fn instantiate_method_signature(
        &self,
        method: &MethodSignature,
    ) -> Result<(TypeVarId, Vec<TypeVarId>, TypeVarId)> {
        self.enter_scope();
        let mut generic_params = method.impl_generics_params.clone();
        generic_params.extend(method.sig.generics_params.clone());
        for param in &generic_params {
            let var = self.register_generic_param(param.name.as_str());
            let bounds = Self::extract_trait_bounds(&param.bounds);
            if !bounds.is_empty() {
                self.inner
                    .borrow_mut()
                    .generic_trait_bounds
                    .insert(var, bounds);
            }
        }

        let receiver_ty = match method.sig.receiver {
            Some(FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic) => {
                Ty::Reference(TypeReference {
                    ty: Box::new(method.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                })
            }
            Some(FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic) => {
                Ty::Reference(TypeReference {
                    ty: Box::new(method.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                })
            }
            Some(
                FunctionParamReceiver::Implicit
                | FunctionParamReceiver::Value
                | FunctionParamReceiver::MutValue,
            ) => method.self_ty.clone(),
            None => Ty::Unit(TypeUnit),
        };
        let receiver_var = self.type_from_ast_ty(&receiver_ty).await?;
        let mut params = Vec::with_capacity(method.sig.params.len() + 1);
        params.push(receiver_var);
        for param in &method.sig.params {
            params.push(self.type_from_ast_ty(&param.ty).await?);
        }
        let ret_var = match method.sig.ret_ty.as_ref() {
            Some(ret_ty) => self.type_from_ast_ty(ret_ty).await?,
            None => self.unit_type_var(),
        };
        let fn_var = self.fresh_type_var();
        self.bind_function_term(fn_var, params, ret_var);
        let scheme = self.generalize(fn_var).await?;
        self.exit_scope();

        let instantiated = self.instantiate_scheme(&scheme).await;
        let function = self
            .function_term_from_ty(&self.resolve_to_ty(instantiated).await?)
            .await
            .ok_or_else(|| typing_error("method scheme did not instantiate to a function"))?;
        let mut params = function.params.into_iter();
        let receiver = params
            .next()
            .ok_or_else(|| typing_error("method scheme has no receiver parameter"))?;
        Ok((receiver, params.collect(), function.ret))
    }

    pub(crate) fn extract_trait_bounds(bounds: &TypeBounds) -> Vec<String> {
        bounds
            .bounds
            .iter()
            .filter_map(|expr| match expr.kind() {
                ExprKind::Name(name) => Some(name.to_string()),
                ExprKind::Value(value) => match value.as_ref() {
                    Value::Type(Ty::Expr(inner)) => match inner.kind() {
                        ExprKind::Name(name) => Some(name.to_string()),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }
}
