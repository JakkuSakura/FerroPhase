use crate::*;
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::module::path::*;
use std::cell::{Ref, RefMut};
use std::collections::{HashMap, HashSet};

impl AstTypeInferencer {
    pub(crate) async fn register_qualified_symbol(&self, path: &QualifiedPath) -> TypeVarId {
        let key = path.to_key();
        if let Some(var) = self.lookup_env_var(&key).await {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    /// Boxed: recurses into itself for nested modules (see `BoxFuture`'s doc
    /// comment). `self` is cloned into the async block (see the reference
    /// pattern above) rather than borrowed, so only `items`/`prefix` bound
    /// the `'a` lifetime now.
    pub(crate) fn register_qualified_items<'a>(
        &self,
        items: &'a [Item],
        prefix: &'a QualifiedPath,
    ) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
            for item in items {
                match item.kind() {
                    ItemKind::Module(module) => {
                        let next = prefix.with_segment(module.name.as_str().to_string());
                        let is_root = {
                            let mut inner = this.inner.borrow_mut();
                            inner.module_defs.insert(next.clone());
                            prefix.is_empty()
                        };
                        if is_root {
                            this.inner
                                .borrow_mut()
                                .root_modules
                                .insert(module.name.as_str().to_string());
                        }
                        this.register_qualified_items(&module.items, &next).await;
                    }
                    ItemKind::DefFunction(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.own_function_sigs_mut()
                            .insert(name.clone(), def.sig.clone());
                        this.own_function_item_ids_mut()
                            .insert(name.clone(), item.id());
                        let var = this.register_qualified_symbol(&name).await;
                        let saved = std::mem::replace(
                            &mut this.inner.borrow_mut().module_path,
                            prefix.clone(),
                        );
                        this.prebind_function_signature(def, var).await;
                        this.inner.borrow_mut().module_path = saved;
                    }
                    ItemKind::DeclFunction(decl) => {
                        let name = prefix.with_segment(decl.name.as_str().to_string());
                        this.own_function_sigs_mut()
                            .insert(name.clone(), decl.sig.clone());
                        if decl.sig.abi.is_c() {
                            this.inner
                                .borrow_mut()
                                .extern_function_signatures
                                .insert(name.clone(), decl.sig.clone());
                        }
                        let var = this.register_qualified_symbol(&name).await;
                        let saved = std::mem::replace(
                            &mut this.inner.borrow_mut().module_path,
                            prefix.clone(),
                        );
                        this.prebind_decl_function_signature(decl, var).await;
                        this.inner.borrow_mut().module_path = saved;
                    }
                    ItemKind::DefConst(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefStatic(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefStruct(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.own_struct_defs_mut()
                            .insert(name.clone(), def.value.clone());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefStructural(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefEnum(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.own_enum_defs_mut()
                            .insert(name.clone(), def.value.clone());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefType(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::OpaqueType(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::DefTrait(def) => {
                        let name = prefix.with_segment(def.name.as_str().to_string());
                        this.own_trait_defs_mut().insert(name.clone());
                        this.register_qualified_symbol(&name).await;
                    }
                    ItemKind::Impl(impl_block) => {
                        if let Some(self_name) = impl_self_ty_name(&impl_block.self_ty) {
                            let struct_path = prefix.with_segment(self_name);
                            for child in &impl_block.items {
                                if let ItemKind::DefFunction(func) = child.kind() {
                                    // Store for method lookup -- keyed purely by
                                    // path, so no struct-vs-enum branch needed.
                                    this.own_method_sigs_mut()
                                        .entry(struct_path.clone())
                                        .or_default()
                                        .push((
                                            func.name.as_str().to_string(),
                                            MethodSignature {
                                                sig: func.sig.clone(),
                                                impl_generics_params: impl_block
                                                    .generics_params
                                                    .clone(),
                                                self_ty: Ty::expr(impl_block.self_ty.clone()),
                                            },
                                        ));
                                    // Also store as a function sig for ::call syntax
                                    let fn_path =
                                        struct_path.with_segment(func.name.as_str().to_string());
                                    this.own_function_sigs_mut()
                                        .insert(fn_path.clone(), func.sig.clone());
                                    this.own_function_item_ids_mut()
                                        .insert(fn_path.clone(), child.id());
                                    this.register_qualified_symbol(&fn_path).await;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        })
    }

    /// Populate `module_defs` and `root_modules` from all known
    /// crates in the workspace so that import resolution can see
    /// module paths like `std::meta`. Also seeds `root_modules` from
    /// *registered* (not-yet-loaded) packages, so `use std::...`-style
    /// paths resolve to the right qualified path even before `std` is
    /// actually loaded — loading itself happens on demand (see
    /// `lookup_struct`/`lookup_function_signature_with_path`).
    pub fn seed_workspace_graph(&self) {
        let mut inner = self.inner.borrow_mut();
        for krate in self.typing_ctx.env_ctx.crates().values() {
            for path in &krate.borrow().module_paths {
                inner.module_defs.insert(path.clone());
                if let Some(head) = path.segments.first() {
                    inner.root_modules.insert(head.clone());
                }
            }
        }
        for name in self.typing_ctx.env_ctx.registered_names() {
            inner.root_modules.insert(name.to_string());
        }
    }

    /// Register pre-parsed items from an external module into the
    /// typer's lookup tables. Used when compiling dependency crates
    /// (e.g. std) whose items need to be available for name resolution.
    pub async fn inject_module(&self, path: &QualifiedPath, items: &[Item]) {
        {
            let mut inner = self.inner.borrow_mut();
            inner.module_defs.insert(path.clone());
            if path.segments.len() == 1 {
                inner.root_modules.insert(path.segments[0].clone());
            }
        }
        self.register_qualified_items(items, path).await;
    }

    /// Borrowed access to this crate's own registry — the "current crate"
    /// is just one more entry in the same root every other crate lives in;
    /// there's no separate local-vs-workspace branch anywhere. Each
    /// accessor's `Ref`/`RefMut` guard is a short-lived temporary (dropped
    /// at the end of the statement that uses it), matching how the plain
    /// `HashMap` fields these replace were always used.
    pub(crate) fn own_struct_defs(&self) -> Ref<'_, HashMap<QualifiedPath, TypeStruct>> {
        Ref::map(self.own_crate.borrow(), |k| &k.struct_defs)
    }
    pub(crate) fn own_struct_defs_mut(&self) -> RefMut<'_, HashMap<QualifiedPath, TypeStruct>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.struct_defs)
    }
    pub(crate) fn own_enum_defs(&self) -> Ref<'_, HashMap<QualifiedPath, TypeEnum>> {
        Ref::map(self.own_crate.borrow(), |k| &k.enum_defs)
    }
    pub(crate) fn own_enum_defs_mut(&self) -> RefMut<'_, HashMap<QualifiedPath, TypeEnum>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.enum_defs)
    }
    pub(crate) fn own_function_sigs(&self) -> Ref<'_, HashMap<QualifiedPath, FunctionSignature>> {
        Ref::map(self.own_crate.borrow(), |k| &k.function_sigs)
    }
    pub(crate) fn own_function_sigs_mut(
        &self,
    ) -> RefMut<'_, HashMap<QualifiedPath, FunctionSignature>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.function_sigs)
    }
    pub(crate) fn own_function_item_ids(
        &self,
    ) -> Ref<'_, HashMap<QualifiedPath, fp_core::ast::ItemId>> {
        Ref::map(self.own_crate.borrow(), |k| &k.function_item_ids)
    }
    pub(crate) fn own_function_item_ids_mut(
        &self,
    ) -> RefMut<'_, HashMap<QualifiedPath, fp_core::ast::ItemId>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.function_item_ids)
    }
    /// Inherent methods declared in an `impl SelfType { .. }` block, keyed
    /// by `SelfType`'s own fully-qualified path -- one shared table
    /// regardless of whether `SelfType` resolves to a struct, an enum, or
    /// anything else nominal (see `PackageCrate::method_sigs`'s doc
    /// comment for why this isn't a field on `Ty` itself).
    pub(crate) fn own_method_sigs(
        &self,
    ) -> Ref<'_, HashMap<QualifiedPath, Vec<(String, MethodSignature)>>> {
        Ref::map(self.own_crate.borrow(), |k| &k.method_sigs)
    }
    pub(crate) fn own_method_sigs_mut(
        &self,
    ) -> RefMut<'_, HashMap<QualifiedPath, Vec<(String, MethodSignature)>>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.method_sigs)
    }
    pub(crate) fn own_trait_defs(&self) -> Ref<'_, HashSet<QualifiedPath>> {
        Ref::map(self.own_crate.borrow(), |k| &k.trait_defs)
    }
    pub(crate) fn own_trait_defs_mut(&self) -> RefMut<'_, HashSet<QualifiedPath>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.trait_defs)
    }

    /// Use a specific shared crate entry (already registered in the root
    /// `WorkspaceContext`, e.g. via `env_ctx.begin_crate`) instead of the
    /// default standalone one `new()` creates — used when typing a
    /// freshly-loaded package (`CompilerDriver::load_package`), so its
    /// registry ends up in the same place every lookup already searches.

    pub(crate) async fn resolve_impl_context(
        &self,
        self_ty: &Expr,
        impl_generics: &[GenericParam],
    ) -> Option<ImplContext> {
        if let ExprKind::Value(value) = self_ty.kind() {
            if let Value::Type(ty) = value.as_ref() {
                let name = format!("<impl:{}>", ty);
                return Some(ImplContext {
                    struct_name: QualifiedPath::new(vec![name]),
                    self_ty: ty.clone(),
                    impl_generics_params: impl_generics.to_vec(),
                });
            }
        }
        let resolved_name = match self_ty.kind() {
            ExprKind::Name(name) => self.resolve_name_key(name),
            _ => None,
        };
        let name = resolved_name
            .or_else(|| self.struct_name_from_expr(self_ty))
            .unwrap_or_else(|| QualifiedPath::new(Vec::new()));

        if name.is_empty() {
            self.emit_error("impl self type must resolve to a struct or enum");
            return None;
        }

        if let Some(def) = self.own_struct_defs().get(&name).cloned() {
            return Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Struct(def),
                impl_generics_params: impl_generics.to_vec(),
            });
        }
        if let Some(def) = self.own_enum_defs().get(&name).cloned() {
            return Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Enum(def),
                impl_generics_params: impl_generics.to_vec(),
            });
        }

        if let Some((resolved, def)) = self.lookup_struct_def_by_name(&name.to_key()).await {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Struct(def),
                impl_generics_params: impl_generics.to_vec(),
            });
        }
        if let Some((resolved, def)) = self.lookup_enum_def_by_name(&name.to_key()) {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Enum(def),
                impl_generics_params: impl_generics.to_vec(),
            });
        }
        if let Some(ty) = self.resolve_impl_self_from_env(&name, impl_generics).await {
            return Some(ty);
        }

        for candidate in self.struct_name_variants_for_path(&name, name.segments.len() == 1) {
            if let Some(def) = self.own_struct_defs().get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Struct(def),
                    impl_generics_params: impl_generics.to_vec(),
                });
            }
        }
        for candidate in self.struct_name_variants_for_path(&name, name.segments.len() == 1) {
            if let Some(def) = self.own_enum_defs().get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Enum(def),
                    impl_generics_params: impl_generics.to_vec(),
                });
            }
        }

        {
            let placeholder_name = name.tail().unwrap_or("Unknown").to_string();
            let placeholder = TypeStruct {
                name: Ident::new(placeholder_name),
                generics_params: Vec::new(),
                repr: ReprOptions::default(),
                fields: Vec::new(),
            };
            self.emit_warning(format!(
                "impl target {} is not a known struct or enum",
                name.to_key()
            ));
            Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Struct(placeholder),
                impl_generics_params: impl_generics.to_vec(),
            })
        }
    }

    pub(crate) async fn resolve_impl_self_from_env(
        &self,
        name: &QualifiedPath,
        impl_generics: &[GenericParam],
    ) -> Option<ImplContext> {
        let mut candidates = Vec::new();
        candidates.push(name.clone());
        if !self.inner.borrow().module_path.is_empty() && name.segments.len() == 1 {
            candidates.push(
                self.inner
                    .borrow()
                    .module_path
                    .with_segment(name.segments[0].clone()),
            );
        }
        for candidate in candidates {
            let key = candidate.to_key();
            if let Some(var) = self.lookup_env_var(&key).await {
                if let Ok(ty) = self.resolve_to_ty(var).await {
                    match ty {
                        Ty::Struct(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Struct(def),
                                impl_generics_params: impl_generics.to_vec(),
                            })
                        }
                        Ty::Enum(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Enum(def),
                                impl_generics_params: impl_generics.to_vec(),
                            })
                        }
                        _ => {}
                    }
                }
            }
        }
        None
    }

    pub(crate) fn ty_for_receiver(
        &self,
        ctx: &ImplContext,
        receiver: &FunctionParamReceiver,
    ) -> Ty {
        match receiver {
            FunctionParamReceiver::Implicit
            | FunctionParamReceiver::Value
            | FunctionParamReceiver::MutValue => ctx.self_ty.clone(),
            FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                }
                .into(),
            ),
            FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                }
                .into(),
            ),
        }
    }

    pub(crate) fn register_method_stub(&self, ctx: &ImplContext, func: &ItemDefFunction) {
        let method = MethodSignature {
            sig: func.sig.clone(),
            impl_generics_params: ctx.impl_generics_params.clone(),
            self_ty: ctx.self_ty.clone(),
        };
        for candidate in self
            .struct_name_variants_for_path(&ctx.struct_name, ctx.struct_name.segments.len() == 1)
        {
            self.own_method_sigs_mut()
                .entry(candidate)
                .or_default()
                .push((func.name.as_str().to_string(), method.clone()));
        }
    }

    pub(crate) fn peel_reference(mut ty: Ty) -> Ty {
        loop {
            match ty {
                Ty::Reference(reference) => {
                    ty = (*reference.ty).clone();
                }
                other => return other,
            }
        }
    }

    pub(crate) async fn predeclare_item(&self, item: &Item) {
        if std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            match item.kind() {
                ItemKind::DefStruct(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefStruct module_path={:?}",
                        self.inner.borrow().module_path
                    );
                }
                ItemKind::DefConst(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefConst module_path={:?}",
                        self.inner.borrow().module_path
                    );
                }
                ItemKind::DefType(def) if def.name.as_str().contains("TypeBuilder") => {}
                ItemKind::DefStructural(def) if def.name.as_str().contains("TypeBuilder") => {}
                _ => {}
            }
        }
        match item.kind() {
            ItemKind::Macro(mac) => {
                self.predeclare_macro_item(mac);
            }
            ItemKind::DefStruct(def) => {
                self.insert_struct_def(&def.name, def.value.clone());
                let var = self.symbol_var(&def.name).await;
                let ty = Ty::Struct(def.value.clone());
                if let Ok(struct_var) = self.type_from_ast_ty(&ty).await {
                    let _ = self.unify(var, struct_var).await;
                }
            }
            ItemKind::DefStructural(def) => {
                let struct_ty = TypeStruct {
                    name: def.name.clone(),
                    generics_params: Vec::new(),
                    repr: ReprOptions::default(),
                    fields: def.value.fields.clone(),
                };
                self.insert_struct_def(&def.name, struct_ty);
                self.register_symbol(&def.name);
            }
            ItemKind::DefType(def) => {
                self.register_symbol(&def.name);
                // Type the value (const block or direct expression). Struct types
                // are resolved via comptime eval and seeded into struct_defs on retry.
                let _ = self.type_from_ast_ty(&def.value).await;
                let is_const_block = matches!(&def.value, Ty::ConstBlock(_));

                if !is_const_block {
                    self.record_unimplemented_symbol(&def.name, &def.attrs);
                } else if let Ty::ConstBlock(block) = &def.value {
                    // Spawn this alias's struct-shape resolution as its own
                    // independent task (see `AstTypeInferencer::tasks`), keyed
                    // by the alias's own name so `force`/`await_struct_alias`
                    // can find and await it -- mirrors the `DefConst` arm
                    // above. Only computes/caches the resolved struct shape
                    // via a throwaway clone of the const-block's inner expr;
                    // the item's own symbol-table bookkeeping stays owned by
                    // the sequential loop's later "already resolved" fast
                    // path (the full-inference `DefType` arm below).
                    let name = def.name.as_str().to_string();
                    if !self.typing_ctx.resolved_types.borrow().contains_key(&name)
                        && !self.tasks.contains(&name)
                    {
                        let this = self.clone();
                        let mut expr_clone = (*block.expr).clone();
                        let expr_id = self.expr_id(&block.expr);
                        let key = format!("__fp_expr_{expr_id}");
                        self.tasks.spawn(name, async move {
                            let _ = this.infer_expr_inner(&mut expr_clone).await;
                            this.await_comptime(&key, &expr_clone).await.map(|_| ())
                        });
                    }
                }

                // Look up the struct by its qualified name, in case a prior
                // pass already resolved it — lets sibling items in this same
                // scope reference it before this item's own full-inference
                // turn comes up.
                let path = if self.inner.borrow().module_path.is_empty() {
                    QualifiedPath::new(vec![def.name.as_str().to_string()])
                } else {
                    self.inner
                        .borrow()
                        .module_path
                        .with_segment(def.name.as_str().to_string())
                };
                let struct_def = self.own_struct_defs().get(&path).cloned().or_else(|| {
                    self.typing_ctx
                        .resolved_types
                        .borrow()
                        .get(def.name.as_str())
                        .cloned()
                        .map(|s| {
                            self.own_struct_defs_mut().insert(path.clone(), s.clone());
                            s
                        })
                });
                if let Some(struct_def) = struct_def {
                    let var = self.symbol_var(&def.name).await;
                    self.bind(var, Ty::Struct(struct_def));
                }
            }
            ItemKind::DefEnum(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let enum_name = self
                    .qualified_name(def.name.as_str())
                    .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                self.own_enum_defs_mut()
                    .insert(enum_name.clone(), def.value.clone());
                self.register_symbol(&def.name);

                let mut variant_keys = Vec::new();
                for variant in &def.value.variants {
                    let qualified = enum_name.with_segment(variant.name.as_str().to_string());
                    variant_keys.push(qualified.clone());
                    let key = qualified.to_key();
                    if self.lookup_env_var(&key).await.is_none() {
                        let var = self.fresh_type_var();
                        self.insert_env(key, EnvEntry::Mono(var));
                    }
                }
                self.inner
                    .borrow_mut()
                    .enum_variants
                    .insert(enum_name, variant_keys);
            }
            ItemKind::DefTrait(def) => {
                let trait_name = self
                    .qualified_name(def.name.as_str())
                    .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                self.own_trait_defs_mut().insert(trait_name);
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                self.register_symbol(&def.name);
                // `entry`/`or_default()` returns a `&mut` borrowed from the
                // `RefMut` guard, so the guard is bound to a named local
                // (`inner`) rather than left as an inline temporary --
                // a bare temporary's scope wouldn't survive past this
                // statement, but `entry` needs to keep being written to for
                // the rest of this arm. Safe to hold across the loop below
                // since nothing in it re-borrows `self.inner` or awaits.
                let mut inner = self.inner.borrow_mut();
                let entry = inner
                    .trait_method_sigs
                    .entry(def.name.as_str().to_string())
                    .or_default();
                for member in &def.items {
                    match member.kind() {
                        ItemKind::DeclFunction(decl) => {
                            if let Some(name) = decl.sig.name.as_ref() {
                                entry.insert(name.as_str().to_string(), decl.sig.clone());
                            }
                        }
                        ItemKind::DefFunction(func) => {
                            entry.insert(func.name.as_str().to_string(), func.sig.clone());
                        }
                        _ => {}
                    }
                }
                drop(inner);
            }
            ItemKind::DefConst(def) => {
                self.register_symbol(&def.name);
                // Spawn this const's comptime-value resolution as its own
                // independent task (see `AstTypeInferencer::tasks`) -- so
                // `force`/`await_comptime` can await it directly regardless
                // of where in the item list it sits, instead of the
                // sequential item loop's own order determining whether a
                // forward reference resolves. This only computes and caches
                // the VALUE (via a throwaway clone of the expr, same
                // fast-path-tolerant typing + hook call the real `DefConst`
                // arm below does) -- it deliberately does *not* run the
                // item's own symbol-table bookkeeping (`symbol_var`/
                // `generalize_symbol`), which the sequential loop still owns
                // exactly once, later, via its normal "already resolved --
                // bind from cache" fast path.
                let name = def.name.as_str().to_string();
                if !self.typing_ctx.resolved_consts.borrow().contains_key(&name)
                    && !self.tasks.contains(&name)
                {
                    let this = self.clone();
                    let mut expr_clone = (*def.value).clone();
                    let key = name.clone();
                    self.tasks.spawn(name, async move {
                        let _ = this.infer_expr_inner(&mut expr_clone).await;
                        this.await_comptime(&key, &expr_clone).await.map(|_| ())
                    });
                }
            }
            ItemKind::DefStatic(def) => {
                self.register_symbol(&def.name);
                if let Ty::Struct(ty) = &def.ty {
                    self.insert_struct_def(&ty.name, ty.clone());
                }
            }
            ItemKind::DefFunction(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let in_impl = self.inner.borrow().impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&def.name, &def.sig, item.id());
                }
                // Extracted to an owned local *before* the `if let`: this
                // chain's final `else` branch awaits, and using the
                // `self.inner.borrow()` call directly as the `if let`
                // scrutinee would otherwise have the guard's scope extended
                // across that `.await` (Rust extends an `if let`
                // scrutinee's temporaries over the whole `if`/`else if`/
                // `else` chain).
                let impl_ctx_opt = self.inner.borrow().impl_stack.last().cloned().flatten();
                let fn_var = if let Some(ctx) = impl_ctx_opt {
                    let key = ctx.struct_name.with_segment(def.name.as_str().to_string());
                    let key_str = key.to_key();
                    if let Some(var) = self.lookup_env_var(&key_str).await {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key_str, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    if !in_impl {
                        self.register_symbol(&def.name);
                    }
                    self.symbol_var(&def.name).await
                };
                if def.sig.generics_params.is_empty() {
                    self.prebind_function_signature(def, fn_var).await;
                } else {
                    let fn_key = self
                        .inner
                        .borrow()
                        .impl_stack
                        .last()
                        .cloned()
                        .flatten()
                        .map(|ctx| {
                            ctx.struct_name
                                .with_segment(def.name.as_str().to_string())
                                .to_key()
                        });
                    self.enter_scope();
                    for param in &def.sig.generics_params {
                        let var = self.register_generic_param(param.name.as_str());
                        let bounds = Self::extract_trait_bounds(&param.bounds);
                        if !bounds.is_empty() {
                            self.inner
                                .borrow_mut()
                                .generic_trait_bounds
                                .insert(var, bounds);
                        }
                    }
                    let mut ok = true;
                    let mut param_vars = Vec::new();
                    for param in &def.sig.params {
                        match self.type_from_ast_ty(&param.ty).await {
                            Ok(var) => param_vars.push(var),
                            Err(_) => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    let ret_var = if ok {
                        if let Some(ret_ty) = def.sig.ret_ty.as_ref() {
                            self.type_from_ast_ty(ret_ty).await.ok()
                        } else {
                            Some(self.unit_type_var())
                        }
                    } else {
                        None
                    };
                    if ok {
                        if let Some(ret_var) = ret_var {
                            self.bind_function_term(fn_var, param_vars, ret_var);
                        } else {
                            ok = false;
                        }
                    }
                    self.exit_scope();
                    if ok {
                        if let Ok(scheme) = self.generalize(fn_var).await {
                            if let Some(key) = fn_key.as_ref() {
                                self.replace_env_entry(key.as_str(), EnvEntry::Poly(scheme));
                            } else {
                                self.replace_env_entry(def.name.as_str(), EnvEntry::Poly(scheme));
                            }
                        }
                    }
                }
            }
            ItemKind::DeclFunction(decl) => {
                let in_impl = self.inner.borrow().impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&decl.name, &decl.sig, item.id());
                    if decl.sig.abi.is_c() {
                        self.record_extern_function_signature(&decl.name, &decl.sig);
                    }
                    self.register_symbol(&decl.name);
                }
                // See the `DefFunction` arm above for why this is extracted
                // before the `if let` chain (its `else` branch awaits).
                let impl_ctx_opt = self.inner.borrow().impl_stack.last().cloned().flatten();
                let fn_var = if let Some(ctx) = impl_ctx_opt {
                    let key = ctx.struct_name.with_segment(decl.name.as_str().to_string());
                    let key_str = key.to_key();
                    if let Some(var) = self.lookup_env_var(&key_str).await {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key_str, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    self.symbol_var(&decl.name).await
                };
                if decl.sig.generics_params.is_empty() && decl.sig.receiver.is_none() {
                    self.prebind_decl_function_signature(decl, fn_var).await;
                }
            }
            ItemKind::Module(module) => {
                self.record_module_def(module.name.as_str());
                self.push_module_path(module.name.as_str());
                self.enter_scope();
                // Read `env.len()` before taking the `module_scope_depths`
                // write borrow -- both are `Inner` fields, and a `RefMut`
                // for the push's receiver held simultaneously with a `Ref`
                // for its argument would panic at runtime (`RefCell`
                // borrows aren't partitioned per-field).
                let env_len = self.inner.borrow().env.len();
                self.inner
                    .borrow_mut()
                    .module_scope_depths
                    .push(env_len.saturating_sub(1));
                self.predeclare_scope_items(&module.collected_items).await;
                self.exit_scope();
                self.inner.borrow_mut().module_scope_depths.pop();
                self.pop_module_path();
                let prefix = if self.inner.borrow().module_path.is_empty() {
                    QualifiedPath::new(vec![module.name.as_str().to_string()])
                } else {
                    self.inner
                        .borrow()
                        .module_path
                        .with_segment(module.name.as_str().to_string())
                };
                self.register_qualified_items(&module.items, &prefix).await;
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self
                    .resolve_impl_context(&impl_block.self_ty, &impl_block.generics_params)
                    .await;
                self.inner.borrow_mut().impl_stack.push(ctx.clone());
                if let Some(ref ctx) = ctx {
                    for child in &impl_block.items {
                        if let ItemKind::DefFunction(func) = child.kind() {
                            self.register_method_stub(ctx, func);
                            for candidate in self.struct_name_variants_for_path(
                                &ctx.struct_name,
                                ctx.struct_name.segments.len() == 1,
                            ) {
                                let key = candidate.with_segment(func.name.as_str().to_string());
                                let key_str = key.to_key();
                                if self.lookup_env_var(&key_str).await.is_none() {
                                    let var = self.fresh_type_var();
                                    self.insert_env(key_str, EnvEntry::Mono(var));
                                    self.prebind_function_signature(func, var).await;
                                }
                            }
                        }
                    }
                }

                self.enter_scope();
                self.predeclare_scope_items(&impl_block.collected_items)
                    .await;
                self.exit_scope();
                self.inner.borrow_mut().impl_stack.pop();
            }
            _ => {}
        }
    }

    pub(crate) fn predeclare_macro_item(&self, mac: &ItemMacro) {
        let macro_name = mac
            .invocation
            .path
            .segments
            .last()
            .map(|ident| ident.as_str());
        let Some(macro_name) = macro_name else {
            return;
        };
        let tokens = tokenize_macro_tokens(&mac.invocation.tokens);
        match macro_name {
            "common_struct" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "struct") {
                    self.register_placeholder_struct(&name);
                }
            }
            "common_enum" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "enum") {
                    self.register_placeholder_enum(&name);
                }
            }
            "plain_value" => {
                if let Some(name) = find_first_type_ident(&tokens) {
                    self.register_placeholder_struct(&name);
                }
            }
            _ => {}
        }
    }

    pub(crate) fn register_placeholder_struct(&self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| QualifiedPath::new(vec![name.to_string()]));
        if self.own_struct_defs().contains_key(&key) {
            return;
        }
        let ty = TypeStruct {
            name: Ident::new(name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            fields: Vec::new(),
        };
        self.own_struct_defs_mut().insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    pub(crate) fn register_placeholder_enum(&self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| QualifiedPath::new(vec![name.to_string()]));
        if self.own_enum_defs().contains_key(&key) {
            return;
        }
        let ty = TypeEnum {
            name: Ident::new(name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            variants: Vec::new(),
        };
        self.own_enum_defs_mut().insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    pub(crate) async fn register_import_aliases(&self, import: &ItemImport) {
        let entries = match self.expand_import_tree(&import.tree, Vec::new()) {
            Ok(entries) => entries,
            Err(err) => {
                self.emit_error(format!("failed to expand import tree: {}", err));
                return;
            }
        };

        for (path_segments, alias) in entries {
            if path_segments.is_empty() {
                continue;
            }
            let mut qualified = QualifiedPath::new(path_segments);
            let parsed = ParsedPath {
                prefix: PathPrefix::Plain,
                segments: qualified.segments.clone(),
            };
            let resolved = {
                let inner = self.inner.borrow();
                resolve_item_path(
                    &parsed,
                    &inner.module_path,
                    &inner.root_modules,
                    &inner.extern_prelude,
                    &inner.module_defs,
                    |candidate| self.item_exists_path(candidate),
                    |name| self.scope_contains_non_module(name),
                )
            };
            if let Some(resolved) = resolved {
                qualified = resolved;
            }
            let mut key = qualified.to_key();
            if self.lookup_env_var(&key).await.is_none()
                && !self.inner.borrow().module_defs.contains(&qualified)
            {
                if let Some(first) = qualified.segments.first() {
                    if (first == "std" || first == "core" || first == "alloc")
                        && qualified.segments.len() > 1
                    {
                        let stripped = QualifiedPath::new(
                            qualified.segments.iter().skip(1).cloned().collect(),
                        );
                        let stripped_key = stripped.to_key();
                        if self.lookup_env_var(&stripped_key).await.is_some()
                            || self.inner.borrow().module_defs.contains(&stripped)
                            || self.item_exists_path(&stripped)
                        {
                            qualified = stripped;
                            key = stripped_key;
                        }
                    }
                }
            }
            if self.lookup_env_var(&key).await.is_some() {
                self.insert_symbol_alias(&alias, qualified);
                continue;
            }
            if self.inner.borrow().module_defs.contains(&qualified) {
                self.insert_module_alias(&alias, qualified);
                continue;
            }
            if self.item_exists_path(&qualified) {
                self.insert_symbol_alias(&alias, qualified);
                continue;
            }
            if self.inner.borrow().lossy_mode {
                let var = self.fresh_type_var();
                self.bind_error(var);
                self.insert_env(key.clone(), EnvEntry::Mono(var));
                self.insert_symbol_alias(&alias, qualified);
            } else {
                let var = self.fresh_type_var();
                self.bind_error(var);
                self.insert_env(key.clone(), EnvEntry::Mono(var));
                self.insert_symbol_alias(&alias, qualified);
            }
        }
    }

    pub(crate) fn insert_module_alias(&self, alias: &str, path: QualifiedPath) {
        if let Some(scope) = self.inner.borrow_mut().module_aliases.last_mut() {
            scope.insert(alias.to_string(), path);
        }
    }

    pub(crate) fn insert_symbol_alias(&self, alias: &str, qualified: QualifiedPath) {
        if let Some(scope) = self.inner.borrow_mut().symbol_aliases.last_mut() {
            scope.insert(alias.to_string(), qualified);
        }
    }

    pub(crate) fn expand_import_tree(
        &self,
        tree: &ItemImportTree,
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        match tree {
            ItemImportTree::Path(path) => self.expand_import_segments(&path.segments, base),
            ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                Ok(results)
            }
            ItemImportTree::Root => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::SelfMod => {
                self.expand_import_segments(&[], self.inner.borrow().module_path.segments.clone())
            }
            ItemImportTree::SuperMod => {
                self.expand_import_segments(&[], self.parent_module_path().segments)
            }
            ItemImportTree::Crate => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
            _ => self.expand_import_segments(std::slice::from_ref(tree), base),
        }
    }

    pub(crate) fn expand_import_segments(
        &self,
        segments: &[ItemImportTree],
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        if segments.is_empty() {
            return Ok(Vec::new());
        }

        let first = &segments[0];
        let rest = &segments[1..];
        match first {
            ItemImportTree::Ident(ident) => {
                let name = ident.name.as_str();
                let mut new_base = base;
                match name {
                    "self" => new_base = self.inner.borrow().module_path.segments.clone(),
                    "super" => new_base = self.parent_module_path().segments,
                    "crate" => new_base = Vec::new(),
                    _ => new_base.push(ident.name.clone()),
                }

                if rest.is_empty() && !matches!(name, "self" | "super" | "crate") {
                    Ok(vec![(new_base.clone(), ident.name.clone())])
                } else if rest.is_empty() {
                    Ok(Vec::new())
                } else {
                    self.expand_import_segments(rest, new_base)
                }
            }
            ItemImportTree::Rename(rename) => {
                if !rest.is_empty() {
                    return Err(typing_error("rename segments must be terminal"));
                }
                let mut new_base = base;
                new_base.push(rename.from.name.clone());
                Ok(vec![(new_base, rename.to.name.clone())])
            }
            ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                if rest.is_empty() {
                    Ok(results)
                } else {
                    let mut final_results = Vec::new();
                    for (path_segments, alias) in results {
                        let mut more = self.expand_import_segments(rest, path_segments.clone())?;
                        if more.is_empty() {
                            final_results.push((path_segments, alias));
                        } else {
                            final_results.append(&mut more);
                        }
                    }
                    Ok(final_results)
                }
            }
            ItemImportTree::Path(path) => {
                let nested = self.expand_import_segments(&path.segments, base.clone())?;
                if rest.is_empty() {
                    Ok(nested)
                } else {
                    let mut results = Vec::new();
                    for (segments_acc, alias) in nested {
                        let mut more = self.expand_import_segments(rest, segments_acc.clone())?;
                        if more.is_empty() {
                            results.push((segments_acc, alias));
                        } else {
                            results.append(&mut more);
                        }
                    }
                    Ok(results)
                }
            }
            ItemImportTree::Root => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::SelfMod => {
                self.expand_import_segments(rest, self.inner.borrow().module_path.segments.clone())
            }
            ItemImportTree::SuperMod => {
                self.expand_import_segments(rest, self.parent_module_path().segments)
            }
            ItemImportTree::Crate => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
        }
    }

    pub(crate) async fn prebind_function_signature(
        &self,
        func: &ItemDefFunction,
        fn_var: TypeVarId,
    ) {
        if matches!(func.body.kind(), ExprKind::Async(_))
            || func
                .sig
                .ret_ty
                .as_ref()
                .map(is_std_task_future_ty)
                .unwrap_or(false)
        {
            return;
        }

        if !func.sig.generics_params.is_empty() || func.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        // Extracted to an owned local first: the guard clause below awaits,
        // and matching directly on `self.inner.borrow()...` would extend
        // the borrow guard's scope across that `.await`.
        let root_kind = self.inner.borrow().type_vars[root].kind.clone();
        if matches!(
            root_kind,
            TypeVarKind::Bound(ty) if self.function_term_from_ty(&ty).await.is_some()
        ) {
            return;
        }

        let module_path = self.inner.borrow().module_path.clone();
        let mut param_vars = Vec::new();
        for param in &func.sig.params {
            match self
                .type_from_ast_ty_in_module(&param.ty, &module_path)
                .await
            {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &func.sig.ret_ty {
            match self.type_from_ast_ty_in_module(ret_ty, &module_path).await {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, Ty::Unit(TypeUnit));
            unit
        };

        self.bind_function_term(fn_var, param_vars, ret_var);
    }

    pub(crate) async fn prebind_decl_function_signature(
        &self,
        decl: &ItemDeclFunction,
        fn_var: TypeVarId,
    ) {
        if !decl.sig.generics_params.is_empty() || decl.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        // See `prebind_function_signature` above: extracted to an owned
        // local before matching, since the guard clause awaits.
        let root_kind = self.inner.borrow().type_vars[root].kind.clone();
        if matches!(
            root_kind,
            TypeVarKind::Bound(ty) if self.function_term_from_ty(&ty).await.is_some()
        ) {
            return;
        }

        let module_path = self.inner.borrow().module_path.clone();
        let mut param_vars = Vec::new();
        for param in &decl.sig.params {
            match self
                .type_from_ast_ty_in_module(&param.ty, &module_path)
                .await
            {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &decl.sig.ret_ty {
            match self.type_from_ast_ty_in_module(ret_ty, &module_path).await {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, Ty::Unit(TypeUnit));
            unit
        };

        self.bind_function_term(fn_var, param_vars, ret_var);
    }

    pub(crate) fn register_generic_param(&self, name: &str) -> TypeVarId {
        let var = self.fresh_type_var();
        self.insert_env(name.to_string(), EnvEntry::Mono(var));
        self.inner
            .borrow_mut()
            .generic_type_vars
            .insert(var, name.to_string());
        if let Some(scope) = self.inner.borrow_mut().generic_scopes.last_mut() {
            scope.insert(name.to_string());
        }
        var
    }

    // unused: generic_name_in_scope (removed)

    pub(crate) fn insert_env(&self, name: String, entry: EnvEntry) {
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.insert(name, entry);
        }
    }

    pub(crate) fn replace_env_entry(&self, name: &str, entry: EnvEntry) {
        for scope in self.inner.borrow_mut().env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), entry);
                return;
            }
        }
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.insert(name.to_string(), entry);
        }
    }

    pub(crate) fn enter_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.current_level += 1;
        inner.env.push(HashMap::new());
        inner.generic_scopes.push(HashSet::new());
        inner.module_aliases.push(HashMap::new());
        inner.symbol_aliases.push(HashMap::new());
        inner.context_env.push(Vec::new());
    }

    pub(crate) fn exit_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.pop();
        inner.generic_scopes.pop();
        inner.module_aliases.pop();
        inner.symbol_aliases.pop();
        inner.context_env.pop();
        if inner.current_level > 0 {
            inner.current_level -= 1;
        }
    }

    pub(crate) fn push_module_path(&self, name: &str) {
        self.inner.borrow_mut().module_path.push(name.to_string());
    }

    pub(crate) fn pop_module_path(&self) {
        let _ = self.inner.borrow_mut().module_path.pop();
    }

    pub(crate) fn record_module_def(&self, name: &str) {
        let mut inner = self.inner.borrow_mut();
        let path = inner.module_path.with_segment(name.to_string());
        inner.module_defs.insert(path);
        if inner.module_path.is_empty() {
            inner.root_modules.insert(name.to_string());
        }
    }

    pub(crate) fn qualified_name(&self, name: &str) -> Option<QualifiedPath> {
        if self.inner.borrow().module_path.is_empty() {
            None
        } else {
            Some(
                self.inner
                    .borrow()
                    .module_path
                    .with_segment(name.to_string()),
            )
        }
    }

    pub(crate) fn insert_struct_def(&self, name: &Ident, def: TypeStruct) {
        let key = if self.inner.borrow().module_path.is_empty() {
            QualifiedPath::new(vec![name.as_str().to_string()])
        } else {
            self.inner
                .borrow()
                .module_path
                .with_segment(name.as_str().to_string())
        };
        self.own_struct_defs_mut().insert(key, def);
    }

    pub(crate) fn insert_enum_def(&self, name: &Ident, def: TypeEnum) {
        let key = if self.inner.borrow().module_path.is_empty() {
            QualifiedPath::new(vec![name.as_str().to_string()])
        } else {
            self.inner
                .borrow()
                .module_path
                .with_segment(name.as_str().to_string())
        };
        self.own_enum_defs_mut().insert(key, def);
    }

    /// Normalize a `DefType` RHS's resolved type into what gets bound under
    /// `name`: a structural type is materialized as a named struct, and a
    /// struct from `TypeBuilder::from(SourceType)` has `SourceType`'s fields
    /// merged in. Shared between the const-block and plain-alias paths in
    /// the `ItemKind::DefType` arm of `infer_item_inner`.
    pub(crate) async fn normalize_deftype_value(&self, name: &Ident, resolved: Ty) -> Ty {
        match resolved {
            Ty::Structural(structural) => {
                let struct_ty = TypeStruct {
                    name: name.clone(),
                    generics_params: Vec::new(),
                    repr: ReprOptions::default(),
                    fields: structural.fields.clone(),
                };
                self.insert_struct_def(name, struct_ty.clone());
                Ty::Struct(struct_ty)
            }
            Ty::Struct(struct_ty) => {
                let mut merged_ty = struct_ty;
                // Merge fields from source struct for TypeBuilder::from(Type)
                if merged_ty.name != *name {
                    let source_name = QualifiedPath::new(vec![merged_ty.name.as_str().to_string()]);
                    let source_def = self.own_struct_defs().get(&source_name).cloned();
                    match source_def {
                        Some(source_def) => {
                            let mut merged = source_def.fields.clone();
                            for f in &merged_ty.fields {
                                if !merged.iter().any(|m| m.name == f.name) {
                                    merged.push(f.clone());
                                }
                            }
                            merged_ty.fields = merged;
                            merged_ty.name = name.clone();
                        }
                        None => {
                            // Don't silently continue with only the new
                            // fields — that would materialize an incomplete
                            // struct for callers to build against. Tell
                            // apart "source's package hasn't loaded yet"
                            // (suspend, then retry the lookup once loaded)
                            // from "source genuinely doesn't exist" (real
                            // error).
                            let registered = source_name
                                .head()
                                .is_some_and(|head| self.typing_ctx.env_ctx.is_registered(head));
                            if registered {
                                self.await_package(source_name.head().unwrap()).await;
                                let found = self.own_struct_defs().get(&source_name).cloned();
                                if let Some(source_def) = found {
                                    let mut merged = source_def.fields.clone();
                                    for f in &merged_ty.fields {
                                        if !merged.iter().any(|m| m.name == f.name) {
                                            merged.push(f.clone());
                                        }
                                    }
                                    merged_ty.fields = merged;
                                    merged_ty.name = name.clone();
                                    self.insert_struct_def(name, merged_ty.clone());
                                    return Ty::Struct(merged_ty);
                                }
                            }
                            self.emit_error(format!(
                                "unknown source type `{}` for type alias `{}`",
                                merged_ty.name.as_str(),
                                name.as_str()
                            ));
                            return Ty::Unknown(TypeUnknown);
                        }
                    }
                }
                self.insert_struct_def(name, merged_ty.clone());
                Ty::Struct(merged_ty)
            }
            Ty::Enum(enum_ty) => {
                self.insert_enum_def(name, enum_ty.clone());
                Ty::Enum(enum_ty)
            }
            other => other,
        }
    }

    pub(crate) fn parent_module_path(&self) -> QualifiedPath {
        self.inner
            .borrow()
            .module_path
            .parent_n(1)
            .unwrap_or_else(|| QualifiedPath::new(Vec::new()))
    }
}
