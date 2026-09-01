use super::*;

impl AstToHirLowerer {
    pub(super) fn prepare_lowering_state(&mut self) {
        let provider_prelude = self
            .workspace
            .crates()
            .iter()
            .find_map(|(package_id, package)| {
                (package.borrow().hir_package_id == self.package_id).then(|| package_id)
            })
            .and_then(|package_id| self.workspace.package_metadata(package_id))
            .and_then(|metadata| metadata.prelude)
            .map(|package_id| hir::PackageId::new(package_id.as_str()));
        if provider_prelude.is_some() {
            self.package.prelude = provider_prelude;
        }
        self.workspace.reset_local_scope();
        self.module_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        self.current_owner = None;
        self.local_id = 0;
        self.current_position = 0;
        self.trait_defs.clear();
        self.trait_def_modules.clear();
        self.structural_value_defs.clear();
        self.const_list_length_scopes.clear();
        self.const_list_length_scopes.push(HashMap::new());
        self.synthetic_items.clear();
        self.package.dependencies = self
            .workspace
            .crates()
            .iter()
            .find(|(_, package)| package.borrow().hir_package_id == self.package_id)
            .map(|(_, package)| {
                let package = package.borrow();
                package
                    .graph
                    .package(&package.package_id)
                    .map(|descriptor| {
                        descriptor
                            .metadata
                            .dependencies
                            .iter()
                            .filter_map(|dependency| dependency.resolved_package_id.as_ref())
                            .map(|id| hir::PackageId::new(id.as_str()))
                            .collect()
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        // Keep predeclared struct fields available for struct update lowering.
    }

    pub(super) fn load_default_prelude_defs(&mut self) {}
    pub(super) fn predeclare_items(&mut self, items: &[ast::Item], tolerant: bool) -> Result<()> {
        for item in items {
            if !self.item_enabled_by_cfg(item) {
                continue;
            }
            if should_drop_quote_item(item) {
                continue;
            }
            if should_drop_const_type_item(item) {
                continue;
            }
            match item.kind() {
                ItemKind::Module(module) => {
                    self.allocate_def_id_for_item(item);
                    self.record_module_def(module.name.as_str());
                    self.push_module_scope(&module.name.name, &module.visibility);
                    self.predeclare_items(&module.items, tolerant)?;
                    self.pop_module_scope();
                }
                ItemKind::DefConst(def_const) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_const.name.name, def_id, &def_const.visibility);
                }
                ItemKind::DefStatic(def_static) if attrs_has_name(&def_static.attrs, "host") => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_static.name.name, def_id, &def_static.visibility);
                }
                ItemKind::DeclStatic(decl) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&decl.name.name, def_id, &ast::Visibility::Public);
                }
                ItemKind::DefStruct(def_struct) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_struct.name.name,
                        def_id.clone(),
                        &def_struct.visibility,
                    );
                    self.register_value_def(
                        &def_struct.name.name,
                        def_id.clone(),
                        &def_struct.visibility,
                    );
                    if attrs_has_name(&def_struct.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id.clone());
                    }
                    self.struct_field_defs
                        .insert(def_id, def_struct.value.fields.clone());
                }
                ItemKind::DefStructural(def_structural) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_structural.name.name,
                        def_id.clone(),
                        &def_structural.visibility,
                    );
                    self.register_value_def(
                        &def_structural.name.name,
                        def_id.clone(),
                        &def_structural.visibility,
                    );
                    if attrs_has_name(&def_structural.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id.clone());
                    }
                    self.struct_field_defs
                        .insert(def_id, def_structural.value.fields.clone());
                }
                ItemKind::OpaqueType(opaque_def) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &opaque_def.name.name,
                        def_id.clone(),
                        &opaque_def.visibility,
                    );
                    self.struct_field_defs.insert(def_id, Vec::new());
                }
                ItemKind::DefEnum(def_enum) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_enum.name.name,
                        def_id.clone(),
                        &def_enum.visibility,
                    );
                    if attrs_has_name(&def_enum.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }

                    let enum_op_class =
                        fp_core::intrinsics::extract_op_attr(&def_enum.attrs, "class");
                    for variant in &def_enum.value.variants {
                        let variant_def_id = self.next_def_id();
                        if let Some(tag) =
                            fp_core::intrinsics::extract_op_attr(&variant.attrs, "variant")
                        {
                            let op = enum_op_class.as_deref().and_then(|class| {
                                self.lowering_config
                                    .operations
                                    .resolve(&fp_core::lang::member_operation_key(class, &tag))
                            });
                            if let Some(op) = op {
                                self.package.op_defs.insert(variant_def_id.clone(), op);
                            }
                        }

                        let variant_path = fp_core::ast::path::QualifiedPath::new(vec![
                            def_enum.name.name.clone(),
                            variant.name.name.clone(),
                        ]);
                        let qualified_variant = variant_path.to_key();
                        let fully_qualified = if self.module_path.is_empty() {
                            qualified_variant.clone()
                        } else {
                            self.module_path.join(&variant_path.segments).to_key()
                        };
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id.clone(),
                            &def_enum.visibility,
                        );
                        self.enum_variant_def_ids
                            .insert(fully_qualified, variant_def_id);
                    }
                }
                ItemKind::DefFunction(def_fn) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_fn.name.name, def_id, &def_fn.visibility);
                }
                ItemKind::DeclFunction(decl_fn) => {
                    // Body-less `extern "C" fn foo(...);` declarations (e.g.
                    // the embedded libc package's platform bindings) must be
                    // registered here, not left to `append_item` (STEP 4),
                    // so that STEP 2's import resolution (in particular glob
                    // re-exports like `pub use macos::*;`) can already see
                    // them when it enumerates the module tree's value
                    // bindings.
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&decl_fn.name.name, def_id, &ast::Visibility::Public);
                }
                ItemKind::DefTrait(def_trait) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_trait.name.name,
                        def_id.clone(),
                        &def_trait.visibility,
                    );
                    if attrs_has_name(&def_trait.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }
                    self.trait_defs
                        .insert(def_trait.name.name.clone(), def_trait.clone());
                    self.trait_def_modules
                        .insert(def_trait.name.name.clone(), self.module_path.clone());
                    for trait_item in &def_trait.items {
                        self.allocate_def_id_for_item(trait_item);
                    }
                }
                ItemKind::DefType(def_type) => {
                    // Every Rust type alias has a definition identity, even
                    // when its target is transparent.  Keep the AST alias
                    // table for substitution, but also publish the normal
                    // type binding so dependent crates resolve it through
                    // the HIR definition graph like rustc does.
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_type.name.name,
                        def_id.clone(),
                        &def_type.visibility,
                    );
                    if let Some(materialized) = self.materialized_type_alias(def_type) {
                        if attrs_has_name(&def_type.attrs, "unimplemented") {
                            self.unimplemented_type_def_ids.insert(def_id.clone());
                        }
                        match materialized {
                            MaterializedTypeAlias::Struct(struct_ty) => {
                                self.struct_field_defs
                                    .insert(def_id, struct_ty.fields.clone());
                            }
                            MaterializedTypeAlias::Structural(structural) => {
                                self.struct_field_defs
                                    .insert(def_id, structural.fields.clone());
                            }
                            MaterializedTypeAlias::Enum(enum_ty) => {
                                for variant in &enum_ty.variants {
                                    let variant_def_id = self.next_def_id();

                                    let variant_path =
                                        fp_core::ast::path::QualifiedPath::new(vec![
                                            def_type.name.name.clone(),
                                            variant.name.name.clone(),
                                        ]);
                                    let qualified_variant = variant_path.to_key();
                                    let fully_qualified = if self.module_path.is_empty() {
                                        qualified_variant.clone()
                                    } else {
                                        self.module_path.join(&variant_path.segments).to_key()
                                    };
                                    self.register_value_def(
                                        &variant.name.name,
                                        variant_def_id.clone(),
                                        &def_type.visibility,
                                    );
                                    self.enum_variant_def_ids
                                        .insert(fully_qualified, variant_def_id);
                                }
                            }
                        }
                    } else {
                        // A transparent alias (`type __darwin_useconds_t =
                        // __uint32_t;`, `type Result<T> = ...;`) — the
                        // aliased type isn't a fresh struct/enum/structural
                        // this declaration itself introduces, so there's no
                        // new nominal HIR item to build. HIR has no
                        // first-class "type alias" item at all, so without
                        // this branch the alias's own name would never be
                        // given a `DefId`/`Res::Def` and could never resolve
                        // anywhere it's referenced — real Rust code (std's
                        // own `pub type Result<T> = ..`-style aliases,
                        // nearly every libc typedef) uses this shape
                        // constantly. Give it a real `DefId` (so qualified
                        // lookups like `macos::useconds_t` still work
                        // exactly like a materializing alias's would), and
                        // record its already-lowered target `TypeExpr` so
                        // `path_ty` can expand it in place at every use.
                        //
                        // Same timing hazard as `ItemKind::Impl` below: a
                        // module-qualified RHS (`type Result = result::
                        // Result<(), Error>;`) needs `result` already
                        // resolved as an import, but this whole method
                        // (STEP 1) runs strictly before STEP 2's import
                        // resolution — so defer exactly the same way,
                        // checked non-mutating before the first mutation.
                        // A comptime-producing alias is lowered as an
                        // expression-bearing type query.  Its expression
                        // may use an import even when its outer syntax is a
                        // `const { ... }` block, so checking only the first
                        // name segment cannot detect the dependency.  Wait
                        // until the import fixed point has run, just as we
                        // do for an ordinary module-qualified alias RHS.
                        let defer = false;
                        if !defer {
                            let def_id = self.allocate_def_id_for_item(item);
                            self.register_type_def(
                                &def_type.name.name,
                                def_id.clone(),
                                &def_type.visibility,
                            );
                            // A type alias whose RHS is a const block can
                            // evaluate to a concrete ADT. Bind the alias in
                            // the value namespace too so a following
                            // `Alias { ... }` literal carries this DefId into
                            // type checking and MIR lowering; its concrete
                            // shape still comes solely from the checked
                            // const-block result.
                            if comptime_type_alias_rhs(&def_type.value).is_some() {
                                self.register_value_def(
                                    &def_type.name.name,
                                    def_id.clone(),
                                    &def_type.visibility,
                                );
                            }
                            let _ = self.transform_type_to_hir(&def_type.value)?;
                        }
                    }
                }
                ItemKind::Impl(_) => {
                    let ItemKind::Impl(impl_block) = item.kind() else {
                        unreachable!();
                    };
                    // Only single-segment bare names (`Vec`, not
                    // `crate::vec::Vec` or a blanket `T`) can plausibly be
                    // waiting on an import that hasn't been processed yet
                    // — everything else keeps today's immediate behavior,
                    // including its immediate failure modes. Checked via
                    // `resolve_type_symbol` (non-mutating) *before* the
                    // first mutation below (`allocate_def_id_for_item`),
                    // so a deferred item has made zero state changes and
                    // is safe to fully re-run later, unmodified.
                    let defer = false;
                    if !defer {
                        self.allocate_def_id_for_item(item);
                        // A self-type can be permanently unresolvable — not a
                        // timing issue an import-order retry would fix, but a
                        // genuine dead end (e.g. its target type lives in a
                        // module that failed to parse in the first place, so
                        // no amount of import resolution will ever find it).
                        // Skip just this one impl rather than aborting HIR
                        // generation for the whole package — the same
                        // "tolerate what's broken, keep what isn't" policy
                        // already applied at the file level (parse errors).
                        self.push_type_scope();
                        for param in &impl_block.generics_params {
                            let def_id = self.next_def_id();
                            self.register_type_generic(&param.name.name, def_id);
                        }
                        let self_path = match self
                            .ast_expr_to_hir_path(&impl_block.self_ty, PathResolutionScope::Type)
                        {
                            Ok(path) => path,
                            Err(error) => {
                                self.pop_type_scope();
                                tracing::debug!(
                                    module = %self.module_path.to_key(),
                                    %error,
                                    "skipping unsupported impl during tolerant predeclaration",
                                );
                                continue;
                            }
                        };
                        let mut method_path = match self.canonical_type_path(&self_path) {
                            Ok(path) => path.segments,
                            Err(error) => {
                                self.pop_type_scope();
                                tracing::debug!(
                                    module = %self.module_path.to_key(),
                                    %error,
                                    "skipping unsupported impl during tolerant predeclaration",
                                );
                                continue;
                            }
                        };
                        self.pop_type_scope();
                        // `impl Vec<&str> { fn join }` / `impl Vec<String> {
                        // fn join }` are two genuinely different,
                        // already-concrete methods (this impl declares no
                        // generic parameter of its own to substitute), but
                        // `canonical_type_path` only ever returns the base
                        // struct's nominal path ("std::alloc::Vec") --
                        // dropping the impl's own concrete generic
                        // arguments entirely. Left alone, both compute the
                        // identical qualified path "std::alloc::Vec::join",
                        // colliding once both reach the same LIR workspace
                        // ("duplicate LIR artifact `Vec__join`"). A truly
                        // generic impl (`impl<T> Vec<T> { fn push }`) is
                        // unaffected — it has its own generic params, so
                        // this check is skipped; its per-call-site
                        // specializations are disambiguated further
                        // downstream instead (hir_to_mir's
                        // `specialization_suffix`, keyed off the
                        // *substituted* type, not the impl itself).
                        if impl_block.generics_params.is_empty() {
                            if let Some(args) = self_path
                                .segments
                                .last()
                                .and_then(|segment| segment.args.as_ref())
                                .filter(|args| !args.args.is_empty())
                            {
                                use std::hash::{Hash, Hasher};
                                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                                for arg in &args.args {
                                    format!("{:?}", arg).hash(&mut hasher);
                                }
                                if let Some(last) = method_path.last_mut() {
                                    last.push_str(&format!("_spec_{:x}", hasher.finish()));
                                }
                            }
                        }
                        for impl_item in &impl_block.items {
                            match impl_item.kind() {
                                ast::ItemKind::DefFunction(_) => {
                                    self.allocate_def_id_for_item(impl_item);
                                }
                                ast::ItemKind::DefConst(_) => {
                                    self.allocate_def_id_for_item(impl_item);
                                }
                                _ => continue,
                            }
                        }
                    } else {
                        // Collection still assigns stable identities to the
                        // impl and its members. Header resolution is owned by
                        // the AST resolver before lowering.
                        self.allocate_def_id_for_item(item);
                        for impl_item in &impl_block.items {
                            if matches!(
                                impl_item.kind(),
                                ast::ItemKind::DefFunction(_) | ast::ItemKind::DefConst(_)
                            ) {
                                self.allocate_def_id_for_item(impl_item);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}
