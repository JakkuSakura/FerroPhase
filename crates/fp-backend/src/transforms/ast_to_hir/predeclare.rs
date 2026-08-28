use super::*;

impl AstToHirLowerer {
    fn bind_prelude_binding(
        package: &mut hir::HirPackage,
        prelude_module: hir::resolve::ModuleId,
        name: &str,
        namespace: hir::Namespace,
        entry: hir::SymbolEntry,
    ) {
        package.module_tree.bind(
            prelude_module,
            namespace,
            name,
            entry,
        );
    }

    pub(super) fn prepare_lowering_state(&mut self) {
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        self.module_visibility.clear();
        self.module_visibility.push(true);
        self.current_owner = None;
        self.local_id = 0;
        self.current_position = 0;
        self.type_aliases.clear();
        self.trait_defs.clear();
        self.trait_def_modules.clear();
        self.structural_value_defs.clear();
        self.const_list_length_scopes.clear();
        self.const_list_length_scopes.push(HashMap::new());
        self.synthetic_items.clear();
        self.package.module_tree = hir::resolve::ModuleTree::new();
        self.package.dependencies = self
            .workspace
            .as_ref()
            .and_then(|workspace| {
                workspace
                    .crates()
                    .iter()
                    .find(|(_, package)| {
                        package.borrow().hir_package_id == self.package_id
                    })
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
                                    .filter_map(|dependency| {
                                        dependency.resolved_package_id.as_ref()
                                    })
                                    .map(|id| hir::PackageId::new(id.as_str()))
                                    .collect()
                            })
                            .unwrap_or_default()
                    })
            })
            .unwrap_or_default();
        self.pending_type_aliases.clear();
        self.pending_impls.clear();
        self.resolved_import_aliases.clear();
        // Keep predeclared struct fields available for struct update lowering.
    }

    pub(super) fn load_default_prelude_defs(&mut self) {
        // This package's own module tree only ever holds *this* module's
        // own declarations. Prelude bindings are installed at the symbol
        // registration boundary, where their real module path, namespace,
        // visibility, and resolved `Res` are still available.
        // rustc installs the prelude of the selected standard library crate
        // through crate metadata. The HIR package records those resolved
        // dependency edges, so unrelated packages loaded in the same session
        // cannot leak bare names into this crate.
        // The provider reports rustc's implicit `std` edge in ordinary
        // Rust package metadata. Do not infer a prelude from whatever
        // unrelated sysroot packages happen to be loaded: `libc`, for
        // example, is not a Rust crate with a core prelude merely because
        // core is present in the shared HIR program.
        let selected_prelude = match self.package_id.as_str() {
            // A sysroot crate has the same implicit prelude while its own
            // source is being resolved. In particular, core and alloc have
            // no dependency edge advertising their own prelude, and std must
            // not accidentally select core merely because core is one of
            // std's dependencies. The source crate's prelude is already in
            // this lowerer's module tree; it is not published in the shared
            // HIR program until transform_package returns.
            "core" | "std" => Some(self.package_id.clone()),
            _ => self
                .package
                .dependencies
                .iter()
                .find(|dependency| dependency.as_str() == "std")
                .cloned(),
        };
        if let Some(dependency_id) = selected_prelude {
            if dependency_id == self.package.id {
                return;
            }
            let Some(hir_package) = self.hir_program.package(&dependency_id) else {
                return;
            };
            let prelude_module = self.package.module_tree.prelude();

            // This is the HIR equivalent of rustc's crate metadata for the
            // selected prelude. The reserved prelude node carries the
            // namespace and resolved DefId of every binding, including
            // re-exports and enum constructors. Consume it directly instead
            // of reconstructing namespaces from flattened export keys or
            // looking up the defining item, both of which lose information at
            // the crate boundary.
            for namespace in [hir::Namespace::Type, hir::Namespace::Value] {
                let bindings = hir_package
                    .module_tree
                    .prelude_bindings(namespace)
                    .map(|(name, entry)| (name.to_owned(), entry.clone()))
                    .collect::<Vec<_>>();
                for (name, entry) in bindings {
                    if self
                        .package
                        .module_tree
                        .lookup(prelude_module, namespace, &name)
                        .is_none()
                    {
                        Self::bind_prelude_binding(
                            &mut self.package,
                            prelude_module,
                            &name,
                            namespace,
                            entry,
                        );
                    }
                }
            }
        }
    }
    /// The current, real mechanism for merging every workspace dependency's
    /// own `def_map`/`def_paths`/`op_defs`/`intrinsic_defs` into this
    /// package's own `program` — called at `:1726`/`:1877`, and depended on
    /// by `hir_to_mir::HirToMirLowerer`'s cross-package lookups (`hir_def_map`,
    /// documented at `hir_to_mir/expr.rs`) as well as `fp-typing`'s own
    /// same-package/cross-package resolution. Not legacy code awaiting
    /// deletion — a future "one shared program, not copied per package"
    /// redesign remains a real architectural option, but until that lands
    /// this is the only mechanism that makes cross-package references work
    /// at all.
    pub(super) fn seed_workspace_definitions(&mut self, program: &mut hir::HirPackage) {
        fn normalize_external_path(
            package_id: &hir::PackageId,
            segments: Vec<String>,
        ) -> Vec<String> {
            hir::HirProgram::canonical_external_path(
                package_id,
                &segments.join("::"),
            )
            .split("::")
            .map(str::to_owned)
            .collect()
        }

        for hir_program in self.hir_program.packages.values() {
            // Rust's extern-prelude spelling normalizes Cargo package names
            // (`skln-core` is imported as `skln_core`). Keep published
            // definition paths consistent with module-tree export keys and
            // `HirProgram::find_export` without cloning the dependency's
            // complete item map.
            for (def_id, def_path) in &hir_program.def_paths {
                let segments = def_path
                    .segments
                    .iter()
                    .map(|segment| segment.as_str().to_owned())
                    .collect::<Vec<_>>();
                let segments = normalize_external_path(&hir_program.id, segments);
                let normalized = hir::DefPath::new(
                    segments.into_iter().map(hir::Symbol::new).collect(),
                );
                program.def_paths.insert(def_id.clone(), normalized.clone());
                self.package.def_paths.insert(def_id.clone(), normalized);
            }
            program.op_defs.extend(hir_program.op_defs.clone());
            program
                .intrinsic_defs
                .extend(hir_program.intrinsic_defs.clone());
            program
                .type_alias_targets
                .extend(hir_program.type_alias_targets.clone());
        }
        // Publish dependency exports into the same qualified module tree used
        // by local resolution. This is the resolver-equivalent of rustc's
        // extern prelude: a path such as `dep::module::PublicType` must be
        // walkable segment by segment during import resolution, before any
        // later direct export fallback is considered. Prefix exports that are
        // stored relative to their package root, while preserving already
        // canonical paths from providers that include that root.
        for (_module_path, hir_package, _exports) in self.hir_program.hir_definitions() {
            // A module is part of the dependency's public namespace even if
            // it has no exported child symbol. Preserve every real module
            // path under the normalized extern-prelude crate root so paths
            // such as `std::fmt` resolve through the same tree walk as local
            // modules.
            for module_path in hir_package
                .module_tree
                .all_paths()
                .filter(|path| hir_package.module_tree.module_exists(path))
            {
                let segments = module_path
                    .segments
                    .iter()
                    .map(|segment| segment.to_owned())
                    .collect::<Vec<_>>();
                let segments = normalize_external_path(&hir_package.id, segments);
                self.package
                    .module_tree
                    .ensure_module(&fp_core::ast::path::QualifiedPath::new(segments));
            }
            // `ModuleTree::public_bindings` is the resolver metadata boundary:
            // it carries the defining path, namespace, visibility, and Res as
            // one record.  Do not infer a namespace from a DefId's item kind;
            // re-exports and definition namespaces do not preserve that
            // correspondence.
            for (source_path, namespace, mut entry) in hir_package
                .module_tree
                .public_bindings()
            {
                let segments = source_path
                    .segments
                    .iter()
                    .map(|segment| segment.to_owned())
                    .collect::<Vec<_>>();
                let segments = normalize_external_path(&hir_package.id, segments);
                let Some(leaf) = segments.last().cloned() else {
                    continue;
                };
                let res = match entry.res {
                    hir::Res::Module(module_path) => hir::Res::Module(
                        normalize_external_path(&hir_package.id, module_path),
                    ),
                    res => res,
                };
                entry.res = res.clone();
                entry.path = Some(fp_core::ast::path::QualifiedPath::new(segments.clone()));
                let path = fp_core::ast::path::QualifiedPath::new(segments.clone());
                let prefix = fp_core::ast::path::QualifiedPath::new(
                    path.segments[..path.segments.len() - 1].to_vec(),
                );
                // `prefix` is usually a real module, but it is a definition
                // namespace for associated items such as `Vec::new` or
                // `RefNode::WorkingTree`.  Preserve that distinction across
                // the crate boundary: marking every prefix as a real module
                // makes the resolver prefer `Res::Module` over the nominal
                // `Res::Def` at the same path.
                let module = self.package.module_tree.ensure_namespace(&prefix);
                if matches!(res, hir::Res::Module(_)) {
                    self.package.module_tree.ensure_module(&path);
                    // Preserve a public module re-export as a binding at its
                    // parent. Import resolution follows this canonical
                    // `Res::Module` target, as rustc does, instead of
                    // treating only the textual path as a module.
                    for namespace in [hir::Namespace::Type, hir::Namespace::Value] {
                        if self
                            .package
                            .module_tree
                            .lookup(module, namespace, &leaf)
                            .is_none()
                        {
                            self.package.module_tree.bind(
                                module,
                                namespace,
                                &leaf,
                                hir::SymbolEntry {
                                    res: res.clone(),
                                    export: entry.export.clone(),
                                    path: entry.path.clone(),
                                },
                            );
                        }
                    }
                }
                if self
                    .package
                    .module_tree
                    .lookup(module, namespace, &leaf)
                    .is_none()
                {
                    self.package.module_tree.bind(
                        module,
                        namespace,
                        &leaf,
                        entry,
                    );
                }
            }
        }
        for path in self
            .workspace
            .as_ref()
            .map(|w| w.module_paths())
            .unwrap_or_default()
        {
            self.package.module_tree.ensure_module(&path);
        }
        // Cross-package `type X = Y;` aliases (e.g. `libc::char`) are
        // *not* eagerly copied in here either — `lookup_type_alias`/
        // `lookup_type_alias_with_key` fall back to `workspace.
        // find_type_alias` lazily on a local-lookup miss instead.
    }

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
                                fp_core::lang::class_and_member_to_portable_op(class, &tag)
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
                        // Record the `Enum::Variant`-qualified registration
                        // first so its more complete path wins the
                        // `def_paths` entry over the bare-name
                        // registration below (see the analogous comment in
                        // `transform_item_to_hir`'s `DefEnum` arm).
                        //
                        // Must go through `record_value_path` (which takes
                        // an already-split `QualifiedPath`), not
                        // `record_value_symbol` (which takes a bare `&str`
                        // name and appends it as a *single* module-tree
                        // segment via `qualify_path`/`with_segment`) — the
                        // latter turned the "::"-joined string
                        // `"Option::None"` into one literal segment named
                        // `"Option::None"` bound directly under module
                        // `option`, instead of a `None` binding under
                        // submodule `option::Option`. Any lookup that
                        // *splits* a qualified key into real segments
                        // (`register_import_binding`'s `Option::{self,
                        // None, Some}` resolution, `load_default_prelude_defs`'s
                        // scan) could then never find it — the actual root
                        // cause of every enum-variant-based prelude import
                        // (`Some`/`None`/`Ok`/`Err`) failing to resolve
                        // crate-wide.
                        self.record_value_path(
                            &self.module_path.join(&variant_path.segments),
                            hir::Res::Def(variant_def_id.clone()),
                            &def_enum.visibility,
                        );
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
                    self.register_type_alias(&def_type.name.name, &def_type.value);
                    if let Some(materialized) = self.materialized_type_alias(def_type) {
                        let def_id = self.allocate_def_id_for_item(item);
                        self.register_type_def(
                            &def_type.name.name,
                            def_id.clone(),
                            &def_type.visibility,
                        );
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
                                    self.record_value_path(
                                        &self.module_path.join(&variant_path.segments),
                                        hir::Res::Def(variant_def_id.clone()),
                                        &def_type.visibility,
                                    );
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
                        let defer = tolerant
                            && type_alias_rhs_first_segment_name(&def_type.value)
                                .map(|name| {
                                    self.resolve_type_symbol(name).is_none()
                                        && !is_primitive_type_name(name)
                                })
                                .unwrap_or(false);
                        if defer {
                            self.pending_type_aliases
                                .push((self.module_path.clone(), item.clone()));
                        } else {
                            let def_id = self.allocate_def_id_for_item(item);
                            self.register_type_def(
                                &def_type.name.name,
                                def_id.clone(),
                                &def_type.visibility,
                            );
                            let target = self.transform_type_to_hir(&def_type.value)?;
                            self.package.type_alias_targets.insert(def_id, target);
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
                    let defer = tolerant
                        && self_type_first_segment_name(&impl_block.self_ty)
                            .map(|name| {
                                let qualified = matches!(
                                    impl_block.self_ty.kind(),
                                    ast::ExprKind::Name(ast::Name::Path(path))
                                        if path.segments.len() > 1
                                );
                                !impl_block
                                    .generics_params
                                    .iter()
                                    .any(|param| param.name.name == name)
                                    && (qualified || self.resolve_type_symbol(name).is_none())
                                    && !is_primitive_type_name(name)
                            })
                            .unwrap_or(false);
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
                                tracing::warn!(
                                    "skipping impl with unresolvable self-type in {}: {error}",
                                    self.module_path.to_key(),
                                );
                                continue;
                            }
                        };
                        let mut method_path = match self.canonical_type_path(&self_path) {
                            Ok(path) => path.segments,
                            Err(error) => {
                                self.pop_type_scope();
                                tracing::warn!(
                                    "skipping impl with unresolvable self-type {:?} in {}: {error}",
                                    self_path,
                                    self.module_path.to_key(),
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
                                ast::ItemKind::DefFunction(function) => {
                                    let method_def_id = self.allocate_def_id_for_item(impl_item);
                                    method_path.push(function.name.name.clone());
                                    self.record_value_path(
                                        &fp_core::ast::path::QualifiedPath::new(
                                            method_path.clone(),
                                        ),
                                        hir::Res::Def(method_def_id),
                                        &function.visibility,
                                    );
                                    method_path.pop();
                                }
                                // An inherent/trait-impl associated const
                                // (`impl char { pub const MIN: char = ...;
                                // }`) — until this arm existed, this loop's
                                // exclusive `DefFunction` match silently
                                // skipped every associated const, so
                                // nothing outside the impl's own body
                                // (predeclare_items runs before any body is
                                // lowered) could ever resolve a reference
                                // to it (`char::MIN`) — not a timing/
                                // ordering issue an import retry could
                                // paper over, since the symbol was never
                                // registered *anywhere*, ever, regardless
                                // of reference order.
                                ast::ItemKind::DefConst(constant) => {
                                    let const_def_id = self.allocate_def_id_for_item(impl_item);
                                    method_path.push(constant.name.name.clone());
                                    self.record_value_path(
                                        &fp_core::ast::path::QualifiedPath::new(
                                            method_path.clone(),
                                        ),
                                        hir::Res::Def(const_def_id),
                                        &constant.visibility,
                                    );
                                    method_path.pop();
                                }
                                _ => continue,
                            }
                        }
                    } else {
                        self.pending_impls
                            .push((self.module_path.clone(), item.clone()));
                        // Collection still assigns stable identities to the
                        // impl and its members. Header resolution belongs to
                        // the later HIR transform, after imports are fixed,
                        // just as rustc resolves impl headers in its late
                        // resolver. Do not enqueue or replay the AST item.
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
