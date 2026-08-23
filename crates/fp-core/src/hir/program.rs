use super::*;

/// The whole compiled result — every package involved, keyed by
/// `PackageId`. `AstToHirLowerer` owns one of these and works package-by-package
/// against it (see `docs/Resolution.md`); resolution across an
/// already-compiled dependency package is a lookup into this same
/// structure, not a separate clone-and-merge pass.
///
/// Packages are `Rc`, not owned — building a `HirProgram` (e.g. a
/// `AstProgram` snapshotting its already-compiled dependency
/// packages, each already an `Rc<HirPackage>`, for a consumer like
/// `HirToMirLowerer` to dispatch cross-package `DefId` lookups against) is
/// then just a handful of `Rc` clones, never a deep clone of every
/// dependency's own items/def_map/def_paths.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct HirProgram {
    pub packages: HashMap<PackageId, std::rc::Rc<HirPackage>>,
    /// Direct name -> `DefId` lookup across every package, for well-known
    /// cross-package lookups by bare name (e.g. `fp_typing`'s well-known
    /// standard-library collection types) — maintained incrementally by
    /// `add_package`, never rescanned per query. First package added to
    /// declare a given name wins (add the current package last, after its
    /// dependencies, for "current package's own name shadows a
    /// dependency's" priority).
    struct_defs_by_name: HashMap<String, DefId>,
}

impl HirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
            struct_defs_by_name: HashMap::new(),
        }
    }

    pub fn package(&self, id: &PackageId) -> Option<&HirPackage> {
        self.packages.get(id).map(|package| package.as_ref())
    }

    /// Inserts `package`, merging its own `struct_defs_by_name` into this
    /// `HirProgram`'s direct lookup index in the same step — the
    /// incremental counterpart to re-deriving that index by scanning every
    /// package's items on every query.
    pub fn add_package(&mut self, package: std::rc::Rc<HirPackage>) {
        for (name, def_id) in &package.struct_defs_by_name {
            self.struct_defs_by_name.entry(name.clone()).or_insert_with(|| def_id.clone());
        }
        self.packages.insert(package.id.clone(), package);
    }

    /// O(1) direct lookup — no package iteration — for a struct declared
    /// under `name` in any package this `HirProgram` knows about.
    pub fn struct_def_id(&self, name: &str) -> Option<DefId> {
        self.struct_defs_by_name.get(name).cloned()
    }

    /// Every item across every package this `HirProgram` knows about — for
    /// callers that genuinely need the full set (e.g. a one-time reverse
    /// index build), not a single `DefId` lookup.
    pub fn all_items(&self) -> impl Iterator<Item = &Item> {
        self.packages.values().flat_map(|package| package.items.iter())
    }

    /// A definition's fully-qualified path, wherever its owning package
    /// lives — routes to that package's own `def_paths` via the `DefId`'s
    /// own `package_id`, so a caller never has to know or track which
    /// package a `DefId` came from before asking this question.
    pub fn def_path(&self, def_id: DefId) -> Option<&DefPath> {
        self.package(&def_id.package_id)?.def_paths.get(&def_id)
    }

    /// A transparent type alias's expansion target — see
    /// `HirPackage::type_alias_targets`'s doc comment for why this table
    /// exists at all.
    pub fn type_alias_target(&self, def_id: DefId) -> Option<&TypeExpr> {
        self.package(&def_id.package_id)?
            .type_alias_targets
            .get(&def_id)
    }

    pub fn item(&self, def_id: DefId) -> Option<&Item> {
        self.package(&def_id.package_id)?.def_map.get(&def_id)
    }

    /// Cross-package counterpart of `HirPackage::member_owner` — routes to
    /// `def_id`'s own package via its `package_id`, so a caller never has
    /// to know or track which package a member `DefId` came from first.
    pub fn member_owner(&self, def_id: DefId) -> Option<DefId> {
        self.package(&def_id.package_id)?.member_owner(def_id)
    }

    /// Cross-package counterpart of `HirPackage::checked_impl_self_ty`.
    pub fn checked_impl_self_ty(&self, hir_id: HirId) -> Option<Ty> {
        self.package(&hir_id.package_id)?.checked_impl_self_ty(hir_id)
    }

    pub fn cache_checked_impl_self_ty(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.cache_checked_impl_self_ty(hir_id, ty);
        }
    }

    /// Cross-package counterpart of `HirPackage::function_signature`.
    pub fn function_signature(&self, hir_id: HirId) -> Option<Ty> {
        self.package(&hir_id.package_id)?.function_signature(hir_id)
    }

    pub fn cache_function_signature(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.cache_function_signature(hir_id, ty);
        }
    }

    /// Cross-package counterpart of `HirPackage::resolved_trait_def`.
    pub fn resolved_trait_def(&self, def_id: DefId) -> Option<std::rc::Rc<Trait>> {
        self.package(&def_id.package_id)?.resolved_trait_def(def_id)
    }

    pub fn cache_resolved_trait_def(&self, def_id: DefId, trait_def: std::rc::Rc<Trait>) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.cache_resolved_trait_def(def_id, trait_def);
        }
    }

    /// Cross-package counterpart of `HirPackage::refinement_hint`.
    pub fn refinement_hint(&self, hir_id: HirId, slot: ParamSlot) -> Option<RefinementHint> {
        self.package(&hir_id.package_id)?.refinement_hint(hir_id, slot)
    }

    pub fn insert_refinement_hint(&self, hir_id: HirId, slot: ParamSlot, hint: RefinementHint) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.insert_refinement_hint(hir_id, slot, hint);
        }
    }

    /// Cross-package counterpart of `HirPackage::take_raw_refinement_hint`.
    /// Cross-package use is not actually expected here (a raw hint is
    /// always taken by the same package's own in-progress check, right
    /// after `check_type_expr` populates it), but routes through
    /// `hir_id.package_id` anyway for consistency with every other
    /// per-`HirId` accessor on this type.
    pub fn take_raw_refinement_hint(&self, hir_id: HirId) -> Option<RefinementHint> {
        self.package(&hir_id.package_id)?.take_raw_refinement_hint(hir_id)
    }

    pub fn insert_raw_refinement_hint(&self, hir_id: HirId, hint: RefinementHint) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.insert_raw_refinement_hint(hir_id, hint);
        }
    }

    /// Cross-package counterpart of `HirPackage::literal_type_hint`.
    pub fn literal_type_hint(&self, hir_id: HirId) -> Option<Vec<String>> {
        self.package(&hir_id.package_id)?.literal_type_hint(hir_id)
    }

    pub fn insert_literal_type_hint(&self, hir_id: HirId, literals: Vec<String>) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.insert_literal_type_hint(hir_id, literals);
        }
    }

    /// Cross-package counterpart of `HirPackage::local_struct_fields`.
    pub fn local_struct_fields(&self, def_id: DefId) -> Option<Vec<(Symbol, Ty)>> {
        self.package(&def_id.package_id)?.local_struct_fields(def_id)
    }

    pub fn insert_local_struct_fields(&self, def_id: DefId, fields: Vec<(Symbol, Ty)>) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.insert_local_struct_fields(def_id, fields);
        }
    }

    // --- Typed results (formerly `PackageTypes`/`ProgramTypes`) -----------
    //
    // Cross-package counterparts of `HirPackage`'s own single-entry
    // get/record pairs, routed by the `HirId`/`DefId`'s own `package_id` —
    // same convention as `member_owner`/`checked_impl_self_ty` above. A
    // record call against a package this `HirProgram` doesn't know about is
    // silently a no-op (mirrors `cache_checked_impl_self_ty`'s shape); every
    // real caller already owns the package it's recording against.

    pub fn expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package(&hir_id.package_id)?.expr_type(hir_id)
    }

    pub fn record_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_expr_type(hir_id, ty);
        }
    }

    pub fn type_expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package(&hir_id.package_id)?.type_expr_type(hir_id)
    }

    pub fn record_type_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_type_expr_type(hir_id, ty);
        }
    }

    pub fn pat_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package(&hir_id.package_id)?.pat_type(hir_id)
    }

    pub fn record_pat_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_pat_type(hir_id, ty);
        }
    }

    pub fn method_resolution(&self, hir_id: HirId) -> Option<DefId> {
        self.package(&hir_id.package_id)?.method_resolution(hir_id)
    }

    pub fn record_method_resolution(&self, hir_id: HirId, def_id: DefId) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_method_resolution(hir_id, def_id);
        }
    }

    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package(&hir_id.package_id)?.generic_call_arg(hir_id)
    }

    pub fn record_generic_call_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_generic_call_arg(hir_id, resolution);
        }
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package(&hir_id.package_id)?.generic_method_arg(hir_id)
    }

    pub fn record_generic_method_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package(&hir_id.package_id) {
            package.record_generic_method_arg(hir_id, resolution);
        }
    }

    pub fn const_type(&self, def_id: DefId) -> Option<Ty> {
        self.package(&def_id.package_id)?.const_type(def_id)
    }

    pub fn record_const_type(&self, def_id: DefId, ty: Ty) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.record_const_type(def_id, ty);
        }
    }

    pub fn const_value(&self, def_id: DefId) -> Option<Value> {
        self.package(&def_id.package_id)?.const_value(def_id)
    }

    pub fn record_const_value(&self, def_id: DefId, value: Value) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.record_const_value(def_id, value);
        }
    }

    pub fn const_block_value(&self, def_id: DefId) -> Option<Value> {
        self.package(&def_id.package_id)?.const_block_value(def_id)
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.record_const_block_value(def_id, value);
        }
    }

    pub fn op_def(&self, def_id: DefId) -> Option<&crate::intrinsics::PortableOp> {
        self.package(&def_id.package_id)?.op_defs.get(&def_id)
    }

    pub fn intrinsic_def(&self, def_id: DefId) -> Option<&CallKind> {
        self.package(&def_id.package_id)?.intrinsic_defs.get(&def_id)
    }

    pub fn is_placeholder_def(&self, def_id: DefId) -> bool {
        self.package(&def_id.package_id)
            .is_some_and(|package| package.placeholder_defs.contains(&def_id))
    }

    /// Every `impl` item (from any package) whose self-type resolves to
    /// `did` — an impl for a type can live in a different package than the
    /// type itself, so this unions every package's own
    /// `HirPackage::impls_by_self_did` rather than only looking in `did`'s
    /// own package. Each per-package lookup is still O(1); only the number
    /// of packages that actually declare a matching impl costs anything.
    pub fn impls_for_adt(&self, did: DefId) -> impl Iterator<Item = &Item> {
        self.packages.values().flat_map(move |package| {
            package
                .impls_by_self_did
                .get(&did)
                .into_iter()
                .flatten()
                .filter_map(move |impl_def_id| package.def_map.get(impl_def_id))
        })
    }

    /// Every `impl` item across every package — the fallback for a
    /// method-call/UFCS-call whose receiver type isn't a resolved ADT
    /// (so there's no `did` to key `impls_for_adt` by).
    pub fn all_impls(&self) -> impl Iterator<Item = &Item> {
        self.all_items()
            .filter(|item| matches!(item.kind, ItemKind::Impl(_)))
    }

    /// Resolves `path` (in namespace `ns`) starting from `from_module` in
    /// package `from`, falling through to another already-compiled
    /// package's own module tree when `path`'s root names a different
    /// package (mirrors how a real cross-crate path resolves — the target
    /// package's own tree, not the caller's).
    /// Resolves `name` (in namespace `ns`) as seen from `from_module` in
    /// package `from` — takes a plain module path, not a `ModuleId`:
    /// `ModuleId` is `ModuleTree`'s own internal node handle, never meant
    /// to leak past this API to a caller (fp-typing, hir_to_ast, ...) that
    /// has no reason to know the tree exists at all, only that it can ask
    /// the program a question about a path.
    /// Cross-package counterpart to an AST-level `find_struct`/`find_enum`,
    /// for a value/type symbol exported by some other package's
    /// `HirPackage::hir_exports` (e.g. `libc::macos::getenv`) — moved from
    /// the old `AstProgram::find_export`. Iterates packages in `PackageId`
    /// order for determinism (the old `AstProgram` version iterated in
    /// package-name-sorted order; this is a different but equally
    /// deterministic tie-break, acceptable since the old order was already
    /// somewhat arbitrary — first match on ambiguity, not a real priority
    /// rule).
    pub fn find_export(&self, key: &str) -> Option<Res> {
        let mut ids: Vec<_> = self.packages.keys().cloned().collect();
        ids.sort();
        for id in ids {
            if let Some(res) = self.packages[&id].hir_exports.get(key) {
                return Some(res.clone());
            }
        }
        None
    }

    /// `find_export` requires the caller's exact fully-qualified key — but
    /// a bare name (`Option`, `Some`) has no way to know which module of
    /// some OTHER package defines it. Scans every package's `hir_exports`
    /// for a key whose LAST path segment matches `name`.
    pub fn find_export_by_name(&self, name: &str) -> Option<Res> {
        let mut ids: Vec<_> = self.packages.keys().cloned().collect();
        ids.sort();
        for id in ids {
            for (key, res) in self.packages[&id].hir_exports.iter() {
                if key.rsplit("::").next().unwrap_or(key.as_str()) == name {
                    return Some(res.clone());
                }
            }
        }
        None
    }

    /// Same idea as `find_export_by_name`, but for a multi-segment suffix
    /// (e.g. `Option::Some`) instead of a single bare name.
    pub fn find_export_by_suffix(&self, suffix: &str) -> Option<Res> {
        let dotted_suffix = format!("::{suffix}");
        let mut ids: Vec<_> = self.packages.keys().cloned().collect();
        ids.sort();
        for id in ids {
            for (key, res) in self.packages[&id].hir_exports.iter() {
                if key == suffix || key.ends_with(&dotted_suffix) {
                    return Some(res.clone());
                }
            }
        }
        None
    }

    /// Every published package's own HIR plus its export table — moved
    /// from the old `AstProgram::hir_definitions` (which read
    /// `CompiledPackage::hir_program`/`hir_exports`; both now live here
    /// directly). `QualifiedPath::new(Vec::new())` is kept as the first
    /// tuple element purely to match that old signature's shape (every
    /// caller destructures it as `_module_path` and ignores it) rather
    /// than touching every call site's destructuring pattern too.
    pub fn hir_definitions(
        &self,
    ) -> Vec<(
        crate::ast::path::QualifiedPath,
        std::rc::Rc<HirPackage>,
        HashMap<String, Res>,
    )> {
        self.packages
            .values()
            .map(|package| {
                (
                    crate::ast::path::QualifiedPath::new(Vec::new()),
                    package.clone(),
                    package.hir_exports.clone(),
                )
            })
            .collect()
    }

    /// Cross-package counterpart to `hir_typeck::expr_path_ty`'s local
    /// associated-method fallback — finds the `impl` block and method whose
    /// `ImplItem::def_id` matches `def_id`. An inherent impl's items are
    /// always minted in the same package as the impl itself (the orphan
    /// rule's HIR-level consequence), so `def_id.package_id` already names
    /// the *only* package that could hold it. Moved from the old
    /// `AstProgram::find_hir_impl_method` (which additionally memoized this
    /// by `def_id` — dropped here since `HirProgram` has no interior
    /// mutability to cache into cheaply; revisit if this shows up as a real
    /// hot path again).
    pub fn find_hir_impl_method(
        &self,
        def_id: DefId,
    ) -> Option<(Generics, TypeExpr, Vec<ImplItem>, Function)> {
        let package = self.package(&def_id.package_id)?;
        let impl_def_id = package.impl_method_item_index.get(&def_id)?;
        let item = package.def_map.get(impl_def_id)?;
        let ItemKind::Impl(impl_item) = &item.kind else {
            return None;
        };
        let function = impl_item.items.iter().find_map(|impl_member| {
            if impl_member.def_id != def_id {
                return None;
            }
            match &impl_member.kind {
                ImplItemKind::Method(function) => Some(function.clone()),
                _ => None,
            }
        })?;
        Some((
            impl_item.generics.clone(),
            impl_item.self_ty.clone(),
            impl_item.items.clone(),
            function,
        ))
    }

    /// Cross-package counterpart to `hir_typeck::expr_path_ty`'s own
    /// same-package enum-variant scan — given a variant's resolved `DefId`,
    /// routes directly to the one package that could define it and returns
    /// the enclosing enum's real declared name. Moved from the old
    /// `AstProgram::find_hir_enum_for_variant`.
    pub fn find_hir_enum_for_variant(&self, def_id: DefId) -> Option<String> {
        let package = self.package(&def_id.package_id)?;
        let enum_def_id = package.enum_variant_item_index.get(&def_id)?;
        let item = package.def_map.get(enum_def_id)?;
        let ItemKind::Enum(enum_def) = &item.kind else {
            return None;
        };
        enum_def
            .variants
            .iter()
            .any(|v| v.def_id == def_id)
            .then(|| enum_def.name.as_str().to_string())
    }

    pub fn resolve(
        &self,
        from: &PackageId,
        from_module: &crate::ast::path::QualifiedPath,
        ns: resolve::Namespace,
        name: &str,
    ) -> Option<&Res> {
        let package = self.package(from)?;
        let module = package.module_tree.module_id(from_module)?;
        package.module_tree.lookup_res(module, ns, name)
    }
}
