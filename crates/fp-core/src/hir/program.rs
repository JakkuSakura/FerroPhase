use super::*;

/// The whole compiled result — every package involved, keyed by
/// `PackageId`. `AstToHirLowerer` owns one of these and works package-by-package
/// against it (see `docs/Resolution.md`); resolution across an
/// already-compiled dependency package is a lookup into this same
/// structure, not a separate clone-and-merge pass.
///
/// Packages are shared, mutable cells — building a `HirProgram` (e.g. a
/// `AstProgram` snapshotting its already-compiled dependency
/// packages, each already an `Rc<HirPackage>`, for a consumer like
/// `HirToMirLowerer` to dispatch cross-package `DefId` lookups against) is
/// then just a handful of `Rc` clones, never a deep clone of every
/// dependency's own items/def_map/def_paths.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct HirProgram {
    pub packages: HashMap<PackageId, std::rc::Rc<std::cell::RefCell<HirPackage>>>,
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

    /// The stable package allocation for `id`. Once installed, callers
    /// mutate this cell in place; replacing it would invalidate every
    /// in-flight `HirId`-keyed result.
    pub fn package_rc(
        &self,
        id: &PackageId,
    ) -> Option<std::rc::Rc<std::cell::RefCell<HirPackage>>> {
        self.packages.get(id).cloned()
    }

    /// Installs a package exactly once. Reusing an existing package id is a
    /// logic error: source HIR must never be rebuilt during compilation.
    pub fn add_package(
        &mut self,
        package: HirPackage,
    ) -> std::rc::Rc<std::cell::RefCell<HirPackage>> {
        assert!(
            !self.packages.contains_key(&package.id),
            "HIR package `{}` was installed more than once",
            package.id
        );
        for (name, def_id) in &package.struct_defs_by_name {
            self.struct_defs_by_name
                .entry(name.clone())
                .or_insert_with(|| def_id.clone());
        }
        let id = package.id.clone();
        let package = std::rc::Rc::new(std::cell::RefCell::new(package));
        self.packages.insert(id, package.clone());
        package
    }

    /// Returns the Rust source spelling of a package's external-crate root.
    /// Cargo permits hyphens in package names, while Rust normalizes them to
    /// underscores in paths (`skln-core` is imported as `skln_core`).
    pub fn external_crate_name(package_id: &PackageId) -> String {
        package_id.as_str().replace('-', "_")
    }

    /// O(1) direct lookup — no package iteration — for a struct declared
    /// under `name` in any package this `HirProgram` knows about.
    pub fn struct_def_id(&self, name: &str) -> Option<DefId> {
        self.struct_defs_by_name.get(name).cloned()
    }

    /// Every item across every package this `HirProgram` knows about — for
    /// callers that genuinely need the full set (e.g. a one-time reverse
    /// index build), not a single `DefId` lookup.
    pub fn all_items(&self) -> impl Iterator<Item = Item> {
        self.packages
            .values()
            .flat_map(|package| package.borrow().items.clone())
    }

    /// A definition's fully-qualified path, wherever its owning package
    /// lives — routes to that package's own `def_paths` via the `DefId`'s
    /// own `package_id`, so a caller never has to know or track which
    /// package a `DefId` came from before asking this question.
    pub fn def_path(&self, def_id: DefId) -> Option<DefPath> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .def_paths
            .get(&def_id)
            .cloned()
    }

    /// A transparent type alias's expansion target — see
    /// `HirPackage::type_alias_targets`'s doc comment for why this table
    /// exists at all.
    pub fn type_alias_target(&self, def_id: DefId) -> Option<TypeExpr> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .type_alias_targets
            .get(&def_id)
            .cloned()
    }

    /// The HIR node whose checked result is this alias's expansion.
    pub fn type_alias_target_hir_id(&self, def_id: DefId) -> Option<HirId> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .type_alias_target_hir_id(&def_id)
    }

    pub fn item(&self, def_id: DefId) -> Option<Item> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .def_map
            .get(&def_id)
            .cloned()
    }

    /// Cross-package counterpart of `HirPackage::member_owner` — routes to
    /// `def_id`'s own package via its `package_id`, so a caller never has
    /// to know or track which package a member `DefId` came from first.
    pub fn member_owner(&self, def_id: DefId) -> Option<DefId> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .member_owner(def_id)
    }

    /// Cross-package counterpart of `HirPackage::checked_impl_self_ty`.
    pub fn checked_impl_self_ty(&self, hir_id: HirId) -> Option<Ty> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .checked_impl_self_ty(hir_id)
    }

    pub fn cache_checked_impl_self_ty(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().cache_checked_impl_self_ty(hir_id, ty);
        }
    }

    /// Cross-package counterpart of `HirPackage::function_signature`.
    pub fn function_signature(&self, hir_id: HirId) -> Option<Ty> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .function_signature(hir_id)
    }

    pub fn cache_function_signature(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().cache_function_signature(hir_id, ty);
        }
    }

    /// Cross-package counterpart of `HirPackage::resolved_trait_def`.
    pub fn resolved_trait_def(&self, def_id: DefId) -> Option<std::rc::Rc<Trait>> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .resolved_trait_def(def_id)
    }

    pub fn cache_resolved_trait_def(&self, def_id: DefId, trait_def: std::rc::Rc<Trait>) {
        if let Some(package) = self.package_rc(&def_id.package_id) {
            package.borrow().cache_resolved_trait_def(def_id, trait_def);
        }
    }

    /// Cross-package counterpart of `HirPackage::refinement_hint`.
    pub fn refinement_hint(&self, hir_id: HirId, slot: ParamSlot) -> Option<RefinementHint> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .refinement_hint(hir_id, slot)
    }

    pub fn insert_refinement_hint(&self, hir_id: HirId, slot: ParamSlot, hint: RefinementHint) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().insert_refinement_hint(hir_id, slot, hint);
        }
    }

    /// Cross-package counterpart of `HirPackage::take_raw_refinement_hint`.
    /// Cross-package use is not actually expected here (a raw hint is
    /// always taken by the same package's own in-progress check, right
    /// after `check_type_expr` populates it), but routes through
    /// `hir_id.package_id` anyway for consistency with every other
    /// per-`HirId` accessor on this type.
    pub fn take_raw_refinement_hint(&self, hir_id: HirId) -> Option<RefinementHint> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .take_raw_refinement_hint(hir_id)
    }

    pub fn insert_raw_refinement_hint(&self, hir_id: HirId, hint: RefinementHint) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().insert_raw_refinement_hint(hir_id, hint);
        }
    }

    /// Cross-package counterpart of `HirPackage::literal_type_hint`.
    pub fn literal_type_hint(&self, hir_id: HirId) -> Option<Vec<String>> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .literal_type_hint(hir_id)
    }

    pub fn insert_literal_type_hint(&self, hir_id: HirId, literals: Vec<String>) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().insert_literal_type_hint(hir_id, literals);
        }
    }

    /// Cross-package counterpart of `HirPackage::local_struct_fields`.
    pub fn local_struct_fields(&self, def_id: DefId) -> Option<Vec<(Symbol, Ty)>> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .local_struct_fields(def_id)
    }

    pub fn insert_local_struct_fields(&self, def_id: DefId, fields: Vec<(Symbol, Ty)>) {
        if let Some(package) = self.package_rc(&def_id.package_id) {
            package.borrow().insert_local_struct_fields(def_id, fields);
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
        self.package_rc(hir_id.package_id())?
            .borrow()
            .expr_type(hir_id)
    }

    pub fn record_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().record_expr_type(hir_id, ty);
        }
    }

    pub fn type_expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .type_expr_type(hir_id)
    }

    pub fn record_type_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().record_type_expr_type(hir_id, ty);
        }
    }

    pub fn pat_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .pat_type(hir_id)
    }

    pub fn record_pat_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().record_pat_type(hir_id, ty);
        }
    }

    pub fn method_resolution(&self, hir_id: HirId) -> Option<DefId> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .method_resolution(hir_id)
    }

    pub fn record_method_resolution(&self, hir_id: HirId, def_id: DefId) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().record_method_resolution(hir_id, def_id);
        }
    }

    pub fn reflection_field_intrinsic(
        &self,
        hir_id: HirId,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .reflection_field_intrinsic(hir_id)
    }

    pub fn reflection_field_intrinsic_at_span(
        &self,
        package_id: PackageId,
        span: crate::span::Span,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.package_rc(&package_id)?
            .borrow()
            .reflection_field_intrinsic_at_span(span)
    }

    pub fn record_reflection_field_intrinsic(
        &self,
        hir_id: HirId,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package
                .borrow()
                .record_reflection_field_intrinsic(hir_id, intrinsic);
        }
    }

    pub fn record_reflection_field_intrinsic_at_span(
        &self,
        package_id: PackageId,
        span: crate::span::Span,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        if let Some(package) = self.package_rc(&package_id) {
            package
                .borrow()
                .record_reflection_field_intrinsic_at_span(span, intrinsic);
        }
    }

    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .generic_call_arg(hir_id)
    }

    pub fn record_generic_call_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package.borrow().record_generic_call_arg(hir_id, resolution);
        }
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package_rc(hir_id.package_id())?
            .borrow()
            .generic_method_arg(hir_id)
    }

    pub fn record_generic_method_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package_rc(hir_id.package_id()) {
            package
                .borrow()
                .record_generic_method_arg(hir_id, resolution);
        }
    }

    pub fn const_type(&self, def_id: DefId) -> Option<Ty> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .const_type(def_id)
    }

    pub fn record_const_type(&self, def_id: DefId, ty: Ty) {
        if let Some(package) = self.package_rc(&def_id.package_id) {
            package.borrow().record_const_type(def_id, ty);
        }
    }

    pub fn const_value(&self, def_id: DefId) -> Option<Value> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .const_value(def_id)
    }

    pub fn record_const_value(&self, def_id: DefId, value: Value) {
        if let Some(package) = self.package_rc(&def_id.package_id) {
            package.borrow().record_const_value(def_id, value);
        }
    }

    pub fn const_block_value(&self, def_id: DefId) -> Option<Value> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .const_block_value(def_id)
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        if let Some(package) = self.package_rc(&def_id.package_id) {
            package.borrow().record_const_block_value(def_id, value);
        }
    }

    pub fn op_def(&self, def_id: DefId) -> Option<crate::intrinsics::PortableOp> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .op_defs
            .get(&def_id)
            .cloned()
    }

    pub fn intrinsic_def(&self, def_id: DefId) -> Option<CallKind> {
        self.package_rc(&def_id.package_id)?
            .borrow()
            .intrinsic_defs
            .get(&def_id)
            .cloned()
    }

    pub fn is_placeholder_def(&self, def_id: DefId) -> bool {
        self.package_rc(&def_id.package_id)
            .is_some_and(|package| package.borrow().placeholder_defs.contains(&def_id))
    }

    /// Every `impl` item (from any package) whose self-type resolves to
    /// `did` — an impl for a type can live in a different package than the
    /// type itself, so this unions every package's own
    /// `HirPackage::impls_by_self_did` rather than only looking in `did`'s
    /// own package. Each per-package lookup is still O(1); only the number
    /// of packages that actually declare a matching impl costs anything.
    pub fn impls_for_adt(&self, did: DefId) -> impl Iterator<Item = Item> {
        self.packages.values().flat_map(move |package| {
            let package = package.borrow();
            package
                .impls_by_self_did
                .get(&did)
                .into_iter()
                .flatten()
                .filter_map(|impl_def_id| package.def_map.get(impl_def_id).cloned())
                .collect::<Vec<_>>()
        })
    }

    /// Every `impl` item (from any package) whose self-type structurally
    /// classifies as `shape` (`HirPackage::impls_by_shape`'s domain) —
    /// the non-ADT counterpart of `impls_for_adt`, for a receiver that's a
    /// concrete primitive/tuple/slice/array/etc. rather than a nominal
    /// struct/enum. Deliberately does *not* union in every impl in the
    /// workspace the way an `all_impls` scan would — see `blanket_impls`
    /// for the one class of impl that genuinely must apply regardless of
    /// shape.
    pub fn impls_for_shape(&self, shape: &str) -> impl Iterator<Item = Item> {
        let shape = shape.to_owned();
        self.packages.values().flat_map(move |package| {
            let package = package.borrow();
            package
                .impls_by_shape
                .get(&shape)
                .into_iter()
                .flatten()
                .filter_map(|impl_def_id| package.def_map.get(impl_def_id).cloned())
                .collect::<Vec<_>>()
        })
    }

    /// Every blanket impl (`impl<T> Trait for T`) across every package —
    /// unioned into every method/associated-item candidate search
    /// regardless of the receiver's own shape, since a blanket impl's
    /// self-type is itself just a bare generic parameter with nothing
    /// concrete to bucket it under.
    pub fn blanket_impls(&self) -> impl Iterator<Item = Item> {
        self.packages.values().flat_map(|package| {
            let package = package.borrow();
            package
                .blanket_impls
                .iter()
                .filter_map(|impl_def_id| package.def_map.get(impl_def_id).cloned())
                .collect::<Vec<_>>()
        })
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
            let package = self.packages[&id].borrow();
            if let Some(res) = package.hir_exports.get(key) {
                return Some(res.clone());
            }
            let root = format!("{}::", Self::external_crate_name(&id));
            if let Some(relative_key) = key.strip_prefix(&root) {
                if let Some(res) = package.hir_exports.get(relative_key) {
                    return Some(res.clone());
                }
            }

            // Export tables can originate from Cargo metadata or from a
            // package lowered before its Rust crate root was normalized.
            // Rust source always spells the external root with underscores,
            // so compare only that first segment while accepting either
            // `crate::item` and package-relative `item` export keys. This is
            // the same namespace normalization rustc applies at the extern
            // prelude boundary, and keeps the resolver independent of which
            // producer populated `hir_exports`.
            let Some((requested_root, requested_relative)) = key.split_once("::") else {
                continue;
            };
            if requested_root != Self::external_crate_name(&id) {
                continue;
            }
            let cargo_root = format!("{}::", id.as_str());
            for export_key in package.hir_exports.keys() {
                let relative = export_key
                    .strip_prefix(&cargo_root)
                    .or_else(|| export_key.strip_prefix(&root))
                    .unwrap_or(export_key.as_str());
                if relative == requested_relative {
                    return package.hir_exports.get(export_key).cloned();
                }
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
            let package = self.packages[&id].borrow();
            for (key, res) in &package.hir_exports {
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
            let package = self.packages[&id].borrow();
            for (key, res) in &package.hir_exports {
                if key == suffix || key.ends_with(&dotted_suffix) {
                    return Some(res.clone());
                }
            }
        }
        None
    }

    /// Every installed package's own HIR plus its export table — moved
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
        std::rc::Rc<std::cell::RefCell<HirPackage>>,
        HashMap<String, Res>,
    )> {
        self.packages
            .values()
            .map(|package| {
                let package_ref = package.borrow();
                let exports = package_ref
                    .hir_exports
                    .iter()
                    .map(|(key, res)| {
                        (
                            Self::canonical_export_key(&package_ref.id, key),
                            res.clone(),
                        )
                    })
                    .collect();
                (
                    crate::ast::path::QualifiedPath::new(Vec::new()),
                    package.clone(),
                    exports,
                )
            })
            .collect()
    }

    /// Returns the canonical qualified key used at the HIR package boundary
    /// for an exported definition. Rust's extern prelude always introduces a
    /// crate-root segment, and Cargo package names are normalized to Rust
    /// identifiers at that boundary (`skln-core` becomes `skln_core`).
    /// Providers may hand the HIR a package-relative key or a key rooted at
    /// either spelling, so normalize all three forms here once.
    pub fn canonical_export_key(package_id: &PackageId, key: &str) -> String {
        Self::canonical_external_path(package_id, key)
    }

    /// Canonical path at the HIR extern-prelude boundary. A provider may
    /// describe a definition using a package-relative path, Cargo's raw
    /// package name, or Rust's normalized crate name. Bundled sysroot
    /// providers may additionally include the crate name twice because the
    /// source corpus is stored below a package directory. Normalize all of
    /// those producer representations before publishing either a key or a
    /// `Res::Module` target.
    pub fn canonical_external_path(package_id: &PackageId, key: &str) -> String {
        let root = Self::external_crate_name(package_id);
        let cargo_root = package_id.as_str();
        let mut segments = key
            .split("::")
            .filter(|segment| !segment.is_empty())
            .map(str::to_owned)
            .collect::<Vec<_>>();
        if segments
            .first()
            .is_none_or(|first| first != &root && first != cargo_root)
        {
            segments.insert(0, root.clone());
        } else {
            segments[0] = root.clone();
        }
        while segments.len() > 1 && segments[1] == root {
            segments.remove(1);
        }
        segments.join("::")
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
        let package = self.package_rc(&def_id.package_id)?;
        let package = package.borrow();
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
        let package = self.package_rc(&def_id.package_id)?;
        let package = package.borrow();
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
    ) -> Option<Res> {
        let package = self.package_rc(from)?;
        let package = package.borrow();
        let module = package.module_tree.module_id(from_module)?;
        package.module_tree.lookup_res(module, ns, name).cloned()
    }

    /// Resolves a path whose first segment is an extern-prelude crate name.
    /// Package ownership is selected by the root segment; the target package's
    /// `ModuleTree` then performs the namespace-specific lookup.
    pub fn resolve_external_path(
        &self,
        path: &crate::ast::path::QualifiedPath,
        ns: resolve::Namespace,
    ) -> Option<Res> {
        self.resolve_external_entry(path, ns)
            .map(|entry| entry.res.clone())
    }

    /// Resolves an extern-prelude path and retains its export metadata for
    /// visibility checking by the caller.
    pub fn resolve_external_entry(
        &self,
        path: &crate::ast::path::QualifiedPath,
        ns: resolve::Namespace,
    ) -> Option<resolve::SymbolEntry> {
        let crate_name = path.head()?;
        let package = self
            .packages
            .values()
            .find(|package| Self::external_crate_name(&package.borrow().id) == crate_name)?
            .borrow();
        package
            .module_tree
            .lookup_crate_path(crate_name, path, ns)
            .cloned()
    }

    /// Resolves an extern-prelude path as a module and returns its canonical
    /// external path for subsequent segment resolution.
    pub fn resolve_external_module_path(
        &self,
        path: &crate::ast::path::QualifiedPath,
    ) -> Option<crate::ast::path::QualifiedPath> {
        let crate_name = path.head()?;
        let package = self
            .packages
            .values()
            .find(|package| Self::external_crate_name(&package.borrow().id) == crate_name)?
            .borrow();
        package
            .module_tree
            .module_exists_crate_path(crate_name, path)
            .then(|| path.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_export_accepts_normalized_external_crate_root() {
        let package_id = PackageId::new("skln-core");
        let mut package = HirPackage::new(package_id);
        for (index, path) in [
            (7, "error::CoreError"),
            (8, "types::ChangesResult"),
            (9, "types::BranchInfo"),
        ] {
            let def_id = DefId::new(package.id.clone(), index);
            package
                .hir_exports
                .insert(path.to_string(), Res::Def(def_id));
        }

        let mut program = HirProgram::new();
        program.add_package(package);

        for (index, path) in [
            (7, "error::CoreError"),
            (8, "types::ChangesResult"),
            (9, "types::BranchInfo"),
        ] {
            assert_eq!(
                program.find_export(&format!("skln_core::{path}")),
                Some(Res::Def(DefId::new(PackageId::new("skln-core"), index)))
            );
        }
    }

    #[test]
    fn find_export_keeps_exact_package_relative_keys() {
        let package_id = PackageId::new("core");
        let def_id = DefId::new(package_id.clone(), 11);
        let mut package = HirPackage::new(package_id);
        package
            .hir_exports
            .insert("core::option::Option".to_string(), Res::Def(def_id.clone()));

        let mut program = HirProgram::new();
        program.add_package(package);

        assert_eq!(
            program.find_export("core::option::Option"),
            Some(Res::Def(def_id))
        );
    }

    #[test]
    fn find_export_normalizes_cargo_root_in_export_key() {
        let package_id = PackageId::new("skln-core");
        let def_id = DefId::new(package_id.clone(), 12);
        let mut package = HirPackage::new(package_id);
        package.hir_exports.insert(
            "skln-core::types::ChangeRange".to_string(),
            Res::Def(def_id.clone()),
        );

        let mut program = HirProgram::new();
        program.add_package(package);

        assert_eq!(
            program.find_export("skln_core::types::ChangeRange"),
            Some(Res::Def(def_id))
        );
    }

    #[test]
    fn hir_definitions_publish_canonical_std_and_skln_core_keys() {
        let mut program = HirProgram::new();
        let alloc_id = PackageId::new("alloc");
        let mut alloc = HirPackage::new(alloc_id.clone());
        for (relative_key, index) in [("string::String", 1), ("vec::Vec", 2), ("sync::Arc", 3)] {
            alloc.hir_exports.insert(
                relative_key.to_string(),
                Res::Def(DefId::new(alloc_id.clone(), index)),
            );
        }
        program.add_package(alloc);

        let core_id = PackageId::new("skln-core");
        let mut core = HirPackage::new(core_id.clone());
        core.hir_exports.insert(
            "error::CoreError".to_string(),
            Res::Def(DefId::new(core_id, 4)),
        );
        program.add_package(core);

        let mut actual = program
            .hir_definitions()
            .into_iter()
            .flat_map(|(_, _, exports)| exports.into_keys())
            .collect::<Vec<_>>();
        actual.sort();
        assert_eq!(
            actual,
            vec![
                "alloc::string::String",
                "alloc::sync::Arc",
                "alloc::vec::Vec",
                "skln_core::error::CoreError",
            ]
        );
    }
}
