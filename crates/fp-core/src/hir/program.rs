use super::*;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

/// The whole compiled result — every package involved, keyed by
/// `PackageId`. `AstToHirLowerer` owns one of these and works package-by-package
/// against it (see `docs/Resolution.md`); resolution across an
/// already-compiled dependency package is a lookup into this same
/// structure, not a separate clone-and-merge pass.
///
/// Packages are shared mutable cells — building a `HirProgram` (e.g. a
/// `AstProgram` snapshotting its already-compiled dependency
/// packages, each already an `Rc<RefCell<HirPackage>>`, for a consumer like
/// `HirToMirLowerer` to dispatch cross-package `DefId` lookups against) is
/// then just a handful of `Rc` clones, never a deep clone of every
/// dependency's own items/def_map/source paths.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct HirProgram {
    pub packages: HashMap<PackageId, Rc<RefCell<HirPackage>>>,
}

impl HirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn package(&self, id: &PackageId) -> Option<Ref<'_, HirPackage>> {
        self.packages.get(id).map(|package| package.borrow())
    }

    /// Same package, but the shared mutable package cell itself.
    pub fn package_rc(&self, id: &PackageId) -> Option<Rc<RefCell<HirPackage>>> {
        self.packages.get(id).cloned()
    }

    /// Resolve a named child from the module identified by `module`.
    /// The package is selected from the module definition id automatically.
    pub fn resolve_module_child(
        &self,
        module: &DefId,
        name: &str,
        namespace: crate::hir::resolve::Namespace,
    ) -> crate::hir::resolve::ResolutionResult {
        self.package(&module.package_id)
            .map(|package| package.module_data.resolve_child(module, name, namespace))
            .unwrap_or(crate::hir::resolve::ResolutionResult::NotFound(
                crate::hir::resolve::ResolutionNotFound::Package(module.package_id.clone()),
            ))
    }

    pub fn prelude_modules(&self, package_id: &PackageId) -> Option<Vec<DefId>> {
        self.package(package_id)
            .map(|package| package.prelude_modules.clone())
    }

    pub fn resolve_module_location(
        &self,
        package_id: &PackageId,
        location: &crate::ast::path::InPackagePath,
    ) -> crate::hir::resolve::ResolutionResult {
        self.resolve_module_location_segments(package_id, &location.segments)
    }

    pub fn resolve_module_location_segments(
        &self,
        package_id: &PackageId,
        segments: &[String],
    ) -> crate::hir::resolve::ResolutionResult {
        let mut module = crate::hir::resolve::ModuleData::virtual_root_for(package_id.clone());
        for segment in segments {
            match self.resolve_module_child(&module, segment, crate::hir::resolve::Namespace::Type)
            {
                crate::hir::resolve::ResolutionResult::Found(path)
                    if let crate::hir::Res::Module(next) = path.res.clone() =>
                {
                    module = next;
                }
                result => return result,
            }
        }
        crate::hir::resolve::ResolutionResult::Found(crate::hir::Path {
            res: crate::hir::Res::Module(module),
            segments: Vec::new(),
        })
    }

    /// Publishes a completed package snapshot into this program.
    ///
    /// The package is the owner of its `def_map`, source-path metadata, module tree,
    /// and derived lookup indexes.  `HirProgram` stores that package by
    /// `PackageId`; it must not reconstruct or take ownership of those
    /// tables in a second global copy.  Reindex the owned snapshot before
    /// publication so callers cannot accidentally publish a bulk-built HIR
    /// package with empty or stale derived indexes.
    pub fn publish_package(&mut self, package: HirPackage) {
        let package_id = package.id.clone();
        let package = Rc::new(RefCell::new(package));

        // Replacement is used when a package is re-lowered after a comptime
        // result becomes available. Rebuild the aggregate index from the
        // authoritative package snapshots so stale entries cannot survive.
        self.packages.insert(package_id, package);
    }

    /// Inserts an already shared package snapshot without changing its
    /// identity. The snapshot must already have complete derived indexes.
    pub fn add_package(&mut self, package: Rc<RefCell<HirPackage>>) {
        let package_id = package.borrow().id.clone();
        self.packages.insert(package_id, package);
    }

    pub fn resolve_module_name_with_rules(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        name: &str,
        namespace: crate::hir::resolve::Namespace,
        rules: crate::hir::resolve::ResolutionRules,
    ) -> crate::hir::resolve::ResolutionResult {
        let _ = rules;
        self.package(package_id)
            .map(|package| {
                let module_id = match self.resolve_module_location(package_id, module) {
                    crate::hir::resolve::ResolutionResult::Found(path)
                        if let crate::hir::Res::Module(id) = path.res.clone() =>
                    {
                        id
                    }
                    _ => crate::hir::resolve::ModuleData::virtual_root_for(package_id.clone()),
                };
                package
                    .module_data
                    .resolve_child(&module_id, name, namespace)
            })
            .unwrap_or(crate::hir::resolve::ResolutionResult::NotFound(
                crate::hir::resolve::ResolutionNotFound::Package(package_id.clone()),
            ))
    }

    pub fn resolve_module_path_with_rules(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        path: &crate::ast::path::InPackagePath,
        namespace: crate::hir::resolve::Namespace,
        rules: crate::hir::resolve::ResolutionRules,
    ) -> crate::hir::resolve::ResolutionResult {
        let _ = rules;
        self.package(package_id)
            .map(|package| {
                let mut module_id = match self.resolve_module_location(package_id, module) {
                    crate::hir::resolve::ResolutionResult::Found(path)
                        if let crate::hir::Res::Module(id) = path.res.clone() =>
                    {
                        id
                    }
                    _ => {
                        return crate::hir::resolve::ResolutionResult::NotFound(
                            crate::hir::resolve::ResolutionNotFound::ModuleDefinition(
                                crate::hir::resolve::ModuleData::virtual_root_for(
                                    package_id.clone(),
                                ),
                            ),
                        );
                    }
                };
                let mut segments = path.segments.iter();
                while let Some(segment) = segments.next() {
                    match package
                        .module_data
                        .resolve_child(&module_id, segment, namespace)
                    {
                        crate::hir::resolve::ResolutionResult::Found(path)
                            if let crate::hir::Res::Module(next) = path.res.clone() =>
                        {
                            module_id = next
                        }
                        result if segments.len() == 0 => return result,
                        result => return result,
                    }
                }
                crate::hir::resolve::ResolutionResult::Found(crate::hir::Path {
                    res: crate::hir::Res::Module(module_id),
                    segments: Vec::new(),
                })
            })
            .unwrap_or(crate::hir::resolve::ResolutionResult::NotFound(
                crate::hir::resolve::ResolutionNotFound::Package(package_id.clone()),
            ))
    }

    pub fn resolve_module_path_final(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        path: &crate::ast::path::InPackagePath,
        namespace: crate::hir::resolve::Namespace,
    ) -> crate::hir::resolve::ResolutionResult {
        self.resolve_module_path_with_rules(
            package_id,
            module,
            path,
            namespace,
            crate::hir::resolve::ResolutionRules::default(),
        )
    }

    pub fn module_exists(
        &self,
        package_id: &PackageId,
        path: &crate::ast::path::InPackagePath,
    ) -> bool {
        matches!(
            self.resolve_module_location(package_id, path),
            crate::hir::resolve::ResolutionResult::Found(path)
                if matches!(path.res, crate::hir::Res::Module(_))
        )
    }

    /// Lookup a nominal struct by its declared name. This is a HIR data query,
    /// not name resolution; source-name resolution remains owned by AST.
    pub fn struct_def_id(&self, name: &str) -> Option<DefId> {
        self.packages
            .values()
            .find_map(|package| package.borrow().struct_defs_by_name.get(name).cloned())
    }

    /// Returns the Rust source spelling of a package's external-crate root.
    /// Cargo permits hyphens in package names, while Rust normalizes them to
    /// underscores in paths (`skln-core` is imported as `skln_core`).
    pub fn external_crate_name(package_id: &PackageId) -> String {
        package_id.as_str().replace('-', "_")
    }

    /// Every item across every package this `HirProgram` knows about — for
    /// callers that genuinely need the full set (e.g. a one-time reverse
    /// index build), not a single `DefId` lookup.
    pub fn all_items(&self) -> impl Iterator<Item = Item> {
        self.packages
            .values()
            .flat_map(|package| package.borrow().items.clone().into_iter())
    }

    pub fn item(&self, def_id: DefId) -> Option<Item> {
        self.package(&def_id.package_id)?
            .def_map
            .get(&def_id)
            .cloned()
    }

    pub fn source_path(&self, def_id: DefId) -> Option<crate::ast::path::InPackagePath> {
        self.package(&def_id.package_id)?
            .source_path(&def_id)
            .cloned()
    }

    /// Cross-package counterpart of `HirPackage::member_owner` — routes to
    /// `def_id`'s own package via its `package_id`, so a caller never has
    /// to know or track which package a member `DefId` came from first.
    pub fn member_owner(&self, def_id: DefId) -> Option<DefId> {
        self.package(&def_id.package_id)?.member_owner(def_id)
    }

    /// Cross-package counterpart of `HirPackage::checked_impl_self_ty`.
    pub fn checked_impl_self_ty(&self, hir_id: HirId) -> Option<Ty> {
        self.package(hir_id.package_id())?
            .checked_impl_self_ty(hir_id)
    }

    pub fn cache_checked_impl_self_ty(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.cache_checked_impl_self_ty(hir_id, ty);
        }
    }

    /// Cross-package counterpart of `HirPackage::function_signature`.
    pub fn function_signature(&self, hir_id: HirId) -> Option<Ty> {
        self.package(hir_id.package_id())?
            .function_signature(hir_id)
    }

    pub fn cache_function_signature(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(hir_id.package_id()) {
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
        self.package(hir_id.package_id())?
            .refinement_hint(hir_id, slot)
    }

    pub fn insert_refinement_hint(&self, hir_id: HirId, slot: ParamSlot, hint: RefinementHint) {
        if let Some(package) = self.package(hir_id.package_id()) {
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
        self.package(hir_id.package_id())?
            .take_raw_refinement_hint(hir_id)
    }

    pub fn insert_raw_refinement_hint(&self, hir_id: HirId, hint: RefinementHint) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.insert_raw_refinement_hint(hir_id, hint);
        }
    }

    /// Cross-package counterpart of `HirPackage::literal_type_hint`.
    pub fn literal_type_hint(&self, hir_id: HirId) -> Option<Vec<String>> {
        self.package(hir_id.package_id())?.literal_type_hint(hir_id)
    }

    pub fn insert_literal_type_hint(&self, hir_id: HirId, literals: Vec<String>) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.insert_literal_type_hint(hir_id, literals);
        }
    }

    /// Cross-package counterpart of `HirPackage::local_struct_fields`.
    pub fn local_struct_fields(&self, def_id: DefId) -> Option<Vec<(Symbol, Ty)>> {
        self.package(&def_id.package_id)?
            .local_struct_fields(def_id)
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
        self.package(hir_id.package_id())?.expr_type(hir_id)
    }

    pub fn record_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_expr_type(hir_id, ty);
        }
    }

    pub fn type_expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package(hir_id.package_id())?.type_expr_type(hir_id)
    }

    pub fn record_type_expr_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_type_expr_type(hir_id, ty);
        }
    }

    pub fn pat_type(&self, hir_id: HirId) -> Option<Ty> {
        self.package(hir_id.package_id())?.pat_type(hir_id)
    }

    pub fn record_pat_type(&self, hir_id: HirId, ty: Ty) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_pat_type(hir_id, ty);
        }
    }

    pub fn method_resolution(&self, hir_id: HirId) -> Option<DefId> {
        self.package(hir_id.package_id())?.method_resolution(hir_id)
    }

    pub fn record_method_resolution(&self, hir_id: HirId, def_id: DefId) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_method_resolution(hir_id, def_id);
        }
    }

    pub fn reflection_field_intrinsic(
        &self,
        hir_id: HirId,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.package(hir_id.package_id())?
            .reflection_field_intrinsic(hir_id)
    }

    pub fn reflection_field_intrinsic_at_span(
        &self,
        package_id: PackageId,
        span: crate::span::Span,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.package(&package_id)?
            .reflection_field_intrinsic_at_span(span)
    }

    pub fn record_reflection_field_intrinsic(
        &self,
        hir_id: HirId,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_reflection_field_intrinsic(hir_id, intrinsic);
        }
    }

    pub fn record_reflection_field_intrinsic_at_span(
        &self,
        package_id: PackageId,
        span: crate::span::Span,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        if let Some(package) = self.package(&package_id) {
            package.record_reflection_field_intrinsic_at_span(span, intrinsic);
        }
    }

    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package(hir_id.package_id())?.generic_call_arg(hir_id)
    }

    pub fn record_generic_call_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package(hir_id.package_id()) {
            package.record_generic_call_arg(hir_id, resolution);
        }
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.package(hir_id.package_id())?
            .generic_method_arg(hir_id)
    }

    pub fn record_generic_method_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        if let Some(package) = self.package(hir_id.package_id()) {
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

    pub fn anonymous_const(&self, def_id: DefId) -> Option<Block> {
        self.package(&def_id.package_id)?.anonymous_const(def_id)
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        if let Some(package) = self.package(&def_id.package_id) {
            package.record_const_block_value(def_id, value);
        }
    }

    pub fn intrinsic_def(&self, def_id: DefId) -> Option<CallKind> {
        self.package(&def_id.package_id)?
            .intrinsic_defs
            .get(&def_id)
            .cloned()
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
    pub fn impls_for_adt(&self, did: DefId) -> impl Iterator<Item = Item> {
        self.packages.values().flat_map(move |package| {
            let package = package.borrow();
            let impl_ids = package
                .impls_by_self_did
                .get(&did)
                .cloned()
                .unwrap_or_default();
            impl_ids
                .into_iter()
                .filter_map(|impl_def_id| package.def_map.get(&impl_def_id).cloned())
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
        self.packages.values().flat_map(move |package| {
            let package = package.borrow();
            let impl_ids = package
                .impls_by_shape
                .get(shape)
                .cloned()
                .unwrap_or_default();
            impl_ids
                .into_iter()
                .filter_map(|impl_def_id| package.def_map.get(&impl_def_id).cloned())
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
            let impl_ids = package.blanket_impls.iter().cloned().collect::<Vec<_>>();
            impl_ids
                .into_iter()
                .filter_map(|impl_def_id| package.def_map.get(&impl_def_id).cloned())
                .collect::<Vec<_>>()
        })
    }
}
