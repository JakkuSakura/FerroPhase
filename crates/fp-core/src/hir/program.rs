use super::*;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

/// Session-wide HIR storage. Cloning this handle preserves both program and
/// package identity; it never creates a package snapshot.
#[derive(Clone, Debug, Default)]
pub struct SharedHirProgram(Rc<RefCell<HirProgram>>);

impl SharedHirProgram {
    pub fn new(program: HirProgram) -> Self {
        Self(Rc::new(RefCell::new(program)))
    }

    /// Returns a clone of the underlying shared HIR program handle.
    pub fn rc(&self) -> Rc<RefCell<HirProgram>> {
        Rc::clone(&self.0)
    }
    pub fn publish_package(&self, package: HirPackage) {
        self.0.borrow_mut().publish_package(package);
    }

    pub fn add_package(&self, package: Rc<RefCell<HirPackage>>) {
        self.0.borrow_mut().add_package(package);
    }

    pub fn package(&self, id: &PackageId) -> Option<Rc<RefCell<HirPackage>>> {
        self.0.borrow().package_rc(id)
    }

    pub fn package_rc(&self, id: &PackageId) -> Option<Rc<RefCell<HirPackage>>> {
        self.0.borrow().package_rc(id)
    }

    pub fn with<R>(&self, f: impl FnOnce(&HirProgram) -> R) -> R {
        f(&self.0.borrow())
    }

    pub fn borrow(&self) -> Ref<'_, HirProgram> {
        self.0.borrow()
    }

    /// Captures the packages published so far in a stable membership view.
    /// Cloning `HirProgram` retains each shared package handle without copying
    /// package data or observing packages published later.
    pub fn snapshot(&self) -> Rc<HirProgram> {
        Rc::new(self.0.borrow().clone())
    }
    pub fn item(&self, def_id: DefId) -> Option<Item> {
        self.0.borrow().item(def_id)
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        self.0.borrow().record_const_block_value(def_id, value);
    }

    pub fn anonymous_const(&self, def_id: DefId) -> Option<Block> {
        self.0.borrow().anonymous_const(def_id)
    }

    pub fn refinement_hint(&self, hir_id: HirId, slot: ParamSlot) -> Option<RefinementHint> {
        self.0.borrow().refinement_hint(hir_id, slot)
    }

    pub fn all_items(&self) -> Vec<Item> {
        self.0.borrow().all_items().collect()
    }

    pub fn source_path(&self, def_id: DefId) -> Option<crate::ast::path::InPackagePath> {
        self.0.borrow().source_path(def_id)
    }
    pub fn member_owner(&self, def_id: DefId) -> Option<DefId> {
        self.0.borrow().member_owner(def_id)
    }
    pub fn local_struct_fields(&self, def_id: DefId) -> Option<Vec<(Symbol, Ty)>> {
        self.0.borrow().local_struct_fields(def_id)
    }

    pub fn expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.0.borrow().expr_type(hir_id)
    }

    pub fn intrinsic_def(&self, id: DefId) -> Option<CallKind> {
        self.0.borrow().intrinsic_def(id)
    }
    pub fn type_expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.0.borrow().type_expr_type(hir_id)
    }

    pub fn record_type_expr_type(&self, hir_id: HirId, ty: Ty) {
        self.0.borrow().record_type_expr_type(hir_id, ty);
    }

    pub fn method_resolution(&self, hir_id: HirId) -> Option<DefId> {
        self.0.borrow().method_resolution(hir_id)
    }

    pub fn resolve_module_name(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        name: &str,
        namespace: crate::hir::resolve::Namespace,
    ) -> crate::hir::resolve::ResolutionResult {
        self.resolve_module_name_with_rules(
            package_id,
            module,
            name,
            namespace,
            crate::hir::resolve::ResolutionRules::default(),
        )
    }

    pub fn resolve_module_name_with_rules(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        name: &str,
        namespace: crate::hir::resolve::Namespace,
        rules: crate::hir::resolve::ResolutionRules,
    ) -> crate::hir::resolve::ResolutionResult {
        self.0
            .borrow()
            .resolve_module_name_with_rules(package_id, module, name, namespace, rules)
    }
    pub fn resolve_module_path(
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

    pub fn resolve_module_path_with_rules(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        path: &crate::ast::path::InPackagePath,
        namespace: crate::hir::resolve::Namespace,
        rules: crate::hir::resolve::ResolutionRules,
    ) -> crate::hir::resolve::ResolutionResult {
        self.0
            .borrow()
            .resolve_module_path_with_rules(package_id, module, path, namespace, rules)
    }
    pub fn module_exists(
        &self,
        package_id: &PackageId,
        path: &crate::ast::path::InPackagePath,
    ) -> bool {
        self.0.borrow().module_exists(package_id, path)
    }
    pub fn resolve_module_path_final(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        path: &crate::ast::path::InPackagePath,
        namespace: crate::hir::resolve::Namespace,
    ) -> crate::hir::resolve::ResolutionResult {
        self.resolve_module_path_final_with_rules(
            package_id,
            module,
            path,
            namespace,
            crate::hir::resolve::ResolutionRules::default(),
        )
    }
    pub fn resolve_module_path_final_with_rules(
        &self,
        package_id: &PackageId,
        module: &crate::ast::path::InPackagePath,
        path: &crate::ast::path::InPackagePath,
        namespace: crate::hir::resolve::Namespace,
        rules: crate::hir::resolve::ResolutionRules,
    ) -> crate::hir::resolve::ResolutionResult {
        match self.resolve_module_path_with_rules(package_id, module, path, namespace, rules) {
            crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(_)) => {
                crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Error)
            }
            result => result,
        }
    }
    pub fn module_member_names(
        &self,
        package_id: &PackageId,
        path: &crate::ast::path::InPackagePath,
    ) -> Option<Vec<crate::hir::resolve::Symbol>> {
        self.package(package_id).and_then(|package| {
            let package = package.borrow();
            let root = crate::hir::resolve::ModuleData::virtual_root_for(package_id.clone());
            let module = if path.segments.is_empty() {
                Some(root)
            } else {
                match package.module_data.resolve_module(
                    &root,
                    &path.segments,
                    crate::hir::resolve::Namespace::Type,
                ) {
                    crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(id)) => {
                        Some(id)
                    }
                    _ => None,
                }
            }?;
            package
                .module_data
                .children(&module)
                .map(|children| children.iter().map(|(name, _, _)| name.clone()).collect())
        })
    }
    pub fn reflection_field_intrinsic(
        &self,
        hir_id: HirId,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.0.borrow().reflection_field_intrinsic(hir_id)
    }

    pub fn reflection_field_intrinsic_at_span(
        &self,
        package_id: PackageId,
        span: crate::span::Span,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.0
            .borrow()
            .reflection_field_intrinsic_at_span(package_id, span)
    }
    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.0.borrow().generic_call_arg(hir_id)
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.0.borrow().generic_method_arg(hir_id)
    }

    pub fn const_value(&self, def_id: DefId) -> Option<Value> {
        self.0.borrow().const_value(def_id)
    }

    pub fn const_block_value(&self, def_id: DefId) -> Option<Value> {
        self.0.borrow().const_block_value(def_id)
    }

    pub fn intrinsic_def(&self, def_id: DefId) -> Option<CallKind> {
        self.0.borrow().intrinsic_def(def_id)
    }

    pub fn find_export(&self, key: &str) -> Option<Res> {
        self.0.borrow().find_export(key)
    }

    pub fn resolve_external_path(
        &self,
        path: &crate::ast::path::QualifiedPath,
        namespace: resolve::Namespace,
    ) -> Option<Res> {
        self.0.borrow().resolve_external_path(path, namespace)
    }

    pub fn resolve_external_entry(
        &self,
        path: &crate::ast::path::QualifiedPath,
        namespace: resolve::Namespace,
    ) -> Option<resolve::SymbolEntry> {
        self.0.borrow().resolve_external_entry(path, namespace)
    }

    pub fn resolve_external_module_path(
        &self,
        path: &crate::ast::path::QualifiedPath,
    ) -> Option<crate::ast::path::QualifiedPath> {
        self.0.borrow().resolve_external_module_path(path)
    }

    pub fn external_module_member_names(
        &self,
        path: &crate::ast::path::QualifiedPath,
    ) -> Option<Vec<String>> {
        self.0.borrow().external_module_member_names(path)
    }
}

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
                crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(next)) => {
                    module = next;
                }
                result => return result,
            }
        }
        crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(module))
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
                    crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(id)) => id,
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
                    crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(id)) => id,
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
                        crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(
                            next,
                        )) => module_id = next,
                        result if segments.len() == 0 => return result,
                        result => return result,
                    }
                }
                crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(module_id))
            })
            .unwrap_or(crate::hir::resolve::ResolutionResult::NotFound(
                crate::hir::resolve::ResolutionNotFound::Package(package_id.clone()),
            ))
    }

    pub fn module_exists(
        &self,
        package_id: &PackageId,
        path: &crate::ast::path::InPackagePath,
    ) -> bool {
        matches!(
            self.resolve_module_location(package_id, path),
            crate::hir::resolve::ResolutionResult::Found(crate::hir::Res::Module(_))
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
        self.exports_by_path.get(key).cloned()
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
        Rc<RefCell<HirPackage>>,
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
    ) -> Option<Res> {
        let package = self.package(from)?;
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
        let crate_name = path.head()?.to_string();
        let package_id = self.packages_by_crate_name.get(&crate_name)?.clone();
        let package = self.packages.get(&package_id)?.clone();

        // Keep the package's ordinary module-tree lookup as the first and
        // authoritative path. This is the normal rustc case and also
        // handles the two representations used by providers (crate-rooted
        // sysroot trees and crate-relative third-party trees).
        if let Some(entry) = package
            .borrow()
            .module_tree
            .lookup_crate_path(&crate_name, path, ns)
            .cloned()
        {
            return Some(entry);
        }
        // A public module re-export is the one case that cannot be answered
        // by the literal path lookup. Walk only intermediate segments,
        // following `Res::Module` exactly as rustc resolves a module path,
        // then perform one normal leaf lookup in the target package.
        let rooted =
            package
                .borrow()
                .module_tree
                .module_exists(&crate::ast::path::QualifiedPath::new(vec![
                    crate_name.clone(),
                ]));
        let mut current = if rooted {
            crate::ast::path::QualifiedPath::new(vec![crate_name.clone()])
        } else {
            crate::ast::path::QualifiedPath::new(Vec::new())
        };
        let first = usize::from(rooted);
        let segments = &path.segments[first..];
        for (index, segment) in segments.iter().enumerate() {
            let candidate = current.with_segment(segment.clone());
            let is_last = index + 1 == segments.len();
            if is_last {
                let target_root = current.head()?.to_string();
                let target_id = self.packages_by_crate_name.get(&target_root)?;
                let target_package = self.packages.get(target_id)?.clone();
                return target_package
                    .borrow()
                    .module_tree
                    .lookup_crate_path(&target_root, &candidate, ns)
                    .cloned();
            }

            let alias = {
                let package_ref = package.borrow();
                let module = package_ref.module_tree.module_id(&current)?;
                package_ref
                    .module_tree
                    .lookup(module, resolve::Namespace::Value, segment)
                    .cloned()
                    .or_else(|| {
                        package_ref
                            .module_tree
                            .lookup(module, resolve::Namespace::Type, segment)
                            .cloned()
                    })
            };
            if let Some(resolve::SymbolEntry {
                res: Res::Module(target),
                export,
                ..
            }) = alias
            {
                if !export.can_access(&[]) {
                    return None;
                }
                let target_root = target.first()?.clone();
                let target_id = self.packages_by_crate_name.get(&target_root)?;
                let target_package = self.packages.get(target_id)?.clone();
                current = crate::ast::path::QualifiedPath::new(target);
                // The package handle changes with the alias target. Keep the
                // lookup state explicit instead of consulting a global name map.
                return Self::resolve_external_entry_from_alias(
                    self,
                    target_package,
                    current,
                    &segments[index + 1..],
                    ns,
                );
            }
            if package.borrow().module_tree.module_exists(&candidate) {
                current = candidate;
                continue;
            }
            return None;
        }
        None
    }

    fn resolve_external_entry_from_alias(
        &self,
        mut package: Rc<RefCell<HirPackage>>,
        mut current: crate::ast::path::QualifiedPath,
        segments: &[String],
        ns: resolve::Namespace,
    ) -> Option<resolve::SymbolEntry> {
        for (index, segment) in segments.iter().enumerate() {
            let candidate = current.with_segment(segment.clone());
            if index + 1 == segments.len() {
                let root = current.head()?.to_string();
                return package
                    .borrow()
                    .module_tree
                    .lookup_crate_path(&root, &candidate, ns)
                    .cloned();
            }
            let alias = {
                let package_ref = package.borrow();
                let module = package_ref.module_tree.module_id(&current)?;
                package_ref
                    .module_tree
                    .lookup(module, resolve::Namespace::Value, segment)
                    .cloned()
                    .or_else(|| {
                        package_ref
                            .module_tree
                            .lookup(module, resolve::Namespace::Type, segment)
                            .cloned()
                    })
            };
            if let Some(resolve::SymbolEntry {
                res: Res::Module(target),
                ..
            }) = alias
            {
                let root = target.first()?.clone();
                let target_id = self.packages_by_crate_name.get(&root)?;
                package = self.packages.get(target_id)?.clone();
                current = crate::ast::path::QualifiedPath::new(target);
            } else if package.borrow().module_tree.module_exists(&candidate) {
                current = candidate;
            } else {
                return None;
            }
        }
        None
    }

    /// Resolves an extern-prelude path as a module and returns its canonical
    /// external path for subsequent segment resolution.
    pub fn resolve_external_module_path(
        &self,
        path: &crate::ast::path::QualifiedPath,
    ) -> Option<crate::ast::path::QualifiedPath> {
        let crate_name = path.head()?.to_string();
        let package_id = self.packages_by_crate_name.get(&crate_name)?.clone();
        let mut package = self.packages.get(&package_id)?.clone();
        let rooted =
            package
                .borrow()
                .module_tree
                .module_exists(&crate::ast::path::QualifiedPath::new(vec![
                    crate_name.clone(),
                ]));
        let first = usize::from(rooted);
        let mut current = if rooted {
            crate::ast::path::QualifiedPath::new(vec![crate_name])
        } else {
            crate::ast::path::QualifiedPath::new(Vec::new())
        };

        for segment in &path.segments[first..] {
            let candidate = current.with_segment(segment.clone());
            let alias = {
                let package_ref = package.borrow();
                let module = package_ref.module_tree.module_id(&current)?;
                package_ref
                    .module_tree
                    .lookup(module, resolve::Namespace::Value, segment)
                    .cloned()
                    .or_else(|| {
                        package_ref
                            .module_tree
                            .lookup(module, resolve::Namespace::Type, segment)
                            .cloned()
                    })
            };
            if let Some(resolve::SymbolEntry {
                res: Res::Module(target),
                export,
                ..
            }) = alias
            {
                if !export.can_access(&[]) {
                    return None;
                }
                let target_root = target.first()?.clone();
                let target_id = self.packages_by_crate_name.get(&target_root)?;
                package = self.packages.get(target_id)?.clone();
                current = crate::ast::path::QualifiedPath::new(target);
                continue;
            }
            if package.borrow().module_tree.module_exists(&candidate) {
                current = candidate;
                continue;
            }
            return None;
        }
        Some(current)
    }

    /// Public direct members of an external module, used to expand a Rust
    /// glob import without copying or reinterpreting the defining package's
    /// resolver entries. The later leaf lookup still returns that entry's
    /// original `Res`, including enum-variant `DefId`s.
    pub fn external_module_member_names(
        &self,
        path: &crate::ast::path::QualifiedPath,
    ) -> Option<Vec<String>> {
        let canonical = self.resolve_external_module_path(path)?;
        let crate_name = canonical.head()?.to_string();
        let package = self
            .packages_by_crate_name
            .get(&crate_name)
            .and_then(|package_id| self.packages.get(package_id))?;
        let package = package.borrow();
        let rooted = package
            .module_tree
            .module_exists(&crate::ast::path::QualifiedPath::new(vec![
                crate_name.clone(),
            ]));
        let internal = if rooted {
            canonical
        } else {
            crate::ast::path::QualifiedPath::new(
                canonical.segments.iter().skip(1).cloned().collect(),
            )
        };
        let module = package.module_tree.module_id(&internal)?;
        let mut names = std::collections::BTreeSet::new();
        for namespace in [resolve::Namespace::Value, resolve::Namespace::Type] {
            for (name, entry) in package.module_tree.bindings(module, namespace) {
                if matches!(entry.export, resolve::SymbolExport::Public) {
                    names.insert(name.to_string());
                }
            }
        }
        for (name, _) in package.module_tree.children(module) {
            names.insert(name.to_string());
        }
        Some(names.into_iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ReprOptions;
    use crate::ast::path::QualifiedPath;
    use crate::hir::resolve::{Namespace, SymbolEntry, SymbolExport};
    use crate::hir::{Enum, EnumVariant, Item, ItemKind, OwnerId, Symbol, Visibility};

    #[test]
    fn shared_program_snapshot_preserves_package_identity_and_membership() {
        let dependency_id = PackageId::new("dependency");
        let dependency = Rc::new(RefCell::new(HirPackage::new(dependency_id.clone())));
        let shared = SharedHirProgram::default();
        shared.add_package(dependency.clone());

        let snapshot = shared.snapshot();
        assert!(Rc::ptr_eq(
            &dependency,
            &snapshot
                .package_rc(&dependency_id)
                .expect("snapshot should retain the dependency handle"),
        ));

        let later_id = PackageId::new("later");
        shared.publish_package(HirPackage::new(later_id.clone()));
        assert!(snapshot.package_rc(&later_id).is_none());
        assert!(shared.package_rc(&later_id).is_some());
    }

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
        program.add_package(std::rc::Rc::new(package.into()));

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
        program.add_package(std::rc::Rc::new(package.into()));

        assert_eq!(
            program.find_export("core::option::Option"),
            Some(Res::Def(def_id))
        );
    }

    #[test]
    fn external_module_member_names_preserve_public_resolver_surface() {
        let package_id = PackageId::new("dependency");
        let mut package = HirPackage::new(package_id.clone());
        let module_path = QualifiedPath::new(vec!["api".to_string()]);
        let module = package.module_tree.ensure_module(&module_path);
        package.module_tree.bind(
            module,
            Namespace::Value,
            "PublicValue",
            SymbolEntry {
                res: Res::Def(DefId::new(package_id.clone(), 1)),
                export: SymbolExport::Public,
                path: Some(module_path.with_segment("PublicValue".to_string())),
            },
        );
        package.module_tree.bind(
            module,
            Namespace::Type,
            "PrivateType",
            SymbolEntry {
                res: Res::Def(DefId::new(package_id.clone(), 2)),
                export: SymbolExport::Scoped(vec!["api".to_string()]),
                path: Some(module_path.with_segment("PrivateType".to_string())),
            },
        );

        let mut program = HirProgram::new();
        program.add_package(std::rc::Rc::new(package.into()));

        assert_eq!(
            program.external_module_member_names(&QualifiedPath::new(vec![
                "dependency".to_string(),
                "api".to_string(),
            ])),
            Some(vec!["PublicValue".to_string()])
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
        program.add_package(std::rc::Rc::new(package.into()));

        assert_eq!(
            program.find_export("skln_core::types::ChangeRange"),
            Some(Res::Def(def_id))
        );
    }

    #[test]
    fn export_index_keeps_definitions_in_their_owning_package() {
        let dependency_id = PackageId::new("dependency");
        let def_id = DefId::new(dependency_id.clone(), 4);
        let mut dependency = HirPackage::new(dependency_id.clone());
        dependency
            .hir_exports
            .insert("api::PublicType".to_string(), Res::Def(def_id.clone()));

        let consumer_id = PackageId::new("consumer");
        let consumer = HirPackage::new(consumer_id.clone());
        let mut program = HirProgram::new();
        program.add_package(std::rc::Rc::new(dependency.into()));
        program.add_package(std::rc::Rc::new(consumer.into()));

        assert_eq!(
            program.find_export("dependency::api::PublicType"),
            Some(Res::Def(def_id.clone()))
        );
        assert!(
            program
                .package(&consumer_id)
                .is_some_and(|package| !package.def_map.contains_key(&def_id))
        );
    }

    #[test]
    fn enum_variant_lookup_uses_the_variant_package_identity() {
        let option_id = PackageId::new("option-provider");
        let conflicting_id = PackageId::new("unrelated-provider");
        let variant_index = 7;

        let mut option = HirPackage::new(option_id.clone());
        let option_enum = DefId::new(option_id.clone(), 1);
        let option_variant = DefId::new(option_id.clone(), variant_index);
        option.add_item(Item {
            hir_id: HirId::new(OwnerId(option_enum.clone()), 0),
            def_id: option_enum,
            visibility: Visibility::Public,
            span: Default::default(),
            kind: ItemKind::Enum(Enum {
                attrs: Vec::new(),
                name: Symbol::new("Option"),
                variants: vec![EnumVariant {
                    attrs: Vec::new(),
                    hir_id: HirId::new(OwnerId(option_variant.clone()), 0),
                    def_id: option_variant.clone(),
                    name: Symbol::new("Some"),
                    discriminant: None,
                    payload: None,
                }],
                generics: Default::default(),
                repr: ReprOptions::default(),
            }),
        });

        let mut unrelated = HirPackage::new(conflicting_id.clone());
        let unrelated_enum = DefId::new(conflicting_id.clone(), 2);
        let unrelated_variant = DefId::new(conflicting_id, variant_index);
        unrelated.add_item(Item {
            hir_id: HirId::new(OwnerId(unrelated_enum.clone()), 0),
            def_id: unrelated_enum,
            visibility: Visibility::Public,
            span: Default::default(),
            kind: ItemKind::Enum(Enum {
                attrs: Vec::new(),
                name: Symbol::new("Unrelated"),
                variants: vec![EnumVariant {
                    attrs: Vec::new(),
                    hir_id: HirId::new(OwnerId(unrelated_variant.clone()), 0),
                    def_id: unrelated_variant,
                    name: Symbol::new("Some"),
                    discriminant: None,
                    payload: None,
                }],
                generics: Default::default(),
                repr: ReprOptions::default(),
            }),
        });

        let mut program = HirProgram::new();
        program.publish_package(unrelated);
        program.publish_package(option);

        assert_eq!(
            program.find_hir_enum_for_variant(option_variant),
            Some("Option".to_string())
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
        program.add_package(std::rc::Rc::new(alloc.into()));

        let core_id = PackageId::new("skln-core");
        let mut core = HirPackage::new(core_id.clone());
        core.hir_exports.insert(
            "error::CoreError".to_string(),
            Res::Def(DefId::new(core_id, 4)),
        );
        program.add_package(std::rc::Rc::new(core.into()));

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

    #[test]
    fn resolve_external_entry_follows_public_module_reexports() {
        let qpath = |segments: &[&str]| {
            crate::ast::path::QualifiedPath::new(
                segments
                    .iter()
                    .map(|segment| (*segment).to_string())
                    .collect(),
            )
        };
        let alloc_id = PackageId::new("alloc");
        let mut alloc = HirPackage::new(alloc_id.clone());
        let alloc_fmt = alloc.module_tree.ensure_module(&qpath(&["alloc", "fmt"]));
        let formatter = DefId::new(alloc_id.clone(), 1);
        alloc.module_tree.bind(
            alloc_fmt,
            Namespace::Type,
            "Formatter",
            SymbolEntry {
                res: Res::Def(formatter.clone()),
                export: SymbolExport::Public,
                path: Some(qpath(&["alloc", "fmt", "Formatter"])),
            },
        );

        let std_id = PackageId::new("std");
        let mut std = HirPackage::new(std_id);
        let std_root = std.module_tree.ensure_module(&qpath(&["std"]));
        std.module_tree.bind(
            std_root,
            Namespace::Value,
            "fmt",
            SymbolEntry {
                res: Res::Module(vec!["alloc".to_string(), "fmt".to_string()]),
                export: SymbolExport::Public,
                path: Some(qpath(&["std", "fmt"])),
            },
        );

        let mut program = HirProgram::new();
        program.publish_package(alloc);
        program.publish_package(std);

        let resolved =
            program.resolve_external_entry(&qpath(&["std", "fmt", "Formatter"]), Namespace::Type);
        assert!(matches!(
            resolved.map(|entry| entry.res),
            Some(Res::Def(def_id)) if def_id == formatter
        ));
        assert_eq!(
            program.resolve_external_module_path(&qpath(&["std", "fmt"])),
            Some(qpath(&["alloc", "fmt"]))
        );
        assert_eq!(
            program.external_module_member_names(&qpath(&["std", "fmt"])),
            Some(vec!["Formatter".to_string()])
        );
    }

    #[test]
    fn resolve_external_entry_descends_through_real_modules() {
        let package_id = PackageId::new("std");
        let mut package = HirPackage::new(package_id.clone());
        let sync = package.module_tree.ensure_module(&QualifiedPath::new(vec![
            "std".to_string(),
            "sync".to_string(),
        ]));
        let arc = DefId::new(package_id, 9);
        package.module_tree.bind(
            sync,
            Namespace::Type,
            "Arc",
            SymbolEntry {
                res: Res::Def(arc.clone()),
                export: SymbolExport::Public,
                path: Some(QualifiedPath::new(vec![
                    "std".to_string(),
                    "sync".to_string(),
                    "Arc".to_string(),
                ])),
            },
        );

        let mut program = HirProgram::new();
        program.publish_package(package);
        let path = QualifiedPath::new(vec![
            "std".to_string(),
            "sync".to_string(),
            "Arc".to_string(),
        ]);

        assert!(matches!(
            program.resolve_external_entry(&path, Namespace::Type),
            Some(entry) if entry.res == Res::Def(arc)
        ));
    }
}
