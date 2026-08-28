// ── Compiled workspace context (typer lookup) ────────────────────

use crate::ast::package::provider::PackageProvider;
use crate::ast::package::{AstPackage, PackageId};
use crate::ast::path::QualifiedPath;
use crate::ast::{FunctionSignature, MethodSignature, TypeEnum, TypeStruct};
use crate::hir::PackageId as HirPackageId;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Shared registry of provider-owned packages and compiler-owned package
/// results for one compilation session. Dependencies are published here by
/// the compiler driver before their dependents are typed.
///
/// Conceptually similar to `hir::HirProgram` — both are "many packages,
/// addressed by `PackageId`" containers — but at a different layer and
/// lifecycle: `hir::HirProgram` is one immutable snapshot of every package's
/// already-lowered HIR, while `AstProgram` is the live, mutable
/// registry compilation itself is built against (spanning every layer, not
/// just HIR, and growing one `begin_package`/`import_package` call at a
/// time as compilation proceeds).
pub struct AstProgram {
    crates: Rc<RefCell<HashMap<PackageId, Rc<RefCell<AstPackage>>>>>,
    /// The single package provider for this workspace, required at
    /// construction and never changed afterward. Callers that need to
    /// combine several concrete providers (e.g. a language's std/libc
    /// provider plus the real input-package provider) build a
    /// `CompositeProvider` wrapping them before constructing the
    /// workspace — `AstProgram` itself never needs to search a list
    /// of providers (previously O(providers × package-list) per lookup,
    /// called once per package in the dependency graph).
    providers: Arc<dyn PackageProvider>,
    current_package: Option<PackageId>,
    prelude: Rc<RefCell<Option<Rc<RefCell<AstPackage>>>>>,
    /// Reverse index from a package's *HIR* id (the `package_id` embedded
    /// in every `hir::DefId` minted while lowering it) back to its
    /// `AstPackage` — lets a HIR-level, `DefId`-keyed lookup
    /// (`find_hir_impl_method`) go straight to the one package that could
    /// possibly own that `DefId` (an inherent impl's items are always
    /// minted in the same package as the impl itself) instead of searching
    /// every loaded package.
    hir_packages: Rc<RefCell<HashMap<HirPackageId, Rc<RefCell<AstPackage>>>>>,
    /// Memoized, name-sorted snapshot of `crates`'s values — the package
    /// set only ever changes via `begin_package`/`import_package` (both
    /// invalidate this), so `sorted_packages` doesn't need to rebuild a
    /// `String` per package and re-sort on every one of its many callers
    /// (`find_struct`/`find_enum`/`find_function_sig`, `method_sigs`,
    /// `module_paths`, ...) — this runs once per unqualified
    /// identifier/path reference across every compiled file.
    sorted_packages_cache: Rc<RefCell<Option<Vec<Rc<RefCell<AstPackage>>>>>>,
}

impl AstProgram {
    pub fn new(provider: Arc<dyn PackageProvider>) -> Self {
        Self {
            crates: Rc::new(RefCell::new(HashMap::new())),
            providers: provider,
            current_package: None,
            prelude: Rc::new(RefCell::new(None)),
            hir_packages: Rc::new(RefCell::new(HashMap::new())),
            sorted_packages_cache: Rc::new(RefCell::new(None)),
        }
    }

    fn sorted_packages(&self) -> Vec<Rc<RefCell<AstPackage>>> {
        if let Some(cached) = self.sorted_packages_cache.borrow().as_ref() {
            return cached.clone();
        }
        let mut packages: Vec<_> = self
            .crates
            .borrow()
            .iter()
            .map(|(package_id, package)| (package_id.to_string(), package.clone()))
            .collect();
        packages.sort_by(|(left, _), (right, _)| left.cmp(right));
        let packages: Vec<_> = packages.into_iter().map(|(_, package)| package).collect();
        *self.sorted_packages_cache.borrow_mut() = Some(packages.clone());
        packages
    }

    /// Create a package-focused workspace view. Provider registrations and
    /// already compiled package entries are shared by value (the packages
    /// themselves remain behind their existing `Rc`s), while lookups see the
    /// package currently being lowered. A dependency must remain visible when
    /// recursive compilation changes the active package; starting with an
    /// empty registry would make visibility depend on recursion order.
    pub fn for_package(&self, package_id: PackageId) -> Self {
        Self {
            crates: self.crates.clone(),
            providers: self.providers.clone(),
            current_package: Some(package_id),
            prelude: self.prelude.clone(),
            hir_packages: self.hir_packages.clone(),
            sorted_packages_cache: self.sorted_packages_cache.clone(),
        }
    }

    pub fn current_package(&self) -> Option<&PackageId> {
        self.current_package.as_ref()
    }

    /// Publish a package source slot and return its compiler-owned result.
    pub fn begin_package(
        &self,
        package_id: PackageId,
        source: AstPackage,
        data_layout: crate::lir::LirDataLayout,
    ) -> Rc<RefCell<AstPackage>> {
        let _ = data_layout;
        let source_package_id = package_id.clone();
        let hir_package_id = HirPackageId::new(package_id.as_str());
        let mut source = source;
        source.hir_package_id = hir_package_id.clone();
        let krate = Rc::new(RefCell::new(source));
        self.crates
            .borrow_mut()
            .insert(source_package_id, krate.clone());
        self.hir_packages
            .borrow_mut()
            .insert(hir_package_id.clone(), krate.clone());
        *self.sorted_packages_cache.borrow_mut() = None;
        krate
    }

    pub fn import_package(&self, package_id: PackageId, package: Rc<RefCell<AstPackage>>) {
        let hir_package_id = package.borrow().hir_package_id.clone();
        self.hir_packages
            .borrow_mut()
            .insert(hir_package_id, package.clone());
        self.crates.borrow_mut().insert(package_id, package);
        *self.sorted_packages_cache.borrow_mut() = None;
    }

    /// Install `std`'s published package as the unqualified prelude lookup
    /// source for ordinary packages. The standard and libc packages do not
    /// import their own prelude.
    pub fn install_prelude(&self, package: Rc<RefCell<AstPackage>>) {
        let Some(current_package) = self.current_package.as_ref() else {
            return;
        };
        if matches!(current_package.as_str(), "std" | "libc") {
            return;
        }
        self.prelude.borrow_mut().replace(package);
    }

    pub fn prelude_package(&self) -> Option<Rc<RefCell<AstPackage>>> {
        self.prelude.borrow().clone()
    }

    /// Cross-package counterpart to `find_struct`/`find_enum`, for a
    /// `type X = Y;` alias (e.g. `libc`'s `pub type char = u8;`) by its
    /// fully-qualified defining path (`"libc::char"`) — so a dependent
    /// package's own alias lookup can resolve an explicitly-qualified
    /// reference like `::libc::char` the same way it already resolves
    /// same-package aliases, without eagerly copying every package's
    /// aliases into the caller's own map first.
    pub fn find_type_alias(&self, key: &str) -> Option<crate::ast::Ty> {
        for package in self.sorted_packages() {
            if let Some(ty) = package.borrow().type_alias_exports.get(key) {
                return Some(ty.clone());
            }
        }
        None
    }

    // `find_export`/`find_export_by_name`/`find_export_by_suffix` moved to
    // `hir::HirProgram` — they read `hir_exports`, which now lives on
    // `hir::HirPackage`, not `AstPackage`.

    pub fn is_loaded(&self, package_id: &PackageId) -> bool {
        self.crates.borrow().contains_key(package_id)
    }

    pub fn compiled_package(&self, package_id: &PackageId) -> Option<Rc<RefCell<AstPackage>>> {
        self.crates.borrow().get(package_id).cloned()
    }

    /// Routes straight to the one package that could own `def_id`
    /// (`def_id.package_id`, the same trick `find_hir_impl_method`/
    /// `find_hir_enum_for_variant` use above) instead of requiring the
    /// caller to already have this workspace's mutable, ambient
    /// `current_package()` set to the right value — a `DefId` already
    /// names its own package, so nothing needs to be "focused" first.
    pub fn compiled_package_for_def(
        &self,
        def_id: crate::hir::DefId,
    ) -> Option<Rc<RefCell<AstPackage>>> {
        self.hir_packages.borrow().get(&def_id.package_id).cloned()
    }

    /// This workspace's one registered provider — a compile always already
    /// has exactly this in hand by the time it needs, e.g.,
    /// `PackageProvider::intrinsic_normalizer()`, so there's no need to
    /// resolve a package id first the way `provider_for` does.
    pub fn provider(&self) -> &Arc<dyn PackageProvider> {
        &self.providers
    }

    pub fn provider_for(&self, package_id: &PackageId) -> Option<Arc<dyn PackageProvider>> {
        let owns_package = self
            .providers
            .list_packages()
            .map(|packages| packages.iter().any(|id| id == package_id))
            .unwrap_or(false);
        owns_package.then(|| self.providers.clone())
    }

    /// Names of every registered package, whether or not it's been loaded
    /// yet — used to seed root-module recognition so `use std::...`-style
    /// paths resolve correctly even before `std` is actually loaded.
    pub fn registered_names(&self) -> Vec<String> {
        self.providers
            .list_packages()
            .ok()
            .into_iter()
            .flatten()
            .map(|id| id.as_str().to_owned())
            .collect()
    }

    /// The current workspace's own packages — as opposed to
    /// `registered_names()`, which also includes anything else this
    /// workspace's provider can supply (e.g. `std`, blended in by
    /// `CompositeProvider`). See `PackageProvider::workspace_packages`'s
    /// doc comment.
    pub fn workspace_packages(&self) -> Vec<String> {
        self.providers
            .workspace_packages()
            .ok()
            .into_iter()
            .flatten()
            .map(|id| id.as_str().to_owned())
            .collect()
    }

    pub fn module_paths(&self) -> Vec<QualifiedPath> {
        let mut paths: Vec<_> = self
            .sorted_packages()
            .into_iter()
            .flat_map(|package| {
                package
                    .borrow()
                    .module_paths
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .collect();
        paths.sort_by_key(|path| path.to_key());
        paths
    }

    /// Search every crate for a struct at `path`, borrowing each crate just
    /// long enough to check — the one clone that remains is the matched
    /// item itself, needed regardless to build an owned `Ty::Struct(..)`.
    pub fn find_struct(&self, path: &QualifiedPath) -> Option<TypeStruct> {
        for krate in self.sorted_packages() {
            if let Some(s) = krate.borrow().struct_defs.get(path) {
                return Some(s.clone());
            }
        }
        if let Some(prelude) = self.prelude.borrow().as_ref() {
            if let Some(s) = prelude.borrow().struct_defs.get(path) {
                return Some(s.clone());
            }
        }
        None
    }

    /// Cross-crate counterpart to `find_struct`, for enums (e.g.
    /// `std::option::Option`/`std::result::Result`, defined in `std`'s own
    /// the enum's own package, not whatever crate is currently being typed).
    pub fn find_enum(&self, path: &QualifiedPath) -> Option<TypeEnum> {
        for krate in self.sorted_packages() {
            if let Some(e) = krate.borrow().enum_defs.get(path) {
                return Some(e.clone());
            }
        }
        if let Some(prelude) = self.prelude.borrow().as_ref() {
            if let Some(e) = prelude.borrow().enum_defs.get(path) {
                return Some(e.clone());
            }
        }
        None
    }

    pub fn find_function_sig(&self, path: &QualifiedPath) -> Option<FunctionSignature> {
        for krate in self.sorted_packages() {
            if let Some(sig) = krate.borrow().function_sigs.get(path) {
                return Some(sig.clone());
            }
        }
        if let Some(prelude) = self.prelude.borrow().as_ref() {
            if let Some(sig) = prelude.borrow().function_sigs.get(path) {
                return Some(sig.clone());
            }
        }
        None
    }

    /// Search every crate for `path`'s inherent methods (see
    /// `AstPackage::method_sigs`'s doc comment) -- the cross-crate
    /// counterpart to `own_method_sigs` in `fp-typing`, mirroring
    /// `find_struct`/`find_function_sig` exactly.
    pub fn find_method_sigs(&self, path: &QualifiedPath) -> Option<Vec<(String, MethodSignature)>> {
        for krate in self.sorted_packages() {
            if let Some(sigs) = krate.borrow().method_sigs.get(path) {
                return Some(sigs.clone());
            }
        }
        if let Some(prelude) = self.prelude.borrow().as_ref() {
            if let Some(sigs) = prelude.borrow().method_sigs.get(path) {
                return Some(sigs.clone());
            }
        }
        None
    }

    pub fn has_module(&self, path: &QualifiedPath) -> bool {
        self.crates
            .borrow()
            .values()
            .any(|krate| krate.borrow().module_paths.contains(path))
            || self
                .prelude
                .borrow()
                .as_ref()
                .is_some_and(|prelude| prelude.borrow().module_paths.contains(path))
    }

    /// Borrow the root map directly. Used by callers that need to iterate
    /// every crate themselves (e.g. an early-return tail-name search, or
    /// gathering LIR units) rather than looking up one qualified path.
    pub fn crates(&self) -> Ref<'_, HashMap<PackageId, Rc<RefCell<AstPackage>>>> {
        self.crates.borrow()
    }

    /// A `TargetBackend`'s view of `id` as a `AstPackage` — every
    /// AST-emitting backend's input. `id` must already be loaded via
    /// `begin_package`/`import_package`.
    pub fn package_source(&self, id: &PackageId) -> crate::error::Result<AstPackage> {
        let package = self.compiled_package(id).ok_or_else(|| {
            crate::error::Error::from(format!(
                "package `{id}` is not present in this compiled workspace"
            ))
        })?;
        Ok(package.borrow().clone())
    }

    // `merged_lir_program` moved to `lir::LirProgram::merged_blob_for_package`
    // — LIR content no longer lives on `AstProgram`/`AstPackage`. The
    // main-function-rename step it also used to do now happens at the call
    // site (`fp-cli`'s `run_compile_pipeline`), which has both the merged
    // `LirProgram` and the HIR data needed to resolve `main`'s `DefId`.
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::package::graph::PackageGraph;
    use crate::ast::package::provider::EmptyProvider;

    #[test]
    fn package_workspace_inherits_installed_prelude() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = workspace.for_package(PackageId::new("parent"));
        let prelude = Rc::new(RefCell::new(AstPackage::new(
            PackageId::new("std"),
            "std",
            PackageGraph::new(Vec::new()),
        )));
        parent.install_prelude(prelude.clone());

        let child = parent.for_package(PackageId::new("child"));

        let inherited = child
            .prelude_package()
            .expect("package workspace should retain the installed prelude");
        assert!(Rc::ptr_eq(&inherited, &prelude));
    }

    #[test]
    fn package_workspace_observes_prelude_installed_after_creation() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = workspace.for_package(PackageId::new("parent"));
        let child = parent.for_package(PackageId::new("child"));
        let prelude = Rc::new(RefCell::new(AstPackage::new(
            PackageId::new("std"),
            "std",
            PackageGraph::new(Vec::new()),
        )));

        parent.install_prelude(prelude.clone());

        assert!(Rc::ptr_eq(
            &child
                .prelude_package()
                .expect("package workspace should observe later prelude installs"),
            &prelude
        ));
    }

    #[test]
    fn package_workspace_inherits_compiled_dependencies() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = workspace.for_package(PackageId::new("parent"));
        let dependency = parent.begin_package(
            PackageId::new("dependency"),
            AstPackage::new(
                PackageId::new("dependency"),
                "dependency",
                PackageGraph::new(Vec::new()),
            ),
            crate::lir::LirDataLayout::x86_64(),
        );

        let child = parent.for_package(PackageId::new("child"));
        let inherited = child
            .compiled_package(&PackageId::new("dependency"))
            .expect("package workspace should retain compiled dependencies");
        assert!(Rc::ptr_eq(&inherited, &dependency));
        assert!(
            child
                .compiled_package_for_def(crate::hir::DefId::new(
                    dependency.borrow().hir_package_id.clone(),
                    0,
                ))
                .is_some()
        );
    }

    #[test]
    fn package_workspace_observes_dependencies_published_after_creation() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = workspace.for_package(PackageId::new("parent"));
        let child = parent.for_package(PackageId::new("child"));
        let dependency = parent.begin_package(
            PackageId::new("dependency"),
            AstPackage::new(
                PackageId::new("dependency"),
                "dependency",
                PackageGraph::new(Vec::new()),
            ),
            crate::lir::LirDataLayout::x86_64(),
        );

        assert!(Rc::ptr_eq(
            &child
                .compiled_package(&PackageId::new("dependency"))
                .expect("package workspace should observe later package publication"),
            &dependency
        ));
        assert!(
            child
                .compiled_package_for_def(crate::hir::DefId::new(
                    dependency.borrow().hir_package_id.clone(),
                    0,
                ))
                .is_some()
        );
    }
}
