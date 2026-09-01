// ── Compiled workspace context (typer lookup) ────────────────────

use crate::ast::package::provider::PackageProvider;
use crate::ast::package::{AstPackage, PackageId, PackageMetadata};
use crate::ast::path::QualifiedPath;
use crate::ast::{FunctionSignature, MethodSignature, TypeEnum, TypeStruct};
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
    prelude: Rc<RefCell<Option<Rc<RefCell<AstPackage>>>>>,
    /// Memoized, name-sorted snapshot of `crates`'s values — the package
    /// set only ever changes via `begin_package`/`import_package` (both
    /// invalidate this), so `sorted_packages` doesn't need to rebuild a
    /// `String` per package and re-sort on every one of its many callers
    /// (`find_struct`/`find_enum`/`find_function_sig`, `method_sigs`,
    /// `module_paths`, ...) — this runs once per unqualified
    /// identifier/path reference across every compiled file.
    sorted_packages_cache: Rc<RefCell<Option<Vec<Rc<RefCell<AstPackage>>>>>>,
    /// Explicit AST local-resolution state. Lowering borrows this facade;
    /// it does not own lexical/local binding maps.
    local_scope: Rc<RefCell<crate::ast::resolve::LocalScope>>,
}

impl AstProgram {
    pub fn new(provider: Arc<dyn PackageProvider>) -> Self {
        Self {
            crates: Rc::new(RefCell::new(HashMap::new())),
            providers: provider,
            prelude: Rc::new(RefCell::new(None)),
            sorted_packages_cache: Rc::new(RefCell::new(None)),
            local_scope: Rc::new(RefCell::new(crate::ast::resolve::LocalScope::new())),
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

    pub fn reset_local_scope(&self) {
        *self.local_scope.borrow_mut() = crate::ast::resolve::LocalScope::new();
    }

    pub fn enter_local_scope(&self) {
        self.local_scope.borrow_mut().enter();
    }

    pub fn leave_local_scope(&self) {
        self.local_scope.borrow_mut().leave();
    }

    pub fn resolve_local(
        &self,
        name: &str,
        namespace: crate::ast::resolve::Namespace,
    ) -> crate::ast::resolve::ResolutionResult {
        self.local_scope.borrow().resolve(name, namespace, self.provider().resolution_rules())
    }

    pub fn declare_local(
        &self,
        name: impl Into<crate::ast::resolve::Symbol>,
        binding: crate::ast::resolve::Binding,
    ) -> crate::ast::resolve::DeclarationOutcome {
        self.local_scope.borrow_mut().declare(name, binding, self.provider().declaration_rules())
    }

    /// Run the AST-stage resolver setup for a loaded package. Declaration
    /// collection is intentionally separate from HIR lowering; this method
    /// seeds the persistent module tree before any AST→HIR consumer runs.
    pub fn resolve_package(&self, package_id: &PackageId) -> crate::error::Result<()> {
        let package = self
            .crates
            .borrow()
            .get(package_id)
            .cloned()
            .ok_or_else(|| crate::error::Error::from(format!("package not found: {package_id}")))?;
        let mut package = package.borrow_mut();
        let paths: Vec<_> = package
            .module_paths
            .iter()
            .cloned()
            .chain(package.items.iter().map(|item| item.module_path.clone()))
            .collect();
        let items = package.items.clone();
        let mut resolver = crate::ast::resolve::AstResolver::for_package(
            package_id.clone(),
            &mut package.module_tree,
            self.providers.declaration_rules(),
            self.providers.resolution_rules(),
        );
        for path in paths {
            resolver.modules.ensure_module(&path);
        }
        resolver.collect_package_items(&items);
        let resolutions = resolver.resolution_table().clone();
        drop(resolver);
        package.resolutions = resolutions;
        Ok(())
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
        let hir_package_id = package_id.clone();
        let mut source = source;
        source.hir_package_id = hir_package_id.clone();
        let krate = Rc::new(RefCell::new(source));
        self.crates
            .borrow_mut()
            .insert(source_package_id, krate.clone());
        *self.sorted_packages_cache.borrow_mut() = None;
        krate
    }

    pub fn import_package(&self, package_id: PackageId, package: Rc<RefCell<AstPackage>>) {
        self.crates.borrow_mut().insert(package_id, package);
        *self.sorted_packages_cache.borrow_mut() = None;
    }

    /// Install `std`'s published package as the unqualified prelude lookup
    /// source for ordinary packages. The standard and libc packages do not
    /// import their own prelude.
    pub fn install_prelude(&self, package: Rc<RefCell<AstPackage>>) {
        self.prelude.borrow_mut().replace(package);
    }

    pub fn prelude_package(&self) -> Option<Rc<RefCell<AstPackage>>> {
        self.prelude.borrow().clone()
    }

    // Cross-package HIR export lookup is owned by `hir::HirProgram`; AST
    // packages intentionally expose no global first-match resolver.

    pub fn is_loaded(&self, package_id: &PackageId) -> bool {
        self.crates.borrow().contains_key(package_id)
    }

    pub fn compiled_package(&self, package_id: &PackageId) -> Option<Rc<RefCell<AstPackage>>> {
        self.crates.borrow().get(package_id).cloned()
    }

    /// Provider-owned metadata for a package already loaded into this
    /// compilation session. Consumers such as name resolution use this
    /// rather than inspecting an `AstPackage`'s storage directly.
    pub fn package_metadata(&self, package_id: &PackageId) -> Option<PackageMetadata> {
        self.compiled_package(package_id).and_then(|package| {
            package
                .borrow()
                .graph
                .package(package_id)
                .map(|descriptor| descriptor.metadata.clone())
        })
    }

    /// Routes straight to the one package that could own `def_id`.
    pub fn compiled_package_for_def(
        &self,
        def_id: crate::hir::DefId,
    ) -> Option<Rc<RefCell<AstPackage>>> {
        self.crates.borrow().get(&def_id.package_id).cloned()
    }

    /// Direct lookup of an AST package by its shared package id.
    pub fn get_ast_package(&self, package_id: &PackageId) -> Rc<RefCell<AstPackage>> {
        self.crates
            .borrow()
            .get(package_id)
            .cloned()
            .unwrap_or_else(|| {
                panic!("AST package for HIR package `{package_id}` is missing")
            })
    }

    /// Resolve a name through the AST package's module tree. Consumers should
    /// use this boundary API rather than reaching into tree storage.
    pub fn resolve_module_name(
        &self,
        package_id: &PackageId,
        module: &QualifiedPath,
        name: &str,
        namespace: crate::ast::resolve::Namespace,
    ) -> crate::ast::resolve::ResolutionResult {
        let package = self.get_ast_package(package_id);
        let package = package.borrow();
        package.module_tree.resolve(
            module,
            name,
            namespace,
            self.provider().resolution_rules(),
        )
    }

    /// Resolve a qualified path through the AST package's module tree.
    pub fn resolve_module_path(
        &self,
        package_id: &PackageId,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: crate::ast::resolve::Namespace,
    ) -> crate::ast::resolve::ResolutionResult {
        let package = self.get_ast_package(package_id);
        let package = package.borrow();
        package.module_tree.resolve_path(
            module,
            path,
            namespace,
            self.provider().resolution_rules(),
        )
    }

    pub fn resolve_module_path_final(
        &self,
        package_id: &PackageId,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: crate::ast::resolve::Namespace,
    ) -> crate::ast::resolve::ResolutionResult {
        let package = self.get_ast_package(package_id);
        let package = package.borrow();
        package.module_tree.resolve_path_final(
            module,
            path,
            namespace,
            self.provider().resolution_rules(),
        )
    }

    /// Resolve an extern-prelude path against the AST package that owns its
    /// crate root. HIR has no resolver facade; all path resolution terminates
    /// here before lowering.
    pub fn resolve_external_path(
        &self,
        path: &QualifiedPath,
        namespace: crate::ast::resolve::Namespace,
    ) -> Option<crate::hir::Res> {
        let root = path.head()?.replace('-', "_");
        let package_id = self
            .crates()
            .keys()
            .find(|id| id.as_str().replace('-', "_") == root)?
            .clone();
        let package = self.get_ast_package(&package_id);
        let package = package.borrow();
        let internal = QualifiedPath::new(path.segments.iter().skip(1).cloned().collect());
        let leaf = internal.segments.last()?.clone();
        let module = QualifiedPath::new(internal.segments[..internal.segments.len() - 1].to_vec());
        match package.module_tree.resolve(&module, &leaf, namespace, self.provider().resolution_rules()) {
            crate::ast::resolve::ResolutionResult::Found(res) => Some(res),
            _ => None,
        }
    }

    /// Resolve an extern-prelude path while respecting the requesting
    /// package's declared dependency edges. This mirrors rustc's extern
    /// prelude: a loaded package is not automatically visible merely because
    /// it happens to exist in the compilation registry.
    pub fn resolve_external_path_from(
        &self,
        requester: &PackageId,
        path: &QualifiedPath,
        namespace: crate::ast::resolve::Namespace,
    ) -> Option<crate::hir::Res> {
        let root = path.head()?.replace('-', "_");
        let target = self
            .crates
            .borrow()
            .keys()
            .find(|id| id.as_str().replace('-', "_") == root)
            .cloned()?;
        let allowed = self
            .compiled_package(requester)
            .and_then(|package| {
                package
                    .borrow()
                    .graph
                    .package(requester)
                    .map(|descriptor| descriptor.metadata.dependencies.iter().filter_map(|dep| dep.resolved_package_id.clone()).collect::<Vec<_>>())
            })
            .is_some_and(|deps| deps.iter().any(|id| id == &target));
        if !allowed {
            return None;
        }
        self.resolve_external_path(path, namespace)
    }

    pub fn resolve_external_module_path_from(
        &self,
        requester: &PackageId,
        path: &QualifiedPath,
    ) -> Option<QualifiedPath> {
        let root = path.head()?.replace('-', "_");
        let target = self
            .crates
            .borrow()
            .keys()
            .find(|id| id.as_str().replace('-', "_") == root)
            .cloned()?;
        let allowed = self
            .compiled_package(requester)
            .and_then(|package| {
                package
                    .borrow()
                    .graph
                    .package(requester)
                    .map(|descriptor| descriptor.metadata.dependencies.iter().filter_map(|dep| dep.resolved_package_id.clone()).collect::<Vec<_>>())
            })
            .is_some_and(|deps| deps.iter().any(|id| id == &target));
        if !allowed {
            return None;
        }
        self.resolve_external_module_path(path)
    }

    pub fn resolve_external_module_path(&self, path: &QualifiedPath) -> Option<QualifiedPath> {
        let root = path.head()?.replace('-', "_");
        let package_id = self.crates().keys().find(|id| id.as_str().replace('-', "_") == root)?.clone();
        let package = self.get_ast_package(&package_id);
        let package = package.borrow();
        let internal = QualifiedPath::new(path.segments.iter().skip(1).cloned().collect());
        package.module_tree.module(&internal).map(|_| path.clone())
    }

    pub fn external_module_member_names(&self, path: &QualifiedPath) -> Option<Vec<String>> {
        let root = path.head()?.replace('-', "_");
        let package_id = self.crates().keys().find(|id| id.as_str().replace('-', "_") == root)?.clone();
        let package = self.get_ast_package(&package_id);
        let package = package.borrow();
        let internal = QualifiedPath::new(path.segments.iter().skip(1).cloned().collect());
        let module = package.module_tree.module(&internal)?;
        Some(module.symbols.keys().map(ToString::to_string).collect())
    }

    pub fn module_exists(&self, package_id: &PackageId, path: &QualifiedPath) -> bool {
        self.get_ast_package(package_id)
            .borrow()
            .module_tree
            .module(path)
            .is_some()
    }

    pub fn module_member_names(
        &self,
        package_id: &PackageId,
        path: &QualifiedPath,
    ) -> Option<Vec<crate::ast::resolve::Symbol>> {
        let package = self.get_ast_package(package_id);
        let package = package.borrow();
        Some(package.module_tree.module(path)?.symbols.keys().cloned().collect())
    }

    pub fn resolve_module_member(
        &self,
        package_id: &PackageId,
        module: &QualifiedPath,
        name: &str,
        namespace: crate::ast::resolve::Namespace,
    ) -> crate::ast::resolve::ResolutionResult {
        self.resolve_module_name(package_id, module, name, namespace)
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
        let parent = &workspace;
        let prelude = Rc::new(RefCell::new(AstPackage::new(
            PackageId::new("std"),
            "std",
            PackageGraph::new(Vec::new()),
        )));
        parent.install_prelude(prelude.clone());

        let child = &workspace;

        let inherited = child
            .prelude_package()
            .expect("package workspace should retain the installed prelude");
        assert!(Rc::ptr_eq(&inherited, &prelude));
    }

    #[test]
    fn package_workspace_observes_prelude_installed_after_creation() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = &workspace;
        let child = &workspace;
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
        let parent = &workspace;
        let dependency = parent.begin_package(
            PackageId::new("dependency"),
            AstPackage::new(
                PackageId::new("dependency"),
                "dependency",
                PackageGraph::new(Vec::new()),
            ),
            crate::lir::LirDataLayout::x86_64(),
        );

        let child = &workspace;
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
        let parent = &workspace;
        let child = &workspace;
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
