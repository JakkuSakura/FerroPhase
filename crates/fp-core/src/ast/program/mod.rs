use crate::ast::package::provider::PackageProvider;
use crate::ast::package::{AstPackage, PackageId, PackageMetadata};
use crate::ast::path::QualifiedPath;
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
}

impl AstProgram {
    pub fn new(provider: Arc<dyn PackageProvider>) -> Self {
        Self {
            crates: Rc::new(RefCell::new(HashMap::new())),
            providers: provider,
        }
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
        let source = source;
        let krate = Rc::new(RefCell::new(source));
        self.crates
            .borrow_mut()
            .insert(source_package_id, krate.clone());
        krate
    }

    pub fn import_package(&self, package_id: PackageId, package: Rc<RefCell<AstPackage>>) {
        self.crates.borrow_mut().insert(package_id, package);
    }

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
                .package
                .metadata
                .clone()
                .into()
        })
    }

    /// Direct lookup of an AST package by its shared package id.
    pub fn get_ast_package(&self, package_id: &PackageId) -> Rc<RefCell<AstPackage>> {
        self.crates
            .borrow()
            .get(package_id)
            .cloned()
            .unwrap_or_else(|| panic!("AST package for HIR package `{package_id}` is missing"))
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
    use crate::ast::package::provider::EmptyProvider;

    #[test]
    fn package_workspace_inherits_compiled_dependencies() {
        let workspace = AstProgram::new(Arc::new(EmptyProvider));
        let parent = &workspace;
        let dependency = parent.begin_package(
            PackageId::new("dependency"),
            AstPackage::new(
                PackageId::new("dependency"),
                "dependency",
                Vec::new(),
            ),
            crate::lir::LirDataLayout::x86_64(),
        );

        let child = &workspace;
        let inherited = child
            .compiled_package(&PackageId::new("dependency"))
            .expect("package workspace should retain compiled dependencies");
        assert!(Rc::ptr_eq(&inherited, &dependency));
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
                Vec::new(),
            ),
            crate::lir::LirDataLayout::x86_64(),
        );

        assert!(Rc::ptr_eq(
            &child
                .compiled_package(&PackageId::new("dependency"))
                .expect("package workspace should observe later package publication"),
            &dependency
        ));
    }
}
