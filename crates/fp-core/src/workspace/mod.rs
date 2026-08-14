use crate::common_struct;
use serde_json::Value;
use std::hash::{Hash, Hasher};

common_struct! {
    pub struct WorkspaceDocument {
        pub manifest: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub packages: Vec<WorkspacePackage>,
    }
}

impl WorkspaceDocument {
    pub fn new(manifest: impl Into<String>) -> Self {
        Self {
            manifest: manifest.into(),
            packages: Vec::new(),
        }
    }

    pub fn with_packages(mut self, packages: Vec<WorkspacePackage>) -> Self {
        self.packages = packages;
        self
    }
}

common_struct! {
    pub struct WorkspacePackage {
        pub name: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub version: Option<String>,
        pub manifest_path: String,
        pub root: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub modules: Vec<WorkspaceModule>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub features: Vec<String>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub dependencies: Vec<WorkspaceDependency>,
    }
}

impl WorkspacePackage {
    pub fn new(
        name: impl Into<String>,
        manifest_path: impl Into<String>,
        root: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            version: None,
            manifest_path: manifest_path.into(),
            root: root.into(),
            modules: Vec::new(),
            features: Vec::new(),
            dependencies: Vec::new(),
        }
    }

    pub fn with_version(mut self, version: Option<String>) -> Self {
        self.version = version;
        self
    }

    pub fn with_modules(mut self, modules: Vec<WorkspaceModule>) -> Self {
        self.modules = modules;
        self
    }

    pub fn with_features(mut self, features: Vec<String>) -> Self {
        self.features = features;
        self
    }

    pub fn with_dependencies(mut self, dependencies: Vec<WorkspaceDependency>) -> Self {
        self.dependencies = dependencies;
        self
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WorkspaceModule {
    pub id: String,
    pub path: String,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub module_path: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub required_features: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub snapshot: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ast: Option<Value>,
}

impl WorkspaceModule {
    pub fn new(id: impl Into<String>, path: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            path: path.into(),
            module_path: Vec::new(),
            language: None,
            required_features: Vec::new(),
            snapshot: None,
            ast: None,
        }
    }

    pub fn with_module_path(mut self, module_path: Vec<String>) -> Self {
        self.module_path = module_path;
        self
    }

    pub fn with_language(mut self, language: Option<String>) -> Self {
        self.language = language;
        self
    }

    pub fn with_required_features(mut self, features: Vec<String>) -> Self {
        self.required_features = features;
        self
    }

    pub fn with_snapshot(mut self, snapshot: Option<String>) -> Self {
        self.snapshot = snapshot;
        self
    }

    pub fn with_ast(mut self, ast: Option<Value>) -> Self {
        self.ast = ast;
        self
    }
}

impl WorkspaceModule {
    fn ast_repr(&self) -> Option<String> {
        self.ast
            .as_ref()
            .and_then(|value| serde_json::to_string(value).ok())
    }
}

impl PartialEq for WorkspaceModule {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.path == other.path
            && self.module_path == other.module_path
            && self.language == other.language
            && self.required_features == other.required_features
            && self.snapshot == other.snapshot
            && self.ast == other.ast
    }
}

impl Eq for WorkspaceModule {}

impl Hash for WorkspaceModule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.path.hash(state);
        self.module_path.hash(state);
        self.language.hash(state);
        self.required_features.hash(state);
        self.snapshot.hash(state);
        if let Some(json) = self.ast_repr() {
            json.hash(state);
        }
    }
}

common_struct! {
    pub struct WorkspaceDependency {
        pub name: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub kind: Option<String>,
    }
}

impl WorkspaceDependency {
    pub fn new(name: impl Into<String>, kind: Option<String>) -> Self {
        Self {
            name: name.into(),
            kind,
        }
    }
}

// ── Compiled workspace context (typer lookup) ────────────────────

use crate::ast::{FunctionSignature, MethodSignature, TypeEnum, TypeStruct};
use crate::hir::PackageId as HirPackageId;
use crate::ast::path::QualifiedPath;
use crate::package::provider::PackageProvider;
use crate::package::{CompiledPackage, PackageId, PackageSource};
use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Shared registry of provider-owned packages and compiler-owned package
/// results for one compilation session. Dependencies are published here by
/// the compiler driver before their dependents are typed.
pub struct WorkspaceContext {
    crates: RefCell<HashMap<PackageId, Rc<RefCell<CompiledPackage>>>>,
    /// The single package provider for this workspace, required at
    /// construction and never changed afterward. Callers that need to
    /// combine several concrete providers (e.g. a language's std/libc
    /// provider plus the real input-package provider) build a
    /// `CompositeProvider` wrapping them before constructing the
    /// workspace — `WorkspaceContext` itself never needs to search a list
    /// of providers (previously O(providers × package-list) per lookup,
    /// called once per package in the dependency graph).
    providers: Arc<dyn PackageProvider>,
    current_package: Option<PackageId>,
    prelude: RefCell<Option<Rc<RefCell<CompiledPackage>>>>,
    next_package_id: Rc<Cell<u32>>,
    /// Reverse index from a package's *HIR* id (the `package_id` embedded
    /// in every `hir::DefId` minted while lowering it) back to its
    /// `CompiledPackage` — lets a HIR-level, `DefId`-keyed lookup
    /// (`find_hir_impl_method`) go straight to the one package that could
    /// possibly own that `DefId` (an inherent impl's items are always
    /// minted in the same package as the impl itself) instead of searching
    /// every loaded package.
    hir_packages: RefCell<HashMap<HirPackageId, Rc<RefCell<CompiledPackage>>>>,
    /// Memoized, name-sorted snapshot of `crates`'s values — the package
    /// set only ever changes via `begin_package`/`import_package` (both
    /// invalidate this), so `sorted_packages` doesn't need to rebuild a
    /// `String` per package and re-sort on every one of its many callers
    /// (`find_export`, `find_struct`/`find_enum`/`find_function_sig`,
    /// `method_sigs`, `module_paths`, `hir_definitions`, ...) — this runs
    /// once per unqualified identifier/path reference across every
    /// compiled file.
    sorted_packages_cache: RefCell<Option<Vec<Rc<RefCell<CompiledPackage>>>>>,
}

impl WorkspaceContext {
    pub fn new(provider: Arc<dyn PackageProvider>) -> Self {
        Self {
            crates: RefCell::new(HashMap::new()),
            providers: provider,
            current_package: None,
            prelude: RefCell::new(None),
            next_package_id: Rc::new(Cell::new(0)),
            hir_packages: RefCell::new(HashMap::new()),
            sorted_packages_cache: RefCell::new(None),
        }
    }

    fn sorted_packages(&self) -> Vec<Rc<RefCell<CompiledPackage>>> {
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

    /// Create an isolated package workspace. Provider registrations are
    /// shared, while compiled package entries are imported explicitly.
    pub fn for_package(&self, package_id: PackageId) -> Self {
        Self {
            crates: RefCell::new(HashMap::new()),
            providers: self.providers.clone(),
            current_package: Some(package_id),
            prelude: RefCell::new(None),
            next_package_id: self.next_package_id.clone(),
            hir_packages: RefCell::new(HashMap::new()),
            sorted_packages_cache: RefCell::new(None),
        }
    }

    pub fn current_package(&self) -> Option<&PackageId> {
        self.current_package.as_ref()
    }

    /// Publish a package source slot and return its compiler-owned result.
    pub fn begin_package(
        &self,
        package_id: PackageId,
        source: PackageSource,
        data_layout: crate::lir::LirDataLayout,
    ) -> Rc<RefCell<CompiledPackage>> {
        let source_package_id = package_id.clone();
        let name = source.name.clone();
        let hir_package_id = HirPackageId(self.next_package_id.get());
        self.next_package_id.set(hir_package_id.0.saturating_add(1));
        let mut krate = CompiledPackage::new(
            hir_package_id,
            name.clone(),
            source.graph.clone(),
            data_layout,
        );
        krate.module_paths = source.module_paths;
        krate.items = source.items;
        let krate = Rc::new(RefCell::new(krate));
        self.crates
            .borrow_mut()
            .insert(source_package_id, krate.clone());
        self.hir_packages
            .borrow_mut()
            .insert(hir_package_id, krate.clone());
        *self.sorted_packages_cache.borrow_mut() = None;
        krate
    }

    pub fn import_package(&self, package_id: PackageId, package: Rc<RefCell<CompiledPackage>>) {
        let hir_package_id = package.borrow().package_id;
        self.hir_packages
            .borrow_mut()
            .insert(hir_package_id, package.clone());
        self.crates.borrow_mut().insert(package_id, package);
        *self.sorted_packages_cache.borrow_mut() = None;
    }

    /// Install `std`'s published package as the unqualified prelude lookup
    /// source for ordinary packages. The standard and libc packages do not
    /// import their own prelude.
    pub fn install_prelude(&self, package: Rc<RefCell<CompiledPackage>>) {
        let Some(current_package) = self.current_package.as_ref() else {
            return;
        };
        if matches!(current_package.as_str(), "std" | "libc") {
            return;
        }
        self.prelude.borrow_mut().replace(package);
    }

    pub fn prelude_package(&self) -> Option<Rc<RefCell<CompiledPackage>>> {
        self.prelude.borrow().clone()
    }

    /// Return immutable HIR definitions published by imported packages.
    pub fn hir_definitions(
        &self,
    ) -> Vec<(
        QualifiedPath,
        crate::hir::Program,
        HashMap<String, crate::hir::Res>,
    )> {
        self.sorted_packages()
            .into_iter()
            .filter_map(|package| {
                let package = package.borrow();
                package.hir_program.clone().map(|program| {
                    (
                        QualifiedPath::new(Vec::new()),
                        program,
                        package.hir_exports.clone(),
                    )
                })
            })
            .collect()
    }

    /// Cross-package counterpart to `find_struct`, but against each
    /// package's *HIR* program rather than the AST-level `struct_defs`
    /// registry — used where the caller specifically needs a `hir::DefId`
    /// (the HIR type-checking pass). O(1) per package via
    /// `hir_struct_defs_by_name`, built once when the package's HIR is
    /// published (`CompiledPackage::set_hir_program`), unlike
    /// `hir_definitions()`, which clones every package's whole HIR
    /// `Program` on every call.
    pub fn find_hir_struct_def_id(&self, name: &str) -> Option<crate::hir::DefId> {
        for package in self.sorted_packages() {
            if let Some(def_id) = package.borrow().hir_struct_defs_by_name.get(name) {
                return Some(*def_id);
            }
        }
        None
    }

    /// Cross-package counterpart to `hir_typeck::expr_path_ty`'s local
    /// associated-method fallback — finds the `impl` block and method whose
    /// `ImplItem::def_id` matches `def_id`. An inherent impl's items are
    /// always minted in the same package as the impl itself (the orphan
    /// rule's HIR-level consequence), so `def_id.package_id` already names
    /// the *only* package that could hold it — go straight to it via
    /// `hir_packages` instead of searching every loaded package. Within
    /// that one package, `hir_impl_method_item_index` (built once when its
    /// HIR is published, alongside `hir_struct_defs_by_name` above) gives
    /// the enclosing impl item's index directly; only that impl's own
    /// method list is then scanned to clone the specific
    /// generics/self-type/items/function the caller needs — instead of
    /// `hir_definitions()`'s full clone of every dependency package's
    /// whole HIR `Program`.
    pub fn find_hir_impl_method(
        &self,
        def_id: crate::hir::DefId,
    ) -> Option<(
        crate::hir::Generics,
        crate::hir::TypeExpr,
        Vec<crate::hir::ImplItem>,
        crate::hir::Function,
    )> {
        let package = self.hir_packages.borrow().get(&def_id.package_id)?.clone();
        let package = package.borrow();
        let &item_index = package.hir_impl_method_item_index.get(&def_id)?;
        let program = package.hir_program.as_ref()?;
        let item = program.items.get(item_index)?;
        let crate::hir::ItemKind::Impl(impl_item) = &item.kind else {
            return None;
        };
        let function = impl_item.items.iter().find_map(|impl_member| {
            if impl_member.def_id != def_id {
                return None;
            }
            match &impl_member.kind {
                crate::hir::ImplItemKind::Method(function) => Some(function.clone()),
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

    /// Cross-package counterpart to `find_struct`/`find_enum`, for a
    /// value/type symbol exported by some other package's `hir_exports`
    /// (e.g. `libc::macos::getenv`), looked up lazily by its fully
    /// qualified key instead of being eagerly copied into the caller's
    /// own `global_value_defs`/`global_type_defs`.
    pub fn find_export(&self, key: &str) -> Option<crate::hir::Res> {
        for package in self.sorted_packages() {
            if let Some(res) = package.borrow().hir_exports.get(key) {
                return Some(res.clone());
            }
        }
        None
    }

    pub fn is_loaded(&self, package_id: &PackageId) -> bool {
        self.crates.borrow().contains_key(package_id)
    }

    pub fn compiled_package(&self, package_id: &PackageId) -> Option<Rc<RefCell<CompiledPackage>>> {
        self.crates.borrow().get(package_id).cloned()
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
    /// `CompiledPackage`, not whatever crate is currently being typed).
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
    /// `CompiledPackage::method_sigs`'s doc comment) -- the cross-crate
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
    pub fn crates(&self) -> Ref<'_, HashMap<PackageId, Rc<RefCell<CompiledPackage>>>> {
        self.crates.borrow()
    }
}
