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
use crate::ast::package::provider::PackageProvider;
use crate::ast::package::{CompiledPackage, PackageId, PackageSource};
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
    /// Reverse index over every loaded package's `hir_exports`, keyed by
    /// each export key's LAST path segment (e.g. `"core::option::Option"`
    /// indexes under `"Option"`) — `find_export_by_name`/
    /// `find_export_by_suffix` used to linear-scan every package's full
    /// `hir_exports` on every call, which made cross-package bare-name
    /// resolution (`Option`, `Some`, `None`, called repeatedly throughout
    /// a compile) pathologically slow once a dependency the size of real
    /// std was loaded. Cached alongside the total export count it was
    /// built from across all packages; rebuilt only when that total grows.
    export_suffix_index: RefCell<Option<(usize, HashMap<String, Vec<(String, crate::hir::Res)>>)>>,
    /// Memoized `find_hir_impl_method` results, keyed by the method's own
    /// `DefId` — that lookup used to clone the whole owning impl's
    /// `items: Vec<ImplItem>` (every one of its sibling methods included)
    /// on every single call, even though the same `DefId` always resolves
    /// to the same impl (a package's published HIR never changes once
    /// set). `Rc`, not owned, so a repeat caller gets a cheap `Rc` clone
    /// instead of re-paying for that.
    impl_method_cache: RefCell<
        HashMap<
            crate::hir::DefId,
            Rc<(
                crate::hir::Generics,
                crate::hir::TypeExpr,
                Vec<crate::hir::ImplItem>,
                crate::hir::Function,
            )>,
        >,
    >,
    /// Every published package's own HIR, incrementally maintained (see
    /// `publish_hir_program`) as each one finishes — not rebuilt from
    /// `crates`/`hir_packages` on demand. A consumer that wants to
    /// dispatch a cross-package `DefId` lookup itself (`MirLowering`, via
    /// `ComptimeRequest`) reads this directly (`hir_program()`, an `Rc`
    /// clone) instead of a caller pre-merging every dependency's
    /// `def_map` into one pretend-single-package `Package` first.
    hir_program: RefCell<Rc<crate::hir::Program>>,
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
            export_suffix_index: RefCell::new(None),
            impl_method_cache: RefCell::new(HashMap::new()),
            hir_program: RefCell::new(Rc::new(crate::hir::Program::new())),
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
            export_suffix_index: RefCell::new(None),
            impl_method_cache: RefCell::new(HashMap::new()),
            hir_program: RefCell::new(Rc::new(crate::hir::Program::new())),
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

    /// Incrementally folds a just-published package's own HIR into the
    /// persistent `hir::Program` this workspace maintains — called
    /// alongside `CompiledPackage::set_hir_program`, once per package, as
    /// each one finishes. `Rc::make_mut` clones the `Program`'s own
    /// `HashMap` (never any package's items/def_map/def_paths) only if
    /// some earlier `hir_program()` caller is still holding the previous
    /// `Rc` — the ordinary case (nobody holding a stale snapshot) is a
    /// plain in-place insert.
    pub fn publish_hir_program(&self, package: std::rc::Rc<crate::hir::Package>) {
        let mut current = self.hir_program.borrow_mut();
        Rc::make_mut(&mut current).packages.insert(package.id, package);
    }

    /// Returns this workspace's persistent `hir::Program` — an `Rc` clone,
    /// not a rebuild (see `publish_hir_program`, the only writer).
    pub fn hir_program(&self) -> std::rc::Rc<crate::hir::Program> {
        self.hir_program.borrow().clone()
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

    /// Return immutable HIR definitions published by imported packages —
    /// the current, real mechanism `ast_to_hir::seed_workspace_definitions`
    /// (`fp-backend`) merges into a package's own `hir::Package`, and that
    /// `fp-typing::hir_typeck` still calls directly at a few sites (see its
    /// own comments there for the narrower, targeted lookups it prefers
    /// elsewhere). Not legacy code awaiting deletion — copying each
    /// dependency's definitions per consuming package is real, current
    /// behavior; a future single-shared-program redesign remains a real
    /// architectural option, but until that lands this is the only
    /// mechanism that makes cross-package references work at all.
    ///
    /// Returns each dependency's `Rc<hir::Package>` (matching
    /// `CompiledPackage::hir_program`'s own storage) rather than an owned
    /// `Package` — this used to deep-clone every dependency's whole HIR
    /// program (every item, `def_map`, `def_paths`, `module_tree`) on
    /// every single call; an `Rc` clone is O(1).

    pub fn hir_definitions(
        &self,
    ) -> Vec<(
        QualifiedPath,
        std::rc::Rc<crate::hir::Package>,
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
    /// `hir::Package::struct_defs_by_name`, built once when the package's
    /// HIR is published (`CompiledPackage::set_hir_program`), unlike
    /// `hir_definitions()`, which clones every package's whole HIR
    /// `Program` on every call.
    pub fn find_hir_struct_def_id(&self, name: &str) -> Option<crate::hir::DefId> {
        for package in self.sorted_packages() {
            let package = package.borrow();
            if let Some(hir_program) = package.hir_program.as_ref() {
                if let Some(def_id) = hir_program.struct_defs_by_name.get(name) {
                    return Some(*def_id);
                }
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
    /// that one package, `hir::Package::impl_method_item_index` (built once
    /// when its HIR is published, alongside `struct_defs_by_name` above)
    /// gives the enclosing impl item's index directly; only that impl's own
    /// method list is then scanned to clone the specific
    /// generics/self-type/items/function the caller needs — instead of
    /// `hir_definitions()`'s full clone of every dependency package's
    /// whole HIR `Program`.
    ///
    /// Memoized by `def_id` in `impl_method_cache` (see its doc comment):
    /// a package's published HIR never changes, so the same `def_id`
    /// always resolves to the same impl — this used to re-clone the whole
    /// owning impl's `items: Vec<ImplItem>` on every single call (once per
    /// cross-package method-call/UFCS-call expression, e.g. every
    /// `Vec::new()`/`String::from(..)` when compiling against std).
    pub fn find_hir_impl_method(
        &self,
        def_id: crate::hir::DefId,
    ) -> Option<
        Rc<(
            crate::hir::Generics,
            crate::hir::TypeExpr,
            Vec<crate::hir::ImplItem>,
            crate::hir::Function,
        )>,
    > {
        if let Some(cached) = self.impl_method_cache.borrow().get(&def_id) {
            return Some(cached.clone());
        }
        let package = self.hir_packages.borrow().get(&def_id.package_id)?.clone();
        let package = package.borrow();
        let program = package.hir_program.as_ref()?;
        let &item_index = program.impl_method_item_index.get(&def_id)?;
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
        let entry = Rc::new((
            impl_item.generics.clone(),
            impl_item.self_ty.clone(),
            impl_item.items.clone(),
            function,
        ));
        self.impl_method_cache
            .borrow_mut()
            .insert(def_id, entry.clone());
        Some(entry)
    }

    /// Cross-package counterpart to `hir_typeck::expr_path_ty`'s own
    /// same-package enum-variant scan — given a variant's resolved `DefId`,
    /// route directly to the one package that could define it
    /// (`def_id.package_id`, the same trick `find_hir_impl_method` uses
    /// above) and scan *only* that package's own HIR items for the `Enum`
    /// whose `variants` contains this `def_id`, returning its real
    /// declared name. A confirmed structural fact (a `DefId` match), never
    /// a guess — `None` here means "this `def_id` is not an enum variant"
    /// (some other `Res::Def`, e.g. a function/const/struct), not
    /// "couldn't find it".
    pub fn find_hir_enum_for_variant(&self, def_id: crate::hir::DefId) -> Option<String> {
        let package = self.hir_packages.borrow().get(&def_id.package_id)?.clone();
        let package = package.borrow();
        let program = package.hir_program.as_ref()?;
        program.items.iter().find_map(|item| {
            let crate::hir::ItemKind::Enum(enum_def) = &item.kind else {
                return None;
            };
            enum_def
                .variants
                .iter()
                .any(|v| v.def_id == def_id)
                .then(|| enum_def.name.as_str().to_string())
        })
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
    /// own module tree.
    pub fn find_export(&self, key: &str) -> Option<crate::hir::Res> {
        for package in self.sorted_packages() {
            if let Some(res) = package.borrow().hir_exports.get(key) {
                return Some(res.clone());
            }
        }
        None
    }

    /// `find_export` requires the caller's exact fully-qualified key —
    /// but a bare name (`Option`, `Some`) has no way to know which
    /// module of some OTHER package defines it (e.g. `core::option::Option`
    /// in real std). This scans every package's `hir_exports` for a key
    /// whose LAST path segment matches `name`, so a bare reference can
    /// still resolve without the caller needing to spell out the
    /// defining module. Ambiguous only in the sense that the first
    /// match (in `sorted_packages` order) wins if two packages export
    /// the same bare name from different modules.
    pub fn find_export_by_name(&self, name: &str) -> Option<crate::hir::Res> {
        self.with_export_suffix_index(|index| {
            index
                .get(name)
                .and_then(|candidates| candidates.first())
                .map(|(_, res)| res.clone())
        })
    }

    /// Same idea as `find_export_by_name`, but for a multi-segment
    /// suffix (e.g. `Option::Some`) instead of a single bare name — the
    /// caller's own module path prefix (e.g. an importing package's
    /// module) never matches the defining package's real qualified key
    /// (e.g. `core::option::Option::Some`), so match on the export key
    /// ending with `"::" + suffix`, or being exactly `suffix`.
    pub fn find_export_by_suffix(&self, suffix: &str) -> Option<crate::hir::Res> {
        let last_segment = suffix.rsplit("::").next().unwrap_or(suffix);
        let dotted_suffix = format!("::{suffix}");
        self.with_export_suffix_index(|index| {
            index.get(last_segment).and_then(|candidates| {
                candidates
                    .iter()
                    .find(|(key, _)| key == suffix || key.ends_with(&dotted_suffix))
                    .map(|(_, res)| res.clone())
            })
        })
    }

    /// Rebuilds `export_suffix_index` only when the total export count
    /// across every loaded package has grown since it was last built —
    /// packages only ever gain exports during compilation (via
    /// `CompiledPackage::hir_exports.extend`), never lose them, so a
    /// grown total reliably signals staleness without needing every
    /// caller of `hir_exports.extend` to explicitly invalidate this cache.
    fn with_export_suffix_index<T>(
        &self,
        f: impl FnOnce(&HashMap<String, Vec<(String, crate::hir::Res)>>) -> T,
    ) -> T {
        let packages = self.sorted_packages();
        let total_exports: usize = packages.iter().map(|p| p.borrow().hir_exports.len()).sum();
        let mut cache = self.export_suffix_index.borrow_mut();
        let needs_rebuild = match &*cache {
            Some((cached_total, _)) => *cached_total != total_exports,
            None => true,
        };
        if needs_rebuild {
            let mut index: HashMap<String, Vec<(String, crate::hir::Res)>> = HashMap::new();
            for package in &packages {
                for (key, res) in package.borrow().hir_exports.iter() {
                    let last_segment = key.rsplit("::").next().unwrap_or(key.as_str());
                    index
                        .entry(last_segment.to_owned())
                        .or_default()
                        .push((key.clone(), res.clone()));
                }
            }
            *cache = Some((total_exports, index));
        }
        f(&cache.as_ref().expect("just populated above").1)
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

    /// A `TargetBackend`'s view of `id` as a `PackageSource` — every
    /// AST-emitting backend's input. `id` must already be loaded via
    /// `begin_package`/`import_package`.
    pub fn package_source(&self, id: &PackageId) -> crate::error::Result<PackageSource> {
        let package = self.compiled_package(id).ok_or_else(|| {
            crate::error::Error::from(format!(
                "package `{id}` is not present in this compiled workspace"
            ))
        })?;
        Ok(crate::ast::package::package_source_from_compiled(id, &package))
    }

    /// Merges every other loaded package's compiled LIR workspace into
    /// `id`'s own (dependencies first, mirroring the same merge order
    /// `evaluate_comptime_lir` uses for comptime execution — see
    /// `fp-compiler`'s `LoweredProgram::lir` this was moved from), then
    /// best-effort resolves `id`'s `main` function (searched by name alone
    /// across the whole package — see `crate::ast::package::resolve_entrypoint_def_id`'s
    /// doc comment for why this is deliberately package-based, not
    /// module-based, and not a fit for a host language whose own
    /// entrypoint convention is module/class-qualified) and renames it to
    /// the bare symbol name `main` in the merged program — native/asm
    /// emitters locate the process entry point by that final, bare symbol
    /// name (see `crate::ast::package::rename_lir_function`'s doc comment), and a
    /// module-nested `main` built from a flattened, ad hoc `LirProgram`
    /// like this one (rather than through `CompilerDriver::select_entrypoint`)
    /// otherwise never gets that renaming. Silently does nothing if `id`
    /// has no `main` (e.g. a pure library package) — this is best-effort,
    /// not a requirement every package must satisfy.
    pub fn merged_lir_program(&self, id: &PackageId) -> crate::error::Result<crate::lir::LirProgram> {
        let package = self.compiled_package(id).ok_or_else(|| {
            crate::error::Error::from(format!(
                "compiled package `{id}` is unavailable for LIR merging"
            ))
        })?;
        let package = package.borrow();
        if package.lir.own_artifacts.artifacts().is_empty() {
            return Err(crate::error::Error::from(format!(
                "compiled package `{id}` contains no LIR artifacts"
            )));
        }
        let mut combined =
            crate::lir::LirWorkspace::new(package.lir.own_artifacts.data_layout.clone());
        for (dependency_id, dep_package) in self.crates.borrow().iter() {
            if dependency_id == id {
                continue;
            }
            combined
                .add_workspace(&dep_package.borrow().lir.own_artifacts)
                .map_err(|error| crate::error::Error::from(error.to_string()))?;
        }
        combined
            .add_workspace(&package.lir.own_artifacts)
            .map_err(|error| crate::error::Error::from(error.to_string()))?;
        let mut lir = combined.to_program();
        if let Ok(def_id) = crate::ast::package::resolve_entrypoint_def_id(id, &package, "main") {
            crate::ast::package::rename_lir_function(&mut lir, def_id, "main");
        }
        Ok(lir)
    }
}
