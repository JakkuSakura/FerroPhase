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

use crate::ast::{FunctionSignature, TypeEnum, TypeStruct};
use crate::module::path::QualifiedPath;
use crate::package::graph::PackageGraph;
use crate::package::provider::PackageProvider;
use crate::package::PackageCrate;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// The single root registry every crate/scope lives in — including the one
/// currently being typed (see `CompilerDriver::load_package` and
/// `AstTypeInferencer::own_crate`), plus a registry of providers that can
/// load more crates on demand. The typer queries this for fully-qualified
/// symbol lookups after checking local scopes; when a lookup misses because
/// a *registered* package hasn't been loaded yet, the caller records a
/// pending request instead of erroring, and the compiler driver loads it via
/// the registered provider — uniformly for `std` or any other registered
/// package, not eagerly up front. Each crate keeps and mutates its own
/// storage (`Rc<RefCell<PackageCrate>>`); lookups across crates borrow that
/// storage rather than cloning it wholesale.
#[derive(Default)]
pub struct WorkspaceContext {
    crates: RefCell<HashMap<String, Rc<RefCell<PackageCrate>>>>,
    providers: HashMap<String, Arc<dyn PackageProvider>>,
}

impl WorkspaceContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a package's loader. Registration itself isn't on-demand —
    /// it just declares "this package exists and can be loaded" — only the
    /// actual load (parsing + typing) is deferred until first reference.
    pub fn register_provider(&mut self, name: impl Into<String>, provider: Arc<dyn PackageProvider>) {
        self.providers.insert(name.into(), provider);
    }

    /// Start a new crate/scope: creates an empty `PackageCrate`, inserts it
    /// into the root under `name`, and returns the same `Rc` so the caller
    /// (e.g. `AstTypeInferencer`, or `CompilerDriver::load_package`) can hold
    /// it directly and mutate it going forward — no re-lookup by name needed
    /// for every write.
    pub fn begin_crate(&self, name: impl Into<String>, graph: PackageGraph) -> Rc<RefCell<PackageCrate>> {
        let name = name.into();
        let krate = Rc::new(RefCell::new(PackageCrate::new(name.clone(), graph)));
        self.crates.borrow_mut().insert(name, krate.clone());
        krate
    }

    pub fn is_loaded(&self, name: &str) -> bool {
        self.crates.borrow().contains_key(name)
    }

    pub fn is_registered(&self, name: &str) -> bool {
        self.providers.contains_key(name)
    }

    pub fn provider(&self, name: &str) -> Option<Arc<dyn PackageProvider>> {
        self.providers.get(name).cloned()
    }

    /// Names of every registered package, whether or not it's been loaded
    /// yet — used to seed root-module recognition so `use std::...`-style
    /// paths resolve correctly even before `std` is actually loaded.
    pub fn registered_names(&self) -> impl Iterator<Item = &str> {
        self.providers.keys().map(|s| s.as_str())
    }

    /// Search every crate for a struct at `path`, borrowing each crate just
    /// long enough to check — the one clone that remains is the matched
    /// item itself, needed regardless to build an owned `Ty::Struct(..)`.
    pub fn find_struct(&self, path: &QualifiedPath) -> Option<TypeStruct> {
        for krate in self.crates.borrow().values() {
            if let Some(s) = krate.borrow().struct_defs.get(path) {
                return Some(s.clone());
            }
        }
        None
    }

    /// Cross-crate counterpart to `find_struct`, for enums (e.g.
    /// `std::option::Option`/`std::result::Result`, defined in `std`'s own
    /// `PackageCrate`, not whatever crate is currently being typed).
    pub fn find_enum(&self, path: &QualifiedPath) -> Option<TypeEnum> {
        for krate in self.crates.borrow().values() {
            if let Some(e) = krate.borrow().enum_defs.get(path) {
                return Some(e.clone());
            }
        }
        None
    }

    pub fn find_function_sig(&self, path: &QualifiedPath) -> Option<FunctionSignature> {
        for krate in self.crates.borrow().values() {
            if let Some(sig) = krate.borrow().function_sigs.get(path) {
                return Some(sig.clone());
            }
        }
        None
    }

    /// Search every crate for `path`'s inherent methods (see
    /// `PackageCrate::method_sigs`'s doc comment) -- the cross-crate
    /// counterpart to `own_method_sigs` in `fp-typing`, mirroring
    /// `find_struct`/`find_function_sig` exactly.
    pub fn find_method_sigs(&self, path: &QualifiedPath) -> Option<Vec<(String, FunctionSignature)>> {
        for krate in self.crates.borrow().values() {
            if let Some(sigs) = krate.borrow().method_sigs.get(path) {
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
    }

    /// Borrow the root map directly. Used by callers that need to iterate
    /// every crate themselves (e.g. an early-return tail-name search, or
    /// gathering LIR units) rather than looking up one qualified path.
    pub fn crates(&self) -> Ref<'_, HashMap<String, Rc<RefCell<PackageCrate>>>> {
        self.crates.borrow()
    }
}
