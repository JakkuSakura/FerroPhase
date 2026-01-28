use crate::common_struct;
use std::hash::{Hash, Hasher};

common_struct! {
    pub struct WorkspaceDocument {
        pub manifest: String,
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
        pub version: Option<String>,
        pub manifest_path: String,
        pub root: String,
        pub modules: Vec<WorkspaceModule>,
        pub features: Vec<String>,
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

#[derive(Debug, Clone)]
pub struct WorkspaceModule {
    pub id: String,
    pub path: String,
    pub module_path: Vec<String>,
    pub language: Option<String>,
    pub required_features: Vec<String>,
    pub snapshot: Option<String>,
    pub ast: Option<String>,
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

    pub fn with_ast(mut self, ast: Option<String>) -> Self {
        self.ast = ast;
        self
    }
}

impl WorkspaceModule {
    fn ast_repr(&self) -> Option<String> {
        self.ast.clone()
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
