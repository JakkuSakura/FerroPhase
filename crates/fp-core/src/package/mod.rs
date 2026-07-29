use std::collections::BTreeMap;
use std::fmt::{self, Display};

use semver::{Version, VersionReq};

use crate::module::{FeatureRef, ModuleId};
use crate::vfs::VirtualPath;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PackageId(pub String);

impl PackageId {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TargetFilter {
    /// Optional Cargo/Rust `cfg` expression captured verbatim.
    pub cfg: Option<String>,
    /// List of logical languages/targets this dependency applies to (e.g. "typescript").
    pub languages: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencyKind {
    Normal,
    Development,
    Build,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DependencyDescriptor {
    pub package: String,
    pub constraint: Option<VersionReq>,
    pub kind: DependencyKind,
    pub features: Vec<FeatureRef>,
    pub optional: bool,
    pub target: TargetFilter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackageMetadata {
    pub edition: Option<String>,
    pub authors: Vec<String>,
    pub description: Option<String>,
    pub license: Option<String>,
    pub keywords: Vec<String>,
    pub registry: Option<String>,
    pub features: BTreeMap<String, Vec<FeatureRef>>,
    pub dependencies: Vec<DependencyDescriptor>,
}

impl Default for PackageMetadata {
    fn default() -> Self {
        Self {
            edition: None,
            authors: Vec::new(),
            description: None,
            license: None,
            keywords: Vec::new(),
            registry: None,
            features: BTreeMap::new(),
            dependencies: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackageDescriptor {
    pub id: PackageId,
    pub name: String,
    pub version: Option<Version>,
    pub manifest_path: VirtualPath,
    pub root: VirtualPath,
    pub metadata: PackageMetadata,
    pub modules: Vec<ModuleId>,
}

pub mod graph;
pub mod provider;

use crate::ast::{FunctionSignature, TypeEnum, TypeStruct};
use crate::lir::LirProgram;
use crate::module::path::QualifiedPath;
use std::collections::{HashMap, HashSet};

/// A compiled crate — the result of type-checking a package.
/// The driver compiles dependency packages first (embedded std,
/// workspace dependencies) and stores the results here so the
/// typer can look up fully-qualified symbols without re-parsing
/// or re-type-checking.
#[derive(Clone, Debug, Default)]
pub struct PackageCrate {
    pub name: String,
    pub graph: graph::PackageGraph,

    /// Compiled type and function definitions, keyed by
    /// fully-qualified path (e.g. `["std","meta","TypeBuilder"]`).
    pub struct_defs: HashMap<QualifiedPath, TypeStruct>,
    pub enum_defs: HashMap<QualifiedPath, TypeEnum>,
    pub function_sigs: HashMap<QualifiedPath, FunctionSignature>,
    pub trait_defs: HashSet<QualifiedPath>,

    /// All known module paths within this crate.
    pub module_paths: HashSet<QualifiedPath>,

    /// Compiled LIR for this crate, produced after the full lowering
    /// pipeline. Used to merge into the caller's LirProgram so the
    /// interpreter can resolve cross-module function calls.
    pub lir_program: Option<LirProgram>,
}

impl PackageCrate {
    pub fn new(name: impl Into<String>, graph: graph::PackageGraph) -> Self {
        let module_paths: HashSet<QualifiedPath> = graph
            .modules()
            .filter(|m| !m.module_path.is_empty())
            .map(|m| QualifiedPath::new(m.module_path.clone()))
            .collect();

        Self {
            name: name.into(),
            graph,
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            function_sigs: HashMap::new(),
            trait_defs: HashSet::new(),
            module_paths,
            lir_program: None,
        }
    }
}
