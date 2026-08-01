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

use crate::ast::{FunctionSignature, Item, ItemId, MethodSignature, TypeEnum, TypeStruct};
use crate::lir::LirCompileUnit;
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
    /// The `ItemId` (see `ast::item::ItemId`'s doc comment) of the
    /// `ItemDefFunction` node each locally-defined `function_sigs` entry
    /// was registered from -- lets a later pass (generic monomorphization)
    /// find that exact AST node again directly, instead of re-deriving a
    /// location from the `QualifiedPath` key (which is a qualification
    /// convention, not a record of real module nesting, and doesn't
    /// generally correspond to a walkable path in a stored `File`). Not
    /// merged into `FunctionSignature` itself: that type is also
    /// constructed for synthetic/extern/builtin signatures with no
    /// backing `Item` at all.
    pub function_item_ids: HashMap<QualifiedPath, ItemId>,
    pub trait_defs: HashSet<QualifiedPath>,

    /// Inherent methods declared in an `impl SelfType { .. }` block, keyed
    /// by `SelfType`'s own fully-qualified path -- deliberately not a field
    /// on `TypeStruct`/`TypeEnum` themselves (nothing outside `fp-typing`
    /// ever reads a struct/enum's methods, so embedding it in the shared
    /// `Ty` representation every other crate also constructs would be
    /// storage those crates never use). One shared table regardless of
    /// whether `SelfType` resolves to a struct, an enum, or anything else
    /// nominal -- registration and lookup don't need to branch on that.
    pub method_sigs: HashMap<QualifiedPath, Vec<(String, MethodSignature)>>,

    /// All known module paths within this crate.
    pub module_paths: HashSet<QualifiedPath>,

    /// Compiled LIR modules for this crate — one unit per module.
    /// The interpreter searches across all units for function definitions.
    pub lir_units: Vec<LirCompileUnit>,

    /// Parsed items per module path, available for on-demand compilation
    /// when lir_units is empty. The CompilerDriver uses these to type-check
    /// and lower modules as needed during comptime evaluation.
    pub items: HashMap<QualifiedPath, Vec<Item>>,
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
            function_item_ids: HashMap::new(),
            trait_defs: HashSet::new(),
            method_sigs: HashMap::new(),
            module_paths,
            lir_units: Vec::new(),
            items: HashMap::new(),
        }
    }
}
