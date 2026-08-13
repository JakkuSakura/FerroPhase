use std::collections::BTreeMap;
use std::fmt::{self, Display};

use semver::{Version, VersionReq};

use crate::ast::module::{FeatureRef, ModuleId};
use crate::vfs::VirtualPath;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PackageId {
    name: String,
    version: Option<Version>,
    source: Option<String>,
}

impl PackageId {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: name.into(),
            version: None,
            source: None,
        }
    }

    pub fn resolved(name: impl Into<String>, version: Version, source: impl Into<String>) -> Self {
        Self::with_source(name, Some(version), source)
    }

    pub fn with_source(
        name: impl Into<String>,
        version: Option<Version>,
        source: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            version,
            source: Some(source.into()),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }

    pub fn version(&self) -> Option<&Version> {
        self.version.as_ref()
    }

    pub fn source(&self) -> Option<&str> {
        self.source.as_deref()
    }
}

impl Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(version) = &self.version {
            write!(f, "@{version}")?;
        }
        if let Some(source) = &self.source {
            write!(f, " [{source}]")?;
        }
        Ok(())
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
    /// Source-level dependency name, retained for diagnostics and aliases.
    pub package: String,
    /// The concrete package selected by Magnet or the provider. Raw manifest
    /// metadata leaves this unset until dependency resolution has completed.
    pub resolved_package_id: Option<PackageId>,
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
use crate::hir::PackageId as HirPackageId;
use crate::hir::Symbol;
use crate::lir::{LirCompileUnit, LirWorkspace};
use crate::ast::path::QualifiedPath;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct PackageItem {
    pub path: QualifiedPath,
    pub item: Item,
}

/// Parsed source returned by a package provider.
///
/// This type intentionally contains no generated HIR identity or compiler
/// registries. Providers describe source; the compiler owns compilation state.
#[derive(Clone, Debug)]
pub struct PackageSource {
    pub package_id: PackageId,
    pub name: String,
    pub graph: graph::PackageGraph,

    /// All known module paths within this package.
    pub module_paths: HashSet<QualifiedPath>,

    /// All parsed source items with their fully qualified source paths.
    pub items: Vec<PackageItem>,

    /// For typed compiles (`typecheck_package`): each item's own
    /// qualified path (module path + name, plain `"::"`-free segments) ->
    /// qualified paths of every other definition it references — raw
    /// facts a target backend can use to compute which imports it
    /// actually needs for spliced-in content, instead of only ever
    /// echoing whatever `use` items happened to already exist in the
    /// source file. Empty for untyped/fallback loads.
    pub referenced_paths: HashMap<Vec<String>, Vec<Vec<String>>>,
}

impl PackageSource {
    pub fn new(package_id: PackageId, name: impl Into<String>, graph: graph::PackageGraph) -> Self {
        let module_paths = graph
            .modules()
            .filter(|module| !module.module_path.is_empty())
            .map(|module| QualifiedPath::new(module.module_path.clone()))
            .collect();

        Self {
            package_id,
            name: name.into(),
            graph,
            module_paths,
            items: Vec::new(),
            referenced_paths: HashMap::new(),
        }
    }
}

/// Compiler-owned state produced by type-checking a package.
///
/// A compiled package is stored in the workspace so dependent packages can
/// query its definitions without re-parsing or re-type-checking it.
#[derive(Clone, Debug)]
pub struct CompiledPackage {
    pub package_id: HirPackageId,
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

    /// Fine-grained LIR artifacts owned by this package.
    pub lir_workspace: LirWorkspace,

    /// HIR definitions published by this package.
    pub hir_program: Option<crate::hir::Program>,

    /// Lifted AST from typed HIR (TypecheckedTranspile mode).
    pub lifted_ast: Option<crate::ast::File>,

    /// Typed HIR lifted back to AST, keyed by each item's own qualified
    /// name (`HirToAstLifter::lift_items_by_path`) rather than by list
    /// position — lets a source item be spliced with its typed
    /// counterpart by identity, tolerating extra (synthetic) or missing
    /// (e.g. per-item lift failures) entries on either side instead of
    /// requiring the two lists to match 1:1 in the same order.
    pub lifted_items_by_path: Option<HashMap<Vec<Symbol>, Item>>,

    /// For each item in `lifted_items_by_path` (same key), the qualified
    /// paths of every other definition it references
    /// (`HirToAstLifter::referenced_paths_by_path`) — raw facts a target
    /// backend can use to compute which imports it actually needs for
    /// spliced-in content, rather than only echoing the source file's
    /// pre-existing `use` items.
    pub referenced_paths_by_path: Option<HashMap<Vec<Symbol>, Vec<Vec<Symbol>>>>,

    /// MIR produced for this package.
    pub mir_program: Option<crate::mir::Program>,

    /// Fully-qualified HIR lookup entries exported by this package.
    pub hir_exports: HashMap<String, crate::hir::Res>,

    /// Fully-qualified `type X = Y;` aliases exported by this package (e.g.
    /// `libc`'s `pub type char = u8;`) — unlike `hir_exports`, these are
    /// consulted purely at AST-to-HIR type-lowering time, before a `Res` even
    /// exists, so they need their own cross-package export/merge path (see
    /// `seed_workspace_definitions`).
    pub type_alias_exports: HashMap<String, crate::ast::Ty>,

    /// All parsed source items with their fully qualified source paths.
    pub items: Vec<PackageItem>,

    /// MIR struct field types keyed by DefId, computed during MIR lowering.
    pub mir_struct_fields: HashMap<crate::mir::DefId, Vec<crate::mir::Ty>>,
    pub mir_adt_defs: HashMap<crate::hir::DefId, crate::mir::ty::AdtDef>,
}

impl CompiledPackage {
    pub fn new(
        package_id: HirPackageId,
        name: impl Into<String>,
        graph: graph::PackageGraph,
        data_layout: crate::lir::LirDataLayout,
    ) -> Self {
        let module_paths: HashSet<QualifiedPath> = graph
            .modules()
            .filter(|m| !m.module_path.is_empty())
            .map(|m| QualifiedPath::new(m.module_path.clone()))
            .collect();

        Self {
            package_id,
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
            lir_workspace: LirWorkspace::new(data_layout),
            hir_program: None,
            lifted_ast: None,
            lifted_items_by_path: None,
            referenced_paths_by_path: None,
            mir_program: None,
            hir_exports: HashMap::new(),
            type_alias_exports: HashMap::new(),
            items: Vec::new(),
            mir_struct_fields: HashMap::new(),
            mir_adt_defs: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PackageId;
    use semver::Version;

    #[test]
    fn resolved_package_ids_distinguish_selected_versions_and_sources() {
        let first = PackageId::resolved("serde", Version::new(1, 0, 0), "registry+crates.io");
        let second = PackageId::resolved("serde", Version::new(1, 1, 0), "registry+crates.io");
        let third = PackageId::resolved("serde", Version::new(1, 0, 0), "git+https://example.test");

        assert_ne!(first, second);
        assert_ne!(first, third);
        assert_eq!(first.as_str(), "serde");
    }
}
