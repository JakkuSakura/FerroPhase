use std::collections::BTreeMap;

use semver::{Version, VersionReq};

use crate::ast::module::{FeatureRef, ModuleId};
pub use crate::package::PackageId;
use crate::vfs::VirtualPath;

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
    /// The package whose implicit prelude is imported for this package.
    /// Providers resolve this per downstream package instead of leaving the
    /// compiler to infer a language- or package-name-specific policy.
    pub prelude: Option<PackageId>,
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
            prelude: None,
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

use crate::ast::path::QualifiedPath;
use crate::ast::{FunctionSignature, Item, ItemId, MethodSignature, TypeEnum, TypeStruct};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct PackageItem {
    /// The *file's* module path — computed once per source file and shared
    /// identically by every item parsed from it (see
    /// `fp_rust::provider::RustPackageProvider::load_package_source` and
    /// `fp_lang::magnet_provider::MagnetWorkspaceProvider::load_package_source`,
    /// the two providers that construct this). It does **not** include the
    /// item's own name — `Item` already carries that itself. Named
    /// `module_path` (not `path`) specifically to make that contract
    /// unambiguous: a prior name of plain `path` invited exactly the wrong
    /// assumption that it was a per-item fully-qualified path.
    pub module_path: QualifiedPath,
    pub item: Item,
}

/// A package's own AST/source-level content — both what a `PackageProvider`
/// hands back (raw parsed source: `items`/`module_paths`/`referenced_paths`)
/// and, once typechecked, the definitions typechecking derives from it
/// (`struct_defs`/`enum_defs`/`function_sigs`/...). One type across both
/// stages rather than two near-identical ones: the typecheck-derived fields
/// are simply empty until typechecking fills them in. Pairs with
/// `hir::HirPackage`/`mir::MirPackage`/`lir::LirPackage` as this layer's
/// `XxxPackage` (there's no separate `AstProgram` — `items` is already the
/// AST layer's un-lowered content, with no further flattening step the way
/// HIR/MIR/LIR each need before backends consume them).
/// Compatibility alias for the pre-merge name — a handful of call sites
/// across `fp-backend`/`fp-cli` still spell this type `CompiledPackage`
/// (its name before `AstPackage` absorbed `PackageSource`); kept as a plain
/// alias rather than renaming every one of those call sites in this change.
pub type CompiledPackage = AstPackage;

#[derive(Clone, Debug)]
pub struct AstPackage {
    pub package_id: PackageId,
    /// This package's identity within the HIR numbering space — distinct
    /// from `package_id` (the source-level id a provider names it by),
    /// minted once by `AstProgram::begin_package` and needed before any
    /// HIR exists (used as the key into `AstProgram::crates` and in
    /// every HIR `DefId`/`HirId` this package ever mints).
    pub hir_package_id: crate::hir::PackageId,
    pub name: String,
    pub graph: graph::PackageGraph,

    /// Persistent module-resolution state populated by the AST resolver.
    pub module_tree: crate::ast::resolve::ModuleTree,
    /// AST node resolutions produced before lowering.
    pub resolutions: HashMap<ItemId, crate::ast::resolve::AstRes>,
    pub expr_resolutions: HashMap<crate::ast::ExprId, crate::ast::resolve::AstRes>,

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

    /// Compiled type and function definitions, keyed by
    /// fully-qualified path (e.g. `["std","meta","TypeBuilder"]`). Empty
    /// until typechecking populates them.
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

    /// Fully-qualified transparent aliases exported by this package. The
    /// declaration module is semantic data: a re-export must expand its RHS
    /// in the scope where that RHS was written, never in an importing module.
    pub type_alias_exports: HashMap<String, TypeAliasExport>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasExport {
    pub target: crate::ast::Ty,
    pub defining_module: QualifiedPath,
}

impl AstPackage {
    pub fn new(package_id: PackageId, name: impl Into<String>, graph: graph::PackageGraph) -> Self {
        let module_paths = graph
            .modules()
            .filter(|module| !module.module_path.is_empty())
            .map(|module| QualifiedPath::new(module.module_path.clone()))
            .collect();

        Self {
            package_id,
            // Overwritten by `AstProgram::begin_package` once it mints
            // this package's real HIR-numbering identity; a fresh
            // `AstPackage` isn't registered with any workspace yet, so
            // there's no real id to give it here.
            hir_package_id: crate::hir::PackageId::default(),
            name: name.into(),
            graph,
            module_tree: crate::ast::resolve::ModuleTree::new(),
            resolutions: HashMap::new(),
            expr_resolutions: HashMap::new(),
            module_paths,
            items: Vec::new(),
            referenced_paths: HashMap::new(),
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            function_sigs: HashMap::new(),
            function_item_ids: HashMap::new(),
            trait_defs: HashSet::new(),
            method_sigs: HashMap::new(),
            type_alias_exports: HashMap::new(),
        }
    }

    /// A one-item package wrapping a single opaque `Item` (e.g.
    /// `Item::precompiled_asm`/`precompiled_lir`/`precompiled_artifact`) —
    /// the shape every foreign-artifact `PackageProvider` (native object/
    /// asm, goasm, urcl, jvm-bytecode, cil, ...) needs, with no real module
    /// graph behind it.
    pub fn single_item(package_id: PackageId, item: Item) -> Self {
        let mut source = Self::new(
            package_id.clone(),
            package_id.as_str(),
            graph::PackageGraph::new(Vec::new()),
        );
        source.items.push(PackageItem {
            module_path: QualifiedPath::new(Vec::new()),
            item,
        });
        source
    }
}

/// One module's worth of items within a package, grouped by module path.
#[derive(Clone, Debug)]
pub struct PackageModule {
    pub path: QualifiedPath,
    pub items: Vec<Item>,
}

impl PackageModule {
    /// The module's path as a `/`-joined relative file path (e.g.
    /// `"config"`, `"repo/backend"`) — the convention every backend
    /// serializer uses to lay out one source file per module.
    pub fn relative_path(&self) -> String {
        self.path.segments.join("/")
    }
}

/// Groups a package's items by their module path — the same per-module
/// split every backend serializer needs to lay out one source file per
/// module. Shared here instead of duplicated per-backend (previously
/// reimplemented identically in `KotlinSerializer::serialize_package` and
/// the CLI's per-module fallback loop). Returned in path-sorted order for
/// stable output.
pub fn split_package_into_modules(source: &AstPackage) -> Vec<PackageModule> {
    let mut modules: BTreeMap<Vec<String>, Vec<Item>> = BTreeMap::new();
    for pkg_item in &source.items {
        modules
            .entry(pkg_item.module_path.segments.clone())
            .or_default()
            .push(pkg_item.item.clone());
    }
    modules
        .into_iter()
        .map(|(segments, items)| PackageModule {
            path: QualifiedPath::new(segments),
            items,
        })
        .collect()
}

/// Resolves the `DefId` of the function named `function_name` anywhere in
/// `hir_package`'s items — package-based, not module-based: it doesn't
/// matter which module the function lives in, only that exactly one item
/// in the package is named `function_name`. Pure over an already-borrowed
/// `hir::HirPackage` so both `CompilerDriver` and `AstProgram` callers can
/// share this without either depending on the other. See
/// `crate::hir::HirPackage::def_paths`'s doc comment for why `sig.name` is
/// always the bare, local identifier and disambiguation instead relies on
/// the recorded def path.
///
/// This is deliberately name-only, not module/class-qualified — every
/// current backend just needs "the function named `main`" and a
/// FerroPhase package conventionally has exactly one. A backend whose
/// *host* language has its own module-qualified entrypoint convention
/// (e.g. JVM/Java, where `public static void main` must live in one
/// specific class the launcher is told to run) isn't served by this
/// helper and shouldn't be forced to be — it should resolve its own
/// entrypoint against its own module/class structure instead of this
/// package-wide, name-only search.
pub fn resolve_entrypoint_def_id(
    package_id: &PackageId,
    hir_package: &crate::hir::HirPackage,
    function_name: &str,
) -> crate::error::Result<crate::hir::DefId> {
    hir_package
        .items
        .iter()
        .find_map(|item| match &item.kind {
            crate::hir::ItemKind::Function(function)
                if function.sig.name.as_str() == function_name =>
            {
                Some(item.def_id.clone())
            }
            _ => None,
        })
        .ok_or_else(|| {
            crate::error::Error::from(format!(
                "package `{package_id}` has no `{function_name}` entrypoint"
            ))
        })
}

/// Renames the LIR function identified by `def_id` to `bare_name` in
/// place. The process entry point is located downstream (native/asm
/// emission) by its final, bare symbol name — a linkage requirement, not a
/// display convention — so a module-nested `main`'s mangled qualified name
/// needs renaming back to the bare name it was resolved by.
pub fn rename_lir_function(
    lir: &mut crate::lir::LirBlob,
    def_id: crate::hir::DefId,
    bare_name: &str,
) {
    for lir_function in lir.functions.iter_mut() {
        if lir_function.def_id.as_ref() == Some(&def_id) {
            lir_function.name = crate::lir::Name::new(bare_name.to_string());
            break;
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
