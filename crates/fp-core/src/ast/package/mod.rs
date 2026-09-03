use std::collections::BTreeMap;

use semver::{Version, VersionReq};

use crate::ast::module::FeatureRef;
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
}

impl PackageDescriptor {
    pub fn empty(id: PackageId, name: impl Into<String>) -> Self {
        Self {
            id,
            name: name.into(),
            version: None,
            manifest_path: VirtualPath::from_path(std::path::Path::new(".")),
            root: VirtualPath::from_path(std::path::Path::new(".")),
            metadata: PackageMetadata::default(),
        }
    }
}

pub mod provider;

use crate::ast::path::InPackagePath;
use crate::ast::{Ident, Item, ItemKind, Module, Visibility};
use std::collections::HashMap;

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
    pub module_path: InPackagePath,
    pub item: Item,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PackagePath {
    pub package_id: PackageId,
    pub path: InPackagePath,
}

impl PackagePath {
    pub fn new(package_id: PackageId, path: InPackagePath) -> Self {
        Self { package_id, path }
    }
}

/// A package's own AST/source-level content — both what a `PackageProvider`
/// hands back (raw parsed source: `items`/`referenced_paths`). Pairs with
/// `hir::HirPackage`/`mir::MirPackage`/`lir::LirPackage` as this layer's
/// `XxxPackage` (there's no separate `AstProgram` — `items` is already the
/// AST layer's un-lowered content, with no further flattening step the way
/// HIR/MIR/LIR each need before backends consume them).
#[derive(Clone, Debug)]
pub struct AstPackage {
    pub package_id: PackageId,
    pub name: String,
    pub package: PackageDescriptor,

    pub prelude_modules: Vec<PackagePath>,

    /// The package's single parsed root module. Nested modules remain items
    /// in this tree and are flattened on demand by `items()`.
    pub module: Module,

    /// For typed compiles (`typecheck_package`): each item's own
    /// qualified path (module path + name, plain `"::"`-free segments) ->
    /// qualified paths of every other definition it references — raw
    /// facts a target backend can use to compute which imports it
    /// actually needs for spliced-in content, instead of only ever
    /// echoing whatever `use` items happened to already exist in the
    /// source file. Empty for untyped/fallback loads.
    pub referenced_paths: HashMap<Vec<String>, Vec<Vec<String>>>,
}

pub trait IntoRootModule {
    fn into_root_module(self) -> Module;
}

impl IntoRootModule for Module {
    fn into_root_module(self) -> Module {
        self
    }
}

impl IntoRootModule for Vec<Module> {
    fn into_root_module(mut self) -> Module {
        if self.len() == 1 {
            return self.pop().unwrap();
        }
        let items = self.into_iter().flat_map(|module| module.items).collect();
        Module {
            attrs: Vec::new(),
            name: Ident::new(""),
            items,
            visibility: Visibility::Public,
            is_external: false,
        }
    }
}

impl AstPackage {
    pub fn new<M: IntoRootModule>(
        package_id: PackageId,
        name: impl Into<String>,
        package: PackageDescriptor,
        module: M,
    ) -> Self {
        Self {
            package_id,
            name: name.into(),
            package,
            prelude_modules: Vec::new(),
            module: module.into_root_module(),
            referenced_paths: HashMap::new(),
        }
    }

    /// A one-item package wrapping a single opaque `Item` (e.g.
    /// `Item::precompiled_asm`/`precompiled_lir`/`precompiled_artifact`) —
    /// the shape every foreign-artifact `PackageProvider` (native object/
    /// asm, goasm, urcl, jvm-bytecode, cil, ...) needs, with no real module
    /// graph behind it.
    pub fn single_item(package_id: PackageId, item: Item) -> Self {
        Self::new(
            package_id.clone(),
            package_id.as_str(),
            PackageDescriptor::empty(package_id.clone(), package_id.as_str()),
            Module {
                attrs: Vec::new(),
                name: Ident::new(""),
                items: vec![item],
                visibility: Visibility::Public,
                is_external: false,
            },
        )
    }

    pub fn items(&self) -> Vec<PackageItem> {
        let mut output = Vec::new();
        let path = if self.module.name.as_str().is_empty() {
            InPackagePath::new(Vec::new())
        } else {
            InPackagePath::new(vec![self.module.name.as_str().to_owned()])
        };
        Self::flatten_module_items_into(&path, &self.module.items, &mut |_| false, &mut output);
        output
    }

    /// Flattens nested AST modules into source items carrying their module
    /// paths. Providers can use this once instead of maintaining their own
    /// recursive module walkers.
    pub fn flatten_module_items(module_path: &InPackagePath, items: &[Item]) -> Vec<PackageItem> {
        let mut output = Vec::new();
        Self::flatten_module_items_into(module_path, items, &mut |_| false, &mut output);
        output
    }

    pub fn flatten_module_items_filtered(
        module_path: &InPackagePath,
        items: &[Item],
        skip: &mut impl FnMut(&Item) -> bool,
    ) -> Vec<PackageItem> {
        let mut output = Vec::new();
        Self::flatten_module_items_into(module_path, items, skip, &mut output);
        output
    }

    fn flatten_module_items_into(
        module_path: &InPackagePath,
        items: &[Item],
        skip: &mut impl FnMut(&Item) -> bool,
        output: &mut Vec<PackageItem>,
    ) {
        for item in items {
            if skip(item) {
                continue;
            }
            if let ItemKind::Module(module) = item.kind() {
                Self::flatten_module_items_into(
                    &module_path.with_segment(module.name.as_str().to_owned()),
                    &module.items,
                    skip,
                    output,
                );
            } else {
                output.push(PackageItem {
                    module_path: module_path.clone(),
                    item: item.clone(),
                });
            }
        }
    }
}

/// Resolves the `DefId` of the function named `function_name` anywhere in
/// `hir_package`'s items — package-based, not module-based: it doesn't
/// matter which module the function lives in, only that exactly one item
/// in the package is named `function_name`. Pure over an already-borrowed
/// `hir::HirPackage` so both `CompilerDriver` and `AstProgram` callers can
/// share this without either depending on the other. See
/// HIR source-path metadata's doc comment for why `sig.name` is
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
