use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, PackageDescriptor, PackageId, PackageItem, PackageMetadata,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;

use crate::FerroFrontend;
use crate::project;

/// `PackageProvider` for ferrophase/`.fp` projects organized in a
/// Magnet-workspace layout — Magnet is FerroPhase's own package manager,
/// the `.fp` analog of Cargo for real Rust projects (see `RustPackageProvider`
/// in `fp-rust` for that side). Parses `.fp` sources via `FerroFrontend`.
/// A member's own source root — either a real directory (walked via
/// `project::list_sources`, the ordinary Magnet-workspace case) or a
/// single standalone `.fp` file with no enclosing project (`fp compile
/// foo.fp` with no `Magnet.toml` anywhere above it) — the degenerate
/// one-module package case, always tagged as the crate root (empty
/// module path) regardless of the file's own name.
enum MemberRoot {
    Dir(PathBuf),
    File(PathBuf),
}

impl MemberRoot {
    fn sources(&self) -> Vec<(String, PathBuf)> {
        match self {
            MemberRoot::Dir(dir) => project::list_sources(dir),
            MemberRoot::File(path) => vec![(String::new(), path.clone())],
        }
    }

    fn manifest_path(&self) -> PathBuf {
        match self {
            MemberRoot::Dir(dir) => dir.join("Cargo.toml"),
            MemberRoot::File(path) => path.clone(),
        }
    }

    fn root_path(&self) -> &Path {
        match self {
            MemberRoot::Dir(dir) => dir,
            MemberRoot::File(path) => path,
        }
    }
}

pub struct MagnetWorkspaceProvider {
    #[allow(dead_code)]
    root: PathBuf,
    members: Vec<(String, MemberRoot)>,
    cache: std::sync::RwLock<HashMap<String, Vec<PackageItem>>>,
}

impl MagnetWorkspaceProvider {
    pub fn discover(start: &Path) -> ProviderResult<Self> {
        if start.is_file() {
            let name = start
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main")
                .to_string();
            return Ok(Self {
                root: start.to_path_buf(),
                members: vec![(name, MemberRoot::File(start.to_path_buf()))],
                cache: Default::default(),
            });
        }
        let root = project::find_manifest(start)
            .ok_or_else(|| ProviderError::other("no Cargo.toml or Magnet.toml found"))?;
        let members = project::list_members(&root)
            .into_iter()
            .map(|(name, dir)| (name, MemberRoot::Dir(dir)))
            .collect();
        Ok(Self {
            root,
            members,
            cache: Default::default(),
        })
    }
}

impl PackageProvider for MagnetWorkspaceProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(self
            .members
            .iter()
            .map(|(name, _)| PackageId::new(name))
            .collect())
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    /// The `.fp`-dialect's own frontend engine — see
    /// `FerroIntrinsicNormalizer`'s doc comment. Real Rust source instead
    /// gets `fp_rust::RustIntrinsicNormalizer` from its own provider
    /// (`RustPackageProvider`), not this one.
    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::normalization::FerroIntrinsicNormalizer::new())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let member_root = self.resolve_root(id)?;
        let mut module_ids = Vec::new();
        for (rel, _) in member_root.sources() {
            let path = module_path_from_relative(&rel);
            module_ids.push(ModuleId::new(&path.to_key()));
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&member_root.manifest_path()),
            root: VirtualPath::from_path(member_root.root_path()),
            metadata: PackageMetadata {
                dependencies: vec![crate::provider::std_dependency()],
                ..Default::default()
            },
            modules: module_ids,
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        if let Ok(mut c) = self.cache.write() {
            c.clear();
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        if let Ok(c) = self.cache.read() {
            if let Some(items) = c.get(id.as_str()) {
                return Ok(package_source_from_items(id, items));
            }
        }

        let member_root = self.resolve_root(id)?;
        let frontend = FerroFrontend::new();
        let mut items = Vec::new();

        for (rel, abs) in member_root.sources() {
            let source = std::fs::read_to_string(&abs)
                .map_err(|e| ProviderError::other(format!("read {}: {}", abs.display(), e)))?;
            let result = frontend
                .parse_file(&source, &abs)
                .map_err(|e| ProviderError::other(format!("parse {}: {}", abs.display(), e)))?;
            let path = module_path_from_relative(&rel);
            items.extend(result.ast.items.into_iter().map(|item| PackageItem {
                module_path: path.clone(),
                item,
            }));
        }

        if let Ok(mut c) = self.cache.write() {
            c.insert(id.as_str().to_string(), items.clone());
        }

        Ok(package_source_from_items(id, &items))
    }
}

impl MagnetWorkspaceProvider {
    fn resolve_root(&self, id: &PackageId) -> ProviderResult<&MemberRoot> {
        self.members
            .iter()
            .find(|(name, _)| name == id.as_str())
            .map(|(_, root)| root)
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }
}

/// Computes the flat, file-derived `PackageItem` path tag for a source file
/// relative to a package's source root (e.g. `"config.fp"` → `["config"]`).
/// Exported so callers outside this module can compute the same tag a
/// discovered package's items are already tagged with.
pub fn module_path_from_relative(rel: &str) -> QualifiedPath {
    if rel.is_empty() {
        return QualifiedPath::new(Vec::new());
    }
    let stem = rel.trim_end_matches(".rs").trim_end_matches(".fp");
    let mut parts: Vec<String> = stem.split('/').map(|s| s.to_string()).collect();
    // Pre-2018-edition module file convention: `foo/mod.rs` *is* module
    // `foo` itself, not a `mod` submodule nested inside it — drop the
    // trailing "mod" segment `mod.rs` would otherwise contribute.
    if parts.len() > 1 && parts.last().map(String::as_str) == Some("mod") {
        parts.pop();
    }
    QualifiedPath::new(parts)
}

fn package_source_from_items(id: &PackageId, items: &[PackageItem]) -> AstPackage {
    let paths: HashSet<_> = items.iter().map(|item| item.module_path.clone()).collect();
    let descriptors: Vec<ModuleDescriptor> = paths
        .into_iter()
        .map(|path| ModuleDescriptor {
            id: ModuleId::new(&path.to_key()),
            package: id.clone(),
            language: ModuleLanguage::Ferro,
            module_path: path.segments.clone(),
            source: VirtualPath::from_path(Path::new(".")),
            exports: Vec::new(),
            requires_features: Vec::new(),
        })
        .collect();
    let module_ids: Vec<_> = descriptors.iter().map(|d| d.id.clone()).collect();
    let package = PackageDescriptor {
        id: id.clone(),
        name: id.as_str().to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(Path::new("Cargo.toml")),
        root: VirtualPath::from_path(Path::new(".")),
        metadata: Default::default(),
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for desc in descriptors {
        graph.insert_module(desc);
    }
    let mut source = AstPackage::new(id.clone(), id.as_str(), graph);
    source.items = items.to_vec();
    source
}
