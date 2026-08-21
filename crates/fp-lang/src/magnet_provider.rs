use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::frontend::LanguageFrontend;
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{PackageDescriptor, PackageId, PackageItem, PackageSource};
use fp_core::vfs::VirtualPath;

use crate::FerroFrontend;
use crate::project;

/// `PackageProvider` for ferrophase/`.fp` projects organized in a
/// Magnet-workspace layout — Magnet is FerroPhase's own package manager,
/// the `.fp` analog of Cargo for real Rust projects (see `RustPackageProvider`
/// in `fp-rust` for that side). Parses `.fp` sources via `FerroFrontend`.
pub struct MagnetWorkspaceProvider {
    #[allow(dead_code)]
    root: PathBuf,
    members: Vec<(String, PathBuf)>,
    cache: std::sync::RwLock<HashMap<String, Vec<PackageItem>>>,
}

impl MagnetWorkspaceProvider {
    pub fn discover(start: &Path) -> ProviderResult<Self> {
        let root = project::find_manifest(start)
            .ok_or_else(|| ProviderError::other("no Cargo.toml or Magnet.toml found"))?;
        let members = project::list_members(&root);
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

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let dir = self.resolve_dir(id)?;
        let mut module_ids = Vec::new();
        for (rel, _) in project::list_sources(&dir) {
            let path = module_path_from_relative(&rel);
            module_ids.push(ModuleId::new(&path.to_key()));
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&dir.join("Cargo.toml")),
            root: VirtualPath::from_path(&dir),
            metadata: Default::default(),
            modules: module_ids,
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        if let Ok(mut c) = self.cache.write() {
            c.clear();
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        if let Ok(c) = self.cache.read() {
            if let Some(items) = c.get(id.as_str()) {
                return Ok(package_source_from_items(id, items));
            }
        }

        let dir = self.resolve_dir(id)?;
        let frontend = FerroFrontend::new();
        let mut items = Vec::new();

        for (rel, abs) in project::list_sources(&dir) {
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
    fn resolve_dir(&self, id: &PackageId) -> ProviderResult<PathBuf> {
        self.members
            .iter()
            .find(|(name, _)| name == id.as_str())
            .map(|(_, dir)| dir.clone())
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }
}

/// Computes the flat, file-derived `PackageItem` path tag for a source file
/// relative to a package's source root (e.g. `"config.fp"` → `["config"]`).
/// Exported so callers outside this module can compute the same tag a
/// discovered package's items are already tagged with.
pub fn module_path_from_relative(rel: &str) -> QualifiedPath {
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

fn package_source_from_items(id: &PackageId, items: &[PackageItem]) -> PackageSource {
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
    let mut source = PackageSource::new(id.clone(), id.as_str(), graph);
    source.items = items.to_vec();
    source
}
