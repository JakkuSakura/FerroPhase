use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::{Item, ItemKind};
use fp_core::frontend::LanguageFrontend;
use fp_core::module::path::QualifiedPath;
use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata, PackageSource,
};
use fp_core::vfs::VirtualPath;
use fp_lang::{FerroFrontend, project};

use crate::RustFrontend;

/// `PackageProvider` for real `.rs`/Cargo-based projects (as opposed to
/// `fp_lang::cargo_provider::CargoWorkspaceProvider`'s own `.fp` dialect).
///
/// Workspace discovery reuses `fp_lang::project` (the same Cargo/Magnet
/// manifest walking `CargoWorkspaceProvider` uses), but parsing goes through
/// `RustFrontend` specifically — kept as its own path (rather than
/// delegating to `CargoWorkspaceProvider` wholesale) so Rust-specific parsing
/// work has a real seam to land in without touching `.fp`-dialect behavior.
pub struct RustPackageProvider {
    root: PathBuf,
    members: Vec<(String, PathBuf)>,
    cache: RwLock<HashMap<String, Vec<PackageItem>>>,
}

impl RustPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        let members = project::find_manifest(&root)
            .map(|manifest_root| project::list_members(&manifest_root))
            .unwrap_or_default();
        Self {
            root,
            members,
            cache: RwLock::new(HashMap::new()),
        }
    }

    pub fn discover(root: &Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }

    fn resolve_dir(&self, id: &PackageId) -> ProviderResult<PathBuf> {
        self.members
            .iter()
            .find(|(name, _)| name == id.as_str())
            .map(|(_, dir)| dir.clone())
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }
}

impl PackageProvider for RustPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(self
            .members
            .iter()
            .map(|(name, _)| PackageId::new(name))
            .collect())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let dir = self.resolve_dir(id)?;
        let mut module_ids = Vec::new();
        for (rel, _) in project::list_sources(&dir) {
            let path = rs_relative_to_module_path(&rel);
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
        let frontend = RustFrontend::new();
        let mut items = Vec::new();

        for (rel, abs) in project::list_sources(&dir) {
            let source = std::fs::read_to_string(&abs)
                .map_err(|e| ProviderError::other(format!("read {}: {}", abs.display(), e)))?;
            let result = frontend
                .parse_file(&source, &abs)
                .map_err(|e| ProviderError::other(format!("parse {}: {}", abs.display(), e)))?;
            let path = rs_relative_to_module_path(&rel);
            items.extend(result.ast.items.into_iter().map(|item| PackageItem {
                path: path.clone(),
                item,
            }));
        }

        if let Ok(mut c) = self.cache.write() {
            c.insert(id.as_str().to_string(), items.clone());
        }

        Ok(package_source_from_items(id, &items))
    }
}

fn rs_relative_to_module_path(rel: &str) -> QualifiedPath {
    let stem = rel.trim_end_matches(".rs").trim_end_matches(".fp");
    let parts: Vec<String> = stem.split('/').map(|s| s.to_string()).collect();
    QualifiedPath::new(parts)
}

fn package_source_from_items(id: &PackageId, items: &[PackageItem]) -> PackageSource {
    use std::collections::HashSet;
    let paths: HashSet<_> = items.iter().map(|item| item.path.clone()).collect();
    let descriptors: Vec<ModuleDescriptor> = paths
        .into_iter()
        .map(|path| ModuleDescriptor {
            id: ModuleId::new(&path.to_key()),
            package: id.clone(),
            language: ModuleLanguage::Rust,
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

const STD_PACKAGE_NAME: &str = "std";
const LIBC_PACKAGE_NAME: &str = "libc";

/// `PackageProvider` for the "std"/"libc" package IDs, backed by real rustc
/// `core`/`alloc`/`std` source (`RustPackageProvider`'s counterpart to
/// `fp_lang::provider::FerroPhaseProvider`). `libc` is delegated straight to
/// `fp-lang`'s embedded copy — there's nothing Rust-specific about C ABI
/// declarations, no need to duplicate them.
///
/// Real std source is far more complex than anything `FerroFrontend` has been
/// validated against (heavy `unsafe`, `#[lang = "..."]` items, `cfg`-gated
/// platform code, const generics, specialization, inline asm, ...). Files
/// that fail to parse are skipped with a warning rather than failing the
/// whole package load, so whatever subset *does* parse is still usable.
pub struct RustStdProvider;

impl PackageProvider for RustStdProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![
            PackageId::new(STD_PACKAGE_NAME),
            PackageId::new(LIBC_PACKAGE_NAME),
        ])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let root = match id.as_str() {
            STD_PACKAGE_NAME => crate::embedded_std::root_dir(),
            LIBC_PACKAGE_NAME => fp_lang::embedded_libc::root_dir(),
            _ => return Err(ProviderError::PackageNotFound(id.clone())),
        };
        let mut metadata = PackageMetadata::default();
        if id.as_str() == STD_PACKAGE_NAME {
            metadata.dependencies.push(DependencyDescriptor {
                package: LIBC_PACKAGE_NAME.to_string(),
                resolved_package_id: Some(PackageId::new(LIBC_PACKAGE_NAME)),
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: Default::default(),
            });
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&root.join("Cargo.toml")),
            root: VirtualPath::from_path(&root),
            metadata,
            modules: Vec::new(),
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<PackageSource> {
        match id.as_str() {
            STD_PACKAGE_NAME => load_real_std_package(),
            LIBC_PACKAGE_NAME => load_embedded_fp_package(
                LIBC_PACKAGE_NAME,
                fp_lang::embedded_libc::root_dir(),
                fp_lang::embedded_libc::module_paths(),
                fp_lang::embedded_libc::read,
            ),
            _ => Err(ProviderError::PackageNotFound(id.clone())),
        }
    }
}

fn flatten_items(path: &QualifiedPath, items: &[Item], output: &mut Vec<PackageItem>) {
    for item in items {
        if let ItemKind::Module(module) = item.kind() {
            flatten_items(
                &path.with_segment(module.name.as_str().to_owned()),
                &module.items,
                output,
            );
        } else {
            output.push(PackageItem {
                path: path.clone(),
                item: item.clone(),
            });
        }
    }
}

/// Parse every embedded real-std `.rs` file, skipping (with a warning) any
/// that `RustFrontend` can't handle yet, rather than failing the whole load.
fn load_real_std_package() -> ProviderResult<PackageSource> {
    let frontend = RustFrontend::new();
    let package_id = PackageId::new(STD_PACKAGE_NAME);
    let root = crate::embedded_std::root_dir();
    let mut descriptors = Vec::new();
    let mut items = Vec::new();
    let mut parsed = 0usize;
    let mut skipped = 0usize;

    for relative_str in crate::embedded_std::module_paths() {
        let path = root.join(relative_str);
        let Some(source) = crate::embedded_std::read(&path) else {
            continue;
        };
        let module_path = rs_relative_to_module_segments(relative_str);
        if module_path.is_empty() {
            continue;
        }
        let result = match frontend.parse_file(source, &path) {
            Ok(result) => result,
            Err(_) => {
                skipped += 1;
                continue;
            }
        };
        parsed += 1;
        flatten_items(
            &QualifiedPath::new(module_path.clone()),
            &result.ast.items,
            &mut items,
        );
        descriptors.push(ModuleDescriptor {
            id: ModuleId::new(module_path.join("::")),
            package: package_id.clone(),
            language: ModuleLanguage::Rust,
            module_path,
            source: VirtualPath::from_path(&path),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
    }
    eprintln!(
        "fp-rust: real std parse result — {parsed} file(s) parsed, {skipped} skipped (parse errors)"
    );

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: STD_PACKAGE_NAME.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join("Cargo.toml")),
        root: VirtualPath::from_path(&root),
        metadata: Default::default(),
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for descriptor in descriptors {
        graph.insert_module(descriptor);
    }
    let mut krate = PackageSource::new(package_id, STD_PACKAGE_NAME, graph);
    krate.items = items;
    Ok(krate)
}

/// Same shape as `fp_lang::provider`'s private `load_embedded_package`, kept
/// as its own copy here since that one isn't exported — used for delegating
/// to `fp-lang`'s embedded `libc` `.fp` source.
fn load_embedded_fp_package(
    package_name: &str,
    root: PathBuf,
    module_paths: &'static [&'static str],
    read: fn(&std::path::Path) -> Option<&'static str>,
) -> ProviderResult<PackageSource> {
    let frontend = FerroFrontend::new();
    let package_id = PackageId::new(package_name);
    let mut descriptors = Vec::new();
    let mut items = Vec::new();

    for relative_str in module_paths {
        let path = root.join(relative_str);
        let Some(source) = read(&path) else {
            continue;
        };
        let module_path = fp_relative_to_module_segments(package_name, relative_str);
        if module_path.is_empty() {
            continue;
        }
        let result = frontend
            .parse_file(source, &path)
            .map_err(|e| ProviderError::other(format!("failed to parse {relative_str}: {e}")))?;
        flatten_items(
            &QualifiedPath::new(module_path.clone()),
            &result.ast.items,
            &mut items,
        );
        descriptors.push(ModuleDescriptor {
            id: ModuleId::new(module_path.join("::")),
            package: package_id.clone(),
            language: ModuleLanguage::Ferro,
            module_path,
            source: VirtualPath::from_path(&path),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
    }

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: package_name.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join("fp.toml")),
        root: VirtualPath::from_path(&root),
        metadata: Default::default(),
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for descriptor in descriptors {
        graph.insert_module(descriptor);
    }
    let mut krate = PackageSource::new(PackageId::new(package_name), package_name, graph);
    krate.items = items;
    Ok(krate)
}

/// `core/option.rs` -> `["std", "core", "option"]`, `alloc/vec/mod.rs` ->
/// `["std", "alloc", "vec"]`, `std/sync/mod.rs` -> `["std", "std", "sync"]`
/// (the third segment is the real `std` facade crate re-exporting
/// `core`/`alloc` — kept distinct from the outer `std` *package* name).
fn rs_relative_to_module_segments(relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec![STD_PACKAGE_NAME.to_string()];
    let stem = relative.trim_end_matches(".rs");
    for part in stem.split('/') {
        if part == "mod" || part.is_empty() {
            continue;
        }
        segments.push(part.to_string());
    }
    segments
}

fn fp_relative_to_module_segments(package_name: &str, relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec![package_name.to_string()];
    let parts: Vec<&str> = relative.trim_end_matches(".fp").split('/').collect();
    if parts.len() == 1 && parts[0] == "mod" {
        return segments;
    }
    for part in parts {
        if part == "mod" {
            continue;
        }
        segments.push(part.to_string());
    }
    segments
}

#[cfg(test)]
mod real_std_parse_coverage {
    use super::*;

    /// Canary, not a strict gate: catches a wholesale regression (e.g. a
    /// frontend change that suddenly can't parse anything) without blocking
    /// gradual improvement to `RustFrontend`'s grammar coverage as it grows
    /// to handle more of real std's `unsafe`/`cfg`/attribute-heavy surface.
    #[test]
    fn measures_real_std_parse_coverage() {
        let frontend = RustFrontend::new();
        let root = crate::embedded_std::root_dir();
        let mut parsed = 0usize;
        let mut skipped = 0usize;
        for relative_str in crate::embedded_std::module_paths() {
            let path = root.join(relative_str);
            let Some(source) = crate::embedded_std::read(&path) else {
                continue;
            };
            match frontend.parse_file(source, &path) {
                Ok(_) => parsed += 1,
                Err(_) => skipped += 1,
            }
        }
        let total = parsed + skipped;
        let pct = parsed as f64 / total as f64 * 100.0;
        eprintln!("fp-rust: real std parse coverage — {parsed}/{total} files ({pct:.1}%)");
        assert!(
            parsed > total / 2,
            "real std parse coverage dropped below 50% ({parsed}/{total} files)"
        );
    }
}
