use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::Item;
use fp_core::frontend::LanguageFrontend;
use fp_core::module::path::QualifiedPath;
use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageMetadata,
    PackageSource,
};
use fp_core::vfs::VirtualPath;

use crate::FerroFrontend;
use crate::embedded_libc;
use crate::embedded_std;

/// `PackageProvider` for the embedded Ferro standard library. `std` is
/// baked into the binary (see `embedded_std`), so there's no real
/// filesystem to watch — `refresh` is a no-op.
pub struct FerroPhaseProvider;

const STD_PACKAGE_NAME: &str = "std";
const LIBC_PACKAGE_NAME: &str = "libc";

fn load_embedded_package(
    package_name: &str,
    root: std::path::PathBuf,
    module_paths: &'static [&'static str],
    read: fn(&std::path::Path) -> Option<&'static str>,
) -> ProviderResult<PackageSource> {
    let frontend = FerroFrontend::new();
    let package_id = PackageId::new(package_name);
    let mut descriptors = Vec::new();
    let mut items_by_path: HashMap<QualifiedPath, Vec<Item>> = HashMap::new();

    for relative_str in module_paths {
        let path = root.join(relative_str);
        let Some(source) = read(&path) else {
            continue;
        };
        let module_path = relative_to_module_segments(package_name, relative_str);
        if module_path.is_empty() {
            continue;
        }
        let result = frontend.parse_file(source, &path).map_err(|e| {
            ProviderError::other(format!("failed to parse {relative_str}: {e}"))
        })?;
        let items = result.ast.items;
        if !items.is_empty() {
            items_by_path.insert(QualifiedPath::new(module_path.clone()), items);
        }
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
    krate.items = items_by_path;
    Ok(krate)
}

impl PackageProvider for FerroPhaseProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![PackageId::new(STD_PACKAGE_NAME), PackageId::new(LIBC_PACKAGE_NAME)])
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let root = match id.as_str() {
            STD_PACKAGE_NAME => embedded_std::root_dir(),
            LIBC_PACKAGE_NAME => embedded_libc::root_dir(),
            _ => return Err(ProviderError::PackageNotFound(id.clone())),
        };
        let mut metadata = PackageMetadata::default();
        if id.as_str() == STD_PACKAGE_NAME {
            metadata.dependencies.push(DependencyDescriptor {
                package: LIBC_PACKAGE_NAME.to_string(),
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
            manifest_path: VirtualPath::from_path(&root.join("fp.toml")),
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
            STD_PACKAGE_NAME => load_embedded_package(
                STD_PACKAGE_NAME,
                embedded_std::root_dir(),
                embedded_std::module_paths(),
                embedded_std::read,
            ),
            LIBC_PACKAGE_NAME => load_embedded_package(
                LIBC_PACKAGE_NAME,
                embedded_libc::root_dir(),
                embedded_libc::module_paths(),
                embedded_libc::read,
            ),
            _ => Err(ProviderError::PackageNotFound(id.clone())),
        }
    }
}

fn relative_to_module_segments(package_name: &str, relative: &str) -> Vec<String> {
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
