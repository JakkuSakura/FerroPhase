use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::Item;
use fp_core::frontend::LanguageFrontend;
use fp_core::module::path::QualifiedPath;
use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::package::{PackageCrate, PackageDescriptor, PackageId};
use fp_core::vfs::VirtualPath;

use crate::embedded_std;
use crate::FerroFrontend;

/// `PackageProvider` for the embedded Ferro standard library. `std` is
/// baked into the binary (see `embedded_std`), so there's no real
/// filesystem to watch — `refresh` is a no-op.
pub struct EmbeddedStdPackageProvider;

const STD_PACKAGE_NAME: &str = "std";

impl EmbeddedStdPackageProvider {
    fn package_id() -> PackageId {
        PackageId::new(STD_PACKAGE_NAME)
    }
}

impl PackageProvider for EmbeddedStdPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![Self::package_id()])
    }

    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id.as_str() != STD_PACKAGE_NAME {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        let std_root = embedded_std::root_dir();
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: STD_PACKAGE_NAME.to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&std_root.join("fp.toml")),
            root: VirtualPath::from_path(&std_root),
            metadata: Default::default(),
            modules: Vec::new(),
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_items(&self, id: &PackageId) -> ProviderResult<PackageCrate> {
        if id.as_str() != STD_PACKAGE_NAME {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }

        let frontend = FerroFrontend::new();
        let std_root = embedded_std::root_dir();
        let package_id = Self::package_id();
        let mut descriptors: Vec<ModuleDescriptor> = Vec::new();
        let mut items_by_path: HashMap<QualifiedPath, Vec<Item>> = HashMap::new();

        for relative_str in embedded_std::module_paths() {
            let path = std_root.join(relative_str);
            let Some(source) = embedded_std::read(&path) else {
                continue;
            };

            let module_path = relative_to_module_segments(relative_str);
            if module_path.is_empty() {
                continue;
            }

            let result = frontend
                .parse_file(source, &path)
                .map_err(|e| ProviderError::other(format!("failed to parse {relative_str}: {e}")))?;

            let items = result.ast.items;
            let qpath = QualifiedPath::new(module_path.clone());
            if !items.is_empty() {
                items_by_path.insert(qpath, items);
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

        let module_ids: Vec<_> = descriptors.iter().map(|d| d.id.clone()).collect();
        let package = PackageDescriptor {
            id: package_id.clone(),
            name: STD_PACKAGE_NAME.to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&std_root.join("fp.toml")),
            root: VirtualPath::from_path(&std_root),
            metadata: Default::default(),
            modules: module_ids,
        };
        let mut graph = PackageGraph::new(vec![package]);
        for desc in descriptors {
            graph.insert_module(desc);
        }

        let mut krate = PackageCrate::new(STD_PACKAGE_NAME, graph);
        krate.items = items_by_path;
        Ok(krate)
    }
}

fn relative_to_module_segments(relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec![STD_PACKAGE_NAME.to_string()];
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
