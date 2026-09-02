use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleLanguage};
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{AstPackage, PackageDescriptor, PackageId, PackageItem};
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;

#[derive(Debug)]
pub struct WitPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl WitPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }

    fn package_id(&self) -> PackageId {
        PackageId::new(
            self.root
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("main"),
        )
    }

    fn snapshot(&self) -> ProviderResult<AstPackage> {
        if !self.root.is_dir() {
            return Err(ProviderError::other(format!(
                "WIT project root is not a directory: {}",
                self.root.display()
            )));
        }
        let package_id = self.package_id();
        let mut files = Vec::new();
        collect_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(ProviderError::other(format!(
                "WIT project contains no `.wit` files: {}",
                self.root.display()
            )));
        }
        let frontend = crate::WitFrontend::new();
        let mut descriptors = Vec::new();
        let mut modules = Vec::new();
        let mut module_paths = HashSet::new();
        for file in files {
            let relative = file.strip_prefix(&self.root).unwrap_or(&file);
            let module_path = QualifiedPath::new(module_path_for(relative));
            let source = std::fs::read_to_string(&file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to read WIT source {}: {error}",
                    file.display()
                ))
            })?;
            let parsed = frontend.parse(&source, Some(&file)).map_err(|error| {
                ProviderError::other(format!(
                    "failed to parse WIT source {}: {error}",
                    file.display()
                ))
            })?;
            if module_paths.insert(module_path.clone()) {
                descriptors.push(ModuleDescriptor {
                    id: module_path.to_key(),
                    package: package_id.clone(),
                    language: ModuleLanguage::Other("wit".to_string()),
                    module_path: module_path.segments.clone(),
                    source: VirtualPath::from_path(relative),
                    exports: Vec::new(),
                    requires_features: Vec::new(),
                });
            }
            modules.push(fp_core::ast::Module {
                attrs: Vec::new(),
                name: fp_core::ast::Ident::new(module_path.tail().unwrap_or("")),
                collected_items: Vec::new(),
                items: parsed.ast.items,
                visibility: fp_core::ast::Visibility::Public,
                is_external: false,
            });
        }
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&self.root),
            root: VirtualPath::from_path(&self.root),
            metadata: Default::default(),
        };
        let graph = descriptor;
        Ok(AstPackage::new(
            package_id.clone(),
            package_id.as_str().to_string(),
            graph,
            modules,
        ))
    }
}

impl PackageProvider for WitPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id()])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let package = self.snapshot()?;
        if &package.package_id != id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        package
            .package
            .package(id)
            .cloned()
            .map(Arc::new)
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        let package = self.snapshot()?;
        if &package.package_id != id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(package)
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }
}

fn collect_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        ProviderError::other(format!(
            "failed to read WIT directory {}: {error}",
            root.display()
        ))
    })? {
        let path = entry
            .map_err(|error| ProviderError::other(error.to_string()))?
            .path();
        if path.is_dir() {
            collect_files(&path, files)?;
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("wit") {
            files.push(path);
        }
    }
    Ok(())
}
fn module_path_for(path: &Path) -> Vec<String> {
    let mut parts = path
        .components()
        .map(|part| part.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<_>>();
    if let Some(last) = parts.last_mut() {
        *last = last.trim_end_matches(".wit").to_string();
    }
    parts
}
