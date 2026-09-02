use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleLanguage};
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{AstPackage, PackageDescriptor, PackageId};
use fp_core::ast::path::InPackagePath;
use fp_core::vfs::VirtualPath;

#[derive(Debug)]
pub struct ZigPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl ZigPackageProvider {
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
                "Zig project root is not a directory: {}",
                self.root.display()
            )));
        }
        let package_id = self.package_id();
        let mut files = Vec::new();
        collect_zig_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(ProviderError::other(format!(
                "Zig project contains no `.zig` files: {}",
                self.root.display()
            )));
        }

        let mut descriptors = Vec::new();
        let mut modules = Vec::new();
        let mut module_paths = HashSet::new();
        for file in files {
            let relative = file.strip_prefix(&self.root).map_err(|error| {
                ProviderError::other(format!(
                    "failed to relativize Zig source {}: {error}",
                    file.display()
                ))
            })?;
            let module_path = InPackagePath::new(module_path_for(relative));
            let source = std::fs::read_to_string(&file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to read Zig source {}: {error}",
                    file.display()
                ))
            })?;
            let mut parser = crate::ZigParser::new().map_err(|error| {
                ProviderError::other(format!("failed to initialize Zig parser: {error}"))
            })?;
            let parsed = parser.parse_str(&source).map_err(|error| {
                ProviderError::other(format!(
                    "failed to parse Zig source {}: {error}",
                    file.display()
                ))
            })?;
            if module_paths.insert(module_path.clone()) {
                descriptors.push(ModuleDescriptor {
                    id: module_path.to_key(),
                    package: package_id.clone(),
                    language: ModuleLanguage::Other("zig".to_string()),
                    module_path: module_path.segments.clone(),
                    source: VirtualPath::from_path(relative),
                    exports: Vec::new(),
                    requires_features: Vec::new(),
                });
            }
            modules.push(fp_core::ast::Module {
                attrs: Vec::new(),
                name: fp_core::ast::Ident::new(module_path.tail().unwrap_or("")),
                items: parsed.items,
                visibility: fp_core::ast::Visibility::Public,
                is_external: false,
            });
        }

        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&self.root.join("build.zig")),
            root: VirtualPath::from_path(&self.root),
            metadata: Default::default(),
        };
        let graph = descriptor;
        let package_name = package_id.as_str().to_string();
        let mut package = AstPackage::new(package_id, package_name, graph, Vec::new());
        package.module.items = modules
            .into_iter()
            .flat_map(|module| module.items)
            .collect();
        Ok(package)
    }
}

impl PackageProvider for ZigPackageProvider {
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
        Ok(Arc::new(package.package.clone()))
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

fn collect_zig_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        ProviderError::other(format!(
            "failed to read Zig project directory {}: {error}",
            root.display()
        ))
    })? {
        let entry = entry.map_err(|error| {
            ProviderError::other(format!(
                "failed to inspect Zig project directory {}: {error}",
                root.display()
            ))
        })?;
        let path = entry.path();
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("");
            if !name.starts_with('.') && !matches!(name, "zig-cache" | "zig-out") {
                collect_zig_files(&path, files)?;
            }
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("zig") {
            files.push(path);
        }
    }
    Ok(())
}

fn module_path_for(path: &Path) -> Vec<String> {
    let mut components = path
        .components()
        .map(|component| component.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<_>>();
    if let Some(last) = components.last_mut() {
        if let Some(stem) = last.strip_suffix(".zig") {
            *last = stem.to_string();
        }
    }
    components
}
