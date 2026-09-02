use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{AstPackage, PackageDescriptor, PackageId, PackageItem};
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;

#[derive(Debug)]
pub struct CPackageProvider {
    root: PathBuf,
}

impl CPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &Path) -> ProviderResult<Self> {
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
                "C project root is not a directory: {}",
                self.root.display()
            )));
        }

        let package_id = self.package_id();
        let mut files = Vec::new();
        collect_c_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(ProviderError::other(format!(
                "C project contains no `.c` or `.h` files: {}",
                self.root.display()
            )));
        }

        let frontend = crate::CFrontend::new().map_err(|error| {
            ProviderError::other(format!("failed to initialize C frontend: {error}"))
        })?;
        let mut descriptors = Vec::with_capacity(files.len());
        let mut items = Vec::new();
        let mut module_paths = HashSet::new();
        for file in files {
            let relative = file.strip_prefix(&self.root).map_err(|error| {
                ProviderError::other(format!(
                    "failed to relativize C source {}: {error}",
                    file.display()
                ))
            })?;
            let module_path = module_path_for(relative);
            let source = std::fs::read_to_string(&file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to read C source {}: {error}",
                    file.display()
                ))
            })?;
            let parsed = frontend.parse_file(&source, &file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to parse C source {}: {error}",
                    file.display()
                ))
            })?;
            module_paths.insert(module_path.clone());
            descriptors.push(ModuleDescriptor {
                id: ModuleId::new(module_path.to_key()),
                package: package_id.clone(),
                language: ModuleLanguage::Other("c".to_string()),
                module_path: module_path.segments.clone(),
                source: VirtualPath::from_path(&file),
                exports: Vec::new(),
                requires_features: Vec::new(),
            });
            items.extend(parsed.ast.items.into_iter().map(|item| PackageItem {
                module_path: module_path.clone(),
                item,
            }));
        }

        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&self.root.join("c.toml")),
            root: VirtualPath::from_path(&self.root),
            metadata: Default::default(),
        };
        let graph = descriptor;
        let package_name = package_id.as_str().to_string();
        let mut package = AstPackage::new(package_id, package_name, graph);
        package.set_items(items);
        Ok(package)
    }
}

impl PackageProvider for CPackageProvider {
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

fn collect_c_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        ProviderError::other(format!(
            "failed to read C project directory {}: {error}",
            root.display()
        ))
    })? {
        let entry = entry.map_err(|error| {
            ProviderError::other(format!(
                "failed to inspect C project directory {}: {error}",
                root.display()
            ))
        })?;
        let path = entry.path();
        if path.is_dir() {
            collect_c_files(&path, files)?;
        } else if path
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| {
                extension.eq_ignore_ascii_case("c") || extension.eq_ignore_ascii_case("h")
            })
        {
            files.push(path);
        }
    }
    Ok(())
}

fn module_path_for(relative: &Path) -> QualifiedPath {
    let mut segments = Vec::new();
    for component in relative.components() {
        let std::path::Component::Normal(segment) = component else {
            continue;
        };
        let Some(segment) = segment.to_str() else {
            continue;
        };
        let segment = segment
            .strip_suffix(".c")
            .or_else(|| segment.strip_suffix(".h"))
            .unwrap_or(segment);
        segments.push(segment.to_string());
    }
    QualifiedPath::new(segments)
}
