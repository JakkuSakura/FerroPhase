use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata, TargetFilter,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;

#[derive(Debug)]
pub struct GoLangPackageProvider {
    #[allow(dead_code)]
    root: PathBuf,
}

impl GoLangPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &std::path::Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }

    fn manifest_path(&self) -> PathBuf {
        self.root.join("go.mod")
    }

    fn snapshot(&self) -> ProviderResult<AstPackage> {
        if !self.root.is_dir() {
            return Err(ProviderError::other(format!(
                "Go project root is not a directory: {}",
                self.root.display()
            )));
        }
        let manifest = crate::read_go_mod(&self.manifest_path()).map_err(|error| {
            ProviderError::metadata(format!("{}: {error}", self.manifest_path().display()))
        })?;
        let package_id = PackageId::with_source(
            manifest.module.clone(),
            None,
            format!("go:{}", self.manifest_path().display()),
        );
        let package_name = manifest.module.clone();
        let mut files = Vec::new();
        collect_go_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(ProviderError::other(format!(
                "Go project contains no `.go` files: {}",
                self.root.display()
            )));
        }

        let frontend = crate::GoFrontend::new();
        let mut descriptors = Vec::new();
        let mut items = Vec::new();
        let mut module_paths = HashSet::new();
        for file in files {
            let relative = file.strip_prefix(&self.root).map_err(|error| {
                ProviderError::other(format!(
                    "failed to relativize Go source {}: {error}",
                    file.display()
                ))
            })?;
            let mut path = crate::estimate_module_path(&self.root, &file);
            if path.is_empty() {
                path.push("root".to_string());
            }
            let module_path = QualifiedPath::new(path);
            let source = std::fs::read_to_string(&file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to read Go source {}: {error}",
                    file.display()
                ))
            })?;
            let parsed = frontend.parse(&source, Some(&file)).map_err(|error| {
                ProviderError::other(format!(
                    "failed to parse Go source {}: {error}",
                    file.display()
                ))
            })?;
            if module_paths.insert(module_path.clone()) {
                let module_id = ModuleId::new(module_path.to_key());
                descriptors.push(ModuleDescriptor {
                    id: module_id,
                    package: package_id.clone(),
                    language: ModuleLanguage::Other("go".to_string()),
                    module_path: module_path.segments.clone(),
                    source: VirtualPath::from_path(relative),
                    exports: Vec::new(),
                    requires_features: Vec::new(),
                });
            }
            items.extend(parsed.ast.items.into_iter().map(|item| PackageItem {
                module_path: module_path.clone(),
                item,
            }));
        }

        let dependencies = manifest
            .dependencies
            .into_iter()
            .map(|dependency| DependencyDescriptor {
                package: dependency.name,
                resolved_package_id: None,
                constraint: dependency
                    .version
                    .as_deref()
                    .map(|version| version.trim_start_matches('v'))
                    .and_then(|version| semver::VersionReq::parse(version).ok()),
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: TargetFilter {
                    cfg: None,
                    languages: vec!["go".to_string()],
                },
            })
            .collect();
        let metadata = PackageMetadata {
            edition: manifest.go_version,
            authors: Vec::new(),
            description: None,
            license: None,
            keywords: Vec::new(),
            registry: None,
            features: Default::default(),
            dependencies,
            prelude: None,
        };
        let module_ids = descriptors.iter().map(|module| module.id.clone()).collect();
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: manifest.module,
            version: None,
            manifest_path: VirtualPath::from_path(&self.manifest_path()),
            root: VirtualPath::from_path(&self.root),
            metadata,
            modules: module_ids,
        };
        let graph = descriptor;
        let mut package = AstPackage::new(package_id, package_name, graph);
        package.items = items;
        Ok(package)
    }
}

impl PackageProvider for GoLangPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.snapshot()?.package_id])
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

fn collect_go_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        ProviderError::other(format!(
            "failed to read Go project directory {}: {error}",
            root.display()
        ))
    })? {
        let entry = entry.map_err(|error| {
            ProviderError::other(format!(
                "failed to inspect Go project directory {}: {error}",
                root.display()
            ))
        })?;
        let path = entry.path();
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("");
            if !name.starts_with('.') && name != "vendor" {
                collect_go_files(&path, files)?;
            }
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("go")
            && path.file_name().and_then(|name| name.to_str()) != Some("go.mod")
        {
            files.push(path);
        }
    }
    Ok(())
}
