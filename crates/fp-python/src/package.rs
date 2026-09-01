use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata, TargetFilter,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;
use semver::Version;

#[derive(Debug)]
pub struct PythonPackageProvider {
    root: PathBuf,
}

impl PythonPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn discover(root: &Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }

    fn manifest_path(&self) -> PathBuf {
        self.root.join("pyproject.toml")
    }

    fn package_id(&self) -> ProviderResult<PackageId> {
        let manifest = crate::read_pyproject(&self.manifest_path()).map_err(|error| {
            ProviderError::metadata(format!("{}: {error}", self.manifest_path().display()))
        })?;
        let version = manifest
            .version
            .as_deref()
            .map(Version::parse)
            .transpose()
            .map_err(|error| {
                ProviderError::metadata(format!("invalid Python package version: {error}"))
            })?;
        Ok(PackageId::with_source(
            manifest.name,
            version,
            format!("python:{}", self.manifest_path().display()),
        ))
    }

    fn snapshot(&self) -> ProviderResult<AstPackage> {
        if !self.root.is_dir() {
            return Err(ProviderError::other(format!(
                "Python project root is not a directory: {}",
                self.root.display()
            )));
        }
        let manifest = crate::read_pyproject(&self.manifest_path()).map_err(|error| {
            ProviderError::metadata(format!("{}: {error}", self.manifest_path().display()))
        })?;
        let version = manifest
            .version
            .as_deref()
            .map(Version::parse)
            .transpose()
            .map_err(|error| {
                ProviderError::metadata(format!("invalid Python package version: {error}"))
            })?;
        let package_id = PackageId::with_source(
            manifest.name.clone(),
            version.clone(),
            format!("python:{}", self.manifest_path().display()),
        );
        let mut files = Vec::new();
        collect_python_files(&self.root, &mut files)?;
        files.sort();
        if files.is_empty() {
            return Err(ProviderError::other(format!(
                "Python project contains no `.py` files: {}",
                self.root.display()
            )));
        }

        let frontend = crate::PythonFrontend::new();
        let mut descriptors = Vec::with_capacity(files.len());
        let mut items = Vec::new();
        let mut module_paths = HashSet::new();
        for file in files {
            let module_path = crate::estimate_module_path(&self.root, &file);
            let module_path = QualifiedPath::new(module_path);
            let source = std::fs::read_to_string(&file).map_err(|error| {
                ProviderError::other(format!(
                    "failed to read Python source {}: {error}",
                    file.display()
                ))
            })?;
            let parsed = frontend.parse(&source, Some(&file)).map_err(|error| {
                ProviderError::other(format!(
                    "failed to parse Python source {}: {error}",
                    file.display()
                ))
            })?;
            module_paths.insert(module_path.clone());
            descriptors.push(ModuleDescriptor {
                id: ModuleId::new(module_path.to_key()),
                package: package_id.clone(),
                language: ModuleLanguage::Other("python".to_string()),
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

        let dependencies = manifest
            .dependencies
            .into_iter()
            .chain(manifest.optional_dependencies.into_values().flatten())
            .map(|dependency| DependencyDescriptor {
                package: dependency,
                resolved_package_id: None,
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: TargetFilter {
                    cfg: None,
                    languages: vec!["python".to_string()],
                },
            })
            .collect();
        let metadata = PackageMetadata {
            edition: None,
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
            name: manifest.name,
            version,
            manifest_path: VirtualPath::from_path(&self.manifest_path()),
            root: VirtualPath::from_path(&self.root),
            metadata,
            modules: module_ids,
        };
        let mut graph = PackageGraph::new(vec![descriptor]);
        let package_name = package_id.as_str().to_string();
        let mut package = AstPackage::new(package_id, package_name, graph);
        package.items = items;
        Ok(package)
    }
}

impl PackageProvider for PythonPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id()?])
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
            .graph
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

fn collect_python_files(root: &Path, files: &mut Vec<PathBuf>) -> ProviderResult<()> {
    for entry in std::fs::read_dir(root).map_err(|error| {
        ProviderError::other(format!(
            "failed to read Python project directory {}: {error}",
            root.display()
        ))
    })? {
        let entry = entry.map_err(|error| {
            ProviderError::other(format!(
                "failed to inspect Python project directory {}: {error}",
                root.display()
            ))
        })?;
        let path = entry.path();
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("");
            if !name.starts_with('.')
                && !matches!(
                    name,
                    "__pycache__" | "build" | "dist" | "target" | "venv" | ".venv"
                )
            {
                collect_python_files(&path, files)?;
            }
        } else if path
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| extension.eq_ignore_ascii_case("py"))
        {
            files.push(path);
        }
    }
    Ok(())
}
