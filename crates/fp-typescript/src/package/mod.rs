use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId,
    PackageMetadata, TargetFilter,
};
use fp_core::vfs::VirtualPath;
use semver::{Version, VersionReq};
use serde::Deserialize;
use walkdir::{DirEntry, WalkDir};

const SKIP_DIR_NAMES: &[&str] = &[
    "node_modules",
    "git",
    "hg",
    "svn",
    "pnpm",
    "dist",
    "build",
    "coverage",
    "tmp",
    "vendor",
    "turbo",
    "target",
    "out",
    "yarn",
    "idea",
    "vscode",
];

#[derive(Debug)]
pub struct TypeScriptPackageProvider {
    root: PathBuf,
    packages: RwLock<HashMap<PackageId, Arc<PackageDescriptor>>>,
    modules: RwLock<HashMap<ModuleId, Arc<ModuleDescriptor>>>,
}

impl TypeScriptPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            packages: RwLock::new(HashMap::new()),
            modules: RwLock::new(HashMap::new()),
        }
    }

    fn package_manifest(&self) -> PathBuf {
        self.root.join("package.json")
    }

    fn read_manifest(&self) -> ProviderResult<PackageJson> {
        read_package_json(&self.package_manifest()).map_err(|err| {
            ProviderError::metadata(format!("{}: {err}", self.package_manifest().display()))
        })
    }

    fn convert_dependencies(
        entries: Option<HashMap<String, serde_json::Value>>,
        kind: DependencyKind,
    ) -> Vec<DependencyDescriptor> {
        entries
            .into_iter()
            .flat_map(|map| map.into_iter())
            .map(|(name, value)| {
                let constraint_raw = value.as_str().or_else(|| {
                    value
                        .as_object()
                        .and_then(|obj| obj.get("version").and_then(|v| v.as_str()))
                });
                let constraint = constraint_raw.and_then(|raw| VersionReq::parse(raw).ok());
                DependencyDescriptor {
                    resolved_package_id: None,
                    package: name,
                    constraint,
                    kind: kind.clone(),
                    features: Vec::new(),
                    optional: false,
                    target: TargetFilter {
                        cfg: None,
                        languages: vec!["typescript".to_string()],
                    },
                }
            })
            .collect()
    }

    fn collect_modules(&self, package_id: &PackageId) -> ProviderResult<Vec<ModuleDescriptor>> {
        let mut descriptors = Vec::new();
        for entry in WalkDir::new(&self.root)
            .into_iter()
            .filter_entry(|entry| should_descend(entry))
            .filter_map(|entry| match entry {
                Ok(e) => Some(e),
                Err(err) => {
                    eprintln!("[fp-typescript] error walking package dir: {err}");
                    None
                }
            })
            .filter(|entry| entry.file_type().is_file())
        {
            let path = entry.into_path();
            if !is_typescript_source(&path) {
                continue;
            }

            let relative = path.strip_prefix(&self.root).unwrap_or(&path);
            let virtual_path = VirtualPath::from_path(relative);
            let module_path = module_path_from_file(relative);
            let module_id = ModuleId::new(format!(
                "{}::{}",
                package_id.as_str(),
                module_path.join("::")
            ));

            descriptors.push(ModuleDescriptor {
                id: module_id,
                package: package_id.clone(),
                language: ModuleLanguage::TypeScript,
                module_path,
                source: virtual_path,
                exports: Vec::new(),
                requires_features: Vec::new(),
            });
        }
        Ok(descriptors)
    }
}

fn should_descend(entry: &DirEntry) -> bool {
    if entry.depth() == 0 || !entry.file_type().is_dir() {
        return true;
    }
    let name = entry.file_name().to_string_lossy();
    let lower = name.to_ascii_lowercase();
    let trimmed = lower.trim_start_matches('.');
    !SKIP_DIR_NAMES
        .iter()
        .any(|skip| lower == *skip || trimmed == *skip)
}

impl PackageProvider for TypeScriptPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        let guard = match self.packages.read() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        let mut packages: Vec<_> = guard.keys().cloned().collect();
        packages.sort_by_key(|id| id.to_string());
        Ok(packages)
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(fp_core::intrinsics::NoopIntrinsicNormalizer)
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let guard = match self.packages.read() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        guard
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn refresh(&self) -> ProviderResult<()> {
        let manifest = self.read_manifest()?;
        let manifest_path = self.package_manifest();
        let version = manifest
            .version
            .as_ref()
            .and_then(|raw| Version::parse(raw).ok());
        let package_id = PackageId::with_source(
            manifest.name.clone(),
            version.clone(),
            format!("npm:{}", manifest_path.display()),
        );

        let mut dependencies = Vec::new();
        dependencies.extend(Self::convert_dependencies(
            manifest.dependencies,
            DependencyKind::Normal,
        ));
        dependencies.extend(Self::convert_dependencies(
            manifest.dev_dependencies,
            DependencyKind::Development,
        ));
        dependencies.extend(Self::convert_dependencies(
            manifest.optional_dependencies,
            DependencyKind::Normal,
        ));

        let metadata = PackageMetadata {
            edition: None,
            authors: manifest.authors.unwrap_or_default(),
            description: manifest.description,
            license: manifest.license,
            keywords: manifest.keywords.unwrap_or_default(),
            registry: None,
            features: BTreeMap::new(),
            dependencies,
            prelude: None,
        };

        let modules = self.collect_modules(&package_id)?;
        let module_ids: Vec<ModuleId> = modules
            .iter()
            .map(|descriptor| descriptor.id.clone())
            .collect();

        let package_descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: manifest.name,
            version,
            manifest_path: VirtualPath::from_path(&manifest_path),
            root: VirtualPath::from_path(&self.root),
            metadata,
            modules: module_ids.clone(),
        };

        match self.packages.write() {
            Ok(mut w) => *w = HashMap::from([(package_id.clone(), Arc::new(package_descriptor))]),
            Err(poison) => {
                *poison.into_inner() =
                    HashMap::from([(package_id.clone(), Arc::new(package_descriptor))])
            }
        }
        match self.modules.write() {
            Ok(mut w) => {
                *w = modules
                    .into_iter()
                    .map(|descriptor| (descriptor.id.clone(), Arc::new(descriptor)))
                    .collect();
            }
            Err(poison) => {
                *poison.into_inner() = modules
                    .into_iter()
                    .map(|descriptor| (descriptor.id.clone(), Arc::new(descriptor)))
                    .collect();
            }
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        let descriptor = self.load_package_metadata(id)?;
        let modules = match self.modules.read() {
            Ok(g) => g,
            Err(poison) => poison.into_inner(),
        };
        let mut graph = PackageGraph::new(vec![(*descriptor).clone()]);
        for module_id in &descriptor.modules {
            let module = modules
                .get(module_id)
                .ok_or_else(|| ProviderError::ModuleNotFound(module_id.clone()))?;
            graph.insert_module((**module).clone());
        }
        Ok(AstPackage::new(id.clone(), descriptor.name.clone(), graph))
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct PackageJson {
    pub name: String,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub authors: Option<Vec<String>>,
    #[serde(default)]
    pub keywords: Option<Vec<String>>,
    #[serde(rename = "dependencies")]
    pub dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "devDependencies")]
    pub dev_dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "optionalDependencies")]
    pub optional_dependencies: Option<HashMap<String, serde_json::Value>>,
}

pub fn read_package_json(path: &Path) -> Result<PackageJson, ProviderError> {
    let contents = fs::read_to_string(path)
        .map_err(|err| ProviderError::metadata(format!("{}: {err}", path.display())))?;
    serde_json::from_str(&contents)
        .map_err(|err| ProviderError::metadata(format!("{}: {err}", path.display())))
}

fn is_typescript_source(path: &Path) -> bool {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("ts") | Some("tsx") => match path.file_name().and_then(|name| name.to_str()) {
            Some(name) if name.ends_with(".d.ts") => false,
            _ => true,
        },
        _ => false,
    }
}

pub fn default_module_roots(root: &Path) -> Vec<PathBuf> {
    let src = root.join("src");
    if src.is_dir() {
        vec![src]
    } else {
        vec![root.to_path_buf()]
    }
}

pub fn estimate_module_path(root: &Path, file_path: &Path) -> Vec<String> {
    estimate_module_path_with_roots(root, &default_module_roots(root), file_path)
}

pub fn estimate_module_path_with_roots(
    root: &Path,
    module_roots: &[PathBuf],
    file_path: &Path,
) -> Vec<String> {
    let module_root = module_roots
        .iter()
        .filter(|candidate| file_path.starts_with(candidate))
        .max_by_key(|candidate| candidate.components().count())
        .cloned()
        .unwrap_or_else(|| root.to_path_buf());
    let rel = file_path
        .strip_prefix(&module_root)
        .or_else(|_| file_path.strip_prefix(root))
        .unwrap_or(file_path);
    module_path_from_file(rel)
}

fn module_path_from_file(path: &Path) -> Vec<String> {
    let mut components = path
        .components()
        .map(|component| component.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<_>>();
    if let Some(last) = components.last_mut() {
        if let Some(stripped) = last.strip_suffix(".tsx") {
            *last = stripped.to_string();
        } else if let Some(stripped) = last.strip_suffix(".ts") {
            *last = stripped.to_string();
        } else if let Some(stripped) = last.strip_suffix(".jsx") {
            *last = stripped.to_string();
        } else if let Some(stripped) = last.strip_suffix(".js") {
            *last = stripped.to_string();
        }
        if last == "index" {
            components.pop();
        }
    }
    components
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use tempfile::tempdir;

    #[test]
    fn collects_typescript_modules() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::write(
            root.join("package.json"),
            r#"{"name": "example", "version": "1.0.0"}"#,
        )?;
        fs::create_dir_all(root.join("src"))?;
        fs::write(root.join("src/lib.ts"), "export const value = 1;")?;
        fs::create_dir_all(root.join("src/util"))?;
        fs::write(
            root.join("src/util/helpers.tsx"),
            "export const Component = () => null;",
        )?;
        fs::create_dir_all(root.join("node_modules/ignored"))?;
        fs::write(
            root.join("node_modules/ignored/index.ts"),
            "export const ignored = true;",
        )?;
        fs::create_dir_all(root.join("dist"))?;
        fs::write(
            root.join("dist/generated.ts"),
            "export const generated = true;",
        )?;

        let provider = TypeScriptPackageProvider::new(root.to_path_buf());
        provider.refresh()?;

        let packages = provider.list_packages()?;
        assert_eq!(packages.len(), 1);
        let package_id = packages[0].clone();

        let source = provider.load_package_source(&package_id)?;
        let module_ids = source
            .graph
            .modules_for_package(&package_id)
            .expect("package graph should contain TypeScript modules");
        assert_eq!(module_ids.len(), 2);

        let module = source
            .graph
            .module(&module_ids[0])
            .expect("package graph should contain module descriptor");
        assert_eq!(module.language, ModuleLanguage::TypeScript);
        Ok(())
    }

    #[test]
    fn reads_package_json_manifest() -> Result<()> {
        let temp = tempdir()?;
        let path = temp.path().join("package.json");
        fs::write(&path, r#"{"name": "example", "version": "1.0.0"}"#)?;

        let manifest = read_package_json(&path).map_err(|err| eyre::eyre!(err.to_string()))?;
        assert_eq!(manifest.name, "example");
        assert_eq!(manifest.version.as_deref(), Some("1.0.0"));
        Ok(())
    }

    #[test]
    fn estimates_typescript_index_module_path() {
        assert_eq!(
            estimate_module_path(Path::new("/proj"), Path::new("/proj/src/features/index.ts")),
            vec!["src".to_string(), "features".to_string()]
        );
    }
}
