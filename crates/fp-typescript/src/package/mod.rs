use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::provider::{
    ModuleProvider, ModuleSource, PackageProvider, ProviderError, ProviderResult,
};
use fp_core::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageMetadata,
    TargetFilter,
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
    modules_by_package: RwLock<HashMap<PackageId, Vec<ModuleId>>>,
}

impl TypeScriptPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            packages: RwLock::new(HashMap::new()),
            modules: RwLock::new(HashMap::new()),
            modules_by_package: RwLock::new(HashMap::new()),
        }
    }

    fn package_manifest(&self) -> PathBuf {
        self.root.join("package.json")
    }

    fn read_manifest(&self) -> ProviderResult<PackageJson> {
        let manifest_path = self.package_manifest();
        let contents = fs::read_to_string(&manifest_path).map_err(|err| {
            ProviderError::metadata(format!("{}: {err}", manifest_path.display()))
        })?;
        serde_json::from_str(&contents)
            .map_err(|err| ProviderError::metadata(format!("{}: {err}", manifest_path.display())))
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
            .filter_map(|entry| entry.ok())
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
        Ok(self.packages.read().unwrap().keys().cloned().collect())
    }

    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        self.packages
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn refresh(&self) -> ProviderResult<()> {
        let manifest = self.read_manifest()?;
        let package_id = PackageId::new(manifest.name.clone());
        let manifest_path = self.package_manifest();
        let version = manifest
            .version
            .as_ref()
            .and_then(|raw| Version::parse(raw).ok());

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

        *self.packages.write().unwrap() =
            HashMap::from([(package_id.clone(), Arc::new(package_descriptor))]);
        *self.modules.write().unwrap() = modules
            .into_iter()
            .map(|descriptor| (descriptor.id.clone(), Arc::new(descriptor)))
            .collect();
        *self.modules_by_package.write().unwrap() = HashMap::from([(package_id, module_ids)]);
        Ok(())
    }
}

impl ModuleSource for TypeScriptPackageProvider {
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        self.modules_by_package
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn load_module_descriptor(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>> {
        self.modules
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::ModuleNotFound(id.clone()))
    }
}

pub struct TypeScriptModuleProvider<P> {
    packages: Arc<P>,
}

impl<P> TypeScriptModuleProvider<P>
where
    P: PackageProvider + ModuleSource + 'static,
{
    pub fn new(packages: Arc<P>) -> Self {
        Self { packages }
    }
}

impl<P> ModuleProvider for TypeScriptModuleProvider<P>
where
    P: PackageProvider + ModuleSource + 'static,
{
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        self.packages.modules_for_package(id)
    }

    fn load_module(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>> {
        self.packages.load_module_descriptor(id)
    }

    fn refresh(&self, id: &PackageId) -> ProviderResult<()> {
        self.packages.refresh()?;
        if self.packages.modules_for_package(id).is_err() {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
struct PackageJson {
    name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    description: Option<String>,
    #[serde(default)]
    license: Option<String>,
    #[serde(default)]
    authors: Option<Vec<String>>,
    #[serde(default)]
    keywords: Option<Vec<String>>,
    #[serde(rename = "dependencies")]
    dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "devDependencies")]
    dev_dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "optionalDependencies")]
    optional_dependencies: Option<HashMap<String, serde_json::Value>>,
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

        let package_id = PackageId::new("example");
        let packages = provider.list_packages()?;
        assert_eq!(packages, vec![package_id.clone()]);

        let module_ids = provider.modules_for_package(&package_id)?;
        assert_eq!(module_ids.len(), 2);

        let module = provider.load_module_descriptor(&module_ids[0])?;
        assert_eq!(module.language, ModuleLanguage::TypeScript);
        Ok(())
    }
}
