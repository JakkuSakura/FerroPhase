use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use semver::{Version, VersionReq};
use toml::Value;

use crate::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use crate::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageMetadata, TargetFilter,
};
use crate::vfs::{DirEntry, FileKind, FsError, VirtualFileSystem, VirtualPath};

pub type ProviderResult<T> = Result<T, ProviderError>;

#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("package not found: {0}")]
    PackageNotFound(PackageId),
    #[error("module not found: {0}")]
    ModuleNotFound(ModuleId),
    #[error("invalid manifest at {path}: {message}")]
    InvalidManifest { path: VirtualPath, message: String },
    #[error("invalid utf8 in {path}: {source}")]
    InvalidUtf8 {
        path: VirtualPath,
        source: std::string::FromUtf8Error,
    },
    #[error(transparent)]
    Fs(#[from] FsError),
    #[error("{0}")]
    Other(String),
}

impl ProviderError {
    pub fn other(message: impl Into<String>) -> Self {
        Self::Other(message.into())
    }
}

pub trait PackageProvider: Send + Sync {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;
}

pub trait ModuleProvider: Send + Sync {
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>>;
    fn load_module(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>>;
    fn refresh(&self, id: &PackageId) -> ProviderResult<()>;
}

// -----------------------------------------------------------------------------
// Cargo package provider
// -----------------------------------------------------------------------------

pub struct CargoPackageProvider {
    vfs: Arc<dyn VirtualFileSystem>,
    workspace_root: VirtualPath,
    cache: RwLock<HashMap<PackageId, Arc<PackageDescriptor>>>,
}

impl CargoPackageProvider {
    pub fn new(vfs: Arc<dyn VirtualFileSystem>, workspace_root: VirtualPath) -> Self {
        Self {
            vfs,
            workspace_root,
            cache: RwLock::new(HashMap::new()),
        }
    }

    fn ensure_cache(&self) -> ProviderResult<()> {
        if self.cache.read().unwrap().is_empty() {
            self.refresh()?;
        }
        Ok(())
    }

    fn read_to_string(&self, path: &VirtualPath) -> ProviderResult<String> {
        let bytes = self.vfs.read(path)?;
        String::from_utf8(bytes).map_err(|source| ProviderError::InvalidUtf8 {
            path: path.clone(),
            source,
        })
    }

    fn parse_workspace_members(&self, manifest: &Value) -> ProviderResult<Vec<VirtualPath>> {
        let mut members = Vec::new();
        if let Some(workspace) = manifest.get("workspace") {
            if let Some(array) = workspace.get("members").and_then(|value| value.as_array()) {
                for item in array {
                    if let Some(member) = item.as_str() {
                        members.extend(self.expand_member_path(member)?);
                    }
                }
            }
        }
        if members.is_empty() {
            members.push(self.workspace_root.clone());
        }
        Ok(members)
    }

    fn expand_member_path(&self, spec: &str) -> ProviderResult<Vec<VirtualPath>> {
        if spec.ends_with("/*") {
            let base = spec.trim_end_matches("/*");
            let base_path = join_relative(&self.workspace_root, base);
            let mut expanded = Vec::new();
            if self.vfs.exists(&base_path) {
                for entry in self.vfs.read_dir(&base_path)? {
                    if matches!(entry.metadata.kind, FileKind::Directory) {
                        expanded.push(entry.path);
                    }
                }
            }
            Ok(expanded)
        } else {
            Ok(vec![join_relative(&self.workspace_root, spec)])
        }
    }

    fn parse_package(
        &self,
        manifest: &Value,
        manifest_path: VirtualPath,
        root: VirtualPath,
    ) -> ProviderResult<PackageDescriptor> {
        let package_table = manifest
            .get("package")
            .and_then(Value::as_table)
            .ok_or_else(|| ProviderError::InvalidManifest {
                path: manifest_path.clone(),
                message: "missing [package] section".into(),
            })?;

        let name = package_table
            .get("name")
            .and_then(Value::as_str)
            .ok_or_else(|| ProviderError::InvalidManifest {
                path: manifest_path.clone(),
                message: "package.name must be a string".into(),
            })?
            .to_string();

        let version = package_table
            .get("version")
            .and_then(Value::as_str)
            .and_then(|raw| Version::parse(raw).ok());

        let authors = package_table
            .get("authors")
            .and_then(Value::as_array)
            .map(|array| {
                array
                    .iter()
                    .filter_map(Value::as_str)
                    .map(|s| s.to_string())
                    .collect()
            })
            .unwrap_or_default();

        let dependencies = self.collect_dependencies(manifest, DependencyKind::Normal, "dependencies")?;
        let mut metadata = PackageMetadata {
            edition: package_table
                .get("edition")
                .and_then(Value::as_str)
                .map(|s| s.to_string()),
            authors,
            description: package_table
                .get("description")
                .and_then(Value::as_str)
                .map(|s| s.to_string()),
            license: package_table
                .get("license")
                .and_then(Value::as_str)
                .map(|s| s.to_string()),
            keywords: package_table
                .get("keywords")
                .and_then(Value::as_array)
                .map(|array| {
                    array
                        .iter()
                        .filter_map(Value::as_str)
                        .map(|s| s.to_string())
                        .collect()
                })
                .unwrap_or_default(),
            registry: package_table
                .get("publish")
                .and_then(Value::as_str)
                .map(|s| s.to_string()),
            features: manifest
                .get("features")
                .and_then(Value::as_table)
                .map(|table| {
                    table
                        .iter()
                        .map(|(feature, value)| {
                            let refs = value
                                .as_array()
                                .map(|array| {
                                    array
                                        .iter()
                                        .filter_map(Value::as_str)
                                        .map(|s| s.to_string())
                                        .collect()
                                })
                                .unwrap_or_default();
                            (feature.clone(), refs)
                        })
                        .collect()
                })
                .unwrap_or_default(),
            dependencies,
        };

        metadata.dependencies.extend(self.collect_dependencies(
            manifest,
            DependencyKind::Development,
            "dev-dependencies",
        )?);
        metadata.dependencies.extend(self.collect_dependencies(
            manifest,
            DependencyKind::Build,
            "build-dependencies",
        )?);

        let descriptor = PackageDescriptor {
            id: PackageId::new(&name),
            name,
            version,
            manifest_path,
            root,
            metadata,
            modules: Vec::new(),
        };
        Ok(descriptor)
    }

    fn collect_dependencies(
        &self,
        manifest: &Value,
        kind: DependencyKind,
        section: &str,
    ) -> ProviderResult<Vec<DependencyDescriptor>> {
        let mut dependencies = Vec::new();
        if let Some(table) = manifest.get(section).and_then(Value::as_table) {
            for (name, value) in table {
                let (constraint, features, optional, target) = match value {
                    Value::String(version) => (
                        VersionReq::parse(version).ok(),
                        Vec::new(),
                        false,
                        TargetFilter::default(),
                    ),
                    Value::Table(map) => {
                        let constraint = map
                            .get("version")
                            .and_then(Value::as_str)
                            .and_then(|raw| VersionReq::parse(raw).ok());
                        let features = map
                            .get("features")
                            .and_then(Value::as_array)
                            .map(|array| {
                                array
                                    .iter()
                                    .filter_map(Value::as_str)
                                    .map(|s| s.to_string())
                                    .collect()
                            })
                            .unwrap_or_default();
                        let optional = map
                            .get("optional")
                            .and_then(Value::as_bool)
                            .unwrap_or(false);
                        let cfg = map
                            .get("cfg")
                            .and_then(Value::as_str)
                            .map(|s| s.to_string());
                        let languages = map
                            .get("languages")
                            .and_then(Value::as_array)
                            .map(|array| {
                                array
                                    .iter()
                                    .filter_map(Value::as_str)
                                    .map(|s| s.to_string())
                                    .collect()
                            })
                            .unwrap_or_default();
                        (
                            constraint,
                            features,
                            optional,
                            TargetFilter { cfg, languages },
                        )
                    }
                    _ => (None, Vec::new(), false, TargetFilter::default()),
                };
                dependencies.push(DependencyDescriptor {
                    package: name.clone(),
                    constraint,
                    kind: kind.clone(),
                    features,
                    optional,
                    target,
                });
            }
        }
        Ok(dependencies)
    }
}

impl PackageProvider for CargoPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.ensure_cache()?;
        Ok(self.cache.read().unwrap().keys().cloned().collect())
    }

    fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        self.ensure_cache()?;
        self.cache
            .read()
            .unwrap()
            .get(id)
            .cloned()
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn refresh(&self) -> ProviderResult<()> {
        let manifest_path = self.workspace_root.join("Cargo.toml");
        let manifest_str = self.read_to_string(&manifest_path)?;
        let manifest_value: Value =
            toml::from_str(&manifest_str).map_err(|err| ProviderError::InvalidManifest {
                path: manifest_path.clone(),
                message: err.to_string(),
            })?;

        let member_roots = self.parse_workspace_members(&manifest_value)?;
        let mut cache = HashMap::new();
        for root in member_roots {
            let cargo_path = root.join("Cargo.toml");
            if !self.vfs.exists(&cargo_path) {
                continue;
            }
            let manifest_str = self.read_to_string(&cargo_path)?;
            let manifest_value: Value =
                toml::from_str(&manifest_str).map_err(|err| ProviderError::InvalidManifest {
                    path: cargo_path.clone(),
                    message: err.to_string(),
                })?;
            let descriptor = self.parse_package(&manifest_value, cargo_path, root.clone())?;
            cache.insert(descriptor.id.clone(), Arc::new(descriptor));
        }

        *self.cache.write().unwrap() = cache;
        Ok(())
    }
}

// -----------------------------------------------------------------------------
// Rust module provider
// -----------------------------------------------------------------------------

pub struct RustModuleProvider<P> {
    vfs: Arc<dyn VirtualFileSystem>,
    packages: Arc<P>,
    modules: RwLock<HashMap<ModuleId, Arc<ModuleDescriptor>>>,
    modules_by_package: RwLock<HashMap<PackageId, Vec<ModuleId>>>,
}

impl<P> RustModuleProvider<P>
where
    P: PackageProvider + 'static,
{
    pub fn new(vfs: Arc<dyn VirtualFileSystem>, packages: Arc<P>) -> Self {
        Self {
            vfs,
            packages,
            modules: RwLock::new(HashMap::new()),
            modules_by_package: RwLock::new(HashMap::new()),
        }
    }

    fn build_modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        let package = self.packages.load_package(id)?;
        let src_root = package.root.join("src");
        if !self.vfs.exists(&src_root) {
            return Ok(Vec::new());
        }

        let descriptors = self.walk_package_sources(&package, &src_root)?;

        let mut module_ids = Vec::new();
        let mut modules_guard = self.modules.write().unwrap();
        for descriptor in descriptors {
            module_ids.push(descriptor.id.clone());
            modules_guard.insert(descriptor.id.clone(), descriptor);
        }
        self.modules_by_package
            .write()
            .unwrap()
            .insert(id.clone(), module_ids.clone());
        Ok(module_ids)
    }

    fn walk_package_sources(
        &self,
        package: &PackageDescriptor,
        src_root: &VirtualPath,
    ) -> ProviderResult<Vec<Arc<ModuleDescriptor>>> {
        let mut results = Vec::new();
        self.walk_directory(package, src_root, src_root, &mut results)?;
        Ok(results)
    }

    fn walk_directory(
        &self,
        package: &PackageDescriptor,
        root: &VirtualPath,
        current: &VirtualPath,
        out: &mut Vec<Arc<ModuleDescriptor>>,
    ) -> ProviderResult<()> {
        for entry in self.vfs.read_dir(current)? {
            let kind = entry.metadata.kind.clone();
            match kind {
                FileKind::Directory => {
                    self.walk_directory(package, root, &entry.path, out)?;
                }
                FileKind::File => {
                    if let Some(descriptor) = self.module_from_entry(package, root, entry)? {
                        out.push(Arc::new(descriptor));
                    }
                }
                FileKind::Symlink => {}
            }
        }
        Ok(())
    }

    fn module_from_entry(
        &self,
        package: &PackageDescriptor,
        root: &VirtualPath,
        entry: DirEntry,
    ) -> ProviderResult<Option<ModuleDescriptor>> {
        let Some(relative) = relative_segments(root, &entry.path) else {
            return Ok(None);
        };

        if relative.is_empty() {
            return Ok(None);
        }

        let filename = relative.last().unwrap();
        if !filename.ends_with(".rs") {
            return Ok(None);
        }

        let mut module_path = relative[..relative.len() - 1].to_vec();
        let stem = filename.trim_end_matches(".rs");
        if stem != "mod" {
            module_path.push(stem.to_string());
        }
        let module_id = make_module_id(&package.id, &module_path);

        let descriptor = ModuleDescriptor {
            id: module_id,
            package: package.id.clone(),
            language: ModuleLanguage::Rust,
            module_path,
            source: entry.path,
            exports: Vec::new(),
            requires_features: Vec::new(),
        };
        Ok(Some(descriptor))
    }
}

impl<P> ModuleProvider for RustModuleProvider<P>
where
    P: PackageProvider + 'static,
{
    fn modules_for_package(&self, id: &PackageId) -> ProviderResult<Vec<ModuleId>> {
        if let Some(mods) = self.modules_by_package.read().unwrap().get(id) {
            return Ok(mods.clone());
        }
        self.build_modules_for_package(id)
    }

    fn load_module(&self, id: &ModuleId) -> ProviderResult<Arc<ModuleDescriptor>> {
        if let Some(descriptor) = self.modules.read().unwrap().get(id) {
            return Ok(descriptor.clone());
        }
        // Module cache miss – try to locate via package iteration.
        for package_id in self.packages.list_packages()? {
            let modules = self.modules_for_package(&package_id)?;
            if modules.iter().any(|candidate| candidate == id) {
                return self
                    .modules
                    .read()
                    .unwrap()
                    .get(id)
                    .cloned()
                    .ok_or_else(|| ProviderError::ModuleNotFound(id.clone()));
            }
        }
        Err(ProviderError::ModuleNotFound(id.clone()))
    }

    fn refresh(&self, id: &PackageId) -> ProviderResult<()> {
        self.build_modules_for_package(id).map(|_| ())
    }
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

fn join_relative(base: &VirtualPath, relative: &str) -> VirtualPath {
    let mut current = base.clone();
    if relative.is_empty() {
        return current;
    }
    for segment in relative.split('/') {
        if segment.is_empty() {
            continue;
        }
        current = current.join(segment);
    }
    current
}

fn relative_segments(base: &VirtualPath, path: &VirtualPath) -> Option<Vec<String>> {
    let base_segments = base.segments();
    let candidate = path.segments();
    if candidate.len() < base_segments.len() {
        return None;
    }
    if !candidate.starts_with(base_segments) {
        return None;
    }
    Some(candidate[base_segments.len()..].to_vec())
}

fn make_module_id(package: &PackageId, module_path: &[String]) -> ModuleId {
    if module_path.is_empty() {
        return ModuleId::new(package.as_str());
    }
    let mut identifier = String::from(package.as_str());
    identifier.push_str("::");
    identifier.push_str(&module_path.join("::"));
    ModuleId::new(identifier)
}
