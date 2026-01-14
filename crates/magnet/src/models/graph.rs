use crate::models::{DependencyModel, ManifestModel, PackageModel, WorkspaceModel};
use crate::registry::{RegistryClient, RegistryOptions};
use eyre::Result;
use fp_core::package::DependencyDescriptor;
use fp_core::package::provider::{ModuleSource, PackageProvider};
use fp_typescript::TypeScriptPackageProvider;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;
#[cfg(feature = "cargo-fallback")]
use std::process::Command;
use std::sync::Arc;
use tracing::info;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageGraph {
    pub schema_version: u32,
    pub root: PathBuf,
    pub packages: Vec<PackageNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selected_package: Option<String>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub build_options: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageNode {
    pub name: String,
    pub version: String,
    pub root: PathBuf,
    pub manifest_path: PathBuf,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
    pub module_roots: Vec<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub entry: Option<PathBuf>,
    pub dependencies: Vec<DependencyEdge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyEdge {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rev: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct PackageGraphOptions {
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub include_dependencies: bool,
    pub include_dev_dependencies: bool,
    pub include_build_dependencies: bool,
    pub cargo_fetch: bool,
    pub resolve_registry: bool,
    pub allow_multiple_versions: bool,
}

impl Default for PackageGraphOptions {
    fn default() -> Self {
        Self {
            offline: false,
            cache_dir: None,
            include_dependencies: true,
            include_dev_dependencies: false,
            include_build_dependencies: false,
            cargo_fetch: true,
            resolve_registry: true,
            allow_multiple_versions: false,
        }
    }
}

impl PackageGraph {
    pub fn from_manifest(manifest: &ManifestModel) -> Result<Self> {
        Self::from_manifest_with_options(manifest, &PackageGraphOptions::default())
    }

    pub fn from_manifest_with_options(
        manifest: &ManifestModel,
        options: &PackageGraphOptions,
    ) -> Result<Self> {
        let root = manifest_root_path(manifest);
        let packages = manifest.list_packages()?;
        let registry = if options.resolve_registry {
            Some(RegistryClient::new(RegistryOptions {
                offline: options.offline,
                cache_dir: resolve_cache_dir(options),
            })?)
        } else {
            None
        };
        let packages = packages
            .into_iter()
            .map(|package| package_to_node(package, registry.as_ref(), options))
            .collect::<Result<Vec<_>>>()?;

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_path(path: &Path) -> Result<Self> {
        Self::from_path_with_options(path, &PackageGraphOptions::default())
    }

    pub fn from_path_with_options(path: &Path, options: &PackageGraphOptions) -> Result<Self> {
        let started_at = Instant::now();
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        if path.is_dir() {
            let magnet_path = path.join("Magnet.toml");
            let cargo_path = path.join("Cargo.toml");
            let pyproject_path = path.join("pyproject.toml");
            let ts_path = path.join("package.json");
            if magnet_path.exists() {
                info!(
                    "graph: loading Magnet manifest from {}",
                    magnet_path.display()
                );
                let manifest = ManifestModel::from_dir(&path)?;
                let graph = Self::from_manifest_with_options(&manifest, options)?;
                info!(
                    "graph: loaded {} package(s) in {:.2?}",
                    graph.packages.len(),
                    started_at.elapsed()
                );
                return Ok(graph);
            }
            if cargo_path.exists() {
                info!(
                    "graph: loading Cargo manifest from {}",
                    cargo_path.display()
                );
                let graph = Self::from_cargo_manifest_with_options(&cargo_path, options)?;
                info!(
                    "graph: loaded {} package(s) in {:.2?}",
                    graph.packages.len(),
                    started_at.elapsed()
                );
                return Ok(graph);
            }
            if pyproject_path.exists() {
                info!(
                    "graph: loading Python manifest from {}",
                    pyproject_path.display()
                );
                return Self::from_python_manifest(&pyproject_path);
            }
            if ts_path.exists() {
                info!(
                    "graph: loading package.json manifest from {}",
                    ts_path.display()
                );
                return Self::from_package_json(&ts_path);
            }
            return Err(eyre::eyre!(
                "No Magnet.toml, Cargo.toml, or package.json found in {}",
                path.display()
            ));
        }

        let file_name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        match file_name {
            "Magnet.toml" => {
                info!("graph: loading Magnet manifest from {}", path.display());
                let root = path.parent().unwrap_or(Path::new("."));
                let manifest = ManifestModel::from_dir(root)?;
                let graph = Self::from_manifest(&manifest)?;
                info!(
                    "graph: loaded {} package(s) in {:.2?}",
                    graph.packages.len(),
                    started_at.elapsed()
                );
                Ok(graph)
            }
            "Cargo.toml" => {
                info!("graph: loading Cargo manifest from {}", path.display());
                let graph = Self::from_cargo_manifest_with_options(&path, options)?;
                info!(
                    "graph: loaded {} package(s) in {:.2?}",
                    graph.packages.len(),
                    started_at.elapsed()
                );
                Ok(graph)
            }
            "pyproject.toml" => Self::from_python_manifest(&path),
            "package.json" => Self::from_package_json(&path),
            _ => Err(eyre::eyre!(
                "Unsupported manifest path {}; expected Magnet.toml, Cargo.toml, or package.json",
                path.display()
            )),
        }
    }

    pub fn from_cargo_manifest(manifest_path: &Path) -> Result<Self> {
        Self::from_cargo_manifest_with_options(manifest_path, &PackageGraphOptions::default())
    }

    pub fn from_cargo_manifest_with_options(
        manifest_path: &Path,
        options: &PackageGraphOptions,
    ) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("Cargo manifest has no parent directory"))?
            .to_path_buf();
        info!("graph: initializing Cargo workspace at {}", root.display());
        let registry = if options.resolve_registry {
            info!("graph: initializing registry client");
            Some(RegistryClient::new(RegistryOptions {
                offline: options.offline,
                cache_dir: resolve_cache_dir(options),
            })?)
        } else {
            None
        };
        #[cfg(feature = "cargo-fallback")]
        {
            if options.cargo_fetch && !options.offline {
                info!("graph: prefetching Cargo dependencies");
                prefetch_cargo_dependencies(manifest_path)?;
            }
        }
        let mut resolver = CargoResolver::new(&root, options, registry)?;
        resolver.load_root(manifest_path)?;
        let packages = resolver.resolve()?;

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_typescript_manifest(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("package.json has no parent directory"))?
            .to_path_buf();
        let provider = Arc::new(TypeScriptPackageProvider::new(root.clone()));
        provider
            .refresh()
            .map_err(|err| eyre::eyre!("Failed to load TypeScript package: {err}"))?;

        let mut packages = Vec::new();
        for package_id in provider
            .list_packages()
            .map_err(|err| eyre::eyre!("Failed to list TypeScript packages: {err}"))?
        {
            let descriptor = provider
                .load_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to load package {package_id}: {err}"))?;
            let _module_ids = provider
                .modules_for_package(&package_id)
                .map_err(|err| eyre::eyre!("Failed to list modules for {package_id}: {err}"))?;
            let module_roots = default_module_roots(&descriptor.root.to_path_buf());
            let entry = detect_entry(&descriptor.root.to_path_buf(), &["ts", "tsx"]);
            let dependencies = descriptor
                .metadata
                .dependencies
                .iter()
                .map(dependency_descriptor_to_edge)
                .collect();

            packages.push(PackageNode {
                name: descriptor.name.clone(),
                version: descriptor
                    .version
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "0.0.0".to_string()),
                root: descriptor.root.to_path_buf(),
                manifest_path: descriptor.manifest_path.to_path_buf(),
                language: Some("typescript".to_string()),
                module_roots,
                entry,
                dependencies,
            });
        }

        Ok(Self {
            schema_version: 1,
            root,
            packages,
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_package_json(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("package.json has no parent directory"))?
            .to_path_buf();
        if has_typescript_sources(&root) || root.join("tsconfig.json").exists() {
            return Self::from_typescript_manifest(manifest_path);
        }

        let manifest = read_package_json(manifest_path)?;
        let dependencies = flatten_package_json_dependencies(&manifest);
        let node = PackageNode {
            name: manifest.name,
            version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
            root: root.clone(),
            manifest_path: manifest_path.to_path_buf(),
            language: Some("javascript".to_string()),
            module_roots: default_module_roots(&root),
            entry: detect_entry(&root, &["js", "mjs", "cjs"]),
            dependencies,
        };

        Ok(Self {
            schema_version: 1,
            root,
            packages: vec![node],
            selected_package: None,
            build_options: HashMap::new(),
        })
    }

    pub fn from_python_manifest(manifest_path: &Path) -> Result<Self> {
        let root = manifest_path
            .parent()
            .ok_or_else(|| eyre::eyre!("pyproject.toml has no parent directory"))?
            .to_path_buf();
        let manifest = read_pyproject(manifest_path)?;
        let dependencies = manifest
            .dependencies
            .into_iter()
            .map(|dep| DependencyEdge {
                name: dep.name,
                package: None,
                version: dep.version,
                resolved_version: None,
                checksum: None,
                source: None,
                path: None,
                git: None,
                rev: None,
                branch: None,
                tag: None,
                workspace: None,
                optional: None,
                features: None,
            })
            .collect();

        let node = PackageNode {
            name: manifest.name,
            version: manifest.version.unwrap_or_else(|| "0.0.0".to_string()),
            root: root.clone(),
            manifest_path: manifest_path.to_path_buf(),
            language: Some("python".to_string()),
            module_roots: default_module_roots(&root),
            entry: detect_entry(&root, &["py"]),
            dependencies,
        };

        Ok(Self {
            schema_version: 1,
            root,
            packages: vec![node],
            selected_package: None,
            build_options: HashMap::new(),
        })
    }
}

#[cfg(feature = "cargo-fallback")]
fn prefetch_cargo_dependencies(manifest_path: &Path) -> Result<()> {
    let status = Command::new("cargo")
        .arg("fetch")
        .arg("--manifest-path")
        .arg(manifest_path)
        .status()
        .map_err(|err| eyre::eyre!("Failed to execute cargo fetch: {err}"))?;
    if !status.success() {
        return Err(eyre::eyre!(
            "cargo fetch failed with status {}",
            status.code().unwrap_or(-1)
        ));
    }
    Ok(())
}

fn resolve_cache_dir(options: &PackageGraphOptions) -> Option<PathBuf> {
    if let Some(cache_dir) = options.cache_dir.clone() {
        return Some(cache_dir);
    }
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Some(PathBuf::from(cache_dir));
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .map(PathBuf::from)?;
    Some(home.join(".cache").join("magnet"))
}

fn package_to_node(
    package: PackageModel,
    registry: Option<&RegistryClient>,
    options: &PackageGraphOptions,
) -> Result<PackageNode> {
    let module_root = package.root_path.join("src");
    let entry = {
        let path = module_root.join("main.fp");
        if path.exists() { Some(path) } else { None }
    };

    let mut dep_map = std::collections::HashMap::new();
    if options.include_dependencies {
        for (name, dep) in package.dependencies {
            dep_map.entry(name).or_insert(dep);
        }
    }
    if options.include_dev_dependencies {
        for (name, dep) in package.dev_dependencies {
            dep_map.entry(name).or_insert(dep);
        }
    }
    if options.include_build_dependencies {
        for (name, dep) in package.build_dependencies {
            dep_map.entry(name).or_insert(dep);
        }
    }

    Ok(PackageNode {
        name: package.name,
        version: package.version,
        root: package.root_path.clone(),
        manifest_path: package.source_path,
        language: None,
        module_roots: vec![module_root],
        entry,
        dependencies: dep_map
            .into_iter()
            .map(|(name, dep)| dependency_to_edge(name, dep, registry))
            .collect::<Result<Vec<_>>>()?,
    })
}

struct CargoResolver {
    root: PathBuf,
    options: PackageGraphOptions,
    registry: Option<RegistryClient>,
    workspace: Option<WorkspaceModel>,
    roots: Vec<PathBuf>,
    seen: HashSet<String>,
    packages: Vec<PackageNode>,
}

impl CargoResolver {
    fn new(
        root: &Path,
        options: &PackageGraphOptions,
        registry: Option<RegistryClient>,
    ) -> Result<Self> {
        Ok(Self {
            root: root.to_path_buf(),
            options: options.clone(),
            registry,
            workspace: None,
            roots: Vec::new(),
            seen: HashSet::new(),
            packages: Vec::new(),
        })
    }

    fn load_root(&mut self, manifest_path: &Path) -> Result<()> {
        let manifest = crate::configs::CargoManifestConfig::from_path(manifest_path)?;
        if manifest.workspace.is_some() {
            let workspace = WorkspaceModel::from_dir(&self.root)?;
            let mut members = workspace.list_members()?;
            info!(
                "graph: workspace members detected: {}",
                members.len()
            );
            self.workspace = Some(workspace);
            self.roots.append(&mut members);
        }
        if manifest.package.is_some() {
            info!("graph: root package detected");
            self.roots.push(self.root.clone());
        }
        if self.roots.is_empty() {
            return Err(eyre::eyre!(
                "Cargo.toml has neither [workspace] nor [package] sections"
            ));
        }
        self.roots.sort();
        self.roots.dedup();
        Ok(())
    }

    fn resolve(&mut self) -> Result<Vec<PackageNode>> {
        let roots = self.roots.clone();
        for root in roots {
            self.resolve_package_dir(&root)?;
        }
        Ok(std::mem::take(&mut self.packages))
    }

    fn resolve_package_dir(&mut self, dir: &Path) -> Result<()> {
        info!("graph: resolving package at {}", dir.display());
        let mut package = PackageModel::from_dir(dir)?;
        if let Some(workspace) = &self.workspace {
            apply_workspace_dependencies(&mut package, workspace)?;
        }

        let key = self.package_key(&package);
        if !self.seen.insert(key) {
            return Ok(());
        }

        let node = package_to_node(package.clone(), self.registry.as_ref(), &self.options)?;
        self.packages.push(node);

        let deps = collect_dependencies(&package, &self.options);
        for (name, dep) in deps {
            self.resolve_dependency(&package.root_path, &name, dep)?;
        }

        Ok(())
    }

    fn resolve_dependency(
        &mut self,
        package_root: &Path,
        name: &str,
        dep: DependencyModel,
    ) -> Result<()> {
        if let Some(path) = dep.path.as_ref() {
            let base = if dep.workspace.unwrap_or(false) {
                self.workspace
                    .as_ref()
                    .map(|ws| ws.root_path.as_path())
                    .unwrap_or(package_root)
            } else {
                package_root
            };
            let path = if path.is_absolute() {
                path.to_path_buf()
            } else {
                base.join(path)
            };
            info!("graph: resolving path dependency {}", path.display());
            return self.resolve_package_dir(&path);
        }

        if dep.git.is_some() {
            info!("graph: skipping git dependency {}", name);
            return Ok(());
        }

        let Some(registry) = self.registry.as_ref() else {
            return Ok(());
        };
        let resolved_name = dep.package.as_deref().unwrap_or(name);
        let Some(version) = dep.version.as_deref() else {
            return Ok(());
        };
        info!("graph: resolving registry dependency {} {}", resolved_name, version);
        let resolved = registry.resolve(resolved_name, Some(version))?;
        self.resolve_package_dir(&resolved.root)
    }

    fn package_key(&self, package: &PackageModel) -> String {
        if self.options.allow_multiple_versions {
            package.source_path.to_string_lossy().to_string()
        } else {
            package.name.clone()
        }
    }
}

fn collect_dependencies(
    package: &PackageModel,
    options: &PackageGraphOptions,
) -> HashMap<String, DependencyModel> {
    let mut dep_map = HashMap::new();
    if options.include_dependencies {
        for (name, dep) in package.dependencies.clone() {
            dep_map.entry(name).or_insert(dep);
        }
    }
    if options.include_dev_dependencies {
        for (name, dep) in package.dev_dependencies.clone() {
            dep_map.entry(name).or_insert(dep);
        }
    }
    if options.include_build_dependencies {
        for (name, dep) in package.build_dependencies.clone() {
            dep_map.entry(name).or_insert(dep);
        }
    }
    dep_map
}

fn apply_workspace_dependencies(
    package: &mut PackageModel,
    workspace: &WorkspaceModel,
) -> Result<()> {
    apply_workspace_dependency_map(&mut package.dependencies, workspace)?;
    apply_workspace_dependency_map(&mut package.dev_dependencies, workspace)?;
    apply_workspace_dependency_map(&mut package.build_dependencies, workspace)?;
    Ok(())
}

fn apply_workspace_dependency_map(
    deps: &mut HashMap<String, DependencyModel>,
    workspace: &WorkspaceModel,
) -> Result<()> {
    for (name, dep) in deps.clone() {
        if dep.workspace.unwrap_or(false) {
            let Some(base) = workspace.dependencies.get(&name) else {
                return Err(eyre::eyre!(
                    "workspace dependency '{}' not found in workspace dependencies",
                    name
                ));
            };
            deps.insert(name, merge_dependency(base, &dep));
        }
    }
    Ok(())
}

fn merge_dependency(base: &DependencyModel, overlay: &DependencyModel) -> DependencyModel {
    let mut out = base.clone();
    if overlay.version.is_some() {
        out.version = overlay.version.clone();
    }
    if overlay.path.is_some() {
        out.path = overlay.path.clone();
    }
    if overlay.git.is_some() {
        out.git = overlay.git.clone();
    }
    if overlay.branch.is_some() {
        out.branch = overlay.branch.clone();
    }
    if overlay.tag.is_some() {
        out.tag = overlay.tag.clone();
    }
    if overlay.rev.is_some() {
        out.rev = overlay.rev.clone();
    }
    if overlay.features.is_some() {
        out.features = overlay.features.clone();
    }
    if overlay.default_features.is_some() {
        out.default_features = overlay.default_features;
    }
    if overlay.optional.is_some() {
        out.optional = overlay.optional;
    }
    if overlay.package.is_some() {
        out.package = overlay.package.clone();
    }
    if overlay.registry.is_some() {
        out.registry = overlay.registry.clone();
    }
    if overlay.workspace.is_some() {
        out.workspace = overlay.workspace;
    }
    out
}

fn dependency_to_edge(
    name: String,
    dep: DependencyModel,
    registry: Option<&RegistryClient>,
) -> Result<DependencyEdge> {
    let mut resolved_version = None;
    let mut checksum = None;
    let mut source = None;
    let mut path = dep.path.clone();

    let resolved_name = dep.package.as_deref().unwrap_or(name.as_str());
    if path.is_none() && dep.git.is_none() && dep.version.is_some() {
        if let Some(registry_name) = dep.registry.as_deref() {
            if registry_name != "crates-io" && registry_name != "crates.io" {
                return Err(eyre::eyre!("unsupported registry '{}'", registry_name));
            }
        }
        if let Some(registry) = registry {
            let resolved = registry.resolve(resolved_name, dep.version.as_deref())?;
            resolved_version = Some(resolved.version.clone());
            checksum = resolved.checksum.clone();
            source = Some("registry".to_string());
            path = Some(resolved.root);
        }
    }

    Ok(DependencyEdge {
        name,
        package: dep.package,
        version: dep.version,
        resolved_version,
        checksum,
        source,
        path,
        git: dep.git,
        rev: dep.rev,
        branch: dep.branch,
        tag: dep.tag,
        workspace: dep.workspace,
        optional: dep.optional,
        features: dep.features,
    })
}

fn dependency_descriptor_to_edge(dep: &DependencyDescriptor) -> DependencyEdge {
    DependencyEdge {
        name: dep.package.clone(),
        package: None,
        version: dep.constraint.as_ref().map(|v| v.to_string()),
        resolved_version: None,
        checksum: None,
        source: None,
        path: None,
        git: None,
        rev: None,
        branch: None,
        tag: None,
        workspace: None,
        optional: Some(dep.optional),
        features: Some(dep.features.clone()),
    }
}

#[derive(Debug, Deserialize)]
struct PackageJsonManifest {
    name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "devDependencies", default)]
    dev_dependencies: Option<HashMap<String, serde_json::Value>>,
    #[serde(rename = "optionalDependencies", default)]
    optional_dependencies: Option<HashMap<String, serde_json::Value>>,
}

fn read_package_json(path: &Path) -> Result<PackageJsonManifest> {
    let contents =
        std::fs::read_to_string(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let manifest: PackageJsonManifest =
        serde_json::from_str(&contents).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    Ok(manifest)
}

fn flatten_package_json_dependencies(manifest: &PackageJsonManifest) -> Vec<DependencyEdge> {
    let mut deps = Vec::new();
    deps.extend(dependencies_from_map(&manifest.dependencies));
    deps.extend(dependencies_from_map(&manifest.dev_dependencies));
    deps.extend(dependencies_from_map(&manifest.optional_dependencies));
    deps
}

fn dependencies_from_map(map: &Option<HashMap<String, serde_json::Value>>) -> Vec<DependencyEdge> {
    let mut deps = Vec::new();
    if let Some(values) = map {
        for (name, value) in values {
            let version = value.as_str().map(|v| v.to_string()).or_else(|| {
                value
                    .as_object()
                    .and_then(|obj| obj.get("version").and_then(|v| v.as_str()))
                    .map(|v| v.to_string())
            });
            deps.push(DependencyEdge {
                name: name.clone(),
                package: None,
                version,
                resolved_version: None,
                checksum: None,
                source: None,
                path: None,
                git: None,
                rev: None,
                branch: None,
                tag: None,
                workspace: None,
                optional: None,
                features: None,
            });
        }
    }
    deps
}

#[derive(Debug)]
struct PyProjectManifest {
    name: String,
    version: Option<String>,
    dependencies: Vec<PyDependency>,
}

#[derive(Debug)]
struct PyDependency {
    name: String,
    version: Option<String>,
}

fn read_pyproject(path: &Path) -> Result<PyProjectManifest> {
    let contents =
        std::fs::read_to_string(path).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let value: toml::Value =
        toml::from_str(&contents).map_err(|err| eyre::eyre!("{}: {err}", path.display()))?;
    let project = value
        .get("project")
        .and_then(|p| p.as_table())
        .ok_or_else(|| eyre::eyre!("{}: missing [project] table", path.display()))?;
    let name = project
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or_else(|| eyre::eyre!("{}: missing project.name", path.display()))?
        .to_string();
    let version = project
        .get("version")
        .and_then(|v| v.as_str())
        .map(|v| v.to_string());
    let dependencies = project
        .get("dependencies")
        .and_then(|v| v.as_array())
        .map(parse_python_dependencies)
        .unwrap_or_default();
    Ok(PyProjectManifest {
        name,
        version,
        dependencies,
    })
}

fn parse_python_dependencies(values: &Vec<toml::Value>) -> Vec<PyDependency> {
    values
        .iter()
        .filter_map(|value| value.as_str())
        .map(|raw| {
            let mut name = String::new();
            let mut version = String::new();
            for ch in raw.chars() {
                if name.is_empty() {
                    if ch.is_whitespace() || is_version_marker(ch) {
                        continue;
                    }
                    if is_name_char(ch) {
                        name.push(ch);
                    }
                    continue;
                }
                if version.is_empty() {
                    if is_name_char(ch) {
                        name.push(ch);
                    } else {
                        version.push(ch);
                    }
                } else {
                    version.push(ch);
                }
            }
            let name = name.trim().to_string();
            let version = if version.trim().is_empty() {
                None
            } else {
                Some(version.trim().to_string())
            };
            PyDependency { name, version }
        })
        .filter(|dep| !dep.name.is_empty())
        .collect()
}

fn is_version_marker(ch: char) -> bool {
    matches!(ch, '<' | '>' | '=' | '!' | '~' | '^')
}

fn is_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
}

fn manifest_root_path(manifest: &ManifestModel) -> PathBuf {
    match manifest {
        ManifestModel::Nexus(nexus) => nexus.root_path.clone(),
        ManifestModel::Workspace(workspace) => workspace.root_path.clone(),
        ManifestModel::Package(package) => package.root_path.clone(),
    }
}


fn default_module_roots(root: &Path) -> Vec<PathBuf> {
    let src = root.join("src");
    if src.is_dir() {
        vec![src]
    } else {
        vec![root.to_path_buf()]
    }
}

fn detect_entry(root: &Path, extensions: &[&str]) -> Option<PathBuf> {
    let src = root.join("src");
    let candidates = if src.is_dir() {
        vec![src]
    } else {
        vec![root.to_path_buf()]
    };
    for base in candidates {
        for stem in ["main", "index", "lib"] {
            for ext in extensions {
                let candidate = base.join(format!("{}.{}", stem, ext));
                if candidate.exists() {
                    return Some(candidate);
                }
            }
        }
    }
    None
}

fn has_typescript_sources(root: &Path) -> bool {
    let src = root.join("src");
    if !src.exists() {
        return false;
    }
    let mut stack = vec![src];
    while let Some(dir) = stack.pop() {
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    if path.file_name().and_then(|n| n.to_str()) == Some("node_modules") {
                        continue;
                    }
                    stack.push(path);
                } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                    if ext == "ts" || ext == "tsx" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::ManifestModel;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn build_graph_from_package_manifest() -> Result<()> {
        let temp = tempdir()?;
        let package_dir = temp.path();
        fs::create_dir_all(package_dir.join("src"))?;
        fs::write(
            package_dir.join("Magnet.toml"),
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
        )?;

        let manifest = ManifestModel::from_dir(package_dir)?;
        let graph = PackageGraph::from_manifest(&manifest)?;

        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_cargo_manifest() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("Cargo.toml"),
            r#"[package]
name = "cargo-demo"
version = "0.1.0"
edition = "2021"
"#,
        )?;
        fs::write(root.join("src").join("lib.rs"), "pub fn demo() {}")?;

        let graph = PackageGraph::from_cargo_manifest(&root.join("Cargo.toml"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "cargo-demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_package_json() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("package.json"),
            r#"{"name": "ts-demo", "version": "1.0.0"}"#,
        )?;
        fs::write(root.join("src").join("index.ts"), "export const value = 1;")?;

        let graph = PackageGraph::from_typescript_manifest(&root.join("package.json"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "ts-demo");
        Ok(())
    }

    #[test]
    fn build_graph_from_javascript_package_json() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("package.json"),
            r#"{"name": "js-demo", "version": "1.0.0"}"#,
        )?;
        fs::write(root.join("src").join("index.js"), "console.log('demo');")?;

        let graph = PackageGraph::from_package_json(&root.join("package.json"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "js-demo");
        assert_eq!(graph.packages[0].language.as_deref(), Some("javascript"));
        Ok(())
    }

    #[test]
    fn build_graph_from_pyproject() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::create_dir_all(root.join("src"))?;
        fs::write(
            root.join("pyproject.toml"),
            r#"[project]
name = "py-demo"
version = "0.1.0"
dependencies = ["requests>=2.0"]
"#,
        )?;
        fs::write(root.join("src").join("main.py"), "print('demo')")?;

        let graph = PackageGraph::from_python_manifest(&root.join("pyproject.toml"))?;
        assert_eq!(graph.packages.len(), 1);
        assert_eq!(graph.packages[0].name, "py-demo");
        assert_eq!(graph.packages[0].language.as_deref(), Some("python"));
        Ok(())
    }

    #[test]
    fn build_graph_from_magnet_workspace_with_dependencies() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::write(
            root.join("Magnet.toml"),
            r#"[workspace]
members = ["crates/*"]
"#,
        )?;

        let crates_dir = root.join("crates");
        fs::create_dir_all(&crates_dir)?;

        let shared_dir = crates_dir.join("shared");
        fs::create_dir_all(shared_dir.join("src"))?;
        fs::write(
            shared_dir.join("Magnet.toml"),
            r#"[package]
name = "shared"
version = "0.1.0"
"#,
        )?;
        fs::write(shared_dir.join("src").join("lib.fp"), "pub fn shared() {}")?;

        let app_dir = crates_dir.join("app");
        fs::create_dir_all(app_dir.join("src"))?;
        fs::write(
            app_dir.join("Magnet.toml"),
            r#"[package]
name = "app"
version = "0.1.0"

[dependencies]
shared = { path = "../shared" }
"#,
        )?;
        fs::write(app_dir.join("src").join("main.fp"), "fn main() {}")?;

        let graph = PackageGraph::from_path(root)?;
        let app = graph
            .packages
            .iter()
            .find(|pkg| pkg.name == "app")
            .expect("missing app package");
        assert!(app.dependencies.iter().any(|dep| dep.name == "shared"));
        Ok(())
    }
}
