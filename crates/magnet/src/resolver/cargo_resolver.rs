use crate::configs::CargoManifestConfig;
use crate::models::{
    dependency_to_edge, DependencyModel, LockIndex, PackageGraphOptions, PackageModel, PackageNode,
    WorkspaceModel, REGISTRY_SOURCE,
};
use crate::registry::ResolvedCrate;
use crate::resolver::{target::TargetContext, RegistryLoaderHandle};
use eyre::Result;
use semver::VersionReq;
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::task::JoinSet;
use tracing::{info, warn};

pub struct CargoResolver {
    root: PathBuf,
    options: PackageGraphOptions,
    registry: Option<RegistryLoaderHandle>,
    lock_index: Option<Arc<LockIndex>>,
    target: TargetContext,
    workspace: Option<WorkspaceModel>,
    roots: Vec<PathBuf>,
    seen: HashSet<String>,
    packages: Vec<PackageNode>,
    registry_roots: HashMap<PathBuf, ResolvedCrate>,
}

impl CargoResolver {
    pub fn new(
        root: &Path,
        options: &PackageGraphOptions,
        registry: Option<RegistryLoaderHandle>,
        lock_index: Option<LockIndex>,
        target: TargetContext,
    ) -> Result<Self> {
        Ok(Self {
            root: root.to_path_buf(),
            options: options.clone(),
            registry,
            lock_index: lock_index.map(Arc::new),
            target,
            workspace: None,
            roots: Vec::new(),
            seen: HashSet::new(),
            packages: Vec::new(),
            registry_roots: HashMap::new(),
        })
    }

    pub fn load_root(&mut self, manifest_path: &Path) -> Result<()> {
        let manifest = CargoManifestConfig::from_path(manifest_path)?;
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

    pub async fn resolve_async(&mut self) -> Result<Vec<PackageNode>> {
        let mut queue: VecDeque<PathBuf> = self.roots.clone().into();
        while let Some(dir) = queue.pop_front() {
            let Some((package, deps)) = self.resolve_package_dir_sync(&dir)? else {
                continue;
            };
            let mut resolved_registry = HashMap::new();
            if let Some(registry) = self.registry.as_ref() {
                let mut join_set = JoinSet::new();
                for (name, dep) in deps.registry {
                    let registry = registry.clone();
                    let lock_index = self.lock_index.clone();
                    join_set.spawn(async move {
                        resolve_registry_dependency(registry, lock_index, name, dep).await
                    });
                }
                while let Some(result) = join_set.join_next().await {
                    match result {
                        Ok(Ok(Some(resolved))) => {
                            queue.push_back(resolved.resolved.root.clone());
                            resolved_registry.insert(resolved.name, resolved.resolved);
                        }
                        Ok(Ok(None)) => {}
                        Ok(Err(err)) => {
                            tracing::warn!("graph: registry resolution failed: {}", err);
                        }
                        Err(err) => {
                            tracing::warn!("graph: registry task failed: {}", err);
                        }
                    }
                }
            }

            for path in deps.paths {
                queue.push_back(path);
            }

            for resolved in resolved_registry.values() {
                self.registry_roots
                    .insert(resolved.root.clone(), resolved.clone());
            }
            let node = package_to_node_with_resolved(
                package,
                resolved_registry,
                &self.options,
                &self.registry_roots,
                &self.target,
            )?;
            self.packages.push(node);
        }
        Ok(std::mem::take(&mut self.packages))
    }

    fn resolve_package_dir_sync(&mut self, dir: &Path) -> Result<Option<(PackageModel, PackageDeps)>> {
        info!("graph: resolving package at {}", dir.display());
        let mut package = match PackageModel::from_dir(dir) {
            Ok(package) => package,
            Err(err) => {
                if dir == self.root.as_path() {
                    return Err(err.wrap_err(format!(
                        "graph: failed to resolve root package at {}",
                        dir.display()
                    )));
                }
                tracing::warn!(
                    "graph: skipping package at {}: {}",
                    dir.display(),
                    err
                );
                return Ok(None);
            }
        };
        if let Some(workspace) = &self.workspace {
            apply_workspace_dependencies(&mut package, workspace)?;
        }

        let key = self.package_key(&package);
        if !self.seen.insert(key) {
            return Ok(None);
        }

        let deps = collect_dependencies(&package, &self.options, &self.target)?;
        let resolved = split_dependencies(&package, deps, &self.workspace)?;
        Ok(Some((package, resolved)))
    }

    fn package_key(&self, package: &PackageModel) -> String {
        if self.options.allow_multiple_versions {
            package.source_path.to_string_lossy().to_string()
        } else {
            package.name.clone()
        }
    }
}

struct PackageDeps {
    paths: Vec<PathBuf>,
    registry: Vec<(String, DependencyModel)>,
}

fn split_dependencies(
    package: &PackageModel,
    deps: HashMap<String, DependencyModel>,
    workspace: &Option<WorkspaceModel>,
) -> Result<PackageDeps> {
    let mut paths = Vec::new();
    let mut registry = Vec::new();
    for (name, dep) in deps {
        if let Some(path) = dep.path.as_ref() {
            let base = if dep.workspace.unwrap_or(false) {
                workspace
                    .as_ref()
                    .map(|ws| ws.root_path.as_path())
                    .unwrap_or(package.root_path.as_path())
            } else {
                package.root_path.as_path()
            };
            let path = if path.is_absolute() {
                path.to_path_buf()
            } else {
                base.join(path)
            };
            if !path.exists() {
                warn!(
                    "graph: skipping path dependency {}, path does not exist",
                    path.display()
                );
                continue;
            }
            let manifest_path = if path.join("Magnet.toml").exists() {
                Some(path.join("Magnet.toml"))
            } else if path.join("Cargo.toml").exists() {
                Some(path.join("Cargo.toml"))
            } else {
                None
            };
            if manifest_path.is_none() {
                warn!(
                    "graph: skipping path dependency {}, missing Cargo.toml or Magnet.toml",
                    path.display()
                );
                continue;
            }
            info!("graph: resolving path dependency {}", path.display());
            paths.push(path);
            continue;
        }

        if dep.git.is_some() {
            info!("graph: skipping git dependency {}", name);
            continue;
        }

        registry.push((name, dep));
    }
    Ok(PackageDeps { paths, registry })
}

struct ResolvedRegistryDependency {
    name: String,
    resolved: ResolvedCrate,
}

async fn resolve_registry_dependency(
    registry: RegistryLoaderHandle,
    lock_index: Option<Arc<LockIndex>>,
    name: String,
    dep: DependencyModel,
) -> Result<Option<ResolvedRegistryDependency>> {
    if let Some(registry_name) = dep.registry.as_deref() {
        if registry_name != "crates-io" && registry_name != "crates.io" {
            tracing::warn!("graph: skipping unsupported registry '{}'", registry_name);
            return Ok(None);
        }
    }
    let Some(version) = dep.version.as_deref() else {
        return Ok(None);
    };
    let resolved_name = dep.package.as_deref().unwrap_or(&name).to_string();
    let req = VersionReq::parse(version)
        .map_err(|err| eyre::eyre!("invalid version requirement '{version}': {err}"))?;
    if let Some(lock_index) = lock_index.as_ref() {
        if let Some(locked) = lock_index.match_registry(&resolved_name, &req) {
            info!(
                "graph: using locked registry dependency {} {}",
                resolved_name, locked.version
            );
            let resolved = registry
                .resolve_locked_async(
                    resolved_name.clone(),
                    locked.version.clone(),
                    locked.checksum.clone(),
                )
                .await?;
            return Ok(Some(ResolvedRegistryDependency { name, resolved }));
        }
    }
    info!("graph: resolving registry dependency {} {}", resolved_name, version);
    let resolved = registry
        .resolve_async(resolved_name.clone(), Some(version.to_string()))
        .await?;
    Ok(Some(ResolvedRegistryDependency { name, resolved }))
}

fn package_to_node_with_resolved(
    package: PackageModel,
    resolved_registry: HashMap<String, ResolvedCrate>,
    options: &PackageGraphOptions,
    registry_roots: &HashMap<PathBuf, ResolvedCrate>,
    target: &TargetContext,
) -> Result<PackageNode> {
    let module_root = package.root_path.join("src");
    let entry = {
        let path = module_root.join("main.fp");
        if path.exists() { Some(path) } else { None }
    };

    let deps = collect_dependencies(&package, options, target)?;
    let checksum = registry_roots
        .get(&package.root_path)
        .and_then(|resolved| resolved.checksum.clone());
    let mut edges = Vec::new();
    for (name, dep) in deps {
        let mut edge = dependency_to_edge(name.clone(), dep, None)?;
        if let Some(resolved) = resolved_registry.get(&name) {
            edge.resolved_version = Some(resolved.version.clone());
            edge.checksum = resolved.checksum.clone();
            edge.source = Some(REGISTRY_SOURCE.to_string());
            edge.path = Some(resolved.root.clone());
        }
        edges.push(edge);
    }

    Ok(PackageNode {
        name: package.name,
        version: package.version,
        root: package.root_path.clone(),
        manifest_path: package.source_path,
        language: None,
        checksum,
        module_roots: vec![module_root],
        entry,
        dependencies: edges,
    })
}

fn collect_dependencies(
    package: &PackageModel,
    options: &PackageGraphOptions,
    target: &TargetContext,
) -> Result<HashMap<String, DependencyModel>> {
    let mut dep_map = HashMap::new();
    if options.include_dependencies {
        for (name, dep) in package.dependencies.clone() {
            if dependency_active(&dep, target)? {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_dev_dependencies {
        for (name, dep) in package.dev_dependencies.clone() {
            if dependency_active(&dep, target)? {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_build_dependencies {
        for (name, dep) in package.build_dependencies.clone() {
            if dependency_active(&dep, target)? {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_dependencies || options.include_dev_dependencies || options.include_build_dependencies {
        for targeted in &package.target_dependencies {
            if !target.is_active(&targeted.target)? {
                continue;
            }
            if options.include_dependencies {
                for (name, dep) in targeted.dependencies.clone() {
                    dep_map.insert(name, dep);
                }
            }
            if options.include_dev_dependencies {
                for (name, dep) in targeted.dev_dependencies.clone() {
                    dep_map.insert(name, dep);
                }
            }
            if options.include_build_dependencies {
                for (name, dep) in targeted.build_dependencies.clone() {
                    dep_map.insert(name, dep);
                }
            }
        }
    }
    Ok(dep_map)
}

fn dependency_active(dep: &DependencyModel, target: &TargetContext) -> Result<bool> {
    match dep.target.as_deref() {
        Some(spec) => target.is_active(spec),
        None => Ok(true),
    }
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
