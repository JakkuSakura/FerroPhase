use crate::configs::CargoManifestConfig;
use crate::models::{
    DependencyModel, LockIndex, PackageGraphOptions, PackageModel, PackageNode, REGISTRY_SOURCE,
    WorkspaceModel, dependency_to_edge, parse_cargo_features,
};
use crate::registry::ResolvedCrate;
use crate::resolver::{RegistryLoaderHandle, target::TargetContext};
use eyre::Result;
use semver::VersionReq;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::time::Instant;
use tracing::{info, warn};

pub struct CargoResolver {
    root: PathBuf,
    options: PackageGraphOptions,
    registry: Option<RegistryLoaderHandle>,
    lock_index: Option<Arc<LockIndex>>,
    target: TargetContext,
    workspace: Option<WorkspaceModel>,
    workspace_members: HashSet<PathBuf>,
    roots: Vec<PathBuf>,
    packages: Vec<PackageNode>,
    registry_roots: HashMap<PathBuf, ResolvedCrate>,
    git_roots: HashMap<PathBuf, GitResolved>,
    git_by_name: HashMap<String, GitResolved>,
    registry_requirements: HashMap<String, HashMap<CompatKey, Vec<VersionReq>>>,
    registry_selected: HashMap<(String, CompatKey), ResolvedCrate>,
    registry_dependents: HashMap<(String, CompatKey), HashSet<PathBuf>>,
    registry_requests: HashMap<(String, CompatKey), Vec<RegistryRequestInfo>>,
    registry_queue: VecDeque<PathBuf>,
    registry_queue_set: HashSet<PathBuf>,
    registry_active: HashSet<PathBuf>,
    feature_requests: HashMap<PathBuf, FeatureRequest>,
    feature_states: HashMap<PathBuf, FeatureState>,
    global_feature_requests: HashMap<String, FeatureRequest>,
    package_index: HashMap<PathBuf, usize>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct CompatKey {
    major: u64,
    minor: Option<u64>,
}

fn compatibility_key(req: &VersionReq) -> CompatKey {
    let mut major = 0u64;
    let mut minor = None;
    for comparator in &req.comparators {
        if matches!(
            comparator.op,
            semver::Op::Caret | semver::Op::Tilde | semver::Op::Exact
        ) {
            major = comparator.major;
            minor = Some(comparator.minor.unwrap_or(0));
            break;
        }
    }
    if minor.is_none() {
        if let Some(first) = req.comparators.first() {
            major = first.major;
            minor = Some(first.minor.unwrap_or(0));
        }
    }
    if major == 0 {
        CompatKey { major, minor }
    } else {
        CompatKey { major, minor: None }
    }
}

fn format_compat_key(key: &CompatKey) -> String {
    match (key.major, key.minor) {
        (0, Some(minor)) => format!("0.{minor}"),
        (0, None) => "0".to_string(),
        (major, _) => format!("{major}"),
    }
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
            workspace_members: HashSet::new(),
            roots: Vec::new(),
            packages: Vec::new(),
            registry_roots: HashMap::new(),
            git_roots: HashMap::new(),
            git_by_name: HashMap::new(),
            registry_requirements: HashMap::new(),
            registry_selected: HashMap::new(),
            registry_dependents: HashMap::new(),
            registry_requests: HashMap::new(),
            registry_queue: VecDeque::new(),
            registry_queue_set: HashSet::new(),
            registry_active: HashSet::new(),
            feature_requests: HashMap::new(),
            feature_states: HashMap::new(),
            global_feature_requests: HashMap::new(),
            package_index: HashMap::new(),
        })
    }

    pub fn load_root(&mut self, manifest_path: &Path) -> Result<()> {
        let manifest = CargoManifestConfig::from_path(manifest_path)?;
        if manifest.workspace.is_some() {
            let workspace = WorkspaceModel::from_dir(&self.root)?;
            let mut members = workspace.list_members()?;
            info!("graph: workspace members detected: {}", members.len());
            let member_set = members
                .iter()
                .map(|path| canonicalize_path(path))
                .collect::<HashSet<_>>();
            self.workspace_members = member_set;
            self.workspace = Some(workspace);
            self.roots.append(&mut members);
        }
        if manifest.package.is_some() {
            info!("graph: root package detected");
            self.roots.push(self.root.clone());
            self.workspace_members.insert(canonicalize_path(&self.root));
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
        let roots = self.roots.clone();
        for root in roots {
            self.add_feature_request(&root, FeatureRequest::root());
        }
        info!(
            "graph: resolver queue initialized with {} root(s)",
            self.registry_queue.len()
        );

        while let Some(dir) = self.registry_queue.pop_front() {
            self.registry_queue_set.remove(&dir);
            if !self.registry_active.insert(dir.clone()) {
                continue;
            }
            let package_started = Instant::now();
            let Some((package, deps, feature_state)) = self.resolve_package_dir_sync(&dir)? else {
                self.registry_active.remove(&dir);
                continue;
            };
            let package_key = canonicalize_path(&package.root_path);
            if let Some(existing) = self.feature_states.get(&package_key) {
                if existing == &feature_state {
                    self.registry_active.remove(&dir);
                    continue;
                }
            }
            self.feature_states
                .insert(package_key.clone(), feature_state.clone());
            info!(
                "graph: dependencies for {} (paths={}, registry={})",
                package.name,
                deps.paths.len(),
                deps.registry.len()
            );

            let mut resolved_registry = HashMap::new();
            if let Some(registry) = self.registry.clone() {
                let registry_started = Instant::now();
                for (name, dep, request) in deps.registry {
                    match self
                        .resolve_registry_dep(&package_key, &registry, name, dep, request)
                        .await
                    {
                        Ok(Some(resolved)) => {
                            self.add_feature_request(
                                &resolved.resolved.root,
                                resolved.request.clone(),
                            );
                            let features_changed = self
                                .global_feature_requests
                                .entry(resolved.name.clone())
                                .or_default()
                                .merge(resolved.request.clone());
                            if features_changed {
                                self.enqueue_registry(resolved.resolved.root.clone());
                            }
                            resolved_registry.insert(resolved.name, resolved.resolved);
                        }
                        Ok(None) => {}
                        Err(err) => {
                            tracing::warn!("graph: registry resolution failed: {}", err);
                        }
                    }
                }
                info!(
                    "graph: resolved registry deps for {} in {:.2?}",
                    package.name,
                    registry_started.elapsed()
                );
            }

            for (path, request) in deps.paths {
                self.add_feature_request(&path, request);
            }

            for (name, dep, request) in deps.git {
                match self.resolve_git_dependency(&name, &dep) {
                    Ok(resolved) => {
                        let root = resolved.root.clone();
                        let git_name = dep.package.as_deref().unwrap_or(&name).to_string();
                        self.git_by_name.insert(git_name, resolved.clone());
                        self.git_roots.insert(root.clone(), resolved);
                        self.add_feature_request(&root, request);
                    }
                    Err(err) => {
                        tracing::warn!("graph: git resolution failed for {}: {}", name, err);
                    }
                }
            }

            let is_workspace_member = self
                .workspace_members
                .contains(&canonicalize_path(&package.root_path));
            let node = package_to_node_with_resolved(
                package,
                resolved_registry,
                &self.options,
                &self.registry_roots,
                &self.git_roots,
                &self.git_by_name,
                is_workspace_member,
                &self.target,
                &feature_state,
            )?;
            let node_key = canonicalize_path(&node.root);
            if let Some(index) = self.package_index.get(&node_key).copied() {
                self.packages[index] = node;
            } else {
                self.package_index.insert(node_key, self.packages.len());
                self.packages.push(node);
            }
            self.registry_active.remove(&dir);
            info!(
                "graph: finished package in {:.2?} (queue remaining={})",
                package_started.elapsed(),
                self.registry_queue.len()
            );
        }
        Ok(std::mem::take(&mut self.packages))
    }

    fn add_feature_request(&mut self, path: &Path, request: FeatureRequest) {
        let key = canonicalize_path(path);
        let entry = self.feature_requests.entry(key.clone()).or_default();
        if entry.merge(request) {
            self.enqueue_registry(key);
        }
    }

    fn enqueue_registry(&mut self, path: PathBuf) {
        if self.registry_queue_set.insert(path.clone()) {
            self.registry_queue.push_back(path);
        }
    }

    fn resolve_package_dir_sync(
        &mut self,
        dir: &Path,
    ) -> Result<Option<(PackageModel, PackageDeps, FeatureState)>> {
        let started_at = Instant::now();
        let mut package = match PackageModel::from_dir(dir) {
            Ok(package) => package,
            Err(err) => {
                if dir == self.root.as_path() {
                    return Err(err.wrap_err(format!(
                        "graph: failed to resolve root package at {}",
                        dir.display()
                    )));
                }
                tracing::warn!("graph: skipping package at {}: {}", dir.display(), err);
                return Ok(None);
            }
        };
        if let Some(workspace) = &self.workspace {
            apply_workspace_dependencies(&mut package, workspace)?;
        }

        let is_workspace_member = self.workspace_members.contains(&canonicalize_path(dir));
        let local_requested = self
            .feature_requests
            .get(&canonicalize_path(dir))
            .cloned()
            .unwrap_or_default();
        let global_entry = self
            .global_feature_requests
            .entry(package.name.clone())
            .or_default();
        global_entry.merge(local_requested.clone());
        let mut combined_request = local_requested.clone();
        combined_request.merge(global_entry.clone());
        let feature_state = resolve_feature_state(&package, &combined_request)?;
        info!(
            "graph: resolving package {} at {}",
            format_package_log(&package, &self.options, &feature_state),
            dir.display()
        );
        let deps = collect_dependencies(
            &package,
            &self.options,
            &self.target,
            is_workspace_member,
            &feature_state,
        )?;
        info!(
            "graph: collected deps for {} in {:.2?}",
            package.name,
            started_at.elapsed()
        );
        let resolved = split_dependencies(&package, deps, &self.workspace, &feature_state)?;
        Ok(Some((package, resolved, feature_state)))
    }

    async fn resolve_registry_dep(
        &mut self,
        package_key: &Path,
        registry: &RegistryLoaderHandle,
        name: String,
        dep: DependencyModel,
        request: FeatureRequest,
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
        let compat_key = compatibility_key(&req);

        let mut combined_request = request.clone();
        if let Some(global) = self.global_feature_requests.get(&resolved_name) {
            combined_request.merge(global.clone());
        }

        self.registry_dependents
            .entry((resolved_name.clone(), compat_key.clone()))
            .or_default()
            .insert(package_key.to_path_buf());
        let request_info = RegistryRequestInfo {
            dependent: package_key.to_path_buf(),
            version_req: version.to_string(),
            registry: dep.registry.clone(),
            package: dep.package.clone(),
            features: combined_request.features.iter().cloned().collect(),
            default_features: combined_request.default,
        };
        self.registry_requests
            .entry((resolved_name.clone(), compat_key.clone()))
            .or_default()
            .push(request_info);

        if let Some(lock_index) = self.lock_index.as_ref() {
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
                self.registry_selected.insert(
                    (resolved_name.clone(), compat_key.clone()),
                    resolved.clone(),
                );
                self.registry_roots
                    .insert(resolved.root.clone(), resolved.clone());
                return Ok(Some(ResolvedRegistryDependency {
                    name,
                    resolved,
                    request: combined_request,
                }));
            }
        }
        if let Some(locked_selected) = self
            .registry_selected
            .get(&(resolved_name.clone(), compat_key.clone()))
        {
            let parsed = semver::Version::parse(&locked_selected.version).map_err(|err| {
                eyre::eyre!(
                    "invalid locked version {}: {}",
                    locked_selected.version,
                    err
                )
            })?;
            if req.matches(&parsed) {
                return Ok(Some(ResolvedRegistryDependency {
                    name,
                    resolved: locked_selected.clone(),
                    request: combined_request,
                }));
            }
        }

        let reqs = self
            .registry_requirements
            .entry(resolved_name.clone())
            .or_default()
            .entry(compat_key.clone())
            .or_default();
        if !reqs.iter().any(|existing| existing == &req) {
            reqs.push(req.clone());
        }
        let reqs_snapshot = reqs.clone();
        let resolved = match registry
            .resolve_with_reqs_async(resolved_name.clone(), reqs_snapshot)
            .await
        {
            Ok(resolved) => resolved,
            Err(err) => {
                warn!(
                    "graph: conflicting requirements for {}; failing resolution: {}",
                    resolved_name, err
                );
                self.log_registry_conflict(&resolved_name, &compat_key);
                return Err(err);
            }
        };
        let mut selection_changed = false;
        if let Some(current) = self
            .registry_selected
            .get(&(resolved_name.clone(), compat_key.clone()))
        {
            if current.version != resolved.version {
                selection_changed = true;
            }
        } else {
            selection_changed = true;
        }
        self.registry_selected.insert(
            (resolved_name.clone(), compat_key.clone()),
            resolved.clone(),
        );
        self.registry_roots
            .insert(resolved.root.clone(), resolved.clone());
        if selection_changed {
            if let Some(dependents) = self
                .registry_dependents
                .get(&(resolved_name.clone(), compat_key.clone()))
            {
                let dependents = dependents.iter().cloned().collect::<Vec<_>>();
                for dependent in dependents {
                    self.enqueue_registry(dependent);
                }
            }
        }

        Ok(Some(ResolvedRegistryDependency {
            name,
            resolved,
            request: combined_request,
        }))
    }

    fn resolve_git_dependency(&self, name: &str, dep: &DependencyModel) -> Result<GitResolved> {
        let url = dep
            .git
            .as_deref()
            .ok_or_else(|| eyre::eyre!("missing git url for {name}"))?;
        let cache_dir = resolve_cache_dir(self.options.cache_dir.clone())?;
        let repo_hash = hex::encode(Sha256::digest(url.as_bytes()));
        let repo_root = cache_dir.join("git").join(repo_hash);
        let checkout_ref = git_checkout_ref(dep);
        let checkout_key = checkout_ref.replace('/', "_");
        let checkout_dir = repo_root.join("checkouts").join(&checkout_key);

        let checkout_dir_str = checkout_dir.to_string_lossy().to_string();
        if !checkout_dir.exists() {
            fs::create_dir_all(&repo_root)?;
            run_git(&vec![
                "clone".to_string(),
                url.to_string(),
                checkout_dir_str.clone(),
            ])?;
        }

        if !self.options.offline && self.options.refresh_index {
            let _ = run_git(&vec![
                "-C".to_string(),
                checkout_dir_str.clone(),
                "fetch".to_string(),
                "--all".to_string(),
                "--tags".to_string(),
            ]);
        }

        if checkout_ref != "HEAD" {
            run_git(&vec![
                "-C".to_string(),
                checkout_dir_str.clone(),
                "checkout".to_string(),
                checkout_ref.clone(),
            ])?;
        }

        let rev = run_git_capture(&vec![
            "-C".to_string(),
            checkout_dir_str,
            "rev-parse".to_string(),
            "HEAD".to_string(),
        ])?;

        let manifest_path = checkout_dir.join("Cargo.toml");
        if !manifest_path.exists() {
            return Err(eyre::eyre!(
                "git dependency {name} has no Cargo.toml at {}",
                checkout_dir.display()
            ));
        }
        let source = format!("git+{}#{}", url, rev);
        Ok(GitResolved {
            url: url.to_string(),
            rev,
            root: checkout_dir,
            source,
        })
    }

    fn log_registry_conflict(&self, crate_name: &str, compat_key: &CompatKey) {
        let Some(requests) = self
            .registry_requests
            .get(&(crate_name.to_string(), compat_key.clone()))
        else {
            warn!(
                "graph: registry conflict on {} with no request details",
                crate_name
            );
            return;
        };
        let mut lines = Vec::new();
        for request in requests {
            let registry = request.registry.as_deref().unwrap_or("crates.io");
            let package = request
                .package
                .as_deref()
                .map(|name| format!(" (package={})", name))
                .unwrap_or_default();
            let mut features = request.features.clone();
            features.sort();
            let features = if features.is_empty() {
                "features=[]".to_string()
            } else {
                format!("features=[{}]", features.join(", "))
            };
            let line = format!(
                "- {}: {}{} registry={} default_features={} {}",
                request.dependent.display(),
                request.version_req,
                package,
                registry,
                request.default_features,
                features
            );
            lines.push(line);
        }
        lines.sort();
        warn!(
            "graph: registry conflict details for {} (compat={}):\n{}",
            crate_name,
            format_compat_key(compat_key),
            lines.join("\n")
        );
    }
}

struct PackageDeps {
    paths: Vec<(PathBuf, FeatureRequest)>,
    registry: Vec<(String, DependencyModel, FeatureRequest)>,
    git: Vec<(String, DependencyModel, FeatureRequest)>,
}

#[derive(Clone, Debug)]
struct GitResolved {
    url: String,
    rev: String,
    root: PathBuf,
    source: String,
}

#[derive(Clone, Debug)]
struct RegistryRequestInfo {
    dependent: PathBuf,
    version_req: String,
    registry: Option<String>,
    package: Option<String>,
    features: Vec<String>,
    default_features: bool,
}

fn split_dependencies(
    package: &PackageModel,
    deps: HashMap<String, DependencyModel>,
    workspace: &Option<WorkspaceModel>,
    feature_state: &FeatureState,
) -> Result<PackageDeps> {
    let mut paths = Vec::new();
    let mut registry = Vec::new();
    let mut git = Vec::new();
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
            let request = request_for_dependency(&name, &dep, feature_state);
            paths.push((path, request));
            continue;
        }

        if dep.git.is_some() {
            let request = request_for_dependency(&name, &dep, feature_state);
            git.push((name, dep, request));
            continue;
        }

        let request = request_for_dependency(&name, &dep, feature_state);
        registry.push((name, dep, request));
    }
    Ok(PackageDeps {
        paths,
        registry,
        git,
    })
}

struct ResolvedRegistryDependency {
    name: String,
    resolved: ResolvedCrate,
    request: FeatureRequest,
}

fn package_to_node_with_resolved(
    package: PackageModel,
    resolved_registry: HashMap<String, ResolvedCrate>,
    options: &PackageGraphOptions,
    registry_roots: &HashMap<PathBuf, ResolvedCrate>,
    git_roots: &HashMap<PathBuf, GitResolved>,
    git_by_name: &HashMap<String, GitResolved>,
    is_workspace_member: bool,
    target: &TargetContext,
    feature_state: &FeatureState,
) -> Result<PackageNode> {
    let module_root = package.root_path.join("src");
    let entry = {
        let path = module_root.join("main.fp");
        if path.exists() { Some(path) } else { None }
    };

    let deps = collect_dependencies(
        &package,
        options,
        target,
        is_workspace_member,
        feature_state,
    )?;
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
        if edge.source.is_none() {
            if let Some(path) = edge.path.as_ref() {
                if let Some(git) = git_roots.get(path) {
                    edge.source = Some(git.source.clone());
                    edge.git = Some(git.url.clone());
                    edge.rev = Some(git.rev.clone());
                }
            }
        }
        if edge.path.is_none() {
            let dep_name = edge.package.as_deref().unwrap_or(edge.name.as_str());
            if let Some(git) = git_by_name.get(dep_name) {
                edge.path = Some(git.root.clone());
                edge.source = Some(git.source.clone());
                edge.git = Some(git.url.clone());
                edge.rev = Some(git.rev.clone());
            }
        }
        edges.push(edge);
    }

    let source = git_roots
        .get(&package.root_path)
        .map(|git| git.source.clone());

    Ok(PackageNode {
        name: package.name,
        version: package.version,
        root: package.root_path.clone(),
        manifest_path: package.source_path,
        language: None,
        checksum,
        source,
        module_roots: vec![module_root],
        entry,
        dependencies: edges,
    })
}

fn collect_dependencies(
    package: &PackageModel,
    options: &PackageGraphOptions,
    target: &TargetContext,
    is_workspace_member: bool,
    feature_state: &FeatureState,
) -> Result<HashMap<String, DependencyModel>> {
    let mut dep_map = HashMap::new();
    if options.include_dependencies {
        for (name, dep) in package.dependencies.clone() {
            if dependency_active(&dep, target, options.include_all_targets)?
                && dependency_enabled(&name, &dep, feature_state)
            {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_dev_dependencies && is_workspace_member {
        for (name, dep) in package.dev_dependencies.clone() {
            if dependency_active(&dep, target, options.include_all_targets)?
                && dependency_enabled(&name, &dep, feature_state)
            {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_build_dependencies {
        for (name, dep) in package.build_dependencies.clone() {
            if dependency_active(&dep, target, options.include_all_targets)?
                && dependency_enabled(&name, &dep, feature_state)
            {
                dep_map.entry(name).or_insert(dep);
            }
        }
    }
    if options.include_dependencies
        || options.include_dev_dependencies
        || options.include_build_dependencies
    {
        for targeted in &package.target_dependencies {
            if !options.include_all_targets && !target.is_active(&targeted.target)? {
                continue;
            }
            if options.include_dependencies {
                for (name, dep) in targeted.dependencies.clone() {
                    if dependency_enabled(&name, &dep, feature_state) {
                        dep_map.insert(name, dep);
                    }
                }
            }
            if options.include_dev_dependencies && is_workspace_member {
                for (name, dep) in targeted.dev_dependencies.clone() {
                    if dependency_enabled(&name, &dep, feature_state) {
                        dep_map.insert(name, dep);
                    }
                }
            }
            if options.include_build_dependencies {
                for (name, dep) in targeted.build_dependencies.clone() {
                    if dependency_enabled(&name, &dep, feature_state) {
                        dep_map.insert(name, dep);
                    }
                }
            }
        }
    }
    Ok(dep_map)
}

fn dependency_active(
    dep: &DependencyModel,
    target: &TargetContext,
    include_all_targets: bool,
) -> Result<bool> {
    if include_all_targets {
        return Ok(true);
    }
    match dep.target.as_deref() {
        Some(spec) => target.is_active(spec),
        None => Ok(true),
    }
}

fn canonicalize_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

#[derive(Clone, Debug, Default)]
struct FeatureRequest {
    default: bool,
    features: HashSet<String>,
}

impl FeatureRequest {
    fn root() -> Self {
        Self {
            default: true,
            features: HashSet::new(),
        }
    }

    fn from_dependency(dep: &DependencyModel) -> Self {
        Self {
            default: dep.default_features(),
            features: dep.features().into_iter().collect(),
        }
    }

    fn merge(&mut self, other: FeatureRequest) -> bool {
        let mut changed = false;
        if other.default && !self.default {
            self.default = true;
            changed = true;
        }
        for feature in other.features {
            if self.features.insert(feature) {
                changed = true;
            }
        }
        changed
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FeatureState {
    enabled_features: HashSet<String>,
    enabled_deps: HashSet<String>,
    dep_features: HashMap<String, HashSet<String>>,
}

fn resolve_feature_state(package: &PackageModel, request: &FeatureRequest) -> Result<FeatureState> {
    let mut enabled_features = HashSet::new();
    if request.default {
        enabled_features.insert("default".to_string());
    }
    enabled_features.extend(request.features.iter().cloned());

    let mut enabled_deps = HashSet::new();
    let mut dep_features: HashMap<String, HashSet<String>> = HashMap::new();
    let feature_map = load_package_features(package)?;

    let mut queue: VecDeque<String> = enabled_features.iter().cloned().collect();
    while let Some(feature) = queue.pop_front() {
        let Some(items) = feature_map.get(&feature) else {
            continue;
        };
        for item in items {
            if let Some(dep) = item.strip_prefix("dep:") {
                enabled_deps.insert(dep.to_string());
                continue;
            }
            if let Some((dep, feat)) = item.split_once('/') {
                enabled_deps.insert(dep.to_string());
                dep_features
                    .entry(dep.to_string())
                    .or_default()
                    .insert(feat.to_string());
                continue;
            }
            if enabled_features.insert(item.to_string()) {
                queue.push_back(item.to_string());
            }
        }
    }

    Ok(FeatureState {
        enabled_features,
        enabled_deps,
        dep_features,
    })
}

fn load_package_features(package: &PackageModel) -> Result<HashMap<String, Vec<String>>> {
    if package
        .source_path
        .file_name()
        .and_then(|name| name.to_str())
        == Some("Cargo.toml")
    {
        return parse_cargo_features(&package.source_path);
    }
    Ok(HashMap::new())
}

fn dependency_enabled(name: &str, dep: &DependencyModel, feature_state: &FeatureState) -> bool {
    if !dep.optional() {
        return true;
    }
    feature_state.enabled_deps.contains(name) || feature_state.enabled_features.contains(name)
}

fn request_for_dependency(
    name: &str,
    dep: &DependencyModel,
    feature_state: &FeatureState,
) -> FeatureRequest {
    let mut request = FeatureRequest::from_dependency(dep);
    if let Some(extra) = feature_state.dep_features.get(name) {
        for feat in extra {
            request.features.insert(feat.clone());
        }
    }
    request
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

fn resolve_cache_dir(cache_dir: Option<PathBuf>) -> Result<PathBuf> {
    if let Some(cache_dir) = cache_dir {
        return Ok(cache_dir);
    }
    if let Some(cache_dir) = std::env::var_os("MAGNET_CACHE_DIR") {
        return Ok(PathBuf::from(cache_dir));
    }
    let home = std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .ok_or_else(|| eyre::eyre!("could not resolve home directory"))?;
    Ok(PathBuf::from(home).join(".cache").join("magnet"))
}

fn git_checkout_ref(dep: &DependencyModel) -> String {
    if let Some(rev) = dep.rev.as_deref() {
        return rev.to_string();
    }
    if let Some(tag) = dep.tag.as_deref() {
        return tag.to_string();
    }
    if let Some(branch) = dep.branch.as_deref() {
        return branch.to_string();
    }
    "HEAD".to_string()
}

fn run_git(args: &[String]) -> Result<()> {
    let status = Command::new("git")
        .args(args)
        .status()
        .map_err(|err| eyre::eyre!("failed to run git {:?}: {}", args, err))?;
    if !status.success() {
        return Err(eyre::eyre!("git {:?} failed with {}", args, status));
    }
    Ok(())
}

fn run_git_capture(args: &[String]) -> Result<String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .map_err(|err| eyre::eyre!("failed to run git {:?}: {}", args, err))?;
    if !output.status.success() {
        return Err(eyre::eyre!("git {:?} failed with {}", args, output.status));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn format_package_log(
    package: &PackageModel,
    options: &PackageGraphOptions,
    feature_state: &FeatureState,
) -> String {
    let registry = package_registry_label(&package.root_path, options.cache_dir.clone());
    let mut features = feature_state
        .enabled_features
        .iter()
        .cloned()
        .collect::<Vec<_>>();
    features.sort();
    let features = if features.is_empty() {
        "[]".to_string()
    } else {
        format!("[{}]", features.join(", "))
    };
    format!(
        "{}@{} registry={} features={}",
        package.name, package.version, registry, features
    )
}

fn package_registry_label(root: &Path, cache_dir: Option<PathBuf>) -> String {
    let cache_dir = match resolve_cache_dir(cache_dir) {
        Ok(dir) => dir,
        Err(_) => return "path".to_string(),
    };
    let registry_root = cache_dir.join("registry").join("crates");
    if root.starts_with(&registry_root) {
        return "crates.io".to_string();
    }
    let git_root = cache_dir.join("git");
    if root.starts_with(&git_root) {
        return "git".to_string();
    }
    "path".to_string()
}
