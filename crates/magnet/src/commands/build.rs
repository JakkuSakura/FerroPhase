//! Command implementation for building FerroPhase code via fp-cli

use crate::configs::ManifestConfig;
use crate::models::{ManifestModel, PackageGraphOptions, PackageModel};
use crate::resolver::project::resolve_graph;
use crate::utils::find_furthest_manifest;
use eyre::{Result, WrapErr, bail};
use glob::glob;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::Instant;
use tracing::info;

pub struct BuildOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub entry: Option<PathBuf>,
    pub resolver: String,
    pub example: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub jobs: usize,
    pub build_options: Vec<String>,
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub fetch: bool,
}

pub fn build(options: &BuildOptions) -> Result<()> {
    let started_at = Instant::now();
    let run_path = resolve_run_path(options)?;
    let start_dir = resolve_start_dir(&run_path)?;
    info!("build: resolved run path {}", run_path.display());
    let (root, manifest) = find_furthest_manifest(&start_dir)?;
    info!("build: using manifest root {}", root.display());
    let single_entry = options.entry.is_some()
        || options.example.is_some()
        || run_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| matches!(ext, "fp" | "rs"))
            .unwrap_or(false);
    let package = resolve_package(&start_dir, &manifest, options.package.as_deref())?;

    let build_config = load_manifest_build_config(&root)?;
    validate_feature_list(
        "build.features",
        &build_config.features,
        &build_config.feature_defs,
    )?;
    let mut build_options = build_config.options.clone();
    if !build_config.features.is_empty() {
        build_options.insert("features".to_string(), build_config.features.join(","));
    }
    let cli_build_options = parse_build_options(&options.build_options)?;
    validate_cli_build_options(
        &cli_build_options,
        &build_config.options,
        &build_config.feature_defs,
    )?;
    build_options.extend(cli_build_options);
    let build_option_args = build_options_to_args(&build_options);
    let graph_options = PackageGraphOptions {
        offline: options.offline,
        cache_dir: options.cache_dir.clone(),
        include_dependencies: true,
        include_dev_dependencies: true,
        include_build_dependencies: true,
        include_all_targets: false,
        cargo_fetch: options.fetch,
        resolve_registry: true,
        allow_multiple_versions: true,
        use_lock: true,
        refresh_index: false,
        write_lock: true,
        target: None,
    };
    let profile = resolve_profile(options.release, options.profile.as_deref());
    info!("build: generating workspace graph");
    let (graph, graph_path) =
        write_workspace_graph(&root, build_options, &graph_options, &profile)?;
    info!("build: graph at {}", graph_path.display());

    let fp_bin = resolve_fp_binary()?;

    if single_entry {
        let entry = resolve_entry(&run_path, &package, options.entry.as_deref())?;
        let sources = collect_sources(&package, &entry)?;
        let output_dir = build_output_dir(&package, &profile);
        info!("build: compiling single entry {}", entry.display());
        return compile_only(
            &fp_bin,
            &package,
            &entry,
            &sources,
            &graph_path,
            &output_dir,
            &build_option_args,
        )
        .map(|_| ());
    }

    let workspace_packages = manifest.list_packages()?;
    let target_packages = if let Some(name) = options.package.as_deref() {
        vec![workspace_packages
            .iter()
            .find(|pkg| pkg.name == name)
            .cloned()
            .ok_or_else(|| eyre::eyre!("Package '{}' not found", name))?]
    } else {
        workspace_packages.clone()
    };

    build_packages_with_dependencies(
        &workspace_packages,
        &target_packages,
        &graph,
        &graph_path,
        &build_option_args,
        &profile,
        &fp_bin,
        options.jobs.max(1),
    )?;

    info!("build: completed in {:.2?}", started_at.elapsed());
    Ok(())
}

fn compile_only(
    fp_bin: &Path,
    package: &PackageModel,
    entry: &Path,
    sources: &[PathBuf],
    graph_path: &Path,
    output_dir: &Path,
    build_options: &[String],
) -> Result<BuildOutcome> {
    let started_at = Instant::now();
    let entry_output = output_path_for_entry(entry, output_dir);
    let fingerprint = build_fingerprint(fp_bin, entry, sources, build_options, graph_path)?;
    if let Some(outcome) = check_build_cache(output_dir, entry, &entry_output, &fingerprint)? {
        return Ok(outcome);
    }

    info!("build: fp binary {}", fp_bin.display());
    info!("build: output dir {}", output_dir.display());
    info!("build: output file {}", entry_output.display());
    info!("build: graph {}", graph_path.display());
    info!("build: sources ({})", sources.len());
    log_sources(sources);

    let mut args: Vec<String> = Vec::new();
    args.push("compile".to_string());
    for source in sources {
        args.push(source.display().to_string());
    }
    args.push("--target".to_string());
    args.push("binary".to_string());
    args.push("--output".to_string());
    args.push(output_dir.display().to_string());
    args.push("--package-graph".to_string());
    args.push(graph_path.display().to_string());
    for option in build_options {
        args.push("--build-option".to_string());
        args.push(option.clone());
    }
    info!("build: fp args {:?}", args);

    let mut command = Command::new(&fp_bin);
    command.args(&args);
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command
        .status()
        .with_context(|| format!("Failed to execute fp at '{}'", fp_bin.display()))?;
    if !status.success() {
        bail!(
            "fp compile failed with status {}",
            status.code().unwrap_or(-1)
        );
    }

    write_build_cache(output_dir, entry, &fingerprint)?;
    info!(
        "build: compiled {} in {:.2?}",
        entry_output.display(),
        started_at.elapsed()
    );
    Ok(BuildOutcome::Built)
}

fn resolve_start_dir(path: &Path) -> Result<PathBuf> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if path.is_file() {
        Ok(path.parent().unwrap_or(Path::new(".")).to_path_buf())
    } else {
        Ok(path)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BuildCache {
    fingerprint: String,
    entry: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuildOutcome {
    Built,
    Skipped,
}

fn build_fingerprint(
    fp_bin: &Path,
    entry: &Path,
    sources: &[PathBuf],
    build_options: &[String],
    graph_path: &Path,
) -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(fp_bin.display().to_string());
    if let Ok(meta) = fs::metadata(fp_bin) {
        hasher.update(meta.len().to_le_bytes());
        if let Ok(modified) = meta.modified() {
            if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                hasher.update(duration.as_secs().to_le_bytes());
                hasher.update(duration.subsec_nanos().to_le_bytes());
            }
        }
    }
    hasher.update(entry.display().to_string());
    for option in build_options {
        hasher.update(option.as_bytes());
    }
    if let Ok(graph_contents) = fs::read(graph_path) {
        hasher.update(graph_contents);
    }

    let mut sources_sorted = sources.to_vec();
    sources_sorted.sort();
    for source in sources_sorted {
        hasher.update(source.display().to_string());
        let contents = fs::read(&source)
            .with_context(|| format!("Failed to read {}", source.display()))?;
        hasher.update(contents);
    }

    let digest = hasher.finalize();
    Ok(format!("{:x}", digest))
}

fn build_cache_path(output_dir: &Path) -> PathBuf {
    output_dir.join(".magnet-build.json")
}

fn check_build_cache(
    output_dir: &Path,
    entry: &Path,
    entry_output: &Path,
    fingerprint: &str,
) -> Result<Option<BuildOutcome>> {
    let cache_path = build_cache_path(output_dir);
    if !entry_output.exists() || !cache_path.exists() {
        return Ok(None);
    }
    let payload = fs::read_to_string(&cache_path)
        .with_context(|| format!("Failed to read {}", cache_path.display()))?;
    let cache: BuildCache =
        serde_json::from_str(&payload).context("Failed to parse build cache")?;
    if cache.fingerprint == fingerprint && cache.entry == entry {
        info!(
            "build: skipping {} (cache hit)",
            entry_output.display()
        );
        return Ok(Some(BuildOutcome::Skipped));
    }
    Ok(None)
}

fn write_build_cache(output_dir: &Path, entry: &Path, fingerprint: &str) -> Result<()> {
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)
            .with_context(|| format!("Failed to create {}", output_dir.display()))?;
    }
    let cache = BuildCache {
        fingerprint: fingerprint.to_string(),
        entry: entry.to_path_buf(),
    };
    let payload = serde_json::to_string_pretty(&cache).context("Failed to encode build cache")?;
    fs::write(build_cache_path(output_dir), payload)
        .with_context(|| "Failed to write build cache".to_string())?;
    Ok(())
}

fn resolve_run_path(options: &BuildOptions) -> Result<PathBuf> {
    if let Some(example) = options.example.as_ref() {
        let cwd =
            std::env::current_dir().map_err(|err| eyre::eyre!("Failed to resolve cwd: {err}"))?;
        let base = cwd.join("examples").join(example);
        if base.exists() {
            return Ok(base);
        }
        let with_ext = base.with_extension("fp");
        if with_ext.exists() {
            return Ok(with_ext);
        }
        bail!("Example '{}' not found at {}", example, base.display());
    }

    Ok(options.path.clone())
}

struct BuildTask {
    name: String,
    package: PackageModel,
    entry: PathBuf,
    sources: Vec<PathBuf>,
    output_dir: PathBuf,
}

fn build_packages_with_dependencies(
    workspace_packages: &[PackageModel],
    target_packages: &[PackageModel],
    graph: &crate::models::PackageGraph,
    graph_path: &Path,
    build_options: &[String],
    profile: &str,
    fp_bin: &Path,
    jobs: usize,
) -> Result<()> {
    let mut root_to_package = HashMap::new();
    for package in workspace_packages {
        root_to_package.insert(package.root_path.clone(), package.clone());
    }

    let (workspace_nodes, node_by_root, node_by_name) =
        workspace_nodes(graph, root_to_package.keys().cloned().collect());
    let (dependencies, dependents) = workspace_edges(graph, &node_by_root, &node_by_name);

    let target_indices = target_packages
        .iter()
        .filter_map(|pkg| node_by_root.get(&pkg.root_path).copied())
        .collect::<Vec<_>>();
    let required = required_nodes(&dependencies, &target_indices);

    let mut tasks = Vec::with_capacity(workspace_nodes.len());
    for (idx, node) in workspace_nodes.iter().enumerate() {
        if !required.contains(&idx) {
            tasks.push(None);
            continue;
        }
        let Some(pkg) = root_to_package.get(&node.root) else {
            tasks.push(None);
            continue;
        };
        let has_fp = has_fp_sources(&pkg.root_path)?;
        if !has_fp {
            tasks.push(None);
            continue;
        }
        let Some(entry) = resolve_package_entry(pkg)? else {
            tasks.push(None);
            continue;
        };
        let sources = collect_sources(pkg, &entry)?;
        let output_dir = build_output_dir(pkg, profile);
        tasks.push(Some(BuildTask {
            name: pkg.name.clone(),
            package: pkg.clone(),
            entry,
            sources,
            output_dir,
        }));
    }

    if jobs <= 1 {
        build_sequential(
            &workspace_nodes,
            &dependencies,
            &dependents,
            &tasks,
            fp_bin,
            graph_path,
            build_options,
            &required,
        )?;
        return Ok(());
    }

    build_parallel(
        &workspace_nodes,
        &dependencies,
        &dependents,
        tasks,
        fp_bin,
        graph_path,
        build_options,
        &required,
        jobs,
    )
}

fn workspace_nodes(
    graph: &crate::models::PackageGraph,
    workspace_roots: HashSet<PathBuf>,
) -> (Vec<crate::models::PackageNode>, HashMap<PathBuf, usize>, HashMap<String, usize>) {
    let mut nodes = Vec::new();
    let mut by_root = HashMap::new();
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for node in &graph.packages {
        if workspace_roots.contains(&node.root) {
            let idx = nodes.len();
            nodes.push(node.clone());
            by_root.insert(node.root.clone(), idx);
            *name_counts.entry(node.name.clone()).or_default() += 1;
        }
    }
    let mut by_name = HashMap::new();
    for (idx, node) in nodes.iter().enumerate() {
        if name_counts.get(&node.name).copied().unwrap_or(0) == 1 {
            by_name.insert(node.name.clone(), idx);
        }
    }
    (nodes, by_root, by_name)
}

fn workspace_edges(
    graph: &crate::models::PackageGraph,
    node_by_root: &HashMap<PathBuf, usize>,
    node_by_name: &HashMap<String, usize>,
) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let mut dependencies = vec![Vec::new(); node_by_root.len()];
    let mut dependents = vec![Vec::new(); node_by_root.len()];

    let canonical_root = |path: &Path| path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    for node in &graph.packages {
        let Some(&idx) = node_by_root.get(&node.root) else {
            continue;
        };
        let mut deps = Vec::new();
        for edge in &node.dependencies {
            let dep_idx = if let Some(path) = edge.path.as_ref() {
                let resolved = canonical_root(path);
                node_by_root.get(&resolved).copied()
            } else if edge.workspace.unwrap_or(false) {
                let name = edge.package.as_ref().unwrap_or(&edge.name);
                node_by_name.get(name).copied()
            } else {
                None
            };
            if let Some(dep_idx) = dep_idx {
                if dep_idx != idx && !deps.contains(&dep_idx) {
                    deps.push(dep_idx);
                }
            }
        }
        dependencies[idx] = deps.clone();
        for dep in deps {
            dependents[dep].push(idx);
        }
    }

    (dependencies, dependents)
}

fn required_nodes(deps: &[Vec<usize>], roots: &[usize]) -> HashSet<usize> {
    let mut visited = HashSet::new();
    let mut stack = roots.to_vec();
    while let Some(idx) = stack.pop() {
        if !visited.insert(idx) {
            continue;
        }
        for dep in &deps[idx] {
            stack.push(*dep);
        }
    }
    visited
}

fn build_sequential(
    nodes: &[crate::models::PackageNode],
    dependencies: &[Vec<usize>],
    dependents: &[Vec<usize>],
    tasks: &[Option<BuildTask>],
    fp_bin: &Path,
    graph_path: &Path,
    build_options: &[String],
    required: &HashSet<usize>,
) -> Result<()> {
    let order = topo_order(dependencies, dependents, required)?;
    let total = order.len();
    for (pos, idx) in order.into_iter().enumerate() {
        if let Some(task) = tasks.get(idx).and_then(|task| task.as_ref()) {
            info!(
                "build: [{} / {}] compiling {} ({})",
                pos + 1,
                total,
                task.name,
                task.entry.display()
            );
            compile_only(
                fp_bin,
                &task.package,
                &task.entry,
                &task.sources,
                graph_path,
                &task.output_dir,
                build_options,
            )?;
        } else {
            info!(
                "build: [{} / {}] skipping {} (no FP/RS sources)",
                pos + 1,
                total,
                nodes[idx].name
            );
        }
    }
    Ok(())
}

fn topo_order(
    dependencies: &[Vec<usize>],
    dependents: &[Vec<usize>],
    required: &HashSet<usize>,
) -> Result<Vec<usize>> {
    let mut indegree = vec![0usize; dependencies.len()];
    for (idx, deps) in dependencies.iter().enumerate() {
        if !required.contains(&idx) {
            continue;
        }
        indegree[idx] = deps.iter().filter(|dep| required.contains(dep)).count();
    }

    let mut queue = VecDeque::new();
    for idx in 0..dependencies.len() {
        if required.contains(&idx) && indegree[idx] == 0 {
            queue.push_back(idx);
        }
    }

    let mut order = Vec::new();
    while let Some(idx) = queue.pop_front() {
        order.push(idx);
        for dep in &dependents[idx] {
            if !required.contains(dep) {
                continue;
            }
            indegree[*dep] = indegree[*dep].saturating_sub(1);
            if indegree[*dep] == 0 {
                queue.push_back(*dep);
            }
        }
    }

    if order.len() != required.len() {
        bail!("dependency graph contains a cycle among workspace packages");
    }

    Ok(order)
}

fn build_parallel(
    nodes: &[crate::models::PackageNode],
    dependencies: &[Vec<usize>],
    dependents: &[Vec<usize>],
    tasks: Vec<Option<BuildTask>>,
    fp_bin: &Path,
    graph_path: &Path,
    build_options: &[String],
    required: &HashSet<usize>,
    jobs: usize,
) -> Result<()> {
    let mut indegree = vec![0usize; dependencies.len()];
    for (idx, deps) in dependencies.iter().enumerate() {
        if !required.contains(&idx) {
            continue;
        }
        indegree[idx] = deps.iter().filter(|dep| required.contains(dep)).count();
    }

    let mut ready = VecDeque::new();
    for idx in 0..dependencies.len() {
        if required.contains(&idx) && indegree[idx] == 0 {
            ready.push_back(idx);
        }
    }

    let total = required.len();
    let state = Arc::new((
        Mutex::new(BuildScheduler {
            indegree,
            ready,
            done: 0,
            total,
            error: None,
        }),
        Condvar::new(),
    ));

    let tasks = Arc::new(tasks);
    let dependents = Arc::new(dependents.to_vec());
    let nodes = Arc::new(nodes.to_vec());
    let fp_bin = fp_bin.to_path_buf();
    let graph_path = graph_path.to_path_buf();
    let build_options = build_options.to_vec();

    thread::scope(|scope| {
        for _ in 0..jobs {
            let state = Arc::clone(&state);
            let tasks = Arc::clone(&tasks);
            let dependents = Arc::clone(&dependents);
            let nodes = Arc::clone(&nodes);
            let fp_bin = fp_bin.clone();
            let graph_path = graph_path.clone();
            let build_options = build_options.clone();
            scope.spawn(move || {
                loop {
                    let idx = {
                        let (lock, cvar) = &*state;
                        let mut scheduler = lock.lock().unwrap();
                        while scheduler.ready.is_empty() && scheduler.done < scheduler.total {
                            if scheduler.error.is_some() {
                                return;
                            }
                            scheduler = cvar.wait(scheduler).unwrap();
                        }
                        if scheduler.error.is_some() || scheduler.done >= scheduler.total {
                            return;
                        }
                        scheduler.ready.pop_front()
                    };

                    let Some(idx) = idx else { continue };

                    let result = if let Some(task) = tasks.get(idx).and_then(|task| task.as_ref()) {
                        info!(
                            "build: compiling {} ({})",
                            task.name,
                            task.entry.display()
                        );
                        compile_only(
                            &fp_bin,
                            &task.package,
                            &task.entry,
                            &task.sources,
                            &graph_path,
                            &task.output_dir,
                            &build_options,
                        )
                        .map(|_| ())
                    } else {
                        info!(
                            "build: skipping {} (no FP/RS sources)",
                            nodes[idx].name
                        );
                        Ok(())
                    };

                    let (lock, cvar) = &*state;
                    let mut scheduler = lock.lock().unwrap();
                    if let Err(err) = result {
                        scheduler.error = Some(err);
                        cvar.notify_all();
                        return;
                    }
                    scheduler.done += 1;
                    for dep in &dependents[idx] {
                        if !required.contains(dep) {
                            continue;
                        }
                        scheduler.indegree[*dep] = scheduler.indegree[*dep].saturating_sub(1);
                        if scheduler.indegree[*dep] == 0 {
                            scheduler.ready.push_back(*dep);
                        }
                    }
                    cvar.notify_all();
                }
            });
        }
    });

    let (lock, _) = &*state;
    let mut scheduler = lock.lock().unwrap();
    if let Some(err) = scheduler.error.take() {
        return Err(err);
    }
    if scheduler.done != scheduler.total {
        bail!("build did not complete for all packages");
    }
    Ok(())
}

struct BuildScheduler {
    indegree: Vec<usize>,
    ready: VecDeque<usize>,
    done: usize,
    total: usize,
    error: Option<eyre::Report>,
}

fn resolve_package(
    start_dir: &Path,
    manifest: &ManifestModel,
    package_name: Option<&str>,
) -> Result<PackageModel> {
    if let Some(name) = package_name {
        let packages = manifest.list_packages()?;
        return packages
            .into_iter()
            .find(|pkg| pkg.name == name)
            .ok_or_else(|| eyre::eyre!("Package '{}' not found", name));
    }

    if let Some(package) = find_nearest_package(start_dir)? {
        return Ok(package);
    }

    let packages = manifest.list_packages()?;
    match packages.len() {
        0 => bail!("No packages found under {}", start_dir.display()),
        1 => Ok(packages.into_iter().next().unwrap()),
        _ => {
            let names: Vec<String> = packages.into_iter().map(|p| p.name).collect();
            bail!(
                "Multiple packages found ({}). Use --package to select one.",
                names.join(", ")
            );
        }
    }
}

fn resolve_entry(path: &Path, package: &PackageModel, entry: Option<&Path>) -> Result<PathBuf> {
    let entry_path = if let Some(entry) = entry {
        resolve_path(entry, &package.root_path)
    } else if path.is_file() {
        resolve_path(path, &package.root_path)
    } else {
        package.root_path.join("src").join("main.fp")
    };

    if !entry_path.exists() {
        bail!("Entry file not found: {}", entry_path.display());
    }

    Ok(entry_path)
}

fn resolve_package_entry(package: &PackageModel) -> Result<Option<PathBuf>> {
    let fp = package.root_path.join("src").join("main.fp");
    if fp.exists() {
        return Ok(Some(fp));
    }
    let fp_lib = package.root_path.join("src").join("lib.fp");
    if fp_lib.exists() {
        return Ok(Some(fp_lib));
    }
    let rs = package.root_path.join("src").join("main.rs");
    if rs.exists() {
        return Ok(Some(rs));
    }
    let rs_lib = package.root_path.join("src").join("lib.rs");
    if rs_lib.exists() {
        return Ok(Some(rs_lib));
    }
    Ok(None)
}

fn resolve_path(path: &Path, root: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    }
}

fn find_nearest_package(start_dir: &Path) -> Result<Option<PackageModel>> {
    let mut current = start_dir;
    loop {
        if current.join("Magnet.toml").exists() || current.join("Cargo.toml").exists() {
            if let Ok(package) = PackageModel::from_dir(current) {
                return Ok(Some(package));
            }
        }
        if let Some(parent) = current.parent() {
            current = parent;
        } else {
            break;
        }
    }
    Ok(None)
}

fn collect_sources(package: &PackageModel, entry: &Path) -> Result<Vec<PathBuf>> {
    let mut sources = BTreeSet::new();
    let src_root = package.root_path.join("src");
    let include_rs = entry
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext == "rs")
        .unwrap_or(false)
        || has_fp_sources(&package.root_path)?;
    if src_root.exists() {
        let pattern = format!("{}/**/*.fp", src_root.display());
        for item in glob(&pattern)? {
            let path = item?;
            sources.insert(path);
        }
        if include_rs {
            let pattern = format!("{}/**/*.rs", src_root.display());
            for item in glob(&pattern)? {
                let path = item?;
                sources.insert(path);
            }
        }
    }

    sources.insert(entry.to_path_buf());

    if sources.is_empty() {
        bail!(
            "No .fp/.rs sources found under {}",
            package.root_path.display()
        );
    }

    Ok(sources.into_iter().collect())
}

fn log_sources(sources: &[PathBuf]) {
    for (idx, source) in sources.iter().enumerate() {
        info!("build: source [{:03}] {}", idx + 1, source.display());
    }
}

fn has_fp_sources(root: &Path) -> Result<bool> {
    let src_root = root.join("src");
    if !src_root.exists() {
        return Ok(false);
    }
    for ext in ["fp", "rs"] {
        let pattern = format!("{}/**/*.{}", src_root.display(), ext);
        for item in glob(&pattern)? {
            if item.is_ok() {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

fn resolve_profile(release: bool, profile: Option<&str>) -> String {
    if let Some(profile) = profile {
        return profile.to_string();
    }
    if release {
        return "release".to_string();
    }
    "debug".to_string()
}

fn build_output_dir(package: &PackageModel, profile: &str) -> PathBuf {
    package
        .root_path
        .join("target")
        .join("magnet")
        .join(profile)
        .join("build")
}

fn parse_build_options(options: &[String]) -> Result<std::collections::HashMap<String, String>> {
    let mut map = std::collections::HashMap::new();
    for option in options {
        let mut iter = option.splitn(2, '=');
        let key = iter.next().unwrap_or("").trim();
        let value = iter.next().unwrap_or("").trim();
        if key.is_empty() {
            bail!("Invalid build option '{}'; expected key=value", option);
        }
        map.insert(key.to_string(), value.to_string());
    }
    Ok(map)
}

fn build_options_to_args(options: &HashMap<String, String>) -> Vec<String> {
    let mut items: Vec<_> = options.iter().collect();
    items.sort_by_key(|(key, _)| *key);
    items
        .into_iter()
        .map(|(key, value)| format!("{key}={value}"))
        .collect()
}

struct ManifestBuildConfig {
    options: HashMap<String, String>,
    features: Vec<String>,
    feature_defs: HashMap<String, Vec<String>>,
}

fn load_manifest_build_config(manifest_root: &Path) -> Result<ManifestBuildConfig> {
    let path = manifest_root.join("Magnet.toml");
    if !path.exists() {
        return Ok(ManifestBuildConfig {
            options: HashMap::new(),
            features: Vec::new(),
            feature_defs: HashMap::new(),
        });
    }
    let config = ManifestConfig::from_file(&path)?;
    let mut options = HashMap::new();
    let mut features = Vec::new();
    if let Some(build) = config.build {
        options.extend(build.options);
        features = build.features;
    }
    Ok(ManifestBuildConfig {
        options,
        features,
        feature_defs: config.features,
    })
}

fn parse_feature_list(raw: &str) -> Vec<String> {
    raw.split(',')
        .map(|item| item.trim())
        .filter(|item| !item.is_empty())
        .map(|item| item.to_string())
        .collect()
}

fn validate_feature_list(
    label: &str,
    features: &[String],
    feature_defs: &HashMap<String, Vec<String>>,
) -> Result<()> {
    let mut missing = Vec::new();
    for feature in features {
        if !feature_defs.contains_key(feature) {
            missing.push(feature.clone());
        }
    }
    if !missing.is_empty() {
        bail!(
            "{} contains undefined feature(s): {}",
            label,
            missing.join(", ")
        );
    }
    Ok(())
}

fn validate_cli_build_options(
    cli_options: &HashMap<String, String>,
    allowed_options: &HashMap<String, String>,
    feature_defs: &HashMap<String, Vec<String>>,
) -> Result<()> {
    for (key, value) in cli_options {
        if key == "features" {
            let features = parse_feature_list(value);
            validate_feature_list("build.options.features", &features, feature_defs)?;
            continue;
        }
        if !allowed_options.contains_key(key) {
            bail!(
                "Unknown build option '{}'; add it under [build.options] in Magnet.toml",
                key
            );
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_features_rejects_unknown() {
        let mut feature_defs = HashMap::new();
        feature_defs.insert("feature_a".to_string(), Vec::new());
        let err = validate_feature_list(
            "build.features",
            &vec!["feature_a".to_string(), "missing".to_string()],
            &feature_defs,
        )
        .unwrap_err();
        assert!(err.to_string().contains("missing"));
    }

    #[test]
    fn validate_cli_build_options_rejects_unknown_option() {
        let mut allowed = HashMap::new();
        allowed.insert("opt_level".to_string(), "2".to_string());
        let cli_options = HashMap::from([("unknown".to_string(), "value".to_string())]);
        let err = validate_cli_build_options(&cli_options, &allowed, &HashMap::new()).unwrap_err();
        assert!(err.to_string().contains("Unknown build option"));
    }

    #[test]
    fn validate_cli_build_options_rejects_unknown_feature() {
        let mut feature_defs = HashMap::new();
        feature_defs.insert("feature_a".to_string(), Vec::new());
        let cli_options =
            HashMap::from([("features".to_string(), "feature_a,missing".to_string())]);
        let err =
            validate_cli_build_options(&cli_options, &HashMap::new(), &feature_defs).unwrap_err();
        assert!(err.to_string().contains("missing"));
    }
}

fn write_workspace_graph(
    manifest_root: &Path,
    build_options: std::collections::HashMap<String, String>,
    graph_options: &PackageGraphOptions,
    profile: &str,
) -> Result<(crate::models::PackageGraph, PathBuf)> {
    let mut graph = resolve_graph(manifest_root, graph_options)?;
    graph.build_options = build_options;
    let output_dir = manifest_root
        .join("target")
        .join("magnet")
        .join(profile)
        .join("build");
    fs::create_dir_all(&output_dir).with_context(|| {
        format!(
            "Failed to create output directory at {}",
            output_dir.display()
        )
    })?;
    let graph_path = output_dir.join("package-graph.json");
    let payload =
        serde_json::to_string_pretty(&graph).context("Failed to serialize package graph")?;
    fs::write(&graph_path, payload)
        .with_context(|| format!("Failed to write {}", graph_path.display()))?;
    Ok((graph, graph_path))
}

fn output_path_for_entry(entry: &Path, output_dir: &Path) -> PathBuf {
    let stem = entry.file_stem().and_then(|s| s.to_str()).unwrap_or("main");
    let ext = if cfg!(windows) { "exe" } else { "out" };
    output_dir.join(format!("{}.{}", stem, ext))
}

fn resolve_fp_binary() -> Result<PathBuf> {
    if let Some(path) = std::env::var_os("FP_BIN") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Ok(path);
        }
    }

    if let Some(path) = locate_workspace_fp() {
        return Ok(path);
    }

    if let Some(path) = command_v("fp") {
        return Ok(path);
    }

    if let Some(path) = find_in_path("fp") {
        return Ok(path);
    }

    bail!("fp binary not found; set FP_BIN or add it to PATH (command -v fp)")
}

fn locate_workspace_fp() -> Option<PathBuf> {
    let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR")?;
    let root = Path::new(&manifest_dir).parent()?.parent()?;
    let candidates = [
        root.join("target").join("debug").join(binary_name()),
        root.join("target").join("release").join(binary_name()),
    ];

    candidates.into_iter().find(|path| path.exists())
}

fn find_in_path(binary: &str) -> Option<PathBuf> {
    let binary_name = if cfg!(windows) {
        format!("{}.exe", binary)
    } else {
        binary.to_string()
    };

    let paths = std::env::var_os("PATH")?;
    std::env::split_paths(&paths)
        .map(|path| path.join(&binary_name))
        .find(|path| path.exists())
}

fn command_v(binary: &str) -> Option<PathBuf> {
    let output = Command::new("sh")
        .arg("-lc")
        .arg(format!("command -v {}", binary))
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if raw.is_empty() {
        return None;
    }
    let path = PathBuf::from(raw);
    if path.exists() { Some(path) } else { None }
}

fn binary_name() -> &'static str {
    if cfg!(windows) { "fp.exe" } else { "fp" }
}
