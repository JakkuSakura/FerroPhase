//! Command implementation for building FerroPhase code via fp-cli

use crate::models::{ManifestModel, PackageGraph, PackageGraphOptions, PackageModel};
use crate::utils::find_furthest_manifest;
use eyre::{Result, WrapErr, bail};
use glob::glob;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
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

    let _fp_requested = single_entry || has_fp_sources(&package.root_path)?;

    let build_options = parse_build_options(&options.build_options)?;
    let graph_options = PackageGraphOptions {
        offline: options.offline,
        cache_dir: options.cache_dir.clone(),
        include_dependencies: true,
        include_dev_dependencies: true,
        include_build_dependencies: true,
        cargo_fetch: options.fetch,
        resolve_registry: true,
        allow_multiple_versions: true,
    };
    let profile = resolve_profile(options.release, options.profile.as_deref());
    info!("build: generating workspace graph");
    let graph_path = write_workspace_graph(&root, build_options, &graph_options, &profile)?;
    info!("build: graph at {}", graph_path.display());

    if single_entry {
        let entry = resolve_entry(&run_path, &package, options.entry.as_deref())?;
        let sources = collect_sources(&package, &entry)?;
        let output_dir = build_output_dir(&package, &profile);
        info!("build: compiling single entry {}", entry.display());
        return compile_only(
            &package,
            &entry,
            &sources,
            &graph_path,
            &output_dir,
            &options.build_options,
        );
    }

    let packages = if let Some(name) = options.package.as_deref() {
        vec![manifest
            .list_packages()?
            .into_iter()
            .find(|pkg| pkg.name == name)
            .ok_or_else(|| eyre::eyre!("Package '{}' not found", name))?]
    } else {
        manifest.list_packages()?
    };

    let package_count = packages.len();
    info!("build: compiling {} package(s)", package_count);
    for (idx, pkg) in packages.into_iter().enumerate() {
        let has_fp = has_fp_sources(&pkg.root_path)?;
        if !has_fp {
            info!(
                "build: [{} / {}] skipping {} (no FP/RS sources)",
                idx + 1,
                package_count,
                pkg.name
            );
            continue;
        }
        let Some(entry) = resolve_package_entry(&pkg)? else {
            info!(
                "build: [{} / {}] skipping {} (no entry)",
                idx + 1,
                package_count,
                pkg.name
            );
            continue;
        };
        let sources = collect_sources(&pkg, &entry)?;
        let output_dir = build_output_dir(&pkg, &profile);
        info!(
            "build: [{} / {}] compiling {} ({})",
            idx + 1,
            package_count,
            pkg.name,
            entry.display()
        );
        compile_only(
            &pkg,
            &entry,
            &sources,
            &graph_path,
            &output_dir,
            &options.build_options,
        )?;
    }

    info!(
        "build: completed in {:.2?}",
        started_at.elapsed()
    );
    Ok(())
}


fn compile_only(
    package: &PackageModel,
    entry: &Path,
    sources: &[PathBuf],
    graph_path: &Path,
    output_dir: &Path,
    build_options: &[String],
) -> Result<()> {
    let started_at = Instant::now();
    let fp_bin = resolve_fp_binary()?;
    let entry_output = output_path_for_entry(entry, output_dir);

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

    info!(
        "build: compiled {} in {:.2?}",
        entry_output.display(),
        started_at.elapsed()
    );
    Ok(())
}

fn resolve_start_dir(path: &Path) -> Result<PathBuf> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if path.is_file() {
        Ok(path.parent().unwrap_or(Path::new(".")).to_path_buf())
    } else {
        Ok(path)
    }
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

fn write_workspace_graph(
    manifest_root: &Path,
    build_options: std::collections::HashMap<String, String>,
    graph_options: &PackageGraphOptions,
    profile: &str,
) -> Result<PathBuf> {
    let mut graph = PackageGraph::from_path_with_options(manifest_root, graph_options)?;
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
    Ok(graph_path)
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
    if path.exists() {
        Some(path)
    } else {
        None
    }
}

fn binary_name() -> &'static str {
    if cfg!(windows) {
        "fp.exe"
    } else {
        "fp"
    }
}
