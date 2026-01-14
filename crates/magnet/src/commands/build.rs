//! Command implementation for building FerroPhase code via fp-cli

use crate::models::{ManifestModel, PackageGraph, PackageGraphOptions, PackageModel};
use crate::utils::find_furthest_manifest;
use eyre::{Context, Result, bail};
use glob::glob;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
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
    let run_path = resolve_run_path(options)?;
    let start_dir = resolve_start_dir(&run_path)?;
    let (root, manifest) = find_furthest_manifest(&start_dir)?;
    let single_entry = options.entry.is_some()
        || options.example.is_some()
        || run_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| matches!(ext, "fp" | "rs"))
            .unwrap_or(false);
    let package = resolve_package(&start_dir, &manifest, options.package.as_deref())?;

    let fp_requested = single_entry || has_fp_sources(&package.root_path)?;
    let is_cargo = !fp_requested
        && package
        .source_path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name == "Cargo.toml")
        .unwrap_or(false)
        && !package.root_path.join("Magnet.toml").exists();
    if is_cargo {
        return run_cargo_build(options, &package);
    }

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
    let graph_path = write_workspace_graph(&root, build_options, &graph_options, &profile)?;

    if single_entry {
        let entry = resolve_entry(&run_path, &package, options.entry.as_deref())?;
        let sources = collect_sources(&package, &entry)?;
        let output_dir = build_output_dir(&package, &profile);
        return compile_only(
            &package,
            &entry,
            &sources,
            &graph_path,
            &output_dir,
            &options.resolver,
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

    for pkg in packages {
        let Some(entry) = resolve_package_entry(&pkg)? else {
            continue;
        };
        let sources = collect_sources(&pkg, &entry)?;
        let output_dir = build_output_dir(&pkg, &profile);
        compile_only(
            &pkg,
            &entry,
            &sources,
            &graph_path,
            &output_dir,
            &options.resolver,
            &options.build_options,
        )?;
    }

    Ok(())
}

fn run_cargo_build(options: &BuildOptions, package: &PackageModel) -> Result<()> {
    if options.entry.is_some() {
        bail!("--entry is not supported for cargo build");
    }

    let cargo_path = package.root_path.join("Cargo.toml");
    if !cargo_path.exists() {
        bail!(
            "Cargo.toml not found for package {}; magnet build currently supports Rust packages only",
            package.name
        );
    }

    let mut command = Command::new("cargo");
    command.arg("build").arg("--manifest-path").arg(&cargo_path);
    if let Some(profile) = options.profile.as_ref() {
        command.arg("--profile").arg(profile);
    } else if options.release {
        command.arg("--release");
    }
    if let Some(example) = options.example.as_ref() {
        command.arg("--example").arg(example);
    }
    if let Some(package) = options.package.as_ref() {
        command.arg("--package").arg(package);
    }
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status().with_context(|| {
        format!(
            "Failed to execute cargo build for {}",
            package.root_path.display()
        )
    })?;
    if !status.success() {
        bail!(
            "cargo build failed with status {}",
            status.code().unwrap_or(-1)
        );
    }

    Ok(())
}

fn compile_only(
    package: &PackageModel,
    entry: &Path,
    sources: &[PathBuf],
    graph_path: &Path,
    output_dir: &Path,
    resolver: &str,
    build_options: &[String],
) -> Result<()> {
    let fp_bin = resolve_fp_binary()?;
    let entry_output = output_path_for_entry(entry, output_dir);

    let mut command = Command::new(&fp_bin);
    command.arg("compile");
    for source in sources {
        command.arg(source);
    }
    command.arg("--target").arg("binary");
    command.arg("--output").arg(output_dir);
    command.arg("--package-graph").arg(graph_path);
    command.arg("--resolver").arg(resolver);
    for option in build_options {
        command.arg("--build-option").arg(option);
    }
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

    info!("build output: {}", entry_output.display());
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
    let rs = package.root_path.join("src").join("main.rs");
    if rs.exists() {
        return Ok(Some(rs));
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
    if src_root.exists() {
        let pattern = format!("{}/**/*.fp", src_root.display());
        for item in glob(&pattern)? {
            let path = item?;
            sources.insert(path);
        }
        let pattern = format!("{}/**/*.rs", src_root.display());
        for item in glob(&pattern)? {
            let path = item?;
            sources.insert(path);
        }
    }

    sources.insert(entry.to_path_buf());

    if sources.is_empty() {
        bail!("No .fp sources found under {}", package.root_path.display());
    }

    Ok(sources.into_iter().collect())
}

fn has_fp_sources(root: &Path) -> Result<bool> {
    let src_root = root.join("src");
    if !src_root.exists() {
        return Ok(false);
    }
    let pattern = format!("{}/**/*.fp", src_root.display());
    for item in glob(&pattern)? {
        if item.is_ok() {
            return Ok(true);
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
        .join(profile)
        .join("magnet")
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
        .join(profile)
        .join("magnet")
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

    if let Some(path) = find_in_path("fp") {
        return Ok(path);
    }

    bail!("fp binary not found; set FP_BIN or add it to PATH")
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

fn binary_name() -> &'static str {
    if cfg!(windows) {
        "fp.exe"
    } else {
        "fp"
    }
}
