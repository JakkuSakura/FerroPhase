//! Command implementation for running FerroPhase benchmarks

use crate::commands::run::resolve_package;
use crate::models::{PackageGraphOptions, PackageModel};
use crate::resolver::project::resolve_graph;
use crate::utils::find_furthest_manifest;
use eyre::{Context, Result, bail};
use glob::glob;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub struct BenchOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub bench: Vec<String>,
    pub benches: bool,
    pub example: Vec<String>,
    pub examples: bool,
    pub args: Vec<String>,
}

pub fn bench(options: &BenchOptions) -> Result<()> {
    let start_dir = resolve_start_dir(&options.path)?;
    let (root, manifest) = find_furthest_manifest(&start_dir)?;
    let package = resolve_package(&start_dir, &manifest, options.package.as_deref())?;

    run_fp_bench(options, &root, &package)
}

fn run_fp_bench(options: &BenchOptions, root: &Path, package: &PackageModel) -> Result<()> {
    let profile = resolve_profile(options.release, options.profile.as_deref());
    let output_dir = build_output_dir(package, &profile, "bench");
    fs::create_dir_all(&output_dir)?;

    let runner_path = output_dir.join("bench_main.fp");
    let runner_contents = r#"fn main() {
    let report = std::bench::run_benches();
    assert!(report.failed == 0);
}
"#;
    fs::write(&runner_path, runner_contents)?;

    let sources = collect_bench_sources(options, package, &runner_path)?;
    let graph_path = write_package_graph(root, package, &output_dir)?;

    compile_and_run(
        package,
        &runner_path,
        &sources,
        &graph_path,
        &output_dir,
        &options.args,
    )
}

fn resolve_start_dir(path: &Path) -> Result<PathBuf> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if path.is_file() {
        Ok(path.parent().unwrap_or(Path::new(".")).to_path_buf())
    } else {
        Ok(path)
    }
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

fn build_output_dir(package: &PackageModel, profile: &str, kind: &str) -> PathBuf {
    package
        .root_path
        .join("target")
        .join(profile)
        .join("magnet")
        .join(kind)
}

fn collect_bench_sources(
    options: &BenchOptions,
    package: &PackageModel,
    runner_path: &Path,
) -> Result<Vec<PathBuf>> {
    let mut sources = BTreeSet::new();
    let src_root = package.root_path.join("src");
    collect_dir_sources(&src_root, &mut sources)?;

    let benches_root = package.root_path.join("benches");
    if !options.bench.is_empty() {
        for bench in &options.bench {
            let path = resolve_named_source(&benches_root, bench)?;
            sources.insert(path);
        }
    } else if (options.benches || options.bench.is_empty()) && benches_root.exists() {
        collect_dir_sources(&benches_root, &mut sources)?;
    }

    let examples_root = package.root_path.join("examples");
    if !options.example.is_empty() {
        for example in &options.example {
            let path = resolve_named_source(&examples_root, example)?;
            sources.insert(path);
        }
    } else if options.examples && examples_root.exists() {
        collect_dir_sources(&examples_root, &mut sources)?;
    }

    sources.insert(runner_path.to_path_buf());

    Ok(sources.into_iter().collect())
}

fn collect_dir_sources(root: &Path, out: &mut BTreeSet<PathBuf>) -> Result<()> {
    if !root.exists() {
        return Ok(());
    }
    for pattern in ["**/*.fp", "**/*.rs"] {
        let glob_pattern = format!("{}/{}", root.display(), pattern);
        for entry in glob(&glob_pattern)? {
            let path = entry?;
            out.insert(path);
        }
    }
    Ok(())
}

fn resolve_named_source(root: &Path, name: &str) -> Result<PathBuf> {
    let base = root.join(name);
    if base.exists() {
        return Ok(base);
    }
    let fp = base.with_extension("fp");
    if fp.exists() {
        return Ok(fp);
    }
    let rs = base.with_extension("rs");
    if rs.exists() {
        return Ok(rs);
    }
    bail!("Entry '{}' not found at {}", name, base.display());
}

fn write_package_graph(root: &Path, package: &PackageModel, output_dir: &Path) -> Result<PathBuf> {
    let offline = env_flag_enabled("MAGNET_OFFLINE");
    let graph_options = PackageGraphOptions {
        offline,
        cache_dir: None,
        include_dependencies: true,
        include_dev_dependencies: true,
        include_build_dependencies: true,
        include_all_targets: false,
        cargo_fetch: true,
        resolve_registry: !offline,
        allow_multiple_versions: false,
        use_lock: true,
        refresh_index: false,
        write_lock: true,
        target: None,
    };
    let mut graph = resolve_graph(root, &graph_options)?;
    graph.selected_package = Some(package.name.clone());
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

fn compile_and_run(
    package: &PackageModel,
    entry: &Path,
    sources: &[PathBuf],
    graph_path: &Path,
    output_dir: &Path,
    args: &[String],
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
    command.arg("--resolver").arg("ferrophase");
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

    let mut exec = Command::new(&entry_output);
    exec.args(args);
    exec.current_dir(&package.root_path);
    exec.stdin(Stdio::inherit());
    exec.stdout(Stdio::inherit());
    exec.stderr(Stdio::inherit());
    let status = exec
        .status()
        .with_context(|| format!("Failed to execute output at '{}'", entry_output.display()))?;
    if !status.success() {
        bail!(
            "Execution failed with status {}",
            status.code().unwrap_or(-1)
        );
    }

    Ok(())
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
    if cfg!(windows) { "fp.exe" } else { "fp" }
}

fn output_path_for_entry(entry: &Path, output_dir: &Path) -> PathBuf {
    let stem = entry.file_stem().and_then(|s| s.to_str()).unwrap_or("main");
    let ext = if cfg!(windows) { "exe" } else { "out" };
    output_dir.join(format!("{}.{}", stem, ext))
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
        .unwrap_or(false)
}
