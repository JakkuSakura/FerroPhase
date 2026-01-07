//! Command implementation for running FerroPhase code via fp-cli

use crate::models::{ManifestModel, PackageModel};
use crate::utils::find_furthest_manifest;
use eyre::{Context, Result, bail};
use glob::glob;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

#[derive(Debug, Clone, Copy)]
pub enum RunMode {
    Compile,
    Interpret,
}

pub struct RunOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub entry: Option<PathBuf>,
    pub mode: RunMode,
}

pub fn run(options: &RunOptions) -> Result<()> {
    let start_dir = resolve_start_dir(&options.path)?;
    let package = resolve_package(&start_dir, options.package.as_deref())?;
    let entry = resolve_entry(&options.path, &package, options.entry.as_deref())?;
    let sources = collect_sources(&package, &entry)?;

    match options.mode {
        RunMode::Compile => run_compile(&package, &entry, &sources),
        RunMode::Interpret => run_interpret(&package, &sources),
    }
}

fn run_compile(package: &PackageModel, entry: &Path, sources: &[PathBuf]) -> Result<()> {
    let fp_bin = resolve_fp_binary()?;
    let output_dir = package.root_path.join("target").join("magnet");
    let entry_output = output_path_for_entry(entry, &output_dir);

    let mut command = Command::new(&fp_bin);
    command.arg("compile");
    for source in sources {
        command.arg(source);
    }
    command.arg("--target").arg("binary");
    command.arg("--output").arg(&output_dir);
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status().with_context(|| {
        format!("Failed to execute fp at '{}'", fp_bin.display())
    })?;
    if !status.success() {
        bail!("fp compile failed with status {}", status.code().unwrap_or(-1));
    }

    let mut exec = Command::new(&entry_output);
    exec.stdin(Stdio::inherit());
    exec.stdout(Stdio::inherit());
    exec.stderr(Stdio::inherit());
    let status = exec.status().with_context(|| {
        format!("Failed to execute output at '{}'", entry_output.display())
    })?;
    if !status.success() {
        bail!(
            "Execution failed with status {}",
            status.code().unwrap_or(-1)
        );
    }

    Ok(())
}

fn run_interpret(package: &PackageModel, sources: &[PathBuf]) -> Result<()> {
    let fp_bin = resolve_fp_binary()?;
    let mut command = Command::new(&fp_bin);
    command.arg("compile");
    for source in sources {
        command.arg(source);
    }
    command.arg("--target").arg("interpret");
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status().with_context(|| {
        format!("Failed to execute fp at '{}'", fp_bin.display())
    })?;
    if !status.success() {
        bail!("fp interpret failed with status {}", status.code().unwrap_or(-1));
    }

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

fn resolve_package(start_dir: &Path, package_name: Option<&str>) -> Result<PackageModel> {
    if let Some(name) = package_name {
        let (_root, manifest) = find_furthest_manifest(start_dir)?;
        let packages = manifest.list_packages()?;
        return packages
            .into_iter()
            .find(|pkg| pkg.name == name)
            .ok_or_else(|| eyre::eyre!("Package '{}' not found", name));
    }

    if let Some(package) = find_nearest_package(start_dir)? {
        return Ok(package);
    }

    let (_root, manifest) = find_furthest_manifest(start_dir)?;
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

fn resolve_path(path: &Path, base: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base.join(path)
    }
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
    }

    sources.insert(entry.to_path_buf());

    if sources.is_empty() {
        bail!("No .fp sources found under {}", package.root_path.display());
    }

    Ok(sources.into_iter().collect())
}

fn output_path_for_entry(entry: &Path, output_dir: &Path) -> PathBuf {
    let stem = entry
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
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

    bail!("fp binary not found; set FP_BIN or add it to PATH");
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

fn find_nearest_package(start_dir: &Path) -> Result<Option<PackageModel>> {
    let mut current = start_dir.to_path_buf();
    loop {
        if current.join("Magnet.toml").exists() || current.join("Cargo.toml").exists() {
            if let Ok(manifest) = ManifestModel::from_dir(&current) {
                if let ManifestModel::Package(package) = manifest {
                    return Ok(Some(package));
                }
            }
        }

        if !current.pop() {
            break;
        }
    }

    Ok(None)
}
