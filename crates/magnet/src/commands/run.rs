//! Command implementation for running FerroPhase code via fp-cli

use crate::configs::ManifestConfig;
use crate::models::{ManifestModel, PackageGraphOptions, PackageModel};
use crate::resolver::project::resolve_graph;
use crate::utils::find_furthest_manifest;
use eyre::{Context, Result, bail};
use glob::glob;
use std::collections::{BTreeSet, HashMap};
use std::fs;
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
    pub resolver: String,
    pub example: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub build_options: Vec<String>,
    pub offline: bool,
    pub cache_dir: Option<PathBuf>,
    pub fetch: bool,
}

pub fn run(options: &RunOptions) -> Result<()> {
    let run_path = resolve_run_path(options)?;
    let start_dir = resolve_start_dir(&run_path)?;
    let (root, manifest) = find_furthest_manifest(&start_dir)?;
    let package = resolve_package(&start_dir, &manifest, options.package.as_deref())?;
    let entry = resolve_entry(&run_path, &package, options.entry.as_deref())?;
    let sources = collect_sources(&package, &entry)?;
    let profile = resolve_profile(options);
    let output_dir = build_output_dir(&package, &profile);
    let build_config = load_manifest_build_config(&root)?;
    validate_feature_list("build.features", &build_config.features, &build_config.feature_defs)?;
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
        include_dev_dependencies: false,
        include_build_dependencies: false,
        cargo_fetch: options.fetch,
        resolve_registry: true,
        allow_multiple_versions: false,
        use_lock: true,
        refresh_index: false,
        write_lock: true,
        target: None,
    };
    let graph_path = write_package_graph(
        &root,
        &package,
        &output_dir,
        build_options,
        &graph_options,
    )?;

    match options.mode {
        RunMode::Compile => run_compile(
            &package,
            &entry,
            &sources,
            &graph_path,
            &options.resolver,
            &output_dir,
            &build_option_args,
        ),
        RunMode::Interpret => run_interpret(&package, &entry),
    }
}

fn run_compile(
    package: &PackageModel,
    entry: &Path,
    sources: &[PathBuf],
    graph_path: &Path,
    resolver: &str,
    output_dir: &Path,
    build_options: &[String],
) -> Result<()> {
    let fp_bin = resolve_fp_binary()?;
    let entry_output = output_path_for_entry(entry, &output_dir);

    let mut command = Command::new(&fp_bin);
    command.arg("compile");
    for source in sources {
        command.arg(source);
    }
    command.arg("--target").arg("binary");
    command.arg("--output").arg(&output_dir);
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

    let mut exec = Command::new(&entry_output);
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

fn run_interpret(package: &PackageModel, entry: &Path) -> Result<()> {
    let fp_bin = resolve_fp_binary()?;
    let mut command = Command::new(&fp_bin);
    command.arg("interpret");
    command.arg(entry);
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command
        .status()
        .with_context(|| format!("Failed to execute fp at '{}'", fp_bin.display()))?;
    if !status.success() {
        bail!(
            "fp interpret failed with status {}",
            status.code().unwrap_or(-1)
        );
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

fn resolve_run_path(options: &RunOptions) -> Result<PathBuf> {
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

pub(crate) fn resolve_package(
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

pub(crate) fn resolve_entry(
    path: &Path,
    package: &PackageModel,
    entry: Option<&Path>,
) -> Result<PathBuf> {
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

pub(crate) fn collect_sources(package: &PackageModel, entry: &Path) -> Result<Vec<PathBuf>> {
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

pub(crate) fn output_path_for_entry(entry: &Path, output_dir: &Path) -> PathBuf {
    let stem = entry.file_stem().and_then(|s| s.to_str()).unwrap_or("main");
    let ext = if cfg!(windows) { "exe" } else { "out" };
    output_dir.join(format!("{}.{}", stem, ext))
}

fn write_package_graph(
    manifest_root: &Path,
    package: &PackageModel,
    output_dir: &Path,
    build_options: std::collections::HashMap<String, String>,
    graph_options: &PackageGraphOptions,
) -> Result<PathBuf> {
    let mut graph = resolve_graph(manifest_root, graph_options)?;
    graph.selected_package = Some(package.name.clone());
    graph.build_options = build_options;
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

fn build_output_dir(package: &PackageModel, profile: &str) -> PathBuf {
    package
        .root_path
        .join("target")
        .join(profile)
        .join("magnet")
}

fn resolve_profile(options: &RunOptions) -> String {
    if let Some(profile) = options.profile.as_ref() {
        return profile.clone();
    }
    if options.release {
        return "release".to_string();
    }
    "debug".to_string()
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
    if cfg!(windows) { "fp.exe" } else { "fp" }
}

pub(crate) fn find_nearest_package(start_dir: &Path) -> Result<Option<PackageModel>> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn resolve_entry_prefers_file_argument() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        let package_dir = root.join("pkg");
        fs::create_dir_all(package_dir.join("src"))?;
        fs::write(
            package_dir.join("Magnet.toml"),
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        )?;

        let entry = package_dir.join("entry.fp");
        fs::write(&entry, "fn main() {}")?;

        let package = PackageModel::from_dir(&package_dir)?;
        let resolved = resolve_entry(&entry, &package, None)?;
        assert_eq!(resolved, entry);

        Ok(())
    }

    #[test]
    fn resolve_entry_defaults_to_main() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        let package_dir = root.join("pkg");
        fs::create_dir_all(package_dir.join("src"))?;
        fs::write(
            package_dir.join("Magnet.toml"),
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        )?;
        fs::write(package_dir.join("src").join("main.fp"), "fn main() {}")?;

        let package = PackageModel::from_dir(&package_dir)?;
        let resolved = resolve_entry(package_dir.as_path(), &package, None)?;
        let expected = package_dir.join("src").join("main.fp");
        assert_eq!(
            resolved.canonicalize().unwrap_or(resolved),
            expected.canonicalize().unwrap_or(expected)
        );

        Ok(())
    }

    #[test]
    fn collect_sources_includes_entry() -> Result<()> {
        let temp = tempdir()?;
        let package_dir = temp.path().join("pkg");
        fs::create_dir_all(package_dir.join("src"))?;
        fs::write(
            package_dir.join("Magnet.toml"),
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        )?;

        let entry = package_dir.join("src").join("main.fp");
        fs::write(&entry, "fn main() {}")?;
        fs::write(package_dir.join("src").join("lib.fp"), "fn lib() {}")?;

        let package = PackageModel::from_dir(&package_dir)?;
        let sources = collect_sources(&package, &entry)?;

        assert!(sources.contains(&entry));
        assert!(sources.iter().any(|path| path.ends_with("lib.fp")));

        Ok(())
    }
}
