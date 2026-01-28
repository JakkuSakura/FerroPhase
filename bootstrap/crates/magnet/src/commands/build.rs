use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use fp_core::formats::json;

use crate::configs::ManifestConfig;
use crate::resolver::project::{resolve_graph, resolve_workspace};
use crate::utils::{collect_sources, find_furthest_manifest};

pub fn run(args: &[String]) -> crate::Result<()> {
    let options = BuildOptions::parse(args)?;
    build(&options)
}

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub entry: Option<PathBuf>,
    pub example: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub build_options: Vec<String>,
    pub jobs: usize,
    pub fp_bin: Option<PathBuf>,
}

impl BuildOptions {
    fn parse(args: &[String]) -> crate::Result<Self> {
        let mut path: Option<PathBuf> = None;
        let mut package = None;
        let mut entry = None;
        let mut example = None;
        let mut release = false;
        let mut profile = None;
        let mut build_options = Vec::new();
        let mut jobs = 1usize;
        let mut fp_bin = None;

        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--package" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --package".to_string().into());
                    };
                    package = Some(value.clone());
                    i += 2;
                }
                "--entry" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --entry".to_string().into());
                    };
                    entry = Some(PathBuf::from(value));
                    i += 2;
                }
                "--example" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --example".to_string().into());
                    };
                    example = Some(value.clone());
                    i += 2;
                }
                "--release" => {
                    release = true;
                    i += 1;
                }
                "--profile" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --profile".to_string().into());
                    };
                    profile = Some(value.clone());
                    i += 2;
                }
                "--build-option" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --build-option".to_string().into());
                    };
                    build_options.push(value.clone());
                    i += 2;
                }
                "--jobs" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --jobs".to_string().into());
                    };
                    jobs = value.parse::<usize>().unwrap_or(1).max(1);
                    i += 2;
                }
                "--fp" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --fp".to_string().into());
                    };
                    fp_bin = Some(PathBuf::from(value));
                    i += 2;
                }
                value => {
                    if path.is_none() {
                        path = Some(PathBuf::from(value));
                        i += 1;
                    } else {
                        return Err(format!("unexpected argument: {value}").into());
                    }
                }
            }
        }

        Ok(BuildOptions {
            path: path.unwrap_or_else(|| PathBuf::from(".")),
            package,
            entry,
            example,
            release,
            profile,
            build_options,
            jobs,
            fp_bin,
        })
    }
}

pub fn build(options: &BuildOptions) -> crate::Result<()> {
    let run_path = resolve_run_path(&options.path);
    let start_dir = resolve_start_dir(&run_path);
    let (root, _manifest) = find_furthest_manifest(&start_dir)?;
    let workspace = resolve_workspace(&root)?;
    let graph = resolve_graph(&root)?;

    let profile = resolve_profile(options.release, options.profile.as_deref());
    let output_root = root.join("target").join(&profile).join("magnet").join("build");
    std::fs::create_dir_all(&output_root)?;
    let graph_path = output_root.join("package-graph.json");
    let graph_value = crate::commands::graph::graph_to_value(&graph);
    let payload = json::to_string_pretty(&graph_value)?;
    std::fs::write(&graph_path, payload)?;

    let fp_bin = resolve_fp_binary(options.fp_bin.as_deref())?;
    let mut build_options = build_option_args(&root)?;
    build_options.extend(options.build_options.clone());

    if let Some(entry) = resolve_entry_override(&run_path, options)? {
        let package = resolve_package_for_entry(&workspace, &entry)?;
        build_entry(&fp_bin, &package.root_path, &entry, &graph_path, &output_root, &build_options, profile.as_str())?;
        return Ok(());
    }

    let packages = if let Some(name) = options.package.as_ref() {
        workspace
            .packages
            .iter()
            .filter(|pkg| pkg.name == *name)
            .cloned()
            .collect::<Vec<_>>()
    } else {
        workspace.packages.clone()
    };

    for package in packages {
        let entry = default_entry_for_package(&package.root_path)?;
        build_entry(&fp_bin, &package.root_path, &entry, &graph_path, &output_root, &build_options, profile.as_str())?;
    }

    let _ = options.jobs;
    Ok(())
}

fn resolve_run_path(path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")).join(path)
    }
}

fn resolve_start_dir(path: &Path) -> PathBuf {
    if path.is_file() {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        path.to_path_buf()
    }
}

pub(crate) fn resolve_profile(release: bool, profile: Option<&str>) -> String {
    if let Some(profile) = profile {
        return profile.to_string();
    }
    if release {
        return "release".to_string();
    }
    "debug".to_string()
}

pub(crate) fn resolve_fp_binary(explicit: Option<&Path>) -> crate::Result<PathBuf> {
    if let Some(path) = explicit {
        return Ok(path.to_path_buf());
    }
    if let Ok(path) = std::env::var("FP_BIN") {
        return Ok(PathBuf::from(path));
    }
    Ok(PathBuf::from("fp"))
}

pub(crate) fn build_option_args(root: &Path) -> crate::Result<Vec<String>> {
    let magnet_path = root.join("Magnet.toml");
    if !magnet_path.exists() {
        return Ok(Vec::new());
    }
    let config = ManifestConfig::from_file(&magnet_path)?;
    let mut options = config.build.options;
    if !config.build.features.is_empty() {
        options.push(format!("features={}", config.build.features.join(",")));
    }
    Ok(options)
}

fn resolve_entry_override(path: &Path, options: &BuildOptions) -> crate::Result<Option<PathBuf>> {
    if let Some(entry) = &options.entry {
        return Ok(Some(entry.clone()));
    }
    if let Some(example) = &options.example {
        return Ok(Some(path.join("examples").join(format!("{example}.fp"))));
    }
    if path.is_file() {
        return Ok(Some(path.to_path_buf()));
    }
    Ok(None)
}

fn resolve_package_for_entry(workspace: &crate::models::WorkspaceModel, entry: &Path) -> crate::Result<crate::models::PackageModel> {
    let mut best = None;
    for package in &workspace.packages {
        if entry.starts_with(&package.root_path) {
            best = Some(package.clone());
            break;
        }
    }
    best.ok_or_else(|| "entry does not belong to workspace".to_string().into())
}

fn default_entry_for_package(root: &Path) -> crate::Result<PathBuf> {
    let main = root.join("src").join("main.fp");
    if main.exists() {
        return Ok(main);
    }
    let lib = root.join("src").join("lib.fp");
    if lib.exists() {
        return Ok(lib);
    }
    Err("no src/main.fp or src/lib.fp found".to_string().into())
}

fn build_entry(
    fp_bin: &Path,
    package_root: &Path,
    entry: &Path,
    graph_path: &Path,
    output_root: &Path,
    build_options: &[String],
    profile: &str,
) -> crate::Result<()> {
    let mut sources = collect_sources(&package_root.join("src"), &["fp"])?;
    if !sources.contains(&entry.to_path_buf()) {
        sources.push(entry.to_path_buf());
    }
    let output_dir = output_root.join(package_root.file_name().unwrap_or_default());
    std::fs::create_dir_all(&output_dir)?;

    let mut command = Command::new(fp_bin);
    command.arg("compile");
    for source in &sources {
        command.arg(source);
    }
    command.arg("--backend").arg("binary");
    command.arg("--output").arg(&output_dir);
    command.arg("--package-graph").arg(graph_path);
    if profile == "release" {
        command.arg("--release");
    }
    for option in build_options {
        command.arg("--build-option").arg(option);
    }
    command.current_dir(package_root);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status()?;
    if !status.success() {
        return Err(format!("fp compile failed with status {:?}", status.code()).into());
    }
    Ok(())
}
