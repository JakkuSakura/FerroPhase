use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::commands::build::{build_option_args, resolve_fp_binary, resolve_profile};
use crate::commands::utils::{output_path_for_entry, resolve_run_path, resolve_start_dir};
use crate::resolver::project::{resolve_graph, resolve_workspace};
use crate::utils::{collect_sources, find_furthest_manifest};
use fp_core::formats::json;

pub fn run(args: &[String]) -> crate::Result<()> {
    let options = RunOptions::parse(args)?;
    execute(&options)
}

#[derive(Debug, Clone)]
pub struct RunOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub entry: Option<PathBuf>,
    pub example: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub args: Vec<String>,
    pub fp_bin: Option<PathBuf>,
}

impl RunOptions {
    fn parse(args: &[String]) -> crate::Result<Self> {
        let mut path: Option<PathBuf> = None;
        let mut package = None;
        let mut entry = None;
        let mut example = None;
        let mut release = false;
        let mut profile = None;
        let mut fp_bin = None;
        let mut extra = Vec::new();
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
                "--fp" => {
                    let Some(value) = args.get(i + 1) else {
                        return Err("missing --fp".to_string().into());
                    };
                    fp_bin = Some(PathBuf::from(value));
                    i += 2;
                }
                "--" => {
                    extra.extend(args[i + 1..].iter().cloned());
                    break;
                }
                value => {
                    if path.is_none() {
                        path = Some(PathBuf::from(value));
                        i += 1;
                    } else {
                        extra.push(value.to_string());
                        i += 1;
                    }
                }
            }
        }

        Ok(RunOptions {
            path: path.unwrap_or_else(|| PathBuf::from(".")),
            package,
            entry,
            example,
            release,
            profile,
            args: extra,
            fp_bin,
        })
    }
}

fn execute(options: &RunOptions) -> crate::Result<()> {
    let run_path = resolve_run_path(&options.path);
    let start_dir = resolve_start_dir(&run_path);
    let (root, _manifest) = find_furthest_manifest(&start_dir)?;
    let workspace = resolve_workspace(&root)?;
    let graph = resolve_graph(&root)?;

    let profile = resolve_profile(options.release, options.profile.as_deref());
    let output_root = root.join("target").join(&profile).join("magnet").join("run");
    std::fs::create_dir_all(&output_root)?;
    let graph_path = output_root.join("package-graph.json");
    let graph_value = crate::commands::graph::graph_to_value(&graph);
    let payload = json::to_string_pretty(&graph_value)?;
    std::fs::write(&graph_path, payload)?;

    let entry = resolve_entry_override(&run_path, options)?;
    let package = resolve_package_for_entry(&workspace, &entry)?;
    let fp_bin = resolve_fp_binary(options.fp_bin.as_deref())?;
    let build_options = build_option_args(&root)?;

    let output_dir = output_root.join(package.root_path.file_name().unwrap_or_default());
    std::fs::create_dir_all(&output_dir)?;
    let sources = collect_sources(&package.root_path.join("src"), &["fp"])?;

    let mut command = Command::new(fp_bin);
    command.arg("compile");
    for source in &sources {
        command.arg(source);
    }
    if !sources.contains(&entry) {
        command.arg(&entry);
    }
    command.arg("--backend").arg("binary");
    command.arg("--output").arg(&output_dir);
    command.arg("--package-graph").arg(&graph_path);
    if profile == "release" {
        command.arg("--release");
    }
    for option in &build_options {
        command.arg("--build-option").arg(option);
    }
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status()?;
    if !status.success() {
        return Err("fp compile failed".to_string().into());
    }

    let output = output_path_for_entry(&entry, &output_dir);
    let mut exec = Command::new(output);
    exec.args(&options.args);
    exec.current_dir(&package.root_path);
    exec.stdin(Stdio::inherit());
    exec.stdout(Stdio::inherit());
    exec.stderr(Stdio::inherit());
    let status = exec.status()?;
    if !status.success() {
        return Err("binary failed".to_string().into());
    }
    Ok(())
}

fn resolve_entry_override(path: &Path, options: &RunOptions) -> crate::Result<PathBuf> {
    if let Some(entry) = &options.entry {
        return Ok(entry.clone());
    }
    if let Some(example) = &options.example {
        return Ok(path.join("examples").join(format!("{example}.fp")));
    }
    if path.is_file() {
        return Ok(path.to_path_buf());
    }
    let main = path.join("src").join("main.fp");
    if main.exists() {
        return Ok(main);
    }
    let lib = path.join("src").join("lib.fp");
    if lib.exists() {
        return Ok(lib);
    }
    Err("no entry file found".to_string().into())
}

fn resolve_package_for_entry(workspace: &crate::models::WorkspaceModel, entry: &Path) -> crate::Result<crate::models::PackageModel> {
    for package in &workspace.packages {
        if entry.starts_with(&package.root_path) {
            return Ok(package.clone());
        }
    }
    Err("entry does not belong to workspace".to_string().into())
}
