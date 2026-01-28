use std::path::PathBuf;
use std::process::{Command, Stdio};

use fp_core::formats::json;

use crate::commands::build::{build_option_args, resolve_fp_binary, resolve_profile};
use crate::commands::utils::{output_path_for_entry, resolve_run_path, resolve_start_dir};
use crate::resolver::project::{resolve_graph, resolve_workspace};
use crate::utils::{collect_sources, find_furthest_manifest};

pub fn run(args: &[String]) -> crate::Result<()> {
    let options = BenchOptions::parse(args)?;
    execute(&options)
}

#[derive(Debug, Clone)]
pub struct BenchOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
    pub args: Vec<String>,
    pub fp_bin: Option<PathBuf>,
}

impl BenchOptions {
    fn parse(args: &[String]) -> crate::Result<Self> {
        let mut path: Option<PathBuf> = None;
        let mut package = None;
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

        Ok(BenchOptions {
            path: path.unwrap_or_else(|| PathBuf::from(".")),
            package,
            release,
            profile,
            args: extra,
            fp_bin,
        })
    }
}

fn execute(options: &BenchOptions) -> crate::Result<()> {
    let run_path = resolve_run_path(&options.path);
    let start_dir = resolve_start_dir(&run_path);
    let (root, _manifest) = find_furthest_manifest(&start_dir)?;
    let workspace = resolve_workspace(&root)?;
    let graph = resolve_graph(&root)?;

    let profile = resolve_profile(options.release, options.profile.as_deref());
    let output_root = root.join("target").join(&profile).join("magnet").join("bench");
    std::fs::create_dir_all(&output_root)?;
    let graph_path = output_root.join("package-graph.json");
    let graph_value = crate::commands::graph::graph_to_value(&graph);
    let payload = json::to_string_pretty(&graph_value)?;
    std::fs::write(&graph_path, payload)?;

    let package = resolve_package(&workspace, options.package.as_deref())?;
    let fp_bin = resolve_fp_binary(options.fp_bin.as_deref())?;
    let build_options = build_option_args(&root)?;

    let output_dir = output_root.join(package.root_path.file_name().unwrap_or_default());
    std::fs::create_dir_all(&output_dir)?;
    let benches = collect_sources(&package.root_path.join("benches"), &["fp"])?;
    if benches.is_empty() {
        return Err("no benches found".to_string().into());
    }

    for bench in benches {
        let mut sources = collect_sources(&package.root_path.join("src"), &["fp"])?;
        sources.push(bench.clone());

        let mut command = Command::new(&fp_bin);
        command.arg("compile");
        for source in &sources {
            command.arg(source);
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

        let output = output_path_for_entry(&bench, &output_dir);
        let mut exec = Command::new(output);
        exec.args(&options.args);
        exec.current_dir(&package.root_path);
        exec.stdin(Stdio::inherit());
        exec.stdout(Stdio::inherit());
        exec.stderr(Stdio::inherit());
        let status = exec.status()?;
        if !status.success() {
            return Err("bench binary failed".to_string().into());
        }
    }

    Ok(())
}

fn resolve_package(
    workspace: &crate::models::WorkspaceModel,
    name: Option<&str>,
) -> crate::Result<crate::models::PackageModel> {
    if let Some(name) = name {
        return workspace
            .packages
            .iter()
            .find(|pkg| pkg.name == name)
            .cloned()
            .ok_or_else(|| format!("package '{name}' not found").into());
    }
    workspace
        .packages
        .first()
        .cloned()
        .ok_or_else(|| "no packages found".to_string().into())
}
