//! Command implementation for rendering package dependencies as a Graphviz graph.

use crate::models::{PackageGraph, PackageGraphOptions};
use crate::resolver::project::resolve_graph;
use eyre::Result;
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::{info, warn};

pub fn graph(config_path: &Path, output_path: Option<&Path>) -> Result<()> {
    let root = resolve_root(config_path);
    let offline = env_flag_enabled("MAGNET_OFFLINE");
    let options = PackageGraphOptions {
        offline,
        cache_dir: None,
        include_dependencies: true,
        include_dev_dependencies: false,
        include_build_dependencies: false,
        cargo_fetch: true,
        resolve_registry: !offline,
        allow_multiple_versions: true,
        use_lock: true,
        write_lock: true,
    };
    let graph = resolve_graph(config_path, &options)?;

    let output_path = output_path
        .map(|path| path.to_path_buf())
        .unwrap_or_else(|| root.join("target").join("dependencies.dot"));
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut file = std::fs::File::create(&output_path)?;
    write_dot(&graph, &mut file)?;
    render_graphviz(&output_path);

    Ok(())
}

fn write_dot(graph: &PackageGraph, out: &mut dyn Write) -> Result<()> {
    writeln!(out, "digraph magnet {{")?;
    writeln!(out, "    rankdir=LR;")?;
    writeln!(out, "    node [shape=box, fontname=\"Helvetica\"];" )?;

    let mut package_nodes = HashMap::new();
    for package in &graph.packages {
        let id = format!("pkg:{}", package.name);
        let label = format!("{}@{}", package.name, package.version);
        package_nodes.insert(package.name.clone(), (id, label));
    }

    let mut nodes: HashMap<String, String> = HashMap::new();
    for (id, label) in package_nodes.values() {
        nodes.insert(id.clone(), label.clone());
    }

    let package_names: HashSet<&str> = package_nodes.keys().map(|s| s.as_str()).collect();

    for package in &graph.packages {
        let from_id = package_nodes
            .get(&package.name)
            .map(|(id, _)| id.as_str())
            .unwrap_or("unknown");
        for dep in &package.dependencies {
            let dep_name = dep.package.as_deref().unwrap_or(dep.name.as_str());
            let version_label = dep
                .resolved_version
                .as_deref()
                .or(dep.version.as_deref())
                .unwrap_or("*");
            let (to_id, to_label) = if package_names.contains(dep_name) {
                let (id, label) = package_nodes
                    .get(dep_name)
                    .expect("package name should exist");
                (id.clone(), label.clone())
            } else {
                let id = format!("dep:{}@{}", dep_name, version_label);
                let label = format!("{}@{}", dep_name, version_label);
                (id, label)
            };
            nodes.entry(to_id.clone()).or_insert(to_label);
            let edge_label = dep.version.as_deref().unwrap_or("");
            if edge_label.is_empty() {
                writeln!(
                    out,
                    "    \"{}\" -> \"{}\";",
                    dot_escape(from_id),
                    dot_escape(&to_id)
                )?;
            } else {
                writeln!(
                    out,
                    "    \"{}\" -> \"{}\" [label=\"{}\"];",
                    dot_escape(from_id),
                    dot_escape(&to_id),
                    dot_escape(edge_label)
                )?;
            }
        }
    }

    for (id, label) in nodes {
        writeln!(
            out,
            "    \"{}\" [label=\"{}\"];",
            dot_escape(&id),
            dot_escape(&label)
        )?;
    }

    writeln!(out, "}}")?;
    Ok(())
}

fn dot_escape(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
        .unwrap_or(false)
}

fn resolve_root(config_path: &Path) -> PathBuf {
    if config_path.is_dir() {
        return config_path.to_path_buf();
    }
    config_path
        .parent()
        .map(|path| path.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."))
}

fn render_graphviz(dot_path: &Path) {
    let output_path = dot_path.with_extension("png");
    let status = Command::new("dot")
        .arg("-Tpng")
        .arg(dot_path)
        .arg("-o")
        .arg(&output_path)
        .status();
    match status {
        Ok(status) if status.success() => {
            info!("wrote graphviz output to {}", output_path.display());
        }
        Ok(status) => {
            warn!(
                "graphviz dot failed (exit code {:?}); output: {}",
                status.code(),
                output_path.display()
            );
        }
        Err(err) => {
            warn!("graphviz dot not available: {}", err);
        }
    }
}
