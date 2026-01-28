use std::path::{Path, PathBuf};
use std::fs;

use fp_core::formats::json;

use crate::resolver::project::resolve_graph;

pub fn run(args: &[String]) -> crate::Result<()> {
    let mut path: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let mut crates_dir: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--output" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing --output".to_string().into());
                };
                output = Some(PathBuf::from(value));
                i += 2;
            }
            "--crates" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing --crates".to_string().into());
                };
                crates_dir = Some(PathBuf::from(value));
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

    let root = path.unwrap_or_else(|| PathBuf::from("."));
    let graph = resolve_graph(&root)?;
    let output = output.unwrap_or_else(|| root.join("export"));
    fs::create_dir_all(&output)?;
    let graph_value = crate::commands::graph::graph_to_value(&graph);
    let payload = json::to_string_pretty(&graph_value)?;
    fs::write(output.join("package-graph.json"), payload)?;

    if let Some(crates_dir) = crates_dir {
        fs::create_dir_all(&crates_dir)?;
        for package in graph.packages() {
            let src = package.root.to_path_buf();
            let dst = crates_dir.join(&package.name);
            copy_dir_recursive(&src, &dst)?;
        }
    }

    Ok(())
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> crate::Result<()> {
    if !dst.exists() {
        fs::create_dir_all(dst)?;
    }
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let target = dst.join(entry.file_name());
        if path.is_dir() {
            copy_dir_recursive(&path, &target)?;
        } else {
            fs::copy(&path, &target)?;
        }
    }
    Ok(())
}
