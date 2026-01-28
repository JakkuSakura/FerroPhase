use std::path::{Path, PathBuf};

use fp_core::ast::{Value, ValueMap};

use crate::configs::{ManifestConfig, WorkspaceConfig};
use crate::generator::generate_workspace_cargo_toml;
use crate::utils::{find_furthest_manifest, read_toml_file, to_string_list, write_toml_file};

pub fn run(args: &[String]) -> crate::Result<()> {
    let mut path: Option<PathBuf> = None;
    let mut from_cargo = false;
    for arg in args {
        if arg == "--from-cargo" {
            from_cargo = true;
        } else if path.is_none() {
            path = Some(PathBuf::from(arg));
        } else {
            return Err(format!("unexpected argument: {arg}").into());
        }
    }

    let target = path.unwrap_or_else(|| PathBuf::from("."));
    if from_cargo {
        init_from_cargo(&target)
    } else {
        init_empty(&target)
    }
}

fn init_empty(path: &Path) -> crate::Result<()> {
    let root = if path.is_file() {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        path.to_path_buf()
    };
    let magnet_path = root.join("Magnet.toml");
    let mut entries = Vec::new();
    let workspace = ValueMap::from_pairs([(
        Value::string("members".to_string()),
        to_string_list(&vec!["crates/*".to_string()]),
    )]);
    entries.push((Value::string("workspace".to_string()), Value::Map(workspace)));
    let value = Value::Map(ValueMap::from_pairs(entries));
    write_toml_file(&magnet_path, &value)
}

fn init_from_cargo(path: &Path) -> crate::Result<()> {
    let (root, manifest) = find_furthest_manifest(path)?;
    let cargo_path = if manifest.file_name().and_then(|name| name.to_str()) == Some("Cargo.toml") {
        manifest
    } else {
        root.join("Cargo.toml")
    };
    let cargo_value = read_toml_file(&cargo_path)?;
    let mut config = ManifestConfig::default();
    if let Some(workspace_table) = crate::utils::get_table(&cargo_value, "workspace") {
        config.workspace = Some(WorkspaceConfig {
            members: crate::utils::map_get_string_list(workspace_table, "members"),
            exclude: crate::utils::map_get_string_list(workspace_table, "exclude"),
        });
    }
    let magnet_path = root.join("Magnet.toml");
    let mut entries = Vec::new();
    if let Some(workspace) = &config.workspace {
        let workspace_map = ValueMap::from_pairs([
            (
                Value::string("members".to_string()),
                to_string_list(&workspace.members),
            ),
            (
                Value::string("exclude".to_string()),
                to_string_list(&workspace.exclude),
            ),
        ]);
        entries.push((Value::string("workspace".to_string()), Value::Map(workspace_map)));
    }
    let value = Value::Map(ValueMap::from_pairs(entries));
    write_toml_file(&magnet_path, &value)?;
    if !cargo_path.exists() {
        generate_workspace_cargo_toml(&config, &cargo_path)?;
    }
    Ok(())
}
