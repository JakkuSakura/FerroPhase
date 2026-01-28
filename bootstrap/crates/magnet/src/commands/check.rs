use std::path::PathBuf;

use crate::configs::ManifestConfig;
use crate::utils::{read_toml_file, map_get_string_list, get_table};

pub fn run(args: &[String]) -> crate::Result<()> {
    let mut config_path: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--config" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing --config path".to_string().into());
                };
                config_path = Some(PathBuf::from(value));
                i += 2;
            }
            value => {
                if config_path.is_none() {
                    config_path = Some(PathBuf::from(value));
                    i += 1;
                } else {
                    return Err(format!("unexpected argument: {value}").into());
                }
            }
        }
    }

    let config_path = config_path.unwrap_or_else(|| PathBuf::from("Magnet.toml"));
    let config = ManifestConfig::from_file(&config_path)?;
    let root = config_path
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    let cargo_path = root.join("Cargo.toml");
    if !cargo_path.exists() {
        return Err("Cargo.toml not found; run magnet generate".to_string().into());
    }
    let cargo = read_toml_file(&cargo_path)?;
    let cargo_members = if let Some(workspace) = get_table(&cargo, "workspace") {
        map_get_string_list(workspace, "members")
    } else {
        Vec::new()
    };
    let magnet_members = config
        .workspace
        .as_ref()
        .map(|ws| ws.members.clone())
        .unwrap_or_default();

    if cargo_members != magnet_members {
        return Err("workspace members differ between Magnet.toml and Cargo.toml".to_string().into());
    }
    Ok(())
}
