use std::path::PathBuf;

use crate::configs::ManifestConfig;
use crate::generator::{generate_workspace_cargo_toml, workspace_output_path};

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
    let output = workspace_output_path(&root);
    generate_workspace_cargo_toml(&config, &output)
}
