//! Command implementation for initializing Magnet.toml files

use crate::configs::{MagnetConfig, MagnetConfigType};
use anyhow::{Context, Result};
use std::path::Path;

/// Initialize a new Magnet.toml file at the specified path
pub fn init(path: &Path) -> Result<()> {
    // Create the directory if it doesn't exist
    if !path.exists() {
        std::fs::create_dir_all(path)
            .context(format!("Failed to create directory at {}", path.display()))?;
    }

    // Detect existing workspace structure
    let target_path = if path.is_dir() {
        path.join("Magnet.toml")
    } else {
        path.to_path_buf()
    };

    // Auto-detect the configuration type based on directory structure
    let config_type = detect_config_type_from_path(path);

    // Create an appropriate minimal configuration
    let config = create_minimal_config(path, config_type)?;

    // Write the configuration to file
    config.save_to_file(&target_path).context(format!(
        "Failed to write Magnet.toml to {}",
        target_path.display()
    ))?;

    println!(
        "Created new {} Magnet.toml at {}",
        match config_type {
            MagnetConfigType::Nexus => "nexus",
            MagnetConfigType::Workspace => "workspace",
            MagnetConfigType::Package => "package",
        },
        target_path.display()
    );

    Ok(())
}

/// Detect the configuration type based on the path
fn detect_config_type_from_path(path: &Path) -> MagnetConfigType {
    // Get normalized path for analysis
    let dir_path = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    };

    // Extract directory components
    let components: Vec<&str> = dir_path
        .components()
        .filter_map(|c| {
            if let std::path::Component::Normal(name) = c {
                name.to_str()
            } else {
                None
            }
        })
        .collect();

    // Check for src directory (indicates package)
    let has_src_dir = dir_path.join("src").is_dir();

    // Check for crates directory (indicates workspace or nexus)
    let has_crates_dir = dir_path.join("crates").is_dir();

    // Check if in a crates directory (indicates package)
    let is_in_crates_dir =
        components.contains(&"crates") && components.last().map_or(false, |last| *last != "crates");

    // Check if in nexus directory (indicates nexus)
    let is_in_nexus_dir = components.contains(&"nexus");

    // Logic to determine configuration type
    if is_in_nexus_dir || (has_crates_dir && !is_in_crates_dir) {
        MagnetConfigType::Nexus
    } else if has_crates_dir || (!is_in_crates_dir && !has_src_dir) {
        MagnetConfigType::Workspace
    } else {
        MagnetConfigType::Package
    }
}

/// Create a minimal configuration of the specified type
fn create_minimal_config(
    _path: &Path,
    config_type: MagnetConfigType,
) -> Result<MagnetConfig> {
    // Create a minimal config with the specified type
    let config = MagnetConfig::new_with_type(config_type);

    Ok(config)
}