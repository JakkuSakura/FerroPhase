//! Command implementation for listing crates in a workspace

use crate::configs::MagnetConfig;
use crate::workspace_manager::WorkspaceManager;
use anyhow::{anyhow, Result};
use std::path::Path;

/// List all crates in the workspace
pub fn list(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let mut workspace_manager = WorkspaceManager::new(config, base_dir)?;

    // Discover related workspaces
    workspace_manager.discover_related_workspaces()?;

    // Get all crates in the workspace
    let all_crates = workspace_manager.get_all_crates();

    println!("\nCrates in workspace:");
    println!("{:<30} {:<15} {:<50}", "NAME", "VERSION", "PATH");
    println!("{:-<30} {:-<15} {:-<50}", "", "", "");

    for crate_info in all_crates {
        let crate_path = crate_info.cargo_toml_path.parent().unwrap_or(Path::new(""));
        let rel_path = pathdiff::diff_paths(crate_path, &workspace_manager.get_root_path())
            .unwrap_or_else(|| crate_path.to_path_buf());

        println!(
            "{:<30} {:<15} {:<50}",
            crate_info.name,
            crate_info.version.unwrap_or_else(|| "unknown".to_string()),
            rel_path.display()
        );
    }

    // Count external workspaces if any
    let external_workspaces = workspace_manager.get_external_workspaces();
    if !external_workspaces.is_empty() {
        println!("\nExternal workspaces: {}", external_workspaces.len());
        for workspace in external_workspaces {
            println!("  - {}", workspace.get_root_path().display());
        }
    }

    Ok(())
}