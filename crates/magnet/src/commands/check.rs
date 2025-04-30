//! Command implementation for checking Magnet.toml for issues

use crate::configs::MagnetConfig;
use crate::manager::WorkspaceManager;
use eyre::{Context, Result, eyre};
use std::path::Path;

/// Check Magnet.toml for issues
pub fn check(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(eyre!("Magnet.toml not found at {}", config_path.display()));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let workspace_manager = WorkspaceManager::new(config, &base_dir)?;

    // Count the crates found
    let crates = workspace_manager.get_all_crates();

    if crates.is_empty() {
        println!(
            "Warning: No workspace members found. Check your 'workspace.members' configuration."
        );

        // Show more detailed information to help with debugging
        println!("\nDetailed information for troubleshooting:");
        println!(
            "  Workspace root: {}",
            workspace_manager.root_path().display()
        );
        println!("  Patterns searched:");
        for pattern in &workspace_manager.primary_workspace.members {
            let full_pattern = format!(
                "{}/{}/Cargo.toml",
                workspace_manager.root_path().display(),
                pattern
            );
            println!("    - {}", full_pattern);
        }

        if workspace_manager.primary_workspace.members.is_empty() {
            println!(
                "\nYour 'workspace.members' array is empty. You need to add glob patterns to find your crates."
            );
            println!(
                "Example: Add `\"crates/*\"` to workspace.members in Magnet.toml if your crates are in a 'crates' directory."
            );
        } else {
            println!("\nPossible reasons for not finding any workspace members:");
            println!(
                "  1. The glob patterns in 'workspace.members' don't match any directories containing Cargo.toml files"
            );
            println!("  2. The crates directory doesn't exist or is in a different location");
            println!("  3. Your crates don't have Cargo.toml files");
            println!("\nTry running this command to see all directories in the workspace root:");
            println!("  ls -la {}", workspace_manager.root_path().display());
        }
    } else {
        println!("Found {} workspace members", crates.len());
    }

    // Check if there's a mismatch between Magnet.toml and Cargo.toml
    let cargo_toml_path = workspace_manager.root_path().join("Cargo.toml");

    if cargo_toml_path.exists() {
        let cargo_content = std::fs::read_to_string(&cargo_toml_path).context(format!(
            "Failed to read Cargo.toml at {}",
            cargo_toml_path.display()
        ))?;

        let cargo_doc: toml::Table = toml::from_str(&cargo_content).context(format!(
            "Failed to parse Cargo.toml at {}",
            cargo_toml_path.display()
        ))?;

        if let Some(toml::Value::Table(workspace)) = cargo_doc.get("workspace") {
            // Compare workspace members
            if let Some(toml::Value::Array(members)) = workspace.get("members") {
                let cargo_members: Vec<String> = members
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| s.to_string())
                    .collect();

                let magnet_members = &workspace_manager.primary_workspace.members;

                if &cargo_members != magnet_members {
                    println!(
                        "Warning: Mismatch between Cargo.toml and Magnet.toml workspace members"
                    );
                    println!("Run 'magnet generate' to update Cargo.toml");
                }
            }
        } else {
            println!("Warning: Cargo.toml exists but doesn't define a workspace");
        }
    }

    println!("Configuration check completed successfully");

    Ok(())
}
