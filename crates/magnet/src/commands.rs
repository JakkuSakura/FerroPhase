//! Command implementations for the magnet CLI

use anyhow::{Context, Result};
use std::path::Path;

use crate::config::MagnetConfig;
use crate::workspace::WorkspaceManager;

/// Initialize a new Magnet.toml file at the specified path
pub fn init(path: &Path) -> Result<()> {
    // Create a new default configuration
    let config = MagnetConfig::new();

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

    // Write the configuration to file
    config.save_to_file(&target_path).context(format!(
        "Failed to write Magnet.toml to {}",
        target_path.display()
    ))?;

    println!("Created new Magnet.toml at {}", target_path.display());

    Ok(())
}

/// Generate Cargo.toml files from Magnet.toml configuration
pub fn generate(config_path: &Path) -> Result<()> {
    // Resolve the config path
    let config_path = if config_path.is_relative() {
        std::env::current_dir()?.join(config_path)
    } else {
        config_path.to_path_buf()
    };

    // Make sure the config file exists
    if !config_path.exists() {
        return Err(anyhow::anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let mut workspace_manager = WorkspaceManager::new(config.clone(), base_dir.clone())?;

    // Discover related workspaces
    workspace_manager.discover_related_workspaces()?;

    // Create a dependency resolver
    let resolver = crate::resolver::DependencyResolver::new(
        workspace_manager.primary_workspace.clone(),
        workspace_manager
            .related_workspaces
            .values()
            .cloned()
            .collect(),
    );

    // Create a generator
    let mut generator = crate::generator::CargoGenerator::new(workspace_manager, resolver);

    // Generate all Cargo.toml files
    generator.generate_all()?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}

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
        return Err(anyhow::anyhow!(
            "Magnet.toml not found at {}",
            config_path.display()
        ));
    }

    // Load the configuration
    let config = MagnetConfig::from_file(&config_path)?;

    // Get the base directory (where Magnet.toml is located)
    let base_dir = config_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Create a workspace manager
    let workspace_manager = WorkspaceManager::new(config, base_dir)?;

    // Count the crates found
    let crates = workspace_manager.get_all_crates();

    if crates.is_empty() {
        println!(
            "Warning: No workspace members found. Check your 'workspace.members' configuration."
        );
    } else {
        println!("Found {} workspace members", crates.len());
    }

    // Check if there's a mismatch between Magnet.toml and Cargo.toml
    let cargo_toml_path = workspace_manager.get_root_path().join("Cargo.toml");

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

                let magnet_members = &workspace_manager.primary_workspace.config.workspace.members;

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
        return Err(anyhow::anyhow!(
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
