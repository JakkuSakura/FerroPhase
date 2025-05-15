//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::configs::MagnetConfig;
use crate::generator::CargoGenerator;
use crate::manager::WorkspaceManager;
use crate::resolver::DependencyResolver;
use eyre::{Context, Result, eyre};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

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
        return Err(eyre!("Magnet.toml not found at {}", config_path.display()));
    }
    let config = MagnetConfig::from_file(&config_path).context(format!(
        "Failed to load Magnet.toml at {}",
        config_path.display()
    ))?;
    // Process the root configuration file and recursively generate all nested workspaces
    let mut workspace_manager =
        WorkspaceManager::new(config.clone(), &config_path).context(format!(
            "Failed to create workspace manager for {}",
            config_path.display()
        ))?;

    // Discover related workspaces
    workspace_manager
        .discover_related_workspaces()
        .context("Failed to discover related workspaces")?;

    let mut processed_paths = HashSet::new(); // Create a workspace manager for this config
    generate_recursively(
        &mut workspace_manager,
        &config_path,
        &mut processed_paths,
        0,
    )?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}

/// Recursively generate Cargo.toml files for a workspace and all its nested workspaces
fn generate_recursively(
    workspace_manager: &mut WorkspaceManager,
    config_path: &Path,
    processed_paths: &mut HashSet<PathBuf>,
    depth: usize,
) -> Result<()> {
    // Add this config path to the processed set
    processed_paths.insert(config_path.to_path_buf());

    // Create indentation for better log readability
    let indent = "  ".repeat(depth);

    // Load the configuration
    let config = MagnetConfig::from_file(config_path).context(format!(
        "Failed to load Magnet.toml at {}",
        config_path.display()
    ))?;

    println!("{}Processing: {}", indent, config_path.display());

    // Create a dependency resolver
    let resolver = DependencyResolver::new(
        workspace_manager.primary_workspace.clone(),
        workspace_manager
            .related_workspaces
            .values()
            .cloned()
            .collect(),
    );

    // Create a generator
    let mut generator = CargoGenerator::new(workspace_manager.clone(), resolver);

    // Generate all Cargo.toml files for this workspace
    generator.generate_all().context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    // Process directly related workspaces from workspace.search_paths
    if let Some(search_paths) = &config.workspace.search_paths {
        for (name, path) in search_paths {
            let nested_config_path = path.join("Magnet.toml");

            if nested_config_path.exists() && !processed_paths.contains(&nested_config_path) {
                println!(
                    "{}Processing related workspace: {} at {}",
                    indent,
                    name,
                    path.display()
                );
                generate_recursively(
                    workspace_manager,
                    &nested_config_path,
                    processed_paths,
                    depth + 1,
                )?;
            }
        }
    }

    // 3. Process each package that has its own Magnet.toml
    for crate_info in workspace_manager.get_all_crates() {
        if let Some(magnet_path) = &crate_info.magnet_toml_path {
            if magnet_path.exists() && !processed_paths.contains(magnet_path) {
                println!(
                    "{}Processing package: {} at {}",
                    indent,
                    crate_info.name,
                    crate_info.path.display()
                );
                generate_recursively(workspace_manager, magnet_path, processed_paths, depth + 1)?;
            }
        }
    }
    
    Ok(())
}
