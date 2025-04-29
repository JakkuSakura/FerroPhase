//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::configs::MagnetConfig;
use crate::generator::CargoGenerator;
use crate::resolver::DependencyResolver;
use crate::workspace_manager::WorkspaceManager;
use anyhow::{anyhow, Result};
use std::path::Path;

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
    let mut workspace_manager = WorkspaceManager::new(config.clone(), base_dir.clone())?;

    // Discover related workspaces
    workspace_manager.discover_related_workspaces()?;

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
    let mut generator = CargoGenerator::new(workspace_manager, resolver);

    // Generate all Cargo.toml files
    generator.generate_all()?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}