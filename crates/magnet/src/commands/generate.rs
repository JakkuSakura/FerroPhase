//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::generator::CargoGenerator;
use crate::manager::NexusManager;
use crate::models::WorkspaceModel;
use eyre::{Context, Result};
use std::path::Path;

/// Generate Cargo.toml files from Magnet.toml configuration
pub fn generate(config_path: &Path) -> Result<()> {
    let indent = "  ".repeat(0);
    println!(
        "{}Processing: {}",
        indent,
        config_path.canonicalize()?.display()
    );
    // Process the root configuration file and recursively generate all nested workspaces
    let nexus_manager = NexusManager::from_workspace(&config_path)?;

    // Load the configuration
    let workspace = WorkspaceModel::from_root_path(&config_path)?;
    let workspace_name = workspace.name.clone();

    // Create a generator
    let mut generator = CargoGenerator::new(nexus_manager.clone());

    // Generate all Cargo.toml files for this workspace
    generator.generate_all(&workspace_name).context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}
