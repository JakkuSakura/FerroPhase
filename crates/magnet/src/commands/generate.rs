//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::generator::CargoGenerator;
use crate::manager::ManifestManager;
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
    let nexus_manager = ManifestManager::from_dir(&&config_path)?;

    // Load the configuration
    let workspace = WorkspaceModel::from_dir(&config_path)?;

    // Create a generator
    let mut generator = CargoGenerator::new(nexus_manager.clone());

    // Generate all Cargo.toml files for this workspace
    generator.generate_all(&workspace).context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}
