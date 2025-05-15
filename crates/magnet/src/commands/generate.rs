//! Command implementation for generating Cargo.toml files from Magnet.toml

use crate::generator::CargoGenerator;
use crate::manager::NexusManager;
use crate::models::WorkspaceModel;
use crate::resolver::DependencyResolver;
use eyre::{Context, Result};
use std::collections::HashSet;
use std::path::Path;

/// Generate Cargo.toml files from Magnet.toml configuration
pub fn generate(config_path: &Path) -> Result<()> {
    // Process the root configuration file and recursively generate all nested workspaces
    let nexus_manager = NexusManager::from_workspace(&config_path)?;

    let mut processed_paths = HashSet::new(); // Create a workspace manager for this config
    // Add this config path to the processed set
    processed_paths.insert(config_path.to_path_buf());

    // Create indentation for better log readability
    let indent = "  ".repeat(0);

    // Load the configuration
    println!("{}Processing: {}", indent, config_path.display());
    let workspace = WorkspaceModel::from_root_path(&config_path)?;
    let workspace_name = workspace.name.clone();

    // Create a dependency resolver
    let resolver = DependencyResolver::new(
        nexus_manager
            .get_workspace(&workspace_name)
            .cloned()
            .unwrap(),
        nexus_manager
            .get_other_workspaces(&workspace_name)
            .into_iter()
            .cloned()
            .collect(),
    );

    // Create a generator
    let mut generator = CargoGenerator::new(nexus_manager.clone(), resolver);

    // Generate all Cargo.toml files for this workspace
    generator.generate_all(&workspace_name).context(format!(
        "Failed to generate Cargo.toml files for {}",
        config_path.display()
    ))?;

    println!("Cargo.toml files updated successfully");

    Ok(())
}
