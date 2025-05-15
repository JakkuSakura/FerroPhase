//! Command implementation for checking Magnet.toml for issues

use crate::manager::NexusManager;
use crate::models::{PackageModel, WorkspaceModel};
use eyre::Result;
use std::path::Path;

/// Check command for verifying the consistency of workspace dependencies
pub fn check(config_path: &Path) -> Result<()> {
    let workspace = WorkspaceModel::from_root_path(config_path)?;
    
    // Create a workspace manager
    let mut nexus_manager = NexusManager::from_workspace(config_path)?;
    for package in workspace.list_packages()? {
        let mut package_model = PackageModel::from_root_path(&package)?;
        nexus_manager.resolve_package_dependencies(&mut package_model)?;
    }

    println!("All package dependencies are properly resolved.");
    Ok(())
}
