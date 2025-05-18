//! Command implementation for checking Magnet.toml for issues

use crate::manager::ManifestManager;
use crate::models::WorkspaceModel;
use eyre::Result;
use std::path::Path;
use tracing::info;

/// Check command for verifying the consistency of workspace dependencies
pub fn check(config_path: &Path) -> Result<()> {
    let workspace = WorkspaceModel::from_dir(config_path)?;

    // Create a workspace manager
    let mut nexus_manager = ManifestManager::from_dir(&config_path)?;
    for mut package in workspace.list_packages()? {
        nexus_manager.resolve_package_dependencies(&mut package)?;
    }

    info!("All package dependencies are properly resolved.");
    Ok(())
}
