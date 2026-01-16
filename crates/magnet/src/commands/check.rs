//! Command implementation for checking Magnet.toml for issues

use crate::manager::ManifestManager;
use crate::models::{PackageGraphOptions, WorkspaceModel};
use crate::resolver::project::resolve_graph;
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

    let offline = env_flag_enabled("MAGNET_OFFLINE");
    let graph_options = PackageGraphOptions {
        offline,
        cache_dir: None,
        include_dependencies: false,
        include_dev_dependencies: false,
        include_build_dependencies: false,
        cargo_fetch: false,
        resolve_registry: !offline,
        allow_multiple_versions: false,
        use_lock: true,
        refresh_index: false,
        write_lock: true,
        target: None,
    };
    let _graph = resolve_graph(&workspace.root_path, &graph_options)?;

    info!("All package dependencies are properly resolved.");
    Ok(())
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
        .unwrap_or(false)
}
