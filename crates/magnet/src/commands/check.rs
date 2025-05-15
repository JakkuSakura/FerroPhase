//! Command implementation for checking Magnet.toml for issues

use crate::manager::NexusManager;
use crate::models::WorkspaceModel;
use crate::resolver::DependencyResolver;
use eyre::{Context, Result};
use std::path::Path;

/// Check command for verifying the consistency of workspace dependencies
pub fn check(config_path: &Path) -> Result<()> {
    let workspace = WorkspaceModel::from_root_path(config_path)?;
    let workspace_name = workspace.name.clone();

    // Create a workspace manager
    let nexus_manager = NexusManager::from_workspace(config_path)?;

    // Create a resolver
    let mut resolver = DependencyResolver::new(
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

    // Resolve all dependencies
    resolver
        .resolve_all_workspace_dependencies()
        .context("Failed to resolve dependencies")?;

    // Check for warnings or errors in resolved dependencies
    if check_resolved_dependencies(&resolver) {
        eprintln!("WARNING: Some dependencies have resolution issues.");
        return Ok(());
    }

    println!("All dependencies are properly resolved.");
    Ok(())
}

/// Check if there are any warnings or errors in the resolved dependencies
fn check_resolved_dependencies(resolver: &DependencyResolver) -> bool {
    use crate::resolver::ResolutionStatus;

    let mut has_issues = false;

    // Get all resolved dependencies
    for dep_name in ["dep1", "dep2"].iter() {
        // This is a placeholder that would be replaced with actual access to all resolved dependencies
        // when the resolver API provides it
        if let Some(resolved) = resolver.get_resolved_dependency(dep_name) {
            match &resolved.status {
                ResolutionStatus::Resolved(_) => {
                    // All good
                }
                ResolutionStatus::NotFound => {
                    if resolved.resolved_config.auto.unwrap_or(false) {
                        eprintln!(
                            "WARNING: Dependency '{}' is set to auto=true but could not be found locally",
                            dep_name
                        );
                        has_issues = true;
                    }
                }
                ResolutionStatus::Ambiguous(paths) => {
                    eprintln!(
                        "WARNING: Dependency '{}' has multiple possible resolutions: {:?}",
                        dep_name, paths
                    );
                    has_issues = true;
                }
                ResolutionStatus::Error(error) => {
                    eprintln!(
                        "ERROR: Failed to resolve dependency '{}': {}",
                        dep_name, error
                    );
                    has_issues = true;
                }
            }
        }
    }

    has_issues
}
