//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::configs::{DependencyConfig, DependencyMap, DetailedDependency};
use crate::models::{NexusModel, PackageModel, WorkspaceModel};
use eyre::{Result, bail};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tracing::warn;

/// Nexus manager
#[derive(Debug, Clone)]
pub struct NexusManager {
    /// All workspaces in the nexus
    pub workspaces: HashMap<String, WorkspaceModel>,
    /// Path to the nexus root directory
    pub nexus_path: PathBuf,
    pub nexus_model: NexusModel,
}

impl NexusManager {
    /// Create a new nexus manager from a workspace
    pub fn from_workspace(workspace_path: &Path) -> Result<Self> {
        // Create the initial workspace
        let workspace = WorkspaceModel::from_root_path(workspace_path)?;
        let Some(nexus_path) = workspace.nexus_path.clone() else {
            bail!(
                "Nexus path not found in the workspace configuration: {}",
                workspace_path.display()
            )
        };

        // Create the manager
        let manager = Self::from_nexus(&nexus_path)?;

        Ok(manager)
    }
    pub fn from_nexus(nexus_path: &Path) -> Result<Self> {
        let nexus_model = NexusModel::from_root_path(nexus_path)?;

        // Create the manager
        let mut manager = Self {
            workspaces: HashMap::new(),
            nexus_path: nexus_model.root_path.clone(),
            nexus_model,
        };

        // Discover related workspaces during initialization
        manager.discover_workspaces()?;

        Ok(manager)
    }

    /// Get a workspace by name
    pub fn get_workspace(&self, workspace_name: &str) -> Option<&WorkspaceModel> {
        self.workspaces.get(workspace_name)
    }

    /// Get the root path of the specified workspace
    pub fn root_path(&self, workspace_name: &str) -> Option<&Path> {
        self.get_workspace(workspace_name).map(|w| w.root_path.as_path())
    }

    /// Discover related workspaces from a specific workspace
    /// This is now private as it's automatically called during initialization
    fn discover_workspaces(&mut self) -> Result<()> {
        for path in &self.nexus_model.members {
            let workspace_path = self.nexus_path.join(path);
            let workspace = WorkspaceModel::from_root_path(&workspace_path)?;
            self.workspaces.insert(workspace.name.clone(), workspace);
        }

        Ok(())
    }

    /// Get all workspaces
    pub fn get_all_workspaces(&self) -> Vec<&WorkspaceModel> {
        self.workspaces.values().collect()
    }

    pub fn get_all_dependencies(&self) -> DependencyMap {
        let mut all_dependencies = DependencyMap::new();

        // Get dependencies from all workspaces
        for workspace in self.workspaces.values() {
            all_dependencies.extend(workspace.dependencies.clone());
        }

        all_dependencies
    }

    /// Get dependencies for a specific workspace
    pub fn get_workspace_dependencies(&self, workspace_name: &str) -> DependencyMap {
        match self.get_workspace(workspace_name) {
            Some(ws) => ws.dependencies.clone(),
            None => DependencyMap::new(),
        }
    }

    /// Resolve a dependency
    pub fn resolve_dependency(
        &mut self,
        package: &PackageModel,
        name: &str,
        dep: &DependencyConfig,
    ) -> Result<DetailedDependency> {
        // Extract the detailed configuration
        let mut detailed_config = match dep {
            DependencyConfig::Simple(version) => DetailedDependency {
                version: Some(version.clone()),
                ..Default::default()
            },
            DependencyConfig::Detailed(detailed) => detailed.clone(),
        };

        // If nexus is set to true, try to find the dependency in the workspace
        if detailed_config.nexus == Some(true) {
            // Auto-discovery: try to find the dependency in any workspace
            let mut matching_crates = Vec::new();

            // Then check in other workspaces
            for workspace in self.get_all_workspaces() {
                if let Ok(package) = workspace.find_package(name) {
                    // TODO: double check crate name if different from package name
                    matching_crates.push(package.clone());
                }
            }

            if matching_crates.len() > 1 {
                bail!(
                    "Multiple matching crates found for dependency '{}': {:?}",
                    name,
                    matching_crates
                )
            } else if matching_crates.len() == 0 {
                warn!("No matching crates found for dependency '{}'", name);
            }
            detailed_config.path = Some(
                pathdiff::diff_paths(&matching_crates[0].root_path, &package.root_path)
                    .expect("Could not compute rel path"),
            );
            detailed_config.nexus = None;
            return Ok(detailed_config);
        }

        Ok(detailed_config)
    }
    pub fn resolve_package_dependencies(&mut self, package: &mut PackageModel) -> Result<()> {
        for (name, dep) in package.dependencies.clone() {
            // Resolve the dependency
            let resolved = self.resolve_dependency(package, &name, &dep);
            match resolved {
                Ok(detailed) => {
                    // Update the package dependencies
                    package
                        .dependencies
                        .insert(name.clone(), DependencyConfig::Detailed(detailed));
                }
                Err(err) => {
                    if let DependencyConfig::Detailed(detailed) = dep {
                        if detailed.optional == Some(true) {
                            warn!("Error resolving dependency '{}': {}", name, err);
                            warn!(
                                "This could be you don't have sufficient permissions to access the workspace"
                            );
                            package.dependencies.remove(&name);
                        }
                    }
                    Err(err)?
                }
            }
        }
        Ok(())
    }
}
