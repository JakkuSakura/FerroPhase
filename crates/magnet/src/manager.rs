//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::configs::{DependencyConfig, DependencyMap, DetailedDependency};
use crate::models::{ManifestModel, PackageModel, WorkspaceModel};
use crate::utils::find_furthest_manifest;
use eyre::{Result, bail};
use std::path::{Path, PathBuf};
use tracing::warn;

/// Nexus manager
#[derive(Debug, Clone)]
pub struct ManifestManager {
    /// Path to the nexus root directory
    pub root_path: PathBuf,
    pub root_manifest: ManifestModel,
}

impl ManifestManager {
    pub fn from_dir(path: &Path) -> Result<Self> {
        let path = path.canonicalize()?;
        let (root_path, model) = find_furthest_manifest(&path)?;

        // Create the manager
        let manager = Self {
            root_path,
            root_manifest: model,
        };

        Ok(manager)
    }

    /// Get a workspace by name
    pub fn get_workspace(&self, workspace_name: &str) -> Option<WorkspaceModel> {
        let workspaces = self.root_manifest.list_workspaces().ok()?;
        for workspace in workspaces {
            if workspace.name == workspace_name {
                return Some(workspace.clone());
            }
        }
        None
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
            for workspace in self.root_manifest.list_workspaces()? {
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
