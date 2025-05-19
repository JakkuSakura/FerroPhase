//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::models::{DependencyModel, DependencyModelMap, ManifestModel, PackageModel, WorkspaceModel};
use crate::utils::{diff_path, find_furthest_manifest};
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
    pub fn get_workspace_dependencies(&self, workspace_name: &str) -> DependencyModelMap {
        match self.get_workspace(workspace_name) {
            Some(ws) => ws.dependencies.clone(),
            None => DependencyModelMap::new(),
        }
    }

    /// Resolve a dependency
    pub fn resolve_dependency(
        &mut self,
        package: &PackageModel,
        name: &str,
        dep: &DependencyModel,
    ) -> Result<DependencyModel> {
        let mut dep = dep.clone();
        // If nexus is set to true, try to find the dependency in the nexus
        if dep.nexus() {
            // Auto-discovery: try to find the dependency in any workspace
            let mut matching_crates = Vec::new();

            // Then check in other workspaces
            for pkg in self.root_manifest.list_packages()? {
                if pkg.name == name {
                    matching_crates.push(pkg.clone());
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
                return Ok(dep);
            }
            dep.path = Some(diff_path(&package.root_path, &matching_crates[0].root_path));
            dep.nexus = None;
            dep.workspace = None;
            return Ok(dep);
        }

        if dep.workspace() {
            let mut matching_crates = Vec::new();

            // Then check in other workspaces
            for workspace in self.root_manifest.list_workspaces()? {
                let Some(dep1) = workspace.find_dependency(name) else {
                    continue;
                };
                matching_crates.push((workspace, dep1.clone()));
            }

            if matching_crates.len() > 1 {
                bail!(
                    "Multiple matching crates found for dependency '{}': {:?}",
                    name,
                    matching_crates
                )
            } else if matching_crates.len() == 0 {
                warn!("No matching crates found for dependency '{}'", name);
                return Ok(dep);
            }
            let Some((workspace, dep1)) = matching_crates.pop() else {
                bail!("No matching crates found for dependency '{}'", name);
            };
            let mut dep_path = dep1.path.clone().unwrap();
            if dep_path.is_absolute() {
                dep.path = Some(dep_path);
            } else {
                dep_path = workspace.root_path.join(dep_path);
                dep.path = Some(diff_path(&package.root_path, &dep_path));
            }

            dep.nexus = None;
            dep.workspace = None;
            return Ok(dep);
        }
        Ok(dep)
    }
    pub fn resolve_package_dependencies(&mut self, package: &mut PackageModel) -> Result<()> {
        for (name, dep) in package.dependencies.clone() {
            // Resolve the dependency
            let resolved = self.resolve_dependency(package, &name, &dep);
            match resolved {
                Ok(detailed) => {
                    // Update the package dependencies
                    package.dependencies.insert(name.clone(), detailed);
                }
                Err(err) => {
                    if dep.optional() {
                        warn!("Error resolving dependency '{}': {}", name, err);
                        warn!(
                            "This could be you don't have sufficient permissions to access the workspace"
                        );
                        package.dependencies.remove(&name);
                    }
                    Err(err)?
                }
            }
        }
        Ok(())
    }
}
