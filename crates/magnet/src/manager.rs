//! WorkspaceModel management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::configs::DependencyMap;
use crate::models::{CrateModel, WorkspaceModel};
use crate::MagnetConfig;
use eyre::{bail, Result};
use std::collections::HashMap;
use std::path::Path;

// TODO: rename it to NexusManager
/// WorkspaceModel manager
#[derive(Debug, Clone)]
pub struct WorkspaceManager {
    /// Primary workspace
    pub primary_workspace: WorkspaceModel,
    /// Related workspaces
    pub related_workspaces: HashMap<String, WorkspaceModel>,
}

impl WorkspaceManager {
    /// Create a new workspace manager
    pub fn new(config: MagnetConfig, workspace_config_path: &Path) -> Result<Self> {
        if !workspace_config_path.exists() {
            bail!(
                "Root path doesn't exist in the current directory: {}",
                workspace_config_path.display()
            )
        }
        if workspace_config_path.file_name() != Some("Cargo.toml".as_ref())
            && workspace_config_path.file_name() != Some("Magnet.toml".as_ref())
        {
            bail!(
                "Root path point to Cargo.toml or Magnet.toml: {}",
                workspace_config_path.display()
            )
        }

        // Create the primary workspace
        let primary_workspace = WorkspaceModel::from_config_file(&config, workspace_config_path)?;
        // TODO: resolve crates & packages
        Ok(Self {
            primary_workspace,
            related_workspaces: HashMap::new(),
        })
    }

    /// Get the root path of the primary workspace
    pub fn root_path(&self) -> &Path {
        self.primary_workspace.root_path()
    }

    /// Discover related workspaces
    pub fn discover_related_workspaces(&mut self) -> Result<()> {
        // Clear existing related workspaces
        self.related_workspaces.clear();

        // Get search paths from config
        let model = &self.primary_workspace;
        let search_paths = model.search_paths.clone();

        if !search_paths.is_empty() {
            // Resolve and load each related workspace
            for (name, rel_path) in search_paths {
                let abs_path = if rel_path.is_absolute() {
                    rel_path
                } else {
                    self.primary_workspace
                        .source_file
                        .as_ref()
                        .unwrap()
                        .join(&rel_path)
                };

                // Check if the path exists
                if !abs_path.exists() {
                    eprintln!(
                        "Warning: Related workspace path does not exist: {}",
                        abs_path.display()
                    );
                    continue;
                }

                // Check for Magnet.toml
                let magnet_toml_path = abs_path.join("Magnet.toml");
                if !magnet_toml_path.exists() {
                    eprintln!(
                        "Warning: No Magnet.toml found in related workspace: {}",
                        abs_path.display()
                    );
                    continue;
                }

                // Load the configuration
                let config = match MagnetConfig::from_file(&magnet_toml_path) {
                    Ok(config) => config,
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to load Magnet.toml from related workspace {}: {}",
                            abs_path.display(),
                            e
                        );
                        continue;
                    }
                };

                // Create the workspace
                match WorkspaceModel::from_config_file(&config, abs_path.as_ref()) {
                    Ok(workspace) => {
                        self.related_workspaces.insert(name, workspace);
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to load related workspace {}: {}",
                            abs_path.display(),
                            e
                        );
                    }
                }
            }
        }

        Ok(())
    }
    pub fn get_package_path(&self, name: &str) -> Option<&Path> {
        // Check the primary workspace first
        if let Some(crate_info) = self.primary_workspace.find_crate(name) {
            return Some(&crate_info.path);
        }

        // Then check related workspaces
        for workspace in self.related_workspaces.values() {
            if let Some(crate_info) = workspace.find_crate(name) {
                return Some(&crate_info.path);
            }
        }

        None
    }

    /// Get all crates across all workspaces
    pub fn get_all_crates(&self) -> Vec<CrateModel> {
        let mut all_crates = self.primary_workspace.crates.clone();

        for workspace in self.related_workspaces.values() {
            all_crates.extend(workspace.crates.clone());
        }

        all_crates
    }

    /// Find a crate by name across all workspaces
    pub fn find_crate(&self, name: &str) -> Option<CrateModel> {
        // First look in the primary workspace
        if let Some(crate_info) = self.primary_workspace.find_crate(name) {
            return Some(crate_info.clone());
        }

        // Then look in related workspaces
        for workspace in self.related_workspaces.values() {
            if let Some(crate_info) = workspace.find_crate(name) {
                return Some(crate_info.clone());
            }
        }

        None
    }

    /// Get all external workspaces
    pub fn get_external_workspaces(&self) -> Vec<&WorkspaceModel> {
        self.related_workspaces.values().collect()
    }
    pub fn get_all_dependencies(&self) -> DependencyMap {
        let mut all_dependencies = DependencyMap::new();

        // Get dependencies from the primary workspace
        all_dependencies.extend(self.primary_workspace.dependencies.clone());

        // Get dependencies from related workspaces
        for workspace in self.related_workspaces.values() {
            all_dependencies.extend(workspace.dependencies.clone());
        }

        all_dependencies
    }
}
