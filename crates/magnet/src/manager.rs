//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::MagnetConfig;
use crate::configs::DependencyMap;
use crate::models::{CrateModel, NexusModel, WorkspaceModel};
use eyre::{Result, bail};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

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
        if !nexus_path.exists() {
            bail!("Does not exist: {}", nexus_path.display())
        }
        let config_path = nexus_path.join("Magnet.toml");
        let nexus_config = MagnetConfig::from_file(&config_path)?;

        let nexus_model = NexusModel::from_config(nexus_config.nexus.unwrap(), &config_path);

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
        self.get_workspace(workspace_name).map(|w| w.root_path())
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

    pub fn get_package_path(&self, name: &str, workspace_name: Option<&str>) -> Option<&Path> {
        // If a workspace is specified, check only that workspace
        if let Some(ws_name) = workspace_name {
            if let Some(workspace) = self.get_workspace(ws_name) {
                if let Some(crate_info) = workspace.find_crate(name) {
                    return Some(&crate_info.path);
                }
            }
            return None;
        }

        // Otherwise, check all workspaces
        for workspace in self.workspaces.values() {
            if let Some(crate_info) = workspace.find_crate(name) {
                return Some(&crate_info.path);
            }
        }

        None
    }

    /// Get all crates across all workspaces
    pub fn get_all_crates(&self) -> Vec<CrateModel> {
        let mut all_crates = Vec::new();

        for workspace in self.workspaces.values() {
            all_crates.extend(workspace.crates.clone());
        }

        all_crates
    }

    /// Get crates in a specific workspace
    pub fn get_workspace_crates(&self, workspace_name: &str) -> Vec<CrateModel> {
        match self.get_workspace(workspace_name) {
            Some(ws) => ws.crates.clone(),
            None => Vec::new(),
        }
    }

    /// Find a crate by name across all workspaces or in a specific workspace
    pub fn find_crate(&self, name: &str, workspace_name: Option<&str>) -> Option<CrateModel> {
        // If a workspace is specified, check only that workspace
        if let Some(ws_name) = workspace_name {
            if let Some(workspace) = self.get_workspace(ws_name) {
                if let Some(crate_info) = workspace.find_crate(name) {
                    return Some(crate_info.clone());
                }
            }
            return None;
        }

        // Otherwise, check all workspaces
        for workspace in self.workspaces.values() {
            if let Some(crate_info) = workspace.find_crate(name) {
                return Some(crate_info.clone());
            }
        }

        None
    }

    /// Get all workspaces except the specified one
    pub fn get_other_workspaces(&self, workspace_name: &str) -> Vec<&WorkspaceModel> {
        self.workspaces
            .iter()
            .filter(|(name, _)| *name != workspace_name)
            .map(|(_, ws)| ws)
            .collect()
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
}
