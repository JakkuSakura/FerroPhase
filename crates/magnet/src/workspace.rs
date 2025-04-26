//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a super-workspace.

use anyhow::{Context, Result, bail};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::config::MagnetConfig;

/// Represents a discovered crate within a workspace
#[derive(Debug, Clone)]
pub struct CrateInfo {
    /// Name of the crate
    pub name: String,
    /// Version of the crate (from Cargo.toml)
    pub version: Option<String>,
    /// Absolute path to the crate directory
    pub path: PathBuf,
    /// Path to the crate's Cargo.toml file
    pub cargo_toml_path: PathBuf,
    /// Path to the crate's Magnet.toml file, if it exists
    pub magnet_toml_path: Option<PathBuf>,
    /// Whether this crate has its own Magnet.toml configuration
    pub has_custom_config: bool,
}

/// Represents a discovered workspace
#[derive(Debug, Clone)]
pub struct WorkspaceInfo {
    /// Name of the workspace (from Magnet.toml project.name)
    pub name: Option<String>,
    /// Absolute path to the workspace root
    pub root_path: PathBuf,
    /// Path to the workspace's Magnet.toml file
    pub magnet_toml_path: PathBuf,
    /// The parsed Magnet.toml configuration
    pub config: MagnetConfig,
    /// Crates discovered in this workspace, keyed by crate name
    pub crates: HashMap<String, CrateInfo>,
    /// Parent workspace, if this is a nested workspace
    pub parent: Option<Box<WorkspaceInfo>>,
    /// Child workspaces, if any
    pub children: Vec<WorkspaceInfo>,
}

impl WorkspaceInfo {
    pub fn get_root_path(&self) -> PathBuf {
        self.root_path.clone()
    }
    /// Returns all crates in this workspace and all child workspaces
    pub fn all_crates(&self) -> HashMap<String, CrateInfo> {
        let mut result = self.crates.clone();
        for child in &self.children {
            for (name, crate_info) in child.all_crates() {
                result.insert(name, crate_info);
            }
        }
        result
    }

    /// Find a crate by name in this workspace and all child workspaces
    pub fn find_crate(&self, name: &str) -> Option<CrateInfo> {
        self.crates.get(name).cloned().or_else(|| {
            for child in &self.children {
                if let Some(crate_info) = child.find_crate(name) {
                    return Some(crate_info);
                }
            }
            None
        })
    }
}

/// Responsible for discovering and managing workspaces and their relationships
#[derive(Debug)]
pub struct WorkspaceManager {
    /// The primary workspace being managed
    pub primary_workspace: WorkspaceInfo,
    /// All related workspaces, keyed by their absolute path
    pub related_workspaces: HashMap<PathBuf, WorkspaceInfo>,
    /// Search paths to look for related workspaces
    pub search_paths: Vec<PathBuf>,
}

impl WorkspaceManager {
    /// Create a new workspace manager with the given config and base directory
    pub fn new(config: MagnetConfig, base_dir: PathBuf) -> Result<Self> {
        // Discover crates in the primary workspace
        let crates = Self::discover_crates(&base_dir, &config)
            .context("Failed to discover crates in primary workspace")?;

        let primary_workspace = WorkspaceInfo {
            name: config.project.name.clone(),
            root_path: base_dir.clone(),
            magnet_toml_path: base_dir.join("Magnet.toml"),
            config,
            crates,
            parent: None,
            children: Vec::new(),
        };

        let manager = Self {
            primary_workspace,
            related_workspaces: HashMap::new(),
            search_paths: Vec::new(),
        };

        // Discover related workspaces
        // We'll do this lazily to avoid infinite recursion

        Ok(manager)
    }

    /// Get the root path of the primary workspace
    pub fn get_root_path(&self) -> PathBuf {
        self.primary_workspace.root_path.clone()
    }

    /// Get all crates in the workspace as a vector
    pub fn get_all_crates(&self) -> Vec<CrateInfo> {
        self.list_all_crates().into_values().collect()
    }

    /// Get all external workspaces
    pub fn get_external_workspaces(&self) -> Vec<&WorkspaceInfo> {
        self.related_workspaces.values().collect()
    }

    /// Find the workspace root (directory containing Magnet.toml)
    pub fn find_workspace_root(start_path: &Path) -> Result<PathBuf> {
        let mut current = start_path.to_path_buf();

        loop {
            let magnet_toml = current.join("Magnet.toml");
            if magnet_toml.exists() {
                return Ok(current);
            }

            // Go up one directory
            if !current.pop() {
                bail!("Could not find Magnet.toml in any parent directory");
            }
        }
    }

    /// Discover crates in a workspace based on the workspace.members patterns
    fn discover_crates(
        root_path: &Path,
        config: &MagnetConfig,
    ) -> Result<HashMap<String, CrateInfo>> {
        let mut crates = HashMap::new();

        for pattern in &config.workspace.members {
            let glob_pattern = format!("{}/{}/Cargo.toml", root_path.display(), pattern);
            for entry in glob::glob(&glob_pattern)
                .context(format!("Invalid glob pattern: {}", glob_pattern))?
            {
                let cargo_toml_path = entry?;
                let crate_dir = cargo_toml_path.parent().unwrap().to_path_buf();
                let magnet_toml_path = crate_dir.join("Magnet.toml");
                let has_custom_config = magnet_toml_path.exists();

                // Extract crate name and version from Cargo.toml
                let cargo_toml_content = std::fs::read_to_string(&cargo_toml_path)?;
                let cargo_toml: toml::Value =
                    toml::from_str(&cargo_toml_content).context(format!(
                        "Failed to parse Cargo.toml at {}",
                        cargo_toml_path.display()
                    ))?;

                let name = cargo_toml
                    .get("package")
                    .and_then(|p| p.get("name"))
                    .and_then(|n| n.as_str())
                    .context(format!(
                        "Failed to extract package name from {}",
                        cargo_toml_path.display()
                    ))?
                    .to_string();

                let version = cargo_toml
                    .get("package")
                    .and_then(|p| p.get("version"))
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());

                crates.insert(
                    name.clone(),
                    CrateInfo {
                        name,
                        version,
                        path: crate_dir,
                        cargo_toml_path,
                        magnet_toml_path: if has_custom_config {
                            Some(magnet_toml_path)
                        } else {
                            None
                        },
                        has_custom_config,
                    },
                );
            }
        }

        Ok(crates)
    }

    /// Discover related workspaces by searching in configured paths
    pub fn discover_related_workspaces(&mut self) -> Result<()> {
        // First check workspace search paths from the config
        let mut search_paths = Vec::new();

        // Add explicitly configured search paths
        if let Some(configured_paths) = self
            .primary_workspace
            .config
            .workspace
            .search_paths
            .as_ref()
        {
            for path in configured_paths.values() {
                let absolute_path = if path.is_absolute() {
                    path.clone()
                } else {
                    self.primary_workspace.root_path.join(path)
                };
                search_paths.push(absolute_path);
            }
        }

        // Add parent directory as a search path
        if let Some(parent) = self.primary_workspace.root_path.parent() {
            search_paths.push(parent.to_path_buf());

            // Also add sibling directories
            if let Ok(entries) = std::fs::read_dir(parent) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_dir() && path != self.primary_workspace.root_path {
                        search_paths.push(path);
                    }
                }
            }
        }

        // Save the search paths
        self.search_paths = search_paths.clone();

        // Search for Magnet.toml files in the search paths
        for search_path in search_paths {
            if search_path == self.primary_workspace.root_path {
                continue; // Skip the primary workspace
            }

            let magnet_toml_path = search_path.join("Magnet.toml");
            if magnet_toml_path.exists() {
                // Found a related workspace
                match MagnetConfig::from_file(&magnet_toml_path) {
                    Ok(config) => {
                        // Discover crates in this workspace
                        match Self::discover_crates(&search_path, &config) {
                            Ok(crates) => {
                                let related_workspace = WorkspaceInfo {
                                    name: config.project.name.clone(),
                                    root_path: search_path.clone(),
                                    magnet_toml_path,
                                    config,
                                    crates,
                                    parent: None,
                                    children: Vec::new(),
                                };

                                self.related_workspaces
                                    .insert(search_path, related_workspace);
                            }
                            Err(e) => {
                                // Log the error but continue with other workspaces
                                eprintln!(
                                    "Warning: Failed to discover crates in related workspace at {}: {}",
                                    search_path.display(),
                                    e
                                );
                            }
                        }
                    }
                    Err(e) => {
                        // Log the error but continue with other workspaces
                        eprintln!(
                            "Warning: Failed to load Magnet.toml at {}: {}",
                            magnet_toml_path.display(),
                            e
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Find a crate by name across all workspaces (primary and related)
    pub fn find_crate(&self, name: &str) -> Option<CrateInfo> {
        // First check the primary workspace
        if let Some(crate_info) = self.primary_workspace.find_crate(name) {
            return Some(crate_info);
        }

        // Then check related workspaces
        for workspace in self.related_workspaces.values() {
            if let Some(crate_info) = workspace.find_crate(name) {
                return Some(crate_info);
            }
        }

        None
    }

    /// List all crates across all workspaces
    pub fn list_all_crates(&self) -> HashMap<String, CrateInfo> {
        let mut result = self.primary_workspace.all_crates();

        // Add crates from related workspaces
        for workspace in self.related_workspaces.values() {
            for (name, crate_info) in workspace.all_crates() {
                // Only add if not already in the primary workspace
                if !result.contains_key(&name) {
                    result.insert(name, crate_info);
                }
            }
        }

        result
    }

    /// Calculate a relative path from one crate to another
    pub fn relative_path_between(&self, from_crate: &CrateInfo, to_crate: &CrateInfo) -> PathBuf {
        pathdiff::diff_paths(&to_crate.path, &from_crate.path)
            .unwrap_or_else(|| to_crate.path.clone())
    }
}
