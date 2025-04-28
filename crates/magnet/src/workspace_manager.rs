//! Workspace management and discovery
//!
//! This module handles workspace discovery, relationship management,
//! and tracking crates across projects in a nexus.

use crate::config::{MagnetConfig, get_workspace_members, get_search_paths};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use anyhow::{Context, Result};

/// Crate information structure 
#[derive(Debug, Clone)]
pub struct CrateInfo {
    /// Name of the crate
    pub name: String,
    /// Version of the crate
    pub version: Option<String>,
    /// Path to the crate directory
    pub path: PathBuf,
    /// Path to the Cargo.toml file
    pub cargo_toml_path: PathBuf,
    /// Path to the Magnet.toml file (if any)
    pub magnet_toml_path: Option<PathBuf>,
    /// Whether this crate has a custom configuration
    pub has_custom_config: bool,
}

/// Workspace configuration
#[derive(Debug, Clone)]
pub struct Workspace {
    /// The magnet configuration for this workspace
    pub config: MagnetConfig,
    /// Root path of the workspace
    pub root_path: PathBuf,
    /// Crates in this workspace
    pub crates: Vec<CrateInfo>,
}

/// Workspace manager
#[derive(Debug)]
pub struct WorkspaceManager {
    /// Primary workspace
    pub primary_workspace: Workspace,
    /// Related workspaces
    pub related_workspaces: HashMap<String, Workspace>,
}

impl Workspace {
    /// Create a new workspace from configuration
    pub fn new(config: MagnetConfig, root_path: PathBuf) -> Result<Self> {
        let mut workspace = Self {
            config,
            root_path,
            crates: Vec::new(),
        };
        
        // Discover crates in the workspace
        workspace.discover_crates()?;
        
        Ok(workspace)
    }
    
    /// Get the root path of this workspace
    pub fn get_root_path(&self) -> &Path {
        &self.root_path
    }
    
    /// Discover crates in the workspace
    pub fn discover_crates(&mut self) -> Result<()> {
        // Check if we have workspace members to discover
        let members = get_workspace_members(&self.config);
        
        // Skip discovery if there are no workspace members
        if members.is_empty() {
            return Ok(());
        }
        
        // Clear existing crates
        self.crates.clear();
        
        // Discover crates based on workspace.members patterns
        for pattern in members {
            // Resolve the pattern relative to the workspace root
            let full_pattern = format!("{}/{}/Cargo.toml", self.root_path.display(), pattern);
            
            // Find all Cargo.toml files matching the pattern
            let matches = match glob::glob(&full_pattern) {
                Ok(matches) => matches,
                Err(e) => {
                    eprintln!("Warning: Invalid glob pattern '{}': {}", full_pattern, e);
                    continue;
                }
            };
            
            // Process each match
            for entry in matches {
                match entry {
                    Ok(cargo_toml_path) => {
                        // Get the crate directory
                        let crate_dir = cargo_toml_path.parent()
                            .ok_or_else(|| anyhow::anyhow!("Invalid Cargo.toml path: {}", cargo_toml_path.display()))?;
                        
                        // Check for Magnet.toml
                        let magnet_toml_path = crate_dir.join("Magnet.toml");
                        let has_magnet_toml = magnet_toml_path.exists();
                        
                        // Parse Cargo.toml to get crate info
                        let crate_info = get_crate_info(&cargo_toml_path, has_magnet_toml)?;
                        
                        // Add to our list if not already present
                        if !self.crates.iter().any(|c| c.name == crate_info.name) {
                            self.crates.push(crate_info);
                        }
                    }
                    Err(e) => {
                        eprintln!("Warning: Error processing glob match: {}", e);
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Find a crate by name
    pub fn find_crate(&self, name: &str) -> Option<&CrateInfo> {
        self.crates.iter().find(|c| c.name == name)
    }
}

impl WorkspaceManager {
    /// Create a new workspace manager
    pub fn new(config: MagnetConfig, root_path: PathBuf) -> Result<Self> {
        // Create the primary workspace
        let primary_workspace = Workspace::new(config, root_path)?;
        
        Ok(Self {
            primary_workspace,
            related_workspaces: HashMap::new(),
        })
    }
    
    /// Get the root path of the primary workspace
    pub fn get_root_path(&self) -> &Path {
        self.primary_workspace.get_root_path()
    }
    
    /// Discover related workspaces
    pub fn discover_related_workspaces(&mut self) -> Result<()> {
        // Clear existing related workspaces
        self.related_workspaces.clear();
        
        // Get search paths from config
        let search_paths = get_search_paths(&self.primary_workspace.config);
        
        if let Some(search_paths) = search_paths {
            // Resolve and load each related workspace
            for (name, rel_path) in search_paths {
                let abs_path = if rel_path.is_absolute() {
                    rel_path
                } else {
                    self.primary_workspace.get_root_path().join(&rel_path)
                };
                
                // Check if the path exists
                if !abs_path.exists() {
                    eprintln!("Warning: Related workspace path does not exist: {}", abs_path.display());
                    continue;
                }
                
                // Check for Magnet.toml
                let magnet_toml_path = abs_path.join("Magnet.toml");
                if !magnet_toml_path.exists() {
                    eprintln!("Warning: No Magnet.toml found in related workspace: {}", abs_path.display());
                    continue;
                }
                
                // Load the configuration
                let config = match MagnetConfig::from_file(&magnet_toml_path) {
                    Ok(config) => config,
                    Err(e) => {
                        eprintln!("Warning: Failed to load Magnet.toml from related workspace {}: {}", abs_path.display(), e);
                        continue;
                    }
                };
                
                // Create the workspace
                match Workspace::new(config, abs_path.clone()) {
                    Ok(workspace) => {
                        self.related_workspaces.insert(name, workspace);
                    }
                    Err(e) => {
                        eprintln!("Warning: Failed to load related workspace {}: {}", abs_path.display(), e);
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Get all crates across all workspaces
    pub fn get_all_crates(&self) -> Vec<CrateInfo> {
        let mut all_crates = self.primary_workspace.crates.clone();
        
        for workspace in self.related_workspaces.values() {
            all_crates.extend(workspace.crates.clone());
        }
        
        all_crates
    }
    
    /// Find a crate by name across all workspaces
    pub fn find_crate(&self, name: &str) -> Option<CrateInfo> {
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
    pub fn get_external_workspaces(&self) -> Vec<&Workspace> {
        self.related_workspaces.values().collect()
    }
}

/// Get information about a crate from its Cargo.toml file
fn get_crate_info(cargo_toml_path: &Path, has_magnet_toml: bool) -> Result<CrateInfo> {
    // Read the Cargo.toml file
    let content = std::fs::read_to_string(cargo_toml_path)
        .context(format!("Failed to read Cargo.toml at {}", cargo_toml_path.display()))?;
    
    // Parse the TOML
    let cargo_toml: toml::Value = toml::from_str(&content)
        .context(format!("Failed to parse Cargo.toml at {}", cargo_toml_path.display()))?;
    
    // Extract crate name and version
    let name = cargo_toml
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .ok_or_else(|| anyhow::anyhow!("Missing package.name in {}", cargo_toml_path.display()))?
        .to_string();
    
    let version = cargo_toml
        .get("package")
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    
    // Get the crate directory
    let crate_dir = cargo_toml_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Invalid Cargo.toml path: {}", cargo_toml_path.display()))?
        .to_path_buf();
    
    // Check for Magnet.toml
    let magnet_toml_path = crate_dir.join("Magnet.toml");
    
    Ok(CrateInfo {
        name,
        version,
        path: crate_dir,
        cargo_toml_path: cargo_toml_path.to_path_buf(),
        magnet_toml_path: if has_magnet_toml { Some(magnet_toml_path) } else { None },
        has_custom_config: has_magnet_toml,
    })
}