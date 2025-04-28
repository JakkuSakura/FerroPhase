//! Workspace configuration for Magnet.toml files

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Workspace-specific configuration (extends WorkspaceConfig)
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct WorkspaceConfigSection {
    /// Name of the workspace
    pub name: Option<String>,
    /// Version of the workspace
    pub version: Option<String>,
    /// Description of the workspace
    #[serde(default)]
    pub description: Option<String>,
    /// Workspace members (glob patterns)
    #[serde(default)]
    pub members: Vec<String>,
    /// Excluded workspace members (glob patterns)
    #[serde(default)]
    pub exclude: Vec<String>,
    /// Cargo resolver version (1 or 2)
    #[serde(default)]
    pub resolver: Option<String>,
    /// Custom workspace metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

/// Workspace configuration (legacy)
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WorkspaceConfig {
    /// Workspace members (glob patterns)
    #[serde(default)]
    pub members: Vec<String>,
    /// Excluded workspace members (glob patterns)
    #[serde(default)]
    pub exclude: Vec<String>,
    /// Cargo resolver version (1 or 2)
    #[serde(default)]
    pub resolver: Option<String>,
    /// Search paths for related workspaces
    #[serde(default)]
    pub search_paths: Option<HashMap<String, PathBuf>>,
    /// Path overrides for specific dependencies
    #[serde(default)]
    pub paths: Option<HashMap<String, PathBuf>>,
    /// Custom workspace metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

impl Default for WorkspaceConfig {
    fn default() -> Self {
        Self {
            members: Vec::new(),
            exclude: Vec::new(),
            resolver: None,
            search_paths: None,
            paths: None,
            custom: HashMap::new(),
        }
    }
}
