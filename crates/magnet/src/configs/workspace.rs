//! Workspace configuration for Magnet.toml files

use crate::configs::DependencyMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

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
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CargoWorkspaceConfig {
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
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dependencies: DependencyMap,
    /// Path overrides for specific dependencies
    #[serde(default)]
    pub paths: Option<HashMap<String, PathBuf>>,
}
impl CargoWorkspaceConfig {
    pub fn new() -> Self {
        Self {
            members: Vec::new(),
            exclude: Vec::new(),
            resolver: None,
            search_paths: None,
            dependencies: DependencyMap::new(),
            paths: None,
        }
    }
}
