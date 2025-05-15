//! Workspace configuration for Magnet.toml files

use crate::configs::DependencyMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Workspace configuration (legacy)
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
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
    pub nexus_path: Option<PathBuf>,
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
    
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dependencies: DependencyMap,
    
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CargoWorkspaceConfigWrapper {
    pub workspace: CargoWorkspaceConfig,
}