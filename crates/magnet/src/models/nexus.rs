//! Domain model for a Nexus, which represents a collection of workspaces.

use crate::configs::NexusConfig;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A nexus model representing a collection of workspaces
#[derive(Debug, Clone)]
pub struct NexusModel {
    /// Name of the nexus
    pub name: String,
    /// Version of the nexus
    pub version: Option<String>,
    /// Description of the nexus
    pub description: Option<String>,
    /// Workspaces included in this nexus (patterns)
    pub workspaces: Vec<String>,
    /// Workspaces excluded from this nexus (patterns)
    pub exclude: Vec<String>,
    /// Default search paths for dependencies
    pub search_paths: HashMap<String, PathBuf>,
    /// Custom nexus metadata
    pub custom: HashMap<String, toml::Value>,
    /// Source path of the nexus configuration
    pub source_path: Option<PathBuf>,
}

impl Default for NexusModel {
    fn default() -> Self {
        Self {
            name: "unnamed-nexus".to_string(),
            version: None,
            description: None,
            workspaces: Vec::new(),
            exclude: Vec::new(),
            search_paths: HashMap::new(),
            custom: HashMap::new(),
            source_path: None,
        }
    }
}

impl From<NexusConfig> for NexusModel {
    fn from(config: NexusConfig) -> Self {
        Self {
            name: config.name.unwrap_or_else(|| "unnamed-nexus".to_string()),
            version: config.version,
            description: config.description,
            workspaces: vec![],
            exclude: vec![],
            search_paths: config.search_paths.unwrap_or_default(),
            custom: config.custom.clone(),
            source_path: None,
        }
    }
}

impl NexusModel {
    /// Create a new nexus model with the given name
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ..Default::default()
        }
    }

    /// Create a nexus model from a config, with additional source path information
    pub fn from_config(config: NexusConfig, source_path: Option<&Path>) -> Self {
        let mut model = Self::from(config);
        if let Some(path) = source_path {
            model.source_path = Some(path.to_path_buf());
        }
        model
    }
}
