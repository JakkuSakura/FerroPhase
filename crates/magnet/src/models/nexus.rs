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
    pub members: Vec<String>,
    /// Workspaces excluded from this nexus (patterns)
    pub exclude: Vec<String>,
    /// Default search paths for dependencies
    pub search_paths: HashMap<String, PathBuf>,
    /// Custom nexus metadata
    pub custom: HashMap<String, toml::Value>,
    pub root_path: PathBuf,
    /// Source path of the nexus configuration
    pub source_path: PathBuf,
}

impl NexusModel {
    /// Create a nexus model from a config, with additional source path information
    pub fn from_config(config: NexusConfig, source_path: &Path) -> Self {
        let root_path = source_path.parent().unwrap().canonicalize().unwrap().to_owned();
        let name = config.name.unwrap_or_else(|| {
            root_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned()
        });
        let source_path = source_path.to_path_buf();
        let model = NexusModel {
            name,
            version: config.version,
            description: config.description,
            members: config.members,
            exclude: config.exclude,
            search_paths: config.search_paths.unwrap_or_default(),
            custom: config.custom.clone(),
            root_path,
            source_path,
        };

        model
    }
}
