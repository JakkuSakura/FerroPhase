//! Package configuration for Magnet.toml files

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Package-specific configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct PackageConfig {
    /// Name of the package
    pub name: Option<String>,
    /// Version of the package
    pub version: Option<String>,
    /// Description of the package
    #[serde(default)]
    pub description: Option<String>,
    /// Authors of the package
    #[serde(default)]
    pub authors: Vec<String>,
    /// Package homepage
    #[serde(default)]
    pub homepage: Option<String>,
    /// Package repository
    #[serde(default)]
    pub repository: Option<String>,
    /// Package documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// Package license
    #[serde(default)]
    pub license: Option<String>,
    /// Custom package metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

/// Project metadata configuration (legacy)
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ProjectConfig {
    /// Name of the project
    pub name: Option<String>,
    /// Version of the project
    pub version: Option<String>,
    /// Description of the project
    #[serde(default)]
    pub description: Option<String>,
    /// Authors of the project
    #[serde(default)]
    pub authors: Vec<String>,
    /// Project homepage
    #[serde(default)]
    pub homepage: Option<String>,
    /// Project repository
    #[serde(default)]
    pub repository: Option<String>,
    /// Project documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// Project license
    #[serde(default)]
    pub license: Option<String>,
    /// Custom metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            name: None,
            version: None,
            description: None,
            authors: Vec::new(),
            homepage: None,
            repository: None,
            documentation: None,
            license: None,
            custom: HashMap::new(),
        }
    }
}