//! Dependency configuration for Magnet.toml files

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Map of dependency name to configuration
pub type DependencyMap = HashMap<String, DependencyConfig>;

/// Configuration for a single dependency
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DependencyConfig {
    /// Simple version string: e.g., "1.0.0"
    Simple(String),
    /// Detailed dependency configuration
    Detailed(DetailedDependency),
}

/// Detailed dependency configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DetailedDependency {
    /// Dependency version
    pub version: Option<String>,
    /// Path to local dependency
    pub path: Option<PathBuf>,
    /// Automatically resolve path to this dependency if found in any workspace
    pub auto: Option<bool>,
    /// Git repository URL
    pub git: Option<String>,
    /// Git branch
    pub branch: Option<String>,
    /// Git tag
    pub tag: Option<String>,
    /// Git revision
    pub rev: Option<String>,
    /// Dependency features to enable
    pub features: Option<Vec<String>>,
    /// Whether all features should be enabled
    pub all_features: Option<bool>,
    /// Whether default features should be enabled
    pub default_features: Option<bool>,
    /// Whether to use the version defined in the workspace
    pub workspace: Option<bool>,
    /// Optional dependency
    pub optional: Option<bool>,
    /// Package name (if different from dependency name)
    pub package: Option<String>,
    /// Registry to use
    pub registry: Option<String>,
    /// Artifact to use
    pub artifact: Option<String>,
    /// Target to use
    pub target: Option<String>,
    /// Custom dependency metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

// Implement conversion from SimpleVersion to DetailedDependency
impl From<&str> for DependencyConfig {
    fn from(version: &str) -> Self {
        DependencyConfig::Simple(version.to_string())
    }
}

impl From<String> for DependencyConfig {
    fn from(version: String) -> Self {
        DependencyConfig::Simple(version)
    }
}

impl From<DetailedDependency> for DependencyConfig {
    fn from(dep: DetailedDependency) -> Self {
        DependencyConfig::Detailed(dep)
    }
}