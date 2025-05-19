//! Dependency configuration for Magnet.toml files

use crate::models::DependencyModel;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;

/// Map of dependency name to configuration
pub type DependencyConfigMap = HashMap<String, DependencyConfig>;

/// Configuration for a single dependency
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DependencyConfig {
    /// Simple version string: e.g., "1.0.0"
    Simple(String),
    /// Detailed dependency configuration
    Detailed(DependencyModel),
}
impl Display for DependencyConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyConfig::Simple(version) => write!(f, "{:?}", version),
            DependencyConfig::Detailed(dep) => dep.fmt(f),
        }
    }
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

impl From<DependencyModel> for DependencyConfig {
    fn from(dep: DependencyModel) -> Self {
        DependencyConfig::Detailed(dep)
    }
}

impl From<DependencyConfig> for DependencyModel {
    fn from(dep: DependencyConfig) -> Self {
        match dep {
            DependencyConfig::Simple(version) => DependencyModel {
                version: Some(version),
                ..Default::default()
            },
            DependencyConfig::Detailed(dep) => dep,
        }
    }
}