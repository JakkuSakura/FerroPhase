//! Dependency configuration for Magnet.toml files

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
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
impl Display for DependencyConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyConfig::Simple(version) => write!(f, "{:?}", version),
            DependencyConfig::Detailed(dep) => {
                write!(f, "{{ ")?;
                if let Some(version) = &dep.version {
                    write!(f, "version = {:?}, ", version)?;
                }
                if let Some(path) = &dep.path {
                    write!(f, "path = {:?}, ", path.display())?;
                }
                if let Some(auto) = &dep.nexus {
                    write!(f, "auto = {}, ", auto)?;
                }
                if let Some(git) = &dep.git {
                    write!(f, "git = {:?}, ", git)?;
                }
                if let Some(branch) = &dep.branch {
                    write!(f, "branch = {:?}, ", branch)?;
                }
                if let Some(tag) = &dep.tag {
                    write!(f, "tag = {:?}, ", tag)?;
                }
                if let Some(rev) = &dep.rev {
                    write!(f, "rev = {:?}, ", rev)?;
                }
                if let Some(features) = &dep.features {
                    write!(f, "features = {:?}, ", features)?;
                }

                if let Some(default_features) = &dep.default_features {
                    write!(f, "default_features = {}, ", default_features)?;
                }
                if let Some(workspace) = &dep.workspace {
                    write!(f, "workspace = {}, ", workspace)?;
                }
                if let Some(optional) = &dep.optional {
                    write!(f, "optional = {}, ", optional)?;
                }
                if let Some(package) = &dep.package {
                    write!(f, "package = {:?}, ", package)?;
                }
                if let Some(registry) = &dep.registry {
                    write!(f, "registry = {:?}, ", registry)?;
                }
                if let Some(artifact) = &dep.artifact {
                    write!(f, "artifact = {:?}, ", artifact)?;
                }
                if let Some(target) = &dep.target {
                    write!(f, "target = {:?}, ", target)?;
                }
                if !dep.custom.is_empty() {
                    write!(f, "custom = {{ ")?;
                    for (key, value) in &dep.custom {
                        write!(f, "{} = {:?}, ", key, value)?;
                    }
                    write!(f, "}}, ")?;
                }
                write!(f, "}}")
            }
        }
    }
}

/// Detailed dependency configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DetailedDependency {
    /// Dependency version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    /// Path to local dependency
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<PathBuf>,
    /// Automatically resolve path to this dependency if found in any workspace
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nexus: Option<bool>,
    /// Git repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    /// Git branch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
    /// Git tag
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,
    /// Git revision
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rev: Option<String>,
    /// Dependency features to enable
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,

    /// Whether default features should be enabled
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_features: Option<bool>,
    /// Whether to use the version defined in the workspace
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<bool>,
    /// Optional dependency
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,
    /// Package name (if different from dependency name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    /// Registry to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,
    /// Artifact to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub artifact: Option<String>,
    /// Target to use
    #[serde(skip_serializing_if = "Option::is_none")]
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
