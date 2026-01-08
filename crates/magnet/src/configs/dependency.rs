//! Dependency configuration for Magnet.toml files

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;

/// Map of dependency name to configuration
pub type DependencyConfigMap = HashMap<String, DependencyConfig>;

/// Detailed dependency configuration for TOML files
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DetailedDependencyConfig {
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
}

/// Configuration for a single dependency
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DependencyConfig {
    /// Simple version string: e.g., "1.0.0"
    Simple(String),
    /// Detailed dependency configuration
    Detailed(DetailedDependencyConfig),
}
impl Display for DetailedDependencyConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(version) = &self.version {
            write!(f, "version = {:?}, ", version)?;
        }
        if let Some(path) = &self.path {
            write!(f, "path = {:?}, ", path.display())?;
        }
        if let Some(nexus) = &self.nexus {
            write!(f, "nexus = {}, ", nexus)?;
        }
        if let Some(git) = &self.git {
            write!(f, "git = {:?}, ", git)?;
        }
        if let Some(branch) = &self.branch {
            write!(f, "branch = {:?}, ", branch)?;
        }
        if let Some(tag) = &self.tag {
            write!(f, "tag = {:?}, ", tag)?;
        }
        if let Some(rev) = &self.rev {
            write!(f, "rev = {:?}, ", rev)?;
        }
        if let Some(features) = &self.features {
            write!(f, "features = {:?}, ", features)?;
        }
        if let Some(default_features) = &self.default_features {
            write!(f, "default-features = {}, ", default_features)?;
        }
        if let Some(workspace) = &self.workspace {
            write!(f, "workspace = {}, ", workspace)?;
        }
        if let Some(optional) = &self.optional {
            write!(f, "optional = {}, ", optional)?;
        }
        if let Some(package) = &self.package {
            write!(f, "package = {:?}, ", package)?;
        }
        if let Some(registry) = &self.registry {
            write!(f, "registry = {:?}, ", registry)?;
        }
        if let Some(artifact) = &self.artifact {
            write!(f, "artifact = {:?}, ", artifact)?;
        }
        if let Some(target) = &self.target {
            write!(f, "target = {:?}, ", target)?;
        }
        write!(f, "}}")
    }
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

// Implement conversion between models and configs
use crate::models::DependencyModel;

impl From<DetailedDependencyConfig> for DependencyModel {
    fn from(config: DetailedDependencyConfig) -> Self {
        DependencyModel {
            version: config.version,
            path: config.path,
            nexus: config.nexus,
            git: config.git,
            branch: config.branch,
            tag: config.tag,
            rev: config.rev,
            features: config.features,
            default_features: config.default_features,
            workspace: config.workspace,
            optional: config.optional,
            package: config.package,
            registry: config.registry,
            artifact: config.artifact,
            target: config.target,
            custom: HashMap::new(), // Not present in config
        }
    }
}

impl From<DependencyModel> for DetailedDependencyConfig {
    fn from(model: DependencyModel) -> Self {
        DetailedDependencyConfig {
            version: model.version,
            path: model.path,
            nexus: model.nexus,
            git: model.git,
            branch: model.branch,
            tag: model.tag,
            rev: model.rev,
            features: model.features,
            default_features: model.default_features,
            workspace: model.workspace,
            optional: model.optional,
            package: model.package,
            registry: model.registry,
            artifact: model.artifact,
            target: model.target,
        }
    }
}

impl From<DependencyModel> for DependencyConfig {
    fn from(model: DependencyModel) -> Self {
        DependencyConfig::Detailed(model.into())
    }
}

impl From<DependencyConfig> for DependencyModel {
    fn from(config: DependencyConfig) -> Self {
        match config {
            DependencyConfig::Simple(version) => DependencyModel {
                version: Some(version),
                ..Default::default()
            },
            DependencyConfig::Detailed(detailed) => detailed.into(),
        }
    }
}
