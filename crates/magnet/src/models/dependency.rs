use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

/// Detailed dependency configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct DependencyModel {
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
impl DependencyModel {
    pub fn nexus(&self) -> bool {
        self.nexus.unwrap_or(false)
    }
    pub fn workspace(&self) -> bool {
        self.workspace.unwrap_or(false)
    }
    pub fn default_features(&self) -> bool {
        self.default_features.unwrap_or(true)
    }
    pub fn optional(&self) -> bool {
        self.optional.unwrap_or(false)
    }
    pub fn features(&self) -> Vec<String> {
        self.features.clone().unwrap_or_default()
    }
}
impl Display for DependencyModel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
        if !self.custom.is_empty() {
            write!(f, "custom = {{ ")?;
            for (key, value) in &self.custom {
                write!(f, "{} = {:?}, ", key, value)?;
            }
            write!(f, "}}, ")?;
        }
        write!(f, "}}")
    }
}

pub type DependencyModelMap = HashMap<String, DependencyModel>;
