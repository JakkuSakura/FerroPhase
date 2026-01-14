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
        let mut parts: Vec<String> = Vec::new();
        if let Some(version) = &self.version {
            parts.push(format!("version = {:?}", version));
        }
        if let Some(path) = &self.path {
            parts.push(format!("path = {:?}", path.display()));
        }
        if let Some(nexus) = &self.nexus {
            parts.push(format!("nexus = {}", nexus));
        }
        if let Some(git) = &self.git {
            parts.push(format!("git = {:?}", git));
        }
        if let Some(branch) = &self.branch {
            parts.push(format!("branch = {:?}", branch));
        }
        if let Some(tag) = &self.tag {
            parts.push(format!("tag = {:?}", tag));
        }
        if let Some(rev) = &self.rev {
            parts.push(format!("rev = {:?}", rev));
        }
        if let Some(features) = &self.features {
            parts.push(format!("features = {:?}", features));
        }

        if let Some(default_features) = &self.default_features {
            parts.push(format!("default-features = {}", default_features));
        }
        if let Some(workspace) = &self.workspace {
            parts.push(format!("workspace = {}", workspace));
        }
        if let Some(optional) = &self.optional {
            parts.push(format!("optional = {}", optional));
        }
        if let Some(package) = &self.package {
            parts.push(format!("package = {:?}", package));
        }
        if let Some(registry) = &self.registry {
            parts.push(format!("registry = {:?}", registry));
        }
        if let Some(artifact) = &self.artifact {
            parts.push(format!("artifact = {:?}", artifact));
        }
        if let Some(target) = &self.target {
            parts.push(format!("target = {:?}", target));
        }
        if !self.custom.is_empty() {
            let mut custom_parts = Vec::new();
            for (key, value) in &self.custom {
                custom_parts.push(format!("{} = {:?}", key, value));
            }
            parts.push(format!("custom = {{ {} }}", custom_parts.join(", ")));
        }

        write!(f, "{{{}}}", parts.join(", "))
    }
}

pub type DependencyModelMap = HashMap<String, DependencyModel>;
