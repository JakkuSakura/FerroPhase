//! Configuration handling for Magnet.toml files
//!
//! This module provides structures and functionality for parsing,
//! validating, and managing Magnet.toml configuration files.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// The main configuration structure representing a Magnet.toml file
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MagnetConfig {
    /// Project metadata
    pub project: ProjectConfig,
    /// Workspace configuration
    pub workspace: WorkspaceConfig,
    /// Dependencies shared across workspace members
    #[serde(default)]
    pub dependencies: DependencyMap,
    /// Development dependencies shared across workspace members
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: DependencyMap,
    /// Build dependencies shared across workspace members
    #[serde(default, rename = "build-dependencies")]
    pub build_dependencies: DependencyMap,
    /// Source path of this configuration
    #[allow(dead_code)]
    #[serde(skip)]
    pub source_path: Option<PathBuf>,
}

/// Project metadata configuration
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

/// Workspace configuration
#[derive(Debug, Clone, Deserialize, Serialize)]
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
}

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
#[allow(dead_code)]
#[allow(clippy::trivially_copy_pass_by_ref)]
impl MagnetConfig {
    /// Load a MagnetConfig from a file
    pub fn from_file(path: &Path) -> Result<Self> {
        // Read the file content
        let content = std::fs::read_to_string(path).context(format!(
            "Failed to read Magnet.toml from {}",
            path.display()
        ))?;

        // Parse the TOML
        let mut config: Self = toml::from_str(&content).context(format!(
            "Failed to parse Magnet.toml from {}",
            path.display()
        ))?;

        // Store the source path
        config.source_path = Some(path.to_path_buf());

        Ok(config)
    }

    /// Save this configuration to a file
    pub fn save_to_file(&self, path: &Path) -> Result<()> {
        // Convert to TOML
        let toml = toml::to_string_pretty(self).context("Failed to serialize Magnet.toml")?;

        // Write to file
        std::fs::write(path, toml)
            .context(format!("Failed to write Magnet.toml to {}", path.display()))?;

        Ok(())
    }

    /// Create a new empty configuration
    pub fn new() -> Self {
        Self {
            project: ProjectConfig {
                name: None,
                version: None,
                description: None,
                authors: Vec::new(),
                homepage: None,
                repository: None,
                documentation: None,
                license: None,
                custom: HashMap::new(),
            },
            workspace: WorkspaceConfig {
                members: Vec::new(),
                exclude: Vec::new(),
                resolver: None,
                search_paths: None,
                paths: None,
                custom: HashMap::new(),
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            source_path: None,
        }
    }

    /// Merge with another configuration, giving priority to the other
    pub fn merge_with(&mut self, other: &Self) {
        // Merge project metadata
        if let Some(name) = &other.project.name {
            self.project.name = Some(name.clone());
        }
        if let Some(version) = &other.project.version {
            self.project.version = Some(version.clone());
        }
        if let Some(description) = &other.project.description {
            self.project.description = Some(description.clone());
        }
        if !other.project.authors.is_empty() {
            self.project.authors = other.project.authors.clone();
        }
        if let Some(homepage) = &other.project.homepage {
            self.project.homepage = Some(homepage.clone());
        }
        if let Some(repository) = &other.project.repository {
            self.project.repository = Some(repository.clone());
        }
        if let Some(documentation) = &other.project.documentation {
            self.project.documentation = Some(documentation.clone());
        }
        if let Some(license) = &other.project.license {
            self.project.license = Some(license.clone());
        }

        // Merge workspace configuration
        if !other.workspace.members.is_empty() {
            self.workspace.members = other.workspace.members.clone();
        }
        if !other.workspace.exclude.is_empty() {
            self.workspace.exclude = other.workspace.exclude.clone();
        }
        if let Some(resolver) = &other.workspace.resolver {
            self.workspace.resolver = Some(resolver.clone());
        }

        // Merge search paths (including custom keys)
        if let Some(search_paths) = &other.workspace.search_paths {
            if self.workspace.search_paths.is_none() {
                self.workspace.search_paths = Some(HashMap::new());
            }

            let self_search_paths = self.workspace.search_paths.as_mut().unwrap();
            for (key, path) in search_paths {
                self_search_paths.insert(key.clone(), path.clone());
            }
        }

        // Merge path overrides
        if let Some(paths) = &other.workspace.paths {
            if self.workspace.paths.is_none() {
                self.workspace.paths = Some(HashMap::new());
            }

            let self_paths = self.workspace.paths.as_mut().unwrap();
            for (key, path) in paths {
                self_paths.insert(key.clone(), path.clone());
            }
        }

        // Merge dependencies (other takes precedence)
        for (name, dep) in &other.dependencies {
            self.dependencies.insert(name.clone(), dep.clone());
        }

        // Merge dev dependencies
        for (name, dep) in &other.dev_dependencies {
            self.dev_dependencies.insert(name.clone(), dep.clone());
        }

        // Merge build dependencies
        for (name, dep) in &other.build_dependencies {
            self.build_dependencies.insert(name.clone(), dep.clone());
        }
    }

    /// Check if a dependency is configured for auto path resolution
    pub fn is_auto_dependency(&self, name: &str) -> bool {
        match self.dependencies.get(name) {
            Some(DependencyConfig::Detailed(dep)) => dep.auto.unwrap_or(false),
            _ => false,
        }
    }

    /// Get a detailed dependency configuration by name
    pub fn get_detailed_dependency(&self, name: &str) -> Option<DetailedDependency> {
        match self.dependencies.get(name) {
            Some(DependencyConfig::Simple(version)) => {
                let mut dep = DetailedDependency::default();
                dep.version = Some(version.clone());
                Some(dep)
            }
            Some(DependencyConfig::Detailed(dep)) => Some(dep.clone()),
            None => None,
        }
    }
}

impl Default for MagnetConfig {
    fn default() -> Self {
        Self::new()
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

impl From<DetailedDependency> for DependencyConfig {
    fn from(dep: DetailedDependency) -> Self {
        DependencyConfig::Detailed(dep)
    }
}
