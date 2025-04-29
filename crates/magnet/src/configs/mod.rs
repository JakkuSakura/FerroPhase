//! Configuration handling for Magnet.toml files
//!
//! This module provides structures and functionality for parsing,
//! validating, and managing Magnet.toml configuration files.

mod nexus;
mod workspace;
mod package;
mod dependencies;
pub use nexus::*;
pub use package::*;
pub use workspace::*;
pub use dependencies::*;


use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
/// Type of Magnet.toml configuration file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum MagnetConfigType {
    /// Nexus configuration (top-level, manages multiple workspaces)
    Nexus,
    /// Workspace configuration (manages multiple packages)
    Workspace,
    /// Package configuration (individual package)
    Package,
}

impl Default for MagnetConfigType {
    fn default() -> Self {
        Self::Package
    }
}
/// The main configuration structure representing a Magnet.toml file
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MagnetConfig {
    /// Project metadata
    #[serde(default)]
    pub project: ProjectConfig,
    /// Workspace configuration
    #[serde(default)]
    pub workspace: WorkspaceConfig,
    /// Nexus configuration (for top-level nexus configs)
    #[serde(default)]
    pub nexus: Option<NexusConfig>,
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
    /// Type of configuration
    #[serde(skip)]
    pub config_type: MagnetConfigType,
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

    /// Create a new configuration with the specified type
    pub fn new_with_type(config_type: MagnetConfigType) -> Self {
        let mut config = Self::new();

        // Set the configuration type
        config.config_type = config_type;

        // Based on the type, ensure appropriate sections exist
        match config_type {
            MagnetConfigType::Nexus => {
                // Initialize nexus-specific fields
                config.nexus = Some(NexusConfig::default());
            },
            MagnetConfigType::Workspace => {
                // Workspace type already has defaults in the WorkspaceConfig
            },
            MagnetConfigType::Package => {
                // Package type is the default
            }
        }

        config
    }

    /// Parse a MagnetConfig from a TOML string
    pub fn from_toml_str(toml_str: &str) -> Result<Self> {
        // Parse the TOML
        let config: Self = toml::from_str(toml_str)
            .context("Failed to parse Magnet.toml from string")?;

        Ok(config)
    }

    /// Get the configuration type based on which sections are defined
    pub fn config_type(&self) -> MagnetConfigType {
        if self.nexus.is_some() {
            MagnetConfigType::Nexus
        } else if !self.workspace.members.is_empty() || self.workspace.search_paths.is_some() {
            MagnetConfigType::Workspace
        } else {
            MagnetConfigType::Package
        }
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
            nexus: None,
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            source_path: None,
            config_type: MagnetConfigType::default(),
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
