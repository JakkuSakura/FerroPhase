//! Configuration handling for Magnet.toml files
//!
//! This module provides structures and functionality for parsing,
//! validating, and managing Magnet.toml configuration files.

mod dependency;
mod cargo;
mod nexus;
mod package;
mod workspace;

pub use dependency::*;
pub use cargo::*;
pub use nexus::*;
pub use package::*;
pub use workspace::*;

use crate::models::PatchMap;
use eyre::{Context, Result};
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
/// which is a superset of Cargo.toml
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ManifestConfig {
    /// Nexus configuration (for top-level nexus configs)
    #[serde(default)]
    pub nexus: Option<NexusConfig>,
    /// Workspace configuration
    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,

    /// Package metadata (alternative to project)
    #[serde(default)]
    pub package: Option<PackageConfig>,

    /// Dependencies shared across workspace members
    #[serde(default, skip_serializing_if = "DependencyConfigMap::is_empty")]
    pub dependencies: DependencyConfigMap,
    /// Development dependencies shared across workspace members
    #[serde(default, skip_serializing_if = "DependencyConfigMap::is_empty")]
    pub dev_dependencies: DependencyConfigMap,
    /// Build dependencies shared across workspace members
    #[serde(default, skip_serializing_if = "DependencyConfigMap::is_empty")]
    pub build_dependencies: DependencyConfigMap,
    /// Patch section for overriding dependencies
    #[serde(default, skip_serializing_if = "PatchMap::is_empty")]
    pub patch: PatchMap,
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
impl ManifestConfig {
    /// Load a MagnetConfig from a file, using cache if available
    pub fn from_file(path: &Path) -> Result<Self> {
        // Cache miss or expired, load from file
        let config = Self::load_file(path)?;

        Ok(config)
    }

    /// Actually load the file from disk (no caching)
    fn load_file(path: &Path) -> Result<Self> {
        let path = path.canonicalize().with_context(|| {
            format!(
                "Failed to canonicalize path for Magnet.toml: {}",
                path.display()
            )
        })?;
        // Read the file content
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read Magnet.toml from {}", path.display()))?;

        // Parse the TOML
        let mut config: Self = toml::from_str(&content)
            .with_context(|| format!("Failed to parse Magnet.toml from {}", path.display()))?;

        // Store the source path
        config.source_path = Some(path);

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
            }
            MagnetConfigType::Workspace => {
                // Workspace type already has defaults in the WorkspaceConfig
            }
            MagnetConfigType::Package => {
                // Package type is the default
            }
        }

        config
    }

    /// Parse a MagnetConfig from a TOML string
    pub fn from_toml_str(toml_str: &str) -> Result<Self> {
        // Parse the TOML
        let config: Self =
            toml::from_str(toml_str).context("Failed to parse Magnet.toml from string")?;

        Ok(config)
    }

    /// Get the configuration type based on which sections are defined
    pub fn config_type(&self) -> MagnetConfigType {
        if self.nexus.is_some() {
            return MagnetConfigType::Nexus;
        }
        if self.workspace.is_some() {
            return MagnetConfigType::Workspace;
        }
        if self.package.is_some() {
            return MagnetConfigType::Package;
        }
        panic!("Magnet config type is undefined: {:?}", self.source_path)
    }

    /// Create a new empty configuration
    pub fn new() -> Self {
        Self {
            package: None,
            workspace: None,
            nexus: None,
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            patch: PatchMap::new(),
            source_path: None,
            config_type: MagnetConfigType::default(),
        }
    }

    /// Get the package name
    pub fn get_name(&self) -> Option<String> {
        let name = self.package.as_ref()?.name.clone();
        if name.is_empty() {
            return None;
        }
        Some(name)
    }

    /// Get the package version
    pub fn get_version(&self) -> Option<String> {
        let version = self.package.as_ref()?.version.clone();
        Some(version)
    }

    /// Get the package/project edition
    pub fn get_edition(&self) -> Option<String> {
        self.package.as_ref()?.edition.clone()
    }

    /// Get the package/project description
    pub fn get_description(&self) -> Option<String> {
        let desc = self.package.as_ref()?.description.clone();
        Some(desc)
    }

    /// Get a formatted name for a node in the tree display based on config
    pub fn get_node_display_name(&self, dir: &Path) -> String {
        // Check if we have a package name from either package or project section
        let package_name = self.get_name();

        // Get the directory name for cases where we need it
        let dir_name = dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or_else(|| dir.to_str().unwrap_or("[invalid-path]"));

        // If we have a specific path, use that directly
        if dir.to_string_lossy() == "." {
            return "./".to_string();
        }

        match package_name {
            Some(name) => {
                // For nexus and workspace configurations, include the directory name
                if self.config_type() == MagnetConfigType::Nexus
                    || self.config_type() == MagnetConfigType::Workspace
                {
                    format!("{} ({})", name, dir_name)
                } else {
                    // For regular packages, just use the package name
                    name
                }
            }
            None => {
                // If no package name is available, use the directory name
                dir_name.to_string()
            }
        }
    }

    /// Read and parse a Cargo.toml file
    pub fn read_cargo_toml(path: &Path) -> Result<toml::Value> {
        // Read the Cargo.toml file
        let content = std::fs::read_to_string(path)
            .context(format!("Failed to read Cargo.toml at {}", path.display()))?;

        // Parse the TOML
        let cargo_toml: toml::Value = toml::from_str(&content)
            .context(format!("Failed to parse Cargo.toml at {}", path.display()))?;

        Ok(cargo_toml)
    }

    /// Get the crate name from a Cargo.toml file
    pub fn get_cargo_crate_name(path: &Path) -> Result<Option<String>> {
        let cargo_toml = Self::read_cargo_toml(path)?;

        let crate_name = cargo_toml
            .get("package")
            .and_then(|p| p.get("name"))
            .and_then(|n| n.as_str())
            .map(|s| s.to_string());

        Ok(crate_name)
    }

    /// Get dependencies from a Cargo.toml file
    pub fn get_cargo_dependencies(path: &Path) -> Result<Vec<String>> {
        let cargo_toml = Self::read_cargo_toml(path)?;

        let deps = cargo_toml
            .get("dependencies")
            .and_then(|d| d.as_table())
            .map(|t| t.keys().cloned().collect::<Vec<String>>())
            .unwrap_or_default();

        Ok(deps)
    }
}

impl Default for ManifestConfig {
    fn default() -> Self {
        Self::new()
    }
}
