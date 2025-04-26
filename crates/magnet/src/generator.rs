//! Cargo.toml generator
//!
//! This module is responsible for generating Cargo.toml files from
//! Magnet.toml configuration and resolved dependencies.

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::config::{DetailedDependency, MagnetConfig};
use crate::resolver::{DependencyResolver, ResolutionStatus, ResolvedDependency};
use crate::workspace::{CrateInfo, WorkspaceManager};

/// Represents a Cargo.toml package section
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CargoPackage {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Package description
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Package authors
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub authors: Vec<String>,
    /// Package edition (e.g., "2021")
    #[serde(default = "default_edition")]
    pub edition: String,
    /// Package license
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    /// Package repository
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    /// Package homepage
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,
    /// Package documentation
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
    /// Custom package metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

fn default_edition() -> String {
    "2021".to_string()
}

/// Represents a Cargo.toml workspace section
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CargoWorkspace {
    /// Workspace members
    pub members: Vec<String>,
    /// Excluded workspace members
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exclude: Vec<String>,
    /// Resolver version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resolver: Option<String>,
    /// Custom workspace metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

/// Represents a Cargo.toml dependency
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum CargoDependency {
    /// Simple version string
    Simple(String),
    /// Detailed dependency configuration
    Detailed(CargoDetailedDependency),
}

/// Represents a detailed Cargo.toml dependency
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CargoDetailedDependency {
    /// Dependency version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    /// Path to local dependency
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    /// Git repository URL
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    /// Git branch
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
    /// Git tag
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,
    /// Git revision
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub rev: Option<String>,
    /// Dependency features to enable
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,
    /// Whether all features should be enabled
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub all_features: Option<bool>,
    /// Whether default features should be enabled
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default_features: Option<bool>,
    /// Whether to use the version defined in the workspace
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace: Option<bool>,
    /// Optional dependency
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,
    /// Package name (if different from dependency name)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    /// Registry to use
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,
    /// Artifact to use
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub artifact: Option<String>,
    /// Target to use
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    /// Custom dependency metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

/// Full Cargo.toml structure
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CargoToml {
    /// Package section (for crates)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<CargoPackage>,
    /// Workspace section (for workspaces)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<CargoWorkspace>,
    /// Dependencies
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dependencies: HashMap<String, CargoDependency>,
    /// Development dependencies
    #[serde(
        default,
        skip_serializing_if = "HashMap::is_empty",
        rename = "dev-dependencies"
    )]
    pub dev_dependencies: HashMap<String, CargoDependency>,
    /// Build dependencies
    #[serde(
        default,
        skip_serializing_if = "HashMap::is_empty",
        rename = "build-dependencies"
    )]
    pub build_dependencies: HashMap<String, CargoDependency>,
    /// Features
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub features: HashMap<String, Vec<String>>,
    /// Target-specific dependencies
    #[serde(flatten)]
    pub targets: HashMap<String, toml::Value>,
    /// Any other custom sections
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

/// Generator for Cargo.toml files from Magnet.toml configurations
pub struct CargoGenerator {
    /// Workspace manager
    workspace_manager: WorkspaceManager,
    /// Dependency resolver
    dependency_resolver: DependencyResolver,
}

impl CargoGenerator {
    /// Create a new CargoGenerator with the given workspace manager and resolver
    pub fn new(
        workspace_manager: WorkspaceManager,
        dependency_resolver: DependencyResolver,
    ) -> Self {
        Self {
            workspace_manager,
            dependency_resolver,
        }
    }

    /// Generate Cargo.toml files for all crates in the primary workspace
    pub fn generate_all(&mut self) -> Result<()> {
        // First, generate the workspace-level Cargo.toml
        self.generate_workspace_cargo_toml()?;

        // Then generate Cargo.toml for each crate
        let all_crates = self.workspace_manager.primary_workspace.all_crates();
        for (_, crate_info) in all_crates {
            self.generate_crate_cargo_toml(&crate_info)?;
        }

        Ok(())
    }

    /// Generate the workspace-level Cargo.toml
    fn generate_workspace_cargo_toml(&self) -> Result<()> {
        // Get the workspace root
        let workspace_root = &self.workspace_manager.primary_workspace.root_path;
        let workspace_config = &self.workspace_manager.primary_workspace.config;

        // Create the workspace section
        let workspace_section = CargoWorkspace {
            members: workspace_config.workspace.members.clone(),
            exclude: workspace_config.workspace.exclude.clone(),
            resolver: workspace_config.workspace.resolver.clone(),
            custom: workspace_config.workspace.custom.clone(),
        };

        // Create the package section if project metadata is available
        let package_section = if let Some(name) = &workspace_config.project.name {
            Some(CargoPackage {
                name: name.clone(),
                version: workspace_config
                    .project
                    .version
                    .clone()
                    .unwrap_or_else(|| "0.1.0".to_string()),
                description: workspace_config.project.description.clone(),
                authors: workspace_config.project.authors.clone(),
                edition: "2021".to_string(),
                license: workspace_config.project.license.clone(),
                repository: workspace_config.project.repository.clone(),
                homepage: workspace_config.project.homepage.clone(),
                documentation: workspace_config.project.documentation.clone(),
                custom: workspace_config.project.custom.clone(),
            })
        } else {
            None
        };

        // Create the Cargo.toml structure
        let cargo_toml = CargoToml {
            package: package_section,
            workspace: Some(workspace_section),
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            features: HashMap::new(),
            targets: HashMap::new(),
            custom: HashMap::new(),
        };

        // Write to file
        let cargo_toml_path = workspace_root.join("Cargo.toml");
        self.write_cargo_toml(&cargo_toml, &cargo_toml_path)?;

        Ok(())
    }

    /// Generate Cargo.toml for a specific crate
    fn generate_crate_cargo_toml(&mut self, crate_info: &CrateInfo) -> Result<()> {
        // Get the workspace and crate configurations
        let workspace_config = &self.workspace_manager.primary_workspace.config;

        // Load crate-specific config if available
        let crate_config = if let Some(magnet_toml_path) = &crate_info.magnet_toml_path {
            match MagnetConfig::from_file(magnet_toml_path) {
                Ok(config) => Some(config),
                Err(e) => {
                    eprintln!(
                        "Warning: Failed to load crate-specific Magnet.toml for {}: {}",
                        crate_info.name, e
                    );
                    None
                }
            }
        } else {
            None
        };

        // Create the package section
        let package_section = CargoPackage {
            name: crate_info.name.clone(),
            version: crate_config
                .as_ref()
                .and_then(|c| c.project.version.clone())
                .or_else(|| workspace_config.project.version.clone())
                .unwrap_or_else(|| "0.1.0".to_string()),
            description: crate_config
                .as_ref()
                .and_then(|c| c.project.description.clone())
                .or_else(|| workspace_config.project.description.clone()),
            authors: if let Some(config) = &crate_config {
                if !config.project.authors.is_empty() {
                    config.project.authors.clone()
                } else {
                    workspace_config.project.authors.clone()
                }
            } else {
                workspace_config.project.authors.clone()
            },
            edition: "2021".to_string(),
            license: crate_config
                .as_ref()
                .and_then(|c| c.project.license.clone())
                .or_else(|| workspace_config.project.license.clone()),
            repository: crate_config
                .as_ref()
                .and_then(|c| c.project.repository.clone())
                .or_else(|| workspace_config.project.repository.clone()),
            homepage: crate_config
                .as_ref()
                .and_then(|c| c.project.homepage.clone())
                .or_else(|| workspace_config.project.homepage.clone()),
            documentation: crate_config
                .as_ref()
                .and_then(|c| c.project.documentation.clone())
                .or_else(|| workspace_config.project.documentation.clone()),
            custom: HashMap::new(),
        };

        // Collect dependencies
        let mut dependencies = HashMap::new();
        let mut dev_dependencies = HashMap::new();
        let mut build_dependencies = HashMap::new();

        // Process workspace dependencies first
        for (dep_name, dep_config) in &workspace_config.dependencies {
            // Check if this dep should be included (not overridden by crate-specific config)
            let should_include = crate_config
                .as_ref()
                .map(|c| !c.dependencies.contains_key(dep_name))
                .unwrap_or(true);

            if should_include {
                // Resolve the dependency
                if let Some(resolved) = self.dependency_resolver.get_resolved_dependency(dep_name) {
                    dependencies.insert(dep_name.clone(), self.to_cargo_dependency(resolved)?);
                } else {
                    // Not yet resolved, resolve it now
                    match self.dependency_resolver.resolve_dependency(
                        dep_name,
                        dep_config,
                        Some(crate_info),
                    ) {
                        Ok(resolved) => {
                            dependencies
                                .insert(dep_name.clone(), self.to_cargo_dependency(&resolved)?);
                        }
                        Err(e) => {
                            eprintln!("Warning: Failed to resolve dependency {}: {}", dep_name, e);
                        }
                    }
                }
            }
        }

        // Process workspace dev dependencies
        for (dep_name, dep_config) in &workspace_config.dev_dependencies {
            // Similar logic as for regular dependencies
            let should_include = crate_config
                .as_ref()
                .map(|c| !c.dev_dependencies.contains_key(dep_name))
                .unwrap_or(true);

            if should_include {
                // Add to dev_dependencies
                dev_dependencies.insert(
                    dep_name.clone(),
                    self.to_cargo_dependency_from_config(dep_config)?,
                );
            }
        }

        // Process workspace build dependencies
        for (dep_name, dep_config) in &workspace_config.build_dependencies {
            // Similar logic as for regular dependencies
            let should_include = crate_config
                .as_ref()
                .map(|c| !c.build_dependencies.contains_key(dep_name))
                .unwrap_or(true);

            if should_include {
                // Add to build_dependencies
                build_dependencies.insert(
                    dep_name.clone(),
                    self.to_cargo_dependency_from_config(dep_config)?,
                );
            }
        }

        // Now process crate-specific dependencies
        if let Some(config) = &crate_config {
            for (dep_name, dep_config) in &config.dependencies {
                // Resolve the dependency
                if let Some(resolved) = self.dependency_resolver.get_resolved_dependency(dep_name) {
                    dependencies.insert(dep_name.clone(), self.to_cargo_dependency(resolved)?);
                } else {
                    // Not yet resolved, resolve it now
                    match self.dependency_resolver.resolve_dependency(
                        dep_name,
                        dep_config,
                        Some(crate_info),
                    ) {
                        Ok(resolved) => {
                            dependencies
                                .insert(dep_name.clone(), self.to_cargo_dependency(&resolved)?);
                        }
                        Err(e) => {
                            eprintln!("Warning: Failed to resolve dependency {}: {}", dep_name, e);
                        }
                    }
                }
            }

            // Process crate-specific dev dependencies
            for (dep_name, dep_config) in &config.dev_dependencies {
                dev_dependencies.insert(
                    dep_name.clone(),
                    self.to_cargo_dependency_from_config(dep_config)?,
                );
            }

            // Process crate-specific build dependencies
            for (dep_name, dep_config) in &config.build_dependencies {
                build_dependencies.insert(
                    dep_name.clone(),
                    self.to_cargo_dependency_from_config(dep_config)?,
                );
            }
        }

        // Create the Cargo.toml structure
        let cargo_toml = CargoToml {
            package: Some(package_section),
            workspace: None,
            dependencies,
            dev_dependencies,
            build_dependencies,
            features: HashMap::new(),
            targets: HashMap::new(),
            custom: HashMap::new(),
        };

        // Write to file
        self.write_cargo_toml(&cargo_toml, &crate_info.cargo_toml_path)?;

        Ok(())
    }

    /// Convert a resolved dependency to a Cargo dependency
    fn to_cargo_dependency(&self, resolved: &ResolvedDependency) -> Result<CargoDependency> {
        match &resolved.status {
            ResolutionStatus::Resolved(path) => {
                // Create a detailed dependency with the resolved path
                let mut cargo_dep = CargoDetailedDependency {
                    path: Some(path.to_string_lossy().to_string()),
                    ..Default::default()
                };

                // Copy other relevant fields from the resolved config
                let config = &resolved.resolved_config;

                cargo_dep.version = config.version.clone();
                cargo_dep.features = config.features.clone();
                cargo_dep.all_features = config.all_features;
                cargo_dep.default_features = config.default_features;
                cargo_dep.optional = config.optional;
                cargo_dep.package = config.package.clone();

                Ok(CargoDependency::Detailed(cargo_dep))
            }
            ResolutionStatus::NotFound => {
                // For external dependencies, just copy the original config
                self.to_cargo_dependency_from_detailed(&resolved.resolved_config)
            }
            ResolutionStatus::Ambiguous(paths) => {
                // This is an error condition
                bail!(
                    "Ambiguous dependency resolution for {}: found multiple matching crates at {:?}",
                    resolved.name,
                    paths
                );
            }
            ResolutionStatus::Error(msg) => {
                // This is an error condition
                bail!("Error resolving dependency {}: {}", resolved.name, msg);
            }
        }
    }

    /// Convert a detailed dependency configuration to a Cargo dependency
    fn to_cargo_dependency_from_detailed(
        &self,
        config: &DetailedDependency,
    ) -> Result<CargoDependency> {
        // If it's just a version, use a simple dependency
        if let Some(version) = &config.version {
            if config.path.is_none()
                && config.git.is_none()
                && config.features.is_none()
                && config.all_features.is_none()
                && config.default_features.is_none()
                && config.optional.is_none()
                && config.package.is_none()
            {
                return Ok(CargoDependency::Simple(version.clone()));
            }
        }

        // Otherwise, create a detailed dependency
        let cargo_dep = CargoDetailedDependency {
            version: config.version.clone(),
            path: config
                .path
                .as_ref()
                .map(|p| p.to_string_lossy().to_string()),
            git: config.git.clone(),
            branch: config.branch.clone(),
            tag: config.tag.clone(),
            rev: config.rev.clone(),
            features: config.features.clone(),
            all_features: config.all_features,
            default_features: config.default_features,
            workspace: config.workspace,
            optional: config.optional,
            package: config.package.clone(),
            registry: config.registry.clone(),
            artifact: config.artifact.clone(),
            target: config.target.clone(),
            custom: HashMap::new(),
        };

        Ok(CargoDependency::Detailed(cargo_dep))
    }

    /// Convert a dependency configuration to a Cargo dependency
    fn to_cargo_dependency_from_config(
        &self,
        config: &crate::config::DependencyConfig,
    ) -> Result<CargoDependency> {
        match config {
            crate::config::DependencyConfig::Simple(version) => {
                Ok(CargoDependency::Simple(version.clone()))
            }
            crate::config::DependencyConfig::Detailed(detailed) => {
                self.to_cargo_dependency_from_detailed(detailed)
            }
        }
    }

    /// Write a Cargo.toml structure to a file
    fn write_cargo_toml(&self, cargo_toml: &CargoToml, path: &Path) -> Result<()> {
        // Convert to TOML
        let toml = toml::to_string_pretty(cargo_toml).context("Failed to serialize Cargo.toml")?;

        // Write to file
        std::fs::write(path, toml)
            .context(format!("Failed to write Cargo.toml to {}", path.display()))?;

        Ok(())
    }
}
