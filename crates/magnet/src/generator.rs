use crate::config::DependencyConfig;
use crate::workspace_manager::{CrateInfo, WorkspaceManager};
use crate::{DependencyResolver, MagnetConfig};
use anyhow::{Context, Result};
use std::path::Path;

/// Cargo.toml generator
pub struct CargoGenerator {
    /// Workspace manager
    workspace_manager: WorkspaceManager,
    /// Dependency resolver
    resolver: DependencyResolver,
}

impl CargoGenerator {
    /// Create a new generator
    pub fn new(workspace_manager: WorkspaceManager, resolver: DependencyResolver) -> Self {
        Self {
            workspace_manager,
            resolver,
        }
    }

    /// Generate all Cargo.toml files
    pub fn generate_all(&mut self) -> Result<()> {
        // First, generate the root Cargo.toml
        self.generate_root_cargo_toml()?;

        // Then, generate Cargo.toml for each crate
        for crate_info in self.workspace_manager.get_all_crates() {
            self.generate_crate_cargo_toml(&crate_info)?;
        }

        Ok(())
    }

    /// Generate the root Cargo.toml file
    fn generate_root_cargo_toml(&self) -> Result<()> {
        // Get the workspace root path
        let workspace_root = self.workspace_manager.get_root_path();

        // Path to the root Cargo.toml
        let cargo_toml_path = workspace_root.join("Cargo.toml");

        // Create a new workspace manifest
        let workspace_manifest = self.generate_workspace_manifest()?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&workspace_manifest)
            .context("Failed to convert workspace manifest to TOML")?;

        // Write to file
        std::fs::write(&cargo_toml_path, toml_string)
            .context(format!("Failed to write to {}", cargo_toml_path.display()))?;

        Ok(())
    }

    /// Generate a Cargo.toml for a specific crate
    fn generate_crate_cargo_toml(&mut self, crate_info: &CrateInfo) -> Result<()> {
        // Skip if the crate has a custom config (don't overwrite)
        if crate_info.has_custom_config {
            return Ok(());
        }

        // Generate the crate manifest
        let crate_manifest = self.generate_crate_manifest(crate_info)?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&crate_manifest)
            .context("Failed to convert crate manifest to TOML")?;

        // Write to file
        std::fs::write(&crate_info.cargo_toml_path, toml_string).context(format!(
            "Failed to write to {}",
            crate_info.cargo_toml_path.display()
        ))?;

        Ok(())
    }

    /// Generate a workspace manifest
    fn generate_workspace_manifest(&self) -> Result<toml::Table> {
        // Get the workspace configuration
        let workspace_config = &self.workspace_manager.primary_workspace.config;

        // Create a new manifest
        let mut manifest = toml::Table::new();

        // Add package section if needed
        let mut package = toml::Table::new();

        if let Some(name) = &workspace_config.project.name {
            package.insert("name".to_string(), toml::Value::String(name.clone()));
        }

        if let Some(version) = &workspace_config.project.version {
            package.insert("version".to_string(), toml::Value::String(version.clone()));
        }

        if let Some(description) = &workspace_config.project.description {
            package.insert(
                "description".to_string(),
                toml::Value::String(description.clone()),
            );
        }

        if !workspace_config.project.authors.is_empty() {
            let authors = workspace_config
                .project
                .authors
                .iter()
                .map(|a| toml::Value::String(a.clone()))
                .collect::<Vec<_>>();

            package.insert("authors".to_string(), toml::Value::Array(authors));
        }

        if let Some(license) = &workspace_config.project.license {
            package.insert("license".to_string(), toml::Value::String(license.clone()));
        }

        if let Some(repository) = &workspace_config.project.repository {
            package.insert(
                "repository".to_string(),
                toml::Value::String(repository.clone()),
            );
        }

        if let Some(homepage) = &workspace_config.project.homepage {
            package.insert(
                "homepage".to_string(),
                toml::Value::String(homepage.clone()),
            );
        }

        if let Some(documentation) = &workspace_config.project.documentation {
            package.insert(
                "documentation".to_string(),
                toml::Value::String(documentation.clone()),
            );
        }

        if !package.is_empty() {
            manifest.insert("package".to_string(), toml::Value::Table(package));
        }

        // Add workspace section
        let mut workspace = toml::Table::new();

        // Add members
        let members = workspace_config.workspace.members.clone();
        let members_value = members
            .iter()
            .map(|m| toml::Value::String(m.clone()))
            .collect::<Vec<_>>();

        if !members_value.is_empty() {
            workspace.insert("members".to_string(), toml::Value::Array(members_value));
        }

        // Add exclude patterns
        let exclude = workspace_config.workspace.exclude.clone();

        if !exclude.is_empty() {
            let exclude_value = exclude
                .iter()
                .map(|e| toml::Value::String(e.clone()))
                .collect::<Vec<_>>();

            workspace.insert("exclude".to_string(), toml::Value::Array(exclude_value));
        }

        // Add resolver if specified
        if let Some(resolver) = &workspace_config.workspace.resolver {
            workspace.insert(
                "resolver".to_string(),
                toml::Value::String(resolver.clone()),
            );
        }

        // Add to manifest
        manifest.insert("workspace".to_string(), toml::Value::Table(workspace));

        Ok(manifest)
    }

    /// Generate a crate manifest
    fn generate_crate_manifest(&mut self, crate_info: &CrateInfo) -> Result<toml::Table> {
        // Create a new manifest
        let mut manifest = toml::Table::new();

        // Get the workspace configuration
        let workspace_config = &self.workspace_manager.primary_workspace.config;

        // Add package section
        let mut package = toml::Table::new();

        // If this crate has its own Magnet.toml, use that; otherwise use workspace defaults
        let package_config = if let Some(magnet_path) = &crate_info.magnet_toml_path {
            if magnet_path.exists() {
                match MagnetConfig::from_file(magnet_path) {
                    Ok(config) => config,
                    Err(_) => workspace_config.clone(),
                }
            } else {
                workspace_config.clone()
            }
        } else {
            workspace_config.clone()
        };

        // Set package name
        package.insert(
            "name".to_string(),
            toml::Value::String(crate_info.name.clone()),
        );

        // Set package version, with fallbacks
        let version = crate_info
            .version
            .clone()
            .or_else(|| package_config.project.version.clone())
            .or_else(|| workspace_config.project.version.clone())
            .unwrap_or_else(|| "0.1.0".to_string());

        package.insert("version".to_string(), toml::Value::String(version));

        // Set description
        if let Some(description) = package_config
            .project
            .description
            .as_ref()
            .or_else(|| workspace_config.project.description.as_ref())
        {
            package.insert(
                "description".to_string(),
                toml::Value::String(description.clone()),
            );
        }

        // Set authors
        let authors = if !package_config.project.authors.is_empty() {
            package_config.project.authors.clone()
        } else {
            workspace_config.project.authors.clone()
        };

        if !authors.is_empty() {
            let authors_value = authors
                .iter()
                .map(|a| toml::Value::String(a.clone()))
                .collect::<Vec<_>>();

            package.insert("authors".to_string(), toml::Value::Array(authors_value));
        }

        // Set license
        if let Some(license) = package_config
            .project
            .license
            .as_ref()
            .or_else(|| workspace_config.project.license.as_ref())
        {
            package.insert("license".to_string(), toml::Value::String(license.clone()));
        }

        // Set repository
        if let Some(repository) = package_config
            .project
            .repository
            .as_ref()
            .or_else(|| workspace_config.project.repository.as_ref())
        {
            package.insert(
                "repository".to_string(),
                toml::Value::String(repository.clone()),
            );
        }

        // Set homepage
        if let Some(homepage) = package_config
            .project
            .homepage
            .as_ref()
            .or_else(|| workspace_config.project.homepage.as_ref())
        {
            package.insert(
                "homepage".to_string(),
                toml::Value::String(homepage.clone()),
            );
        }

        // Set documentation
        if let Some(documentation) = package_config
            .project
            .documentation
            .as_ref()
            .or_else(|| workspace_config.project.documentation.as_ref())
        {
            package.insert(
                "documentation".to_string(),
                toml::Value::String(documentation.clone()),
            );
        }

        // Set workspace = true to indicate this is part of a workspace
        package.insert("workspace".to_string(), toml::Value::Boolean(true));

        // Add the package section to the manifest
        manifest.insert("package".to_string(), toml::Value::Table(package));

        // Add dependencies
        let mut dependencies = toml::Table::new();

        // Resolve dependencies
        let resolved_deps = self.resolver.resolve_dependencies(
            &crate_info.name,
            &package_config.dependencies,
            &workspace_config.dependencies,
        )?;

        // Add all resolved dependencies
        for (name, dep) in resolved_deps {
            // Add the dependency to the manifest
            match dep {
                DependencyConfig::Simple(version) => {
                    dependencies.insert(name, toml::Value::String(version));
                }
                DependencyConfig::Detailed(detailed) => {
                    let mut dep_table = toml::Table::new();

                    // Add the detailed dependency configuration
                    if let Some(version) = detailed.version {
                        dep_table.insert("version".to_string(), toml::Value::String(version));
                    }

                    if let Some(path) = detailed.path {
                        // Convert path to a relative path if possible
                        let rel_path = pathdiff::diff_paths(
                            &path,
                            crate_info
                                .cargo_toml_path
                                .parent()
                                .unwrap_or(Path::new(".")),
                        )
                        .unwrap_or_else(|| path.clone());

                        dep_table.insert(
                            "path".to_string(),
                            toml::Value::String(rel_path.to_string_lossy().to_string()),
                        );
                    }

                    if let Some(git) = detailed.git {
                        dep_table.insert("git".to_string(), toml::Value::String(git));
                    }

                    if let Some(branch) = detailed.branch {
                        dep_table.insert("branch".to_string(), toml::Value::String(branch));
                    }

                    if let Some(tag) = detailed.tag {
                        dep_table.insert("tag".to_string(), toml::Value::String(tag));
                    }

                    if let Some(rev) = detailed.rev {
                        dep_table.insert("rev".to_string(), toml::Value::String(rev));
                    }

                    if let Some(features) = detailed.features {
                        let features_value = features
                            .iter()
                            .map(|f| toml::Value::String(f.clone()))
                            .collect::<Vec<_>>();

                        dep_table
                            .insert("features".to_string(), toml::Value::Array(features_value));
                    }

                    if let Some(all_features) = detailed.all_features {
                        dep_table.insert(
                            "all-features".to_string(),
                            toml::Value::Boolean(all_features),
                        );
                    }

                    if let Some(default_features) = detailed.default_features {
                        dep_table.insert(
                            "default-features".to_string(),
                            toml::Value::Boolean(default_features),
                        );
                    }

                    if let Some(workspace) = detailed.workspace {
                        dep_table.insert("workspace".to_string(), toml::Value::Boolean(workspace));
                    }

                    if let Some(optional) = detailed.optional {
                        dep_table.insert("optional".to_string(), toml::Value::Boolean(optional));
                    }

                    if let Some(package) = detailed.package {
                        dep_table.insert("package".to_string(), toml::Value::String(package));
                    }

                    if let Some(registry) = detailed.registry {
                        dep_table.insert("registry".to_string(), toml::Value::String(registry));
                    }

                    dependencies.insert(name, toml::Value::Table(dep_table));
                }
            }
        }

        // Add the dependencies section to the manifest if not empty
        if !dependencies.is_empty() {
            manifest.insert("dependencies".to_string(), toml::Value::Table(dependencies));
        }

        // Add dev-dependencies if any
        let mut dev_dependencies = toml::Table::new();

        // Resolve dev-dependencies
        let resolved_dev_deps = self.resolver.resolve_dependencies(
            &crate_info.name,
            &package_config.dev_dependencies,
            &workspace_config.dev_dependencies,
        )?;

        // Add all resolved dev-dependencies
        for (name, dep) in resolved_dev_deps {
            // Add the dependency to the manifest in the same way as for regular dependencies
            match dep {
                DependencyConfig::Simple(version) => {
                    dev_dependencies.insert(name, toml::Value::String(version));
                }
                DependencyConfig::Detailed(detailed) => {
                    let mut dep_table = toml::Table::new();

                    // Add the detailed dependency configuration (same as above)
                    if let Some(version) = detailed.version {
                        dep_table.insert("version".to_string(), toml::Value::String(version));
                    }

                    if let Some(path) = detailed.path {
                        // Convert path to a relative path if possible
                        let rel_path = pathdiff::diff_paths(
                            &path,
                            crate_info
                                .cargo_toml_path
                                .parent()
                                .unwrap_or(Path::new(".")),
                        )
                        .unwrap_or_else(|| path.clone());

                        dep_table.insert(
                            "path".to_string(),
                            toml::Value::String(rel_path.to_string_lossy().to_string()),
                        );
                    }

                    // Add other fields as needed (similar to regular dependencies)
                    // ...

                    dev_dependencies.insert(name, toml::Value::Table(dep_table));
                }
            }
        }

        // Add the dev-dependencies section to the manifest if not empty
        if !dev_dependencies.is_empty() {
            manifest.insert(
                "dev-dependencies".to_string(),
                toml::Value::Table(dev_dependencies),
            );
        }

        // Add build-dependencies if any
        let mut build_dependencies = toml::Table::new();

        // Resolve build-dependencies
        let resolved_build_deps = self.resolver.resolve_dependencies(
            &crate_info.name,
            &package_config.build_dependencies,
            &workspace_config.build_dependencies,
        )?;

        // Add all resolved build-dependencies
        for (name, dep) in resolved_build_deps {
            // Similar to regular dependencies...
            match dep {
                DependencyConfig::Simple(version) => {
                    build_dependencies.insert(name, toml::Value::String(version));
                }
                DependencyConfig::Detailed(detailed) => {
                    // Similar to regular dependencies...
                    let mut dep_table = toml::Table::new();

                    if let Some(version) = detailed.version {
                        dep_table.insert("version".to_string(), toml::Value::String(version));
                    }

                    // Add other fields as needed

                    build_dependencies.insert(name, toml::Value::Table(dep_table));
                }
            }
        }

        // Add the build-dependencies section to the manifest if not empty
        if !build_dependencies.is_empty() {
            manifest.insert(
                "build-dependencies".to_string(),
                toml::Value::Table(build_dependencies),
            );
        }

        // Add custom metadata
        for (key, value) in &package_config.project.custom {
            // Skip fields we've already handled
            if [
                "name",
                "version",
                "description",
                "authors",
                "license",
                "repository",
                "homepage",
                "documentation",
            ]
            .contains(&key.as_str())
            {
                continue;
            }

            // Add custom fields at the root
            manifest.insert(key.clone(), value.clone());
        }

        Ok(manifest)
    }
}
