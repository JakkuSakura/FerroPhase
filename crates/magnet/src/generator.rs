use crate::DependencyResolver;
use crate::configs::{CargoPackageConfig, CargoWorkspaceConfig};
use crate::manager::WorkspaceManager;
use crate::models::PackageModel;
use eyre::{Context, ContextCompat, Result};

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
        self.generate_workspace_cargo_toml()?;
        for package in self.workspace_manager.primary_workspace.packages.clone() {
            self.generate_package_cargo_toml(&package)?;
        }

        Ok(())
    }

    /// Generate the root Cargo.toml file
    fn generate_workspace_cargo_toml(&self) -> Result<()> {
        // Get the workspace root path
        let workspace_root = self.workspace_manager.root_path();

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

    /// Generate a workspace manifest
    fn generate_workspace_manifest(&self) -> Result<CargoWorkspaceConfig> {
        // Get the workspace configuration
        let workspace_config = &self.workspace_manager.primary_workspace;

        // Create a new workspace manifest
        let mut manifest = CargoWorkspaceConfig::new();

        // Set the members
        manifest.members = self
            .workspace_manager
            .get_all_crates()
            .iter()
            .map(|crate_info| crate_info.path.to_string_lossy().to_string())
            .collect();

        // Set the resolver
        manifest.resolver = workspace_config.resolver.clone();

        // Set the dependencies
        manifest.dependencies = workspace_config.dependencies.clone();

        Ok(manifest)
    }
    // FIXME: distinguish package vs crate
    fn generate_package_cargo_toml(&mut self, package: &PackageModel) -> Result<()> {
        // Get the package path
        let package_path = self
            .workspace_manager
            .get_package_path(&package.name)
            .context(format!("No path found for package {}", package.name))?;

        // Path to the package Cargo.toml
        let cargo_toml_path = package_path.join("Cargo.toml");

        // Create a new package manifest
        let package_manifest = self.generate_package_manifest(package)?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&package_manifest)
            .context("Failed to convert package manifest to TOML")?;

        // Write to file
        std::fs::write(&cargo_toml_path, toml_string)
            .context(format!("Failed to write to {}", cargo_toml_path.display()))?;

        Ok(())
    }
    /// Generate a crate manifest
    fn generate_package_manifest(&mut self, model: &PackageModel) -> Result<CargoPackageConfig> {
        let dependencies = self.resolver.resolve_dependencies(
            &model.name,
            &model.dependencies,
            &self.workspace_manager.get_all_dependencies(),
        )?;

        let manifest = CargoPackageConfig {
            name: model.name.clone(),
            version: model.version.clone(),
            edition: model.edition.clone(),
            description: model.description.clone(),
            license: model.license.clone(),
            authors: model.authors.clone(),
            homepage: model.homepage.clone(),
            repository: model.repository.clone(),
            documentation: model.documentation.clone(),
            dependencies,
        };
        Ok(manifest)
    }
}
