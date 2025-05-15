use crate::configs::{CargoPackageConfig, CargoPackageConfigWrapper, CargoWorkspaceConfig, CargoWorkspaceConfigWrapper};
use crate::manager::NexusManager;
use crate::models::PackageModel;
use crate::DependencyResolver;
use eyre::{Context, Result};

/// Cargo.toml generator
pub struct CargoGenerator {
    /// Nexus manager
    nexus_manager: NexusManager,
    /// Dependency resolver
    resolver: DependencyResolver,
}

impl CargoGenerator {
    /// Create a new generator
    pub fn new(nexus_manager: NexusManager, resolver: DependencyResolver) -> Self {
        Self {
            nexus_manager,
            resolver,
        }
    }

    /// Generate all Cargo.toml files for a specific workspace
    pub fn generate_all(&mut self, workspace_name: &str) -> Result<()> {
        // First, generate the root Cargo.toml
        self.generate_workspace_cargo_toml(workspace_name)?;

        // Get the specified workspace
        let workspace = match self.nexus_manager.get_workspace(workspace_name) {
            Some(ws) => ws.clone(),
            None => return Err(eyre::eyre!("Workspace '{}' not found", workspace_name)),
        };
        // Generate for all packages in the specified workspace
        for member in workspace.get_members()? {
            let package = PackageModel::from_root_path(&workspace.root_path.join(member))?;
            self.generate_package_cargo_toml(&package, workspace_name)?;
        }

        Ok(())
    }

    /// Generate the root Cargo.toml file for a specific workspace
    fn generate_workspace_cargo_toml(&self, workspace_name: &str) -> Result<()> {
        // Get the workspace root path
        let workspace_root = match self.nexus_manager.root_path(workspace_name) {
            Some(path) => path,
            None => return Err(eyre::eyre!("Workspace '{}' not found", workspace_name)),
        };

        // Path to the root Cargo.toml
        let cargo_toml_path = workspace_root.join("Cargo.toml");
        println!("Generating Cargo.toml at {}", cargo_toml_path.display());

        // Create a new workspace manifest
        let workspace_manifest = self.generate_workspace_manifest(workspace_name)?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&CargoWorkspaceConfigWrapper {
            workspace: workspace_manifest,
        })
        .context("Failed to convert workspace manifest to TOML")?;

        // Write to file
        std::fs::write(&cargo_toml_path, toml_string)
            .context(format!("Failed to write to {}", cargo_toml_path.display()))?;

        Ok(())
    }

    /// Generate a workspace manifest for a specific workspace
    fn generate_workspace_manifest(&self, workspace_name: &str) -> Result<CargoWorkspaceConfig> {
        // Get the workspace configuration
        let workspace_config = match self.nexus_manager.get_workspace(workspace_name) {
            Some(ws) => ws,
            None => return Err(eyre::eyre!("Workspace '{}' not found", workspace_name)),
        };

        // Create a new workspace manifest
        let manifest = CargoWorkspaceConfig {
            members: workspace_config.members.clone(),
            exclude: workspace_config.exclude.clone(),
            resolver: workspace_config.resolver.clone(),
            dependencies: workspace_config.dependencies.clone(),
        };

        Ok(manifest)
    }

    fn generate_package_cargo_toml(
        &mut self,
        package: &PackageModel,
        workspace_name: &str,
    ) -> Result<()> {
        // Get the package path
        let package_path = package.root_path.as_path();
        // Path to the package Cargo.toml
        let cargo_toml_path = package_path.join("Cargo.toml");

        // Create a new package manifest
        let package_manifest = self.generate_package_manifest(package, workspace_name)?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&package_manifest)
            .context("Failed to convert package manifest to TOML")?;

        // Write to file
        std::fs::write(&cargo_toml_path, toml_string)
            .context(format!("Failed to write to {}", cargo_toml_path.display()))?;

        Ok(())
    }

    /// Generate a crate manifest
    fn generate_package_manifest(
        &mut self,
        model: &PackageModel,
        workspace_name: &str,
    ) -> Result<CargoPackageConfigWrapper> {
        let workspace_dependencies = self
            .nexus_manager
            .get_workspace_dependencies(workspace_name);

        let dependencies = self.resolver.resolve_dependencies(
            &model.name,
            &model.dependencies,
            &workspace_dependencies,
        )?;

        let package = CargoPackageConfig {
            name: model.name.clone(),
            version: model.version.clone(),
            edition: model.edition.clone(),
            description: model.description.clone(),
            license: model.license.clone(),
            authors: model.authors.clone(),
            homepage: model.homepage.clone(),
            repository: model.repository.clone(),
            documentation: model.documentation.clone(),
        };
        Ok(CargoPackageConfigWrapper {
            package,
            dependencies,
        })
    }
}
