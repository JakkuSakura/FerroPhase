// filepath: /home/jakku/Dev/SHLL/crates/magnet/src/generator.rs
use crate::configs::{ManifestConfig, WorkspaceConfig, PackageConfig};
use crate::manager::ManifestManager;
use crate::models::{PackageModel, WorkspaceModel};
use eyre::{Context, Result};
use tracing::info;

/// Cargo.toml generator
pub struct CargoGenerator {
    /// Nexus manager
    nexus_manager: ManifestManager,
}

impl CargoGenerator {
    /// Create a new generator
    pub fn new(nexus_manager: ManifestManager) -> Self {
        Self { nexus_manager }
    }

    /// Generate all Cargo.toml files for a specific workspace
    pub fn generate_all(&mut self, workspace: &WorkspaceModel) -> Result<()> {
        // First, generate the root Cargo.toml
        self.generate_workspace_cargo_toml(workspace)?;

        // Generate for all packages in the specified workspace
        for mut package in workspace.list_packages()? {
            self.generate_package_cargo_toml(&mut package)?;
        }

        Ok(())
    }

    /// Generate a workspace manifest for a specific workspace
    fn generate_workspace_manifest(
        &self,
        workspace: &WorkspaceModel,
    ) -> Result<ManifestConfig> {
        // Create a new manifest config
        let mut manifest = ManifestConfig::new();
        
        // Set workspace configuration
        let workspace_config = WorkspaceConfig {
            members: workspace.members.clone(),
            exclude: workspace.exclude.clone(),
            resolver: workspace.resolver.clone(),
            dependencies: workspace.dependencies.clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            custom: workspace.custom.clone(),
        };
        
        manifest.workspace = Some(workspace_config);
        
        // Add the patch section if it exists in the original config
        manifest.patch = workspace.patch.clone();

        Ok(manifest)
    }

    /// Generate the root Cargo.toml file for a specific workspace
    fn generate_workspace_cargo_toml(&self, workspace: &WorkspaceModel) -> Result<()> {
        // Path to the root Cargo.toml
        let cargo_toml_path = workspace.root_path.join("Cargo.toml");
        info!("Generating Cargo.toml at {}", cargo_toml_path.display());

        // Create a new workspace manifest using WorkspaceConfig
        let manifest = self.generate_workspace_manifest(&workspace)?;

        // Convert to TOML string
        let toml_string = toml::to_string_pretty(&manifest)
            .context("Failed to convert workspace manifest to TOML")?;

        // Write to file
        std::fs::write(&cargo_toml_path, toml_string)
            .context(format!("Failed to write to {}", cargo_toml_path.display()))?;

        Ok(())
    }

    fn generate_package_cargo_toml(&mut self, package: &mut PackageModel) -> Result<()> {
        // Get the package path
        let package_path = package.root_path.as_path();
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
    fn generate_package_manifest(
        &mut self,
        model: &mut PackageModel,
    ) -> Result<ManifestConfig> {
        self.nexus_manager.resolve_package_dependencies(model)?;

        // Create a new manifest config
        let mut manifest = ManifestConfig::new();
        
        // Create package section
        manifest.package = Some(PackageConfig {
            name: model.name.clone(),
            version: model.version.clone(),
            edition: Some(model.edition.clone()),
            description: model.description.clone(),
            license: model.license.clone(),
            authors: model.authors.clone(),
            homepage: model.homepage.clone(),
            repository: model.repository.clone(),
            documentation: model.documentation.clone(),
            custom: model.custom.clone(),
        });
        
        // Add dependencies
        manifest.dependencies = model
            .dependencies
            .clone()
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect();
            
        // Get the patch section if it exists in the source Magnet.toml file
        manifest.patch = model.patch.clone();

        Ok(manifest)
    }
}
