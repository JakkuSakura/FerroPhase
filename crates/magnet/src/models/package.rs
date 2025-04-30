use crate::configs::{DependencyMap, PackageConfig};
use crate::MagnetConfig;
use eyre::ContextCompat;
use eyre::Result;
use std::collections::HashMap;
use std::path::Path;

/// Package-specific configuration
#[derive(Debug, Clone, Default)]
pub struct PackageModel {
    /// Name of the package
    pub name: String,
    /// Version of the package
    pub version: String,
    pub edition: String,
    /// Description of the package
    pub description: String,
    /// Authors of the package
    pub authors: Vec<String>,
    /// Package homepage
    pub homepage: Option<String>,
    /// Package repository
    pub repository: Option<String>,
    /// Package documentation URL
    pub documentation: Option<String>,
    /// Package license
    pub license: Option<String>,
    /// Custom package metadata
    pub custom: HashMap<String, toml::Value>,
    pub dependencies: DependencyMap,
}
impl PackageModel {
    pub fn from_config_file(config: MagnetConfig, path: &Path) -> Result<Self> {
        let package = config
            .package
            .clone()
            .with_context(|| format!("No package found in {}", path.display()))?;
        // Create a new PackageModel instance
        let model = PackageModel {
            name: package.name,
            version: package.version,
            edition: config.get_edition().unwrap_or("2024".to_string()),
            description: package.description,
            authors: package.authors,
            homepage: package.homepage,
            repository: package.repository,
            documentation: package.documentation,
            license: package.license,
            custom: package.custom,
            dependencies: config.dependencies,
        };

        Ok(model)
    }
}
impl From<PackageConfig> for PackageModel {
    fn from(config: PackageConfig) -> Self {
        Self {
            name: config.name,
            version: config.version,
            edition: "".to_string(),
            description: config.description,
            authors: config.authors,
            homepage: config.homepage,
            repository: config.repository,
            documentation: config.documentation,
            license: config.license,
            custom: config.custom,
            dependencies: Default::default(),
        }
    }
}
