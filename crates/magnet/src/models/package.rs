use crate::configs::ManifestConfig;
use crate::models::DependencyModelMap;
use eyre::ContextCompat;
use eyre::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

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
    pub dependencies: DependencyModelMap,
    /// Patch section for overriding dependencies
    pub patch: Option<toml::value::Table>,
    pub root_path: PathBuf,
    pub source_path: PathBuf,
}
impl PackageModel {
    pub fn from_dir(root_path: &Path) -> Result<Self> {
        let root_path = root_path.canonicalize()?;
        if !root_path.exists() {
            eyre::bail!(
                "Root path doesn't exist in the current directory: {}",
                root_path.display()
            )
        }
        let config_path = if root_path.join("Magnet.toml").exists() {
            root_path.join("Magnet.toml")
        } else if root_path.join("Cargo.toml").exists() {
            root_path.join("Cargo.toml")
        } else {
            eyre::bail!(
                "Root path must point to Cargo.toml or Magnet.toml: {}",
                root_path.display()
            )
        };
        let config = ManifestConfig::from_file(&config_path)?;

        let package = config
            .package
            .clone()
            .with_context(|| format!("No package found in {}", root_path.display()))?;
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
            dependencies: config.dependencies
                .clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            patch: config.patch,
            root_path: root_path.to_path_buf(),
            source_path: config_path,
        };

        Ok(model)
    }
}
