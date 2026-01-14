use crate::configs::{CargoManifestConfig, ManifestConfig};
use crate::models::{DependencyModelMap, PatchMap, parse_patch_table};
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
    pub dev_dependencies: DependencyModelMap,
    pub build_dependencies: DependencyModelMap,
    /// Patch section for overriding dependencies
    pub patch: PatchMap,
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
        if config_path.file_name().and_then(|n| n.to_str()) == Some("Cargo.toml") {
            let config = CargoManifestConfig::from_path(&config_path)?;
            return Self::from_cargo_config(config, &config_path, &root_path);
        }

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
            dependencies: config
                .dependencies
                .clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            dev_dependencies: config
                .dev_dependencies
                .clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            build_dependencies: config
                .build_dependencies
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

    fn from_cargo_config(
        config: CargoManifestConfig,
        config_path: &Path,
        root_path: &Path,
    ) -> Result<Self> {
        let package = config
            .package
            .clone()
            .with_context(|| format!("No package found in {}", config_path.display()))?;
        let model = PackageModel {
            name: package.name,
            version: package.version,
            edition: package.edition.unwrap_or_else(|| "2015".to_string()),
            description: package.description,
            authors: package.authors,
            homepage: package.homepage,
            repository: package.repository,
            documentation: package.documentation,
            license: package.license,
            custom: HashMap::new(),
            dependencies: config
                .dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            dev_dependencies: config
                .dev_dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            build_dependencies: config
                .build_dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            patch: parse_patch_table(config.patch),
            root_path: root_path.to_path_buf(),
            source_path: config_path.to_path_buf(),
        };
        Ok(model)
    }
}
