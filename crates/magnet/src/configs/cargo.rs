//! Cargo.toml configuration parsing for Magnet.

use crate::configs::{CargoPackageConfig, CargoWorkspaceConfig, DependencyConfigMap};
use eyre::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct CargoManifestConfig {
    #[serde(default)]
    pub package: Option<CargoPackageConfig>,
    #[serde(default)]
    pub workspace: Option<CargoWorkspaceConfig>,
    #[serde(default)]
    pub dependencies: DependencyConfigMap,
    #[serde(rename = "dev-dependencies", default)]
    pub dev_dependencies: DependencyConfigMap,
    #[serde(rename = "build-dependencies", default)]
    pub build_dependencies: DependencyConfigMap,
    #[serde(default)]
    pub patch: Option<toml::value::Table>,
    #[serde(skip)]
    pub source_path: Option<PathBuf>,
}

impl CargoManifestConfig {
    pub fn from_path(path: &Path) -> Result<Self> {
        let path = path.canonicalize().with_context(|| {
            format!("Failed to canonicalize path for Cargo.toml: {}", path.display())
        })?;
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read Cargo.toml from {}", path.display()))?;
        let mut config: Self =
            toml::from_str(&content).with_context(|| format!("Failed to parse {}", path.display()))?;
        config.source_path = Some(path);
        Ok(config)
    }
}
