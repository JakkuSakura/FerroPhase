//! Package configuration for Magnet.toml files

use crate::configs::DependencyConfigMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Package-specific configuration
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct PackageConfig {
    /// Name of the package
    pub name: String,
    /// Version of the package
    pub version: String,
    /// Description of the package
    #[serde(default)]
    pub description: String,
    pub edition: Option<String>,
    /// Authors of the package
    #[serde(default)]
    pub authors: Vec<String>,
    /// Package homepage
    #[serde(default)]
    pub homepage: Option<String>,
    /// Package repository
    #[serde(default)]
    pub repository: Option<String>,
    /// Package documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// Package license
    #[serde(default)]
    pub license: Option<String>,
    /// Custom package metadata
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CargoPackageConfig {
    /// Name of the package
    pub name: String,
    /// Version of the package
    pub version: String,
    pub edition: String,
    /// Description of the package
    #[serde(default)]
    pub description: String,
    /// Authors of the package
    #[serde(default)]
    pub authors: Vec<String>,
    /// Package homepage
    #[serde(default)]
    pub homepage: Option<String>,
    /// Package repository
    #[serde(default)]
    pub repository: Option<String>,
    /// Package documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    /// Package license
    #[serde(default)]
    pub license: Option<String>,
}
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct CargoPackageConfigWrapper {
    pub package: CargoPackageConfig,
    pub dependencies: DependencyConfigMap,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<toml::value::Table>,
}
