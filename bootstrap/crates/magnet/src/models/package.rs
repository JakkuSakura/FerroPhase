use std::path::PathBuf;

use crate::configs::PackageConfig;
use crate::models::DependencyModel;

#[derive(Debug, Clone)]
pub struct PackageModel {
    pub name: String,
    pub version: Option<String>,
    pub root_path: PathBuf,
    pub manifest_path: PathBuf,
    pub metadata: PackageConfig,
    pub dependencies: Vec<DependencyModel>,
}
