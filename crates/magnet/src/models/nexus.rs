//! Domain model for a Nexus, which represents a collection of workspaces.

use crate::configs::ManifestConfig;
use crate::models::{PackageModel, PatchMap, WorkspaceModel};
use crate::utils::glob_relative;
use eyre::ContextCompat;
use eyre::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A nexus model representing a collection of workspaces
#[derive(Debug, Clone)]
pub struct NexusModel {
    /// Name of the nexus
    pub name: String,
    /// Version of the nexus
    pub version: Option<String>,
    /// Description of the nexus
    pub description: Option<String>,
    /// Workspaces included in this nexus (patterns)
    pub members: Vec<String>,
    /// Workspaces excluded from this nexus (patterns)
    pub exclude: Vec<String>,
    pub patch: PatchMap,
    /// Custom nexus metadata
    pub custom: HashMap<String, toml::Value>,
    pub root_path: PathBuf,
    /// Source path of the nexus configuration
    pub source_path: PathBuf,
}

impl NexusModel {
    /// Create a nexus model from a config, with additional source path information
    pub fn from_dir(root_path: &Path) -> Result<Self> {
        let root_path = root_path.canonicalize()?;
        let config_path = root_path.join("Magnet.toml");
        let config = ManifestConfig::from_file(&config_path)?;
        let config1 = config.nexus.with_context(|| {
            format!("No nexus configuration found in {}", config_path.display())
        })?;
        let name = config1.name.unwrap_or_else(|| {
            root_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned()
        });
        let source_path = root_path.to_path_buf();
        let model = NexusModel {
            name,
            version: config1.version,
            description: config1.description,
            members: config1.members,
            exclude: config1.exclude,
            custom: config1.custom.clone(),
            patch: config.patch,
            root_path,
            source_path,
        };

        Ok(model)
    }
    pub fn list_members(&self) -> Result<Vec<PathBuf>> {
        let mut valid_members = Vec::new();
        for pattern in &self.members {
            valid_members.extend(glob_relative(&self.source_path, pattern, true)?);
        }
        Ok(valid_members)
    }
    pub fn list_workspaces(&self) -> Result<Vec<WorkspaceModel>> {
        let all_members: Vec<PathBuf> = self.list_members()?;
        let mut workspaces = Vec::new();
        for member in all_members.iter() {
            let Ok(workspace) = WorkspaceModel::from_dir(member) else {
                continue;
            };
            workspaces.push(workspace);
        }
        Ok(workspaces)
    }

    pub fn list_packages(&self) -> Result<Vec<PackageModel>> {
        let all_members: Vec<PathBuf> = self.list_members()?;
        let mut packages = Vec::new();
        for member in all_members {
            let Ok(package) = PackageModel::from_dir(&member) else {
                continue;
            };
            packages.push(package);
        }
        Ok(packages)
    }
}
