//! Domain model for a Workspace, which is a collection of packages.

use crate::MagnetConfig;
use crate::configs::{DependencyMap, WorkspaceConfig};
use crate::models::{CrateModel, PackageModel};
use eyre::Result;
use glob::glob;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A workspace model representing a collection of packages
#[derive(Debug, Clone)]
pub struct WorkspaceModel {
    /// Name of the workspace
    pub name: String,
    /// Description of the workspace
    pub description: Option<String>,
    /// Workspace members (glob patterns)
    pub members: Vec<String>,
    /// Excluded workspace members (glob patterns)
    pub exclude: Vec<String>,
    /// Cargo resolver version (1 or 2)
    pub resolver: Option<String>,
    /// Search paths for related workspaces
    pub search_paths: HashMap<String, PathBuf>,
    /// Path overrides for specific dependencies
    pub paths: HashMap<String, PathBuf>,
    /// Custom workspace metadata
    pub custom: HashMap<String, toml::Value>,
    pub dependencies: DependencyMap,
    /// Source path of the workspace configuration
    pub source_file: Option<PathBuf>,
    pub crates: Vec<CrateModel>,
    pub packages: Vec<PackageModel>,
}

impl Default for WorkspaceModel {
    fn default() -> Self {
        Self {
            name: "unnamed-workspace".to_string(),
            description: None,
            members: Vec::new(),
            exclude: Vec::new(),
            resolver: None,
            search_paths: HashMap::new(),
            paths: HashMap::new(),
            custom: HashMap::new(),
            dependencies: Default::default(),
            source_file: None,
            crates: vec![],
            packages: vec![],
        }
    }
}
impl From<WorkspaceConfig> for WorkspaceModel {
    fn from(config: WorkspaceConfig) -> Self {
        Self {
            name: "".to_string(),
            description: None,
            members: config.members,
            exclude: config.exclude,
            resolver: config.resolver,
            search_paths: config.search_paths.unwrap_or_default(),
            paths: config.paths.unwrap_or_default(),
            custom: config.custom,
            dependencies: Default::default(),
            source_file: None,
            crates: vec![],
            packages: vec![],
        }
    }
}

impl WorkspaceModel {
    /// Create a new workspace model with the given name
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ..Default::default()
        }
    }
    pub fn from_config_file(config: &MagnetConfig, source_path: &Path) -> Result<Self> {
        let mut model = Self::from(config.workspace.clone());
        model.dependencies = config.dependencies.clone();
        model.source_file = Some(source_path.to_path_buf());
        Ok(model)
    }

    /// Find a crate by name
    pub fn find_crate(&self, name: &str) -> Option<&CrateModel> {
        self.crates.iter().find(|c| c.name == name)
    }
    pub fn root_path(&self) -> &Path {
        self.source_file
            .as_ref()
            .expect("No source file found")
            .parent()
            .expect("No parent directory found")
    }
    pub fn get_members(&self) -> Result<Vec<String>> {
        let mut all_members = vec![];
        let root_path = self.root_path();
        // handle globs
        for member in self.members.iter() {
            if member.contains("*") {
                let abs_path = root_path.join(member);
                let globeed = glob(&abs_path.to_string_lossy().to_string())?;
                for entry in globeed {
                    match entry {
                        Ok(path) => {
                            let relative_path = path
                                .strip_prefix(root_path)
                                .expect("Failed to strip prefix");
                            all_members.push(relative_path.to_string_lossy().to_string());
                        }
                        Err(e) => {
                            eprintln!("Error processing glob pattern: {}", e);
                        }
                    }
                }
            } else {
                all_members.push(member.to_string());
            }
        }

        Ok(all_members)
    }
}
