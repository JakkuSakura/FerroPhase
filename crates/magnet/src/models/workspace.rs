//! Domain model for a Workspace, which is a collection of packages.

use crate::MagnetConfig;
use crate::configs::DependencyMap;
use crate::models::{CrateModel, PackageModel};
use eyre::{ContextCompat, Result};
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
    pub root_path: PathBuf,
    /// Source path of the workspace configuration
    pub source_path: PathBuf,
    pub crates: Vec<CrateModel>,
    pub packages: Vec<PackageModel>,
    pub nexus_path: Option<PathBuf>,
}

impl WorkspaceModel {
    pub fn from_root_path(root_path: &Path) -> Result<Self> {
        if !root_path.exists() {
            eyre::bail!(
                "Root path doesn't exist in the current directory: {}",
                root_path.display()
            )
        }
        let source_path = if root_path.join("Magnet.toml").exists() {
            root_path.join("Magnet.toml")
        } else if root_path.join("Cargo.toml").exists() {
            root_path.join("Cargo.toml")
        } else {
            eyre::bail!(
                "Root path must point to Cargo.toml or Magnet.toml: {}",
                root_path.display()
            )
        };
        let config = MagnetConfig::from_file(&source_path)?;
        let config1 = config
            .workspace
            .clone()
            .with_context(|| format!("No workspace found in {}", source_path.display()))?;
        let root_path = source_path
            .parent()
            .unwrap()
            .canonicalize()?
            .to_owned();
        let name = root_path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned();

        let model = WorkspaceModel {
            name,
            description: None,
            members: config1.members,
            exclude: config1.exclude,
            resolver: config1.resolver,
            search_paths: config1.search_paths.unwrap_or_default(),
            paths: config1.paths.unwrap_or_default(),
            custom: config1.custom,
            dependencies: config.dependencies.clone(),
            source_path: source_path.to_path_buf(),
            root_path,
            crates: vec![],
            packages: vec![],
            nexus_path: config1.nexus_path,
        };

        Ok(model)
    }

    /// Find a crate by name
    pub fn find_crate(&self, name: &str) -> Option<&CrateModel> {
        self.crates.iter().find(|c| c.name == name)
    }
    pub fn root_path(&self) -> &Path {
        self.root_path.as_path()
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
