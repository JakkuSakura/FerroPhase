//! Domain model for a Workspace, which is a collection of packages.

use crate::configs::{DependencyMap, ManifestConfig};
use crate::models::PackageModel;
use crate::utils::glob_relative;
use eyre::{ContextCompat, Result, bail};
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
    /// Patch section for overriding dependencies
    pub patch: Option<toml::value::Table>,
    pub root_path: PathBuf,
    /// Source path of the workspace configuration
    pub source_path: PathBuf,
}

impl WorkspaceModel {
    pub fn from_dir(root_path: &Path) -> Result<Self> {
        let root_path = root_path.canonicalize()?;
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
            bail!(
                "Root path must point to Cargo.toml or Magnet.toml: {}",
                root_path.display()
            )
        };
        let config = ManifestConfig::from_file(&source_path)?;
        let config1 = config
            .workspace
            .clone()
            .with_context(|| format!("No workspace found in {}", source_path.display()))?;
        let root_path = source_path.parent().unwrap().canonicalize()?.to_owned();
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
            patch: config.patch,
            source_path: source_path.to_path_buf(),
            root_path,
        };

        Ok(model)
    }
    pub fn list_members(&self) -> Result<Vec<PathBuf>> {
        let mut all_members: Vec<PathBuf> = vec![];
        let root_path = &self.root_path;
        // handle globs
        for member in self.members.iter() {
            all_members.extend(glob_relative(root_path, member, true)?);
        }
        // handle excludes
        for exclude in self.exclude.iter() {
            let exclude_paths = glob_relative(root_path, exclude, false)?;
            for exclude_path in exclude_paths {
                all_members.retain(|path| path != &exclude_path);
            }
        }
        Ok(all_members)
    }
    /// list packages paths joined with the workspace root path
    pub fn list_packages(&self) -> Result<Vec<PackageModel>> {
        let all_members: Vec<PathBuf> = self.list_members()?;
        let mut packages: Vec<PackageModel> = vec![];
        for member in all_members.iter() {
            let package = PackageModel::from_dir(member)?;
            packages.push(package);
        }
        Ok(packages)
    }
    pub fn find_package(&self, package_name: &str) -> Result<PackageModel> {
        for package in self.list_packages()? {
            if package.name == package_name {
                return Ok(package);
            }
        }
        bail!(
            "Package '{}' not found in workspace '{}'",
            package_name,
            self.name
        )
    }
}
