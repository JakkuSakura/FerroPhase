//! Domain model for a Workspace, which is a collection of packages.

use crate::configs::{CargoManifestConfig, ManifestConfig};
use crate::models::{DependencyModel, DependencyModelMap, PackageModel, PatchMap};
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

    /// Custom workspace metadata
    pub custom: HashMap<String, toml::Value>,
    pub dependencies: DependencyModelMap,
    /// Patch section for overriding dependencies
    pub patch: PatchMap,
    pub root_path: PathBuf,
    /// Source path of the workspace configuration
    pub source_path: PathBuf,
}

impl WorkspaceModel {
    pub fn from_dir(root_path: &Path) -> Result<Self> {
        let root_path = root_path.canonicalize()?;
        if !root_path.exists() {
            bail!(
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
        if source_path.file_name().and_then(|n| n.to_str()) == Some("Cargo.toml") {
            let config = CargoManifestConfig::from_path(&source_path)?;
            let workspace = config
                .workspace
                .clone()
                .with_context(|| format!("No workspace found in {}", source_path.display()))?;
            let root_path = source_path
                .parent()
                .with_context(|| format!("Missing parent for {}", source_path.display()))?
                .canonicalize()?
                .to_owned();
            let name = root_path
                .file_name()
                .and_then(|name| name.to_str())
                .with_context(|| format!("Missing workspace name for {}", root_path.display()))?
                .to_string();

            let model = WorkspaceModel {
                name,
                description: None,
                members: workspace.members,
                exclude: workspace.exclude,
                resolver: workspace.resolver,
                custom: HashMap::new(),
                dependencies: workspace
                    .dependencies
                    .clone()
                    .into_iter()
                    .map(|(k, v)| (k, v.into()))
                    .collect(),
                patch: crate::models::parse_patch_table(config.patch),
                source_path: source_path.to_path_buf(),
                root_path,
            };

            return Ok(model);
        }

        let config = ManifestConfig::from_file(&source_path)?;
        let config1 = config
            .workspace
            .clone()
            .with_context(|| format!("No workspace found in {}", source_path.display()))?;
        let root_path = source_path
            .parent()
            .with_context(|| format!("Missing parent for {}", source_path.display()))?
            .canonicalize()?
            .to_owned();
        let name = root_path
            .file_name()
            .and_then(|name| name.to_str())
            .with_context(|| format!("Missing workspace name for {}", root_path.display()))?
            .to_string();

        let model = WorkspaceModel {
            name,
            description: None,
            members: config1.members,
            exclude: config1.exclude,
            resolver: config1.resolver,
            custom: config1.custom,
            dependencies: config1
                .dependencies
                .clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
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
    pub fn find_dependency(&self, name: &str) -> Option<DependencyModel> {
        self.dependencies.get(name).cloned()
    }
}
