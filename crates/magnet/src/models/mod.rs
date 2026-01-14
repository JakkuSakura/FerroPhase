use eyre::{Result, bail};
use std::path::{Path, PathBuf};

#[path = "crate.rs"]
mod crate_;
mod cargo;
mod dependency;
mod graph;
mod lock;
mod nexus;
mod package;
mod patch;
mod workspace;

use crate::configs::ManifestConfig;
pub use crate_::*;
pub use cargo::*;
pub use dependency::*;
pub use graph::*;
pub use lock::*;
pub use nexus::*;
pub use package::*;
pub use patch::*;
pub use workspace::*;

/// a valid manifest model is either:
/// - a NexusModel
/// - a WorkspaceModel
/// - a PackageModel
///
/// There are root manifest case in cargo, but we don't support it yet.
#[derive(Debug, Clone)]
pub enum ManifestModel {
    Nexus(NexusModel),
    Workspace(WorkspaceModel),
    Package(PackageModel),
}
impl ManifestModel {
    /// Create a manifest model from a directory
    pub fn from_dir(root_path: &Path) -> Result<Self> {
        let root_path = root_path.canonicalize()?;
        let config_path = if root_path.join("Magnet.toml").exists() {
            root_path.join("Magnet.toml")
        } else if root_path.join("Cargo.toml").exists() {
            root_path.join("Cargo.toml")
        } else {
            bail!(
                "No Magnet.toml or Cargo.toml file found in the directory: {}",
                root_path.display()
            );
        };
        if config_path.file_name().and_then(|n| n.to_str()) == Some("Cargo.toml") {
            let cargo = crate::configs::CargoManifestConfig::from_path(&config_path)?;
            if cargo.workspace.is_some() {
                WorkspaceModel::from_dir(&root_path).map(ManifestModel::Workspace)
            } else if cargo.package.is_some() {
                PackageModel::from_dir(&root_path).map(ManifestModel::Package)
            } else {
                bail!("Cargo.toml has neither [workspace] nor [package] section");
            }
        } else {
            let config = ManifestConfig::from_file(&config_path)?;
            if let Some(_nexus) = config.nexus {
                NexusModel::from_dir(&root_path).map(ManifestModel::Nexus)
            } else if let Some(_workspace) = config.workspace {
                WorkspaceModel::from_dir(&root_path).map(ManifestModel::Workspace)
            } else if let Some(_package) = config.package {
                PackageModel::from_dir(&root_path).map(ManifestModel::Package)
            } else {
                unreachable!()
            }
        }
    }
    pub fn name(&self) -> String {
        match self {
            ManifestModel::Nexus(nexus) => nexus.name.clone(),
            ManifestModel::Workspace(workspace) => workspace.name.clone(),
            ManifestModel::Package(package) => package.name.clone(),
        }
    }
    pub fn list_members(&self) -> Result<Vec<PathBuf>> {
        match self {
            ManifestModel::Nexus(nexus) => nexus.list_members(),
            ManifestModel::Workspace(workspace) => workspace.list_members(),
            ManifestModel::Package(package) => Ok(vec![package.source_path.clone()]),
        }
    }
    pub fn list_member_manifests(&self) -> Result<Vec<ManifestModel>> {
        let members = self.list_members()?;
        let mut manifests = Vec::new();
        for member in members.iter() {
            let manifest = ManifestModel::from_dir(member)?;
            manifests.push(manifest);
        }
        Ok(manifests)
    }
    pub fn list_workspaces(&self) -> Result<Vec<WorkspaceModel>> {
        match self {
            ManifestModel::Nexus(nexus) => nexus.list_workspaces(),
            ManifestModel::Workspace(workspace) => Ok(vec![workspace.clone()]),
            ManifestModel::Package(_) => Ok(vec![]),
        }
    }
    pub fn list_packages(&self) -> Result<Vec<PackageModel>> {
        match self {
            ManifestModel::Nexus(nexus) => nexus.list_packages(),
            ManifestModel::Workspace(workspace) => workspace.list_packages(),
            ManifestModel::Package(package) => Ok(vec![package.clone()]),
        }
    }
    pub fn patch(&self) -> &PatchMap {
        match self {
            ManifestModel::Nexus(nexus) => &nexus.patch,
            ManifestModel::Workspace(workspace) => &workspace.patch,
            ManifestModel::Package(package) => &package.patch,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn cargo_root_with_workspace_and_package_is_workspace() -> Result<()> {
        let temp = tempdir()?;
        let root = temp.path();
        fs::write(
            root.join("Cargo.toml"),
            r#"[package]
name = "root-demo"
version = "0.1.0"
edition = "2024"

[workspace]
members = ["crates/*"]
"#,
        )?;

        let manifest = ManifestModel::from_dir(root)?;
        assert!(matches!(manifest, ManifestModel::Workspace(_)));
        Ok(())
    }
}
