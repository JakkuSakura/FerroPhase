//! Utility functions for the magnet CLI tool

use crate::models::ManifestModel;
use eyre::{Result, bail};
use std::path::{Path, PathBuf};
use tracing::warn;

/// Returns the path to the closest Magnet.toml file, starting from the given directory
/// The returned path should contain the `[nexus]` section
pub fn find_furthest_manifest(start_dir: &Path) -> Result<(PathBuf, ManifestModel)> {
    let mut current = start_dir.to_path_buf();

    let mut best_found = None;
    let mut best_score = 0;
    while current.parent().is_some() {
        let Ok(manifest) = ManifestModel::from_dir(&current) else {
            current.pop();
            continue;
        };
        let new_score = match manifest {
            ManifestModel::Nexus(_) => 3,
            ManifestModel::Workspace(_) => 2,
            ManifestModel::Package(_) => 1,
        };
        if new_score <= best_score {
            break;
        }
        best_score = new_score;
        best_found = Some((current.clone(), manifest));
        current.pop();
    }
    match best_found {
        Some(manifest) => Ok(manifest),
        None => bail!(
            "No Magnet.toml/Cargo.toml file found in the directory or any parent directories: {}",
            start_dir.display()
        ),
    }
}
pub fn glob_relative(path: &Path, pattern: &str, allow_error: bool) -> Result<Vec<PathBuf>> {
    let path = path.canonicalize()?;
    let mut result = Vec::new();
    let pattern = format!("{}/{}", path.display(), pattern);
    for entry in glob::glob(&pattern)? {
        match entry {
            Ok(path) => result.push(path),
            Err(e) if allow_error => warn!("Error matching pattern {}: {}", pattern, e),
            Err(e) => bail!("Error matching pattern {}: {}", pattern, e),
        }
    }
    Ok(result)
}
