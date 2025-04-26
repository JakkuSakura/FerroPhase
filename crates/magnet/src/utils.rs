//! Utility functions for the magnet CLI tool

use std::path::{Path, PathBuf};
use anyhow::Result;

/// Returns the path to the closest Magnet.toml file, starting from the given directory
/// and looking upwards through parent directories
pub fn find_nearest_magnet_toml(start_dir: &Path) -> Option<PathBuf> {
    let mut current = start_dir.to_path_buf();
    
    loop {
        let magnet_toml = current.join("Magnet.toml");
        if magnet_toml.exists() {
            return Some(magnet_toml);
        }
        
        // Go up one directory
        if !current.pop() {
            return None;
        }
    }
}

/// Calculates the relative path between two paths.
/// Uses the pathdiff crate internally.
pub fn relative_path(from: &Path, to: &Path) -> PathBuf {
    pathdiff::diff_paths(to, from).unwrap_or_else(|| to.to_path_buf())
}

/// Reads a Cargo.toml file and extracts the package name and version
pub fn extract_package_info(cargo_toml_path: &Path) -> Result<(String, Option<String>)> {
    // Read the Cargo.toml file
    let content = std::fs::read_to_string(cargo_toml_path)?;
    
    // Parse it as TOML
    let parsed: toml::Value = toml::from_str(&content)?;
    
    // Extract package info
    let package = parsed.get("package")
        .ok_or_else(|| anyhow::anyhow!("No [package] section found in {}", cargo_toml_path.display()))?;
    
    let name = package.get("name")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("No name field in [package] section in {}", cargo_toml_path.display()))?
        .to_string();
    
    let version = package.get("version")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    
    Ok((name, version))
}