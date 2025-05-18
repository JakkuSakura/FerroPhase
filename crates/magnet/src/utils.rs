//! Utility functions for the magnet CLI tool

use crate::models::ManifestModel;
use eyre::{Result, bail};
use std::path::{Path, PathBuf};
use tracing::warn;
use tracing_subscriber::{fmt, EnvFilter};

/// Log level for the application
#[derive(Debug, Clone, Copy)]
pub enum LogLevel {
    Info,
    Debug,
    Trace,
}

/// Setup logging with the specified verbosity level
pub fn setup_logs(log_level: LogLevel) -> Result<()> {
    let filter_level = match log_level {
        LogLevel::Info => "info",
        LogLevel::Debug => "debug",
        LogLevel::Trace => "trace",
    };
    
    // Initialize the logger with specified level
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(filter_level));
    
    fmt::fmt()
        .with_env_filter(filter)
        .with_target(false)
        .try_init()
        .map_err(|e| eyre::eyre!("Failed to initialize logger: {}", e))?;
    
    Ok(())
}

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

/// Clean up the destination directory before generating files
pub fn clean_directory(dir: &Path, exclude_patterns: &[&str]) -> Result<()> {
    if !dir.exists() {
        return Ok(());
    }
    
    // Check if the path is a directory
    if !dir.is_dir() {
        bail!("Path is not a directory: {}", dir.display());
    }
    
    // Read directory entries
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        
        // Skip if matches exclude patterns
        if exclude_patterns.iter().any(|pattern| {
            glob::Pattern::new(pattern).map_or(false, |p| p.matches_path(&path))
        }) {
            continue;
        }
        
        // Remove file or directory
        if path.is_dir() {
            std::fs::remove_dir_all(&path)?;
        } else {
            std::fs::remove_file(&path)?;
        }
    }
    
    Ok(())
}

/// Copy a file or directory from source to destination
pub fn copy_path(source: &Path, dest: &Path) -> Result<()> {
    if !source.exists() {
        return Ok(());
    }
    
    if source.is_dir() {
        // Create destination directory if it doesn't exist
        if !dest.exists() {
            std::fs::create_dir_all(dest)?;
        }
        
        // Copy each entry in the directory
        for entry in std::fs::read_dir(source)? {
            let entry = entry?;
            let source_path = entry.path();
            let file_name = source_path.file_name().unwrap();
            let dest_path = dest.join(file_name);
            
            copy_path(&source_path, &dest_path)?;
        }
    } else {
        // Make sure parent directory exists
        if let Some(parent) = dest.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent)?;
            }
        }
        
        // Copy the file
        std::fs::copy(source, dest)?;
    }
    
    Ok(())
}

/// Create symbolic link to a directory (platform-specific implementation)
pub fn create_symlink(source: &Path, dest: &Path) -> Result<()> {
    // Make sure parent directory exists
    if let Some(parent) = dest.parent() {
        if !parent.exists() {
            std::fs::create_dir_all(parent)?;
        }
    }
    
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(source, dest)
            .map_err(|e| eyre::eyre!("Failed to create symlink from {} to {}: {}", 
                source.display(), dest.display(), e))?;
    }
    
    #[cfg(windows)]
    {
        if source.is_dir() {
            std::os::windows::fs::symlink_dir(source, dest)
                .map_err(|e| eyre::eyre!("Failed to create symlink from {} to {}: {}", 
                    source.display(), dest.display(), e))?;
        } else {
            std::os::windows::fs::symlink_file(source, dest)
                .map_err(|e| eyre::eyre!("Failed to create symlink from {} to {}: {}", 
                    source.display(), dest.display(), e))?;
        }
    }
    
    Ok(())
}
