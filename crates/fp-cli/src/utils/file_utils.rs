//! File and path utilities

use crate::{CliError, Result};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use globset::{Glob, GlobSet, GlobSetBuilder};

/// Utilities for working with files and paths
pub struct FileUtils;

impl FileUtils {
    /// Find all files matching a pattern in a directory
    pub fn find_files(
        dir: &Path,
        include_patterns: &[String],
        exclude_patterns: &[String],
    ) -> Result<Vec<PathBuf>> {
        let include_set = Self::build_glob_set(include_patterns)?;
        let exclude_set = Self::build_glob_set(exclude_patterns)?;
        
        let mut files = Vec::new();
        
        for entry in WalkDir::new(dir).follow_links(false) {
            let entry = entry.map_err(|e| CliError::Io(e.into()))?;
            let path = entry.path();
            
            if !entry.file_type().is_file() {
                continue;
            }
            
            let relative_path = path.strip_prefix(dir)
                .map_err(|_| CliError::InvalidInput("Invalid path structure".to_string()))?;
            
            // Check include patterns (if any)
            if !include_patterns.is_empty() && !include_set.is_match(relative_path) {
                continue;
            }
            
            // Check exclude patterns
            if !exclude_patterns.is_empty() && exclude_set.is_match(relative_path) {
                continue;
            }
            
            files.push(path.to_path_buf());
        }
        
        Ok(files)
    }
    
    /// Find all FerroPhase source files in a directory
    pub fn find_ferrophase_files(dir: &Path) -> Result<Vec<PathBuf>> {
        Self::find_files(dir, &["**/*.fp".to_string()], &[])
    }
    
    /// Check if a file has a FerroPhase extension
    pub fn is_ferrophase_file(path: &Path) -> bool {
        path.extension().map_or(false, |ext| ext == "fp")
    }
    
    /// Get the relative path from one directory to another
    pub fn relative_path(from: &Path, to: &Path) -> Result<PathBuf> {
        pathdiff::diff_paths(to, from)
            .ok_or_else(|| CliError::InvalidInput("Cannot compute relative path".to_string()))
    }
    
    /// Ensure a directory exists, creating it if necessary
    pub fn ensure_dir_exists(dir: &Path) -> Result<()> {
        if !dir.exists() {
            std::fs::create_dir_all(dir)
                .map_err(|e| CliError::Io(e))?;
        }
        Ok(())
    }
    
    /// Copy a file, creating parent directories if necessary
    pub fn copy_file(src: &Path, dst: &Path) -> Result<()> {
        if let Some(parent) = dst.parent() {
            Self::ensure_dir_exists(parent)?;
        }
        
        std::fs::copy(src, dst)
            .map_err(|e| CliError::Io(e))?;
        
        Ok(())
    }
    
    /// Write content to a file, creating parent directories if necessary
    pub fn write_file(path: &Path, content: &str) -> Result<()> {
        if let Some(parent) = path.parent() {
            Self::ensure_dir_exists(parent)?;
        }
        
        std::fs::write(path, content)
            .map_err(|e| CliError::Io(e))?;
        
        Ok(())
    }
    
    /// Get the modification time of a file
    pub fn modification_time(path: &Path) -> Result<std::time::SystemTime> {
        let metadata = std::fs::metadata(path)
            .map_err(|e| CliError::Io(e))?;
        
        metadata.modified()
            .map_err(|e| CliError::Io(e))
    }
    
    /// Check if a file is newer than another
    pub fn is_newer(file1: &Path, file2: &Path) -> Result<bool> {
        let time1 = Self::modification_time(file1)?;
        let time2 = Self::modification_time(file2)?;
        
        Ok(time1 > time2)
    }
    
    /// Build a GlobSet from a list of patterns
    fn build_glob_set(patterns: &[String]) -> Result<GlobSet> {
        let mut builder = GlobSetBuilder::new();
        
        for pattern in patterns {
            let glob = Glob::new(pattern)
                .map_err(|e| CliError::InvalidInput(format!("Invalid glob pattern '{}': {}", pattern, e)))?;
            builder.add(glob);
        }
        
        builder.build()
            .map_err(|e| CliError::InvalidInput(format!("Failed to build glob set: {}", e)))
    }
}

/// File watching utilities
pub struct FileWatcher {
    watched_files: Vec<PathBuf>,
    last_modified: std::collections::HashMap<PathBuf, std::time::SystemTime>,
}

impl FileWatcher {
    /// Create a new file watcher
    pub fn new() -> Self {
        Self {
            watched_files: Vec::new(),
            last_modified: std::collections::HashMap::new(),
        }
    }
    
    /// Add a file to watch
    pub fn watch(&mut self, path: PathBuf) -> Result<()> {
        let modified_time = FileUtils::modification_time(&path)?;
        self.last_modified.insert(path.clone(), modified_time);
        self.watched_files.push(path);
        Ok(())
    }
    
    /// Check if any watched files have been modified
    pub fn check_for_changes(&mut self) -> Result<Vec<PathBuf>> {
        let mut changed_files = Vec::new();
        
        for file in &self.watched_files {
            if let Ok(current_time) = FileUtils::modification_time(file) {
                if let Some(&last_time) = self.last_modified.get(file) {
                    if current_time > last_time {
                        changed_files.push(file.clone());
                        self.last_modified.insert(file.clone(), current_time);
                    }
                }
            }
        }
        
        Ok(changed_files)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_find_ferrophase_files() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();
        
        // Create test files
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/main.fp"), "fn main() {}").unwrap();
        fs::write(root.join("src/lib.fp"), "pub mod lib;").unwrap();
        fs::write(root.join("src/other.rs"), "// Rust file").unwrap();
        
        let files = FileUtils::find_ferrophase_files(root).unwrap();
        assert_eq!(files.len(), 2);
        
        let file_names: Vec<_> = files.iter()
            .filter_map(|p| p.file_name().and_then(|n| n.to_str()))
            .collect();
        assert!(file_names.contains(&"main.fp"));
        assert!(file_names.contains(&"lib.fp"));
    }
    
    #[test]
    fn test_find_files_with_patterns() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();
        
        // Create test files
        fs::create_dir_all(root.join("src")).unwrap();
        fs::create_dir_all(root.join("tests")).unwrap();
        fs::write(root.join("src/main.fp"), "").unwrap();
        fs::write(root.join("tests/test.fp"), "").unwrap();
        fs::write(root.join("README.md"), "").unwrap();
        
        // Find only .fp files
        let files = FileUtils::find_files(
            root,
            &["**/*.fp".to_string()],
            &[]
        ).unwrap();
        assert_eq!(files.len(), 2);
        
        // Exclude tests directory
        let files = FileUtils::find_files(
            root,
            &["**/*.fp".to_string()],
            &["tests/**".to_string()]
        ).unwrap();
        assert_eq!(files.len(), 1);
    }
    
    #[test]
    fn test_is_ferrophase_file() {
        assert!(FileUtils::is_ferrophase_file(Path::new("main.fp")));
        assert!(FileUtils::is_ferrophase_file(Path::new("src/lib.fp")));
        assert!(!FileUtils::is_ferrophase_file(Path::new("main.rs")));
        assert!(!FileUtils::is_ferrophase_file(Path::new("README.md")));
    }
    
    #[test]
    fn test_file_watcher() {
        let temp_dir = TempDir::new().unwrap();
        let test_file = temp_dir.path().join("test.fp");
        
        fs::write(&test_file, "initial content").unwrap();
        
        let mut watcher = FileWatcher::new();
        watcher.watch(test_file.clone()).unwrap();
        
        // No changes initially
        let changes = watcher.check_for_changes().unwrap();
        assert!(changes.is_empty());
        
        // Modify file (sleep to ensure different timestamp)
        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&test_file, "modified content").unwrap();
        
        // Should detect change
        let changes = watcher.check_for_changes().unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0], test_file);
    }
}