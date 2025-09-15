//! Project management utilities

use crate::{CliError, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// FerroPhase project configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub project: ProjectInfo,
    pub compilation: CompilationSettings,
    pub languages: Option<LanguageSettings>,
    pub dependencies: Option<toml::Value>,
    pub dev_dependencies: Option<toml::Value>,
    pub features: Option<toml::Value>,
    pub build: Option<BuildSettings>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectInfo {
    pub name: String,
    pub version: String,
    pub edition: String,
    pub template: Option<String>,
    pub description: Option<String>,
    pub authors: Option<Vec<String>>,
    pub license: Option<String>,
    pub repository: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompilationSettings {
    pub target: String,
    pub opt_level: u8,
    pub debug: bool,
    pub include_dirs: Option<Vec<PathBuf>>,
    pub defines: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageSettings {
    pub python: Option<PythonConfig>,
    pub javascript: Option<JavaScriptConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PythonConfig {
    pub enabled: bool,
    pub version: Option<String>,
    pub packages: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JavaScriptConfig {
    pub enabled: bool,
    pub runtime: Option<String>,
    pub packages: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildSettings {
    pub script: Option<String>,
    pub parallel: Option<bool>,
}

impl ProjectConfig {
    /// Load project configuration from Ferrophase.toml
    pub fn load_from_dir(dir: &Path) -> Result<Self> {
        let config_path = dir.join("Ferrophase.toml");
        Self::load_from_file(&config_path)
    }
    
    /// Load project configuration from a specific file
    pub fn load_from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| CliError::Project(format!("Failed to read project config {}: {}", path.display(), e)))?;
        
        let config: Self = toml::from_str(&content)
            .map_err(|e| CliError::Project(format!("Failed to parse project config {}: {}", path.display(), e)))?;
        
        Ok(config)
    }
    
    /// Save project configuration to file
    pub fn save_to_file(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| CliError::Project(format!("Failed to serialize project config: {}", e)))?;
        
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| CliError::Project(format!("Failed to create config directory: {}", e)))?;
        }
        
        std::fs::write(path, content)
            .map_err(|e| CliError::Project(format!("Failed to write project config: {}", e)))?;
        
        Ok(())
    }
    
    /// Check if this directory contains a FerroPhase project
    pub fn is_ferrophase_project(dir: &Path) -> bool {
        dir.join("Ferrophase.toml").exists()
    }
    
    /// Find the project root directory starting from the given path
    pub fn find_project_root(start: &Path) -> Option<PathBuf> {
        let mut current = start.to_path_buf();
        
        loop {
            if Self::is_ferrophase_project(&current) {
                return Some(current);
            }
            
            if !current.pop() {
                break;
            }
        }
        
        None
    }
    
    /// Get the source directory for this project
    pub fn src_dir(&self) -> PathBuf {
        PathBuf::from("src") // Default, could be configurable
    }
    
    /// Get the output directory for this project
    pub fn output_dir(&self) -> PathBuf {
        PathBuf::from("target") // Default, could be configurable
    }
    
    /// Check if a language is enabled in this project
    pub fn is_language_enabled(&self, language: &str) -> bool {
        match language {
            "python" => self.languages
                .as_ref()
                .and_then(|l| l.python.as_ref())
                .map(|p| p.enabled)
                .unwrap_or(false),
            "javascript" => self.languages
                .as_ref()
                .and_then(|l| l.javascript.as_ref())
                .map(|j| j.enabled)
                .unwrap_or(false),
            _ => false,
        }
    }
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            project: ProjectInfo {
                name: "ferrophase-project".to_string(),
                version: "0.1.0".to_string(),
                edition: "2021".to_string(),
                template: Some("basic".to_string()),
                description: Some("A FerroPhase project".to_string()),
                authors: None,
                license: None,
                repository: None,
            },
            compilation: CompilationSettings {
                target: "rust".to_string(),
                opt_level: 2,
                debug: false,
                include_dirs: None,
                defines: None,
            },
            languages: Some(LanguageSettings {
                python: Some(PythonConfig {
                    enabled: false,
                    version: Some("3.8+".to_string()),
                    packages: None,
                }),
                javascript: Some(JavaScriptConfig {
                    enabled: false,
                    runtime: Some("node".to_string()),
                    packages: None,
                }),
            }),
            dependencies: None,
            dev_dependencies: None,
            features: None,
            build: None,
        }
    }
}

/// Utilities for working with FerroPhase projects
pub struct ProjectUtils;

impl ProjectUtils {
    /// Validate that all required files exist in a project
    pub fn validate_project(project_dir: &Path) -> Result<()> {
        let config_path = project_dir.join("Ferrophase.toml");
        if !config_path.exists() {
            return Err(CliError::Project(format!(
                "Missing Ferrophase.toml in {}",
                project_dir.display()
            )));
        }
        
        let src_dir = project_dir.join("src");
        if !src_dir.exists() {
            return Err(CliError::Project(format!(
                "Missing src directory in {}",
                project_dir.display()
            )));
        }
        
        Ok(())
    }
    
    /// Get all FerroPhase source files in a project
    pub fn find_source_files(project_dir: &Path) -> Result<Vec<PathBuf>> {
        let src_dir = project_dir.join("src");
        let mut files = Vec::new();
        
        fn collect_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
            for entry in std::fs::read_dir(dir).map_err(|e| CliError::Io(e))? {
                let entry = entry.map_err(|e| CliError::Io(e))?;
                let path = entry.path();
                
                if path.is_dir() {
                    collect_files(&path, files)?;
                } else if path.extension().map_or(false, |ext| ext == "fp") {
                    files.push(path);
                }
            }
            Ok(())
        }
        
        collect_files(&src_dir, &mut files)?;
        Ok(files)
    }
    
    /// Check project dependencies
    pub fn check_dependencies(config: &ProjectConfig) -> Result<Vec<String>> {
        let mut missing = Vec::new();
        
        // Check language runtimes
        if config.is_language_enabled("python") {
            if !Self::check_python_available() {
                missing.push("Python runtime".to_string());
            }
        }
        
        if config.is_language_enabled("javascript") {
            if !Self::check_node_available() {
                missing.push("Node.js runtime".to_string());
            }
        }
        
        Ok(missing)
    }
    
    fn check_python_available() -> bool {
        std::process::Command::new("python3")
            .arg("--version")
            .output()
            .is_ok()
    }
    
    fn check_node_available() -> bool {
        std::process::Command::new("node")
            .arg("--version")
            .output()
            .is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    
    #[test]
    fn test_default_config() {
        let config = ProjectConfig::default();
        assert_eq!(config.project.name, "ferrophase-project");
        assert_eq!(config.compilation.target, "rust");
        assert_eq!(config.compilation.opt_level, 2);
    }
    
    #[test]
    fn test_config_serialization() {
        let config = ProjectConfig::default();
        let toml_str = toml::to_string(&config).unwrap();
        let deserialized: ProjectConfig = toml::from_str(&toml_str).unwrap();
        
        assert_eq!(config.project.name, deserialized.project.name);
        assert_eq!(config.compilation.target, deserialized.compilation.target);
    }
    
    #[test]
    fn test_find_project_root() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("my_project");
        std::fs::create_dir_all(&project_dir).unwrap();
        
        // Create Ferrophase.toml
        let config = ProjectConfig::default();
        config.save_to_file(&project_dir.join("Ferrophase.toml")).unwrap();
        
        // Test finding from subdirectory
        let sub_dir = project_dir.join("src").join("sub");
        std::fs::create_dir_all(&sub_dir).unwrap();
        
        let found_root = ProjectConfig::find_project_root(&sub_dir).unwrap();
        assert_eq!(found_root, project_dir);
    }
    
    #[test]
    fn test_language_enabled() {
        let mut config = ProjectConfig::default();
        
        // Enable Python
        if let Some(ref mut languages) = config.languages {
            if let Some(ref mut python) = languages.python {
                python.enabled = true;
            }
        }
        
        assert!(config.is_language_enabled("python"));
        assert!(!config.is_language_enabled("javascript"));
        assert!(!config.is_language_enabled("unknown"));
    }
}