//! CLI configuration and settings management

use crate::{CliError, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// CLI configuration loaded from config files and environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliConfig {
    /// Compilation settings
    pub compilation: CompilationConfig,
    
    /// Project settings
    pub project: ProjectConfig,
    
    /// Development settings
    pub dev: DevConfig,
    
    /// Language server settings
    pub lsp: LspConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompilationConfig {
    /// Default target for compilation
    pub default_target: String,
    
    /// Default optimization level
    pub default_opt_level: u8,
    
    /// Include directories
    pub include_dirs: Vec<PathBuf>,
    
    /// Predefined constants
    pub defines: Vec<String>,
    
    /// Enable debug information by default
    pub debug: bool,
    
    /// Custom optimization passes
    pub optimization_passes: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Default project template
    pub default_template: String,
    
    /// Automatically initialize git repository
    pub auto_git: bool,
    
    /// Default project structure
    pub default_structure: ProjectStructure,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectStructure {
    /// Source directory name
    pub src_dir: String,
    
    /// Tests directory name
    pub tests_dir: String,
    
    /// Examples directory name
    pub examples_dir: String,
    
    /// Documentation directory name
    pub docs_dir: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DevConfig {
    /// Enable file watching by default
    pub auto_watch: bool,
    
    /// REPL settings
    pub repl: ReplConfig,
    
    /// Formatting settings
    pub formatting: FormattingConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReplConfig {
    /// Enable multi-line mode by default
    pub multiline: bool,
    
    /// History file location
    pub history_file: Option<PathBuf>,
    
    /// Maximum history entries
    pub max_history: usize,
    
    /// Enable syntax highlighting
    pub syntax_highlighting: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormattingConfig {
    /// Indent size
    pub indent_size: usize,
    
    /// Use tabs instead of spaces
    pub use_tabs: bool,
    
    /// Maximum line length
    pub max_line_length: usize,
    
    /// Format on save
    pub format_on_save: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspConfig {
    /// Enable language server features
    pub enabled: bool,
    
    /// Port for TCP communication
    pub port: Option<u16>,
    
    /// Enable completion
    pub completion: bool,
    
    /// Enable diagnostics
    pub diagnostics: bool,
    
    /// Enable hover information
    pub hover: bool,
    
    /// Enable goto definition
    pub goto_definition: bool,
}

impl Default for CliConfig {
    fn default() -> Self {
        Self {
            compilation: CompilationConfig {
                default_target: "rust".to_string(),
                default_opt_level: 2,
                include_dirs: vec![],
                defines: vec![],
                debug: false,
                optimization_passes: vec![
                    "specialize".to_string(),
                    "inline".to_string(),
                    "const_eval".to_string(),
                ],
            },
            project: ProjectConfig {
                default_template: "basic".to_string(),
                auto_git: true,
                default_structure: ProjectStructure {
                    src_dir: "src".to_string(),
                    tests_dir: "tests".to_string(),
                    examples_dir: "examples".to_string(),
                    docs_dir: "docs".to_string(),
                },
            },
            dev: DevConfig {
                auto_watch: false,
                repl: ReplConfig {
                    multiline: false,
                    history_file: None,
                    max_history: 1000,
                    syntax_highlighting: true,
                },
                formatting: FormattingConfig {
                    indent_size: 4,
                    use_tabs: false,
                    max_line_length: 100,
                    format_on_save: false,
                },
            },
            lsp: LspConfig {
                enabled: true,
                port: None,
                completion: true,
                diagnostics: true,
                hover: true,
                goto_definition: true,
            },
        }
    }
}

impl CliConfig {
    /// Load configuration from file, falling back to defaults
    pub fn load(config_path: Option<&Path>) -> Result<Self> {
        let config = if let Some(path) = config_path {
            Self::load_from_file(path)?
        } else {
            // Try to find config in standard locations
            let mut config = Self::default();
            
            // Try current directory
            if let Ok(local_config) = Self::load_from_file(Path::new("ferrophase.toml")) {
                config = config.merge(local_config);
            }
            
            // Try home directory
            if let Some(home_dir) = dirs::home_dir() {
                let home_config = home_dir.join(".ferrophase.toml");
                if let Ok(home_config) = Self::load_from_file(&home_config) {
                    config = config.merge(home_config);
                }
            }
            
            // Try system config directory
            if let Some(config_dir) = dirs::config_dir() {
                let system_config = config_dir.join("ferrophase").join("config.toml");
                if let Ok(system_config) = Self::load_from_file(&system_config) {
                    config = config.merge(system_config);
                }
            }
            
            config
        };
        
        Ok(config)
    }
    
    /// Load configuration from a specific file
    pub fn load_from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| CliError::Config(format!("Failed to read config file {}: {}", path.display(), e)))?;
        
        let config: Self = toml::from_str(&content)
            .map_err(|e| CliError::Config(format!("Failed to parse config file {}: {}", path.display(), e)))?;
        
        Ok(config)
    }
    
    /// Save configuration to file
    pub fn save_to_file(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| CliError::Config(format!("Failed to serialize config: {}", e)))?;
        
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| CliError::Config(format!("Failed to create config directory: {}", e)))?;
        }
        
        std::fs::write(path, content)
            .map_err(|e| CliError::Config(format!("Failed to write config file: {}", e)))?;
        
        Ok(())
    }
    
    /// Merge this configuration with another, with the other taking precedence
    pub fn merge(self, other: Self) -> Self {
        // For now, just replace with other. In a real implementation,
        // you might want more sophisticated merging logic.
        other
    }
    
    /// Get the default config file path for the current user
    pub fn default_config_path() -> Option<PathBuf> {
        dirs::config_dir().map(|dir| dir.join("ferrophase").join("config.toml"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    
    #[test]
    fn test_default_config() {
        let config = CliConfig::default();
        assert_eq!(config.compilation.default_target, "rust");
        assert_eq!(config.compilation.default_opt_level, 2);
        assert_eq!(config.project.default_template, "basic");
    }
    
    #[test]
    fn test_config_serialization() {
        let config = CliConfig::default();
        let toml_str = toml::to_string(&config).unwrap();
        let deserialized: CliConfig = toml::from_str(&toml_str).unwrap();
        
        assert_eq!(config.compilation.default_target, deserialized.compilation.default_target);
    }
    
    #[test]
    fn test_config_file_operations() {
        let config = CliConfig::default();
        let temp_file = NamedTempFile::new().unwrap();
        
        // Save config
        config.save_to_file(temp_file.path()).unwrap();
        
        // Load config
        let loaded_config = CliConfig::load_from_file(temp_file.path()).unwrap();
        
        assert_eq!(config.compilation.default_target, loaded_config.compilation.default_target);
    }
}