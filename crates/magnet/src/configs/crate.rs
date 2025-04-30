//! Crate and workspace structures and operations
//!
//! This module contains structures that represent crates and workspaces,
//! along with their associated operations.

use std::path::PathBuf;

/// Crate information structure
#[derive(Debug, Clone)]
pub struct CrateConfig {
    /// Name of the crate
    pub name: String,
    /// Version of the crate
    pub version: Option<String>,
    /// Path to the crate directory
    pub path: PathBuf,
    /// Path to the Cargo.toml file
    pub cargo_toml_path: PathBuf,
    /// Path to the Magnet.toml file (if any)
    pub magnet_toml_path: Option<PathBuf>,
    /// Whether this crate has a custom configuration
    pub has_custom_config: bool,
}
