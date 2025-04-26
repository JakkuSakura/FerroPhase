// Magnet - A tool for managing Rust super-workspaces
// This module exposes the library functionality for use in tests and the CLI

//! # Magnet
//!
//! Magnet is a tool for managing Rust super-workspaces, providing a way to
//! coordinate dependencies across multiple Cargo workspaces.
//!
//! ## Features
//!
//! - **Super-workspaces**: Manage dependencies across multiple separate projects
//! - **Automatic path resolution**: Automatically find and link local crates with `auto = true`
//! - **Multi-level configuration**: Manage dependencies at workspace and crate levels
//! - **Consistency checking**: Verify dependencies are properly synchronized
//!
//! ## Core modules
//!
//! - `config`: Configuration handling for Magnet.toml files
//! - `workspace`: Workspace discovery and management
//! - `generator`: Cargo.toml generation from Magnet configuration
//! - `resolver`: Dependency resolution across workspaces
//! - `commands`: CLI command implementations

// Public modules
pub mod config;
pub mod workspace;
pub mod generator;
pub mod resolver;
pub mod commands;
pub mod utils;

// Re-export commonly used items
pub use config::MagnetConfig;
pub use workspace::WorkspaceManager;
pub use generator::CargoGenerator;
pub use resolver::DependencyResolver;

// Export version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");