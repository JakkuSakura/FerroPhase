// Magnet - A tool for managing Rust nexus workspaces
// This module exposes the library functionality for use in tests and the CLI

//! # Magnet
//!
//! Magnet is a tool for managing Rust nexus workspaces, providing a way to
//! coordinate dependencies across multiple Cargo workspaces.
//!
//! ## Features
//!
//! - **Nexus**: Manage dependencies across multiple separate projects and workspaces
//! - **Automatic path resolution**: Automatically find and link local packages with `auto = true`
//! - **Multi-level configuration**: Manage dependencies at workspace and package levels
//! - **Consistency checking**: Verify dependencies are properly synchronized
//!
//! ## Architecture
//!
//! Magnet uses a hierarchical structure:
//! - **Nexus**: A collection of related workspaces (previously called super-workspace)
//! - **Workspace**: A Cargo workspace containing multiple packages
//! - **Package**: A unit containing Rust crates (Magnet doesn't manage individual crates)
//!
//! ## Core modules
//!
//! - `config`: Configuration handling for Magnet.toml files
//! - `manager`: Workspace discovery and management
//! - `generator`: Cargo.toml generation from Magnet configuration
//! - `resolver`: Dependency resolution across workspaces
//! - `commands`: CLI command implementations

// Public modules
pub mod configs;
pub mod manager;
pub mod generator;
pub mod resolver;
pub mod commands;
pub mod utils;
pub mod models;

// Re-export commonly used items
pub use configs::MagnetConfig;
pub use manager::NexusManager;
pub use generator::CargoGenerator;
pub use resolver::DependencyResolver;

// Export version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");