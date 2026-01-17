//! FerroPhase CLI Library
//!
//! This crate provides the command-line interface for FerroPhase, a meta-compilation
//! framework that enables multi-language development with advanced compile-time capabilities.

pub mod cli;
pub mod codegen;
pub mod commands;
pub mod compilation;
pub mod diagnostics;
pub mod languages;
pub mod pipeline;
pub mod utils;

// Re-export core types for convenience, avoiding conflicts
pub use fp_backend::*;
pub use fp_core::{Error as CoreError, Result as CoreResult, ast, context, ops, span};

// CLI-specific error handling
pub mod error {
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum CliError {
        #[error("IO error: {0}")]
        Io(#[from] std::io::Error),

        #[error("Configuration error: {0}")]
        Config(String),

        #[error("Compilation error: {0}")]
        Compilation(String),

        #[error("Project error: {0}")]
        Project(String),

        #[error("Invalid input: {0}")]
        InvalidInput(String),

        #[error("Transpilation error: {0}")]
        Transpile(String),

        #[error("Missing dependency: {0}")]
        MissingDependency(String),

        #[error("Core error: {0}")]
        Core(#[from] fp_core::error::Error),
    }

    pub type Result<T> = std::result::Result<T, CliError>;
}

pub use error::{CliError, Result};
