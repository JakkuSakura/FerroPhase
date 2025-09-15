//! FerroPhase CLI Library
//! 
//! This crate provides the command-line interface for FerroPhase, a meta-compilation
//! framework that enables multi-language development with advanced compile-time capabilities.

pub mod cli;
pub mod commands;
pub mod config;
pub mod diagnostics;
pub mod project;
pub mod utils;

// Re-export core types for convenience
pub use fp_core::*;
pub use fp_optimize::*;
pub use fp_rust_lang::*;

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
        
        #[error("Missing dependency: {0}")]
        MissingDependency(String),
    }
    
    pub type Result<T> = std::result::Result<T, CliError>;
}

pub use error::{CliError, Result};