//! FerroPhase CLI Library
//!
//! This crate provides the command-line interface for FerroPhase, a meta-compilation
//! framework that enables multi-language development with advanced compile-time capabilities.

pub mod cli;
pub mod commands;
pub mod compilation;
pub mod compile_options;
pub mod compiler;
pub mod container;
pub mod diagnostics;
pub mod languages;
pub mod materialize;
pub mod utils;

// Re-export core types for convenience, avoiding conflicts
pub use fp_backend::*;
pub use fp_core::{Error as CoreError, Result as CoreResult, ast, context, ops, span};

// The generic external-compile-target extension point (see
// `languages::registry`'s doc comment) — re-exported at the crate root so an
// embedding binary (e.g. `skln-fp-graph`'s `fp-graph`) only needs `fp_cli::`,
// not `fp_cli::languages::registry::`. Note there is no pre-existing
// `LanguageTarget` re-export at this crate root to collide with: only the
// enum's *module* path (`crate::languages::backend::LanguageTarget`, now
// renamed `BuiltinLanguageTarget`) ever existed before this change.
pub use languages::registry::{
    LanguageTarget, LanguageTargetContext, LanguageTargetPackage, find_registered_language_target,
    register_language_target,
};
pub use languages::backend::BuiltinLanguageTarget;

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

        #[error("Target emission error: {0}")]
        TargetEmit(String),

        #[error("Missing dependency: {0}")]
        MissingDependency(String),

        #[error("Core error: {0}")]
        Core(#[from] fp_core::error::Error),
    }

    pub type Result<T> = std::result::Result<T, CliError>;
}

pub use error::{CliError, Result};
