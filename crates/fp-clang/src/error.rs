//! Error types for fp-clang

use std::path::PathBuf;

pub type Result<T> = std::result::Result<T, ClangError>;

#[derive(Debug, thiserror::Error)]
pub enum ClangError {
    #[error("Clang not found: {0}")]
    ClangNotFound(String),

    #[error("Clang compilation failed: {0}")]
    CompilationFailed(String),

    #[error("Failed to parse clang output: {0}")]
    ParseError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("File not found: {0}")]
    FileNotFound(PathBuf),

    #[error("Invalid file extension: {0}")]
    InvalidExtension(String),

    #[error("LLVM IR error: {0}")]
    LlvmIrError(String),

    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(String),

    #[error("{0}")]
    Other(String),
}

impl From<String> for ClangError {
    fn from(s: String) -> Self {
        ClangError::Other(s)
    }
}

impl From<&str> for ClangError {
    fn from(s: &str) -> Self {
        ClangError::Other(s.to_string())
    }
}
