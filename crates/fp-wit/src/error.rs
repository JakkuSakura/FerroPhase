use std::path::PathBuf;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum WitError {
    #[error("failed to read `{0}`: {1}")]
    Io(PathBuf, #[source] std::io::Error),
    #[error("failed to parse WIT: {0}")]
    Parse(#[from] anyhow::Error),
    #[error("unsupported WIT feature: {0}")]
    UnsupportedFeature(String),
}

pub type WitResult<T> = Result<T, WitError>;
