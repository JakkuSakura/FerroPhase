use crate::span::Span;
use thiserror::Error;
use std::result;

#[derive(Debug)]
pub struct SyntaxError {}

#[derive(Debug)]
pub struct OptimizationError {
    pub message: String,
    pub code: Option<String>,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0:?}")]
    SyntaxError(Span, SyntaxError),
    #[error("Optimization error: {0}")]
    OptimizationError(Span, OptimizationError),
    #[error("Generic error: {0}")]
    Generic(String),
}

pub type Result<T> = result::Result<T, Error>;

// Convert from eyre::Report to our Error type
impl From<eyre::Report> for Error {
    fn from(err: eyre::Report) -> Self {
        Error::Generic(err.to_string())
    }
}

// Convert from std::io::Error to our Error type
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Generic(e.to_string())
    }
}
impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Generic(s)
    }
}
impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Generic(e.to_string())
    }
}
