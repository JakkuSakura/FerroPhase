use crate::diagnostics::Diagnostic;
use crate::span::Span;
use eyre::Error as EyreError;
use std::result;
use thiserror::Error;

#[derive(Debug)]
pub struct SyntaxError {}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0:?}")]
    SyntaxError(Span, SyntaxError),
    #[error("{0}")]
    Diagnostic(Diagnostic),
    #[error("Runtime error: {0}")]
    RuntimeError(crate::ast::RuntimeError),
    #[error("Generic error: {0}")]
    Generic(EyreError),
}

impl Error {
    pub fn diagnostic(diagnostic: Diagnostic) -> Self {
        Error::Diagnostic(diagnostic)
    }
}

pub type Result<T> = result::Result<T, Error>;

impl From<EyreError> for Error {
    fn from(err: EyreError) -> Self {
        Error::Generic(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Generic(e.into())
    }
}
impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Generic(EyreError::msg(s))
    }
}
impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::Generic(EyreError::msg(s.to_string()))
    }
}
impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Generic(EyreError::new(e))
    }
}
impl From<crate::ast::RuntimeError> for Error {
    fn from(e: crate::ast::RuntimeError) -> Self {
        Error::RuntimeError(e)
    }
}
