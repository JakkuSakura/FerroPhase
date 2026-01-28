use crate::diagnostics::Diagnostic;
use crate::span::Span;
use std::fmt;
use std::result;

#[derive(Debug, Clone)]
pub struct SyntaxError;

#[derive(Debug)]
pub enum Error {
    SyntaxError(Span, SyntaxError),
    Diagnostic(Diagnostic),
    RuntimeError(crate::ast::RuntimeError),
    Message(String),
    Io(std::io::Error),
}

impl Error {
    pub fn diagnostic(diagnostic: Diagnostic) -> Self {
        Error::Diagnostic(diagnostic)
    }
}

pub type Result<T> = result::Result<T, Error>;

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}
impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Message(s)
    }
}
impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::Message(s.to_string())
    }
}
impl From<crate::ast::RuntimeError> for Error {
    fn from(e: crate::ast::RuntimeError) -> Self {
        Error::RuntimeError(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::SyntaxError(_, _) => write!(f, "syntax error"),
            Error::Diagnostic(diagnostic) => write!(f, "{}", diagnostic.message),
            Error::RuntimeError(err) => write!(f, "runtime error: {}", err),
            Error::Message(message) => write!(f, "{}", message),
            Error::Io(err) => write!(f, "io error: {}", err),
        }
    }
}

impl std::error::Error for Error {}
