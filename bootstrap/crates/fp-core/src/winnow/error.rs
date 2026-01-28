use std::fmt;

#[derive(Debug, Clone)]
pub struct ContextError {
    message: Option<String>,
}

impl ContextError {
    pub fn new() -> Self {
        Self { message: None }
    }

    pub fn with_message(message: impl Into<String>) -> Self {
        Self {
            message: Some(message.into()),
        }
    }
}

impl fmt::Display for ContextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(message) = &self.message {
            write!(f, "{message}")
        } else {
            write!(f, "parse error")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    Fail,
    Verify,
    Tag,
    Other,
}

#[derive(Debug, Clone)]
pub enum ErrMode<E> {
    Backtrack(E),
    Cut(E),
    Incomplete(usize),
}

impl<E: fmt::Display> fmt::Display for ErrMode<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrMode::Backtrack(err) => write!(f, "backtrack: {}", err),
            ErrMode::Cut(err) => write!(f, "cut: {}", err),
            ErrMode::Incomplete(needed) => write!(f, "incomplete: {}", needed),
        }
    }
}

pub trait FromExternalError<I, E> {
    fn from_external_error(_input: &I, _kind: ErrorKind, error: E) -> Self;
}

impl<I, E> FromExternalError<I, E> for ContextError
where
    E: fmt::Display,
{
    fn from_external_error(_input: &I, _kind: ErrorKind, error: E) -> Self {
        ContextError::with_message(error.to_string())
    }
}
