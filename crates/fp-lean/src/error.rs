//! Parse-time errors for the Lean frontend.

use fp_core::error::Error as CoreError;

#[derive(Debug, thiserror::Error)]
pub enum LeanParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,
    #[error("unexpected character `{0}`")]
    UnexpectedChar(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("unterminated block comment")]
    UnterminatedBlockComment,
    #[error("expected {expected}, found {found}")]
    Expected { expected: String, found: String },
}

impl From<LeanParseError> for CoreError {
    fn from(err: LeanParseError) -> Self {
        CoreError::from(err.to_string())
    }
}
