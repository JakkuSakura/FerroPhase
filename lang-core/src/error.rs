use crate::span::Span;
use thiserror::Error;
#[derive(Debug)]
pub struct SyntaxError {}
#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0:?}")]
    SyntaxError(Span, SyntaxError),
}
