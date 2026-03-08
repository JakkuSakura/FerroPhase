use fp_core::mir;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum JvmError {
    #[error("JVM lowering failed: {0}")]
    Lowering(String),
    #[error("unsupported MIR item: {0}")]
    UnsupportedItem(&'static str),
    #[error("unsupported MIR type: {0:?}")]
    UnsupportedType(mir::ty::Ty),
    #[error("unsupported MIR rvalue")]
    UnsupportedRvalue,
    #[error("unsupported MIR terminator")]
    UnsupportedTerminator,
    #[error("missing MIR body for function {0}")]
    MissingBody(String),
    #[error("missing MIR terminator in block {0}")]
    MissingTerminator(usize),
    #[error("integer literal out of range for JVM int: {0}")]
    IntOutOfRange(i64),
}
