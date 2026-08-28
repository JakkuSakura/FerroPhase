use thiserror::Error;

impl From<fp_interpret::VmError> for CompilerDriverError {
    fn from(e: fp_interpret::VmError) -> Self {
        CompilerDriverError::Interpreter(e.to_string())
    }
}

impl From<fp_bytecode::BytecodeError> for CompilerDriverError {
    fn from(e: fp_bytecode::BytecodeError) -> Self {
        CompilerDriverError::UnsupportedWork(format!("bytecode: {}", e))
    }
}

#[derive(Debug, Error)]
pub enum CompilerDriverError {
    #[error("{0}")]
    Core(#[from] fp_core::Error),
    #[error("interpreter error: {0}")]
    Interpreter(String),
    #[error("missing HIR {0}")]
    MissingHir(String),
    #[error("missing LIR {0}")]
    MissingLir(String),
    #[error("unsupported compiler work: {0}")]
    UnsupportedWork(String),
    #[error("internal compiler error: {0}")]
    InternalCompilerError(String),
    #[error("unresolvable comptime cycle: {0}")]
    UnresolvableComptime(String),
    #[error("package resolution failed: {0}")]
    UnresolvablePackage(String),
}
