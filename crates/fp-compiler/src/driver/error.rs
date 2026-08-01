use thiserror::Error;

use crate::module_resolution::ModuleResolutionError;
use crate::scheduler::{
    AstId, BytecodeId, ConstValueId, HirId, LirId, MirId, RuntimeValueId, SchedulerError,
    TypedAstId,
};

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
    #[error("scheduler error: {0}")]
    Scheduler(#[from] SchedulerError),
    #[error("interpreter error: {0}")]
    Interpreter(String),
    #[error("missing AST {0}")]
    MissingAst(AstId),
    #[error("missing typed AST {0}")]
    MissingTypedAst(TypedAstId),
    #[error("missing HIR {0}")]
    MissingHir(HirId),
    #[error("missing MIR {0}")]
    MissingMir(MirId),
    #[error("missing LIR {0}")]
    MissingLir(LirId),
    #[error("missing const value {0}")]
    MissingConstValue(ConstValueId),
    #[error("missing runtime value {0}")]
    MissingRuntimeValue(RuntimeValueId),
    #[error("missing bytecode {0}")]
    MissingBytecode(BytecodeId),
    #[error("module resolution failed: {0}")]
    ModuleResolution(#[from] ModuleResolutionError),
    #[error("unsupported compiler work: {0}")]
    UnsupportedWork(String),
    #[error("unresolvable comptime cycle: {0}")]
    UnresolvableComptime(String),
    #[error("package {0} could not be loaded (no provider registered, or the provider failed repeatedly)")]
    UnresolvablePackage(String),
}
