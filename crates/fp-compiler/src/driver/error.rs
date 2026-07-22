use thiserror::Error;

use crate::module_resolution::ModuleResolutionError;
use crate::scheduler::{
    AstId, ConstValueId, HirId, LirId, MirId, RuntimeValueId, SchedulerError, TypedAstId,
};

#[derive(Debug, Error)]
pub enum CompilerDriverError {
    #[error("{0}")]
    Core(#[from] fp_core::Error),
    #[error("scheduler error: {0}")]
    Scheduler(#[from] SchedulerError),
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
    #[error("module resolution failed: {0}")]
    ModuleResolution(#[from] ModuleResolutionError),
    #[error("unsupported compiler work: {0}")]
    UnsupportedWork(String),
    #[error("unresolvable comptime cycle for {0}")]
    UnresolvableComptime(AstId),
}
