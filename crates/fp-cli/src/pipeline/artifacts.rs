use crate::CliError;
use crate::languages::frontend::FrontendSnapshot;
use fp_core::ast::Node;
use fp_core::{hir, lir, mir};
use fp_interpret::const_eval::ConstEvalOutcome;

#[derive(Debug, Clone)]
pub struct FrontendBundle {
    pub source_language: String,
    pub ast: Node,
    pub frontend_snapshot: Option<FrontendSnapshot>,
    pub const_eval: Option<ConstEvalOutcome>,
}

#[derive(Debug, Clone)]
pub struct MirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: hir::Program,
    pub mir_program: mir::Program,
}

#[derive(Debug, Clone)]
pub struct LirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: hir::Program,
    pub mir_program: mir::Program,
    pub lir_program: lir::LirProgram,
}

#[derive(Debug)]
pub(crate) struct MirArtifacts {
    pub(crate) mir_program: mir::Program,
}

#[derive(Debug)]
pub(crate) struct LirArtifacts {
    pub(crate) lir_program: lir::LirProgram,
}

/// LLVM artifacts produced by LIR→LLVM lowering.
#[derive(Debug)]
pub(crate) struct LlvmArtifacts {
    pub(crate) ir_text: String,
    pub(crate) ir_path: std::path::PathBuf,
}

#[allow(dead_code)]
pub(crate) fn _unused_error(_e: CliError) {}
