use crate::CliError;
use fp_core::lir;

/// Backend intermediate artifacts (MIR/LIR text and program) used by the pipeline.
/// Internal to the `pipeline` module.
#[derive(Debug)]
pub(crate) struct BackendArtifacts {
    pub(crate) lir_program: lir::LirProgram,
    pub(crate) mir_text: String,
    pub(crate) lir_text: String,
}

/// LLVM artifacts produced by LIR→LLVM lowering.
#[derive(Debug)]
pub(crate) struct LlvmArtifacts {
    pub(crate) ir_text: String,
    pub(crate) ir_path: std::path::PathBuf,
}

#[allow(dead_code)]
pub(crate) fn _unused_error(_e: CliError) {}
