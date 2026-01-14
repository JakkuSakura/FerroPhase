use crate::pipeline::{Pipeline, PipelineOptions, STAGE_EMIT_WASM};
use crate::CliError;
use fp_core::diagnostics::Diagnostic;
use fp_core::lir;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) struct WasmEmitContext {
    pub lir_program: lir::LirProgram,
    pub base_path: PathBuf,
}

pub(crate) struct WasmEmitStage;

impl PipelineStage for WasmEmitStage {
    type SrcCtx = WasmEmitContext;
    type DstCtx = PathBuf;

    fn name(&self) -> &'static str {
        STAGE_EMIT_WASM
    }

    fn run(
        &self,
        context: WasmEmitContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<PathBuf, PipelineError> {
        let wasm_path = context.base_path.with_extension("wasm");
        if let Some(parent) = wasm_path.parent() {
            if let Err(err) = fs::create_dir_all(parent) {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to create output directory: {}", err))
                        .with_source_context(STAGE_EMIT_WASM),
                );
                return Err(PipelineError::new(
                    STAGE_EMIT_WASM,
                    "Failed to create output directory",
                ));
            }
        }

        let wasm_bytes = match fp_wasm::emit_wasm(&context.lir_program) {
            Ok(bytes) => bytes,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to emit wasm: {}", err))
                        .with_source_context(STAGE_EMIT_WASM),
                );
                return Err(PipelineError::new(STAGE_EMIT_WASM, "wasm emit failed"));
            }
        };
        if let Err(err) = fs::write(&wasm_path, wasm_bytes) {
            diagnostics.push(
                Diagnostic::error(format!("Failed to write wasm output: {}", err))
                    .with_source_context(STAGE_EMIT_WASM),
            );
            return Err(PipelineError::new(STAGE_EMIT_WASM, "wasm write failed"));
        }

        Ok(wasm_path)
    }
}

impl Pipeline {
    pub(crate) fn stage_emit_wasm(
        &self,
        lir_program: &lir::LirProgram,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = WasmEmitStage;
        let context = WasmEmitContext {
            lir_program: lir_program.clone(),
            base_path: base_path.to_path_buf(),
        };
        self.run_pipeline_stage(STAGE_EMIT_WASM, stage, context, options)
    }
}
