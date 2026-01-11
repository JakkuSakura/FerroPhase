use crate::pipeline::{Pipeline, STAGE_EMIT_WASM};
use crate::CliError;
use fp_core::diagnostics::Diagnostic;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineOptions, PipelineStage};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::debug;

pub(crate) struct WasmEmitContext {
    pub llvm_ir_path: PathBuf,
    pub base_path: PathBuf,
    pub options: PipelineOptions,
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

        let (mut cmd, uses_driver) = if let Some(linker_path) = context.options.target_linker.as_ref()
        {
            let is_direct = is_direct_wasm_linker(linker_path);
            (Command::new(linker_path), !is_direct)
        } else {
            (Command::new("clang"), true)
        };

        cmd.arg(&context.llvm_ir_path);
        let target = context
            .options
            .target_triple
            .as_deref()
            .unwrap_or("wasm32-unknown-unknown");
        if uses_driver {
            cmd.arg(format!("--target={}", target));
            if let Some(sysroot) = context.options.target_sysroot.as_ref() {
                cmd.arg(format!("--sysroot={}", sysroot.display()));
            }
            cmd.arg("-nostdlib");
            cmd.arg("-Wl,--no-entry");
            cmd.arg("-Wl,--export-all");
            cmd.arg("-Wl,--allow-undefined");
            if context.options.release {
                cmd.arg("-O2");
            }
        } else {
            cmd.arg("--no-entry");
            cmd.arg("--export-all");
            cmd.arg("--allow-undefined");
        }
        cmd.arg("-o").arg(&wasm_path);

        let output = match cmd.output() {
            Ok(output) => output,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to invoke clang: {}", err))
                        .with_source_context(STAGE_EMIT_WASM),
                );
                return Err(PipelineError::new(
                    STAGE_EMIT_WASM,
                    "Failed to invoke clang",
                ));
            }
        };

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut message = stderr.trim().to_string();
            if message.is_empty() {
                message = stdout.trim().to_string();
            }
            if message.is_empty() {
                message = "clang failed without diagnostics".to_string();
            }
            diagnostics.push(
                Diagnostic::error(format!("clang failed: {}", message))
                    .with_source_context(STAGE_EMIT_WASM),
            );
            return Err(PipelineError::new(STAGE_EMIT_WASM, "clang failed"));
        }

        if !context.options.save_intermediates {
            if let Err(err) = fs::remove_file(&context.llvm_ir_path) {
                debug!(
                    error = %err,
                    path = %context.llvm_ir_path.display(),
                    "failed to remove intermediate LLVM IR file after wasm emit"
                );
            }
        }

        Ok(wasm_path)
    }
}

fn is_direct_wasm_linker(path: &Path) -> bool {
    let name = path
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or_default()
        .to_lowercase();
    name.contains("wasm-ld") || name == "ld.lld"
}

impl Pipeline {
    pub(crate) fn stage_emit_wasm(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = WasmEmitStage;
        let context = WasmEmitContext {
            llvm_ir_path: llvm_ir_path.to_path_buf(),
            base_path: base_path.to_path_buf(),
            options: options.clone(),
        };
        self.run_pipeline_stage(STAGE_EMIT_WASM, stage, context, options)
    }
}
