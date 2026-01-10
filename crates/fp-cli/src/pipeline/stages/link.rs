use super::super::*;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use std::process::Command;

pub(crate) struct LinkContext {
    pub llvm_ir_path: PathBuf,
    pub base_path: PathBuf,
    pub options: PipelineOptions,
}

pub(crate) struct LinkStage;

impl PipelineStage for LinkStage {
    type SrcCtx = LinkContext;
    type DstCtx = PathBuf;

    fn name(&self) -> &'static str {
        STAGE_LINK_BINARY
    }

    fn run(
        &self,
        context: LinkContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<PathBuf, PipelineError> {
        let binary_path = context.base_path.with_extension(
            if is_windows_target(context.options.target_triple.as_deref()) {
                "exe"
            } else {
                "out"
            },
        );

        if let Some(parent) = binary_path.parent() {
            if let Err(err) = fs::create_dir_all(parent) {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to create output directory: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(PipelineError::new(
                    STAGE_LINK_BINARY,
                    "Failed to create output directory",
                ));
            }
        }

        let clang_available = Command::new("clang").arg("--version").output();
        if matches!(clang_available, Err(_)) {
            diagnostics.push(
                Diagnostic::error(
                    "`clang` not found in PATH; install LLVM toolchain to produce binaries"
                        .to_string(),
                )
                .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(PipelineError::new(
                STAGE_LINK_BINARY,
                "`clang` not found in PATH",
            ));
        }

        let llvm_ir_text = fs::read_to_string(&context.llvm_ir_path).unwrap_or_default();
        let requires_eh = llvm_ir_text.contains("landingpad") || llvm_ir_text.contains("invoke");
        let linker = if requires_eh { "clang++" } else { "clang" };
        if requires_eh {
            let clangxx_available = Command::new("clang++").arg("--version").output();
            if matches!(clangxx_available, Err(_)) {
                diagnostics.push(
                    Diagnostic::error(
                        "`clang++` not found in PATH; install LLVM toolchain to produce binaries with unwind support"
                            .to_string(),
                    )
                    .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(PipelineError::new(
                    STAGE_LINK_BINARY,
                    "`clang++` not found in PATH",
                ));
            }
        }
        let mut cmd = Command::new(linker);
        cmd.arg(&context.llvm_ir_path);
        if requires_eh {
            let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../crates/fp-llvm/runtime/fp_unwind.cc");
            cmd.arg(runtime_path);
            cmd.arg("-fexceptions");
        }
        if let Some(target_triple) = context.options.target_triple.as_deref() {
            cmd.arg("--target").arg(target_triple);
        }
        if let Some(sysroot) = context.options.target_sysroot.as_ref() {
            cmd.arg("--sysroot").arg(sysroot);
        }
        if let Some(linker_path) = context.options.target_linker.as_ref() {
            cmd.arg(format!("-fuse-ld={}", linker_path.display()));
        }
        cmd.arg("-o").arg(&binary_path);
        if context.options.release {
            cmd.arg("-O2");
        }

        let output = match cmd.output() {
            Ok(output) => output,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to invoke clang: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(PipelineError::new(
                    STAGE_LINK_BINARY,
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
                    .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(PipelineError::new(STAGE_LINK_BINARY, "clang failed"));
        }

        if !context.options.save_intermediates {
            if let Err(err) = fs::remove_file(&context.llvm_ir_path) {
                debug!(
                    error = %err,
                    path = %context.llvm_ir_path.display(),
                    "failed to remove intermediate LLVM IR file after linking"
                );
            }
        }

        Ok(binary_path)
    }
}

fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}

impl Pipeline {
    pub(crate) fn stage_link_binary(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = LinkStage;
        let context = LinkContext {
            llvm_ir_path: llvm_ir_path.to_path_buf(),
            base_path: base_path.to_path_buf(),
            options: options.clone(),
        };
        self.run_pipeline_stage(STAGE_LINK_BINARY, stage, context, options)
    }
}
