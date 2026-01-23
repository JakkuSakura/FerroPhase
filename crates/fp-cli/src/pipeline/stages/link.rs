use super::super::*;
use fp_core::config;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use std::process::Command;

pub(crate) struct LinkNativeContext {
    pub base_path: PathBuf,
    pub options: PipelineOptions,
    pub lir_program: fp_core::lir::LirProgram,
}

pub(crate) struct LinkCraneliftContext {
    pub base_path: PathBuf,
    pub options: PipelineOptions,
    pub lir_program: fp_core::lir::LirProgram,
}

pub(crate) struct LinkLlvmContext {
    pub llvm_ir_path: PathBuf,
    pub base_path: PathBuf,
    pub options: PipelineOptions,
}

pub(crate) struct LinkNativeStage;
pub(crate) struct LinkCraneliftStage;
pub(crate) struct LinkLlvmStage;

impl PipelineStage for LinkNativeStage {
    type SrcCtx = LinkNativeContext;
    type DstCtx = PathBuf;

    fn name(&self) -> &'static str {
        STAGE_LINK_BINARY
    }

    fn run(
        &self,
        context: LinkNativeContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<PathBuf, PipelineError> {
        let binary_path = binary_output_path(&context.base_path, &context.options);
        ensure_output_dir(&binary_path, diagnostics)?;
        if (context.options.lossy.enabled || config::lossy_mode())
            && context.lir_program.functions.is_empty()
        {
            diagnostics.push(
                Diagnostic::warning(
                    "lossy mode: skipping native link because the LIR program has no functions"
                        .to_string(),
                )
                .with_source_context(STAGE_LINK_BINARY),
            );
            if let Err(err) = fs::write(&binary_path, "") {
                diagnostics.push(
                    Diagnostic::error(format!("Failed to create placeholder binary: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(PipelineError::new(
                    STAGE_LINK_BINARY,
                    "Failed to create placeholder binary",
                ));
            }
            return Ok(binary_path);
        }

        let mut cfg = fp_native::config::NativeConfig::executable(&binary_path)
            .with_target_triple(context.options.target_triple.clone())
            .with_target_cpu(context.options.target_cpu.clone())
            .with_target_features(context.options.target_features.clone())
            .with_sysroot(context.options.target_sysroot.clone())
            .with_fuse_ld(context.options.target_linker.clone())
            .with_linker_driver(context.options.linker.clone())
            .with_release(context.options.release);
        if context.options.save_intermediates {
            let dump_path = context.base_path.with_extension("asm");
            cfg = cfg.with_asm_dump(Some(dump_path));
        }
        let emitter = fp_native::NativeEmitter::new(cfg);
        emitter.emit(context.lir_program, None).map_err(|e| {
            diagnostics.push(
                Diagnostic::error(format!("fp-native failed: {}", e))
                    .with_source_context(STAGE_LINK_BINARY),
            );
            PipelineError::new(STAGE_LINK_BINARY, "fp-native failed")
        })?;

        Ok(binary_path)
    }
}

impl PipelineStage for LinkLlvmStage {
    type SrcCtx = LinkLlvmContext;
    type DstCtx = PathBuf;

    fn name(&self) -> &'static str {
        STAGE_LINK_BINARY
    }

    fn run(
        &self,
        context: LinkLlvmContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<PathBuf, PipelineError> {
        let binary_path = binary_output_path(&context.base_path, &context.options);
        ensure_output_dir(&binary_path, diagnostics)?;

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
        let default_linker = if requires_eh { "clang++" } else { "clang" };
        // If unwind/exception support is required and the user didn't override the default
        // linker driver (CLI defaults to `clang`), use `clang++` to ensure the C++ runtime
        // and ABI libraries are linked correctly.
        let linker = match context.options.linker.as_deref() {
            Some("clang") if requires_eh => "clang++",
            Some(other) => other,
            None => default_linker,
        };
        if requires_eh {
            let clangxx_available = Command::new(linker).arg("--version").output();
            if matches!(clangxx_available, Err(_)) {
                diagnostics.push(
                    Diagnostic::error(format!(
                        "`{}` not found in PATH; install toolchain to produce binaries with unwind support",
                        linker
                    ))
                    .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(PipelineError::new(
                    STAGE_LINK_BINARY,
                    "linker not found in PATH",
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

            // On Apple platforms, the exception ABI lives in libc++abi and is not always
            // pulled in transitively. Link it explicitly to satisfy __cxa_* and typeinfo.
            if is_apple_target(context.options.target_triple.as_deref()) {
                cmd.arg("-lc++");
                cmd.arg("-lc++abi");
            }
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

impl PipelineStage for LinkCraneliftStage {
    type SrcCtx = LinkCraneliftContext;
    type DstCtx = PathBuf;

    fn name(&self) -> &'static str {
        STAGE_LINK_BINARY
    }

    fn run(
        &self,
        context: LinkCraneliftContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<PathBuf, PipelineError> {
        let binary_path = binary_output_path(&context.base_path, &context.options);
        ensure_output_dir(&binary_path, diagnostics)?;

        let object_path = object_output_path(&context.base_path, &context.options);
        let mut cfg = fp_cranelift::config::CraneliftConfig::object(&object_path)
            .with_target_triple(context.options.target_triple.clone())
            .with_target_cpu(context.options.target_cpu.clone())
            .with_target_features(context.options.target_features.clone())
            .with_sysroot(context.options.target_sysroot.clone())
            .with_linker_driver(context.options.linker.clone())
            .with_fuse_ld(context.options.target_linker.clone())
            .with_release(context.options.release)
            .with_keep_object(context.options.save_intermediates);
        if context.options.save_intermediates {
            let dump_path = context.base_path.with_extension("clif");
            cfg = cfg.with_asm_dump(Some(dump_path));
        }

        let emitter = fp_cranelift::CraneliftEmitter::new(cfg);
        emitter.emit(context.lir_program, None).map_err(|e| {
            diagnostics.push(
                Diagnostic::error(format!("fp-cranelift failed: {}", e))
                    .with_source_context(STAGE_LINK_BINARY),
            );
            PipelineError::new(STAGE_LINK_BINARY, "fp-cranelift failed")
        })?;

        let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../crates/fp-cranelift/runtime/fp_cranelift_runtime.c");
        let extra = vec![runtime_path];
        link_object_with_clang(
            &object_path,
            &binary_path,
            &context.options,
            diagnostics,
            &extra,
        )?;

        if !context.options.save_intermediates {
            if let Err(err) = fs::remove_file(&object_path) {
                debug!(
                    error = %err,
                    path = %object_path.display(),
                    "failed to remove intermediate Cranelift object file after linking"
                );
            }
        }

        Ok(binary_path)
    }
}

fn binary_output_path(base_path: &Path, options: &PipelineOptions) -> PathBuf {
    base_path.with_extension(if is_windows_target(options.target_triple.as_deref()) {
        "exe"
    } else {
        "out"
    })
}

fn object_output_path(base_path: &Path, options: &PipelineOptions) -> PathBuf {
    base_path.with_extension(if is_windows_target(options.target_triple.as_deref()) {
        "obj"
    } else {
        "o"
    })
}

fn ensure_output_dir(
    binary_path: &Path,
    diagnostics: &mut PipelineDiagnostics,
) -> Result<(), PipelineError> {
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
    Ok(())
}

fn link_object_with_clang(
    object_path: &Path,
    binary_path: &Path,
    options: &PipelineOptions,
    diagnostics: &mut PipelineDiagnostics,
    extra_inputs: &[PathBuf],
) -> Result<(), PipelineError> {
    let linker = options
        .linker
        .as_deref()
        .unwrap_or("clang");
    let mut cmd = Command::new(linker);
    cmd.arg(object_path);
    for input in extra_inputs {
        cmd.arg(input);
    }
    if !extra_inputs.is_empty() {
        cmd.arg("-lm");
    }
    if let Some(target_triple) = options.target_triple.as_deref() {
        cmd.arg("--target").arg(target_triple);
    }
    if let Some(sysroot) = options.target_sysroot.as_ref() {
        cmd.arg("--sysroot").arg(sysroot);
    }
    if let Some(linker_path) = options.target_linker.as_ref() {
        cmd.arg(format!("-fuse-ld={}", linker_path.display()));
    }
    cmd.arg("-o").arg(binary_path);
    if options.release {
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

    Ok(())
}

fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}

fn is_apple_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(any(target_os = "macos", target_os = "ios")),
    };
    triple.contains("apple") || triple.contains("darwin") || triple.contains("macos")
}

impl Pipeline {
    pub(crate) fn stage_link_binary(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = LinkLlvmStage;
        let context = LinkLlvmContext {
            llvm_ir_path: llvm_ir_path.to_path_buf(),
            base_path: base_path.to_path_buf(),
            options: options.clone(),
        };
        self.run_pipeline_stage(STAGE_LINK_BINARY, stage, context, options)
    }

    pub(crate) fn stage_link_binary_native(
        &self,
        lir_program: &fp_core::lir::LirProgram,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = LinkNativeStage;
        let context = LinkNativeContext {
            base_path: base_path.to_path_buf(),
            options: options.clone(),
            lir_program: lir_program.clone(),
        };
        self.run_pipeline_stage(STAGE_LINK_BINARY, stage, context, options)
    }

    pub(crate) fn stage_link_binary_cranelift(
        &self,
        lir_program: &fp_core::lir::LirProgram,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<PathBuf, CliError> {
        let stage = LinkCraneliftStage;
        let context = LinkCraneliftContext {
            base_path: base_path.to_path_buf(),
            options: options.clone(),
            lir_program: lir_program.clone(),
        };
        self.run_pipeline_stage(STAGE_LINK_BINARY, stage, context, options)
    }
}
