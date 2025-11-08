use super::super::*;

impl Pipeline {
    pub(crate) fn stage_link_binary(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<PathBuf, CliError> {
        let binary_path = base_path.with_extension(if cfg!(target_os = "windows") { "exe" } else { "out" });

        if let Some(parent) = binary_path.parent() {
            if let Err(err) = fs::create_dir_all(parent) {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Failed to create output directory: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(Self::stage_failure(STAGE_LINK_BINARY));
            }
        }

        let clang_available = Command::new("clang").arg("--version").output();
        if matches!(clang_available, Err(_)) {
            manager.add_diagnostic(
                Diagnostic::error("`clang` not found in PATH; install LLVM toolchain to produce binaries".to_string())
                    .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(Self::stage_failure(STAGE_LINK_BINARY));
        }

        let mut cmd = Command::new("clang");
        cmd.arg(llvm_ir_path).arg("-o").arg(&binary_path);
        if options.release { cmd.arg("-O2"); }

        let output = match cmd.output() {
            Ok(output) => output,
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Failed to invoke clang: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                );
                return Err(Self::stage_failure(STAGE_LINK_BINARY));
            }
        };

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut message = stderr.trim().to_string();
            if message.is_empty() { message = stdout.trim().to_string(); }
            if message.is_empty() { message = "clang failed without diagnostics".to_string(); }
            manager.add_diagnostic(
                Diagnostic::error(format!("clang failed: {}", message))
                    .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(Self::stage_failure(STAGE_LINK_BINARY));
        }

        if !options.save_intermediates {
            if let Err(err) = fs::remove_file(llvm_ir_path) {
                debug!(error = %err, path = %llvm_ir_path.display(), "failed to remove intermediate LLVM IR file after linking");
            }
        }

        Ok(binary_path)
    }
}
