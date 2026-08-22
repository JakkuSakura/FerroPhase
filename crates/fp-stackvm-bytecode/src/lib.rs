//! `TargetBackend` for the `bytecode`/`text-bytecode` targets, plus
//! `--exec` support for them.
//!
//! This lives in its own crate rather than in `fp-bytecode` because
//! running the compiled bytecode requires `fp-stackvm`'s `Vm`, and
//! `fp-stackvm` itself depends on `fp-bytecode` for the bytecode format
//! types its lowering pass and VM operate on — `fp-bytecode` depending
//! back on `fp-stackvm` would be a cycle. Sitting above both breaks that
//! cycle: neither `fp-bytecode` nor `fp-stackvm` need to know this crate
//! exists.

/// `TargetBackend` for the bytecode/text-bytecode `--target` options — a
/// package's own compiled MIR (already sitting on `CompiledPackage` from
/// the shared workspace's typecheck pass) lowers directly to
/// `BytecodeProgram`, the same as `CompilerDriver::compile_bytecode` used
/// to do, just read from the workspace instead of re-driving a second
/// compile.
pub struct BytecodeBackend {
    pub output: std::path::PathBuf,
    pub emit_text: bool,
    pub save_intermediates: bool,
}

impl fp_core::backend::TargetBackend for BytecodeBackend {
    fn emit_package_artifact(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::ast::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let package = workspace.compiled_package(package_id).ok_or_else(|| {
            fp_core::error::Error::from(format!("package `{package_id}` is unavailable"))
        })?;
        let mir = package.borrow().mir_program.clone().ok_or_else(|| {
            fp_core::error::Error::from(format!("package `{package_id}` has no MIR program"))
        })?;
        let bytecode = fp_bytecode::lower_program(&mir)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        if let Some(parent) = self.output.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let wants_text = self.emit_text
            || self.output.extension().and_then(|ext| ext.to_str()) == Some("ftbc");

        if self.save_intermediates || wants_text {
            let rendered = fp_bytecode::format_program(&bytecode);
            let text_path = if wants_text {
                self.output.clone()
            } else {
                self.output.with_extension("ftbc")
            };
            std::fs::write(&text_path, rendered)?;
        }

        if !wants_text || self.save_intermediates {
            let bytes = fp_bytecode::encode_file(&bytecode)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            let binary_path = if wants_text {
                self.output.with_extension("fbc")
            } else {
                self.output.clone()
            };
            std::fs::write(binary_path, bytes)?;
        }

        Ok(())
    }

    fn exec(&self) -> fp_core::error::Result<()> {
        if self.emit_text {
            return Err(fp_core::error::Error::from(
                "--exec is not supported for text-bytecode output".to_string(),
            ));
        }
        let bytes = std::fs::read(&self.output)?;
        let file = fp_bytecode::decode_file(&bytes)
            .map_err(|e| fp_core::error::Error::from(format!("failed to decode bytecode: {e}")))?;
        let vm = fp_stackvm::Vm::new(file.program);
        vm.run_main()
            .map_err(|e| fp_core::error::Error::from(format!("bytecode execution failed: {e}")))?;
        Ok(())
    }
}
