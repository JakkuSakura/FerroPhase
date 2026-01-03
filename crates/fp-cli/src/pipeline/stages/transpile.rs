use super::super::*;

impl Pipeline {
    pub(crate) fn stage_closure_lowering(
        &self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        match fp_optimize::transformations::lower_closures(ast) {
            Ok(()) => Ok(()),
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Closure lowering failed: {}", err))
                        .with_source_context(STAGE_CLOSURE_LOWERING),
                );
                Err(Self::stage_failure(STAGE_CLOSURE_LOWERING))
            }
        }
    }

    pub(crate) fn stage_materialize_runtime_intrinsics(
        &self,
        ast: &mut Node,
        target: &PipelineTarget,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        if options.bootstrap_mode {
            return Ok(());
        }

        let materializer = IntrinsicsMaterializer::for_target(target);
        let result = materializer.materialize(ast);

        match result {
            Ok(()) => Ok(()),
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Failed to materialize runtime intrinsics: {}", err))
                        .with_source_context(STAGE_RUNTIME_MATERIALIZE),
                );
                Err(Self::stage_failure(STAGE_RUNTIME_MATERIALIZE))
            }
        }
    }

    pub(crate) fn stage_backend_lowering(
        &self,
        hir_program: &hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
        manager: &DiagnosticManager,
    ) -> Result<BackendArtifacts, CliError> {
        let mut mir_lowering = MirLowering::new();
        if self.bootstrap_mode || options.bootstrap_mode {
            mir_lowering.set_error_tolerance(true);
        }
        let mir_result = mir_lowering.transform(hir_program.clone());
        let (mir_diags, mir_had_errors) = mir_lowering.take_diagnostics();
        manager.add_diagnostics(mir_diags);
        let mir_program = match (mir_result, mir_had_errors) {
            (Ok(program), false) => program,
            (Ok(_), true) => {
                manager.add_diagnostic(
                    Diagnostic::error("HIR→MIR lowering reported errors".to_string())
                        .with_source_context(STAGE_BACKEND_LOWERING),
                );
                return Err(Self::stage_failure(STAGE_BACKEND_LOWERING));
            }
            (Err(err), _) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("HIR→MIR lowering failed: {}", err))
                        .with_source_context(STAGE_BACKEND_LOWERING),
                );
                return Err(Self::stage_failure(STAGE_BACKEND_LOWERING));
            }
        };

        let mut pretty_opts = PrettyOptions::default();
        pretty_opts.show_spans = options.debug.verbose;
        let mir_text = format!("{}", pretty(&mir_program, pretty_opts.clone()));

        if options.save_intermediates {
            if let Err(err) = fs::write(base_path.with_extension("mir"), &mir_text) {
                debug!(error = %err, "failed to persist MIR intermediate");
            }
        }

        let mut lir_generator = LirGenerator::new();
        let lir_program = lir_generator
            .transform(mir_program.clone())
            .map_err(|err| CliError::Compilation(format!("MIR→LIR lowering failed: {}", err)))?;

        let lir_text = format!("{}", pretty(&lir_program, pretty_opts));

        if options.save_intermediates {
            if let Err(err) = fs::write(base_path.with_extension("lir"), &lir_text) {
                debug!(error = %err, "failed to persist LIR intermediate");
            }
        }

        Ok(BackendArtifacts {
            lir_program,
            mir_text,
            lir_text,
        })
    }

    pub(crate) fn generate_llvm_artifacts(
        &self,
        lir_program: &lir::LirProgram,
        base_path: &Path,
        source_path: Option<&Path>,
        retain_file: bool,
        options: &PipelineOptions,
    ) -> Result<LlvmArtifacts, CliError> {
        let llvm_path = base_path.with_extension("ll");
        let module_name = base_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(sanitize_module_identifier)
            .unwrap_or_else(|| "module".to_string());
        let allow_unresolved_globals = self.bootstrap_mode || options.bootstrap_mode;
        let config = LlvmConfig::new()
            .with_linker(LinkerConfig::executable(&llvm_path))
            .with_module_name(module_name)
            .with_allow_unresolved_globals(allow_unresolved_globals);
        let compiler = LlvmCompiler::new(config);

        // In bootstrap mode avoid reading LLVM text back from disk because std::fs
        // calls may be normalised away in the self-hosted compiler.
        let ir_text = if self.bootstrap_mode || options.bootstrap_mode {
            match compiler.compile_to_string(lir_program.clone(), source_path) {
                Ok((_path, text)) => text,
                Err(err) => {
                    return Err(CliError::Compilation(format!(
                        "LIR→LLVM lowering failed: {}",
                        err
                    )));
                }
            }
        } else {
            compiler
                .compile(lir_program.clone(), source_path)
                .map_err(|err| {
                    CliError::Compilation(format!("LIR→LLVM lowering failed: {}", err))
                })?;
            fs::read_to_string(&llvm_path)?
        };

        if !retain_file && !options.save_intermediates {
            if let Err(err) = fs::remove_file(&llvm_path) {
                debug!(error = %err, "failed to remove temporary LLVM IR file");
            }
        }

        Ok(LlvmArtifacts {
            ir_text,
            ir_path: llvm_path,
        })
    }
}
