use super::super::artifacts::{LirArtifacts, MirArtifacts};
use super::super::*;
use fp_backend::optimizer::{MirOptimizer, OptimizationPlan};
use fp_core::mir;
use fp_core::config;
use fp_llvm::target::{OptimizationLevel, TargetConfig};
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use std::sync::Arc;

pub(crate) struct HirToMirContext {
    pub hir_program: Arc<hir::Program>,
    pub options: PipelineOptions,
    pub base_path: PathBuf,
}

pub(crate) struct HirToMirStage;

impl PipelineStage for HirToMirStage {
    type SrcCtx = HirToMirContext;
    type DstCtx = MirArtifacts;

    fn name(&self) -> &'static str {
        STAGE_HIR_TO_MIR
    }

    fn run(
        &self,
        context: HirToMirContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<MirArtifacts, PipelineError> {
        let mut mir_lowering = MirLowering::new();
        let tolerate_errors = context.options.error_tolerance.enabled || config::lossy_mode();
        mir_lowering.set_error_tolerance(tolerate_errors);
        let mir_result = mir_lowering.transform(context.hir_program.as_ref().clone());
        let (mir_diags, mir_had_errors) = mir_lowering.take_diagnostics();
        diagnostics.extend(mir_diags);
        let mut mir_program = match (mir_result, mir_had_errors) {
            (Ok(program), false) => program,
            (Ok(_), true) => {
                if tolerate_errors {
                    diagnostics.push(
                        Diagnostic::warning(
                            "HIR→MIR lowering reported errors; continuing due to error tolerance"
                                .to_string(),
                        )
                        .with_source_context(STAGE_HIR_TO_MIR),
                    );
                    mir::Program::new()
                } else {
                    diagnostics.push(
                        Diagnostic::error("HIR→MIR lowering reported errors".to_string())
                            .with_source_context(STAGE_HIR_TO_MIR),
                    );
                    return Err(PipelineError::new(
                        STAGE_HIR_TO_MIR,
                        "HIR→MIR lowering reported errors",
                    ));
                }
            }
            (Err(err), _) => {
                if tolerate_errors {
                    diagnostics.push(
                        Diagnostic::warning(format!(
                            "HIR→MIR lowering failed: {}; continuing due to error tolerance",
                            err
                        ))
                        .with_source_context(STAGE_HIR_TO_MIR),
                    );
                    mir::Program::new()
                } else {
                    diagnostics.push(
                        Diagnostic::error(format!("HIR→MIR lowering failed: {}", err))
                            .with_source_context(STAGE_HIR_TO_MIR),
                    );
                    return Err(PipelineError::new(
                        STAGE_HIR_TO_MIR,
                        "HIR→MIR lowering failed",
                    ));
                }
            }
        };

        let opt_plan = OptimizationPlan::for_level(context.options.optimization_level);
        if !opt_plan.is_empty() {
            let optimizer = MirOptimizer::new();
            if let Err(err) = optimizer.apply_plan(&mut mir_program, &opt_plan) {
                diagnostics.push(
                    Diagnostic::error(format!("MIR optimization failed: {}", err))
                        .with_source_context(STAGE_HIR_TO_MIR),
                );
                return Err(PipelineError::new(
                    STAGE_HIR_TO_MIR,
                    "MIR optimization failed",
                ));
            }
        }

        let mut pretty_opts = PrettyOptions::default();
        pretty_opts.show_spans = context.options.debug.verbose;
        let mir_text = format!("{}", pretty(&mir_program, pretty_opts.clone()));

        if context.options.save_intermediates {
            if let Err(err) = fs::write(context.base_path.with_extension("mir"), &mir_text) {
                debug!(error = %err, "failed to persist MIR intermediate");
            }
        }

        Ok(MirArtifacts { mir_program })
    }
}

pub(crate) struct MirToLirContext {
    pub mir_program: Arc<mir::Program>,
    pub options: PipelineOptions,
    pub base_path: PathBuf,
}

pub(crate) struct MirToLirStage;

impl PipelineStage for MirToLirStage {
    type SrcCtx = MirToLirContext;
    type DstCtx = LirArtifacts;

    fn name(&self) -> &'static str {
        STAGE_MIR_TO_LIR
    }

    fn run(
        &self,
        context: MirToLirContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<LirArtifacts, PipelineError> {
        let mut lir_generator = LirGenerator::new();
        let lir_program = match lir_generator.transform(context.mir_program.as_ref().clone()) {
            Ok(program) => program,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("MIR→LIR lowering failed: {}", err))
                        .with_source_context(STAGE_MIR_TO_LIR),
                );
                return Err(PipelineError::new(
                    STAGE_MIR_TO_LIR,
                    "MIR→LIR lowering failed",
                ));
            }
        };

        let mut pretty_opts = PrettyOptions::default();
        pretty_opts.show_spans = context.options.debug.verbose;
        let lir_text = format!("{}", pretty(&lir_program, pretty_opts));

        if context.options.save_intermediates {
            if let Err(err) = fs::write(context.base_path.with_extension("lir"), &lir_text) {
                debug!(error = %err, "failed to persist LIR intermediate");
            }
        }

        Ok(LirArtifacts { lir_program })
    }
}

impl Pipeline {
    pub(crate) fn stage_hir_to_mir(
        &self,
        hir_program: &hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<MirArtifacts, CliError> {
        let stage = HirToMirStage;
        let context = HirToMirContext {
            hir_program: Arc::new(hir_program.clone()),
            options: options.clone(),
            base_path: base_path.to_path_buf(),
        };
        self.run_pipeline_stage(STAGE_HIR_TO_MIR, stage, context, options)
    }

    pub(crate) fn stage_mir_to_lir(
        &self,
        mir_program: &mir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<LirArtifacts, CliError> {
        let stage = MirToLirStage;
        let context = MirToLirContext {
            mir_program: Arc::new(mir_program.clone()),
            options: options.clone(),
            base_path: base_path.to_path_buf(),
        };
        self.run_pipeline_stage(STAGE_MIR_TO_LIR, stage, context, options)
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
        let allow_unresolved_globals = false;
        let target_config = target_config_from_options(options);
        let config = LlvmConfig::new()
            .with_linker(LinkerConfig::executable(&llvm_path))
            .with_module_name(module_name)
            .with_allow_unresolved_globals(allow_unresolved_globals)
            .with_target(target_config);
        let compiler = LlvmCompiler::new(config);

        compiler
            .compile(lir_program.clone(), source_path)
            .map_err(|err| CliError::Compilation(format!("LIR→LLVM lowering failed: {}", err)))?;
        let ir_text = fs::read_to_string(&llvm_path)?;

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

fn target_config_from_options(options: &PipelineOptions) -> TargetConfig {
    let base = if let Some(triple) = options.target_triple.as_deref() {
        TargetConfig::for_triple(triple)
    } else {
        TargetConfig::host()
    };

    let mut config = base.with_optimization(match options.optimization_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        _ => OptimizationLevel::Aggressive,
    });

    if let Some(cpu) = options.target_cpu.as_deref() {
        config = config.with_cpu(cpu);
    }
    if let Some(features) = options.target_features.as_deref() {
        config = config.with_features(features);
    }

    config
}

fn sanitize_module_identifier(id: &str) -> String {
    let mut name = String::with_capacity(id.len());
    for ch in id.chars() {
        if ch.is_ascii_alphanumeric() {
            name.push(ch);
        } else {
            name.push('_');
        }
    }
    if name.is_empty() {
        name.push_str("module");
    }
    name
}
