use crate::CliError;
use crate::codegen::CodeGenerator;
use crate::config::{PipelineConfig, PipelineOptions, PipelineTarget};
use crate::frontend::{
    FrontendRegistry, FrontendResult, FrontendSnapshot, LanguageFrontend, RustFrontend,
};
use crate::languages;
use crate::languages::detect_source_language;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{AstSerializer, Node, NodeKind, RuntimeValue, Value};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{
    Diagnostic, DiagnosticDisplayOptions, DiagnosticManager, DiagnosticReport,
};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::{hir, lir, mir};
use fp_llvm::{LlvmCompiler, LlvmConfig, linking::LinkerConfig};
use fp_optimize::ConstEvaluationOrchestrator;
use fp_optimize::orchestrators::const_evaluation::ConstEvalOutcome;
use fp_optimize::transformations::{HirGenerator, IrTransform, LirGenerator, MirLowering};
use fp_optimize::typing::TypingDiagnosticLevel;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info_span, warn};

const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_BACKEND_LOWERING: &str = "hir→mir→lir";
const STAGE_INTERPRET: &str = "interpret";

const EXT_AST: &str = "ast";
const EXT_AST_TYPED: &str = "ast-typed";
const EXT_AST_EVAL: &str = "ast-eval";
const EXT_HIR: &str = "hir";

#[derive(Debug)]
pub enum PipelineInput {
    Expression(String),
    File(PathBuf),
}

#[derive(Debug)]
pub enum PipelineOutput {
    Value(Value),
    RuntimeValue(RuntimeValue),
    Code(String),
}

struct TypeEnrichmentArtifacts {
    typed_ast: Node,
}

struct CompilationArtifacts {
    llvm_ir: PathBuf,
    diagnostics: Vec<Diagnostic>,
}

struct ConstEvalArtifacts {
    typed_ast: Node,
    hir_program: hir::Program,
    outcome: ConstEvalOutcome,
}

struct BackendArtifacts {
    mir_program: mir::Program,
    lir_program: lir::LirProgram,
    mir_text: String,
    lir_text: String,
}

struct LlvmArtifacts {
    ir_text: String,
    _ir_path: PathBuf,
}

pub struct Pipeline {
    frontends: Arc<FrontendRegistry>,
    default_runtime: String,
    serializer: Option<Arc<dyn AstSerializer>>,
    source_language: Option<String>,
    frontend_snapshot: Option<FrontendSnapshot>,
    last_const_eval: Option<ConstEvalOutcome>,
}

impl Pipeline {
    pub fn new() -> Self {
        let mut registry = FrontendRegistry::new();
        let rust_frontend: Arc<dyn LanguageFrontend> = Arc::new(RustFrontend::new());
        registry.register(rust_frontend);

        Self {
            frontends: Arc::new(registry),
            default_runtime: "literal".to_string(),
            serializer: None,
            source_language: None,
            frontend_snapshot: None,
            last_const_eval: None,
        }
    }

    pub fn with_runtime(runtime_name: &str) -> Self {
        let mut pipeline = Self::new();
        pipeline.default_runtime = runtime_name.to_string();
        pipeline
    }

    pub fn with_frontend_registry(registry: Arc<FrontendRegistry>) -> Self {
        Self {
            frontends: registry,
            default_runtime: "literal".to_string(),
            serializer: None,
            source_language: None,
            frontend_snapshot: None,
            last_const_eval: None,
        }
    }

    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.default_runtime = runtime_name.to_string();
    }

    pub fn last_const_eval_outcome(&self) -> Option<&ConstEvalOutcome> {
        self.last_const_eval.as_ref()
    }

    pub fn take_last_const_eval_stdout(&mut self) -> Option<Vec<String>> {
        self.last_const_eval
            .as_mut()
            .map(|outcome| std::mem::take(&mut outcome.stdout))
    }

    pub async fn execute(
        &mut self,
        input: PipelineInput,
        config: &PipelineConfig,
    ) -> Result<PipelineOutput, CliError> {
        let options: PipelineOptions = config.into();
        self.execute_with_options(input, options).await
    }

    pub fn parse_source_public(&mut self, source: &str) -> Result<Node, CliError> {
        let frontend = self
            .frontends
            .get(languages::FERROPHASE)
            .ok_or_else(|| CliError::Compilation("Default frontend not registered".to_string()))?;

        let FrontendResult {
            ast,
            serializer,
            snapshot,
            ..
        } = frontend
            .parse(source, None)
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.frontend_snapshot = snapshot;
        self.source_language = Some(frontend.language().to_string());

        Ok(ast)
    }

    pub async fn execute_with_options(
        &mut self,
        input: PipelineInput,
        mut options: PipelineOptions,
    ) -> Result<PipelineOutput, CliError> {
        let read_span = info_span!("pipeline.read_input");
        let _enter_read = read_span.enter();
        let (source, base_path, input_path) = match input {
            PipelineInput::Expression(expr) => (expr, PathBuf::from("expression"), None),
            PipelineInput::File(path) => {
                let source = std::fs::read_to_string(&path).map_err(|e| {
                    CliError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to read file {}: {}", path.display(), e),
                    ))
                })?;
                let base_path = path.with_extension("");
                (source, base_path, Some(path))
            }
        };
        drop(_enter_read);
        debug!(path = ?input_path, "loaded input source");

        self.serializer = None;
        self.source_language = None;
        self.frontend_snapshot = None;
        self.last_const_eval = None;

        options.base_path = Some(base_path.clone());

        let language = options
            .source_language
            .clone()
            .or_else(|| {
                input_path
                    .as_ref()
                    .and_then(|path| detect_source_language(path).map(|lang| lang.name.to_string()))
            })
            .unwrap_or_else(|| languages::FERROPHASE.to_string());

        self.source_language = Some(language.clone());

        let frontend = self.frontends.get(&language).ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        let parse_span = info_span!("pipeline.frontend", language = %language);
        let _enter_parse = parse_span.enter();
        let FrontendResult {
            last: _last,
            ast: ast_node,
            serializer,
            snapshot,
        } = frontend.parse(&source, input_path.as_deref())?;
        drop(_enter_parse);

        register_threadlocal_serializer(serializer.clone());

        self.serializer = Some(serializer.clone());
        self.frontend_snapshot = snapshot;

        match options.target {
            PipelineTarget::Rust => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    CliError::Compilation("Missing base path for transpilation".to_string())
                })?;

                let diagnostic_manager = DiagnosticManager::new();

                let type_report = self.run_type_enrichment_stage(
                    &ast_node,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let TypeEnrichmentArtifacts { typed_ast } = self.collect_stage(
                    STAGE_TYPE_ENRICH,
                    type_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let const_report = self.run_const_eval_stage(
                    typed_ast,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let ConstEvalArtifacts {
                    typed_ast: evaluated_typed_ast,
                    hir_program: _hir_program,
                    outcome,
                } = self.collect_stage(
                    STAGE_CONST_EVAL,
                    const_report,
                    &diagnostic_manager,
                    &options,
                )?;
                self.last_const_eval = Some(outcome.clone());

                if options.execute_main {
                    let diagnostics = diagnostic_manager.get_diagnostics();
                    self.emit_diagnostics(&diagnostics, None, &options);
                    return Ok(PipelineOutput::Value(Value::unit()));
                }

                let rust_span = info_span!("pipeline.codegen", target = "rust");
                let _enter_rust = rust_span.enter();
                let rust_code = CodeGenerator::generate_rust_code(&evaluated_typed_ast)?;
                drop(_enter_rust);

                let diagnostics = diagnostic_manager.get_diagnostics();
                self.emit_diagnostics(&diagnostics, None, &options);

                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Interpret => {
                let runtime = if options.runtime.runtime_type.is_empty() {
                    self.default_runtime.clone()
                } else {
                    options.runtime.runtime_type.clone()
                };

                match runtime.as_str() {
                    "literal" => {
                        let interpret_span = info_span!("pipeline.interpret", runtime = "literal");
                        let _enter_interp = interpret_span.enter();
                        let result = self
                            .interpret_ast(&ast_node, &options, input_path.as_deref())
                            .await?;
                        drop(_enter_interp);
                        Ok(PipelineOutput::Value(result))
                    }
                    _ => {
                        let interpret_span = info_span!(
                            "pipeline.interpret",
                            runtime = %runtime
                        );
                        let _enter_interp = interpret_span.enter();
                        let result = self
                            .interpret_ast_runtime(
                                &ast_node,
                                &runtime,
                                &options,
                                input_path.as_deref(),
                            )
                            .await?;
                        drop(_enter_interp);
                        Ok(PipelineOutput::RuntimeValue(result))
                    }
                }
            }
            PipelineTarget::Llvm => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    CliError::Compilation("Missing base path for LLVM generation".to_string())
                })?;

                let diagnostic_manager = DiagnosticManager::new();

                let type_report = self.run_type_enrichment_stage(
                    &ast_node,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let TypeEnrichmentArtifacts { typed_ast } = self.collect_stage(
                    STAGE_TYPE_ENRICH,
                    type_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let const_report = self.run_const_eval_stage(
                    typed_ast,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let ConstEvalArtifacts {
                    typed_ast: _evaluated_typed_ast,
                    hir_program,
                    outcome,
                } = self.collect_stage(
                    STAGE_CONST_EVAL,
                    const_report,
                    &diagnostic_manager,
                    &options,
                )?;
                self.last_const_eval = Some(outcome.clone());

                if options.debug.verbose {
                    debug!(
                        mutations = outcome.mutations_applied,
                        consts = outcome.evaluated_constants.len(),
                        "const evaluation completed"
                    );
                }

                let backend_report =
                    self.run_backend_lowering_stage(&hir_program, &options, base_path)?;
                let BackendArtifacts {
                    mir_program: _mir_program,
                    lir_program,
                    mir_text: _mir_text,
                    lir_text: _lir_text,
                } = self.collect_stage(
                    STAGE_BACKEND_LOWERING,
                    backend_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let llvm_artifacts = self.generate_llvm_artifacts(
                    &lir_program,
                    base_path,
                    input_path.as_deref(),
                    &options,
                )?;

                let diagnostics = diagnostic_manager.get_diagnostics();
                self.emit_diagnostics(&diagnostics, None, &options);

                Ok(PipelineOutput::Code(llvm_artifacts.ir_text))
            }
            PipelineTarget::Binary => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    CliError::Compilation("Missing base path for binary generation".to_string())
                })?;

                let diagnostic_manager = DiagnosticManager::new();

                let type_report = self.run_type_enrichment_stage(
                    &ast_node,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let TypeEnrichmentArtifacts { typed_ast } = self.collect_stage(
                    STAGE_TYPE_ENRICH,
                    type_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let const_report = self.run_const_eval_stage(
                    typed_ast,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let ConstEvalArtifacts {
                    typed_ast: evaluated_typed_ast,
                    outcome,
                    ..
                } = self.collect_stage(
                    STAGE_CONST_EVAL,
                    const_report,
                    &diagnostic_manager,
                    &options,
                )?;
                self.last_const_eval = Some(outcome.clone());

                if options.debug.verbose {
                    debug!(
                        mutations = outcome.mutations_applied,
                        consts = outcome.evaluated_constants.len(),
                        "const evaluation completed"
                    );
                }

                let rust_code = CodeGenerator::generate_rust_code(&evaluated_typed_ast)?;

                if options.save_intermediates {
                    if let Err(err) = fs::write(base_path.with_extension("rs"), &rust_code) {
                        debug!(error = %err, "failed to persist generated Rust code");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                self.emit_diagnostics(&diagnostics, None, &options);

                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Bytecode => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    CliError::Compilation("Missing base path for bytecode generation".to_string())
                })?;

                let diagnostic_manager = DiagnosticManager::new();

                let type_report = self.run_type_enrichment_stage(
                    &ast_node,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let TypeEnrichmentArtifacts { typed_ast } = self.collect_stage(
                    STAGE_TYPE_ENRICH,
                    type_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let const_report = self.run_const_eval_stage(
                    typed_ast,
                    &options,
                    input_path.as_deref(),
                    base_path,
                )?;
                let ConstEvalArtifacts {
                    typed_ast: _evaluated_typed_ast,
                    hir_program,
                    outcome,
                } = self.collect_stage(
                    STAGE_CONST_EVAL,
                    const_report,
                    &diagnostic_manager,
                    &options,
                )?;
                self.last_const_eval = Some(outcome.clone());

                if options.debug.verbose {
                    debug!(
                        mutations = outcome.mutations_applied,
                        consts = outcome.evaluated_constants.len(),
                        "const evaluation completed"
                    );
                }

                let backend_report =
                    self.run_backend_lowering_stage(&hir_program, &options, base_path)?;
                let BackendArtifacts {
                    mir_program: _mir_program,
                    lir_program: _lir_program,
                    mir_text,
                    lir_text,
                } = self.collect_stage(
                    STAGE_BACKEND_LOWERING,
                    backend_report,
                    &diagnostic_manager,
                    &options,
                )?;

                let diagnostics = diagnostic_manager.get_diagnostics();
                self.emit_diagnostics(&diagnostics, None, &options);

                let bytecode_repr = format!("; MIR\n{}\n\n; LIR\n{}", mir_text, lir_text);

                Ok(PipelineOutput::Code(bytecode_repr))
            }
        }
    }

    fn compile(
        &self,
        ast: Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
    ) -> Result<CompilationArtifacts, CliError> {
        let base_path = options.base_path.as_ref().ok_or_else(|| {
            CliError::Compilation("Missing base path for compilation".to_string())
        })?;

        let diagnostic_manager = DiagnosticManager::new();

        let type_report = self.run_type_enrichment_stage(&ast, options, file_path, base_path)?;
        let TypeEnrichmentArtifacts { typed_ast } =
            self.collect_stage(STAGE_TYPE_ENRICH, type_report, &diagnostic_manager, options)?;

        let const_report = self.run_const_eval_stage(typed_ast, options, file_path, base_path)?;
        let ConstEvalArtifacts {
            typed_ast: _evaluated_typed,
            hir_program: _hir_program,
            outcome: _,
        } = self.collect_stage(STAGE_CONST_EVAL, const_report, &diagnostic_manager, options)?;

        let _diagnostics = diagnostic_manager.get_diagnostics();

        Err(CliError::Compilation(
            "typed backend pipeline not yet implemented".to_string(),
        ))
    }

    fn run_const_eval_stage(
        &self,
        mut typed_ast: Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<DiagnosticReport<ConstEvalArtifacts>, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut const_evaluator = ConstEvaluationOrchestrator::new(serializer.clone());
        const_evaluator.set_debug_assertions(!options.release);
        const_evaluator.set_execute_main(options.execute_main);

        let mut diagnostics = Vec::new();

        let outcome = match const_evaluator.evaluate(&mut typed_ast, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                return Ok(DiagnosticReport::failure(vec![diagnostic]));
            }
        };

        diagnostics.extend(outcome.diagnostics.iter().cloned());
        if outcome.has_errors {
            return Ok(DiagnosticReport::failure(diagnostics));
        }

        match fp_optimize::typing::annotate(&mut typed_ast) {
            Ok(post_outcome) => {
                let mut saw_error = false;
                for message in post_outcome.diagnostics {
                    match message.level {
                        TypingDiagnosticLevel::Warning => diagnostics.push(
                            Diagnostic::warning(message.message)
                                .with_source_context(STAGE_CONST_EVAL),
                        ),
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            diagnostics.push(
                                Diagnostic::error(message.message)
                                    .with_source_context(STAGE_CONST_EVAL),
                            );
                        }
                    }
                }

                if saw_error || post_outcome.has_errors {
                    return Ok(DiagnosticReport::failure(diagnostics));
                }
            }
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("AST typing failed after const evaluation: {}", err))
                        .with_source_context(STAGE_CONST_EVAL),
                );
                return Ok(DiagnosticReport::failure(diagnostics));
            }
        }

        if let Err(err) = fp_optimize::materialize_runtime_intrinsics(&mut typed_ast) {
            diagnostics.push(
                Diagnostic::error(format!("Failed to materialize runtime intrinsics: {}", err))
                    .with_source_context(STAGE_CONST_EVAL),
            );
            return Ok(DiagnosticReport::failure(diagnostics));
        }

        match fp_optimize::typing::annotate(&mut typed_ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    match message.level {
                        TypingDiagnosticLevel::Warning => diagnostics.push(
                            Diagnostic::warning(message.message)
                                .with_source_context(STAGE_CONST_EVAL),
                        ),
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            diagnostics.push(
                                Diagnostic::error(message.message)
                                    .with_source_context(STAGE_CONST_EVAL),
                            );
                        }
                    }
                }

                if saw_error || outcome.has_errors {
                    return Ok(DiagnosticReport::failure(diagnostics));
                }
            }
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!(
                        "AST typing failed after intrinsic materialization: {}",
                        err
                    ))
                    .with_source_context(STAGE_CONST_EVAL),
                );
                return Ok(DiagnosticReport::failure(diagnostics));
            }
        }

        let hir_report = self.run_hir_stage(&typed_ast, options, file_path, base_path)?;
        let mut hir_diagnostics = hir_report.diagnostics;
        let hir_program = match hir_report.value {
            Some(program) => program,
            None => {
                diagnostics.append(&mut hir_diagnostics);
                return Ok(DiagnosticReport::failure(diagnostics));
            }
        };
        diagnostics.append(&mut hir_diagnostics);

        if options.save_intermediates {
            let mut ast_opts = PrettyOptions::default();
            ast_opts.show_spans = options.debug.verbose;
            let rendered_ast = format!("{}", pretty(&typed_ast, ast_opts));
            if let Err(err) = fs::write(base_path.with_extension(EXT_AST_EVAL), rendered_ast) {
                debug!(
                    error = %err,
                    "failed to persist evaluated AST (ast-eval) intermediate after const eval"
                );
            }
        }

        Ok(DiagnosticReport::success_with_diagnostics(
            ConstEvalArtifacts {
                typed_ast,
                hir_program,
                outcome,
            },
            diagnostics,
        ))
    }

    fn run_backend_lowering_stage(
        &self,
        hir_program: &hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<BackendArtifacts>, CliError> {
        let mut diagnostics = Vec::new();

        let mut mir_lowering = MirLowering::new();
        let mir_program = mir_lowering
            .transform(hir_program)
            .map_err(|err| CliError::Compilation(format!("HIR→MIR lowering failed: {}", err)))?;
        let (mut mir_diags, mir_had_errors) = mir_lowering.take_diagnostics();
        diagnostics.append(&mut mir_diags);
        if mir_had_errors {
            return Ok(DiagnosticReport::failure(diagnostics));
        }

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

        Ok(DiagnosticReport::success_with_diagnostics(
            BackendArtifacts {
                mir_program,
                lir_program,
                mir_text,
                lir_text,
            },
            diagnostics,
        ))
    }

    fn generate_llvm_artifacts(
        &self,
        lir_program: &lir::LirProgram,
        base_path: &Path,
        source_path: Option<&Path>,
        options: &PipelineOptions,
    ) -> Result<LlvmArtifacts, CliError> {
        let llvm_path = base_path.with_extension("ll");
        let config = LlvmConfig::new().with_linker(LinkerConfig::executable(&llvm_path));
        let compiler = LlvmCompiler::new(config);

        compiler
            .compile(lir_program.clone(), source_path)
            .map_err(|err| CliError::Compilation(format!("LIR→LLVM lowering failed: {}", err)))?;

        let ir_text = fs::read_to_string(&llvm_path)?;

        if !options.save_intermediates {
            if let Err(err) = fs::remove_file(&llvm_path) {
                debug!(error = %err, "failed to remove temporary LLVM IR file");
            }
        }

        Ok(LlvmArtifacts {
            ir_text,
            _ir_path: llvm_path,
        })
    }

    fn run_type_enrichment_stage(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<DiagnosticReport<TypeEnrichmentArtifacts>, CliError> {
        let mut diagnostics = Vec::new();

        let _ = file_path;

        let mut typed_ast = ast.clone();

        if let Err(err) = fp_optimize::normalize_intrinsics(&mut typed_ast) {
            diagnostics.push(
                Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                    .with_source_context(STAGE_TYPE_ENRICH),
            );
            return Ok(DiagnosticReport::failure(diagnostics));
        }

        match fp_optimize::typing::annotate(&mut typed_ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    match message.level {
                        TypingDiagnosticLevel::Warning => diagnostics.push(
                            Diagnostic::warning(message.message)
                                .with_source_context(STAGE_TYPE_ENRICH),
                        ),
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            diagnostics.push(
                                Diagnostic::error(message.message)
                                    .with_source_context(STAGE_TYPE_ENRICH),
                            );
                        }
                    }
                }

                if saw_error || outcome.has_errors {
                    return Ok(DiagnosticReport::failure(diagnostics));
                }
            }
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("AST typing failed: {}", err))
                        .with_source_context(STAGE_TYPE_ENRICH),
                );
                return Ok(DiagnosticReport::failure(diagnostics));
            }
        }

        if options.save_intermediates {
            let mut pretty_opts = PrettyOptions::default();
            pretty_opts.show_spans = options.debug.verbose;

            let rendered_ast = format!("{}", pretty(ast, pretty_opts.clone()));
            if let Err(err) = fs::write(base_path.with_extension(EXT_AST), rendered_ast) {
                debug!(
                    error = %err,
                    "failed to persist AST intermediate"
                );
            }

            let rendered_typed = format!("{}", pretty(&typed_ast, pretty_opts));
            if let Err(err) = fs::write(base_path.with_extension(EXT_AST_TYPED), rendered_typed) {
                debug!(
                    error = %err,
                    "failed to persist typed AST intermediate"
                );
            }
        }

        Ok(DiagnosticReport::success_with_diagnostics(
            TypeEnrichmentArtifacts { typed_ast },
            diagnostics,
        ))
    }

    fn run_hir_stage(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<DiagnosticReport<hir::Program>, CliError> {
        let mut generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if options.error_tolerance.enabled {
            generator.enable_error_tolerance(options.error_tolerance.max_errors);
        }

        if matches!(ast.kind(), NodeKind::Item(_)) {
            let diag = Diagnostic::error(
                "Top-level items are not supported; provide a file or expression".to_string(),
            )
            .with_source_context(STAGE_AST_TO_HIR);
            return Ok(DiagnosticReport::failure(vec![diag]));
        }

        let result = match ast.kind() {
            NodeKind::Expr(expr) => generator.transform(expr),
            NodeKind::File(file) => generator.transform(file),
            NodeKind::Item(_) => unreachable!(),
        };

        let (errors, warnings) = generator.take_diagnostics();
        let diagnostic_manager = DiagnosticManager::new();

        diagnostic_manager.add_diagnostics(warnings.clone());

        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("AST→HIR transformation failed: {}", e))
                    .with_source_context(STAGE_AST_TO_HIR),
            );
        }

        if !errors.is_empty() {
            diagnostic_manager.add_diagnostics(errors.clone());
        }

        match result {
            Ok(program) if errors.is_empty() => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_HIR), rendered) {
                        warn!(error = %err, "failed to persist HIR intermediate");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::success_with_diagnostics(
                    program,
                    diagnostics,
                ))
            }
            _ => {
                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::failure(diagnostics))
            }
        }
    }

    async fn interpret_ast(
        &self,
        _ast: &Node,
        _options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<Value, CliError> {
        Err(CliError::Compilation(
            "AST interpreter not yet ported".to_string(),
        ))
    }

    async fn interpret_ast_runtime(
        &self,
        _ast: &Node,
        _runtime_name: &str,
        _options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<RuntimeValue, CliError> {
        Err(CliError::Compilation(
            "AST runtime interpreter not yet ported".to_string(),
        ))
    }

    fn emit_diagnostics(
        &self,
        diagnostics: &[Diagnostic],
        stage_context: Option<&str>,
        options: &PipelineOptions,
    ) {
        if diagnostics.is_empty() {
            return;
        }

        let display_options = self.diagnostic_display_options(options);
        DiagnosticManager::emit(diagnostics, stage_context, &display_options);
    }

    fn collect_stage<T>(
        &self,
        stage: &'static str,
        report: DiagnosticReport<T>,
        manager: &DiagnosticManager,
        options: &PipelineOptions,
    ) -> Result<T, CliError> {
        let DiagnosticReport { value, diagnostics } = report;
        self.emit_diagnostics(&diagnostics, Some(stage), options);
        manager.add_diagnostics(diagnostics.clone());

        value.ok_or_else(|| {
            CliError::Compilation(format!(
                "{} stage failed; see diagnostics for details",
                stage
            ))
        })
    }

    fn diagnostic_display_options(&self, options: &PipelineOptions) -> DiagnosticDisplayOptions {
        DiagnosticDisplayOptions::new(options.debug.verbose)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_frontend_parses_expression() {
        let mut pipeline = Pipeline::new();
        let expr = "1 + 2";
        assert!(pipeline.parse_source_public(expr).is_ok());
    }
}
