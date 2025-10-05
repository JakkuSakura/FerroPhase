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
    Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager, DiagnosticReport,
};
use fp_core::intrinsics::runtime::RuntimeIntrinsicStrategy;
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::{hir, lir};
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions, InterpreterOutcome};
use fp_llvm::{
    LlvmCompiler, LlvmConfig, linking::LinkerConfig, runtime::LlvmRuntimeIntrinsicStrategy,
};
use fp_optimize::ConstEvaluationOrchestrator;
use fp_optimize::orchestrators::const_evaluation::ConstEvalOutcome;
use fp_optimize::passes::materialize_intrinsics::NoopIntrinsicStrategy;
use fp_optimize::transformations::{HirGenerator, IrTransform, LirGenerator, MirLowering};
use fp_optimize::typing::TypingDiagnosticLevel;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use tracing::{debug, info_span, warn};

const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_BACKEND_LOWERING: &str = "hir→mir→lir";
const STAGE_TYPE_POST_CONST: &str = "ast→typed(post-const)";
const STAGE_SPECIALIZE: &str = "specialize";
const STAGE_RUNTIME_MATERIALIZE: &str = "materialize-runtime";
const STAGE_TYPE_POST_SPECIALIZE: &str = "ast→typed(post-specialize)";
const STAGE_TYPE_POST_MATERIALIZE: &str = "ast→typed(post-materialize)";
const STAGE_TYPE_POST_CLOSURE: &str = "ast→typed(post-closure)";
const STAGE_CLOSURE_LOWERING: &str = "closure-lowering";
const STAGE_LINK_BINARY: &str = "link-binary";
const STAGE_INTRINSIC_NORMALIZE: &str = "intrinsic-normalize";
const STAGE_AST_INTERPRET: &str = "ast-interpret";

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
    Binary(PathBuf),
}

struct BackendArtifacts {
    lir_program: lir::LirProgram,
    mir_text: String,
    lir_text: String,
}

struct LlvmArtifacts {
    ir_text: String,
    ir_path: PathBuf,
}

struct IntrinsicsMaterializer {
    strategy: Box<dyn RuntimeIntrinsicStrategy>,
}

impl IntrinsicsMaterializer {
    fn for_target(target: &PipelineTarget) -> Self {
        match target {
            PipelineTarget::Llvm | PipelineTarget::Binary => Self {
                strategy: Box::new(LlvmRuntimeIntrinsicStrategy),
            },
            _ => Self {
                strategy: Box::new(NoopIntrinsicStrategy),
            },
        }
    }

    fn materialize(&self, ast: &mut Node) -> fp_core::error::Result<()> {
        fp_optimize::materialize_runtime_intrinsics(ast, self.strategy.as_ref())
    }
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
        let (source, base_path, input_path) = self.read_input(input)?;
        options.base_path = Some(base_path.clone());

        self.reset_state();

        let language = self.resolve_language(&options, input_path.as_ref());
        let frontend = self.frontends.get(&language).ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        let ast = self.parse_with_frontend(&frontend, &source, input_path.as_deref())?;

        match options.target {
            PipelineTarget::Interpret => {
                self.execute_interpret_target(ast, &options, input_path.as_deref())
                    .await
            }
            ref target => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    let msg = match target {
                        PipelineTarget::Rust => "Missing base path for transpilation",
                        PipelineTarget::Llvm => "Missing base path for LLVM generation",
                        PipelineTarget::Binary => "Missing base path for binary generation",
                        PipelineTarget::Bytecode => "Missing base path for bytecode generation",
                        PipelineTarget::Interpret => unreachable!(),
                    };
                    CliError::Compilation(msg.to_string())
                })?;

                self.execute_compilation_target(
                    target,
                    ast,
                    &options,
                    base_path,
                    input_path.as_deref(),
                )
            }
        }
    }

    fn read_input(
        &self,
        input: PipelineInput,
    ) -> Result<(String, PathBuf, Option<PathBuf>), CliError> {
        let span = info_span!("pipeline.read_input");
        let _enter = span.enter();

        let (source, base_path, path) = match input {
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

        debug!(path = ?path, "loaded input source");
        Ok((source, base_path, path))
    }

    fn reset_state(&mut self) {
        self.serializer = None;
        self.source_language = None;
        self.frontend_snapshot = None;
        self.last_const_eval = None;
    }

    fn resolve_language(
        &mut self,
        options: &PipelineOptions,
        input_path: Option<&PathBuf>,
    ) -> String {
        let language = options
            .source_language
            .clone()
            .or_else(|| {
                input_path
                    .and_then(|path| detect_source_language(path).map(|lang| lang.name.to_string()))
            })
            .unwrap_or_else(|| languages::FERROPHASE.to_string());

        self.source_language = Some(language.clone());
        language
    }

    fn parse_with_frontend(
        &mut self,
        frontend: &Arc<dyn LanguageFrontend>,
        source: &str,
        input_path: Option<&Path>,
    ) -> Result<Node, CliError> {
        let span = info_span!("pipeline.frontend", language = %frontend.language());
        let _enter = span.enter();

        let FrontendResult {
            last: _last,
            ast,
            serializer,
            snapshot,
            diagnostics,
        } = frontend.parse(source, input_path)?;

        if diagnostics.has_errors() {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.frontend_snapshot = snapshot;

        Ok(ast)
    }

    async fn execute_interpret_target(
        &mut self,
        ast: Node,
        options: &PipelineOptions,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        let runtime = if options.runtime.runtime_type.is_empty() {
            self.default_runtime.clone()
        } else {
            options.runtime.runtime_type.clone()
        };

        match runtime.as_str() {
            "literal" => {
                let span = info_span!("pipeline.interpret", runtime = "literal");
                let _enter = span.enter();
                let value = self.interpret_ast(&ast, options, input_path).await?;
                Ok(PipelineOutput::Value(value))
            }
            _ => {
                let span = info_span!("pipeline.interpret", runtime = %runtime);
                let _enter = span.enter();
                let value = self
                    .interpret_ast_runtime(&ast, &runtime, options, input_path)
                    .await?;
                Ok(PipelineOutput::RuntimeValue(value))
            }
        }
    }

    fn execute_compilation_target(
        &mut self,
        target: &PipelineTarget,
        mut ast: Node,
        options: &PipelineOptions,
        base_path: &Path,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        let diagnostic_manager = DiagnosticManager::new();

        let normalize_report = self.stage_normalize_intrinsics(&mut ast);
        self.collect_stage(
            STAGE_INTRINSIC_NORMALIZE,
            normalize_report,
            &diagnostic_manager,
            options,
        )?;

        let pre_const_type_report = self.stage_type_check(&mut ast, STAGE_TYPE_ENRICH);
        self.collect_stage(
            STAGE_TYPE_ENRICH,
            pre_const_type_report,
            &diagnostic_manager,
            options,
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST, options)?;
            self.save_pretty(&ast, base_path, EXT_AST_TYPED, options)?;
        }

        let closure_report = self.stage_closure_lowering(&mut ast);
        self.collect_stage(
            STAGE_CLOSURE_LOWERING,
            closure_report,
            &diagnostic_manager,
            options,
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, "ast-closure", options)?;
        }

        let post_closure_type_report = self.stage_type_check(&mut ast, STAGE_TYPE_POST_CLOSURE);
        self.collect_stage(
            STAGE_TYPE_POST_CLOSURE,
            post_closure_type_report,
            &diagnostic_manager,
            options,
        )?;

        let const_report = self.stage_const_eval(&mut ast, options)?;
        let outcome =
            self.collect_stage(STAGE_CONST_EVAL, const_report, &diagnostic_manager, options)?;
        self.last_const_eval = Some(outcome.clone());

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST_EVAL, options)?;
        }

        let post_const_type_report = self.stage_type_check(&mut ast, STAGE_TYPE_POST_CONST);
        self.collect_stage(
            STAGE_TYPE_POST_CONST,
            post_const_type_report,
            &diagnostic_manager,
            options,
        )?;

        let specialize_report = self.stage_specialize(&mut ast);
        self.collect_stage(
            STAGE_SPECIALIZE,
            specialize_report,
            &diagnostic_manager,
            options,
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, "ast-specialized", options)?;
        }

        let post_specialize_type_report =
            self.stage_type_check(&mut ast, STAGE_TYPE_POST_SPECIALIZE);
        self.collect_stage(
            STAGE_TYPE_POST_SPECIALIZE,
            post_specialize_type_report,
            &diagnostic_manager,
            options,
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, "ast-specialized-typed", options)?;
        }

        let materialize_report = self.stage_materialize_runtime_intrinsics(&mut ast, target);
        self.collect_stage(
            STAGE_RUNTIME_MATERIALIZE,
            materialize_report,
            &diagnostic_manager,
            options,
        )?;

        let post_materialize_type_report =
            self.stage_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        self.collect_stage(
            STAGE_TYPE_POST_MATERIALIZE,
            post_materialize_type_report,
            &diagnostic_manager,
            options,
        )?;

        let output = if matches!(target, PipelineTarget::Rust) {
            let span = info_span!("pipeline.codegen", target = "rust");
            let _enter = span.enter();
            let code = CodeGenerator::generate_rust_code(&ast)?;
            PipelineOutput::Code(code)
        } else {
            let hir_report = self.stage_hir_generation(&ast, options, input_path, base_path)?;
            let hir_program =
                self.collect_stage(STAGE_AST_TO_HIR, hir_report, &diagnostic_manager, options)?;

            match target {
                PipelineTarget::Llvm => {
                    let backend_report =
                        self.stage_backend_lowering(&hir_program.clone(), options, base_path)?;
                    let backend = self.collect_stage(
                        STAGE_BACKEND_LOWERING,
                        backend_report,
                        &diagnostic_manager,
                        options,
                    )?;

                    let llvm = self.generate_llvm_artifacts(
                        &backend.lir_program,
                        base_path,
                        input_path,
                        false,
                        options,
                    )?;
                    PipelineOutput::Code(llvm.ir_text)
                }
                PipelineTarget::Binary => {
                    let backend_report =
                        self.stage_backend_lowering(&hir_program.clone(), options, base_path)?;
                    let backend = self.collect_stage(
                        STAGE_BACKEND_LOWERING,
                        backend_report,
                        &diagnostic_manager,
                        options,
                    )?;

                    let llvm = self.generate_llvm_artifacts(
                        &backend.lir_program,
                        base_path,
                        input_path,
                        true,
                        options,
                    )?;

                    let link_report = self.stage_link_binary(&llvm.ir_path, base_path, options);
                    let binary_path = self.collect_stage(
                        STAGE_LINK_BINARY,
                        link_report,
                        &diagnostic_manager,
                        options,
                    )?;

                    PipelineOutput::Binary(binary_path)
                }
                PipelineTarget::Bytecode => {
                    let backend_report =
                        self.stage_backend_lowering(&hir_program, options, base_path)?;
                    let backend = self.collect_stage(
                        STAGE_BACKEND_LOWERING,
                        backend_report,
                        &diagnostic_manager,
                        options,
                    )?;

                    let repr =
                        format!("; MIR\n{}\n\n; LIR\n{}", backend.mir_text, backend.lir_text);
                    PipelineOutput::Code(repr)
                }
                PipelineTarget::Rust | PipelineTarget::Interpret => unreachable!(),
            }
        };

        let diagnostics = diagnostic_manager.get_diagnostics();
        self.emit_diagnostics(&diagnostics, None, options);

        Ok(output)
    }

    fn stage_normalize_intrinsics(&self, ast: &mut Node) -> DiagnosticReport<()> {
        match fp_optimize::normalize_intrinsics(ast) {
            Ok(()) => DiagnosticReport::success_with_diagnostics((), Vec::new()),
            Err(err) => DiagnosticReport::failure(vec![
                Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                    .with_source_context(STAGE_INTRINSIC_NORMALIZE),
            ]),
        }
    }

    fn stage_type_check(&self, ast: &mut Node, stage_label: &'static str) -> DiagnosticReport<()> {
        let mut diagnostics = Vec::new();

        match fp_optimize::typing::annotate(ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    match message.level {
                        TypingDiagnosticLevel::Warning => diagnostics.push(
                            Diagnostic::warning(message.message).with_source_context(stage_label),
                        ),
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            diagnostics.push(
                                Diagnostic::error(message.message).with_source_context(stage_label),
                            );
                        }
                    }
                }

                if saw_error || outcome.has_errors {
                    return DiagnosticReport::failure(diagnostics);
                }
            }
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("AST typing failed: {}", err))
                        .with_source_context(stage_label),
                );
                return DiagnosticReport::failure(diagnostics);
            }
        }

        DiagnosticReport::success_with_diagnostics((), diagnostics)
    }

    fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<DiagnosticReport<ConstEvalOutcome>, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!options.release);
        orchestrator.set_execute_main(options.execute_main);

        let mut diagnostics = Vec::new();

        let outcome = match orchestrator.evaluate(ast, &shared_context) {
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

        Ok(DiagnosticReport::success_with_diagnostics(
            outcome,
            diagnostics,
        ))
    }

    fn stage_closure_lowering(&self, ast: &mut Node) -> DiagnosticReport<()> {
        match fp_optimize::lower_closures(ast) {
            Ok(()) => DiagnosticReport::success_with_diagnostics((), Vec::new()),
            Err(err) => DiagnosticReport::failure(vec![
                Diagnostic::error(format!("Closure lowering failed: {}", err))
                    .with_source_context(STAGE_CLOSURE_LOWERING),
            ]),
        }
    }

    fn stage_materialize_runtime_intrinsics(
        &self,
        ast: &mut Node,
        target: &PipelineTarget,
    ) -> DiagnosticReport<()> {
        let materializer = IntrinsicsMaterializer::for_target(target);
        let result = materializer.materialize(ast);

        match result {
            Ok(()) => DiagnosticReport::success_with_diagnostics((), Vec::new()),
            Err(err) => DiagnosticReport::failure(vec![
                Diagnostic::error(format!("Failed to materialize runtime intrinsics: {}", err))
                    .with_source_context(STAGE_RUNTIME_MATERIALIZE),
            ]),
        }
    }

    fn stage_specialize(&self, ast: &mut Node) -> DiagnosticReport<()> {
        match fp_optimize::specialize(ast) {
            Ok(()) => DiagnosticReport::success_with_diagnostics((), Vec::new()),
            Err(err) => DiagnosticReport::failure(vec![
                Diagnostic::error(format!("Specialization failed: {}", err))
                    .with_source_context(STAGE_SPECIALIZE),
            ]),
        }
    }

    fn stage_link_binary(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> DiagnosticReport<PathBuf> {
        let binary_path = base_path.with_extension(if cfg!(target_os = "windows") {
            "exe"
        } else {
            "out"
        });

        if let Some(parent) = binary_path.parent() {
            if let Err(err) = fs::create_dir_all(parent) {
                return DiagnosticReport::failure(vec![
                    Diagnostic::error(format!("Failed to create output directory: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                ]);
            }
        }

        let clang_available = Command::new("clang").arg("--version").output();
        if matches!(clang_available, Err(_)) {
            return DiagnosticReport::failure(vec![
                Diagnostic::error(
                    "`clang` not found in PATH; install LLVM toolchain to produce binaries"
                        .to_string(),
                )
                .with_source_context(STAGE_LINK_BINARY),
            ]);
        }

        let mut cmd = Command::new("clang");
        cmd.arg(llvm_ir_path).arg("-o").arg(&binary_path);
        if options.release {
            cmd.arg("-O2");
        }

        let output = match cmd.output() {
            Ok(output) => output,
            Err(err) => {
                return DiagnosticReport::failure(vec![
                    Diagnostic::error(format!("Failed to invoke clang: {}", err))
                        .with_source_context(STAGE_LINK_BINARY),
                ]);
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
            return DiagnosticReport::failure(vec![
                Diagnostic::error(format!("clang failed: {}", message))
                    .with_source_context(STAGE_LINK_BINARY),
            ]);
        }

        if !options.save_intermediates {
            if let Err(err) = fs::remove_file(llvm_ir_path) {
                debug!(
                    error = %err,
                    path = %llvm_ir_path.display(),
                    "failed to remove intermediate LLVM IR file after linking"
                );
            }
        }

        DiagnosticReport::success_with_diagnostics(binary_path, Vec::new())
    }

    fn stage_backend_lowering(
        &self,
        hir_program: &hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<BackendArtifacts>, CliError> {
        let mut diagnostics = Vec::new();

        let mut mir_lowering = MirLowering::new();
        let mir_program = mir_lowering
            .transform(hir_program.clone())
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
                lir_program,
                mir_text,
                lir_text,
            },
            diagnostics,
        ))
    }

    fn save_pretty(
        &self,
        ast: &Node,
        base_path: &Path,
        extension: &str,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        if !options.save_intermediates {
            return Ok(());
        }

        let mut pretty_opts = PrettyOptions::default();
        pretty_opts.show_spans = options.debug.verbose;
        let rendered = format!("{}", pretty(ast, pretty_opts));
        if let Err(err) = fs::write(base_path.with_extension(extension), rendered) {
            debug!(
                error = %err,
                extension = extension,
                "failed to persist {} intermediate",
                extension
            );
        }

        Ok(())
    }

    fn generate_llvm_artifacts(
        &self,
        lir_program: &lir::LirProgram,
        base_path: &Path,
        source_path: Option<&Path>,
        retain_file: bool,
        options: &PipelineOptions,
    ) -> Result<LlvmArtifacts, CliError> {
        let llvm_path = base_path.with_extension("ll");
        let config = LlvmConfig::new().with_linker(LinkerConfig::executable(&llvm_path));
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

    fn stage_hir_generation(
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

    fn run_ast_interpreter(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        mode: InterpreterMode,
    ) -> Result<(Value, InterpreterOutcome), CliError> {
        let mut working_ast = ast.clone();
        let ctx = SharedScopedContext::new();
        let interpreter_opts = InterpreterOptions {
            mode,
            debug_assertions: !options.release,
            diagnostics: None,
            diagnostic_context: STAGE_AST_INTERPRET,
        };
        let mut interpreter = AstInterpreter::new(&ctx, interpreter_opts);

        let value = match working_ast.kind_mut() {
            NodeKind::File(_) => {
                interpreter.interpret(&mut working_ast);
                interpreter.execute_main().unwrap_or_else(Value::unit)
            }
            NodeKind::Expr(expr) => interpreter.evaluate_expression(expr),
            NodeKind::Item(_) => {
                return Err(CliError::Compilation(
                    "Standalone item interpretation is not supported".to_string(),
                ));
            }
        };

        let outcome = interpreter.take_outcome();
        self.emit_diagnostics(&outcome.diagnostics, Some(STAGE_AST_INTERPRET), options);

        if outcome.has_errors {
            return Err(CliError::Compilation(
                "AST interpretation failed; see diagnostics for details".to_string(),
            ));
        }

        Ok((value, outcome))
    }

    async fn interpret_ast(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<Value, CliError> {
        let (value, mut outcome) =
            self.run_ast_interpreter(ast, options, InterpreterMode::RunTime)?;

        if !outcome.stdout.is_empty() {
            for chunk in outcome.stdout.drain(..) {
                print!("{}", chunk);
            }
            let _ = io::stdout().flush();
        }

        Ok(value)
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &Node,
        runtime_name: &str,
        options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<RuntimeValue, CliError> {
        match runtime_name {
            "rust" | "literal" => {
                let (value, mut outcome) =
                    self.run_ast_interpreter(ast, options, InterpreterMode::RunTime)?;

                if !outcome.stdout.is_empty() {
                    for chunk in outcome.stdout.drain(..) {
                        print!("{}", chunk);
                    }
                    let _ = io::stdout().flush();
                }

                Ok(RuntimeValue::literal(value))
            }
            other => Err(CliError::Compilation(format!(
                "Unsupported interpreter runtime: {}",
                other
            ))),
        }
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

        let has_errors = diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        if has_errors {
            return Err(CliError::Compilation(format!(
                "{} stage failed; see diagnostics for details",
                stage
            )));
        }

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
