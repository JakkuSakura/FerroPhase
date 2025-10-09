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
    Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager,
};
use fp_core::intrinsics::{IntrinsicMaterializer, IntrinsicNormalizer};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::{hir, lir};
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions, InterpreterOutcome};
use fp_llvm::{
    LlvmCompiler, LlvmConfig, linking::LinkerConfig, runtime::LlvmRuntimeIntrinsicMaterializer,
};
use fp_optimize::orchestrators::{ConstEvalOutcome, ConstEvaluationOrchestrator};
use fp_optimize::passes::{
    lower_closures,
    materialize_intrinsics,
    normalize_intrinsics,
    remove_generic_templates,
    NoopIntrinsicMaterializer,
};
use fp_optimize::transformations::{HirGenerator, IrTransform, LirGenerator, MirLowering};
use fp_typing::TypingDiagnosticLevel;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use tracing::{debug, info_span, warn};

const STAGE_FRONTEND: &str = "frontend";
const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_BACKEND_LOWERING: &str = "hir→mir→lir";
const STAGE_RUNTIME_MATERIALIZE: &str = "materialize-runtime";
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
    strategy: Box<dyn IntrinsicMaterializer>,
}

impl IntrinsicsMaterializer {
    fn for_target(target: &PipelineTarget) -> Self {
        match target {
            PipelineTarget::Llvm | PipelineTarget::Binary => Self {
                strategy: Box::new(LlvmRuntimeIntrinsicMaterializer),
            },
            _ => Self {
                strategy: Box::new(NoopIntrinsicMaterializer),
            },
        }
    }

    fn materialize(&self, ast: &mut Node) -> fp_core::error::Result<()> {
        materialize_intrinsics(ast, self.strategy.as_ref())
    }
}

pub struct Pipeline {
    frontends: Arc<FrontendRegistry>,
    default_runtime: String,
    serializer: Option<Arc<dyn AstSerializer>>,
    intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    source_language: Option<String>,
    frontend_snapshot: Option<FrontendSnapshot>,
    last_const_eval: Option<ConstEvalOutcome>,
}

#[derive(Debug, Clone, Default)]
pub struct TranspilePreparationOptions {
    pub run_const_eval: bool,
    pub save_intermediates: bool,
    pub base_path: Option<PathBuf>,
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
            intrinsic_normalizer: None,
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
            intrinsic_normalizer: None,
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
            intrinsic_normalizer,
            snapshot,
            diagnostics,
            ..
        } = frontend
            .parse(source, None)
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        if !collected_diagnostics.is_empty() {
            DiagnosticManager::emit(
                &collected_diagnostics,
                Some(STAGE_FRONTEND),
                &DiagnosticDisplayOptions::default(),
            );
        }

        if collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error)
        {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.frontend_snapshot = snapshot;
        self.source_language = Some(frontend.language().to_string());

        Ok(ast)
    }

    #[cfg(test)]
    pub fn parse_source_with_path_for_tests(
        &mut self,
        source: &str,
        path: &Path,
    ) -> Result<Node, CliError> {
        let frontend = self
            .frontends
            .get(languages::FERROPHASE)
            .ok_or_else(|| CliError::Compilation("Default frontend not registered".to_string()))?;

        let FrontendResult {
            ast,
            serializer,
            intrinsic_normalizer,
            snapshot,
            diagnostics,
            ..
        } = frontend
            .parse(source, Some(path))
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        if !collected_diagnostics.is_empty() {
            DiagnosticManager::emit(
                &collected_diagnostics,
                Some(STAGE_FRONTEND),
                &DiagnosticDisplayOptions::default(),
            );
        }

        if collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error)
        {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
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

        let ast = self.parse_with_frontend(&frontend, &source, input_path.as_deref(), &options)?;

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
        self.intrinsic_normalizer = None;
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
        options: &PipelineOptions,
    ) -> Result<Node, CliError> {
        let span = info_span!("pipeline.frontend", language = %frontend.language());
        let _enter = span.enter();

        let FrontendResult {
            last: _last,
            ast,
            serializer,
            intrinsic_normalizer,
            snapshot,
            diagnostics,
        } = frontend.parse(source, input_path)?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        self.emit_diagnostics(&collected_diagnostics, Some(STAGE_FRONTEND), options);

        let has_errors = collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        if has_errors {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.frontend_snapshot = snapshot;

        Ok(ast)
    }

    pub fn prepare_for_transpile(
        &mut self,
        ast: &mut Node,
        options: &TranspilePreparationOptions,
    ) -> Result<(), CliError> {
        let mut pipeline_options = PipelineOptions::default();
        pipeline_options.save_intermediates = options.save_intermediates;
        pipeline_options.base_path = options.base_path.clone();

        let diagnostic_manager = DiagnosticManager::new();

        self.run_stage(
            STAGE_INTRINSIC_NORMALIZE,
            &diagnostic_manager,
            &pipeline_options,
            |pipeline| pipeline.stage_normalize_intrinsics(ast, &diagnostic_manager),
        )?;

        self.run_stage(
            STAGE_TYPE_ENRICH,
            &diagnostic_manager,
            &pipeline_options,
            |pipeline| pipeline.stage_type_check(ast, STAGE_TYPE_ENRICH, &diagnostic_manager),
        )?;

        if options.run_const_eval {
            let outcome = self.run_stage(
                STAGE_CONST_EVAL,
                &diagnostic_manager,
                &pipeline_options,
                |pipeline| pipeline.stage_const_eval(ast, &pipeline_options, &diagnostic_manager),
            )?;
            self.last_const_eval = Some(outcome.clone());
            remove_generic_templates(ast)?;

            if options.save_intermediates {
                if let Some(base_path) = pipeline_options.base_path.as_ref() {
                    self.save_pretty(ast, base_path, EXT_AST_EVAL, &pipeline_options)?;
                }
            }
        }

        let diagnostics = diagnostic_manager.get_diagnostics();
        self.emit_diagnostics(&diagnostics, None, &pipeline_options);

        if diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error)
        {
            return Err(Self::stage_failure("transpile preparation"));
        }

        Ok(())
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

        self.run_stage(
            STAGE_INTRINSIC_NORMALIZE,
            &diagnostic_manager,
            options,
            |pipeline| pipeline.stage_normalize_intrinsics(&mut ast, &diagnostic_manager),
        )?;

        self.run_stage(
            STAGE_TYPE_ENRICH,
            &diagnostic_manager,
            options,
            |pipeline| pipeline.stage_type_check(&mut ast, STAGE_TYPE_ENRICH, &diagnostic_manager),
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST, options)?;
            self.save_pretty(&ast, base_path, EXT_AST_TYPED, options)?;
        }

        let outcome =
            self.run_stage(STAGE_CONST_EVAL, &diagnostic_manager, options, |pipeline| {
                pipeline.stage_const_eval(&mut ast, options, &diagnostic_manager)
            })?;
        self.last_const_eval = Some(outcome.clone());

        // Remove generic template functions after specialization
        remove_generic_templates(&mut ast)?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST_EVAL, options)?;
        }

        self.run_stage(
            STAGE_RUNTIME_MATERIALIZE,
            &diagnostic_manager,
            options,
            |pipeline| {
                pipeline.stage_materialize_runtime_intrinsics(&mut ast, target, &diagnostic_manager)
            },
        )?;

        self.run_stage(
            STAGE_TYPE_POST_MATERIALIZE,
            &diagnostic_manager,
            options,
            |pipeline| {
                pipeline.stage_type_check(
                    &mut ast,
                    STAGE_TYPE_POST_MATERIALIZE,
                    &diagnostic_manager,
                )
            },
        )?;

        self.run_stage(
            STAGE_CLOSURE_LOWERING,
            &diagnostic_manager,
            options,
            |pipeline| pipeline.stage_closure_lowering(&mut ast, &diagnostic_manager),
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, "ast-closure", options)?;
        }

        self.run_stage(
            STAGE_TYPE_POST_CLOSURE,
            &diagnostic_manager,
            options,
            |pipeline| {
                pipeline.stage_type_check(&mut ast, STAGE_TYPE_POST_CLOSURE, &diagnostic_manager)
            },
        )?;

        let output = if matches!(target, PipelineTarget::Rust) {
            let span = info_span!("pipeline.codegen", target = "rust");
            let _enter = span.enter();
            let code = CodeGenerator::generate_rust_code(&ast)?;
            PipelineOutput::Code(code)
        } else {
            let hir_program =
                self.run_stage(STAGE_AST_TO_HIR, &diagnostic_manager, options, |pipeline| {
                    pipeline.stage_hir_generation(
                        &ast,
                        options,
                        input_path,
                        base_path,
                        &diagnostic_manager,
                    )
                })?;

            match target {
                PipelineTarget::Llvm => {
                    let backend = self.run_stage(
                        STAGE_BACKEND_LOWERING,
                        &diagnostic_manager,
                        options,
                        |pipeline| {
                            pipeline.stage_backend_lowering(
                                &hir_program,
                                options,
                                base_path,
                                &diagnostic_manager,
                            )
                        },
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
                    let backend = self.run_stage(
                        STAGE_BACKEND_LOWERING,
                        &diagnostic_manager,
                        options,
                        |pipeline| {
                            pipeline.stage_backend_lowering(
                                &hir_program,
                                options,
                                base_path,
                                &diagnostic_manager,
                            )
                        },
                    )?;

                    let llvm = self.generate_llvm_artifacts(
                        &backend.lir_program,
                        base_path,
                        input_path,
                        true,
                        options,
                    )?;

                    let binary_path = self.run_stage(
                        STAGE_LINK_BINARY,
                        &diagnostic_manager,
                        options,
                        |pipeline| {
                            pipeline.stage_link_binary(
                                &llvm.ir_path,
                                base_path,
                                options,
                                &diagnostic_manager,
                            )
                        },
                    )?;

                    PipelineOutput::Binary(binary_path)
                }
                PipelineTarget::Bytecode => {
                    let backend = self.run_stage(
                        STAGE_BACKEND_LOWERING,
                        &diagnostic_manager,
                        options,
                        |pipeline| {
                            pipeline.stage_backend_lowering(
                                &hir_program,
                                options,
                                base_path,
                                &diagnostic_manager,
                            )
                        },
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

    fn stage_normalize_intrinsics(
        &self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        if let Some(normalizer) = self.intrinsic_normalizer.as_ref() {
            if let Err(err) = normalizer.normalize(ast) {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                return Err(Self::stage_failure(STAGE_INTRINSIC_NORMALIZE));
            }
            return Ok(());
        }

        match normalize_intrinsics(ast) {
            Ok(()) => Ok(()),
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                Err(Self::stage_failure(STAGE_INTRINSIC_NORMALIZE))
            }
        }
    }

    fn stage_type_check(
        &self,
        ast: &mut Node,
        stage_label: &'static str,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        match fp_typing::annotate(ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    let diagnostic = match message.level {
                        TypingDiagnosticLevel::Warning => {
                            Diagnostic::warning(message.message).with_source_context(stage_label)
                        }
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            Diagnostic::error(message.message).with_source_context(stage_label)
                        }
                    };
                    manager.add_diagnostic(diagnostic);
                }

                if saw_error || outcome.has_errors {
                    return Err(Self::stage_failure(stage_label));
                }
                Ok(())
            }
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("AST typing failed: {}", err))
                        .with_source_context(stage_label),
                );
                Err(Self::stage_failure(stage_label))
            }
        }
    }

    fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<ConstEvalOutcome, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!options.release);
        orchestrator.set_execute_main(options.execute_main);

        let outcome = match orchestrator.evaluate(ast, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                manager.add_diagnostic(diagnostic);
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        };

        manager.add_diagnostics(outcome.diagnostics.clone());
        if outcome.has_errors {
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }

        Ok(outcome)
    }

    fn stage_closure_lowering(
        &self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        match lower_closures(ast) {
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

    fn stage_materialize_runtime_intrinsics(
        &self,
        ast: &mut Node,
        target: &PipelineTarget,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
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

    #[cfg(test)]
    fn stage_type_check_for_tests(
        &self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        self.stage_type_check(ast, STAGE_TYPE_ENRICH, manager)
    }

    fn stage_link_binary(
        &self,
        llvm_ir_path: &Path,
        base_path: &Path,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<PathBuf, CliError> {
        let binary_path = base_path.with_extension(if cfg!(target_os = "windows") {
            "exe"
        } else {
            "out"
        });

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
                Diagnostic::error(
                    "`clang` not found in PATH; install LLVM toolchain to produce binaries"
                        .to_string(),
                )
                .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(Self::stage_failure(STAGE_LINK_BINARY));
        }

        let mut cmd = Command::new("clang");
        cmd.arg(llvm_ir_path).arg("-o").arg(&binary_path);
        if options.release {
            cmd.arg("-O2");
        }

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
            if message.is_empty() {
                message = stdout.trim().to_string();
            }
            if message.is_empty() {
                message = "clang failed without diagnostics".to_string();
            }
            manager.add_diagnostic(
                Diagnostic::error(format!("clang failed: {}", message))
                    .with_source_context(STAGE_LINK_BINARY),
            );
            return Err(Self::stage_failure(STAGE_LINK_BINARY));
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

        Ok(binary_path)
    }

    fn stage_backend_lowering(
        &self,
        hir_program: &hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
        manager: &DiagnosticManager,
    ) -> Result<BackendArtifacts, CliError> {
        let mut mir_lowering = MirLowering::new();
        let mir_program = mir_lowering
            .transform(hir_program.clone())
            .map_err(|err| CliError::Compilation(format!("HIR→MIR lowering failed: {}", err)))?;
        let (mir_diags, mir_had_errors) = mir_lowering.take_diagnostics();
        manager.add_diagnostics(mir_diags);
        if mir_had_errors {
            return Err(Self::stage_failure(STAGE_BACKEND_LOWERING));
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

        Ok(BackendArtifacts {
            lir_program,
            mir_text,
            lir_text,
        })
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

        use fp_core::pretty::{PrettyOptions, pretty};

        let pretty_options = PrettyOptions {
            show_types: extension != EXT_AST,
            show_spans: options.debug.verbose,
            indent_size: 2,
        };
        let rendered = format!("{}", pretty(ast, pretty_options));

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
        manager: &DiagnosticManager,
    ) -> Result<hir::Program, CliError> {
        let mut generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if options.error_tolerance.enabled {
            generator.enable_error_tolerance(options.error_tolerance.max_errors);
        }

        if matches!(ast.kind(), NodeKind::Item(_)) {
            manager.add_diagnostic(
                Diagnostic::error(
                    "Top-level items are not supported; provide a file or expression".to_string(),
                )
                .with_source_context(STAGE_AST_TO_HIR),
            );
            return Err(Self::stage_failure(STAGE_AST_TO_HIR));
        }

        let result = match ast.kind() {
            NodeKind::Expr(expr) => generator.transform(expr),
            NodeKind::File(file) => generator.transform(file),
            NodeKind::Item(_) => unreachable!(),
        };

        let (errors, warnings) = generator.take_diagnostics();

        let has_transform_error = !errors.is_empty() || result.is_err();

        if !warnings.is_empty() {
            manager.add_diagnostics(warnings);
        }

        if let Err(e) = &result {
            manager.add_diagnostic(
                Diagnostic::error(format!("AST→HIR transformation failed: {}", e))
                    .with_source_context(STAGE_AST_TO_HIR),
            );
        }

        if !errors.is_empty() {
            manager.add_diagnostics(errors);
        }

        if has_transform_error {
            return Err(Self::stage_failure(STAGE_AST_TO_HIR));
        }

        let program = result.expect("hir generation errors accounted for");

        if options.save_intermediates {
            let mut pretty_opts = PrettyOptions::default();
            pretty_opts.show_spans = options.debug.verbose;
            let rendered = format!("{}", pretty(&program, pretty_opts));
            if let Err(err) = fs::write(base_path.with_extension(EXT_HIR), rendered) {
                warn!(error = %err, "failed to persist HIR intermediate");
            }
        }

        Ok(program)
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

    fn run_stage<T, F>(
        &mut self,
        stage: &'static str,
        manager: &DiagnosticManager,
        options: &PipelineOptions,
        action: F,
    ) -> Result<T, CliError>
    where
        F: FnOnce(&mut Self) -> Result<T, CliError>,
    {
        let snapshot = manager.snapshot();
        let result = action(self);
        let diagnostics = manager.diagnostics_since(snapshot);
        self.emit_diagnostics(&diagnostics, Some(stage), options);
        result
    }

    fn stage_failure(stage: &str) -> CliError {
        CliError::Compilation(format!(
            "{} stage failed; see diagnostics for details",
            stage
        ))
    }

    fn diagnostic_display_options(&self, options: &PipelineOptions) -> DiagnosticDisplayOptions {
        DiagnosticDisplayOptions::new(options.debug.verbose)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::PipelineTarget;
    use fp_core::diagnostics::DiagnosticManager;
    use fp_core::intrinsics::IntrinsicCallKind;
    use fp_core::{ast, hir, lir};
    use std::collections::HashSet;
    use std::path::{Path, PathBuf};

    struct PipelineHarness {
        pipeline: Pipeline,
        diagnostics: DiagnosticManager,
        options: PipelineOptions,
    }

    impl PipelineHarness {
        fn new(target: PipelineTarget) -> Self {
            let mut options = PipelineOptions::default();
            options.target = target.clone();
            options.base_path = Some(PathBuf::from("unit_test_output"));

            Self {
                pipeline: Pipeline::new(),
                diagnostics: DiagnosticManager::new(),
                options,
            }
        }

        fn parse(&mut self, source: &str) -> Node {
            self.pipeline
                .parse_source_with_path_for_tests(source, Path::new("unit_test.fp"))
                .expect("frontend should succeed")
        }

        fn fail_with_diagnostics(&self, stage: &str, err: CliError) -> ! {
            let diagnostics = self.diagnostics.get_diagnostics();
            panic!(
                "{} must succeed: {:?}; diagnostics: {:?}",
                stage, err, diagnostics
            );
        }

        fn normalize(&self, ast: &mut Node) {
            if let Err(err) = self
                .pipeline
                .stage_normalize_intrinsics(ast, &self.diagnostics)
            {
                self.fail_with_diagnostics("intrinsic normalization", err);
            }
        }

        fn type_check(&self, ast: &mut Node) {
            if let Err(err) = self
                .pipeline
                .stage_type_check_for_tests(ast, &self.diagnostics)
            {
                self.fail_with_diagnostics("type checking", err);
            }
        }

        fn rerun_type_check(&self, ast: &mut Node, stage: &'static str) {
            if let Err(err) = self
                .pipeline
                .stage_type_check(ast, stage, &self.diagnostics)
            {
                self.fail_with_diagnostics(stage, err);
            }
        }

        fn closure_lowering(&self, ast: &mut Node) {
            if let Err(err) = self.pipeline.stage_closure_lowering(ast, &self.diagnostics) {
                self.fail_with_diagnostics("closure lowering", err);
            }
        }

        fn materialize_runtime(&self, ast: &mut Node, target: PipelineTarget) {
            if let Err(err) =
                self.pipeline
                    .stage_materialize_runtime_intrinsics(ast, &target, &self.diagnostics)
            {
                self.fail_with_diagnostics("runtime materialisation", err);
            }
        }

        fn const_eval(&mut self, ast: &mut Node) -> ConstEvalOutcome {
            let previous = self.options.execute_main;
            self.options.execute_main = true;
            let outcome =
                match self
                    .pipeline
                    .stage_const_eval(ast, &self.options, &self.diagnostics)
                {
                    Ok(outcome) => outcome,
                    Err(err) => self.fail_with_diagnostics("const evaluation", err),
                };
            self.options.execute_main = previous;
            outcome
        }

        fn hir(&self, ast: &Node) -> hir::Program {
            match self.pipeline.stage_hir_generation(
                ast,
                &self.options,
                None,
                Path::new("unit_test"),
                &self.diagnostics,
            ) {
                Ok(program) => program,
                Err(err) => self.fail_with_diagnostics("AST→HIR lowering", err),
            }
        }

        fn backend(&self, hir: &hir::Program) -> BackendArtifacts {
            match self.pipeline.stage_backend_lowering(
                hir,
                &self.options,
                Path::new("unit_test"),
                &self.diagnostics,
            ) {
                Ok(artifacts) => artifacts,
                Err(err) => self.fail_with_diagnostics("HIR→MIR→LIR lowering", err),
            }
        }

        fn ensure_no_errors(&self) {
            let diagnostics = self.diagnostics.get_diagnostics();
            if diagnostics
                .iter()
                .any(|diag| matches!(diag.level, DiagnosticLevel::Error))
            {
                panic!("diagnostics contained errors: {:?}", diagnostics);
            }
        }
    }

    fn stdout_lines(outcome: &ConstEvalOutcome) -> Vec<String> {
        outcome.stdout.clone()
    }

    fn assert_stdout_contains(outcome: &ConstEvalOutcome, expected: &str) {
        let lines = stdout_lines(outcome);
        assert!(
            lines.iter().any(|line| line.contains(expected)),
            "expected stdout to contain {:?}, got {:?}",
            expected,
            lines
        );
    }

    fn find_intrinsic_calls(ast: &ast::Node) -> Vec<ast::ExprIntrinsicCall> {
        struct Collector(Vec<ast::ExprIntrinsicCall>);

        impl Collector {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                match expr.kind() {
                    ast::ExprKind::IntrinsicCall(call) => self.0.push(call.clone()),
                    ast::ExprKind::Block(block) => {
                        for stmt in &block.stmts {
                            match stmt {
                                ast::BlockStmt::Expr(expr_stmt) => {
                                    self.visit_expr(expr_stmt.expr.as_ref())
                                }
                                ast::BlockStmt::Let(stmt_let) => {
                                    if let Some(init) = &stmt_let.init {
                                        self.visit_expr(init);
                                    }
                                    if let Some(on_drop) = &stmt_let.diverge {
                                        self.visit_expr(on_drop);
                                    }
                                }
                                ast::BlockStmt::Item(item) => self.visit_item(item),
                                ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
                            }
                        }
                    }
                    ast::ExprKind::If(expr_if) => {
                        self.visit_expr(&expr_if.cond);
                        self.visit_expr(&expr_if.then);
                        if let Some(elze) = &expr_if.elze {
                            self.visit_expr(elze);
                        }
                    }
                    ast::ExprKind::Loop(expr_loop) => self.visit_expr(&expr_loop.body),
                    ast::ExprKind::While(expr_while) => {
                        self.visit_expr(&expr_while.cond);
                        self.visit_expr(&expr_while.body);
                    }
                    ast::ExprKind::Match(expr_match) => {
                        for case in &expr_match.cases {
                            self.visit_expr(&case.cond);
                            self.visit_expr(&case.body);
                        }
                    }
                    ast::ExprKind::Let(expr_let) => self.visit_expr(&expr_let.expr),
                    ast::ExprKind::Assign(assign) => {
                        self.visit_expr(&assign.target);
                        self.visit_expr(&assign.value);
                    }
                    ast::ExprKind::Invoke(invoke) => {
                        for arg in &invoke.args {
                            self.visit_expr(arg);
                        }
                    }
                    ast::ExprKind::Struct(struct_expr) => {
                        self.visit_expr(struct_expr.name.as_ref());
                        for field in &struct_expr.fields {
                            if let Some(expr) = &field.value {
                                self.visit_expr(expr);
                            }
                        }
                    }
                    ast::ExprKind::Structural(structural_expr) => {
                        for field in &structural_expr.fields {
                            if let Some(expr) = &field.value {
                                self.visit_expr(expr);
                            }
                        }
                    }
                    ast::ExprKind::IntrinsicCollection(collection) => match collection {
                        ast::ExprIntrinsicCollection::VecElements { elements } => {
                            for elem in elements {
                                self.visit_expr(elem);
                            }
                        }
                        ast::ExprIntrinsicCollection::VecRepeat { elem, len } => {
                            self.visit_expr(elem);
                            self.visit_expr(len);
                        }
                        ast::ExprIntrinsicCollection::HashMapEntries { entries } => {
                            for entry in entries {
                                self.visit_expr(&entry.key);
                                self.visit_expr(&entry.value);
                            }
                        }
                    },
                    ast::ExprKind::Array(array_expr) => {
                        for elem in &array_expr.values {
                            self.visit_expr(elem);
                        }
                    }
                    ast::ExprKind::ArrayRepeat(repeat) => {
                        self.visit_expr(&repeat.elem);
                        self.visit_expr(&repeat.len);
                    }
                    ast::ExprKind::Tuple(tuple_expr) => {
                        for elem in &tuple_expr.values {
                            self.visit_expr(elem);
                        }
                    }
                    ast::ExprKind::BinOp(binop) => {
                        self.visit_expr(&binop.lhs);
                        self.visit_expr(&binop.rhs);
                    }
                    ast::ExprKind::UnOp(unop) => self.visit_expr(&unop.val),
                    ast::ExprKind::Reference(reference) => self.visit_expr(&reference.referee),
                    ast::ExprKind::Dereference(deref) => self.visit_expr(&deref.referee),
                    ast::ExprKind::Select(select) => self.visit_expr(&select.obj),
                    ast::ExprKind::Index(index_expr) => {
                        self.visit_expr(&index_expr.obj);
                        self.visit_expr(&index_expr.index);
                    }
                    ast::ExprKind::Cast(cast_expr) => self.visit_expr(&cast_expr.expr),
                    ast::ExprKind::Closure(closure) => self.visit_expr(&closure.body),
                    ast::ExprKind::Closured(closured) => {
                        self.visit_expr(closured.expr.as_ref());
                    }
                    ast::ExprKind::Try(expr_try) => self.visit_expr(&expr_try.expr),
                    ast::ExprKind::Paren(paren) => self.visit_expr(&paren.expr),
                    ast::ExprKind::FormatString(format) => {
                        for arg in &format.args {
                            self.visit_expr(arg);
                        }
                    }
                    ast::ExprKind::Value(_)
                    | ast::ExprKind::Locator(_)
                    | ast::ExprKind::Id(_)
                    | ast::ExprKind::Item(_)
                    | ast::ExprKind::Any(_) => {}
                    ast::ExprKind::Splat(splat) => self.visit_expr(&splat.iter),
                    ast::ExprKind::SplatDict(splat) => self.visit_expr(&splat.dict),
                    ast::ExprKind::Range(range) => {
                        if let Some(start) = &range.start {
                            self.visit_expr(start);
                        }
                        if let Some(end) = &range.end {
                            self.visit_expr(end);
                        }
                        if let Some(step) = &range.step {
                            self.visit_expr(step);
                        }
                    }
                }
            }

            fn visit_item(&mut self, item: &ast::Item) {
                match item.kind() {
                    ast::ItemKind::DefFunction(function) => self.visit_expr(function.body.as_ref()),
                    ast::ItemKind::DefConst(def) => self.visit_expr(&def.value),
                    ast::ItemKind::DefStatic(def) => self.visit_expr(&def.value),
                    ast::ItemKind::Impl(_) => {}
                    ast::ItemKind::Module(module) => {
                        for child in &module.items {
                            self.visit_item(child);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut collector = Collector(Vec::new());
        match ast.kind() {
            ast::NodeKind::File(file) => {
                for item in &file.items {
                    collector.visit_item(item);
                }
            }
            ast::NodeKind::Expr(expr) => collector.visit_expr(expr),
            ast::NodeKind::Item(item) => collector.visit_item(item),
        }
        collector.0
    }

    #[test]
    fn rust_frontend_parses_expression() {
        let mut pipeline = Pipeline::new();
        assert!(pipeline.parse_source_public("1 + 2").is_ok());
    }

    #[test]
    fn example01_const_blocks_and_arithmetic() {
        let source = r#"
fn main() {
    const BUFFER: i64 = 1024 * 4;
    const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
    let optimized = const { BUFFER / 1024 };
    let strategy = const {
        if FACTORIAL_5 > 100 { "large" } else { "small" }
    };
    println!("{} {}", optimized, strategy);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        assert!(matches!(ast.kind(), ast::NodeKind::File(_)));
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "4 large");
        harness.ensure_no_errors();
    }

    #[test]
    fn example02_string_processing_len_checks() {
        let source = r#"
fn main() {
    const NAME: &str = "FerroPhase";
    const VERSION: &str = "0.1.0";
    const NAME_LEN: usize = 10;
    const VERSION_LEN: usize = 5;

    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);

    const IS_EMPTY: bool = NAME_LEN == 0;
    const IS_LONG: bool = NAME_LEN > 5;
    println!("empty={}, long={}", IS_EMPTY, IS_LONG);

    const BANNER: &str = "FerroPhase v0.1.0";
    println!("banner='{}'", BANNER);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "name='FerroPhase' len=10");
        assert_stdout_contains(&outcome, "version='0.1.0' len=5");
        assert_stdout_contains(&outcome, "empty=false, long=true");
        assert_stdout_contains(&outcome, "banner='FerroPhase v0.1.0'");
        harness.ensure_no_errors();
    }

    #[test]
    fn example03_control_flow_const_evaluation() {
        let source = r#"
fn main() {
    const TEMP: i64 = 25;
    const WEATHER: &str = if TEMP > 30 { "hot" } else if TEMP > 20 { "warm" } else { "cold" };
    const ACTIVITY: &str = if WEATHER == "warm" { "outdoor" } else { "indoor" };
    println!("{} {}", WEATHER, ACTIVITY);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "warm outdoor");
        harness.ensure_no_errors();
    }

    #[test]
    fn example04_struct_introspection_intrinsics() {
        let source = r#"
struct Point {
    x: f64,
    y: f64,
}

const SIZE: usize = sizeof!(Point);
const FIELDS: usize = field_count!(Point);
const HAS_X: bool = hasfield!(Point, "x");
const METHODS: usize = method_count!(Point);

fn main() {
    println!("{} {} {} {}", SIZE, FIELDS, HAS_X, METHODS);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "16 2 true 0");
        harness.ensure_no_errors();
    }

    #[test]
    fn example05_struct_generation_with_const_switches() {
        let source = r#"
struct Config {
    x: i64,
    y: i64,
}

const FLAG_A: bool = true;
const FLAG_B: bool = false;

const CONFIG: Config = Config {
    x: if FLAG_A { 100 } else { 10 },
    y: if FLAG_B { 200 } else { 20 },
};

fn main() {
    println!("{} {}", CONFIG.x, CONFIG.y);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "100 20");
        harness.ensure_no_errors();
    }

    #[test]
    fn example06_struct_methods_and_impls() {
        let source = r#"
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn translate(&mut self, dx: i64, dy: i64) {
        self.x += dx;
        self.y += dy;
    }

    fn distance2(&self) -> i64 {
        self.x * self.x + self.y * self.y
    }
}

fn main() {
    let mut p = Point { x: 3, y: 4 };
    p.translate(1, 2);
    println!("{}", p.distance2());
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_methods.fp"))
            .expect("frontend should parse struct methods");
        let diagnostics = DiagnosticManager::new();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &diagnostics)
            .expect("normalization should succeed for struct methods");
        pipeline
            .stage_type_check_for_tests(&mut ast, &diagnostics)
            .expect("type checking should succeed for struct methods");

        let mut options = PipelineOptions::default();
        options.execute_main = true;

        let result = pipeline.stage_const_eval(&mut ast, &options, &diagnostics);
        assert!(
            result.is_err(),
            "method calls are not yet supported in const eval"
        );

        let messages: Vec<_> = diagnostics
            .get_diagnostics()
            .iter()
            .map(|diag| diag.message.clone())
            .collect();
        assert!(
            messages
                .iter()
                .any(|msg| msg.contains("method calls are not supported in const evaluation")),
            "expected const-eval to report unsupported methods, got {:?}",
            messages
        );
    }

    #[test]
    fn example07_compile_time_validation_flags() {
        let source = r#"
struct Data {
    a: i64,
    b: i64,
    c: [u8; 16],
}

const SIZE: usize = sizeof!(Data);
const FIELDS: usize = field_count!(Data);
const SIZE_OK: bool = SIZE <= 64;
const IS_ALIGNED: bool = SIZE % 8 == 0;

fn main() {
    println!("{} {} {} {}", SIZE, FIELDS, SIZE_OK, IS_ALIGNED);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "24 3 true true");
        harness.ensure_no_errors();
    }

    #[test]
    fn example08_metaprogramming_constants() {
        let source = r#"
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

const FIELD_COUNT: usize = field_count!(Point3D);
const TYPE_NAME: &str = type_name!(Point3D);

fn main() {
    println!("{} {}", TYPE_NAME, FIELD_COUNT);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "struct Point3D");
        assert_stdout_contains(&outcome, " 3");
        harness.ensure_no_errors();
    }

    #[test]
    fn example09_higher_order_functions_lowering() {
        let source = r#"
fn apply(a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    op(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    println!("{}", apply(3, 4, add));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.closure_lowering(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        let backend = harness.backend(&hir);

        let mut function_refs = HashSet::new();
        for function in &backend.lir_program.functions {
            for block in &function.basic_blocks {
                for instr in &block.instructions {
                    if let lir::LirInstructionKind::Call { function, .. } = &instr.kind {
                        if let lir::LirValue::Function(name) = function {
                            function_refs.insert(name.clone());
                        }
                    }
                }
            }
        }

        assert!(
            backend
                .lir_program
                .functions
                .iter()
                .any(|function| function.name.as_str().contains("add")),
            "expected lowered program to contain function named 'add'"
        );
        assert!(
            function_refs.iter().any(|name| name.contains("apply")),
            "expected call sites to reference the apply wrapper"
        );
        harness.ensure_no_errors();
    }

    #[test]
    fn example10_print_intrinsic_types() {
        let source = r#"
fn main() {
    println!("value: {}", 42);
    println!();
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);

        let intrinsic_calls = find_intrinsic_calls(&ast);
        assert!(
            intrinsic_calls
                .iter()
                .any(|call| call.kind == IntrinsicCallKind::Println)
        );

        let outcome = harness.const_eval(&mut ast);
        assert!(
            stdout_lines(&outcome)
                .iter()
                .any(|line| line.trim().is_empty())
        );
        harness.ensure_no_errors();
    }

    #[test]
    fn example11_function_specialisation_patterns() {
        let source = r#"
fn add(a: i64, b: i64) -> i64 { a + b }
fn double(x: i64) -> i64 { x * 2 }
fn compose(x: i64) -> i64 { double(add(x, 1)) }

fn main() {
    println!("{}", compose(10));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.closure_lowering(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        harness.backend(&hir);
        harness.ensure_no_errors();
    }

    #[test]
    fn example12_pattern_matching_guards() {
        let source = r#"
fn classify(n: i64) -> &'static str {
    if n == 0 {
        "zero"
    } else if n < 0 {
        "negative"
    } else if n % 2 == 0 {
        "even"
    } else {
        "odd"
    }
}

fn main() {
    println!("{} {} {}", classify(-5), classify(0), classify(7));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "negative zero odd");
        harness.ensure_no_errors();
    }

    #[test]
    fn example13_loops_and_breaks() {
        let source = r#"
fn main() {
    let mut sum = 0;
    let mut i = 0;
    while i < 5 {
        sum = sum + i;
        i = i + 1;
    }
    println!("{}", sum);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.closure_lowering(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        harness.backend(&hir);
        harness.ensure_no_errors();
    }

    #[test]
    fn example14_type_arithmetic_struct_merge() {
        let source = r#"
type Foo = t! {
    struct {
        a: i64,
        b: i64,
    }
};

struct Bar {
    c: i64,
    d: i64,
}

type FooPlusBar = t! {
    Foo + Bar
};

fn main() {
    let value = FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
    println!("{} {} {} {}", value.a, value.b, value.c, value.d);
}
"#;
        let mut pipeline = Pipeline::new();
        let diagnostics = DiagnosticManager::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_type_arith.fp"))
            .expect("frontend should parse type arithmetic placeholders");

        pipeline
            .stage_normalize_intrinsics(&mut ast, &diagnostics)
            .expect("normalization should run before type arithmetic fails");

        let result = pipeline.stage_type_check_for_tests(&mut ast, &diagnostics);
        assert!(result.is_err(), "type arithmetic is not yet implemented");

        let messages: Vec<_> = diagnostics
            .get_diagnostics()
            .iter()
            .map(|diag| diag.message.clone())
            .collect();
        assert!(
            messages
                .iter()
                .any(|msg| msg.contains("type inference for item not implemented")),
            "expected placeholder diagnostics for type arithmetic, got {:?}",
            messages
        );
    }

    #[test]
    fn example15_enums_and_discriminants() {
        let source = r#"
enum Value {
    A = 1,
    B = 2,
    C = 5,
}

fn main() {
    let val = Value::C;
    println!("{}", val as i32);
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_enum.fp"))
            .expect("frontend should produce an AST even for enums");
        let diagnostics = DiagnosticManager::new();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &diagnostics)
            .expect("normalization should succeed before const-eval");
        pipeline
            .stage_type_check_for_tests(&mut ast, &diagnostics)
            .expect("type checking should succeed before hitting interpreter limitations");

        let mut options = PipelineOptions::default();
        options.execute_main = true;

        let result = pipeline.stage_const_eval(&mut ast, &options, &diagnostics);
        assert!(result.is_err(), "enum const-eval should currently fail");

        let messages: Vec<_> = diagnostics
            .get_diagnostics()
            .iter()
            .map(|diag| diag.message.clone())
            .collect();
        assert!(
            messages
                .iter()
                .any(|msg| msg.contains("unresolved symbol 'Value::C'")),
            "expected const-eval to report unresolved enum variant, got {:?}",
            messages
        );
    }

    #[test]
    fn example16_traits_with_default_methods() {
        let source = r#"
trait Shape {
    fn area(&self) -> f64;

    fn describe(&self) {
        println!("{:.2}", self.area());
    }
}

struct Circle {
    radius: f64,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        3.14159 * self.radius * self.radius
    }
}

fn main() {
    let circle = Circle { radius: 5.0 };
    circle.describe();
}
"#;
        let mut pipeline = Pipeline::new();
        let diagnostics = DiagnosticManager::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_traits.fp"))
            .expect("frontend should run even for unsupported traits");

        pipeline
            .stage_normalize_intrinsics(&mut ast, &diagnostics)
            .expect("intrinsic normalization should still succeed");

        let result = pipeline.stage_type_check_for_tests(&mut ast, &diagnostics);
        assert!(result.is_err(), "trait typing should currently fail");

        let messages: Vec<_> = diagnostics
            .get_diagnostics()
            .iter()
            .map(|diag| diag.message.clone())
            .collect();
        assert!(
            messages
                .iter()
                .any(|msg| msg.contains("type inference for item not implemented")),
            "expected trait lowering to report missing inference, got {:?}",
            messages
        );
    }

    #[test]
    fn example17_generics_and_trait_bounds() {
        let source = r#"
fn identity<T>(value: T) -> T {
    value
}

fn main() {
    println!("{}", identity(42));
}
"#;
        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "42");
        harness.ensure_no_errors();
    }

    #[test]
    fn example18_comptime_collections() {
        let source = r#"
fn main() {
    const THIRD: i64 = const {
        let items = [1, 2, 3, 4];
        items[2]
    };
    println!("{}", THIRD);
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_collections.fp"))
            .expect("frontend should handle const block arrays");
        let diagnostics = DiagnosticManager::new();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &diagnostics)
            .expect("normalization must succeed before const-eval");
        pipeline
            .stage_type_check_for_tests(&mut ast, &diagnostics)
            .expect("type checking should succeed for const array usage");

        let mut options = PipelineOptions::default();
        options.execute_main = true;

        let result = pipeline.stage_const_eval(&mut ast, &options, &diagnostics);
        assert!(
            result.is_err(),
            "array indexing is not yet supported during const eval"
        );

        let messages: Vec<_> = diagnostics
            .get_diagnostics()
            .iter()
            .map(|diag| diag.message.clone())
            .collect();
        assert!(
            messages
                .iter()
                .any(|msg| msg.contains("expression not supported in AST interpretation")),
            "expected const-eval to report unsupported indexing, got {:?}",
            messages
        );
    }
}
