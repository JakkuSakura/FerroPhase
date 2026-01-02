use crate::CliError;
use crate::codegen::CodeGenerator;
use crate::config::{PipelineConfig, PipelineOptions, PipelineTarget};
use crate::languages::frontend::{
    FerroFrontend, FlatbuffersFrontend, FrontendResult, FrontendSnapshot, JsonSchemaFrontend,
    LanguageFrontend, PrqlFrontend, SqlFrontend, TypeScriptFrontend, WitFrontend,
};
use crate::languages::{self, detect_source_language};
#[cfg(feature = "bootstrap")]
use fp_core::ast::json as ast_json;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{AstSerializer, Node, NodeKind, RuntimeValue, Value};
#[cfg(feature = "bootstrap")]
use fp_core::ast::{AstSnapshot, snapshot};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{
    Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager,
};
#[cfg(feature = "bootstrap")]
use fp_core::intrinsics::NoopIntrinsicNormalizer;
use fp_core::intrinsics::{IntrinsicMaterializer, IntrinsicNormalizer};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::workspace::{WorkspaceDocument, WorkspaceModule, WorkspacePackage};
use fp_core::{hir, lir};
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions, InterpreterOutcome};
use fp_llvm::{
    LlvmCompiler, LlvmConfig, linking::LinkerConfig, runtime::LlvmRuntimeIntrinsicMaterializer,
};
use fp_optimize::orchestrators::{ConstEvalOutcome, ConstEvaluationOrchestrator};
use fp_optimize::passes::{
    NoopIntrinsicMaterializer, lower_closures, materialize_intrinsics, normalize_intrinsics,
};
use fp_optimize::transformations::{HirGenerator, IrTransform, LirGenerator, MirLowering};
#[cfg(feature = "bootstrap")]
use fp_rust::printer::RustPrinter;
use fp_typescript::frontend::TsParseMode;
use fp_typing::TypingDiagnosticLevel;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex, OnceLock};
use tokio::runtime::Handle;
use tokio::task;
use tracing::{debug, info_span, warn};

// Begin internal submodules extracted for clarity
mod artifacts;
mod diagnostics;
mod stages;
mod workspace;
use self::diagnostics as diag;
use artifacts::{BackendArtifacts, LlvmArtifacts};
use workspace::{WorkspaceLirReplay, determine_main_package_name};

#[cfg(feature = "bootstrap")]
const BOOTSTRAP_ENV_VAR: &str = "FERROPHASE_BOOTSTRAP";

// AST invariant checks
fn ast_contains_quote_or_splice(node: &Node) -> bool {
    use fp_core::ast;
    fn expr_has(e: &ast::Expr) -> bool {
        use ast::ExprKind::*;
        match e.kind() {
            Quote(_) | Splice(_) => true,
            Block(b) => {
                b.stmts.iter().any(stmt_has) || b.last_expr().map(expr_has).unwrap_or(false)
            }
            If(i) => {
                expr_has(i.cond.as_ref())
                    || expr_has(i.then.as_ref())
                    || i.elze
                        .as_ref()
                        .map(|x| expr_has(x.as_ref()))
                        .unwrap_or(false)
            }
            Loop(l) => expr_has(l.body.as_ref()),
            While(w) => expr_has(w.cond.as_ref()) || expr_has(w.body.as_ref()),
            Match(m) => m
                .cases
                .iter()
                .any(|c| expr_has(c.cond.as_ref()) || expr_has(c.body.as_ref())),
            Let(l) => expr_has(l.expr.as_ref()),
            Assign(a) => expr_has(a.target.as_ref()) || expr_has(a.value.as_ref()),
            Invoke(i) => i.args.iter().any(expr_has),
            Struct(s) => {
                expr_has(s.name.as_ref())
                    || s.fields
                        .iter()
                        .any(|f| f.value.as_ref().map(expr_has).unwrap_or(false))
            }
            Structural(s) => s
                .fields
                .iter()
                .any(|f| f.value.as_ref().map(expr_has).unwrap_or(false)),
            IntrinsicContainer(c) => match c {
                ast::ExprIntrinsicContainer::VecElements { elements } => {
                    elements.iter().any(expr_has)
                }
                ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    expr_has(elem) || expr_has(len)
                }
                ast::ExprIntrinsicContainer::HashMapEntries { entries } => entries
                    .iter()
                    .any(|e| expr_has(&e.key) || expr_has(&e.value)),
            },
            Array(a) => a.values.iter().any(expr_has),
            ArrayRepeat(r) => expr_has(r.elem.as_ref()) || expr_has(r.len.as_ref()),
            Tuple(t) => t.values.iter().any(expr_has),
            BinOp(b) => expr_has(b.lhs.as_ref()) || expr_has(b.rhs.as_ref()),
            UnOp(u) => expr_has(u.val.as_ref()),
            Reference(r) => expr_has(r.referee.as_ref()),
            Dereference(d) => expr_has(d.referee.as_ref()),
            Select(s) => expr_has(s.obj.as_ref()),
            Index(i) => expr_has(i.obj.as_ref()) || expr_has(i.index.as_ref()),
            Cast(c) => expr_has(c.expr.as_ref()),
            Closure(cl) => expr_has(cl.body.as_ref()),
            Closured(cl) => expr_has(cl.expr.as_ref()),
            Try(t) => expr_has(t.expr.as_ref()),
            Paren(p) => expr_has(p.expr.as_ref()),
            For(f) => expr_has(f.iter.as_ref()) || expr_has(f.body.as_ref()),
            Async(a) => expr_has(a.expr.as_ref()),
            FormatString(fs) => fs.args.iter().any(expr_has),
            Value(_) | Locator(_) | Id(_) | Item(_) | Any(_) | Await(_) | Range(_) | Splat(_)
            | SplatDict(_) | IntrinsicCall(_) | Macro(_) => false,
        }
    }
    fn stmt_has(s: &ast::BlockStmt) -> bool {
        match s {
            ast::BlockStmt::Expr(e) => expr_has(e.expr.as_ref()),
            ast::BlockStmt::Let(l) => {
                l.init.as_ref().map(expr_has).unwrap_or(false)
                    || l.diverge.as_ref().map(expr_has).unwrap_or(false)
            }
            ast::BlockStmt::Item(i) => item_has(i),
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => false,
        }
    }
    fn item_has(i: &ast::Item) -> bool {
        match i.kind() {
            ast::ItemKind::DefFunction(f) => {
                if f.sig.is_const || f.sig.quote_kind.is_some() {
                    false
                } else {
                    expr_has(f.body.as_ref())
                }
            }
            ast::ItemKind::DefConst(c) => expr_has(&c.value),
            ast::ItemKind::DefStatic(s) => expr_has(&s.value),
            ast::ItemKind::Module(m) => m.items.iter().any(item_has),
            _ => false,
        }
    }
    match node.kind() {
        NodeKind::Expr(e) => expr_has(e),
        NodeKind::Item(i) => item_has(i),
        NodeKind::File(f) => f.items.iter().any(item_has),
        _ => false,
    }
}
const BOOTSTRAP_SNAPSHOT_ENV_VAR: &str = "FERROPHASE_BOOTSTRAP_SNAPSHOT";
#[cfg(feature = "bootstrap")]
const SNAPSHOT_SCHEMA_VERSION: u32 = 1;

static BOOTSTRAP_DIAG_CACHE: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();

#[cfg(feature = "bootstrap")]
fn detect_bootstrap_mode() -> bool {
    use std::env;

    match env::var_os(BOOTSTRAP_ENV_VAR) {
        None => false,
        Some(value) => {
            if value.is_empty() {
                return true;
            }

            match value.to_str() {
                Some(text) => matches!(
                    text.trim().to_ascii_lowercase().as_str(),
                    "1" | "true" | "yes" | "on"
                ),
                None => false,
            }
        }
    }
}

#[cfg(not(feature = "bootstrap"))]
const fn detect_bootstrap_mode() -> bool {
    false
}

fn detect_snapshot_export() -> bool {
    use std::env;

    let enabled = match env::var_os(BOOTSTRAP_SNAPSHOT_ENV_VAR) {
        None => false,
        Some(value) => {
            if value.is_empty() {
                return true;
            }
            match value.to_str() {
                Some(text) => matches!(
                    text.trim().to_ascii_lowercase().as_str(),
                    "1" | "true" | "yes" | "on"
                ),
                None => false,
            }
        }
    };

    if enabled {
        debug!("snapshot export requested");
    }

    enabled
}

#[cfg(feature = "bootstrap")]
fn build_ast_snapshot(ast: &Node) -> AstSnapshot {
    use std::time::{SystemTime, UNIX_EPOCH};

    let created_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;

    AstSnapshot {
        schema_version: SNAPSHOT_SCHEMA_VERSION,
        tool_version: env!("CARGO_PKG_VERSION").to_string(),
        created_ms,
        ast: ast.clone(),
    }
}

const STAGE_FRONTEND: &str = "frontend";
const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_BACKEND_LOWERING: &str = "hir→mir→lir";
const STAGE_RUNTIME_MATERIALIZE: &str = "materialize-runtime";
const STAGE_TYPE_POST_MATERIALIZE: &str = "ast→typed(post-materialize)";
const STAGE_LINK_BINARY: &str = "link-binary";
const STAGE_INTRINSIC_NORMALIZE: &str = "intrinsic-normalize";
const STAGE_AST_INTERPRET: &str = "ast-interpret";

fn stage_enabled(options: &PipelineOptions, stage: &str) -> bool {
    !options.disabled_stages.iter().any(|s| s == stage)
}

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

// BackendArtifacts and LlvmArtifacts moved to pipeline::artifacts

struct IntrinsicsMaterializer {
    strategy: Box<dyn IntrinsicMaterializer>,
}

impl IntrinsicsMaterializer {
    fn for_target(target: &PipelineTarget) -> Self {
        match target {
            PipelineTarget::Llvm | PipelineTarget::Binary | PipelineTarget::Rust => Self {
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
    frontends: HashMap<String, Arc<dyn LanguageFrontend>>,
    default_runtime: String,
    bootstrap_mode: bool,
    serializer: Option<Arc<dyn AstSerializer>>,
    intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    source_language: Option<String>,
    frontend_snapshot: Option<FrontendSnapshot>,
    last_const_eval: Option<ConstEvalOutcome>,
    typescript_frontend: Option<Arc<TypeScriptFrontend>>,
    typescript_parse_mode: TsParseMode,
}

#[derive(Debug, Clone, Default)]
pub struct TranspilePreparationOptions {
    pub run_const_eval: bool,
    pub save_intermediates: bool,
    pub base_path: Option<PathBuf>,
}

impl Pipeline {
    pub fn new() -> Self {
        let mut frontends: HashMap<String, Arc<dyn LanguageFrontend>> = HashMap::new();
        let mut register = |fe: Arc<dyn LanguageFrontend>| {
            let key = fe.language().to_lowercase();
            frontends.insert(key, fe.clone());
            for ext in fe.extensions() {
                frontends.insert(ext.to_lowercase(), fe.clone());
            }
        };
        let ferro_frontend: Arc<dyn LanguageFrontend> = Arc::new(FerroFrontend::new());
        register(ferro_frontend);
        let wit_frontend: Arc<dyn LanguageFrontend> = Arc::new(WitFrontend::new());
        register(wit_frontend);
        let ts_frontend_concrete = Arc::new(TypeScriptFrontend::new(TsParseMode::Loose));
        let ts_frontend: Arc<dyn LanguageFrontend> = ts_frontend_concrete.clone();
        register(ts_frontend);
        let sql_frontend: Arc<dyn LanguageFrontend> = Arc::new(SqlFrontend::new());
        register(sql_frontend);
        let prql_frontend: Arc<dyn LanguageFrontend> = Arc::new(PrqlFrontend::new());
        register(prql_frontend);
        let jsonschema_frontend: Arc<dyn LanguageFrontend> = Arc::new(JsonSchemaFrontend::new());
        register(jsonschema_frontend);
        let flatbuffers_frontend: Arc<dyn LanguageFrontend> = Arc::new(FlatbuffersFrontend::new());
        register(flatbuffers_frontend);
        let bootstrap_mode = detect_bootstrap_mode();

        #[cfg(feature = "bootstrap")]
        if bootstrap_mode {
            debug!("FERROPHASE_BOOTSTRAP detected; running pipeline in bootstrap mode");
        }

        Self {
            frontends,
            default_runtime: "literal".to_string(),
            bootstrap_mode,
            serializer: None,
            intrinsic_normalizer: None,
            source_language: None,
            frontend_snapshot: None,
            last_const_eval: None,
            typescript_frontend: Some(ts_frontend_concrete),
            typescript_parse_mode: TsParseMode::Loose,
        }
    }

    pub fn with_runtime(runtime_name: &str) -> Self {
        let mut pipeline = Self::new();
        pipeline.default_runtime = runtime_name.to_string();
        pipeline
    }

    // with_frontend_registry removed; pipeline owns its registered frontends

    pub fn set_serializer(&mut self, serializer: Arc<dyn AstSerializer>) {
        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer);
    }

    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.default_runtime = runtime_name.to_string();
    }

    fn refresh_bootstrap_mode(&mut self) {
        #[cfg(feature = "bootstrap")]
        {
            let env_enabled = detect_bootstrap_mode();
            if env_enabled != self.bootstrap_mode {
                debug!(
                    enabled = env_enabled,
                    "FERROPHASE_BOOTSTRAP toggled; updating pipeline bootstrap mode"
                );
                self.bootstrap_mode = env_enabled;
            }
        }
    }

    pub fn bootstrap_mode(&self) -> bool {
        self.bootstrap_mode
    }

    pub fn last_const_eval_outcome(&self) -> Option<&ConstEvalOutcome> {
        self.last_const_eval.as_ref()
    }

    pub fn set_typescript_parse_mode(&mut self, mode: TsParseMode) {
        if let Some(frontend) = &self.typescript_frontend {
            frontend.set_parse_mode(mode);
        }
        self.typescript_parse_mode = mode;
    }

    pub fn typescript_parse_mode(&self) -> TsParseMode {
        self.typescript_parse_mode
    }

    pub fn typescript_frontend(&self) -> Option<Arc<TypeScriptFrontend>> {
        self.typescript_frontend.as_ref().map(Arc::clone)
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
        self.refresh_bootstrap_mode();

        let mut options: PipelineOptions = config.into();
        options.bootstrap_mode |= self.bootstrap_mode;
        self.execute_with_options(input, options).await
    }

    pub fn parse_source_public(
        &mut self,
        source: &str,
        path: Option<&Path>,
    ) -> Result<Node, CliError> {
        let language_key = path
            .and_then(|path| detect_source_language(path).map(|lang| lang.name))
            .unwrap_or(languages::FERROPHASE);

        let frontend = self
            .frontends
            .get(language_key)
            .cloned()
            .or_else(|| self.frontends.get(languages::FERROPHASE).cloned())
            .ok_or_else(|| CliError::Compilation("No suitable frontend registered".to_string()))?;

        let FrontendResult {
            ast,
            serializer,
            intrinsic_normalizer,
            snapshot,
            diagnostics,
            ..
        } = frontend
            .parse(source, path)
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        if !collected_diagnostics.is_empty() {
            DiagnosticManager::emit(
                &collected_diagnostics,
                Some(STAGE_FRONTEND),
                &DiagnosticDisplayOptions::default(),
            );
        }

        let has_errors = collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        let treat_as_errors = if language_key == languages::TYPESCRIPT
            && matches!(self.typescript_parse_mode, TsParseMode::Loose)
        {
            false
        } else {
            has_errors
        };

        if treat_as_errors {
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

    fn parse_input_source(
        &mut self,
        options: &PipelineOptions,
        source: &str,
        input_path: Option<&PathBuf>,
    ) -> Result<Node, CliError> {
        let language = self.resolve_language(options, input_path);
        let frontend = self.frontends.get(&language).cloned().ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        self.parse_with_frontend(&frontend, source, input_path.map(|p| p.as_path()), options)
    }

    fn try_load_bootstrap_ast(&mut self, path: &Path) -> Result<Option<Node>, CliError> {
        #[cfg(feature = "bootstrap")]
        {
            if !self.bootstrap_mode {
                return Ok(None);
            }

            let is_json = path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext.eq_ignore_ascii_case("json"))
                .unwrap_or(false);

            if !is_json {
                return Ok(None);
            }

            if let Ok(snapshot) = snapshot::load_snapshot_from_file(path) {
                if snapshot.schema_version != SNAPSHOT_SCHEMA_VERSION {
                    warn!(
                        expected = SNAPSHOT_SCHEMA_VERSION,
                        actual = snapshot.schema_version,
                        "bootstrap snapshot schema mismatch"
                    );
                }
                let serializer: Arc<dyn AstSerializer> = Arc::new(RustPrinter::new_with_rustfmt());
                register_threadlocal_serializer(serializer.clone());
                self.serializer = Some(serializer);
                let normalizer: Arc<dyn IntrinsicNormalizer> =
                    Arc::new(NoopIntrinsicNormalizer::default());
                self.intrinsic_normalizer = Some(normalizer);
                self.source_language = Some("bootstrap-json".to_string());
                self.frontend_snapshot = None;
                return Ok(Some(snapshot.ast));
            }

            match ast_json::load_node_from_file(path) {
                Ok(node) => {
                    let serializer: Arc<dyn AstSerializer> =
                        Arc::new(RustPrinter::new_with_rustfmt());
                    register_threadlocal_serializer(serializer.clone());
                    self.serializer = Some(serializer);
                    let normalizer: Arc<dyn IntrinsicNormalizer> =
                        Arc::new(NoopIntrinsicNormalizer::default());
                    self.intrinsic_normalizer = Some(normalizer);
                    self.source_language = Some("bootstrap-json".to_string());
                    self.frontend_snapshot = None;
                    Ok(Some(node))
                }
                Err(err) => Err(CliError::Compilation(format!(
                    "Failed to load bootstrap AST {}: {}",
                    path.display(),
                    err
                ))),
            }
        }
        #[cfg(not(feature = "bootstrap"))]
        {
            let _ = path;
            Ok(None)
        }
    }

    #[cfg(test)]
    pub fn parse_source_with_path_for_tests(
        &mut self,
        source: &str,
        path: &Path,
    ) -> Result<Node, CliError> {
        self.parse_source_public(source, Some(path))
    }

    pub async fn execute_with_options(
        &mut self,
        input: PipelineInput,
        mut options: PipelineOptions,
    ) -> Result<PipelineOutput, CliError> {
        self.refresh_bootstrap_mode();
        options.bootstrap_mode |= self.bootstrap_mode;
        options.emit_bootstrap_snapshot |= detect_snapshot_export();
        if options.bootstrap_mode {
            options.error_tolerance.enabled = true;
            // Zero means unlimited tolerance; otherwise allow large slack for bootstrap noise.
            if options.error_tolerance.max_errors == 0 {
                options.error_tolerance.max_errors = usize::MAX;
            } else {
                options.error_tolerance.max_errors = options.error_tolerance.max_errors.max(512);
            }
        }

        let explicit_base_path = options.base_path.clone();
        let (source, default_base_path, input_path) = self.read_input(input)?;
        options.base_path = explicit_base_path.or_else(|| Some(default_base_path.clone()));

        self.reset_state();

        let ast = {
            let path_ref = input_path.as_ref();
            if options.bootstrap_mode {
                if let Some(path) = path_ref {
                    if let Some(node) = self.try_load_bootstrap_ast(path.as_path())? {
                        node
                    } else {
                        self.parse_input_source(&options, &source, path_ref)?
                    }
                } else {
                    self.parse_input_source(&options, &source, path_ref)?
                }
            } else {
                self.parse_input_source(&options, &source, path_ref)?
            }
        };

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

                if let NodeKind::Workspace(workspace) = ast.kind() {
                    if options.bootstrap_mode {
                        return self.execute_workspace_target_blocking(
                            workspace.clone(),
                            target,
                            &options,
                            base_path,
                            input_path.as_deref().map(|p| p.as_ref()),
                        );
                    }
                    return self.execute_workspace_target(
                        workspace.clone(),
                        target,
                        &options,
                        base_path,
                        input_path.as_deref(),
                    );
                }

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
        diag::emit(&collected_diagnostics, Some(STAGE_FRONTEND), options);

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
        self.refresh_bootstrap_mode();
        let mut pipeline_options = PipelineOptions::default();
        pipeline_options.save_intermediates = options.save_intermediates;
        pipeline_options.base_path = options.base_path.clone();
        pipeline_options.bootstrap_mode = self.bootstrap_mode;
        pipeline_options.emit_bootstrap_snapshot |= detect_snapshot_export();

        let diagnostic_manager = DiagnosticManager::new();

        if stage_enabled(&pipeline_options, STAGE_INTRINSIC_NORMALIZE) {
            self.run_stage(
                STAGE_INTRINSIC_NORMALIZE,
                &diagnostic_manager,
                &pipeline_options,
                |pipeline| pipeline.stage_normalize_intrinsics(ast, &diagnostic_manager),
            )?;
        }

        self.run_stage(
            STAGE_RUNTIME_MATERIALIZE,
            &diagnostic_manager,
            &pipeline_options,
            |pipeline| {
                pipeline.stage_materialize_runtime_intrinsics(
                    ast,
                    &PipelineTarget::Rust,
                    &pipeline_options,
                    &diagnostic_manager,
                )
            },
        )?;

        // For transpilation we prefer to run const-eval before type checking so that
        // quote/splice expansion and other compile-time rewrites are reflected in the
        // typed AST.
        if options.run_const_eval && stage_enabled(&pipeline_options, STAGE_CONST_EVAL) {
            let outcome = self.run_stage(
                STAGE_CONST_EVAL,
                &diagnostic_manager,
                &pipeline_options,
                |pipeline| pipeline.stage_const_eval(ast, &pipeline_options, &diagnostic_manager),
            )?;
            self.last_const_eval = Some(outcome.clone());
        }

        if stage_enabled(&pipeline_options, STAGE_TYPE_ENRICH) {
            self.run_stage(
                STAGE_TYPE_ENRICH,
                &diagnostic_manager,
                &pipeline_options,
                |pipeline| {
                    pipeline.stage_type_check(
                        ast,
                        STAGE_TYPE_ENRICH,
                        &diagnostic_manager,
                        &pipeline_options,
                    )
                },
            )?;
        }

        if options.run_const_eval && stage_enabled(&pipeline_options, STAGE_CONST_EVAL) {
            if options.save_intermediates {
                if let Some(base_path) = pipeline_options.base_path.as_ref() {
                    self.save_pretty(ast, base_path, EXT_AST_EVAL, &pipeline_options)?;
                }
            }

            #[cfg(feature = "bootstrap")]
            if pipeline_options.emit_bootstrap_snapshot {
                if let Some(base_path) = pipeline_options.base_path.as_ref() {
                    self.save_bootstrap_snapshot(ast, base_path)?;
                }
            }
        }

        let diagnostics = diagnostic_manager.get_diagnostics();
        diag::emit(&diagnostics, None, &pipeline_options);

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
        mut ast: Node,
        options: &PipelineOptions,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        let diagnostic_manager = DiagnosticManager::new();

        if stage_enabled(options, STAGE_INTRINSIC_NORMALIZE) {
            self.run_stage(
                STAGE_INTRINSIC_NORMALIZE,
                &diagnostic_manager,
                options,
                |pipeline| pipeline.stage_normalize_intrinsics(&mut ast, &diagnostic_manager),
            )?;
        }

        if stage_enabled(options, STAGE_CONST_EVAL) {
            self.run_stage(
                STAGE_CONST_EVAL,
                &diagnostic_manager,
                options,
                |pipeline| pipeline.stage_const_eval(&mut ast, options, &diagnostic_manager),
            )?;
        }

        if stage_enabled(options, STAGE_TYPE_ENRICH) {
            self.run_stage(
                STAGE_TYPE_ENRICH,
                &diagnostic_manager,
                options,
                |pipeline| {
                    pipeline.stage_type_check(
                        &mut ast,
                        STAGE_TYPE_ENRICH,
                        &diagnostic_manager,
                        options,
                    )
                },
            )?;
        }
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

        if stage_enabled(options, STAGE_INTRINSIC_NORMALIZE) {
            self.run_stage(
                STAGE_INTRINSIC_NORMALIZE,
                &diagnostic_manager,
                options,
                |pipeline| pipeline.stage_normalize_intrinsics(&mut ast, &diagnostic_manager),
            )?;
        }

        let outcome = if options.bootstrap_mode || !stage_enabled(options, STAGE_CONST_EVAL) {
            ConstEvalOutcome::default()
        } else {
            self.run_stage(STAGE_CONST_EVAL, &diagnostic_manager, options, |pipeline| {
                pipeline.stage_const_eval(&mut ast, options, &diagnostic_manager)
            })?
        };
        self.last_const_eval = Some(outcome.clone());

        if stage_enabled(options, STAGE_TYPE_ENRICH) {
            self.run_stage(
                STAGE_TYPE_ENRICH,
                &diagnostic_manager,
                options,
                |pipeline| {
                    pipeline.stage_type_check(
                        &mut ast,
                        STAGE_TYPE_ENRICH,
                        &diagnostic_manager,
                        options,
                    )
                },
            )?;
        }

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST, options)?;
            self.save_pretty(&ast, base_path, EXT_AST_TYPED, options)?;
        }

        if options.save_intermediates && !options.bootstrap_mode {
            self.save_pretty(&ast, base_path, EXT_AST_EVAL, options)?;
        }

        #[cfg(feature = "bootstrap")]
        if options.emit_bootstrap_snapshot {
            self.save_bootstrap_snapshot(&ast, base_path)?;
        }

        // No post-materialize typing for compilation targets; transpile pipeline handles it.

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
        diag::emit(&diagnostics, None, options);

        Ok(output)
    }

    fn execute_workspace_target(
        &self,
        workspace: WorkspaceDocument,
        target: &PipelineTarget,
        options: &PipelineOptions,
        base_path: &Path,
        snapshot_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        match target {
            PipelineTarget::Interpret => Ok(PipelineOutput::Value(Value::unit())),
            PipelineTarget::Binary => {
                let snapshot_root = snapshot_path
                    .and_then(|path| path.parent().map(Path::to_path_buf))
                    .ok_or_else(|| {
                        CliError::Compilation(
                            "workspace binary replay requires a snapshot path".to_string(),
                        )
                    })?;
                let modules_dir = base_path.with_extension("modules");
                fs::create_dir_all(&modules_dir).map_err(CliError::Io)?;

                let replay = replay_workspace_modules(
                    &workspace,
                    &PipelineTarget::Llvm,
                    options,
                    &modules_dir,
                    Some(snapshot_root.as_path()),
                )?;

                // Only fail for modules that participate in the main package binary.
                let main_pkg = determine_main_package_name(&workspace);
                let main_bin_ids: HashSet<String> = workspace
                    .packages
                    .iter()
                    .filter(|p| p.name == main_pkg)
                    .flat_map(|p| p.modules.iter())
                    .filter(|m| m.module_path.first().map(|s| s.as_str()) == Some("bin"))
                    .map(|m| m.id.clone())
                    .collect();
                let mut hard_fail: Vec<String> = Vec::new();
                for (id, msg) in &replay.failed_modules {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: {}", id, msg));
                    }
                }
                for id in &replay.missing_snapshots {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: missing snapshot", id));
                    }
                }
                if !hard_fail.is_empty() {
                    return Err(CliError::Compilation(format!(
                        "workspace replay failed for main modules: {}",
                        hard_fail.join(", ")
                    )));
                }

                let llvm_ir_path = base_path.with_extension("ll");
                let llvm_text = assemble_workspace_llvm_ir(&workspace, &replay);
                fs::write(&llvm_ir_path, llvm_text.as_bytes()).map_err(CliError::Io)?;

                let diagnostic_manager = DiagnosticManager::new();
                let link_result =
                    self.stage_link_binary(&llvm_ir_path, base_path, options, &diagnostic_manager);
                let diagnostics = diagnostic_manager.get_diagnostics();
                diag::emit(&diagnostics, Some(STAGE_LINK_BINARY), options);

                match link_result {
                    Ok(binary_path) => Ok(PipelineOutput::Binary(binary_path)),
                    Err(err) => Err(CliError::Compilation(format!(
                        "workspace linking failed: {}",
                        err
                    ))),
                }
            }
            PipelineTarget::Llvm | PipelineTarget::Rust | PipelineTarget::Bytecode => {
                let snapshot_root =
                    snapshot_path.and_then(|path| path.parent().map(Path::to_path_buf));
                let modules_dir = base_path.with_extension("modules");
                fs::create_dir_all(&modules_dir).map_err(CliError::Io)?;

                let replay = replay_workspace_modules(
                    &workspace,
                    target,
                    options,
                    &modules_dir,
                    snapshot_root.as_deref(),
                )?;

                // Only fail for modules that participate in the main package binary.
                let main_pkg = determine_main_package_name(&workspace);
                let main_bin_ids: HashSet<String> = workspace
                    .packages
                    .iter()
                    .filter(|p| p.name == main_pkg)
                    .flat_map(|p| p.modules.iter())
                    .filter(|m| m.module_path.first().map(|s| s.as_str()) == Some("bin"))
                    .map(|m| m.id.clone())
                    .collect();
                let mut hard_fail: Vec<String> = Vec::new();
                for (id, msg) in &replay.failed_modules {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: {}", id, msg));
                    }
                }
                for id in &replay.missing_snapshots {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: missing snapshot", id));
                    }
                }
                if !hard_fail.is_empty() {
                    return Err(CliError::Compilation(format!(
                        "workspace replay failed for main modules: {}",
                        hard_fail.join(", ")
                    )));
                }

                let artifact = match target {
                    PipelineTarget::Llvm => assemble_workspace_llvm_ir(&workspace, &replay),
                    _ => render_workspace_module_artifact(&workspace, target, &replay),
                };

                Ok(PipelineOutput::Code(artifact))
            }
        }
    }

    fn should_emit_bootstrap_diagnostic(&self, stage: &'static str, message: &str) -> bool {
        if !self.bootstrap_mode {
            return true;
        }
        let cache = BOOTSTRAP_DIAG_CACHE.get_or_init(|| Mutex::new(HashSet::new()));
        let key = format!("{}::{}", stage, message);
        if let Ok(mut guard) = cache.lock() {
            guard.insert(key)
        } else {
            true
        }
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

    #[cfg(feature = "bootstrap")]
    fn save_bootstrap_snapshot(&self, ast: &Node, base_path: &Path) -> Result<(), CliError> {
        let snapshot_path = base_path.with_extension("ast.json");

        if let Some(parent) = snapshot_path.parent() {
            fs::create_dir_all(parent).map_err(CliError::Io)?;
        }

        let snapshot = build_ast_snapshot(ast);

        snapshot::write_snapshot_to_file(&snapshot_path, &snapshot).map_err(|err| {
            CliError::Compilation(format!(
                "Failed to write bootstrap AST snapshot {}: {}",
                snapshot_path.display(),
                err
            ))
        })
    }

    /// Compile a single AST JSON snapshot file into LIR artifacts (blocking, no Tokio required).
    pub(crate) fn compile_snapshot_to_lir_blocking(
        &mut self,
        snapshot_path: &Path,
        mut options: PipelineOptions,
        module_base_path: &Path,
    ) -> Result<BackendArtifacts, CliError> {
        self.refresh_bootstrap_mode();
        options.bootstrap_mode |= self.bootstrap_mode;
        options.emit_bootstrap_snapshot |= detect_snapshot_export();

        let base_path = module_base_path.to_path_buf();

        self.reset_state();

        // Load AST snapshot directly if available; otherwise parse source file
        let ast = match self.try_load_bootstrap_ast(snapshot_path)? {
            Some(node) => node,
            None => {
                let source = std::fs::read_to_string(snapshot_path).map_err(|e| {
                    CliError::Compilation(format!(
                        "failed to read input {}: {}",
                        snapshot_path.display(),
                        e
                    ))
                })?;
                self.parse_input_source(&options, &source, Some(&snapshot_path.to_path_buf()))?
            }
        };

        let mut ast = ast;
        let diagnostic_manager = DiagnosticManager::new();

        self.run_stage(
            STAGE_INTRINSIC_NORMALIZE,
            &diagnostic_manager,
            &options,
            |pipeline| pipeline.stage_normalize_intrinsics(&mut ast, &diagnostic_manager),
        )?;

        self.run_stage(
            STAGE_TYPE_ENRICH,
            &diagnostic_manager,
            &options,
            |pipeline| {
                pipeline.stage_type_check(
                    &mut ast,
                    STAGE_TYPE_ENRICH,
                    &diagnostic_manager,
                    &options,
                )
            },
        )?;

        if options.save_intermediates {
            self.save_pretty(&ast, &base_path, EXT_AST, &options)?;
            self.save_pretty(&ast, &base_path, EXT_AST_TYPED, &options)?;
        }

        if options.bootstrap_mode {
            self.last_const_eval = Some(ConstEvalOutcome::default());
        } else {
            let outcome = self.run_stage(
                STAGE_CONST_EVAL,
                &diagnostic_manager,
                &options,
                |pipeline| pipeline.stage_const_eval(&mut ast, &options, &diagnostic_manager),
            )?;
            self.last_const_eval = Some(outcome);
        }

        if options.save_intermediates && !options.bootstrap_mode {
            self.save_pretty(&ast, &base_path, EXT_AST_EVAL, &options)?;
        }

        #[cfg(feature = "bootstrap")]
        if options.emit_bootstrap_snapshot {
            self.save_bootstrap_snapshot(&ast, &base_path)?;
        }

        self.run_stage(
            STAGE_RUNTIME_MATERIALIZE,
            &diagnostic_manager,
            &options,
            |pipeline| {
                pipeline.stage_materialize_runtime_intrinsics(
                    &mut ast,
                    &PipelineTarget::Llvm,
                    &options,
                    &diagnostic_manager,
                )
            },
        )?;

        if !options.bootstrap_mode {
            self.run_stage(
                STAGE_TYPE_POST_MATERIALIZE,
                &diagnostic_manager,
                &options,
                |pipeline| {
                    pipeline.stage_type_check(
                        &mut ast,
                        STAGE_TYPE_POST_MATERIALIZE,
                        &diagnostic_manager,
                        &options,
                    )
                },
            )?;

        }

        let hir_program = self.run_stage(
            STAGE_AST_TO_HIR,
            &diagnostic_manager,
            &options,
            |pipeline| {
                pipeline.stage_hir_generation(
                    &ast,
                    &options,
                    Some(snapshot_path),
                    &base_path,
                    &diagnostic_manager,
                )
            },
        )?;

        self.run_stage(
            STAGE_BACKEND_LOWERING,
            &diagnostic_manager,
            &options,
            |pipeline| {
                pipeline.stage_backend_lowering(
                    &hir_program,
                    &options,
                    &base_path,
                    &diagnostic_manager,
                )
            },
        )
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
            NodeKind::Query(_) => {
                return Err(CliError::Compilation(
                    "Query documents cannot be interpreted".to_string(),
                ));
            }
            NodeKind::Schema(_) => {
                return Err(CliError::Compilation(
                    "Schema documents cannot be interpreted".to_string(),
                ));
            }
            NodeKind::Workspace(_) => {
                return Err(CliError::Compilation(
                    "Workspace documents cannot be interpreted".to_string(),
                ));
            }
        };

        let outcome = interpreter.take_outcome();
        diag::emit(&outcome.diagnostics, Some(STAGE_AST_INTERPRET), options);

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
        diag::emit(&diagnostics, Some(stage), options);
        result
    }

    fn stage_failure(stage: &str) -> CliError {
        CliError::Compilation(format!(
            "{} stage failed; see diagnostics for details",
            stage
        ))
    }

    // moved to pipeline::diagnostics

    /// Blocking, bootstrap-friendly compilation entry from a snapshot file path.
    /// Only supports non-Interpret targets; intended for Stage 1→2 bootstrap.
    pub fn execute_compilation_from_snapshot_blocking(
        &mut self,
        snapshot_path: &Path,
        mut options: PipelineOptions,
    ) -> Result<PipelineOutput, CliError> {
        self.refresh_bootstrap_mode();
        options.bootstrap_mode |= self.bootstrap_mode;
        options.emit_bootstrap_snapshot |= detect_snapshot_export();

        // Ensure a base path is set for artifacts
        let base_path = options.base_path.clone().ok_or_else(|| {
            CliError::Compilation("Missing base path for compilation".to_string())
        })?;

        self.reset_state();

        // Load AST snapshot directly if available; otherwise fall back to parsing
        // via the standard frontend selection. try_load_bootstrap_ast will handle
        // feature-gated JSON loading when supported.
        let ast = match self.try_load_bootstrap_ast(snapshot_path)? {
            Some(node) => node,
            None => {
                let source = std::fs::read_to_string(snapshot_path).map_err(|e| {
                    CliError::Compilation(format!(
                        "failed to read input {}: {}",
                        snapshot_path.display(),
                        e
                    ))
                })?;
                self.parse_input_source(&options, &source, Some(&snapshot_path.to_path_buf()))?
            }
        };

        // Workspace or single module
        match ast.kind() {
            NodeKind::Workspace(workspace) => self.execute_workspace_target_blocking(
                workspace.clone(),
                &options.target,
                &options,
                &base_path,
                Some(snapshot_path),
            ),
            _ => self.execute_compilation_target(
                &options.target,
                ast,
                &options,
                &base_path,
                Some(snapshot_path),
            ),
        }
    }

    fn execute_workspace_target_blocking(
        &mut self,
        workspace: WorkspaceDocument,
        target: &PipelineTarget,
        options: &PipelineOptions,
        base_path: &Path,
        snapshot_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        match target {
            PipelineTarget::Binary => {
                // Same as async variant: assemble LLVM then link
                let snapshot_root = snapshot_path.and_then(|p| p.parent());
                let modules_dir = base_path.with_extension("modules");
                fs::create_dir_all(&modules_dir).map_err(CliError::Io)?;
                let lir_replay = replay_workspace_modules_lir_blocking(
                    &workspace,
                    options,
                    &modules_dir,
                    snapshot_root,
                )?;

                // Only fail for modules that participate in the main package binary.
                let main_pkg = determine_main_package_name(&workspace);
                let main_bin_ids: HashSet<String> = workspace
                    .packages
                    .iter()
                    .filter(|p| p.name == main_pkg)
                    .flat_map(|p| p.modules.iter())
                    .filter(|m| m.module_path.first().map(|s| s.as_str()) == Some("bin"))
                    .map(|m| m.id.clone())
                    .collect();
                let mut hard_fail: Vec<String> = Vec::new();
                for (id, msg) in &lir_replay.failed_modules {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: {}", id, msg));
                    }
                }
                for id in &lir_replay.missing_snapshots {
                    if main_bin_ids.contains(id) {
                        hard_fail.push(format!("{} :: missing snapshot", id));
                    }
                }
                if !hard_fail.is_empty() {
                    return Err(CliError::Compilation(format!(
                        "workspace replay failed for main modules: {}",
                        hard_fail.join(", ")
                    )));
                }

                let merged_lir = workspace::assemble_workspace_lir_program(&workspace, &lir_replay);
                let llvm = self.generate_llvm_artifacts(
                    &merged_lir,
                    base_path,
                    snapshot_path,
                    true,
                    options,
                )?;
                let llvm_ir_path = llvm.ir_path.clone();

                let diagnostic_manager = DiagnosticManager::new();
                let link_result =
                    self.stage_link_binary(&llvm_ir_path, base_path, options, &diagnostic_manager);
                let diagnostics = diagnostic_manager.get_diagnostics();
                diag::emit(&diagnostics, Some(STAGE_LINK_BINARY), options);
                match link_result {
                    Ok(binary_path) => Ok(PipelineOutput::Binary(binary_path)),
                    Err(err) => Err(CliError::Compilation(format!(
                        "workspace linking failed: {}",
                        err
                    ))),
                }
            }
            PipelineTarget::Llvm | PipelineTarget::Rust | PipelineTarget::Bytecode => {
                let snapshot_root = snapshot_path.and_then(|p| p.parent());
                let modules_dir = base_path.with_extension("modules");
                fs::create_dir_all(&modules_dir).map_err(CliError::Io)?;
                if matches!(target, PipelineTarget::Llvm) {
                    let lir_replay = replay_workspace_modules_lir_blocking(
                        &workspace,
                        options,
                        &modules_dir,
                        snapshot_root,
                    )?;
                    // Only fail for main modules
                    let main_pkg = determine_main_package_name(&workspace);
                    let main_bin_ids: HashSet<String> = workspace
                        .packages
                        .iter()
                        .filter(|p| p.name == main_pkg)
                        .flat_map(|p| p.modules.iter())
                        .filter(|m| m.module_path.first().map(|s| s.as_str()) == Some("bin"))
                        .map(|m| m.id.clone())
                        .collect();
                    let mut hard_fail: Vec<String> = Vec::new();
                    for (id, msg) in &lir_replay.failed_modules {
                        if main_bin_ids.contains(id) {
                            hard_fail.push(format!("{} :: {}", id, msg));
                        }
                    }
                    for id in &lir_replay.missing_snapshots {
                        if main_bin_ids.contains(id) {
                            hard_fail.push(format!("{} :: missing snapshot", id));
                        }
                    }
                    if !hard_fail.is_empty() {
                        return Err(CliError::Compilation(format!(
                            "workspace replay failed for main modules: {}",
                            hard_fail.join(", ")
                        )));
                    }
                    let merged_lir =
                        workspace::assemble_workspace_lir_program(&workspace, &lir_replay);
                    let llvm = self.generate_llvm_artifacts(
                        &merged_lir,
                        base_path,
                        snapshot_path,
                        false,
                        options,
                    )?;
                    Ok(PipelineOutput::Code(llvm.ir_text))
                } else {
                    let replay = replay_workspace_modules_blocking(
                        &workspace,
                        target,
                        options,
                        &modules_dir,
                        snapshot_root,
                    )?;
                    // legacy non-LLVM artifact
                    let artifact = render_workspace_module_artifact(&workspace, target, &replay);
                    Ok(PipelineOutput::Code(artifact))
                }
            }
            PipelineTarget::Interpret => Err(CliError::Compilation(
                "interpret target not supported in blocking bootstrap path".to_string(),
            )),
        }
    }
}

struct WorkspaceReplay {
    module_outputs: Vec<(String, PathBuf)>,
    module_sections: Vec<(String, String)>,
    missing_snapshots: Vec<String>,
    failed_modules: Vec<(String, String)>,
}

// WorkspaceLirModule/Replay moved to pipeline::workspace

// determine_main_package_name moved to pipeline::workspace

fn replay_workspace_modules(
    workspace: &WorkspaceDocument,
    target: &PipelineTarget,
    options: &PipelineOptions,
    modules_dir: &Path,
    snapshot_root: Option<&Path>,
) -> Result<WorkspaceReplay, CliError> {
    let mut module_outputs = Vec::new();
    let mut module_sections = Vec::new();
    let mut missing_snapshots = Vec::new();
    let mut failed_modules = Vec::new();

    let mut depended_packages = HashSet::new();
    for package in &workspace.packages {
        for dependency in &package.dependencies {
            depended_packages.insert(dependency.name.clone());
        }
    }
    let root_packages: HashSet<String> = workspace
        .packages
        .iter()
        .filter(|pkg| !depended_packages.contains(&pkg.name))
        .map(|pkg| pkg.name.clone())
        .collect();

    for package in &workspace.packages {
        for module in &package.modules {
            if !should_include_workspace_module(package, module, &root_packages) {
                continue;
            }

            let snapshot_rel = match &module.snapshot {
                Some(path) => path,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };

            let snapshot_root = match snapshot_root {
                Some(root) => root,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };

            let module_snapshot_path = snapshot_root.join(snapshot_rel);
            if !module_snapshot_path.exists() {
                missing_snapshots.push(module.id.clone());
                continue;
            }

            let module_name = sanitize_module_identifier(&module.id);
            let module_output_path =
                workspace_module_output_path(modules_dir, &module_name, target);

            let module_options = PipelineOptions {
                target: target.clone(),
                runtime: options.runtime.clone(),
                source_language: options.source_language.clone(),
                optimization_level: options.optimization_level,
                save_intermediates: options.save_intermediates,
                base_path: Some(module_output_path.clone()),
                debug: options.debug.clone(),
                error_tolerance: options.error_tolerance.clone(),
                release: options.release,
                execute_main: false,
                bootstrap_mode: true,
                emit_bootstrap_snapshot: false,
                disabled_stages: options.disabled_stages.clone(),
            };

            let mut sub_pipeline = Pipeline::new();
            let module_output = match task::block_in_place(|| {
                let handle = Handle::try_current().map_err(|_| {
                    CliError::Compilation(
                        "workspace module replay requires an active Tokio runtime".to_string(),
                    )
                })?;
                handle.block_on(sub_pipeline.execute_with_options(
                    PipelineInput::File(module_snapshot_path.clone()),
                    module_options,
                ))
            }) {
                Ok(output) => output,
                Err(err) => {
                    debug!(
                        module = %module.id,
                        error = %err,
                        "workspace module replay failed"
                    );
                    failed_modules.push((module.id.clone(), err.to_string()));
                    continue;
                }
            };

            let (written_path, section) =
                write_workspace_module_output(target, module_output, &module_output_path)?;

            if let Some(section_content) = section {
                module_sections.push((module.id.clone(), section_content));
            }
            module_outputs.push((module.id.clone(), written_path));
        }
    }

    Ok(WorkspaceReplay {
        module_outputs,
        module_sections,
        missing_snapshots,
        failed_modules,
    })
}

fn replay_workspace_modules_blocking(
    workspace: &WorkspaceDocument,
    target: &PipelineTarget,
    options: &PipelineOptions,
    modules_dir: &Path,
    snapshot_root: Option<&Path>,
) -> Result<WorkspaceReplay, CliError> {
    let mut module_outputs = Vec::new();
    let mut module_sections = Vec::new();
    let mut missing_snapshots = Vec::new();
    let mut failed_modules = Vec::new();

    let mut depended_packages = HashSet::new();
    for package in &workspace.packages {
        for dependency in &package.dependencies {
            depended_packages.insert(dependency.name.clone());
        }
    }
    let root_packages: HashSet<String> = workspace
        .packages
        .iter()
        .filter(|pkg| !depended_packages.contains(&pkg.name))
        .map(|pkg| pkg.name.clone())
        .collect();

    for package in &workspace.packages {
        for module in &package.modules {
            if !should_include_workspace_module(package, module, &root_packages) {
                continue;
            }
            let snapshot_rel = match &module.snapshot {
                Some(path) => path,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };
            let snapshot_root = match snapshot_root {
                Some(root) => root,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };
            let module_snapshot_path = snapshot_root.join(snapshot_rel);
            if !module_snapshot_path.exists() {
                missing_snapshots.push(module.id.clone());
                continue;
            }

            let module_name = sanitize_module_identifier(&module.id);
            let module_output_path =
                workspace_module_output_path(modules_dir, &module_name, target);

            let module_options = PipelineOptions {
                target: target.clone(),
                runtime: options.runtime.clone(),
                source_language: options.source_language.clone(),
                optimization_level: options.optimization_level,
                save_intermediates: options.save_intermediates,
                base_path: Some(module_output_path.clone()),
                debug: options.debug.clone(),
                error_tolerance: options.error_tolerance.clone(),
                release: options.release,
                execute_main: false,
                bootstrap_mode: true,
                emit_bootstrap_snapshot: false,
                disabled_stages: options.disabled_stages.clone(),
            };

            let mut sub_pipeline = Pipeline::new();
            let module_output = match sub_pipeline
                .execute_compilation_from_snapshot_blocking(&module_snapshot_path, module_options)
            {
                Ok(output) => output,
                Err(err) => {
                    debug!(
                        module = %module.id,
                        error = %err,
                        "workspace module replay failed (blocking)"
                    );
                    failed_modules.push((module.id.clone(), err.to_string()));
                    continue;
                }
            };

            let (written_path, section) =
                write_workspace_module_output(target, module_output, &module_output_path)?;
            if let Some(section_content) = section {
                module_sections.push((module.id.clone(), section_content));
            }
            module_outputs.push((module.id.clone(), written_path));
        }
    }

    Ok(WorkspaceReplay {
        module_outputs,
        module_sections,
        missing_snapshots,
        failed_modules,
    })
}

fn replay_workspace_modules_lir_blocking(
    workspace: &WorkspaceDocument,
    options: &PipelineOptions,
    modules_dir: &Path,
    snapshot_root: Option<&Path>,
) -> Result<WorkspaceLirReplay, CliError> {
    let main_package = determine_main_package_name(workspace);
    let mut modules: Vec<workspace::WorkspaceLirModule> = Vec::new();
    let mut missing_snapshots = Vec::new();
    let mut failed_modules = Vec::new();

    for package in &workspace.packages {
        // Limit replay to the main package to keep bootstrap fast and focused.
        if package.name != main_package {
            continue;
        }
        for module in &package.modules {
            if !should_include_workspace_module(package, module, &HashSet::new()) {
                continue;
            }
            let snapshot_rel = match &module.snapshot {
                Some(path) => path,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };
            let snapshot_root = match snapshot_root {
                Some(root) => root,
                None => {
                    missing_snapshots.push(module.id.clone());
                    continue;
                }
            };
            let module_snapshot_path = snapshot_root.join(snapshot_rel);
            if !module_snapshot_path.exists() {
                missing_snapshots.push(module.id.clone());
                continue;
            }

            // Create a per-module base path for intermediates
            let module_name = sanitize_module_identifier(&module.id);
            let module_base_path =
                workspace_module_output_path(modules_dir, &module_name, &PipelineTarget::Llvm);

            let mut sub = Pipeline::new();
            let mut module_options = options.clone();
            module_options.base_path = Some(module_base_path.clone());
            module_options.target = PipelineTarget::Llvm;

            match sub.compile_snapshot_to_lir_blocking(
                &module_snapshot_path,
                module_options,
                &module_base_path,
            ) {
                Ok(backend) => {
                    modules.push(workspace::WorkspaceLirModule {
                        id: module.id.clone(),
                        package: package.name.clone(),
                        _kind: module.module_path.first().cloned(),
                        lir: backend.lir_program,
                    });
                }
                Err(err) => {
                    failed_modules.push((module.id.clone(), err.to_string()));
                }
            }
        }
    }

    Ok(WorkspaceLirReplay {
        modules,
        missing_snapshots,
        failed_modules,
    })
}

fn should_include_workspace_module(
    _package: &WorkspacePackage,
    module: &WorkspaceModule,
    _root_packages: &HashSet<String>,
) -> bool {
    // Load all workspace modules to make every symbol visible to the replay pipeline,
    // but filter out tests and examples.
    if let Some(kind) = module.module_path.first() {
        match kind.as_str() {
            "tests" | "examples" => return false,
            // Only include binary targets explicitly; library modules from non-root packages are excluded above.
            "bin" => return true,
            _ => {}
        }
    }
    // Include library modules by default
    true
}

fn assemble_workspace_llvm_ir(workspace: &WorkspaceDocument, replay: &WorkspaceReplay) -> String {
    let mut output = String::new();
    let _ = writeln!(&mut output, "; FerroPhase workspace bundle");
    let _ = writeln!(&mut output, "; manifest: {}", workspace.manifest);
    output.push('\n');

    let mut seen_datalayout = false;
    let mut seen_triple = false;
    let mut seen_declares = HashSet::new();
    let mut defined_functions = HashSet::new();

    // Build a quick index from module id to (package name, module_path-first)
    let mut module_index: HashMap<String, (String, Option<String>)> = HashMap::new();
    for package in &workspace.packages {
        for module in &package.modules {
            module_index.insert(
                module.id.clone(),
                (package.name.clone(), module.module_path.first().cloned()),
            );
        }
    }

    // Determine the main package to keep definitions for. Priority:
    // 1) FP_BOOTSTRAP_MAIN environment variable
    // 2) a package named "fp-cli" if present
    // 3) a root package (not depended upon by others), first in lexical order
    // 4) the first package in the manifest order
    let main_package = std::env::var("FP_BOOTSTRAP_MAIN")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .or_else(|| {
            workspace
                .packages
                .iter()
                .find(|p| p.name == "fp-cli")
                .map(|p| p.name.clone())
        })
        .or_else(|| {
            let mut depended = HashSet::new();
            for pkg in &workspace.packages {
                for dep in &pkg.dependencies {
                    depended.insert(dep.name.clone());
                }
            }
            let mut roots: Vec<_> = workspace
                .packages
                .iter()
                .filter(|p| !depended.contains(&p.name))
                .map(|p| p.name.clone())
                .collect();
            roots.sort();
            roots.into_iter().next()
        })
        .or_else(|| workspace.packages.first().map(|p| p.name.clone()))
        .unwrap_or_else(|| "fp-cli".to_string());

    for (_, content) in &replay.module_sections {
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("define ") {
                if let Some(name) = extract_function_name(trimmed) {
                    defined_functions.insert(name.to_string());
                }
            }
        }
    }

    let mut sections = replay.module_sections.clone();
    sections.sort_by(|a, b| a.0.cmp(&b.0));
    for (module_id, content) in &sections {
        let _ = writeln!(&mut output, "; module {}", module_id);
        let (pkg_name, kind) = module_index
            .get(module_id)
            .cloned()
            .unwrap_or_else(|| (String::new(), None));
        let keep_definitions = pkg_name == main_package && matches!(kind.as_deref(), Some("bin"));
        let mut skipping_func = false;
        let mut brace_depth: i32 = 0;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if skipping_func {
                // Track braces to find function end
                let opens = line.matches('{').count() as i32;
                let closes = line.matches('}').count() as i32;
                brace_depth += opens - closes;
                if brace_depth <= 0 {
                    skipping_func = false;
                    brace_depth = 0;
                }
                continue;
            }
            if trimmed.starts_with("target datalayout") {
                if seen_datalayout {
                    continue;
                }
                seen_datalayout = true;
            } else if trimmed.starts_with("target triple") {
                if seen_triple {
                    continue;
                }
                seen_triple = true;
            } else if trimmed.starts_with("declare ") {
                if let Some(name) = extract_function_name(trimmed) {
                    if defined_functions.contains(name) {
                        continue;
                    }
                    if !seen_declares.insert(name.to_string()) {
                        continue;
                    }
                }
            } else if trimmed.starts_with("define ") {
                if !keep_definitions {
                    // Skip entire function body for non-main modules.
                    // Initialize skipping until matching closing brace.
                    skipping_func = true;
                    brace_depth =
                        line.matches('{').count() as i32 - line.matches('}').count() as i32;
                    if brace_depth <= 0 {
                        skipping_func = false;
                        brace_depth = 0;
                    }
                    continue;
                }
            }

            output.push_str(line);
            output.push('\n');
        }
        if !output.ends_with('\n') {
            output.push('\n');
        }
        output.push('\n');
    }

    if replay.module_sections.is_empty() {
        let _ = writeln!(&mut output, "; no modules were replayed");
    }

    // Note: failed/missing modules are now hard errors upstream; do not emit their list here.

    output
}

// assemble_workspace_lir_program moved to pipeline::workspace

fn extract_function_name(line: &str) -> Option<&str> {
    let at_pos = line.find('@')?;
    let rest = &line[at_pos + 1..];
    let end = rest
        .find(|c: char| c == '(' || c.is_whitespace())
        .unwrap_or_else(|| rest.len());
    Some(&rest[..end])
}

fn render_workspace_module_artifact(
    workspace: &WorkspaceDocument,
    target: &PipelineTarget,
    replay: &WorkspaceReplay,
) -> String {
    let mut artifact = String::new();
    let comment_prefix = workspace_comment_prefix(target);

    if replay.module_sections.is_empty() {
        artifact.push_str(&render_workspace_summary(workspace));
        if !replay.missing_snapshots.is_empty() {
            artifact.push_str("\n[missing module snapshots]\n");
            for module_id in &replay.missing_snapshots {
                let _ = writeln!(&mut artifact, "- {}", module_id);
            }
        }
        if !replay.failed_modules.is_empty() {
            artifact.push_str("\n[failed module replays]\n");
            for (module_id, message) in &replay.failed_modules {
                let _ = writeln!(&mut artifact, "- {} :: {}", module_id, message);
            }
        }

        return artifact;
    }

    for (module_id, content) in &replay.module_sections {
        let _ = writeln!(&mut artifact, "{} module {}", comment_prefix, module_id);
        artifact.push_str(content);
        if !content.ends_with('\n') {
            artifact.push('\n');
        }
        artifact.push('\n');
    }

    if !replay.module_outputs.is_empty() {
        let _ = writeln!(&mut artifact, "{} module artifacts:", comment_prefix);
        for (module_id, path) in &replay.module_outputs {
            let _ = writeln!(
                &mut artifact,
                "{} - {} => {}",
                comment_prefix,
                module_id,
                path.display()
            );
        }
        artifact.push('\n');
    }

    if !replay.missing_snapshots.is_empty() {
        let _ = writeln!(
            &mut artifact,
            "{} missing module snapshots:",
            comment_prefix
        );
        for module_id in &replay.missing_snapshots {
            let _ = writeln!(&mut artifact, "{} - {}", comment_prefix, module_id);
        }
    }

    if !replay.failed_modules.is_empty() {
        if !artifact.ends_with('\n') {
            artifact.push('\n');
        }
        let _ = writeln!(&mut artifact, "{} failed module replays:", comment_prefix);
        for (module_id, message) in &replay.failed_modules {
            let _ = writeln!(
                &mut artifact,
                "{} - {} :: {}",
                comment_prefix, module_id, message
            );
        }
    }

    artifact
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

fn workspace_module_output_path(
    modules_dir: &Path,
    module_name: &str,
    target: &PipelineTarget,
) -> PathBuf {
    let extension = match target {
        PipelineTarget::Llvm => "ll",
        PipelineTarget::Rust => "rs",
        PipelineTarget::Bytecode => "bc",
        PipelineTarget::Binary => "bin",
        PipelineTarget::Interpret => "out",
    };

    let mut path = modules_dir.join(module_name);
    path.set_extension(extension);
    path
}

fn workspace_comment_prefix(target: &PipelineTarget) -> &'static str {
    match target {
        PipelineTarget::Rust => "//",
        _ => ";",
    }
}

#[allow(dead_code)]
// removed unused copy_current_executable helper (was never called)

fn write_workspace_module_output(
    target: &PipelineTarget,
    module_output: PipelineOutput,
    desired_path: &Path,
) -> Result<(PathBuf, Option<String>), CliError> {
    if let Some(parent) = desired_path.parent() {
        fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    match module_output {
        PipelineOutput::Code(code) => {
            fs::write(desired_path, code.as_bytes()).map_err(CliError::Io)?;
            Ok((desired_path.to_path_buf(), Some(code)))
        }
        PipelineOutput::Binary(path) => match target {
            PipelineTarget::Binary => {
                if path != desired_path {
                    fs::copy(&path, desired_path).map_err(CliError::Io)?;
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::PermissionsExt;
                        let mode = fs::metadata(&path)
                            .map_err(CliError::Io)?
                            .permissions()
                            .mode();
                        fs::set_permissions(desired_path, std::fs::Permissions::from_mode(mode))
                            .map_err(CliError::Io)?;
                    }
                }
                Ok((desired_path.to_path_buf(), None))
            }
            _ => Err(CliError::Compilation(format!(
                "workspace replay produced a binary artifact for target {:?}",
                target
            ))),
        },
        PipelineOutput::Value(_) | PipelineOutput::RuntimeValue(_) => {
            Err(CliError::Compilation(format!(
                "workspace replay produced unsupported output for target {:?}",
                target
            )))
        }
    }
}

fn render_workspace_summary(workspace: &WorkspaceDocument) -> String {
    let mut buffer = String::new();
    let _ = writeln!(buffer, "; FerroPhase workspace summary");
    let _ = writeln!(buffer, "; manifest: {}", workspace.manifest);

    if workspace.packages.is_empty() {
        let _ = writeln!(buffer, "; packages: <none>");
        return buffer;
    }

    for package in &workspace.packages {
        let version = package
            .version
            .as_deref()
            .map(|v| format!(" {}", v))
            .unwrap_or_default();
        let _ = writeln!(buffer, "; package {}{}", package.name, version);
        let _ = writeln!(buffer, ";   manifest: {}", package.manifest_path);
        let _ = writeln!(buffer, ";   root: {}", package.root);

        if !package.modules.is_empty() {
            let _ = writeln!(buffer, ";   modules:");
            for module in &package.modules {
                let language = module.language.as_deref().unwrap_or("unknown");
                let _ = writeln!(buffer, ";     - {} [{}]", module.path, language);
            }
        }

        if !package.features.is_empty() {
            let _ = writeln!(buffer, ";   features: {}", package.features.join(", "));
        }

        if !package.dependencies.is_empty() {
            let rendered_deps = package
                .dependencies
                .iter()
                .map(|dep| {
                    dep.kind
                        .as_deref()
                        .map(|kind| format!("{} ({kind})", dep.name))
                        .unwrap_or_else(|| dep.name.clone())
                })
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(buffer, ";   dependencies: {}", rendered_deps);
        }
    }

    buffer
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
            let pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = target.clone();
            options.base_path = Some(PathBuf::from("unit_test_output"));
            options.bootstrap_mode = pipeline.bootstrap_mode();

            Self {
                pipeline,
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

        fn type_check(&mut self, ast: &mut Node) {
            if let Err(err) = self
                .pipeline
                .stage_type_check_for_tests(ast, &self.diagnostics)
            {
                self.fail_with_diagnostics("type checking", err);
            }
        }

        fn rerun_type_check(&mut self, ast: &mut Node, stage: &'static str) {
            if let Err(err) =
                self.pipeline
                    .stage_type_check(ast, stage, &self.diagnostics, &self.options)
            {
                self.fail_with_diagnostics(stage, err);
            }
        }

        fn materialize_runtime(&self, ast: &mut Node, target: PipelineTarget) {
            if let Err(err) = self.pipeline.stage_materialize_runtime_intrinsics(
                ast,
                &target,
                &self.options,
                &self.diagnostics,
            ) {
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

        fn hir(&mut self, ast: &Node) -> hir::Program {
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
                    ast::ExprKind::IntrinsicContainer(collection) => match collection {
                        ast::ExprIntrinsicContainer::VecElements { elements } => {
                            for elem in elements {
                                self.visit_expr(elem);
                            }
                        }
                        ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                            self.visit_expr(elem);
                            self.visit_expr(len);
                        }
                        ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
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
                    ast::ExprKind::Quote(q) => {
                        for stmt in &q.block.stmts {
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
                    ast::ExprKind::Splice(s) => self.visit_expr(&s.token),
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
                    _ => {}
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
            ast::NodeKind::Query(_) | ast::NodeKind::Schema(_) | ast::NodeKind::Workspace(_) => {}
        }
        collector.0
    }

    #[test]
    fn rust_frontend_parses_expression() {
        let mut pipeline = Pipeline::new();
        assert!(pipeline.parse_source_public("1 + 2", None).is_ok());
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
            .expect("normalization for type arithmetic example should succeed");

        let result = pipeline.stage_type_check_for_tests(&mut ast, &diagnostics);
        if let Err(err) = result {
            panic!("type arithmetic example failed to type-check: {}", err);
        }
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
        if let Err(err) = pipeline.stage_type_check_for_tests(&mut ast, &diagnostics) {
            panic!(
                "type checking should succeed before hitting interpreter limitations: {err:?}; diagnostics: {:?}",
                diagnostics.get_diagnostics()
            );
        }

        let mut options = PipelineOptions::default();
        options.execute_main = true;

        pipeline
            .stage_const_eval(&mut ast, &options, &diagnostics)
            .expect("enum const-eval should succeed");
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

        pipeline
            .stage_type_check_for_tests(&mut ast, &diagnostics)
            .expect("trait typing should succeed");
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
