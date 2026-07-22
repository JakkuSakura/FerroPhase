use std::path::{Path, PathBuf};

use fp_compiler::{
    AstId, CompilerDriver, CompilerWork, ConstValueId, ExecutionMode, FullyQualifiedPath,
    LirConsumer, RuntimeValueId, ScopeId,
};
use fp_core::{
    ast::{Node, Value},
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendResult, LanguageFrontend},
};
use fp_lang::FerroFrontend;
use fp_typing::{TypingDiagnostic, TypingDiagnosticLevel};

use crate::{CliError, Result};

pub fn check_path(path: &Path, syntax_only: bool) -> Result<()> {
    let ast = parse_file(path)?;
    if syntax_only {
        return Ok(());
    }

    let identity = CompilerIdentity::for_file(path);
    let mut driver = CompilerDriver::new();
    driver.state.insert_ast(identity.ast_id.clone(), ast);
    driver.scheduler.submit(CompilerWork::TypeAst {
        ast: identity.ast_id.clone(),
        scope: identity.scope_id(),
        path: identity.path.clone(),
        consumers: Vec::new(),
    });
    drain_driver(&mut driver)
}

pub fn eval_expr(source: &str) -> Result<Value> {
    let ast = parse_expr(source)?;
    execute_ast(ast, CompilerIdentity::for_expr(), ExecutionMode::Comptime)
}

pub fn eval_file(path: &Path) -> Result<Value> {
    let ast = parse_file(path)?;
    execute_ast(ast, CompilerIdentity::for_file(path), ExecutionMode::Runtime)
}

pub fn interpret_file(path: &Path) -> Result<Value> {
    let ast = parse_file(path)?;
    execute_ast(ast, CompilerIdentity::for_file(path), ExecutionMode::Runtime)
}

fn execute_ast(ast: Node, identity: CompilerIdentity, mode: ExecutionMode) -> Result<Value> {
    let value_key = identity.path.to_key();
    let ast_id = identity.ast_id.clone();
    let scope_id = identity.scope_id();
    let path = identity.path.clone();
    let mut driver = CompilerDriver::new();
    driver.state.insert_ast(ast_id.clone(), ast);
    driver.scheduler.submit(CompilerWork::TypeAst {
        ast: ast_id,
        scope: scope_id,
        path,
        consumers: vec![match mode {
            ExecutionMode::Comptime => LirConsumer::ExecuteComptime,
            ExecutionMode::Runtime => LirConsumer::ExecuteRuntime,
        }],
    });
    drain_driver(&mut driver)?;

    match mode {
        ExecutionMode::Comptime => driver
            .state
            .const_value(&ConstValueId::new(format!("const_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
        ExecutionMode::Runtime => driver
            .state
            .runtime_value(&RuntimeValueId::new(format!("runtime_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
    }
}

fn drain_driver(driver: &mut CompilerDriver) -> Result<()> {
    while driver
        .run_next()
        .map_err(|err| CliError::Compilation(err.to_string()))?
        .is_some()
    {}
    emit_typing_diagnostics(driver.state.typing_diagnostics())
}

fn parse_expr(source: &str) -> Result<Node> {
    let frontend = FerroFrontend::new();
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_expr(source)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics())?;
    Ok(ast)
}

fn parse_file(path: &Path) -> Result<Node> {
    let frontend = FerroFrontend::new();
    let source = std::fs::read_to_string(path).map_err(CliError::Io)?;
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_file(&source, path)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics())?;
    Ok(ast)
}

fn emit_frontend_diagnostics(diagnostics: &[Diagnostic]) -> Result<()> {
    DiagnosticManager::emit(
        diagnostics,
        Some("frontend"),
        &DiagnosticDisplayOptions::default(),
    );
    if diagnostics
        .iter()
        .any(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
    {
        return Err(CliError::Compilation(
            "frontend stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn emit_typing_diagnostics(diagnostics: &[TypingDiagnostic]) -> Result<()> {
    let rendered: Vec<Diagnostic<String>> = diagnostics.iter().map(as_core_diagnostic).collect();
    DiagnosticManager::emit(
        &rendered,
        Some("typing"),
        &DiagnosticDisplayOptions::default(),
    );
    if diagnostics
        .iter()
        .any(|diagnostic| matches!(diagnostic.level, TypingDiagnosticLevel::Error))
    {
        return Err(CliError::Compilation(
            "typing stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn as_core_diagnostic(diagnostic: &TypingDiagnostic) -> Diagnostic<String> {
    let mut rendered = match diagnostic.level {
        TypingDiagnosticLevel::Error => Diagnostic::error(diagnostic.message.clone()),
        TypingDiagnosticLevel::Warning => Diagnostic::warning(diagnostic.message.clone()),
    }
    .with_source_context("typing".to_string());

    if let Some(span) = diagnostic.span {
        rendered = rendered.with_span(span);
    }

    rendered
}

struct CompilerIdentity {
    path: FullyQualifiedPath,
    ast_id: AstId,
}

impl CompilerIdentity {
    fn for_expr() -> Self {
        Self::new(vec!["cli".to_string(), "eval_expr".to_string()])
    }

    fn for_file(path: &Path) -> Self {
        let canonical = path.canonicalize().unwrap_or_else(|_| PathBuf::from(path));
        Self::new(vec!["cli".to_string(), canonical.display().to_string()])
    }

    fn new(segments: Vec<String>) -> Self {
        let path = FullyQualifiedPath::from_segments(segments);
        let ast_id = AstId::new(format!("ast:{}", path.to_key()));
        Self { path, ast_id }
    }

    fn scope_id(&self) -> ScopeId {
        ScopeId::new(self.path.to_key())
    }
}
