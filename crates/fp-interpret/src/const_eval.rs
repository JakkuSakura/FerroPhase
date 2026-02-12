use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::{
    register_threadlocal_serializer, AstSerializer, Ident, Item, ItemKind, MacroExpansionParser,
    Module, Node, Ty, Value, Visibility,
};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result as CoreResult;
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::lang::{collect_lang_items, register_threadlocal_lang_items};
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};

use crate::engine::{AstInterpreter, InterpreterMode, InterpreterOptions, StdoutMode};

pub const STAGE_CONST_EVAL: &str = "const-eval";

/// Result of running const evaluation on the typed AST.
#[derive(Debug, Default, Clone)]
pub struct ConstEvalOutcome {
    pub evaluated_constants: HashMap<String, Value>,
    pub mutations_applied: bool,
    pub diagnostics: Vec<Diagnostic>,
    pub has_errors: bool,
    pub stdout: Vec<String>,
    pub closure_types: HashMap<String, Ty>,
}

/// Const-evaluation orchestrator that operates directly on the typed AST.
pub struct ConstEvaluationOrchestrator {
    serializer: Arc<dyn AstSerializer>,
    diagnostics: Option<Arc<fp_core::diagnostics::DiagnosticManager>>,
    debug_assertions: bool,
    execute_main: bool,
}

impl ConstEvaluationOrchestrator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            serializer,
            diagnostics: None,
            debug_assertions: false,
            execute_main: false,
        }
    }

    pub fn with_diagnostics(
        mut self,
        manager: Arc<fp_core::diagnostics::DiagnosticManager>,
    ) -> Self {
        self.diagnostics = Some(manager);
        self
    }

    pub fn set_debug_assertions(&mut self, enabled: bool) {
        self.debug_assertions = enabled;
    }

    pub fn set_execute_main(&mut self, enabled: bool) {
        self.execute_main = enabled;
    }

    pub fn evaluate(
        &mut self,
        ast: &mut Node,
        ctx: &SharedScopedContext,
        macro_parser: Option<Arc<dyn MacroExpansionParser>>,
        intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    ) -> CoreResult<ConstEvalOutcome> {
        register_threadlocal_serializer(self.serializer.clone());
        let options = InterpreterOptions {
            mode: InterpreterMode::CompileTime,
            debug_assertions: self.debug_assertions,
            diagnostics: self.diagnostics.clone(),
            diagnostic_context: STAGE_CONST_EVAL,
            module_resolution: None,
            macro_parser,
            intrinsic_normalizer,
            stdout_mode: StdoutMode::Capture,
            target_env: fp_core::cfg::TargetEnv::host(),
        };

        let mut interpreter = AstInterpreter::new(ctx, options);
        interpreter.enable_incremental_typing(ast);

        interpreter.interpret(ast);

        if self.execute_main {
            let _ = interpreter.execute_main();
        }

        let outcome = interpreter.take_outcome();

        Ok(ConstEvalOutcome {
            evaluated_constants: outcome.evaluated_constants,
            mutations_applied: outcome.mutations_applied,
            diagnostics: outcome.diagnostics,
            has_errors: outcome.has_errors,
            stdout: outcome.stdout,
            closure_types: outcome.closure_types,
        })
    }
}

pub struct ConstEvalContext {
    pub ast: Node,
    pub options: ConstEvalOptions,
    pub serializer: Option<Arc<dyn AstSerializer>>,
    pub macro_parser: Option<Arc<dyn MacroExpansionParser>>,
    pub intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    pub std_modules: Vec<Node>,
}

#[derive(Debug, Clone, Copy)]
pub struct ConstEvalOptions {
    pub release: bool,
    pub execute_main: bool,
}

pub struct ConstEvalStage;

pub struct ConstEvalResult {
    pub ast: Node,
    pub outcome: ConstEvalOutcome,
}

impl PipelineStage for ConstEvalStage {
    type SrcCtx = ConstEvalContext;
    type DstCtx = ConstEvalResult;

    fn name(&self) -> &'static str {
        STAGE_CONST_EVAL
    }

    fn run(
        &self,
        context: ConstEvalContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> std::result::Result<ConstEvalResult, PipelineError> {
        let serializer = match context.serializer.clone() {
            Some(serializer) => serializer,
            None => {
                diagnostics.push(
                    Diagnostic::error("No serializer registered for const-eval".to_string())
                        .with_source_context(STAGE_CONST_EVAL),
                );
                return Err(PipelineError::new(
                    STAGE_CONST_EVAL,
                    "No serializer registered for const-eval",
                ));
            }
        };
        register_threadlocal_serializer(serializer.clone());

        let mut ast = context.ast;
        for std_node in context.std_modules {
            ast = merge_std_module(ast, std_node, diagnostics)?;
        }
        let lang_items = collect_lang_items(&ast);
        register_threadlocal_lang_items(lang_items);
        let normalization = if let Some(normalizer) = context.intrinsic_normalizer.as_ref() {
            fp_core::intrinsics::normalize_intrinsics_with(&mut ast, normalizer.as_ref())
        } else {
            fp_core::intrinsics::normalize_intrinsics(&mut ast)
        };
        normalization.map_err(|err| {
            diagnostics.push(
                Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                    .with_source_context(STAGE_CONST_EVAL),
            );
            PipelineError::new(STAGE_CONST_EVAL, "Intrinsic normalization failed")
        })?;
        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!context.options.release);
        orchestrator.set_execute_main(context.options.execute_main);

        let outcome = match orchestrator.evaluate(
            &mut ast,
            &shared_context,
            context.macro_parser,
            context.intrinsic_normalizer.clone(),
        ) {
            Ok(outcome) => outcome,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("Const evaluation failed: {}", err))
                        .with_source_context(STAGE_CONST_EVAL),
                );
                return Err(PipelineError::new(
                    STAGE_CONST_EVAL,
                    "Const evaluation failed",
                ));
            }
        };

        diagnostics.extend(outcome.diagnostics.clone());

        Ok(ConstEvalResult { ast, outcome })
    }
}

fn merge_std_module(
    ast: Node,
    std_node: Node,
    diagnostics: &mut PipelineDiagnostics,
) -> std::result::Result<Node, PipelineError> {
    let Node { ty, kind } = ast;
    let Node { kind: std_kind, .. } = std_node;
    let fp_core::ast::NodeKind::File(mut file) = kind else {
        diagnostics.push(
            Diagnostic::error("std injection expects a file AST".to_string())
                .with_source_context(STAGE_CONST_EVAL),
        );
        return Err(PipelineError::new(
            STAGE_CONST_EVAL,
            "std injection expects a file AST",
        ));
    };
    let fp_core::ast::NodeKind::File(std_file) = std_kind else {
        diagnostics.push(
            Diagnostic::error("std module must be a file".to_string())
                .with_source_context(STAGE_CONST_EVAL),
        );
        return Err(PipelineError::new(
            STAGE_CONST_EVAL,
            "std module must be a file",
        ));
    };
    let mut std_module = None;
    let mut std_items = Vec::new();
    for item in std_file.items {
        if let ItemKind::Module(module) = item.kind() {
            if module.name.as_str() == "std" {
                std_module = Some(module.clone());
                continue;
            }
        }
        std_items.push(item);
    }
    let mut std_module = match std_module {
        Some(mut module) => {
            module.items.extend(std_items);
            module
        }
        None => Module {
            attrs: Vec::new(),
            name: Ident::new("std"),
            items: std_items,
            visibility: Visibility::Public,
            is_external: false,
        },
    };

    let mut merged_into_existing = false;
    for item in &mut file.items {
        if let ItemKind::Module(existing) = item.kind_mut() {
            if existing.name.as_str() == "std" {
                existing.items.extend(std::mem::take(&mut std_module.items));
                merged_into_existing = true;
                break;
            }
        }
    }
    if !merged_into_existing {
        let mut items = Vec::with_capacity(file.items.len() + 1);
        items.push(Item::from(ItemKind::Module(std_module)));
        items.append(&mut file.items);
        file.items = items;
    }
    Ok(Node {
        ty,
        kind: fp_core::ast::NodeKind::File(file),
    })
}
