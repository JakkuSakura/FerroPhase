use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::{
    register_threadlocal_serializer, AstSerializer, MacroExpansionParser, Node, Ty, Value,
};
use fp_core::cfg::TargetEnv;
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result as CoreResult;
use fp_core::intrinsics::IntrinsicNormalizer;

use crate::engine::{
    AstInterpreter, InterpreterCapability, InterpreterMode, InterpreterOptions, StdoutMode,
};

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
            mode: InterpreterMode::Comptime,
            capability: InterpreterCapability::default(),
            debug_assertions: self.debug_assertions,
            diagnostics: self.diagnostics.clone(),
            diagnostic_context: STAGE_CONST_EVAL,
            module_resolution: None,
            macro_parser,
            intrinsic_normalizer,
            stdout_mode: StdoutMode::Capture,
            target_env: TargetEnv::host(),
            command_mock_state: None,
            runtime_extern_hook: None,
            jit: None,
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
