use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node, Ty, Value};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result;
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_typing::AstTypeInferencer;

const DIAGNOSTIC_CONTEXT: &str = "const-eval";

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
    diagnostics: Option<Arc<DiagnosticManager>>,
    debug_assertions: bool,
    execute_main: bool,
}

impl ConstEvaluationOrchestrator {
    pub fn new(_serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            diagnostics: None,
            debug_assertions: false,
            execute_main: false,
        }
    }

    pub fn with_diagnostics(mut self, manager: Arc<DiagnosticManager>) -> Self {
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
    ) -> Result<ConstEvalOutcome> {
        let options = InterpreterOptions {
            mode: InterpreterMode::CompileTime,
            debug_assertions: self.debug_assertions,
            diagnostics: self.diagnostics.clone(),
            diagnostic_context: DIAGNOSTIC_CONTEXT,
        };

        let mut interpreter = AstInterpreter::new(ctx, options);

        // Initialize the typer with the full AST context before using it incrementally
        // This ensures the typer knows about all declarations when typing individual expressions
        let mut typer = AstTypeInferencer::new().with_context(ctx);
        typer.initialize_from_node(ast);

        interpreter.set_typer(typer);

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
