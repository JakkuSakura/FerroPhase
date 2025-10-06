use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node, Ty, Value};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result;
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions};

use crate::typing::{self, TypingDiagnosticLevel};

const SPECIALIZE_CONTEXT: &str = "const-eval::specialize";
const TYPING_CONTEXT: &str = "const-eval::typing";

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
        let mut diagnostics = Vec::new();
        let mut has_errors = false;
        let mut mutations_applied = false;

        match self.specialize_generics(ast) {
            Ok((mut extra_diagnostics, typing_had_errors, mutated)) => {
                diagnostics.append(&mut extra_diagnostics);
                if typing_had_errors {
                    has_errors = true;
                }
                if mutated {
                    mutations_applied = true;
                }
            }
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!(
                        "failed to specialize generics during const evaluation: {}",
                        err
                    ))
                    .with_source_context(SPECIALIZE_CONTEXT),
                );
                return Ok(ConstEvalOutcome {
                    evaluated_constants: HashMap::new(),
                    mutations_applied,
                    diagnostics,
                    has_errors: true,
                    stdout: Vec::new(),
                    closure_types: HashMap::new(),
                });
            }
        }

        if has_errors {
            return Ok(ConstEvalOutcome {
                evaluated_constants: HashMap::new(),
                mutations_applied,
                diagnostics,
                has_errors: true,
                stdout: Vec::new(),
                closure_types: HashMap::new(),
            });
        }

        let options = InterpreterOptions {
            mode: InterpreterMode::CompileTime,
            debug_assertions: self.debug_assertions,
            diagnostics: self.diagnostics.clone(),
            diagnostic_context: DIAGNOSTIC_CONTEXT,
        };

        let mut interpreter = AstInterpreter::new(ctx, options);
        interpreter.interpret(ast);

        if self.execute_main {
            let _ = interpreter.execute_main();
        }

        let outcome = interpreter.take_outcome();

        diagnostics.extend(outcome.diagnostics);
        has_errors |= outcome.has_errors;
        mutations_applied |= outcome.mutations_applied;

        Ok(ConstEvalOutcome {
            evaluated_constants: outcome.evaluated_constants,
            mutations_applied,
            diagnostics,
            has_errors,
            stdout: outcome.stdout,
            closure_types: outcome.closure_types,
        })
    }

    fn specialize_generics(&self, ast: &mut Node) -> Result<(Vec<Diagnostic>, bool, bool)> {
        crate::specialize(ast)?;

        let typing_outcome = typing::annotate(ast)?;
        let mut diagnostics = Vec::new();
        let mut had_errors = typing_outcome.has_errors;

        for diagnostic in typing_outcome.diagnostics {
            let message = diagnostic.message;
            let level = diagnostic.level;
            let base = match level {
                TypingDiagnosticLevel::Warning => Diagnostic::warning(message.clone()),
                TypingDiagnosticLevel::Error => Diagnostic::error(message.clone()),
            };

            diagnostics.push(base.with_source_context(TYPING_CONTEXT));
            if matches!(level, TypingDiagnosticLevel::Error) {
                had_errors = true;
            }
        }

        Ok((diagnostics, had_errors, true))
    }
}
