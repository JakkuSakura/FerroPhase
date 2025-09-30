use fp_core::ast::Node;
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::Result;
use std::sync::Arc;

/// Result of running const evaluation on the typed AST.
#[derive(Debug, Default)]
pub struct ConstEvalOutcome;

/// Temporary AST-based const-evaluation orchestrator stub.
pub struct ConstEvaluationOrchestrator {
    diagnostics: Option<Arc<DiagnosticManager>>,
}

impl ConstEvaluationOrchestrator {
    pub fn new(_serializer: Arc<dyn fp_core::ast::AstSerializer>) -> Self {
        Self { diagnostics: None }
    }

    pub fn with_diagnostics(mut self, manager: Arc<DiagnosticManager>) -> Self {
        self.diagnostics = Some(manager);
        self
    }

    pub fn set_debug_assertions(&mut self, _enabled: bool) {}

    pub fn evaluate(
        &mut self,
        _ast: &Node,
        _ctx: &SharedScopedContext,
    ) -> Result<ConstEvalOutcome> {
        Ok(ConstEvalOutcome::default())
    }
}
