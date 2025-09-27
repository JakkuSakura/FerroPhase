use fp_core::ast::Value;
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::Result;
use fp_core::hir::typed as thir;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::info;

use crate::orchestrators::interpretation::{InterpretationOrchestrator, InterpreterMode};
use crate::queries::DependencyQueries;
use crate::utils::evaluation_context::EvaluationContext;

/// Result of running typed const evaluation.
#[derive(Debug, Default)]
pub struct ConstEvalOutcome {
    pub values: HashMap<thir::ty::DefId, Value>,
}

pub struct ConstEvaluationOrchestrator {
    interpreter: InterpretationOrchestrator,
    dependency_queries: DependencyQueries,
    eval_ctx: EvaluationContext,
    diagnostics: Option<Arc<DiagnosticManager>>,
}

impl ConstEvaluationOrchestrator {
    pub fn new(serializer: Arc<dyn fp_core::ast::AstSerializer>) -> Self {
        let _ = serializer; // serializer kept for parity with previous API (may be reused later)
        Self {
            interpreter: InterpretationOrchestrator::new(InterpreterMode::Const),
            dependency_queries: DependencyQueries::new(),
            eval_ctx: EvaluationContext::new(),
            diagnostics: None,
        }
    }

    pub fn with_diagnostics(mut self, manager: Arc<DiagnosticManager>) -> Self {
        self.diagnostics = Some(manager.clone());
        self.interpreter = self.interpreter.with_diagnostics(manager);
        self
    }

    pub fn evaluate(
        &mut self,
        program: &thir::Program,
        ctx: &SharedScopedContext,
    ) -> Result<ConstEvalOutcome> {
        info!("Starting typed const evaluation");

        self.eval_ctx.discover_const_blocks(program)?;
        let order = self
            .dependency_queries
            .compute_topological_order(self.eval_ctx.get_dependencies())?;

        for block_id in order {
            self.eval_ctx.begin_evaluation(block_id)?;
            let (body_id, _def_id) = {
                let block = self
                    .eval_ctx
                    .blocks()
                    .get(&block_id)
                    .expect("block must exist after discovery");
                (block.body_id, block.def_id)
            };

            let body = program.bodies.get(&body_id).ok_or_else(|| {
                fp_core::diagnostics::report_error(format!(
                    "Missing THIR body {:?} for const block",
                    body_id
                ))
            })?;

            let const_values = self.eval_ctx.clone_results();
            let value = self
                .interpreter
                .evaluate_body(body, program, ctx, &const_values)?;
            self.eval_ctx.set_block_result(block_id, value)?;
        }

        let values = self.eval_ctx.clone_results();

        info!("Typed const evaluation finished");

        Ok(ConstEvalOutcome { values })
    }
}
