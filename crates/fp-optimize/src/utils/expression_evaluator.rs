use std::sync::Arc;

use fp_core::ast::{AstSerializer, Expr, Node, Value};
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;

use crate::passes::SpecializePass;
use crate::utils::optimizer::FoldOptimizer;

/// Placeholder expression evaluator for the typed interpreter migration.
pub struct ExpressionEvaluator {
    pub opt: FoldOptimizer,
}

impl ExpressionEvaluator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        // For now we load the default optimizer stack, though it currently only forwards expressions.
        let opt = FoldOptimizer::new(
            serializer.clone(),
            Box::new(SpecializePass::new(serializer)),
        );
        Self { opt }
    }

    pub fn interpret_tree(&self, _node: Node, _ctx: &SharedScopedContext) -> Result<Value> {
        Err(crate::error::optimization_error(
            "typed expression evaluator not implemented yet",
        ))
    }

    pub fn interpret_expr(&self, _node: Expr, _ctx: &SharedScopedContext) -> Result<Value> {
        Err(crate::error::optimization_error(
            "typed expression evaluator not implemented yet",
        ))
    }
}
