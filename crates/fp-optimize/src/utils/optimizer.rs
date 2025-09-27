use crate::passes::{InlinePass, SpecializePass};
use crate::utils::OptimizePass;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use std::sync::Arc;

pub fn load_optimizers(serializer: Arc<dyn AstSerializer>) -> Vec<FoldOptimizer> {
    let optimizers: Vec<Box<dyn OptimizePass>> = vec![
        Box::new(SpecializePass::new(serializer.clone())),
        Box::new(InlinePass::new(serializer.clone())),
    ];

    optimizers
        .into_iter()
        .map(|pass| FoldOptimizer::new(serializer.clone(), pass))
        .collect()
}

pub struct FoldOptimizer {
    serializer: Arc<dyn AstSerializer>,
    pass: Box<dyn OptimizePass>,
}

impl FoldOptimizer {
    pub fn new(serializer: Arc<dyn AstSerializer>, pass: Box<dyn OptimizePass>) -> Self {
        Self { serializer, pass }
    }

    pub fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        let _ = &self.serializer;
        self.pass.optimize_expr(expr, ctx)
    }

    pub fn optimize_item(&self, item: Item, ctx: &SharedScopedContext) -> Result<Item> {
        self.pass.optimize_item(item, ctx)
    }
}
