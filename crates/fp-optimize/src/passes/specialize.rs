use crate::utils::OptimizePass;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;

pub struct SpecializePass {
    _spec_id: AtomicUsize,
    serializer: Arc<dyn AstSerializer>,
}

impl SpecializePass {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            _spec_id: AtomicUsize::default(),
            serializer,
        }
    }
}

impl OptimizePass for SpecializePass {
    fn name(&self) -> &str {
        "specialize"
    }

    fn optimize_expr(&self, expr: Expr, _ctx: &SharedScopedContext) -> Result<Expr> {
        let _ = &self.serializer;
        Ok(expr)
    }

    fn optimize_item(&self, item: Item, _ctx: &SharedScopedContext) -> Result<Item> {
        let _ = &self.serializer;
        Ok(item)
    }
}
