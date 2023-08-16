use crate::context::ScopedContext;
use crate::tree::*;
use common::*;

mod inliner;
mod specializer;
use crate::value::FunctionValue;
pub use inliner::*;
pub use specializer::*;

#[allow(unused_variables)]
pub trait OptimizePass {
    fn name(&self) -> &str;
    fn optimize_item_pre(&self, item: Item, ctx: &ScopedContext) -> Result<Option<Item>> {
        Ok(Some(item))
    }
    fn optimize_item_post(&self, item: Item, ctx: &ScopedContext) -> Result<Option<Item>> {
        Ok(Some(item))
    }
    fn optimize_expr_pre(&self, expr: Expr, ctx: &ScopedContext) -> Result<Expr> {
        Ok(expr)
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ScopedContext) -> Result<Expr> {
        Ok(expr)
    }

    fn optimize_invoke_pre(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ScopedContext,
    ) -> Result<Invoke> {
        Ok(invoke)
    }
    fn optimize_invoke_post(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ScopedContext,
    ) -> Result<Invoke> {
        Ok(invoke)
    }
    fn optimize_block_pre(&self, block: Block, ctx: &ScopedContext) -> Result<Expr> {
        Ok(Expr::block(block))
    }
    fn optimize_block_post(&self, block: Block, ctx: &ScopedContext) -> Result<Expr> {
        Ok(Expr::block(block))
    }
}
