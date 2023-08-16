use crate::context::ExecutionContext;
use crate::tree::*;
use crate::*;
use common::*;

mod inliner;
mod specializer;
pub use inliner::*;
pub use specializer::*;
#[allow(unused_variables)]
pub trait OptimizePass {
    fn name(&self) -> &str;
    fn optimize_item_pre(&self, item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        Ok(Some(item))
    }
    fn optimize_item_post(&self, item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        Ok(Some(item))
    }
    fn optimize_expr_pre(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        Ok(expr)
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        Ok(expr)
    }
}
