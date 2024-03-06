use crate::context::SharedScopedContext;
use crate::expr::*;
use common::*;

mod inline;
mod interpret;
mod specialize;

use crate::ast::{Item, Module};
use crate::value::Value;
pub use inline::*;
pub use interpret::*;
pub use specialize::*;

#[allow(unused_variables)]
pub trait OptimizePass {
    fn name(&self) -> &str;

    fn optimize_item(&self, item: Item, ctx: &SharedScopedContext) -> Result<Item> {
        Ok(item)
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        Ok(pat.clone())
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        Ok(expr)
    }

    fn optimize_module(&self, module: Module, ctx: &SharedScopedContext) -> Result<Module> {
        Ok(module)
    }
    fn evaluate_invoke(&self, invoke: Invoke, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        Ok(ControlFlow::Continue)
    }
    fn optimize_invoke(
        &self,
        invoke: Invoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        Ok(invoke.into())
    }
    fn evaluate_condition(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
}

pub struct NoopPass;
impl OptimizePass for NoopPass {
    fn name(&self) -> &str {
        "noop"
    }
}
