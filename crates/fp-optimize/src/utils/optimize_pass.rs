// OptimizePass trait - interface for optimization passes

use fp_core::ast::Value;
use fp_core::ast::{ControlFlow, Expr, ExprInvoke};
use fp_core::ast::{Item, Module};
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;

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

    fn evaluate_invoke(
        &self,
        invoke: ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        Ok(ControlFlow::Continue)
    }

    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
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
