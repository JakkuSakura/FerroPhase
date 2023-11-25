use crate::ast::*;
use crate::context::ArcScopedContext;
use crate::value::ValueFunction;
use common::*;

mod inline;
mod interpret;
mod specialize;

pub use inline::*;
pub use interpret::*;
pub use specialize::*;

// TODO: merge pre and post to simplify the pass
#[allow(unused_variables)]
pub trait OptimizePass {
    fn name(&self) -> &str;
    fn optimize_item_pre(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        Ok(item)
    }
    fn optimize_item_post(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        Ok(item)
    }
    fn optimize_expr_pre(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        Ok(expr)
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        Ok(expr)
    }
    fn optimize_module_pre(&self, module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        Ok(module)
    }
    fn optimize_module_post(&self, module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        Ok(module)
    }
    fn optimize_invoke_pre(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        Ok(invoke)
    }
    fn optimize_invoke_post(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        Ok(invoke)
    }
    fn evaluate_condition(
        &self,
        expr: Expr,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        Ok(None)
    }
    fn evaluate_invoke(
        &self,
        invoke: Invoke,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        Ok(None)
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        Ok(pat.clone())
    }
    fn optimize_bin_op(&self, invoke: Invoke, ctx: &ArcScopedContext) -> Result<Expr> {
        Ok(Expr::Invoke(invoke.into()))
    }
}

pub struct NoopPass;
impl OptimizePass for NoopPass {
    fn name(&self) -> &str {
        "noop"
    }
}
pub type MultiplePass = Vec<Box<dyn OptimizePass>>;
impl OptimizePass for MultiplePass {
    fn name(&self) -> &str {
        "multiple"
    }
    fn optimize_item_pre(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        let mut item = item;
        for pass in self {
            item = pass.optimize_item_pre(item, ctx)?;
        }
        Ok(item)
    }
    fn optimize_item_post(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        let mut item = item;
        for pass in self {
            item = pass.optimize_item_post(item, ctx)?;
        }
        Ok(item)
    }
    fn optimize_expr_pre(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        let mut expr = expr;
        for pass in self {
            expr = pass.optimize_expr_pre(expr, ctx)?;
        }
        Ok(expr)
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        let mut expr = expr;
        for pass in self {
            expr = pass.optimize_expr_post(expr, ctx)?;
        }
        Ok(expr)
    }
    fn optimize_invoke_pre(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        let mut invoke = invoke;
        for pass in self {
            invoke = pass.optimize_invoke_pre(invoke, func, ctx)?;
        }
        Ok(invoke)
    }
    fn optimize_invoke_post(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        let mut invoke = invoke;
        for pass in self {
            invoke = pass.optimize_invoke_post(invoke, func, ctx)?;
        }
        Ok(invoke)
    }
    fn evaluate_condition(
        &self,
        expr: Expr,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        // don't know what to do if multiple passes return different results
        for pass in self {
            if let Some(flow) = pass.evaluate_condition(expr.clone(), ctx)? {
                return Ok(Some(flow));
            }
        }
        Ok(None)
    }
    fn try_evaluate_expr(&self, pat: &Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        let mut pat = pat.clone();
        for pass in self {
            pat = pass.try_evaluate_expr(&pat, ctx)?;
        }
        Ok(pat)
    }
    fn optimize_module_pre(&self, module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        let mut module = module;
        for pass in self {
            module = pass.optimize_module_pre(module, ctx)?;
        }
        Ok(module)
    }
    fn optimize_module_post(&self, module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        let mut module = module;
        for pass in self {
            module = pass.optimize_module_post(module, ctx)?;
        }
        Ok(module)
    }
    fn evaluate_invoke(
        &self,
        invoke: Invoke,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        // don't know what to do if multiple passes return different results
        for pass in self {
            if let Some(flow) = pass.evaluate_invoke(invoke.clone(), ctx)? {
                return Ok(Some(flow));
            }
        }
        Ok(None)
    }
    fn optimize_bin_op(&self, invoke: Invoke, ctx: &ArcScopedContext) -> Result<Expr> {
        // don't know what to do if multiple passes return different results
        let invoke = invoke;
        for pass in self {
            return pass.optimize_bin_op(invoke, ctx);
        }
        Ok(Expr::Invoke(invoke.into()))
    }
}
