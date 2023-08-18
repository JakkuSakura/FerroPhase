use crate::ast::*;
use crate::context::ScopedContext;
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
    fn optimize_item_pre(&self, item: Item, ctx: &ScopedContext) -> Result<Option<Item>> {
        let mut item = item;
        for pass in self {
            if let Some(new_item) = pass.optimize_item_pre(item, ctx)? {
                item = new_item;
            } else {
                return Ok(None);
            }
        }
        Ok(Some(item))
    }
    fn optimize_item_post(&self, item: Item, ctx: &ScopedContext) -> Result<Option<Item>> {
        let mut item = item;
        for pass in self {
            if let Some(new_item) = pass.optimize_item_post(item, ctx)? {
                item = new_item;
            } else {
                return Ok(None);
            }
        }
        Ok(Some(item))
    }
    fn optimize_expr_pre(&self, expr: Expr, ctx: &ScopedContext) -> Result<Expr> {
        let mut expr = expr;
        for pass in self {
            expr = pass.optimize_expr_pre(expr, ctx)?;
        }
        Ok(expr)
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ScopedContext) -> Result<Expr> {
        let mut expr = expr;
        for pass in self {
            expr = pass.optimize_expr_post(expr, ctx)?;
        }
        Ok(expr)
    }
    fn optimize_invoke_pre(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ScopedContext,
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
        func: &FunctionValue,
        ctx: &ScopedContext,
    ) -> Result<Invoke> {
        let mut invoke = invoke;
        for pass in self {
            invoke = pass.optimize_invoke_post(invoke, func, ctx)?;
        }
        Ok(invoke)
    }
}
