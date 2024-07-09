mod inline;
mod interpret;
mod optimizer;
mod specialize;

pub use inline::*;
pub use interpret::*;
pub use optimizer::*;
pub use specialize::*;

use common::*;
use lang_core::ast::Value;
use lang_core::ast::{AstExpr, ControlFlow, ExprInvoke};
use lang_core::ast::{AstItem, Module};
use lang_core::context::SharedScopedContext;

#[allow(unused_variables)]
pub trait OptimizePass {
    fn name(&self) -> &str;

    fn optimize_item(&self, item: AstItem, ctx: &SharedScopedContext) -> Result<AstItem> {
        Ok(item)
    }

    fn try_evaluate_expr(&self, pat: &AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        Ok(pat.clone())
    }
    fn optimize_expr(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
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
    ) -> Result<AstExpr> {
        Ok(invoke.into())
    }
    fn evaluate_condition(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
}

pub struct NoopPass;
impl OptimizePass for NoopPass {
    fn name(&self) -> &str {
        "noop"
    }
}
