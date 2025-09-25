use crate::utils::OptimizePass;
// Replace common::* with specific imports
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::id::Locator;
use std::sync::Arc;
use tracing::warn;

pub struct InlinePass {
    pub serializer: Arc<dyn AstSerializer>,
}
impl InlinePass {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self { serializer }
    }

    pub fn inline_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        match expr {
            Expr::Value(value) => self.inline_value(value.into(), ctx).map(Expr::value),
            _ => Ok(expr),
        }
    }
    pub fn inline_invoke(
        &self,
        mut invoke: ExprInvoke,
        func: &Value,
        _ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => {
                if let Some(name) = &func.name {
                    match name.as_str() {
                        "print" => {
                            invoke.target = Locator::ident(name.clone()).into();
                            return Ok(Expr::Invoke(invoke.into()));
                        }
                        _ if invoke.args.is_empty() => return Ok(func.body.get()),
                        _ => {}
                    };
                }
            }
            Value::BinOpKind(kind) => {
                warn!("TODO: inline binop {:?}", kind);
            }
            _ => {}
        }

        Ok(Expr::Invoke(invoke.into()))
    }
    pub fn try_get_pat(&self, ident: Locator, ctx: &SharedScopedContext) -> Result<Expr> {
        match ctx.get_expr(ident.to_path()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::Locator(ident)),
        }
    }

    pub fn try_get_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        match expr {
            Expr::Locator(ident) => self.try_get_pat(ident, ctx),
            _ => Ok(expr),
        }
    }
    pub fn inline_value(&self, value: Value, ctx: &SharedScopedContext) -> Result<Value> {
        match value {
            Value::Expr(expr) => self.inline_expr(expr.get(), ctx).map(Value::expr),
            _ => Ok(value),
        }
    }
}

impl OptimizePass for InlinePass {
    fn name(&self) -> &str {
        "inline"
    }
    fn evaluate_invoke(
        &self,
        invoke: ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        if invoke.args.is_empty() {
            Ok(ControlFlow::Into)
        } else {
            Ok(ControlFlow::Continue)
        }
    }
    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        self.inline_invoke(invoke, func, ctx)
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        self.inline_expr(expr, ctx)
    }
}
