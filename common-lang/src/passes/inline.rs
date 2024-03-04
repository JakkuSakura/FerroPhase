use crate::context::ArcScopedContext;
use crate::expr::*;
use crate::passes::OptimizePass;
use crate::value::Value;
use crate::Serializer;
use common::*;
use std::rc::Rc;

pub struct InlinePass {
    pub serializer: Rc<dyn Serializer>,
}
impl InlinePass {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self { serializer }
    }

    pub fn inline_expr(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        match expr {
            Expr::Value(value) => self.inline_value(*value, ctx).map(Expr::value),
            _ => Ok(expr),
        }
    }
    pub fn inline_invoke(
        &self,
        mut invoke: Invoke,
        func: &Value,
        _ctx: &ArcScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => {
                if let Some(name) = &func.name {
                    match name.as_str() {
                        "print" => {
                            invoke.func = Expr::ident(name.clone()).into();
                            return Ok(Expr::Invoke(invoke.into()));
                        }
                        _ if invoke.args.is_empty() => return Ok(func.body.clone()),
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
    pub fn try_get_pat(&self, ident: crate::id::Locator, ctx: &ArcScopedContext) -> Result<Expr> {
        match ctx.get_expr(ident.clone()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::Locator(ident)),
        }
    }

    pub fn try_get_expr(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        match expr {
            Expr::Locator(ident) => self.try_get_pat(ident, ctx),
            _ => Ok(expr),
        }
    }
    pub fn inline_value(&self, value: Value, ctx: &ArcScopedContext) -> Result<Value> {
        match value {
            Value::Expr(expr) => self.inline_expr(expr, ctx).map(Value::expr),
            _ => Ok(value),
        }
    }
}

impl OptimizePass for InlinePass {
    fn name(&self) -> &str {
        "inline"
    }
    fn evaluate_invoke(&self, invoke: Invoke, _ctx: &ArcScopedContext) -> Result<ControlFlow> {
        if invoke.args.is_empty() {
            Ok(ControlFlow::Into)
        } else {
            Ok(ControlFlow::Continue)
        }
    }
    fn optimize_invoke(
        &self,
        invoke: Invoke,
        func: &Value,
        ctx: &ArcScopedContext,
    ) -> Result<Expr> {
        self.inline_invoke(invoke, func, ctx)
    }
    fn optimize_expr(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        self.inline_expr(expr, ctx)
    }
}
