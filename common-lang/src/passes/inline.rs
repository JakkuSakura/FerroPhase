use crate::ast::*;
use crate::context::ArcScopedContext;
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
            Expr::Value(value) => self.inline_value(value, ctx).map(Expr::value),
            Expr::Invoke(invoke) => self.inline_invoke(invoke, ctx),
            _ => Ok(expr),
        }
    }
    pub fn inline_invoke(&self, mut invoke: Invoke, ctx: &ArcScopedContext) -> Result<Expr> {
        if invoke.args.is_empty() {
            let fun = self.try_get_expr(*invoke.func.clone(), ctx)?;
            match fun {
                Expr::Value(Value::Function(f)) => {
                    if let Some(name) = &f.name {
                        match name.as_str() {
                            "print" => {
                                invoke.func = Expr::ident(name.clone()).into();
                                return Ok(Expr::Invoke(invoke));
                            }
                            _ => {}
                        }
                    }
                    Ok(*f.body)
                }
                _ => Ok(Expr::Invoke(invoke)),
            }
        } else {
            Ok(Expr::Invoke(invoke))
        }
    }
    pub fn try_get_pat(&self, ident: Locator, ctx: &ArcScopedContext) -> Result<Expr> {
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
            Value::Expr(expr) => self.inline_expr(*expr, ctx).map(Value::expr),
            _ => Ok(value),
        }
    }
}

impl OptimizePass for InlinePass {
    fn name(&self) -> &str {
        "inline"
    }
    fn optimize_expr_post(&self, expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        self.inline_expr(expr, ctx)
    }
}
