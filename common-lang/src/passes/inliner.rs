use crate::context::ExecutionContext;
use crate::passes::OptimizePass;
use crate::tree::*;
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

    pub fn inline_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        match expr {
            Expr::Value(value) => self.inline_value(value, ctx).map(Expr::Value),
            Expr::Invoke(invoke) => self.inline_invoke(invoke, ctx),
            _ => Ok(expr),
        }
    }
    pub fn inline_invoke(&self, invoke: Invoke, ctx: &ExecutionContext) -> Result<Expr> {
        if invoke.args.is_empty() {
            let fun = self.try_get_expr(*invoke.func.clone(), ctx)?;
            match fun {
                Expr::Value(Value::Function(f)) => Ok(*f.body),
                _ => Ok(Expr::Invoke(invoke)),
            }
        } else {
            Ok(Expr::Invoke(invoke))
        }
    }
    pub fn try_get_ident(&self, ident: Ident, ctx: &ExecutionContext) -> Result<Expr> {
        match ctx.get_expr(ident.clone()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::Ident(ident)),
        }
    }
    pub fn try_get_path(&self, path: Path, ctx: &ExecutionContext) -> Result<Expr> {
        match ctx.get_expr(path.clone()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::path(path)),
        }
    }
    pub fn try_get_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        match expr {
            Expr::Ident(ident) => self.try_get_ident(ident, ctx),
            Expr::Path(path) => self.try_get_path(path, ctx),
            _ => Ok(expr),
        }
    }
    pub fn inline_value(&self, value: Value, ctx: &ExecutionContext) -> Result<Value> {
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
    fn optimize_expr_post(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        self.inline_expr(expr, ctx)
    }
}
