use crate::context::ExecutionContext;
use crate::passes::OptimizePass;
use crate::tree::*;
use crate::value::Value;
use crate::Serializer;
use common::*;
use std::rc::Rc;

pub struct Inliner {
    pub serializer: Rc<dyn Serializer>,
}
impl Inliner {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self { serializer }
    }

    pub fn inline_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        match expr {
            Expr::Ident(ident) => self.inline_ident(ident, ctx),
            Expr::Path(path) => self.inline_path(path, ctx),
            Expr::Value(value) => self.inline_value(value, ctx).map(Expr::Value),
            _ => Ok(expr),
        }
    }
    pub fn inline_ident(&self, ident: Ident, ctx: &ExecutionContext) -> Result<Expr> {
        match ctx.get_expr(ident.clone()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::Ident(ident)),
        }
    }
    pub fn inline_path(&self, path: Path, ctx: &ExecutionContext) -> Result<Expr> {
        match ctx.get_expr(path.clone()) {
            Some(expr) => Ok(expr),
            None => Ok(Expr::path(path)),
        }
    }
    pub fn inline_value(&self, value: Value, ctx: &ExecutionContext) -> Result<Value> {
        match value {
            Value::Expr(expr) => self.inline_expr(*expr, ctx).map(Value::expr),
            _ => Ok(value),
        }
    }

    pub fn inline_stmt(&self, stmt: Statement, ctx: &ExecutionContext) -> Result<Statement> {
        match stmt {
            Statement::Expr(expr) => {
                let expr = self.inline_expr(expr, ctx)?;
                Ok(Statement::Expr(expr))
            }
            Statement::Let(node) => {
                let value = self.inline_expr(node.value, ctx)?;
                Ok(Statement::Let(Let {
                    name: node.name.clone(),
                    ty: node.ty.clone(),
                    value,
                }))
            }
            _ => Ok(stmt.clone()),
        }
    }
}

impl OptimizePass for Inliner {
    fn optimize_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        self.inline_expr(expr, ctx)
    }
}
