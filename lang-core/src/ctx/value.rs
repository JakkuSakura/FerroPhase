use crate::ast::Value;
use crate::ast::{AstExpr, ExprId};
use crate::ctx::Context;
use eyre::Result;

pub trait ValueSystem {
    fn get_value_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<Value> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_value_from_expr_id(&self, ctx: &Context, id: ExprId) -> Result<Value> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}
impl ValueSystem for () {}
