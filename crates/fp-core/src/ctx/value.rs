use crate::ast::AstValue;
use crate::ast::{AstExpr, ExprId};
use crate::ctx::Context;
use crate::error::Result;

pub trait ValueSystem {
    fn get_value_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<AstValue> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_value_from_expr_id(&self, ctx: &Context, id: ExprId) -> Result<AstValue> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}
impl ValueSystem for () {}
