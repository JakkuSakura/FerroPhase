use crate::ast::{AstExpr, ExprId};
use crate::ast::{AstType, Value};
use crate::ctx::Context;
use eyre::Result;

pub trait TypeSystem {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<AstType> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_ty_from_expr_id(&self, ctx: &Context, id: ExprId) -> Result<AstType> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_ty_from_value(&self, ctx: &Context, value: &Value) -> Result<AstType> {
        let _ = ctx;
        let _ = value;
        unimplemented!()
    }
    fn get_ty_from_value_id(&self, ctx: &Context, id: u32) -> Result<AstType> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}

impl TypeSystem for () {}
