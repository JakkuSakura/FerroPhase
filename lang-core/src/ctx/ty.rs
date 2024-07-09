use crate::ast::{AstExpr, ExprId};
use crate::ast::{Type, Value};
use crate::ctx::Context;
use eyre::Result;

pub trait TypeSystem {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<Type> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_ty_from_expr_id(&self, ctx: &Context, id: ExprId) -> Result<Type> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_ty_from_value(&self, ctx: &Context, value: &Value) -> Result<Type> {
        let _ = ctx;
        let _ = value;
        unimplemented!()
    }
    fn get_ty_from_value_id(&self, ctx: &Context, id: u32) -> Result<Type> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}

impl TypeSystem for () {}
