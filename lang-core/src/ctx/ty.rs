use crate::ctx::Context;
use crate::expr::{Expr, ExprId};
use crate::value::{Type, Value};
use eyre::Result;

pub trait TypingSystem {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<Type> {
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

impl TypingSystem for () {}
