use crate::ast::Expr;
use crate::error::Result;

use crate::ast::{Ty, Value};
use crate::ctx::Context;

pub trait SerializeSystem {
    fn get_serialized_from_ty(&self, ctx: &Context, ty: &Ty) -> Result<String> {
        let _ = ctx;
        let _ = ty;
        unimplemented!()
    }
    fn get_serialized_from_ty_id(&self, ctx: &Context, id: u32) -> Result<String> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_serialized_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<String> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_serialized_from_expr_id(&self, ctx: &Context, id: u32) -> Result<String> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_serialized_from_value(&self, ctx: &Context, value: &Value) -> Result<String> {
        let _ = ctx;
        let _ = value;
        unimplemented!()
    }
    fn get_serialized_from_value_id(&self, ctx: &Context, id: u32) -> Result<String> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}

impl SerializeSystem for () {}

pub trait DeserializeSystem {
    fn get_ty_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<Ty> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_ty_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<Ty> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_expr_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<Expr> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_expr_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<Expr> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_value_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<Value> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_value_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<Value> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}
impl DeserializeSystem for () {}
