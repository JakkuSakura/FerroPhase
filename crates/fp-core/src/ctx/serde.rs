use crate::ast::AstExpr;
use crate::error::Result;

use crate::ast::{AstType, AstValue};
use crate::ctx::Context;

pub trait SerializeSystem {
    fn get_serialized_from_ty(&self, ctx: &Context, ty: &AstType) -> Result<String> {
        let _ = ctx;
        let _ = ty;
        unimplemented!()
    }
    fn get_serialized_from_ty_id(&self, ctx: &Context, id: u32) -> Result<String> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_serialized_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<String> {
        let _ = ctx;
        let _ = expr;
        unimplemented!()
    }
    fn get_serialized_from_expr_id(&self, ctx: &Context, id: u32) -> Result<String> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_serialized_from_value(&self, ctx: &Context, value: &AstValue) -> Result<String> {
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
    fn get_ty_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<AstType> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_ty_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<AstType> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_expr_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<AstExpr> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_expr_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<AstExpr> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
    fn get_value_from_serialized(&self, ctx: &Context, serialized: &str) -> Result<AstValue> {
        let _ = ctx;
        let _ = serialized;
        unimplemented!()
    }
    fn get_value_from_serialized_id(&self, ctx: &Context, id: u32) -> Result<AstValue> {
        let _ = ctx;
        let _ = id;
        unimplemented!()
    }
}
impl DeserializeSystem for () {}
