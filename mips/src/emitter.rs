use crate::register::RegisterId;
use eyre::{bail, Result};
use lang_core::context::SharedScopedContext;
use lang_core::expr::{Expr, Invoke};
use lang_core::ops::BinOpKind;
use lang_core::value::{Type, TypeInt, TypePrimitive, Value};
use proc_macro2::TokenStream;
use syn::__private::quote::quote;

pub struct MipsEmitter {}

impl MipsEmitter {
    pub fn new() -> Self {
        Self {}
    }
    pub fn emit_binop_int(
        &self,
        op: BinOpKind,
        lhs: RegisterId,
        rhs: RegisterId,
        ret: RegisterId,
        _ctx: &SharedScopedContext,
    ) -> Result<TokenStream> {
        let opcode = match op {
            BinOpKind::Add => quote!("add"),
            BinOpKind::Sub => quote!("sub"),
            BinOpKind::Mul => quote!("mul"),
            BinOpKind::Div => quote!("div"),
            BinOpKind::Mod => quote!("rem"),
            BinOpKind::BitXor => quote!("xor"),
            BinOpKind::BitAnd => quote!("and"),
            BinOpKind::BitOr => quote!("or"),

            _ => bail!("Unsupported binop {} with type int", op,),
        };

        Ok(quote!(
            #opcode #lhs, #rhs, #ret;
        ))
    }
    pub fn emit_binop(
        &self,
        op: BinOpKind,
        lhs: RegisterId,
        rhs: RegisterId,
        ret: RegisterId,
        ty: Type,
        _ctx: &SharedScopedContext,
    ) -> Result<TokenStream> {
        match ty {
            Type::Primitive(TypePrimitive::Int(TypeInt::I32)) => {
                self.emit_binop_int(op, lhs, rhs, ret, _ctx)
            }
            _ => bail!("Unsupported type {}", ty),
        }
    }
    pub fn emit_expr(&self, expr: &Expr, ctx: &SharedScopedContext) -> Result<TokenStream> {
        match expr {
            Expr::Invoke(Invoke { func, args: _ }) => match func.get() {
                Expr::Value(value) => match *value {
                    Value::BinOpKind(kind) => self.emit_binop(
                        kind,
                        1,
                        2,
                        3,
                        Type::Primitive(TypePrimitive::Int(TypeInt::I32)),
                        ctx,
                    ),
                    _ => bail!("Unsupported expr {}", func),
                },
                _ => bail!("Unsupported expr {}", func),
            },
            _ => bail!("Unsupported expr {}", expr),
        }
    }
}
