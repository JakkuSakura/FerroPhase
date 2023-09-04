use crate::ast::*;
use crate::ast::{Ident, Invoke};
use crate::ops::*;
use crate::value::*;
use std::fmt::Display;
use std::hash::Hash;

common_derives! {
    /// TypeExpr is an expression that returns a type
    pub enum TypeExpr {
        Locator(Locator),
        BinOp(TypeBinOp),
        Invoke(Invoke),
        SelfType(SelfType),
        Value(TypeValue),
        Expr(Box<Expr>),
    }
}

impl TypeExpr {
    pub fn path(path: Path) -> TypeExpr {
        TypeExpr::Locator(Locator::path(path))
    }
    pub fn ident(ident: Ident) -> TypeExpr {
        TypeExpr::Locator(Locator::ident(ident))
    }
    pub fn unit() -> TypeExpr {
        TypeExpr::value(TypeValue::unit())
    }
    pub fn value(v: TypeValue) -> TypeExpr {
        match v {
            TypeValue::Expr(expr) => *expr,
            _ => TypeExpr::Value(v),
        }
    }
    pub fn type_bound(expr: TypeExpr) -> TypeExpr {
        TypeExpr::value(TypeValue::type_bound(expr))
    }
    pub fn as_locator(&self) -> Option<&Locator> {
        match self {
            TypeExpr::Locator(pat) => Some(pat),
            _ => None,
        }
    }
    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            TypeExpr::Value(TypeValue::Struct(struct_)) => Some(struct_),
            _ => None,
        }
    }
}
impl Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpr::Locator(locator) => Display::fmt(locator, f),
            // TypeExpr::BinOp(bin_op) => Display::fmt(bin_op, f),
            // TypeExpr::Invoke(invoke) => Display::fmt(invoke, f),
            TypeExpr::SelfType(self_type) => Display::fmt(self_type, f),
            // TypeExpr::Value(value) => Display::fmt(value, f),
            // TypeExpr::Expr(expr) => Display::fmt(expr, f),
            _ => panic!("cannot display type expr: {:?}", self),
        }
    }
}
common_derives! {
    pub enum TypeBinOp {
        Add(AddOp<TypeExpr>),
        Sub(SubOp<TypeExpr>),
    }

}
common_derives! {
    pub struct SelfType {
        pub reference: bool,
        pub mutability: bool,
    }
}
impl Display for SelfType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.reference {
            write!(f, "&")?;
        }
        if self.mutability {
            write!(f, "mut ")?;
        }
        write!(f, "Self")
    }
}
