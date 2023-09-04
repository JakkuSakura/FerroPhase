use crate::ast::*;
use crate::ast::{Ident, Invoke};
use crate::ops::*;
use crate::value::*;
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
