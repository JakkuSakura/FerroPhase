use crate::expr::*;
use crate::id::Locator;
use crate::ty::{TypeStruct, TypeValue};
use crate::{common_enum, common_struct};
use std::fmt::Display;
use std::hash::Hash;

common_enum! {
    /// TypeExpr is an expression that returns a type
    pub enum TypeExpr {
        Locator(Locator),
        BinOp(Box<TypeBinOp>),
        Invoke(Box<Invoke>),
        SelfType(Box<SelfType>),
        Value(Box<TypeValue>),
        Expr(Expr),
    }
}

impl TypeExpr {
    pub fn path(path: crate::id::Path) -> TypeExpr {
        TypeExpr::Locator(Locator::path(path))
    }
    pub fn ident(ident: crate::id::Ident) -> TypeExpr {
        TypeExpr::Locator(Locator::ident(ident))
    }
    pub fn unit() -> TypeExpr {
        TypeExpr::value(TypeValue::unit())
    }
    pub fn value(v: TypeValue) -> TypeExpr {
        match v {
            TypeValue::Expr(expr) => *expr,
            _ => TypeExpr::Value(v.into()),
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
    pub fn as_struct(&self) -> Option<&TypeStruct> {
        match self {
            TypeExpr::Value(v) => v.as_struct(),
            _ => None,
        }
    }
    pub fn is_any(&self) -> bool {
        match self {
            TypeExpr::Value(v) => v.is_any(),
            _ => false,
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
            TypeExpr::Value(value) => Display::fmt(value, f),
            // TypeExpr::Expr(expr) => Display::fmt(expr, f),
            _ => panic!("cannot display type expr: {:?}", self),
        }
    }
}

common_enum! {
    pub enum TypeBinOp {
        Add {
            left: TypeExpr,
            right: TypeExpr,
        },
        Sub {
            left: TypeExpr,
            right: TypeExpr,
        },
    }

}
common_struct! {
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
