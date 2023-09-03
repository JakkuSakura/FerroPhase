use crate::ast::*;
use crate::ast::{Ident, Invoke};
use crate::ops::*;
use crate::value::*;

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum TypeExpr {
    Pat(Pat),
    BinOp(TypeBinOp),
    Invoke(Invoke),
    SelfType(SelfType),
    Value(TypeValue),
}

impl TypeExpr {
    pub fn path(path: Path) -> TypeExpr {
        TypeExpr::Pat(Pat::path(path))
    }
    pub fn ident(ident: Ident) -> TypeExpr {
        TypeExpr::Pat(Pat::ident(ident))
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
    pub fn as_pat(&self) -> Option<&Pat> {
        match self {
            TypeExpr::Pat(pat) => Some(pat),
            _ => None,
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum TypeBinOp {
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct SelfType {
    pub reference: bool,
    pub mutability: bool,
}
