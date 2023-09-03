use crate::ast::*;
use crate::ast::{Ident, Invoke};
use crate::ops::*;
use crate::value::*;
use std::hash::{Hash, Hasher};

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum TypeExpr {
    Locator(Locator),
    BinOp(TypeBinOp),
    Invoke(Invoke),
    SelfType(SelfType),
    Value(TypeValue),
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
}
impl Hash for TypeExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypeExpr::Value(value) => value.hash(state),
            _ => panic!("cannot hash type expr: {:?}", self),
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
