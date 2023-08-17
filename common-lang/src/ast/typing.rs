use crate::ast::*;
use crate::ast::{Ident, Invoke};
use crate::ops::*;
use crate::value::*;

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeExpr {
    Ident(Ident),
    Path(Path),
    BinOp(TypeBinOp),
    Invoke(Invoke),
    SelfType(SelfType),
    Value(TypeValue),
}

impl TypeExpr {
    pub fn path(path: Path) -> TypeExpr {
        if path.segments.len() == 1 {
            TypeExpr::Ident(path.segments[0].clone())
        } else {
            TypeExpr::Path(path)
        }
    }
    pub fn ident(ident: Ident) -> TypeExpr {
        TypeExpr::Ident(ident)
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
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeBinOp {
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct SelfType {
    pub reference: bool,
    pub mutability: bool,
}
