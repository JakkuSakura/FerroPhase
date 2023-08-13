use crate::ops::*;
use crate::tree::*;
use crate::value::*;

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeExpr {
    Ident(Ident),
    Path(Path),
    Op(TypeOp),
    Invoke(InvokeExpr),
    Value(TypeValue),
}

impl TypeExpr {
    pub fn unit() -> TypeExpr {
        TypeExpr::value(TypeValue::Primitive(PrimitiveType::Unit))
    }
    pub fn value(v: TypeValue) -> TypeExpr {
        match v {
            TypeValue::Expr(expr) => *expr,
            _ => TypeExpr::Value(v),
        }
    }
}
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct RequireTrait {
    pub name: Ident,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct RequireTraits {
    pub traits: Vec<RequireTrait>,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeOp {
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
}
