use crate::ops::*;
use crate::tree::*;
use crate::value::*;

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeExpr {
    Ident(Ident),
    Path(Path),
    Op(TypeOp),
    Invoke(Invoke),
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
pub struct ImplTrait {
    pub name: Ident,
}
impl ImplTrait {
    pub fn new(name: Ident) -> Self {
        Self { name }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct ImplTraits {
    pub traits: Vec<ImplTrait>,
}
impl ImplTraits {
    pub fn new(traits: Vec<ImplTrait>) -> Self {
        Self { traits }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeOp {
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
}
