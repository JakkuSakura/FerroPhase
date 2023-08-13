use crate::tree::{Ident, RequireTraits, TypeExpr};
use common::*;

/// TypeValue is a solid type value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeValue {
    Primitive(PrimitiveType),
    NamedStruct(NamedStructType),
    UnnamedStruct(UnnamedStructType),
    Function(FunctionType),
    RequireTraits(RequireTraits),
    Tuple(TupleType),
    Vec(VecType),
    Expr(Box<TypeExpr>),
}
impl TypeValue {
    pub fn unit() -> TypeValue {
        TypeValue::Primitive(PrimitiveType::Unit)
    }
    pub fn bool() -> TypeValue {
        TypeValue::Primitive(PrimitiveType::Bool)
    }
    pub fn expr(e: TypeExpr) -> Self {
        match e {
            TypeExpr::Value(v) => v,
            _ => TypeValue::Expr(Box::new(e)),
        }
    }
}
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum PrimitiveType {
    I64,
    F64,
    Bool,
    Unit,
    Char,
    String,
    Type,
    List,
}

impl PrimitiveType {
    pub fn i64() -> PrimitiveType {
        PrimitiveType::I64
    }
    pub fn f64() -> PrimitiveType {
        PrimitiveType::F64
    }
    pub fn bool() -> PrimitiveType {
        PrimitiveType::Bool
    }
    pub fn ty() -> PrimitiveType {
        PrimitiveType::Type
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VecType {
    pub ty: Box<TypeValue>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TupleType {
    pub types: Vec<TypeValue>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldTypeValue {
    pub name: Ident,
    pub value: TypeValue,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamedStructType {
    pub name: Ident,
    pub fields: Vec<FieldTypeValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnnamedStructType {
    pub fields: Vec<FieldTypeValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub params: Vec<TypeValue>,
    pub ret: Box<TypeValue>,
}
