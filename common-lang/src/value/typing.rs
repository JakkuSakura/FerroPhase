use crate::tree::{AnyBox, Ident, Path, TypeExpr};
use crate::value::GenericParam;
use common::*;
use std::fmt::Debug;

/// TypeValue is a solid type value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeValue {
    Primitive(PrimitiveType),
    NamedStruct(NamedStructType),
    UnnamedStruct(UnnamedStructType),
    Function(FunctionType),
    ImplTraits(ImplTraits),
    TypeBounds(TypeBounds),
    Tuple(TupleType),
    Vec(VecType),
    Any(AnyType),
    Unit(UnitType),
    Nothing(NothingType),
    Type(TypeType),
    Expr(Box<TypeExpr>),
    AnyBox(AnyBox),
}
impl TypeValue {
    pub fn unit() -> TypeValue {
        TypeValue::Unit(UnitType)
    }
    pub fn any() -> TypeValue {
        TypeValue::Any(AnyType)
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
    pub fn path(path: Path) -> TypeValue {
        TypeValue::expr(TypeExpr::path(path))
    }
    pub fn ident(ident: Ident) -> TypeValue {
        TypeValue::expr(TypeExpr::Ident(ident))
    }
    pub fn any_box<T: Debug + 'static>(any: T) -> Self {
        Self::AnyBox(AnyBox::new(any))
    }

    pub fn impl_trait(name: Ident) -> Self {
        Self::ImplTraits(ImplTraits {
            bounds: TypeBounds::new(TypeExpr::ident(name)),
        })
    }
    pub fn type_bound(expr: TypeExpr) -> Self {
        Self::TypeBounds(TypeBounds::new(expr))
    }
}
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum IntType {
    I64,
    U64,
    I32,
    U32,
    I16,
    U16,
    I8,
    U8,
    BigInt,
}
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum DecimalType {
    F64,
    F32,
    BigDecimal,
    Decimal { precision: u32, scale: u32 },
}
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum PrimitiveType {
    Int(IntType),
    Decimal(DecimalType),
    Bool,
    Char,
    String,
    List,
}

impl PrimitiveType {
    pub fn i64() -> PrimitiveType {
        PrimitiveType::Int(IntType::I64)
    }
    pub fn f64() -> PrimitiveType {
        PrimitiveType::Decimal(DecimalType::F64)
    }
    pub fn bool() -> PrimitiveType {
        PrimitiveType::Bool
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
    pub generics_params: Vec<GenericParam>,
    pub ret: Box<TypeValue>,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct ImplTraits {
    pub bounds: TypeBounds,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct TypeBounds {
    pub bounds: Vec<TypeExpr>,
}
impl TypeBounds {
    pub fn any() -> Self {
        Self { bounds: vec![] }
    }
    pub fn new(expr: TypeExpr) -> Self {
        if matches!(expr, TypeExpr::Value(TypeValue::Any(_))) {
            return TypeBounds::any();
        }
        TypeBounds { bounds: vec![expr] }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnyType;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitType;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NothingType;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeType;
