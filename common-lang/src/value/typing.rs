use crate::ast::{AnyBox, AnyBoxable, Ident, Path, TypeExpr};
use crate::common_derives;
use crate::value::*;
use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

common_derives! {
    /// TypeValue is a solid type value
    pub enum TypeValue {
        Primitive(PrimitiveType),
        NamedStruct(NamedStructType),
        UnnamedStruct(UnnamedStructType),
        Function(FunctionType),
        ImplTraits(ImplTraits),
        TypeBounds(TypeBounds),
        Literal(LiteralType),
        Tuple(TupleType),
        Vec(VecType),
        Any(AnyType),
        Unit(UnitType),
        Nothing(NothingType),
        Type(TypeType),
        Reference(ReferenceType),
        Expr(Box<TypeExpr>),
        AnyBox(AnyBox),
    }

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
        TypeValue::expr(TypeExpr::ident(ident))
    }
    pub fn reference(ty: TypeValue) -> Self {
        TypeValue::Reference(ReferenceType {
            ty: Box::new(ty),
            mutability: None,
            lifetime: None,
        })
    }
    pub fn any_box<T: AnyBoxable>(any: T) -> Self {
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
impl Display for TypeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeValue::Literal(value) => Display::fmt(value, f),
            _ => panic!("cannot display type value: {:?}", self),
        }
    }
}

common_derives! {
    #[derive(Copy)]
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
}
common_derives! {
    #[derive(Copy)]
    pub enum DecimalType {
        F64,
        F32,
        BigDecimal,
        Decimal { precision: u32, scale: u32 },
    }
}
common_derives! {
    #[derive(Copy)]
    pub enum PrimitiveType {
        Int(IntType),
        Decimal(DecimalType),
        Bool,
        Char,
        String,
        List,
    }
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

common_derives! {
    pub struct VecType {
        pub ty: Box<TypeValue>,
    }
}

common_derives! {
    pub struct TupleType {
        pub types: Vec<TypeValue>,
    }
}

common_derives! {
    pub struct FieldTypeValue {
        pub name: Ident,
        pub value: TypeValue,
    }
}
common_derives! {
    pub struct NamedStructType {
        pub name: Ident,
        pub fields: Vec<FieldTypeValue>,
    }
}
common_derives! {
    pub struct UnnamedStructType {
        pub fields: Vec<FieldTypeValue>,
    }
}
common_derives! {
    pub struct FunctionType {
        pub params: Vec<TypeValue>,
        pub generics_params: Vec<GenericParam>,
        pub ret: Box<TypeValue>,
    }
}
common_derives! {
    pub struct ImplTraits {
        pub bounds: TypeBounds,
    }
}

common_derives! {
    pub struct TypeBounds {
        pub bounds: Vec<TypeExpr>,
    }
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
macro_rules! plain_type {
    ($name: ident) => {
        common_derives! {
            pub struct $name;
        }
    };
}
plain_type! { AnyType }
plain_type! { UnitType }
plain_type! { NothingType }
plain_type! { TypeType }

common_derives! {
    pub struct ReferenceType {
        pub ty: Box<TypeValue>,
        pub mutability: Option<bool>,
        pub lifetime: Option<Ident>,
    }
}

common_derives! {
    pub struct LiteralType {
        pub value: Box<Value>,
    }
}
impl LiteralType {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }
}
impl Display for LiteralType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.value, f)
    }
}
