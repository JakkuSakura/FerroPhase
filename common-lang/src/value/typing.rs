use crate::ast::{AnyBox, AnyBoxable, Ident, Locator, Path, TypeExpr};
use crate::value::*;
use crate::{common_derives, common_enum, get_threadlocal_serializer};
use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

common_enum! {
    /// TypeValue is a solid type value
    pub enum TypeValue {
        Primitive(PrimitiveType),
        Struct(StructType),
        Structural(StructuralType),
        Enum(EnumType),
        Function(Box<FunctionType>),
        ImplTraits(ImplTraits),
        TypeBounds(TypeBounds),
        Value(Box<ValueType>),
        Tuple(TupleType),
        Vec(Box<VecType>),
        Any(AnyType),
        Unit(UnitType),
        Nothing(NothingType),
        Type(TypeType),
        Reference(Box<ReferenceType>),
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
    pub fn is_any(&self) -> bool {
        matches!(self, TypeValue::Any(_))
    }
    pub fn bool() -> TypeValue {
        TypeValue::Primitive(PrimitiveType::Bool)
    }
    pub fn expr(e: TypeExpr) -> Self {
        match e {
            TypeExpr::Value(v) => *v,
            _ => TypeValue::Expr(Box::new(e)),
        }
    }
    pub fn value(v: Value) -> Self {
        match v {
            Value::Expr(expr) => TypeValue::Expr(TypeExpr::Expr(expr).into()),
            _ => TypeValue::Value(ValueType::new(v).into()),
        }
    }
    pub fn path(path: Path) -> TypeValue {
        TypeValue::expr(TypeExpr::path(path))
    }
    pub fn ident(ident: Ident) -> TypeValue {
        TypeValue::expr(TypeExpr::ident(ident))
    }
    pub fn reference(ty: TypeValue) -> Self {
        TypeValue::Reference(
            ReferenceType {
                ty: Box::new(ty),
                mutability: None,
                lifetime: None,
            }
            .into(),
        )
    }
    pub fn any_box<T: AnyBoxable>(any: T) -> Self {
        Self::AnyBox(AnyBox::new(any))
    }

    pub fn impl_trait(name: Ident) -> Self {
        Self::ImplTraits(ImplTraits {
            bounds: TypeBounds::new(TypeExpr::ident(name)),
        })
    }
    pub fn locator(locator: Locator) -> Self {
        Self::expr(TypeExpr::Locator(locator))
    }
    pub fn type_bound(expr: TypeExpr) -> Self {
        Self::TypeBounds(TypeBounds::new(expr))
    }
    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            TypeValue::Struct(s) => Some(s),
            _ => None,
        }
    }
    pub fn unwrap_reference(&self) -> &TypeValue {
        match self {
            TypeValue::Reference(r) => &r.ty,
            _ => self,
        }
    }
}
impl Display for TypeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_type(self).unwrap();
        f.write_str(&s)
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
impl Display for IntType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntType::I64 => write!(f, "i64"),
            IntType::U64 => write!(f, "u64"),
            IntType::I32 => write!(f, "i32"),
            IntType::U32 => write!(f, "u32"),
            IntType::I16 => write!(f, "i16"),
            IntType::U16 => write!(f, "u16"),
            IntType::I8 => write!(f, "i8"),
            IntType::U8 => write!(f, "u8"),
            IntType::BigInt => write!(f, "bigint"),
        }
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
impl Display for DecimalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DecimalType::F64 => write!(f, "f64"),
            DecimalType::F32 => write!(f, "f32"),
            DecimalType::BigDecimal => write!(f, "bigdecimal"),
            DecimalType::Decimal { precision, scale } => {
                write!(f, "decimal({},{})", precision, scale)
            }
        }
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
impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&TypeValue::Primitive(*self))
            .unwrap();
        f.write_str(&s)
    }
}

common_derives! {
    pub struct VecType {
        pub ty: TypeValue,
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
    pub struct StructType {
        pub name: Ident,
        pub fields: Vec<FieldTypeValue>,
    }
}
common_derives! {
    pub struct EnumType {
        pub name: Ident,
        pub variants: Vec<EnumTypeVariant>,
    }
}

common_derives! {
    pub struct EnumTypeVariant {
        pub name: Ident,
        pub value: TypeValue,
    }
}

common_derives! {
    pub struct StructuralType {
        pub fields: Vec<FieldTypeValue>,
    }
}
common_derives! {
    pub struct FunctionType {
        pub params: Vec<TypeValue>,
        pub generics_params: Vec<GenericParam>,
        pub ret: TypeValue,
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
        if expr.is_any() {
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
impl Display for ReferenceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&TypeValue::Reference(self.clone().into()))
            .unwrap();

        f.write_str(&s)
    }
}

common_derives! {
    pub struct ValueType {
        pub value: Value,
    }
}
impl ValueType {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }
}
impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.value, f)
    }
}
