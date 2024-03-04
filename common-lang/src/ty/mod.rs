pub mod system;
use crate::expr::TypeExpr;
use crate::id::Ident;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::value::*;
use crate::{common_enum, common_struct, get_threadlocal_serializer};
use common::*;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
pub type TypeId = u64;

common_enum! {
    /// TypeValue is a solid type value
    pub enum TypeValue {
        Primitive(TypePrimitive),
        Struct(TypeStruct),
        Structural(TypeStructural),
        Enum(TypeEnum),
        Function(Box<TypeFunction>),
        ImplTraits(ImplTraits),
        TypeBounds(TypeBounds),
        Value(Box<ValueType>),
        Tuple(TypeTuple),
        Vec(Box<TypeVec>),
        Any(TypeAny),
        Unit(TypeUnit),
        Nothing(TypeNothing),
        Type(TypeType),
        Reference(Box<TypeReference>),
        Expr(Box<TypeExpr>),
        AnyBox(AnyBox),
    }

}
impl TypeValue {
    pub const fn unit() -> TypeValue {
        TypeValue::Unit(TypeUnit {})
    }
    pub const UNIT: TypeValue = TypeValue::Unit(TypeUnit {});
    pub const fn any() -> TypeValue {
        TypeValue::Any(TypeAny {})
    }
    pub const ANY: TypeValue = TypeValue::Any(TypeAny {});

    pub fn is_any(&self) -> bool {
        matches!(self, TypeValue::Any(_))
    }
    pub fn bool() -> TypeValue {
        TypeValue::Primitive(TypePrimitive::Bool)
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
    pub fn path(path: crate::id::Path) -> TypeValue {
        TypeValue::expr(TypeExpr::path(path))
    }
    pub fn ident(ident: Ident) -> TypeValue {
        TypeValue::expr(TypeExpr::ident(ident))
    }
    pub fn reference(ty: TypeValue) -> Self {
        TypeValue::Reference(
            TypeReference {
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
    pub fn locator(locator: crate::id::Locator) -> Self {
        Self::expr(TypeExpr::Locator(locator))
    }
    pub fn type_bound(expr: TypeExpr) -> Self {
        Self::TypeBounds(TypeBounds::new(expr))
    }
    pub fn as_struct(&self) -> Option<&TypeStruct> {
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

common_enum! {
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
common_enum! {
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
common_enum! {
    #[derive(Copy)]
    pub enum TypePrimitive {
        Int(IntType),
        Decimal(DecimalType),
        Bool,
        Char,
        String,
        List,
    }
}

impl TypePrimitive {
    pub fn i64() -> TypePrimitive {
        TypePrimitive::Int(IntType::I64)
    }
    pub fn f64() -> TypePrimitive {
        TypePrimitive::Decimal(DecimalType::F64)
    }
    pub fn bool() -> TypePrimitive {
        TypePrimitive::Bool
    }
}
impl Display for TypePrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&TypeValue::Primitive(*self))
            .unwrap();
        f.write_str(&s)
    }
}

common_struct! {
    pub struct TypeVec {
        pub ty: TypeValue,
    }
}

common_struct! {
    pub struct TypeTuple {
        pub types: Vec<TypeValue>,
    }
}

common_struct! {
    pub struct FieldTypeValue {
        pub name: Ident,
        pub value: TypeValue,
    }
}
common_struct! {
    pub struct TypeStruct {
        pub name: Ident,
        pub fields: Vec<FieldTypeValue>,
    }
}
common_struct! {
    pub struct TypeEnum {
        pub name: Ident,
        pub variants: Vec<EnumTypeVariant>,
    }
}

common_struct! {
    pub struct EnumTypeVariant {
        pub name: Ident,
        pub value: TypeValue,
    }
}

common_struct! {
    pub struct TypeStructural {
        pub fields: Vec<FieldTypeValue>,
    }
}
common_struct! {
    pub struct TypeFunction {
        pub params: Vec<TypeValue>,
        pub generics_params: Vec<GenericParam>,
        pub ret: TypeValue,
    }
}
common_struct! {
    pub struct ImplTraits {
        pub bounds: TypeBounds,
    }
}

common_struct! {
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
        common_struct! {
            pub struct $name {}
        }
    };
}
plain_type! { TypeAny }
plain_type! { TypeUnit }
plain_type! { TypeNothing }
plain_type! { TypeType }

common_struct! {
    pub struct TypeReference {
        pub ty: Box<TypeValue>,
        pub mutability: Option<bool>,
        pub lifetime: Option<Ident>,
    }
}
impl Display for TypeReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&TypeValue::Reference(self.clone().into()))
            .unwrap();

        f.write_str(&s)
    }
}

common_struct! {
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
