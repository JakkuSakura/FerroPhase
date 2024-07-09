use crate::ast::*;
use crate::id::Ident;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};
use std::fmt::{Display, Formatter};
use std::hash::Hash;

pub type TypeId = u64;

common_enum! {
    /// TypeValue is a solid type value
    pub enum AstType {
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
        Unknown(TypeUnknown),
        Nothing(TypeNothing),
        Type(TypeType),
        Reference(Box<TypeReference>),
        Expr(Box<AstExpr>),
        AnyBox(AnyBox),
    }

}
impl AstType {
    pub const fn unit() -> AstType {
        AstType::Unit(TypeUnit)
    }
    pub const UNIT: AstType = AstType::Unit(TypeUnit);
    pub const fn any() -> AstType {
        AstType::Any(TypeAny)
    }
    pub const ANY: AstType = AstType::Any(TypeAny);
    pub const fn unknown() -> AstType {
        AstType::Unknown(TypeUnknown)
    }
    pub const UNKNOWN: AstType = AstType::Unknown(TypeUnknown);
    pub fn is_any(&self) -> bool {
        matches!(self, AstType::Any(_))
    }
    pub fn bool() -> AstType {
        AstType::Primitive(TypePrimitive::Bool)
    }
    pub fn expr(e: AstExpr) -> Self {
        match e {
            AstExpr::Value(ty) => Self::value(ty),
            _ => AstType::Expr(Box::new(e)),
        }
    }
    pub fn value(v: impl Into<Value>) -> Self {
        let v = v.into();
        match v {
            Value::Expr(expr) => Self::expr(*expr),
            Value::Type(ty) => ty,
            _ => AstType::Value(ValueType::new(v).into()),
        }
    }
    pub fn path(path: crate::id::Path) -> AstType {
        AstType::expr(AstExpr::path(path))
    }
    pub fn ident(ident: Ident) -> AstType {
        AstType::expr(AstExpr::ident(ident))
    }
    pub fn reference(ty: AstType) -> Self {
        AstType::Reference(
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
            bounds: TypeBounds::new(AstExpr::ident(name)),
        })
    }
    pub fn locator(locator: crate::id::Locator) -> Self {
        Self::expr(AstExpr::Locator(locator))
    }
    pub fn type_bound(expr: AstExpr) -> Self {
        Self::TypeBounds(TypeBounds::new(expr))
    }
    pub fn as_struct(&self) -> Option<&TypeStruct> {
        match self {
            AstType::Struct(s) => Some(s),
            _ => None,
        }
    }
    pub fn unwrap_reference(&self) -> &AstType {
        match self {
            AstType::Reference(r) => &r.ty,
            _ => self,
        }
    }
}
impl Display for AstType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_type(self).unwrap();
        f.write_str(&s)
    }
}

common_enum! {
    #[derive(Copy)]
    pub enum TypeInt {
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
impl Display for TypeInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInt::I64 => write!(f, "i64"),
            TypeInt::U64 => write!(f, "u64"),
            TypeInt::I32 => write!(f, "i32"),
            TypeInt::U32 => write!(f, "u32"),
            TypeInt::I16 => write!(f, "i16"),
            TypeInt::U16 => write!(f, "u16"),
            TypeInt::I8 => write!(f, "i8"),
            TypeInt::U8 => write!(f, "u8"),
            TypeInt::BigInt => write!(f, "bigint"),
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
        Int(TypeInt),
        Decimal(DecimalType),
        Bool,
        Char,
        String,
        List,
    }
}

impl TypePrimitive {
    pub fn i64() -> TypePrimitive {
        TypePrimitive::Int(TypeInt::I64)
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
            .serialize_type(&AstType::Primitive(*self))
            .unwrap();
        f.write_str(&s)
    }
}

common_struct! {
    pub struct TypeVec {
        pub ty: AstType,
    }
}

common_struct! {
    pub struct TypeTuple {
        pub types: Vec<AstType>,
    }
}

common_struct! {
    pub struct FieldTypeValue {
        pub name: Ident,
        pub value: AstType,
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
        pub value: AstType,
    }
}

common_struct! {
    pub struct TypeStructural {
        pub fields: Vec<FieldTypeValue>,
    }
}
common_struct! {
    pub struct TypeFunction {
        pub params: Vec<AstType>,
        pub generics_params: Vec<GenericParam>,
        pub ret: AstType,
    }
}
common_struct! {
    pub struct ImplTraits {
        pub bounds: TypeBounds,
    }
}

common_struct! {
    pub struct TypeBounds {
        pub bounds: Vec<AstExpr>,
    }
}
impl TypeBounds {
    pub fn any() -> Self {
        Self { bounds: vec![] }
    }
    pub fn new(expr: AstExpr) -> Self {
        TypeBounds { bounds: vec![expr] }
    }
}
macro_rules! plain_type {
    ($name: ident) => {
        common_struct! {
            pub struct $name;
        }
    };
}
plain_type! { TypeAny }
plain_type! { TypeUnit }
plain_type! { TypeUnknown }
plain_type! { TypeNothing }
plain_type! { TypeType }

common_struct! {
    pub struct TypeReference {
        pub ty: Box<AstType>,
        pub mutability: Option<bool>,
        pub lifetime: Option<Ident>,
    }
}
impl Display for TypeReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&AstType::Reference(self.clone().into()))
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
