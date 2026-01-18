use crate::ast::*;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};
use crate::span::Span;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

pub type TypeId = u64;
pub type BType = Box<Ty>;
common_struct! {
    /// Type of a quoted fragment token.
    /// - `kind` records the fragment kind (expr/stmt/item/type).
    /// - `item` refines item fragments (fn/struct/enum/...) when present.
    /// - `inner` may carry the inner expression/type when applicable (e.g., expr quoting).
    pub struct TypeQuote {
        pub kind: QuoteFragmentKind,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub item: Option<QuoteItemKind>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub inner: Option<Box<Ty>>,
    }
}

common_enum! {
    /// Binary type-level operator (e.g. A + B).
    #[derive(Copy)]
    pub enum TypeBinaryOpKind {
        Add,
        /// Union / alternative branches of types.
        Union,
        /// Intersection of struct-like types (common fields only).
        Intersect,
        /// Field removal from a struct-like type.
        Subtract,
    }
}

common_struct! {
    /// Symbolic type-level binary operation over types.
    ///
    /// This is represented directly in the `Ty` enum via
    /// `Ty::TypeBinaryOp` and interpreted by later phases that
    /// understand how to combine the operand types.
    pub struct TypeBinaryOp {
        pub kind: TypeBinaryOpKind,
        pub lhs: BType,
        pub rhs: BType,
    }
}
common_enum! {
    /// TypeValue is a solid type value
    pub enum Ty {
        Primitive(TypePrimitive),
        TokenStream(TypeTokenStream),
        Struct(TypeStruct),
        Structural(TypeStructural),
        Enum(TypeEnum),
        Function(TypeFunction),
        ImplTraits(ImplTraits),
        TypeBounds(TypeBounds),
        Value(TypeValue),
        Tuple(TypeTuple),
        Vec(TypeVec),
        Array(TypeArray),
        Any(TypeAny),
        Unit(TypeUnit),
        Unknown(TypeUnknown),
        Nothing(TypeNothing),
        Type(TypeType),
        Reference(TypeReference),
        Slice(TypeSlice),
        Expr(BExpr),
        Quote(TypeQuote),
        TypeBinaryOp(Box<TypeBinaryOp>),
        AnyBox(AnyBox),
    }

}
impl Ty {
    pub const fn unit() -> Ty {
        Ty::Unit(TypeUnit)
    }
    pub const UNIT: Ty = Ty::Unit(TypeUnit);
    pub const fn any() -> Ty {
        Ty::Any(TypeAny)
    }
    pub const ANY: Ty = Ty::Any(TypeAny);
    pub const fn unknown() -> Ty {
        Ty::Unknown(TypeUnknown)
    }
    pub const UNKNOWN: Ty = Ty::Unknown(TypeUnknown);
    pub fn is_any(&self) -> bool {
        matches!(self, Ty::Any(_))
    }
    pub fn bool() -> Ty {
        Ty::Primitive(TypePrimitive::Bool)
    }
    pub fn expr(expr: Expr) -> Self {
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::Value(value) => Self::value(*value),
            other => Ty::Expr(Box::new(Expr::from_parts(ty, other))),
        }
    }
    pub fn value(v: impl Into<Value>) -> Self {
        let v = v.into();
        match v {
            Value::Expr(expr) => Self::expr(*expr),
            Value::Type(ty) => ty,
            _ => Ty::Value(TypeValue::new(v).into()),
        }
    }
    pub fn path(path: Path) -> Ty {
        Ty::expr(Expr::path(path))
    }
    pub fn ident(ident: Ident) -> Ty {
        Ty::expr(Expr::ident(ident))
    }
    pub fn reference(ty: Ty) -> Self {
        Ty::Reference(
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
            bounds: TypeBounds::new(Expr::ident(name)),
        })
    }
    pub fn locator(locator: Locator) -> Self {
        Self::expr(Expr::locator(locator))
    }
    pub fn type_bound(expr: Expr) -> Self {
        Self::TypeBounds(TypeBounds::new(expr))
    }
    pub fn as_struct(&self) -> Option<&TypeStruct> {
        match self {
            Ty::Struct(s) => Some(s),
            _ => None,
        }
    }
    pub fn unwrap_reference(&self) -> &Ty {
        match self {
            Ty::Reference(r) => &r.ty,
            _ => self,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Ty::TokenStream(_) => Span::null(),
            Ty::Struct(ty) => ty.span(),
            Ty::Structural(ty) => ty.span(),
            Ty::Enum(ty) => ty.span(),
            Ty::Function(ty) => ty.span(),
            Ty::ImplTraits(ty) => ty.span(),
            Ty::TypeBounds(ty) => ty.span(),
            Ty::Value(ty) => ty.span(),
            Ty::Tuple(ty) => ty.span(),
            Ty::Vec(ty) => ty.span(),
            Ty::Array(ty) => ty.span(),
            Ty::Reference(ty) => ty.span(),
            Ty::Slice(ty) => ty.span(),
            Ty::Expr(expr) => expr.span(),
            Ty::Quote(ty) => ty.span(),
            Ty::TypeBinaryOp(op) => op.span(),
            _ => Span::null(),
        }
    }
}
impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(serializer) = try_get_threadlocal_serializer() {
            match serializer.serialize_type(self) {
                Ok(s) => f.write_str(&s),
                Err(_) => write!(f, "{:?}", self),
            }
        } else {
            // Fallback when no serializer is registered
            write!(f, "{:?}", self)
        }
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
        if let Some(serializer) = try_get_threadlocal_serializer() {
            match serializer.serialize_type(&Ty::Primitive(*self)) {
                Ok(s) => f.write_str(&s),
                Err(_) => write!(f, "{:?}", self),
            }
        } else {
            write!(f, "{:?}", self)
        }
    }
}

common_struct! {
    pub struct TypeVec {
        pub ty: BType,
    }
}

common_struct! {
    pub struct TypeArray {
        pub elem: BType,
        pub len: BExpr,
    }
}

common_struct! {
    pub struct TypeTuple {
        pub types: Vec<Ty>,
    }
}

common_struct! {
    pub struct StructuralField {
        pub name: Ident,
        pub value: Ty,
    }
}
impl StructuralField {
    pub fn new(name: Ident, value: Ty) -> Self {
        Self { name, value }
    }
}
common_struct! {
    pub struct TypeStruct {
        pub name: Ident,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub generics_params: Vec<GenericParam>,
        pub fields: Vec<StructuralField>,
    }
}
common_struct! {
    pub struct TypeEnum {
        pub name: Ident,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub generics_params: Vec<GenericParam>,
        pub variants: Vec<EnumTypeVariant>,
    }
}

common_struct! {
    pub struct EnumTypeVariant {
        pub name: Ident,
        pub value: Ty,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub discriminant: Option<BExpr>,
    }
}

common_struct! {
    pub struct TypeStructural {
        pub fields: Vec<StructuralField>,
    }
}
common_struct! {
    pub struct TypeFunction {
        pub params: Vec<Ty>,
        pub generics_params: Vec<GenericParam>,
        pub ret_ty: Option<BType>,
    }
}
common_struct! {
    pub struct ImplTraits {
        pub bounds: TypeBounds,
    }
}

common_struct! {
    pub struct TypeBounds {
        pub bounds: Vec<Expr>,
    }
}
impl TypeBounds {
    pub fn any() -> Self {
        Self { bounds: vec![] }
    }
    pub fn new(expr: Expr) -> Self {
        TypeBounds { bounds: vec![expr] }
    }

    pub fn span(&self) -> Span {
        Span::union(self.bounds.iter().map(Expr::span))
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
plain_type! { TypeTokenStream }
plain_type! { TypeUnit }
plain_type! { TypeUnknown }
plain_type! { TypeNothing }
plain_type! { TypeType }

common_struct! {
    pub struct TypeReference {
        pub ty: BType,
        pub mutability: Option<bool>,
        pub lifetime: Option<Ident>,
    }
}
impl TypeReference {
    pub fn span(&self) -> Span {
        self.ty.span()
    }
}
impl Display for TypeReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_type(&Ty::Reference(self.clone().into()))
            .unwrap();
        f.write_str(&s)
    }
}

common_struct! {
    pub struct TypeValue {
        pub value: BValue,
    }
}
impl TypeValue {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn span(&self) -> Span {
        self.value.span()
    }
}
impl Display for TypeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.value, f)
    }
}

common_struct! {
    pub struct TypeSlice {
        pub elem: BType,
    }
}
impl TypeSlice {
    pub fn span(&self) -> Span {
        self.elem.span()
    }
}

impl TypeBinaryOp {
    pub fn span(&self) -> Span {
        Span::union([self.lhs.span(), self.rhs.span()])
    }
}

impl TypeVec {
    pub fn span(&self) -> Span {
        self.ty.span()
    }
}

impl TypeArray {
    pub fn span(&self) -> Span {
        Span::union([self.elem.span(), self.len.span()])
    }
}

impl TypeTuple {
    pub fn span(&self) -> Span {
        Span::union(self.types.iter().map(Ty::span))
    }
}

impl StructuralField {
    pub fn span(&self) -> Span {
        self.value.span()
    }
}

impl TypeStruct {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(StructuralField::span))
    }
}

impl EnumTypeVariant {
    pub fn span(&self) -> Span {
        Span::union(
            [
                Some(self.value.span()),
                self.discriminant.as_ref().map(|expr| expr.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

impl TypeEnum {
    pub fn span(&self) -> Span {
        Span::union(self.variants.iter().map(EnumTypeVariant::span))
    }
}

impl TypeStructural {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(StructuralField::span))
    }
}

impl TypeFunction {
    pub fn span(&self) -> Span {
        Span::union(
            self.params
                .iter()
                .map(Ty::span)
                .chain(self.ret_ty.as_ref().map(|ty| ty.span())),
        )
    }
}

impl TypeQuote {
    pub fn span(&self) -> Span {
        Span::union(self.inner.as_ref().map(|ty| ty.span()))
    }
}

impl ImplTraits {
    pub fn span(&self) -> Span {
        self.bounds.span()
    }
}
