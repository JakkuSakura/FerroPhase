use fp_core::ast::{Ty, TypeEnum, TypePrimitive, TypeStruct, TypeStructural};

#[derive(Clone, Debug)]
pub(crate) struct TypeScheme {
    #[allow(dead_code)]
    pub(crate) vars: usize,
    pub(crate) body: SchemeType,
}

#[derive(Clone, Debug)]
pub(crate) enum SchemeType {
    Var(u32),
    Primitive(TypePrimitive),
    Unit,
    Nothing,
    Tuple(Vec<SchemeType>),
    Function(Vec<SchemeType>, Box<SchemeType>),
    Struct(TypeStruct),
    Structural(TypeStructural),
    Enum(TypeEnum),
    // Union of two types from `A | B` in type expressions.
    Union(Box<SchemeType>, Box<SchemeType>),
    Slice(Box<SchemeType>),
    Vec(Box<SchemeType>),
    Array(Box<SchemeType>),
    Reference(Box<SchemeType>),
    Boxed(Box<SchemeType>),
    Any,
    Custom(Ty),
    Unknown,
}
