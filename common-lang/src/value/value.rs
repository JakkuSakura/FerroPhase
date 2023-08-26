use crate::ast::*;
use crate::ops::{BinOpKind, UnOpKind};
use crate::value::{TypeBounds, TypeValue};
use common::{Deserialize, Serialize};
use std::fmt::Debug;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Value {
    Int(IntValue),
    Bool(BoolValue),
    Decimal(DecimalValue),
    Char(CharValue),
    String(StringValue),
    List(ListValue),
    Unit(UnitValue),
    Null(NullValue),
    Undefined(UndefinedValue),
    Type(TypeValue),
    Struct(StructValue),
    Function(FunctionValue),
    Tuple(TupleValue),
    Expr(Box<Expr>),
    BinOpKind(BinOpKind),
    UnOpKind(UnOpKind),
    Any(AnyBox),
}
impl Value {
    pub fn bool(b: bool) -> Value {
        Value::Bool(BoolValue::new(b))
    }
    pub fn decimal(d: f64) -> Value {
        Value::Decimal(DecimalValue::new(d))
    }
    pub fn int(i: i64) -> Value {
        Value::Int(IntValue::new(i))
    }
    pub fn unit() -> Value {
        Value::Unit(UnitValue)
    }
    pub fn null() -> Value {
        Value::Null(NullValue)
    }
    pub fn expr(e: Expr) -> Self {
        match e {
            Expr::Value(v) => v,
            _ => Value::Expr(Box::new(e)),
        }
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn undefined() -> Self {
        Self::Undefined(UndefinedValue)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Ord, PartialOrd)]
pub struct IntValue {
    pub value: i64,
}

impl IntValue {
    pub fn new(i: i64) -> Self {
        Self { value: i }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Ord, PartialOrd)]
pub struct BoolValue {
    pub value: bool,
}

impl BoolValue {
    pub fn new(i: bool) -> Self {
        Self { value: i }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DecimalValue {
    pub value: f64,
}

impl Eq for DecimalValue {}

impl DecimalValue {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CharValue {
    pub value: char,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StringValue {
    pub value: String,
    pub owned: bool,
}

impl StringValue {
    pub fn new_owned(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: true,
        }
    }
    pub fn new_ref(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ListValue {
    pub values: Vec<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct UnitValue;
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NullValue;
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct UndefinedValue;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FieldValue {
    pub name: Ident,
    pub value: Value,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StructValue {
    pub name: TypeExpr, // either Ident or Struct
    pub fields: Vec<FieldValue>,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Ident,
    pub ty: TypeValue,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GenericParam {
    pub name: Ident,
    pub bounds: TypeBounds,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionValue {
    pub name: Option<Ident>,
    pub params: Vec<FunctionParam>,
    pub generics_params: Vec<GenericParam>,
    pub ret: TypeValue,
    pub body: Box<Expr>,
}
impl FunctionValue {
    pub fn is_runtime_only(&self) -> bool {
        self.generics_params.is_empty()
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TupleValue {
    pub values: Vec<Value>,
}
