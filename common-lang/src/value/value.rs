use crate::tree::*;
use crate::value::TypeValue;
use common::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    Int(IntValue),
    Bool(BoolValue),
    Decimal(DecimalValue),
    Char(CharValue),
    String(StringValue),
    List(ListValue),
    Unit(UnitValue),
    Type(TypeValue),
    Struct(StructExpr),
    Function(FunctionExpr),
    Tuple(TupleValue),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
pub struct IntValue {
    pub value: i64,
}

impl IntValue {
    pub fn new(i: i64) -> Self {
        Self { value: i }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
pub struct BoolValue {
    pub value: bool,
}

impl BoolValue {
    pub fn new(i: bool) -> Self {
        Self { value: i }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialOrd, PartialEq)]
pub struct DecimalValue {
    pub value: f64,
}

impl DecimalValue {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CharValue {
    pub value: char,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListValue {
    pub value: Vec<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitValue;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldValue {
    pub name: Ident,
    pub value: Value,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructValue {
    pub name: TypeExpr, // either Ident or Struct
    pub fields: Vec<FieldValue>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionValue {
    pub params: Vec<ParamExpr>,
    pub ret: TypeValue,
    pub body: Block,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TupleValue {
    pub values: Vec<Value>,
}
