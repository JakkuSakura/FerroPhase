use crate::ast::*;
use crate::ops::{BinOpKind, UnOpKind};
use crate::value::{TypeBounds, TypeValue};
use common::*;
use serde_json::json;
use std::fmt::Debug;

pub trait ToJson {
    fn to_json(&self) -> Result<serde_json::Value>;
    fn to_value<T: DeserializeOwned>(&self) -> Result<T>
    where
        Self: Sized,
    {
        let json = self.to_json()?;
        Ok(serde_json::from_value(json)?)
    }
}
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
impl ToJson for Value {
    fn to_json(&self) -> Result<serde_json::Value> {
        match self {
            Value::Int(i) => i.to_json(),
            Value::Bool(b) => b.to_json(),
            Value::Decimal(d) => d.to_json(),
            Value::Char(c) => c.to_json(),
            Value::String(s) => s.to_json(),
            Value::List(l) => l.to_json(),
            Value::Unit(u) => u.to_json(),
            Value::Null(n) => n.to_json(),
            Value::Undefined(u) => u.to_json(),
            Value::Struct(s) => s.to_json(),
            Value::Tuple(t) => t.to_json(),
            _ => bail!("cannot convert value to json: {:?}", self),
        }
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
impl ToJson for IntValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(self.value))
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
impl ToJson for BoolValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(self.value))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecimalValue {
    pub value: f64,
}
impl PartialEq for DecimalValue {
    fn eq(&self, other: &Self) -> bool {
        self.value.total_cmp(&other.value) == std::cmp::Ordering::Equal
    }
}

impl Eq for DecimalValue {}
impl PartialOrd for DecimalValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.value.total_cmp(&other.value))
    }
}
impl Ord for DecimalValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.total_cmp(&other.value)
    }
}
impl DecimalValue {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}
impl ToJson for DecimalValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(self.value))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CharValue {
    pub value: char,
}
impl CharValue {
    pub fn new(v: char) -> Self {
        Self { value: v }
    }
}
impl ToJson for CharValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(self.value))
    }
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

impl ToJson for StringValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(self.value))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ListValue {
    pub values: Vec<Value>,
}
impl ListValue {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values }
    }
}
impl ToJson for ListValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        let values: Vec<_> = self.values.iter().map(|x| x.to_json()).try_collect()?;
        Ok(json!(values))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct UnitValue;
impl ToJson for UnitValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(null))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NullValue;
impl ToJson for NullValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(null))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct UndefinedValue;
impl ToJson for UndefinedValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        Ok(json!(null))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FieldValue {
    pub name: Ident,
    pub value: Value,
}
impl FieldValue {
    pub fn new(name: Ident, value: Value) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StructValue {
    pub name: TypeExpr, // either Ident or Struct
    pub fields: Vec<FieldValue>,
}
impl StructValue {
    pub fn new(name: TypeExpr, fields: Vec<FieldValue>) -> Self {
        Self { name, fields }
    }
}
impl ToJson for StructValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        let mut map = serde_json::Map::new();
        for field in &self.fields {
            map.insert(field.name.name.clone(), field.value.to_json()?);
        }
        Ok(json!(map))
    }
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
impl TupleValue {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values }
    }
}

impl ToJson for TupleValue {
    fn to_json(&self) -> Result<serde_json::Value> {
        let values: Vec<_> = self.values.iter().map(|x| x.to_json()).try_collect()?;
        Ok(json!(values))
    }
}
