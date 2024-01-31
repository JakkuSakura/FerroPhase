mod value;

use crate::expr::Expr;
use crate::ops::{BinOpKind, UnOpKind};
use crate::ty::TypeValue;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::utils::to_json::ToJson;
use crate::{common_enum, get_threadlocal_serializer};
use common::bail;
use std::fmt::{Display, Formatter};
pub use value::*;

pub type ValueId = u64;

common_enum! {
    pub enum Value {
        Int(ValueInt),
        Bool(ValueBool),
        Decimal(ValueDecimal),
        Char(ValueChar),
        String(ValueString),
        List(ValueList),
        Bytes(ValueBytes),
        Pointer(ValuePointer),
        Offset(ValueOffset),
        Unit(ValueUnit),
        Null(ValueNull),
        None(ValueNone),
        Some(ValueSome),
        Option(ValueOption),
        Undefined(ValueUndefined),
        Escaped(ValueEscaped),
        Type(TypeValue),
        Struct(ValueStruct),
        Structural(ValueStructural),
        Function(ValueFunction),
        Tuple(ValueTuple),
        Expr(Expr),
        BinOpKind(BinOpKind),
        UnOpKind(UnOpKind),
        Any(AnyBox),
    }
}
impl Value {
    pub fn bool(b: bool) -> Value {
        Value::Bool(ValueBool::new(b))
    }
    pub fn decimal(d: f64) -> Value {
        Value::Decimal(ValueDecimal::new(d))
    }
    pub fn int(i: i64) -> Value {
        Value::Int(ValueInt::new(i))
    }
    pub fn unit() -> Value {
        Value::Unit(ValueUnit)
    }
    pub fn is_unit(&self) -> bool {
        match self {
            Value::Unit(_) => true,
            _ => false,
        }
    }
    pub fn null() -> Value {
        Value::Null(ValueNull)
    }
    pub fn expr(e: Expr) -> Self {
        match e {
            Expr::Value(v) => *v,
            _ => Value::Expr(e),
        }
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn undefined() -> Self {
        Self::Undefined(ValueUndefined)
    }
    pub fn as_structural(&self) -> Option<&ValueStructural> {
        match self {
            Value::Struct(s) => Some(&s.structural),
            Value::Structural(s) => Some(s),
            _ => None,
        }
    }
}
impl ToJson for Value {
    fn to_json(&self) -> common::Result<serde_json::Value> {
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
            Value::None(n) => n.to_json(),
            Value::Some(s) => s.to_json(),
            Value::Option(o) => o.to_json(),
            _ => bail!("cannot convert value to json: {:?}", self),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_value(self).unwrap();
        f.write_str(&s)
    }
}
