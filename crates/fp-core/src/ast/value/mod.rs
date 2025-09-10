mod ty;
mod value;

pub use ty::*;
pub use value::*;

use crate::ast::{get_threadlocal_serializer, AstExpr, BExpr};
use crate::common_enum;
use crate::ops::{BinOpKind, UnOpKind};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::utils::to_json::ToJson;
use crate::bail;
use std::fmt::{Display, Formatter};

pub type ValueId = u64;
pub type BValue = Box<AstValue>;
common_enum! {
    pub enum AstValue {
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
        Type(AstType),
        Struct(ValueStruct),
        Structural(ValueStructural),
        Function(ValueFunction),
        Tuple(ValueTuple),
        Expr(BExpr),
        BinOpKind(BinOpKind),
        UnOpKind(UnOpKind),
        Any(AnyBox),
    }
}
impl AstValue {
    pub fn get(&self) -> Self {
        self.clone()
    }

    pub fn bool(b: bool) -> AstValue {
        AstValue::Bool(ValueBool::new(b))
    }
    pub fn decimal(d: f64) -> AstValue {
        AstValue::Decimal(ValueDecimal::new(d))
    }
    pub fn int(i: i64) -> AstValue {
        AstValue::Int(ValueInt::new(i))
    }
    pub fn unit() -> AstValue {
        AstValue::Unit(ValueUnit)
    }
    pub fn is_unit(&self) -> bool {
        match self {
            AstValue::Unit(_) => true,
            _ => false,
        }
    }
    pub fn null() -> AstValue {
        AstValue::Null(ValueNull)
    }
    pub const NULL: AstValue = AstValue::Null(ValueNull);

    pub fn expr(e: impl Into<AstExpr>) -> Self {
        match e.into() {
            AstExpr::Value(v) => *v,
            e => AstValue::Expr(e.into()),
        }
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub const fn undefined() -> Self {
        Self::Undefined(ValueUndefined)
    }
    pub const UNDEFINED: Self = Self::Undefined(ValueUndefined);
    pub fn as_structural(&self) -> Option<&ValueStructural> {
        match self {
            AstValue::Struct(s) => Some(&s.structural),
            AstValue::Structural(s) => Some(s),
            _ => None,
        }
    }
}
impl ToJson for AstValue {
    fn to_json(&self) -> crate::Result<serde_json::Value> {
        match self {
            AstValue::Int(i) => i.to_json(),
            AstValue::Bool(b) => b.to_json(),
            AstValue::Decimal(d) => d.to_json(),
            AstValue::Char(c) => c.to_json(),
            AstValue::String(s) => s.to_json(),
            AstValue::List(l) => l.to_json(),
            AstValue::Unit(u) => u.to_json(),
            AstValue::Null(n) => n.to_json(),
            AstValue::Undefined(u) => u.to_json(),
            AstValue::Struct(s) => s.to_json(),
            AstValue::Tuple(t) => t.to_json(),
            AstValue::None(n) => n.to_json(),
            AstValue::Some(s) => s.to_json(),
            AstValue::Option(o) => o.to_json(),
            _ => bail!("cannot convert value to json: {:?}", self),
        }
    }
}
impl Display for AstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_value(self).unwrap();
        f.write_str(&s)
    }
}
impl From<BValue> for AstValue {
    fn from(e: BValue) -> Self {
        *e
    }
}
