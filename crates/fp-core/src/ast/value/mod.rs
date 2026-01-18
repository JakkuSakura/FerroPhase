mod runtime;
mod ty;
mod value;

pub use runtime::*;
pub use ty::*;
pub use value::*;

use crate::ast::{get_threadlocal_serializer, BExpr, BlockStmt, Expr, ExprKind, Item};
use crate::common_enum;
use crate::ops::{BinOpKind, UnOpKind};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::utils::to_json::ToJson;
use crate::span::Span;
use std::fmt::{Display, Formatter};

pub type ValueId = u64;
pub type BValue = Box<Value>;
common_enum! {
    pub enum Value {
        Int(ValueInt),
        BigInt(ValueBigInt),
        Bool(ValueBool),
        Decimal(ValueDecimal),
        BigDecimal(ValueBigDecimal),
        Char(ValueChar),
        String(ValueString),
        List(ValueList),
        Map(ValueMap),
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
        Type(Ty),
        Struct(ValueStruct),
        Structural(ValueStructural),
        Function(ValueFunction),
        Tuple(ValueTuple),
        QuoteToken(ValueQuoteToken),
        TokenStream(ValueTokenStream),
        Expr(BExpr),
        BinOpKind(BinOpKind),
        UnOpKind(UnOpKind),
        Any(AnyBox),
    }
}
impl Value {
    pub fn get(&self) -> Self {
        self.clone()
    }

    pub fn bool(b: bool) -> Value {
        Value::Bool(ValueBool::new(b))
    }
    pub fn decimal(d: f64) -> Value {
        Value::Decimal(ValueDecimal::new(d))
    }
    pub fn big_decimal(d: bigdecimal::BigDecimal) -> Value {
        Value::BigDecimal(ValueBigDecimal::new(d))
    }
    pub fn int(i: i64) -> Value {
        Value::Int(ValueInt::new(i))
    }
    pub fn big_int(i: num_bigint::BigInt) -> Value {
        Value::BigInt(ValueBigInt::new(i))
    }
    pub fn unit() -> Value {
        Value::Unit(ValueUnit)
    }
    pub fn string(s: String) -> Value {
        Value::String(ValueString::new_owned(s))
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
    pub const NULL: Value = Value::Null(ValueNull);

    pub fn map(pairs: impl IntoIterator<Item = (Value, Value)>) -> Value {
        Value::Map(ValueMap::from_pairs(pairs))
    }

    pub fn expr(e: impl Into<Expr>) -> Self {
        let expr: Expr = e.into();
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::Value(v) => *v,
            other => Value::Expr(Expr::from_parts(ty, other).into()),
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
            Value::Struct(s) => Some(&s.structural),
            Value::Structural(s) => Some(s),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Value::Expr(expr) => expr.span(),
            Value::List(list) => span_union(list.values.iter().map(Value::span)),
            Value::Tuple(tuple) => span_union(tuple.values.iter().map(Value::span)),
            Value::Map(map) => span_union(
                map.entries
                    .iter()
                    .flat_map(|entry| [entry.key.span(), entry.value.span()]),
            ),
            Value::Struct(value_struct) => value_struct.span(),
            Value::Structural(structural) => structural.span(),
            Value::Some(some) => some.value.span(),
            Value::Option(option) => option
                .value
                .as_ref()
                .map(|value| value.span())
                .unwrap_or_else(Span::null),
            Value::QuoteToken(token) => token.span(),
            Value::TokenStream(stream) => stream.span(),
            _ => Span::null(),
        }
    }
}
impl ToJson for Value {
    fn to_json(&self) -> crate::Result<serde_json::Value> {
        match self {
            Value::Int(i) => i.to_json(),
            Value::Bool(b) => b.to_json(),
            Value::Decimal(d) => d.to_json(),
            Value::BigDecimal(d) => d.to_json(),
            Value::BigInt(i) => i.to_json(),
            Value::Char(c) => c.to_json(),
            Value::String(s) => s.to_json(),
            Value::List(l) => l.to_json(),
            Value::Map(m) => m.to_json(),
            Value::Unit(u) => u.to_json(),
            Value::Null(n) => n.to_json(),
            Value::Undefined(u) => u.to_json(),
            Value::Struct(s) => s.to_json(),
            Value::Tuple(t) => t.to_json(),
            Value::None(n) => n.to_json(),
            Value::Some(s) => s.to_json(),
            Value::Option(o) => o.to_json(),
            Value::TokenStream(_) => bail!("cannot convert token stream to json"),
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

fn span_union<I>(spans: I) -> Span
where
    I: IntoIterator<Item = Span>,
{
    Span::union(spans)
}

impl ValueStruct {
    pub fn span(&self) -> Span {
        self.structural.span()
    }
}

impl ValueStructural {
    pub fn span(&self) -> Span {
        span_union(self.fields.iter().map(ValueField::span))
    }
}

impl ValueField {
    pub fn span(&self) -> Span {
        self.value.span()
    }
}

impl ValueMapEntry {
    pub fn span(&self) -> Span {
        span_union([self.key.span(), self.value.span()])
    }
}

impl ValueQuoteToken {
    pub fn span(&self) -> Span {
        match &self.value {
            QuoteTokenValue::Expr(expr) => expr.span(),
            QuoteTokenValue::Stmts(stmts) => span_union(stmts.iter().map(BlockStmt::span)),
            QuoteTokenValue::Items(items) => span_union(items.iter().map(Item::span)),
            QuoteTokenValue::Type(_) => Span::null(),
        }
    }
}
impl From<BValue> for Value {
    fn from(e: BValue) -> Self {
        *e
    }
}
