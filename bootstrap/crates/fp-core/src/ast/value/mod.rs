mod runtime;
mod ty;
mod value;

pub use runtime::*;
pub use ty::*;
pub use value::*;

use crate::ast::{get_threadlocal_serializer, BExpr, BlockStmt, Expr, ExprKind, Item};
use crate::ops::{BinOpKind, UnOpKind};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::span::Span;
use std::fmt::{Display, Formatter};

pub type ValueId = u64;
pub type BValue = Box<Value>;
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Value {
    Int(ValueInt),
    Bool(ValueBool),
    Decimal(ValueDecimal),
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

impl From<ValueInt> for Value {
    fn from(value: ValueInt) -> Self {
        Value::Int(value)
    }
}

impl From<ValueBool> for Value {
    fn from(value: ValueBool) -> Self {
        Value::Bool(value)
    }
}

impl From<ValueDecimal> for Value {
    fn from(value: ValueDecimal) -> Self {
        Value::Decimal(value)
    }
}

impl From<ValueChar> for Value {
    fn from(value: ValueChar) -> Self {
        Value::Char(value)
    }
}

impl From<ValueString> for Value {
    fn from(value: ValueString) -> Self {
        Value::String(value)
    }
}

impl From<ValueList> for Value {
    fn from(value: ValueList) -> Self {
        Value::List(value)
    }
}

impl From<ValueMap> for Value {
    fn from(value: ValueMap) -> Self {
        Value::Map(value)
    }
}

impl From<ValueBytes> for Value {
    fn from(value: ValueBytes) -> Self {
        Value::Bytes(value)
    }
}

impl From<ValuePointer> for Value {
    fn from(value: ValuePointer) -> Self {
        Value::Pointer(value)
    }
}

impl From<ValueOffset> for Value {
    fn from(value: ValueOffset) -> Self {
        Value::Offset(value)
    }
}

impl From<ValueUnit> for Value {
    fn from(value: ValueUnit) -> Self {
        Value::Unit(value)
    }
}

impl From<ValueNull> for Value {
    fn from(value: ValueNull) -> Self {
        Value::Null(value)
    }
}

impl From<ValueNone> for Value {
    fn from(value: ValueNone) -> Self {
        Value::None(value)
    }
}

impl From<ValueSome> for Value {
    fn from(value: ValueSome) -> Self {
        Value::Some(value)
    }
}

impl From<ValueOption> for Value {
    fn from(value: ValueOption) -> Self {
        Value::Option(value)
    }
}

impl From<ValueUndefined> for Value {
    fn from(value: ValueUndefined) -> Self {
        Value::Undefined(value)
    }
}

impl From<ValueEscaped> for Value {
    fn from(value: ValueEscaped) -> Self {
        Value::Escaped(value)
    }
}

impl From<Ty> for Value {
    fn from(value: Ty) -> Self {
        Value::Type(value)
    }
}

impl From<ValueStruct> for Value {
    fn from(value: ValueStruct) -> Self {
        Value::Struct(value)
    }
}

impl From<ValueStructural> for Value {
    fn from(value: ValueStructural) -> Self {
        Value::Structural(value)
    }
}

impl From<ValueFunction> for Value {
    fn from(value: ValueFunction) -> Self {
        Value::Function(value)
    }
}

impl From<ValueTuple> for Value {
    fn from(value: ValueTuple) -> Self {
        Value::Tuple(value)
    }
}

impl From<ValueQuoteToken> for Value {
    fn from(value: ValueQuoteToken) -> Self {
        Value::QuoteToken(value)
    }
}

impl From<ValueTokenStream> for Value {
    fn from(value: ValueTokenStream) -> Self {
        Value::TokenStream(value)
    }
}

impl From<BExpr> for Value {
    fn from(value: BExpr) -> Self {
        Value::Expr(value)
    }
}

impl From<BinOpKind> for Value {
    fn from(value: BinOpKind) -> Self {
        Value::BinOpKind(value)
    }
}

impl From<UnOpKind> for Value {
    fn from(value: UnOpKind) -> Self {
        Value::UnOpKind(value)
    }
}

impl From<AnyBox> for Value {
    fn from(value: AnyBox) -> Self {
        Value::Any(value)
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
    pub fn int(i: i64) -> Value {
        Value::Int(ValueInt::new(i))
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
