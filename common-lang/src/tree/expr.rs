use crate::ops::BuiltinFn;
use crate::tree::*;
use crate::value::{UnitValue, Value};
use serde::{Deserialize, Serialize};

/// Expr is an expression that returns a value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Ident(Ident),
    Path(Path),
    Value(Value),
    Block(Block),
    Cond(Cond),
    Invoke(Invoke),
    BuiltinFn(BuiltinFn),
    Select(Select),
    Reference(Reference),
    Any(AnyBox),
}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(UnitValue))
    }
    pub fn value(v: Value) -> Expr {
        match v {
            Value::Unit(_) => Expr::unit(),
            _ => Expr::Value(v),
        }
    }
    pub fn any<T: Debug + 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invoke {
    pub fun: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum SelectType {
    Unknown,
    Field,
    Method,
    Function,
    Const,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Select {
    pub obj: Box<Expr>,
    pub field: Ident,
    pub select: SelectType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reference {
    pub referee: Box<Expr>,
    pub mutable: Option<bool>,
}
