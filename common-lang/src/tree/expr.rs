use crate::ops::{BuiltinFn, Invoke};
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
    Invoke(InvokeExpr),
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
    pub fn any<T: 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
pub type InvokeExpr = Invoke<Expr>;
