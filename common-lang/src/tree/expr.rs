use crate::builtins::BuiltinFn;
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
}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(UnitValue))
    }
}
