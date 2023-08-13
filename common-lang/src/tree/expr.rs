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
    Struct(StructExpr),
    Any(AnyBox),
}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(UnitValue))
    }
}
pub type InvokeExpr = Invoke<Expr>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructExpr {
    pub name: TypeExpr, // either Ident or Struct
    pub fields: Vec<FieldValueExpr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExpr {
    pub params: Vec<ParamExpr>,
    pub ret: TypeExpr,
    pub body: Block,
}
