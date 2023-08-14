use crate::ops::{BinOp, BinOpKind};
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
    Stmt(Box<Expr>),
    Cond(Cond),
    Invoke(Invoke),
    Select(Select),
    Reference(Reference),
    BinOp(BinOp<Expr>),
    BinOpKind(BinOpKind),
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
    pub fn block(block: Block) -> Expr {
        if block.stmts.len() == 1 {
            let last = block.stmts.last().unwrap();
            if let Item::Expr(expr) = last {
                return if block.last_value {
                    expr.clone()
                } else {
                    Expr::Stmt(expr.clone().into())
                };
            }
        }
        Expr::Block(block)
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
