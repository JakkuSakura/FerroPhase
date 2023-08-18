use crate::ast::*;
use crate::ops::BinOp;
use crate::value::{UnitValue, Value};
use serde::{Deserialize, Serialize};

/// Expr is an expression that returns a value
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Expr {
    Pat(Pat),
    Value(Value),
    Block(Block),
    Cond(Cond),
    Invoke(Invoke),
    Select(Select),
    Reference(Reference),
    BinOp(BinOp<Expr>),
    Any(AnyBox),
}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(UnitValue))
    }
    pub fn value(v: Value) -> Expr {
        match v {
            Value::Expr(expr) => *expr,
            _ => Expr::Value(v),
        }
    }
    pub fn ident(name: Ident) -> Expr {
        Expr::Pat(Pat::ident(name))
    }
    pub fn path(path: Path) -> Expr {
        Expr::Pat(Pat::path(path))
    }
    pub fn block(block: Block) -> Expr {
        if block.stmts.len() == 1 {
            let last = block.stmts.last().unwrap();
            if let Statement::Expr(expr) = last {
                return expr.clone();
            }
            if let Statement::SideEffect(expr) = last {
                if let Expr::Block(block) = &expr.expr {
                    let mut block = block.clone();
                    block.make_last_side_effect();
                    return Expr::block(block);
                }
            }
        }
        if block.stmts.is_empty() {
            return Expr::unit();
        }
        Expr::Block(block)
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Invoke {
    pub func: Box<Expr>,
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Select {
    pub obj: Box<Expr>,
    pub field: Ident,
    pub select: SelectType,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Reference {
    pub referee: Box<Expr>,
    pub mutable: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Cond {
    pub cases: Vec<CondCase>,
    pub if_style: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct CondCase {
    pub cond: Expr,
    pub body: Expr,
}
