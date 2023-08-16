use crate::ops::BinOp;
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
    pub fn path(path: Path) -> Expr {
        if path.segments.len() == 1 {
            return Expr::Ident(path.segments[0].clone());
        }
        Expr::Path(path)
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
    pub fn any<T: Debug + 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
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
