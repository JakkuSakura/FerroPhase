use common::*;
use std::fmt::Debug;

mod stmt;
mod typing;
mod value;

use crate::common_enum;
use crate::id::{Ident, Locator, Path};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::value::{Value, ValueUnit};
pub use stmt::*;
pub use typing::*;
pub use value::*;

pub type ExprId = u64;

common_enum! {
    /// Expr is an expression that returns a value
    /// aka ValueExpr
    pub enum Expr {
        Locator(Locator),
        Value(Box<Value>),
        Block(Block),
        Match(Match),
        If(If),
        Invoke(Box<Invoke>),
        Select(Box<Select>),
        Struct(Box<StructExpr>),
        Reference(Box<Reference>),
        Any(AnyBox),
    }

}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(ValueUnit).into())
    }
    pub fn is_unit(&self) -> bool {
        match self {
            Expr::Value(value) => value.is_unit(),
            _ => false,
        }
    }
    pub fn value(v: Value) -> Expr {
        match v {
            Value::Expr(expr) => expr,
            Value::Any(any) => Expr::Any(any),
            _ => Expr::Value(v.into()),
        }
    }
    pub fn ident(name: Ident) -> Expr {
        Expr::Locator(Locator::ident(name))
    }
    pub fn path(path: Path) -> Expr {
        Expr::Locator(Locator::path(path))
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
