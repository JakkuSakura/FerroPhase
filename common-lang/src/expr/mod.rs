use crate::id::{Ident, Locator, Path};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::value::{Value, ValueUnit};
use crate::{common_enum, get_threadlocal_serializer};
use common::*;
use std::fmt::{Debug, Display, Formatter};

mod arena;
mod closure;
mod stmt;
mod typing;
mod value;

pub use arena::*;
pub use closure::*;
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
        Invoke(Invoke),
        Select(Box<Select>),
        Struct(Box<StructExpr>),
        Reference(Box<Reference>),

        /// closured because it's conceptually a closure, not a real one
        Closured(Box<Closure<Expr>>),

        SelfType(Box<SelfType>),

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
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_expr(self).unwrap();
        f.write_str(&s)
    }
}
