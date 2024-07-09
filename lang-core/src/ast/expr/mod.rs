use crate::ast::{BValue, Type, Value, ValueUnit};
use crate::id::{Ident, Locator, Path};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, get_threadlocal_serializer};
use std::fmt::{Debug, Display, Formatter};

mod closure;
mod stmt;
mod typing;
mod value;

pub use closure::*;
pub use stmt::*;
pub use typing::*;
pub use value::*;

pub type ExprId = u64;
pub type BExpr = Box<Expr>;

common_enum! {
    /// Expr is an expression that returns a value, note that a Type is also a Value
    pub enum Expr {
        Locator(Locator),
        Value(BValue),
        Block(ExprBlock),
        Match(ExprMatch),
        If(ExprIf),
        Loop(ExprLoop),
        Invoke(ExprInvoke),
        BinOp(ExprBinOp),
        UnOp(ExprUnOp),
        Assign(ExprAssign),
        Select(ExprSelect),
        Index(ExprIndex),
        InitStruct(ExprInitStruct),
        InitStructual(ExprInitStructural),
        Reference(ExprReference),

        /// closured because it's conceptually a closure, not a real one
        Closured(ExprClosure),

        SelfType(ExprSelfType),

        Any(AnyBox),
    }

}

impl Expr {
    pub fn get(&self) -> Self {
        self.clone()
    }
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
            Value::Expr(expr) => *expr,
            Value::Any(any) => Expr::Any(any),
            Value::Type(Type::Expr(expr)) => *expr,
            _ => Expr::Value(v.into()),
        }
    }
    pub fn ident(name: Ident) -> Expr {
        Expr::Locator(Locator::ident(name))
    }
    pub fn path(path: Path) -> Expr {
        Expr::Locator(Locator::path(path))
    }
    pub fn block(block: ExprBlock) -> Expr {
        if block.stmts.len() == 0 {
            return block.ret.map(|x| *x).unwrap_or(Expr::unit());
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
impl From<BExpr> for Expr {
    fn from(expr: BExpr) -> Self {
        *expr
    }
}
