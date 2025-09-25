use crate::ast::{get_threadlocal_serializer, BItem, BValue, Ty, Value, ValueUnit};
use crate::common_enum;
use crate::id::{Ident, Locator, Path};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use std::fmt::{Debug, Display, Formatter};

mod closure;
mod stmt;
mod value;

pub use closure::*;
pub use stmt::*;
pub use value::*;

pub type ExprId = u64;
pub type BExpr = Box<Expr>;

common_enum! {
    /// Expr is an expression that returns a value, note that a Type is also a Value
    pub enum Expr {
        /// An id for the expression node
        Id(ExprId),
        Locator(Locator),
        Value(BValue),
        Block(ExprBlock),
        Match(ExprMatch),
        If(ExprIf),
        Loop(ExprLoop),
        While(ExprWhile),
        Invoke(ExprInvoke),
        BinOp(ExprBinOp),
        UnOp(ExprUnOp),
        Assign(ExprAssign),
        Select(ExprSelect),
        Index(ExprIndex),
        Struct(ExprStruct),
        Structural(ExprStructural),
        Reference(ExprReference),
        Dereference(ExprDereference),
        Tuple(ExprTuple),
        Try(ExprTry),
        Let(ExprLet),
        Closure(ExprClosure),
        Array(ExprArray),
        /// closured because it's conceptually a closure, not a real one
        Closured(ExprClosured),
        Paren(ExprParen),
        Range(ExprRange),
        /// Format string expression (f-string) for println! and similar built-ins
        FormatString(ExprFormatString),

        Splat(ExprSplat),
        SplatDict(ExprSplatDict),
        /// for items in dynamic languages
        Item(BItem),
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
            Value::Type(Ty::Expr(expr)) => *expr,
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
        block.into_expr()
    }
    pub fn into_block(self) -> ExprBlock {
        match self {
            Expr::Block(block) => block,
            _ => ExprBlock::new_expr(self),
        }
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
