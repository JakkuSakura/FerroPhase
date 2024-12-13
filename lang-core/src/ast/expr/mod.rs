use crate::ast::{get_threadlocal_serializer, AstType, AstValue, BItem, BValue, ValueUnit};
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
pub type BExpr = Box<AstExpr>;

common_enum! {
    /// Expr is an expression that returns a value, note that a Type is also a Value
    pub enum AstExpr {
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

        Splat(ExprSplat),
        SplatDict(ExprSplatDict),
        /// for items in dynamic languages
        Item(BItem),
        Any(AnyBox),
    }

}

impl AstExpr {
    pub fn get(&self) -> Self {
        self.clone()
    }
    pub fn unit() -> AstExpr {
        AstExpr::Value(AstValue::Unit(ValueUnit).into())
    }
    pub fn is_unit(&self) -> bool {
        match self {
            AstExpr::Value(value) => value.is_unit(),
            _ => false,
        }
    }
    pub fn value(v: AstValue) -> AstExpr {
        match v {
            AstValue::Expr(expr) => *expr,
            AstValue::Any(any) => AstExpr::Any(any),
            AstValue::Type(AstType::Expr(expr)) => *expr,
            _ => AstExpr::Value(v.into()),
        }
    }
    pub fn ident(name: Ident) -> AstExpr {
        AstExpr::Locator(Locator::ident(name))
    }
    pub fn path(path: Path) -> AstExpr {
        AstExpr::Locator(Locator::path(path))
    }
    pub fn block(block: ExprBlock) -> AstExpr {
        block.into_expr()
    }
    pub fn into_block(self) -> ExprBlock {
        match self {
            AstExpr::Block(block) => block,
            _ => ExprBlock::new_expr(self),
        }
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_expr(self).unwrap();
        f.write_str(&s)
    }
}
impl From<BExpr> for AstExpr {
    fn from(expr: BExpr) -> Self {
        *expr
    }
}
