use crate::ast::{
    get_threadlocal_serializer, BItem, BValue, Ident, Locator, Path, Ty, TySlot, Value, ValueUnit,
};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};
use std::fmt::{Debug, Display, Formatter};

mod closure;
mod collection;
mod stmt;
mod value;

pub use closure::*;
pub use collection::*;
pub use stmt::*;
pub use value::*;

pub type ExprId = u64;
pub type BExpr = Box<Expr>;

common_enum! {
    /// Expr is an expression that returns a value, note that a Type is also a Value
    pub enum ExprKind {
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
        Cast(ExprCast),
        Reference(ExprReference),
        Dereference(ExprDereference),
        Tuple(ExprTuple),
        Try(ExprTry),
        Let(ExprLet),
        Closure(ExprClosure),
        Array(ExprArray),
        ArrayRepeat(ExprArrayRepeat),
        IntrinsicCollection(ExprIntrinsicCollection),
        IntrinsicCall(ExprIntrinsicCall),
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

common_struct! {
    pub struct Expr {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty: TySlot,
        #[serde(flatten)]
        pub kind: ExprKind,
    }
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self { ty: None, kind }
    }

    pub fn with_ty(kind: ExprKind, ty: TySlot) -> Self {
        Self { ty, kind }
    }

    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    pub fn ty_mut(&mut self) -> &mut TySlot {
        &mut self.ty
    }

    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = Some(ty);
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut ExprKind {
        &mut self.kind
    }

    pub fn into_parts(self) -> (TySlot, ExprKind) {
        (self.ty, self.kind)
    }

    pub fn from_parts(ty: TySlot, kind: ExprKind) -> Self {
        Self { ty, kind }
    }

    pub fn with_ty_slot(mut self, ty: TySlot) -> Self {
        self.ty = ty;
        self
    }

    pub fn get(&self) -> Self {
        self.clone()
    }
    pub fn unit() -> Expr {
        ExprKind::Value(Value::Unit(ValueUnit).into()).into()
    }
    pub fn is_unit(&self) -> bool {
        match &self.kind {
            ExprKind::Value(value) => value.is_unit(),
            _ => false,
        }
    }
    pub fn value(v: Value) -> Expr {
        match v {
            Value::Expr(expr) => *expr,
            Value::Any(any) => ExprKind::Any(any).into(),
            Value::Type(Ty::Expr(expr)) => *expr,
            _ => ExprKind::Value(v.into()).into(),
        }
    }
    pub fn ident(name: Ident) -> Expr {
        ExprKind::Locator(Locator::from_ident(name)).into()
    }
    pub fn path(path: Path) -> Expr {
        ExprKind::Locator(Locator::path(path)).into()
    }
    pub fn locator(locator: Locator) -> Expr {
        ExprKind::Locator(locator).into()
    }
    pub fn block(block: ExprBlock) -> Expr {
        block.into_expr()
    }
    pub fn into_block(self) -> ExprBlock {
        let (ty, kind) = self.into_parts();
        match kind {
            ExprKind::Block(block) => block,
            other => ExprBlock::new_expr(Expr::from_parts(ty, other)),
        }
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        ExprKind::Any(AnyBox::new(any)).into()
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_expr(self).unwrap();
        f.write_str(&s)
    }
}
impl<T> From<T> for Expr
where
    ExprKind: From<T>,
{
    fn from(value: T) -> Self {
        Expr::new(ExprKind::from(value))
    }
}
impl From<BExpr> for Expr {
    fn from(expr: BExpr) -> Self {
        *expr
    }
}
