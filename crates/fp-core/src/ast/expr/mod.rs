use crate::ast::{
    get_threadlocal_serializer, BItem, BValue, ExprMacro, Ident, MacroInvocation, Name, Path, Ty,
    TySlot, Value, ValueUnit,
};
use crate::span::Span;
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
        Name(Name),
        Value(BValue),
        Block(ExprBlock),
        Match(ExprMatch),
        If(ExprIf),
        Loop(ExprLoop),
        While(ExprWhile),
        Return(ExprReturn),
        Break(ExprBreak),
        Continue(ExprContinue),
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
        For(ExprFor),
        Async(ExprAsync),
        Let(ExprLet),
        Closure(ExprClosure),
        Array(ExprArray),
        ArrayRepeat(ExprArrayRepeat),
        ConstBlock(ExprConstBlock),
        IntrinsicContainer(ExprIntrinsicContainer),
        IntrinsicCall(ExprIntrinsicCall),
        /// quote keyword – capture code as data at compile time
        Quote(ExprQuote),
        /// splice keyword – insert previously quoted code
        Splice(ExprSplice),
        /// closured because it's conceptually a closure, not a real one
        Closured(ExprClosured),
        Await(ExprAwait),
        Paren(ExprParen),
        Range(ExprRange),
        /// Format string expression (f-string) for println! and similar built-ins
        FormatString(ExprStringTemplate),

        Splat(ExprSplat),
        SplatDict(ExprSplatDict),
        Macro(ExprMacro),
        /// for items in dynamic languages
        Item(BItem),
        Any(AnyBox),
    }

}

common_struct! {
    pub struct Expr {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty: TySlot,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub span: Option<Span>,
        #[serde(flatten)]
        pub kind: ExprKind,
    }
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self {
            ty: None,
            span: None,
            kind,
        }
    }

    pub fn with_ty(kind: ExprKind, ty: TySlot) -> Self {
        Self {
            ty,
            span: None,
            kind,
        }
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

    pub fn span(&self) -> Span {
        self.span.unwrap_or_else(|| self.kind.span())
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
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
        Self {
            ty,
            span: None,
            kind,
        }
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
        ExprKind::Name(Name::from_ident(name)).into()
    }
    pub fn path(path: Path) -> Expr {
        ExprKind::Name(Name::path(path)).into()
    }
    pub fn name(name: Name) -> Expr {
        ExprKind::Name(name).into()
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
    pub fn macro_invocation(invocation: MacroInvocation) -> Self {
        ExprKind::Macro(ExprMacro::new(invocation)).into()
    }
}

fn union_spans(spans: impl IntoIterator<Item = Span>) -> Span {
    Span::union(spans)
}

impl ExprKind {
    pub fn span(&self) -> Span {
        match self {
            ExprKind::Id(_) => Span::null(),
            ExprKind::Name(locator) => locator.span(),
            ExprKind::Value(value) => value.span(),
            ExprKind::Any(_) => Span::null(),
            ExprKind::Block(block) => block.span(),
            ExprKind::Match(match_expr) => match_expr.span(),
            ExprKind::If(expr_if) => expr_if.span(),
            ExprKind::Loop(expr_loop) => expr_loop.span(),
            ExprKind::While(expr_while) => expr_while.span(),
            ExprKind::Return(expr_return) => expr_return.span(),
            ExprKind::Break(expr_break) => expr_break.span(),
            ExprKind::Continue(_) => Span::null(),
            ExprKind::Invoke(invoke) => invoke.span(),
            ExprKind::BinOp(binop) => union_spans([binop.lhs.span(), binop.rhs.span()]),
            ExprKind::UnOp(unop) => unop.val.span(),
            ExprKind::Assign(assign) => union_spans([assign.target.span(), assign.value.span()]),
            ExprKind::Select(select) => select.span(),
            ExprKind::Index(index) => index.span(),
            ExprKind::Struct(expr_struct) => expr_struct.span(),
            ExprKind::Structural(expr_structural) => expr_structural.span(),
            ExprKind::Cast(cast) => cast.span(),
            ExprKind::Reference(reference) => reference.span(),
            ExprKind::Dereference(deref) => deref.span(),
            ExprKind::Tuple(tuple) => tuple.span(),
            ExprKind::Try(expr_try) => expr_try.span(),
            ExprKind::For(expr_for) => expr_for.span(),
            ExprKind::Async(async_expr) => async_expr.span(),
            ExprKind::Let(expr_let) => expr_let.span(),
            ExprKind::Closure(closure) => closure.span(),
            ExprKind::Array(array) => union_spans(array.values.iter().map(Expr::span)),
            ExprKind::ArrayRepeat(repeat) => union_spans([repeat.elem.span(), repeat.len.span()]),
            ExprKind::ConstBlock(block) => block.expr.span(),
            ExprKind::IntrinsicContainer(container) => container.span(),
            ExprKind::IntrinsicCall(call) => call.span(),
            ExprKind::Quote(quote) => quote.span(),
            ExprKind::Splice(splice) => splice.span(),
            ExprKind::Closured(closured) => closured.span(),
            ExprKind::Await(await_expr) => await_expr.span(),
            ExprKind::Paren(paren) => paren.expr.span(),
            ExprKind::Range(range) => range.span(),
            ExprKind::FormatString(template) => template.span(),
            ExprKind::Splat(splat) => splat.iter.span(),
            ExprKind::SplatDict(splat) => splat.dict.span(),
            ExprKind::Macro(mac) => mac.span(),
            ExprKind::Item(item) => item.span(),
        }
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
