use crate::ast::{
    get_threadlocal_serializer, BItem, BValue, ExprMacro, Ident, Locator, MacroInvocation, Path,
    Ty, TySlot, Value, ValueUnit,
};
use crate::span::Span;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::common_struct;
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

/// Expr is an expression that returns a value, note that a Type is also a Value
#[derive(Debug, Clone, PartialEq, Hash)]
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

impl From<ExprId> for ExprKind {
    fn from(value: ExprId) -> Self {
        ExprKind::Id(value)
    }
}

impl From<Locator> for ExprKind {
    fn from(value: Locator) -> Self {
        ExprKind::Locator(value)
    }
}

impl From<BValue> for ExprKind {
    fn from(value: BValue) -> Self {
        ExprKind::Value(value)
    }
}

impl From<ExprBlock> for ExprKind {
    fn from(value: ExprBlock) -> Self {
        ExprKind::Block(value)
    }
}

impl From<ExprMatch> for ExprKind {
    fn from(value: ExprMatch) -> Self {
        ExprKind::Match(value)
    }
}

impl From<ExprIf> for ExprKind {
    fn from(value: ExprIf) -> Self {
        ExprKind::If(value)
    }
}

impl From<ExprLoop> for ExprKind {
    fn from(value: ExprLoop) -> Self {
        ExprKind::Loop(value)
    }
}

impl From<ExprWhile> for ExprKind {
    fn from(value: ExprWhile) -> Self {
        ExprKind::While(value)
    }
}

impl From<ExprReturn> for ExprKind {
    fn from(value: ExprReturn) -> Self {
        ExprKind::Return(value)
    }
}

impl From<ExprBreak> for ExprKind {
    fn from(value: ExprBreak) -> Self {
        ExprKind::Break(value)
    }
}

impl From<ExprContinue> for ExprKind {
    fn from(value: ExprContinue) -> Self {
        ExprKind::Continue(value)
    }
}

impl From<ExprInvoke> for ExprKind {
    fn from(value: ExprInvoke) -> Self {
        ExprKind::Invoke(value)
    }
}

impl From<ExprBinOp> for ExprKind {
    fn from(value: ExprBinOp) -> Self {
        ExprKind::BinOp(value)
    }
}

impl From<ExprUnOp> for ExprKind {
    fn from(value: ExprUnOp) -> Self {
        ExprKind::UnOp(value)
    }
}

impl From<ExprAssign> for ExprKind {
    fn from(value: ExprAssign) -> Self {
        ExprKind::Assign(value)
    }
}

impl From<ExprSelect> for ExprKind {
    fn from(value: ExprSelect) -> Self {
        ExprKind::Select(value)
    }
}

impl From<ExprIndex> for ExprKind {
    fn from(value: ExprIndex) -> Self {
        ExprKind::Index(value)
    }
}

impl From<ExprStruct> for ExprKind {
    fn from(value: ExprStruct) -> Self {
        ExprKind::Struct(value)
    }
}

impl From<ExprStructural> for ExprKind {
    fn from(value: ExprStructural) -> Self {
        ExprKind::Structural(value)
    }
}

impl From<ExprCast> for ExprKind {
    fn from(value: ExprCast) -> Self {
        ExprKind::Cast(value)
    }
}

impl From<ExprReference> for ExprKind {
    fn from(value: ExprReference) -> Self {
        ExprKind::Reference(value)
    }
}

impl From<ExprDereference> for ExprKind {
    fn from(value: ExprDereference) -> Self {
        ExprKind::Dereference(value)
    }
}

impl From<ExprTuple> for ExprKind {
    fn from(value: ExprTuple) -> Self {
        ExprKind::Tuple(value)
    }
}

impl From<ExprTry> for ExprKind {
    fn from(value: ExprTry) -> Self {
        ExprKind::Try(value)
    }
}

impl From<ExprFor> for ExprKind {
    fn from(value: ExprFor) -> Self {
        ExprKind::For(value)
    }
}

impl From<ExprAsync> for ExprKind {
    fn from(value: ExprAsync) -> Self {
        ExprKind::Async(value)
    }
}

impl From<ExprLet> for ExprKind {
    fn from(value: ExprLet) -> Self {
        ExprKind::Let(value)
    }
}

impl From<ExprClosure> for ExprKind {
    fn from(value: ExprClosure) -> Self {
        ExprKind::Closure(value)
    }
}

impl From<ExprArray> for ExprKind {
    fn from(value: ExprArray) -> Self {
        ExprKind::Array(value)
    }
}

impl From<ExprArrayRepeat> for ExprKind {
    fn from(value: ExprArrayRepeat) -> Self {
        ExprKind::ArrayRepeat(value)
    }
}

impl From<ExprConstBlock> for ExprKind {
    fn from(value: ExprConstBlock) -> Self {
        ExprKind::ConstBlock(value)
    }
}

impl From<ExprIntrinsicContainer> for ExprKind {
    fn from(value: ExprIntrinsicContainer) -> Self {
        ExprKind::IntrinsicContainer(value)
    }
}

impl From<ExprIntrinsicCall> for ExprKind {
    fn from(value: ExprIntrinsicCall) -> Self {
        ExprKind::IntrinsicCall(value)
    }
}

impl From<ExprQuote> for ExprKind {
    fn from(value: ExprQuote) -> Self {
        ExprKind::Quote(value)
    }
}

impl From<ExprSplice> for ExprKind {
    fn from(value: ExprSplice) -> Self {
        ExprKind::Splice(value)
    }
}

impl From<ExprClosured> for ExprKind {
    fn from(value: ExprClosured) -> Self {
        ExprKind::Closured(value)
    }
}

impl From<ExprAwait> for ExprKind {
    fn from(value: ExprAwait) -> Self {
        ExprKind::Await(value)
    }
}

impl From<ExprParen> for ExprKind {
    fn from(value: ExprParen) -> Self {
        ExprKind::Paren(value)
    }
}

impl From<ExprRange> for ExprKind {
    fn from(value: ExprRange) -> Self {
        ExprKind::Range(value)
    }
}

impl From<ExprStringTemplate> for ExprKind {
    fn from(value: ExprStringTemplate) -> Self {
        ExprKind::FormatString(value)
    }
}

impl From<ExprSplat> for ExprKind {
    fn from(value: ExprSplat) -> Self {
        ExprKind::Splat(value)
    }
}

impl From<ExprSplatDict> for ExprKind {
    fn from(value: ExprSplatDict) -> Self {
        ExprKind::SplatDict(value)
    }
}

impl From<ExprMacro> for ExprKind {
    fn from(value: ExprMacro) -> Self {
        ExprKind::Macro(value)
    }
}

impl From<BItem> for ExprKind {
    fn from(value: BItem) -> Self {
        ExprKind::Item(value)
    }
}

impl From<AnyBox> for ExprKind {
    fn from(value: AnyBox) -> Self {
        ExprKind::Any(value)
    }
}

common_struct! {
    pub struct Expr {
        pub ty: TySlot,
        pub span: Option<Span>,
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
            ExprKind::Locator(locator) => locator.span(),
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
            ExprKind::ArrayRepeat(repeat) => {
                union_spans([repeat.elem.span(), repeat.len.span()])
            }
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
