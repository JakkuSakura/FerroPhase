use crate::ast::{TypeBinaryOpKind, TypePrimitive};
use crate::intrinsics::IntrinsicCallKind;
use std::collections::HashMap;

pub mod ident;
pub mod pretty;
pub mod ty;

pub use ident::Symbol;
pub use ty::{Abi, Ty};

pub type HirId = u32;
pub type DefId = u32;
pub type NodeId = u32;

// Remove the old type alias
// pub type Symbol = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub def_map: HashMap<DefId, Item>,
    pub next_hir_id: HirId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub hir_id: HirId,
    pub def_id: DefId,
    pub visibility: Visibility,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Const(Const),
    Impl(Impl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub sig: FunctionSig,
    pub body: Option<Body>,
    pub is_const: bool,
    pub is_extern: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    pub name: Symbol,
    pub inputs: Vec<Param>,
    pub output: TypeExpr,
    pub generics: Generics,
    pub abi: Abi,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub hir_id: HirId,
    pub pat: Pat,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Symbol,
    pub fields: Vec<StructField>,
    pub generics: Generics,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: Symbol,
    pub variants: Vec<EnumVariant>,
    pub generics: Generics,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub hir_id: HirId,
    pub def_id: DefId,
    pub name: Symbol,
    pub discriminant: Option<Expr>,
    pub payload: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub ty: TypeExpr,
    pub vis: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Const {
    pub name: Symbol,
    pub ty: TypeExpr,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    pub generics: Generics,
    pub trait_ty: Option<TypeExpr>,
    pub self_ty: TypeExpr,
    pub items: Vec<ImplItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: ImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    Method(Function),
    AssocConst(Const),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub hir_id: HirId,
    pub params: Vec<Param>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Lit),
    Path(Path),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Reference(ExprReference),
    Call(Box<Expr>, Vec<CallArg>),
    MethodCall(Box<Expr>, Symbol, Vec<CallArg>),
    FieldAccess(Box<Expr>, Symbol),
    Index(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>, Box<TypeExpr>),
    Struct(Path, Vec<StructExprField>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<MatchArm>),
    Block(Block),
    IntrinsicCall(IntrinsicCallExpr),
    FormatString(FormatString),
    Let(Pat, Box<TypeExpr>, Option<Box<Expr>>),
    Assign(Box<Expr>, Box<Expr>),
    Return(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    Continue,
    Loop(Block),
    While(Box<Expr>, Block),
    Array(Vec<Expr>),
    ArrayRepeat { elem: Box<Expr>, len: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub hir_id: HirId,
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprReference {
    pub hir_id: HirId,
    pub mutable: crate::hir::ty::Mutability,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExprField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallArg {
    pub name: Symbol,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatString {
    pub parts: Vec<FormatTemplatePart>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormatTemplatePart {
    Literal(String),
    Placeholder(FormatPlaceholder),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatPlaceholder {
    pub arg_ref: FormatArgRef,
    pub format_spec: Option<crate::ast::FormatSpec>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormatArgRef {
    Implicit,
    Positional(usize),
    Named(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntrinsicCallExpr {
    pub kind: IntrinsicCallKind,
    pub callargs: Vec<CallArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub hir_id: HirId,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub hir_id: HirId,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Local(Local),
    Item(Item),
    Expr(Expr),
    Semi(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub hir_id: HirId,
    pub pat: Pat,
    pub ty: Option<TypeExpr>,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pat {
    pub hir_id: HirId,
    pub kind: PatKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatKind {
    Wild,
    Binding { name: Symbol, mutable: bool },
    Struct(Path, Vec<PatField>, bool),
    TupleStruct(Path, Vec<Pat>),
    Variant(Path),
    Tuple(Vec<Pat>),
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub pat: Pat,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub hir_id: HirId,
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStructuralField {
    pub name: Symbol,
    pub ty: Box<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStructural {
    pub fields: Vec<TypeStructuralField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeBinaryOp {
    pub kind: TypeBinaryOpKind,
    pub lhs: Box<TypeExpr>,
    pub rhs: Box<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExprKind {
    Primitive(TypePrimitive),
    Path(Path),
    Structural(TypeStructural),
    TypeBinaryOp(TypeBinaryOp),
    Tuple(Vec<Box<TypeExpr>>),
    Array(Box<TypeExpr>, Option<Box<Expr>>),
    Slice(Box<TypeExpr>),
    Ptr(Box<TypeExpr>),
    Ref(Box<TypeExpr>),
    FnPtr(FnPtrType),
    Never,
    Infer,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnPtrType {
    pub inputs: Vec<Box<TypeExpr>>,
    pub output: Box<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub res: Option<Res>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub name: Symbol,
    pub args: Option<GenericArgs>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
    Type(Box<TypeExpr>),
    Const(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: GenericParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericParamKind {
    Type { default: Option<Box<TypeExpr>> },
    Const { ty: Box<TypeExpr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WherePredicate {
    BoundPredicate {
        bounded_ty: Box<TypeExpr>,
        bounds: Vec<TypeBound>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeBound {
    Trait(Path),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Str(String),
    Char(char),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Neg,
    Deref,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Res {
    Def(DefId),
    Local(HirId),
    SelfTy,
    Module(Vec<String>),
}

// Temporary types until we have proper implementations
pub type Span = crate::span::Span;

// Default implementations
impl Default for Generics {
    fn default() -> Self {
        Self {
            params: Vec::new(),
            where_clause: None,
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            def_map: HashMap::new(),
            next_hir_id: 0,
        }
    }

    pub fn next_id(&mut self) -> HirId {
        let id = self.next_hir_id;
        self.next_hir_id += 1;
        id
    }
}

impl Function {
    pub fn new(sig: FunctionSig, body: Option<Body>, is_const: bool, is_extern: bool) -> Self {
        Self {
            sig,
            body,
            is_const,
            is_extern,
        }
    }
}

impl Expr {
    pub fn new(hir_id: HirId, kind: ExprKind, span: Span) -> Self {
        Self { hir_id, kind, span }
    }
}

impl TypeExpr {
    pub fn new(hir_id: HirId, kind: TypeExprKind, span: Span) -> Self {
        Self { hir_id, kind, span }
    }
}

impl Program {
    pub fn span(&self) -> Span {
        Span::union(self.items.iter().map(Item::span))
    }
}

impl Item {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl ItemKind {
    pub fn span(&self) -> Span {
        match self {
            ItemKind::Function(func) => func.span(),
            ItemKind::Struct(stru) => stru.span(),
            ItemKind::Enum(enm) => enm.span(),
            ItemKind::Const(cons) => cons.span(),
            ItemKind::Impl(imp) => imp.span(),
        }
    }
}

impl Function {
    pub fn span(&self) -> Span {
        Span::union(
            self.body
                .as_ref()
                .map(Body::span)
                .into_iter()
                .chain([self.sig.span()]),
        )
    }
}

impl FunctionSig {
    pub fn span(&self) -> Span {
        Span::union(
            self.inputs
                .iter()
                .map(Param::span)
                .chain([self.output.span(), self.generics.span()]),
        )
    }
}

impl Param {
    pub fn span(&self) -> Span {
        Span::union([self.pat.span(), self.ty.span()])
    }
}

impl Struct {
    pub fn span(&self) -> Span {
        Span::union(
            self.fields
                .iter()
                .map(StructField::span)
                .chain([self.generics.span()]),
        )
    }
}

impl Enum {
    pub fn span(&self) -> Span {
        Span::union(
            self.variants
                .iter()
                .map(EnumVariant::span)
                .chain([self.generics.span()]),
        )
    }
}

impl EnumVariant {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.discriminant.as_ref().map(Expr::span),
                self.payload.as_ref().map(TypeExpr::span),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

impl StructField {
    pub fn span(&self) -> Span {
        self.ty.span()
    }
}

impl Const {
    pub fn span(&self) -> Span {
        Span::union([self.ty.span(), self.body.span()])
    }
}

impl Impl {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.trait_ty.as_ref().map(TypeExpr::span),
                Some(self.self_ty.span()),
                Some(Span::union(self.items.iter().map(ImplItem::span))),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

impl ImplItem {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl ImplItemKind {
    pub fn span(&self) -> Span {
        match self {
            ImplItemKind::Method(func) => func.span(),
            ImplItemKind::AssocConst(cons) => cons.span(),
        }
    }
}

impl Body {
    pub fn span(&self) -> Span {
        Span::union(
            self.params
                .iter()
                .map(Param::span)
                .chain([self.value.span()]),
        )
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl ExprKind {
    pub fn span(&self) -> Span {
        match self {
            ExprKind::Literal(_) => Span::null(),
            ExprKind::Path(path) => path.span(),
            ExprKind::Binary(_, lhs, rhs) => Span::union([lhs.span(), rhs.span()]),
            ExprKind::Unary(_, expr) => expr.span(),
            ExprKind::Reference(reference) => reference.expr.span(),
            ExprKind::Call(func, args) => Span::union(
                Some(func.span())
                    .into_iter()
                    .chain(args.iter().map(CallArg::span)),
            ),
            ExprKind::MethodCall(receiver, _, args) => Span::union(
                Some(receiver.span())
                    .into_iter()
                    .chain(args.iter().map(CallArg::span)),
            ),
            ExprKind::FieldAccess(expr, _) => expr.span(),
            ExprKind::Index(expr, index) => Span::union([expr.span(), index.span()]),
            ExprKind::Cast(expr, ty) => Span::union([expr.span(), ty.span()]),
            ExprKind::Struct(path, fields) => Span::union(
                Some(path.span())
                    .into_iter()
                    .chain(fields.iter().map(StructExprField::span)),
            ),
            ExprKind::If(cond, then, elze) => Span::union(
                [
                    Some(cond.span()),
                    Some(then.span()),
                    elze.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
            ExprKind::Match(expr, arms) => Span::union(
                Some(expr.span())
                    .into_iter()
                    .chain(arms.iter().map(MatchArm::span)),
            ),
            ExprKind::Block(block) => block.span(),
            ExprKind::IntrinsicCall(call) => call.span(),
            ExprKind::FormatString(format) => format.span(),
            ExprKind::Let(pat, ty, expr) => Span::union(
                [
                    Some(pat.span()),
                    Some(ty.span()),
                    expr.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
            ExprKind::Assign(lhs, rhs) => Span::union([lhs.span(), rhs.span()]),
            ExprKind::Return(expr) => expr
                .as_ref()
                .map(|inner| inner.span())
                .unwrap_or_else(Span::null),
            ExprKind::Break(expr) => expr
                .as_ref()
                .map(|inner| inner.span())
                .unwrap_or_else(Span::null),
            ExprKind::Continue => Span::null(),
            ExprKind::Loop(block) => block.span(),
            ExprKind::While(cond, block) => Span::union([cond.span(), block.span()]),
            ExprKind::Array(exprs) => Span::union(exprs.iter().map(Expr::span)),
            ExprKind::ArrayRepeat { elem, len } => Span::union([elem.span(), len.span()]),
        }
    }
}

impl MatchArm {
    pub fn span(&self) -> Span {
        Span::union(
            [
                Some(self.pat.span()),
                self.guard.as_ref().map(Expr::span),
                Some(self.body.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

impl StructExprField {
    pub fn span(&self) -> Span {
        self.expr.span()
    }
}

impl CallArg {
    pub fn span(&self) -> Span {
        self.value.span()
    }
}

impl FormatString {
    pub fn span(&self) -> Span {
        Span::union(self.parts.iter().map(FormatTemplatePart::span))
    }
}

impl FormatTemplatePart {
    pub fn span(&self) -> Span {
        match self {
            FormatTemplatePart::Literal(_) => Span::null(),
            FormatTemplatePart::Placeholder(placeholder) => placeholder.span(),
        }
    }
}

impl FormatPlaceholder {
    pub fn span(&self) -> Span {
        self.format_spec
            .as_ref()
            .map(|_| Span::null())
            .unwrap_or_else(Span::null)
    }
}

impl IntrinsicCallExpr {
    pub fn span(&self) -> Span {
        Span::union(self.callargs.iter().map(CallArg::span))
    }
}

impl Block {
    pub fn span(&self) -> Span {
        Span::union(
            self.stmts
                .iter()
                .map(Stmt::span)
                .chain(self.expr.as_ref().map(|expr| expr.span())),
        )
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl StmtKind {
    pub fn span(&self) -> Span {
        match self {
            StmtKind::Local(local) => local.span(),
            StmtKind::Item(item) => item.span(),
            StmtKind::Expr(expr) => expr.span(),
            StmtKind::Semi(expr) => expr.span(),
        }
    }
}

impl Local {
    pub fn span(&self) -> Span {
        Span::union(
            [
                Some(self.pat.span()),
                self.ty.as_ref().map(TypeExpr::span),
                self.init.as_ref().map(Expr::span),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

impl Pat {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl PatKind {
    pub fn span(&self) -> Span {
        match self {
            PatKind::Wild => Span::null(),
            PatKind::Binding { .. } => Span::null(),
            PatKind::Struct(path, fields, _) => Span::union(
                Some(path.span())
                    .into_iter()
                    .chain(fields.iter().map(PatField::span)),
            ),
            PatKind::TupleStruct(path, pats) => Span::union(
                Some(path.span())
                    .into_iter()
                    .chain(pats.iter().map(Pat::span)),
            ),
            PatKind::Variant(path) => path.span(),
            PatKind::Tuple(pats) => Span::union(pats.iter().map(Pat::span)),
            PatKind::Lit(_) => Span::null(),
        }
    }
}

impl PatField {
    pub fn span(&self) -> Span {
        self.pat.span()
    }
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl TypeStructuralField {
    pub fn span(&self) -> Span {
        self.ty.span()
    }
}

impl TypeStructural {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(TypeStructuralField::span))
    }
}

impl TypeBinaryOp {
    pub fn span(&self) -> Span {
        Span::union([self.lhs.span(), self.rhs.span()])
    }
}

impl TypeExprKind {
    pub fn span(&self) -> Span {
        match self {
            TypeExprKind::Primitive(_) => Span::null(),
            TypeExprKind::Path(path) => path.span(),
            TypeExprKind::Structural(structural) => structural.span(),
            TypeExprKind::TypeBinaryOp(op) => op.span(),
            TypeExprKind::Tuple(types) => Span::union(types.iter().map(|ty| ty.span())),
            TypeExprKind::Array(ty, len) => Span::union(
                Some(ty.span())
                    .into_iter()
                    .chain(len.as_ref().map(|expr| expr.span())),
            ),
            TypeExprKind::Slice(ty) => ty.span(),
            TypeExprKind::Ptr(ty) => ty.span(),
            TypeExprKind::Ref(ty) => ty.span(),
            TypeExprKind::FnPtr(func) => func.span(),
            TypeExprKind::Never | TypeExprKind::Infer | TypeExprKind::Error => Span::null(),
        }
    }
}

impl FnPtrType {
    pub fn span(&self) -> Span {
        Span::union(
            self.inputs
                .iter()
                .map(|ty| ty.span())
                .chain([self.output.span()]),
        )
    }
}

impl Path {
    pub fn span(&self) -> Span {
        Span::union(self.segments.iter().map(PathSegment::span))
    }
}

impl PathSegment {
    pub fn span(&self) -> Span {
        self.args
            .as_ref()
            .map(GenericArgs::span)
            .unwrap_or_else(Span::null)
    }
}

impl GenericArgs {
    pub fn span(&self) -> Span {
        Span::union(self.args.iter().map(GenericArg::span))
    }
}

impl GenericArg {
    pub fn span(&self) -> Span {
        match self {
            GenericArg::Type(ty) => ty.span(),
            GenericArg::Const(expr) => expr.span(),
        }
    }
}

impl Generics {
    pub fn span(&self) -> Span {
        Span::union(
            self.params
                .iter()
                .map(GenericParam::span)
                .chain(self.where_clause.as_ref().map(WhereClause::span)),
        )
    }
}

impl GenericParam {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl GenericParamKind {
    pub fn span(&self) -> Span {
        match self {
            GenericParamKind::Type { default } => default
                .as_ref()
                .map(|ty| ty.span())
                .unwrap_or_else(Span::null),
            GenericParamKind::Const { ty } => ty.span(),
        }
    }
}

impl WhereClause {
    pub fn span(&self) -> Span {
        Span::union(self.predicates.iter().map(WherePredicate::span))
    }
}

impl WherePredicate {
    pub fn span(&self) -> Span {
        match self {
            WherePredicate::BoundPredicate { bounded_ty, bounds } => Span::union(
                Some(bounded_ty.span())
                    .into_iter()
                    .chain(bounds.iter().map(TypeBound::span)),
            ),
        }
    }
}

impl TypeBound {
    pub fn span(&self) -> Span {
        match self {
            TypeBound::Trait(path) => path.span(),
        }
    }
}

impl Lit {
    pub fn span(&self) -> Span {
        Span::null()
    }
}
