use crate::ast::{TypeBinaryOpKind, TypePrimitive};
use crate::intrinsics::IntrinsicCallKind;
use std::collections::HashMap;

pub mod ident;
pub mod pretty;
pub mod ty;

pub use ident::Symbol;
pub use ty::Ty;

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
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    pub name: Symbol,
    pub inputs: Vec<Param>,
    pub output: TypeExpr,
    pub generics: Generics,
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
    pub fn new(sig: FunctionSig, body: Option<Body>, is_const: bool) -> Self {
        Self {
            sig,
            body,
            is_const,
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
