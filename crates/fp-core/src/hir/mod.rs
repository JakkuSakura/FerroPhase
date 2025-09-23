use std::collections::HashMap;

pub type HirId = u32;
pub type DefId = u32;
pub type NodeId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct HirProgram {
    pub items: Vec<HirItem>,
    pub def_map: HashMap<DefId, HirItem>,
    pub next_hir_id: HirId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirItem {
    pub hir_id: HirId,
    pub def_id: DefId,
    pub kind: HirItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItemKind {
    Function(HirFunction),
    Struct(HirStruct),
    Const(HirConst),
    Impl(HirImpl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunction {
    pub sig: HirFunctionSig,
    pub body: Option<HirBody>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunctionSig {
    pub name: Symbol,
    pub inputs: Vec<HirParam>,
    pub output: HirTy,
    pub generics: HirGenerics,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub hir_id: HirId,
    pub pat: HirPat,
    pub ty: HirTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStruct {
    pub name: Symbol,
    pub fields: Vec<HirStructField>,
    pub generics: HirGenerics,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStructField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub ty: HirTy,
    pub vis: HirVisibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirConst {
    pub name: Symbol,
    pub ty: HirTy,
    pub body: HirBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirImpl {
    pub generics: HirGenerics,
    pub self_ty: HirTy,
    pub items: Vec<HirImplItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirImplItem {
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: HirImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirImplItemKind {
    Method(HirFunction),
    AssocConst(HirConst),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBody {
    pub hir_id: HirId,
    pub params: Vec<HirParam>,
    pub value: HirExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub hir_id: HirId,
    pub kind: HirExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    Literal(HirLit),
    Path(HirPath),
    Binary(HirBinOp, Box<HirExpr>, Box<HirExpr>),
    Unary(HirUnOp, Box<HirExpr>),
    Call(Box<HirExpr>, Vec<HirExpr>),
    MethodCall(Box<HirExpr>, Symbol, Vec<HirExpr>),
    FieldAccess(Box<HirExpr>, Symbol),
    Struct(HirPath, Vec<HirStructExprField>),
    If(Box<HirExpr>, Box<HirExpr>, Option<Box<HirExpr>>),
    Block(HirBlock),
    Let(HirPat, Box<HirTy>, Option<Box<HirExpr>>),
    Assign(Box<HirExpr>, Box<HirExpr>),
    Return(Option<Box<HirExpr>>),
    Break(Option<Box<HirExpr>>),
    Continue,
    Loop(HirBlock),
    While(Box<HirExpr>, HirBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStructExprField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub expr: HirExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBlock {
    pub hir_id: HirId,
    pub stmts: Vec<HirStmt>,
    pub expr: Option<Box<HirExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStmt {
    pub hir_id: HirId,
    pub kind: HirStmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmtKind {
    Local(HirLocal),
    Item(HirItem),
    Expr(HirExpr),
    Semi(HirExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirLocal {
    pub hir_id: HirId,
    pub pat: HirPat,
    pub ty: Option<HirTy>,
    pub init: Option<HirExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirPat {
    pub hir_id: HirId,
    pub kind: HirPatKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirPatKind {
    Wild,
    Binding(Symbol),
    Struct(HirPath, Vec<HirPatField>),
    Tuple(Vec<HirPat>),
    Lit(HirLit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirPatField {
    pub hir_id: HirId,
    pub name: Symbol,
    pub pat: HirPat,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTy {
    pub hir_id: HirId,
    pub kind: HirTyKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirTyKind {
    Path(HirPath),
    Tuple(Vec<Box<HirTy>>),
    Array(Box<HirTy>, Option<Box<HirExpr>>),
    Ptr(Box<HirTy>),
    Ref(Box<HirTy>),
    Never,
    Infer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirPath {
    pub segments: Vec<HirPathSegment>,
    pub res: Option<Res>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirPathSegment {
    pub name: Symbol,
    pub args: Option<HirGenericArgs>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirGenericArgs {
    pub args: Vec<HirGenericArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirGenericArg {
    Type(Box<HirTy>),
    Const(Box<HirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirGenerics {
    pub params: Vec<HirGenericParam>,
    pub where_clause: Option<HirWhereClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirGenericParam {
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: HirGenericParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirGenericParamKind {
    Type { default: Option<Box<HirTy>> },
    Const { ty: Box<HirTy> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirWhereClause {
    pub predicates: Vec<HirWherePredicate>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirWherePredicate {
    BoundPredicate {
        bounded_ty: Box<HirTy>,
        bounds: Vec<HirTypeBound>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirTypeBound {
    Trait(HirPath),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirLit {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Str(String),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirBinOp {
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
pub enum HirUnOp {
    Not,
    Neg,
    Deref,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirVisibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Res {
    Def(DefId),
    Local(HirId),
    SelfTy,
}

// Temporary types until we have proper implementations
pub type Symbol = String;
pub type Span = crate::span::Span;

// Default implementations
impl Default for HirGenerics {
    fn default() -> Self {
        Self {
            params: Vec::new(),
            where_clause: None,
        }
    }
}

impl HirProgram {
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

impl HirFunction {
    pub fn new(sig: HirFunctionSig, body: Option<HirBody>, is_const: bool) -> Self {
        Self {
            sig,
            body,
            is_const,
        }
    }
}

impl HirExpr {
    pub fn new(hir_id: HirId, kind: HirExprKind, span: Span) -> Self {
        Self { hir_id, kind, span }
    }
}

impl HirTy {
    pub fn new(hir_id: HirId, kind: HirTyKind, span: Span) -> Self {
        Self { hir_id, kind, span }
    }
}
