use std::collections::HashMap;

use crate::intrinsics::{IntrinsicCall, IntrinsicCallPayload as GenericIntrinsicCallPayload};

pub mod ty;
pub use ty::Ty;
pub mod pretty;

pub type ThirId = u32;
pub type LocalId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
    pub next_thir_id: ThirId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub thir_id: ThirId,
    pub kind: ItemKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Const(Const),
    Impl(Impl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Symbol,
    pub path: Vec<Symbol>,
    pub def_id: Option<DefId>,
    pub sig: FunctionSig,
    pub body_id: Option<BodyId>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    pub inputs: Vec<Ty>,
    pub output: Ty,
    pub c_variadic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub fields: Vec<StructField>,
    pub variant_data: VariantData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub thir_id: ThirId,
    pub ty: Ty,
    pub vis: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Const {
    pub ty: Ty,
    pub body_id: BodyId,
    pub def_id: Option<ty::DefId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    pub self_ty: Ty,
    pub items: Vec<ImplItem>,
    pub trait_ref: Option<TraitRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    pub thir_id: ThirId,
    pub kind: ImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    Method(Function),
    AssocConst(Const),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub params: Vec<Param>,
    pub value: Expr,
    pub locals: Vec<LocalDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ty: Ty,
    pub pat: Option<Pat>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDecl {
    pub ty: Ty,
    pub source_info: SourceInfo,
    pub internal: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub thir_id: ThirId,
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Lit),
    Local(LocalId),
    /// Reference to a function, constant, or global item.
    Path(ItemRef),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Cast(Box<Expr>, Ty),
    Call {
        fun: Box<Expr>,
        args: Vec<Expr>,
        from_hir_call: bool,
    },
    IntrinsicCall(ThirIntrinsicCall),
    Deref(Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Field {
        base: Box<Expr>,
        field_idx: usize,
    },
    VarRef {
        id: LocalId,
    },
    UpvarRef {
        closure_def_id: DefId,
        var_hir_id: HirId,
    },
    Borrow {
        borrow_kind: BorrowKind,
        arg: Box<Expr>,
    },
    AddressOf {
        mutability: Mutability,
        arg: Box<Expr>,
    },
    Break {
        value: Option<Box<Expr>>,
    },
    Continue,
    Return {
        value: Option<Box<Expr>>,
    },
    Block(Block),
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Scope {
        region_scope: Scope,
        value: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_opt: Option<Box<Expr>>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<Arm>,
    },
    Loop {
        body: Box<Expr>,
    },
    Let {
        expr: Box<Expr>,
        pat: Pat,
    },
    LogicalOp {
        op: LogicalOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Use(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub targeted_by_break: bool,
    pub region: Scope,
    pub span: Span,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub safety_mode: BlockSafetyMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatString {
    pub parts: Vec<FormatTemplatePart>,
    pub args: Vec<Expr>,
    pub kwargs: Vec<FormatKwArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormatTemplatePart {
    Literal(String),
    Placeholder(FormatPlaceholder),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatPlaceholder {
    pub arg_ref: FormatArgRef,
    pub format_spec: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormatArgRef {
    Implicit,
    Positional(usize),
    Named(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatKwArg {
    pub name: String,
    pub value: Expr,
}

pub type ThirIntrinsicCall = IntrinsicCall<ThirIntrinsicCallPayload>;
pub type ThirIntrinsicCallPayload = GenericIntrinsicCallPayload<Expr, FormatString>;

#[derive(Debug, Clone, PartialEq)]
pub struct ItemRef {
    pub name: Symbol,
    pub def_id: Option<DefId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(Expr),
    Let {
        remainder_scope: Scope,
        init_scope: Scope,
        pattern: Pat,
        initializer: Option<Expr>,
        lint_level: LintLevel,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub pattern: Pat,
    pub guard: Option<Guard>,
    pub body: Expr,
    pub lint_level: LintLevel,
    pub scope: Scope,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Guard {
    pub cond: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pat {
    pub thir_id: ThirId,
    pub kind: PatKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatKind {
    Wild,
    Binding {
        mutability: Mutability,
        name: Symbol,
        mode: BindingMode,
        var: LocalId,
        ty: Ty,
    },
    Variant {
        adt_def: AdtDef,
        substs: SubstsRef,
        variant_index: VariantIdx,
        subpatterns: Vec<FieldPat>,
    },
    Leaf {
        subpatterns: Vec<FieldPat>,
    },
    Deref {
        subpattern: Box<Pat>,
    },
    Constant {
        value: ConstValue,
    },
    Range {
        lo: PatRangeBoundary,
        hi: PatRangeBoundary,
        end: RangeEnd,
    },
    Slice {
        prefix: Vec<Pat>,
        slice: Option<Box<Pat>>,
        suffix: Vec<Pat>,
    },
    Array {
        prefix: Vec<Pat>,
        slice: Option<Box<Pat>>,
        suffix: Vec<Pat>,
    },
    Or {
        pats: Vec<Pat>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPat {
    pub field: FieldIdx,
    pub pattern: Pat,
}

// Literal types
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Bool(bool),
    Int(i128, IntTy),
    Uint(u128, UintTy),
    Float(f64, FloatTy),
    Str(String),
    ByteStr(Vec<u8>),
    Byte(u8),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatTy {
    F32,
    F64,
}

// Operation types
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
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

// Type system support
#[derive(Debug, Clone, PartialEq)]
pub enum Mutability {
    Mut,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BorrowKind {
    Shared,
    Mut { allow_two_phase_borrow: bool },
    Unique,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindingMode {
    ByValue,
    ByRef(BorrowKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockSafetyMode {
    Safe,
    Unsafe,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Restricted { path: Path, id: NodeId },
    Inherited,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantData {
    Struct(Vec<FieldDef>, bool),
    Tuple(Vec<FieldDef>),
    Unit,
}

// Forward declarations and type aliases
pub type Scope = u32;
pub type LintLevel = u32;
pub type Symbol = String;
pub type Span = crate::span::Span;
pub type DefId = ty::DefId;
pub type HirId = u32;
pub type NodeId = u32;
pub type FieldIdx = usize;
pub type VariantIdx = usize;
pub type AdtDef = (); // Placeholder
pub type SubstsRef = (); // Placeholder
pub type ConstValue = (); // Placeholder
pub type TraitRef = (); // Placeholder
pub type Path = (); // Placeholder
pub type FieldDef = (); // Placeholder
pub type SourceInfo = Span;

#[derive(Debug, Clone, PartialEq)]
pub enum PatRangeBoundary {
    Finite(ConstValue),
    NegInfinity,
    PosInfinity,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RangeEnd {
    Included,
    Excluded,
}

// Implementation helpers
impl Program {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bodies: HashMap::new(),
            next_thir_id: 0,
        }
    }

    pub fn next_id(&mut self) -> ThirId {
        let id = self.next_thir_id;
        self.next_thir_id += 1;
        id
    }

    pub fn body(&self, id: BodyId) -> &Body {
        &self.bodies[&id]
    }

    pub fn body_mut(&mut self, id: BodyId) -> &mut Body {
        self.bodies.get_mut(&id).unwrap()
    }
}

impl Expr {
    pub fn new(thir_id: ThirId, kind: ExprKind, ty: Ty, span: Span) -> Self {
        Self {
            thir_id,
            kind,
            ty,
            span,
        }
    }
}

impl Pat {
    pub fn new(thir_id: ThirId, kind: PatKind, ty: Ty, span: Span) -> Self {
        Self {
            thir_id,
            kind,
            ty,
            span,
        }
    }
}

impl BodyId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}
