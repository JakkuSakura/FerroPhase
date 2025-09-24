use std::collections::HashMap;

pub mod ty;
pub use ty::Ty;

pub type ThirId = u32;
pub type LocalId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct ThirProgram {
    pub items: Vec<ThirItem>,
    pub bodies: HashMap<BodyId, ThirBody>,
    pub next_thir_id: ThirId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirItem {
    pub thir_id: ThirId,
    pub kind: ThirItemKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ThirItemKind {
    Function(ThirFunction),
    Struct(ThirStruct),
    Const(ThirConst),
    Impl(ThirImpl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunction {
    pub sig: ThirFunctionSig,
    pub body_id: Option<BodyId>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunctionSig {
    pub inputs: Vec<Ty>,
    pub output: Ty,
    pub c_variadic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirStruct {
    pub fields: Vec<ThirStructField>,
    pub variant_data: VariantData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirStructField {
    pub thir_id: ThirId,
    pub ty: Ty,
    pub vis: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirConst {
    pub ty: Ty,
    pub body_id: BodyId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirImpl {
    pub self_ty: Ty,
    pub items: Vec<ThirImplItem>,
    pub trait_ref: Option<TraitRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirImplItem {
    pub thir_id: ThirId,
    pub kind: ThirImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ThirImplItemKind {
    Method(ThirFunction),
    AssocConst(ThirConst),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct ThirBody {
    pub params: Vec<Param>,
    pub value: ThirExpr,
    pub locals: Vec<LocalDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ty: Ty,
    pub pat: Option<ThirPat>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDecl {
    pub ty: Ty,
    pub source_info: SourceInfo,
    pub internal: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirExpr {
    pub thir_id: ThirId,
    pub kind: ThirExprKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ThirExprKind {
    Literal(ThirLit),
    Local(LocalId),
    /// Reference to a function, constant, or global item.
    Path(ItemRef),
    Binary(BinOp, Box<ThirExpr>, Box<ThirExpr>),
    Unary(UnOp, Box<ThirExpr>),
    Cast(Box<ThirExpr>, Ty),
    Call {
        fun: Box<ThirExpr>,
        args: Vec<ThirExpr>,
        from_hir_call: bool,
    },
    Deref(Box<ThirExpr>),
    Index(Box<ThirExpr>, Box<ThirExpr>),
    Field {
        base: Box<ThirExpr>,
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
        arg: Box<ThirExpr>,
    },
    AddressOf {
        mutability: Mutability,
        arg: Box<ThirExpr>,
    },
    Break {
        value: Option<Box<ThirExpr>>,
    },
    Continue,
    Return {
        value: Option<Box<ThirExpr>>,
    },
    Block(ThirBlock),
    Assign {
        lhs: Box<ThirExpr>,
        rhs: Box<ThirExpr>,
    },
    AssignOp {
        op: BinOp,
        lhs: Box<ThirExpr>,
        rhs: Box<ThirExpr>,
    },
    Scope {
        region_scope: Scope,
        value: Box<ThirExpr>,
    },
    If {
        cond: Box<ThirExpr>,
        then: Box<ThirExpr>,
        else_opt: Option<Box<ThirExpr>>,
    },
    Match {
        scrutinee: Box<ThirExpr>,
        arms: Vec<Arm>,
    },
    Loop {
        body: Box<ThirExpr>,
    },
    Let {
        expr: Box<ThirExpr>,
        pat: ThirPat,
    },
    LogicalOp {
        op: LogicalOp,
        lhs: Box<ThirExpr>,
        rhs: Box<ThirExpr>,
    },
    Use(Box<ThirExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirBlock {
    pub targeted_by_break: bool,
    pub region: Scope,
    pub span: Span,
    pub stmts: Vec<ThirStmt>,
    pub expr: Option<Box<ThirExpr>>,
    pub safety_mode: BlockSafetyMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemRef {
    pub name: Symbol,
    pub def_id: Option<DefId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirStmt {
    pub kind: ThirStmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ThirStmtKind {
    Expr(ThirExpr),
    Let {
        remainder_scope: Scope,
        init_scope: Scope,
        pattern: ThirPat,
        initializer: Option<ThirExpr>,
        lint_level: LintLevel,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub pattern: ThirPat,
    pub guard: Option<Guard>,
    pub body: ThirExpr,
    pub lint_level: LintLevel,
    pub scope: Scope,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Guard {
    pub cond: ThirExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirPat {
    pub thir_id: ThirId,
    pub kind: ThirPatKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ThirPatKind {
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
        subpattern: Box<ThirPat>,
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
        prefix: Vec<ThirPat>,
        slice: Option<Box<ThirPat>>,
        suffix: Vec<ThirPat>,
    },
    Array {
        prefix: Vec<ThirPat>,
        slice: Option<Box<ThirPat>>,
        suffix: Vec<ThirPat>,
    },
    Or {
        pats: Vec<ThirPat>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPat {
    pub field: FieldIdx,
    pub pattern: ThirPat,
}

// Literal types
#[derive(Debug, Clone, PartialEq)]
pub enum ThirLit {
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
impl ThirProgram {
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

    pub fn body(&self, id: BodyId) -> &ThirBody {
        &self.bodies[&id]
    }

    pub fn body_mut(&mut self, id: BodyId) -> &mut ThirBody {
        self.bodies.get_mut(&id).unwrap()
    }
}

impl ThirExpr {
    pub fn new(thir_id: ThirId, kind: ThirExprKind, ty: Ty, span: Span) -> Self {
        Self {
            thir_id,
            kind,
            ty,
            span,
        }
    }
}

impl ThirPat {
    pub fn new(thir_id: ThirId, kind: ThirPatKind, ty: Ty, span: Span) -> Self {
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
