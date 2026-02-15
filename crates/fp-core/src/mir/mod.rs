use std::collections::HashMap;

use crate::intrinsics::IntrinsicCallKind;

pub mod ident;
pub mod pretty;
pub mod ty;

pub use ident::{Path, Symbol};
pub use ty::Ty;

pub type MirId = u32;
pub type LocalId = u32;
pub type BasicBlockId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub mir_id: MirId,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Static(Static),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Symbol,
    pub path: Vec<Symbol>,
    pub def_id: Option<ty::DefId>,
    pub sig: FunctionSig,
    pub body_id: BodyId,
    pub abi: ty::Abi,
    pub is_extern: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    pub inputs: Vec<Ty>,
    pub output: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Static {
    pub ty: Ty,
    pub init: Operand,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub basic_blocks: Vec<BasicBlockData>,
    pub locals: Vec<LocalDecl>,
    pub arg_count: usize,
    pub return_local: LocalId,
    pub var_debug_info: Vec<VarDebugInfo>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
    pub is_cleanup: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub source_info: SourceInfo,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    IntrinsicCall {
        kind: IntrinsicCallKind,
        format: String,
        args: Vec<Operand>,
    },
    SetDiscriminant {
        place: Place,
        variant_index: VariantIdx,
    },
    StorageLive(LocalId),
    StorageDead(LocalId),
    Retag(RetagKind, Place),
    AscribeUserType(Place, UserTypeProjection, Variance),
    Nop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Terminator {
    pub source_info: SourceInfo,
    pub kind: TerminatorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TerminatorKind {
    Goto {
        target: BasicBlockId,
    },
    SwitchInt {
        discr: Operand,
        switch_ty: Ty,
        targets: SwitchTargets,
    },
    Resume,
    Abort,
    Return,
    Unreachable,
    Drop {
        place: Place,
        target: BasicBlockId,
        unwind: Option<BasicBlockId>,
    },
    DropAndReplace {
        place: Place,
        value: Operand,
        target: BasicBlockId,
        unwind: Option<BasicBlockId>,
    },
    Call {
        func: Operand,
        args: Vec<Operand>,
        destination: Option<(Place, BasicBlockId)>,
        cleanup: Option<BasicBlockId>,
        from_hir_call: bool,
        fn_span: Span,
    },
    Assert {
        cond: Operand,
        expected: bool,
        msg: AssertMessage,
        target: BasicBlockId,
        cleanup: Option<BasicBlockId>,
    },
    Yield {
        value: Operand,
        resume: BasicBlockId,
        resume_arg: Place,
        drop: Option<BasicBlockId>,
    },
    GeneratorDrop,
    FalseEdge {
        real_target: BasicBlockId,
        imaginary_target: BasicBlockId,
    },
    FalseUnwind {
        real_target: BasicBlockId,
        unwind: Option<BasicBlockId>,
    },
    InlineAsm {
        template: Vec<InlineAsmTemplatePiece>,
        operands: Vec<InlineAsmOperand>,
        options: InlineAsmOptions,
        line_spans: Vec<Span>,
        destination: Option<BasicBlockId>,
        cleanup: Option<BasicBlockId>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchTargets {
    pub values: Vec<u128>,
    pub targets: Vec<BasicBlockId>,
    pub otherwise: BasicBlockId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rvalue {
    Use(Operand),
    IntrinsicCall {
        kind: IntrinsicCallKind,
        format: String,
        args: Vec<Operand>,
    },
    Repeat(Operand, u64),
    Ref(Region, BorrowKind, Place),
    ThreadLocalRef(DefId),
    AddressOf(Mutability, Place),
    Len(Place),
    Cast(CastKind, Operand, Ty),
    BinaryOp(BinOp, Operand, Operand),
    CheckedBinaryOp(BinOp, Operand, Operand),
    NullaryOp(NullOp, Ty),
    UnaryOp(UnOp, Operand),
    Discriminant(Place),
    Aggregate(AggregateKind, Vec<Operand>),
    ContainerLiteral {
        kind: ContainerKind,
        elements: Vec<Operand>,
    },
    ContainerMapLiteral {
        kind: ContainerKind,
        entries: Vec<(Operand, Operand)>,
    },
    ContainerLen {
        kind: ContainerKind,
        container: Operand,
    },
    ContainerGet {
        kind: ContainerKind,
        container: Operand,
        key: Operand,
    },
    ShallowInitBox(Operand, Ty),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(Constant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    pub local: LocalId,
    pub projection: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlaceElem {
    Deref,
    Field(FieldIdx, Ty),
    Index(LocalId),
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },
    Downcast(Option<Symbol>, VariantIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    pub span: Span,
    pub user_ty: Option<UserTypeAnnotationIndex>,
    pub literal: ConstantKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantKind {
    Ty(ConstTy),
    Val(ConstValue, Ty),
    /// Reference to a function by name (simple)
    Fn(Symbol, Ty),
    /// Reference to a global constant by name
    Global(Symbol, Ty),
    Null,
    Int(i64),
    UInt(u64),
    Float(f64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDecl {
    pub mutability: Mutability,
    pub local_info: LocalInfo,
    pub internal: bool,
    pub is_block_tail: Option<BlockTailInfo>,
    pub ty: Ty,
    pub user_ty: Option<UserTypeProjections>,
    pub source_info: SourceInfo,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LocalInfo {
    User(ClearCrossCrate<BindingForm>),
    StaticRef {
        def_id: DefId,
        is_thread_local: bool,
    },
    Other,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDebugInfo {
    pub name: Symbol,
    pub source_info: SourceInfo,
    pub value: VarDebugInfoContents,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDebugInfoContents {
    Place(Place),
    Const(Constant),
}

// Type definitions and placeholders
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
pub enum CastKind {
    Misc,
    Pointer(PointerCast),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PointerCast {
    ReifyFnPointer,
    UnsafeFnPointer,
    ClosureFnPointer,
    MutToConstPointer,
    ArrayToPointer,
    Unsize,
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
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NullOp {
    Box,
    SizeOf,
    AlignOf,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AggregateKind {
    Array(Ty),
    Tuple,
    Adt(
        AdtDef,
        VariantIdx,
        SubstsRef,
        Option<UserTypeAnnotationIndex>,
    ),
    Closure(DefId, SubstsRef),
    Generator(DefId, SubstsRef, Movability),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContainerKind {
    List { elem_ty: Ty, len: u64 },
    Map { key_ty: Ty, value_ty: Ty, len: u64 },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RetagKind {
    FnEntry,
    TwoPhase,
    Raw,
    Default,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssertMessage {
    BoundsCheck { len: Operand, index: Operand },
    Overflow(BinOp, Operand, Operand),
    OverflowNeg(Operand),
    DivisionByZero(Operand),
    RemainderByZero(Operand),
    ResumedAfterReturn,
    ResumedAfterPanic,
    GeneratorResumedAfterReturn,
    GeneratorResumedAfterPanic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockTailInfo {
    pub tail_result_is_ignored: bool,
    pub span: Span,
}

// Forward declarations and type aliases
pub type Span = crate::span::Span;
pub type DefId = ty::DefId;
pub type FieldIdx = usize;
pub type VariantIdx = usize;
pub type UserTypeAnnotationIndex = u32;
pub type SourceInfo = Span;
pub type AdtDef = (); // Placeholder
pub type SubstsRef = (); // Placeholder
pub type ConstTy = (); // Placeholder
pub type Region = (); // Placeholder
pub type UserTypeProjection = (); // Placeholder
pub type UserTypeProjections = Vec<UserTypeProjection>;
pub type Variance = (); // Placeholder
pub type Movability = (); // Placeholder
pub type ClearCrossCrate<T> = T; // Placeholder
pub type BindingForm = (); // Placeholder
pub type InlineAsmTemplatePiece = String; // Placeholder
pub type InlineAsmOperand = (); // Placeholder
pub type InlineAsmOptions = u32; // Placeholder

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Str(String),
    Null,
    Fn(Symbol),
    Tuple(Vec<ConstValue>),
    Array(Vec<ConstValue>),
    Struct(Vec<ConstValue>),
    List {
        elements: Vec<ConstValue>,
        elem_ty: ty::Ty,
    },
    Map {
        entries: Vec<(ConstValue, ConstValue)>,
        key_ty: ty::Ty,
        value_ty: ty::Ty,
    },
}

// Implementation helpers
impl Program {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bodies: HashMap::new(),
        }
    }
}

impl Body {
    pub fn new(
        basic_blocks: Vec<BasicBlockData>,
        locals: Vec<LocalDecl>,
        arg_count: usize,
        span: Span,
    ) -> Self {
        Self {
            basic_blocks,
            locals,
            arg_count,
            return_local: 0, // First local is always return
            var_debug_info: Vec::new(),
            span,
        }
    }

    pub fn local_kind(&self, local: LocalId) -> LocalKind {
        let local = local as usize;
        if local == 0 {
            LocalKind::ReturnPointer
        } else if local <= self.arg_count {
            LocalKind::Arg
        } else {
            LocalKind::Temp
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LocalKind {
    Arg,
    Var,
    Temp,
    ReturnPointer,
}

impl BasicBlockData {
    pub fn new(terminator: Option<Terminator>) -> Self {
        Self {
            statements: Vec::new(),
            terminator,
            is_cleanup: false,
        }
    }
}

impl Program {
    pub fn span(&self) -> Span {
        Span::union(self.items.iter().map(Item::span))
    }
}

impl Item {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl ItemKind {
    pub fn span(&self) -> Span {
        match self {
            ItemKind::Function(func) => func.span(),
            ItemKind::Static(stat) => stat.span(),
        }
    }
}

impl Function {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl FunctionSig {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl Static {
    pub fn span(&self) -> Span {
        self.init.span()
    }
}

impl Body {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl BasicBlockData {
    pub fn span(&self) -> Span {
        Span::union(
            self.statements
                .iter()
                .map(Statement::span)
                .chain(self.terminator.as_ref().map(Terminator::span)),
        )
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        self.source_info
    }
}

impl StatementKind {
    pub fn span(&self) -> Span {
        match self {
            StatementKind::Assign(_, rvalue) => rvalue.span(),
            StatementKind::IntrinsicCall { args, .. } => {
                Span::union(args.iter().map(Operand::span))
            }
            StatementKind::SetDiscriminant { .. }
            | StatementKind::StorageLive(_)
            | StatementKind::StorageDead(_)
            | StatementKind::Retag(_, _)
            | StatementKind::AscribeUserType(_, _, _)
            | StatementKind::Nop => Span::null(),
        }
    }
}

impl Terminator {
    pub fn span(&self) -> Span {
        self.source_info
    }
}

impl TerminatorKind {
    pub fn span(&self) -> Span {
        match self {
            TerminatorKind::Goto { .. }
            | TerminatorKind::Resume
            | TerminatorKind::Abort
            | TerminatorKind::Return
            | TerminatorKind::Unreachable
            | TerminatorKind::GeneratorDrop => Span::null(),
            TerminatorKind::SwitchInt { discr, .. } => discr.span(),
            TerminatorKind::Drop { place, .. } => place.span(),
            TerminatorKind::DropAndReplace { place, value, .. } => {
                Span::union([place.span(), value.span()])
            }
            TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } => Span::union(
                Some(func.span())
                    .into_iter()
                    .chain(args.iter().map(Operand::span))
                    .chain(destination.as_ref().map(|(place, _)| place.span())),
            ),
            TerminatorKind::Assert { cond, .. } => cond.span(),
            TerminatorKind::Yield {
                value, resume_arg, ..
            } => Span::union([value.span(), resume_arg.span()]),
            TerminatorKind::FalseEdge { .. } | TerminatorKind::FalseUnwind { .. } => Span::null(),
            TerminatorKind::InlineAsm { line_spans, .. } => Span::union(line_spans.iter().copied()),
        }
    }
}

impl Rvalue {
    pub fn span(&self) -> Span {
        match self {
            Rvalue::Use(operand) => operand.span(),
            Rvalue::IntrinsicCall { args, .. } => Span::union(args.iter().map(Operand::span)),
            Rvalue::Repeat(op, _) => op.span(),
            Rvalue::Ref(_, _, place) => place.span(),
            Rvalue::ThreadLocalRef(_) => Span::null(),
            Rvalue::AddressOf(_, place) => place.span(),
            Rvalue::Len(place) => place.span(),
            Rvalue::Cast(_, op, _) => op.span(),
            Rvalue::BinaryOp(_, lhs, rhs) | Rvalue::CheckedBinaryOp(_, lhs, rhs) => {
                Span::union([lhs.span(), rhs.span()])
            }
            Rvalue::NullaryOp(_, _) => Span::null(),
            Rvalue::UnaryOp(_, op) => op.span(),
            Rvalue::Discriminant(place) => place.span(),
            Rvalue::Aggregate(_, ops) => Span::union(ops.iter().map(Operand::span)),
            Rvalue::ContainerLiteral { elements, .. } => {
                Span::union(elements.iter().map(Operand::span))
            }
            Rvalue::ContainerMapLiteral { entries, .. } => {
                Span::union(entries.iter().flat_map(|(k, v)| [k.span(), v.span()]))
            }
            Rvalue::ContainerLen { container, .. } => container.span(),
            Rvalue::ContainerGet { container, key, .. } => {
                Span::union([container.span(), key.span()])
            }
            Rvalue::ShallowInitBox(op, _) => op.span(),
        }
    }
}

impl Operand {
    pub fn span(&self) -> Span {
        match self {
            Operand::Copy(place) | Operand::Move(place) => place.span(),
            Operand::Constant(constant) => constant.span,
        }
    }
}

impl Place {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl Constant {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl LocalDecl {
    pub fn span(&self) -> Span {
        self.source_info
    }
}

impl VarDebugInfo {
    pub fn span(&self) -> Span {
        self.source_info
    }
}

impl VarDebugInfoContents {
    pub fn span(&self) -> Span {
        match self {
            VarDebugInfoContents::Place(place) => place.span(),
            VarDebugInfoContents::Const(constant) => constant.span,
        }
    }
}

impl BlockTailInfo {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl Place {
    pub fn from_local(local: LocalId) -> Self {
        Self {
            local,
            projection: Vec::new(),
        }
    }

    pub fn project_deeper(mut self, elem: PlaceElem) -> Self {
        self.projection.push(elem);
        self
    }
}

impl Operand {
    pub fn copy(place: Place) -> Self {
        Self::Copy(place)
    }

    pub fn move_op(place: Place) -> Self {
        Self::Move(place)
    }

    pub fn constant(constant: Constant) -> Self {
        Self::Constant(constant)
    }
}

impl BodyId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}
