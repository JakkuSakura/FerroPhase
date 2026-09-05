use crate::ast::{TypeBinaryOpKind, TypePrimitive};
use crate::intrinsics::CallKind;
use crate::query::{QueryIrDocument, QueryOrigin};
use std::collections::{HashMap, HashSet};
use std::fmt;

pub type PackageId = crate::package::PackageId;

pub mod ident;
pub mod package;
pub mod place;
pub mod pretty;
pub mod program;
pub mod refinement;
pub mod resolve;
pub mod ty;

pub use ident::Symbol;
pub use package::HirPackage;
pub use program::HirProgram;
pub use refinement::{ParamSlot, RefinementHint};
pub use ty::{Abi, Ty};

pub type NodeId = u32;

/// HIR's own name for a runtime/const value — the same representation
/// `ast::Value` already is (comptime results don't need a distinct
/// HIR-shaped value type), aliased here so HIR-owned data
/// (`HirPackage`'s typed-results fields, ...) names it as `hir::Value`, not
/// a lower layer's type reaching up into this one.
pub type Value = crate::ast::Value;

/// The `DefId` of the closest enclosing item-like definition for a HIR node
/// (rustc's `OwnerId`). Wrapping a real `DefId` means the owner already
/// carries its defining `PackageId`, so two separately-lowered packages can
/// never produce colliding node ids even though each mints its own
/// `ItemLocalId`s from zero.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OwnerId(pub DefId);

impl OwnerId {
    /// The package-root owner, used for nodes lowered outside any item-like
    /// definition (a top-level type alias, a standalone expression's
    /// synthesized `main`, ...). `DefId` index 0 is reserved for this
    /// sentinel — real items are minted from index 1 (see `HirPackage::
    /// next_def_id`).
    pub fn root(package_id: PackageId) -> Self {
        OwnerId(DefId::new(package_id, 0))
    }
}

/// An index unique only within a single `OwnerId`'s scope (rustc's
/// `ItemLocalId`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ItemLocalId(pub u32);

/// Identifies a HIR node with rustc's two-level id: the `owner` (enclosing
/// item-like definition) plus a `local_id` unique only within that owner's
/// scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HirId {
    pub owner: OwnerId,
    pub local_id: ItemLocalId,
}

impl HirId {
    pub fn new(owner: OwnerId, local_id: u32) -> Self {
        Self {
            owner,
            local_id: ItemLocalId(local_id),
        }
    }

    pub fn package_id(&self) -> &PackageId {
        &self.owner.0.package_id
    }

    pub fn local_id(&self) -> u32 {
        self.local_id.0
    }
}

impl Default for HirId {
    fn default() -> Self {
        Self::new(OwnerId::root(PackageId::new("__dummy__")), 0)
    }
}

impl fmt::Display for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.owner.0, self.local_id.0)
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct DefId {
    pub package_id: PackageId,
    pub index: u32,
}

impl DefId {
    /// A "local, unqualified index" `DefId` with no real package identity
    /// — used only where the caller genuinely has no package context (see
    /// call sites); prefer `DefId::new` with a real `PackageId` wherever
    /// one is available.
    pub fn local(index: u32) -> Self {
        Self {
            package_id: PackageId::new(""),
            index,
        }
    }

    pub fn new(package_id: PackageId, index: u32) -> Self {
        Self { package_id, index }
    }

    pub fn saturating_add(self, amount: u32) -> Self {
        Self {
            index: self.index.saturating_add(amount),
            ..self
        }
    }

    /// A deterministic string identity for this `DefId`, for the one
    /// place a `DefId` still needs to be addressed by a plain string (the
    /// LIR interpreter's own global table, which is string-keyed) —
    /// computed purely from the `DefId`'s own fields, never from a
    /// source span or a surface name. Every site that needs to name the
    /// same comptime-pending const (building its `LirComptimeEntry`/
    /// `mir::ExecutableConst` key, and building a `Global` operand that
    /// references it elsewhere) calls this on the same `def_id`, so
    /// there is only ever one name because there is only ever one
    /// function producing it from the one real identity.
    pub fn comptime_const_symbol(&self) -> String {
        format!("__fp_const_{}_{}", self.package_id.as_str(), self.index)
    }
}

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.package_id, self.index)
    }
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
    TypeAlias(TypeAlias),
    Const(Const),
    Impl(Impl),
    Trait(Trait),
    Query(Query),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: Symbol,
    pub target: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub sig: FunctionSig,
    pub body: Option<Block>,
    pub is_const: bool,
    pub is_extern: bool,
    pub is_async: bool,
    pub attrs: Vec<crate::ast::Attribute>,
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
    pub is_context: bool,
    pub as_tuple: bool,
    pub as_dict: bool,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Symbol,
    pub fields: Vec<StructField>,
    pub generics: Generics,
    pub repr: crate::ast::ReprOptions,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    /// Source declaration metadata retained through HIR for target backends
    /// that need semantic derives or attributes after type checking.
    pub attrs: Vec<crate::ast::Attribute>,
    pub name: Symbol,
    pub variants: Vec<EnumVariant>,
    pub generics: Generics,
    pub repr: crate::ast::ReprOptions,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub attrs: Vec<crate::ast::Attribute>,
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
    pub mutable: bool,
    pub is_host: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    pub generics: Generics,
    pub trait_ty: Option<TypeExpr>,
    pub self_ty: TypeExpr,
    pub items: Vec<ImplItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Query {
    pub origin: QueryOrigin,
    pub ir: QueryIrDocument,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    pub def_id: DefId,
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: ImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    Method(Function),
    AssocConst(Const),
    AssocType(AssocType),
}

/// A trait definition — declares the methods/associated types every `impl
/// Trait for X` is expected to provide (or inherit from a default). Unlike
/// `Impl`, this is never `Self`-specific: it's the shared declaration every
/// concrete impl is checked/resolved against (see `HirTypeChecker::
/// method_output`'s trait-default-method fallback, which searches here when
/// a concrete impl doesn't redeclare a requested method itself).
#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub generics: Generics,
    pub items: Vec<TraitItem>,
    /// This trait's own supertrait bounds (`trait Fn<Args>: FnMut<Args>`)
    /// — real `core::ops::function`'s own `Fn`/`FnMut` declare no
    /// associated types of their own at all; `Output` is declared only on
    /// `FnOnce`, reached solely through this chain. Needed so a still-
    /// generic `F::Output` projection (`F: Fn<A>`) can find `Output` by
    /// walking supertraits, the same way real Rust's own associated-type
    /// lookup does.
    pub supertraits: Vec<Path>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitItem {
    pub def_id: DefId,
    pub hir_id: HirId,
    pub name: Symbol,
    pub kind: TraitItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TraitItemKind {
    /// A trait method — `Function.body` is `Some` for a default-provided
    /// method (e.g. `Iterator::map`) and `None` for one every impl must
    /// supply itself (e.g. `Iterator::next`). Only the `Some` case is ever
    /// used as a fallback signature source; an impl that doesn't redeclare
    /// an abstract (`None`-bodied) method is a genuine error, not something
    /// to fall back on.
    Method(Function),
    /// An associated constant declaration. A trait may provide a default
    /// initializer, but an abstract associated constant has no body.
    AssocConst(TraitAssocConst),
    /// A bare `type Item;` declaration — no bound type (that binding is
    /// always on the impl side, `ImplItemKind::AssocType`); this only
    /// records that the name exists so a trait method's signature can
    /// reference `Self::Item` and have somewhere to resolve it from.
    AssocType(TraitAssocType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitAssocConst {
    pub name: Symbol,
    pub ty: TypeExpr,
    pub body: Option<Body>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitAssocType {
    pub name: Symbol,
    /// Bounds declared on the associated type, such as
    /// `type Owned: Borrow<Self>`.
    pub bounds: Vec<TypeExpr>,
}

/// An impl block's own `type Target = Y;` binding for one of its trait's
/// associated types. No `body`/default resolution here — this is always
/// the CURRENT impl's own concrete binding (see `HirTypeChecker::
/// impl_assoc_types`, which is deliberately scoped to just this, not full
/// trait-default/witness resolution).
#[derive(Debug, Clone, PartialEq)]
pub struct AssocType {
    pub name: Symbol,
    pub ty: TypeExpr,
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
    Path(QPath),
    Query(Query),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Reference(ExprReference),
    Call(Box<Expr>, Vec<CallArg>),
    MethodCall(Box<Expr>, Symbol, Option<GenericArgs>, Vec<CallArg>),
    FieldAccess(Box<Expr>, Symbol),
    Index(Box<Expr>, Box<Expr>),
    Slice(SliceExpr),
    Cast(Box<Expr>, Box<TypeExpr>),
    Struct(QPath, Vec<StructExprField>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<MatchArm>),
    Try(TryExpr),
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
    With(Box<Expr>, Box<Expr>),
    Array(Vec<Expr>),
    ArrayRepeat {
        elem: Box<Expr>,
        len: Box<Expr>,
    },
    Tuple(Vec<Expr>),
    /// A `const { ... }` block. Structurally this node IS the const
    /// context indicator: the type checker eagerly resolves `body`'s
    /// value via `TypingShared::request_comptime` whenever it encounters
    /// this variant, independent of any name.
    ConstBlock(ExprConstBlock),
    /// A closure literal, kept as a first-class node (params + body, no
    /// struct/function synthesis) — mirrors rustc's own ordering: a
    /// closure stays a real, richly-typed expression throughout type
    /// checking, with its parameter/return types resolved via ordinary
    /// expected-type propagation from its call site (see
    /// `HirTypeChecker`'s `Closure` arm and `expected_expr_types`), and
    /// only gets "compiled away" into a captures-struct-plus-call-function
    /// shape later, as a lowering concern (`HirLoweringConfig::
    /// defunctionalize_closures` controls whether a closure ever reaches
    /// this variant at all, or is defunctionalized earlier by
    /// `ClosureLowering` for pipelines — e.g. Native — that need MIR).
    Closure(ExprClosure),
    /// A real, un-desugared `for pat in iter { body }` — only ever
    /// constructed when the target's `LanguageCapabilities::
    /// first_class_for_loops` is set (see `ast_to_hir::exprs::
    /// transform_for_to_hir`); every pipeline that hasn't opted in
    /// (in particular `PipelineMode::Native`, whose MIR has no iterator-
    /// protocol concept) still eagerly desugars into an index-based
    /// `While`/`Loop` before HIR generation, exactly as before this
    /// variant existed, so this is simply never produced for those.
    For(Box<Pat>, Box<Expr>, Block),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprConstBlock {
    /// This const block's own identity, minted the same way every other
    /// item/def is during AST-to-HIR lowering (see
    /// `AstToHirLowerer::next_def_id`) — used to key its resolved comptime
    /// value in `HirPackage::const_block_values`, so every comptime unit
    /// (named consts and const blocks alike) is identified the same way.
    pub def_id: DefId,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprClosure {
    /// Each parameter's declared type is `TypeExprKind::Infer` unless the
    /// source explicitly annotated it — real closures are overwhelmingly
    /// unannotated (`|s| ..`), relying entirely on expected-type inference
    /// from the call site, same as rustc.
    pub params: Vec<Param>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub hir_id: HirId,
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryExpr {
    pub expr: Box<Expr>,
    pub catches: Vec<TryCatch>,
    pub elze: Option<Box<Expr>>,
    pub finally: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryCatch {
    pub hir_id: HirId,
    pub pat: Option<Pat>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprReference {
    pub hir_id: HirId,
    pub mutable: crate::hir::ty::Mutability,
    pub raw: bool,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SliceExpr {
    pub hir_id: HirId,
    pub base: Box<Expr>,
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub inclusive: bool,
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
    pub kind: CallKind,
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
    Struct(QPath, Vec<PatField>, bool),
    TupleStruct(QPath, Vec<Pat>),
    Variant(QPath),
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
    Path(QPath),
    Structural(TypeStructural),
    TypeBinaryOp(TypeBinaryOp),
    Tuple(Vec<Box<TypeExpr>>),
    Array(Box<TypeExpr>, Option<Box<Expr>>),
    Slice(Box<TypeExpr>),
    Ptr {
        inner: Box<TypeExpr>,
        mutable: bool,
    },
    Ref(Box<TypeExpr>),
    FnPtr(FnPtrType),
    /// A dynamic trait object. The first path is the principal trait and
    /// remaining paths are additional (normally auto-trait) bounds.
    Dynamic(Vec<Path>),
    /// A `const { ... }` block appearing in type position (either the value
    /// of a `type X = const { ... };` alias or nested inside another type,
    /// e.g. an array length). The block's own const-ness comes purely from
    /// appearing here structurally; its value is resolved by the type
    /// checker via `TypingShared::request_comptime`. The `DefId` is this
    /// block's own identity (see `ExprConstBlock::def_id`'s doc comment) —
    /// used to key its resolved value in `HirPackage::const_block_values`.
    ConstBlock(DefId, Box<Expr>),
    Never,
    Infer,
    Error,
    /// The `type`/`type<T>` surface annotation — a compile-time value that
    /// is itself a type (e.g. `TypeBuilder.ty: type`, mutated by
    /// `create_struct`/`addfield`/`build_type`). Lowers to `TyKind::Type`:
    /// an opaque handle into the comptime interpreter's own type pool, not
    /// a plain integer — see `TyKind::Type`'s own doc comment.
    Type,
    /// The `any` surface annotation — a fully type-erased runtime value
    /// (e.g. `spawn(fut: any) -> any`). Lowers to `TyKind::Any`: a fixed,
    /// concrete "erased" type, not an inference placeholder — see
    /// `TyKind::Any`'s own doc comment for why this must not reuse `Infer`.
    Any,
    /// `{binder : base // predicate}` — a refinement/subtype type (Lean 4's
    /// `Subtype`). Purely syntactic, like `ConstBlock`: the type checker
    /// discharges `predicate` (via `decide`/`omega`, see `fp-typing`'s
    /// `refinement` module) and resolves this to `base`'s `TyKind` directly.
    /// There is deliberately no corresponding `TyKind::Refinement` — nothing
    /// past typing ever needs to know this existed.
    Refinement {
        base: Box<TypeExpr>,
        binder: Symbol,
        predicate: Box<Expr>,
    },
    /// A string literal type, e.g. `"foo"`. Purely syntactic, like
    /// `Refinement`: the type checker resolves the literal string and
    /// erases this to `TyKind::Slice(i8)` (the same shape a plain `str`
    /// resolves to) — no `TyKind` counterpart exists.
    LiteralString(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnPtrType {
    pub inputs: Vec<Box<TypeExpr>>,
    pub output: Box<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path<R = Res> {
    /// Source span covering the complete path.
    pub span: Span,
    /// Resolution of the path's terminal item for an ordinary `QPath::Resolved`
    /// path. A `Path` always owns the complete sequence of ordinary segments;
    /// an associated-item tail is represented by `QPath::TypeRelative`
    /// instead of being encoded as a truncated `Path`.
    pub res: R,
    /// Complete path segments, matching rustc HIR's `Path`: module/type
    /// prefixes and the terminal item are retained in order. Generic
    /// arguments stay attached to their corresponding segment.
    pub segments: Vec<PathSegment>,
}

impl<R> Path<R> {
    pub fn new(res: R, segments: Vec<PathSegment>) -> Self {
        Self {
            span: Span::null(),
            res,
            segments,
        }
    }

    pub fn with_span(span: Span, res: R, segments: Vec<PathSegment>) -> Self {
        Self { span, res, segments }
    }

    pub fn base(res: R) -> Self {
        Self::new(res, Vec::new())
    }

    /// Uniform accessors shared with `QPath` consumers.
    pub fn segments(&self) -> &[PathSegment] {
        &self.segments
    }

    pub fn segments_mut(&mut self) -> &mut [PathSegment] {
        &mut self.segments
    }

    pub fn res_ref(&self) -> &R {
        &self.res
    }
}

impl<R: Clone> Path<R> {
    pub fn res(&self) -> R {
        self.res.clone()
    }
}

/// A qualified HIR path, matching rustc's split between ordinary resolved
/// paths and type-relative associated-item paths.
#[derive(Debug, Clone, PartialEq)]
pub enum QPath {
    /// An ordinary path, optionally explicitly qualified by a `Self` type:
    /// `Trait::Item` or `<T as Trait>::Item`.
    Resolved(Option<Box<TypeExpr>>, Path),
    /// An associated item whose receiver is a type and whose single item
    /// segment is resolved during type checking: `<T>::Assoc` or `T::Assoc`.
    /// The receiver's own path (when path-shaped) remains a complete
    /// `QPath::Resolved` path; it is not folded into this segment.
    TypeRelative(Box<TypeExpr>, PathSegment),
}

impl QPath {
    /// Return the span of the qualified receiver (`qself`) in this path.
    ///
    /// For `QPath::Resolved`, rustc uses the path span because the receiver
    /// is represented separately from (and has no independent span in) the
    /// HIR path. Type-relative paths retain the receiver's own span.
    pub fn qself_span(&self) -> Span {
        match self {
            Self::Resolved(_, path) => path.span(),
            Self::TypeRelative(receiver, _) => receiver.span(),
        }
    }

    pub fn resolved(path: Path) -> Self {
        Self::Resolved(None, path)
    }

    pub fn qualified(receiver: TypeExpr, path: Path) -> Self {
        Self::Resolved(Some(Box::new(receiver)), path)
    }

    pub fn type_relative(receiver: TypeExpr, segment: PathSegment) -> Self {
        Self::TypeRelative(Box::new(receiver), segment)
    }

    pub fn path(&self) -> Option<&Path> {
        match self {
            Self::Resolved(_, path) => Some(path),
            Self::TypeRelative(_, _) => None,
        }
    }

    pub fn into_path(self) -> Option<Path> {
        match self {
            Self::Resolved(_, path) => Some(path),
            Self::TypeRelative(_, _) => None,
        }
    }

    pub fn segments(&self) -> &[PathSegment] {
        match self {
            Self::Resolved(_, path) => &path.segments,
            Self::TypeRelative(_, segment) => std::slice::from_ref(segment),
        }
    }

    pub fn segments_mut(&mut self) -> &mut [PathSegment] {
        match self {
            Self::Resolved(_, path) => &mut path.segments,
            Self::TypeRelative(_, segment) => std::slice::from_mut(segment),
        }
    }

    pub fn res_ref(&self) -> &Res {
        match self {
            Self::Resolved(_, path) => &path.res,
            Self::TypeRelative(_, segment) => &segment.res,
        }
    }

    pub fn res(&self) -> Res {
        match self {
            Self::Resolved(_, path) => path.res.clone(),
            Self::TypeRelative(_, segment) => segment.res.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    /// Source spelling of this ordinary path component. In
    /// `QPath::Resolved`, every component remains in `Path::segments`;
    /// `QPath::TypeRelative` owns exactly one deferred associated component.
    pub ident: Symbol,
    /// HIR identity for this path segment, matching rustc HIR.
    pub hir_id: HirId,
    /// Resolution of this component. A type-relative associated component
    /// uses `Res::Error` here and is resolved by type checking, matching
    /// rustc HIR.
    pub res: Res,
    pub args: Option<GenericArgs>,
    /// Whether generic arguments were omitted and should be inferred. This
    /// mirrors rustc HIR's `PathSegment::infer_args`; an explicit `::<_>` is
    /// represented by `args = Some(...)` with an `Infer` generic argument.
    pub infer_args: bool,
    /// Whether this segment is the child of a delegation path. FerroPhase
    /// does not currently expose delegation syntax, but retaining rustc's
    /// bit keeps the HIR path-segment shape lossless for future lowering.
    pub delegation_child_segment: bool,
}

impl PathSegment {
    pub fn new(ident: impl Into<Symbol>, args: Option<GenericArgs>) -> Self {
        let infer_args = args.is_none();
        Self {
            ident: ident.into(),
            hir_id: HirId::default(),
            args,
            infer_args,
            res: Res::Error,
            delegation_child_segment: false,
        }
    }

    pub fn with_hir_id(
        ident: impl Into<Symbol>,
        hir_id: HirId,
        args: Option<GenericArgs>,
        res: Res,
        infer_args: bool,
    ) -> Self {
        Self {
            ident: ident.into(),
            hir_id,
            args,
            infer_args,
            res,
            delegation_child_segment: false,
        }
    }

    /// Return this segment's arguments, using rustc's empty-list view when
    /// the source omitted an argument list.
    pub fn args(&self) -> &GenericArgs {
        self.args.as_ref().unwrap_or(GenericArgs::NONE)
    }
}

/// A lifetime argument in HIR.
///
/// Rustc keeps a lifetime as a first-class HIR node rather than reducing it
/// to its spelling.  The type checker currently erases regions, but retaining
/// the identity and source metadata here keeps path generic arguments
/// lossless for diagnostics and later lowering stages.
#[derive(Debug, Clone, PartialEq)]
pub struct Lifetime {
    pub hir_id: HirId,
    pub ident: Symbol,
    pub kind: LifetimeKind,
    pub source: LifetimeSource,
    pub syntax: LifetimeSyntax,
    pub span: Span,
}

impl Lifetime {
    pub fn new(
        hir_id: HirId,
        ident: impl Into<Symbol>,
        kind: LifetimeKind,
        source: LifetimeSource,
        syntax: LifetimeSyntax,
        span: Span,
    ) -> Self {
        Self {
            hir_id,
            ident: ident.into(),
            kind,
            source,
            syntax,
            span,
        }
    }

    /// Construct a lifetime from a parsed spelling when no dedicated lifetime
    /// declaration is available. Named lifetimes carry their HIR identity as
    /// the parameter identity; unlike rustc, this compiler does not allocate a
    /// separate `LocalDefId` for erased regions.
    pub fn from_name(name: impl Into<Symbol>, hir_id: HirId, span: Span) -> Self {
        let ident = name.into();
        let (kind, syntax) = match ident.as_str() {
            "'static" => (LifetimeKind::Static, LifetimeSyntax::ExplicitBound),
            "'_" => (LifetimeKind::Infer, LifetimeSyntax::ExplicitAnonymous),
            _ => (LifetimeKind::Param(hir_id.clone()), LifetimeSyntax::ExplicitBound),
        };
        Self::new(
            hir_id,
            ident,
            kind,
            LifetimeSource::Path,
            syntax,
            span,
        )
    }

    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl From<&str> for Lifetime {
    fn from(name: &str) -> Self {
        Self::from_name(name, HirId::default(), Span::null())
    }
}

impl From<String> for Lifetime {
    fn from(name: String) -> Self {
        Self::from_name(name, HirId::default(), Span::null())
    }
}

impl fmt::Display for Lifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ident.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LifetimeKind {
    Param(HirId),
    ImplicitObjectLifetimeDefault,
    Error,
    Infer,
    Static,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifetimeSource {
    Reference,
    Path,
    OutlivesBound,
    PreciseCapturing,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifetimeSyntax {
    Implicit,
    ExplicitAnonymous,
    ExplicitBound,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
    /// Associated-item constraints attached to this segment, matching
    /// rustc HIR's `GenericArgs::constraints`.
    pub constraints: Vec<AssocItemConstraint>,
    /// Whether these arguments originated from parenthesized trait syntax.
    /// Rustc stores the input tuple as the first generic argument and the
    /// return type as an `Output` associated constraint; this marker preserves
    /// the syntax distinction without duplicating those semantic values.
    pub parenthesized: GenericArgsParentheses,
    /// Span covering the complete argument list, including delimiters when
    /// source information is available. Generated HIR uses a null span.
    pub span_ext: Span,
}

impl GenericArgs {
    /// The empty argument list used by rustc for a path segment without
    /// explicit arguments. `PathSegment::args` exposes this view while the
    /// optional field still preserves whether arguments were written.
    pub const NONE: &'static Self = &Self {
        args: Vec::new(),
        constraints: Vec::new(),
        parenthesized: GenericArgsParentheses::No,
        span_ext: Span {
            file: 0,
            lo: 0,
            hi: 0,
        },
    };

    /// Return the parenthesized trait inputs and synthesized `Output` type.
    ///
    /// Rustc lowers `Trait(A, B) -> C` into one tuple type argument and one
    /// `Output = C` associated-item constraint. Keeping this accessor beside
    /// the representation makes consumers rely on that invariant instead of
    /// independently interpreting the two lists.
    pub fn paren_sugar_inputs_output(&self) -> Option<(&[Box<TypeExpr>], &TypeExpr)> {
        if self.parenthesized != GenericArgsParentheses::ParenSugar {
            return None;
        }
        let Some(inputs) = self.args.iter().find_map(|arg| {
            let GenericArg::Type(input) = arg else {
                return None;
            };
            let TypeExprKind::Tuple(inputs) = &input.kind else {
                return None;
            };
            Some(inputs.as_slice())
        }) else {
            return None;
        };
        let [AssocItemConstraint {
            ident,
            kind: AssocItemConstraintKind::Equality {
                term: Term::Ty(output),
            },
            ..
        }] = self.constraints.as_slice()
        else {
            return None;
        };
        (ident.as_str() == "Output").then_some((inputs, output.as_ref()))
    }

    /// Return the synthesized output type for parenthesized trait syntax.
    pub fn paren_sugar_output(&self) -> Option<&TypeExpr> {
        self.paren_sugar_inputs_output().map(|(_, output)| output)
    }

    /// Match rustc HIR's `GenericArgs::is_empty`: only positional arguments
    /// determine emptiness. Associated-item constraints are kept in a
    /// separate list and therefore do not make this view non-empty.
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Return the number of explicit lifetime arguments.
    #[inline]
    pub fn num_lifetime_args(&self) -> usize {
        self.args
            .iter()
            .filter(|arg| matches!(arg, GenericArg::Lifetime(_)))
            .count()
    }

    /// Whether at least one explicit lifetime argument is present.
    #[inline]
    pub fn has_lifetime_args(&self) -> bool {
        self.args
            .iter()
            .any(|arg| matches!(arg, GenericArg::Lifetime(_)))
    }

    /// Return the number of explicit type and const arguments.
    ///
    /// This mirrors rustc's diagnostic-oriented `num_generic_params` view;
    /// inference arguments count as type-or-const arguments.
    #[inline]
    pub fn num_generic_params(&self) -> usize {
        self.args
            .iter()
            .filter(|arg| !matches!(arg, GenericArg::Lifetime(_)))
            .count()
    }

    /// Return the source span for non-empty generic argument syntax, matching
    /// rustc's optional `span_ext()` accessor. A zero-width span represents a
    /// synthesized or absent argument list.
    pub fn span_ext(&self) -> Option<Span> {
        (!self.span_ext.is_null() && self.span_ext.lo != self.span_ext.hi)
            .then_some(self.span_ext)
    }
}

impl Default for GenericArgs {
    fn default() -> Self {
        Self {
            args: Vec::new(),
            constraints: Vec::new(),
            parenthesized: GenericArgsParentheses::No,
            span_ext: Span::null(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericArgsParentheses {
    No,
    ReturnTypeNotation,
    ParenSugar,
}

/// The kind of inference represented by a generic `_` argument.
///
/// Rustc keeps this distinction on the inference argument itself because a
/// syntactic wildcard can remain ambiguous between a type and a const until
/// generic argument lowering, while `{ _ }` is unambiguously a const.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferArgKind {
    TypeOrConst,
    Const,
}

/// Metadata carried by an inferred generic argument, matching rustc HIR's
/// `InferArg` rather than collapsing `_` to an unlocated unit variant.
#[derive(Debug, Clone, PartialEq)]
pub struct InferArg {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: InferArgKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
    Lifetime(Lifetime),
    Type(Box<TypeExpr>),
    Const(Box<ConstArg>),
    /// An inferred generic argument (`_`), matching rustc HIR's dedicated
    /// `GenericArg::Infer` variant rather than encoding it as a type node.
    Infer(InferArg),
}

/// A constant argument entering the type system.
///
/// Rustc keeps const arguments separate from ordinary expressions because a
/// bare path (`N`) and an arbitrary expression (`{ N + 1 }`) participate in
/// generic argument lowering differently.  The owned HIR representation uses
/// the same distinction while retaining the existing expression tree for
/// anonymous constants.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstArg {
    pub hir_id: HirId,
    pub kind: ConstArgKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstArgKind {
    /// A path-shaped const argument, normally a const parameter or item.
    Path(QPath),
    /// An arbitrary const expression (including blocks and operators).
    Anon(Box<Expr>),
    /// A literal const argument.
    Literal { lit: Lit, negated: bool },
    /// A const argument that could not be lowered.
    Error(ty::ErrorGuaranteed),
    /// An unambiguous const inference argument (`{ _ }`).
    Infer(InferArg),
}

impl ConstArg {
    pub fn from_expr(expr: Expr) -> Self {
        let hir_id = expr.hir_id.clone();
        let span = expr.span;
        let kind = match expr.kind {
            ExprKind::Path(path) => ConstArgKind::Path(path),
            ExprKind::Literal(lit) => ConstArgKind::Literal {
                lit,
                negated: false,
            },
            other => ConstArgKind::Anon(Box::new(Expr {
                hir_id: hir_id.clone(),
                kind: other,
                span,
            })),
        };
        Self { hir_id, kind, span }
    }
}

/// A constraint on an associated item of a path segment.
///
/// The constrained item can itself be generic (`Item<'a> = T`). Rustc keeps
/// those arguments in `AssocItemConstraint::gen_args`, separate from the
/// equality/bound payload, so HIR does the same.
#[derive(Debug, Clone, PartialEq)]
pub struct AssocItemConstraint {
    /// HIR identity of the constraint itself, matching rustc HIR rather than
    /// treating a constraint as metadata owned solely by its path segment.
    pub hir_id: HirId,
    pub ident: Symbol,
    /// HIR always has generic arguments for a constraint. An AST constraint
    /// without an explicit argument list lowers to an empty `GenericArgs`.
    pub gen_args: GenericArgs,
    pub kind: AssocItemConstraintKind,
    pub span: Span,
}

/// The right-hand side of an associated-item equality constraint.
///
/// Rustc's HIR preserves whether an equality binds an associated type or an
/// associated const. Keeping that distinction avoids turning const bindings
/// into invalid type expressions during later lowering stages.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Ty(Box<TypeExpr>),
    Const(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssocItemConstraintKind {
    Equality { term: Term },
    /// A bound constraint such as `Item: Trait`, matching rustc's
    /// `AssocItemConstraintKind::Bound` terminology.
    Bound { bounds: Vec<TypeExpr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: Option<WhereClause>,
    /// Source span covering the complete generic parameter/where-clause
    /// list, matching rustc HIR's explicit `Generics::span` metadata.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub hir_id: HirId,
    pub def_id: DefId,
    pub name: Symbol,
    /// Source span covering the complete generic parameter declaration.
    /// Rustc stores this independently of the parameter kind so spans remain
    /// available for lifetime/type parameters without a default value.
    pub span: Span,
    /// Whether this parameter participates in rustc's `pure_wrt_drop`
    /// analysis. FerroPhase does not perform that analysis yet, but retaining
    /// the bit keeps the HIR declaration shape lossless.
    pub pure_wrt_drop: bool,
    pub kind: GenericParamKind,
    /// Span of the parameter's `:` token when one was written.
    pub colon_span: Option<Span>,
    /// Origin of this parameter, matching rustc's `GenericParamSource`.
    pub source: GenericParamSource,
    /// This parameter's own trait bounds (`T: Iterator<Item = U>`, `F:
    /// FnOnce() -> R`, ...) so `path_ty` can resolve a still-generic
    /// `T::AssocName` projection (`F::Output`, `I::Item`, ...) from the
    /// bound that actually declares it, instead of only ever resolving
    /// `T::AssocName` once `T` is a concrete type. A `Fn`/`FnOnce`/
    /// `FnMut(..) -> R` bound (fp-lang's own parser folds this sugar
    /// straight into a `TypeExprKind::FnPtr`, discarding the trait name —
    /// see `parse_simple_type`'s `name(...)` branch) carries its `Output`
    /// as `FnPtr`'s own `output`; any other bound is a real `Path` (a
    /// named trait, with the bound's own generic args — including an
    /// explicit associated-type binding like `Item = U` — carried on it
    /// exactly as an ordinary trait-bound expression already is).
    pub bounds: Vec<TypeExpr>,
    /// Explicit associated-type bindings carried by one of `bounds`'
    /// own trait-bound generic-arg lists (`I: Iterator<Item = U>` binds
    /// `Item` to `U` directly, as opposed to merely bounding `I` by the
    /// `Iterator` trait with no committed value for `Item`) — kept
    /// separate from `bounds` itself since `TypeExprKind::Path`'s own
    /// `GenericArgs` has no slot for "this arg is actually a `name =
    /// type` binding, not a positional type argument" (see
    /// `GenericParam::bounds`'s own doc comment on why `Ident = Type`
    /// generic args are otherwise dropped entirely). Checked before
    /// falling back to a supertrait-declares-it-but-doesn't-bind-it
    /// opaque placeholder — an explicit binding gives a real, concrete
    /// answer.
    pub explicit_bindings: Vec<(Symbol, TypeExpr)>,
    /// Trait bounds on associated projections rooted at this parameter.
    /// The projection uses the associated type name because unresolved
    /// projections are represented by the same opaque parameter name.
    pub projection_bounds: Vec<(Symbol, Vec<TypeExpr>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericParamSource {
    /// A parameter declared in an item's generic parameter list.
    Generics,
    /// A parameter introduced by a higher-ranked `for<...>` binder.
    Binder,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingLifetimeKind {
    /// An explicitly written `'_` lifetime.
    Underscore,
    /// A lifetime elided after `&`.
    Ampersand,
    /// A lifetime elided in a bracketed generic argument list.
    Comma,
    /// A lifetime elided in a list without written brackets.
    Brackets,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifetimeParamKind {
    /// A named lifetime declared explicitly, such as `'a`.
    Explicit,
    /// An anonymous lifetime synthesized from an elided source lifetime.
    Elided(MissingLifetimeKind),
    /// A lifetime declaration whose source was invalid.
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericParamKind {
    /// A named lifetime parameter. Region checking is not implemented yet,
    /// but retaining the declaration preserves rustc's generic parameter
    /// ordering and keeps lifetime arguments aligned with their source.
    Lifetime { kind: LifetimeParamKind },
    Type {
        default: Option<Box<TypeExpr>>,
        /// Whether this parameter was synthesized while lowering `impl Trait`.
        synthetic: bool,
    },
    Const {
        ty: Box<TypeExpr>,
        default: Option<Box<ConstArg>>,
    },
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
    /// A `b"..."` byte-string literal — typed as `&[u8; N]`.
    Bytes(Vec<u8>),
    /// A `c"..."` C-string literal (implicitly NUL-terminated, no interior
    /// NULs) — typed as `&std::ffi::CStr`.
    CStr(Vec<u8>),
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
    Box,
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
    /// A function parameter binding. Kept distinct from a local because
    /// diagnostics and lowering may need to preserve the declaration class.
    Parameter(HirId),
    /// A generic parameter is represented by its declaration identity, just
    /// like rustc's `Res::Def(DefKind::TyParam/ConstParam, DefId)`.
    Generic(DefId),
    SelfTy,
    /// A module namespace, identified by the module item's definition id.
    /// The path is retained by the module tree and is only used internally
    /// while traversing a qualified path; resolved consumers use this stable
    /// semantic identity rather than reconstructing a source path.
    Module(DefId),
    /// A language-level builtin identified by source name rather than a
    /// nominal definition. This is separate from `BuiltinSelfType`, which is
    /// the HIR impl-shape marker used by method lookup.
    BuiltinName(String),
    /// Resolution failed after lookup/ambiguity diagnostics were recorded.
    Error,
    /// A non-nominal `impl` self-type shape with no `DefId` of its own —
    /// `&T`/`&mut T`, `[T]`, `[T; N]`. Mirrors rustc's `SimplifiedType`
    /// fast-reject bucketing (`rustc_middle::ty::fast_reject`): identifies
    /// only the shallow outer shape, not the referent/element type, so
    /// multiple impls of the same shape share one bucket. `.method()`
    /// call resolution does not use this — it re-derives structural
    /// self-type equality per candidate impl independently. The AST→HIR
    /// lowering stage records semantic impl identity separately; this value
    /// remains the HIR marker for structural dispatch.
    Builtin(BuiltinSelfType),
}

impl Res {
    /// Treat `Res::Error` as the required-field equivalent of an absent
    /// resolution for legacy query code.
    pub fn as_ref(&self) -> Option<&Self> {
        (!matches!(self, Self::Error)).then_some(self)
    }

    pub fn is_some(&self) -> bool {
        !matches!(self, Self::Error)
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::Error)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinSelfType {
    Reference {
        mutable: bool,
    },
    Slice,
    Array,
    RawPtr {
        mutable: bool,
    },
    /// The never type `!` (`std::core::primitive_docs`'s `impl ! {}`).
    Never,
    /// The unit type `()`.
    Unit,
    /// Any tuple type (`(T,)`, `(T, U)`, ...) — arity-blind, same accepted
    /// imprecision as `Reference`/`Slice`/`Array` above.
    Tuple,
    /// Any function-pointer type (`fn(T) -> Ret`) — arity-blind, same
    /// accepted imprecision.
    Function,
    /// A primitive scalar named directly (`u8`, `i32`, `bool`, `str`, ...) —
    /// unlike the other variants here, this one *is* precise (the name
    /// picks an exact primitive, not a shape shared by many types), but it
    /// still has no `DefId` of its own to resolve through `Res::Def` the
    /// way a struct/enum/trait does. Exists so a value-position path like
    /// `u8::MAX`/`u8::from_str_radix(..)` (real std's own inherent
    /// consts/methods on primitives, reached via the same type-relative
    /// path shape as `Map::new`/`T::default`) has a `Res` to resolve
    /// through at all — see `name_to_hir_path_with_scope`'s type-relative
    /// fallback.
    Primitive(String),
}

impl BuiltinSelfType {
    pub fn bucket_key(&self) -> &str {
        match self {
            BuiltinSelfType::Reference { mutable: false } => "&",
            BuiltinSelfType::Reference { mutable: true } => "&mut",
            BuiltinSelfType::Slice => "[]",
            BuiltinSelfType::Array => "[;N]",
            BuiltinSelfType::RawPtr { mutable: false } => "*const",
            BuiltinSelfType::RawPtr { mutable: true } => "*mut",
            BuiltinSelfType::Never => "!",
            BuiltinSelfType::Unit => "()",
            BuiltinSelfType::Tuple => "(,)",
            BuiltinSelfType::Function => "fn(..)",
            BuiltinSelfType::Primitive(name) => name.as_str(),
        }
    }
}

// Temporary types until we have proper implementations
pub type Span = crate::span::Span;

// Default implementations
impl Default for Generics {
    fn default() -> Self {
        Self {
            params: Vec::new(),
            where_clause: None,
            span: Span::null(),
        }
    }
}

/// A generic function/method call whose concrete type arguments have been
/// resolved and are ready for monomorphization.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericCallResolution {
    pub def_id: DefId,
    pub args: Vec<Ty>,
}

impl Function {
    pub fn new(sig: FunctionSig, body: Option<Block>, is_const: bool, is_extern: bool) -> Self {
        Self {
            sig,
            body,
            is_const,
            is_extern,
            is_async: false,
            attrs: Vec::new(),
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
            ItemKind::TypeAlias(alias) => alias.target.span,
            ItemKind::Const(cons) => cons.span(),
            ItemKind::Impl(imp) => imp.span(),
            ItemKind::Trait(tr) => tr.span(),
            ItemKind::Query(query) => query.span(),
            ItemKind::Expr(expr) => expr.span(),
        }
    }
}

impl Function {
    pub fn span(&self) -> Span {
        Span::union(
            self.body
                .as_ref()
                .map(Block::span)
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

impl Query {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl Trait {
    pub fn span(&self) -> Span {
        Span::union(self.items.iter().map(TraitItem::span))
    }
}

impl TraitItem {
    pub fn span(&self) -> Span {
        self.kind.span()
    }
}

impl TraitItemKind {
    pub fn span(&self) -> Span {
        match self {
            TraitItemKind::Method(func) => func.span(),
            TraitItemKind::AssocConst(konst) => Span::union(
                [konst.ty.span()]
                    .into_iter()
                    .chain(konst.body.as_ref().map(Body::span)),
            ),
            TraitItemKind::AssocType(_) => Span::default(),
        }
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
            ImplItemKind::AssocType(assoc) => assoc.ty.span(),
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
            ExprKind::Query(query) => query.span,
            ExprKind::Binary(_, lhs, rhs) => Span::union([lhs.span(), rhs.span()]),
            ExprKind::Unary(_, expr) => expr.span(),
            ExprKind::Reference(reference) => reference.expr.span(),
            ExprKind::Call(func, args) => Span::union(
                Some(func.span())
                    .into_iter()
                    .chain(args.iter().map(CallArg::span)),
            ),
            ExprKind::MethodCall(receiver, _, _, args) => Span::union(
                Some(receiver.span())
                    .into_iter()
                    .chain(args.iter().map(CallArg::span)),
            ),
            ExprKind::FieldAccess(expr, _) => expr.span(),
            ExprKind::Index(expr, index) => Span::union([expr.span(), index.span()]),
            ExprKind::Slice(slice) => Span::union(
                [
                    Some(slice.base.span()),
                    slice.start.as_ref().map(|expr| expr.span()),
                    slice.end.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
            ExprKind::Cast(expr, ty) => Span::union([expr.span(), ty.span()]),
            ExprKind::Struct(path, fields) => Span::union(
                Some(path.qself_span())
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
            ExprKind::Try(expr_try) => expr_try.span(),
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
            ExprKind::For(_pat, iter, body) => Span::union([iter.span(), body.span()]),
            ExprKind::With(context, body) => Span::union([context.span(), body.span()]),
            ExprKind::Array(exprs) => Span::union(exprs.iter().map(Expr::span)),
            ExprKind::ArrayRepeat { elem, len } => Span::union([elem.span(), len.span()]),
            ExprKind::Tuple(exprs) => Span::union(exprs.iter().map(Expr::span)),
            ExprKind::ConstBlock(const_block) => const_block.body.span(),
            ExprKind::Closure(closure) => Span::union(
                closure
                    .params
                    .iter()
                    .map(Param::span)
                    .chain([closure.body.span()]),
            ),
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

impl TryExpr {
    pub fn span(&self) -> Span {
        Span::union(
            Some(self.expr.span())
                .into_iter()
                .chain(self.catches.iter().map(TryCatch::span))
                .chain(self.elze.as_ref().map(|expr| expr.span()))
                .chain(self.finally.as_ref().map(|expr| expr.span())),
        )
    }
}

impl TryCatch {
    pub fn span(&self) -> Span {
        Span::union(
            self.pat
                .as_ref()
                .map(Pat::span)
                .into_iter()
                .chain([self.body.span()]),
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
                Some(path.qself_span())
                    .into_iter()
                    .chain(fields.iter().map(PatField::span)),
            ),
            PatKind::TupleStruct(path, pats) => Span::union(
                Some(path.span())
                    .into_iter()
                    .chain(pats.iter().map(Pat::span)),
            ),
            PatKind::Variant(path) => path.qself_span(),
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
            TypeExprKind::Ptr { inner: ty, .. } => ty.span(),
            TypeExprKind::Ref(ty) => ty.span(),
            TypeExprKind::FnPtr(func) => func.span(),
            TypeExprKind::Dynamic(bounds) => Span::union(bounds.iter().map(Path::span)),
            TypeExprKind::ConstBlock(_, body) => body.span(),
            TypeExprKind::Never
            | TypeExprKind::Infer
            | TypeExprKind::Error
            | TypeExprKind::Type
            | TypeExprKind::Any => Span::null(),
            TypeExprKind::Refinement {
                base, predicate, ..
            } => Span::union([base.span(), predicate.span()]),
            TypeExprKind::LiteralString(_) => Span::null(),
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

impl<R> Path<R> {
    pub fn span(&self) -> Span {
        self.span
            .or(Span::union(self.segments.iter().map(PathSegment::span)))
    }
}

impl QPath {
    pub fn span(&self) -> Span {
        match self {
            Self::Resolved(_, path) => path.span(),
            Self::TypeRelative(receiver, segment) => Span::union([receiver.span(), segment.span()]),
        }
    }
}

impl PathSegment {
    pub fn span(&self) -> Span {
        self.args
            .as_ref()
            .and_then(GenericArgs::span_ext)
            .unwrap_or_else(Span::null)
    }
}

impl GenericArgs {
    /// Return the span inside the surrounding delimiters, matching rustc
    /// HIR's `GenericArgs::span`. The complete source span remains available
    /// through `span_ext`.
    pub fn span(&self) -> Option<Span> {
        let span = self.span_ext()?;
        Some(Span::new(
            span.file,
            span.lo.saturating_add(1),
            span.hi.saturating_sub(1),
        ))
    }
}

impl GenericArg {
    pub fn span(&self) -> Span {
        match self {
            GenericArg::Lifetime(lifetime) => lifetime.span(),
            GenericArg::Type(ty) => ty.span(),
            GenericArg::Const(const_arg) => const_arg.span,
            GenericArg::Infer(infer) => infer.span,
        }
    }

    /// Return the HIR identity carried by this generic argument, matching
    /// rustc's uniform `GenericArg::hir_id` accessor.
    pub fn hir_id(&self) -> HirId {
        match self {
            GenericArg::Lifetime(lifetime) => lifetime.hir_id.clone(),
            GenericArg::Type(ty) => ty.hir_id.clone(),
            GenericArg::Const(const_arg) => const_arg.hir_id.clone(),
            GenericArg::Infer(infer) => infer.hir_id.clone(),
        }
    }

    /// Human-readable category used by rustc diagnostics.
    pub fn descr(&self) -> &'static str {
        match self {
            Self::Lifetime(_) => "lifetime",
            Self::Type(_) => "type",
            Self::Const(_) => "constant",
            Self::Infer(InferArg {
                kind: InferArgKind::TypeOrConst,
                ..
            }) => "placeholder",
            Self::Infer(InferArg {
                kind: InferArgKind::Const,
                ..
            }) => "constant",
        }
    }

    /// Whether this argument may be either a type or a const argument.
    #[inline]
    pub fn is_ty_or_const(&self) -> bool {
        !matches!(self, Self::Lifetime(_))
    }

    /// Return the ordering class used by rustc's generic-parameter checks.
    pub fn to_ord(&self) -> crate::ast::ParamKindOrd {
        match self {
            Self::Lifetime(_) => crate::ast::ParamKindOrd::Lifetime,
            Self::Type(_) | Self::Const(_) | Self::Infer(_) => {
                crate::ast::ParamKindOrd::TypeOrConst
            }
        }
    }
}

impl AssocItemConstraint {
    pub fn span(&self) -> Span {
        let payload = match &self.kind {
            AssocItemConstraintKind::Equality { term } => match term {
                Term::Ty(ty) => ty.span(),
                Term::Const(expr) => expr.span(),
            },
            AssocItemConstraintKind::Bound { bounds } => {
                Span::union(bounds.iter().map(TypeExpr::span))
            }
        };
        self.span
            .or(Span::union([
                self.gen_args.span_ext().unwrap_or_else(Span::null),
                payload,
            ]))
    }

    /// Obtain the right-hand side of an associated type equality constraint.
    pub fn ty(&self) -> Option<&TypeExpr> {
        match &self.kind {
            AssocItemConstraintKind::Equality {
                term: Term::Ty(ty),
            } => Some(ty),
            _ => None,
        }
    }

    /// Obtain the right-hand side of an associated constant equality
    /// constraint.
    pub fn ct(&self) -> Option<&Expr> {
        match &self.kind {
            AssocItemConstraintKind::Equality {
                term: Term::Const(expr),
            } => Some(expr),
            _ => None,
        }
    }
}

impl Generics {
    pub fn span(&self) -> Span {
        self.span.or(Span::union(
            self.params
                .iter()
                .map(GenericParam::span)
                .chain(self.where_clause.as_ref().map(WhereClause::span)),
        ))
    }
}

impl GenericParam {
    pub fn is_impl_trait(&self) -> bool {
        matches!(
            self.kind,
            GenericParamKind::Type {
                synthetic: true,
                ..
            }
        )
    }

    pub fn is_elided_lifetime(&self) -> bool {
        matches!(
            self.kind,
            GenericParamKind::Lifetime {
                kind: LifetimeParamKind::Elided(_)
            }
        )
    }

    pub fn is_lifetime(&self) -> bool {
        matches!(self.kind, GenericParamKind::Lifetime { .. })
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl GenericParamKind {
    pub fn span(&self) -> Span {
        match self {
            GenericParamKind::Lifetime { .. } => Span::null(),
            GenericParamKind::Type { default, .. } => default
                .as_ref()
                .map(|ty| ty.span())
                .unwrap_or_else(Span::null),
            GenericParamKind::Const { ty, default } => Span::union(
                [Some(ty.span()), default.as_ref().map(|const_arg| const_arg.span)]
                    .into_iter()
                    .flatten(),
            ),
        }
    }
}

#[cfg(test)]
mod path_tests {
    use super::{
        AssocItemConstraint, AssocItemConstraintKind, GenericArg, GenericArgs,
        GenericArgsParentheses, HirId, InferArg, InferArgKind, Lifetime, LifetimeKind, OwnerId,
        PackageId, Path, PathSegment, QPath, Term, TypeExpr, TypeExprKind,
    };
    use crate::span::Span;

    #[test]
    fn omitted_segment_arguments_use_rustc_empty_view() {
        let segment = PathSegment::new("Item", None);

        assert!(segment.args.is_none());
        assert_eq!(segment.args(), GenericArgs::NONE);
        assert!(segment.args().args.is_empty());
        assert!(segment.infer_args);
    }

    #[test]
    fn path_resolution_payload_is_generic_like_rustc() {
        let path: Path<Option<super::Res>> = Path::new(None, vec![]);

        assert_eq!(path.res_ref(), &None);
        assert_eq!(path.res(), None);
    }

    #[test]
    fn const_args_preserve_rustc_node_kinds() {
        let path_expr = super::Expr::new(
            HirId::default(),
            super::ExprKind::Path(QPath::resolved(Path::new(
                super::Res::Error,
                vec![PathSegment::new("N", None)],
            ))),
            Span::new(0, 1, 2),
        );
        let path_arg = super::ConstArg::from_expr(path_expr);
        assert!(matches!(path_arg.kind, super::ConstArgKind::Path(_)));
        assert_eq!(path_arg.span, Span::new(0, 1, 2));

        let literal_arg = super::ConstArg::from_expr(super::Expr::new(
            HirId::default(),
            super::ExprKind::Literal(super::Lit::Integer(3)),
            Span::new(0, 3, 4),
        ));
        assert!(matches!(
            literal_arg.kind,
            super::ConstArgKind::Literal {
                lit: super::Lit::Integer(3),
                negated: false
            }
        ));
    }

    #[test]
    fn generic_parameter_spans_are_stored_explicitly() {
        let span = Span::new(0, 10, 20);
        let parameter = super::GenericParam {
            hir_id: HirId::default(),
            def_id: super::DefId::local(1),
            name: "T".into(),
            span,
            pure_wrt_drop: false,
            kind: super::GenericParamKind::Type {
                default: None,
                synthetic: false,
            },
            colon_span: None,
            source: super::GenericParamSource::Generics,
            bounds: Vec::new(),
            explicit_bindings: Vec::new(),
            projection_bounds: Vec::new(),
        };
        assert_eq!(parameter.span(), span);
        assert_eq!(
            super::Generics {
                params: vec![parameter],
                where_clause: None,
                span,
            }
            .span(),
            span
        );
    }

    #[test]
    fn parenthesized_arguments_expose_rustc_shape() {
        let input = TypeExpr::new(
            Default::default(),
            TypeExprKind::Tuple(vec![Box::new(TypeExpr::new(
                Default::default(),
                TypeExprKind::Never,
                Default::default(),
            ))]),
            Default::default(),
        );
        let output = TypeExpr::new(
            Default::default(),
            TypeExprKind::Never,
            Default::default(),
        );
        let args = GenericArgs {
            args: vec![
                GenericArg::Lifetime("'a".into()),
                GenericArg::Type(Box::new(input)),
            ],
            constraints: vec![AssocItemConstraint {
                hir_id: Default::default(),
                ident: "Output".into(),
                gen_args: GenericArgs::default(),
                kind: AssocItemConstraintKind::Equality {
                    term: Term::Ty(Box::new(output)),
                },
                span: Default::default(),
            }],
            parenthesized: GenericArgsParentheses::ParenSugar,
            span_ext: Default::default(),
        };

        let (inputs, output) = args
            .paren_sugar_inputs_output()
            .expect("valid parenthesized generic arguments");
        assert_eq!(inputs.len(), 1);
        assert!(matches!(output.kind, TypeExprKind::Never));
        assert!(args.paren_sugar_output().is_some());
    }

    #[test]
    fn generic_args_match_rustc_empty_and_span_views() {
        let constraint_only = GenericArgs {
            args: Vec::new(),
            constraints: vec![AssocItemConstraint {
                hir_id: Default::default(),
                ident: "Output".into(),
                gen_args: GenericArgs::default(),
                kind: AssocItemConstraintKind::Bound { bounds: Vec::new() },
                span: Span::null(),
            }],
            parenthesized: GenericArgsParentheses::No,
            span_ext: Span::new(1, 4, 7),
        };
        assert!(constraint_only.is_empty());
        assert_eq!(constraint_only.span_ext(), Some(Span::new(1, 4, 7)));
        assert_eq!(constraint_only.span(), Some(Span::new(1, 5, 6)));

        let synthesized = GenericArgs {
            span_ext: Span::new(1, 7, 7),
            ..GenericArgs::default()
        };
        assert!(synthesized.is_empty());
        assert_eq!(synthesized.span_ext(), None);
        assert_eq!(synthesized.span(), None);
    }

    #[test]
    fn generic_arg_views_match_rustc_categories() {
        let lifetime = GenericArg::Lifetime(Lifetime::from_name(
            "'a",
            HirId::default(),
            Span::null(),
        ));
        let ty = GenericArg::Type(Box::new(TypeExpr::new(
            HirId::default(),
            TypeExprKind::Never,
            Span::null(),
        )));
        let placeholder = GenericArg::Infer(InferArg {
            hir_id: HirId::default(),
            span: Span::null(),
            kind: InferArgKind::TypeOrConst,
        });
        let constant_placeholder = GenericArg::Infer(InferArg {
            hir_id: HirId::default(),
            span: Span::null(),
            kind: InferArgKind::Const,
        });

        assert_eq!(lifetime.descr(), "lifetime");
        assert!(!lifetime.is_ty_or_const());
        assert_eq!(lifetime.to_ord(), crate::ast::ParamKindOrd::Lifetime);
        assert_eq!(ty.descr(), "type");
        assert!(ty.is_ty_or_const());
        assert_eq!(ty.to_ord(), crate::ast::ParamKindOrd::TypeOrConst);
        assert_eq!(placeholder.descr(), "placeholder");
        assert!(placeholder.is_ty_or_const());
        assert_eq!(
            placeholder.to_ord(),
            crate::ast::ParamKindOrd::TypeOrConst
        );
        assert_eq!(constant_placeholder.descr(), "constant");
        assert!(constant_placeholder.is_ty_or_const());

        let args = GenericArgs {
            args: vec![lifetime, ty, placeholder],
            ..GenericArgs::default()
        };
        assert_eq!(args.num_lifetime_args(), 1);
        assert!(args.has_lifetime_args());
        assert_eq!(args.num_generic_params(), 2);
    }

    #[test]
    fn lifetime_arguments_keep_hir_identity_and_kind() {
        let span = Span::new(0, 11, 13);
        let lifetime = Lifetime::from_name(
            "'a",
            HirId::new(OwnerId::root(PackageId::new("p")), 7),
            span,
        );
        let arg = GenericArg::Lifetime(lifetime.clone());

        assert_eq!(lifetime.as_str(), "'a");
        assert!(matches!(&lifetime.kind, LifetimeKind::Param(_)));
        assert_eq!(arg.hir_id(), lifetime.hir_id.clone());
        assert_eq!(arg.span(), span);
    }

    #[test]
    fn associated_constraints_expose_typed_rhs() {
        let ty = TypeExpr::new(
            Default::default(),
            TypeExprKind::Never,
            Default::default(),
        );
        let type_constraint = AssocItemConstraint {
            hir_id: Default::default(),
            ident: "Item".into(),
            gen_args: GenericArgs::default(),
            kind: AssocItemConstraintKind::Equality {
                term: Term::Ty(Box::new(ty)),
            },
            span: Default::default(),
        };
        assert!(type_constraint.ty().is_some());
        assert!(type_constraint.ct().is_none());
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
