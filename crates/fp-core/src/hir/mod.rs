use crate::ast::{TypeBinaryOpKind, TypePrimitive};
use crate::intrinsics::{CallKind, IntrinsicKind};
use crate::query::{QueryIrDocument, QueryOrigin};
use std::collections::{HashMap, HashSet};
use std::fmt;

pub mod ident;
pub mod package;
pub mod path;
pub mod place;
pub mod pretty;
pub mod program;
pub mod refinement;
pub mod resolve;
pub mod ty;

pub use ident::{DefPath, Symbol};
pub use package::HirPackage;
pub use path::HirPath;
pub use program::HirProgram;
pub use refinement::{ParamSlot, RefinementHint};
pub use resolve::{ModuleId, ModuleTree, Namespace, SymbolEntry, SymbolExport};
pub use ty::{Abi, Ty};

pub type NodeId = u32;

/// HIR's own name for a runtime/const value — the same representation
/// `ast::Value` already is (comptime results don't need a distinct
/// HIR-shaped value type), aliased here so HIR-owned data
/// (`HirPackage`'s typed-results fields, ...) names it as `hir::Value`, not
/// a lower layer's type reaching up into this one.
pub type Value = crate::ast::Value;

/// A package's HIR-level identity — the package's own name, reused
/// directly (not a separately-assigned sequential index). A numeric
/// newtype here previously let a forgotten/wrong assignment silently
/// default to a "plausible-looking" `PackageId(0)` that could collide
/// with another package's real id in a `HashMap` with no diagnostic at
/// all (see the `HirPackage.id`/`HirProgram::add_package` bug this
/// replaced); a wrong or missing string id is immediately, visibly wrong
/// instead. Not `Copy` (a `String` isn't) — `DefId`/`HirId`, which embed
/// this, aren't `Copy` either as a result; both are still cheap `Clone`s,
/// and every real usage is `HashMap`-keyed rather than densely
/// array-indexed, so this isn't a hot-path concern.
#[derive(
    Debug,
    Clone,
    Default,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct PackageId(pub String);

impl PackageId {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

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
        format!("__fp_const_{}_{}", self.package_id.0, self.index)
    }
}

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.package_id.0, self.index)
    }
}

// Remove the old type alias
// pub type Symbol = String;

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
    Trait(Trait),
    Query(Query),
    Expr(Expr),
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
    pub name: Symbol,
    pub variants: Vec<EnumVariant>,
    pub generics: Generics,
    pub repr: crate::ast::ReprOptions,
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
    /// A bare `type Item;` declaration — no bound type (that binding is
    /// always on the impl side, `ImplItemKind::AssocType`); this only
    /// records that the name exists so a trait method's signature can
    /// reference `Self::Item` and have somewhere to resolve it from.
    AssocType(TraitAssocType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitAssocType {
    pub name: Symbol,
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
    Path(Path),
    Query(Query),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Reference(ExprReference),
    Call(Box<Expr>, Vec<CallArg>),
    MethodCall(Box<Expr>, Symbol, Vec<CallArg>),
    FieldAccess(Box<Expr>, Symbol),
    Index(Box<Expr>, Box<Expr>),
    Slice(SliceExpr),
    Cast(Box<Expr>, Box<TypeExpr>),
    Struct(Path, Vec<StructExprField>),
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
    pub def_id: DefId,
    pub name: Symbol,
    pub kind: GenericParamKind,
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
    SelfTy,
    Module(Vec<String>),
    /// A non-nominal `impl` self-type shape with no `DefId` of its own —
    /// `&T`/`&mut T`, `[T]`, `[T; N]`. Mirrors rustc's `SimplifiedType`
    /// fast-reject bucketing (`rustc_middle::ty::fast_reject`): identifies
    /// only the shallow outer shape, not the referent/element type, so
    /// multiple impls of the same shape share one bucket. `.method()`
    /// call resolution does not use this — it re-derives structural
    /// self-type equality per candidate impl independently. This exists
    /// only so `canonical_type_path` can produce a key for the impl
    /// during HIR lowering, consumed by UFCS-style explicit-path lookups.
    Builtin(BuiltinSelfType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
            ExprKind::MethodCall(receiver, _, args) => Span::union(
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
