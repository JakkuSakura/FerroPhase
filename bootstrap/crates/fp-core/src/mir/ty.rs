// use std::collections::HashMap; // Temporarily disabled - unused
use std::fmt;

pub type DefId = u32;
pub type LocalDefId = u32;
pub type InternedTy = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// The primitive boolean type. Written as `bool`.
    Bool,

    /// The primitive character type; holds a Unicode scalar value.
    Char,

    /// A primitive signed integer type. For example, `i32`.
    Int(IntTy),

    /// A primitive unsigned integer type. For example, `u32`.
    Uint(UintTy),

    /// A primitive floating-point type. For example, `f64`.
    Float(FloatTy),

    /// Algebraic data types (ADT). For example: structures, enumerations and unions.
    Adt(AdtDef, SubstsRef),

    /// An array with the given length. `[T; n]`.
    Array(Box<Ty>, ConstKind),

    /// The pointee of an array slice. Written as `[T]`.
    Slice(Box<Ty>),

    /// A raw pointer. Written as `*const T` or `*mut T`
    RawPtr(TypeAndMut),

    /// A reference; a pointer with an associated lifetime. Written as
    /// `&'a mut T` or `&'a T`.
    Ref(Region, Box<Ty>, Mutability),

    /// The anonymous type of a function declaration/definition. Each
    /// function has a unique type.
    FnDef(DefId, SubstsRef),

    /// A pointer to a function. Written as `fn() -> i32`.
    FnPtr(PolyFnSig),

    /// A trait object. Written as `dyn for<'b> Trait<'b, Assoc = u32> + Send + 'a`.
    Dynamic(Vec<ExistentialPredicate>, Region),

    /// The anonymous type of a closure. Used to represent the type of `|a| a`.
    Closure(DefId, SubstsRef),

    /// The anonymous type of a generator. Used to represent the type of
    /// `|a| yield a`.
    Generator(DefId, SubstsRef, Movability),

    /// A type representing the types stored inside a generator.
    GeneratorWitness(Vec<Box<Ty>>),

    /// The never type `!`.
    Never,

    /// A tuple type. For example, `(i32, bool)`.
    Tuple(Vec<Box<Ty>>),

    /// The projection of an associated type. For example,
    /// `<T as Trait<..>>::N`.
    Projection(ProjectionTy),

    /// Opaque (`impl Trait`) type found in a return type.
    Opaque(DefId, SubstsRef),

    /// A type parameter; for example, `T` in `fn f<T>(x: T) {}`.
    Param(ParamTy),

    /// Bound type variable, used only when preparing a trait query.
    Bound(DebruijnIndex, BoundTy),

    /// A placeholder type - universally quantified higher-ranked type.
    Placeholder(PlaceholderType),

    /// A type variable used during type checking.
    Infer(InferTy),

    /// A placeholder for a type which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAndMut {
    pub ty: Box<Ty>,
    pub mutbl: Mutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Mut,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyFnSig {
    pub binder: Binder<FnSig>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSig {
    pub inputs: Vec<Box<Ty>>,
    pub output: Box<Ty>,
    pub c_variadic: bool,
    pub unsafety: Unsafety,
    pub abi: Abi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Unsafety {
    Unsafe,
    Normal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Abi {
    Rust,
    C { unwind: bool },
    Cdecl,
    Stdcall,
    Fastcall,
    Vectorcall,
    Thiscall,
    Aapcs,
    Win64,
    SysV64,
    PtxKernel,
    Msp430Interrupt,
    X86Interrupt,
    AmdGpuKernel,
    EfiApi,
    AvrInterrupt,
    AvrNonBlockingInterrupt,
    CCmseNonSecureCall,
    Wasm,
    System { unwind: bool },
    RustIntrinsic,
    RustCall,
    PlatformIntrinsic,
    Unadjusted,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamTy {
    pub index: u32,
    pub name: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundTy {
    pub var: BoundVar,
    pub kind: BoundTyKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BoundTyKind {
    Anon,
    Param(Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoundVar {
    pub index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlaceholderType {
    pub universe: UniverseIndex,
    pub name: BoundVar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniverseIndex {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferTy {
    TyVar(TyVid),
    IntVar(IntVid),
    FloatVar(FloatVid),
    FreshTy(u32),
    FreshIntTy(u32),
    FreshFloatTy(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVid {
    pub index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntVid {
    pub index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatVid {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorGuaranteed {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectionTy {
    pub substs: SubstsRef,
    pub item_def_id: DefId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstKind {
    /// A const generic parameter.
    Param(ParamConst),

    /// Infer the value of the const.
    Infer(InferConst),

    /// Bound const variable, used only when preparing a trait query.
    Bound(DebruijnIndex, BoundVar),

    /// A placeholder const - universally quantified higher-ranked const.
    Placeholder(PlaceholderConst),

    /// An unnormalized const item such as an anon const or assoc const or free const item.
    Unevaluated(UnevaluatedConst),

    /// Used to hold computed value.
    Value(ConstValue),

    /// A placeholder for a const which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamConst {
    pub index: u32,
    pub name: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferConst {
    Var(ConstVid),
    Fresh(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstVid {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlaceholderConst {
    pub universe: UniverseIndex,
    pub name: BoundVar,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnevaluatedConst {
    pub def: DefId,
    pub substs: SubstsRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
    Scalar(Scalar),
    ZeroSized,
    Slice {
        data: Vec<u8>,
        start: usize,
        end: usize,
    },
    ByRef {
        alloc: AllocId,
        offset: Size,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Scalar {
    Int(ScalarInt),
    Ptr(Pointer),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScalarInt {
    pub data: u128,
    pub size: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pointer {
    pub alloc_id: AllocId,
    pub offset: Size,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AllocId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Size {
    pub bytes: u64,
}

// Type system infrastructure
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtDef {
    pub did: DefId,
    pub variants: Vec<VariantDef>,
    pub flags: AdtFlags,
    pub repr: ReprOptions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDef {
    pub def_id: DefId,
    pub ctor_def_id: Option<DefId>,
    pub ident: Symbol,
    pub discr: VariantDiscr,
    pub fields: Vec<FieldDef>,
    pub ctor_kind: CtorKind,
    pub is_recovered: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef {
    pub did: DefId,
    pub ident: Symbol,
    pub vis: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CtorKind {
    Fn,
    Const,
    Fictive,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariantDiscr {
    Explicit(DefId),
    Relative(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Crate,
    Restricted {
        path: SimplifiedType,
        id: LocalDefId,
    },
    Inherited,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReprOptions {
    pub int: Option<IntegerType>,
    pub align: Option<Align>,
    pub pack: Option<Align>,
    pub flags: ReprFlags,
    pub field_shuffle_seed: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegerType {
    Pointer(bool),
    Fixed(Integer, bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Integer {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Align {
    pub pow2: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AdtFlags(u32);

impl AdtFlags {
    pub const NO_ADT_FLAGS: AdtFlags = AdtFlags(0);
    pub const IS_ENUM: AdtFlags = AdtFlags(1 << 0);
    pub const IS_UNION: AdtFlags = AdtFlags(1 << 1);
    pub const IS_STRUCT: AdtFlags = AdtFlags(1 << 2);
    pub const HAS_CTOR: AdtFlags = AdtFlags(1 << 3);
    pub const IS_PHANTOM_DATA: AdtFlags = AdtFlags(1 << 4);
    pub const IS_FUNDAMENTAL: AdtFlags = AdtFlags(1 << 5);
    pub const IS_BOX: AdtFlags = AdtFlags(1 << 6);
    pub const IS_MANUALLY_DROP: AdtFlags = AdtFlags(1 << 7);

    pub const fn empty() -> Self {
        AdtFlags(0)
    }

    pub fn contains(self, other: AdtFlags) -> bool {
        (self.0 & other.0) == other.0
    }
}

impl std::ops::BitOr for AdtFlags {
    type Output = AdtFlags;
    fn bitor(self, rhs: AdtFlags) -> Self::Output {
        AdtFlags(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for AdtFlags {
    fn bitor_assign(&mut self, rhs: AdtFlags) {
        self.0 |= rhs.0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReprFlags(u8);

impl ReprFlags {
    pub const IS_C: ReprFlags = ReprFlags(1 << 0);
    pub const IS_SIMD: ReprFlags = ReprFlags(1 << 1);
    pub const IS_TRANSPARENT: ReprFlags = ReprFlags(1 << 2);
    pub const IS_LINEAR: ReprFlags = ReprFlags(1 << 3);
    pub const IS_PACKED: ReprFlags = ReprFlags(1 << 4);

    pub const fn empty() -> Self {
        ReprFlags(0)
    }

    pub fn contains(self, other: ReprFlags) -> bool {
        (self.0 & other.0) == other.0
    }
}

impl std::ops::BitOr for ReprFlags {
    type Output = ReprFlags;
    fn bitor(self, rhs: ReprFlags) -> Self::Output {
        ReprFlags(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for ReprFlags {
    fn bitor_assign(&mut self, rhs: ReprFlags) {
        self.0 |= rhs.0;
    }
}

// Substitutions and generics
pub type SubstsRef = Vec<GenericArg>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Lifetime(Region),
    Type(Ty),
    Const(ConstKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Region {
    ReEarlyBound(EarlyBoundRegion),
    ReLateBound(DebruijnIndex, BoundRegion),
    ReFree(FreeRegion),
    ReStatic,
    ReVar(RegionVid),
    RePlaceholder(PlaceholderRegion),
    ReEmpty(UniverseIndex),
    ReErased,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EarlyBoundRegion {
    pub def_id: DefId,
    pub index: u32,
    pub name: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundRegion {
    pub var: BoundVar,
    pub kind: BoundRegionKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BoundRegionKind {
    BrAnon(u32),
    BrNamed(DefId, Symbol),
    BrEnv,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FreeRegion {
    pub scope: DefId,
    pub bound_region: BoundRegion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegionVid {
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlaceholderRegion {
    pub universe: UniverseIndex,
    pub name: BoundRegion,
}

// Trait system support
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExistentialPredicate {
    Trait(ExistentialTraitRef),
    Projection(ExistentialProjection),
    AutoTrait(DefId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExistentialTraitRef {
    pub def_id: DefId,
    pub substs: SubstsRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExistentialProjection {
    pub item_def_id: DefId,
    pub substs: SubstsRef,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Ty(Ty),
    Const(ConstKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Movability {
    Static,
    Movable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binder<T> {
    pub value: T,
    pub bound_vars: Vec<BoundVariableKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BoundVariableKind {
    Ty(BoundTyKind),
    Region(BoundRegionKind),
    Const,
}

// Helper types
pub use super::Symbol;
pub type SimplifiedType = String; // Simplified for now

// Implementation helpers
impl Ty {
    pub fn bool() -> Self {
        Self { kind: TyKind::Bool }
    }

    pub fn char() -> Self {
        Self { kind: TyKind::Char }
    }

    pub fn int(int_ty: IntTy) -> Self {
        Self {
            kind: TyKind::Int(int_ty),
        }
    }

    pub fn uint(uint_ty: UintTy) -> Self {
        Self {
            kind: TyKind::Uint(uint_ty),
        }
    }

    pub fn float(float_ty: FloatTy) -> Self {
        Self {
            kind: TyKind::Float(float_ty),
        }
    }

    pub fn never() -> Self {
        Self {
            kind: TyKind::Never,
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self.kind,
            TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self.kind,
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
    }

    pub fn is_integral(&self) -> bool {
        matches!(self.kind, TyKind::Int(_) | TyKind::Uint(_))
    }

    pub fn is_floating_point(&self) -> bool {
        matches!(self.kind, TyKind::Float(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self.kind, TyKind::Int(_))
    }

    pub fn is_machine(&self) -> bool {
        matches!(
            self.kind,
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TyKind::Bool => write!(f, "bool"),
            TyKind::Char => write!(f, "char"),
            TyKind::Int(int_ty) => write!(f, "{}", int_ty),
            TyKind::Uint(uint_ty) => write!(f, "{}", uint_ty),
            TyKind::Float(float_ty) => write!(f, "{}", float_ty),
            TyKind::Never => write!(f, "!"),
            TyKind::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            _ => write!(f, "<??>"), // Simplified display for complex types
        }
    }
}

impl fmt::Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntTy::Isize => write!(f, "isize"),
            IntTy::I8 => write!(f, "i8"),
            IntTy::I16 => write!(f, "i16"),
            IntTy::I32 => write!(f, "i32"),
            IntTy::I64 => write!(f, "i64"),
            IntTy::I128 => write!(f, "i128"),
        }
    }
}

impl fmt::Display for UintTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UintTy::Usize => write!(f, "usize"),
            UintTy::U8 => write!(f, "u8"),
            UintTy::U16 => write!(f, "u16"),
            UintTy::U32 => write!(f, "u32"),
            UintTy::U64 => write!(f, "u64"),
            UintTy::U128 => write!(f, "u128"),
        }
    }
}

impl fmt::Display for FloatTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatTy::F32 => write!(f, "f32"),
            FloatTy::F64 => write!(f, "f64"),
        }
    }
}
