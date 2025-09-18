// use std::collections::HashMap; // Temporarily disabled - unused
use std::fmt;

pub type DefId = u32;
pub type LocalDefId = u32;
pub type InternedTy = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    pub kind: TyKind,
    pub flags: TypeFlags,
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
    Slice { data: Vec<u8>, start: usize, end: usize },
    ByRef { alloc: AllocId, offset: Size },
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
    Restricted { path: SimplifiedType, id: LocalDefId },
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

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct AdtFlags: u32 {
        const NO_ADT_FLAGS        = 0;
        const IS_ENUM             = 1 << 0;
        const IS_UNION            = 1 << 1;
        const IS_STRUCT           = 1 << 2;
        const HAS_CTOR            = 1 << 3;
        const IS_PHANTOM_DATA     = 1 << 4;
        const IS_FUNDAMENTAL      = 1 << 5;
        const IS_BOX              = 1 << 6;
        const IS_MANUALLY_DROP    = 1 << 7;
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct ReprFlags: u8 {
        const IS_C               = 1 << 0;
        const IS_SIMD            = 1 << 1;
        const IS_TRANSPARENT     = 1 << 2;
        const IS_LINEAR          = 1 << 3;
        const IS_PACKED          = 1 << 4;
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TypeFlags: u32 {
        const HAS_PARAMS                 = 1 << 0;
        const HAS_TY_INFER               = 1 << 1;
        const HAS_RE_INFER               = 1 << 2;
        const HAS_CT_INFER               = 1 << 3;
        const HAS_TY_PLACEHOLDER         = 1 << 4;
        const HAS_RE_PLACEHOLDER         = 1 << 5;
        const HAS_CT_PLACEHOLDER         = 1 << 6;
        const HAS_TY_BOUND               = 1 << 7;
        const HAS_RE_BOUND               = 1 << 8;
        const HAS_CT_BOUND               = 1 << 9;
        const HAS_FREE_LOCAL_REGIONS     = 1 << 10;
        const HAS_TY_ERR                 = 1 << 11;
        const HAS_PROJECTION             = 1 << 12;
        const HAS_CT_PROJECTION          = 1 << 13;
        const STILL_FURTHER_SPECIALIZABLE = 1 << 14;
        const HAS_RE_LATE_BOUND          = 1 << 15;
        const HAS_FREE_REGIONS           = 1 << 16;
        const HAS_RE_ERASED              = 1 << 17;
        const NEEDS_DROP                 = 1 << 18;
        const NEEDS_SUBST                = 1 << 19;
        const HAS_NORMALIZABLE_PROJECTION = 1 << 20;
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
pub type Symbol = String;
pub type SimplifiedType = String; // Simplified for now

// Implementation helpers
impl Ty {
    pub fn new(kind: TyKind) -> Self {
        let flags = TypeFlags::from_ty_kind(&kind);
        Self { kind, flags }
    }
    
    pub fn bool() -> Self {
        Self::new(TyKind::Bool)
    }
    
    pub fn char() -> Self {
        Self::new(TyKind::Char)
    }
    
    pub fn int(int_ty: IntTy) -> Self {
        Self::new(TyKind::Int(int_ty))
    }
    
    pub fn uint(uint_ty: UintTy) -> Self {
        Self::new(TyKind::Uint(uint_ty))
    }
    
    pub fn float(float_ty: FloatTy) -> Self {
        Self::new(TyKind::Float(float_ty))
    }
    
    pub fn never() -> Self {
        Self::new(TyKind::Never)
    }
    
    pub fn is_primitive(&self) -> bool {
        matches!(
            self.kind,
            TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
    }
    
    pub fn is_numeric(&self) -> bool {
        matches!(self.kind, TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_))
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

impl TypeFlags {
    pub fn from_ty_kind(kind: &TyKind) -> Self {
        let mut flags = TypeFlags::empty();
        
        match kind {
            TyKind::Param(_) => {
                flags |= TypeFlags::HAS_PARAMS;
                flags |= TypeFlags::NEEDS_SUBST;
            }
            TyKind::Infer(_) => {
                flags |= TypeFlags::HAS_TY_INFER;
            }
            TyKind::Error(_) => {
                flags |= TypeFlags::HAS_TY_ERR;
            }
            TyKind::Projection(_) => {
                flags |= TypeFlags::HAS_PROJECTION;
            }
            TyKind::Placeholder(_) => {
                flags |= TypeFlags::HAS_TY_PLACEHOLDER;
            }
            TyKind::Bound(_, _) => {
                flags |= TypeFlags::HAS_TY_BOUND;
            }
            _ => {}
        }
        
        flags
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