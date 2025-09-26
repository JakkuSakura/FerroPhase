use std::fmt;

use super::{
    AdtDef, BoundTy, BoundTyKind, ConstKind, ConstValue, DebruijnIndex, DefId, ErrorGuaranteed,
    ExistentialPredicate, FloatTy, IntTy, Movability, ParamTy, PlaceholderType, PolyFnSig,
    ProjectionTy, Region, SubstsRef, TypeAndMut, UintTy,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Adt(AdtDef, SubstsRef),
    Array(Box<Ty>, ConstKind),
    Slice(Box<Ty>),
    RawPtr(TypeAndMut),
    Ref(Region, Box<Ty>, super::Mutability),
    FnDef(DefId, SubstsRef),
    FnPtr(PolyFnSig),
    Dynamic(Vec<ExistentialPredicate>, Region),
    Closure(DefId, SubstsRef),
    Generator(DefId, SubstsRef, Movability),
    GeneratorWitness(Vec<Box<Ty>>),
    Never,
    Tuple(Vec<Box<Ty>>),
    Projection(ProjectionTy),
    Opaque(DefId, SubstsRef),
    Param(ParamTy),
    Bound(DebruijnIndex, BoundTy),
    Placeholder(PlaceholderType),
    Infer(super::InferTy),
    Error(ErrorGuaranteed),
}

impl Ty {
    pub fn bool() -> Self {
        Ty::Bool
    }

    pub fn char() -> Self {
        Ty::Char
    }

    pub fn int(int_ty: IntTy) -> Self {
        Ty::Int(int_ty)
    }

    pub fn uint(uint_ty: UintTy) -> Self {
        Ty::Uint(uint_ty)
    }

    pub fn float(float_ty: FloatTy) -> Self {
        Ty::Float(float_ty)
    }

    pub fn never() -> Self {
        Ty::Never
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Ty::Bool | Ty::Char | Ty::Int(_) | Ty::Uint(_) | Ty::Float(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Ty::Int(_) | Ty::Uint(_) | Ty::Float(_))
    }

    pub fn is_integral(&self) -> bool {
        matches!(self, Ty::Int(_) | Ty::Uint(_))
    }

    pub fn is_floating_point(&self) -> bool {
        matches!(self, Ty::Float(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Ty::Int(_))
    }

    pub fn is_machine(&self) -> bool {
        matches!(self, Ty::Int(_) | Ty::Uint(_) | Ty::Float(_))
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::Int(int_ty) => write!(f, "{}", int_ty),
            Ty::Uint(uint_ty) => write!(f, "{}", uint_ty),
            Ty::Float(float_ty) => write!(f, "{}", float_ty),
            Ty::Never => write!(f, "!"),
            Ty::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            _ => write!(f, "<??>"),
        }
    }
}
