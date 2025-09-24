//! MIR-level view of shared type information.

pub use crate::types::{
    Abi, AdtDef, AdtFlags, ConstKind, ConstValue, FloatTy, FnSig, GenericArg, IntTy, Mutability,
    PolyFnSig, Region, Ty, TyKind, TypeAndMut, TypeFlags, UintTy,
};

pub type DefId = crate::types::DefId;
pub type SubstsRef = crate::types::SubstsRef;
