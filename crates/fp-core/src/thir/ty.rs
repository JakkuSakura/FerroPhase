//! THIR-level type aliases and helpers.
//!
//! The THIR stage primarily re-uses the shared type definitions from
//! `fp_core::types`, but we expose them through this module so that
//! transformations can depend on THIR types without importing the
//! entire shared namespace.

pub use crate::types::{
    Abi, AdtDef, AdtFlags, ConstKind, ConstValue, FloatTy, FnSig, GenericArg, IntTy, Mutability,
    PolyFnSig, Region, Ty, TyKind, TypeAndMut, TypeFlags, UintTy,
};

pub type DefId = crate::types::DefId;
pub type SubstsRef = crate::types::SubstsRef;
