//! THIR-level type aliases and helpers.
//!
//! The THIR stage re-uses the shared type definitions from
//! `fp_core::hir::ty`, but we expose them through this module so that
//! transformations can depend on THIR types without importing the
//! entire shared namespace.

pub use crate::hir::ty::{
    Abi, AdtDef, AdtFlags, ConstKind, ConstValue, FloatTy, FnSig, GenericArg, IntTy, Mutability,
    PolyFnSig, Region, Ty, TyKind, TypeAndMut, UintTy,
};

pub type DefId = crate::hir::ty::DefId;
pub type SubstsRef = crate::hir::ty::SubstsRef;
