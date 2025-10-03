//! Typed HIR type aliases and helpers.
//!
//! The HIR lowering stage re-uses the shared type definitions from
//! `fp_core::hir::ty`, but we expose them through this module so that
//! transformations can depend on typed HIR types without importing the
//! entire shared namespace.

pub use crate::hir::ty::{
    Abi, AdtDef, AdtFlags, ConstKind, ConstValue, FloatTy, FnSig, GenericArg, IntTy, Mutability,
    PolyFnSig, Region, Ty, TyKind, TypeAndMut, UintTy,
};

pub type DefId = crate::hir::ty::DefId;
pub type SubstsRef = crate::hir::ty::SubstsRef;
