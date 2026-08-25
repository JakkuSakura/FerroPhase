// Modular HIR→MIR lowering: re-export the implementation from submodules.
pub(crate) use fp_core::intrinsics::IntrinsicKind;
pub(crate) use fp_core::ast::{Value, ValueList, ValueMap, ValueTuple};
pub(crate) use fp_core::hir::place::{
    project_hir_assign_target, HirAssignTargetBase, HirAssignTargetProjection,
};
pub(crate) use fp_core::mir::ty::{
    AdtDef, ConstKind, ConstValue, FloatTy, GenericArg, IntTy, Mutability, Scalar, ScalarInt,
    Ty, TyKind, TypeAndMut, UintTy,
};
pub(crate) use fp_core::mir::{MethodLoweringInfo, Symbol};
pub(crate) use fp_core::ops::format_value_with_spec;
pub(crate) use fp_core::span::Span;
pub(crate) use std::collections::{HashMap, HashSet};
pub(crate) use std::collections::hash_map::DefaultHasher;
pub(crate) use std::hash::{Hash, Hasher};

mod body;
mod body_control_flow;
mod body_environment;
mod borrow;
mod call_args;
mod const_expr;
mod control_flow; // planned
mod expr;
mod guards;
mod intrinsics;
mod patterns;
mod places;
mod specialization;
mod statements;
mod stmt; // planned
mod type_names;
mod types;
mod variants;

pub use body::*;
pub use expr::*;
