// Modular MIR→LIR lowering shim; re-export implementation from submodules.
pub(crate) use fp_core::mir::ty::{Ty, TypeAndMut};
pub(crate) use std::cell::RefCell;

mod abi; // planned
mod aggregates;
mod const_eval; // existing const-eval helpers
mod instr;
mod layout; // planned
mod operations;
mod places;
mod terminators;
#[cfg(test)]
mod tests;
mod type_conversion;
mod type_helpers;

pub use instr::*;
