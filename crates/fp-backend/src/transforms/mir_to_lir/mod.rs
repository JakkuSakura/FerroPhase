// Modular MIR→LIR lowering shim; re-export implementation from submodules.
mod abi; // planned
mod const_eval; // existing const-eval helpers
mod instr;
mod layout; // planned
#[cfg(test)]
mod tests;

pub use instr::*;
