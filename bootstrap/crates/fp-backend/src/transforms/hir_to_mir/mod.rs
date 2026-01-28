// Modular HIR→MIR lowering: re-export the implementation from submodules.
mod borrow;
mod control_flow; // planned
mod expr;
mod stmt; // planned
mod types; // planned // planned

pub use expr::*;
