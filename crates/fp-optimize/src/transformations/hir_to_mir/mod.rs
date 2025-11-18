// Modular HIR→MIR lowering: re-export the implementation from submodules.
mod expr;
mod stmt;        // planned
mod control_flow;// planned
mod types;       // planned
mod borrow;      // planned

pub use expr::*;
