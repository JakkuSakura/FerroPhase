// Modular HIR→MIR lowering: re-export the implementation from submodules.
mod body;
mod borrow;
mod control_flow; // planned
mod expr;
mod stmt; // planned
mod types; // planned // planned

pub use body::*;
pub use expr::*;
