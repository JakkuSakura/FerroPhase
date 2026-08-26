//! Bytecode lowering for the shared LIR interpreter.
//!
//! This crate intentionally contains no bytecode execution engine. Use
//! [`lowering::lower_program`] to convert bytecode into an `fp_core::lir::LirBlob`
//! and execute that blob with `fp-interpret`.

pub mod lowering;
