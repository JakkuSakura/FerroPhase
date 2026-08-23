//! Bytecode → LIR lowering pass.
//!
//! Converts a stack-based [`BytecodeProgram`][fp_bytecode::BytecodeProgram]
//! into the register-based SSA [`LirBlob`][fp_core::lir::LirBlob]
//! consumed by [`fp-interpret`][fp_interpret].
//!
//! # Architecture
//!
//! ```text
//! BytecodeProgram ──▶ lower_program() ──▶ LirBlob ──▶ fp-interpret
//! ```
//!
//! The pass simulates the bytecode operand stack at compile time and
//! assigns a fresh virtual register ([`RegisterId`]) to every produced
//! value.  Bytecode local variables become `Alloca` stack slots in the
//! entry block with `Load`/`Store` access.
//!
//! # Compound value ABI
//!
//! Scalar values (`Int`, `UInt`, `Float`, `Bool`) flow through LIR
//! registers directly as 64-bit words.
//!
//! Compound values (`Tuple`, `Array`, `List`, `Map`, `Str`) are stored
//! on the managed object heap that `fp-interpret` already maintains
//! (`objects: Vec<Value>`).  The LIR representation carries opaque
//! **object handles** (`u64` indices into that table).  Construction
//! and field access are lowered to calls to runtime intrinsics
//! (`__bc_make_tuple`, `__bc_array_get`, etc.).
//!
//! # Limitations
//!
//! - Multi-predecessor blocks do not emit φ-nodes; the simulated stack
//!   is cleared at each block entry.  This is sound for bytecode that
//!   was produced by a φ-aware lowering, but would produce incorrect
//!   LIR for general bytecode.
//! - Most [`CallKind`] variants beyond `Println`/`Print`/
//!   `Format`/`Len`/`TimeNow` return [`LowerError::Unsupported`].
//! - The runtime intrinsics (`__bc_*`) are declared as external
//!   `Call` targets but not yet implemented in `fp-interpret`.

pub mod cfg;
pub mod constants;
pub mod context;
pub mod function;
pub mod ops;

#[cfg(test)]
mod tests;

pub use context::lower_program;

use std::fmt;

/// Errors raised during bytecode→LIR lowering.
#[derive(Debug, Clone)]
pub enum LowerError {
    /// The bytecode contains an instruction or pattern the lowerer does
    /// not yet handle.
    Unsupported(String),
    /// An internal invariant was violated (e.g. stack underflow during
    /// simulation, missing constant pool entry).
    Internal(String),
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LowerError::Unsupported(msg) => write!(f, "unsupported bytecode: {msg}"),
            LowerError::Internal(msg) => write!(f, "internal lowering error: {msg}"),
        }
    }
}

impl std::error::Error for LowerError {}

/// Shorthand for lowering results.
pub(crate) type LowerResult<T> = Result<T, LowerError>;
