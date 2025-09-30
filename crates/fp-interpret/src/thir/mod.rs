//! THIR (Typed High-level Intermediate Representation) interpretation.
//!
//! This module provides runtime and compile-time interpretation of THIR,
//! enabling const evaluation and runtime execution.

pub mod format;
mod interpreter;

pub use format::build_printf_format;
pub use interpreter::{InterpretationOrchestrator, InterpreterMode};
