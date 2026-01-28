//! FerroPhase Interpreter
//!
//! This crate provides interpretation capabilities for FerroPhase IRs,
//! supporting both compile-time (const evaluation) and runtime execution.

pub mod const_eval;
pub mod engine;
pub mod error;
pub mod intrinsics;
