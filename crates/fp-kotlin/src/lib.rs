//! Kotlin source target transpilation support
//!
//! Walks the FerroPhase AST and emits idiomatic Kotlin source code.
//! Handles data classes, enum classes, functions, imports, and full expression trees.

pub mod serializer;

pub use serializer::KotlinSerializer;
