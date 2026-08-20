//! Godot / GDScript AST target support for FerroPhase.

pub mod package;
mod serializer;

pub use serializer::{GdscriptBackend, GdscriptSerializer};

#[cfg(test)]
mod tests;
