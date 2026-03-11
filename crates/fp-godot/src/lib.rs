//! Godot / GDScript AST target support for FerroPhase.

mod serializer;

pub use serializer::GdscriptSerializer;

#[cfg(test)]
mod tests;
