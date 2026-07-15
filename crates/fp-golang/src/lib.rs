//! Go language support for FerroPhase.

pub mod frontend;
pub mod manifest;
pub mod parser;
pub mod serializer;

pub use frontend::GoFrontend;
pub use manifest::{read_go_mod, GoDependency, GoModManifest};
pub use parser::GoParser;
pub use serializer::GoSerializer;

#[cfg(test)]
mod tests;
