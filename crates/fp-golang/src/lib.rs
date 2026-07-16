//! Go language support for FerroPhase.

pub mod frontend;
pub mod manifest;
pub mod parser;
pub mod serializer;

pub use frontend::GoFrontend;
pub use manifest::{
    GoDependency, GoModManifest, default_module_roots, estimate_module_path,
    estimate_module_path_with_roots, read_go_mod,
};
pub use parser::GoParser;
pub use serializer::GoSerializer;

#[cfg(test)]
mod tests;
