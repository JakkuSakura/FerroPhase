//! Go language support for FerroPhase.

pub mod frontend;
pub mod parser;
pub mod serializer;

pub use frontend::GoFrontend;
pub use parser::GoParser;
pub use serializer::GoSerializer;

#[cfg(test)]
mod tests;
