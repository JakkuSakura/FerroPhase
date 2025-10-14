//! Zig language support for FerroPhase.
//!
//! The Zig backend now mirrors other language crates with printer and
//! parser modules so expansion can follow a consistent layout.

pub mod parser;
pub mod printer;

pub use parser::ZigParser;
pub use printer::ZigSerializer;
