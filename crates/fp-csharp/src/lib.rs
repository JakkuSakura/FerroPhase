//! C# code generation and transpilation support

pub mod package;
pub mod printer;
pub mod serializer;

pub use printer::CSharpPrinter;
pub use serializer::{CSharpBackend, CSharpSerializer};
