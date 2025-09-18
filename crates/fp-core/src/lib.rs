#![feature(iterator_try_collect)]

#[macro_use]
pub mod macros;

pub mod ast;
pub mod context;
pub mod cst;
pub mod ctx;
pub mod error;
pub mod hir;
pub mod id;
pub mod mir;
pub mod ops;
pub mod passes;
pub mod pat;
pub mod printer;
pub mod span;
pub mod thir;
pub mod utils;


// Re-export commonly used items for convenience
pub use tracing;

// Alias for error types
pub type Error = crate::error::Error;
pub type Result<T> = crate::error::Result<T>;