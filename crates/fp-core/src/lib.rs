#![feature(iterator_try_collect)]

#[macro_use]
pub mod macros;

pub mod ast;
pub mod context;
pub mod cst;
pub mod ctx;
pub mod diagnostics;
pub mod error;
pub mod frontend;
pub mod hir;
pub mod id;
pub mod lir;
pub mod mir;
pub mod ops;
pub mod pat;
pub mod printer;
pub mod span;
pub mod utils;

// Re-export commonly used items for convenience
pub use tracing;

pub use frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
pub use hir::typed as thir;

// Alias for error types
pub type Error = crate::error::Error;
pub type Result<T> = crate::error::Result<T>;
