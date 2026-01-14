#![feature(iterator_try_collect)]

#[macro_use]
pub mod macros;

pub mod ast;
pub mod collections;
pub mod config;
pub mod context;
pub mod cst;
pub mod ctx;
pub mod diagnostics;
pub mod error;
pub mod frontend;
pub mod hir;
pub mod intrinsics;
pub mod lang;
pub mod lir;
pub mod mir;
pub mod module;
pub mod ops;
pub mod package;
pub mod pretty;
pub mod printer;
pub mod query;
pub mod span;
pub mod source_map;
pub mod utils;
pub mod vfs;
pub mod workspace;

// Re-export commonly used items for convenience
pub use tracing;

pub use frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

// Alias for error types
pub type Error = crate::error::Error;
pub type Result<T> = crate::error::Result<T>;
