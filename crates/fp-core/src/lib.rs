#[macro_use]
pub mod macros;

pub mod asmir;
pub mod ast;
pub mod backend;
pub mod capabilities;
pub mod cfg;
pub mod collections;
pub mod container;
pub mod context;
pub mod diagnostics;
pub mod error;
pub mod executor;
pub mod frontend;
pub mod hir;
pub mod intrinsics;
pub mod lang;
pub mod lir;
pub mod mir;
pub mod ops;
pub mod place;
pub mod pretty;
pub mod printer;
pub mod query;
pub mod source_map;
pub mod span;
pub mod utils;
pub mod vfs;
pub mod writer;

// Re-export commonly used items for convenience
pub use tracing;

pub use frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

// Alias for error types
pub type Error = crate::error::Error;
pub type Result<T> = crate::error::Result<T>;
