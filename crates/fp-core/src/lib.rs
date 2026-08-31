#[macro_use]
pub mod macros;

pub mod asmir;
pub mod ast;
pub mod backend;
pub mod cache;
pub mod capabilities;
pub mod cfg;
pub mod collections;
pub mod container;
pub mod context;
pub mod diagnostics;
pub mod embedded_std;
pub mod error;
pub mod executor;
pub mod frontend;
pub mod hir;
pub mod host_function;
pub mod host_layout;
pub mod intrinsics;
pub mod lang;
pub mod lir;
pub mod mir;
pub mod ops;
pub mod package;
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

pub use fp_host_layout_derive::{Host, HostLayout};
pub use frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
pub use host_function::HostFunctionDescriptor;
pub use host_layout::{HostFieldDescriptor, HostLayout, HostLayoutDescriptor, HostLayoutRegistry};

// Alias for error types
pub type Error = crate::error::Error;
pub type Result<T> = crate::error::Result<T>;
