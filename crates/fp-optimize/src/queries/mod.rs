// Queries - stateless operations for extracting information from AST and type system

pub mod type_queries;
pub mod dependency_queries;
pub mod scope_queries;

pub use type_queries::*;
pub use dependency_queries::*;
pub use scope_queries::*;