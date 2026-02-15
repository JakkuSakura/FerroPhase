//! FerroPhase frontend for SQL-like query sources.

pub use fp_core::query::SqlDialect;
pub mod ast;
pub mod dialect;
pub mod frontend;
pub mod parser;
pub mod select;
pub mod split;
pub mod sql_ast;

/// Canonical language identifier for SQL sources.
pub const SQL: &str = "sql";

pub use dialect::{parse_sql_dialect, sqlparser_dialect};
pub use frontend::SqlFrontend;
pub use select::extract_select_projection;
pub use split::{
    ensure_engine_clause, replace_engine_case_insensitive, split_statements,
    strip_leading_sql_comments,
};

#[cfg(test)]
mod tests;
