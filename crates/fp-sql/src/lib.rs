//! FerroPhase frontend for SQL-like query sources.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result as CoreResult;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::query::{QueryDocument, QuerySerializer};

pub use fp_core::query::SqlDialect;

pub mod ast {
    pub use sqlparser::ast::*;
}

pub mod parser {
    pub use sqlparser::parser::Parser;
}

pub mod dialect {
    pub use sqlparser::dialect::ClickHouseDialect;
}

/// Split a SQL payload into individual statements, respecting quoting rules.
pub fn split_statements(input: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut current = String::new();
    let mut in_single = false;
    let mut in_double = false;
    let mut prev_char: Option<char> = None;

    for ch in input.chars() {
        match ch {
            '\'' if !in_double && !matches!(prev_char, Some('\\')) => {
                in_single = !in_single;
                current.push(ch);
            }
            '"' if !in_single && !matches!(prev_char, Some('\\')) => {
                in_double = !in_double;
                current.push(ch);
            }
            ';' if !in_single && !in_double => {
                if !current.trim().is_empty() {
                    statements.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
        prev_char = Some(ch);
    }

    if !current.trim().is_empty() {
        statements.push(current.trim().to_string());
    }

    statements
}

/// Strip leading `--` and `/* */` style SQL comments.
pub fn strip_leading_sql_comments(mut sql: &str) -> &str {
    loop {
        let trimmed = sql.trim_start();
        if trimmed.is_empty() {
            return trimmed;
        }
        if trimmed.starts_with("--") {
            if let Some(line_end) = trimmed.find('\n') {
                sql = &trimmed[line_end + 1..];
                continue;
            } else {
                return "";
            }
        }
        if trimmed.starts_with("/*") {
            if let Some(end_idx) = trimmed.find("*/") {
                sql = &trimmed[end_idx + 2..];
                continue;
            } else {
                return "";
            }
        }
        return trimmed;
    }
}

/// Replace a ClickHouse engine name `MixedMergeTree` (case-insensitive) with another engine.
pub fn replace_engine_case_insensitive(sql: &str, new_engine: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    if let Some(pos) = lower.find("mixedmergetree") {
        let end = pos + "mixedmergetree".len();
        let mut result =
            String::with_capacity(sql.len() - ("mixedmergetree".len()) + new_engine.len());
        result.push_str(&sql[..pos]);
        result.push_str(new_engine);
        result.push_str(&sql[end..]);
        result
    } else {
        sql.to_string()
    }
}

/// Ensure a `ENGINE = <engine>` clause exists in a CREATE TABLE.
pub fn ensure_engine_clause(sql: &str, engine: &str) -> String {
    if sql.to_ascii_lowercase().contains("engine") {
        return sql.to_string();
    }

    let lower = sql.to_ascii_lowercase();
    if let Some(order_pos) = lower.find("order by") {
        let (before, after) = sql.split_at(order_pos);
        format!("{} ENGINE = {} {}", before.trim_end(), engine, after)
    } else {
        let trimmed = sql.trim_end();
        let has_semicolon = trimmed.ends_with(';');
        let base = trimmed.trim_end_matches(';').trim_end();
        if has_semicolon {
            format!("{} ENGINE = {};", base, engine)
        } else {
            format!("{} ENGINE = {}", base, engine)
        }
    }
}

/// Canonical language identifier for SQL sources.
pub const SQL: &str = "sql";

/// Basic SQL frontend that tokenises query statements into a query document.
#[derive(Debug, Clone)]
pub struct SqlFrontend {
    dialect: SqlDialect,
}

impl SqlFrontend {
    /// Create a frontend that assumes a generic SQL dialect.
    pub fn new() -> Self {
        Self {
            dialect: SqlDialect::Generic,
        }
    }

    /// Create a frontend that targets a concrete SQL dialect.
    pub fn with_dialect(dialect: SqlDialect) -> Self {
        Self { dialect }
    }

    fn build_document(&self, source: &str, path: Option<&Path>) -> QueryDocument {
        let mut document = QueryDocument::sql(source.to_string(), self.dialect.clone());
        if let Some(path) = path {
            if let Some(name) = path.file_name().and_then(|value| value.to_str()) {
                document = document.with_name(name.to_string());
            }
        }
        document
    }
}

impl Default for SqlFrontend {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageFrontend for SqlFrontend {
    fn language(&self) -> &'static str {
        SQL
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["sql"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let document = self.build_document(source, path);

        if document.is_empty() {
            diagnostics.add_diagnostic(
                Diagnostic::warning("SQL input is empty".to_string())
                    .with_source_context("sql frontend"),
            );
        }

        let serializer: Arc<dyn AstSerializer> = Arc::new(QuerySerializer::new());
        let serialized = serializer.serialize_query(&document).ok();
        let description = match path {
            Some(path) => format!("SQL document {}", path.display()),
            None => "SQL document <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized,
        };

        let node = Node::query(document.clone());

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer,
            intrinsic_normalizer: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::diagnostics::DiagnosticLevel;

    #[test]
    fn parses_basic_select() {
        let frontend = SqlFrontend::new();
        let result = frontend
            .parse("SELECT 1;", None)
            .expect("sql frontend should parse");

        match result.ast.kind() {
            fp_core::ast::NodeKind::Query(doc) => {
                assert!(!doc.is_empty(), "query document should not be empty");
                if let Some(sql) = doc.kind.as_sql() {
                    assert_eq!(sql.statements.len(), 1);
                    assert!(sql.statements[0].text.to_uppercase().contains("SELECT"));
                } else {
                    panic!("expected sql query kind");
                }
            }
            other => panic!("expected query node, found {other:?}"),
        }

        assert!(!result
            .diagnostics
            .get_diagnostics()
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error));
    }

    #[test]
    fn parses_multiple_statements_and_attaches_name() {
        use std::path::Path;

        let frontend = SqlFrontend::with_dialect(SqlDialect::Sqlite);
        let source = "CREATE TABLE items(id INTEGER);\nINSERT INTO items VALUES (1);\n";
        let result = frontend
            .parse(source, Some(Path::new("queries/schema.sql")))
            .expect("sql frontend should parse multiple statements");

        let fp_core::ast::NodeKind::Query(doc) = result.ast.kind() else {
            panic!("expected query node");
        };
        let sql = doc.kind.as_sql().expect("expected sql query");
        assert_eq!(doc.name.as_deref(), Some("schema.sql"));
        assert_eq!(sql.dialect, SqlDialect::Sqlite);
        assert_eq!(sql.statements.len(), 2, "expected two SQL statements");
        assert_eq!(sql.statements[0].text, "CREATE TABLE items(id INTEGER)");
        assert_eq!(sql.statements[1].text, "INSERT INTO items VALUES (1)");
    }
}
