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

pub mod dialect {
    pub use sqlparser::dialect::*;
}

pub mod parser {
    pub use sqlparser::parser::*;
}

/// Canonical language identifier for SQL sources.
pub const SQL: &str = "sql";

pub fn split_statements(sql: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut current = String::new();
    let mut in_single = false;
    let mut in_double = false;
    let mut in_backtick = false;
    let mut chars = sql.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' if !in_double && !in_backtick => {
                in_single = !in_single;
                current.push(ch);
            }
            '"' if !in_single && !in_backtick => {
                in_double = !in_double;
                current.push(ch);
            }
            '`' if !in_single && !in_double => {
                in_backtick = !in_backtick;
                current.push(ch);
            }
            ';' if !in_single && !in_double && !in_backtick => {
                if !current.trim().is_empty() {
                    statements.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    if !current.trim().is_empty() {
        statements.push(current.trim().to_string());
    }

    statements
}

pub fn strip_leading_sql_comments(sql: &str) -> &str {
    let mut remaining = sql.trim_start();
    loop {
        let trimmed = remaining.trim_start();
        if trimmed.starts_with("--") {
            if let Some(idx) = trimmed.find('\n') {
                remaining = &trimmed[idx + 1..];
                continue;
            } else {
                return "";
            }
        }
        if trimmed.starts_with("/*") {
            if let Some(idx) = trimmed.find("*/") {
                remaining = &trimmed[idx + 2..];
                continue;
            } else {
                return "";
            }
        }
        return trimmed;
    }
}

pub fn parse_sql_dialect(name: &str) -> Option<SqlDialect> {
    let lowered = name.trim().to_ascii_lowercase();
    match lowered.as_str() {
        "" => None,
        "generic" => Some(SqlDialect::Generic),
        "clickhouse" | "ch" => Some(SqlDialect::ClickHouse),
        "postgres" | "postgresql" | "pg" => Some(SqlDialect::Postgres),
        "mysql" => Some(SqlDialect::Mysql),
        "sqlite" => Some(SqlDialect::Sqlite),
        "mssql" | "sqlserver" => Some(SqlDialect::Mssql),
        "duckdb" => Some(SqlDialect::DuckDb),
        "bigquery" => Some(SqlDialect::BigQuery),
        _ => None,
    }
}

pub fn sqlparser_dialect(dialect: SqlDialect) -> Box<dyn dialect::Dialect> {
    match dialect {
        SqlDialect::ClickHouse => Box::new(dialect::ClickHouseDialect {}),
        SqlDialect::Postgres => Box::new(dialect::PostgreSqlDialect {}),
        SqlDialect::Mysql => Box::new(dialect::MySqlDialect {}),
        SqlDialect::Sqlite => Box::new(dialect::SQLiteDialect {}),
        SqlDialect::Mssql => Box::new(dialect::MsSqlDialect {}),
        SqlDialect::DuckDb => Box::new(dialect::DuckDbDialect {}),
        SqlDialect::BigQuery => Box::new(dialect::BigQueryDialect {}),
        SqlDialect::Generic => Box::new(dialect::GenericDialect {}),
    }
}

pub fn replace_engine_case_insensitive(sql: &str, engine: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    let Some(idx) = find_keyword(&lower, "engine") else {
        return sql.to_string();
    };
    let mut cursor = idx + "engine".len();
    let bytes = sql.as_bytes();
    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
        cursor += 1;
    }
    if cursor < bytes.len() && bytes[cursor] == b'=' {
        cursor += 1;
    }
    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
        cursor += 1;
    }
    let start = cursor;
    while cursor < bytes.len()
        && (bytes[cursor].is_ascii_alphanumeric() || bytes[cursor] == b'_')
    {
        cursor += 1;
    }

    let mut output = String::new();
    output.push_str(sql[..start].trim_end());
    output.push(' ');
    output.push_str(engine);
    output.push(' ');
    output.push_str(sql[cursor..].trim_start());
    output.trim().to_string()
}

pub fn ensure_engine_clause(sql: &str, engine: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    if find_keyword(&lower, "engine").is_some() {
        return sql.to_string();
    }

    if let Some(order_idx) = find_keyword(&lower, "order by") {
        let (head, tail) = sql.split_at(order_idx);
        let mut output = head.trim_end().to_string();
        output.push_str(" ENGINE = ");
        output.push_str(engine);
        output.push(' ');
        output.push_str(tail.trim_start());
        return output;
    }

    let mut output = sql.trim_end().to_string();
    output.push_str(" ENGINE = ");
    output.push_str(engine);
    output
}

fn find_keyword(haystack_lower: &str, keyword: &str) -> Option<usize> {
    let target = keyword.to_ascii_lowercase();
    let mut start = 0;
    while let Some(idx) = haystack_lower[start..].find(&target) {
        let absolute = start + idx;
        if is_word_boundary(haystack_lower, absolute, target.len()) {
            return Some(absolute);
        }
        start = absolute + target.len();
    }
    None
}

fn is_word_boundary(text: &str, start: usize, len: usize) -> bool {
    let bytes = text.as_bytes();
    let before = start.checked_sub(1).and_then(|idx| bytes.get(idx));
    let after = bytes.get(start + len);

    let is_boundary = |b: &u8| !b.is_ascii_alphanumeric() && *b != b'_';

    let before_ok = before.map(is_boundary).unwrap_or(true);
    let after_ok = after.map(is_boundary).unwrap_or(true);
    before_ok && after_ok
}

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
            macro_parser: None,
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
