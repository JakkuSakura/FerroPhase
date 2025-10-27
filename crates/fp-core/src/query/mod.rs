//! Query document representations shared by SQL- and PRQL-based frontends.
//!
//! The current design focuses on lightweight parsing and normalisation of
//! textual query sources so they can participate in the shared `Node` wrapper
//! alongside AST nodes. Downstream stages that understand query nodes can
//! inspect the structured statements while other consumers still have access to
//! the original source text.

use crate::ast::AstSerializer;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};
use std::fmt::{self, Display, Formatter};

// Dialects supported by the SQL frontend.
common_enum! {
    pub enum SqlDialect {
        Generic,
        Postgres,
        Mysql,
        Sqlite,
        Mssql,
        DuckDb,
        BigQuery,
    }
}

impl Default for SqlDialect {
    fn default() -> Self {
        Self::Generic
    }
}

impl Display for SqlDialect {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            SqlDialect::Generic => "generic",
            SqlDialect::Postgres => "postgres",
            SqlDialect::Mysql => "mysql",
            SqlDialect::Sqlite => "sqlite",
            SqlDialect::Mssql => "mssql",
            SqlDialect::DuckDb => "duckdb",
            SqlDialect::BigQuery => "bigquery",
        })
    }
}

// Structured representation of a query document.
common_struct! {
    pub struct QueryDocument {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,
        #[serde(flatten)]
        pub kind: QueryKind,
    }
}

impl QueryDocument {
    /// Construct a SQL query document from raw source.
    pub fn sql(source: impl Into<String>, dialect: SqlDialect) -> Self {
        let query = SqlQuery::from_source(source.into(), dialect);
        Self {
            name: None,
            kind: QueryKind::Sql(query),
        }
    }

    /// Construct a PRQL pipeline from raw source.
    pub fn prql(pipeline: impl Into<String>) -> Self {
        let prql = PrqlQuery::new(pipeline.into());
        Self {
            name: None,
            kind: QueryKind::Prql(prql),
        }
    }

    /// Returns true when the query document has no statements or pipeline text.
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            QueryKind::Sql(sql) => {
                sql.statements.is_empty()
                    && sql
                        .raw
                        .as_ref()
                        .map(|s| s.trim().is_empty())
                        .unwrap_or(true)
            }
            QueryKind::Prql(prql) => prql.pipeline.trim().is_empty(),
            QueryKind::Any(_) => false,
        }
    }

    /// Render the query back to a textual representation.
    pub fn to_string_render(&self) -> String {
        match &self.kind {
            QueryKind::Sql(sql) => sql.to_string(),
            QueryKind::Prql(prql) => prql.to_string(),
            QueryKind::Any(any) => format!("{any:?}"),
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

impl Display for QueryDocument {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            writeln!(f, "-- {}", name)?;
        }
        match &self.kind {
            QueryKind::Sql(sql) => write!(f, "{sql}"),
            QueryKind::Prql(prql) => write!(f, "{prql}"),
            QueryKind::Any(any) => write!(f, "{any:?}"),
        }
    }
}

// Variants for query document payloads.
common_enum! {
    pub enum QueryKind {
        Sql(SqlQuery),
        Prql(PrqlQuery),
        Any(AnyBox),
    }
}

impl QueryKind {
    pub fn as_sql(&self) -> Option<&SqlQuery> {
        match self {
            QueryKind::Sql(sql) => Some(sql),
            _ => None,
        }
    }

    pub fn as_prql(&self) -> Option<&PrqlQuery> {
        match self {
            QueryKind::Prql(prql) => Some(prql),
            _ => None,
        }
    }

    pub fn any<T: AnyBoxable>(value: T) -> Self {
        QueryKind::Any(AnyBox::new(value))
    }
}

// Individual SQL statement captured during lightweight parsing.
common_struct! {
    pub struct QueryStatement {
        pub text: String,
    }
}

impl QueryStatement {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into().trim().to_string(),
        }
    }
}

impl Display for QueryStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.text)
    }
}

// SQL query representation with optional original source.
common_struct! {
    pub struct SqlQuery {
        #[serde(default)]
        pub dialect: SqlDialect,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub statements: Vec<QueryStatement>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub raw: Option<String>,
    }
}

impl SqlQuery {
    pub fn new(statements: Vec<QueryStatement>, dialect: SqlDialect) -> Self {
        Self {
            dialect,
            statements,
            raw: None,
        }
    }

    pub fn from_source(source: String, dialect: SqlDialect) -> Self {
        let statements = split_sql_statements(&source)
            .into_iter()
            .map(QueryStatement::new)
            .collect::<Vec<_>>();
        Self {
            dialect,
            statements,
            raw: Some(source),
        }
    }
}

impl Display for SqlQuery {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(raw) = &self.raw {
            return f.write_str(raw.trim());
        }

        for (idx, stmt) in self.statements.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "{};", stmt)?;
        }
        Ok(())
    }
}

// PRQL pipeline representation with optional compiled SQL.
common_struct! {
    pub struct PrqlQuery {
        pub pipeline: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub target: Option<SqlDialect>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub compiled: Vec<QueryStatement>,
    }
}

impl PrqlQuery {
    pub fn new(pipeline: String) -> Self {
        Self {
            pipeline,
            target: None,
            compiled: Vec::new(),
        }
    }

    pub fn with_target(mut self, dialect: SqlDialect) -> Self {
        self.target = Some(dialect);
        self
    }

    pub fn with_compiled(mut self, statements: Vec<QueryStatement>) -> Self {
        self.compiled = statements;
        self
    }
}

impl Display for PrqlQuery {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.pipeline.trim())
    }
}

/// Basic SQL statement splitter that respects string literals.
pub fn split_sql_statements(source: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut current = String::new();
    let mut single_quote = false;
    let mut double_quote = false;
    let mut backtick = false;
    let mut prev_char: Option<char> = None;

    for ch in source.chars() {
        match ch {
            '\'' if !double_quote && !backtick && prev_char != Some('\\') => {
                single_quote = !single_quote;
                current.push(ch);
            }
            '"' if !single_quote && !backtick && prev_char != Some('\\') => {
                double_quote = !double_quote;
                current.push(ch);
            }
            '`' if !single_quote && !double_quote && prev_char != Some('\\') => {
                backtick = !backtick;
                current.push(ch);
            }
            ';' if !single_quote && !double_quote && !backtick => {
                let trimmed = current.trim();
                if !trimmed.is_empty() {
                    statements.push(trimmed.to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
        prev_char = Some(ch);
    }

    let trimmed = current.trim();
    if !trimmed.is_empty() {
        statements.push(trimmed.to_string());
    }

    statements
}

/// Minimal serializer that renders a query document back to text.
#[derive(Debug, Default, Clone, Copy)]
pub struct QuerySerializer;

impl QuerySerializer {
    pub fn new() -> Self {
        Self
    }
}

impl AstSerializer for QuerySerializer {
    fn serialize_query(&self, query: &QueryDocument) -> std::result::Result<String, crate::Error> {
        Ok(query.to_string_render())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_sql_statements_handles_quotes_and_semicolons() {
        let source = "SELECT 1; INSERT INTO items VALUES ('value;still');\nUPDATE t SET name = \"semi;colon\" WHERE id = 1;";
        let statements = split_sql_statements(source);
        assert_eq!(
            statements.len(),
            3,
            "expected three statements: {statements:?}"
        );
        assert_eq!(statements[0], "SELECT 1");
        assert_eq!(
            statements[1], "INSERT INTO items VALUES ('value;still')",
            "second statement should keep quoted semicolon"
        );
        assert_eq!(
            statements[2],
            "UPDATE t SET name = \"semi;colon\" WHERE id = 1"
        );
    }

    #[test]
    fn query_document_sql_tracks_name_and_serializes() {
        let document = QueryDocument::sql("SELECT 42", SqlDialect::Postgres).with_name("query.sql");
        assert!(!document.is_empty());
        assert_eq!(document.name.as_deref(), Some("query.sql"));
        assert_eq!(
            document.kind.as_sql().unwrap().dialect,
            SqlDialect::Postgres
        );
        let rendered = document.to_string_render();
        assert_eq!(rendered, "SELECT 42");
    }

    #[test]
    fn query_document_prql_records_pipeline() {
        let document = QueryDocument::prql("from table").with_name("pipeline.prql");
        assert_eq!(document.name.as_deref(), Some("pipeline.prql"));
        let QueryKind::Prql(prql) = &document.kind else {
            panic!("expected prql variant");
        };
        assert_eq!(prql.pipeline, "from table");
        assert!(prql.compiled.is_empty());
    }
}
