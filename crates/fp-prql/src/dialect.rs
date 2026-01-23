use fp_core::query::SqlDialect;

pub fn detect_target_dialect(source: &str) -> Option<SqlDialect> {
    for line in source.lines() {
        let trimmed = line.trim();
        if let Some(value) = trimmed.strip_prefix("target:") {
            return map_target(value.trim());
        } else if let Some(value) = trimmed.strip_prefix("sql_dialect:") {
            return map_target(value.trim());
        }
    }
    None
}

fn map_target(value: &str) -> Option<SqlDialect> {
    let trimmed = value.trim().trim_matches(|ch| ch == '"' || ch == '\'');
    let lowered = trimmed.to_ascii_lowercase();
    let tokens: Vec<&str> = lowered
        .split(|ch: char| !ch.is_ascii_alphanumeric())
        .filter(|token| !token.is_empty())
        .collect();
    let matches = |needle: &str| lowered == needle || tokens.iter().any(|token| *token == needle);

    if matches("clickhouse") || matches("ch") {
        Some(SqlDialect::ClickHouse)
    } else if matches("postgres") || matches("postgresql") || matches("pg") {
        Some(SqlDialect::Postgres)
    } else if matches("mysql") {
        Some(SqlDialect::Mysql)
    } else if matches("sqlite") {
        Some(SqlDialect::Sqlite)
    } else if matches("duckdb") {
        Some(SqlDialect::DuckDb)
    } else if matches("bigquery") {
        Some(SqlDialect::BigQuery)
    } else if matches("mssql") || matches("sqlserver") {
        Some(SqlDialect::Mssql)
    } else {
        None
    }
}
