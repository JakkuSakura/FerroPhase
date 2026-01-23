use fp_core::query::SqlDialect;
use sqlparser::dialect;

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
