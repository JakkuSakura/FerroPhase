use fp_core::error::Result as CoreResult;
use fp_core::query::{QueryDocument, QueryKind, QueryStatement, SqlDialect};

use crate::dialect::detect_target_dialect;

#[derive(Debug, Clone)]
pub struct PrqlCompileResult {
    pub target: Option<SqlDialect>,
    pub statements: Vec<String>,
}

pub fn compile_prql(
    source: &str,
    target_override: Option<SqlDialect>,
) -> CoreResult<PrqlCompileResult> {
    let document = build_document(source, None, target_override);
    let mut target = None;
    let mut statements = Vec::new();

    if let QueryKind::Prql(prql) = &document.kind {
        target = prql.target.clone();
        statements = prql.compiled.iter().map(|stmt| stmt.text.clone()).collect();
    }

    Ok(PrqlCompileResult { target, statements })
}

pub fn build_document(
    source: &str,
    path: Option<&std::path::Path>,
    target_override: Option<SqlDialect>,
) -> QueryDocument {
    let mut document = QueryDocument::prql(source.to_string());

    if let Some(path) = path {
        if let Some(name) = path.file_name().and_then(|value| value.to_str()) {
            document = document.with_name(name.to_string());
        }
    }

    if let QueryKind::Prql(prql) = &mut document.kind {
        let target = target_override.or_else(|| detect_target_dialect(source));
        if let Some(target) = target {
            prql.target = Some(target);
        }

        if let Some(compiled_sql) = naive_prql_to_sql(source) {
            prql.compiled = fp_core::query::split_sql_statements(&compiled_sql)
                .into_iter()
                .map(QueryStatement::new)
                .collect();
            if prql.target.is_none() {
                prql.target = Some(SqlDialect::Generic);
            }
        }
    }

    document
}

fn naive_prql_to_sql(source: &str) -> Option<String> {
    let mut from_table: Option<String> = None;
    let mut selects: Vec<String> = Vec::new();
    let mut filters: Vec<String> = Vec::new();
    let mut limits: Option<String> = None;

    for chunk in source.split('|') {
        let trimmed = chunk.trim();
        if trimmed.is_empty() {
            continue;
        }
        for line in trimmed.lines() {
            let entry = line.trim();
            if entry.is_empty() {
                continue;
            }
            if let Some(rest) = entry.strip_prefix("from ") {
                from_table = Some(rest.trim().to_string());
            } else if let Some(rest) = entry.strip_prefix("select") {
                if let Some(fields) = extract_group(rest) {
                    selects.extend(
                        fields
                            .into_iter()
                            .map(|field| field.trim_matches('.').to_string()),
                    );
                }
            } else if let Some(rest) = entry.strip_prefix("filter ") {
                let normalized = rest.trim().replace("==", "=");
                filters.push(normalized);
            } else if let Some(rest) = entry.strip_prefix("take ") {
                limits = Some(rest.trim().to_string());
            }
        }
    }

    let table = from_table?;
    let select_clause = if selects.is_empty() {
        "*".to_string()
    } else {
        selects.join(", ")
    };

    let mut sql = format!("SELECT {} FROM {}", select_clause, table);

    if !filters.is_empty() {
        sql.push_str(" WHERE ");
        sql.push_str(&filters.join(" AND "));
    }

    if let Some(limit) = limits {
        sql.push_str(" LIMIT ");
        sql.push_str(&limit);
    }

    Some(sql)
}

fn extract_group(rest: &str) -> Option<Vec<String>> {
    let trimmed = rest.trim();
    let start = trimmed.find(['{', '['])?;
    let end = trimmed.rfind(['}', ']'])?;
    let group = &trimmed[start + 1..end];
    let mut fields = Vec::new();
    for field in group.split(',') {
        let entry = field.trim();
        if entry.is_empty() {
            continue;
        }
        if let Some((_, value)) = entry.split_once(':') {
            fields.push(value.trim().to_string());
        } else {
            fields.push(entry.to_string());
        }
    }
    if fields.is_empty() {
        None
    } else {
        Some(fields)
    }
}
