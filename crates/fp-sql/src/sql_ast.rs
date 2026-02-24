use anyhow::{anyhow, Result};
use fp_core::sql_ast as ast;
use sqlparser::ast as sql;
use sqlparser::dialect::ClickHouseDialect;
use sqlparser::keywords::Keyword;
use sqlparser::parser::Parser;
use sqlparser::tokenizer::{Token, Tokenizer, Whitespace, Word};
use std::collections::HashMap;

pub fn parse_sql_ast(
    source: &str,
    dialect: fp_core::query::SqlDialect,
) -> Result<Vec<ast::Statement>> {
    let statements = crate::split_statements(source);
    let mut out = Vec::new();
    for stmt in statements {
        let rewritten = rewrite_insert_columns_with_dots(&stmt);
        let rewritten = rewrite_alter_mutation(&rewritten);
        let (format, stripped) = split_format_clause(&rewritten);
        let (stripped, sample_ratio) = split_sample_clause(&stripped);
        let is_create_table = starts_with_keywords(&stripped, &["create", "table"]);
        let create_clauses = if is_create_table {
            Some(extract_create_table_clauses_from_sql(&stripped)?)
        } else {
            None
        };
        let sanitized = if is_create_table {
            sanitize_create_table_sql(&stripped)?
        } else {
            stripped
        };
        let dialect_impl = crate::sqlparser_dialect(dialect.clone());
        let mut parsed = Parser::parse_sql(dialect_impl.as_ref(), &sanitized)
            .map_err(|err| anyhow!("failed to parse SQL: {err}"))?;
        if parsed.is_empty() {
            continue;
        }
        if parsed.len() != 1 {
            return Err(anyhow!(
                "multiple statements detected; split before parsing"
            ));
        }
        let stmt = parsed.remove(0);
        out.push(convert_statement(
            stmt,
            format.clone(),
            sample_ratio,
            create_clauses,
        )?);
    }
    Ok(out)
}

fn rewrite_alter_mutation(sql: &str) -> String {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(_) => return sql.to_string(),
    };

    let mut depth = 0i32;
    let mut table_idx = None;
    let mut mutation_idx = None;
    let mut mutation_kind = None::<&str>;

    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if word.value.eq_ignore_ascii_case("alter") {
                    if let Some(next_idx) = next_non_ws(&tokens, idx + 1) {
                        if let Token::Word(next_word) = &tokens[next_idx] {
                            if next_word.value.eq_ignore_ascii_case("table") {
                                table_idx = Some(next_idx);
                            }
                        }
                    }
                } else if word.value.eq_ignore_ascii_case("update") {
                    mutation_idx = Some(idx);
                    mutation_kind = Some("update");
                    break;
                } else if word.value.eq_ignore_ascii_case("delete") {
                    mutation_idx = Some(idx);
                    mutation_kind = Some("delete");
                    break;
                }
            }
            _ => {}
        }
        idx += 1;
    }

    let (Some(table_idx), Some(mutation_idx), Some(kind)) =
        (table_idx, mutation_idx, mutation_kind)
    else {
        return sql.to_string();
    };
    let table_start = match next_non_ws(&tokens, table_idx + 1) {
        Some(idx) => idx,
        None => return sql.to_string(),
    };
    if table_start >= mutation_idx {
        return sql.to_string();
    }
    let table_sql = tokens[table_start..mutation_idx]
        .iter()
        .map(|t: &Token| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let rest_start = next_non_ws(&tokens, mutation_idx + 1).unwrap_or(tokens.len());
    let rest_sql = tokens[rest_start..]
        .iter()
        .map(|t: &Token| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let table_sql = table_sql.trim();
    let rest_sql = rest_sql.trim();
    if table_sql.is_empty() {
        return sql.to_string();
    }
    match kind {
        "update" => format!("UPDATE {table_sql} SET {rest_sql}"),
        "delete" => format!("DELETE FROM {table_sql} {rest_sql}"),
        _ => sql.to_string(),
    }
}

fn replace_keyword_case_insensitive(input: &str, keyword: &str, replacement: &str) -> String {
    let lower = input.to_ascii_lowercase();
    let mut out = String::with_capacity(input.len());
    let mut cursor = 0usize;
    while let Some(pos) = find_keyword(&lower[cursor..], keyword) {
        let abs = cursor + pos;
        out.push_str(&input[cursor..abs]);
        out.push_str(replacement);
        cursor = abs + keyword.len();
    }
    out.push_str(&input[cursor..]);
    out
}

fn find_keyword(haystack_lower: &str, keyword: &str) -> Option<usize> {
    let bytes = haystack_lower.as_bytes();
    let needle = keyword.as_bytes();
    if needle.is_empty() || needle.len() > bytes.len() {
        return None;
    }
    for idx in 0..=bytes.len() - needle.len() {
        if haystack_lower[idx..].starts_with(keyword) {
            let before_ok = idx == 0 || !bytes[idx - 1].is_ascii_alphanumeric();
            let after_idx = idx + needle.len();
            let after_ok = after_idx >= bytes.len() || !bytes[after_idx].is_ascii_alphanumeric();
            if before_ok && after_ok {
                return Some(idx);
            }
        }
    }
    None
}

fn starts_with_keywords(sql: &str, keywords: &[&str]) -> bool {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(_) => return false,
    };
    let mut idx = 0usize;
    for keyword in keywords {
        let Some(next_idx) = next_non_ws(&tokens, idx) else {
            return false;
        };
        match &tokens[next_idx] {
            Token::Word(word) if word.value.eq_ignore_ascii_case(keyword) => {
                idx = next_idx + 1;
            }
            _ => return false,
        }
    }
    true
}

fn convert_statement(
    stmt: sql::Statement,
    format: Option<String>,
    sample_ratio: Option<f64>,
    create_clauses: Option<CreateTableClauses>,
) -> Result<ast::Statement> {
    match stmt {
        sql::Statement::CreateTable {
            name,
            columns,
            engine,
            order_by,
            query,
            ..
        } => {
            let clauses = create_clauses.unwrap_or(CreateTableClauses {
                ttl: None,
                sample_by: None,
                as_table: None,
                engine: None,
                materialized_columns: HashMap::new(),
            });
            let cols = columns
                .into_iter()
                .map(|col| convert_column_def(col, &clauses.materialized_columns))
                .collect::<Result<Vec<_>>>()?;
            let as_table = clauses
                .as_table
                .as_ref()
                .map(|name| convert_object_name(name.clone()));
            let has_as_table = as_table.is_some();
            Ok(ast::Statement::CreateTable {
                name: convert_object_name(name),
                columns: cols,
                engine: clauses.engine.or(engine),
                order_by: order_by.map(convert_order_by_idents),
                as_table,
                ttl: clauses.ttl,
                sample_by: clauses.sample_by,
                query: if has_as_table {
                    None
                } else {
                    query.map(|q| Box::new(convert_query(*q, format.clone(), sample_ratio)))
                },
            })
        }
        sql::Statement::CreateView { name, query, .. } => Ok(ast::Statement::CreateView {
            name: convert_object_name(name),
            query: Box::new(convert_query(*query, format, sample_ratio)),
        }),
        sql::Statement::Drop {
            object_type, names, ..
        } => Ok(ast::Statement::Drop {
            object_type: match object_type {
                sql::ObjectType::Table => ast::ObjectType::Table,
                sql::ObjectType::View => ast::ObjectType::View,
                _ => ast::ObjectType::Table,
            },
            names: names.into_iter().map(convert_object_name).collect(),
        }),
        sql::Statement::AlterTable {
            name, operations, ..
        } => Ok(ast::Statement::AlterTable {
            name: convert_object_name(name),
            operations: operations
                .into_iter()
                .filter_map(convert_alter_op)
                .collect(),
        }),
        sql::Statement::Insert {
            table_name,
            columns,
            source,
            ..
        } => Ok(ast::Statement::Insert {
            table_name: convert_object_name(table_name),
            columns: columns.into_iter().map(convert_ident).collect(),
            source: source.map(|q| Box::new(convert_query(*q, format, sample_ratio))),
        }),
        sql::Statement::Query(query) => Ok(ast::Statement::Query(Box::new(convert_query(
            *query,
            format,
            sample_ratio,
        )))),
        sql::Statement::Update {
            table,
            assignments,
            selection,
            ..
        } => {
            let table_name = match table.relation {
                sql::TableFactor::Table { name, .. } => convert_object_name(name),
                _ => return Err(anyhow!("unsupported UPDATE table reference")),
            };
            Ok(ast::Statement::Update {
                table: table_name,
                assignments: assignments.into_iter().map(convert_assignment).collect(),
                selection: selection.map(convert_expr),
            })
        }
        sql::Statement::Delete {
            from, selection, ..
        } => Ok(ast::Statement::Delete {
            from: from.into_iter().map(convert_table_with_joins).collect(),
            selection: selection.map(convert_expr),
        }),
        _ => Err(anyhow!("unsupported SQL statement")),
    }
}

fn convert_query(
    query: sql::Query,
    format: Option<String>,
    sample_ratio: Option<f64>,
) -> ast::Query {
    ast::Query {
        body: Box::new(convert_set_expr(*query.body)),
        order_by: query.order_by.into_iter().map(convert_order_by).collect(),
        limit: query.limit.map(convert_expr),
        offset: query.offset.map(|o| convert_expr(o.value)),
        format,
        sample_ratio: sample_ratio.map(ast::SampleRatio),
    }
}

fn convert_set_expr(expr: sql::SetExpr) -> ast::SetExpr {
    match expr {
        sql::SetExpr::Select(select) => ast::SetExpr::Select(Box::new(convert_select(*select))),
        sql::SetExpr::Values(values) => ast::SetExpr::Values(ast::Values {
            rows: values
                .rows
                .into_iter()
                .map(|row| row.into_iter().map(convert_expr).collect())
                .collect(),
        }),
        sql::SetExpr::SetOperation {
            left,
            right,
            op,
            set_quantifier,
        } => {
            let op = match op {
                sql::SetOperator::Union => ast::SetOperator::Union,
                _ => ast::SetOperator::Union,
            };
            let set_quantifier = match set_quantifier {
                sql::SetQuantifier::All => ast::SetQuantifier::All,
                _ => ast::SetQuantifier::Distinct,
            };
            ast::SetExpr::SetOperation {
                left: Box::new(convert_set_expr(*left)),
                right: Box::new(convert_set_expr(*right)),
                op,
                set_quantifier,
            }
        }
        _ => ast::SetExpr::Values(ast::Values { rows: Vec::new() }),
    }
}

fn convert_select(select: sql::Select) -> ast::Select {
    ast::Select {
        projection: select
            .projection
            .into_iter()
            .map(convert_select_item)
            .collect(),
        from: select
            .from
            .into_iter()
            .map(convert_table_with_joins)
            .collect(),
        selection: select.selection.map(convert_expr),
        group_by: convert_group_by(select.group_by),
        having: select.having.map(convert_expr),
        distinct: select.distinct.map(|_| ast::Distinct),
    }
}

fn convert_select_item(item: sql::SelectItem) -> ast::SelectItem {
    match item {
        sql::SelectItem::UnnamedExpr(expr) => ast::SelectItem::UnnamedExpr(convert_expr(expr)),
        sql::SelectItem::ExprWithAlias { expr, alias } => ast::SelectItem::ExprWithAlias {
            expr: convert_expr(expr),
            alias: convert_ident(alias),
        },
        _ => ast::SelectItem::Wildcard,
    }
}

fn convert_group_by(group_by: sql::GroupByExpr) -> ast::GroupByExpr {
    match group_by {
        sql::GroupByExpr::Expressions(exprs) => {
            ast::GroupByExpr::Expressions(exprs.into_iter().map(convert_expr).collect())
        }
        _ => ast::GroupByExpr::Expressions(Vec::new()),
    }
}

fn convert_table_with_joins(twj: sql::TableWithJoins) -> ast::TableWithJoins {
    ast::TableWithJoins {
        relation: convert_table_factor(twj.relation),
        joins: twj.joins.into_iter().map(convert_join).collect(),
    }
}

fn convert_table_factor(tf: sql::TableFactor) -> ast::TableFactor {
    match tf {
        sql::TableFactor::Table { name, alias, .. } => ast::TableFactor::Table {
            name: convert_object_name(name),
            alias: alias.map(convert_alias),
        },
        sql::TableFactor::Derived {
            subquery, alias, ..
        } => ast::TableFactor::Derived {
            subquery: Box::new(convert_query(*subquery, None, None)),
            alias: alias.map(convert_alias),
        },
        _ => ast::TableFactor::Table {
            name: ast::ObjectName::new(vec![ast::Ident::new("")]),
            alias: None,
        },
    }
}

fn convert_alias(alias: sql::TableAlias) -> ast::TableAlias {
    ast::TableAlias {
        name: convert_ident(alias.name),
    }
}

fn convert_join(join: sql::Join) -> ast::Join {
    ast::Join {
        relation: convert_table_factor(join.relation),
        join_operator: convert_join_operator(join.join_operator),
    }
}

fn convert_join_operator(op: sql::JoinOperator) -> ast::JoinOperator {
    match op {
        sql::JoinOperator::LeftOuter(constraint) => {
            ast::JoinOperator::LeftOuter(convert_join_constraint(constraint))
        }
        sql::JoinOperator::Inner(constraint) => {
            ast::JoinOperator::Inner(convert_join_constraint(constraint))
        }
        _ => ast::JoinOperator::Inner(ast::JoinConstraint::None),
    }
}

fn convert_join_constraint(constraint: sql::JoinConstraint) -> ast::JoinConstraint {
    match constraint {
        sql::JoinConstraint::On(expr) => ast::JoinConstraint::On(convert_expr(expr)),
        _ => ast::JoinConstraint::None,
    }
}

fn convert_order_by(order: sql::OrderByExpr) -> ast::OrderByExpr {
    ast::OrderByExpr {
        expr: convert_expr(order.expr),
        asc: order.asc,
    }
}

fn convert_order_by_idents(order: Vec<sql::Ident>) -> Vec<ast::Ident> {
    order.into_iter().map(convert_ident).collect()
}

fn convert_column_def(
    def: sql::ColumnDef,
    materialized_columns: &HashMap<String, ast::Expr>,
) -> Result<ast::ColumnDef> {
    let mut options = def
        .options
        .into_iter()
        .filter_map(convert_column_option)
        .collect::<Vec<_>>();
    let name = convert_ident(def.name);
    if let Some(expr) = materialized_columns.get(&name.value.to_ascii_lowercase()) {
        options.retain(|opt| !matches!(opt.option, ast::ColumnOption::Default(_)));
        options.push(ast::ColumnOptionDef {
            option: ast::ColumnOption::Materialized(expr.clone()),
        });
    }
    Ok(ast::ColumnDef {
        name,
        data_type: convert_data_type(def.data_type)?,
        options,
    })
}

fn convert_column_option(def: sql::ColumnOptionDef) -> Option<ast::ColumnOptionDef> {
    match def.option {
        sql::ColumnOption::Default(expr) => Some(ast::ColumnOptionDef {
            option: ast::ColumnOption::Default(convert_expr(expr)),
        }),
        _ => None,
    }
}

fn convert_assignment(assign: sql::Assignment) -> ast::Assignment {
    ast::Assignment {
        id: assign.id.into_iter().map(convert_ident).collect(),
        value: convert_expr(assign.value),
    }
}

fn convert_alter_op(op: sql::AlterTableOperation) -> Option<ast::AlterTableOperation> {
    match op {
        sql::AlterTableOperation::AddColumn { column_def, .. } => {
            let def = convert_column_def(column_def, &HashMap::new()).ok()?;
            Some(ast::AlterTableOperation::AddColumn { column_def: def })
        }
        _ => None,
    }
}

fn convert_expr(expr: sql::Expr) -> ast::Expr {
    match expr {
        sql::Expr::Identifier(ident) => ast::Expr::Identifier(convert_ident(ident)),
        sql::Expr::CompoundIdentifier(idents) => {
            ast::Expr::CompoundIdentifier(idents.into_iter().map(convert_ident).collect())
        }
        sql::Expr::Value(val) => ast::Expr::Value(convert_value(val)),
        sql::Expr::BinaryOp { left, op, right } => ast::Expr::BinaryOp {
            left: Box::new(convert_expr(*left)),
            op: convert_binary_op(op),
            right: Box::new(convert_expr(*right)),
        },
        sql::Expr::UnaryOp { op, expr } => ast::Expr::UnaryOp {
            op: convert_unary_op(op),
            expr: Box::new(convert_expr(*expr)),
        },
        sql::Expr::Nested(expr) => ast::Expr::Nested(Box::new(convert_expr(*expr))),
        sql::Expr::Tuple(exprs) => ast::Expr::Tuple(exprs.into_iter().map(convert_expr).collect()),
        sql::Expr::Array(array) => {
            ast::Expr::Array(array.elem.into_iter().map(convert_expr).collect())
        }
        sql::Expr::Function(func) => ast::Expr::Function(convert_function(func)),
        sql::Expr::Cast {
            expr, data_type, ..
        } => ast::Expr::Cast {
            expr: Box::new(convert_expr(*expr)),
            data_type: convert_data_type(data_type).unwrap_or(ast::DataType::String),
        },
        sql::Expr::InList {
            expr,
            list,
            negated,
        } => ast::Expr::InList {
            expr: Box::new(convert_expr(*expr)),
            list: list.into_iter().map(convert_expr).collect(),
            negated,
        },
        sql::Expr::InSubquery {
            expr,
            subquery,
            negated,
        } => ast::Expr::InSubquery {
            expr: Box::new(convert_expr(*expr)),
            subquery: Box::new(convert_query(*subquery, None, None)),
            negated,
        },
        sql::Expr::Interval(interval) => ast::Expr::Interval(convert_interval(interval)),
        _ => ast::Expr::Value(ast::Value::Null),
    }
}

fn convert_function(func: sql::Function) -> ast::Function {
    ast::Function {
        name: convert_object_name(func.name),
        args: func
            .args
            .into_iter()
            .filter_map(convert_function_arg)
            .collect(),
        over: func.over.map(convert_window_type),
    }
}

fn convert_function_arg(arg: sql::FunctionArg) -> Option<ast::FunctionArg> {
    match arg {
        sql::FunctionArg::Unnamed(sql::FunctionArgExpr::Expr(expr)) => Some(
            ast::FunctionArg::Unnamed(ast::FunctionArgExpr::Expr(convert_expr(expr))),
        ),
        _ => None,
    }
}

fn convert_window_type(window: sql::WindowType) -> ast::WindowType {
    match window {
        sql::WindowType::WindowSpec(spec) => {
            ast::WindowType::WindowSpec(Box::new(convert_window_spec(spec)))
        }
        _ => ast::WindowType::WindowSpec(Box::new(ast::WindowSpec::default())),
    }
}

fn convert_window_spec(spec: sql::WindowSpec) -> ast::WindowSpec {
    ast::WindowSpec {
        partition_by: spec.partition_by.into_iter().map(convert_expr).collect(),
        order_by: spec.order_by.into_iter().map(convert_order_by).collect(),
        window_frame: spec.window_frame.map(convert_window_frame).map(Box::new),
    }
}

fn convert_window_frame(frame: sql::WindowFrame) -> ast::WindowFrame {
    ast::WindowFrame {
        units: match frame.units {
            sql::WindowFrameUnits::Rows => ast::WindowFrameUnits::Rows,
            sql::WindowFrameUnits::Range => ast::WindowFrameUnits::Range,
            _ => ast::WindowFrameUnits::Rows,
        },
        start_bound: convert_window_bound(frame.start_bound),
        end_bound: frame.end_bound.map(convert_window_bound),
    }
}

fn convert_window_bound(bound: sql::WindowFrameBound) -> ast::WindowFrameBound {
    match bound {
        sql::WindowFrameBound::Preceding(opt) => {
            ast::WindowFrameBound::Preceding(opt.map(|expr| Box::new(convert_expr(*expr))))
        }
        sql::WindowFrameBound::Following(opt) => {
            ast::WindowFrameBound::Following(opt.map(|expr| Box::new(convert_expr(*expr))))
        }
        sql::WindowFrameBound::CurrentRow => ast::WindowFrameBound::CurrentRow,
    }
}

fn convert_interval(interval: sql::Interval) -> ast::Interval {
    ast::Interval {
        value: Box::new(convert_expr(*interval.value)),
        leading_field: interval.leading_field.map(convert_datetime_field),
    }
}

fn convert_datetime_field(field: sql::DateTimeField) -> ast::DateTimeField {
    match field {
        sql::DateTimeField::Year => ast::DateTimeField::Year,
        sql::DateTimeField::Month => ast::DateTimeField::Month,
        sql::DateTimeField::Week => ast::DateTimeField::Week,
        sql::DateTimeField::Day => ast::DateTimeField::Day,
        sql::DateTimeField::Hour => ast::DateTimeField::Hour,
        sql::DateTimeField::Minute => ast::DateTimeField::Minute,
        sql::DateTimeField::Second => ast::DateTimeField::Second,
        _ => ast::DateTimeField::Day,
    }
}

fn convert_binary_op(op: sql::BinaryOperator) -> ast::BinaryOperator {
    match op {
        sql::BinaryOperator::Plus => ast::BinaryOperator::Plus,
        sql::BinaryOperator::Minus => ast::BinaryOperator::Minus,
        sql::BinaryOperator::Multiply => ast::BinaryOperator::Multiply,
        sql::BinaryOperator::Divide => ast::BinaryOperator::Divide,
        sql::BinaryOperator::Modulo => ast::BinaryOperator::Modulo,
        sql::BinaryOperator::Eq => ast::BinaryOperator::Eq,
        sql::BinaryOperator::NotEq => ast::BinaryOperator::NotEq,
        sql::BinaryOperator::Lt => ast::BinaryOperator::Lt,
        sql::BinaryOperator::LtEq => ast::BinaryOperator::LtEq,
        sql::BinaryOperator::Gt => ast::BinaryOperator::Gt,
        sql::BinaryOperator::GtEq => ast::BinaryOperator::GtEq,
        sql::BinaryOperator::And => ast::BinaryOperator::And,
        sql::BinaryOperator::Or => ast::BinaryOperator::Or,
        _ => ast::BinaryOperator::Eq,
    }
}

fn convert_unary_op(op: sql::UnaryOperator) -> ast::UnaryOperator {
    match op {
        sql::UnaryOperator::Not => ast::UnaryOperator::Not,
        sql::UnaryOperator::Minus => ast::UnaryOperator::Minus,
        sql::UnaryOperator::Plus => ast::UnaryOperator::Plus,
        _ => ast::UnaryOperator::Plus,
    }
}

fn convert_value(val: sql::Value) -> ast::Value {
    match val {
        sql::Value::Number(v, long) => ast::Value::Number(v, long),
        sql::Value::SingleQuotedString(v) => ast::Value::SingleQuotedString(v),
        sql::Value::DoubleQuotedString(v) => ast::Value::DoubleQuotedString(v),
        sql::Value::Boolean(v) => ast::Value::Boolean(v),
        sql::Value::Null => ast::Value::Null,
        sql::Value::UnQuotedString(v) => ast::Value::UnquotedString(v),
        _ => ast::Value::Null,
    }
}

fn convert_data_type(ty: sql::DataType) -> Result<ast::DataType> {
    Ok(match ty {
        sql::DataType::Boolean | sql::DataType::Bool => ast::DataType::Boolean,
        sql::DataType::Int64 | sql::DataType::BigInt(_) | sql::DataType::Int8(_) => {
            ast::DataType::Int64
        }
        sql::DataType::UnsignedBigInt(_) | sql::DataType::UnsignedInt8(_) => {
            ast::DataType::UnsignedBigInt
        }
        sql::DataType::Float64
        | sql::DataType::Double
        | sql::DataType::DoublePrecision
        | sql::DataType::Real
        | sql::DataType::Float(_)
        | sql::DataType::Float4
        | sql::DataType::Float8 => ast::DataType::Float64,
        sql::DataType::Decimal(info) | sql::DataType::Numeric(info) | sql::DataType::Dec(info) => {
            match info {
                sql::ExactNumberInfo::PrecisionAndScale(p, s) => ast::DataType::Decimal {
                    precision: p as u8,
                    scale: s as u8,
                },
                sql::ExactNumberInfo::Precision(p) => ast::DataType::Decimal {
                    precision: p as u8,
                    scale: 0,
                },
                sql::ExactNumberInfo::None => ast::DataType::Decimal {
                    precision: 18,
                    scale: 4,
                },
            }
        }
        sql::DataType::Date => ast::DataType::Date,
        sql::DataType::Datetime(_) | sql::DataType::Timestamp(_, _) => ast::DataType::DateTime,
        sql::DataType::Uuid => ast::DataType::Uuid,
        sql::DataType::Array(elem) => {
            let inner = match elem {
                sql::ArrayElemTypeDef::SquareBracket(data) => convert_data_type(*data)?,
                sql::ArrayElemTypeDef::AngleBracket(data) => convert_data_type(*data)?,
                sql::ArrayElemTypeDef::None => ast::DataType::String,
            };
            ast::DataType::Array(Box::new(inner))
        }
        sql::DataType::Custom(name, modifiers) => {
            ast::DataType::Custom(name.to_string(), modifiers)
        }
        _ => ast::DataType::String,
    })
}

fn convert_ident(ident: sql::Ident) -> ast::Ident {
    ast::Ident {
        value: ident.value,
        quote_style: ident.quote_style,
    }
}

fn convert_object_name(name: sql::ObjectName) -> ast::ObjectName {
    ast::ObjectName::new(name.0.into_iter().map(convert_ident).collect())
}

struct CreateTableClauses {
    ttl: Option<ast::Expr>,
    sample_by: Option<ast::Expr>,
    as_table: Option<sql::ObjectName>,
    engine: Option<String>,
    materialized_columns: HashMap<String, ast::Expr>,
}

fn extract_create_table_clauses_from_sql(sql: &str) -> Result<CreateTableClauses> {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens = tokenizer
        .tokenize()
        .map_err(|err| anyhow!("failed to tokenize: {err}"))?;

    let mut depth = 0i32;
    let mut ttl = None;
    let mut sample_by = None;
    let mut as_table = None;
    let mut engine = None;
    let mut materialized_columns = HashMap::new();
    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if word.value.eq_ignore_ascii_case("engine") {
                    if let Some(next_idx) = next_non_ws(&tokens, idx + 1) {
                        let mut cursor = next_idx;
                        if matches!(tokens[cursor], Token::Eq) {
                            cursor = next_non_ws(&tokens, cursor + 1).unwrap_or(cursor);
                        }
                        if let Token::Word(next_word) = &tokens[cursor] {
                            engine = Some(next_word.value.clone());
                        }
                    }
                }
                if word.value.eq_ignore_ascii_case("sample") {
                    if is_keyword_sequence(&tokens, idx, &["sample", "by"]) {
                        if let Some((start, end)) = capture_clause(&tokens, idx)? {
                            let expr_sql = tokens[start + 2..end]
                                .iter()
                                .map(|t: &Token| t.to_string())
                                .collect::<Vec<_>>()
                                .join(" ");
                            sample_by = parse_expr_from_sql(&expr_sql).ok().map(convert_expr);
                        }
                    }
                }
                if word.value.eq_ignore_ascii_case("ttl") {
                    if let Some((start, end)) = capture_clause(&tokens, idx)? {
                        let expr_sql = tokens[start + 1..end]
                            .iter()
                            .map(|t: &Token| t.to_string())
                            .collect::<Vec<_>>()
                            .join(" ");
                        ttl = parse_expr_from_sql(&expr_sql).ok().map(convert_expr);
                    }
                }
                if word.value.eq_ignore_ascii_case("as") {
                    if let Some(next_idx) = next_non_ws(&tokens, idx + 1) {
                        if let Token::Word(next_word) = &tokens[next_idx] {
                            if !next_word.value.eq_ignore_ascii_case("select") {
                                let name =
                                    sql::ObjectName(vec![sql::Ident::new(next_word.value.clone())]);
                                as_table = Some(name);
                            }
                        }
                    }
                }
            }
            Token::Word(word) if depth == 1 && word.value.eq_ignore_ascii_case("materialized") => {
                if let Some((name, expr)) = extract_materialized_column(&tokens, idx, depth) {
                    materialized_columns.insert(name.to_ascii_lowercase(), convert_expr(expr));
                }
            }
            _ => {}
        }
        idx += 1;
    }

    Ok(CreateTableClauses {
        ttl,
        sample_by,
        as_table,
        engine,
        materialized_columns,
    })
}

fn sanitize_create_table_sql(sql: &str) -> Result<String> {
    let sql = replace_keyword_case_insensitive(sql, "materialized", "DEFAULT");
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, &sql);
    let mut tokens = tokenizer
        .tokenize()
        .map_err(|err| anyhow!("failed to tokenize: {err}"))?;
    rewrite_array_types(&mut tokens)?;
    rewrite_enum_equals(&mut tokens)?;
    rewrite_materialized_column_options(&mut tokens)?;
    strip_create_table_clauses(&mut tokens)?;
    let tokens = rewrite_create_as_table(tokens);
    let sanitized = tokens
        .iter()
        .map(|t: &Token| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    Ok(sanitized)
}

fn rewrite_materialized_column_options(tokens: &mut [Token]) -> Result<()> {
    for token in tokens.iter_mut() {
        match token {
            Token::Word(word) if word.value.eq_ignore_ascii_case("materialized") => {
                *word = Word {
                    value: "DEFAULT".to_string(),
                    quote_style: None,
                    keyword: Keyword::DEFAULT,
                };
            }
            _ => {}
        }
    }
    Ok(())
}

fn rewrite_create_as_table(tokens: Vec<Token>) -> Vec<Token> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut depth = 0i32;
    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 && word.value.eq_ignore_ascii_case("as") => {
                if let Some(next_idx) = next_non_ws(&tokens, idx + 1) {
                    if let Token::Word(next_word) = &tokens[next_idx] {
                        if !next_word.value.eq_ignore_ascii_case("select") {
                            out.push(tokens[idx].clone());
                            out.push(Token::Whitespace(Whitespace::Space));
                            out.push(Token::Word(Word {
                                value: "SELECT".to_string(),
                                quote_style: None,
                                keyword: Keyword::SELECT,
                            }));
                            out.push(Token::Whitespace(Whitespace::Space));
                            out.push(Token::Mul);
                            out.push(Token::Whitespace(Whitespace::Space));
                            out.push(Token::Word(Word {
                                value: "FROM".to_string(),
                                quote_style: None,
                                keyword: Keyword::FROM,
                            }));
                            out.push(Token::Whitespace(Whitespace::Space));
                            out.push(tokens[next_idx].clone());
                            idx = next_idx + 1;
                            continue;
                        }
                    }
                }
            }
            _ => {}
        }
        out.push(tokens[idx].clone());
        idx += 1;
    }
    out
}

fn extract_materialized_column(
    tokens: &[Token],
    materialized_idx: usize,
    depth_at_idx: i32,
) -> Option<(String, sql::Expr)> {
    let start = find_column_start(tokens, materialized_idx, depth_at_idx)?;
    let name = column_name_from_range(tokens, start, materialized_idx)?;
    let expr_start = next_non_ws(tokens, materialized_idx + 1)?;
    let end = find_column_end(tokens, expr_start, depth_at_idx);
    let expr_sql = tokens[expr_start..end]
        .iter()
        .map(|t: &Token| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let expr = parse_expr_from_sql(&expr_sql).ok()?;
    Some((name, expr))
}

fn find_column_start(tokens: &[Token], idx: usize, depth_at_idx: i32) -> Option<usize> {
    let mut depth = depth_at_idx;
    let mut pos = idx;
    while pos > 0 {
        pos -= 1;
        match &tokens[pos] {
            Token::RParen => depth += 1,
            Token::LParen => {
                if depth == depth_at_idx {
                    return Some(pos + 1);
                }
                depth -= 1;
            }
            Token::Comma if depth == depth_at_idx => return Some(pos + 1),
            _ => {}
        }
    }
    None
}

fn column_name_from_range(tokens: &[Token], start: usize, end: usize) -> Option<String> {
    for token in &tokens[start..end] {
        if let Token::Word(word) = token {
            return Some(word.value.clone());
        }
    }
    None
}

fn find_column_end(tokens: &[Token], start: usize, depth_at_start: i32) -> usize {
    let mut depth = depth_at_start;
    for (idx, token) in tokens.iter().enumerate().skip(start) {
        match token {
            Token::LParen => depth += 1,
            Token::RParen => {
                depth -= 1;
                if depth < depth_at_start {
                    return idx;
                }
            }
            Token::Comma if depth == depth_at_start => return idx,
            _ => {}
        }
    }
    tokens.len()
}

fn strip_create_table_clauses(tokens: &mut [Token]) -> Result<()> {
    let mut depth = 0i32;
    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if is_keyword_sequence(tokens, idx, &["engine", "="]) {
                    if let Some((start, end)) = capture_clause(tokens, idx)? {
                        for i in start..end {
                            tokens[i] = Token::Whitespace(Whitespace::Space);
                        }
                    }
                }
                if is_keyword_sequence(tokens, idx, &["partition", "by"])
                    || is_keyword_sequence(tokens, idx, &["primary", "key"])
                    || is_keyword_sequence(tokens, idx, &["sample", "by"])
                    || is_keyword_sequence(tokens, idx, &["ttl"])
                    || is_keyword_sequence(tokens, idx, &["settings"])
                    || is_keyword_sequence(tokens, idx, &["order", "by"])
                {
                    if let Some((start, end)) = capture_clause(tokens, idx)? {
                        for i in start..end {
                            tokens[i] = Token::Whitespace(Whitespace::Space);
                        }
                    }
                }
            }
            _ => {}
        }
        idx += 1;
    }
    Ok(())
}

fn rewrite_array_types(tokens: &mut [Token]) -> Result<()> {
    let mut idx = 0usize;
    while idx < tokens.len() {
        if let Token::Word(word) = &tokens[idx] {
            if word.value.eq_ignore_ascii_case("array") {
                if let Some(open_idx) = next_non_ws(tokens, idx + 1) {
                    if matches!(tokens[open_idx], Token::LParen) {
                        let close_idx = find_matching_paren(tokens, open_idx)?;
                        tokens[idx] = Token::Word(Word {
                            value: "ARRAY".to_string(),
                            quote_style: None,
                            keyword: Keyword::ARRAY,
                        });
                        tokens[open_idx] = Token::Lt;
                        tokens[close_idx] = Token::Gt;
                        idx = close_idx + 1;
                        continue;
                    }
                }
            }
        }
        idx += 1;
    }
    Ok(())
}

fn rewrite_enum_equals(tokens: &mut [Token]) -> Result<()> {
    let mut idx = 0usize;
    while idx < tokens.len() {
        if let Token::Word(word) = &tokens[idx] {
            if word.value.eq_ignore_ascii_case("enum8") || word.value.eq_ignore_ascii_case("enum16")
            {
                if let Some(open_idx) = next_non_ws(tokens, idx + 1) {
                    if matches!(tokens[open_idx], Token::LParen) {
                        let close_idx = find_matching_paren(tokens, open_idx)?;
                        for pos in open_idx + 1..close_idx {
                            if matches!(tokens[pos], Token::Eq) {
                                tokens[pos] = Token::Whitespace(Whitespace::Space);
                            }
                        }
                        idx = close_idx + 1;
                        continue;
                    }
                }
            }
        }
        idx += 1;
    }
    Ok(())
}

fn split_format_clause(sql: &str) -> (Option<String>, String) {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens: Vec<Token> = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(_) => return (None, sql.trim().to_string()),
    };
    let mut depth = 0i32;
    let mut format_idx = None;
    let mut format_token = None;
    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if word.value.eq_ignore_ascii_case("format") {
                    let next = next_non_ws(&tokens, idx + 1);
                    if let Some(next_idx) = next {
                        if let Token::Word(format_word) = &tokens[next_idx] {
                            format_idx = Some(idx);
                            format_token = Some(format_word.value.clone());
                            break;
                        }
                    }
                }
            }
            _ => {}
        }
        idx += 1;
    }

    if let Some(start) = format_idx {
        let mut end = start + 1;
        while end < tokens.len() {
            if let Token::SemiColon = tokens[end] {
                break;
            }
            end += 1;
        }
        let mut trimmed = Vec::new();
        for (idx, token) in tokens.iter().enumerate() {
            if idx >= start && idx < end {
                continue;
            }
            trimmed.push(token.to_string());
        }
        return (format_token, trimmed.join(" "));
    }

    (None, sql.trim().to_string())
}

fn split_sample_clause(sql: &str) -> (String, Option<f64>) {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens: Vec<Token> = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(_) => return (sql.trim().to_string(), None),
    };
    let mut depth = 0i32;
    let mut sample_start = None;
    let mut sample_end = None;
    let mut ratio = None;

    let mut idx = 0usize;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if word.value.eq_ignore_ascii_case("sample") {
                    sample_start = Some(idx);
                    let mut cursor = idx + 1;
                    while cursor < tokens.len() {
                        if let Token::Word(next_word) = &tokens[cursor] {
                            if is_select_clause_start(next_word) {
                                break;
                            }
                        }
                        if let Token::Number(num, _) = &tokens[cursor] {
                            ratio = parse_sample_ratio(num);
                            sample_end = Some(cursor + 1);
                        }
                        cursor += 1;
                    }
                    sample_end = sample_end.or(Some(cursor));
                    break;
                }
            }
            _ => {}
        }
        idx += 1;
    }

    if let (Some(start), Some(end)) = (sample_start, sample_end) {
        let mut trimmed = Vec::new();
        for (idx, token) in tokens.iter().enumerate() {
            if idx >= start && idx < end {
                continue;
            }
            trimmed.push(token.to_string());
        }
        return (trimmed.join(" "), ratio);
    }

    (sql.trim().to_string(), None)
}

fn rewrite_insert_columns_with_dots(sql: &str) -> String {
    let mut tokenizer = Tokenizer::new(&ClickHouseDialect {}, sql);
    let tokens: Vec<Token> = match tokenizer.tokenize() {
        Ok(tokens) => tokens,
        Err(_) => return rewrite_insert_columns_with_dots_fallback(sql),
    };

    let mut output = Vec::new();
    let mut idx = 0usize;
    let mut in_insert_columns = false;
    let mut paren_depth = 0i32;

    while idx < tokens.len() {
        let token = &tokens[idx];
        if is_insert_start(&tokens, idx) {
            in_insert_columns = true;
        }

        if in_insert_columns {
            match token {
                Token::LParen => {
                    paren_depth += 1;
                    output.push(token.clone());
                    idx += 1;
                    continue;
                }
                Token::RParen => {
                    paren_depth -= 1;
                    output.push(token.clone());
                    if paren_depth == 0 {
                        in_insert_columns = false;
                    }
                    idx += 1;
                    continue;
                }
                Token::Word(word) if paren_depth == 1 => {
                    let next_idx = next_non_ws(&tokens, idx + 1);
                    let last_idx = next_idx.and_then(|i| next_non_ws(&tokens, i + 1));
                    if let (Some(dot_idx), Some(name_idx)) = (next_idx, last_idx) {
                        if matches!(tokens[dot_idx], Token::Period) {
                            if let Token::Word(next_word) = &tokens[name_idx] {
                                let combined = format!("{}.{}", word.value, next_word.value);
                                output.push(Token::Word(Word {
                                    value: combined,
                                    quote_style: Some('"'),
                                    keyword: Keyword::NoKeyword,
                                }));
                                idx = name_idx + 1;
                                continue;
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        output.push(token.clone());
        idx += 1;
    }

    output
        .iter()
        .map(|t: &Token| t.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

fn rewrite_insert_columns_with_dots_fallback(sql: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    let insert_idx = match lower.find("insert") {
        Some(idx) => idx,
        None => return sql.to_string(),
    };
    let into_idx = match lower[insert_idx..].find("into") {
        Some(idx) => insert_idx + idx,
        None => return sql.to_string(),
    };
    let open_idx = match lower[into_idx..].find('(') {
        Some(idx) => into_idx + idx,
        None => return sql.to_string(),
    };

    let mut depth = 0i32;
    let mut close_idx = None;
    let mut in_single = false;
    let mut in_double = false;
    let mut in_backtick = false;
    for (offset, ch) in sql[open_idx..].char_indices() {
        match ch {
            '\'' if !in_double && !in_backtick => in_single = !in_single,
            '"' if !in_single && !in_backtick => in_double = !in_double,
            '`' if !in_single && !in_double => in_backtick = !in_backtick,
            '(' if !(in_single || in_double || in_backtick) => {
                depth += 1;
            }
            ')' if !(in_single || in_double || in_backtick) => {
                depth -= 1;
                if depth == 0 {
                    close_idx = Some(open_idx + offset);
                    break;
                }
            }
            _ => {}
        }
    }
    let Some(close_idx) = close_idx else {
        return sql.to_string();
    };

    let cols = &sql[open_idx + 1..close_idx];
    let mut rewritten_cols = Vec::new();
    for part in cols.split(',') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }
        let rewritten = if trimmed.starts_with('"')
            || trimmed.starts_with('`')
            || trimmed.starts_with('\'')
            || !trimmed.contains('.')
        {
            trimmed.to_string()
        } else {
            format!("\"{}\"", trimmed)
        };
        rewritten_cols.push(rewritten);
    }

    let mut out = String::new();
    out.push_str(&sql[..open_idx + 1]);
    out.push_str(&rewritten_cols.join(", "));
    out.push_str(&sql[close_idx..]);
    out
}

fn is_insert_start(tokens: &[Token], idx: usize) -> bool {
    match tokens.get(idx) {
        Some(Token::Word(word)) if word.value.eq_ignore_ascii_case("insert") => {
            if let Some(Token::Word(next)) =
                next_non_ws(tokens, idx + 1).and_then(|i| tokens.get(i))
            {
                next.value.eq_ignore_ascii_case("into")
            } else {
                false
            }
        }
        _ => false,
    }
}

fn is_select_clause_start(word: &Word) -> bool {
    matches!(
        word.value.to_ascii_lowercase().as_str(),
        "where" | "group" | "order" | "limit" | "format" | "settings" | "union"
    )
}

fn parse_sample_ratio(text: &str) -> Option<f64> {
    let trimmed = text.trim();
    if let Some(value) = trimmed.strip_suffix('%') {
        return value.parse::<f64>().ok().map(|v| v / 100.0);
    }
    trimmed.parse::<f64>().ok()
}

fn parse_expr_from_sql(expr_sql: &str) -> Result<sql::Expr> {
    let query = format!("SELECT {expr_sql}");
    let mut stmts = Parser::parse_sql(&ClickHouseDialect {}, &query)
        .map_err(|err| anyhow!("failed to parse expr: {err}"))?;
    let stmt = stmts.remove(0);
    if let sql::Statement::Query(query) = stmt {
        if let sql::SetExpr::Select(select) = *query.body {
            if let Some(item) = select.projection.first() {
                if let sql::SelectItem::UnnamedExpr(expr) = item {
                    return Ok(expr.clone());
                }
            }
        }
    }
    Err(anyhow!("failed to parse expression"))
}

fn is_keyword_sequence(tokens: &[Token], start: usize, seq: &[&str]) -> bool {
    let mut idx = start;
    for keyword in seq {
        let Some(next_idx) = next_non_ws(tokens, idx) else {
            return false;
        };
        match &tokens[next_idx] {
            Token::Word(word) if word.value.eq_ignore_ascii_case(keyword) => {
                idx = next_idx + 1;
            }
            Token::Eq if *keyword == "=" => idx = next_idx + 1,
            _ => return false,
        }
    }
    true
}

fn next_non_ws(tokens: &[Token], start: usize) -> Option<usize> {
    let mut idx = start;
    while idx < tokens.len() {
        match tokens[idx] {
            Token::Whitespace(_) => idx += 1,
            _ => return Some(idx),
        }
    }
    None
}

fn find_matching_paren(tokens: &[Token], start: usize) -> Result<usize> {
    let mut depth = 0i32;
    for (idx, token) in tokens.iter().enumerate().skip(start) {
        match token {
            Token::LParen => depth += 1,
            Token::RParen => {
                depth -= 1;
                if depth == 0 {
                    return Ok(idx);
                }
            }
            _ => {}
        }
    }
    Err(anyhow!("unbalanced parentheses"))
}

fn capture_clause(tokens: &[Token], start_idx: usize) -> Result<Option<(usize, usize)>> {
    let mut depth = 0i32;
    let mut idx = start_idx;
    while idx < tokens.len() {
        match &tokens[idx] {
            Token::LParen => depth += 1,
            Token::RParen => depth -= 1,
            Token::Word(word) if depth == 0 => {
                if idx > start_idx && is_clause_start(word) {
                    return Ok(Some((start_idx, idx)));
                }
            }
            _ => {}
        }
        idx += 1;
    }
    Ok(Some((start_idx, tokens.len())))
}

fn is_clause_start(word: &Word) -> bool {
    matches!(
        word.value.to_ascii_lowercase().as_str(),
        "partition" | "primary" | "sample" | "ttl" | "settings" | "order" | "as"
    )
}
