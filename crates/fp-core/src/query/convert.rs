use crate::query::semantic::{
    QueryDelete, QueryExpr, QueryFrom, QueryInsert, QueryInsertSource, QueryIrStmt, QueryJoin,
    QueryJoinKind, QueryRelation, QuerySelect, QuerySelectItem, QuerySetExpr, QuerySetOperator,
    QuerySetQuantifier, QueryUpdate, QueryValues,
};
use crate::sql_ast;

pub fn query_ir_to_statement(stmt: &QueryIrStmt) -> Option<sql_ast::Statement> {
    match stmt {
        QueryIrStmt::Query(query) => Some(sql_ast::Statement::Query(Box::new(query_ir_to_query(
            query,
        )))),
        QueryIrStmt::Insert(insert) => Some(sql_ast::Statement::Insert {
            table_name: insert.table_name.clone(),
            columns: insert.columns.clone(),
            source: Some(Box::new(match &insert.source {
                QueryInsertSource::Values(values) => sql_ast::Query {
                    body: Box::new(sql_ast::SetExpr::Values(sql_ast::Values {
                        rows: values.rows.clone(),
                    })),
                    order_by: Vec::new(),
                    limit: None,
                    offset: None,
                    format: None,
                    sample_ratio: None,
                },
                QueryInsertSource::Query(query) => query_ir_to_query(query),
            })),
        }),
        QueryIrStmt::Update(update) => Some(sql_ast::Statement::Update {
            table: update.table.clone(),
            assignments: update.assignments.clone(),
            selection: update.selection.clone(),
        }),
        QueryIrStmt::Delete(delete) => Some(sql_ast::Statement::Delete {
            from: delete
                .from
                .iter()
                .map(query_from_to_table_with_joins)
                .collect(),
            selection: delete.selection.clone(),
        }),
    }
}

pub fn statement_to_query_ir(stmt: &sql_ast::Statement) -> Option<QueryIrStmt> {
    match stmt {
        sql_ast::Statement::Query(query) => Some(QueryIrStmt::Query(query_to_query_ir(query))),
        sql_ast::Statement::Insert {
            table_name,
            columns,
            source,
        } => {
            let source = match source.as_deref() {
                Some(query) => match query.body.as_ref() {
                    sql_ast::SetExpr::Values(values) => QueryInsertSource::Values(QueryValues {
                        rows: values.rows.clone(),
                    }),
                    _ => QueryInsertSource::Query(Box::new(query_to_query_ir(query))),
                },
                None => QueryInsertSource::Values(QueryValues::default()),
            };
            Some(QueryIrStmt::Insert(QueryInsert {
                table_name: table_name.clone(),
                columns: columns.clone(),
                source,
            }))
        }
        sql_ast::Statement::Update {
            table,
            assignments,
            selection,
        } => Some(QueryIrStmt::Update(QueryUpdate {
            table: table.clone(),
            assignments: assignments.clone(),
            selection: selection.clone(),
        })),
        sql_ast::Statement::Delete { from, selection } => Some(QueryIrStmt::Delete(QueryDelete {
            from: from.iter().map(table_with_joins_to_query_from).collect(),
            selection: selection.clone(),
        })),
        _ => None,
    }
}

pub fn query_ir_to_query(query: &QueryExpr) -> sql_ast::Query {
    sql_ast::Query {
        body: Box::new(query_set_expr_to_sql(&query.body)),
        order_by: query.order_by.clone(),
        limit: query.limit.clone(),
        offset: query.offset.clone(),
        format: None,
        sample_ratio: None,
    }
}

pub fn query_to_query_ir(query: &sql_ast::Query) -> QueryExpr {
    QueryExpr {
        body: sql_set_expr_to_query(query.body.as_ref()),
        order_by: query.order_by.clone(),
        limit: query.limit.clone(),
        offset: query.offset.clone(),
    }
}

fn query_set_expr_to_sql(expr: &QuerySetExpr) -> sql_ast::SetExpr {
    match expr {
        QuerySetExpr::Select(select) => sql_ast::SetExpr::Select(Box::new(sql_ast::Select {
            projection: select
                .projection
                .iter()
                .map(query_select_item_to_sql)
                .collect(),
            from: select
                .from
                .iter()
                .map(query_from_to_table_with_joins)
                .collect(),
            selection: select.selection.clone(),
            group_by: sql_ast::GroupByExpr::Expressions(select.group_by.clone()),
            having: select.having.clone(),
            distinct: select.distinct.then_some(sql_ast::Distinct),
        })),
        QuerySetExpr::Values(values) => sql_ast::SetExpr::Values(sql_ast::Values {
            rows: values.rows.clone(),
        }),
        QuerySetExpr::SetOperation {
            left,
            right,
            op,
            quantifier,
        } => sql_ast::SetExpr::SetOperation {
            left: Box::new(query_set_expr_to_sql(left)),
            right: Box::new(query_set_expr_to_sql(right)),
            op: match op {
                QuerySetOperator::Union => sql_ast::SetOperator::Union,
            },
            set_quantifier: match quantifier {
                QuerySetQuantifier::All => sql_ast::SetQuantifier::All,
                QuerySetQuantifier::Distinct => sql_ast::SetQuantifier::Distinct,
            },
        },
    }
}

fn sql_set_expr_to_query(expr: &sql_ast::SetExpr) -> QuerySetExpr {
    match expr {
        sql_ast::SetExpr::Select(select) => QuerySetExpr::Select(QuerySelect {
            projection: select
                .projection
                .iter()
                .map(sql_select_item_to_query)
                .collect(),
            from: select
                .from
                .iter()
                .map(table_with_joins_to_query_from)
                .collect(),
            selection: select.selection.clone(),
            group_by: match &select.group_by {
                sql_ast::GroupByExpr::Expressions(exprs) => exprs.clone(),
            },
            having: select.having.clone(),
            distinct: select.distinct.is_some(),
        }),
        sql_ast::SetExpr::Values(values) => QuerySetExpr::Values(QueryValues {
            rows: values.rows.clone(),
        }),
        sql_ast::SetExpr::SetOperation {
            left,
            right,
            op,
            set_quantifier,
        } => QuerySetExpr::SetOperation {
            left: Box::new(sql_set_expr_to_query(left)),
            right: Box::new(sql_set_expr_to_query(right)),
            op: match op {
                sql_ast::SetOperator::Union => QuerySetOperator::Union,
            },
            quantifier: match set_quantifier {
                sql_ast::SetQuantifier::All => QuerySetQuantifier::All,
                sql_ast::SetQuantifier::Distinct => QuerySetQuantifier::Distinct,
            },
        },
    }
}

fn query_select_item_to_sql(item: &QuerySelectItem) -> sql_ast::SelectItem {
    match item {
        QuerySelectItem::UnnamedExpr(expr) => sql_ast::SelectItem::UnnamedExpr(expr.clone()),
        QuerySelectItem::ExprWithAlias { expr, alias } => sql_ast::SelectItem::ExprWithAlias {
            expr: expr.clone(),
            alias: alias.clone(),
        },
        QuerySelectItem::Wildcard => sql_ast::SelectItem::Wildcard,
    }
}

fn sql_select_item_to_query(item: &sql_ast::SelectItem) -> QuerySelectItem {
    match item {
        sql_ast::SelectItem::UnnamedExpr(expr) => QuerySelectItem::UnnamedExpr(expr.clone()),
        sql_ast::SelectItem::ExprWithAlias { expr, alias } => QuerySelectItem::ExprWithAlias {
            expr: expr.clone(),
            alias: alias.clone(),
        },
        sql_ast::SelectItem::Wildcard => QuerySelectItem::Wildcard,
    }
}

fn query_from_to_table_with_joins(from: &QueryFrom) -> sql_ast::TableWithJoins {
    sql_ast::TableWithJoins {
        relation: query_relation_to_table_factor(&from.relation),
        joins: from.joins.iter().map(query_join_to_sql).collect(),
    }
}

fn table_with_joins_to_query_from(from: &sql_ast::TableWithJoins) -> QueryFrom {
    QueryFrom {
        relation: table_factor_to_query_relation(&from.relation),
        joins: from.joins.iter().map(sql_join_to_query).collect(),
    }
}

fn query_relation_to_table_factor(relation: &QueryRelation) -> sql_ast::TableFactor {
    match relation {
        QueryRelation::Table { name, alias } => sql_ast::TableFactor::Table {
            name: name.clone(),
            alias: alias.clone().map(|name| sql_ast::TableAlias { name }),
        },
        QueryRelation::Derived { subquery, alias } => sql_ast::TableFactor::Derived {
            subquery: Box::new(query_ir_to_query(subquery)),
            alias: alias.clone().map(|name| sql_ast::TableAlias { name }),
        },
    }
}

fn table_factor_to_query_relation(relation: &sql_ast::TableFactor) -> QueryRelation {
    match relation {
        sql_ast::TableFactor::Table { name, alias } => QueryRelation::Table {
            name: name.clone(),
            alias: alias.as_ref().map(|alias| alias.name.clone()),
        },
        sql_ast::TableFactor::Derived { subquery, alias } => QueryRelation::Derived {
            subquery: Box::new(query_to_query_ir(subquery)),
            alias: alias.as_ref().map(|alias| alias.name.clone()),
        },
    }
}

fn query_join_to_sql(join: &QueryJoin) -> sql_ast::Join {
    sql_ast::Join {
        relation: query_relation_to_table_factor(&join.relation),
        join_operator: match join.kind {
            QueryJoinKind::Inner => sql_ast::JoinOperator::Inner(
                join.on
                    .clone()
                    .map(sql_ast::JoinConstraint::On)
                    .unwrap_or(sql_ast::JoinConstraint::None),
            ),
            QueryJoinKind::LeftOuter => sql_ast::JoinOperator::LeftOuter(
                join.on
                    .clone()
                    .map(sql_ast::JoinConstraint::On)
                    .unwrap_or(sql_ast::JoinConstraint::None),
            ),
        },
    }
}

fn sql_join_to_query(join: &sql_ast::Join) -> QueryJoin {
    let (kind, on) = match &join.join_operator {
        sql_ast::JoinOperator::Inner(constraint) => {
            (QueryJoinKind::Inner, join_constraint_to_expr(constraint))
        }
        sql_ast::JoinOperator::LeftOuter(constraint) => (
            QueryJoinKind::LeftOuter,
            join_constraint_to_expr(constraint),
        ),
    };
    QueryJoin {
        relation: table_factor_to_query_relation(&join.relation),
        kind,
        on,
    }
}

fn join_constraint_to_expr(constraint: &sql_ast::JoinConstraint) -> Option<sql_ast::Expr> {
    match constraint {
        sql_ast::JoinConstraint::On(expr) => Some(expr.clone()),
        sql_ast::JoinConstraint::None => None,
    }
}
