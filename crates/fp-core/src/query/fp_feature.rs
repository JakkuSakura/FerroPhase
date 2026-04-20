use std::path::Path;

use crate::ast::{
    Expr, ExprField, ExprInvoke, ExprInvokeTarget, ExprKind, ExprSelect, File, Name, Node,
    NodeKind, Value,
};
use crate::ops::{BinOpKind, UnOpKind};
use crate::query::{
    QueryDelete, QueryDocument, QueryExpr, QueryFrom, QueryInsert, QueryInsertSource,
    QueryIrDocument, QueryIrStmt, QueryOrigin, QueryRelation, QuerySelect, QuerySelectItem,
    QuerySetExpr, QueryUpdate, QueryValues,
};
use crate::sql_ast::{
    self, Assignment, BinaryOperator, Expr as SqlExpr, Ident, ObjectName, OrderByExpr,
    UnaryOperator, Value as SqlValue,
};

pub fn lower_fp_expr_to_query(expr: &Expr, path: Option<&Path>) -> Option<QueryDocument> {
    let statement = lower_query_statement(expr)?;
    let name = query_name(path);
    let semantic = QueryIrDocument {
        name: name.clone(),
        statements: vec![statement],
    };
    let mut document = QueryDocument::from_semantic(semantic, QueryOrigin::Fp);
    if let Some(name) = name {
        document = document.with_name(name);
    }
    Some(document)
}

pub fn lower_fp_file_to_query(file: &File, path: Option<&Path>) -> Option<QueryDocument> {
    if !file.attrs.is_empty() || file.items.len() != 1 {
        return None;
    }
    lower_fp_expr_to_query(file.items[0].as_expr()?, path)
}

pub fn lower_fp_node_to_query(node: &Node, path: Option<&Path>) -> Option<QueryDocument> {
    match node.kind() {
        NodeKind::Expr(expr) => lower_fp_expr_to_query(expr, path),
        NodeKind::File(file) => {
            let fallback_path = (file.path.as_os_str() != "<expr>").then_some(file.path.as_path());
            lower_fp_file_to_query(file, path.or(fallback_path))
        }
        _ => None,
    }
}

pub fn promote_fp_query_node(node: &mut Node, path: Option<&Path>) -> bool {
    let Some(query) = lower_fp_node_to_query(node, path) else {
        return false;
    };
    *node = Node::query(query);
    true
}

#[derive(Default)]
struct QueryPipeline {
    from: Option<QueryRelation>,
    filters: Vec<SqlExpr>,
    projection: Option<Vec<QuerySelectItem>>,
    order_by: Vec<OrderByExpr>,
    limit: Option<SqlExpr>,
    offset: Option<SqlExpr>,
    distinct: bool,
    terminal: Option<QueryTerminal>,
}

enum QueryTerminal {
    Insert {
        source: QueryInsertSource,
        columns: Vec<Ident>,
    },
    Update(Vec<Assignment>),
    Delete,
}

fn lower_query_statement(expr: &Expr) -> Option<QueryIrStmt> {
    let mut pipeline = QueryPipeline::default();
    collect_query_steps(expr, &mut pipeline)?;
    pipeline.finish()
}

impl QueryPipeline {
    fn finish(self) -> Option<QueryIrStmt> {
        let from = self.from?;
        let selection = fold_and(self.filters);
        let query = QueryExpr {
            body: QuerySetExpr::Select(QuerySelect {
                projection: self
                    .projection
                    .unwrap_or_else(|| vec![QuerySelectItem::Wildcard]),
                from: vec![QueryFrom {
                    relation: from.clone(),
                    joins: Vec::new(),
                }],
                selection,
                group_by: Vec::new(),
                having: None,
                distinct: self.distinct,
            }),
            order_by: self.order_by,
            limit: self.limit,
            offset: self.offset,
        };

        match self.terminal {
            None => Some(QueryIrStmt::Query(query)),
            Some(QueryTerminal::Insert { source, columns }) => {
                Some(QueryIrStmt::Insert(QueryInsert {
                    table_name: relation_table_name(&from)?,
                    columns,
                    source,
                }))
            }
            Some(QueryTerminal::Update(assignments)) => Some(QueryIrStmt::Update(QueryUpdate {
                table: relation_table_name(&from)?,
                assignments,
                selection: query_selection(&query),
            })),
            Some(QueryTerminal::Delete) => Some(QueryIrStmt::Delete(QueryDelete {
                from: vec![QueryFrom {
                    relation: from,
                    joins: Vec::new(),
                }],
                selection: query_selection(&query),
            })),
        }
    }
}

fn collect_query_steps(expr: &Expr, pipeline: &mut QueryPipeline) -> Option<()> {
    let ExprKind::Invoke(invoke) = expr.kind() else {
        return None;
    };
    match &invoke.target {
        ExprInvokeTarget::Function(name) => lower_root_invoke(name, invoke, pipeline),
        ExprInvokeTarget::Method(select) => {
            collect_query_steps(select.obj.as_ref(), pipeline)?;
            lower_method_invoke(select, invoke, pipeline)
        }
        _ => None,
    }
}

fn lower_root_invoke(name: &Name, invoke: &ExprInvoke, pipeline: &mut QueryPipeline) -> Option<()> {
    if !invoke.kwargs.is_empty() || invoke.args.len() != 1 || name_tail(name) != "from" {
        return None;
    }
    pipeline.from = Some(relation_from_expr(&invoke.args[0])?);
    Some(())
}

fn lower_method_invoke(
    select: &ExprSelect,
    invoke: &ExprInvoke,
    pipeline: &mut QueryPipeline,
) -> Option<()> {
    if !invoke.kwargs.is_empty() {
        return None;
    }

    match select.field.as_str() {
        "filter" | "where" if invoke.args.len() == 1 => {
            pipeline
                .filters
                .push(sql_expr(callable_expr(&invoke.args[0])?)?);
            Some(())
        }
        "select" | "map" => {
            pipeline.projection = Some(projection_items(&invoke.args)?);
            Some(())
        }
        "take" | "limit" if invoke.args.len() == 1 => {
            pipeline.limit = Some(sql_expr(callable_expr(&invoke.args[0])?)?);
            Some(())
        }
        "skip" | "offset" if invoke.args.len() == 1 => {
            pipeline.offset = Some(sql_expr(callable_expr(&invoke.args[0])?)?);
            Some(())
        }
        "sort" | "sort_by" | "order_by" => {
            pipeline.order_by = order_items(&invoke.args)?;
            Some(())
        }
        "distinct" if invoke.args.is_empty() => {
            pipeline.distinct = true;
            Some(())
        }
        "insert" if invoke.args.len() == 1 => {
            let (source, columns) = insert_source(&invoke.args[0])?;
            pipeline.terminal = Some(QueryTerminal::Insert { source, columns });
            Some(())
        }
        "update" if invoke.args.len() == 1 => {
            pipeline.terminal = Some(QueryTerminal::Update(update_assignments(&invoke.args[0])?));
            Some(())
        }
        "delete" if invoke.args.is_empty() => {
            pipeline.terminal = Some(QueryTerminal::Delete);
            Some(())
        }
        _ => None,
    }
}

fn projection_items(args: &[Expr]) -> Option<Vec<QuerySelectItem>> {
    if args.len() != 1 {
        return args
            .iter()
            .map(|expr| Some(QuerySelectItem::UnnamedExpr(sql_expr(expr)?)))
            .collect();
    }
    let expr = projection_source(args)?;
    match expr.kind() {
        ExprKind::Structural(structural) => structural
            .fields
            .iter()
            .map(projection_item_from_field)
            .collect(),
        ExprKind::Tuple(tuple) => tuple
            .values
            .iter()
            .map(|expr| Some(QuerySelectItem::UnnamedExpr(sql_expr(expr)?)))
            .collect(),
        _ => Some(vec![QuerySelectItem::UnnamedExpr(sql_expr(expr)?)]),
    }
}

fn projection_item_from_field(field: &ExprField) -> Option<QuerySelectItem> {
    match &field.value {
        Some(value) => Some(QuerySelectItem::ExprWithAlias {
            expr: sql_expr(value)?,
            alias: sql_ident(field.name.as_str()),
        }),
        None => Some(QuerySelectItem::UnnamedExpr(SqlExpr::Identifier(
            sql_ident(field.name.as_str()),
        ))),
    }
}

fn order_items(args: &[Expr]) -> Option<Vec<OrderByExpr>> {
    if args.len() != 1 {
        return args.iter().map(order_expr).collect();
    }
    let source = projection_source(args)?;
    match source.kind() {
        ExprKind::Tuple(tuple) => tuple.values.iter().map(order_expr).collect(),
        _ => Some(vec![order_expr(source)?]),
    }
}

fn order_expr(expr: &Expr) -> Option<OrderByExpr> {
    let expr = callable_expr(expr)?;
    if let ExprKind::Invoke(invoke) = expr.kind() {
        if invoke.kwargs.is_empty() && invoke.args.len() == 1 {
            if let ExprInvokeTarget::Function(name) = &invoke.target {
                match name_tail(name) {
                    "asc" => {
                        return Some(OrderByExpr {
                            expr: sql_expr(callable_expr(&invoke.args[0])?)?,
                            asc: Some(true),
                        });
                    }
                    "desc" => {
                        return Some(OrderByExpr {
                            expr: sql_expr(callable_expr(&invoke.args[0])?)?,
                            asc: Some(false),
                        });
                    }
                    _ => {}
                }
            }
        }
    }
    Some(OrderByExpr {
        expr: sql_expr(expr)?,
        asc: None,
    })
}

fn update_assignments(expr: &Expr) -> Option<Vec<Assignment>> {
    let expr = callable_expr(expr)?;
    let ExprKind::Structural(structural) = expr.kind() else {
        return None;
    };
    structural
        .fields
        .iter()
        .map(|field| {
            let value = field.value.as_ref()?;
            Some(Assignment {
                id: vec![sql_ident(field.name.as_str())],
                value: sql_expr(value)?,
            })
        })
        .collect()
}

fn insert_source(expr: &Expr) -> Option<(QueryInsertSource, Vec<Ident>)> {
    if let Some(QueryIrStmt::Query(query)) = lower_query_statement(expr) {
        return Some((QueryInsertSource::Query(Box::new(query)), Vec::new()));
    }

    let expr = callable_expr(expr)?;
    let columns = insert_columns(expr);
    let rows = insert_rows(expr)?;
    Some((QueryInsertSource::Values(QueryValues { rows }), columns))
}

fn insert_rows(expr: &Expr) -> Option<Vec<Vec<SqlExpr>>> {
    match expr.kind() {
        ExprKind::Array(array) => {
            if array.values.is_empty() {
                return Some(Vec::new());
            }
            if matches!(array.values[0].kind(), ExprKind::Structural(_)) {
                let columns = structural_columns(&array.values[0])?;
                array
                    .values
                    .iter()
                    .map(|row| structural_row(row, &columns))
                    .collect()
            } else {
                Some(vec![array
                    .values
                    .iter()
                    .map(sql_expr)
                    .collect::<Option<Vec<_>>>()?])
            }
        }
        ExprKind::Structural(_) => {
            let columns = structural_columns(expr)?;
            Some(vec![structural_row(expr, &columns)?])
        }
        ExprKind::Tuple(tuple) => Some(vec![tuple
            .values
            .iter()
            .map(sql_expr)
            .collect::<Option<Vec<_>>>()?]),
        _ => Some(vec![vec![sql_expr(expr)?]]),
    }
}

fn structural_columns(expr: &Expr) -> Option<Vec<String>> {
    let ExprKind::Structural(structural) = expr.kind() else {
        return None;
    };
    Some(
        structural
            .fields
            .iter()
            .map(|field| field.name.as_str().to_string())
            .collect(),
    )
}

fn structural_row(expr: &Expr, columns: &[String]) -> Option<Vec<SqlExpr>> {
    let ExprKind::Structural(structural) = expr.kind() else {
        return None;
    };
    let mut row = Vec::with_capacity(columns.len());
    for column in columns {
        let field = structural
            .fields
            .iter()
            .find(|field| field.name.as_str() == column.as_str())?;
        row.push(sql_expr(field.value.as_ref()?)?);
    }
    Some(row)
}

fn insert_columns(expr: &Expr) -> Vec<Ident> {
    let names = match expr.kind() {
        ExprKind::Array(array) if !array.values.is_empty() => structural_columns(&array.values[0]),
        _ => structural_columns(expr),
    };
    names
        .unwrap_or_default()
        .into_iter()
        .map(|name| sql_ident(&name))
        .collect()
}

fn relation_from_expr(expr: &Expr) -> Option<QueryRelation> {
    match expr.kind() {
        ExprKind::Name(name) => Some(QueryRelation::Table {
            name: object_name_from_name(name),
            alias: None,
        }),
        ExprKind::Paren(paren) => relation_from_expr(paren.expr.as_ref()),
        ExprKind::Value(value) => match &**value {
            Value::String(value) => Some(QueryRelation::Table {
                name: object_name_from_string(&value.value),
                alias: None,
            }),
            _ => None,
        },
        _ => None,
    }
}

fn relation_table_name(relation: &QueryRelation) -> Option<ObjectName> {
    match relation {
        QueryRelation::Table { name, .. } => Some(name.clone()),
        QueryRelation::Derived { .. } => None,
    }
}

fn object_name_from_name(name: &Name) -> ObjectName {
    let path = name.to_path();
    ObjectName::new(
        path.segments
            .into_iter()
            .map(sql_ident_from_ident)
            .collect(),
    )
}

fn object_name_from_string(value: &str) -> ObjectName {
    ObjectName::new(value.split('.').map(sql_ident).collect())
}

fn callable_expr(expr: &Expr) -> Option<&Expr> {
    match expr.kind() {
        ExprKind::Closure(closure) => Some(closure.body.as_ref()),
        ExprKind::Paren(paren) => callable_expr(paren.expr.as_ref()),
        _ => Some(expr),
    }
}

fn projection_source(args: &[Expr]) -> Option<&Expr> {
    if args.len() != 1 {
        return None;
    }
    callable_expr(&args[0])
}

fn query_selection(query: &QueryExpr) -> Option<SqlExpr> {
    match &query.body {
        QuerySetExpr::Select(select) => select.selection.clone(),
        _ => None,
    }
}

fn fold_and(filters: Vec<SqlExpr>) -> Option<SqlExpr> {
    let mut iter = filters.into_iter();
    let first = iter.next()?;
    Some(iter.fold(first, |left, right| SqlExpr::BinaryOp {
        left: Box::new(left),
        op: BinaryOperator::And,
        right: Box::new(right),
    }))
}

fn sql_expr(expr: &Expr) -> Option<SqlExpr> {
    match expr.kind() {
        ExprKind::Name(name) => sql_expr_from_name(name),
        ExprKind::Select(select) => sql_expr_from_select(select),
        ExprKind::Value(value) => sql_expr_from_value(value.as_ref()),
        ExprKind::BinOp(bin) => Some(SqlExpr::BinaryOp {
            left: Box::new(sql_expr(bin.lhs.as_ref())?),
            op: sql_binop(bin.kind)?,
            right: Box::new(sql_expr(bin.rhs.as_ref())?),
        }),
        ExprKind::UnOp(unary) => Some(SqlExpr::UnaryOp {
            op: sql_unop(unary.op.clone())?,
            expr: Box::new(sql_expr(unary.val.as_ref())?),
        }),
        ExprKind::Paren(paren) => Some(SqlExpr::Nested(Box::new(sql_expr(paren.expr.as_ref())?))),
        ExprKind::Tuple(tuple) => Some(SqlExpr::Tuple(
            tuple
                .values
                .iter()
                .map(sql_expr)
                .collect::<Option<Vec<_>>>()?,
        )),
        ExprKind::Array(array) => Some(SqlExpr::Array(
            array
                .values
                .iter()
                .map(sql_expr)
                .collect::<Option<Vec<_>>>()?,
        )),
        ExprKind::Invoke(invoke) => sql_expr_from_invoke(invoke),
        ExprKind::Closure(closure) => sql_expr(closure.body.as_ref()),
        _ => None,
    }
}

fn sql_expr_from_name(name: &Name) -> Option<SqlExpr> {
    let path = name.to_path();
    if path.segments.len() == 1 {
        Some(SqlExpr::Identifier(sql_ident_from_ident(
            path.segments.first()?.clone(),
        )))
    } else {
        Some(SqlExpr::CompoundIdentifier(
            path.segments
                .into_iter()
                .map(sql_ident_from_ident)
                .collect(),
        ))
    }
}

fn sql_expr_from_select(select: &ExprSelect) -> Option<SqlExpr> {
    match select.obj.kind() {
        ExprKind::Name(name) => {
            let mut parts = object_name_from_name(name).parts;
            parts.push(sql_ident(select.field.as_str()));
            Some(SqlExpr::CompoundIdentifier(parts))
        }
        ExprKind::Select(_) => {
            let SqlExpr::CompoundIdentifier(mut parts) = sql_expr(select.obj.as_ref())? else {
                return None;
            };
            parts.push(sql_ident(select.field.as_str()));
            Some(SqlExpr::CompoundIdentifier(parts))
        }
        _ => None,
    }
}

fn sql_expr_from_value(value: &Value) -> Option<SqlExpr> {
    Some(match value {
        Value::Int(value) => SqlExpr::Value(SqlValue::Number(value.value.to_string(), false)),
        Value::UInt(value) => SqlExpr::Value(SqlValue::Number(value.value.to_string(), false)),
        Value::BigInt(value) => SqlExpr::Value(SqlValue::Number(value.value.to_string(), true)),
        Value::Bool(value) => SqlExpr::Value(SqlValue::Boolean(value.value)),
        Value::Decimal(value) => SqlExpr::Value(SqlValue::Number(value.value.to_string(), false)),
        Value::BigDecimal(value) => {
            SqlExpr::Value(SqlValue::Number(value.value.to_string(), false))
        }
        Value::String(value) => SqlExpr::Value(SqlValue::SingleQuotedString(value.value.clone())),
        Value::Null(_) => SqlExpr::Value(SqlValue::Null),
        Value::Expr(expr) => return sql_expr(expr.as_ref()),
        _ => return None,
    })
}

fn sql_expr_from_invoke(invoke: &ExprInvoke) -> Option<SqlExpr> {
    if !invoke.kwargs.is_empty() {
        return None;
    }
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        return None;
    };
    let args = invoke
        .args
        .iter()
        .map(|expr| {
            Some(sql_ast::FunctionArg::Unnamed(
                sql_ast::FunctionArgExpr::Expr(sql_expr(expr)?),
            ))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(SqlExpr::Function(sql_ast::Function {
        name: object_name_from_name(name),
        args,
        over: None,
    }))
}

fn sql_binop(kind: BinOpKind) -> Option<BinaryOperator> {
    Some(match kind {
        BinOpKind::Add => BinaryOperator::Plus,
        BinOpKind::Sub => BinaryOperator::Minus,
        BinOpKind::Mul => BinaryOperator::Multiply,
        BinOpKind::Div => BinaryOperator::Divide,
        BinOpKind::Mod => BinaryOperator::Modulo,
        BinOpKind::Eq => BinaryOperator::Eq,
        BinOpKind::Ne => BinaryOperator::NotEq,
        BinOpKind::Lt => BinaryOperator::Lt,
        BinOpKind::Le => BinaryOperator::LtEq,
        BinOpKind::Gt => BinaryOperator::Gt,
        BinOpKind::Ge => BinaryOperator::GtEq,
        BinOpKind::And => BinaryOperator::And,
        BinOpKind::Or => BinaryOperator::Or,
        _ => return None,
    })
}

fn sql_unop(kind: UnOpKind) -> Option<UnaryOperator> {
    Some(match kind {
        UnOpKind::Not => UnaryOperator::Not,
        UnOpKind::Neg => UnaryOperator::Minus,
        _ => return None,
    })
}

fn sql_ident(value: &str) -> Ident {
    Ident::new(value)
}

fn sql_ident_from_ident(ident: crate::ast::Ident) -> Ident {
    Ident::new(ident.to_string())
}

fn name_tail(name: &Name) -> &str {
    match name {
        Name::Ident(ident) => ident.as_str(),
        Name::Path(path) => path.last().as_str(),
        Name::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str()).unwrap_or(""),
    }
}

fn query_name(path: Option<&Path>) -> Option<String> {
    path.and_then(|path| path.file_name())
        .and_then(|value| value.to_str())
        .map(|value| value.to_string())
}
