use crate::sql_ast;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryOrigin {
    Sql,
    Prql,
    Fp,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryCoverage {
    LegacyOnly,
    Dual,
    StructuredOnly,
    Partial,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryFallback {
    None,
    CachedSqlAst,
    StructuredToSqlAst,
    PrqlToSql,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QueryBridge {
    pub origin: Option<QueryOrigin>,
    pub coverage: Option<QueryCoverage>,
    pub fallback: Option<QueryFallback>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QueryIrDocument {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub statements: Vec<QueryIrStmt>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryIrStmt {
    Query(QueryExpr),
    Insert(QueryInsert),
    Update(QueryUpdate),
    Delete(QueryDelete),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub struct QueryExpr {
    pub body: QuerySetExpr,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub order_by: Vec<sql_ast::OrderByExpr>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub limit: Option<sql_ast::Expr>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub offset: Option<sql_ast::Expr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QuerySetExpr {
    Select(QuerySelect),
    Values(QueryValues),
    SetOperation {
        left: Box<QuerySetExpr>,
        right: Box<QuerySetExpr>,
        op: QuerySetOperator,
        quantifier: QuerySetQuantifier,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QueryValues {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rows: Vec<Vec<sql_ast::Expr>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QuerySetOperator {
    Union,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QuerySetQuantifier {
    All,
    Distinct,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QuerySelect {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub projection: Vec<QuerySelectItem>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub from: Vec<QueryFrom>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub selection: Option<sql_ast::Expr>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub group_by: Vec<sql_ast::Expr>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub having: Option<sql_ast::Expr>,
    #[serde(default)]
    pub distinct: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QuerySelectItem {
    UnnamedExpr(sql_ast::Expr),
    ExprWithAlias {
        expr: sql_ast::Expr,
        alias: sql_ast::Ident,
    },
    Wildcard,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub struct QueryFrom {
    pub relation: QueryRelation,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub joins: Vec<QueryJoin>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryRelation {
    Table {
        name: sql_ast::ObjectName,
        alias: Option<sql_ast::Ident>,
    },
    Derived {
        subquery: Box<QueryExpr>,
        alias: Option<sql_ast::Ident>,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub struct QueryJoin {
    pub relation: QueryRelation,
    pub kind: QueryJoinKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub on: Option<sql_ast::Expr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryJoinKind {
    Inner,
    LeftOuter,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub struct QueryInsert {
    pub table_name: sql_ast::ObjectName,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub columns: Vec<sql_ast::Ident>,
    pub source: QueryInsertSource,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum QueryInsertSource {
    Values(QueryValues),
    Query(Box<QueryExpr>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QueryUpdate {
    pub table: sql_ast::ObjectName,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub assignments: Vec<sql_ast::Assignment>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub selection: Option<sql_ast::Expr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Default, Hash)]
pub struct QueryDelete {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub from: Vec<QueryFrom>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub selection: Option<sql_ast::Expr>,
}
