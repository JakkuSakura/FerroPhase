use crate::{common_enum, common_struct};
use std::fmt::{self, Display, Formatter};

common_struct! {
    #[derive(Default)]
    pub struct Ident {
        pub value: String,
        pub quote_style: Option<char>,
    }
}

impl Ident {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            quote_style: None,
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.quote_style {
            Some(quote) => write!(f, "{quote}{}{quote}", self.value),
            None => f.write_str(&self.value),
        }
    }
}

common_struct! {
    #[derive(Default)]
    pub struct ObjectName {
        pub parts: Vec<Ident>,
    }
}

impl ObjectName {
    pub fn new(parts: Vec<Ident>) -> Self {
        Self { parts }
    }
}

impl Display for ObjectName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let joined = self
            .parts
            .iter()
            .map(|part| part.to_string())
            .collect::<Vec<_>>()
            .join(".");
        f.write_str(&joined)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum Value {
    Number(String, bool),
    SingleQuotedString(String),
    DoubleQuotedString(String),
    Boolean(bool),
    Null,
    UnquotedString(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(v, long) => write!(f, "{}{}", v, if *long { "L" } else { "" }),
            Value::SingleQuotedString(v) => write!(f, "'{}'", v),
            Value::DoubleQuotedString(v) => write!(f, "\"{}\"", v),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Null => f.write_str("NULL"),
            Value::UnquotedString(v) => f.write_str(v),
        }
    }
}

common_enum! {
    pub enum BinaryOperator {
        Plus,
        Minus,
        Multiply,
        Divide,
        Modulo,
        Eq,
        NotEq,
        Lt,
        LtEq,
        Gt,
        GtEq,
        And,
        Or,
    }
}

common_enum! {
    pub enum UnaryOperator {
        Not,
        Plus,
        Minus,
    }
}

common_enum! {
    pub enum DateTimeField {
        Year,
        Month,
        Week,
        Day,
        Hour,
        Minute,
        Second,
    }
}

common_struct! {
    pub struct Interval {
        pub value: Box<Expr>,
        pub leading_field: Option<DateTimeField>,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum Expr {
    Identifier(Ident),
    CompoundIdentifier(Vec<Ident>),
    Value(Value),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>,
    },
    Nested(Box<Expr>),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    Function(Function),
    Cast {
        expr: Box<Expr>,
        data_type: DataType,
    },
    InList {
        expr: Box<Expr>,
        list: Vec<Expr>,
        negated: bool,
    },
    InSubquery {
        expr: Box<Expr>,
        subquery: Box<Query>,
        negated: bool,
    },
    Interval(Interval),
}

common_struct! {
    pub struct Function {
        pub name: ObjectName,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub args: Vec<FunctionArg>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub over: Option<WindowType>,
    }
}

common_enum! {
    pub enum FunctionArg {
        Unnamed(FunctionArgExpr),
    }
}

common_enum! {
    pub enum FunctionArgExpr {
        Expr(Expr),
    }
}

common_struct! {
    pub struct OrderByExpr {
        pub expr: Expr,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub asc: Option<bool>,
    }
}

common_enum! {
    pub enum WindowType {
        WindowSpec(Box<WindowSpec>),
    }
}

common_struct! {
    #[derive(Default)]
    pub struct WindowSpec {
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub partition_by: Vec<Expr>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub order_by: Vec<OrderByExpr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub window_frame: Option<Box<WindowFrame>>,
    }
}

common_enum! {
    pub enum WindowFrameUnits {
        Rows,
        Range,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum WindowFrameBound {
    Preceding(Option<Box<Expr>>),
    Following(Option<Box<Expr>>),
    CurrentRow,
}

common_struct! {
    pub struct WindowFrame {
        pub units: WindowFrameUnits,
        pub start_bound: WindowFrameBound,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub end_bound: Option<WindowFrameBound>,
    }
}

common_enum! {
    pub enum SetOperator {
        Union,
    }
}

common_enum! {
    pub enum SetQuantifier {
        All,
        Distinct,
    }
}

common_struct! {
    pub struct Query {
        pub body: Box<SetExpr>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub order_by: Vec<OrderByExpr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub limit: Option<Expr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub offset: Option<Expr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub format: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub sample_ratio: Option<SampleRatio>,
    }
}

common_enum! {
    pub enum SetExpr {
        Select(Box<Select>),
        Values(Values),
        SetOperation {
            left: Box<SetExpr>,
            right: Box<SetExpr>,
            op: SetOperator,
            set_quantifier: SetQuantifier,
        },
    }
}

common_struct! {
    pub struct Values {
        pub rows: Vec<Vec<Expr>>,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct SampleRatio(pub f64);

impl SampleRatio {
    pub fn as_f64(&self) -> f64 {
        self.0
    }
}

impl std::hash::Hash for SampleRatio {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

common_enum! {
    pub enum SelectItem {
        UnnamedExpr(Expr),
        ExprWithAlias { expr: Expr, alias: Ident },
        Wildcard,
    }
}

common_enum! {
    pub enum GroupByExpr {
        Expressions(Vec<Expr>),
    }
}

impl Default for GroupByExpr {
    fn default() -> Self {
        GroupByExpr::Expressions(Vec::new())
    }
}

common_struct! {
    pub struct Select {
        pub projection: Vec<SelectItem>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub from: Vec<TableWithJoins>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub selection: Option<Expr>,
        #[serde(default)]
        pub group_by: GroupByExpr,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub having: Option<Expr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub distinct: Option<Distinct>,
    }
}

common_struct! {
    pub struct Distinct;
}

common_struct! {
    pub struct TableWithJoins {
        pub relation: TableFactor,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub joins: Vec<Join>,
    }
}

common_enum! {
    pub enum TableFactor {
        Table { name: ObjectName, alias: Option<TableAlias> },
        Derived { subquery: Box<Query>, alias: Option<TableAlias> },
    }
}

common_struct! {
    pub struct TableAlias {
        pub name: Ident,
    }
}

common_struct! {
    pub struct Join {
        pub relation: TableFactor,
        pub join_operator: JoinOperator,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum JoinOperator {
    Inner(JoinConstraint),
    LeftOuter(JoinConstraint),
}

common_enum! {
    pub enum JoinConstraint {
        On(Expr),
        None,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum DataType {
    Boolean,
    Int64,
    UnsignedBigInt,
    Float64,
    Decimal { precision: u8, scale: u8 },
    Date,
    DateTime,
    DateTime64(u32),
    Uuid,
    Ipv4,
    String,
    Array(Box<DataType>),
    Tuple(Vec<DataType>),
    Map(Box<DataType>, Box<DataType>),
    Custom(String, Vec<String>),
}

common_struct! {
    pub struct ColumnDef {
        pub name: Ident,
        pub data_type: DataType,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub options: Vec<ColumnOptionDef>,
    }
}

common_struct! {
    pub struct ColumnOptionDef {
        pub option: ColumnOption,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Hash)]
pub enum ColumnOption {
    Default(Expr),
    Materialized(Expr),
}

common_enum! {
    pub enum ObjectType {
        Table,
        View,
    }
}

common_struct! {
    pub struct Assignment {
        pub id: Vec<Ident>,
        pub value: Expr,
    }
}

common_enum! {
    pub enum AlterTableOperation {
        AddColumn { column_def: ColumnDef },
    }
}

common_enum! {
    pub enum Statement {
        CreateTable {
            name: ObjectName,
            columns: Vec<ColumnDef>,
            engine: Option<String>,
            order_by: Option<Vec<Ident>>,
            as_table: Option<ObjectName>,
            ttl: Option<Expr>,
            sample_by: Option<Expr>,
            query: Option<Box<Query>>,
        },
        CreateView {
            name: ObjectName,
            query: Box<Query>,
        },
        Drop {
            object_type: ObjectType,
            names: Vec<ObjectName>,
        },
        AlterTable {
            name: ObjectName,
            operations: Vec<AlterTableOperation>,
        },
        Insert {
            table_name: ObjectName,
            columns: Vec<Ident>,
            source: Option<Box<Query>>,
        },
        Query(Box<Query>),
        Update {
            table: ObjectName,
            assignments: Vec<Assignment>,
            selection: Option<Expr>,
        },
        Delete {
            from: Vec<TableWithJoins>,
            selection: Option<Expr>,
        },
    }
}

fn join_display<T: Display>(items: &[T], sep: &str) -> String {
    items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Boolean => f.write_str("Boolean"),
            DataType::Int64 => f.write_str("Int64"),
            DataType::UnsignedBigInt => f.write_str("UInt64"),
            DataType::Float64 => f.write_str("Float64"),
            DataType::Decimal { precision, scale } => {
                write!(f, "Decimal({precision}, {scale})")
            }
            DataType::Date => f.write_str("Date"),
            DataType::DateTime => f.write_str("DateTime"),
            DataType::DateTime64(precision) => write!(f, "DateTime64({precision})"),
            DataType::Uuid => f.write_str("UUID"),
            DataType::Ipv4 => f.write_str("IPv4"),
            DataType::String => f.write_str("String"),
            DataType::Array(inner) => write!(f, "Array({inner})"),
            DataType::Tuple(inner) => write!(f, "Tuple({})", join_display(inner, ", ")),
            DataType::Map(key, value) => write!(f, "Map({key}, {value})"),
            DataType::Custom(name, modifiers) => {
                if modifiers.is_empty() {
                    f.write_str(name)
                } else {
                    write!(f, "{}({})", name, modifiers.join(", "))
                }
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(ident) => write!(f, "{ident}"),
            Expr::CompoundIdentifier(idents) => f.write_str(&join_display(idents, ".")),
            Expr::Value(value) => write!(f, "{value}"),
            Expr::BinaryOp { left, op, right } => {
                let op_str = match op {
                    BinaryOperator::Plus => "+",
                    BinaryOperator::Minus => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Modulo => "%",
                    BinaryOperator::Eq => "=",
                    BinaryOperator::NotEq => "!=",
                    BinaryOperator::Lt => "<",
                    BinaryOperator::LtEq => "<=",
                    BinaryOperator::Gt => ">",
                    BinaryOperator::GtEq => ">=",
                    BinaryOperator::And => "AND",
                    BinaryOperator::Or => "OR",
                };
                write!(f, "{left} {op_str} {right}")
            }
            Expr::UnaryOp { op, expr } => match op {
                UnaryOperator::Not => write!(f, "NOT {expr}"),
                UnaryOperator::Plus => write!(f, "+{expr}"),
                UnaryOperator::Minus => write!(f, "-{expr}"),
            },
            Expr::Nested(expr) => write!(f, "({expr})"),
            Expr::Tuple(exprs) => write!(f, "({})", join_display(exprs, ", ")),
            Expr::Array(exprs) => write!(f, "[{}]", join_display(exprs, ", ")),
            Expr::Function(func) => write!(f, "{func}"),
            Expr::Cast { expr, data_type } => write!(f, "CAST({expr} AS {data_type})"),
            Expr::InList {
                expr,
                list,
                negated,
            } => {
                let not = if *negated { " NOT" } else { "" };
                write!(f, "{expr}{not} IN ({})", join_display(list, ", "))
            }
            Expr::InSubquery {
                expr,
                subquery,
                negated,
            } => {
                let not = if *negated { " NOT" } else { "" };
                write!(f, "{expr}{not} IN ({subquery})")
            }
            Expr::Interval(interval) => {
                write!(f, "INTERVAL {}", interval.value)?;
                if let Some(field) = &interval.leading_field {
                    let field_str = match field {
                        DateTimeField::Year => "YEAR",
                        DateTimeField::Month => "MONTH",
                        DateTimeField::Week => "WEEK",
                        DateTimeField::Day => "DAY",
                        DateTimeField::Hour => "HOUR",
                        DateTimeField::Minute => "MINUTE",
                        DateTimeField::Second => "SECOND",
                    };
                    write!(f, " {field_str}")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| match arg {
                FunctionArg::Unnamed(expr) => match expr {
                    FunctionArgExpr::Expr(expr) => expr.to_string(),
                },
            })
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}({})", self.name, args)?;
        if let Some(over) = &self.over {
            write!(f, " OVER {over}")?;
        }
        Ok(())
    }
}

impl Display for WindowType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WindowType::WindowSpec(spec) => write!(f, "({})", spec.as_ref()),
        }
    }
}

impl Display for WindowSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        if !self.partition_by.is_empty() {
            parts.push(format!(
                "PARTITION BY {}",
                join_display(&self.partition_by, ", ")
            ));
        }
        if !self.order_by.is_empty() {
            parts.push(format!("ORDER BY {}", join_display(&self.order_by, ", ")));
        }
        if let Some(frame) = &self.window_frame {
            parts.push(frame.to_string());
        }
        f.write_str(&parts.join(" "))
    }
}

impl Display for WindowFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let units = match self.units {
            WindowFrameUnits::Rows => "ROWS",
            WindowFrameUnits::Range => "RANGE",
        };
        write!(f, "{units} BETWEEN {}", self.start_bound)?;
        if let Some(end) = &self.end_bound {
            write!(f, " AND {end}")?;
        }
        Ok(())
    }
}

impl Display for WindowFrameBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WindowFrameBound::Preceding(Some(expr)) => write!(f, "{expr} PRECEDING"),
            WindowFrameBound::Preceding(None) => f.write_str("UNBOUNDED PRECEDING"),
            WindowFrameBound::Following(Some(expr)) => write!(f, "{expr} FOLLOWING"),
            WindowFrameBound::Following(None) => f.write_str("UNBOUNDED FOLLOWING"),
            WindowFrameBound::CurrentRow => f.write_str("CURRENT ROW"),
        }
    }
}

impl Display for SelectItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SelectItem::UnnamedExpr(expr) => write!(f, "{expr}"),
            SelectItem::ExprWithAlias { expr, alias } => write!(f, "{expr} AS {alias}"),
            SelectItem::Wildcard => f.write_str("*"),
        }
    }
}

impl Display for GroupByExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GroupByExpr::Expressions(exprs) => f.write_str(&join_display(exprs, ", ")),
        }
    }
}

impl Display for OrderByExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)?;
        if let Some(asc) = self.asc {
            if asc {
                f.write_str(" ASC")?;
            } else {
                f.write_str(" DESC")?;
            }
        }
        Ok(())
    }
}

impl Display for TableWithJoins {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.relation)?;
        for join in &self.joins {
            write!(f, " {join}")?;
        }
        Ok(())
    }
}

impl Display for TableFactor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TableFactor::Table { name, alias } => {
                write!(f, "{name}")?;
                if let Some(alias) = alias {
                    write!(f, " AS {}", alias.name)?;
                }
                Ok(())
            }
            TableFactor::Derived { subquery, alias } => {
                write!(f, "({subquery})")?;
                if let Some(alias) = alias {
                    write!(f, " AS {}", alias.name)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Join {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.join_operator {
            JoinOperator::Inner(constraint) => {
                write!(f, "INNER JOIN {}{}", self.relation, constraint)
            }
            JoinOperator::LeftOuter(constraint) => {
                write!(f, "LEFT JOIN {}{}", self.relation, constraint)
            }
        }
    }
}

impl Display for JoinConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JoinConstraint::On(expr) => write!(f, " ON {expr}"),
            JoinConstraint::None => Ok(()),
        }
    }
}

impl Display for Select {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "SELECT")?;
        if self.distinct.is_some() {
            f.write_str(" DISTINCT")?;
        }
        if self.projection.is_empty() {
            f.write_str(" *")?;
        } else {
            write!(f, " {}", join_display(&self.projection, ", "))?;
        }
        if !self.from.is_empty() {
            write!(f, " FROM {}", join_display(&self.from, ", "))?;
        }
        if let Some(selection) = &self.selection {
            write!(f, " WHERE {selection}")?;
        }
        let GroupByExpr::Expressions(exprs) = &self.group_by;
        if !exprs.is_empty() {
            write!(f, " GROUP BY {}", join_display(exprs, ", "))?;
        }
        if let Some(having) = &self.having {
            write!(f, " HAVING {having}")?;
        }
        Ok(())
    }
}

impl Display for Values {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let rows = self
            .rows
            .iter()
            .map(|row| format!("({})", join_display(row, ", ")))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "VALUES {rows}")
    }
}

impl Display for SetExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SetExpr::Select(select) => write!(f, "{select}"),
            SetExpr::Values(values) => write!(f, "{values}"),
            SetExpr::SetOperation {
                left,
                right,
                op,
                set_quantifier,
            } => {
                let op_str = match op {
                    SetOperator::Union => "UNION",
                };
                let quant = match set_quantifier {
                    SetQuantifier::All => " ALL",
                    SetQuantifier::Distinct => "",
                };
                write!(f, "{left} {op_str}{quant} {right}")
            }
        }
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.body)?;
        if !self.order_by.is_empty() {
            write!(f, " ORDER BY {}", join_display(&self.order_by, ", "))?;
        }
        if let Some(limit) = &self.limit {
            write!(f, " LIMIT {limit}")?;
        }
        if let Some(offset) = &self.offset {
            write!(f, " OFFSET {offset}")?;
        }
        if let Some(sample) = &self.sample_ratio {
            write!(f, " SAMPLE {}", sample.as_f64())?;
        }
        if let Some(format) = &self.format {
            write!(f, " FORMAT {format}")?;
        }
        Ok(())
    }
}

impl Display for ColumnOption {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ColumnOption::Default(expr) => write!(f, "DEFAULT {expr}"),
            ColumnOption::Materialized(expr) => write!(f, "MATERIALIZED {expr}"),
        }
    }
}

impl Display for ColumnDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.name, self.data_type)?;
        for opt in &self.options {
            write!(f, " {}", opt.option)?;
        }
        Ok(())
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", join_display(&self.id, "."), self.value)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::CreateTable {
                name,
                columns,
                engine,
                order_by,
                as_table,
                ttl,
                sample_by,
                query,
            } => {
                write!(f, "CREATE TABLE {name}")?;
                if !columns.is_empty() {
                    write!(f, " ({})", join_display(columns, ", "))?;
                }
                if let Some(as_table) = as_table {
                    write!(f, " AS {as_table}")?;
                }
                if let Some(query) = query {
                    write!(f, " AS {query}")?;
                }
                if let Some(engine) = engine {
                    write!(f, " ENGINE = {engine}")?;
                }
                if let Some(order_by) = order_by {
                    write!(f, " ORDER BY {}", join_display(order_by, ", "))?;
                }
                if let Some(sample_by) = sample_by {
                    write!(f, " SAMPLE BY {sample_by}")?;
                }
                if let Some(ttl) = ttl {
                    write!(f, " TTL {ttl}")?;
                }
                Ok(())
            }
            Statement::CreateView { name, query } => write!(f, "CREATE VIEW {name} AS {query}"),
            Statement::Drop { object_type, names } => {
                let kind = match object_type {
                    ObjectType::Table => "TABLE",
                    ObjectType::View => "VIEW",
                };
                write!(f, "DROP {kind} {}", join_display(names, ", "))
            }
            Statement::AlterTable { name, operations } => {
                write!(f, "ALTER TABLE {name}")?;
                if !operations.is_empty() {
                    let ops = operations
                        .iter()
                        .map(|op| match op {
                            AlterTableOperation::AddColumn { column_def } => {
                                format!("ADD COLUMN {column_def}")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, " {ops}")?;
                }
                Ok(())
            }
            Statement::Insert {
                table_name,
                columns,
                source,
            } => {
                write!(f, "INSERT INTO {table_name}")?;
                if !columns.is_empty() {
                    write!(f, " ({})", join_display(columns, ", "))?;
                }
                if let Some(source) = source {
                    write!(f, " {source}")?;
                }
                Ok(())
            }
            Statement::Query(query) => write!(f, "{query}"),
            Statement::Update {
                table,
                assignments,
                selection,
            } => {
                write!(f, "UPDATE {table} SET {}", join_display(assignments, ", "))?;
                if let Some(selection) = selection {
                    write!(f, " WHERE {selection}")?;
                }
                Ok(())
            }
            Statement::Delete { from, selection } => {
                write!(f, "DELETE FROM {}", join_display(from, ", "))?;
                if let Some(selection) = selection {
                    write!(f, " WHERE {selection}")?;
                }
                Ok(())
            }
        }
    }
}
