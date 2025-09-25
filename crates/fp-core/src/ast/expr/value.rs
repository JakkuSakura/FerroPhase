use std::fmt::{Display, Formatter};
use std::hash::Hash;

use crate::ast::{get_threadlocal_serializer, BExpr, Expr, Ty, Value};
use crate::ast::{BType, ValueFunction};
use crate::id::{Ident, Locator};
use crate::ops::{BinOpKind, UnOpKind};
use crate::pat::{BPattern, Pattern};
use crate::{common_enum, common_struct};

common_enum! {
    pub enum ExprInvokeTarget {
        Function(Locator),
        Type(Ty),
        Method(ExprSelect),
        Closure(ValueFunction),
        BinOp(BinOpKind),
        Expr(BExpr),
    }
}
impl ExprInvokeTarget {
    pub fn expr(expr: Expr) -> Self {
        match expr {
            Expr::Locator(locator) => Self::Function(locator.clone()),
            Expr::Select(select) => Self::Method(select.clone()),
            Expr::Value(value) => Self::value(*value),
            _ => Self::Expr(expr.into()),
        }
    }
    pub fn value(value: Value) -> Self {
        match value {
            Value::Function(func) => Self::Closure(func.clone()),
            Value::BinOpKind(kind) => Self::BinOp(kind.clone()),
            Value::Type(ty) => Self::Type(ty.clone()),
            Value::Expr(expr) => Self::expr(*expr),
            _ => panic!("Invalid value for ExprInvokeTarget::value: {}", value),
        }
    }
}

common_struct! {
    pub struct ExprInvoke {
        pub target: ExprInvokeTarget,
        pub args: Vec<Expr>,
    }
}
impl Display for ExprInvoke {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_invoke(self).unwrap();

        f.write_str(&s)
    }
}

common_struct! {
    pub struct ExprFormatString {
        /// Template parts - alternating literals and placeholders
        pub parts: Vec<FormatTemplatePart>,
        /// Positional arguments to substitute into placeholders
        pub args: Vec<Expr>,
        /// Named keyword arguments for named placeholders
        pub kwargs: Vec<FormatKwArg>,
    }
}

common_enum! {
    pub enum FormatTemplatePart {
        /// A literal string part
        Literal(String),
        /// A placeholder that references an argument
        Placeholder(FormatPlaceholder),
    }
}

common_struct! {
    pub struct FormatPlaceholder {
        /// Argument reference - can be positional index, name, or implicit
        pub arg_ref: FormatArgRef,
        /// Optional format specification (e.g., ":02d", ":.2f")
        pub format_spec: Option<String>,
    }
}

common_enum! {
    pub enum FormatArgRef {
        /// Implicit positional argument (next in sequence)
        Implicit,
        /// Explicit positional argument by index (e.g., {0}, {1})
        Positional(usize),
        /// Named argument (e.g., {name}, {value})
        Named(String),
    }
}

common_struct! {
    pub struct FormatKwArg {
        /// The keyword name
        pub name: String,
        /// The expression value
        pub value: Expr,
    }
}

impl Display for ExprFormatString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "format!(\"")?;
        // Reconstruct the template string from parts
        for part in &self.parts {
            write!(f, "{}", part)?;
        }
        write!(f, "\"")?;
        for arg in &self.args {
            write!(f, ", {}", arg)?;
        }
        for kwarg in &self.kwargs {
            write!(f, ", {}={}", kwarg.name, kwarg.value)?;
        }
        write!(f, ")")
    }
}

impl Display for FormatTemplatePart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatTemplatePart::Literal(s) => write!(f, "{}", s),
            FormatTemplatePart::Placeholder(placeholder) => write!(f, "{{{}}}", placeholder),
        }
    }
}

impl Display for FormatPlaceholder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.arg_ref)?;
        if let Some(spec) = &self.format_spec {
            write!(f, ":{}", spec)?;
        }
        Ok(())
    }
}

impl Display for FormatArgRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatArgRef::Implicit => Ok(()), // Empty for implicit {}
            FormatArgRef::Positional(idx) => write!(f, "{}", idx),
            FormatArgRef::Named(name) => write!(f, "{}", name),
        }
    }
}

impl Display for FormatKwArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

common_enum! {
    pub enum ExprSelectType {
        Unknown,
        Field,
        Method,
        Function,
        Const,
    }

}

common_struct! {
    pub struct ExprSelect {
        pub obj: BExpr,
        pub field: Ident,
        pub select: ExprSelectType,
    }
}

common_struct! {
    pub struct ExprIndex {
        pub obj: BExpr,
        pub index: BExpr,
    }
}

common_struct! {
    pub struct ExprReference {
        pub referee: BExpr,
        pub mutable: Option<bool>,
    }
}
common_struct! {
    pub struct ExprDereference {
        pub referee: BExpr,
    }
}

common_struct! {
    pub struct ExprMatch {
        pub cases: Vec<ExprMatchCase>,
    }
}

common_struct! {
    pub struct ExprIf {
        pub cond: BExpr,
        pub then: BExpr,
        pub elze: Option<BExpr>,
    }
}
common_struct! {
    pub struct ExprLoop {
        pub label: Option<Ident>,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprWhile {
        pub cond: BExpr,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprMatchCase {
        pub cond: BExpr,
        pub body: BExpr,
    }
}

common_enum! {
    pub enum ControlFlow {
        Continue,
        #[from(ignore)]
        Break(Option<Expr>),
        #[from(ignore)]
        Return(Option<Expr>),
        Into,
        #[from(ignore)]
        IntoAndBreak(Option<Expr>),
    }
}
common_struct! {
    pub struct ExprStruct {
        pub name: BExpr,
        pub fields: Vec<ExprField>,
    }
}
impl ExprStruct {
    pub fn new_ident(name: Ident, fields: Vec<ExprField>) -> Self {
        Self {
            name: Expr::ident(name).into(),
            fields,
        }
    }
    pub fn new(name: BExpr, fields: Vec<ExprField>) -> Self {
        Self { name, fields }
    }
}
common_struct! {
    pub struct ExprStructural {
        pub fields: Vec<ExprField>,
    }
}
common_struct! {
    pub struct ExprField {
        pub name: Ident,
        pub value: Option<Expr>,
    }
}
impl ExprField {
    pub fn new(name: Ident, value: Expr) -> Self {
        Self {
            name,
            value: Some(value),
        }
    }
    pub fn new_no_value(name: Ident) -> Self {
        Self { name, value: None }
    }
}
common_struct! {
    pub struct ExprBinOp {
        pub kind: BinOpKind,
        pub lhs: BExpr,
        pub rhs: BExpr,
    }
}
common_struct! {
    pub struct ExprUnOp {
        pub op: UnOpKind,
        pub val: BExpr,

    }
}

common_struct! {
    pub struct ExprAssign {
        pub target: BExpr,
        pub value: BExpr,
    }
}
common_struct! {
    pub struct ExprParen {
        pub expr: BExpr,
    }
}
common_enum! {
    pub enum ExprRangeLimit {
        Inclusive,
        Exclusive,
    }
}
common_struct! {
    pub struct ExprRange {
        pub start: Option<BExpr>,
        pub limit: ExprRangeLimit,
        pub end: Option<BExpr>,
        pub step: Option<BExpr>,
    }
}

common_struct! {
    pub struct ExprTuple {
        pub values: Vec<Expr>,
    }
}

common_struct! {
    pub struct ExprTry {
        pub expr: BExpr,
    }
}

common_struct! {
    pub struct ExprLet {
        pub pat: BPattern,
        pub expr: BExpr,
    }
}
common_struct! {
    pub struct ExprClosure {
        pub params: Vec<Pattern>,
        pub ret_ty: Option<BType>,
        pub movability: Option<bool>,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprArray {
        pub values: Vec<Expr>,
    }
}
common_struct! {
    /// To "splat" or expand an iterable.
    /// For example, in Python, `*a` will expand `a` into the arguments of a function
    pub struct ExprSplat {
        pub iter: Box<Expr>,
    }
}
common_struct! {
    /// To "splat" or expand a dict.
    /// For example, in Python, `**d` will expand `d` into the keyword arguments of a function
    pub struct ExprSplatDict {
        pub dict: Box<Expr>,
    }
}
