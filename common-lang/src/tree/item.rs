use crate::tree::*;
use crate::value::{FunctionValue, TypeValue};
use common::{Deserialize, Serialize};

/// Item is an syntax tree node that "declares" a thing without returning a value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    Module(Module),
    Def(Define),
    Import(Import),
    Impl(Impl),
    Any(AnyBox),
}

impl Item {
    pub fn any<T: Debug + 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
pub type ItemChunk = Vec<Item>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: Ident,
    pub items: ItemChunk,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatementExpr {
    pub expr: Expr,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    Item(Box<Item>),
    Let(Let),
    StmtExpr(StatementExpr),
    Expr(Expr),
    Any(AnyBox),
}
impl Statement {
    pub fn any<T: Debug + 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn item(item: Item) -> Self {
        Self::Item(Box::new(item))
    }
    pub fn stmt_expr(expr: Expr) -> Self {
        Self::StmtExpr(StatementExpr { expr })
    }
    pub fn maybe_stmt_expr(expr: Expr, is_stmt: bool) -> Self {
        if is_stmt {
            Self::stmt_expr(expr)
        } else {
            Self::Expr(expr)
        }
    }
    pub fn try_make_stmt(&mut self) {
        if let Self::Expr(expr) = self {
            *self = Self::stmt_expr(expr.clone());
        }
    }
    pub fn try_make_expr(&mut self) {
        if let Self::StmtExpr(expr) = self {
            *self = Self::Expr(expr.expr.clone());
        }
    }
}
pub type StatementChunk = Vec<Statement>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: StatementChunk,
}

#[derive(Debug, Clone, Serialize, Deserialize, Copy)]
pub enum Visibility {
    Public,
    Private,
    Inherited,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum DefKind {
    Unknown,
    Function,
    Type,
    Const,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DefValue {
    Function(FunctionValue),
    Type(TypeExpr),
    Const(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Define {
    pub name: Ident,
    pub kind: DefKind,
    pub ty: Option<TypeValue>,
    pub value: DefValue,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Let {
    pub name: Ident,
    pub ty: Option<TypeValue>,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    pub visibility: Visibility,
    pub path: Path,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    pub target: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CondCase {
    pub cond: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cond {
    pub cases: Vec<CondCase>,
    pub if_style: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForEach {
    pub variable: Ident,
    pub iterable: Tree,
    pub body: Block,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct While {
    pub cond: Tree,
    pub body: Block,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Impl {
    pub name: Ident,
    pub defs: Vec<Define>,
}
