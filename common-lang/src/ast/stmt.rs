use crate::ast::{AnyBox, AnyBoxable, Block, Expr, Ident, Item, Tree};
use crate::value::TypeValue;
use common::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SideEffect {
    pub expr: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    Item(Box<Item>),
    Let(Let),
    SideEffect(SideEffect),
    Expr(Expr),
    Any(AnyBox),
}

impl Statement {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn item(item: Item) -> Self {
        Self::Item(Box::new(item))
    }
    pub fn stmt_expr(expr: Expr) -> Self {
        Self::SideEffect(SideEffect { expr })
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
        if let Self::SideEffect(expr) = self {
            *self = Self::Expr(expr.expr.clone());
        }
    }
}

pub type StatementChunk = Vec<Statement>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Let {
    pub name: Ident,
    pub ty: Option<TypeValue>,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    pub target: Expr,
    pub value: Expr,
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
