use crate::tree::*;
use crate::value::{FunctionValue, TypeValue};
use common::{Deserialize, Serialize};

/// Item is an syntax tree node that "declares" a thing without returning a value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    Module(Module),
    Def(Define),
    Import(Import),
    Stmt(Expr),
    Expr(Expr),
    Impl(Impl),
    Any(AnyBox),
}

impl Item {
    pub fn try_make_stmt(&mut self) {
        if let Item::Expr(expr) = self {
            *self = Item::Stmt(expr.clone());
        }
    }
    pub fn try_make_expr(&mut self) {
        if let Item::Stmt(expr) = self {
            *self = Item::Expr(expr.clone());
        }
    }
    pub fn any<T: Debug + 'static>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Item>,
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
    Variable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DefValue {
    Function(FunctionValue),
    Type(TypeExpr),
    Const(Expr),
    Variable(Expr),
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
