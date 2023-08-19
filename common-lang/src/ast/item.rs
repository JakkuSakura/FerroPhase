use crate::ast::*;
use crate::value::{FunctionValue, TypeValue};
use common::{Deserialize, Serialize};

/// Item is an syntax tree node that "declares" a thing without returning a value
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Item {
    Module(Module),
    Def(Define),
    Import(Import),
    Impl(Impl),
    Expr(Expr),
    Any(AnyBox),
}

impl Item {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
}
pub type ItemChunk = Vec<Item>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub items: ItemChunk,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Block {
    pub stmts: StatementChunk,
}
impl Block {
    pub fn new(stmts: StatementChunk) -> Self {
        Self { stmts }
    }
    pub fn prepend(lhs: StatementChunk, rhs: Expr) -> Self {
        let mut stmts = lhs;
        match rhs {
            Expr::Block(block) => {
                stmts.extend(block.stmts);
            }
            _ => {
                stmts.push(Statement::Expr(rhs));
            }
        }
        Self::new(stmts)
    }
    pub fn make_last_side_effect(&mut self) {
        if let Some(last) = self.stmts.last_mut() {
            last.try_make_stmt();
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Copy)]
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum DefValue {
    Function(FunctionValue),
    Type(TypeExpr),
    Const(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Define {
    pub name: Ident,
    pub kind: DefKind,
    pub ty: Option<TypeValue>,
    pub value: DefValue,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Import {
    pub visibility: Visibility,
    pub path: Path,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Impl {
    pub name: Ident,
    pub defs: Vec<Define>,
}
