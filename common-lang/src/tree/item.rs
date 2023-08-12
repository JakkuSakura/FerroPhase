use crate::tree::*;
use common::{Deserialize, Serialize};

/// Item is an syntax tree node that "declares" a thing without returning a value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    Module(Module),
    Def(Def),
    Import(Import),
}

/// Tree is any syntax tree element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Tree {
    Item(Item),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Item>,
    pub last_value: bool,
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
    Function(FuncDecl),
    Type(TypeExpr),
    Const(Expr),
    Variable(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Def {
    pub name: Ident,
    pub kind: DefKind,
    pub ty: Option<TypeExpr>,
    pub value: DefValue,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamExpr {
    pub name: Ident,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncDecl {
    pub name: Ident,
    pub params: Vec<ParamExpr>,
    pub ret: TypeExpr,
    pub body: Block,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    pub visibility: Visibility,
    pub segments: Vec<Ident>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldValueExpr {
    pub name: Ident,
    pub value: Expr,
}
