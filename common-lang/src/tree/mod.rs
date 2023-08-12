use common::*;
use std::fmt::Debug;

mod expr;
mod item;

mod typing;

pub use expr::*;
pub use item::*;

pub use typing::*;

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invoke {
    pub fun: Box<Expr>,
    pub args: Vec<Expr>,
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
    pub defs: Vec<Def>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum SelectType {
    Unknown,
    Field,
    Method,
    Function,
    Const,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Select {
    pub obj: Box<Expr>,
    pub field: Ident,
    pub select: SelectType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reference {
    pub referee: Expr,
    pub mutable: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Uplifted {
    pub uplifted: Tree,
    pub raw: Tree,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Path {
    pub segments: Vec<Ident>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildStructExpr {
    pub name: TypeExpr, // either Ident or Struct
    pub fields: Vec<FieldValueExpr>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildFunctionExpr {
    pub params: Vec<ParamExpr>,
    pub ret: TypeExpr,
    pub body: Block,
}
