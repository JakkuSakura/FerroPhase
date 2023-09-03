use crate::ast::*;
use crate::value::{FunctionValue, TypeValue};
use common::{Deserialize, Serialize};

/// Item is an syntax tree node that "declares" a thing without returning a value
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Item {
    Module(Module),
    Define(Define),
    Import(Import),
    Impl(Impl),
    Expr(Expr),
    Any(AnyBox),
}

impl Item {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            Self::Expr(expr) => Some(expr),
            _ => None,
        }
    }
    pub fn as_define(&self) -> Option<&Define> {
        match self {
            Self::Define(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_module(&self) -> Option<&Module> {
        match self {
            Self::Module(module) => Some(module),
            _ => None,
        }
    }
    pub fn as_import(&self) -> Option<&Import> {
        match self {
            Self::Import(import) => Some(import),
            _ => None,
        }
    }
    pub fn as_impl(&self) -> Option<&Impl> {
        match self {
            Self::Impl(impl_) => Some(impl_),
            _ => None,
        }
    }
}
pub type ItemChunk = Vec<Item>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub items: ItemChunk,
    pub visibility: Visibility,
}
impl Module {
    pub fn find_item(&self, name: &str) -> Option<&Item> {
        self.items.iter().find(|item| match item {
            Item::Define(define) => define.name.as_str() == name,
            Item::Module(module) => module.name.as_str() == name,
            Item::Import(import) => import.path.to_string().as_str() == name,
            _ => false,
        })
    }
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
impl DefValue {
    pub fn as_function(&self) -> Option<&FunctionValue> {
        match self {
            DefValue::Function(fn_) => Some(fn_),
            _ => None,
        }
    }
    pub fn as_type(&self) -> Option<&TypeExpr> {
        match self {
            DefValue::Type(ty) => Some(ty),
            _ => None,
        }
    }
    pub fn as_const(&self) -> Option<&Expr> {
        match self {
            DefValue::Const(expr) => Some(expr),
            _ => None,
        }
    }
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
    pub trait_ty: Option<Pat>,
    pub self_ty: TypeExpr,
    pub items: ItemChunk,
}
impl Impl {
    pub fn find_item(&self, name: &str) -> Option<&Item> {
        self.items.iter().find(|item| match item {
            Item::Define(define) => define.name.as_str() == name,
            Item::Module(module) => module.name.as_str() == name,
            Item::Import(import) => import.path.to_string().as_str() == name,
            _ => false,
        })
    }
}
