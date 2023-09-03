use crate::ast::{AnyBox, AnyBoxable, Block, Expr, Ident, Item};
use crate::common_derives;
use crate::value::TypeValue;
use common::{Deserialize, Serialize};
use std::hash::Hash;
common_derives! {
    pub struct SideEffect {
        pub expr: Expr,
    }

}
impl SideEffect {
    pub fn new(expr: Expr) -> Self {
        match expr {
            Expr::Block(block) => {
                let mut block = block;
                block.make_last_side_effect();
                Self {
                    expr: Expr::Block(block),
                }
            }
            _ => Self { expr },
        }
    }
}

common_derives! {
    pub enum Statement {
        Item(Box<Item>),
        Let(Let),
        Assign(Assign),
        SideEffect(SideEffect),
        ForEach(ForEach),
        While(While),
        Loop(Loop),
        Expr(Expr),
        Any(AnyBox),
    }
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

common_derives! {
    pub struct Let {
        pub name: Ident,
        pub mutability: Option<bool>,
        pub ty: Option<TypeValue>,
        pub value: Expr,
    }
}

common_derives! {
    pub struct Assign {
        pub target: Expr,
        pub value: Expr,
    }
}

common_derives! {
    pub struct ForEach {
        pub variable: Ident,
        pub iterable: Expr,
        pub body: Block,
    }
}

common_derives! {
    pub struct While {
        pub cond: Expr,
        pub body: Block,
    }
}
common_derives! {
    pub struct Loop {
        pub body: Block,
    }
}
