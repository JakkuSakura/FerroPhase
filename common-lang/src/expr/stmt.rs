use crate::common_enum;
use crate::common_struct;
use crate::expr::{Expr, Item};
use crate::id::Ident;
use crate::pat::Pattern;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use std::hash::Hash;

common_enum! {
    pub enum Statement {
        Item(Box<Item>),
        Let(StatementLet),
        Assign(StatementAssign),
        SideEffect(SideEffect),
        ForEach(StatementForEach),
        While(StatementWhile),
        Loop(StatementLoop),
        Expr(Expr),
        Any(AnyBox),
    }
}

impl Statement {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(crate::utils::anybox::AnyBox::new(any))
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
    pub fn is_unit(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.is_unit(),
            Self::SideEffect(expr) => expr.expr.is_unit(),
            _ => false,
        }
    }
}

common_struct! {
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
common_struct! {
    pub struct StatementLet {
        pub pat: Pattern,
        pub value: Expr,
    }
}

common_struct! {
    pub struct StatementAssign {
        pub target: Expr,
        pub value: Expr,
    }
}

common_struct! {
    pub struct StatementForEach {
        pub variable: Ident,
        pub iterable: Expr,
        pub body: Block,
    }
}

common_struct! {
    pub struct StatementWhile {
        pub cond: Expr,
        pub body: Block,
    }
}
common_struct! {
    pub struct StatementLoop {
        pub body: Block,
    }
}

pub type StatementChunk = Vec<Statement>;

common_struct! {
    pub struct Block {
        pub stmts: StatementChunk,
    }
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
