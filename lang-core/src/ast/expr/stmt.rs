use std::hash::Hash;

use crate::ast::{AstExpr, AstItem, BExpr, BItem};
use crate::common_enum;
use crate::common_struct;
use crate::pat::Pattern;
use crate::utils::anybox::{AnyBox, AnyBoxable};

common_enum! {
    pub enum Statement {
        Item(BItem),
        Let(StatementLet),
        Expr(AstExpr),
        Any(AnyBox),
    }
}

impl Statement {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn item(item: AstItem) -> Self {
        Self::Item(Box::new(item))
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.is_unit(),
            Self::Item(item) => item.is_unit(),
            _ => false,
        }
    }
}

common_struct! {
    pub struct StatementLet {
        pub pat: Pattern,
        pub value: AstExpr,
    }
}

pub type StatementChunk = Vec<Statement>;

common_struct! {
    pub struct ExprBlock {
        pub stmts: StatementChunk,
        pub ret: Option<BExpr>
    }
}
