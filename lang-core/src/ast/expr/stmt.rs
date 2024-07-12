use std::hash::Hash;

use crate::ast::{AstExpr, AstItem, BExpr, BItem};
use crate::common_enum;
use crate::common_struct;
use crate::id::Ident;
use crate::pat::{Pattern, PatternIdent};
use crate::utils::anybox::{AnyBox, AnyBoxable};

common_enum! {
    pub enum BlockStmt {
        Item(BItem),
        Let(StmtLet),
        Expr(AstExpr),
        Any(AnyBox),
    }
}

impl BlockStmt {
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
    pub struct StmtLet {
        pub pat: Pattern,
        pub value: AstExpr,
    }
}
impl StmtLet {
    pub fn new_simple(name: Ident, value: AstExpr) -> Self {
        Self {
            pat: Pattern::Ident(PatternIdent::new(name)),
            value,
        }
    }
    pub fn make_mut(&mut self) {
        if let Pattern::Ident(name) = &mut self.pat {
            name.mutability = Some(true);
        } else {
            unreachable!("Pattern::Ident expected")
        }
    }
}

pub type StmtChunk = Vec<BlockStmt>;

common_struct! {
    pub struct ExprBlock {
        pub stmts: StmtChunk,
        pub ret: Option<BExpr>
    }
}
impl ExprBlock {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            ret: None,
        }
    }
    pub fn push_stmt(&mut self, stmt: BlockStmt) {
        self.stmts.push(stmt);
    }
    pub fn push_expr(&mut self, stmt: BExpr) {
        let prev = self.ret.replace(stmt);
        if let Some(prev) = prev {
            self.stmts.push(BlockStmt::Expr(*prev));
        }
    }
}
