use std::hash::Hash;

use crate::ast::{AstExpr, AstItem, AstType, BExpr, BItem};
use crate::common_enum;
use crate::common_struct;
use crate::id::Ident;
use crate::pat::{Pattern, PatternIdent, PatternType};
use crate::utils::anybox::{AnyBox, AnyBoxable};

common_enum! {
    pub enum BlockStmt {
        Item(BItem),
        Let(StmtLet),
        Expr(AstExpr),
        /// really noop
        Noop,
        Any(AnyBox),
    }
}

impl BlockStmt {
    pub fn noop() -> Self {
        Self::Noop
    }
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
            Self::Noop => true,
            _ => false,
        }
    }
}

common_struct! {
    pub struct StmtLet {
        pub pat: Pattern,
        pub init: Option<AstExpr>,
    }
}
impl StmtLet {
    pub fn new_name_type_value(name: Ident, ty: AstType, value: AstExpr) -> Self {
        Self {
            pat: Pattern::Type(PatternType::new(
                Pattern::Ident(PatternIdent::new(name)),
                ty,
            )),
            init: Some(value),
        }
    }
    pub fn new_simple(name: Ident, value: AstExpr) -> Self {
        Self {
            pat: Pattern::Ident(PatternIdent::new(name)),
            init: Some(value),
        }
    }
    pub fn make_mut(&mut self) {
        self.pat.make_mut()
    }
}

pub type StmtChunk = Vec<BlockStmt>;

common_struct! {
    pub struct ExprBlock {
        pub stmts: StmtChunk,
        pub expr: Option<BExpr>
    }
}
impl ExprBlock {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            expr: None,
        }
    }
    pub fn push_up(&mut self) {
        if let Some(expr) = self.expr.take() {
            self.stmts.push(BlockStmt::Expr(*expr));
        }
    }
    pub fn extend(&mut self, other: ExprBlock) {
        self.push_up();
        self.stmts.extend(other.stmts);
        self.expr = other.expr;
    }
    pub fn extend_chunk(&mut self, chunk: StmtChunk) {
        self.push_up();
        self.stmts.extend(chunk);
    }
    pub fn push_stmt(&mut self, stmt: BlockStmt) {
        self.stmts.push(stmt);
        self.push_up();
    }
    pub fn push_expr(&mut self, stmt: BExpr) {
        self.push_up();
        self.expr = Some(stmt);
    }
}
