use std::hash::Hash;

use crate::ast::{
    BExpr, BItem, Expr, ExprKind, Ident, Item, Pattern, PatternIdent, PatternKind, PatternType, Ty,
};
use crate::common_enum;
use crate::common_struct;
use crate::span::Span;
use crate::utils::anybox::{AnyBox, AnyBoxable};

common_enum! {
    pub enum BlockStmt {
        Item(BItem),
        Let(StmtLet),
        Expr(BlockStmtExpr),
        /// really noop
        Noop,
        Any(AnyBox),
    }
}

impl BlockStmt {
    pub fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.expr.span(),
            Self::Item(item) => item.span(),
            Self::Let(stmt) => Span::union(
                [
                    Some(stmt.pat.span()),
                    stmt.init.as_ref().map(|expr| expr.span()),
                    stmt.diverge.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
            Self::Noop | Self::Any(_) => Span::null(),
        }
    }
    pub fn noop() -> Self {
        Self::Noop
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn item(item: Item) -> Self {
        Self::Item(Box::new(item))
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Self::Expr(expr) => expr.expr.is_unit(),
            Self::Item(item) => item.is_unit(),
            Self::Noop => true,
            _ => false,
        }
    }
}
common_struct! {
    pub struct BlockStmtExpr {
        pub expr: BExpr,
        /// default is to keep semicolon, but for some expr like if, the default case is different
        pub semicolon: Option<bool>,
    }
}
impl BlockStmtExpr {
    pub fn new(expr: impl Into<BExpr>) -> Self {
        Self {
            expr: expr.into(),
            semicolon: None,
        }
    }
    pub fn with_semicolon(mut self, semicolon: bool) -> Self {
        self.semicolon = Some(semicolon);
        self
    }
    pub fn has_value(&self) -> bool {
        self.semicolon != Some(true)
    }
}

common_struct! {
    pub struct StmtLet {
        pub pat: Pattern,
        pub init: Option<Expr>,
        pub diverge: Option<Expr>,
    }
}
impl StmtLet {
    pub fn new(pat: Pattern, init: Option<Expr>, diverge: Option<Expr>) -> Self {
        assert!(diverge.is_none() || init.is_some(), "diverge without init");
        Self { pat, init, diverge }
    }
    pub fn new_typed(name: Ident, ty: Ty, value: Expr) -> Self {
        Self {
            pat: Pattern::from(PatternKind::Type(PatternType::new(
                Pattern::from(PatternKind::Ident(PatternIdent::new(name))),
                ty,
            ))),
            init: Some(value),
            diverge: None,
        }
    }
    pub fn new_simple(name: Ident, value: Expr) -> Self {
        Self {
            pat: Pattern::from(PatternKind::Ident(PatternIdent::new(name))),
            init: Some(value),
            diverge: None,
        }
    }
    pub fn make_mut(&mut self) {
        self.pat.make_mut()
    }
}

pub type StmtChunk = Vec<BlockStmt>;

common_struct! {
    pub struct ExprBlock {
        #[serde(default)]
        pub span: Span,
        pub stmts: StmtChunk,
    }
}
impl ExprBlock {
    pub fn new() -> Self {
        Self {
            span: Span::null(),
            stmts: Vec::new(),
        }
    }
    pub fn new_stmts(stmts: StmtChunk) -> Self {
        Self {
            span: Span::null(),
            stmts,
        }
    }
    pub fn new_stmts_expr(stmts: StmtChunk, expr: impl Into<BExpr>) -> Self {
        let mut this = Self {
            span: Span::null(),
            stmts,
        };
        this.push_expr(expr);
        this
    }
    pub fn new_expr(expr: Expr) -> Self {
        Self {
            span: Span::null(),
            stmts: vec![BlockStmt::Expr(BlockStmtExpr::new(expr))],
        }
    }
    pub fn seal(&mut self) {
        if let Some(expr) = self.stmts.last_mut() {
            if let BlockStmt::Expr(expr) = expr {
                if expr.semicolon == Some(false) {
                    expr.semicolon = Some(true);
                }
            }
        }
    }
    pub fn extend(&mut self, other: ExprBlock) {
        self.seal();
        self.stmts.extend(other.stmts);
    }
    pub fn extend_chunk(&mut self, chunk: StmtChunk) {
        self.seal();
        self.stmts.extend(chunk);
    }
    pub fn push_stmt(&mut self, stmt: BlockStmt) {
        self.stmts.push(stmt);
        self.seal();
    }
    pub fn push_expr(&mut self, stmt: impl Into<BExpr>) {
        self.seal();
        self.push_stmt(BlockStmt::Expr(
            BlockStmtExpr::new(stmt).with_semicolon(false),
        ));
    }
    pub fn last_expr(&self) -> Option<&Expr> {
        let stmt = self.stmts.last()?;
        let BlockStmt::Expr(expr) = stmt else {
            return None;
        };
        if !expr.has_value() {
            return None;
        }
        Some(&*expr.expr)
    }
    pub fn last_expr_mut(&mut self) -> Option<&mut Expr> {
        let stmt = self.stmts.last_mut()?;
        let BlockStmt::Expr(expr) = stmt else {
            return None;
        };
        if !expr.has_value() {
            return None;
        }
        Some(&mut expr.expr)
    }
    pub fn into_expr(mut self) -> Expr {
        if self.stmts.len() == 1 {
            if let Some(expr) = self.last_expr_mut() {
                return std::mem::replace(expr, Expr::unit());
            }
        }

        ExprKind::Block(self).into()
    }
    /// returns the first few stmts, leaving behind the last expr
    pub fn first_stmts(&self) -> &[BlockStmt] {
        if self.last_expr().is_some() {
            &self.stmts[..self.stmts.len() - 1]
        } else {
            &self.stmts
        }
    }

    pub fn span(&self) -> Span {
        if self.span.is_null() {
            Span::union(self.stmts.iter().map(BlockStmt::span))
        } else {
            self.span
        }
    }
}
