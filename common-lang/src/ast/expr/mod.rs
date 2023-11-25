mod typing;
mod value;
use crate::ast::Statement;
use crate::common_derives;
pub use typing::*;
pub use value::*;

pub type StatementChunk = Vec<Statement>;

common_derives! {
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
