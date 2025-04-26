use eyre::Result;

use fp_core::ast::{BlockStmt, BlockStmtExpr};
use fp_core::context::SharedScopedContext;

use crate::emitter::expr::MipsEmitExprResult;
use crate::emitter::MipsEmitter;
use crate::storage::register::MipsRegisterOwned;

impl MipsEmitter {
    fn emit_stmt_expr(
        &mut self,
        stmt: &BlockStmtExpr,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        let mut ret = self.emit_expr(&stmt.expr, ctx)?;
        if stmt.semicolon == Some(true) {
            ret.ret = MipsRegisterOwned::zero();
        }
        Ok(ret)
    }
    pub fn emit_statement(
        &mut self,
        stmt: &BlockStmt,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match stmt {
            BlockStmt::Expr(expr) => self.emit_stmt_expr(expr, ctx),
            BlockStmt::Item(item) => self.emit_item(item, ctx),
            _ => unimplemented!("emit_statement: {:?}", stmt),
        }
    }
}
