use eyre::Result;

use lang_core::ast::Statement;
use lang_core::context::SharedScopedContext;

use crate::emitter::expr::MipsEmitExprResult;
use crate::emitter::MipsEmitter;

impl MipsEmitter {
    pub fn emit_statement(
        &mut self,
        stmt: &Statement,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match stmt {
            Statement::Expr(expr) => self.emit_expr(expr, ctx),
            Statement::Item(item) => self.emit_item(item, ctx),
            _ => unimplemented!("emit_statement: {:?}", stmt),
        }
    }
}
