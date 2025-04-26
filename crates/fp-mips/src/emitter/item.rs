use eyre::Result;

use fp_core::ast::AstItem;
use fp_core::context::SharedScopedContext;

use crate::emitter::expr::MipsEmitExprResult;
use crate::emitter::MipsEmitter;

impl MipsEmitter {
    pub fn emit_item(
        &mut self,
        item: &AstItem,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match item {
            AstItem::Expr(expr) => self.emit_expr(expr, ctx),
            AstItem::DefFunction(f) => self.emit_def_function(f, ctx),
            _ => unimplemented!("emit_item: {:?}", item),
        }
    }
}
