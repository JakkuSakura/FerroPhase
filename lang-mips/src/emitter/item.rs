use eyre::Result;

use lang_core::ast::Item;
use lang_core::context::SharedScopedContext;

use crate::emitter::expr::MipsEmitExprResult;
use crate::emitter::MipsEmitter;

impl MipsEmitter {
    pub fn emit_item(
        &mut self,
        item: &Item,
        ctx: &SharedScopedContext,
    ) -> Result<MipsEmitExprResult> {
        match item {
            Item::Expr(expr) => self.emit_expr(expr, ctx),
            Item::DefFunction(f) => self.emit_function(f, ctx),
            _ => unimplemented!("emit_item: {:?}", item),
        }
    }
}
