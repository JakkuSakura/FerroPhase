use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    // Evaluate a single block statement during const-eval.
    // Returns Some(value) if the statement is an expression statement that yields a value.
    pub(super) fn eval_stmt(&mut self, stmt: &mut BlockStmt) -> Option<Value> {
        match stmt {
            BlockStmt::Expr(expr_stmt) => {
                let value = self.eval_expr(expr_stmt.expr.as_mut());
                if expr_stmt.has_value() {
                    Some(value)
                } else {
                    None
                }
            }
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    let value = self.eval_expr(init);
                    // If a closure is pending, annotate its types onto the pattern/init
                    if self.pending_closure.is_some() {
                        let expr_ref = stmt_let.init.as_mut();
                        self.annotate_pending_closure(Some(&mut stmt_let.pat), expr_ref);
                    }
                    self.bind_pattern(&stmt_let.pat, value);
                } else {
                    self.emit_error(
                        "let bindings without initializer are not supported in const blocks",
                    );
                }
                None
            }
            BlockStmt::Item(item) => {
                self.evaluate_item(item.as_mut());
                None
            }
            BlockStmt::Noop => None,
            BlockStmt::Any(_) => {
                self.emit_error("unsupported statement in const block");
                None
            }
        }
    }
}
