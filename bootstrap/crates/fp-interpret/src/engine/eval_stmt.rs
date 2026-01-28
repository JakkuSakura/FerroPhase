use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    // Evaluate a single block statement during const-eval.
    // Returns Some(value) if the statement is an expression statement that yields a value.
    pub(super) fn eval_stmt(&mut self, stmt: &mut BlockStmt) -> Option<Value> {
        let span = match stmt {
            BlockStmt::Expr(expr_stmt) => expr_stmt.expr.span,
            BlockStmt::Let(stmt_let) => stmt_let
                .init
                .as_ref()
                .and_then(|expr| expr.span),
            BlockStmt::Item(item) => item.span,
            BlockStmt::Noop | BlockStmt::Any(_) => None,
        };
        let _guard = self.push_span(span);
        match stmt {
            BlockStmt::Expr(expr_stmt) => {
                if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                    if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime)
                    {
                        self.emit_error("splice is only supported during const evaluation");
                        return None;
                    }
                    let Some(fragments) = self.resolve_splice_fragments(splice.token.as_mut())
                    else {
                        return None;
                    };
                    let mut pending_items = Vec::new();
                    let mut pending_stmts = Vec::new();
                    let mut pending_exprs = Vec::new();
                    for fragment in fragments {
                        match fragment {
                            QuotedFragment::Items(items) => pending_items.extend(items),
                            QuotedFragment::Stmts(stmts) => pending_stmts.extend(stmts),
                            QuotedFragment::Expr(expr) => pending_exprs.push(expr),
                            QuotedFragment::Type(_) => {
                                self.emit_error("splice<type> is not valid in statement position");
                                return None;
                            }
                        }
                    }
                    if !pending_items.is_empty() {
                        if self.value_env.len() > 1 && self.type_env.len() > 1 {
                            let current_values = self.value_env.pop().unwrap();
                            let current_types = self.type_env.pop().unwrap();
                            for item in pending_items.iter_mut() {
                                self.evaluate_item(item);
                            }
                            self.value_env.push(current_values);
                            self.type_env.push(current_types);
                        } else {
                            for item in pending_items.iter_mut() {
                                self.evaluate_item(item);
                            }
                        }
                        self.append_pending_items(pending_items);
                        self.mark_mutated();
                    }
                    if !pending_stmts.is_empty() {
                        let mut block = ExprBlock::new_stmts(pending_stmts);
                        self.eval_block(&mut block);
                    }
                    if !pending_exprs.is_empty() {
                        for mut expr in pending_exprs {
                            let _ = self.eval_expr(&mut expr);
                        }
                    }
                    return None;
                }
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
