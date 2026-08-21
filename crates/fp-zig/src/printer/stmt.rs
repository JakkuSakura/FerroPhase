use eyre::Result;
use fp_core::ast::{BlockStmt, ExprBlock, ExprIntrinsicCall, ExprKind};
use fp_core::intrinsics::CallKind;

use super::ZigEmitter;

impl ZigEmitter {
    pub(super) fn emit_function_body(
        &mut self,
        def: &fp_core::ast::ItemDefFunction,
    ) -> Result<bool> {
        self.emit_block(&def.body)
    }

    fn emit_block(&mut self, block: &ExprBlock) -> Result<bool> {
        let mut emitted = false;
        for stmt in &block.stmts {
            if self.emit_stmt(stmt)? {
                emitted = true;
            }
        }
        Ok(emitted)
    }

    fn emit_stmt(&mut self, stmt: &BlockStmt) -> Result<bool> {
        match stmt {
            BlockStmt::Let(stmt_let) => {
                if let Some(ident) = stmt_let.pat.as_ident() {
                    if let Some(init) = &stmt_let.init {
                        if let Some(value) = self.render_expr(init) {
                            self.push_line(&format!("var {} = {};", ident.name, value));
                        } else {
                            self.push_comment("TODO: unsupported initializer in Zig backend");
                        }
                    } else {
                        self.push_line(&format!("var {} = undefined;", ident.name));
                    }
                } else {
                    self.push_comment(
                        "TODO: complex patterns in let bindings are not supported yet",
                    );
                }
                Ok(true)
            }
            BlockStmt::Expr(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref();
                if let ExprKind::Return(ret) = expr.kind() {
                    if let Some(value) = &ret.value {
                        if let Some(rendered) = self.render_expr(value) {
                            if rendered.is_empty() {
                                self.push_line("return;");
                            } else {
                                self.push_line(&format!("return {};", rendered));
                            }
                        } else {
                            self.push_line("return;");
                        }
                    } else {
                        self.push_line("return;");
                    }
                    return Ok(true);
                }
                if let ExprKind::Break(_) = expr.kind() {
                    self.push_line("break;");
                    return Ok(true);
                }
                if let ExprKind::Continue(_) = expr.kind() {
                    self.push_line("continue;");
                    return Ok(true);
                }
                if let ExprKind::IntrinsicCall(call) = expr.kind() {
                    if self.emit_intrinsic_statement(call)? {
                        return Ok(true);
                    }
                }

                if let ExprKind::Block(block_expr) = expr.kind() {
                    return self.emit_block(block_expr);
                }

                if let Some(rendered) = self.render_expr(expr) {
                    if expr_stmt.has_value() {
                        if rendered.is_empty() {
                            self.push_line("return;");
                        } else {
                            self.push_line(&format!("return {};", rendered));
                        }
                    } else if !rendered.is_empty() {
                        self.push_line(&format!("{};", rendered));
                    }
                    return Ok(true);
                }

                if expr_stmt.has_value() {
                    self.push_line("return;");
                    return Ok(true);
                }

                self.push_comment("TODO: unsupported expression statement in Zig backend");
                Ok(true)
            }
            BlockStmt::Item(item) => {
                self.emit_item(item.as_ref())?;
                Ok(true)
            }
            BlockStmt::Defer(_) => {
                self.push_comment("TODO: defer statements are not supported in Zig backend");
                Ok(true)
            }
            BlockStmt::Noop => Ok(false),
        }
    }

    fn emit_intrinsic_statement(&mut self, call: &ExprIntrinsicCall) -> Result<bool> {
        match call.kind {
            CallKind::Print | CallKind::Println => {
                let newline = matches!(call.kind, CallKind::Println);
                if let Some(rendered) = self.render_print_call(call, newline) {
                    self.push_line(&rendered);
                    return Ok(true);
                }
            }
            _ => {}
        }
        Ok(false)
    }
}
