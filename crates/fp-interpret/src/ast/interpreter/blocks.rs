use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn evaluate_function_body(&mut self, expr: &mut Expr) {
        let expr_ty_snapshot = expr.ty().cloned();
        let mut updated_expr_ty: Option<Ty> = None;

        match expr.kind_mut() {
            ExprKind::Quote(_quote) => {
                // Quoted fragments inside function analysis remain as-is until evaluated (e.g., by splice)
            }
            ExprKind::Splice(splice) => {
                if !self.in_const_region() {
                    self.emit_error("splice is only valid inside const { ... } regions");
                    return;
                }
                match splice.token.kind() {
                    ExprKind::Quote(q) => match self.build_quoted_fragment(q) {
                        QuotedFragment::Expr(e) => {
                            *expr = e;
                            self.mark_mutated();
                        }
                        QuotedFragment::Stmts(_)
                        | QuotedFragment::Items(_)
                        | QuotedFragment::Type(_) => {
                            self.emit_error(
                                "cannot splice non-expression token in expression position",
                            );
                        }
                    },
                    _ => {
                        self.emit_error("splice expects a quote token expression");
                    }
                }
            }
            ExprKind::IntrinsicContainer(collection) => {
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                self.evaluate_function_body(expr);
                return;
            }
            ExprKind::Block(block) => self.evaluate_function_block(block),
            ExprKind::If(if_expr) => {
                self.evaluate_function_body(if_expr.then.as_mut());
                if let Some(else_expr) = if_expr.elze.as_mut() {
                    self.evaluate_function_body(else_expr);
                }
                self.evaluate_function_body(if_expr.cond.as_mut());
            }
            ExprKind::Loop(loop_expr) => self.evaluate_function_body(loop_expr.body.as_mut()),
            ExprKind::While(while_expr) => {
                self.evaluate_function_body(while_expr.cond.as_mut());
                self.evaluate_function_body(while_expr.body.as_mut());
            }
            ExprKind::Match(match_expr) => {
                for case in match_expr.cases.iter_mut() {
                    self.evaluate_function_body(case.cond.as_mut());
                    self.evaluate_function_body(case.body.as_mut());
                }
            }
            ExprKind::Let(expr_let) => self.evaluate_function_body(expr_let.expr.as_mut()),
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Function(locator_ref) = &invoke.target {
                    if let Some(ident) = locator_ref.as_ident() {
                        if self.lookup_closure(ident.as_str()).is_some() {
                            let mut cloned_expr = Expr::new(ExprKind::Invoke(invoke.clone()));
                            if let Some(ref ty) = expr_ty_snapshot {
                                cloned_expr.set_ty(ty.clone());
                            }
                            let saved_mutated = self.mutations_applied;
                            let saved_stdout_len = self.stdout.len();
                            let _ = self.eval_expr(&mut cloned_expr);
                            if let Some(ty) = self.pending_expr_ty.take() {
                                updated_expr_ty = Some(ty);
                            }
                            self.mutations_applied = saved_mutated;
                            if self.stdout.len() > saved_stdout_len {
                                self.stdout.truncate(saved_stdout_len);
                            }
                        }
                    }
                }

                if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
                    let _ = self.resolve_function_call(locator, &mut invoke.args);
                }

                match &mut invoke.target {
                    ExprInvokeTarget::Expr(target) => self.evaluate_function_body(target.as_mut()),
                    ExprInvokeTarget::Method(select) => {
                        self.evaluate_function_body(select.obj.as_mut())
                    }
                    ExprInvokeTarget::Closure(closure) => {
                        self.evaluate_function_body(closure.body.as_mut())
                    }
                    ExprInvokeTarget::Function(_)
                    | ExprInvokeTarget::Type(_)
                    | ExprInvokeTarget::BinOp(_) => {}
                }

                for arg in invoke.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
            }
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` should have been lowered before const evaluation",
                    macro_expr.invocation.path
                ));
            }
            ExprKind::IntrinsicCall(call) => {
                if matches!(call.kind, IntrinsicCallKind::ConstBlock) {
                    // Enter const region and analyze the block body for splices/quotes
                    if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                        if let Some(body) = args.first_mut() {
                            self.enter_const_region();
                            self.evaluate_function_body(body);
                            self.exit_const_region();
                        }
                    }
                }
                self.evaluate_intrinsic_for_function_analysis(call);
            }
            ExprKind::Await(await_expr) => {
                self.evaluate_function_body(await_expr.base.as_mut());
            }
            ExprKind::Reference(reference) => {
                self.evaluate_function_body(reference.referee.as_mut());
            }
            ExprKind::Paren(paren) => self.evaluate_function_body(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.evaluate_function_body(assign.target.as_mut());
                self.evaluate_function_body(assign.value.as_mut());
            }
            ExprKind::Select(select) => self.evaluate_function_body(select.obj.as_mut()),
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(inner) => self.evaluate_function_body(inner.as_mut()),
                Value::Function(function) => self.evaluate_function_body(function.body.as_mut()),
                _ => {}
            },
            ExprKind::Closured(closured) => self.evaluate_function_body(closured.expr.as_mut()),
            ExprKind::Closure(closure) => self.evaluate_function_body(closure.body.as_mut()),
            ExprKind::Try(expr_try) => self.evaluate_function_body(expr_try.expr.as_mut()),
            ExprKind::FormatString(template) => {
                for arg in template.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.evaluate_function_body(&mut kwarg.value);
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                self.evaluate_function_body(repeat.elem.as_mut());
                self.evaluate_function_body(repeat.len.as_mut());
            }
            ExprKind::Cast(cast) => self.evaluate_function_body(cast.expr.as_mut()),
            ExprKind::Locator(locator) => {
                // Try to specialize generic function references
                if let Some(expected_ty) = &expr_ty_snapshot {
                    if matches!(expected_ty, Ty::Function(_)) {
                        self.specialize_function_reference(locator, expected_ty);
                    }
                }
            }
            ExprKind::Item(item) => self.evaluate_item(item.as_mut()),
            ExprKind::Any(_)
            | ExprKind::Id(_)
            | ExprKind::UnOp(_)
            | ExprKind::BinOp(_)
            | ExprKind::Dereference(_)
            | ExprKind::Index(_)
            | ExprKind::Range(_)
            | ExprKind::Tuple(_)
            | ExprKind::Array(_)
            | ExprKind::Struct(_)
            | ExprKind::Structural(_)
            | ExprKind::Splat(_)
            | ExprKind::SplatDict(_) => {}
        }

        if let Some(ty) = updated_expr_ty {
            expr.set_ty(ty);
        }
    }

    pub(super) fn evaluate_function_block(&mut self, block: &mut ExprBlock) {
        self.push_scope();
        // Rebuild statements to allow splice expansion
        let mut new_stmts: Vec<BlockStmt> = Vec::with_capacity(block.stmts.len());
        for mut stmt in std::mem::take(&mut block.stmts) {
            match &mut stmt {
                BlockStmt::Expr(expr_stmt) => {
                    // Handle statement-position splice expansion
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                        if !self.in_const_region() {
                            self.emit_error("splice is only valid inside const { ... } regions");
                            // Keep original statement to avoid dropping code
                            new_stmts.push(stmt);
                        } else {
                            match splice.token.kind() {
                                ExprKind::Quote(q) => {
                                    match self.build_quoted_fragment(q) {
                                        QuotedFragment::Stmts(stmts) => {
                                            for s in stmts {
                                                new_stmts.push(s.clone());
                                            }
                                            self.mark_mutated();
                                        }
                                        QuotedFragment::Items(_items) => {
                                            // Forbid: item splicing inside function bodies is not supported
                                            // Suggest moving item emission to a module-level const region.
                                            self.emit_error(
                                            "item splicing is not allowed inside function bodies; move item emission to a module-level const block",
                                        );
                                            // Keep the original statement to avoid silently dropping code.
                                            new_stmts.push(stmt);
                                        }
                                        QuotedFragment::Expr(e) => {
                                            let mut es = expr_stmt.clone();
                                            es.expr = e.into();
                                            es.semicolon = Some(true);
                                            new_stmts.push(BlockStmt::Expr(es));
                                            self.mark_mutated();
                                        }
                                        QuotedFragment::Type(_) => {
                                            self.emit_error("cannot splice a type fragment at statement position");
                                            new_stmts.push(stmt);
                                        }
                                    }
                                }
                                _ => {
                                    self.emit_error("splice expects a quote token expression");
                                    new_stmts.push(stmt);
                                }
                            }
                        }
                    } else if let ExprKind::IntrinsicCall(call) = expr_stmt.expr.kind_mut() {
                        if matches!(call.kind, IntrinsicCallKind::ConstBlock) {
                            // Inline const { ... } statements into surrounding block
                            if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                                if let Some(body_expr) = args.first_mut() {
                                    if let ExprKind::Block(inner_block) = body_expr.kind_mut() {
                                        self.enter_const_region();
                                        self.evaluate_function_block(inner_block);
                                        self.exit_const_region();
                                        for s in inner_block.stmts.clone() {
                                            new_stmts.push(s);
                                        }
                                        self.mark_mutated();
                                        continue;
                                    }
                                }
                            }
                        }
                        // Generic case
                        self.evaluate_function_body(expr_stmt.expr.as_mut());
                        new_stmts.push(stmt);
                    } else {
                        // Generic case: analyze expression and keep statement
                        self.evaluate_function_body(expr_stmt.expr.as_mut());
                        new_stmts.push(stmt);
                    }
                }
                BlockStmt::Item(item) => {
                    self.evaluate_item(item.as_mut());
                    new_stmts.push(stmt);
                }
                BlockStmt::Let(let_stmt) => {
                    self.evaluate_function_let_stmt(let_stmt);
                    new_stmts.push(stmt);
                }
                BlockStmt::Noop | BlockStmt::Any(_) => new_stmts.push(stmt),
            }
        }
        block.stmts = new_stmts;
        if let Some(last) = block.last_expr_mut() {
            self.evaluate_function_body(last);
        }
        self.pop_scope();
    }

    // Removed: evaluate_function_stmt was unused and duplicative of evaluate_function_block logic.

    pub(super) fn evaluate_function_let_stmt(&mut self, stmt_let: &mut StmtLet) {
        if let Some(init) = stmt_let.init.as_mut() {
            if Self::expr_is_potential_closure(init) {
                self.evaluate_closure_initializer(&mut stmt_let.pat, init);
            }
            self.evaluate_function_body(init);
        }
        if let Some(diverge) = stmt_let.diverge.as_mut() {
            self.evaluate_function_body(diverge);
        }
    }

    pub(super) fn expr_is_potential_closure(expr: &Expr) -> bool {
        expr.ty()
            .map(|ty| matches!(ty, Ty::Function(_)))
            .unwrap_or(false)
            || matches!(expr.kind(), ExprKind::Closure(_))
    }

    pub(super) fn evaluate_closure_initializer(&mut self, pat: &mut Pattern, init: &mut Expr) {
        let mut expr_clone = init.clone();
        let saved_mutated = self.mutations_applied;
        let saved_pending = self.pending_closure.take();
        let saved_stdout_len = self.stdout.len();
        let value = self.eval_expr(&mut expr_clone);
        if self.pending_closure.is_some() {
            self.annotate_pending_closure(Some(pat), Some(init));
        }
        self.bind_pattern(pat, value);
        self.mutations_applied = saved_mutated;
        self.pending_closure = saved_pending;
        if self.stdout.len() > saved_stdout_len {
            self.stdout.truncate(saved_stdout_len);
        }
    }
}
