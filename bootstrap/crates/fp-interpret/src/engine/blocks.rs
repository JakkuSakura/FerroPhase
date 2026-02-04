use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn evaluate_function_body(&mut self, expr: &mut Expr) {
        let expr_ty_snapshot = expr.ty().cloned();
        let mut updated_expr_ty: Option<Ty> = None;

        if self.try_fold_runtime_const_collection_expr(expr) {
            return;
        }

        match expr.kind_mut() {
            ExprKind::Quote(_quote) => {
                // Quoted fragments inside function analysis remain as-is until evaluated (e.g., by splice)
            }
            ExprKind::Splice(splice) => {
                if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime) {
                    self.emit_error("splice is only supported during const evaluation");
                    return;
                }
                let Some(mut fragments) = self.resolve_splice_fragments(splice.token.as_mut())
                else {
                    return;
                };
                if fragments.len() != 1 {
                    self.emit_error("splice in expression position expects one fragment");
                    return;
                }
                match fragments.remove(0) {
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
            ExprKind::For(for_expr) => {
                // Syntax-only traversal: visit the iterator expression first, then the loop body,
                // so const/quote/splice constructs are not missed.
                self.evaluate_function_body(for_expr.iter.as_mut());
                self.evaluate_function_body(for_expr.body.as_mut());
            }
            ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.evaluate_function_body(value);
                }
            }
            ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.evaluate_function_body(value);
                }
            }
            ExprKind::Continue(_) => {}
            ExprKind::Name(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        let mut replacement = Expr::value(value);
                        replacement.ty = expr.ty.clone();
                        *expr = replacement;
                        self.mark_mutated();
                    }
                }
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

                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    let mut locator = locator.clone();
                    if let Some(function) =
                        self.resolve_function_call(&mut locator, invoke, ResolutionMode::Default)
                    {
                        invoke.target = ExprInvokeTarget::Function(locator.clone());
                        let const_params: Vec<_> = function
                            .sig
                            .params
                            .iter()
                            .enumerate()
                            .filter(|(_, param)| param.is_const)
                            .collect();
                        if !const_params.is_empty() {
                            let mut const_values = Vec::new();
                            let mut runtime_args = Vec::new();
                            let mut args = std::mem::take(&mut invoke.args);
                            for (idx, param) in function.sig.params.iter().enumerate() {
                                let Some(arg) = args.get_mut(idx) else {
                                    break;
                                };
                                if param.is_const {
                                    let value = self.eval_expr(arg);
                                    if matches!(value, Value::Undefined(_) | Value::Any(_)) {
                                        self.emit_error(format!(
                                            "const argument `{}` must be a compile-time value",
                                            param.name.as_str()
                                        ));
                                        continue;
                                    }
                                    const_values.push((param.clone(), value));
                                } else {
                                    runtime_args.push(args[idx].clone());
                                }
                            }

                            if const_values.len() == const_params.len() {
                                let base_name = function.name.as_str().to_string();
                                let const_key = const_values
                                    .iter()
                                    .map(|(_, value)| format!("{:?}", value))
                                    .collect::<Vec<_>>()
                                    .join("|");
                                let specialization_name = if let Some(cache) =
                                    self.specialization_cache.get(&base_name)
                                {
                                    cache.get(&const_key).cloned()
                                } else {
                                    None
                                };
                                let specialization_name = if let Some(name) = specialization_name {
                                    name
                                } else {
                                    let counter = self
                                        .specialization_counter
                                        .entry(base_name.clone())
                                        .or_default();
                                    *counter += 1;
                                    let name = format!("{}__const_{}", base_name, *counter);
                                    self.specialization_cache
                                        .entry(base_name.clone())
                                        .or_default()
                                        .insert(const_key.clone(), name.clone());

                                    let mut specialized = function.clone();
                                    specialized.name = Ident::new(name.clone());
                                    specialized.sig.params = specialized
                                        .sig
                                        .params
                                        .into_iter()
                                        .filter(|param| !param.is_const)
                                        .collect();

                                    self.push_scope();
                                    for (param, value) in &const_values {
                                        if let Some(scope) = self.type_env.last_mut() {
                                            scope.insert(
                                                param.name.as_str().to_string(),
                                                param.ty.clone(),
                                            );
                                        }
                                        self.insert_value(param.name.as_str(), value.clone());
                                    }
                                    self.evaluate_function_body(specialized.body.as_mut());
                                    self.pop_scope();

                                    self.functions.insert(name.clone(), specialized.clone());
                                    self.append_pending_items(vec![Item::new(
                                        ItemKind::DefFunction(specialized),
                                    )]);
                                    name
                                };

                                match &mut locator {
                                    Name::Ident(ident) => {
                                        *ident = Ident::new(specialization_name.clone());
                                    }
                                    Name::Path(path) => {
                                        if let Some(last) = path.segments.last_mut() {
                                            *last = Ident::new(specialization_name.clone());
                                        }
                                    }
                                    Name::ParameterPath(path) => {
                                        if let Some(last) = path.segments.last_mut() {
                                            last.ident = Ident::new(specialization_name.clone());
                                            last.args.clear();
                                        }
                                    }
                                }

                                invoke.target = ExprInvokeTarget::Function(locator.clone());

                                invoke.args = runtime_args;
                            } else {
                                invoke.args = args;
                            }
                        }
                    }
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
                let parser = match self.macro_parser.clone() {
                    Some(parser) => parser,
                    None => {
                        self.emit_error_at(
                            macro_expr.invocation.span,
                            "macro expansion requires a parser hook",
                        );
                        return;
                    }
                };
                if self.macro_depth > 64 {
                    self.emit_error_at(
                        macro_expr.invocation.span,
                        "macro expansion exceeded recursion limit",
                    );
                    return;
                }
                self.macro_depth += 1;
                let expanded = self
                    .expand_macro_invocation(&macro_expr.invocation, MacroExpansionContext::Expr)
                    .and_then(|tokens| parser.parse_expr(&tokens));
                self.macro_depth = self.macro_depth.saturating_sub(1);
                match expanded {
                    Ok(new_expr) => {
                        *expr = new_expr;
                        self.mark_mutated();
                        self.evaluate_function_body(expr);
                        return;
                    }
                    Err(err) => {
                        self.emit_error_at(macro_expr.invocation.span, err.to_string());
                        return;
                    }
                }
            }
            ExprKind::ConstBlock(const_block) => {
                let mut value_expr = const_block.expr.as_ref().clone();
                self.enter_const_region();
                let value = self.eval_expr(&mut value_expr);
                self.exit_const_region();
                let mut replacement = Expr::value(value.clone());
                if let Some(ty) = expr_ty_snapshot.clone() {
                    replacement.ty = Some(ty);
                }
                *expr = replacement;
                self.mark_mutated();
                return;
            }
            ExprKind::IntrinsicCall(call) => {
                if self.should_replace_intrinsic_with_value(call.kind, &Value::unit()) {
                    let value = self.eval_intrinsic(call);
                    if !matches!(value, Value::Undefined(_)) {
                        let mut replacement = Expr::value(value.clone());
                        if let Some(ty) = expr_ty_snapshot.clone() {
                            replacement.ty = Some(ty);
                        }
                        *expr = replacement;
                        self.mark_mutated();
                        return;
                    }
                }
                self.evaluate_intrinsic_for_function_analysis(call);
            }
            ExprKind::Await(await_expr) => {
                self.evaluate_function_body(await_expr.base.as_mut());
            }
            ExprKind::Async(async_expr) => {
                // `async` is a syntactic marker at the AST layer; keep traversing the inner expr.
                self.evaluate_function_body(async_expr.expr.as_mut());
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
                let _ = template;
            }
            ExprKind::ArrayRepeat(repeat) => {
                self.evaluate_function_body(repeat.elem.as_mut());
                self.evaluate_function_body(repeat.len.as_mut());
            }
            ExprKind::Cast(cast) => self.evaluate_function_body(cast.expr.as_mut()),
            ExprKind::Name(locator) => {
                // Try to specialize generic function references
                if let Some(expected_ty) = &expr_ty_snapshot {
                    if matches!(expected_ty, Ty::Function(_)) {
                        self.specialize_function_reference(locator, expected_ty);
                    }
                }
            }
            ExprKind::Item(item) => self.evaluate_item(item.as_mut()),
            ExprKind::UnOp(unop) => self.evaluate_function_body(unop.val.as_mut()),
            ExprKind::BinOp(binop) => {
                self.evaluate_function_body(binop.lhs.as_mut());
                self.evaluate_function_body(binop.rhs.as_mut());
            }
            ExprKind::Dereference(deref) => self.evaluate_function_body(deref.referee.as_mut()),
            ExprKind::Index(index) => {
                self.evaluate_function_body(index.obj.as_mut());
                self.evaluate_function_body(index.index.as_mut());
            }
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.evaluate_function_body(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.evaluate_function_body(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.evaluate_function_body(step.as_mut());
                }
            }
            ExprKind::Tuple(tuple) => {
                for value in tuple.values.iter_mut() {
                    self.evaluate_function_body(value);
                }
            }
            ExprKind::Array(array) => {
                for value in array.values.iter_mut() {
                    self.evaluate_function_body(value);
                }
            }
            ExprKind::Struct(strukt) => {
                self.evaluate_function_body(strukt.name.as_mut());
                for field in strukt.fields.iter_mut() {
                    if let Some(value) = field.value.as_mut() {
                        self.evaluate_function_body(value);
                    }
                }
            }
            ExprKind::Structural(structural) => {
                for field in structural.fields.iter_mut() {
                    if let Some(value) = field.value.as_mut() {
                        self.evaluate_function_body(value);
                    }
                }
            }
            ExprKind::Splat(splat) => self.evaluate_function_body(splat.iter.as_mut()),
            ExprKind::SplatDict(splat) => self.evaluate_function_body(splat.dict.as_mut()),
            ExprKind::Any(_) | ExprKind::Id(_) => {}
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
                        if !self.in_const_region()
                            && !matches!(self.mode, InterpreterMode::CompileTime)
                        {
                            self.emit_error("splice is only supported during const evaluation");
                            // Keep original statement to avoid dropping code
                            new_stmts.push(stmt);
                        } else {
                            let Some(fragments) =
                                self.resolve_splice_fragments(splice.token.as_mut())
                            else {
                                new_stmts.push(stmt);
                                continue;
                            };
                            for fragment in fragments {
                                match fragment {
                                    QuotedFragment::Stmts(stmts) => {
                                        for s in stmts {
                                            new_stmts.push(s.clone());
                                        }
                                        self.mark_mutated();
                                    }
                                    QuotedFragment::Expr(e) => {
                                        let mut es = expr_stmt.clone();
                                        es.expr = e.into();
                                        es.semicolon = Some(true);
                                        new_stmts.push(BlockStmt::Expr(es));
                                        self.mark_mutated();
                                    }
                                    QuotedFragment::Items(items) => {
                                        for item in items {
                                            new_stmts.push(BlockStmt::Item(Box::new(item)));
                                        }
                                        self.mark_mutated();
                                    }
                                    QuotedFragment::Type(_) => {
                                        self.emit_error(
                                            "splice<type> is not valid in statement position",
                                        );
                                    }
                                }
                            }
                        }
                    } else if let ExprKind::ConstBlock(const_block) = expr_stmt.expr.kind_mut() {
                        // Evaluate const { ... } and splice generated statements into the block.
                        if let ExprKind::Block(inner_block) = const_block.expr.kind_mut() {
                            self.pending_stmt_splices.push(Vec::new());
                            self.enter_const_region();
                            let flow = self.eval_block_runtime(inner_block);
                            self.exit_const_region();
                            let pending = self.pending_stmt_splices.pop().unwrap_or_default();
                            match flow {
                                RuntimeFlow::Value(_) => {}
                                RuntimeFlow::Break(_)
                                | RuntimeFlow::Continue
                                | RuntimeFlow::Return(_)
                                | RuntimeFlow::Panic(_) => {
                                    self.emit_error(
                                        "control flow is not allowed at top-level of const blocks",
                                    );
                                }
                            }
                            if !pending.is_empty() {
                                new_stmts.extend(pending);
                            }
                            self.mark_mutated();
                            continue;
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
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && is_quote_only_item(item.as_ref())
                    {
                        self.mark_mutated();
                    } else {
                        new_stmts.push(stmt);
                    }
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
