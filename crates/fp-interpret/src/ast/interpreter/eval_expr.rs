use super::*;
use fp_core::ast::Locator;

impl<'ctx> AstInterpreter<'ctx> {
    /// Evaluate an expression in runtime-capable mode, returning structured control-flow.
    pub(super) fn eval_expr_runtime(&mut self, expr: &mut Expr) -> RuntimeFlow {
        let expr_ty_snapshot = expr.ty().cloned();
        match expr.kind_mut() {
            ExprKind::IntrinsicContainer(collection) => {
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                self.eval_expr_runtime(expr)
            }
            ExprKind::Quote(quote) => {
                if !self.in_const_region() {
                    self.emit_error("quote is only supported in const regions");
                    return RuntimeFlow::Value(Value::undefined());
                }
                let kind = quote.kind;
                let fragment = self.build_quoted_fragment(quote);
                RuntimeFlow::Value(self.quote_token_from_fragment_kind(fragment, kind))
            }
            ExprKind::Splice(_splice) => {
                if !self.in_const_region() {
                    self.emit_error("splice is only supported in const regions");
                    return RuntimeFlow::Value(Value::undefined());
                }
                let ExprKind::Splice(splice) = expr.kind_mut() else {
                    return RuntimeFlow::Value(Value::undefined());
                };
                let Some(mut fragments) = self.resolve_splice_fragments(splice.token.as_mut())
                else {
                    return RuntimeFlow::Value(Value::undefined());
                };
                if fragments.len() != 1 {
                    self.emit_error("splice in expression position expects a single quote token");
                    return RuntimeFlow::Value(Value::undefined());
                }
                match fragments.remove(0) {
                    QuotedFragment::Expr(mut quoted) => self.eval_expr_runtime(&mut quoted),
                    QuotedFragment::Stmts(stmts) => {
                        let mut block = ExprBlock::new_stmts(stmts);
                        self.eval_block_runtime(&mut block)
                    }
                    QuotedFragment::Items(_) | QuotedFragment::Type(_) => {
                        self.emit_error("cannot splice non-expression token in expression position");
                        RuntimeFlow::Value(Value::undefined())
                    }
                }
            }
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(inner) => {
                    let mut cloned = inner.as_ref().clone();
                    self.eval_expr_runtime(&mut cloned)
                }
                other => RuntimeFlow::Value(other.clone()),
            },
            ExprKind::Locator(locator) => {
                if let Some(variant) = self.resolve_enum_variant(locator) {
                    return RuntimeFlow::Value(variant);
                }
                if let Some(expected_ty) = expr_ty_snapshot.clone() {
                    if matches!(expected_ty, Ty::Function(_)) {
                        self.specialize_function_reference(locator, &expected_ty);
                    }
                }
                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        if let Some(placeholder) = self.imported_placeholder_value(value.clone()) {
                            return RuntimeFlow::Value(placeholder);
                        }
                        return RuntimeFlow::Value(value);
                    }
                }
                if let Some(function) = self.functions.get(&locator.to_string()) {
                    return RuntimeFlow::Value(Value::Function(ValueFunction {
                        sig: function.sig.clone(),
                        body: function.body.clone(),
                    }));
                }
                let mut candidate_names = vec![locator.to_string()];
                if let Some(ident) = locator.as_ident() {
                    candidate_names.push(ident.as_str().to_string());
                }
                for name in candidate_names {
                    if let Some(template) = self.generic_functions.get(&name) {
                        return RuntimeFlow::Value(Value::Function(ValueFunction {
                            sig: template.function.sig.clone(),
                            body: template.function.body.clone(),
                        }));
                    }
                }
                RuntimeFlow::Value(self.resolve_qualified(locator.to_string()))
            }
            ExprKind::BinOp(binop) => {
                let lhs = match self.eval_value_runtime(binop.lhs.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                let rhs = match self.eval_value_runtime(binop.rhs.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                RuntimeFlow::Value(self.handle_result(self.evaluate_binop(binop.kind, lhs, rhs)))
            }
            ExprKind::UnOp(unop) => {
                let value = match self.eval_value_runtime(unop.val.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                RuntimeFlow::Value(self.handle_result(self.evaluate_unary(unop.op.clone(), value)))
            }
            ExprKind::If(if_expr) => {
                let cond = match self.eval_value_runtime(if_expr.cond.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                match cond {
                    Value::Bool(b) => {
                        if b.value {
                            self.eval_expr_runtime(if_expr.then.as_mut())
                        } else if let Some(else_) = if_expr.elze.as_mut() {
                            self.eval_expr_runtime(else_)
                        } else {
                            RuntimeFlow::Value(Value::unit())
                        }
                    }
                    _ => {
                        self.emit_error("expected boolean condition in runtime expression");
                        RuntimeFlow::Value(Value::undefined())
                    }
                }
            }
            ExprKind::Match(expr_match) => {
                if let Some(scrutinee) = expr_match.scrutinee.as_mut() {
                    let scrutinee_value = match self.eval_value_runtime(scrutinee.as_mut()) {
                        Ok(value) => value,
                        Err(flow) => return flow,
                    };
                    for case in &mut expr_match.cases {
                        self.push_scope();
                        let pat_matches = case
                            .pat
                            .as_ref()
                            .map(|pat| self.pattern_matches(pat, &scrutinee_value))
                            .unwrap_or(false);

                        if pat_matches {
                            if let Some(guard) = case.guard.as_mut() {
                                let guard_value = match self.eval_value_runtime(guard.as_mut()) {
                                    Ok(value) => value,
                                    Err(flow) => {
                                        self.pop_scope();
                                        return flow;
                                    }
                                };
                                match guard_value {
                                    Value::Bool(b) if b.value => {
                                        let out = self.eval_expr_runtime(case.body.as_mut());
                                        self.pop_scope();
                                        return out;
                                    }
                                    Value::Bool(_) => {}
                                    _ => {
                                        self.emit_error(
                                            "expected boolean match guard in runtime expression",
                                        );
                                        self.pop_scope();
                                        return RuntimeFlow::Value(Value::undefined());
                                    }
                                }
                            } else {
                                let out = self.eval_expr_runtime(case.body.as_mut());
                                self.pop_scope();
                                return out;
                            }
                        }
                        self.pop_scope();
                    }
                    return RuntimeFlow::Value(Value::unit());
                }

                for case in &mut expr_match.cases {
                    let cond = match self.eval_value_runtime(case.cond.as_mut()) {
                        Ok(value) => value,
                        Err(flow) => return flow,
                    };
                    match cond {
                        Value::Bool(b) => {
                            if b.value {
                                return self.eval_expr_runtime(case.body.as_mut());
                            }
                        }
                        _ => {
                            self.emit_error("expected boolean match condition in runtime expression");
                            return RuntimeFlow::Value(Value::undefined());
                        }
                    }
                }
                RuntimeFlow::Value(Value::unit())
            }
            ExprKind::Block(block) => self.eval_block_runtime(block),
            ExprKind::Tuple(tuple) => {
                let mut values = Vec::with_capacity(tuple.values.len());
                for expr in tuple.values.iter_mut() {
                    match self.eval_value_runtime(expr) {
                        Ok(value) => values.push(value),
                        Err(flow) => return flow,
                    }
                }
                RuntimeFlow::Value(Value::Tuple(ValueTuple::new(values)))
            }
            ExprKind::Array(array) => {
                let mut values = Vec::with_capacity(array.values.len());
                for expr in array.values.iter_mut() {
                    match self.eval_value_runtime(expr) {
                        Ok(value) => values.push(value),
                        Err(flow) => return flow,
                    }
                }
                RuntimeFlow::Value(Value::List(ValueList::new(values)))
            }
            ExprKind::ArrayRepeat(repeat) => {
                RuntimeFlow::Value(self.eval_array_repeat_runtime(repeat))
            }
            ExprKind::Range(range) => {
                let start = match range.start.as_mut() {
                    Some(expr) => match self.eval_value_runtime(expr) {
                        Ok(value) => value,
                        Err(flow) => return flow,
                    },
                    None => Value::int(0),
                };
                let end = match range.end.as_mut() {
                    Some(expr) => match self.eval_value_runtime(expr) {
                        Ok(value) => value,
                        Err(flow) => return flow,
                    },
                    None => Value::int(0),
                };
                let (start, end) = match (start, end) {
                    (Value::Int(start), Value::Int(end)) => (start.value, end.value),
                    _ => {
                        self.emit_error("range bounds must be integers");
                        return RuntimeFlow::Value(Value::undefined());
                    }
                };
                let mut values = Vec::new();
                let mut current = start;
                let inclusive = matches!(range.limit, fp_core::ast::ExprRangeLimit::Inclusive);
                while if inclusive { current <= end } else { current < end } {
                    values.push(Value::int(current));
                    current += 1;
                }
                RuntimeFlow::Value(Value::List(ValueList::new(values)))
            }
            ExprKind::Struct(struct_expr) => {
                RuntimeFlow::Value(self.evaluate_struct_literal_runtime(struct_expr))
            }
            ExprKind::Structural(struct_expr) => {
                let mut fields = Vec::with_capacity(struct_expr.fields.len());
                for field in struct_expr.fields.iter_mut() {
                    let value = if let Some(expr) = field.value.as_mut() {
                        match self.eval_value_runtime(expr) {
                            Ok(value) => value,
                            Err(flow) => return flow,
                        }
                    } else {
                        self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                            self.emit_error(format!(
                                "missing initializer for field '{}' in structural literal",
                                field.name
                            ));
                            Value::undefined()
                        })
                    };
                    fields.push(ValueField::new(field.name.clone(), value));
                }
                RuntimeFlow::Value(Value::Structural(ValueStructural::new(fields)))
            }
            ExprKind::Select(select) => {
                let target = match self.eval_value_runtime(select.obj.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                RuntimeFlow::Value(self.evaluate_select(target, &select.field.name))
            }
            ExprKind::Index(index_expr) => {
                let target = match self.eval_value_runtime(index_expr.obj.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                let index_value = match self.eval_value_runtime(index_expr.index.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                RuntimeFlow::Value(self.evaluate_index(target, index_value))
            }
            ExprKind::IntrinsicCall(call) => self.eval_intrinsic_runtime(call),
            ExprKind::Reference(reference) => self.eval_expr_runtime(reference.referee.as_mut()),
            ExprKind::Paren(paren) => self.eval_expr_runtime(paren.expr.as_mut()),
            ExprKind::Assign(assign) => self.eval_assign_runtime(assign),
            ExprKind::Let(expr_let) => {
                let value = match self.eval_value_runtime(expr_let.expr.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                self.bind_pattern(&expr_let.pat, value.clone());
                RuntimeFlow::Value(value)
            }
            ExprKind::Invoke(invoke) => self.eval_invoke_runtime_flow(invoke),
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` should have been lowered before runtime evaluation",
                    macro_expr.invocation.path
                ));
                RuntimeFlow::Value(Value::undefined())
            }
            ExprKind::Closure(closure) => {
                RuntimeFlow::Value(self.capture_runtime_closure(closure, expr_ty_snapshot.clone()))
            }
            ExprKind::Closured(closured) => {
                let mut inner = closured.expr.as_ref().clone();
                self.eval_expr_runtime(&mut inner)
            }
            ExprKind::Cast(cast) => {
                let target_ty = cast.ty.clone();
                let value = match self.eval_value_runtime(cast.expr.as_mut()) {
                    Ok(value) => value,
                    Err(flow) => return flow,
                };
                let result = self.cast_value_to_type(value, &target_ty);
                expr.set_ty(target_ty);
                RuntimeFlow::Value(result)
            }
            ExprKind::Loop(loop_expr) => self.eval_loop_runtime(loop_expr),
            ExprKind::While(while_expr) => self.eval_while_runtime(while_expr),
            ExprKind::For(for_expr) => self.eval_for_runtime(for_expr),
            ExprKind::Try(expr_try) => self.eval_expr_runtime(expr_try.expr.as_mut()),
            ExprKind::FormatString(template) => {
                if let Ok(output) = self.render_format_template_runtime(template) {
                    self.stdout.push(output);
                }
                RuntimeFlow::Value(Value::unit())
            }
            ExprKind::Any(_any) => {
                self.emit_error(
                    "expression not supported in runtime interpretation: unsupported Raw expression",
                );
                RuntimeFlow::Value(Value::undefined())
            }
            other => {
                self.emit_error(format!(
                    "expression not supported in runtime interpretation: {:?}",
                    other
                ));
                RuntimeFlow::Value(Value::undefined())
            }
        }
    }

    fn eval_value_runtime(&mut self, expr: &mut Expr) -> std::result::Result<Value, RuntimeFlow> {
        match self.eval_expr_runtime(expr) {
            RuntimeFlow::Value(value) => Ok(value),
            other => Err(other),
        }
    }

    pub(super) fn eval_expr(&mut self, expr: &mut Expr) -> Value {
        let expr_ty_snapshot = expr.ty().cloned();

        // Check if we need to specialize a function reference before matching
        let should_specialize_fn_ref = if let ExprKind::Locator(_) = expr.kind() {
            expr_ty_snapshot
                .as_ref()
                .map_or(false, |ty| matches!(ty, Ty::Function(_)))
        } else {
            false
        };

        match expr.kind_mut() {
            ExprKind::IntrinsicContainer(collection) => {
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                return self.eval_expr(expr);
            }
            ExprKind::Quote(_quote) => {
                if matches!(self.mode, InterpreterMode::CompileTime) {
                    if let ExprKind::Quote(quote) = expr.kind() {
                        let kind = quote.kind;
                        let fragment = self.build_quoted_fragment(quote);
                        return self.quote_token_from_fragment_kind(fragment, kind);
                    }
                    return Value::undefined();
                }
                self.emit_error("quote cannot be evaluated at runtime");
                return Value::undefined();
            }
            ExprKind::Splice(_splice) => {
                if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime) {
                    self.emit_error("splice is only supported during const evaluation");
                    return Value::undefined();
                }
                let ExprKind::Splice(splice) = expr.kind_mut() else {
                    return Value::undefined();
                };
                let Some(fragments) = self.resolve_splice_fragments(splice.token.as_mut()) else {
                    return Value::undefined();
                };
                if fragments.len() != 1 {
                    self.emit_error("splice in expression position expects a single fragment");
                    return Value::undefined();
                }
                match fragments.into_iter().next().unwrap() {
                    QuotedFragment::Expr(mut quoted) => self.eval_expr(&mut quoted),
                    QuotedFragment::Stmts(stmts) => {
                        let mut block = ExprBlock::new_stmts(stmts);
                        self.eval_block(&mut block)
                    }
                    QuotedFragment::Type(ty) => Value::Type(ty),
                    QuotedFragment::Items(_) => {
                        self.emit_error("splice<item> is not valid in expression position");
                        Value::undefined()
                    }
                }
            }
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(inner) => {
                    let mut cloned = inner.as_ref().clone();
                    self.eval_expr(&mut cloned)
                }
                Value::Type(ty) => Value::Type(self.materialize_const_type(ty.clone())),
                other => other.clone(),
            },
            ExprKind::Locator(locator) => {
                // First, try to specialize generic function reference if we have type info
                if should_specialize_fn_ref {
                    if let Some(expected_ty) = &expr_ty_snapshot {
                        tracing::debug!(
                            "Attempting to specialize function reference {} with type {:?}",
                            locator,
                            expected_ty
                        );
                        if let Some(_specialized) =
                            self.specialize_function_reference(locator, expected_ty)
                        {
                            tracing::debug!("Successfully specialized {} to {}", locator, locator);
                            // Locator has been updated to point to specialized function
                            // Return unit for now, the reference will be used by caller
                            return Value::unit();
                        } else {
                            tracing::debug!("Failed to specialize {}", locator);
                        }
                    }
                }

                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        if let Some(placeholder) = self.imported_placeholder_value(value.clone()) {
                            return placeholder;
                        }
                        if matches!(value, Value::List(_) | Value::Map(_)) {
                            return value;
                        }
                        let mut replacement = Expr::value(value.clone());
                        replacement.ty = expr.ty.clone();
                        // Type is already copied from original expr, no need to re-infer
                        *expr = replacement;
                        value
                    } else if let Some(closure) = self.lookup_closure(ident.as_str()) {
                        self.pending_closure = Some(closure);
                        Value::unit()
                    } else if let Some(ty) = self.lookup_type(ident.as_str()) {
                        Value::Type(ty)
                    } else {
                        let mut candidate_names = vec![locator.to_string(), ident.as_str().to_string()];
                        candidate_names.sort();
                        candidate_names.dedup();
                        for name in candidate_names {
                            if let Some(template) = self.generic_functions.get(&name) {
                                return Value::Function(ValueFunction {
                                    sig: template.function.sig.clone(),
                                    body: template.function.body.clone(),
                                });
                            }
                        }
                        self.resolve_qualified(locator.to_string())
                    }
                } else {
                    if let Some(template) = self.generic_functions.get(&locator.to_string()) {
                        return Value::Function(ValueFunction {
                            sig: template.function.sig.clone(),
                            body: template.function.body.clone(),
                        });
                    }
                    self.resolve_qualified(locator.to_string())
                }
            }
            ExprKind::BinOp(binop) => {
                let lhs = self.eval_expr(binop.lhs.as_mut());
                let rhs = self.eval_expr(binop.rhs.as_mut());
                self.handle_result(self.evaluate_binop(binop.kind, lhs, rhs))
            }
            ExprKind::UnOp(unop) => {
                let value = self.eval_expr(unop.val.as_mut());
                self.handle_result(self.evaluate_unary(unop.op.clone(), value))
            }
            ExprKind::If(if_expr) => {
                let cond = self.eval_expr(if_expr.cond.as_mut());
                match cond {
                    Value::Bool(b) => {
                        if b.value {
                            self.eval_expr(if_expr.then.as_mut())
                        } else if let Some(else_) = if_expr.elze.as_mut() {
                            self.eval_expr(else_)
                        } else {
                            Value::unit()
                        }
                    }
                    _ => {
                        self.emit_error("expected boolean condition in const expression");
                        Value::undefined()
                    }
                }
            }
            ExprKind::Match(expr_match) => {
                if let Some(scrutinee) = expr_match.scrutinee.as_mut() {
                    let scrutinee_value = self.eval_expr(scrutinee.as_mut());
                    for case in &mut expr_match.cases {
                        self.push_scope();

                        let pat_matches = case
                            .pat
                            .as_ref()
                            .map(|pat| self.pattern_matches(pat, &scrutinee_value))
                            .unwrap_or(false);

                        if pat_matches {
                            if let Some(guard) = case.guard.as_mut() {
                                let guard_value = self.eval_expr(guard.as_mut());
                                match guard_value {
                                    Value::Bool(b) if b.value => {
                                        let out = self.eval_expr(case.body.as_mut());
                                        self.pop_scope();
                                        return out;
                                    }
                                    Value::Bool(_) => {
                                        // Guard failed; fall through.
                                    }
                                    _ => {
                                        self.emit_error(
                                            "expected boolean match guard in const expression",
                                        );
                                        self.pop_scope();
                                        return Value::undefined();
                                    }
                                }
                            } else {
                                let out = self.eval_expr(case.body.as_mut());
                                self.pop_scope();
                                return out;
                            }
                        }

                        self.pop_scope();
                    }
                    return Value::unit();
                }

                // Legacy lowering: boolean conditions.
                for case in &mut expr_match.cases {
                    let cond = self.eval_expr(case.cond.as_mut());
                    match cond {
                        Value::Bool(b) => {
                            if b.value {
                                return self.eval_expr(case.body.as_mut());
                            }
                        }
                        _ => {
                            self.emit_error("expected boolean match condition in const expression");
                            return Value::undefined();
                        }
                    }
                }
                Value::unit()
            }
            ExprKind::Block(block) => self.eval_block(block),
            ExprKind::Tuple(tuple) => {
                let values = tuple
                    .values
                    .iter_mut()
                    .map(|expr| self.eval_expr(expr))
                    .collect();
                Value::Tuple(ValueTuple::new(values))
            }
            ExprKind::Array(array) => {
                let values = array
                    .values
                    .iter_mut()
                    .map(|expr| self.eval_expr(expr))
                    .collect();
                Value::List(ValueList::new(values))
            }
            ExprKind::ArrayRepeat(repeat) => self.eval_array_repeat(repeat),
            ExprKind::Struct(struct_expr) => self.evaluate_struct_literal(struct_expr),
            ExprKind::Structural(struct_expr) => {
                let fields = struct_expr
                    .fields
                    .iter_mut()
                    .map(|field| {
                        let value = field
                            .value
                            .as_mut()
                            .map(|expr| self.eval_expr(expr))
                            .unwrap_or_else(|| {
                                self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                                    self.emit_error(format!(
                                        "missing initializer for field '{}' in structural literal",
                                        field.name
                                    ));
                                    Value::undefined()
                                })
                            });
                        ValueField::new(field.name.clone(), value)
                    })
                    .collect();
                Value::Structural(ValueStructural::new(fields))
            }
            ExprKind::Select(select) => {
                let target = self.eval_expr(select.obj.as_mut());
                self.evaluate_select(target, &select.field.name)
            }
            ExprKind::Index(index_expr) => {
                let target = self.eval_expr(index_expr.obj.as_mut());
                if let ExprKind::Range(range) = index_expr.index.kind_mut() {
                    return self.evaluate_range_index(target, range);
                }
                let index_value = self.eval_expr(index_expr.index.as_mut());
                self.evaluate_index(target, index_value)
            }
            ExprKind::IntrinsicCall(call) => {
                let kind = call.kind;
                let mut assign_target: Option<String> = None;
                if matches!(kind, IntrinsicCallKind::AddField) {
                    if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &call.payload
                    {
                        if let Some(first) = args.first() {
                            if let ExprKind::Locator(locator) = first.kind() {
                                if let Some(ident) = locator.as_ident() {
                                    assign_target = Some(ident.as_str().to_string());
                                }
                            }
                        }
                    }
                }
                let value = self.eval_intrinsic(call);
                if let Some(name) = assign_target {
                    if let Some(stored) = self.lookup_stored_value_mut(&name) {
                        if stored.assign(value.clone()) {
                            self.mark_mutated();
                        }
                    }
                }
                if self.should_replace_intrinsic_with_value(kind, &value) {
                    let mut replacement = Expr::value(value.clone());
                    replacement.ty = expr.ty.clone();
                    // Type is already copied from original expr, no need to re-infer
                    *expr = replacement;
                    self.mark_mutated();
                    return value;
                }
                value
            }
            ExprKind::Reference(reference) => self.eval_expr(reference.referee.as_mut()),
            ExprKind::Paren(paren) => self.eval_expr(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                let flow = self.eval_assign_runtime(assign);
                self.finish_runtime_flow(flow)
            }
            ExprKind::Let(expr_let) => {
                let value = self.eval_expr(expr_let.expr.as_mut());
                self.bind_pattern(&expr_let.pat, value.clone());
                value
            }
            ExprKind::Invoke(invoke) => {
                let result = self.eval_invoke(invoke);
                if let Some(ty) = self.pending_expr_ty.take() {
                    expr.set_ty(ty);
                }
                result
            }
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` should have been lowered before const evaluation",
                    macro_expr.invocation.path
                ));
                Value::undefined()
            }
            ExprKind::Closure(closure) => {
                if let Some(Ty::Function(ref fn_sig)) = expr_ty_snapshot.as_ref() {
                    Self::annotate_expr_closure(closure, fn_sig);
                }
                let captured = self.capture_closure(closure, expr_ty_snapshot.clone());
                self.pending_closure = Some(captured.clone());
                Value::Any(AnyBox::new(captured))
            }
            ExprKind::Closured(closured) => {
                let mut inner = closured.expr.as_ref().clone();
                self.eval_expr(&mut inner)
            }
            ExprKind::Cast(cast) => {
                let target_ty = cast.ty.clone();
                let value = self.eval_expr(cast.expr.as_mut());
                let result = self.cast_value_to_type(value, &target_ty);
                expr.set_ty(target_ty);
                result
            }
            ExprKind::Loop(loop_expr) => {
                let flow = self.eval_loop_runtime(loop_expr);
                self.finish_runtime_flow(flow)
            }
            ExprKind::While(while_expr) => {
                let flow = self.eval_while_runtime(while_expr);
                self.finish_runtime_flow(flow)
            }
            ExprKind::For(for_expr) => {
                let flow = self.eval_for_runtime(for_expr);
                self.finish_runtime_flow(flow)
            }
            ExprKind::Any(_any) => {
                self.emit_error(
                    "expression not supported in AST interpretation: unsupported Raw expression",
                );
                Value::undefined()
            }
            other => {
                self.emit_error(format!(
                    "expression not supported in AST interpretation: {:?}",
                    other
                ));
                Value::undefined()
            }
        }
    }

    pub(super) fn eval_invoke(&mut self, invoke: &mut ExprInvoke) -> Value {
        if self.in_const_region() {
            return self.eval_invoke_compile_time(invoke);
        }
        match self.mode {
            InterpreterMode::CompileTime => self.eval_invoke_compile_time(invoke),
            InterpreterMode::RunTime => self.eval_invoke_runtime(invoke),
        }
    }

    pub(super) fn eval_invoke_compile_time(&mut self, invoke: &mut ExprInvoke) -> Value {
        if let ExprInvokeTarget::Method(select) = &mut invoke.target {
            if select.field.name.as_str() == "push" && invoke.args.len() == 1 {
                let value = self.eval_expr(&mut invoke.args[0]);
                if let ExprKind::Locator(locator) = select.obj.kind() {
                    let binding = match locator {
                        Locator::Ident(ident) => Some(ident.as_str().to_string()),
                        Locator::Path(path) => path
                            .segments
                            .last()
                            .map(|segment| segment.as_str().to_string()),
                        Locator::ParameterPath(path) => path
                            .segments
                            .last()
                            .map(|segment| segment.ident.as_str().to_string()),
                    };
                    if let Some(name) = binding {
                        if let Some(stored) = self.lookup_stored_value_mut(&name) {
                            if let Some(shared) = stored.shared_handle() {
                                match shared.lock() {
                                    Ok(mut guard) => match &mut *guard {
                                        Value::List(list) => {
                                            list.values.push(value);
                                            self.update_mutable_constant(
                                                &name,
                                                Value::List(list.clone()),
                                            );
                                            self.mark_mutated();
                                            self.pending_expr_ty = Some(Ty::unit());
                                            return Value::unit();
                                        }
                                        other => {
                                            self.emit_error(format!(
                                                "push expects a mutable list, found {}",
                                                other
                                            ));
                                            return Value::undefined();
                                        }
                                    },
                                    Err(err) => {
                                        let mut guard = err.into_inner();
                                        match &mut *guard {
                                        Value::List(list) => {
                                            list.values.push(value);
                                            self.update_mutable_constant(
                                                &name,
                                                Value::List(list.clone()),
                                            );
                                            self.mark_mutated();
                                            self.pending_expr_ty = Some(Ty::unit());
                                            return Value::unit();
                                        }
                                            other => {
                                                self.emit_error(format!(
                                                    "push expects a mutable list, found {}",
                                                    other
                                                ));
                                                return Value::undefined();
                                            }
                                        }
                                    }
                                }
                            }
                            self.emit_error("push requires a mutable binding");
                            return Value::undefined();
                        }
                    }
                }
                self.emit_error("push requires a mutable list binding");
                return Value::undefined();
            }

            if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                return match value {
                    Value::List(list) => Value::int(list.values.len() as i64),
                    Value::Map(map) => Value::int(map.len() as i64),
                    Value::String(text) => Value::int(text.value.chars().count() as i64),
                    Value::QuoteToken(token) => match token.value {
                        QuoteTokenValue::Items(items) => Value::int(items.len() as i64),
                        _ => {
                            self.emit_error(format!(
                                "'len' is only supported on compile-time arrays, lists, and maps, found {:?}",
                                Value::QuoteToken(token)
                            ));
                            Value::undefined()
                        }
                    },
                    other => {
                        self.emit_error(format!(
                            "'len' is only supported on compile-time arrays, lists, and maps, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            if matches!(
                select.field.name.as_str(),
                "starts_with" | "ends_with" | "contains"
            ) && invoke.args.len() == 1
            {
                let value = self.eval_expr(select.obj.as_mut());
                let arg = self.eval_expr(&mut invoke.args[0]);
                let hay = match value {
                    Value::String(text) => text.value,
                    other => {
                        self.emit_error(format!(
                            "string method '{}' expects a string receiver, found {:?}",
                            select.field.name, other
                        ));
                        return Value::undefined();
                    }
                };
                let needle = match arg {
                    Value::String(text) => text.value,
                    Value::Char(ch) => ch.value.to_string(),
                    other => {
                        self.emit_error(format!(
                            "string method '{}' expects a string argument, found {:?}",
                            select.field.name, other
                        ));
                        return Value::undefined();
                    }
                };
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::Bool));
                let result = match select.field.name.as_str() {
                    "starts_with" => hay.starts_with(&needle),
                    "ends_with" => hay.ends_with(&needle),
                    "contains" => hay.contains(&needle),
                    _ => false,
                };
                return Value::bool(result);
            }

            if select.field.name.as_str() == "to_string" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::String));
                return match value {
                    Value::String(_) => value,
                    Value::Int(int_val) => Value::string(int_val.value.to_string()),
                    Value::Decimal(dec_val) => Value::string(dec_val.value.to_string()),
                    Value::Bool(bool_val) => Value::string(bool_val.value.to_string()),
                    Value::Char(char_val) => Value::string(char_val.value.to_string()),
                    other => {
                        self.emit_error(format!(
                            "'to_string' is only supported on primitive compile-time values, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            if select.field.name.as_str() == "iter" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                return match value {
                    Value::List(list) => Value::List(list),
                    other => {
                        self.emit_error(format!(
                            "'iter' is only supported on compile-time arrays and lists, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            if select.field.name.as_str() == "enumerate" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                return match value {
                    Value::List(list) => {
                        let values = list
                            .values
                            .into_iter()
                            .enumerate()
                            .map(|(idx, value)| {
                                Value::Tuple(ValueTuple::new(vec![Value::int(idx as i64), value]))
                            })
                            .collect();
                        Value::List(ValueList::new(values))
                    }
                    other => {
                        self.emit_error(format!(
                            "'enumerate' is only supported on compile-time arrays and lists, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            let flow = self.eval_invoke_runtime_flow(invoke);
            return self.finish_runtime_flow(flow);
        }

        match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if invoke.args.is_empty() {
                    if let Some(value) = self.try_eval_method_chain(locator) {
                        return value;
                    }
                }

                if let Some(value) =
                    self.try_handle_const_collection_invoke(locator, &mut invoke.args)
                {
                    return value;
                }

                if let Some(function) = self.resolve_function_call(locator, &mut invoke.args) {
                    // Call user-defined const function
                    let evaluated = self.evaluate_args(&mut invoke.args);
                    let value = self.call_function(function, evaluated);
                    return value;
                }

                if let Some(Value::Function(function)) = self.lookup_callable_value(locator) {
                    let evaluated = self.evaluate_args(&mut invoke.args);
                    return self.call_value_function(&function, evaluated);
                }

                self.emit_error(format!("cannot resolve function '{}'", locator));
                Value::undefined()
            }
            ExprInvokeTarget::Expr(expr) => {
                let target = self.eval_expr(expr.as_mut());
                match target {
                    Value::Function(function) => {
                        let evaluated = self.evaluate_args(&mut invoke.args);
                        self.call_value_function(&function, evaluated)
                    }
                    _ => {
                        self.emit_error(
                            "attempted to call a non-function value in const evaluation",
                        );
                        Value::undefined()
                    }
                }
            }
            ExprInvokeTarget::Closure(func) => {
                let args = self.evaluate_args(&mut invoke.args);
                let ret_ty = Self::value_function_ret_ty(func);
                self.set_pending_expr_ty(ret_ty);
                self.call_value_function(func, args)
            }
            ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {
                let evaluated = self.evaluate_args(&mut invoke.args);
                Value::Tuple(ValueTuple::new(evaluated))
            }
            ExprInvokeTarget::Method(_) => unreachable!(),
        }
    }

    fn try_eval_method_chain(&mut self, locator: &Locator) -> Option<Value> {
        let text = locator.to_string();
        if !text.contains('.') {
            return None;
        }

        let mut segments = text.split('.');
        let base = segments.next()?;
        let mut value = self.lookup_value(base)?;

        for method in segments {
            match method {
                "iter" => match value {
                    Value::List(list) => value = Value::List(list),
                    other => {
                        self.emit_error(format!(
                            "'iter' is only supported on compile-time arrays and lists, found {:?}",
                            other
                        ));
                        return Some(Value::undefined());
                    }
                },
                "enumerate" => match value {
                    Value::List(list) => {
                        let values = list
                            .values
                            .into_iter()
                            .enumerate()
                            .map(|(idx, value)| {
                                Value::Tuple(ValueTuple::new(vec![Value::int(idx as i64), value]))
                            })
                            .collect();
                        value = Value::List(ValueList::new(values));
                    }
                    other => {
                        self.emit_error(format!(
                            "'enumerate' is only supported on compile-time arrays and lists, found {:?}",
                            other
                        ));
                        return Some(Value::undefined());
                    }
                },
                _ => return None,
            }
        }

        Some(value)
    }

    pub(super) fn eval_invoke_runtime(&mut self, invoke: &mut ExprInvoke) -> Value {
        let flow = self.eval_invoke_runtime_flow(invoke);
        self.finish_runtime_flow(flow)
    }

    pub(super) fn eval_invoke_runtime_flow(&mut self, invoke: &mut ExprInvoke) -> RuntimeFlow {
        match &mut invoke.target {
            ExprInvokeTarget::Method(select) => {
                return self.eval_method_call_runtime(select, &mut invoke.args);
            }
            ExprInvokeTarget::Function(locator) => {
                if let Some(info) = self.lookup_enum_variant(locator) {
                    if let EnumVariantPayload::Tuple(arity) = info.payload {
                        let args = match self.evaluate_args_runtime(&mut invoke.args) {
                            Ok(values) => values,
                            Err(flow) => return flow,
                        };
                        if args.len() != arity {
                            self.emit_error(format!(
                                "enum variant {} expects {} argument(s), got {}",
                                info.variant_name,
                                arity,
                                args.len()
                            ));
                            return RuntimeFlow::Value(Value::undefined());
                        }
                        let payload = if args.len() == 1 {
                            args.into_iter().next()
                        } else {
                            Some(Value::Tuple(ValueTuple::new(args)))
                        };
                        return RuntimeFlow::Value(self.build_enum_value(&info, payload));
                    }
                }

                if self.is_printf_symbol(&locator.to_string()) {
                    let evaluated = match self.evaluate_args_runtime(&mut invoke.args) {
                        Ok(values) => values,
                        Err(flow) => return flow,
                    };
                    if let Some(Value::String(format_str)) = evaluated.first() {
                        match format_runtime_string(&format_str.value, &evaluated[1..]) {
                            Ok(output) => {
                                if !output.is_empty() {
                                    self.stdout.push(output);
                                }
                            }
                            Err(err) => self.emit_error(err.to_string()),
                        }
                    } else {
                        self.emit_error(
                            "printf expects a string literal as the first argument at runtime",
                        );
                    }
                    return RuntimeFlow::Value(Value::unit());
                }

                if let Some(value) =
                    self.try_handle_const_collection_invoke(locator, &mut invoke.args)
                {
                    return RuntimeFlow::Value(value);
                }

                let args = match self.evaluate_args_runtime(&mut invoke.args) {
                    Ok(values) => values,
                    Err(flow) => return flow,
                };
                for (expr, value) in invoke.args.iter_mut().zip(args.iter()) {
                    if expr.ty().is_none() {
                        if let Some(ty) = self.infer_value_ty(value) {
                            expr.set_ty(ty);
                        }
                    }
                }

                if let Some(function) = self.resolve_function_call(locator, &mut invoke.args) {
                    let mut impl_context = None;
                    let segments = Self::locator_segments(locator);
                    if segments.len() >= 2 {
                        let self_ty = segments[segments.len() - 2].clone();
                        let method_name = segments[segments.len() - 1].as_str();
                        if let Some(methods) = self.impl_methods.get(&self_ty) {
                            if methods.contains_key(method_name) {
                                impl_context = Some(ImplContext {
                                    self_ty: Some(self_ty),
                                    trait_ty: None,
                                });
                            }
                        }
                    }

                    if let Some(context) = impl_context {
                        self.impl_stack.push(context);
                        let flow = self.call_function_runtime(function, args);
                        self.impl_stack.pop();
                        return flow;
                    }
                    return self.call_function_runtime(function, args);
                }

                let mut candidate_names = vec![locator.to_string()];
                if let Some(ident) = locator.as_ident() {
                    candidate_names.push(ident.as_str().to_string());
                }
                for name in candidate_names {
                    if let Some(template) = self.generic_functions.get(&name) {
                        return self.call_function_runtime(template.function.clone(), args);
                    }
                }

                if let Some(value) = self.lookup_callable_value(locator) {
                    match value {
                        Value::Function(function) => {
                            return self.call_value_function_runtime(&function, args);
                        }
                        Value::Any(any) => {
                            if let Some(closure) = any.downcast_ref::<ConstClosure>() {
                                return self.call_const_closure_runtime(closure, args);
                            }
                        }
                        _ => {}
                    }
                }

                self.emit_error(format!("cannot resolve function '{}'", locator));
                RuntimeFlow::Value(Value::undefined())
            }
            ExprInvokeTarget::Expr(expr) => {
                let target = match self.eval_expr_runtime(expr.as_mut()) {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                match target {
                    Value::Function(function) => {
                        let args = match self.evaluate_args_runtime(&mut invoke.args) {
                            Ok(values) => values,
                            Err(flow) => return flow,
                        };
                        self.call_value_function_runtime(&function, args)
                    }
                    Value::Any(any) => {
                        if let Some(closure) = any.downcast_ref::<ConstClosure>() {
                            let args = match self.evaluate_args_runtime(&mut invoke.args) {
                                Ok(values) => values,
                                Err(flow) => return flow,
                            };
                            return self.call_const_closure_runtime(closure, args);
                        }
                        self.emit_error("attempted to call a non-function value at runtime");
                        RuntimeFlow::Value(Value::undefined())
                    }
                    _ => {
                        self.emit_error("attempted to call a non-function value at runtime");
                        RuntimeFlow::Value(Value::undefined())
                    }
                }
            }
            ExprInvokeTarget::Closure(func) => {
                let args = match self.evaluate_args_runtime(&mut invoke.args) {
                    Ok(values) => values,
                    Err(flow) => return flow,
                };
                let ret_ty = Self::value_function_ret_ty(func);
                self.set_pending_expr_ty(ret_ty);
                self.call_value_function_runtime(func, args)
            }
            ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {
                let args = match self.evaluate_args_runtime(&mut invoke.args) {
                    Ok(values) => values,
                    Err(flow) => return flow,
                };
                RuntimeFlow::Value(Value::Tuple(ValueTuple::new(args)))
            }
        }
    }

    fn evaluate_args_runtime(
        &mut self,
        args: &mut Vec<Expr>,
    ) -> std::result::Result<Vec<Value>, RuntimeFlow> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args.iter_mut() {
            match self.eval_expr_runtime(arg) {
                RuntimeFlow::Value(value) => values.push(value),
                other => return Err(other),
            }
        }
        Ok(values)
    }

    fn eval_method_call_runtime(
        &mut self,
        select: &mut fp_core::ast::ExprSelect,
        args: &mut Vec<Expr>,
    ) -> RuntimeFlow {
        let receiver = self.resolve_receiver_binding(select.obj.as_mut());
        let method_name = select.field.name.as_str().to_string();

        if args.is_empty() {
            match method_name.as_str() {
                "len" => match receiver.value {
                    Value::List(list) => {
                        return RuntimeFlow::Value(Value::int(list.values.len() as i64));
                    }
                    Value::Tuple(tuple) => {
                        return RuntimeFlow::Value(Value::int(tuple.values.len() as i64));
                    }
                    Value::String(string) => {
                        return RuntimeFlow::Value(Value::int(string.value.chars().count() as i64));
                    }
                    Value::Map(map) => {
                        return RuntimeFlow::Value(Value::int(map.entries.len() as i64));
                    }
                    Value::QuoteToken(ref token) => match &token.value {
                        QuoteTokenValue::Items(items) => {
                            return RuntimeFlow::Value(Value::int(items.len() as i64));
                        }
                        _ => {}
                    },
                    _ => {}
                },
                "iter" => match receiver.value {
                    Value::List(list) => return RuntimeFlow::Value(Value::List(list)),
                    Value::String(string) => {
                        let chars = string
                            .value
                            .chars()
                            .map(|ch| Value::Char(fp_core::ast::ValueChar::new(ch)))
                            .collect();
                        return RuntimeFlow::Value(Value::List(ValueList::new(chars)));
                    }
                    _ => {}
                },
                "enumerate" => match receiver.value {
                    Value::List(list) => {
                        let items = list
                            .values
                            .into_iter()
                            .enumerate()
                            .map(|(idx, value)| {
                                Value::Tuple(ValueTuple::new(vec![Value::int(idx as i64), value]))
                            })
                            .collect();
                        return RuntimeFlow::Value(Value::List(ValueList::new(items)));
                    }
                    Value::String(string) => {
                        let items = string
                            .value
                            .chars()
                            .enumerate()
                            .map(|(idx, ch)| {
                                Value::Tuple(ValueTuple::new(vec![
                                    Value::int(idx as i64),
                                    Value::Char(fp_core::ast::ValueChar::new(ch)),
                                ]))
                            })
                            .collect();
                        return RuntimeFlow::Value(Value::List(ValueList::new(items)));
                    }
                    _ => {}
                },
                "to_string" => {
                    let value = match receiver.value {
                        Value::String(value) => Value::String(value),
                        Value::Int(value) => Value::string(value.value.to_string()),
                        Value::Decimal(value) => Value::string(value.value.to_string()),
                        Value::Bool(value) => Value::string(value.value.to_string()),
                        Value::Char(value) => Value::string(value.value.to_string()),
                        other => {
                            self.emit_error(format!(
                                "'to_string' is only supported on primitive runtime values, found {:?}",
                                other
                            ));
                            Value::undefined()
                        }
                    };
                    return RuntimeFlow::Value(value);
                }
                _ => {}
            }
        }

        let Some(function) = self.resolve_method_function(&receiver, &method_name, args) else {
            self.emit_error(format!("cannot resolve method '{}'", method_name));
            return RuntimeFlow::Value(Value::undefined());
        };

        let arg_values = match self.evaluate_args_runtime(args) {
            Ok(values) => values,
            Err(flow) => return flow,
        };
        self.call_method_runtime(function, receiver, arg_values)
    }

    pub(super) fn evaluate_args(&mut self, args: &mut Vec<Expr>) -> Vec<Value> {
        args.iter_mut().map(|arg| self.eval_expr(arg)).collect()
    }

    pub(super) fn evaluate_arg_slice(&mut self, args: &mut [Expr]) -> Vec<Value> {
        args.iter_mut().map(|arg| self.eval_expr(arg)).collect()
    }

    // Helpers moved from interpreter.rs to support const-eval invoke resolution.
    pub(super) fn lookup_callable_value(&mut self, locator: &Locator) -> Option<Value> {
        locator
            .as_ident()
            .and_then(|ident| self.lookup_value(ident.as_str()))
    }

    pub(super) fn resolve_function_call(
        &mut self,
        locator: &mut Locator,
        args: &mut [Expr],
    ) -> Option<ItemDefFunction> {
        let mut candidate_names = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            let simple = ident.as_str().to_string();
            if !candidate_names.contains(&simple) {
                candidate_names.push(simple);
            }
        }

        for name in &candidate_names {
            if let Some(function) = self.functions.get(name).cloned() {
                // Annotate arguments with expected parameter types
                self.annotate_invoke_args_slice(args, &function.sig.params);
                return Some(function);
            }
        }

        for name in &candidate_names {
            if let Some(template) = self.generic_functions.get(name).cloned() {
                if let Some(function) =
                    self.instantiate_generic_function(name, template, locator, args)
                {
                    // Annotate arguments now that we know the specialized function signature
                    self.annotate_invoke_args_slice(args, &function.sig.params);
                    return Some(function);
                }
            }
        }

        // Fallback: find by fully qualified name
        if let Some(function) = {
            if let Some(ident) = locator.as_ident() {
                self.functions.get(ident.as_str()).cloned()
            } else {
                None
            }
        }
        .or_else(|| self.functions.get(&locator.to_string()).cloned())
        {
            self.annotate_invoke_args_slice(args, &function.sig.params);
            Some(function)
        } else {
            None
        }
    }

    pub(super) fn try_handle_const_collection_invoke(
        &mut self,
        locator: &Locator,
        args: &mut [Expr],
    ) -> Option<Value> {
        let segments = Self::locator_segments(locator);

        let ends_with = |suffix: &[&str]| -> bool {
            if segments.len() < suffix.len() {
                return false;
            }
            segments
                .iter()
                .rev()
                .zip(suffix.iter().rev())
                .all(|(segment, expected)| segment == expected)
        };

        if ends_with(&["Vec", "new"]) {
            if !args.is_empty() {
                let _ = self.evaluate_arg_slice(args);
                self.emit_error("Vec::new does not take arguments in const evaluation");
                return Some(Value::undefined());
            }
            self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                ty: Box::new(Ty::Any(TypeAny)),
            })));
            return Some(Value::List(ValueList::new(Vec::new())));
        }

        if ends_with(&["Vec", "with_capacity"]) {
            let _ = self.evaluate_arg_slice(args);
            self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                ty: Box::new(Ty::Any(TypeAny)),
            })));
            return Some(Value::List(ValueList::new(Vec::new())));
        }

        if ends_with(&["Vec", "from"]) {
            let evaluated = self.evaluate_arg_slice(args);
            if evaluated.len() != 1 {
                self.emit_error("Vec::from expects a single iterable argument in const evaluation");
                return Some(Value::undefined());
            }

            match evaluated.into_iter().next() {
                None => {
                    self.emit_error("Vec::from expects one argument during const evaluation");
                    return Some(Value::undefined());
                }
                Some(val) => match val {
                    Value::List(list) => {
                        self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                            ty: Box::new(Ty::Any(TypeAny)),
                        })));
                        return Some(Value::List(ValueList::new(list.values)));
                    }
                    other => {
                        self.emit_error(format!(
                            "Vec::from expects a list literal during const evaluation, got {:?}",
                            other
                        ));
                        return Some(Value::undefined());
                    }
                },
            }
        }

        if ends_with(&["HashMap", "new"]) {
            if !args.is_empty() {
                let _ = self.evaluate_arg_slice(args);
                self.emit_error("HashMap::new does not take arguments in const evaluation");
                return Some(Value::undefined());
            }
            return Some(Value::map(Vec::new()));
        }

        if ends_with(&["HashMap", "with_capacity"]) {
            let _ = self.evaluate_arg_slice(args);
            return Some(Value::map(Vec::new()));
        }

        if ends_with(&["HashMap", "from"]) {
            let evaluated = self.evaluate_arg_slice(args);
            if evaluated.len() != 1 {
                self.emit_error(
                    "HashMap::from expects a single iterable argument in const evaluation",
                );
                return Some(Value::undefined());
            }

            match evaluated.into_iter().next() {
                None => {
                    self.emit_error(
                        "HashMap::from expects one iterable argument during const evaluation",
                    );
                    return Some(Value::undefined());
                }
                Some(val) => match val {
                    Value::List(list) => {
                        let mut pairs = Vec::with_capacity(list.values.len());
                        for entry in list.values {
                            match entry {
                                Value::Tuple(tuple) if tuple.values.len() == 2 => {
                                    let mut iter = tuple.values.into_iter();
                                    if let (Some(key), Some(value)) = (iter.next(), iter.next()) {
                                        pairs.push((key, value));
                                    } else {
                                        self.emit_error(
                                            "HashMap::from tuple entry did not contain two elements",
                                        );
                                        return Some(Value::undefined());
                                    }
                                }
                                Value::List(inner) if inner.values.len() == 2 => {
                                    let mut iter = inner.values.into_iter();
                                    if let (Some(key), Some(value)) = (iter.next(), iter.next()) {
                                        pairs.push((key, value));
                                    } else {
                                        self.emit_error(
                                            "HashMap::from list entry did not contain two elements",
                                        );
                                        return Some(Value::undefined());
                                    }
                                }
                                other => {
                                    self.emit_error(format!(
                                        "HashMap::from expects entries to be tuples or 2-element lists, found {:?}",
                                        other
                                    ));
                                    return Some(Value::undefined());
                                }
                            }
                        }
                        return Some(Value::map(pairs));
                    }
                    other => {
                        self.emit_error(format!(
                            "HashMap::from expects a list of key/value pairs during const evaluation, got {:?}",
                            other
                        ));
                        return Some(Value::undefined());
                    }
                },
            }
        }

        None
    }
}
