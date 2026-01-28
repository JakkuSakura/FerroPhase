use super::*;
use fp_core::ast::Locator;

impl<'ctx> AstInterpreter<'ctx> {
    /// Evaluate an expression in runtime-capable mode, returning structured control-flow.
    pub(super) fn eval_expr_runtime(&mut self, expr: &mut Expr) -> RuntimeFlow {
        if self.stack_eval_active {
            let flow = self.eval_expr_runtime_inner(expr);
            return flow;
        }
        if let Err(err) = self.begin_runtime_eval(expr) {
            self.emit_error(err);
            return RuntimeFlow::Value(Value::undefined());
        }
        let flow = match self.run_runtime_tasks_limit(usize::MAX) {
            RuntimeStepOutcome::Yielded => RuntimeFlow::Value(Value::undefined()),
            RuntimeStepOutcome::Complete(flow) => flow,
        };
        self.finish_active_eval();
        flow
    }

    pub(super) fn run_runtime_tasks_limit(&mut self, max_steps: usize) -> RuntimeStepOutcome {
        let mut steps = 0usize;
        while steps < max_steps {
            let Some(task) = self.runtime_tasks.pop() else {
                break;
            };
            steps += 1;
            match task {
                RuntimeTask::Eval(expr_ptr) => {
                    let expr = unsafe { &mut *expr_ptr };
                    let _expr_guard = self.push_expr_frame(EvalMode::Runtime, expr);
                    match expr.kind_mut() {
                        ExprKind::Value(value) => {
                            let value = match value.as_ref() {
                                Value::Expr(inner) => {
                                    let mut cloned = inner.as_ref().clone();
                                    match self.eval_expr_runtime_inner(&mut cloned) {
                                        RuntimeFlow::Value(value) => value,
                                        other => {
                                            return RuntimeStepOutcome::Complete(other);
                                        }
                                    }
                                }
                                other => other.clone(),
                            };
                            self.runtime_value_stack.push(RuntimeFlow::Value(value));
                        }
                        ExprKind::Locator(_locator) => {
                            let flow = match self.eval_expr_runtime_inner(expr) {
                                RuntimeFlow::Value(value) => RuntimeFlow::Value(value),
                                other => other,
                            };
                            self.runtime_value_stack.push(flow);
                        }
                        ExprKind::BinOp(binop) => {
                            self.runtime_tasks
                                .push(RuntimeTask::ApplyBinOp(binop.kind));
                            self.runtime_tasks.push(RuntimeTask::Eval(
                                binop.rhs.as_mut() as *mut Expr,
                            ));
                            self.runtime_tasks.push(RuntimeTask::Eval(
                                binop.lhs.as_mut() as *mut Expr,
                            ));
                        }
                        ExprKind::UnOp(unop) => {
                            self.runtime_tasks
                                .push(RuntimeTask::ApplyUnOp(unop.op.clone()));
                            self.runtime_tasks
                                .push(RuntimeTask::Eval(unop.val.as_mut() as *mut Expr));
                        }
                        ExprKind::If(if_expr) => {
                            self.runtime_tasks.push(RuntimeTask::ApplyIf {
                                then_expr: if_expr.then.as_mut() as *mut Expr,
                                else_expr: if_expr
                                    .elze
                                    .as_mut()
                                    .map(|expr| expr.as_mut() as *mut Expr),
                            });
                            self.runtime_tasks
                                .push(RuntimeTask::Eval(if_expr.cond.as_mut() as *mut Expr));
                        }
                        ExprKind::Tuple(tuple) => {
                            let len = tuple.values.len();
                            self.runtime_tasks.push(RuntimeTask::ApplyCollect {
                                len,
                                kind: CollectKind::Tuple,
                            });
                            for expr in tuple.values.iter_mut().rev() {
                                self.runtime_tasks.push(RuntimeTask::Eval(expr as *mut Expr));
                            }
                        }
                        ExprKind::Array(array) => {
                            let len = array.values.len();
                            self.runtime_tasks.push(RuntimeTask::ApplyCollect {
                                len,
                                kind: CollectKind::Array,
                            });
                            for expr in array.values.iter_mut().rev() {
                                self.runtime_tasks.push(RuntimeTask::Eval(expr as *mut Expr));
                            }
                        }
                        ExprKind::Range(range) => {
                            self.runtime_tasks.push(RuntimeTask::ApplyRange {
                                inclusive: matches!(range.limit, ExprRangeLimit::Inclusive),
                            });
                            if let Some(expr) = range.end.as_mut() {
                                self.runtime_tasks
                                    .push(RuntimeTask::Eval(expr.as_mut() as *mut Expr));
                            } else {
                                self.runtime_tasks
                                    .push(RuntimeTask::PushValue(Value::int(0)));
                            }
                            if let Some(expr) = range.start.as_mut() {
                                self.runtime_tasks
                                    .push(RuntimeTask::Eval(expr.as_mut() as *mut Expr));
                            } else {
                                self.runtime_tasks
                                    .push(RuntimeTask::PushValue(Value::int(0)));
                            }
                        }
                        ExprKind::Select(select) => {
                            self.runtime_tasks.push(RuntimeTask::ApplySelect {
                                field: select.field.name.clone(),
                            });
                            self.runtime_tasks
                                .push(RuntimeTask::Eval(select.obj.as_mut() as *mut Expr));
                        }
                        ExprKind::Index(index_expr) => {
                            if let ExprKind::Range(range) = index_expr.index.kind_mut() {
                                let has_start = range.start.is_some();
                                let has_end = range.end.is_some();
                                self.runtime_tasks.push(RuntimeTask::ApplyIndex {
                                    has_start,
                                    has_end,
                                    inclusive: matches!(range.limit, ExprRangeLimit::Inclusive),
                                });
                                if let Some(expr) = range.end.as_mut() {
                                    self.runtime_tasks
                                        .push(RuntimeTask::Eval(expr.as_mut() as *mut Expr));
                                }
                                if let Some(expr) = range.start.as_mut() {
                                    self.runtime_tasks
                                        .push(RuntimeTask::Eval(expr.as_mut() as *mut Expr));
                                }
                                self.runtime_tasks.push(RuntimeTask::Eval(
                                    index_expr.obj.as_mut() as *mut Expr,
                                ));
                            } else {
                                self.runtime_tasks.push(RuntimeTask::ApplyIndex {
                                    has_start: true,
                                    has_end: true,
                                    inclusive: false,
                                });
                                self.runtime_tasks.push(RuntimeTask::Eval(
                                    index_expr.index.as_mut() as *mut Expr,
                                ));
                                self.runtime_tasks.push(RuntimeTask::Eval(
                                    index_expr.obj.as_mut() as *mut Expr,
                                ));
                            }
                        }
                        ExprKind::Paren(paren) => {
                            self.runtime_tasks
                                .push(RuntimeTask::Eval(paren.expr.as_mut() as *mut Expr));
                        }
                        ExprKind::Cast(cast) => {
                            let ty = cast.ty.clone();
                            self.runtime_tasks.push(RuntimeTask::ApplyCast { ty });
                            self.runtime_tasks
                                .push(RuntimeTask::Eval(cast.expr.as_mut() as *mut Expr));
                        }
                        _ => {
                            let flow = self.eval_expr_runtime_inner(expr);
                            self.runtime_value_stack.push(flow);
                        }
                    }
                }
                RuntimeTask::PushValue(value) => {
                    self.runtime_value_stack.push(RuntimeFlow::Value(value));
                }
                RuntimeTask::ApplyBinOp(op) => {
                    let rhs = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(rhs_value) = rhs else {
                        return RuntimeStepOutcome::Complete(rhs);
                    };
                    let lhs = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(lhs_value) = lhs else {
                        return RuntimeStepOutcome::Complete(lhs);
                    };
                    let value =
                        self.handle_result(self.evaluate_binop(op, lhs_value, rhs_value));
                    self.runtime_value_stack.push(RuntimeFlow::Value(value));
                }
                RuntimeTask::ApplyUnOp(op) => {
                    let value_flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(value) = value_flow else {
                        return RuntimeStepOutcome::Complete(value_flow);
                    };
                    let value = self.handle_result(self.evaluate_unary(op, value));
                    self.runtime_value_stack.push(RuntimeFlow::Value(value));
                }
                RuntimeTask::ApplyIf {
                    then_expr,
                    else_expr,
                } => {
                    let cond_flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(cond) = cond_flow else {
                        return RuntimeStepOutcome::Complete(cond_flow);
                    };
                    match cond {
                        Value::Bool(b) => {
                            if b.value {
                                self.runtime_tasks.push(RuntimeTask::Eval(then_expr));
                            } else if let Some(elze) = else_expr {
                                self.runtime_tasks.push(RuntimeTask::Eval(elze));
                            } else {
                                self.runtime_value_stack
                                    .push(RuntimeFlow::Value(Value::unit()));
                            }
                        }
                        _ => {
                            self.emit_error("expected boolean condition in runtime expression");
                            self.runtime_value_stack
                                .push(RuntimeFlow::Value(Value::undefined()));
                        }
                    }
                }
                RuntimeTask::ApplyCollect { len, kind } => {
                    let mut values = Vec::with_capacity(len);
                    for _ in 0..len {
                        let flow = match self.runtime_value_stack.pop() {
                            Some(flow) => flow,
                            None => RuntimeFlow::Value(Value::undefined()),
                        };
                        let RuntimeFlow::Value(value) = flow else {
                            return RuntimeStepOutcome::Complete(flow);
                        };
                        values.push(value);
                    }
                    values.reverse();
                    let value = match kind {
                        CollectKind::Tuple => Value::Tuple(ValueTuple::new(values)),
                        CollectKind::Array => Value::List(ValueList::new(values)),
                    };
                    self.runtime_value_stack.push(RuntimeFlow::Value(value));
                }
                RuntimeTask::ApplyCast { ty } => {
                    let flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(value) = flow else {
                        return RuntimeStepOutcome::Complete(flow);
                    };
                    let result = self.cast_value_to_type(value, &ty);
                    self.runtime_value_stack
                        .push(RuntimeFlow::Value(result));
                }
                RuntimeTask::ApplyRange { inclusive } => {
                    let end_flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(end) = end_flow else {
                        return RuntimeStepOutcome::Complete(end_flow);
                    };
                    let start_flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(start) = start_flow else {
                        return RuntimeStepOutcome::Complete(start_flow);
                    };
                    let start = match self.numeric_to_i64(&start, "range start") {
                        Some(value) => value,
                        None => {
                            self.runtime_value_stack
                                .push(RuntimeFlow::Value(Value::undefined()));
                            continue;
                        }
                    };
                    let end = match self.numeric_to_i64(&end, "range end") {
                        Some(value) => value,
                        None => {
                            self.runtime_value_stack
                                .push(RuntimeFlow::Value(Value::undefined()));
                            continue;
                        }
                    };
                    let mut values = Vec::new();
                    let mut current = start;
                    while if inclusive {
                        current <= end
                    } else {
                        current < end
                    } {
                        values.push(Value::int(current));
                        current += 1;
                    }
                    self.runtime_value_stack
                        .push(RuntimeFlow::Value(Value::List(ValueList::new(values))));
                }
                RuntimeTask::ApplySelect { field } => {
                    let target_flow = match self.runtime_value_stack.pop() {
                        Some(flow) => flow,
                        None => RuntimeFlow::Value(Value::undefined()),
                    };
                    let RuntimeFlow::Value(target) = target_flow else {
                        return RuntimeStepOutcome::Complete(target_flow);
                    };
                    let value = self.evaluate_select(target, &field);
                    self.runtime_value_stack.push(RuntimeFlow::Value(value));
                }
                RuntimeTask::ApplyIndex {
                    has_start,
                    has_end,
                    inclusive,
                } => {
                    if has_start && has_end {
                        let index_flow = match self.runtime_value_stack.pop() {
                            Some(flow) => flow,
                            None => RuntimeFlow::Value(Value::undefined()),
                        };
                        let RuntimeFlow::Value(index_value) = index_flow else {
                            return RuntimeStepOutcome::Complete(index_flow);
                        };
                        let target_flow = match self.runtime_value_stack.pop() {
                            Some(flow) => flow,
                            None => RuntimeFlow::Value(Value::undefined()),
                        };
                        let RuntimeFlow::Value(target) = target_flow else {
                            return RuntimeStepOutcome::Complete(target_flow);
                        };
                        let value = self.evaluate_index(target, index_value);
                        self.runtime_value_stack.push(RuntimeFlow::Value(value));
                    } else {
                        let end = if has_end {
                            let end_flow = match self.runtime_value_stack.pop() {
                                Some(flow) => flow,
                                None => RuntimeFlow::Value(Value::undefined()),
                            };
                            let RuntimeFlow::Value(end) = end_flow else {
                                return RuntimeStepOutcome::Complete(end_flow);
                            };
                            match self.numeric_to_non_negative_usize(&end, "range end") {
                                Some(value) => Some(value),
                                None => None,
                            }
                        } else {
                            None
                        };
                        let start = if has_start {
                            let start_flow = match self.runtime_value_stack.pop() {
                                Some(flow) => flow,
                                None => RuntimeFlow::Value(Value::undefined()),
                            };
                            let RuntimeFlow::Value(start) = start_flow else {
                                return RuntimeStepOutcome::Complete(start_flow);
                            };
                            match self.numeric_to_non_negative_usize(&start, "range start") {
                                Some(value) => Some(value),
                                None => None,
                            }
                        } else {
                            None
                        };
                        let target_flow = match self.runtime_value_stack.pop() {
                            Some(flow) => flow,
                            None => RuntimeFlow::Value(Value::undefined()),
                        };
                        let RuntimeFlow::Value(target) = target_flow else {
                            return RuntimeStepOutcome::Complete(target_flow);
                        };
                        let value = self.evaluate_range_index_slices(
                            target,
                            start,
                            end,
                            inclusive,
                        );
                        self.runtime_value_stack.push(RuntimeFlow::Value(value));
                    }
                }
            }
        }

        if !self.runtime_tasks.is_empty() {
            return RuntimeStepOutcome::Yielded;
        }

        match self.runtime_value_stack.pop() {
            Some(flow) => RuntimeStepOutcome::Complete(flow),
            None => RuntimeStepOutcome::Complete(RuntimeFlow::Value(Value::undefined())),
        }
    }

    fn eval_expr_runtime_inner(&mut self, expr: &mut Expr) -> RuntimeFlow {
        self.ensure_expr_typed(expr);
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
                        self.emit_error(
                            "cannot splice non-expression token in expression position",
                        );
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
                self.apply_local_import_alias(locator);
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
                            self.emit_error(
                                "expected boolean match condition in runtime expression",
                            );
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
                let start = match self.numeric_to_i64(&start, "range start") {
                    Some(value) => value,
                    None => return RuntimeFlow::Value(Value::undefined()),
                };
                let end = match self.numeric_to_i64(&end, "range end") {
                    Some(value) => value,
                    None => return RuntimeFlow::Value(Value::undefined()),
                };
                let mut values = Vec::new();
                let mut current = start;
                let inclusive = matches!(range.limit, fp_core::ast::ExprRangeLimit::Inclusive);
                while if inclusive {
                    current <= end
                } else {
                    current < end
                } {
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
                if let ExprKind::Range(range) = index_expr.index.kind_mut() {
                    let target = match self.eval_value_runtime(index_expr.obj.as_mut()) {
                        Ok(value) => value,
                        Err(flow) => return flow,
                    };
                    let start = match range.start.as_mut() {
                        Some(expr) => match self.eval_value_runtime(expr) {
                            Ok(value) => Some(value),
                            Err(flow) => return flow,
                        },
                        None => None,
                    };
                    let end = match range.end.as_mut() {
                        Some(expr) => match self.eval_value_runtime(expr) {
                            Ok(value) => Some(value),
                            Err(flow) => return flow,
                        },
                        None => None,
                    };
                    let start_idx = match start {
                        Some(value) => match self.numeric_to_non_negative_usize(&value, "range start")
                        {
                            Some(value) => Some(value),
                            None => return RuntimeFlow::Value(Value::undefined()),
                        },
                        None => None,
                    };
                    let end_idx = match end {
                        Some(value) => match self.numeric_to_non_negative_usize(&value, "range end") {
                            Some(value) => Some(value),
                            None => return RuntimeFlow::Value(Value::undefined()),
                        },
                        None => None,
                    };
                    return RuntimeFlow::Value(self.evaluate_range_index_slices(
                        target,
                        start_idx,
                        end_idx,
                        matches!(range.limit, ExprRangeLimit::Inclusive),
                    ));
                }

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
            ExprKind::Reference(reference) => {
                if reference.mutable.unwrap_or(false) {
                    if let ExprKind::Locator(locator) = reference.referee.kind() {
                        if let Some(ident) = locator.as_ident() {
                            if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                                if let Some(shared) = stored.shared_handle() {
                                    return RuntimeFlow::Value(Value::Any(AnyBox::new(RuntimeRef {
                                        shared,
                                    })));
                                }
                                self.emit_error(format!(
                                    "mutable reference requires mutable binding for '{}'",
                                    ident.as_str()
                                ));
                                return RuntimeFlow::Value(Value::undefined());
                            }
                        }
                    }
                    self.emit_error("mutable reference target must be a named binding");
                    RuntimeFlow::Value(Value::undefined())
                } else {
                    self.eval_expr_runtime(reference.referee.as_mut())
                }
            }
            ExprKind::Dereference(deref) => {
                if let ExprKind::Locator(locator) = deref.referee.kind() {
                    if let Some(ident) = locator.as_ident() {
                        if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                            return RuntimeFlow::Value(stored.value());
                        }
                    }
                }
                let target = match self.eval_expr_runtime(deref.referee.as_mut()) {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                if let Value::Any(any) = target {
                    if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                        let value = runtime_ref
                            .shared
                            .lock()
                            .map(|value| value.clone())
                            .unwrap_or_else(|err| err.into_inner().clone());
                        return RuntimeFlow::Value(value);
                    }
                }
                self.emit_error("cannot dereference non-reference value");
                RuntimeFlow::Value(Value::undefined())
            }
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
            ExprKind::Return(ret) => {
                let value = if let Some(expr) = ret.value.as_mut() {
                    let flow = self.eval_expr_runtime(expr);
                    Some(self.finish_runtime_flow(flow))
                } else {
                    None
                };
                RuntimeFlow::Return(value)
            }
            ExprKind::Break(brk) => {
                let value = if let Some(expr) = brk.value.as_mut() {
                    let flow = self.eval_expr_runtime(expr);
                    Some(self.finish_runtime_flow(flow))
                } else {
                    None
                };
                RuntimeFlow::Break(value)
            }
            ExprKind::Continue(_) => RuntimeFlow::Continue,
            ExprKind::ConstBlock(const_block) => {
                self.enter_const_region();
                let result = self.eval_expr(const_block.expr.as_mut());
                self.exit_const_region();
                RuntimeFlow::Value(result)
            }
            ExprKind::Invoke(invoke) => self.eval_invoke_runtime_flow(invoke),
            ExprKind::Macro(macro_expr) => {
                self.emit_error_at(
                    macro_expr.invocation.span,
                    format!(
                        "macro `{}` should have been lowered before runtime evaluation",
                        macro_expr.invocation.path
                    ),
                );
                RuntimeFlow::Value(Value::undefined())
            }
            ExprKind::Closure(closure) => {
                RuntimeFlow::Value(self.capture_runtime_closure(closure, expr_ty_snapshot.clone()))
            }
            ExprKind::Closured(closured) => {
                let mut inner = closured.expr.as_ref().clone();
                self.eval_expr_runtime(&mut inner)
            }
            ExprKind::Async(async_expr) => {
                let future = self.capture_runtime_future(async_expr.expr.as_ref());
                RuntimeFlow::Value(Value::Any(AnyBox::new(future)))
            }
            ExprKind::Await(await_expr) => {
                let flow = self.eval_expr_runtime(await_expr.base.as_mut());
                let value = self.finish_runtime_flow(flow);
                self.await_runtime_value(value)
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
                let mut args = Vec::new();
                let mut kwargs = Vec::new();
                if let Ok(output) =
                    self.render_format_template_runtime(template, &mut args, &mut kwargs)
                {
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
        if self.stack_eval_active {
            return self.eval_expr_inner(expr);
        }
        if let Err(err) = self.begin_const_eval(expr) {
            self.emit_error(err);
            return Value::undefined();
        }
        let value = match self.run_const_tasks_limit(usize::MAX) {
            EvalStepOutcome::Yielded => Value::undefined(),
            EvalStepOutcome::Complete(value) => value,
        };
        self.finish_active_eval();
        value
    }

    pub(super) fn run_const_tasks_limit(&mut self, max_steps: usize) -> EvalStepOutcome {
        let mut steps = 0usize;
        while steps < max_steps {
            let Some(task) = self.const_tasks.pop() else {
                break;
            };
            steps += 1;
            match task {
                ConstTask::Eval(expr_ptr) => {
                    let expr = unsafe { &mut *expr_ptr };
                    let _expr_guard = self.push_expr_frame(EvalMode::Const, expr);
                    match expr.kind_mut() {
                        ExprKind::Value(value) => {
                            let value = match value.as_ref() {
                                Value::Expr(inner) => {
                                    let mut cloned = inner.as_ref().clone();
                                    self.eval_expr_inner(&mut cloned)
                                }
                                other => other.clone(),
                            };
                            self.const_value_stack.push(value);
                        }
                        ExprKind::Locator(_) => {
                            let value = self.eval_expr_inner(expr);
                            self.const_value_stack.push(value);
                        }
                        ExprKind::BinOp(binop) => {
                            self.const_tasks
                                .push(ConstTask::ApplyBinOp(binop.kind));
                            self.const_tasks
                                .push(ConstTask::Eval(binop.rhs.as_mut() as *mut Expr));
                            self.const_tasks
                                .push(ConstTask::Eval(binop.lhs.as_mut() as *mut Expr));
                        }
                        ExprKind::UnOp(unop) => {
                            self.const_tasks
                                .push(ConstTask::ApplyUnOp(unop.op.clone()));
                            self.const_tasks
                                .push(ConstTask::Eval(unop.val.as_mut() as *mut Expr));
                        }
                        ExprKind::If(if_expr) => {
                            self.const_tasks.push(ConstTask::ApplyIf {
                                then_expr: if_expr.then.as_mut() as *mut Expr,
                                else_expr: if_expr
                                    .elze
                                    .as_mut()
                                    .map(|expr| expr.as_mut() as *mut Expr),
                            });
                            self.const_tasks
                                .push(ConstTask::Eval(if_expr.cond.as_mut() as *mut Expr));
                        }
                        ExprKind::Tuple(tuple) => {
                            let len = tuple.values.len();
                            self.const_tasks.push(ConstTask::ApplyCollect {
                                len,
                                kind: CollectKind::Tuple,
                            });
                            for expr in tuple.values.iter_mut().rev() {
                                self.const_tasks.push(ConstTask::Eval(expr as *mut Expr));
                            }
                        }
                        ExprKind::Array(array) => {
                            let len = array.values.len();
                            self.const_tasks.push(ConstTask::ApplyCollect {
                                len,
                                kind: CollectKind::Array,
                            });
                            for expr in array.values.iter_mut().rev() {
                                self.const_tasks.push(ConstTask::Eval(expr as *mut Expr));
                            }
                        }
                        ExprKind::Range(range) => {
                            self.const_tasks.push(ConstTask::ApplyRange {
                                inclusive: matches!(range.limit, ExprRangeLimit::Inclusive),
                            });
                            if let Some(expr) = range.end.as_mut() {
                                self.const_tasks
                                    .push(ConstTask::Eval(expr.as_mut() as *mut Expr));
                            } else {
                                self.const_tasks
                                    .push(ConstTask::PushValue(Value::int(0)));
                            }
                            if let Some(expr) = range.start.as_mut() {
                                self.const_tasks
                                    .push(ConstTask::Eval(expr.as_mut() as *mut Expr));
                            } else {
                                self.const_tasks
                                    .push(ConstTask::PushValue(Value::int(0)));
                            }
                        }
                        ExprKind::Select(select) => {
                            self.const_tasks.push(ConstTask::ApplySelect {
                                field: select.field.name.clone(),
                            });
                            self.const_tasks
                                .push(ConstTask::Eval(select.obj.as_mut() as *mut Expr));
                        }
                        ExprKind::Index(index_expr) => {
                            if let ExprKind::Range(range) = index_expr.index.kind_mut() {
                                let has_start = range.start.is_some();
                                let has_end = range.end.is_some();
                                self.const_tasks.push(ConstTask::ApplyIndex {
                                    has_start,
                                    has_end,
                                    inclusive: matches!(range.limit, ExprRangeLimit::Inclusive),
                                });
                                if let Some(expr) = range.end.as_mut() {
                                    self.const_tasks
                                        .push(ConstTask::Eval(expr.as_mut() as *mut Expr));
                                }
                                if let Some(expr) = range.start.as_mut() {
                                    self.const_tasks
                                        .push(ConstTask::Eval(expr.as_mut() as *mut Expr));
                                }
                                self.const_tasks
                                    .push(ConstTask::Eval(index_expr.obj.as_mut() as *mut Expr));
                            } else {
                                self.const_tasks.push(ConstTask::ApplyIndex {
                                    has_start: true,
                                    has_end: true,
                                    inclusive: false,
                                });
                                self.const_tasks
                                    .push(ConstTask::Eval(index_expr.index.as_mut() as *mut Expr));
                                self.const_tasks
                                    .push(ConstTask::Eval(index_expr.obj.as_mut() as *mut Expr));
                            }
                        }
                        ExprKind::Paren(paren) => {
                            self.const_tasks
                                .push(ConstTask::Eval(paren.expr.as_mut() as *mut Expr));
                        }
                        ExprKind::Cast(cast) => {
                            let ty = cast.ty.clone();
                            self.const_tasks.push(ConstTask::ApplyCast { ty });
                            self.const_tasks
                                .push(ConstTask::Eval(cast.expr.as_mut() as *mut Expr));
                        }
                        _ => {
                            let value = self.eval_expr_inner(expr);
                            self.const_value_stack.push(value);
                        }
                    }
                }
                ConstTask::PushValue(value) => {
                    self.const_value_stack.push(value);
                }
                ConstTask::ApplyBinOp(op) => {
                    let rhs = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let lhs = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let value = self.handle_result(self.evaluate_binop(op, lhs, rhs));
                    self.const_value_stack.push(value);
                }
                ConstTask::ApplyUnOp(op) => {
                    let value = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let value = self.handle_result(self.evaluate_unary(op, value));
                    self.const_value_stack.push(value);
                }
                ConstTask::ApplyIf {
                    then_expr,
                    else_expr,
                } => {
                    let cond = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    match cond {
                        Value::Bool(b) => {
                            if b.value {
                                self.const_tasks.push(ConstTask::Eval(then_expr));
                            } else if let Some(elze) = else_expr {
                                self.const_tasks.push(ConstTask::Eval(elze));
                            } else {
                                self.const_value_stack.push(Value::unit());
                            }
                        }
                        _ => {
                            self.emit_error("expected boolean condition in const expression");
                            self.const_value_stack.push(Value::undefined());
                        }
                    }
                }
                ConstTask::ApplyCollect { len, kind } => {
                    let mut values = Vec::with_capacity(len);
                    for _ in 0..len {
                        values.push(self.const_value_stack.pop().unwrap_or_else(Value::undefined));
                    }
                    values.reverse();
                    let value = match kind {
                        CollectKind::Tuple => Value::Tuple(ValueTuple::new(values)),
                        CollectKind::Array => Value::List(ValueList::new(values)),
                    };
                    self.const_value_stack.push(value);
                }
                ConstTask::ApplyCast { ty } => {
                    let value = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let result = self.cast_value_to_type(value, &ty);
                    self.const_value_stack.push(result);
                }
                ConstTask::ApplyRange { inclusive } => {
                    let end = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let start = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let start = match self.numeric_to_i64(&start, "range start") {
                        Some(value) => value,
                        None => {
                            self.const_value_stack.push(Value::undefined());
                            continue;
                        }
                    };
                    let end = match self.numeric_to_i64(&end, "range end") {
                        Some(value) => value,
                        None => {
                            self.const_value_stack.push(Value::undefined());
                            continue;
                        }
                    };
                    let mut values = Vec::new();
                    let mut current = start;
                    while if inclusive {
                        current <= end
                    } else {
                        current < end
                    } {
                        values.push(Value::int(current));
                        current += 1;
                    }
                    self.const_value_stack.push(Value::List(ValueList::new(values)));
                }
                ConstTask::ApplySelect { field } => {
                    let target = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                    let value = self.evaluate_select(target, &field);
                    self.const_value_stack.push(value);
                }
                ConstTask::ApplyIndex {
                    has_start,
                    has_end,
                    inclusive,
                } => {
                    if has_start && has_end {
                        let index_value =
                            self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                        let target = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                        let value = self.evaluate_index(target, index_value);
                        self.const_value_stack.push(value);
                    } else {
                        let end = if has_end {
                            let end = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                            self.numeric_to_non_negative_usize(&end, "range end")
                        } else {
                            None
                        };
                        let start = if has_start {
                            let start =
                                self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                            self.numeric_to_non_negative_usize(&start, "range start")
                        } else {
                            None
                        };
                        let target = self.const_value_stack.pop().unwrap_or_else(Value::undefined);
                        let value = self.evaluate_range_index_slices(
                            target,
                            start,
                            end,
                            inclusive,
                        );
                        self.const_value_stack.push(value);
                    }
                }
            }
        }

        if !self.const_tasks.is_empty() {
            return EvalStepOutcome::Yielded;
        }

        match self.const_value_stack.pop() {
            Some(value) => EvalStepOutcome::Complete(value),
            None => EvalStepOutcome::Complete(Value::undefined()),
        }
    }

    fn eval_expr_inner(&mut self, expr: &mut Expr) -> Value {
        let _guard = self.push_span(expr.span);
        self.ensure_expr_typed(expr);
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
                self.apply_local_import_alias(locator);
                // First, try to specialize generic function reference if we have type info
                if should_specialize_fn_ref {
                    if let Some(expected_ty) = &expr_ty_snapshot {
                        fp_core::debug!(
                            "Attempting to specialize function reference {} with type {:?}",
                            locator,
                            expected_ty
                        );
                        if let Some(_specialized) =
                            self.specialize_function_reference(locator, expected_ty)
                        {
                            fp_core::debug!("Successfully specialized {} to {}", locator, locator);
                            // Locator has been updated to point to specialized function
                            // Return unit for now, the reference will be used by caller
                            return Value::unit();
                        } else {
                            fp_core::debug!("Failed to specialize {}", locator);
                        }
                    }
                }

                if let Some(ident) = locator.as_ident() {
                    if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                        let value = stored.value();
                        if let Some(placeholder) = self.imported_placeholder_value(value.clone()) {
                            return placeholder;
                        }
                        if matches!(value, Value::List(_) | Value::Map(_))
                            || matches!(stored, StoredValue::Shared(_))
                            || self.loop_depth > 0
                        {
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
                        let mut candidate_names =
                            vec![locator.to_string(), ident.as_str().to_string()];
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
                    if let Some(first) = call.args.first() {
                        if let ExprKind::Locator(locator) = first.kind() {
                            if let Some(ident) = locator.as_ident() {
                                assign_target = Some(ident.as_str().to_string());
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
            ExprKind::Reference(reference) => {
                if reference.mutable.unwrap_or(false) {
                    if let ExprKind::Locator(locator) = reference.referee.kind() {
                        if let Some(ident) = locator.as_ident() {
                            if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                                if let Some(shared) = stored.shared_handle() {
                                    return Value::Any(AnyBox::new(RuntimeRef { shared }));
                                }
                                self.emit_error(format!(
                                    "mutable reference requires mutable binding for '{}'",
                                    ident.as_str()
                                ));
                                return Value::undefined();
                            }
                        }
                    }
                    self.emit_error("mutable reference target must be a named binding");
                    Value::undefined()
                } else {
                    self.eval_expr(reference.referee.as_mut())
                }
            }
            ExprKind::Dereference(deref) => {
                if let ExprKind::Locator(locator) = deref.referee.kind() {
                    if let Some(ident) = locator.as_ident() {
                        if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                            return stored.value();
                        }
                    }
                }
                let target = self.eval_expr(deref.referee.as_mut());
                if let Value::Any(any) = target {
                    if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                        return runtime_ref
                            .shared
                            .lock()
                            .map(|value| value.clone())
                            .unwrap_or_else(|err| err.into_inner().clone());
                    }
                }
                self.emit_error("cannot dereference non-reference value");
                Value::undefined()
            }
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
            ExprKind::Return(_) => {
                self.emit_error("`return` is not supported during const evaluation");
                Value::undefined()
            }
            ExprKind::Break(_) => {
                self.emit_error("`break` is not supported during const evaluation");
                Value::undefined()
            }
            ExprKind::Continue(_) => {
                self.emit_error("`continue` is not supported during const evaluation");
                Value::undefined()
            }
            ExprKind::ConstBlock(const_block) => {
                self.enter_const_region();
                let value = self.eval_expr(const_block.expr.as_mut());
                self.exit_const_region();
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
                let parser = match self.macro_parser.clone() {
                    Some(parser) => parser,
                    None => {
                        self.emit_error_at(
                            macro_expr.invocation.span,
                            "macro expansion requires a parser hook",
                        );
                        return Value::undefined();
                    }
                };
                if self.macro_depth > 64 {
                    self.emit_error_at(
                        macro_expr.invocation.span,
                        "macro expansion exceeded recursion limit",
                    );
                    return Value::undefined();
                }
                self.macro_depth += 1;
                let expanded = self
                    .expand_macro_invocation(&macro_expr.invocation, MacroExpansionContext::Expr)
                    .and_then(|tokens| parser.parse_expr(&tokens));
                self.macro_depth = self.macro_depth.saturating_sub(1);
                match expanded {
                    Ok(mut new_expr) => {
                        if let Some(ty) = expr_ty_snapshot.clone() {
                            new_expr.ty = Some(ty);
                        }
                        *expr = new_expr;
                        self.mark_mutated();
                        self.eval_expr(expr)
                    }
                    Err(err) => {
                        self.emit_error_at(macro_expr.invocation.span, err.to_string());
                        Value::undefined()
                    }
                }
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
        if !invoke.kwargs.is_empty() {
            match invoke.target {
                ExprInvokeTarget::Function(_) => {}
                _ => {
                    self.emit_error("keyword arguments are only supported on function calls");
                    return Value::undefined();
                }
            }
        }
        if let ExprInvokeTarget::Method(select) = &mut invoke.target {
            let method_name = select.field.name.as_str();
            if matches!(
                method_name,
                "has_field"
                    | "has_method"
                    | "fields"
                    | "method_count"
                    | "field_name_at"
                    | "field_type"
                    | "type_name"
                    | "struct_size"
            ) {
                let receiver_value = self.eval_expr(select.obj.as_mut());
                let args = self.evaluate_args(&mut invoke.args);
                if let Some(value) = self.eval_type_method_call(receiver_value, method_name, args)
                {
                    return value;
                }
            }
            if method_name == "contains" && invoke.args.len() == 1 {
                let receiver_value = self.eval_expr(select.obj.as_mut());
                let needle = self.eval_expr(&mut invoke.args[0]);
                let Value::List(list) = receiver_value else {
                    self.emit_error("contains expects a list receiver");
                    return Value::undefined();
                };
                let found = list.values.iter().any(|value| match value {
                    Value::Structural(structural) => {
                        if let Some(field) = structural.get_field(&Ident::new("name".to_string()))
                        {
                            field.value == needle
                        } else {
                            *value == needle
                        }
                    }
                    _ => *value == needle,
                });
                return Value::bool(found);
            }
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
                                            self.set_pending_expr_ty(Some(Ty::unit()));
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
                                                self.set_pending_expr_ty(Some(Ty::unit()));
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
                self.set_pending_expr_ty(Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))));
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
                self.set_pending_expr_ty(Some(Ty::Primitive(TypePrimitive::Bool)));
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
                self.set_pending_expr_ty(Some(Ty::Primitive(TypePrimitive::String)));
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
                let mut locator = locator.clone();
                if let Some(ident) = locator.as_ident() {
                    if ident.as_str() == "type" {
                        if invoke.args.len() != 1 {
                            self.emit_error("type expects exactly one argument");
                            return Value::undefined();
                        }
                        let value = self.eval_expr(&mut invoke.args[0]);
                        return match value {
                            Value::Type(ty) => Value::Type(self.materialize_type(ty)),
                            other => Value::Type(self.type_from_value(&other)),
                        };
                    }
                }
                if invoke.args.is_empty() {
                    if let Some(value) = self.try_eval_method_chain(&locator) {
                        return value;
                    }
                }

                if let Some(value) =
                    self.try_handle_const_collection_invoke(&locator, &mut invoke.args)
                {
                    return value;
                }

                if let Some(function) =
                    self.resolve_function_call(&mut locator, invoke, ResolutionMode::Default)
                {
                    invoke.target = ExprInvokeTarget::Function(locator.clone());
                    // Call user-defined const function
                    let evaluated = self.evaluate_args(&mut invoke.args);
                    if let Some(stack) = Self::module_stack_from_locator(&locator) {
                        let saved = std::mem::take(&mut self.module_stack);
                        self.module_stack = stack;
                        let value = self.call_function(function, evaluated);
                        self.module_stack = saved;
                        return value;
                    }
                    return self.call_function(function, evaluated);
                }

                if let Some(Value::Function(function)) = self.lookup_callable_value(&locator) {
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

    fn module_stack_from_locator(locator: &Locator) -> Option<Vec<String>> {
        let path = match locator {
            Locator::Path(path) => path,
            Locator::ParameterPath(param_path) => {
                if param_path.segments.len() <= 1 {
                    return None;
                }
                return Some(
                    param_path
                        .segments
                        .iter()
                        .take(param_path.segments.len().saturating_sub(1))
                        .map(|seg| seg.ident.as_str().to_string())
                        .collect(),
                );
            }
            _ => return None,
        };
        if path.segments.len() <= 1 {
            return None;
        }
        Some(
            path.segments
                .iter()
                .take(path.segments.len().saturating_sub(1))
                .map(|seg| seg.name.clone())
                .collect(),
        )
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
        if !invoke.kwargs.is_empty() {
            match invoke.target {
                ExprInvokeTarget::Function(_) => {}
                _ => {
                    self.emit_error("keyword arguments are only supported on function calls");
                    return RuntimeFlow::Value(Value::undefined());
                }
            }
        }
        match &mut invoke.target {
            ExprInvokeTarget::Method(select) => {
                return self.eval_method_call_runtime(select, &mut invoke.args);
            }
            ExprInvokeTarget::Function(locator) => {
                let mut locator = locator.clone();
                if let Some(ident) = locator.as_ident() {
                    if ident.as_str() == "type" {
                        if invoke.args.len() != 1 {
                            self.emit_error("type expects exactly one argument");
                            return RuntimeFlow::Value(Value::undefined());
                        }
                        let flow = self.eval_expr_runtime(&mut invoke.args[0]);
                        let value = self.finish_runtime_flow(flow);
                        let output = match value {
                            Value::Type(ty) => Value::Type(self.materialize_type(ty)),
                            other => Value::Type(self.type_from_value(&other)),
                        };
                        return RuntimeFlow::Value(output);
                    }
                }
                if let Some(info) = self.lookup_enum_variant(&locator) {
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
                    self.try_handle_const_collection_invoke(&locator, &mut invoke.args)
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

                if let Some(function) =
                    self.resolve_function_call(&mut locator, invoke, ResolutionMode::Default)
                {
                    invoke.target = ExprInvokeTarget::Function(locator.clone());
                    let mut impl_context = None;
                    let segments = Self::locator_segments(&locator);
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
                        let flow = if let Some(stack) = Self::module_stack_from_locator(&locator) {
                            let saved = std::mem::take(&mut self.module_stack);
                            self.module_stack = stack;
                            let flow = self.call_function_runtime(function, args);
                            self.module_stack = saved;
                            flow
                        } else {
                            self.call_function_runtime(function, args)
                        };
                        self.impl_stack.pop();
                        return flow;
                    }
                    if let Some(stack) = Self::module_stack_from_locator(&locator) {
                        let saved = std::mem::take(&mut self.module_stack);
                        self.module_stack = stack;
                        let flow = self.call_function_runtime(function, args);
                        self.module_stack = saved;
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
                        if let Some(stack) = Self::module_stack_from_locator(&locator) {
                            let saved = std::mem::take(&mut self.module_stack);
                            self.module_stack = stack;
                            let flow = self.call_function_runtime(template.function.clone(), args);
                            self.module_stack = saved;
                            return flow;
                        }
                        return self.call_function_runtime(template.function.clone(), args);
                    }
                }

                if let Some(flow) = self.try_call_extern_function(&locator, &args) {
                    return flow;
                }

                if let Some(value) = self.lookup_callable_value(&locator) {
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

        if matches!(
            method_name.as_str(),
            "has_field"
                | "has_method"
                | "fields"
                | "method_count"
                | "field_name_at"
                | "field_type"
                | "type_name"
                | "struct_size"
        ) {
            let arg_values = match self.evaluate_args_runtime(args) {
                Ok(values) => values,
                Err(flow) => return flow,
            };
            if let Some(value) =
                self.eval_type_method_call(receiver.value.clone(), method_name.as_str(), arg_values)
            {
                return RuntimeFlow::Value(value);
            }
        }
        if method_name.as_str() == "contains" && args.len() == 1 {
            let Value::List(list) = &receiver.value else {
                self.emit_error("contains expects a list receiver");
                return RuntimeFlow::Value(Value::undefined());
            };
            let needle = match self.evaluate_args_runtime(args) {
                Ok(values) => values.into_iter().next().unwrap_or_else(Value::undefined),
                Err(flow) => return flow,
            };
            let found = list.values.iter().any(|value| match value {
                Value::Structural(structural) => {
                    if let Some(field) = structural.get_field(&Ident::new("name".to_string())) {
                        field.value == needle
                    } else {
                        *value == needle
                    }
                }
                _ => *value == needle,
            });
            return RuntimeFlow::Value(Value::bool(found));
        }

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

        if method_name == "push" && args.len() == 1 {
            let value = match self.evaluate_args_runtime(args) {
                Ok(mut values) => values.pop().unwrap_or_else(Value::undefined),
                Err(flow) => return flow,
            };
            let Some(shared) = receiver.shared else {
                self.emit_error("push requires a mutable receiver");
                return RuntimeFlow::Value(Value::undefined());
            };
            let mut guard = match shared.lock() {
                Ok(guard) => guard,
                Err(err) => err.into_inner(),
            };
            match &mut *guard {
                Value::List(list) => {
                    list.values.push(value);
                    return RuntimeFlow::Value(Value::unit());
                }
                other => {
                    self.emit_error(format!(
                        "'push' is only supported on Vec values, found {:?}",
                        other
                    ));
                    return RuntimeFlow::Value(Value::undefined());
                }
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
        invoke: &mut ExprInvoke,
        mode: ResolutionMode,
    ) -> Option<ItemDefFunction> {
        self.apply_local_import_alias(locator);
        if matches!(mode, ResolutionMode::Attribute) && !Self::locator_is_qualified(locator) {
            return None;
        }

        let mut candidate_names = vec![locator.to_string()];
        if matches!(mode, ResolutionMode::Default) {
            if let Some(ident) = locator.as_ident() {
                let simple = ident.as_str().to_string();
                if !candidate_names.contains(&simple) {
                    candidate_names.push(simple);
                }
            }
            if let Locator::Path(path) = locator {
                if path.segments.len() >= 2 {
                    let type_name = path.segments[path.segments.len() - 2].as_str();
                    let func_name = path.segments[path.segments.len() - 1].as_str();
                    let short = format!("{}::{}", type_name, func_name);
                    if !candidate_names.contains(&short) {
                        candidate_names.push(short);
                    }
                }
            }
            if let Locator::ParameterPath(path) = locator {
                if path.segments.len() >= 2 {
                    let type_name = path.segments[path.segments.len() - 2]
                        .ident
                        .as_str();
                    let func_name = path.segments[path.segments.len() - 1]
                        .ident
                        .as_str();
                    let short = format!("{}::{}", type_name, func_name);
                    if !candidate_names.contains(&short) {
                        candidate_names.push(short);
                    }
                }
            }
        }

        for name in &candidate_names {
            if let Some(function) = self.functions.get(name).cloned() {
                if !self.apply_kwargs_to_invoke(invoke, &function.sig.params) {
                    return None;
                }
                // Annotate arguments with expected parameter types
                self.annotate_invoke_args_slice(&mut invoke.args, &function.sig.params);
                return Some(function);
            }
        }

        for name in &candidate_names {
            if let Some(template) = self.generic_functions.get(name).cloned() {
                if !self.apply_kwargs_to_invoke(invoke, &template.function.sig.params) {
                    return None;
                }
                if let Some(function) =
                    self.instantiate_generic_function(name, template, locator, &mut invoke.args)
                {
                    // Annotate arguments now that we know the specialized function signature
                    self.annotate_invoke_args_slice(&mut invoke.args, &function.sig.params);
                    return Some(function);
                }
            }
        }

        // Fallback: find by fully qualified name
        if let Some(function) = self.functions.get(&locator.to_string()).cloned() {
            if !self.apply_kwargs_to_invoke(invoke, &function.sig.params) {
                return None;
            }
            self.annotate_invoke_args_slice(&mut invoke.args, &function.sig.params);
            Some(function)
        } else {
            None
        }
    }

    fn apply_kwargs_to_invoke(&mut self, invoke: &mut ExprInvoke, params: &[FunctionParam]) -> bool {
        if invoke.kwargs.is_empty() {
            return true;
        }

        let mut slots: Vec<Option<Expr>> = vec![None; params.len()];
        for (idx, arg) in invoke.args.drain(..).enumerate() {
            if idx >= params.len() {
                self.emit_error(format!(
                    "function expects {} arguments, found {}",
                    params.len(),
                    idx + 1
                ));
                return false;
            }
            slots[idx] = Some(arg);
        }

        for kwarg in invoke.kwargs.drain(..) {
            let pos = params
                .iter()
                .position(|param| param.name.as_str() == kwarg.name.as_str());
            let Some(index) = pos else {
                self.emit_error(format!("unknown keyword argument '{}'", kwarg.name));
                return false;
            };
            if slots[index].is_some() {
                self.emit_error(format!("duplicate keyword argument '{}'", kwarg.name));
                return false;
            }
            slots[index] = Some(kwarg.value);
        }

        for (idx, slot) in slots.iter_mut().enumerate() {
            if slot.is_some() {
                continue;
            }
            if let Some(default) = params[idx].default.as_ref() {
                *slot = Some(Expr::value(default.clone()));
                continue;
            }
            self.emit_error(format!(
                "missing argument '{}' at position {}",
                params[idx].name.as_str(),
                idx
            ));
            return false;
        }

        invoke.args = slots.into_iter().map(|slot| slot.unwrap()).collect();
        true
    }

    fn locator_is_qualified(locator: &Locator) -> bool {
        match locator {
            Locator::Ident(_) => false,
            Locator::Path(path) => path.segments.len() > 1,
            Locator::ParameterPath(path) => path.segments.len() > 1,
        }
    }

    fn apply_local_import_alias(&mut self, locator: &mut Locator) -> bool {
        match locator {
            Locator::Ident(ident) => {
                let Some(target) = self.local_imports.get(ident.as_str()) else {
                    return false;
                };
                let Some(new_locator) = Self::locator_from_import_target(target) else {
                    return false;
                };
                *locator = new_locator;
                true
            }
            Locator::Path(path) => {
                let Some(first) = path.segments.first() else {
                    return false;
                };
                let Some(target) = self.local_imports.get(first.as_str()) else {
                    return false;
                };
                let Some(mut segments) = Self::import_target_segments(target) else {
                    return false;
                };
                segments.extend(path.segments.iter().skip(1).cloned());
                *locator = Locator::path(Path::new(segments));
                true
            }
            Locator::ParameterPath(param_path) => {
                let Some(first) = param_path.segments.first() else {
                    return false;
                };
                let Some(target) = self.local_imports.get(first.ident.as_str()) else {
                    return false;
                };
                let Some(mut segments) = Self::import_target_segments(target) else {
                    return false;
                };
                for segment in param_path.segments.iter().skip(1) {
                    segments.push(segment.ident.clone());
                }
                *locator = Locator::path(Path::new(segments));
                true
            }
        }
    }

    fn locator_from_import_target(target: &str) -> Option<Locator> {
        let segments = Self::import_target_segments(target)?;
        Some(Locator::path(Path::new(segments)))
    }

    fn import_target_segments(target: &str) -> Option<Vec<Ident>> {
        let segments: Vec<Ident> = target
            .split("::")
            .filter(|segment| !segment.is_empty())
            .map(Ident::new)
            .collect();
        if segments.is_empty() {
            None
        } else {
            Some(segments)
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
