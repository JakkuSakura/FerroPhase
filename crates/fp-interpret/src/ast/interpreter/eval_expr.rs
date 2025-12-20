use super::*;

impl<'ctx> AstInterpreter<'ctx> {
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
                self.emit_error(
                    "quote cannot be evaluated directly; use it with splice inside const blocks",
                );
                return Value::undefined();
            }
            ExprKind::Splice(_splice) => {
                self.emit_error(
                    "splice cannot be evaluated to a value; it is only valid inside const regions to insert code",
                );
                return Value::undefined();
            }
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(inner) => {
                    let mut cloned = inner.as_ref().clone();
                    self.eval_expr(&mut cloned)
                }
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
                        self.resolve_qualified(locator.to_string())
                    }
                } else {
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
            ExprKind::IntrinsicCall(call) => {
                let kind = call.kind;
                let value = self.eval_intrinsic(call);
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
                self.emit_error("assignment is not allowed during AST interpretation");
                self.eval_expr(assign.value.as_mut())
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
                self.pending_closure = Some(captured);
                Value::unit()
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
        match self.mode {
            InterpreterMode::CompileTime => self.eval_invoke_compile_time(invoke),
            InterpreterMode::RunTime => self.eval_invoke_runtime(invoke),
        }
    }

    pub(super) fn eval_invoke_compile_time(&mut self, invoke: &mut ExprInvoke) -> Value {
        if let ExprInvokeTarget::Method(select) = &mut invoke.target {
            if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                return match value {
                    Value::List(list) => Value::int(list.values.len() as i64),
                    Value::Map(map) => Value::int(map.len() as i64),
                    other => {
                        self.emit_error(format!(
                            "'len' is only supported on compile-time arrays, lists, and maps, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
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

            self.emit_error("method calls are not supported in const evaluation yet");
            return Value::undefined();
        }

        match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
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

    pub(super) fn eval_invoke_runtime(&mut self, invoke: &mut ExprInvoke) -> Value {
        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if self.is_printf_symbol(&locator.to_string()) {
                let mut evaluated = Vec::with_capacity(invoke.args.len());
                for arg in invoke.args.iter_mut() {
                    evaluated.push(self.eval_expr(arg));
                }
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
                return Value::unit();
            }
        }

        for arg in invoke.args.iter_mut() {
            self.eval_expr(arg);
        }

        self.emit_error("runtime function calls are not yet supported in interpreter mode");
        Value::undefined()
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
