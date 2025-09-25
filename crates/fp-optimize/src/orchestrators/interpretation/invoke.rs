use super::*;

impl InterpretationOrchestrator {
    pub fn interpret_invoke(&self, node: &ExprInvoke, ctx: &SharedScopedContext) -> Result<Value> {
        // FIXME: call stack may not work properly
        match &node.target {
            ExprInvokeTarget::Function(locator) => {
                self.interpret_invoke_function(locator, &node.args, ctx)
            }
            ExprInvokeTarget::Method(select) => {
                self.interpret_invoke_method(select, &node.args, ctx)
            }
            ExprInvokeTarget::Expr(e) => self.interpret_invoke_expr(e, &node.args, ctx),
            ExprInvokeTarget::Type(_) => {
                opt_bail!("Type invocation not yet supported")
            }
            ExprInvokeTarget::Closure(_) => {
                opt_bail!("Closure invocation not yet supported")
            }
            ExprInvokeTarget::BinOp(op) => self.interpret_invoke_binop(op.clone(), &node.args, ctx),
        }
    }

    fn interpret_invoke_function(
        &self,
        locator: &Locator,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        match locator {
            Locator::Ident(ident) => {
                let func = self.interpret_ident(&ident, ctx, true)?;
                self.interpret_invoke(
                    &ExprInvoke {
                        target: ExprInvokeTarget::expr(Expr::value(func).into()),
                        args: args.to_vec(),
                    },
                    ctx,
                )
            }
            Locator::Path(path) => {
                // For single-segment paths like "println", try to get it as an identifier first
                if path.segments.len() == 1 {
                    let func = self.interpret_ident(&path.segments[0], ctx, true)?;
                    self.interpret_invoke(
                        &ExprInvoke {
                            target: ExprInvokeTarget::expr(Expr::value(func).into()),
                            args: args.to_vec(),
                        },
                        ctx,
                    )
                } else {
                    // For multi-segment paths, use context lookup
                    let func = ctx.get_value_recursive(path).ok_or_else(|| {
                        optimization_error(format!("could not find function {:?} in context", path))
                    })?;
                    self.interpret_invoke(
                        &ExprInvoke {
                            target: ExprInvokeTarget::expr(Expr::value(func).into()),
                            args: args.to_vec(),
                        },
                        ctx,
                    )
                }
            }
            Locator::ParameterPath(_) => {
                opt_bail!("ParameterPath invocation not yet supported")
            }
        }
    }

    fn interpret_invoke_method(
        &self,
        select: &ExprSelect,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        let obj_runtime = self.interpret_expr_runtime(&select.obj.get(), ctx)?;
        let method_name = &select.field.name;

        // First, try to find a custom method defined in an impl block
        if let Some(custom_method) = self.lookup_custom_method(&obj_runtime, method_name, ctx)? {
            // Handle receiver methods differently
            if let Value::Function(func) = &custom_method {
                if func.sig.receiver.is_some() {
                    // Method has a receiver (&self, &mut self, or self)
                    return self.interpret_invoke_method_with_receiver(
                        &custom_method,
                        &obj_runtime,
                        args,
                        ctx,
                    );
                } else {
                    // Static method or associated function - no receiver
                    return self.interpret_invoke(
                        &ExprInvoke {
                            target: ExprInvokeTarget::expr(Expr::value(custom_method).into()),
                            args: args.to_vec(),
                        },
                        ctx,
                    );
                }
            }
        }

        // Handle built-in string methods for const evaluation
        match (obj_runtime.get_value(), method_name.as_str()) {
            (Value::String(s), "len") => {
                if !args.is_empty() {
                    return Err(optimization_error(
                        "String.len() method takes no arguments".to_string(),
                    ));
                }
                return Ok(Value::int(s.value.len() as i64));
            }
            (Value::String(s), "contains") => {
                if args.len() != 1 {
                    return Err(optimization_error(
                        "String.contains() method takes exactly one argument".to_string(),
                    ));
                }
                let arg_val = self.interpret_expr(&args[0], ctx)?;
                if let Value::String(needle) = arg_val {
                    return Ok(Value::bool(s.value.contains(&needle.value)));
                } else {
                    return Err(optimization_error(
                        "String.contains() argument must be a string".to_string(),
                    ));
                }
            }
            (Value::String(s), "find") => {
                if args.len() != 1 {
                    return Err(optimization_error(
                        "String.find() method takes exactly one argument".to_string(),
                    ));
                }
                let arg_val = self.interpret_expr(&args[0], ctx)?;
                if let Value::String(needle) = arg_val {
                    match s.value.find(&needle.value) {
                        Some(pos) => return Ok(Value::int(pos as i64)),
                        None => return Ok(Value::int(-1)),
                    }
                } else {
                    return Err(optimization_error(
                        "String.find() argument must be a string".to_string(),
                    ));
                }
            }
            _ => {}
        }

        // Fall back to built-in methods via runtime pass
        let args_runtime: Vec<RuntimeValue> = args
            .iter()
            .map(|arg| self.interpret_expr_runtime(arg, ctx))
            .try_collect()?;

        let result_runtime = self
            .runtime_pass
            .call_method(obj_runtime, method_name, args_runtime)
            .map_err(|e| optimization_error(format!("Method call failed: {}", e)))?;

        Ok(result_runtime.get_value())
    }

    fn lookup_custom_method(
        &self,
        obj: &RuntimeValue,
        method_name: &str,
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        // Determine the type of the object
        let type_name = match obj.get_value() {
            Value::Struct(ref s) => s.ty.name.clone(),
            _ => return Ok(None), // Only structs can have custom methods for now
        };

        // Look up the method in context using TypeName::method_name
        let method_key = Ident::new(&format!("{}::{}", type_name.as_str(), method_name));

        Ok(ctx.get_value(method_key))
    }

    fn interpret_invoke_method_with_receiver(
        &self,
        method: &Value,
        obj: &RuntimeValue,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        if let Value::Function(func) = method {
            // Create a new context for the function call
            let func_ctx = ctx.child(Ident::new("__method_call__"), Visibility::Private, true);

            // Bind 'self' to the context based on receiver type
            func_ctx.insert_value(Ident::new("self"), obj.get_value());

            // Process regular arguments
            let processed_args = self.interpret_args(args, ctx)?;

            // Check argument count (excluding self)
            if processed_args.len() != func.sig.params.len() {
                opt_bail!(format!(
                    "Method {} expects {} arguments, got {}",
                    func.sig.name.as_ref().unwrap_or(&Ident::new("unknown")),
                    func.sig.params.len(),
                    processed_args.len()
                ));
            }

            // Bind regular parameters
            for (param, arg) in func.sig.params.iter().zip(processed_args.iter()) {
                func_ctx.insert_value(param.name.clone(), arg.clone());
            }

            // Execute the function body
            self.interpret_expr(&func.body, &func_ctx)
        } else {
            opt_bail!("Expected function value for method invocation")
        }
    }

    fn interpret_invoke_expr(
        &self,
        expr: &Expr,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        match expr {
            Expr::Value(value) => match value.as_ref() {
                Value::BinOpKind(kind) => self.interpret_invoke_binop(kind.clone(), args, ctx),
                Value::UnOpKind(func) => {
                    opt_ensure!(args.len() == 1, format!("Expected 1 arg for {:?}", func));
                    let arg = self.interpret_expr(&args[0].get(), ctx)?;
                    self.interpret_invoke_unop(func.clone(), arg, ctx)
                }
                Value::Function(func) => {
                    // Invoke a user-defined function
                    let args = self.interpret_args(args, ctx)?;
                    self.interpret_invoke_function_value(func, &args, ctx)
                }
                _ => opt_bail!(format!(
                    "Could not invoke expression with value {:?}",
                    value
                )),
            },
            Expr::Any(any) => {
                if let Some(exp) = any.downcast_ref::<BuiltinFn>() {
                    let args = self.interpret_args(args, ctx)?;
                    Ok(exp.invoke(&args, ctx)?)
                } else {
                    opt_bail!(format!("Could not invoke Any expression {:?}", any))
                }
            }
            _ => opt_bail!(format!("Could not invoke expression {:?}", expr)),
        }
    }

    fn interpret_invoke_function_value(
        &self,
        func: &ValueFunction,
        args: &[Value],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        // Create a new context for the function call
        let func_ctx = ctx.child(Ident::new("__function_call__"), Visibility::Private, true);

        // Debug information
        let anonymous_name = Ident::new("anonymous");
        let func_name = func.sig.name.as_ref().unwrap_or(&anonymous_name);
        let param_names: Vec<String> = func.sig.params.iter().map(|p| p.name.to_string()).collect();

        // Check argument count
        if args.len() != func.sig.params.len() {
            opt_bail!(format!(
                "Function {} expects {} arguments, got {}. Params: [{}], Args: [{}]",
                func_name,
                func.sig.params.len(),
                args.len(),
                param_names.join(", "),
                args.iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        // Bind arguments to parameters
        for (param, arg) in func.sig.params.iter().zip(args.iter()) {
            func_ctx.insert_value(param.name.clone(), arg.clone());
        }

        // Execute the function body
        self.interpret_expr(&func.body, &func_ctx)
    }

    pub fn interpret_args(&self, node: &[Expr], ctx: &SharedScopedContext) -> Result<Vec<Value>> {
        // Evaluate each argument to a concrete value so builtins like println!
        // receive consts/literals rather than locals/expr wrappers in comptime mode.
        node.iter()
            .map(|x| self.interpret_expr(&x.get(), ctx))
            .try_collect()
    }

    pub fn interpret_invoke_binop(
        &self,
        op: BinOpKind,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        let builtin_fn = self.lookup_bin_op_kind(op)?;
        let args = self.interpret_args(args, ctx)?;
        Ok(builtin_fn.invoke(&args, ctx)?)
    }

    pub fn interpret_invoke_unop(
        &self,
        op: UnOpKind,
        arg: Value,
        _ctx: &SharedScopedContext,
    ) -> Result<Value> {
        match op {
            UnOpKind::Neg => match arg {
                Value::Int(val) => Ok(Value::Int(ValueInt::new(-val.value))),
                Value::Decimal(val) => Ok(Value::Decimal(ValueDecimal::new(-val.value))),
                _ => opt_bail!(format!("Failed to interpret {:?}", op)),
            },
            UnOpKind::Not => match arg {
                Value::Bool(val) => Ok(Value::Bool(ValueBool::new(!val.value))),
                _ => opt_bail!(format!("Failed to interpret {:?}", op)),
            },
            _ => opt_bail!(format!("Could not process {:?}", op)),
        }
    }
}
