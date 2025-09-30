use super::*;
use fp_rust::parser::RustParser;
use syn::parse::Parser;

impl InterpretationOrchestrator {
    pub fn interpret_cond(&self, node: &ExprMatch, ctx: &SharedScopedContext) -> Result<Value> {
        for case in &node.cases {
            let interpret = self.interpret_expr(&case.cond, ctx)?;
            match interpret {
                Value::Bool(x) => {
                    if x.value {
                        return self.interpret_expr(&case.body, ctx);
                    } else {
                        continue;
                    }
                }
                _ => {
                    opt_bail!(format!(
                        "Failed to interpret {:?} => {:?}",
                        case.cond, interpret
                    ))
                }
            }
        }
        Ok(Value::unit())
    }

    pub fn interpret_print(
        se: &dyn AstSerializer,
        args: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let formatted: Vec<_> = args
            .into_iter()
            .map(|x| se.serialize_expr(x))
            .try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(())
    }

    pub fn interpret_ident(
        &self,
        ident: &Ident,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match ident.as_str() {
            // TODO: can we remove these?
            "+" if resolve => Ok(Value::any(builtin_add())),
            "-" if resolve => Ok(Value::any(builtin_sub())),
            "*" if resolve => Ok(Value::any(builtin_mul())),
            ">" if resolve => Ok(Value::any(builtin_gt())),
            ">=" if resolve => Ok(Value::any(builtin_ge())),
            "==" if resolve => Ok(Value::any(builtin_eq())),
            "<=" if resolve => Ok(Value::any(builtin_le())),
            "<" if resolve => Ok(Value::any(builtin_lt())),
            "&&" if resolve => Ok(Value::any(builtin_and())),
            "||" if resolve => Ok(Value::any(builtin_or())),
            "print" if resolve => Ok(Value::any(builtin_print(self.serializer.clone()))),
            "println!" if resolve => Ok(Value::any(builtin_println(self.serializer.clone()))),
            "println" if resolve => Ok(Value::any(builtin_println(self.serializer.clone()))),
            "strlen!" if resolve => Ok(Value::any(builtin_strlen())),
            "strlen" if resolve => Ok(Value::any(builtin_strlen_fn())),
            "concat!" if resolve => Ok(Value::any(builtin_concat())),
            "concat" if resolve => Ok(Value::any(builtin_concat_fn())),
            "true" => Ok(Value::bool(true)),
            "false" => Ok(Value::bool(false)),
            "None" => Ok(Value::None(ValueNone)),
            "null" => Ok(Value::Null(ValueNull)),
            "unit" => Ok(Value::Unit(ValueUnit)),
            "undefined" => Ok(Value::Undefined(ValueUndefined)),
            "Some" => Ok(Value::any(builtin_some())),
            // Metaprogramming intrinsics
            // Core introspection intrinsics
            "sizeof!" if resolve => Ok(Value::any(builtin_sizeof())),
            "reflect_fields!" if resolve => Ok(Value::any(builtin_reflect_fields())),
            "hasmethod!" if resolve => Ok(Value::any(builtin_hasmethod())),
            "type_name!" if resolve => Ok(Value::any(builtin_type_name())),

            // Struct creation and manipulation intrinsics
            "create_struct!" if resolve => Ok(Value::any(builtin_create_struct())),
            "clone_struct!" if resolve => Ok(Value::any(builtin_clone_struct())),
            "addfield!" if resolve => Ok(Value::any(builtin_addfield())),

            // Struct querying intrinsics
            "hasfield!" if resolve => Ok(Value::any(builtin_hasfield())),
            "field_count!" if resolve => Ok(Value::any(builtin_field_count())),
            "method_count!" if resolve => Ok(Value::any(builtin_method_count())),
            "field_type!" if resolve => Ok(Value::any(builtin_field_type())),
            "struct_size!" if resolve => Ok(Value::any(builtin_struct_size())),

            // Code generation intrinsics
            "generate_method!" if resolve => Ok(Value::any(builtin_generate_method())),

            // Compile-time validation intrinsics
            "compile_error!" if resolve => Ok(Value::any(builtin_compile_error())),
            "compile_warning!" if resolve => Ok(Value::any(builtin_compile_warning())),
            _ => {
                debug!("Get value recursive {:?}", ident);
                ctx.get_value_recursive(ident).ok_or_else(|| {
                    optimization_error(format!("could not find {:?} in context", ident.name))
                })
            }
        }
    }
    // Bitwise AND operation

    pub fn interpret_format_string(
        &self,
        format_str: &fp_core::ast::ExprFormatString,
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        tracing::debug!(
            "Interpreting structured format string with {} parts and {} args",
            format_str.parts.len(),
            format_str.args.len()
        );

        // Evaluate all arguments
        let mut evaluated_args: Vec<Value> = Vec::new();
        for (i, arg) in format_str.args.iter().enumerate() {
            tracing::debug!("Evaluating format arg {}: {:?}", i, arg);
            let value = self.interpret_expr(arg, ctx)?;
            tracing::debug!("Arg {} evaluated to value: {:?}", i, value);
            evaluated_args.push(value);
        }

        // TODO: Evaluate kwargs when we support named arguments

        // Process template parts
        let mut result = String::new();
        let mut implicit_arg_index = 0;

        for (i, part) in format_str.parts.iter().enumerate() {
            match part {
                fp_core::ast::FormatTemplatePart::Literal(literal) => {
                    tracing::debug!("Part {}: Adding literal '{}'", i, literal);
                    result.push_str(literal);
                }
                fp_core::ast::FormatTemplatePart::Placeholder(placeholder) => {
                    tracing::debug!(
                        "Part {}: Processing placeholder {:?}",
                        i,
                        placeholder.arg_ref
                    );

                    // Determine which argument to use
                    let selected_value = match &placeholder.arg_ref {
                        fp_core::ast::FormatArgRef::Implicit => {
                            if let Some(value) = evaluated_args.get(implicit_arg_index) {
                                implicit_arg_index += 1;
                                Some(value)
                            } else {
                                tracing::warn!("Not enough arguments for implicit placeholder");
                                None
                            }
                        }
                        fp_core::ast::FormatArgRef::Positional(index) => {
                            if let Some(value) = evaluated_args.get(*index) {
                                Some(value)
                            } else {
                                tracing::warn!("Positional argument {} out of range", index);
                                None
                            }
                        }
                        fp_core::ast::FormatArgRef::Named(name) => {
                            tracing::warn!("Named argument '{}' not yet supported", name);
                            None
                        }
                    };

                    if let Some(value) = selected_value {
                        let spec = placeholder.format_spec.as_deref();
                        let formatted = format_value_with_spec(value, spec)?;
                        tracing::debug!(
                            "Part {}: Substituting placeholder with '{}'",
                            i,
                            formatted
                        );
                        result.push_str(&formatted);
                    } else {
                        result.push_str("{}");
                    }
                }
            }
        }

        tracing::debug!("Final structured format string result: '{}'", result);
        Ok(Value::String(fp_core::ast::ValueString::new_owned(result)))
    }

    // Helper method to convert Value to string representation

    fn value_to_string(&self, value: &Value) -> Result<String> {
        format_value_with_spec(value, None)
    }

    // If expression handler

    pub fn interpret_if_expr(
        &self,
        if_expr: &fp_core::ast::ExprIf,
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        // Evaluate the condition
        let condition = self.interpret_expr(&if_expr.cond, ctx)?;

        // Check if condition is a boolean
        match condition {
            Value::Bool(b) => {
                if b.value {
                    // Execute then branch
                    self.interpret_expr(&if_expr.then, ctx)
                } else {
                    // Execute else branch if it exists
                    if let Some(else_expr) = &if_expr.elze {
                        self.interpret_expr(else_expr, ctx)
                    } else {
                        Ok(Value::unit()) // No else branch, return unit
                    }
                }
            }
            Value::Any(_) => {
                // Handle the case where condition contains unsupported expressions (like cfg! macro)
                Err(optimization_error(
                    "Cannot evaluate if condition: contains unsupported cfg! macro. The cfg!(debug_assertions) macro is not supported in const evaluation. Use a literal boolean instead.".to_string()
                ))
            }
            _ => Err(optimization_error(format!(
                "If condition must be boolean, got: {:?}",
                condition
            ))),
        }
    }

    pub fn interpret_assign(
        &self,
        assign: &fp_core::ast::ExprAssign,
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        // Evaluate the right-hand side first
        let value = self.interpret_expr(&assign.value, ctx)?;

        // Now handle the assignment target
        match &*assign.target {
            Expr::Locator(locator) => {
                // Simple variable assignment: x = value
                if let Some(ident) = locator.as_ident() {
                    ctx.insert_value(ident.as_str(), value.clone());
                    Ok(value)
                } else {
                    Err(optimization_error(
                        "Assignment target must be a simple identifier".to_string(),
                    ))
                }
            }
            Expr::Select(select) => {
                // Field assignment: obj.field = value
                match &*select.obj {
                    Expr::Locator(locator) => {
                        if let Some(var_name) = locator.as_ident() {
                            // Get the current struct value
                            if let Some(current_value) = ctx.get_value(var_name.clone()) {
                                match current_value {
                                    Value::Struct(mut struct_val) => {
                                        // Find and update the field
                                        for field in &mut struct_val.structural.fields {
                                            if field.name == select.field {
                                                field.value = value.clone();
                                                // Update the struct in context
                                                ctx.insert_value(
                                                    var_name.clone(),
                                                    Value::Struct(struct_val),
                                                );
                                                return Ok(value);
                                            }
                                        }
                                        Err(optimization_error(format!(
                                            "Field '{}' not found in struct",
                                            select.field
                                        )))
                                    }
                                    _ => Err(optimization_error(format!(
                                        "Cannot assign field to non-struct type: {:?}",
                                        current_value
                                    ))),
                                }
                            } else {
                                Err(optimization_error(format!(
                                    "Variable '{}' not found",
                                    var_name
                                )))
                            }
                        } else {
                            Err(optimization_error(
                                "Field assignment target must be a simple variable".to_string(),
                            ))
                        }
                    }
                    _ => Err(optimization_error(
                        "Field assignment to complex expressions not yet supported".to_string(),
                    )),
                }
            }
            _ => Err(optimization_error(format!(
                "Unsupported assignment target: {:?}",
                assign.target
            ))),
        }
    }

    pub fn interpret_struct_expr(
        &self,
        node: &ExprStruct,
        ctx: &SharedScopedContext,
    ) -> Result<ValueStruct> {
        let value: Value = self.interpret_expr(&node.name.get(), ctx)?.try_conv()?;
        let ty: Ty = value.try_conv()?;
        let struct_ = ty.try_conv()?;
        let fields: Vec<_> = node
            .fields
            .iter()
            .map(|x| {
                Ok::<_, fp_core::error::Error>(ValueField {
                    name: x.name.clone(),

                    value: match &x.value {
                        Some(value) => self.interpret_expr(value, ctx)?,
                        None => self.interpret_expr(&Expr::ident(x.name.clone()), ctx)?,
                    },
                })
            })
            .try_collect()?;
        Ok(ValueStruct {
            ty: struct_,
            structural: ValueStructural { fields },
        })
    }

    pub fn interpret_struct_value(
        &self,
        node: &ValueStruct,
        ctx: &SharedScopedContext,
    ) -> Result<ValueStruct> {
        let fields: Vec<_> = node
            .structural
            .fields
            .iter()
            .map(|x| {
                Ok::<_, fp_core::error::Error>(ValueField {
                    name: x.name.clone(),
                    value: self.interpret_value(&x.value, ctx, true)?,
                })
            })
            .try_collect()?;
        Ok(ValueStruct {
            ty: node.ty.clone(),
            structural: ValueStructural { fields },
        })
    }

    pub fn interpret_select(&self, s: &ExprSelect, ctx: &SharedScopedContext) -> Result<Value> {
        tracing::debug!(
            "Interpreting field access: {}.{}",
            self.serializer
                .serialize_expr(&s.obj.get())
                .unwrap_or_default(),
            s.field.name
        );
        let obj0 = self.interpret_expr(&s.obj.get(), ctx)?;
        tracing::debug!("Field access object resolved to: {:?}", obj0);
        let obj = obj0.as_structural().ok_or_else(|| {
            optimization_error(format!(
                "Expected structural type, got {}",
                self.serializer.serialize_value(&obj0).unwrap_or_default()
            ))
        })?;

        let value = obj.get_field(&s.field).ok_or_else(|| {
            optimization_error(format!(
                "Could not find field {} in {}",
                s.field,
                self.serializer.serialize_value(&obj0).unwrap_or_default()
            ))
        })?;

        Ok(value.value.clone())
    }

    pub fn interpret_tuple(
        &self,
        node: &ValueTuple,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<ValueTuple> {
        let values: Vec<_> = node
            .values
            .iter()
            .map(|x| self.interpret_value(x, ctx, resolve))
            .try_collect()?;
        Ok(ValueTuple {
            values: values.into_iter().map(|x| x.into()).collect(),
        })
    }

    pub fn interpret_type(&self, node: &Ty, ctx: &SharedScopedContext) -> Result<Ty> {
        // TODO: handle closure
        self.evaluate_type_value(node, ctx)
    }

    pub fn interpret_function_value(
        &self,
        node: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<ValueFunction> {
        // TODO: handle unnamed function, need to pass closure to here
        let (_, context) = ctx
            .get_function(node.name.clone().unwrap())
            .ok_or_else(|| {
                optimization_error(format!(
                    "Could not find function {} in context",
                    node.sig.name.as_ref().unwrap()
                ))
            })?;

        let sub = context.child(Ident::new("__call__"), Visibility::Private, true);
        for generic in &node.generics_params {
            let ty = self.evaluate_type_bounds(&generic.bounds, ctx)?;
            sub.insert_value_with_ctx(generic.name.clone(), ty.into());
        }
        let params: Vec<_> = node
            .params
            .iter()
            .map(|x| {
                Ok::<_, fp_core::error::Error>(FunctionParam::new(
                    x.name.clone(),
                    self.interpret_type(&x.ty, &sub)?,
                ))
            })
            .try_collect()?;
        let sig = FunctionSignature {
            name: node.sig.name.clone(),
            receiver: None,
            params,
            generics_params: node.generics_params.clone(),
            ret_ty: if let Some(ret_ty) = &node.sig.ret_ty {
                Some(self.interpret_type(ret_ty, &sub)?)
            } else {
                None
            },
        };

        Ok(ValueFunction {
            sig,
            body: node.body.clone(),
        })
    }

    pub fn interpret_value(
        &self,
        val: &Value,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match val {
            Value::Type(n) => self.interpret_type(n, ctx).map(Value::Type),
            Value::Struct(n) => self.interpret_struct_value(n, ctx).map(Value::Struct),
            Value::Structural(_) => opt_bail!(format!("Failed to interpret {:?}", val)),
            Value::Function(n) => self.interpret_function_value(n, ctx).map(Value::Function),
            Value::Tuple(n) => self.interpret_tuple(n, ctx, resolve).map(Value::Tuple),
            Value::Expr(n) => self.interpret_expr(&n.get(), ctx),
            Value::Any(_n) => {
                if self.ignore_missing_items {
                    return Ok(val.clone());
                }

                opt_bail!(format!("Failed to interpret {:?}", val))
            }
            Value::Some(val) => Ok(Value::Some(ValueSome::new(
                self.interpret_value(&val.value, ctx, resolve)?.into(),
            ))),
            Value::Option(value) => Ok(Value::Option(ValueOption::new(
                value
                    .value
                    .as_ref()
                    .map(|x| self.interpret_value(&x, ctx, resolve))
                    .transpose()?,
            ))),
            Value::BinOpKind(x) if resolve => {
                self.lookup_bin_op_kind(x.clone()).map(|x| Value::any(x))
            }
            _ => Ok(val.clone()),
        }
    }

    pub fn lookup_bin_op_kind(&self, op: BinOpKind) -> Result<BuiltinFn> {
        use BinOpKind::*;
        let builtin = match op {
            Add | AddTrait => builtin_add(),
            Sub => builtin_sub(),
            Mul => builtin_mul(),
            Gt => builtin_gt(),
            Ge => builtin_ge(),
            Lt => builtin_lt(),
            Le => builtin_le(),
            Eq => builtin_eq(),
            Ne => builtin_ne(),
            And => builtin_and(),
            Or => builtin_or(),
            _ => {
                return Err(optimization_error(format!(
                    "Binary operator {:?} not supported in const evaluation",
                    op
                )))
            }
        };
        Ok(builtin)
    }

    pub fn interpret_binop(&self, binop: &ExprBinOp, ctx: &SharedScopedContext) -> Result<Value> {
        let builtin_fn = self.lookup_bin_op_kind(binop.kind.clone())?;
        let lhs = self.interpret_expr(&binop.lhs.get(), ctx)?;
        let rhs = self.interpret_expr(&binop.rhs.get(), ctx)?;
        Ok(builtin_fn.invoke(&vec![lhs, rhs], ctx)?)
    }

    pub fn interpret_expr_common(
        &self,
        node: &Expr,
        ctx: &SharedScopedContext,
        resolve: bool,
    ) -> Result<Value> {
        match node {
            Expr::Locator(Locator::Ident(n)) => self.interpret_ident(n, ctx, resolve),
            Expr::Locator(n) => ctx
                .get_value_recursive(n.to_path())
                .ok_or_else(|| optimization_error(format!("could not find {:?} in context", n))),
            Expr::Value(n) => self.interpret_value(n, ctx, resolve),
            Expr::Block(n) => self.interpret_block(n, ctx),
            Expr::IntrinsicCall(call) => {
                use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

                match call.kind {
                    IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                        let template = match &call.payload {
                            IntrinsicCallPayload::Format { template } => template,
                            IntrinsicCallPayload::Args { .. } => {
                                return Err(optimization_error(
                                    "print intrinsics require format payload",
                                ))
                            }
                        };
                        let formatted = self.interpret_format_string(template, ctx)?;
                        let mut text = self.value_to_string(&formatted)?;
                        if matches!(call.kind, IntrinsicCallKind::Println) {
                            text.push('\n');
                        }
                        ctx.root().print_str(text);
                        Ok(Value::unit())
                    }
                    IntrinsicCallKind::Len => {
                        let arg_expr = match &call.payload {
                            IntrinsicCallPayload::Args { args } => args.first().ok_or_else(|| {
                                optimization_error("len intrinsic expects a single argument")
                            })?,
                            IntrinsicCallPayload::Format { .. } => {
                                return Err(optimization_error(
                                    "len intrinsic should not use format payload",
                                ))
                            }
                        };
                        let value = self.interpret_expr(arg_expr, ctx)?;
                        self.perform_strlen(&[value])
                    }
                }
            }
            Expr::Match(c) => self.interpret_cond(c, ctx),
            Expr::Invoke(invoke) => self.interpret_invoke(invoke, ctx),
            Expr::BinOp(op) => self.interpret_binop(op, ctx),
            Expr::UnOp(op) => {
                let arg = self.interpret_expr(&op.val, ctx)?;
                self.interpret_invoke_unop(op.op.clone(), arg, ctx)
            }
            Expr::Any(n) => {
                // Handle macros specially
                if let Some(raw_macro) = n.downcast_ref::<fp_rust::RawExprMacro>() {
                    // Check if this is a cfg! macro by looking at the macro path
                    if raw_macro.raw.mac.path.is_ident("cfg") {
                        // cfg! macro - for now, assume debug mode is disabled in const evaluation
                        return Ok(Value::bool(false));
                    }

                    // Handle strlen! macro
                    if raw_macro.raw.mac.path.is_ident("strlen") {
                        let values =
                            self.evaluate_macro_arguments(&raw_macro.raw.mac.tokens, ctx)?;
                        let builtin = builtin_strlen();
                        return builtin.invoke(&values, ctx);
                    }

                    // Handle concat! macro
                    if raw_macro.raw.mac.path.is_ident("concat") {
                        let values =
                            self.evaluate_macro_arguments(&raw_macro.raw.mac.tokens, ctx)?;
                        let builtin = builtin_concat();
                        return builtin.invoke(&values, ctx);
                    }

                    // Handle introspection macros
                    if raw_macro.raw.mac.path.is_ident("sizeof") {
                        // Parse the argument inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();

                        // Try to get the struct type from context
                        let ident = fp_core::id::Ident::new(arg_name);

                        // First try as a type
                        if let Some(type_value) =
                            ctx.get_type(fp_core::id::Path::from(ident.clone()))
                        {
                            let sizeof_builtin = builtin_sizeof();
                            return sizeof_builtin.invoke(&[Value::Type(type_value)], ctx);
                        }

                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let Value::Type(type_value) = value {
                                let sizeof_builtin = builtin_sizeof();
                                return sizeof_builtin.invoke(&[Value::Type(type_value)], ctx);
                            }
                        }

                        opt_bail!(format!("sizeof! could not find type: {}", arg_name));
                    }

                    if raw_macro.raw.mac.path.is_ident("field_count") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();

                        let ident = fp_core::id::Ident::new(arg_name);

                        // First try as a type
                        if let Some(type_value) =
                            ctx.get_type(fp_core::id::Path::from(ident.clone()))
                        {
                            let field_count_builtin = builtin_field_count();
                            return field_count_builtin.invoke(&[Value::Type(type_value)], ctx);
                        }

                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let Value::Type(type_value) = value {
                                let field_count_builtin = builtin_field_count();
                                return field_count_builtin.invoke(&[Value::Type(type_value)], ctx);
                            }
                        }

                        opt_bail!(format!("field_count! could not find type: {}", arg_name));
                    }

                    if raw_macro.raw.mac.path.is_ident("method_count") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();
                        let arg_name = tokens_str.trim();

                        let ident = fp_core::id::Ident::new(arg_name);

                        // First try as a type
                        if let Some(type_value) =
                            ctx.get_type(fp_core::id::Path::from(ident.clone()))
                        {
                            let method_count_builtin = builtin_method_count();
                            return method_count_builtin.invoke(&[Value::Type(type_value)], ctx);
                        }

                        // Then try as a value (which might be a struct definition)
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            if let Value::Type(type_value) = value {
                                let method_count_builtin = builtin_method_count();
                                return method_count_builtin
                                    .invoke(&[Value::Type(type_value)], ctx);
                            }
                        }

                        opt_bail!(format!("method_count! could not find type: {}", arg_name));
                    }

                    if raw_macro.raw.mac.path.is_ident("hasfield") {
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();

                        // Parse: Type, "field_name"
                        if let Some((type_name, field_name)) = tokens_str.split_once(',') {
                            let type_name = type_name.trim();
                            let field_name = field_name.trim();

                            // Remove quotes from field_name
                            let field_name =
                                if field_name.starts_with('"') && field_name.ends_with('"') {
                                    &field_name[1..field_name.len() - 1]
                                } else {
                                    field_name
                                };

                            let ident = fp_core::id::Ident::new(type_name);

                            // First try as a type
                            if let Some(type_value) =
                                ctx.get_type(fp_core::id::Path::from(ident.clone()))
                            {
                                let hasfield_builtin = builtin_hasfield();
                                return hasfield_builtin.invoke(
                                    &[
                                        Value::Type(type_value),
                                        Value::string(field_name.to_string()),
                                    ],
                                    ctx,
                                );
                            }

                            // Then try as a value (which might be a struct definition)
                            if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                                if let Value::Type(type_value) = value {
                                    let hasfield_builtin = builtin_hasfield();
                                    return hasfield_builtin.invoke(
                                        &[
                                            Value::Type(type_value),
                                            Value::string(field_name.to_string()),
                                        ],
                                        ctx,
                                    );
                                }
                            }

                            opt_bail!(format!("hasfield! could not find type: {}", type_name));
                        } else {
                            opt_bail!(
                                "hasfield! expects 2 arguments: hasfield!(Type, \"field_name\")"
                            );
                        }
                    }
                }
                Ok(Value::Any(n.clone()))
            }
            Expr::Select(s) => self.interpret_select(s, ctx),
            Expr::Struct(s) => self.interpret_struct_expr(s, ctx).map(Value::Struct),
            Expr::Paren(p) => self.interpret_expr(&p.expr, ctx),
            Expr::If(if_expr) => self.interpret_if_expr(if_expr, ctx),
            Expr::Assign(assign) => self.interpret_assign(assign, ctx),
            Expr::FormatString(format_str) => self.interpret_format_string(format_str, ctx),
            _ => opt_bail!(format!(
                "Unsupported expression type in interpreter: {:?}",
                node
            )),
        }
    }

    pub fn interpret_expr(&self, node: &Expr, ctx: &SharedScopedContext) -> Result<Value> {
        self.interpret_expr_common(node, ctx, true)
    }

    pub fn interpret_expr_no_resolve(
        &self,
        node: &Expr,
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        self.interpret_expr_common(node, ctx, false)
    }

    fn parse_macro_arguments(&self, tokens: &proc_macro2::TokenStream) -> Result<Vec<Expr>> {
        let parser = RustParser::new();
        let parsed = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
            .parse2(tokens.clone())
            .map_err(|err| {
                optimization_error(format!("Failed to parse macro arguments: {}", err))
            })?;

        let mut expressions = Vec::with_capacity(parsed.len());
        for syn_expr in parsed.into_iter() {
            let expr = parser.parse_expr(syn_expr).map_err(|err| {
                optimization_error(format!("Failed to lower macro argument: {}", err))
            })?;
            expressions.push(expr);
        }

        Ok(expressions)
    }

    fn evaluate_macro_arguments(
        &self,
        tokens: &proc_macro2::TokenStream,
        ctx: &SharedScopedContext,
    ) -> Result<Vec<Value>> {
        let expressions = self.parse_macro_arguments(tokens)?;
        let mut values = Vec::with_capacity(expressions.len());
        for expr in expressions {
            values.push(self.interpret_expr(&expr, ctx)?);
        }
        Ok(values)
    }

    pub fn evaluate_const_expression(
        &self,
        expr: &Expr,
        ctx: &SharedScopedContext,
        _intrinsic_context: &crate::utils::IntrinsicEvaluationContext,
    ) -> fp_core::error::Result<Value> {
        // For now, delegate to the existing interpreter
        // TODO: Integrate with side-effect-aware intrinsics
        self.interpret_expr_no_resolve(expr, ctx)
    }
}
