mod typing;

use crate::utils::{FoldOptimizer, OptimizePass};
// Replace common::* with specific imports
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::{Context, ValueSystem};
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use fp_core::ops::*;
use fp_core::passes::{LiteralRuntimePass, RuntimePass};
use fp_core::utils::conv::TryConv;
use itertools::Itertools;
use std::sync::Arc;
use tracing::debug;
// use std::any::Any;

// Import our error helpers
use crate::error::optimization_error;
use crate::opt_bail;
use crate::opt_ensure;

#[derive(Clone)]
pub struct InterpretationOrchestrator {
    pub serializer: Arc<dyn AstSerializer>,
    pub ignore_missing_items: bool,
    pub runtime_pass: Arc<dyn RuntimePass>,
}

impl InterpretationOrchestrator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            serializer,
            ignore_missing_items: false,
            runtime_pass: Arc::new(LiteralRuntimePass::default()),
        }
    }

    pub fn with_runtime_pass(mut self, runtime_pass: Arc<dyn RuntimePass>) -> Self {
        self.runtime_pass = runtime_pass;
        self
    }

    pub fn set_runtime_pass(&mut self, runtime_pass: Arc<dyn RuntimePass>) {
        self.runtime_pass = runtime_pass;
    }

    /// Interpret expression with runtime semantics
    pub fn interpret_expr_runtime(
        &self,
        expr: &Expr,
        ctx: &SharedScopedContext,
    ) -> Result<RuntimeValue> {
        match expr {
            Expr::Locator(Locator::Ident(ident)) => {
                // Try to get runtime value first
                if let Some(runtime_value) = ctx.get_runtime_value_recursive_path(ident) {
                    Ok(runtime_value)
                } else {
                    // Fall back to literal interpretation
                    let literal = self.interpret_ident(ident, ctx, true)?;
                    Ok(self.runtime_pass.create_runtime_value(literal))
                }
            }
            Expr::Select(select) => {
                let obj = self.interpret_expr_runtime(&select.obj.get(), ctx)?;
                self.runtime_pass
                    .access_field(obj, &select.field.name)
                    .map_err(|e| optimization_error(format!("Field access failed: {}", e)))
            }
            Expr::Invoke(invoke) => self.interpret_invoke_runtime(invoke, ctx),
            _ => {
                // For other expressions, interpret as literal then wrap
                let literal = self.interpret_expr(expr, ctx)?;
                Ok(self.runtime_pass.create_runtime_value(literal))
            }
        }
    }

    /// Runtime-aware method invocation
    pub fn interpret_invoke_runtime(
        &self,
        node: &ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<RuntimeValue> {
        match &node.target {
            ExprInvokeTarget::Method(select) => {
                // Method call with runtime semantics
                let obj = self.interpret_expr_runtime(&select.obj.get(), ctx)?;
                let args: Vec<RuntimeValue> = node
                    .args
                    .iter()
                    .map(|arg| self.interpret_expr_runtime(arg, ctx))
                    .try_collect()?;

                self.runtime_pass
                    .call_method(obj, &select.field.name, args)
                    .map_err(|e| optimization_error(format!("Method call failed: {}", e)))
            }
            _ => {
                // Fall back to regular interpretation for non-method calls
                let result = self.interpret_invoke(node, ctx)?;
                Ok(self.runtime_pass.create_runtime_value(result))
            }
        }
    }

    /// Runtime-aware assignment
    pub fn assign_runtime(
        &self,
        target: &Expr,
        value: RuntimeValue,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        self.runtime_pass
            .assign(target, value, ctx)
            .map_err(|e| optimization_error(format!("Assignment failed: {}", e)))
    }

    pub fn interpret_items(&self, node: &ItemChunk, ctx: &SharedScopedContext) -> Result<Value> {
        let result: Vec<_> = node
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(Value::unit()))
    }
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
    pub fn interpret_import(&self, _node: &ItemImport, _ctx: &SharedScopedContext) -> Result<()> {
        Ok(())
    }

    pub fn interpret_impl(&self, node: &ItemImpl, ctx: &SharedScopedContext) -> Result<()> {
        // Get the type name that this impl block is for
        let type_name = match &node.self_ty {
            Expr::Locator(Locator::Ident(ident)) => ident.clone(),
            _ => {
                #[allow(unreachable_code)]
                return opt_bail!("Only simple type names are supported in impl blocks for now");
            }
        };

        // For each function in the impl block, register it as a method for the type
        for item in &node.items {
            if let Item::DefFunction(func_def) = item {
                // Store the method in context with a special naming scheme: TypeName::method_name
                let method_key = Ident::new(&format!(
                    "{}::{}",
                    type_name.as_str(),
                    func_def.name.as_str()
                ));
                let func_value = func_def._to_value();
                ctx.insert_value(method_key, Value::Function(func_value));
            }
        }

        Ok(())
    }
    pub fn interpret_block(&self, node: &ExprBlock, ctx: &SharedScopedContext) -> Result<Value> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);

        // FIRST PASS: Process all items (const declarations, structs, functions)
        // Items can reference each other and need to be processed before statements
        for stmt in node.first_stmts().iter() {
            if let BlockStmt::Item(item) = stmt {
                self.interpret_item(item, &ctx)?;
            }
        }

        // SECOND PASS: Process all non-item statements (expressions, let statements, etc.)
        for stmt in node.first_stmts().iter() {
            if !matches!(stmt, BlockStmt::Item(_)) {
                self.interpret_stmt(&stmt, &ctx)?;
            }
        }

        // Process final expression if any
        if let Some(expr) = node.last_expr() {
            self.interpret_expr(&expr, &ctx)
        } else {
            Ok(Value::unit())
        }
    }

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
            "print" if resolve => Ok(Value::any(builtin_print(self.serializer.clone()))),
            "println!" if resolve => Ok(Value::any(builtin_println(self.serializer.clone()))),
            "println" if resolve => Ok(Value::any(builtin_println(self.serializer.clone()))),
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
    pub fn builtin_bitand(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "BitAnd expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::int(a.value & b.value)),
                _ => Err(optimization_error(format!(
                    "BitAnd operation not supported for types: {:?} & {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Logical AND operation
    pub fn builtin_logical_and(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("&&".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "LogicalAnd expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value && b.value)),
                _ => Err(optimization_error(format!(
                    "LogicalAnd operation not supported for types: {:?} && {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Logical OR operation
    pub fn builtin_logical_or(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("||".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "LogicalOr expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::bool(a.value || b.value)),
                _ => Err(optimization_error(format!(
                    "LogicalOr operation not supported for types: {:?} || {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Division operation
    pub fn builtin_div(&self) -> BuiltinFn {
        BuiltinFn::new_with_ident("/".into(), move |args, _ctx| {
            if args.len() != 2 {
                return Err(optimization_error(format!(
                    "Division expects 2 arguments, got: {}",
                    args.len()
                )));
            }

            match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => {
                    if b.value == 0 {
                        return Err(optimization_error("Division by zero".to_string()));
                    }
                    Ok(Value::int(a.value / b.value))
                }
                _ => Err(optimization_error(format!(
                    "Division operation not supported for types: {:?} / {:?}",
                    args[0], args[1]
                ))),
            }
        })
    }

    // Format string expression handler - evaluate structured template parts with args
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
        let mut evaluated_args = Vec::new();
        for (i, arg) in format_str.args.iter().enumerate() {
            tracing::debug!("Evaluating format arg {}: {:?}", i, arg);
            let value = self.interpret_expr(arg, ctx)?;
            let value_str = self.value_to_string(&value)?;
            tracing::debug!("Arg {} evaluated to: '{}'", i, value_str);
            evaluated_args.push(value_str);
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
                    let arg_value = match &placeholder.arg_ref {
                        fp_core::ast::FormatArgRef::Implicit => {
                            if implicit_arg_index < evaluated_args.len() {
                                let value = &evaluated_args[implicit_arg_index];
                                implicit_arg_index += 1;
                                value.clone()
                            } else {
                                tracing::warn!("Not enough arguments for implicit placeholder");
                                "{}".to_string() // Keep placeholder if no arg available
                            }
                        }
                        fp_core::ast::FormatArgRef::Positional(index) => {
                            if *index < evaluated_args.len() {
                                evaluated_args[*index].clone()
                            } else {
                                tracing::warn!("Positional argument {} out of range", index);
                                format!("{{{}}}", index) // Keep placeholder if no arg available
                            }
                        }
                        fp_core::ast::FormatArgRef::Named(name) => {
                            // TODO: Handle named arguments from kwargs
                            tracing::warn!("Named argument '{}' not yet supported", name);
                            format!("{{{}}}", name) // Keep placeholder for now
                        }
                    };

                    // TODO: Apply format specification if present
                    if let Some(_format_spec) = &placeholder.format_spec {
                        tracing::debug!(
                            "Format specification not yet implemented, using raw value"
                        );
                    }

                    tracing::debug!("Part {}: Substituting placeholder with '{}'", i, arg_value);
                    result.push_str(&arg_value);
                }
            }
        }

        tracing::debug!("Final structured format string result: '{}'", result);
        Ok(Value::String(fp_core::ast::ValueString::new_owned(result)))
    }

    // Helper method to convert Value to string representation
    fn value_to_string(&self, value: &Value) -> Result<String> {
        match value {
            Value::String(s) => Ok(s.value.clone()),
            Value::Int(i) => Ok(i.value.to_string()),
            Value::Decimal(d) => Ok(d.value.to_string()),
            Value::Bool(b) => Ok(b.value.to_string()),
            Value::Unit(_) => Ok("()".to_string()),
            _ => Ok(format!("{:?}", value)),
        }
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

    pub fn lookup_bin_op_kind(&self, op: BinOpKind) -> Result<BuiltinFn> {
        match op {
            BinOpKind::Add => Ok(builtin_add()),
            BinOpKind::AddTrait => {
                let this = self.clone();
                Ok(BuiltinFn::new(op, move |args, value| {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            let value = this.interpret_value(x, value, true)?;
                            match value {
                                Value::Type(Ty::ImplTraits(impls)) => Ok(impls.bounds),
                                _ => opt_bail!(format!("Expected impl Traits, got {:?}", value)),
                            }
                        })
                        .try_collect()?;
                    Ok(Ty::ImplTraits(ImplTraits {
                        bounds: TypeBounds {
                            bounds: args.into_iter().flat_map(|x| x.bounds).collect(),
                        },
                    })
                    .into())
                }))
            }
            BinOpKind::Sub => Ok(builtin_sub()),
            BinOpKind::Mul => Ok(builtin_mul()),
            BinOpKind::Div => Ok(self.builtin_div()),
            // BinOpKind::Mod => Ok(builtin_mod()),
            BinOpKind::Gt => Ok(builtin_gt()),
            BinOpKind::Lt => Ok(builtin_lt()),
            BinOpKind::Ge => Ok(builtin_ge()),
            BinOpKind::Le => Ok(builtin_le()),
            BinOpKind::Eq => Ok(builtin_eq()),
            BinOpKind::Ne => Ok(builtin_ne()),
            BinOpKind::BitAnd => Ok(self.builtin_bitand()),
            BinOpKind::Or => Ok(self.builtin_logical_or()),
            BinOpKind::And => Ok(self.builtin_logical_and()),
            // BinOpKind::BitOr => {}
            // BinOpKind::BitXor => {}
            // BinOpKind::Any(_) => {}
            _ => opt_bail!(format!("Could not process {:?}", op)),
        }
    }

    pub fn interpret_def_function(
        &self,
        def: &ItemDefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let name = &def.name;
        ctx.insert_value_with_ctx(name.clone(), Value::Function(def._to_value()));
        Ok(())
    }
    pub fn interpret_def_struct(
        &self,
        def: &ItemDefStruct,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Ty::Struct(def.value.clone()).into());
        Ok(())
    }
    pub fn interpret_def_enum(&self, def: &ItemDefEnum, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Ty::Enum(def.value.clone()).into());
        Ok(())
    }
    pub fn interpret_def_type(&self, def: &ItemDefType, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Value::Type(def.value.clone()));
        Ok(())
    }
    pub fn interpret_def_const(&self, def: &ItemDefConst, ctx: &SharedScopedContext) -> Result<()> {
        let value = self.interpret_expr(&def.value, ctx)?;
        tracing::debug!("Storing const {}: {:?}", def.name.name, value);
        ctx.insert_value_with_ctx(def.name.clone(), value);
        Ok(())
    }
    pub fn interpret_args(&self, node: &[Expr], ctx: &SharedScopedContext) -> Result<Vec<Value>> {
        // Evaluate each argument to a concrete value so builtins like println!
        // receive consts/literals rather than locals/expr wrappers in comptime mode.
        node.iter()
            .map(|x| self.interpret_expr(&x.get(), ctx))
            .try_collect()
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
    pub fn interpret_binop(&self, binop: &ExprBinOp, ctx: &SharedScopedContext) -> Result<Value> {
        let builtin_fn = self.lookup_bin_op_kind(binop.kind.clone())?;
        let lhs = self.interpret_expr(&binop.lhs.get(), ctx)?;
        let rhs = self.interpret_expr(&binop.rhs.get(), ctx)?;
        Ok(builtin_fn.invoke(&vec![lhs, rhs], ctx)?)
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
                        // Parse the argument inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();

                        // Simple parsing: remove whitespace and try to interpret as identifier
                        let arg_name = tokens_str.trim();

                        // Try to get the value from context
                        let ident = fp_core::id::Ident::new(arg_name);
                        if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                            match value {
                                Value::String(s) => return Ok(Value::int(s.value.len() as i64)),
                                _ => opt_bail!(format!(
                                    "strlen! expects string argument, got {:?}",
                                    value
                                )),
                            }
                        } else {
                            opt_bail!(format!("strlen! could not find variable: {}", arg_name));
                        }
                    }

                    // Handle concat! macro
                    if raw_macro.raw.mac.path.is_ident("concat") {
                        // Parse the arguments inside the macro
                        let tokens = &raw_macro.raw.mac.tokens;
                        let tokens_str = tokens.to_string();

                        // Simple parsing: split by comma and evaluate each argument
                        let mut result = String::new();

                        for arg in tokens_str.split(',') {
                            let arg = arg.trim();

                            // Try to interpret as string literal first
                            if arg.starts_with('"') && arg.ends_with('"') {
                                // String literal - remove quotes
                                let literal = &arg[1..arg.len() - 1];
                                result.push_str(literal);
                            } else {
                                // Try to get the value from context
                                let ident = fp_core::id::Ident::new(arg);
                                if let Some(value) = ctx.get_value(fp_core::id::Path::from(ident)) {
                                    match value {
                                        Value::String(s) => result.push_str(&s.value),
                                        Value::Int(i) => result.push_str(&i.value.to_string()),
                                        Value::Bool(b) => result.push_str(&b.value.to_string()),
                                        _ => opt_bail!(format!(
                                            "concat! cannot convert {:?} to string",
                                            value
                                        )),
                                    }
                                } else {
                                    opt_bail!(format!("concat! could not find variable: {}", arg));
                                }
                            }
                        }

                        return Ok(Value::string(result));
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
    pub fn interpret_item(&self, node: &Item, ctx: &SharedScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_items(&n.items, ctx),
            Item::DefFunction(n) => self.interpret_def_function(n, ctx).map(|_| Value::unit()),
            Item::DefStruct(n) => self.interpret_def_struct(n, ctx).map(|_| Value::unit()),
            Item::DefEnum(n) => self.interpret_def_enum(n, ctx).map(|_| Value::unit()),
            Item::DefType(n) => self.interpret_def_type(n, ctx).map(|_| Value::unit()),
            Item::DefConst(n) => self.interpret_def_const(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),
            Item::Impl(n) => self.interpret_impl(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => opt_bail!(format!("Failed to interpret {:?}", node)),
        }
    }

    pub fn interpret_let(&self, node: &StmtLet, ctx: &SharedScopedContext) -> Result<Value> {
        if let Some(init) = &node.init {
            let value = self.interpret_expr(&init, ctx)?;
            ctx.insert_value(
                node.pat
                    .as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
                value.clone(),
            );
            Ok(value)
        } else {
            ctx.insert_value(
                node.pat
                    .as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
                Value::undefined(),
            );
            Ok(Value::unit())
        }
    }

    pub fn interpret_stmt(
        &self,
        node: &BlockStmt,
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        // eprintln!("DEBUG: interpret_stmt called with: {:?}", node);
        match node {
            BlockStmt::Let(n) => self.interpret_let(n, ctx).map(|_| None),
            BlockStmt::Expr(n) => {
                self.interpret_expr(&n.expr, ctx).map(
                    |x| {
                        if n.has_value() {
                            Some(x)
                        } else {
                            None
                        }
                    },
                )
            }
            BlockStmt::Item(item) => self.interpret_item(item, ctx).map(|_| None),
            BlockStmt::Any(any_box) => {
                // Handle macro statements
                if any_box.downcast_ref::<fp_rust::RawStmtMacro>().is_some() {
                    // Macros like println! are now converted to function calls at parse time
                    // For any remaining macros, just return unit
                    Ok(None)
                } else {
                    opt_bail!(format!("Unsupported Any statement type: {:?}", any_box))
                }
            }
            BlockStmt::Noop => Ok(None),
        }
    }

    pub fn interpret_tree(&self, node: &Node, ctx: &SharedScopedContext) -> Result<Value> {
        match node {
            Node::Item(item) => self.interpret_item(item, ctx),
            Node::Expr(expr) => self.interpret_expr(expr, ctx),
            Node::File(file) => self.interpret_items(&file.items, ctx),
        }
    }

    /// Evaluate a const expression with const-eval aware intrinsics
    /// This is the main method for const evaluation
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

impl OptimizePass for InterpretationOrchestrator {
    fn name(&self) -> &str {
        "interpretation"
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        Ok(Expr::value(value))
    }

    fn optimize_item(&self, _item: Item, _ctx: &SharedScopedContext) -> Result<Item> {
        Ok(Item::unit())
    }

    fn evaluate_condition(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        let value = self.interpret_expr_no_resolve(&expr, ctx)?;
        match value {
            Value::Bool(b) => {
                if b.value {
                    Ok(ControlFlow::IntoAndBreak(None))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }
            _ => opt_bail!(format!("Failed to interpret {:?} => {:?}", expr, value)),
        }
    }
    fn evaluate_invoke(
        &self,
        _invoke: ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => self.interpret_expr(&func.body.get(), ctx).map(Expr::value),
            Value::BinOpKind(kind) => self
                .interpret_invoke_binop(kind.clone(), &invoke.args, ctx)
                .map(Expr::value),
            Value::UnOpKind(func) => {
                opt_ensure!(
                    invoke.args.len() == 1,
                    format!("Expected 1 arg for {:?}", func)
                );
                let arg = self.interpret_expr(&invoke.args[0].get(), ctx)?;
                self.interpret_invoke_unop(func.clone(), arg, ctx)
                    .map(Expr::value)
            }
            _ => opt_bail!(format!("Could not invoke {:?}", func)),
        }
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        // First try the simple approach for basic expressions
        if let Some(value) = ctx.try_get_value_from_expr(pat) {
            return Ok(Expr::value(value));
        }

        // If simple approach fails, use full interpretation for complex expressions
        let value = self.interpret_expr(pat, ctx)?;
        Ok(Expr::value(value))
    }
}

impl ValueSystem for InterpretationOrchestrator {
    fn get_value_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<Value> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));
        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            Expr::Value(value) => Ok(*value),
            _ => opt_bail!(format!("Expected value, got {:?}", expr)),
        }
    }
}
