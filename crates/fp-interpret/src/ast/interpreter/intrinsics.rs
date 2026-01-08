use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    // Runtime intrinsic evaluation that propagates control-flow.
    pub(super) fn eval_intrinsic_runtime(&mut self, call: &mut ExprIntrinsicCall) -> RuntimeFlow {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                match self.render_intrinsic_call_runtime(call) {
                    Ok(output) => {
                        if call.kind == IntrinsicCallKind::Print {
                            if let Some(last) = self.stdout.last_mut() {
                                last.push_str(&output);
                            } else {
                                self.stdout.push(output);
                            }
                        } else {
                            self.stdout.push(output);
                        }
                    }
                    Err(err) => self.emit_error(err),
                }
                RuntimeFlow::Value(Value::unit())
            }
            IntrinsicCallKind::Format => {
                match self.render_intrinsic_call_runtime(call) {
                    Ok(output) => RuntimeFlow::Value(Value::string(output)),
                    Err(err) => {
                        self.emit_error(err);
                        RuntimeFlow::Value(Value::string(String::new()))
                    }
                }
            }
            IntrinsicCallKind::DebugAssertions => RuntimeFlow::Value(Value::bool(self.debug_assertions)),
            IntrinsicCallKind::Panic => {
                let message = self.intrinsic_panic_message(call);
                self.stdout.push(format!("panic: {}", message));
                RuntimeFlow::Panic(Value::string(message))
            }
            IntrinsicCallKind::CatchUnwind => {
                let args = match &mut call.payload {
                    IntrinsicCallPayload::Args { args } => args,
                    IntrinsicCallPayload::Format { .. } => {
                        self.emit_error("catch_unwind does not accept formatted payloads");
                        return RuntimeFlow::Value(Value::bool(false));
                    }
                };
                if args.len() != 1 {
                    self.emit_error("catch_unwind expects exactly one callable argument");
                    return RuntimeFlow::Value(Value::bool(false));
                }
                let callable = self.eval_expr_runtime(&mut args[0]);
                let value = match callable {
                    RuntimeFlow::Value(value) => value,
                    RuntimeFlow::Panic(_) => return RuntimeFlow::Value(Value::bool(false)),
                    other => return other,
                };
                let flow = self.invoke_runtime_callable(value, Vec::new());
                match flow {
                    RuntimeFlow::Panic(_) => RuntimeFlow::Value(Value::bool(false)),
                    other => RuntimeFlow::Value(Value::bool(matches!(other, RuntimeFlow::Value(_)))),
                }
            }
            _ => RuntimeFlow::Value(self.eval_intrinsic(call)),
        }
    }

    pub(super) fn should_replace_intrinsic_with_value(
        &self,
        kind: IntrinsicCallKind,
        value: &Value,
    ) -> bool {
        if matches!(value, Value::Undefined(_)) {
            return false;
        }

        matches!(
            kind,
            IntrinsicCallKind::SizeOf
                | IntrinsicCallKind::FieldCount
                | IntrinsicCallKind::FieldType
                | IntrinsicCallKind::StructSize
                | IntrinsicCallKind::TypeName
                | IntrinsicCallKind::HasField
                | IntrinsicCallKind::HasMethod
                | IntrinsicCallKind::MethodCount
                | IntrinsicCallKind::ReflectFields
        )
    }

    pub(super) fn eval_intrinsic(&mut self, call: &mut ExprIntrinsicCall) -> Value {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                match self.render_intrinsic_call(call) {
                    Ok(output) => {
                        if call.kind == IntrinsicCallKind::Print {
                            if let Some(last) = self.stdout.last_mut() {
                                last.push_str(&output);
                            } else {
                                self.stdout.push(output);
                            }
                        } else {
                            self.stdout.push(output);
                        }
                    }
                    Err(err) => self.emit_error(err),
                }
                Value::unit()
            }
            IntrinsicCallKind::Format => match self.render_intrinsic_call(call) {
                Ok(output) => Value::string(output),
                Err(err) => {
                    self.emit_error(err);
                    Value::string(String::new())
                }
            },
            IntrinsicCallKind::DebugAssertions => Value::bool(self.debug_assertions),
            IntrinsicCallKind::Panic | IntrinsicCallKind::CatchUnwind => {
                self.emit_error(format!(
                    "intrinsic {:?} is not supported during const evaluation",
                    call.kind
                ));
                Value::undefined()
            }
            _ => {
                let intrinsic_name = match super::intrinsic_symbol(call.kind) {
                    Some(name) => name,
                    None => {
                        self.emit_error(format!(
                            "intrinsic {:?} is not supported during const evaluation",
                            call.kind
                        ));
                        return Value::undefined();
                    }
                };

                let args = match &mut call.payload {
                    fp_core::intrinsics::IntrinsicCallPayload::Args { args } => args,
                    fp_core::intrinsics::IntrinsicCallPayload::Format { .. } => {
                        self.emit_error(
                            "format-style intrinsics are not supported in const evaluation",
                        );
                        return Value::undefined();
                    }
                };

                let evaluated: Vec<_> = args.iter_mut().map(|expr| self.eval_expr(expr)).collect();
                if matches!(call.kind, IntrinsicCallKind::CompileWarning) {
                    if let Some(Value::String(message)) = evaluated.first() {
                        self.emit_warning(message.value.clone());
                    }
                }
                match self.intrinsics.get(intrinsic_name) {
                    Some(function) => match function.call(&evaluated, self.ctx) {
                        Ok(value) => value,
                        Err(err) => {
                            self.emit_error(err.to_string());
                            Value::undefined()
                        }
                    },
                    None => {
                        self.emit_error(format!(
                            "intrinsic '{}' is not registered for const evaluation",
                            intrinsic_name
                        ));
                        Value::undefined()
                    }
                }
            }
        }
    }

    pub(super) fn evaluate_intrinsic_for_function_analysis(
        &mut self,
        call: &mut ExprIntrinsicCall,
    ) {
        if self.should_replace_intrinsic_with_value(call.kind, &Value::unit()) {
            let value = self.eval_intrinsic(call);
            if !matches!(value, Value::Undefined(_)) {
                return;
            }
        }

        match &mut call.payload {
            IntrinsicCallPayload::Format { template } => {
                for arg in template.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.evaluate_function_body(&mut kwarg.value);
                }
            }
            IntrinsicCallPayload::Args { args } => {
                for arg in args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
            }
        }
    }

    fn intrinsic_panic_message(&mut self, call: &mut ExprIntrinsicCall) -> String {
        match &mut call.payload {
            IntrinsicCallPayload::Args { args } => {
                if args.is_empty() {
                    return "panic! macro triggered".to_string();
                }
                if args.len() > 1 {
                    self.emit_error("panic expects zero or one argument");
                }
                let flow = self.eval_expr_runtime(&mut args[0]);
                let value = self.finish_runtime_flow(flow);
                format!("{}", value)
            }
            IntrinsicCallPayload::Format { .. } => {
                self.emit_error("panic does not accept formatted payloads");
                "panic! macro triggered".to_string()
            }
        }
    }

    fn invoke_runtime_callable(&mut self, value: Value, args: Vec<Value>) -> RuntimeFlow {
        match value {
            Value::Function(function) => self.call_value_function_runtime(&function, args),
            Value::Any(any) => {
                if let Some(closure) = any.downcast_ref::<ConstClosure>() {
                    return self.call_const_closure_runtime(closure, args);
                }
                self.emit_error("catch_unwind expects a callable value");
                RuntimeFlow::Value(Value::undefined())
            }
            _ => {
                self.emit_error("catch_unwind expects a callable value");
                RuntimeFlow::Value(Value::undefined())
            }
        }
    }

    pub(super) fn render_intrinsic_call(
        &mut self,
        call: &mut ExprIntrinsicCall,
    ) -> std::result::Result<String, String> {
        match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                self.render_format_template(template)
            }
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                let mut rendered = Vec::with_capacity(args.len());
                for expr in args.iter_mut() {
                    let value = self.eval_expr(expr);
                    let text =
                        format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
                    rendered.push(text);
                }
                Ok(rendered.join(" "))
            }
        }
    }

    fn render_intrinsic_call_runtime(
        &mut self,
        call: &mut ExprIntrinsicCall,
    ) -> std::result::Result<String, String> {
        match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                self.render_format_template_runtime(template)
            }
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                let mut rendered = Vec::with_capacity(args.len());
                for expr in args.iter_mut() {
                    let flow = self.eval_expr_runtime(expr);
                    let value = self.finish_runtime_flow(flow);
                    let text =
                        format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
                    rendered.push(text);
                }
                Ok(rendered.join(" "))
            }
        }
    }

    pub(super) fn render_format_template(
        &mut self,
        template: &mut ExprFormatString,
    ) -> std::result::Result<String, String> {
        let positional: Vec<Value> = template
            .args
            .iter_mut()
            .map(|expr| self.eval_expr(expr))
            .collect();

        for (expr, value) in template.args.iter_mut().zip(positional.iter()) {
            if let Some(kind) = match expr.kind() {
                ExprKind::IntrinsicCall(call) => Some(call.kind),
                _ => None,
            } {
                if self.should_replace_intrinsic_with_value(kind, value) {
                    let mut replacement = Expr::value(value.clone());
                    replacement.ty = expr.ty.clone();
                    *expr = replacement;
                    self.mark_mutated();
                }
            }
        }

        let mut named = HashMap::new();
        for kwarg in template.kwargs.iter_mut() {
            let value = self.eval_expr(&mut kwarg.value);
            named.insert(kwarg.name.clone(), value);
        }

        let mut output = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(literal) => output.push_str(literal),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let index = implicit_index;
                            implicit_index += 1;
                            positional.get(index)
                        }
                        FormatArgRef::Positional(index) => positional.get(*index),
                        FormatArgRef::Named(name) => named.get(name),
                    }
                    .ok_or_else(|| match &placeholder.arg_ref {
                        FormatArgRef::Implicit => format!(
                            "implicit format placeholder {{}} has no argument at position {}",
                            implicit_index.saturating_sub(1)
                        ),
                        FormatArgRef::Positional(index) => format!(
                            "format placeholder {{{}}} references missing positional argument",
                            index
                        ),
                        FormatArgRef::Named(name) => format!(
                            "format placeholder {{{name}}} references undefined keyword argument"
                        ),
                    })?;

                    let formatted =
                        format_value_with_spec(value, placeholder.format_spec.as_deref())
                            .map_err(|err| err.to_string())?;
                    output.push_str(&formatted);
                }
            }
        }

        Ok(output)
    }

    pub(super) fn render_format_template_runtime(
        &mut self,
        template: &mut ExprFormatString,
    ) -> std::result::Result<String, String> {
        let positional: Vec<Value> = template
            .args
            .iter_mut()
            .map(|expr| {
                let flow = self.eval_expr_runtime(expr);
                self.finish_runtime_flow(flow)
            })
            .collect();

        let mut named = HashMap::new();
        for kwarg in template.kwargs.iter_mut() {
            let flow = self.eval_expr_runtime(&mut kwarg.value);
            let value = self.finish_runtime_flow(flow);
            named.insert(kwarg.name.clone(), value);
        }

        let mut output = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(literal) => output.push_str(literal),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let index = implicit_index;
                            implicit_index += 1;
                            positional.get(index)
                        }
                        FormatArgRef::Positional(index) => positional.get(*index),
                        FormatArgRef::Named(name) => named.get(name),
                    };
                    if let Some(value) = value {
                        let rendered = format_value_with_spec(
                            value,
                            placeholder.format_spec.as_deref(),
                        )
                        .map_err(|err| err.to_string())?;
                        output.push_str(&rendered);
                    } else {
                        return Err("format placeholder out of range".to_string());
                    }
                }
            }
        }

        Ok(output)
    }
}
