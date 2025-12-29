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
            IntrinsicCallKind::DebugAssertions => RuntimeFlow::Value(Value::bool(self.debug_assertions)),
            IntrinsicCallKind::Break => {
                if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                    if args.len() > 1 {
                        self.emit_error("`break` accepts at most one value");
                    }
                    let value = if let Some(expr) = args.first_mut() {
                        let flow = self.eval_expr_runtime(expr);
                        Some(self.finish_runtime_flow(flow))
                    } else {
                        None
                    };
                    return RuntimeFlow::Break(value);
                }
                RuntimeFlow::Break(None)
            }
            IntrinsicCallKind::Continue => {
                if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                    if !args.is_empty() {
                        self.emit_error("`continue` does not accept a value");
                    }
                }
                RuntimeFlow::Continue
            }
            IntrinsicCallKind::Return => {
                if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                    if args.len() > 1 {
                        self.emit_error("`return` accepts at most one value");
                    }
                    let value = if let Some(expr) = args.first_mut() {
                        let flow = self.eval_expr_runtime(expr);
                        Some(self.finish_runtime_flow(flow))
                    } else {
                        None
                    };
                    return RuntimeFlow::Return(value);
                }
                RuntimeFlow::Return(None)
            }
            IntrinsicCallKind::ConstBlock => {
                if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                    if let Some(expr) = args.first_mut() {
                        self.enter_const_region();
                        let flow = self.eval_expr_runtime(expr);
                        self.exit_const_region();
                        return flow;
                    }
                }
                self.emit_error("const block requires an argument");
                RuntimeFlow::Value(Value::undefined())
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
                | IntrinsicCallKind::ConstBlock
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
            IntrinsicCallKind::DebugAssertions => Value::bool(self.debug_assertions),
            IntrinsicCallKind::Break => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if args.len() > 1 {
                        self.emit_error("`break` accepts at most one value in const evaluation");
                    }
                    if let Some(expr) = args.first_mut() {
                        return self.eval_expr(expr);
                    }
                }
                Value::unit()
            }
            IntrinsicCallKind::Continue => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if !args.is_empty() {
                        self.emit_error("`continue` does not accept a value in const evaluation");
                    }
                }
                Value::unit()
            }
            IntrinsicCallKind::ConstBlock => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if let Some(expr) = args.first_mut() {
                        self.enter_const_region();
                        let value = self.eval_expr(expr);
                        self.exit_const_region();
                        return value;
                    }
                }
                self.emit_error("const block requires an argument");
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
