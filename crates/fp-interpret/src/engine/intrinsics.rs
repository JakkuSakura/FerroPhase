use super::*;
use fp_core::ast::ExprKwArg;
use proc_macro2::TokenStream as ProcMacroTokenStream;
use std::str::FromStr;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

impl<'ctx> AstInterpreter<'ctx> {
    fn interpolate_literal_template(
        &mut self,
        literal: &str,
        named: &HashMap<String, Value>,
    ) -> std::result::Result<String, String> {
        if !literal.contains('{') {
            return Ok(literal.to_string());
        }
        let mut out = String::new();
        let mut chars = literal.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch != '{' {
                out.push(ch);
                continue;
            }
            let mut ident = String::new();
            let mut closed = false;
            while let Some(next) = chars.peek().copied() {
                if next == '}' {
                    chars.next();
                    closed = true;
                    break;
                }
                if ident.is_empty() {
                    if next.is_ascii_alphabetic() || next == '_' {
                        ident.push(next);
                        chars.next();
                        continue;
                    }
                    break;
                }
                if next.is_ascii_alphanumeric() || next == '_' {
                    ident.push(next);
                    chars.next();
                    continue;
                }
                break;
            }
            if ident.is_empty() || !closed {
                out.push('{');
                out.push_str(&ident);
                if closed {
                    out.push('}');
                }
                continue;
            }
            if let Some(value) = named
                .get(&ident)
                .cloned()
                .or_else(|| self.lookup_value(&ident))
            {
                let rendered =
                    format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
                out.push_str(&rendered);
            } else {
                out.push('{');
                out.push_str(&ident);
                out.push('}');
            }
        }
        Ok(out)
    }
    // Runtime intrinsic evaluation that propagates control-flow.
    pub(super) fn eval_intrinsic_runtime(&mut self, call: &mut ExprIntrinsicCall) -> RuntimeFlow {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                match self.render_intrinsic_call_runtime(call) {
                    Ok(output) => {
                        if call.kind == IntrinsicCallKind::Print {
                            self.emit_stdout_fragment(output);
                        } else {
                            self.emit_stdout_line(output);
                        }
                    }
                    Err(err) => self.emit_error(err),
                }
                RuntimeFlow::Value(Value::unit())
            }
            IntrinsicCallKind::Format => match self.render_intrinsic_call_runtime(call) {
                Ok(output) => RuntimeFlow::Value(Value::string(output)),
                Err(err) => {
                    self.emit_error(err);
                    RuntimeFlow::Value(Value::string(String::new()))
                }
            },
            IntrinsicCallKind::DebugAssertions => {
                RuntimeFlow::Value(Value::bool(self.debug_assertions))
            }
            IntrinsicCallKind::Panic => {
                let message = self.intrinsic_panic_message(call);
                self.emit_stdout_line(format!("panic: {}", message));
                RuntimeFlow::Panic(Value::string(message))
            }
            IntrinsicCallKind::CatchUnwind => {
                if !call.kwargs.is_empty() {
                    self.emit_error("catch_unwind does not accept named arguments");
                    return RuntimeFlow::Value(Value::bool(false));
                }
                let args = &mut call.args;
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
                    other => {
                        RuntimeFlow::Value(Value::bool(matches!(other, RuntimeFlow::Value(_))))
                    }
                }
            }
            IntrinsicCallKind::TimeNow => {
                if !call.kwargs.is_empty() {
                    self.emit_error("time::now intrinsic does not accept named arguments");
                }
                if !call.args.is_empty() {
                    self.emit_error("time::now intrinsic expects no arguments");
                }
                match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(duration) => RuntimeFlow::Value(Value::decimal(duration.as_secs_f64())),
                    Err(_) => {
                        self.emit_error("system clock is before UNIX_EPOCH");
                        RuntimeFlow::Value(Value::decimal(0.0))
                    }
                }
            }
            IntrinsicCallKind::Sleep => {
                if !call.kwargs.is_empty() {
                    self.emit_error("time::sleep intrinsic does not accept named arguments");
                }
                if call.args.len() != 1 {
                    self.emit_error("time::sleep intrinsic expects one argument");
                    return RuntimeFlow::Value(Value::unit());
                }
                let flow = self.eval_expr_runtime(&mut call.args[0]);
                let value = match flow {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                let seconds = match value {
                    Value::Decimal(decimal) => decimal.value,
                    Value::Int(int) => int.value as f64,
                    other => {
                        self.emit_error(format!(
                            "time::sleep expects a numeric duration, got {other}"
                        ));
                        return RuntimeFlow::Value(Value::unit());
                    }
                };
                if !seconds.is_finite() || seconds < 0.0 {
                    self.emit_error("time::sleep expects a non-negative, finite duration");
                    return RuntimeFlow::Value(Value::unit());
                }
                self.mark_task_sleep(Duration::from_secs_f64(seconds));
                RuntimeFlow::Value(Value::unit())
            }
            IntrinsicCallKind::Spawn => {
                if !call.kwargs.is_empty() {
                    self.emit_error("task::spawn intrinsic does not accept named arguments");
                }
                if call.args.len() != 1 {
                    self.emit_error("task::spawn intrinsic expects one argument");
                    return RuntimeFlow::Value(Value::undefined());
                }
                let flow = self.eval_expr_runtime(&mut call.args[0]);
                let value = match flow {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                if let Some(future) = self.extract_runtime_future(&value) {
                    let handle = self.spawn_runtime_future(future);
                    return RuntimeFlow::Value(Value::any(handle));
                }
                self.emit_error("task::spawn expects a Future value");
                RuntimeFlow::Value(Value::undefined())
            }
            IntrinsicCallKind::Join => {
                if !call.kwargs.is_empty() {
                    self.emit_error("task::join intrinsic does not accept named arguments");
                }
                if call.args.is_empty() {
                    self.emit_error("task::join intrinsic expects at least one argument");
                    return RuntimeFlow::Value(Value::undefined());
                }
                let mut scheduler_handles = Vec::with_capacity(call.args.len());
                for arg in call.args.iter_mut() {
                    let flow = self.eval_expr_runtime(arg);
                    let value = match flow {
                        RuntimeFlow::Value(value) => value,
                        other => return other,
                    };
                    if let Some(handle) = self.extract_task_handle(&value) {
                        scheduler_handles.push(handle.id);
                        continue;
                    }
                    if let Some(future) = self.extract_runtime_future(&value) {
                        let handle = self.spawn_runtime_future(future);
                        scheduler_handles.push(handle.id);
                        continue;
                    }
                    self.emit_error("task::join expects Task or Future arguments");
                    return RuntimeFlow::Value(Value::undefined());
                }

                loop {
                    let mut all_ready = true;
                    let mut values = Vec::with_capacity(scheduler_handles.len());
                    for id in &scheduler_handles {
                        match self.task_result(*id) {
                            Some(RuntimeFlow::Value(value)) => values.push(value),
                            Some(RuntimeFlow::Panic(value)) => return RuntimeFlow::Panic(value),
                            _ => {
                                all_ready = false;
                                break;
                            }
                        }
                    }
                    if all_ready {
                        if values.len() == 1 {
                            return RuntimeFlow::Value(values.remove(0));
                        }
                        return RuntimeFlow::Value(Value::Tuple(ValueTuple::new(values)));
                    }
                    if !self.tick_scheduler() {
                        self.emit_error("no runnable tasks available during join");
                        return RuntimeFlow::Value(Value::undefined());
                    }
                }
            }
            IntrinsicCallKind::Select => {
                if !call.kwargs.is_empty() {
                    self.emit_error("task::select intrinsic does not accept named arguments");
                }
                if call.args.len() < 2 {
                    self.emit_error("task::select intrinsic expects at least two arguments");
                    return RuntimeFlow::Value(Value::undefined());
                }
                let mut scheduler_handles = Vec::with_capacity(call.args.len());
                for arg in call.args.iter_mut() {
                    let flow = self.eval_expr_runtime(arg);
                    let value = match flow {
                        RuntimeFlow::Value(value) => value,
                        other => return other,
                    };
                    if let Some(handle) = self.extract_task_handle(&value) {
                        scheduler_handles.push(handle.id);
                        continue;
                    }
                    if let Some(future) = self.extract_runtime_future(&value) {
                        let handle = self.spawn_runtime_future(future);
                        scheduler_handles.push(handle.id);
                        continue;
                    }
                    self.emit_error("task::select expects Task or Future arguments");
                    return RuntimeFlow::Value(Value::undefined());
                }
                match self.run_scheduler_until_any(&scheduler_handles) {
                    Some((idx, RuntimeFlow::Value(value))) => {
                        RuntimeFlow::Value(Value::Tuple(ValueTuple::new(vec![
                            Value::int(idx as i64),
                            value,
                        ])))
                    }
                    Some((_idx, flow)) => flow,
                    None => {
                        self.emit_error("no runnable tasks available during select");
                        RuntimeFlow::Value(Value::undefined())
                    }
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
                | IntrinsicCallKind::VecType
                | IntrinsicCallKind::StructSize
                | IntrinsicCallKind::TypeName
                | IntrinsicCallKind::TypeOf
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
                            self.emit_stdout_fragment(output);
                        } else {
                            self.emit_stdout_line(output);
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
            IntrinsicCallKind::TypeOf => {
                if call.args.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        call.args.len()
                    ));
                    return Value::undefined();
                }
                let target = &mut call.args[0];
                if let Some(ty) = target.ty() {
                    return Value::Type(ty.clone());
                }
                let value = self.eval_expr(target);
                Value::Type(self.type_from_value(&value))
            }
            IntrinsicCallKind::VecType => {
                if call.args.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        call.args.len()
                    ));
                    return Value::undefined();
                }
                let value = self.eval_intrinsic_type_arg(&mut call.args[0]);
                let Value::Type(ty) = value else {
                    self.emit_error("vec_type! expects a type argument");
                    return Value::undefined();
                };
                Value::Type(Ty::Vec(TypeVec {
                    ty: Box::new(self.materialize_type(ty)),
                }))
            }
            IntrinsicCallKind::ProcMacroTokenStreamFromStr => {
                if call.args.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        call.args.len()
                    ));
                    return Value::undefined();
                }
                let input = self.eval_expr(&mut call.args[0]);
                let Value::String(text) = input else {
                    self.emit_error("proc macro token_stream_from_str expects a string argument");
                    return Value::undefined();
                };
                match ProcMacroTokenStream::from_str(text.value.as_str()) {
                    Ok(stream) => Value::TokenStream(ValueTokenStream {
                        tokens: super::macro_token_trees_from_proc_macro_stream(stream),
                    }),
                    Err(err) => {
                        self.emit_error(err.to_string());
                        Value::undefined()
                    }
                }
            }
            IntrinsicCallKind::ProcMacroTokenStreamToString => {
                if call.args.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        call.args.len()
                    ));
                    return Value::undefined();
                }
                let input = self.eval_expr(&mut call.args[0]);
                let Value::TokenStream(stream) = input else {
                    self.emit_error(
                        "proc macro token_stream_to_string expects a TokenStream argument",
                    );
                    return Value::undefined();
                };
                Value::string(token_stream_to_string(&stream.tokens))
            }
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

                let mut evaluated = Vec::with_capacity(call.args.len() + call.kwargs.len());
                for (idx, arg) in call.args.iter_mut().enumerate() {
                    let mode = self.intrinsic_type_arg_mode(call.kind, idx);
                    let value = match mode {
                        TypeArgMode::Required => self.eval_intrinsic_type_arg(arg),
                        TypeArgMode::Fallback => {
                            let value = self.eval_expr(arg);
                            if matches!(value, Value::Undefined(_)) {
                                self.eval_intrinsic_type_arg(arg)
                            } else {
                                self.materialize_type_value(value)
                            }
                        }
                        TypeArgMode::None => self.eval_expr(arg),
                    };
                    evaluated.push(value);
                }
                for kwarg in call.kwargs.iter_mut() {
                    evaluated.push(self.eval_expr(&mut kwarg.value));
                }
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

    fn eval_intrinsic_type_arg(&mut self, expr: &mut Expr) -> Value {
        if let Some(value) = self.eval_type_value_from_expr(expr) {
            return value;
        }
        self.eval_expr(expr)
    }

    fn eval_type_value_from_expr(&mut self, expr: &mut Expr) -> Option<Value> {
        match expr.kind_mut() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Type(ty) => Some(Value::Type(self.materialize_type(ty.clone()))),
                _ => None,
            },
            ExprKind::BinOp(binop) => {
                let lhs = self.eval_type_value_from_expr(binop.lhs.as_mut());
                let rhs = self.eval_type_value_from_expr(binop.rhs.as_mut());
                match (lhs, rhs) {
                    (Some(Value::Type(lhs_ty)), Some(Value::Type(rhs_ty))) => {
                        let kind = match binop.kind {
                            BinOpKind::Add | BinOpKind::AddTrait => TypeBinaryOpKind::Add,
                            BinOpKind::Sub => TypeBinaryOpKind::Subtract,
                            BinOpKind::And | BinOpKind::BitAnd => TypeBinaryOpKind::Intersect,
                            BinOpKind::Or | BinOpKind::BitOr => TypeBinaryOpKind::Union,
                            _ => return None,
                        };
                        Some(Value::Type(Ty::TypeBinaryOp(
                            fp_core::ast::TypeBinaryOp {
                                kind,
                                lhs: Box::new(lhs_ty),
                                rhs: Box::new(rhs_ty),
                            }
                            .into(),
                        )))
                    }
                    _ => None,
                }
            }
            ExprKind::Reference(reference) => {
                if let ExprKind::Name(locator) = reference.referee.kind() {
                    let key = Self::locator_base_name(locator);
                    if let Some((lifetime, base_name)) =
                        Self::split_static_lifetime_name(key.as_str())
                    {
                        if let Some(path) = self.parse_symbol_path(base_name) {
                            if let Some(base_ty) = self.resolve_type_binding(&path) {
                                let ty = Ty::Reference(TypeReference {
                                    ty: base_ty.into(),
                                    mutability: reference.mutable,
                                    lifetime: Some(Ident::new(lifetime)),
                                });
                                return Some(Value::Type(ty));
                            }
                        }
                    }
                }
                let mut base_expr = reference.referee.as_mut().clone();
                if let Some(Value::Type(base_ty)) = self.eval_type_value_from_expr(&mut base_expr) {
                    let ty = Ty::Reference(TypeReference {
                        ty: base_ty.into(),
                        mutability: reference.mutable,
                        lifetime: None,
                    });
                    Some(Value::Type(ty))
                } else {
                    None
                }
            }
            ExprKind::Name(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        if matches!(value, Value::Type(_)) {
                            return Some(self.materialize_type_value(value));
                        }
                    }
                    if let Some(path) = self.parse_symbol_path(ident.as_str()) {
                        if let Some(ty) = self.resolve_type_binding(&path) {
                            return Some(Value::Type(ty));
                        }
                    }
                }
                let value = self.resolve_qualified(locator.to_string());
                if matches!(value, Value::Type(_)) {
                    Some(self.materialize_type_value(value))
                } else {
                    None
                }
            }
            ExprKind::ConstBlock(_) | ExprKind::Macro(_) => {
                let value = self.eval_expr(expr);
                if matches!(value, Value::Type(_)) {
                    Some(self.materialize_type_value(value))
                } else {
                    None
                }
            }
            _ => {
                let value = self.eval_expr(expr);
                if matches!(value, Value::Type(_)) {
                    Some(self.materialize_type_value(value))
                } else {
                    None
                }
            }
        }
    }

    fn materialize_type_value(&mut self, value: Value) -> Value {
        match value {
            Value::Type(ty) => Value::Type(self.materialize_type(ty)),
            other => other,
        }
    }

    pub(super) fn materialize_type(&mut self, mut ty: Ty) -> Ty {
        self.evaluate_ty(&mut ty);
        struct InterpreterTypeHooks<'a, 'ctx> {
            interpreter: &'a mut AstInterpreter<'ctx>,
        }

        impl fp_typing::TypeMaterializeHooks for InterpreterTypeHooks<'_, '_> {
            fn resolve_name(&mut self, locator: &Name) -> Option<Ty> {
                let path = AstInterpreter::locator_path(locator)?;
                self.interpreter.resolve_type_binding(&path)
            }

            fn eval_const_expr(&mut self, expr: &mut Expr) -> Option<Ty> {
                match self.interpreter.eval_expr(expr) {
                    Value::Type(resolved) => Some(resolved),
                    _ => None,
                }
            }
        }

        let mut hooks = InterpreterTypeHooks { interpreter: self };
        fp_typing::materialize_type_with_hooks(ty, &mut hooks)
    }

    fn split_static_lifetime_name(name: &str) -> Option<(&'static str, &str)> {
        let remainder = name.strip_prefix("'static")?;
        if remainder.is_empty() {
            None
        } else {
            Some(("static", remainder))
        }
    }

    fn intrinsic_type_arg_mode(&self, kind: IntrinsicCallKind, idx: usize) -> TypeArgMode {
        match kind {
            IntrinsicCallKind::AddField => {
                if idx == 0 || idx == 2 {
                    TypeArgMode::Required
                } else {
                    TypeArgMode::None
                }
            }
            IntrinsicCallKind::SizeOf
            | IntrinsicCallKind::ReflectFields
            | IntrinsicCallKind::HasMethod
            | IntrinsicCallKind::TypeName
            | IntrinsicCallKind::FieldType
            | IntrinsicCallKind::VecType
            | IntrinsicCallKind::FieldNameAt => {
                if idx == 0 {
                    TypeArgMode::Required
                } else {
                    TypeArgMode::None
                }
            }
            IntrinsicCallKind::HasField
            | IntrinsicCallKind::FieldCount
            | IntrinsicCallKind::MethodCount
            | IntrinsicCallKind::StructSize => {
                if idx == 0 {
                    TypeArgMode::Required
                } else {
                    TypeArgMode::None
                }
            }
            _ => TypeArgMode::None,
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

        for arg in call.args.iter_mut() {
            self.evaluate_function_body(arg);
        }
        for kwarg in call.kwargs.iter_mut() {
            self.evaluate_function_body(&mut kwarg.value);
        }
    }

    pub(super) fn type_from_value(&self, value: &Value) -> Ty {
        fp_typing::type_from_value(value)
    }

    fn intrinsic_panic_message(&mut self, call: &mut ExprIntrinsicCall) -> String {
        if call.args.is_empty() {
            return "panic! macro triggered".to_string();
        }
        if call.args.len() > 1 {
            self.emit_error("panic expects zero or one argument");
        }
        let flow = self.eval_expr_runtime(&mut call.args[0]);
        let value = self.finish_runtime_flow(flow);
        format!("{}", value)
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
        match split_format_call(call) {
            Ok(Some((template, args, kwargs))) => {
                return self.render_format_template(template, args, kwargs);
            }
            Ok(None) => {}
            Err(err) => return Err(err),
        }

        let mut rendered = Vec::with_capacity(call.args.len() + call.kwargs.len());
        for expr in call.args.iter_mut() {
            let value = self.eval_expr(expr);
            let text = format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
            rendered.push(text);
        }
        for kwarg in call.kwargs.iter_mut() {
            let value = self.eval_expr(&mut kwarg.value);
            let text = format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
            rendered.push(text);
        }
        Ok(rendered.join(" "))
    }

    fn render_intrinsic_call_runtime(
        &mut self,
        call: &mut ExprIntrinsicCall,
    ) -> std::result::Result<String, String> {
        match split_format_call(call) {
            Ok(Some((template, args, kwargs))) => {
                return self.render_format_template_runtime(template, args, kwargs);
            }
            Ok(None) => {}
            Err(err) => return Err(err),
        }

        let mut rendered = Vec::with_capacity(call.args.len() + call.kwargs.len());
        for expr in call.args.iter_mut() {
            let flow = self.eval_expr_runtime(expr);
            let value = self.finish_runtime_flow(flow);
            let text = format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
            rendered.push(text);
        }
        for kwarg in call.kwargs.iter_mut() {
            let flow = self.eval_expr_runtime(&mut kwarg.value);
            let value = self.finish_runtime_flow(flow);
            let text = format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
            rendered.push(text);
        }
        Ok(rendered.join(" "))
    }

    pub(super) fn render_format_template(
        &mut self,
        template: &mut ExprStringTemplate,
        args: &mut [Expr],
        kwargs: &mut [ExprKwArg],
    ) -> std::result::Result<String, String> {
        let positional: Vec<Value> = args.iter_mut().map(|expr| self.eval_expr(expr)).collect();

        for (expr, value) in args.iter_mut().zip(positional.iter()) {
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
        for kwarg in kwargs.iter_mut() {
            let value = self.eval_expr(&mut kwarg.value);
            named.insert(kwarg.name.clone(), value);
        }

        let mut output = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(literal) => {
                    let rendered = self.interpolate_literal_template(literal, &named)?;
                    output.push_str(&rendered);
                }
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let index = implicit_index;
                            implicit_index += 1;
                            positional.get(index).cloned()
                        }
                        FormatArgRef::Positional(index) => positional.get(*index).cloned(),
                        FormatArgRef::Named(name) => {
                            named.get(name).cloned().or_else(|| self.lookup_value(name))
                        }
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

                    let formatted = format_value_with_spec(
                        &value,
                        placeholder
                            .format_spec
                            .as_ref()
                            .map(|spec| spec.raw.as_str()),
                    )
                    .map_err(|err| err.to_string())?;
                    output.push_str(&formatted);
                }
            }
        }

        Ok(output)
    }

    pub(super) fn render_format_template_runtime(
        &mut self,
        template: &mut ExprStringTemplate,
        args: &mut [Expr],
        kwargs: &mut [ExprKwArg],
    ) -> std::result::Result<String, String> {
        let positional: Vec<Value> = args
            .iter_mut()
            .map(|expr| {
                let flow = self.eval_expr_runtime(expr);
                self.finish_runtime_flow(flow)
            })
            .collect();

        let mut named = HashMap::new();
        for kwarg in kwargs.iter_mut() {
            let flow = self.eval_expr_runtime(&mut kwarg.value);
            let value = self.finish_runtime_flow(flow);
            named.insert(kwarg.name.clone(), value);
        }

        let mut output = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(literal) => {
                    let rendered = self.interpolate_literal_template(literal, &named)?;
                    output.push_str(&rendered);
                }
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let index = implicit_index;
                            implicit_index += 1;
                            positional.get(index).cloned()
                        }
                        FormatArgRef::Positional(index) => positional.get(*index).cloned(),
                        FormatArgRef::Named(name) => {
                            named.get(name).cloned().or_else(|| self.lookup_value(name))
                        }
                    };
                    if let Some(value) = value {
                        let rendered = format_value_with_spec(
                            &value,
                            placeholder
                                .format_spec
                                .as_ref()
                                .map(|spec| spec.raw.as_str()),
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

#[derive(Clone, Copy)]
enum TypeArgMode {
    Required,
    Fallback,
    None,
}

fn token_stream_to_string(tokens: &[MacroTokenTree]) -> String {
    fn is_ident_like(text: &str) -> bool {
        text.chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
    }
    fn needs_space(prev: &str, next: &str) -> bool {
        is_ident_like(prev) && is_ident_like(next)
    }
    fn flatten(tokens: &[MacroTokenTree], out: &mut Vec<String>) {
        for tree in tokens {
            match tree {
                MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
                MacroTokenTree::Group(group) => {
                    let (open, close) = match group.delimiter {
                        fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                        fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                        fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                    };
                    out.push(open.to_string());
                    flatten(&group.tokens, out);
                    out.push(close.to_string());
                }
            }
        }
    }

    let mut parts = Vec::new();
    flatten(tokens, &mut parts);
    let mut out = String::new();
    let mut prev: Option<String> = None;
    for part in parts {
        if let Some(prev_part) = prev.as_deref() {
            if needs_space(prev_part, &part) {
                out.push(' ');
            }
        }
        out.push_str(&part);
        prev = Some(part);
    }
    out
}

fn split_format_call(
    call: &mut ExprIntrinsicCall,
) -> std::result::Result<Option<(&mut ExprStringTemplate, &mut [Expr], &mut [ExprKwArg])>, String> {
    let (first, rest) = match call.args.split_first_mut() {
        Some(parts) => parts,
        None => return Ok(None),
    };

    if matches!(first.kind(), ExprKind::FormatString(_)) {
        if let ExprKind::FormatString(template) = first.kind_mut() {
            return Ok(Some((template, rest, call.kwargs.as_mut_slice())));
        }
        return Err("format template is missing".to_string());
    }

    let template_text = match first.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(string) => Some(string.value.clone()),
            _ => None,
        },
        _ => None,
    };

    if !matches!(
        call.kind,
        IntrinsicCallKind::Format | IntrinsicCallKind::Print | IntrinsicCallKind::Println
    ) {
        return Ok(None);
    }

    let Some(template_text) = template_text else {
        return Ok(None);
    };

    let parts = fp_core::ast::parse_format_template(&template_text)
        .map_err(|err| format!("invalid format template: {err}"))?;
    first.kind = ExprKind::FormatString(ExprStringTemplate { parts });

    if let ExprKind::FormatString(template) = first.kind_mut() {
        Ok(Some((template, rest, call.kwargs.as_mut_slice())))
    } else {
        Err("failed to build format template".to_string())
    }
}
