use super::*;
use std::sync::Arc;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn annotate_expr_closure(closure: &mut ExprClosure, fn_sig: &TypeFunction) {
        for (param, param_ty) in closure.params.iter_mut().zip(fn_sig.params.iter()) {
            if !matches!(param_ty, Ty::Unknown(_)) {
                param.set_ty(param_ty.clone());
            }
        }

        if let Some(ret_ty) = fn_sig.ret_ty.as_ref() {
            if !matches!(ret_ty.as_ref(), Ty::Unknown(_)) {
                closure.body.set_ty(ret_ty.as_ref().clone());
            }
        }
    }

    pub(super) fn annotate_const_closure(closure: &mut ConstClosure, fn_sig: &TypeFunction) {
        for (param, param_ty) in closure.params.iter_mut().zip(fn_sig.params.iter()) {
            if !matches!(param_ty, Ty::Unknown(_)) {
                param.set_ty(param_ty.clone());
            }
        }

        closure.function_ty = Some(Ty::Function(fn_sig.clone()));
    }

    pub(super) fn annotate_pending_closure(
        &mut self,
        pattern: Option<&mut Pattern>,
        expr: Option<&mut Expr>,
    ) {
        if self.pending_closure.is_none() {
            return;
        }

        let candidate_ty = self
            .pending_closure
            .as_ref()
            .and_then(|pending| pending.function_ty.clone())
            .or_else(|| expr.as_ref().and_then(|e| e.ty().cloned()))
            .or_else(|| pattern.as_ref().and_then(|p| p.ty().cloned()));

        let Some(function_ty) = candidate_ty else {
            return;
        };
        if matches!(function_ty, Ty::Unknown(_)) {
            return;
        }

        if let Some(expr) = expr {
            expr.set_ty(function_ty.clone());
            if let Ty::Function(ref fn_sig) = function_ty {
                if let ExprKind::Closure(closure) = expr.kind_mut() {
                    Self::annotate_expr_closure(closure, fn_sig);
                }
            }
        }

        if let Some(pattern) = pattern {
            pattern.set_ty(function_ty.clone());
        }

        if let Some(pending) = self.pending_closure.as_mut() {
            pending.function_ty = Some(function_ty.clone());
            if let Ty::Function(ref fn_sig) = function_ty {
                Self::annotate_const_closure(pending, fn_sig);
            }
        }
    }

    pub(super) fn capture_closure(&self, closure: &ExprClosure, ty: Option<Ty>) -> ConstClosure {
        let mut captured = ConstClosure {
            params: closure.params.clone(),
            ret_ty: closure.ret_ty.as_ref().map(|ty| (**ty).clone()),
            body: closure.body.as_ref().clone(),
            captured_values: self.value_env.clone(),
            captured_types: self.type_env.clone(),
            module_stack: self.module_stack.clone(),
            function_ty: ty,
        };
        if let Some(fn_sig) = captured.function_ty.as_ref().and_then(|ty| match ty {
            Ty::Function(fn_sig) => Some(fn_sig.clone()),
            _ => None,
        }) {
            Self::annotate_const_closure(&mut captured, &fn_sig);
        }
        captured
    }

    // Capture a runtime closure with its environment snapshot.
    pub(super) fn capture_runtime_closure(
        &mut self,
        closure: &ExprClosure,
        ty: Option<Ty>,
    ) -> Value {
        let captured = self.capture_closure(closure, ty);
        Value::Any(AnyBox::new(captured))
    }

    pub(super) fn call_function(&mut self, function: ItemDefFunction, args: Vec<Value>) -> Value {
        if !function.sig.generics_params.is_empty() {
            self.emit_error(format!(
                "generic functions are not supported in const evaluation: {}",
                function.name.as_str()
            ));
            return Value::undefined();
        }
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function '{}' expected {} arguments, found {}",
                function.name.as_str(),
                function.sig.params.len(),
                args.len()
            ));
            return Value::undefined();
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Const,
            CallFrameKind::Function(function.name.as_str().to_string()),
            function.body.as_ref().span,
        );
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }
        let body = function.body.as_ref().clone();
        let result = if let Some(kind) = function.sig.quote_kind {
            // Quote functions lower their body into a quote token internally.
            // Call sites always receive a quote token, never a raw value.
            let was_in_const = self.in_const_region();
            if !was_in_const {
                self.enter_const_region();
            }
            let result = self.build_quote_token_from_body(kind, &body);
            if !was_in_const {
                self.exit_const_region();
            }
            result
        } else {
            let mut body = body;
            self.eval_expr(&mut body)
        };
        self.pop_scope();
        result
    }

    pub(super) fn call_value_function(
        &mut self,
        function: &ValueFunction,
        args: Vec<Value>,
    ) -> Value {
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function literal expected {} arguments, found {}",
                function.sig.params.len(),
                args.len()
            ));
            return Value::undefined();
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Const,
            CallFrameKind::ValueFunction,
            function.body.as_ref().span,
        );
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }
        let body = function.body.as_ref().clone();
        let result = if let Some(kind) = function.sig.quote_kind {
            // Quote functions lower their body into a quote token internally.
            let was_in_const = self.in_const_region();
            if !was_in_const {
                self.enter_const_region();
            }
            let result = self.build_quote_token_from_body(kind, &body);
            if !was_in_const {
                self.exit_const_region();
            }
            result
        } else {
            let mut body = body;
            self.eval_expr(&mut body)
        };
        self.pop_scope();
        result
    }

    // Execute a runtime function call with return/break propagation.
    pub(super) fn call_function_runtime(
        &mut self,
        function: ItemDefFunction,
        args: Vec<Value>,
    ) -> RuntimeFlow {
        if function.sig.quote_kind.is_some() {
            self.emit_error("quote functions can only be invoked in const evaluation");
            return RuntimeFlow::Value(Value::undefined());
        }
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function '{}' expected {} arguments, found {}",
                function.name.as_str(),
                function.sig.params.len(),
                args.len()
            ));
            return RuntimeFlow::Value(Value::undefined());
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Runtime,
            CallFrameKind::Function(function.name.as_str().to_string()),
            function.body.as_ref().span,
        );

        self.function_depth += 1;
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            if let Value::Any(any) = &value {
                if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                    self.insert_shared_value(param.name.as_str(), Arc::clone(&runtime_ref.shared));
                    continue;
                }
            }
            self.insert_mutable_value(param.name.as_str(), value);
        }
        let mut body = function.body.as_ref().clone();
        let flow = self.eval_expr_runtime(&mut body);
        self.pop_scope();
        self.function_depth -= 1;

        match flow {
            RuntimeFlow::Return(value) => RuntimeFlow::Value(value.unwrap_or_else(Value::unit)),
            RuntimeFlow::Break(_) | RuntimeFlow::Continue => {
                self.emit_error("loop control flow escaped a function");
                RuntimeFlow::Value(Value::undefined())
            }
            other => other,
        }
    }

    // Execute a runtime method call with receiver binding.
    pub(super) fn call_method_runtime(
        &mut self,
        function: ItemDefFunction,
        receiver: ReceiverBinding,
        args: Vec<Value>,
    ) -> RuntimeFlow {
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "method '{}' expected {} arguments, found {}",
                function.name.as_str(),
                function.sig.params.len(),
                args.len()
            ));
            return RuntimeFlow::Value(Value::undefined());
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Runtime,
            CallFrameKind::Method(function.name.as_str().to_string()),
            function.body.as_ref().span,
        );

        let receiver_kind = function
            .sig
            .receiver
            .unwrap_or(fp_core::ast::FunctionParamReceiver::Value);

        self.function_depth += 1;
        self.push_scope();

        let impl_context = self
            .value_type_name(&receiver.value)
            .map(|self_ty| ImplContext {
                self_ty: Some(self_ty),
                trait_ty: None,
            });
        if let Some(context) = impl_context.clone() {
            self.impl_stack.push(context);
        }

        let receiver_name = "#self";
        let plain_self_name = "self";
        match receiver_kind {
            fp_core::ast::FunctionParamReceiver::RefMut
            | fp_core::ast::FunctionParamReceiver::MutValue
            | fp_core::ast::FunctionParamReceiver::RefMutStatic => {
                if let Some(shared) = receiver.shared.clone() {
                    self.insert_shared_value(receiver_name, shared.clone());
                    self.insert_shared_value(plain_self_name, shared);
                } else {
                    self.emit_error("mutable receiver requires a mutable binding");
                    self.insert_mutable_value(receiver_name, receiver.value.clone());
                    self.insert_mutable_value(plain_self_name, receiver.value.clone());
                }
            }
            _ => {
                self.insert_value(receiver_name, receiver.value.clone());
                self.insert_value(plain_self_name, receiver.value.clone());
            }
        }

        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }
        let mut body = function.body.as_ref().clone();
        let flow = self.eval_expr_runtime(&mut body);
        if impl_context.is_some() {
            self.impl_stack.pop();
        }
        self.pop_scope();
        self.function_depth -= 1;

        match flow {
            RuntimeFlow::Return(value) => RuntimeFlow::Value(value.unwrap_or_else(Value::unit)),
            RuntimeFlow::Break(_) | RuntimeFlow::Continue => {
                self.emit_error("loop control flow escaped a method");
                RuntimeFlow::Value(Value::undefined())
            }
            other => other,
        }
    }

    // Execute a runtime function value call.
    pub(super) fn call_value_function_runtime(
        &mut self,
        function: &ValueFunction,
        args: Vec<Value>,
    ) -> RuntimeFlow {
        if function.sig.quote_kind.is_some() {
            self.emit_error("quote functions can only be invoked in const evaluation");
            return RuntimeFlow::Value(Value::undefined());
        }
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function literal expected {} arguments, found {}",
                function.sig.params.len(),
                args.len()
            ));
            return RuntimeFlow::Value(Value::undefined());
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Runtime,
            CallFrameKind::ValueFunction,
            function.body.as_ref().span,
        );

        self.function_depth += 1;
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            if let Value::Any(any) = &value {
                if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                    self.insert_shared_value(param.name.as_str(), Arc::clone(&runtime_ref.shared));
                    continue;
                }
            }
            self.insert_mutable_value(param.name.as_str(), value);
        }
        let mut body = function.body.as_ref().clone();
        let flow = self.eval_expr_runtime(&mut body);
        self.pop_scope();
        self.function_depth -= 1;

        match flow {
            RuntimeFlow::Return(value) => RuntimeFlow::Value(value.unwrap_or_else(Value::unit)),
            RuntimeFlow::Break(_) | RuntimeFlow::Continue => {
                self.emit_error("loop control flow escaped a function value");
                RuntimeFlow::Value(Value::undefined())
            }
            other => other,
        }
    }

    // Execute a captured closure with its environment snapshot.
    pub(super) fn call_const_closure_runtime(
        &mut self,
        closure: &ConstClosure,
        args: Vec<Value>,
    ) -> RuntimeFlow {
        if closure.params.len() != args.len() {
            self.emit_error(format!(
                "closure expected {} arguments, found {}",
                closure.params.len(),
                args.len()
            ));
            return RuntimeFlow::Value(Value::undefined());
        }
        let _call_guard = self.push_call_frame(
            EvalMode::Runtime,
            CallFrameKind::ConstClosure,
            closure.body.span,
        );

        let saved_values = std::mem::replace(&mut self.value_env, closure.captured_values.clone());
        let saved_types = std::mem::replace(&mut self.type_env, closure.captured_types.clone());
        let saved_modules = std::mem::replace(&mut self.module_stack, closure.module_stack.clone());

        self.function_depth += 1;
        self.push_scope();
        for (param, value) in closure.params.iter().zip(args.into_iter()) {
            self.bind_pattern(param, value);
        }
        let mut body = closure.body.clone();
        let flow = self.eval_expr_runtime(&mut body);
        self.pop_scope();
        self.function_depth -= 1;

        self.value_env = saved_values;
        self.type_env = saved_types;
        self.module_stack = saved_modules;

        match flow {
            RuntimeFlow::Return(value) => RuntimeFlow::Value(value.unwrap_or_else(Value::unit)),
            RuntimeFlow::Break(_) | RuntimeFlow::Continue => {
                self.emit_error("loop control flow escaped a closure");
                RuntimeFlow::Value(Value::undefined())
            }
            other => other,
        }
    }
}
