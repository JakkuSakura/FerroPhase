use super::*;

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

    pub(super) fn annotate_pending_closure(&mut self, pattern: Option<&mut Pattern>, expr: Option<&mut Expr>) {
        if self.pending_closure.is_none() {
            return;
        }

        let candidate_ty = self
            .pending_closure
            .as_ref()
            .and_then(|pending| pending.function_ty.clone())
            .or_else(|| expr.as_ref().and_then(|e| e.ty().cloned()))
            .or_else(|| pattern.as_ref().and_then(|p| p.ty().cloned()));

        let Some(function_ty) = candidate_ty else { return; };
        if matches!(function_ty, Ty::Unknown(_)) { return; }

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

    pub(super) fn call_const_closure(&mut self, closure: &ConstClosure, args: Vec<Value>) -> Value {
        if closure.params.len() != args.len() {
            self.emit_error(format!(
                "closure expected {} arguments, found {}",
                closure.params.len(),
                args.len()
            ));
            return Value::undefined();
        }
        let saved_values = mem::replace(&mut self.value_env, closure.captured_values.clone());
        let saved_types = mem::replace(&mut self.type_env, closure.captured_types.clone());
        let saved_modules = mem::replace(&mut self.module_stack, closure.module_stack.clone());
        self.push_scope();
        for (pattern, value) in closure.params.iter().zip(args.into_iter()) {
            self.bind_pattern(pattern, value);
        }
        let mut body = closure.body.clone();
        let result = self.eval_expr(&mut body);
        self.pop_scope();
        self.value_env = saved_values;
        self.type_env = saved_types;
        self.module_stack = saved_modules;
        result
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
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }
        let mut body = function.body.as_ref().clone();
        let result = self.eval_expr(&mut body);
        self.pop_scope();
        result
    }

    pub(super) fn call_value_function(&mut self, function: &ValueFunction, args: Vec<Value>) -> Value {
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function literal expected {} arguments, found {}",
                function.sig.params.len(),
                args.len()
            ));
            return Value::undefined();
        }
        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }
        let mut body = function.body.as_ref().clone();
        let result = self.eval_expr(&mut body);
        self.pop_scope();
        result
    }
}

