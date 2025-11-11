use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn insert_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Plain(value));
        }
    }

    pub(super) fn lookup_value(&self, name: &str) -> Option<Value> {
        for scope in self.value_env.iter().rev() {
            match scope.get(name) {
                Some(StoredValue::Plain(v)) => return Some(v.clone()),
                Some(StoredValue::Closure(_)) => return None,
                None => {}
            }
        }
        None
    }

    pub(super) fn lookup_closure(&self, name: &str) -> Option<ConstClosure> {
        for scope in self.value_env.iter().rev() {
            match scope.get(name) {
                Some(StoredValue::Closure(closure)) => return Some(closure.clone()),
                Some(StoredValue::Plain(_)) => return None,
                None => {}
            }
        }
        None
    }

    pub(super) fn bind_pattern(&mut self, pattern: &Pattern, value: Value) {
        if let Some(ident) = pattern.as_ident() {
            self.insert_value(ident.as_str(), value);
        }
    }

    pub(super) fn take_pending_closure(&mut self) -> Option<ConstClosure> {
        self.pending_closure.take()
    }

    pub(super) fn set_pending_expr_ty(&mut self, ty: Option<Ty>) {
        let ty = ty.unwrap_or_else(|| Ty::Unit(TypeUnit));
        self.pending_expr_ty = Some(ty);
    }

    pub(super) fn value_function_ret_ty(function: &ValueFunction) -> Option<Ty> {
        function.sig.ret_ty.clone()
    }

    pub(super) fn item_function_ret_ty(function: &ItemDefFunction) -> Option<Ty> {
        function.sig.ret_ty.clone().or_else(|| {
            function
                .ty
                .as_ref()
                .and_then(|ty| Self::type_function_ret_ty(ty))
        })
    }

    pub(super) fn insert_type(&mut self, name: &str, ty: Ty) {
        if let Some(scope) = self.type_env.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }
}

