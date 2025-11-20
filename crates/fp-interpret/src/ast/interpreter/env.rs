use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn insert_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Plain(value));
        }
    }

    pub(super) fn lookup_value(&self, name: &str) -> Option<Value> {
        for scope in self.value_env.iter().rev() {
            if let Some(StoredValue::Plain(v)) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    // closures are not stored in value_env; pending_closure holds captures for annotation
    pub(super) fn lookup_closure(&self, _name: &str) -> Option<ConstClosure> {
        None
    }

    pub(super) fn bind_pattern(&mut self, pattern: &Pattern, value: Value) {
        if let Some(ident) = pattern.as_ident() {
            self.insert_value(ident.as_str(), value);
        }
    }

    // removed unused: take_pending_closure

    pub(super) fn set_pending_expr_ty(&mut self, ty: Option<Ty>) {
        let ty = ty.unwrap_or_else(|| Ty::Unit(TypeUnit));
        self.pending_expr_ty = Some(ty);
    }

    pub(super) fn value_function_ret_ty(function: &ValueFunction) -> Option<Ty> {
        function.sig.ret_ty.clone()
    }

    // removed unused: item_function_ret_ty

    pub(super) fn insert_type(&mut self, name: &str, ty: Ty) {
        if let Some(scope) = self.type_env.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }
}
