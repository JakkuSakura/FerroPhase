use super::*;
use fp_core::ast::PatternKind;

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
        match pattern.kind() {
            PatternKind::Ident(ident) => {
                self.insert_value(ident.ident.as_str(), value);
            }
            PatternKind::Type(inner) => self.bind_pattern(&inner.pat, value),
            PatternKind::Tuple(tuple) => {
                if let Value::Tuple(items) = value {
                    for (pat, val) in tuple.patterns.iter().zip(items.values.iter().cloned()) {
                        self.bind_pattern(pat, val);
                    }
                }
            }
            _ => {
                // Best-effort; other pattern kinds are not yet supported by the const evaluator.
            }
        }
    }

    pub(super) fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> bool {
        match pattern.kind() {
            PatternKind::Wildcard(_) => true,
            PatternKind::Ident(_) => {
                self.bind_pattern(pattern, value.clone());
                true
            }
            PatternKind::Type(inner) => self.pattern_matches(&inner.pat, value),
            PatternKind::Tuple(tuple) => match value {
                Value::Tuple(items) if items.values.len() == tuple.patterns.len() => tuple
                    .patterns
                    .iter()
                    .zip(items.values.iter())
                    .all(|(pat, val)| self.pattern_matches(pat, val)),
                _ => false,
            },
            PatternKind::Variant(variant) => {
                if variant.pattern.is_some() {
                    return false;
                }
                let mut name_expr = variant.name.clone();
                let expected = self.eval_expr(&mut name_expr);
                match (&expected, value) {
                    (Value::Int(a), Value::Int(b)) => a.value == b.value,
                    (Value::Bool(a), Value::Bool(b)) => a.value == b.value,
                    (Value::String(a), Value::String(b)) => a.value == b.value,
                    _ => false,
                }
            }
            _ => false,
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
