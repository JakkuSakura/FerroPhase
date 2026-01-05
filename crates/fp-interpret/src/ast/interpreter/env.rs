use super::*;
use fp_core::ast::{ItemKind, PatternKind, PatternQuote, QuoteItemKind, QuoteTokenValue};
use std::sync::{Arc, Mutex};

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn insert_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Plain(value));
        }
    }

    pub(super) fn insert_mutable_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::shared(value));
        }
    }

    pub(super) fn insert_shared_value(&mut self, name: &str, shared: Arc<Mutex<Value>>) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Shared(shared));
        }
    }

    pub(super) fn lookup_value(&self, name: &str) -> Option<Value> {
        for scope in self.value_env.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.value());
            }
        }
        None
    }

    pub(super) fn lookup_stored_value(&self, name: &str) -> Option<StoredValue> {
        for scope in self.value_env.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub(super) fn lookup_stored_value_mut(&mut self, name: &str) -> Option<&mut StoredValue> {
        for scope in self.value_env.iter_mut().rev() {
            if scope.contains_key(name) {
                return scope.get_mut(name);
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
                if ident.mutability.unwrap_or(false) {
                    self.insert_mutable_value(ident.ident.as_str(), value);
                } else {
                    self.insert_value(ident.ident.as_str(), value);
                }
            }
            PatternKind::Bind(bind) => {
                self.insert_value(bind.ident.ident.as_str(), value.clone());
                self.bind_pattern(&bind.pattern, value);
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
            PatternKind::Bind(bind) => {
                if self.pattern_matches(&bind.pattern, value) {
                    self.insert_value(bind.ident.ident.as_str(), value.clone());
                    true
                } else {
                    false
                }
            }
            PatternKind::Quote(quote) => self.quote_pattern_matches(quote, value),
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
                if let Value::Any(any) = value {
                    if let Some(enum_value) = any.downcast_ref::<RuntimeEnum>() {
                        let expected_name = match variant.name.kind() {
                            ExprKind::Locator(locator) => Self::locator_base_name(locator),
                            _ => variant.name.to_string(),
                        };
                        if enum_value.variant_name != expected_name {
                            return false;
                        }
                        if let Some(inner) = variant.pattern.as_ref() {
                            if let Some(payload) = enum_value.payload.as_ref() {
                                return self.pattern_matches(inner, payload);
                            }
                            return false;
                        }
                        return true;
                    }
                }

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
            PatternKind::Struct(pattern_struct) => match value {
                Value::Struct(struct_value) => pattern_struct.fields.iter().all(|field| {
                    struct_value
                        .structural
                        .fields
                        .iter()
                        .find(|existing| existing.name.as_str() == field.name.as_str())
                        .map(|existing| {
                            field
                                .rename
                                .as_ref()
                                .map(|pat| self.pattern_matches(pat, &existing.value))
                                .unwrap_or(true)
                        })
                        .unwrap_or(false)
                }),
                Value::Structural(structural) => pattern_struct.fields.iter().all(|field| {
                    structural
                        .fields
                        .iter()
                        .find(|existing| existing.name.as_str() == field.name.as_str())
                        .map(|existing| {
                            field
                                .rename
                                .as_ref()
                                .map(|pat| self.pattern_matches(pat, &existing.value))
                                .unwrap_or(true)
                        })
                        .unwrap_or(false)
                }),
                _ => false,
            },
            PatternKind::Structural(pattern_struct) => match value {
                Value::Structural(structural) => pattern_struct.fields.iter().all(|field| {
                    structural
                        .fields
                        .iter()
                        .find(|existing| existing.name.as_str() == field.name.as_str())
                        .map(|existing| {
                            field
                                .rename
                                .as_ref()
                                .map(|pat| self.pattern_matches(pat, &existing.value))
                                .unwrap_or(true)
                        })
                        .unwrap_or(false)
                }),
                Value::Struct(struct_value) => pattern_struct.fields.iter().all(|field| {
                    struct_value
                        .structural
                        .fields
                        .iter()
                        .find(|existing| existing.name.as_str() == field.name.as_str())
                        .map(|existing| {
                            field
                                .rename
                                .as_ref()
                                .map(|pat| self.pattern_matches(pat, &existing.value))
                                .unwrap_or(true)
                        })
                        .unwrap_or(false)
                }),
                _ => false,
            },
            _ => false,
        }
    }

    fn quote_pattern_matches(&mut self, quote: &PatternQuote, value: &Value) -> bool {
        let Value::QuoteToken(token) = value else {
            return false;
        };
        if token.kind != quote.fragment {
            return false;
        }
        let Some(expected_item) = quote.item else {
            return true;
        };
        let QuoteTokenValue::Items(items) = &token.value else {
            return false;
        };
        if items.len() != 1 {
            return false;
        }
        match items[0].kind() {
            ItemKind::DefFunction(_) => matches!(expected_item, QuoteItemKind::Function),
            ItemKind::DefStruct(_) => matches!(expected_item, QuoteItemKind::Struct),
            ItemKind::DefStructural(_) => matches!(expected_item, QuoteItemKind::Struct),
            ItemKind::DefEnum(_) => matches!(expected_item, QuoteItemKind::Enum),
            ItemKind::DefTrait(_) => matches!(expected_item, QuoteItemKind::Trait),
            ItemKind::Impl(_) => matches!(expected_item, QuoteItemKind::Impl),
            ItemKind::DefType(_) => matches!(expected_item, QuoteItemKind::Type),
            ItemKind::DefConst(_) | ItemKind::DeclConst(_) => {
                matches!(expected_item, QuoteItemKind::Const)
            }
            ItemKind::DefStatic(_) | ItemKind::DeclStatic(_) => {
                matches!(expected_item, QuoteItemKind::Static)
            }
            ItemKind::Module(_) => matches!(expected_item, QuoteItemKind::Module),
            ItemKind::Import(_) => matches!(expected_item, QuoteItemKind::Use),
            ItemKind::Macro(_) => matches!(expected_item, QuoteItemKind::Macro),
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
