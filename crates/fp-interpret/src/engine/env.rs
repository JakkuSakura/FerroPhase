use super::*;
use fp_core::ast::{
    AttrMeta, ItemKind, PatternKind, PatternQuote, PatternQuotePlural, QuoteItemKind,
    QuoteTokenValue, ValueField, ValueList, ValueQuoteToken, ValueStructural,
};
use std::sync::{Arc, Mutex};

impl<'ctx> AstInterpreter<'ctx> {
    pub(super) fn insert_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Plain(value.clone()));
        }
        if self.in_std_module() {
            if let Some(root) = self.value_env.first_mut() {
                root.insert(name.to_string(), StoredValue::Plain(value));
            }
        }
    }

    pub(super) fn insert_mutable_value(&mut self, name: &str, value: Value) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::shared(value.clone()));
        }
        if self.in_std_module() {
            if let Some(root) = self.value_env.first_mut() {
                root.insert(name.to_string(), StoredValue::shared(value));
            }
        }
    }

    pub(super) fn insert_shared_value(&mut self, name: &str, shared: Arc<Mutex<Value>>) {
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Shared(Arc::clone(&shared)));
        }
        if self.in_std_module() {
            if let Some(root) = self.value_env.first_mut() {
                root.insert(name.to_string(), StoredValue::Shared(shared));
            }
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
            PatternKind::QuotePlural(quote) => self.quote_plural_pattern_matches(quote, value),
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
        if !quote.fields.is_empty() {
            let QuoteTokenValue::Items(items) = &token.value else {
                return false;
            };
            if items.len() != 1 {
                return false;
            }
            let item = &items[0];
            let allow_any_item = quote.item.is_none() && token.kind == QuoteFragmentKind::Item;

            for field in &quote.fields {
                let Some(field_value) =
                    self.quote_item_field_value(item, quote, field.name.as_str(), allow_any_item)
                else {
                    return false;
                };
                if let Some(rename) = field.rename.as_ref() {
                    if !self.pattern_matches(rename, &field_value) {
                        return false;
                    }
                } else {
                    self.insert_value(field.name.as_str(), field_value.clone());
                }
            }
            return true;
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

    fn quote_item_field_value(
        &mut self,
        item: &Item,
        quote: &PatternQuote,
        name: &str,
        allow_any_item: bool,
    ) -> Option<Value> {
        let base = match name {
            "name" => item
                .get_ident()
                .map(|ident| Value::string(ident.name.clone())),
            _ => None,
        };

        if allow_any_item || matches!(quote.item, Some(QuoteItemKind::Function)) {
            if let Some(func) = item.as_function() {
                return match name {
                    "name" => Some(Value::string(func.name.name.clone())),
                    "params" => {
                        let mut params = Vec::new();
                        for param in &func.sig.params {
                            let fields = vec![
                                ValueField::new(
                                    Ident::new("name"),
                                    Value::string(param.name.name.clone()),
                                ),
                                ValueField::new(Ident::new("ty"), Value::Type(param.ty.clone())),
                            ];
                            params.push(Value::Structural(ValueStructural::new(fields)));
                        }
                        Some(Value::List(ValueList::new(params)))
                    }
                    "ret" => Some(Value::Type(
                        func.sig.ret_ty.clone().unwrap_or_else(Ty::unit),
                    )),
                    "body" => Some(Value::expr(func.body.as_ref().clone())),
                    "attrs" => {
                        let attrs = func
                            .attrs
                            .iter()
                            .map(|attr| {
                                let name = match &attr.meta {
                                    AttrMeta::Path(path) => path.last().as_str().to_string(),
                                    AttrMeta::List(list) => list.name.last().as_str().to_string(),
                                    AttrMeta::NameValue(nv) => nv.name.last().as_str().to_string(),
                                };
                                Value::string(name)
                            })
                            .collect();
                        Some(Value::List(ValueList::new(attrs)))
                    }
                    _ => base,
                };
            }
        }

        if allow_any_item || matches!(quote.item, Some(QuoteItemKind::Struct)) {
            if let Some(def) = item.as_struct() {
                return match name {
                    "name" => Some(Value::string(def.name.name.clone())),
                    "fields" => Some(Value::List(ValueList::new(
                        def.value
                            .fields
                            .iter()
                            .map(|field| {
                                Value::Structural(ValueStructural::new(vec![
                                    ValueField::new(
                                        Ident::new("name"),
                                        Value::string(field.name.name.clone()),
                                    ),
                                    ValueField::new(
                                        Ident::new("ty"),
                                        Value::Type(field.value.clone()),
                                    ),
                                ]))
                            })
                            .collect(),
                    ))),
                    _ => base,
                };
            }
            if let ItemKind::DefStructural(def) = item.kind() {
                return match name {
                    "name" => Some(Value::string(def.name.name.clone())),
                    "fields" => Some(Value::List(ValueList::new(
                        def.value
                            .fields
                            .iter()
                            .map(|field| {
                                Value::Structural(ValueStructural::new(vec![
                                    ValueField::new(
                                        Ident::new("name"),
                                        Value::string(field.name.name.clone()),
                                    ),
                                    ValueField::new(
                                        Ident::new("ty"),
                                        Value::Type(field.value.clone()),
                                    ),
                                ]))
                            })
                            .collect(),
                    ))),
                    _ => base,
                };
            }
        }

        if allow_any_item || matches!(quote.item, Some(QuoteItemKind::Enum)) {
            if let Some(def) = item.as_enum() {
                return match name {
                    "name" => Some(Value::string(def.name.name.clone())),
                    "variants" => Some(Value::List(ValueList::new(
                        def.value
                            .variants
                            .iter()
                            .map(|variant| {
                                Value::Structural(ValueStructural::new(vec![
                                    ValueField::new(
                                        Ident::new("name"),
                                        Value::string(variant.name.name.clone()),
                                    ),
                                    ValueField::new(
                                        Ident::new("ty"),
                                        Value::Type(variant.value.clone()),
                                    ),
                                ]))
                            })
                            .collect(),
                    ))),
                    _ => base,
                };
            }
        }

        base
    }

    fn quote_plural_pattern_matches(&mut self, quote: &PatternQuotePlural, value: &Value) -> bool {
        let Value::QuoteToken(token) = value else {
            return false;
        };
        if token.kind != quote.fragment {
            return false;
        }
        let QuoteTokenValue::Items(items) = &token.value else {
            return false;
        };
        if !items
            .iter()
            .all(|item| self.item_matches_quote_plural(item, quote.fragment))
        {
            return false;
        }
        let values = items
            .iter()
            .cloned()
            .map(|item| {
                Value::QuoteToken(ValueQuoteToken {
                    kind: QuoteFragmentKind::Item,
                    value: QuoteTokenValue::Items(vec![item]),
                })
            })
            .collect::<Vec<_>>();
        if quote.patterns.len() == 1 {
            let list = Value::List(ValueList::new(values));
            return self.pattern_matches(&quote.patterns[0], &list);
        }
        if quote.patterns.len() != values.len() {
            return false;
        }
        quote
            .patterns
            .iter()
            .zip(values.iter())
            .all(|(pat, val)| self.pattern_matches(pat, val))
    }

    fn item_matches_quote_plural(&self, item: &Item, kind: QuoteFragmentKind) -> bool {
        matches!(kind, QuoteFragmentKind::Item)
            && matches!(
                item.kind(),
                ItemKind::DefFunction(_)
                    | ItemKind::DefStruct(_)
                    | ItemKind::DefStructural(_)
                    | ItemKind::DefEnum(_)
                    | ItemKind::DefTrait(_)
                    | ItemKind::Impl(_)
                    | ItemKind::DefConst(_)
                    | ItemKind::DeclConst(_)
                    | ItemKind::DefStatic(_)
                    | ItemKind::DeclStatic(_)
                    | ItemKind::Module(_)
                    | ItemKind::Import(_)
                    | ItemKind::Macro(_)
            )
    }

    // removed unused: take_pending_closure

    pub(super) fn set_pending_expr_ty(&mut self, ty: Option<Ty>) {
        if self.typer.is_none() {
            return;
        }
        let ty = ty.unwrap_or_else(|| Ty::Unit(TypeUnit));
        self.pending_expr_ty = Some(ty);
    }

    pub(super) fn value_function_ret_ty(function: &ValueFunction) -> Option<Ty> {
        function.sig.ret_ty.clone()
    }

    // removed unused: item_function_ret_ty

    pub(super) fn insert_type(&mut self, name: &str, ty: Ty) {
        if let Some(scope) = self.type_env.last_mut() {
            scope.insert(name.to_string(), ty.clone());
        }
        if self.in_std_module() {
            self.global_types.insert(name.to_string(), ty);
        }
    }
}
