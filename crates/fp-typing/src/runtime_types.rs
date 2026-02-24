use fp_core::ast::*;
use fp_core::module::path::PathPrefix;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub enum TypeBindingMatch {
    Scoped { index: usize, key: String, ty: Ty },
    Global { key: String, ty: Ty },
    MissingImported { name: String },
    Missing,
}

pub fn resolve_type_binding_match(
    type_env: &[HashMap<String, Ty>],
    global_types: &HashMap<String, Ty>,
    imported_types: &HashSet<String>,
    path: &Path,
) -> TypeBindingMatch {
    let Some(base_ident) = path.segments.last() else {
        return TypeBindingMatch::Missing;
    };

    for idx in (0..type_env.len()).rev() {
        if let Some((key, ty)) = type_env[idx]
            .iter()
            .find(|(key, _)| key_matches_path(key, path))
            .map(|(key, ty)| (key.clone(), ty.clone()))
        {
            return TypeBindingMatch::Scoped {
                index: idx,
                key,
                ty,
            };
        }

        if let Some((key, ty)) = type_env[idx]
            .iter()
            .find(|(key, _)| key_matches_base_ident(key, base_ident))
            .map(|(key, ty)| (key.clone(), ty.clone()))
        {
            return TypeBindingMatch::Scoped {
                index: idx,
                key,
                ty,
            };
        }
    }

    if let Some((key, ty)) = global_types
        .iter()
        .find(|(key, _)| key_matches_path(key, path))
        .map(|(key, ty)| (key.clone(), ty.clone()))
    {
        return TypeBindingMatch::Global { key, ty };
    }

    if let Some((key, ty)) = global_types
        .iter()
        .find(|(key, _)| key_matches_base_ident(key, base_ident))
        .map(|(key, ty)| (key.clone(), ty.clone()))
    {
        return TypeBindingMatch::Global { key, ty };
    }

    if let Some(name) = imported_types.iter().find(|key| key_matches_path(key, path)) {
        return TypeBindingMatch::MissingImported { name: name.clone() };
    }

    if let Some(name) = imported_types
        .iter()
        .find(|key| key_matches_base_ident(key, base_ident))
    {
        return TypeBindingMatch::MissingImported { name: name.clone() };
    }

    TypeBindingMatch::Missing
}

fn key_matches_path(key: &str, path: &Path) -> bool {
    let mut key_iter = key.split("::").filter(|segment| !segment.is_empty());

    match path.prefix {
        PathPrefix::Root => {}
        PathPrefix::Crate => {
            if key_iter.next() != Some("crate") {
                return false;
            }
        }
        PathPrefix::SelfMod => {
            if key_iter.next() != Some("self") {
                return false;
            }
        }
        PathPrefix::Super(depth) => {
            for _ in 0..depth {
                if key_iter.next() != Some("super") {
                    return false;
                }
            }
        }
        PathPrefix::Plain => {}
    }

    for segment in &path.segments {
        if key_iter.next() != Some(segment.as_str()) {
            return false;
        }
    }

    key_iter.next().is_none()
}

fn key_matches_base_ident(key: &str, base_ident: &Ident) -> bool {
    !key.contains("::") && key == base_ident.as_str()
}

pub fn builtin_type_bindings() -> HashMap<String, Ty> {
    let mut bindings = HashMap::new();
    bindings.insert("i64".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
    bindings.insert("u64".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::U64)));
    bindings.insert("i32".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::I32)));
    bindings.insert("u32".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::U32)));
    bindings.insert("i16".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::I16)));
    bindings.insert("u16".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::U16)));
    bindings.insert("i8".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::I8)));
    bindings.insert("u8".to_string(), Ty::Primitive(TypePrimitive::Int(TypeInt::U8)));
    bindings.insert(
        "isize".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
    );
    bindings.insert(
        "usize".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::U64)),
    );
    bindings.insert("bool".to_string(), Ty::Primitive(TypePrimitive::Bool));
    bindings.insert("char".to_string(), Ty::Primitive(TypePrimitive::Char));
    bindings.insert("str".to_string(), Ty::Primitive(TypePrimitive::String));
    bindings.insert("String".to_string(), Ty::Primitive(TypePrimitive::String));
    bindings
}

pub fn infer_value_ty(value: &Value) -> Option<Ty> {
    match value {
        Value::Int(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
        Value::BigInt(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))),
        Value::Decimal(_) => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
        Value::BigDecimal(_) => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::BigDecimal))),
        Value::Bool(_) => Some(Ty::Primitive(TypePrimitive::Bool)),
        Value::Char(_) => Some(Ty::Primitive(TypePrimitive::Char)),
        Value::String(_) => Some(Ty::Primitive(TypePrimitive::String)),
        Value::List(_) => Some(Ty::Primitive(TypePrimitive::List)),
        Value::Struct(struct_value) => Some(Ty::Struct(struct_value.ty.clone())),
        Value::TokenStream(_) => Some(Ty::TokenStream(TypeTokenStream)),
        Value::Function(function) => Some(Ty::Function(TypeFunction {
            params: function
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: function.sig.generics_params.clone(),
            ret_ty: function.sig.ret_ty.as_ref().map(|ty| Box::new(ty.clone())),
        })),
        _ => None,
    }
}

pub fn type_from_value(value: &Value) -> Ty {
    match value {
        Value::Type(_) => Ty::Type(TypeType::new(fp_core::span::Span::null())),
        Value::Tuple(tuple) => Ty::Tuple(TypeTuple {
            types: tuple.values.iter().map(type_from_value).collect(),
        }),
        Value::Unit(_) => Ty::Unit(TypeUnit),
        other => infer_value_ty(other).unwrap_or(Ty::Any(TypeAny)),
    }
}

pub trait TypeMaterializeHooks {
    fn resolve_name(&mut self, locator: &Name) -> Option<Ty>;
    fn eval_const_expr(&mut self, expr: &mut Expr) -> Option<Ty>;
}

pub fn materialize_type_with_hooks(ty: Ty, hooks: &mut impl TypeMaterializeHooks) -> Ty {
    let Ty::Expr(mut expr) = ty else {
        return ty;
    };

    match expr.kind() {
        ExprKind::Name(locator) => hooks.resolve_name(locator).unwrap_or(Ty::Expr(expr)),
        ExprKind::ConstBlock(_) => hooks.eval_const_expr(expr.as_mut()).unwrap_or(Ty::Expr(expr)),
        _ => Ty::Expr(expr),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_prefers_most_local_qualified_key() {
        let mut scope0 = HashMap::new();
        scope0.insert("Vec".to_string(), Ty::Primitive(TypePrimitive::Bool));
        let mut scope1 = HashMap::new();
        scope1.insert(
            "std::collections::Vec".to_string(),
            Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        );

        let result = resolve_type_binding_match(
            &[scope0, scope1],
            &HashMap::new(),
            &HashSet::new(),
            &Path::plain(vec![
                Ident::new("std"),
                Ident::new("collections"),
                Ident::new("Vec"),
            ]),
        );

        match result {
            TypeBindingMatch::Scoped { index, key, .. } => {
                assert_eq!(index, 1);
                assert_eq!(key, "std::collections::Vec");
            }
            other => panic!("expected scoped match, got {other:?}"),
        }
    }

    #[test]
    fn resolve_falls_back_to_base_segment() {
        let mut globals = HashMap::new();
        globals.insert(
            "Vec".to_string(),
            Ty::Primitive(TypePrimitive::Int(TypeInt::I32)),
        );

        let result = resolve_type_binding_match(
            &[],
            &globals,
            &HashSet::new(),
            &Path::plain(vec![
                Ident::new("std"),
                Ident::new("collections"),
                Ident::new("Vec"),
            ]),
        );

        match result {
            TypeBindingMatch::Global { key, .. } => assert_eq!(key, "Vec"),
            other => panic!("expected global match, got {other:?}"),
        }
    }

    #[test]
    fn resolve_reports_missing_import_by_qualified_key() {
        let mut imported = HashSet::new();
        imported.insert("crate::types::UserId".to_string());

        let result = resolve_type_binding_match(
            &[],
            &HashMap::new(),
            &imported,
            &Path::new(
                fp_core::module::path::PathPrefix::Crate,
                vec![Ident::new("types"), Ident::new("UserId")],
            ),
        );

        match result {
            TypeBindingMatch::MissingImported { name } => {
                assert_eq!(name, "crate::types::UserId")
            }
            other => panic!("expected missing import match, got {other:?}"),
        }
    }
}
