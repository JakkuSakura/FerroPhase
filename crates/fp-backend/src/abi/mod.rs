use fp_core::ast::{AttrMeta, Attribute, ExprKind, Value};
use fp_core::hir;
use fp_core::mir;

pub fn is_c_abi_hir(abi: &hir::Abi) -> bool {
    matches!(abi, hir::Abi::C { .. } | hir::Abi::System { .. })
}

pub fn is_c_abi_mir(abi: &mir::ty::Abi) -> bool {
    matches!(abi, mir::ty::Abi::C { .. } | mir::ty::Abi::System { .. })
}

pub fn extern_symbol_name(full: &str) -> String {
    full.rsplit("::").next().unwrap_or(full).to_string()
}

pub fn extern_symbol_name_with_attrs(full: &str, attrs: &[Attribute]) -> String {
    if let Some(link_name) = extern_link_name_attr(attrs) {
        return link_name;
    }
    extern_symbol_name(full)
}

fn extern_link_name_attr(attrs: &[Attribute]) -> Option<String> {
    let attr = attrs.iter().find(|attr| match &attr.meta {
        AttrMeta::NameValue(nv) => nv.name.last().as_str() == "link_name",
        _ => false,
    })?;
    let AttrMeta::NameValue(meta) = &attr.meta else {
        return None;
    };
    match meta.value.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(text) => Some(text.value.clone()),
            _ => None,
        },
        _ => None,
    }
}
