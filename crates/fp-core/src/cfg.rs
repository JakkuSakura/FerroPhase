use crate::ast::{self, File, ItemKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetEnv {
    pub os: String,
    pub lang: Option<String>,
    pub pointer_width: String,
}

impl TargetEnv {
    pub fn host() -> Self {
        Self {
            os: host_target_os(),
            lang: None,
            pointer_width: host_pointer_width(),
        }
    }

    pub fn from_triple(triple: Option<&str>) -> Self {
        Self {
            os: target_os_from_triple(triple),
            lang: None,
            pointer_width: pointer_width_from_triple(triple),
        }
    }
}

pub fn filter_items_in_file(file: &mut File, env: &TargetEnv) {
    filter_items(&mut file.items, env);
}

fn filter_items(items: &mut Vec<ast::Item>, env: &TargetEnv) {
    items.retain(|item| item_enabled_by_cfg(item, env));
    for item in items.iter_mut() {
        filter_item(item, env);
    }
}

fn filter_item(item: &mut ast::Item, env: &TargetEnv) {
    if let ItemKind::Module(module) = item.kind_mut() {
        filter_items(&mut module.items, env);
    }
}

pub fn item_enabled_by_cfg(item: &ast::Item, env: &TargetEnv) -> bool {
    let Some(attrs) = item_attrs(item) else {
        return true;
    };
    cfg_attrs_enabled(attrs, env)
}

fn item_attrs(item: &ast::Item) -> Option<&[ast::Attribute]> {
    match item.kind() {
        ItemKind::Module(module) => Some(&module.attrs),
        ItemKind::DefStruct(def) => Some(&def.attrs),
        ItemKind::DefStructural(def) => Some(&def.attrs),
        ItemKind::DefEnum(def) => Some(&def.attrs),
        ItemKind::DefType(def) => Some(&def.attrs),
        ItemKind::OpaqueType(def) => Some(&def.attrs),
        ItemKind::DefConst(def) => Some(&def.attrs),
        ItemKind::DefStatic(def) => Some(&def.attrs),
        ItemKind::DefFunction(def) => Some(&def.attrs),
        ItemKind::DefTrait(def) => Some(&def.attrs),
        ItemKind::DeclFunction(decl) => Some(&decl.attrs),
        ItemKind::Import(import) => Some(&import.attrs),
        ItemKind::Impl(impl_block) => Some(&impl_block.attrs),
        _ => None,
    }
}

fn cfg_attrs_enabled(attrs: &[ast::Attribute], env: &TargetEnv) -> bool {
    for attr in attrs {
        let ast::AttrMeta::List(list) = &attr.meta else {
            continue;
        };
        if list.name.last().as_str() != "cfg" {
            continue;
        }
        if !cfg_list_items_enabled(&list.items, env) {
            return false;
        }
    }
    true
}

fn cfg_list_items_enabled(items: &[ast::AttrMeta], env: &TargetEnv) -> bool {
    if items.is_empty() {
        return false;
    }
    items.iter().all(|item| cfg_meta_enabled(item, env))
}

/// Evaluates a single `cfg` predicate (the parsed inner content of
/// `#[cfg(...)]`, or of a `cfg!(...)` macro invocation, which uses the
/// identical grammar). `pub` so it can also be reused to normalize `cfg!()`
/// as an expression-position macro, not just attribute-position filtering.
pub fn cfg_meta_enabled(meta: &ast::AttrMeta, env: &TargetEnv) -> bool {
    match meta {
        ast::AttrMeta::Path(path) => match path.last().as_str() {
            "unix" => env.os == "linux" || env.os == "macos",
            "windows" => env.os == "windows",
            _ => false,
        },
        ast::AttrMeta::NameValue(nv) => {
            let Some(value) = string_literal_value(&nv.value) else {
                return false;
            };
            match nv.name.last().as_str() {
                "target_os" => value == env.os,
                "target_lang" => env.lang.as_deref() == Some(value.as_str()),
                "target_pointer_width" => value == env.pointer_width,
                _ => false,
            }
        }
        ast::AttrMeta::List(list) => match list.name.last().as_str() {
            "any" => list.items.iter().any(|item| cfg_meta_enabled(item, env)),
            "all" => list.items.iter().all(|item| cfg_meta_enabled(item, env)),
            "not" => {
                if list.items.len() != 1 {
                    return false;
                }
                !cfg_meta_enabled(&list.items[0], env)
            }
            _ => false,
        },
    }
}

fn string_literal_value(expr: &ast::Expr) -> Option<String> {
    if let ast::ExprKind::Value(value) = expr.kind() {
        if let ast::Value::String(string) = &**value {
            return Some(string.value.clone());
        }
    }
    None
}

fn host_target_os() -> String {
    if cfg!(target_os = "linux") {
        "linux".to_string()
    } else if cfg!(target_os = "macos") {
        "macos".to_string()
    } else if cfg!(target_os = "windows") {
        "windows".to_string()
    } else {
        "unknown".to_string()
    }
}

fn host_pointer_width() -> String {
    usize::BITS.to_string()
}

fn pointer_width_from_triple(triple: Option<&str>) -> String {
    let Some(triple) = triple else {
        return host_pointer_width();
    };
    let triple = triple.to_ascii_lowercase();
    if triple.starts_with("i686")
        || triple.starts_with("i386")
        || triple.starts_with("arm-")
        || triple.starts_with("armv7")
        || triple.starts_with("thumbv7")
        || triple.starts_with("wasm32")
    {
        "32".to_string()
    } else if triple.starts_with("x86_64")
        || triple.starts_with("aarch64")
        || triple.starts_with("wasm64")
    {
        "64".to_string()
    } else {
        host_pointer_width()
    }
}

fn target_os_from_triple(triple: Option<&str>) -> String {
    let Some(triple) = triple else {
        return host_target_os();
    };
    let triple = triple.to_ascii_lowercase();
    if triple.contains("apple-darwin") || triple.contains("apple") || triple.contains("darwin") {
        "macos".to_string()
    } else if triple.contains("windows") || triple.contains("mingw") {
        "windows".to_string()
    } else if triple.contains("linux") {
        "linux".to_string()
    } else {
        "unknown".to_string()
    }
}
