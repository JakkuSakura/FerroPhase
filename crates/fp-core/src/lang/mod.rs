use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{AttrMeta, Attribute, ExprKind, File, Ident, Item, ItemKind, Name, Path, Value};
use crate::intrinsics::{CallKind, lang_instrinstic_call_kind, lang_instrinstic_for_lang_item};

#[derive(Clone, Default)]
pub struct LangItemRegistry {
    items: HashMap<String, Path>,
    ops: HashMap<String, Path>,
}

impl LangItemRegistry {
    pub fn insert(&mut self, name: impl Into<String>, path: Path) {
        self.items.insert(name.into(), path);
    }

    pub fn insert_op(&mut self, name: impl Into<String>, path: Path) {
        self.ops.insert(name.into(), path);
    }

    pub fn extend(&mut self, other: LangItemRegistry) {
        for (name, path) in other.items {
            self.items.insert(name, path);
        }
        for (name, path) in other.ops {
            self.ops.insert(name, path);
        }
    }

    pub fn get_path(&self, name: &str) -> Option<&Path> {
        self.items.get(name)
    }

    pub fn get_op_path(&self, name: &str) -> Option<&Path> {
        self.ops.get(name)
    }
}

thread_local! {
    static LANG_ITEMS: RefCell<Option<LangItemRegistry>> = RefCell::new(None);
}

pub fn register_threadlocal_lang_items(registry: LangItemRegistry) {
    LANG_ITEMS.with(|slot| {
        *slot.borrow_mut() = Some(registry);
    });
}

pub fn try_get_threadlocal_lang_items() -> Option<LangItemRegistry> {
    LANG_ITEMS.with(|slot| slot.borrow().clone())
}

pub fn collect_lang_items(file: &File) -> LangItemRegistry {
    let mut registry = LangItemRegistry::default();
    let mut module_path = Vec::new();
    collect_lang_items_from_items(&file.items, &mut module_path, &mut registry);
    registry
}

pub fn lookup_intrinsic(name: &Name) -> Option<CallKind> {
    let name = lookup_intrinsic_name(name)?;
    lang_instrinstic_for_lang_item(&name)
        .and_then(lang_instrinstic_call_kind)
        .and_then(|kind| kind.intrinsic_kind().map(CallKind::from))
}

pub fn lookup_op_intrinsic(name: &Name) -> Option<CallKind> {
    let name = lookup_op_name(name)?;
    lang_instrinstic_for_lang_item(&name)
        .and_then(lang_instrinstic_call_kind)
        .and_then(|kind| kind.op_kind().map(CallKind::from))
}

pub fn lookup_intrinsic_name(name: &Name) -> Option<String> {
    let registry = try_get_threadlocal_lang_items()?;
    let name_segments: Vec<&str> = match name {
        Name::Ident(ident) => vec![ident.name.as_str()],
        Name::Path(path) => path.segments.iter().map(|seg| seg.name.as_str()).collect(),
        _ => return None,
    };

    for (name, path) in registry.items {
        let path_segments: Vec<&str> = path.segments.iter().map(|seg| seg.name.as_str()).collect();
        if path_segments == name_segments {
            return Some(name);
        }
    }
    None
}

pub fn lookup_op_name(name: &Name) -> Option<String> {
    let registry = try_get_threadlocal_lang_items()?;
    let name_segments: Vec<&str> = match name {
        Name::Ident(ident) => vec![ident.name.as_str()],
        Name::Path(path) => path.segments.iter().map(|seg| seg.name.as_str()).collect(),
        _ => return None,
    };

    for (op, path) in registry.ops {
        let path_segments: Vec<&str> = path.segments.iter().map(|seg| seg.name.as_str()).collect();
        if path_segments == name_segments {
            return Some(op);
        }
    }
    None
}

pub fn extract_intrinsic_item(attrs: &[Attribute]) -> Option<String> {
    extract_intrinsic_attribute(attrs)
}

fn collect_lang_items_from_items(
    items: &[Item],
    module_path: &mut Vec<Ident>,
    registry: &mut LangItemRegistry,
) {
    for item in items {
        match item.kind() {
            ItemKind::Module(module) => {
                module_path.push(module.name.clone());
                collect_lang_items_from_items(&module.items, module_path, registry);
                module_path.pop();
            }
            ItemKind::DefFunction(function) => {
                if let Some(lang_name) = extract_intrinsic_attribute(&function.attrs) {
                    let mut segments = module_path.clone();
                    segments.push(function.name.clone());
                    registry.insert(lang_name, Path::plain(segments));
                }
                if let Some(op_name) = extract_op_attribute(&function.attrs) {
                    let mut segments = module_path.clone();
                    segments.push(function.name.clone());
                    registry.insert_op(op_name, Path::plain(segments));
                }
            }
            _ => {}
        }
    }
}

fn extract_intrinsic_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_named_attribute(attrs, "intrinsic")
}

fn extract_op_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_named_attribute(attrs, "op")
}

fn extract_named_attribute(attrs: &[Attribute], name: &str) -> Option<String> {
    for attr in attrs {
        let AttrMeta::NameValue(meta) = &attr.meta else {
            continue;
        };
        if meta.name.last().as_str() != name {
            continue;
        }
        if let ExprKind::Value(value) = meta.value.kind() {
            if let Value::String(string) = &**value {
                return Some(string.value.clone());
            }
        }
    }
    None
}
