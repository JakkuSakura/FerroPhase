use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{
    AttrMeta, Attribute, ExprKind, Ident, Item, ItemKind, Name, Node, Path, Value,
};
use crate::intrinsics::IntrinsicCallKind;

#[derive(Clone, Default)]
pub struct LangItemRegistry {
    items: HashMap<String, Path>,
}

impl LangItemRegistry {
    pub fn insert(&mut self, name: impl Into<String>, path: Path) {
        self.items.insert(name.into(), path);
    }

    pub fn extend(&mut self, other: LangItemRegistry) {
        for (name, path) in other.items {
            self.items.insert(name, path);
        }
    }

    pub fn get_path(&self, name: &str) -> Option<&Path> {
        self.items.get(name)
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

pub fn collect_lang_items(node: &Node) -> LangItemRegistry {
    let mut registry = LangItemRegistry::default();
    let mut module_path = Vec::new();
    match node.kind() {
        crate::ast::NodeKind::File(file) => {
            collect_lang_items_from_items(&file.items, &mut module_path, &mut registry);
        }
        crate::ast::NodeKind::Item(item) => {
            collect_lang_items_from_items(
                std::slice::from_ref(item),
                &mut module_path,
                &mut registry,
            );
        }
        crate::ast::NodeKind::Expr(_)
        | crate::ast::NodeKind::Query(_)
        | crate::ast::NodeKind::Schema(_)
        | crate::ast::NodeKind::Workspace(_) => {}
    }
    registry
}

pub fn lookup_lang_item_intrinsic(locator: &Name) -> Option<IntrinsicCallKind> {
    let registry = try_get_threadlocal_lang_items()?;
    let locator_segments: Vec<&str> = match locator {
        Name::Ident(ident) => vec![ident.name.as_str()],
        Name::Path(path) => path.segments.iter().map(|seg| seg.name.as_str()).collect(),
        _ => return None,
    };

    for (name, path) in registry.items {
        let path_segments: Vec<&str> = path.segments.iter().map(|seg| seg.name.as_str()).collect();
        if path_segments == locator_segments {
            if let Some(kind) = intrinsic_kind_for_lang_item(&name) {
                return Some(kind);
            }
        }
    }
    None
}

fn intrinsic_kind_for_lang_item(name: &str) -> Option<IntrinsicCallKind> {
    match name {
        "time_now" => Some(IntrinsicCallKind::TimeNow),
        "create_struct" => Some(IntrinsicCallKind::CreateStruct),
        "addfield" => Some(IntrinsicCallKind::AddField),
        _ => None,
    }
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
                if let Some(lang_name) = extract_lang_attribute(&function.attrs) {
                    let mut segments = module_path.clone();
                    segments.push(function.name.clone());
                    registry.insert(lang_name, Path::new(segments));
                }
            }
            _ => {}
        }
    }
}

fn extract_lang_attribute(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        let AttrMeta::NameValue(meta) = &attr.meta else {
            continue;
        };
        if meta.name.last().as_str() != "lang" {
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
