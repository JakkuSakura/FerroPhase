use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{AttrMeta, Attribute, ExprKind, File, Ident, Item, ItemKind, Name, Path, Value};
use crate::intrinsics::{CallKind, OpKind, lang_intrinsic_call_kind, lang_intrinsic_for_lang_item};

#[derive(Clone, Default)]
pub struct LangItemRegistry {
    items: HashMap<String, Path>,
    /// Free-function portable ops, keyed by `OpKind` directly (not by the
    /// raw `#[op(func = "...")]` tag string) — the tag string is converted
    /// to its `OpKind` exactly once, at scan time
    /// (`collect_lang_items_from_items`), via `OpKind::from_op_tag`. This is
    /// the std source's own declared path for that op: no separate
    /// hardcoded `OpKind -> path` table needed anywhere downstream (see
    /// `PortableOpResolver::resolve_call_op`, and the retired
    /// `compile_mode_std_path`), and no reverse `OpKind -> tag` mapping
    /// needed either — both lookup directions are a single `HashMap` op
    /// on this one field.
    ops: HashMap<OpKind, Path>,
    /// Method-position portable ops, keyed by `"{opclass}.{opmethod}"` (e.g.
    /// `"Option.as_ref"`) — populated by scanning `impl` blocks tagged
    /// `#[op(class = "...")]` for methods tagged `#[op(method = "...")]`.
    /// Kept separate from `ops` (matched by the receiver's resolved type
    /// name, not a static call path).
    method_ops: HashMap<String, OpKind>,
}

impl LangItemRegistry {
    pub fn insert(&mut self, name: impl Into<String>, path: Path) {
        self.items.insert(name.into(), path);
    }

    /// `tag` is the raw `#[op(func = "...")]` attribute value — converted to
    /// its `OpKind` here, once, via `OpKind::from_op_tag`. Silently a no-op
    /// if the tag doesn't name a known op (e.g. a typo, or a tag reserved
    /// for future use) rather than storing an unusable string key.
    pub fn insert_op(&mut self, tag: &str, path: Path) {
        if let Some(kind) = OpKind::from_op_tag(tag) {
            self.ops.insert(kind, path);
        }
    }

    pub fn insert_method_op(&mut self, opclass: &str, opmethod: &str, kind: OpKind) {
        self.method_ops.insert(format!("{opclass}.{opmethod}"), kind);
    }

    pub fn extend(&mut self, other: LangItemRegistry) {
        for (name, path) in other.items {
            self.items.insert(name, path);
        }
        for (kind, path) in other.ops {
            self.ops.insert(kind, path);
        }
        for (key, kind) in other.method_ops {
            self.method_ops.insert(key, kind);
        }
    }

    pub fn get_path(&self, name: &str) -> Option<&Path> {
        self.items.get(name)
    }

    /// The std source's own declared path for a free-function portable op
    /// (e.g. `OpKind::FsReadDir` -> `std::fs::read_dir`'s real path) — direct
    /// lookup, no reverse name mapping.
    pub fn get_op_path(&self, kind: OpKind) -> Option<&Path> {
        self.ops.get(&kind)
    }

    /// Finds which (if any) registered free-function op's declared path
    /// matches `segments` exactly — the call-site direction (used by
    /// `PortableOpResolver::resolve_call_op`).
    pub fn find_op_by_call_segments(&self, segments: &[&str]) -> Option<OpKind> {
        self.ops
            .iter()
            .find(|(_, path)| {
                path.segments.iter().map(|seg| seg.name.as_str()).collect::<Vec<_>>() == segments
            })
            .map(|(kind, _)| *kind)
    }

    /// Looks up a method-position portable op by the receiver's real type
    /// name and the method name being called — `"{opclass}.{opmethod}"`.
    pub fn get_method_op(&self, opclass: &str, opmethod: &str) -> Option<OpKind> {
        self.method_ops.get(&format!("{opclass}.{opmethod}")).copied()
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

/// Scans one already-loaded package item (as returned by a `PackageProvider`,
/// with no enclosing `File`) for `#[intrinsic = "..."]`/`#[op(...)]` markers,
/// same as `collect_lang_items` but for a single item rather than a whole
/// file — lets a package-source-level pass (e.g. a native materializer's
/// provider wrapper) accumulate one registry across every item a package
/// yields, one at a time, before merging with `LangItemRegistry::extend`.
pub fn collect_lang_items_from_item(item: &Item) -> LangItemRegistry {
    let mut registry = LangItemRegistry::default();
    let mut module_path = Vec::new();
    collect_lang_items_from_items(std::slice::from_ref(item), &mut module_path, &mut registry);
    registry
}

pub fn lookup_intrinsic(name: &Name) -> Option<CallKind> {
    let name = lookup_intrinsic_name(name)?;
    lang_intrinsic_for_lang_item(&name)
        .and_then(lang_intrinsic_call_kind)
        .and_then(|kind| kind.intrinsic_kind().map(CallKind::from))
}

pub fn lookup_op_intrinsic(name: &Name) -> Option<CallKind> {
    let registry = try_get_threadlocal_lang_items()?;
    let name_segments: Vec<&str> = match name {
        Name::Ident(ident) => vec![ident.name.as_str()],
        Name::Path(path) => path.segments.iter().map(|seg| seg.name.as_str()).collect(),
        _ => return None,
    };
    registry
        .find_op_by_call_segments(&name_segments)
        .map(CallKind::Op)
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
                if let Some(op_name) = extract_opfunc_attribute(&function.attrs) {
                    let mut segments = module_path.clone();
                    segments.push(function.name.clone());
                    registry.insert_op(&op_name, Path::plain(segments));
                }
            }
            ItemKind::Impl(impl_block) => {
                // Method-position portable ops: an `impl` block tagged
                // `#[op(class = "Foo")]` with methods tagged
                // `#[op(method = "bar")]` registers under the lookup key
                // `"Foo.bar"` — the receiver's real resolved type name (its
                // own def-path segment, expected to match the `class`
                // value) plus the method name being called, looked up at
                // HIR-to-AST lift time (post-typecheck, see
                // `PortableOpResolver`). Not recursed into further —
                // methods only, no nested impls.
                if let Some(opclass) = extract_opclass_attribute(&impl_block.attrs) {
                    for member in &impl_block.items {
                        let ItemKind::DefFunction(function) = member.kind() else {
                            continue;
                        };
                        let Some(opmethod) = extract_opmethod_attribute(&function.attrs) else {
                            continue;
                        };
                        if let Some(kind) = OpKind::from_op_tag(&opmethod) {
                            registry.insert_method_op(&opclass, &opmethod, kind);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn extract_intrinsic_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_named_attribute(attrs, "intrinsic")
}

fn extract_opfunc_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_op_call_value(attrs, "func")
}

fn extract_opclass_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_op_call_value(attrs, "class")
}

fn extract_opmethod_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_op_call_value(attrs, "method")
}

/// Extracts `key`'s string value from a call-style `#[op(key = "value")]`
/// attribute (`#[op(class = "Foo")]`, `#[op(method = "bar")]`,
/// `#[op(func = "baz")]`) — the single, canonical portable-op marker
/// recognized across every declaration position (free function, impl
/// block, method).
fn extract_op_call_value(attrs: &[Attribute], key: &str) -> Option<String> {
    for attr in attrs {
        let AttrMeta::List(list) = &attr.meta else {
            continue;
        };
        if list.name.last().as_str() != "op" {
            continue;
        }
        for item in &list.items {
            let AttrMeta::NameValue(meta) = item else {
                continue;
            };
            if meta.name.last().as_str() != key {
                continue;
            }
            if let ExprKind::Value(value) = meta.value.kind() {
                if let Value::String(string) = &**value {
                    return Some(string.value.clone());
                }
            }
        }
    }
    None
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
