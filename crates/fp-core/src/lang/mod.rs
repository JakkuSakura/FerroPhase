use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{AttrMeta, Attribute, ExprKind, File, Ident, Item, ItemKind, Name, Path, Value};
use crate::intrinsics::{
    ArityShape, CallKind, PortableOp, ResultTypeRule, lang_intrinsic_call_kind,
    lang_intrinsic_for_lang_item,
};

/// The declaration key for a method or enum-variant operation. Function
/// operations use their attribute value verbatim; member operations retain
/// the declaring class so `Path.exists` and `path_exists` remain distinct.
pub fn member_operation_key(class: &str, member: &str) -> String {
    format!("{class}.{member}")
}

#[cfg(test)]
mod registry_tests {
    use super::{LangItemRegistry, OperationSelector, ResultTypeRule};
    use crate::ast::{Ident, Path};

    #[test]
    fn attributes_define_exact_function_and_member_keys() {
        let mut registry = LangItemRegistry::default();
        registry.insert_op("path_exists", Path::plain(vec![Ident::new("path_exists")]));
        registry.insert_method_declaration(
            "Path",
            "exists",
            1,
            ResultTypeRule::AlwaysBool,
            Path::plain(vec![Ident::new("Path"), Ident::new("exists")]),
        );

        assert!(
            registry
                .resolve_operation(OperationSelector::DeclarationKey("path_exists"))
                .is_some()
        );
        assert!(
            registry
                .resolve_operation(OperationSelector::DeclarationKey("Path.exists"))
                .is_some()
        );
        assert!(
            registry
                .resolve_operation(OperationSelector::DeclarationKey("exists"))
                .is_none()
        );
    }
}

#[derive(Clone, Debug, Default)]
pub struct LangItemRegistry {
    items: HashMap<String, Path>,
    /// All portable operations, keyed by the declaration's exact source
    /// representation: a function tag (`path_exists`) or a qualified member
    /// tag (`Path.exists`, including variants). The canonical operation name
    /// lives in the binding and is used for cross-language interchange.
    operations: HashMap<String, PortableOpBinding>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableOpBinding {
    pub op: PortableOp,
    pub path: Path,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperationSelector<'a> {
    DeclarationKey(&'a str),
    PortableName(&'a str),
}

impl LangItemRegistry {
    pub fn insert(&mut self, name: impl Into<String>, path: Path) {
        self.items.insert(name.into(), path);
    }

    pub fn insert_operation(&mut self, key: impl Into<String>, op: PortableOp, path: Path) {
        self.operations
            .insert(key.into(), PortableOpBinding { op, path });
    }

    pub fn insert_op(&mut self, tag: &str, path: Path) {
        self.insert_operation(
            tag,
            PortableOp::new(
                tag,
                ArityShape {
                    receiver: false,
                    min_args: 0,
                },
                ResultTypeRule::NotStaticallyKnowable,
            ),
            path,
        );
    }

    pub fn insert_method_op(&mut self, opclass: &str, opmethod: &str, op: PortableOp, path: Path) {
        self.insert_operation(member_operation_key(opclass, opmethod), op, path);
    }

    /// Registers an operation declaration from its source attribute. The
    /// declaration registry owns operation construction; no core-wide list
    /// of known operation names is consulted.
    pub fn insert_method_declaration(
        &mut self,
        opclass: &str,
        opmethod: &str,
        min_args: usize,
        result_rule: ResultTypeRule,
        path: Path,
    ) {
        let key = member_operation_key(opclass, opmethod);
        self.insert_operation(
            key.clone(),
            PortableOp::new(
                key,
                ArityShape {
                    receiver: true,
                    min_args,
                },
                result_rule,
            ),
            path,
        );
    }

    /// Registers an enum-variant operation declaration. Variants use the
    /// same exact member key as methods, but their operation call has no
    /// receiver and carries its payload as the first argument.
    pub fn insert_variant_declaration(&mut self, opclass: &str, opvariant: &str, path: Path) {
        let key = member_operation_key(opclass, opvariant);
        self.insert_operation(
            key.clone(),
            PortableOp::new(
                key,
                ArityShape {
                    receiver: false,
                    min_args: 1,
                },
                ResultTypeRule::NotStaticallyKnowable,
            ),
            path,
        );
    }

    pub fn resolve(&self, key: &str) -> Option<PortableOp> {
        self.operations.get(key).map(|binding| binding.op.clone())
    }

    pub fn extend(&mut self, other: LangItemRegistry) {
        for (name, path) in other.items {
            self.items.insert(name, path);
        }
        for (key, binding) in other.operations {
            self.operations.insert(key, binding);
        }
    }

    pub fn get_path(&self, name: &str) -> Option<&Path> {
        self.items.get(name)
    }

    pub fn resolve_operation(&self, selector: OperationSelector<'_>) -> Option<&PortableOpBinding> {
        match selector {
            OperationSelector::DeclarationKey(key) => self.operations.get(key),
            OperationSelector::PortableName(name) => self
                .operations
                .values()
                .find(|binding| binding.op.name() == name),
        }
    }

    pub fn get_op_path(&self, name: &str) -> Option<&Path> {
        self.resolve_operation(OperationSelector::PortableName(name))
            .map(|binding| &binding.path)
    }

    /// Finds which (if any) registered free-function op's declared path
    /// matches `segments` exactly — the call-site direction (used by
    /// `PortableOpResolver::resolve_call_op`).
    pub fn find_op_by_call_segments(&self, segments: &[&str]) -> Option<PortableOp> {
        let name = self
            .operations
            .iter()
            .find(|(_, binding)| {
                binding
                    .path
                    .segments
                    .iter()
                    .map(|seg| seg.ident.as_str())
                    .collect::<Vec<_>>()
                    == segments
            })
            .map(|(_, binding)| binding.op.clone())?;
        Some(name)
    }

    /// Looks up a method-position portable op by the receiver's real type
    /// name and the method name being called — `"{opclass}.{opmethod}"`.
    pub fn get_method_op(&self, opclass: &str, opmethod: &str) -> Option<PortableOp> {
        self.resolve_operation(OperationSelector::DeclarationKey(&format!(
            "{opclass}.{opmethod}"
        )))
        .map(|binding| binding.op.clone())
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
    lang_intrinsic_for_lang_item(&name).and_then(lang_intrinsic_call_kind)
}

pub fn lookup_intrinsic_name(name: &Name) -> Option<String> {
    let registry = try_get_threadlocal_lang_items()?;
    let name_segments: Vec<&str> = match name {
        Name::Ident(ident) => vec![ident.name.as_str()],
        Name::Path(path) => path
            .segments
            .iter()
            .map(|seg| seg.ident.name.as_str())
            .collect(),
        _ => return None,
    };

    for (name, path) in registry.items {
        let path_segments: Vec<&str> = path
            .segments
            .iter()
            .map(|seg| seg.ident.name.as_str())
            .collect();
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
            ItemKind::DefEnum(def_enum) => {
                let Some(opclass) = extract_opclass_attribute(&def_enum.attrs) else {
                    continue;
                };
                for variant in &def_enum.value.variants {
                    let Some(opvariant) = extract_opvariant_attribute(&variant.attrs) else {
                        continue;
                    };
                    let mut segments = module_path.clone();
                    segments.push(def_enum.name.clone());
                    segments.push(variant.name.clone());
                    registry.insert_variant_declaration(
                        &opclass,
                        &opvariant,
                        Path::plain(segments),
                    );
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
                        registry.insert_method_declaration(
                            &opclass,
                            &opmethod,
                            function.sig.params.len() + 1,
                            ResultTypeRule::NotStaticallyKnowable,
                            Path::plain(
                                module_path
                                    .iter()
                                    .cloned()
                                    .chain(std::iter::once(Ident::new(opmethod.clone())))
                                    .collect(),
                            ),
                        );
                    }
                }
            }
            ItemKind::DefTrait(def_trait) => {
                // Same shape as `Impl` above, but for trait default methods
                // (e.g. `Iterator::position`) — the trait's own declaration
                // carries `#[op(class = "...")]`, not an enclosing `impl`.
                if let Some(opclass) = extract_opclass_attribute(&def_trait.attrs) {
                    for member in &def_trait.items {
                        let ItemKind::DefFunction(function) = member.kind() else {
                            continue;
                        };
                        let Some(opmethod) = extract_opmethod_attribute(&function.attrs) else {
                            continue;
                        };
                        registry.insert_method_declaration(
                            &opclass,
                            &opmethod,
                            function.sig.params.len() + 1,
                            ResultTypeRule::NotStaticallyKnowable,
                            Path::plain(
                                module_path
                                    .iter()
                                    .cloned()
                                    .chain(std::iter::once(Ident::new(opmethod.clone())))
                                    .collect(),
                            ),
                        );
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

fn extract_opvariant_attribute(attrs: &[Attribute]) -> Option<String> {
    extract_op_call_value(attrs, "variant")
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
