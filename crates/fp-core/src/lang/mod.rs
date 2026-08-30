use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{AttrMeta, Attribute, ExprKind, File, Ident, Item, ItemKind, Name, Path, Value};
use crate::intrinsics::{
    CallKind, PortableOp, PortableOpRegistry, lang_intrinsic_call_kind,
    lang_intrinsic_for_lang_item,
};

/// The central, canonical portable-op registry (formerly the closed
/// `OpKind` enum) — every language's own `#[op(...)]`/`@Op(...)` tag
/// resolves against this by name. See `PortableOpRegistry::builtin`'s doc
/// comment for the canonicalization convention.
fn central_registry() -> &'static PortableOpRegistry {
    static REGISTRY: std::sync::OnceLock<PortableOpRegistry> = std::sync::OnceLock::new();
    REGISTRY.get_or_init(PortableOpRegistry::builtin)
}

/// Translates a short, unqualified `#[op(method = "...")]`/
/// `#[op(variant = "...")]` tag using its enclosing declaration's own
/// `#[op(class = "...")]` name for context, into the central registry's
/// canonical flat name — so std source can write natural, short tags
/// (`#[op(method = "new")]` inside `#[op(class = "Vec")]`) instead of
/// inventing a globally-unique flat string per op. Checked before falling
/// back to a direct name match (for ops that are unambiguous without any
/// class context, and for flat, no-enclosing-declaration free functions).
fn class_and_member_to_canonical_name(class: &str, member: &str) -> Option<&'static str> {
    match (class, member) {
        ("Default", "default") => Some("default"),
        ("FromStr", "from_str") => Some("from_str"),
        ("Vec", "new") => Some("vec_new"),
        ("Vec", "from") => Some("vec_from"),
        ("Option", "some") => Some("option_some"),
        ("Option", "none") => Some("option_none"),
        ("Option", "unwrap") => Some("option_unwrap"),
        ("Option", "filter") => Some("option_filter"),
        ("Result", "ok") => Some("result_ok"),
        ("Result", "err") => Some("result_err"),
        ("Result", "map") => Some("result_map"),
        ("Result", "map_err") => Some("result_map_err"),
        ("Result", "is_ok") => Some("result_is_ok"),
        ("Result", "is_err") => Some("result_is_err"),
        ("Result", "ok_value") => Some("result_ok_value"),
        ("Result", "err_value") => Some("result_err_value"),
        ("Result", "unwrap") => Some("result_unwrap"),
        ("Result", "unwrap_or") => Some("result_unwrap_or"),
        ("IoError", "new") => Some("io_error_new"),
        ("Vec", "push") => Some("vec_push"),
        ("Vec", "extend") => Some("vec_extend"),
        ("Vec", "from_iter") => Some("vec_from_iter"),
        ("slice", "to_vec") => Some("slice_to_vec"),
        ("slice", "to_vec_in") => Some("slice_to_vec_in"),
        ("Option", "as_ref") => Some("as_ref"),
        ("Option", "unwrap_or") => Some("unwrap_or"),
        ("Option", "map_or") => Some("map_or"),
        ("Option", "iter") => Some("iter"),
        ("Option", "and_then") => Some("and_then"),
        ("Option", "clone") => Some("clone"),
        ("Option", "is_none") => Some("is_none"),
        ("Option", "as_deref") => Some("as_deref"),
        ("str", "trim_end") => Some("trim_end"),
        ("str", "trim_start") => Some("trim_start"),
        ("str", "trim") => Some("trim"),
        ("str", "split_whitespace") => Some("split_whitespace"),
        ("str", "split") => Some("split"),
        ("str", "lines") => Some("lines"),
        ("str", "starts_with") => Some("starts_with"),
        ("str", "ends_with") => Some("ends_with"),
        ("str", "parse") => Some("str_parse"),
        ("str", "char_indices") => Some("str_char_indices"),
        ("str", "split_at") => Some("str_split_at"),
        ("str", "strip_prefix") => Some("str_strip_prefix"),
        ("slice", "split_at") => Some("slice_split_at"),
        ("slice", "strip_prefix") => Some("slice_strip_prefix"),
        ("bool", "then_some") => Some("bool_then_some"),
        ("RangeInclusive", "contains") => Some("range_inclusive_contains"),
        ("String", "from_utf8_lossy") => Some("string_from_utf8_lossy"),
        ("String", "from_utf8") => Some("string_from_utf8"),
        ("File", "create") => Some("file_create"),
        ("Path", "canonicalize") => Some("path_canonicalize"),
        ("Path", "exists") => Some("path_exists"),
        ("Path", "parent") => Some("path_parent"),
        ("Path", "to_path_buf") => Some("path_to_path_buf"),
        ("Path", "join") => Some("path_join"),
        ("Path", "file_name") => Some("path_file_name"),
        ("Path", "to_string_lossy") => Some("path_to_string_lossy"),
        ("DirEntry", "path") => Some("dir_entry_path"),
        ("DirEntry", "file_type") => Some("dir_entry_file_type"),
        ("DirEntry", "file_name") => Some("dir_entry_file_name"),
        ("FileType", "is_dir") => Some("file_type_is_dir"),
        ("OsStr", "to_string_lossy") => Some("os_str_to_string_lossy"),
        ("slice", "join") => Some("slice_join"),
        ("Write", "write_all") => Some("write_all"),
        ("Option", "take") => Some("option_take"),
        ("Duration", "from_secs") => Some("duration_from_secs"),
        ("Duration", "from_millis") => Some("duration_from_millis"),
        ("Iterator", "position") => Some("position"),
        ("Iterator", "filter") => Some("filter"),
        ("Iterator", "collect") => Some("collect"),
        ("Iterator", "find_map") => Some("find_map"),
        ("char", "is_digit") => Some("char_is_digit"),
        ("char", "is_alphabetic") => Some("char_is_alphabetic"),
        ("char", "is_whitespace") => Some("char_is_whitespace"),
        ("char", "is_ascii_alphabetic") => Some("char_is_ascii_alphabetic"),
        ("char", "is_ascii_digit") => Some("char_is_ascii_digit"),
        ("char", "is_ascii_hexdigit") => Some("char_is_ascii_hexdigit"),
        ("Command", "new") => Some("command_new"),
        ("Command", "arg") => Some("command_arg"),
        ("Command", "args") => Some("command_args"),
        ("Command", "current_dir") => Some("command_current_dir"),
        ("Command", "stdin") => Some("command_stdin"),
        ("Command", "stdout") => Some("command_stdout"),
        ("Command", "stderr") => Some("command_stderr"),
        ("Command", "spawn") => Some("command_spawn"),
        ("Command", "output") => Some("command_output"),
        ("Command", "status") => Some("command_status"),
        ("Stdio", "piped") => Some("stdio_piped"),
        ("Stdio", "inherit") => Some("stdio_inherit"),
        ("Stdio", "null") => Some("stdio_null"),
        ("Child", "kill") => Some("child_kill"),
        ("Child", "wait") => Some("child_wait"),
        ("Child", "try_wait") => Some("child_try_wait"),
        ("Child", "wait_with_output") => Some("child_wait_with_output"),
        ("ExitStatus", "success") => Some("exit_status_success"),
        ("Process", "new") => Some("process_new"),
        ("Process", "shell") => Some("process_shell"),
        ("Process", "arg") => Some("process_arg"),
        ("Process", "args") => Some("process_args"),
        ("Process", "current_dir") => Some("process_current_dir"),
        ("Process", "run") => Some("process_run"),
        ("Process", "ok") => Some("process_ok"),
        ("Process", "output") => Some("process_output"),
        ("Process", "status") => Some("process_status"),
        ("TcpStream", "write") => Some("tcp_stream_write"),
        _ => None,
    }
}

/// Resolves a bare `#[op(func = "...")]`/`#[op(method = "...")]` tag string
/// directly against the central registry (no class context) — the public
/// entry point `ast_to_hir` uses for free-function/enum-variant tags. See
/// `class_and_member_to_portable_op` for the class-context version (impl
/// methods, where the tag alone may be ambiguous without knowing the
/// enclosing `#[op(class = "...")]`).
pub fn resolve_portable_op_tag(tag: &str) -> Option<PortableOp> {
    central_registry().resolve(tag)
}

/// Resolves a `#[op(method = "...")]` tag using its enclosing declaration's
/// `#[op(class = "...")]` name for context (see
/// `class_and_member_to_canonical_name`'s doc comment) — falls back to a
/// direct tag match for ops that are unambiguous without class context.
pub fn class_and_member_to_portable_op(class: &str, member: &str) -> Option<PortableOp> {
    let canonical: Option<String> = class_and_member_to_canonical_name(class, member)
        .map(str::to_string)
        .or_else(|| {
            central_registry()
                .contains(member)
                .then(|| member.to_string())
        });
    canonical.and_then(|name| central_registry().resolve(&name))
}

#[cfg(test)]
mod tests {
    use super::class_and_member_to_portable_op;

    #[test]
    fn resolves_collection_and_string_member_operations() {
        for (class, member, expected) in [
            ("Vec", "extend", "vec_extend"),
            ("Vec", "from", "vec_from"),
            ("Vec", "from_iter", "vec_from_iter"),
            ("Result", "unwrap", "result_unwrap"),
            ("Default", "default", "default"),
            ("FromStr", "from_str", "from_str"),
            ("IoError", "new", "io_error_new"),
            ("slice", "to_vec", "slice_to_vec"),
            ("slice", "to_vec_in", "slice_to_vec_in"),
            ("Iterator", "filter", "filter"),
            ("Iterator", "collect", "collect"),
            ("str", "lines", "lines"),
            ("str", "starts_with", "starts_with"),
            ("str", "ends_with", "ends_with"),
            ("str", "parse", "str_parse"),
            ("str", "char_indices", "str_char_indices"),
            ("str", "split_at", "str_split_at"),
            ("str", "strip_prefix", "str_strip_prefix"),
            ("slice", "split_at", "slice_split_at"),
            ("slice", "strip_prefix", "slice_strip_prefix"),
            ("bool", "then_some", "bool_then_some"),
            ("RangeInclusive", "contains", "range_inclusive_contains"),
            ("Path", "canonicalize", "path_canonicalize"),
            ("Path", "exists", "path_exists"),
            ("Path", "parent", "path_parent"),
            ("Path", "to_path_buf", "path_to_path_buf"),
            ("Path", "join", "path_join"),
            ("Path", "file_name", "path_file_name"),
            ("Path", "to_string_lossy", "path_to_string_lossy"),
            ("DirEntry", "path", "dir_entry_path"),
            ("DirEntry", "file_type", "dir_entry_file_type"),
            ("DirEntry", "file_name", "dir_entry_file_name"),
            ("FileType", "is_dir", "file_type_is_dir"),
            ("OsStr", "to_string_lossy", "os_str_to_string_lossy"),
            ("slice", "join", "slice_join"),
            ("Write", "write_all", "write_all"),
            ("Option", "take", "option_take"),
            ("Option", "filter", "option_filter"),
            ("char", "is_digit", "char_is_digit"),
            ("char", "is_alphabetic", "char_is_alphabetic"),
            ("char", "is_whitespace", "char_is_whitespace"),
            ("Duration", "from_secs", "duration_from_secs"),
            ("Duration", "from_millis", "duration_from_millis"),
            ("Child", "wait_with_output", "child_wait_with_output"),
            ("File", "create", "file_create"),
            ("Process", "new", "process_new"),
            ("Process", "shell", "process_shell"),
            ("Process", "status", "process_status"),
            ("TcpStream", "write", "tcp_stream_write"),
        ] {
            assert_eq!(
                class_and_member_to_portable_op(class, member)
                    .expect("registered portable operation")
                    .name(),
                expected
            );
        }
    }
}

#[derive(Clone, Default)]
pub struct LangItemRegistry {
    items: HashMap<String, Path>,
    /// Free-function portable ops, keyed by canonical name (resolved
    /// against the central registry — see `central_registry`) — the std
    /// source's own declared path for that op: no separate hardcoded
    /// name -> path table needed anywhere downstream, and no reverse
    /// mapping needed either, both lookup directions are a single
    /// `HashMap` op on this one field.
    ops: HashMap<String, Path>,
    /// Method-position portable ops, keyed by `"{opclass}.{opmethod}"` (e.g.
    /// `"Option.as_ref"`) — populated by scanning `impl` blocks tagged
    /// `#[op(class = "...")]` for methods tagged `#[op(method = "...")]`.
    /// Kept separate from `ops` (matched by the receiver's resolved type
    /// name, not a static call path).
    method_ops: HashMap<String, PortableOp>,
}

impl LangItemRegistry {
    pub fn insert(&mut self, name: impl Into<String>, path: Path) {
        self.items.insert(name.into(), path);
    }

    /// `tag` is the raw `#[op(func = "...")]` attribute value — resolved
    /// against the central registry here, once. Silently a no-op if the tag
    /// doesn't name a known op (e.g. a typo, or a tag reserved for future
    /// use) rather than storing an unusable string key — a real "unknown
    /// portable op" diagnostic belongs at a build-time self-check over the
    /// vendored std source, not here (see the central registry's own doc
    /// comment).
    pub fn insert_op(&mut self, tag: &str, path: Path) {
        if central_registry().contains(tag) {
            self.ops.insert(tag.to_string(), path);
        }
    }

    pub fn insert_method_op(&mut self, opclass: &str, opmethod: &str, op: PortableOp) {
        self.method_ops.insert(format!("{opclass}.{opmethod}"), op);
    }

    pub fn extend(&mut self, other: LangItemRegistry) {
        for (name, path) in other.items {
            self.items.insert(name, path);
        }
        for (name, path) in other.ops {
            self.ops.insert(name, path);
        }
        for (key, op) in other.method_ops {
            self.method_ops.insert(key, op);
        }
    }

    pub fn get_path(&self, name: &str) -> Option<&Path> {
        self.items.get(name)
    }

    /// The std source's own declared path for a free-function portable op
    /// (e.g. `"fs_read_dir"` -> `std::fs::read_dir`'s real path) — direct
    /// lookup, no reverse name mapping.
    pub fn get_op_path(&self, name: &str) -> Option<&Path> {
        self.ops.get(name)
    }

    /// Finds which (if any) registered free-function op's declared path
    /// matches `segments` exactly — the call-site direction (used by
    /// `PortableOpResolver::resolve_call_op`).
    pub fn find_op_by_call_segments(&self, segments: &[&str]) -> Option<PortableOp> {
        let name = self
            .ops
            .iter()
            .find(|(_, path)| {
                path.segments
                    .iter()
                    .map(|seg| seg.name.as_str())
                    .collect::<Vec<_>>()
                    == segments
            })
            .map(|(name, _)| name.clone())?;
        central_registry().resolve(&name)
    }

    /// Looks up a method-position portable op by the receiver's real type
    /// name and the method name being called — `"{opclass}.{opmethod}"`.
    pub fn get_method_op(&self, opclass: &str, opmethod: &str) -> Option<PortableOp> {
        self.method_ops
            .get(&format!("{opclass}.{opmethod}"))
            .cloned()
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
                        // The tag might already spell the canonical name
                        // directly (unambiguous ops, e.g. `as_ref`), or need
                        // class context to disambiguate (e.g. `new` inside
                        // `Vec` -> `vec_new`) — try both.
                        let canonical = class_and_member_to_canonical_name(&opclass, &opmethod)
                            .or_else(|| {
                                central_registry()
                                    .contains(&opmethod)
                                    .then_some(opmethod.as_str())
                            });
                        if let Some(op) =
                            canonical.and_then(|name| central_registry().resolve(name))
                        {
                            registry.insert_method_op(&opclass, &opmethod, op);
                        }
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
                        let canonical = class_and_member_to_canonical_name(&opclass, &opmethod)
                            .or_else(|| {
                                central_registry()
                                    .contains(&opmethod)
                                    .then_some(opmethod.as_str())
                            });
                        if let Some(op) =
                            canonical.and_then(|name| central_registry().resolve(name))
                        {
                            registry.insert_method_op(&opclass, &opmethod, op);
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
