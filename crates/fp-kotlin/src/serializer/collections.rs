use super::*;

pub fn collect_mutated_field_names<'a>(
    items: impl Iterator<Item = &'a PackageItem>,
) -> HashSet<String> {
    let mut out = HashSet::new();
    for pkg_item in items {
        collect_mutated_fields_in_item(&pkg_item.item, &mut out);
    }
    out
}

fn collect_mutated_fields_in_item(item: &Item, out: &mut HashSet<String>) {
    match item.kind() {
        ItemKind::DefFunction(f) => collect_mutated_fields_in_stmts(&f.body.stmts, out),
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_mutated_fields_in_item(item, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_mutated_fields_in_item(item, out);
            }
        }
        _ => {}
    }
}

/// Workspace-wide field name -> Kotlin element type, for every struct field
/// whose Kotlin type is `List<T>`/`MutableList<T>` — the same information
/// `emit_struct` records into `KotlinEmitter.field_element_types`, but
/// collected across every package up front rather than per-file. A field can
/// be *declared* in one package's struct and *used* from another (e.g.
/// `FileChange.hunks: Vec<Hunk>` declared in `skln-core`, read via `.len()`
/// from `skln-git`) — the consuming file's own emitter never sees that
/// struct's definition, so `.len()`/range-indexing disambiguation
/// (`is_known_list_receiver`) would otherwise silently fall back to the
/// `String` case for any cross-package field. Each `KotlinEmitter` seeds its
/// own `field_element_types` from this before adding its file-local names
/// (locals/params never need cross-file visibility, so those stay per-file).
pub fn collect_list_field_names<'a>(
    items: impl Iterator<Item = &'a PackageItem>,
) -> HashMap<String, String> {
    let mut out = HashMap::new();
    let scratch = KotlinEmitter::new();
    for pkg_item in items {
        collect_list_fields_in_item(&pkg_item.item, &scratch, &mut out);
    }
    out
}

/// Workspace-wide struct field names whose Kotlin type is `String` — same
/// shape and rationale as `collect_list_field_names`, backing the
/// `.clone()`-vs-`.copy()` disambiguation (see `string_field_names`'s doc
/// comment).
pub fn collect_string_field_names<'a>(
    items: impl Iterator<Item = &'a PackageItem>,
) -> HashSet<String> {
    let mut out = HashSet::new();
    let scratch = KotlinEmitter::new();
    for pkg_item in items {
        collect_string_fields_in_item(&pkg_item.item, &scratch, &mut out);
    }
    out
}

fn collect_string_fields_in_item(item: &Item, e: &KotlinEmitter, out: &mut HashSet<String>) {
    match item.kind() {
        ItemKind::DefStruct(s) => {
            for field in &s.value.fields {
                if e.kotlin_type_from_ty(&field.value) == "String" {
                    out.insert(field.name.name.clone());
                }
            }
        }
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_string_fields_in_item(item, e, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_string_fields_in_item(item, e, out);
            }
        }
        _ => {}
    }
}

/// Workspace-wide struct field names whose declared type is an `enum class`
/// — same shape and rationale as `collect_string_field_names`, backing the
/// `.clone()`-vs-`.copy()` disambiguation (see `enum_field_names`'s doc
/// comment). A field referencing a type declared in a *different* package
/// (e.g. `FileChange.status: FileStatus`, declared in a sibling crate)
/// never actually shows up as `Ty::Enum(_)` here — the frontend can't tell
/// a cross-package struct reference from an enum reference by shape alone
/// before typecheck runs, so both come through as the same generic named-
/// type-reference shape (`Ty::Expr`). Resolve those by name instead: first
/// collect every enum's own name from any `ItemKind::DefEnum` anywhere in
/// the workspace, then check each field's referenced name against that set.
pub fn collect_enum_field_names<'a>(
    items: impl Iterator<Item = &'a PackageItem>,
) -> HashSet<String> {
    let items: Vec<&PackageItem> = items.collect();
    let mut enum_type_names = HashSet::new();
    for pkg_item in &items {
        collect_enum_type_names_in_item(&pkg_item.item, &mut enum_type_names);
    }
    let mut out = HashSet::new();
    for pkg_item in &items {
        collect_enum_fields_in_item(&pkg_item.item, &enum_type_names, &mut out);
    }
    out
}

fn collect_enum_type_names_in_item(item: &Item, out: &mut HashSet<String>) {
    match item.kind() {
        ItemKind::DefEnum(e) => {
            out.insert(e.name.name.clone());
        }
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_enum_type_names_in_item(item, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_enum_type_names_in_item(item, out);
            }
        }
        _ => {}
    }
}

fn field_type_is_enum(ty: &Ty, enum_type_names: &HashSet<String>) -> bool {
    match ty {
        Ty::Enum(_) => true,
        Ty::Expr(expr) => {
            let name = expr_to_name(expr);
            let bare = name.rsplit('.').next().unwrap_or(&name);
            enum_type_names.contains(bare)
        }
        _ => false,
    }
}

fn collect_enum_fields_in_item(
    item: &Item,
    enum_type_names: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    match item.kind() {
        ItemKind::DefStruct(s) => {
            for field in &s.value.fields {
                if field_type_is_enum(&field.value, enum_type_names) {
                    out.insert(field.name.name.clone());
                }
            }
        }
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_enum_fields_in_item(item, enum_type_names, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_enum_fields_in_item(item, enum_type_names, out);
            }
        }
        _ => {}
    }
}

/// Workspace-wide `enum_name -> (rust_variant_name -> real_kotlin_variant_name)`
/// registry, built once from every `DefEnum`'s own definition — the single
/// source of truth `emit_enum` itself uses to name a variant's Kotlin
/// sealed-subclass (faithfully, i.e. unchanged from the Rust source name —
/// see `emit_enum`'s doc comment for why). A match arm
/// referencing that variant (`render_match_pat`) looks itself up here
/// instead of re-deriving the name by string-manipulating its own
/// (source-side, possibly differently-qualified) pattern text — the same
/// "provider registers once, consumer looks up by identity" shape used
/// for portable ops (`fp_core::lang::class_and_member_to_portable_op`), just keyed by this
/// compile's own enum/variant *names* (unambiguous within one compile)
/// rather than a cross-package `DefId` (which nothing on the `Pattern`/
/// `Item` AST types carries — see `HirToAstLifter::lift_path`, which
/// drops `hir::Path::res` other than for renamed locals).
pub fn collect_enum_variant_names<'a>(
    items: impl Iterator<Item = &'a PackageItem>,
) -> HashMap<String, HashMap<String, String>> {
    let mut out = HashMap::new();
    for pkg_item in items {
        collect_enum_variant_names_in_item(&pkg_item.item, &mut out);
    }
    out
}

fn collect_enum_variant_names_in_item(
    item: &Item,
    out: &mut HashMap<String, HashMap<String, String>>,
) {
    match item.kind() {
        ItemKind::DefEnum(en) => {
            let variants = en
                .value
                .variants
                .iter()
                .map(|v| (v.name.name.clone(), v.name.name.clone()))
                .collect();
            out.insert(en.name.name.clone(), variants);
        }
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_enum_variant_names_in_item(item, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_enum_variant_names_in_item(item, out);
            }
        }
        _ => {}
    }
}

/// If `kt` is a `List<T>`/`MutableList<T>`/`Set<T>`/`MutableSet<T>` Kotlin
/// type string, returns `T` — these are exactly the collection kinds that
/// use `.size` (not `.length`) and support `.subList`/range-slicing the same
/// way, which is what every caller of this helper cares about.
pub(super) fn sized_collection_element_type(kt: &str) -> Option<&str> {
    ["MutableList<", "List<", "MutableSet<", "Set<"]
        .iter()
        .find_map(|prefix| kt.strip_prefix(prefix))
        .and_then(|s| s.strip_suffix('>'))
}

fn collect_list_fields_in_item(item: &Item, e: &KotlinEmitter, out: &mut HashMap<String, String>) {
    match item.kind() {
        ItemKind::DefStruct(s) => {
            for field in &s.value.fields {
                let kt = e.kotlin_type_from_ty(&field.value);
                if let Some(elem) = sized_collection_element_type(&kt) {
                    out.insert(field.name.name.clone(), elem.to_string());
                }
            }
        }
        ItemKind::Impl(impl_block) => {
            for item in &impl_block.items {
                collect_list_fields_in_item(item, e, out);
            }
        }
        ItemKind::Module(m) => {
            for item in &m.items {
                collect_list_fields_in_item(item, e, out);
            }
        }
        _ => {}
    }
}

fn collect_mutated_fields_in_stmts(stmts: &[BlockStmt], out: &mut HashSet<String>) {
    for stmt in stmts {
        match stmt {
            BlockStmt::Expr(e) => collect_mutated_fields_in_expr(&e.expr, out),
            BlockStmt::Let(l) => {
                if let Some(init) = &l.init {
                    collect_mutated_fields_in_expr(init, out);
                }
            }
            _ => {}
        }
    }
}

fn collect_mutated_fields_in_expr(expr: &Expr, out: &mut HashSet<String>) {
    match expr.kind() {
        ExprKind::Assign(a) => {
            if let ExprKind::Select(sel) = a.target.kind() {
                out.insert(sel.field.name.to_string());
            }
            collect_mutated_fields_in_expr(&a.target, out);
            collect_mutated_fields_in_expr(&a.value, out);
        }
        ExprKind::Block(b) => collect_mutated_fields_in_stmts(&b.stmts, out),
        ExprKind::If(i) => {
            collect_mutated_fields_in_expr(&i.cond, out);
            collect_mutated_fields_in_expr(&i.then, out);
            if let Some(elze) = &i.elze {
                collect_mutated_fields_in_expr(elze, out);
            }
        }
        ExprKind::Loop(l) => collect_mutated_fields_in_expr(&l.body, out),
        ExprKind::While(w) => {
            collect_mutated_fields_in_expr(&w.cond, out);
            collect_mutated_fields_in_expr(&w.body, out);
        }
        ExprKind::For(f) => {
            collect_mutated_fields_in_expr(&f.iter, out);
            collect_mutated_fields_in_expr(&f.body, out);
        }
        ExprKind::Match(m) => {
            if let Some(scrutinee) = &m.scrutinee {
                collect_mutated_fields_in_expr(scrutinee, out);
            }
            for case in &m.cases {
                if let Some(guard) = &case.guard {
                    collect_mutated_fields_in_expr(guard, out);
                }
                collect_mutated_fields_in_expr(&case.body, out);
            }
        }
        ExprKind::Invoke(inv) => {
            for arg in &inv.args {
                collect_mutated_fields_in_expr(arg, out);
            }
        }
        ExprKind::BinOp(b) => {
            collect_mutated_fields_in_expr(&b.lhs, out);
            collect_mutated_fields_in_expr(&b.rhs, out);
        }
        ExprKind::UnOp(u) => collect_mutated_fields_in_expr(&u.val, out),
        ExprKind::Select(s) => collect_mutated_fields_in_expr(&s.obj, out),
        ExprKind::Index(idx) => {
            collect_mutated_fields_in_expr(&idx.obj, out);
            collect_mutated_fields_in_expr(&idx.index, out);
        }
        ExprKind::Reference(r) => collect_mutated_fields_in_expr(&r.referee, out),
        ExprKind::Dereference(d) => collect_mutated_fields_in_expr(&d.referee, out),
        ExprKind::Cast(c) => collect_mutated_fields_in_expr(&c.expr, out),
        ExprKind::Paren(p) => collect_mutated_fields_in_expr(&p.expr, out),
        ExprKind::Try(t) => collect_mutated_fields_in_expr(&t.expr, out),
        ExprKind::Let(l) => collect_mutated_fields_in_expr(&l.expr, out),
        ExprKind::Return(r) => {
            if let Some(v) = &r.value {
                collect_mutated_fields_in_expr(v, out);
            }
        }
        ExprKind::Closure(c) => collect_mutated_fields_in_expr(&c.body, out),
        ExprKind::Tuple(t) => {
            for v in &t.values {
                collect_mutated_fields_in_expr(v, out);
            }
        }
        ExprKind::Array(a) => {
            for v in &a.values {
                collect_mutated_fields_in_expr(v, out);
            }
        }
        ExprKind::Struct(s) => {
            for f in &s.fields {
                if let Some(v) = &f.value {
                    collect_mutated_fields_in_expr(v, out);
                }
            }
        }
        _ => {}
    }
}
