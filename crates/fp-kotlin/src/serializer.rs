use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write as _;

use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprKind, File, Item,
    ItemDefEnum, ItemDefFunction, ItemDefStruct, ItemImport, ItemKind, ItemDefConst,
    Ty, TypeInt, TypePrimitive, StructuralField, TySlot,
    EnumTypeVariant, FunctionParam, FormatTemplatePart, FormatArgRef,
    Value, ExprInvokeTarget,
    StmtLet, BExpr, Pattern, PatternKind,
};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::intrinsics::calls::{CallKind, KnownClass, KnownPackage, OpKind};
use fp_core::package::{PackageItem, PackageSource};
use eyre::{bail, Result};

// ── Emitter context ──────────────────────────────────────────────────────────

struct KotlinEmitter {
    code: String,
    indent: usize,
    var_counter: usize,
    stub_names: HashSet<String>,
    /// Names of sibling modules generated into the same (default) Kotlin package —
    /// imports targeting these are skipped since they're already visible.
    local_modules: HashSet<String>,
    /// Names of sibling packages in this compile's workspace (e.g. other Cargo
    /// crates in the same `magnet transpile` run) — imports of these are
    /// skipped for the same reason as `local_modules`: every generated package
    /// lives in its own default (unnamed) Kotlin package with no import path.
    workspace_packages: HashSet<String>,
    /// Workspace-wide struct field names ever assigned to (`x.field = ...`),
    /// computed by `collect_mutated_field_names` — a field defined in this
    /// file's struct might only ever be mutated from a different package
    /// (e.g. skln-core's `FileChange` mutated from skln-git's diff parser),
    /// so this has to be workspace-wide, not derived from this file alone.
    /// Used to decide `val` vs `var` when emitting a struct's fields.
    mutated_fields: HashSet<String>,
    /// Struct field names in this file whose Kotlin type is `Long` — used to append
    /// an `L` suffix to bare int literals compared against them (`assert_eq!` lifts
    /// each side of a comparison into its own `let`, so this is tracked across the
    /// paired `__fp_assert_left`/`__fp_assert_right` statements it generates).
    long_field_names: HashSet<String>,
    pending_assert_long: bool,
    /// Struct field name → Kotlin element type, for fields typed `List<T>`/
    /// `MutableList<T>` — used to type a `for` loop's variable when iterating
    /// that field directly (Rust `for` patterns can't carry an explicit type
    /// annotation the way closures can, so this is inferred instead).
    field_element_types: HashMap<String, String>,
    /// Variables bound from `.split(...)` — Kotlin's version returns a `List`, not
    /// a stateful iterator, so a later `.next()?` on one of these needs indexed
    /// access instead of the usual (erasing) method-call rendering.
    split_iter_vars: HashSet<String>,
    /// Per-variable consumed-index counter for the `split_iter_vars` rewrite.
    next_call_counters: HashMap<String, usize>,
    /// Stack of currently-open block scopes (only pushed around match-arm body
    /// rendering, where Rust's `let`-shadowing shows up) — used to detect a
    /// same-scope re-`let` of a name, which Kotlin doesn't allow as a flat
    /// re-declaration. Empty outside that context, so this is a no-op elsewhere.
    declared_names: Vec<HashSet<String>>,
}

impl KotlinEmitter {
    fn new() -> Self {
        Self {
            code: String::new(),
            indent: 0,
            var_counter: 0,
            stub_names: HashSet::new(),
            local_modules: HashSet::new(),
            workspace_packages: HashSet::new(),
            mutated_fields: HashSet::new(),
            long_field_names: HashSet::new(),
            pending_assert_long: false,
            split_iter_vars: HashSet::new(),
            next_call_counters: HashMap::new(),
            field_element_types: HashMap::new(),
            declared_names: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.declared_names.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.declared_names.pop();
    }

    /// True if `name` is already declared in any currently-open scope.
    fn is_declared(&self, name: &str) -> bool {
        self.declared_names.iter().any(|s| s.contains(name))
    }

    /// Record `name` as declared in the innermost currently-open scope. A no-op
    /// if no scope is open (i.e. outside match-arm body rendering).
    fn declare_name(&mut self, name: &str) {
        if let Some(top) = self.declared_names.last_mut() {
            top.insert(name.to_string());
        }
    }

    fn fresh_var(&mut self, base: &str) -> String {
        self.var_counter += 1;
        if self.var_counter == 1 { base.to_string() } else { format!("{}{}", base, self.var_counter) }
    }

    fn push_line(&mut self, line: &str) {
        for _ in 0..self.indent { self.code.push_str("    "); }
        self.code.push_str(line);
        self.code.push('\n');
    }

    fn push_str(&mut self, s: &str) { self.code.push_str(s); }
}

// ── Serializer entry ─────────────────────────────────────────────────────────

pub struct KotlinSerializer;

impl AstSerializer for KotlinSerializer {
    fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = KotlinEmitter::new();
        emit_file(file, &mut emitter)?;
        let mut out = String::from("// Generated by FerroPhase — Kotlin target\n\n");
        out.push_str(&emitter.code);
        Ok(out)
    }
}

impl KotlinSerializer {
    /// Serialize a package into per-module Kotlin files with Gradle manifest.
    /// `workspace_packages` is the full set of sibling package names in this
    /// compile (e.g. every other Cargo crate in the same `magnet transpile`
    /// run) — used to recognize cross-package imports within the workspace
    /// and to skip emitting an (unresolvable) Kotlin import for them.
    /// `mutated_fields` is the workspace-wide set of struct field names ever
    /// assigned to (`x.field = ...`) anywhere — see `collect_mutated_field_names`
    /// — used to decide `val` vs `var` when emitting a struct's fields.
    /// Returns `Vec<(relative_path, code)>` — source files + build files.
    pub fn serialize_package(
        &self,
        source: &PackageSource,
        workspace_packages: &HashSet<String>,
        mutated_fields: &HashSet<String>,
    ) -> Result<Vec<(String, String)>> {
        use std::collections::BTreeMap;
        let mut modules: BTreeMap<String, Vec<Item>> = BTreeMap::new();
        for pkg_item in &source.items {
            let key = pkg_item.path.segments.join("/");
            modules.entry(key).or_default().push(pkg_item.item.clone());
        }

        let pkg_name = &source.name;
        let mut files = Vec::new();

        // Collect cross-package dependencies from imports
        let deps = collect_workspace_deps(&source.items, pkg_name, workspace_packages);

        // Every generated file lives in the default (unnamed) Kotlin package, so
        // imports of sibling modules within this package are both unnecessary and
        // unresolvable (there's no package literally named e.g. "config").
        let local_modules: HashSet<String> = modules.keys()
            .map(|k| k.rsplit('/').next().unwrap_or(k).to_string())
            .collect();

        // Gradle manifest
        files.push(("settings.gradle.kts".into(), settings_gradle(pkg_name)));
        files.push(("build.gradle.kts".into(), build_gradle(pkg_name, &deps)));

        // Source files under src/main/kotlin/
        for (mod_path, items) in modules {
            let file = File {
                path: std::path::PathBuf::from(&mod_path),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items,
            };
            let mut emitter = KotlinEmitter::new();
            emitter.local_modules = local_modules.clone();
            emitter.workspace_packages = workspace_packages.clone();
            emitter.mutated_fields = mutated_fields.clone();
            emit_file(&file, &mut emitter)
                .map_err(|e| eyre::eyre!("serialize {}: {}", mod_path, e))?;
            let mut code = String::from("// Generated by FerroPhase — Kotlin target\n\n");
            code.push_str(&emitter.code);
            let out_path = format!("src/main/kotlin/{}.kt", mod_path);
            files.push((out_path, code));
        }
        Ok(files)
    }
}

fn settings_gradle(name: &str) -> String {
    format!("rootProject.name = \"{}\"\n", name.replace('-', "_"))
}

fn build_gradle(name: &str, deps: &[String]) -> String {
    let group = format!("com.{}", name.replace('-', "."));
    let dep_lines: String = deps.iter()
        .map(|d| {
            // Rust crate names use underscores, project dirs use hyphens
            let dir_name = d.replace('_', "-");
            format!("    implementation(project(\":{}\"))\n", dir_name)
        })
        .collect();
    format!(
        "plugins {{\n    kotlin(\"jvm\") version \"2.1.0\"\n}}\n\n\
         group = \"{}\"\n\
         version = \"0.1.0\"\n\n\
         repositories {{\n    mavenCentral()\n}}\n\n\
         dependencies {{\n    testImplementation(kotlin(\"test\"))\n{}}}\n\n\
         kotlin {{\n    jvmToolchain(21)\n}}\n",
        group, dep_lines,
    )
}

/// Struct field names ever assigned to (`x.field = ...`) anywhere across the
/// given items. Field-name-keyed rather than per-struct: the AST here carries
/// no resolved type info to tell which struct a given `Select` targets, and
/// (as with `long_field_names` elsewhere in this file) collisions between
/// unrelated structs sharing a field name haven't shown up in practice.
/// Callers pass the *workspace-wide* item set (every package, not just one),
/// since a field can be defined in one package and mutated through a `&mut`
/// reference in another.
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

fn collect_workspace_deps(
    items: &[PackageItem],
    pkg_name: &str,
    workspace_packages: &HashSet<String>,
) -> Vec<String> {
    use std::collections::BTreeSet;
    let mut deps = BTreeSet::new();
    for pkg_item in items {
        collect_deps_from_item(&pkg_item.item, &mut deps);
    }
    deps.into_iter()
        .filter(|d| {
            if d.as_str() == pkg_name { return false; }
            // Accept if it matches a sibling workspace package (with or without hyphens)
            workspace_packages.contains(d.as_str())
                || workspace_packages.contains(&d.replace('_', "-"))
        })
        .collect()
}

fn collect_deps_from_item(item: &Item, deps: &mut BTreeSet<String>) {
    use fp_core::ast::ItemKind;
    match item.kind() {
        ItemKind::Import(imp) => {
            let path = flatten_import_tree(&imp.tree);
            if !path.starts_with("std.") && !path.starts_with("serde")
                && !path.starts_with("winnow") && !path.starts_with('.')
                && !path.is_empty()
                && !path.starts_with("java")
                && !path.starts_with("thiserror")
                && !path.starts_with("tracing")
                && !path.starts_with("async_trait")
                && !path.starts_with("anyhow")
                && !path.starts_with("toml")
                && !path.starts_with("serde_json")
                && !path.starts_with("tokio")
            {
                let pkg = path.split('.').next().unwrap_or(&path);
                deps.insert(pkg.to_string());
            }
        }
        ItemKind::Module(m) => {
            for child in &m.items { collect_deps_from_item(child, deps); }
        }
        ItemKind::DefFunction(f) => {
            for stmt in &f.body.stmts { collect_deps_from_stmt(stmt, deps); }
        }
        _ => {}
    }
}

fn collect_deps_from_stmt(stmt: &BlockStmt, deps: &mut BTreeSet<String>) {
    match stmt {
        BlockStmt::Item(item) => collect_deps_from_item(item, deps),
        _ => {}
    }
}

fn emit_file(file: &File, e: &mut KotlinEmitter) -> Result<()> {
    let mut imports = Vec::new();
    let mut non_imports = Vec::new();
    for item in &file.items {
        if let ItemKind::Import(imp) = item.kind() { imports.push(imp.clone()); }
        else { non_imports.push(item); }
    }
    for imp in &imports { emit_import(imp, e)?; }
    if !imports.is_empty() { e.push_line(""); }
    for item in non_imports { emit_item(item, e)?; }
    Ok(())
}

// ── Items ────────────────────────────────────────────────────────────────────

fn emit_item(item: &Item, e: &mut KotlinEmitter) -> Result<()> {
    match item.kind() {
        ItemKind::DefStruct(s) => emit_struct(s, e),
        ItemKind::DefEnum(en) => emit_enum(en, e),
        ItemKind::DefFunction(f) => emit_function(f, e),
        ItemKind::Module(m) => {
            for child in &m.items { emit_item(child, e)?; }
            Ok(())
        }
        ItemKind::Import(imp) => emit_import(imp, e),
        ItemKind::DefConst(c) => {
            let name = c.name.name.as_str();
            let val = render_expr(&c.value, e)?;
            e.push_line(&format!("val {} = {}", name, val));
            Ok(())
        }
        ItemKind::DefTrait(_) | ItemKind::Macro(_) | ItemKind::DefStructural(_) => Ok(()),
        ItemKind::Impl(impl_block) => {
            let self_name = expr_to_name(&impl_block.self_ty);
            for item in &impl_block.items {
                if let ItemKind::DefFunction(f) = item.kind() {
                    if f.sig.receiver.is_none() {
                        // Static method → top-level stub
                        emit_impl_static_stub(f, &self_name, e)?;
                    }
                    // Instance methods with receiver → skip for now
                }
            }
            Ok(())
        }
        ItemKind::Expr(expr) => {
            if let ExprKind::Block(block) = expr.kind() {
                for stmt in &block.stmts { emit_stmt(stmt, e, false)?; }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

// ── Struct ───────────────────────────────────────────────────────────────────

fn emit_struct(s: &ItemDefStruct, e: &mut KotlinEmitter) -> Result<()> {
    let name = s.name.name.as_str();
    let fields = &s.value.fields;
    e.push_line(&format!("data class {}(", name));
    for (i, field) in fields.iter().enumerate() {
        let comma = if i < fields.len() - 1 { "," } else { "" };
        let kt = kotlin_type_from_ty(&field.value, e);
        if kt == "Long" {
            e.long_field_names.insert(field.name.name.clone());
        }
        if let Some(elem) = kt.strip_prefix("MutableList<").or_else(|| kt.strip_prefix("List<")).and_then(|s| s.strip_suffix('>')) {
            e.field_element_types.insert(field.name.name.clone(), elem.to_string());
        }
        let mutability = if e.mutated_fields.contains(&field.name.name) { "var" } else { "val" };
        e.push_line(&format!("    {} {}: {}{}", mutability, field.name.name, kt, comma));
    }
    e.push_line(")\n");
    Ok(())
}

// ── Enum ─────────────────────────────────────────────────────────────────────

fn emit_enum(en: &ItemDefEnum, e: &mut KotlinEmitter) -> Result<()> {
    let name = en.name.name.as_str();
    let variants = &en.value.variants;
    let has_data = variants.iter().any(|v| !matches!(v.value, Ty::Unit(_)));

    if has_data {
        e.push_line(&format!("sealed class {} {{", name));
        for (i, variant) in variants.iter().enumerate() {
            let vname = variant.name.name.to_uppercase();
            match &variant.value {
                Ty::Unit(_) | Ty::Nothing(_) => {
                    e.push_line(&format!("    object {} : {}()", vname, name));
                }
                Ty::Struct(s) => {
                    let fields: Vec<String> = s.fields.iter()
                        .map(|f| format!("val {}: {}", f.name.name, kotlin_type_from_ty(&f.value, e)))
                        .collect();
                    e.push_line(&format!("    data class {}({}) : {}()",
                        vname, fields.join(", "), name));
                }
                Ty::Structural(s) => {
                    let fields: Vec<String> = s.fields.iter()
                        .map(|f| format!("val {}: {}", f.name.name, kotlin_type_from_ty(&f.value, e)))
                        .collect();
                    e.push_line(&format!("    data class {}({}) : {}()",
                        vname, fields.join(", "), name));
                }
                Ty::Expr(expr) => {
                    let ty_str = kotlin_type_from_ty(&Ty::Expr(expr.clone()), e);
                    e.push_line(&format!("    data class {}(val __data: {}) : {}()",
                        vname, ty_str, name));
                }
                _ => {
                    e.push_line(&format!("    data class {}(vararg __data: Any?) : {}()", vname, name));
                }
            }
            if i < variants.len() - 1 { e.push_line(""); }
        }
        e.push_line("}\n");
    } else {
        e.push_line(&format!("enum class {} {{", name));
        for (i, variant) in variants.iter().enumerate() {
            let comma = if i < variants.len() - 1 { "," } else { "" };
            e.push_line(&format!("    {}{}", variant.name.name.to_uppercase(), comma));
        }
        e.push_line("}\n");
    }
    Ok(())
}

// ── Function ─────────────────────────────────────────────────────────────────

fn emit_impl_static_stub(f: &ItemDefFunction, self_name: &str, e: &mut KotlinEmitter) -> Result<()> {
    let name = f.name.name.as_str();
    if !e.stub_names.insert(name.to_string()) {
        return Ok(()); // already emitted
    }
    let params = f.sig.params.iter()
        .map(|p| format!("{}: {}", p.name.name, kotlin_type_from_ty(&p.ty, e)))
        .collect::<Vec<_>>().join(", ");
    let ret = f.sig.ret_ty.as_ref()
        .map(|ty| kotlin_type_from_ty(ty, e).replace("Self", self_name))
        .map(|ty| format!(": {}", ty))
        .unwrap_or_else(|| ": Unit".to_string());

    e.push_line(&format!("fun {}({}){} = throw NotImplementedError(\"impl stub for {}::{}\")",
        name, params, ret, self_name, name));
    e.push_line("");
    Ok(())
}

fn emit_impl_function(f: &ItemDefFunction, self_name: &str, e: &mut KotlinEmitter) -> Result<()> {
    let name = f.name.name.as_str();
    // Skip the first param (self) — Kotlin extension functions have implicit receiver
    let params = f.sig.params.iter()
        .map(|p| format!("{}: {}", p.name.name, kotlin_type_from_ty(&p.ty, e)))
        .collect::<Vec<_>>().join(", ");
    let ret = f.sig.ret_ty.as_ref()
        .map(|ty| format!(": {}", kotlin_type_from_ty(ty, e).replace("Self", self_name)))
        .unwrap_or_else(|| ": Unit".to_string());

    e.push_line(&format!("fun {}.{}({}){} {{", self_name, name, params, ret));
    e.indent += 1;
    if is_winnow_parser(&f.body.stmts) {
        e.push_line("throw NotImplementedError(\"parser function not transpilable\")");
    } else if is_async_tokio_fn(&f.body.stmts) {
        e.push_line("throw NotImplementedError(\"async function not transpilable\")");
    } else {
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let is_tail = i == len - 1 && f.sig.ret_ty.is_some();
            emit_stmt(stmt, e, is_tail)?;
        }
    }
    e.indent -= 1;
    e.push_line("}\n");
    Ok(())
}

fn emit_function(f: &ItemDefFunction, e: &mut KotlinEmitter) -> Result<()> {
    let name = f.name.name.as_str();
    let params = f.sig.params.iter()
        .map(|p| {
            let kt = kotlin_type_from_ty(&p.ty, e);
            // Same `.len()` vs `.size` tracking as `let`-bound locals (see
            // `field_element_types`'s doc comment) — a List-typed parameter
            // needs to be known by name too.
            if let Some(elem) = kt.strip_prefix("MutableList<").or_else(|| kt.strip_prefix("List<")).and_then(|s| s.strip_suffix('>')) {
                e.field_element_types.insert(p.name.name.clone(), elem.to_string());
            }
            format!("{}: {}", p.name.name, kt)
        })
        .collect::<Vec<_>>().join(", ");
    let ret = f.sig.ret_ty.as_ref()
        .map(|ty| format!(": {}", kotlin_type_from_ty(ty, e)))
        .unwrap_or_else(|| ": Unit".to_string());

    e.push_line(&format!("fun {}({}){} {{", name, params, ret));
    e.indent += 1;
    if is_winnow_parser(&f.body.stmts) {
        e.push_line("throw NotImplementedError(\"parser function not transpilable\")");
    } else if is_async_tokio_fn(&f.body.stmts) {
        e.push_line("throw NotImplementedError(\"async function not transpilable\")");
    } else {
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let is_tail = i == len - 1 && f.sig.ret_ty.is_some();
            emit_stmt(stmt, e, is_tail)?;
        }
    }
    e.indent -= 1;
    e.push_line("}\n");
    Ok(())
}

/// Detect async functions that use tokio/await/futures patterns.
fn is_async_tokio_fn(stmts: &[BlockStmt]) -> bool {
    for stmt in stmts {
        if stmt_contains_tokio(stmt) { return true; }
    }
    false
}

fn stmt_contains_tokio(stmt: &BlockStmt) -> bool {
    match stmt {
        BlockStmt::Expr(se) => expr_contains_tokio(&se.expr),
        BlockStmt::Let(l) => {
            if let Some(init) = &l.init { expr_contains_tokio(init) } else { false }
        }
        BlockStmt::Item(item) => item_contains_tokio(item),
        _ => false,
    }
}

fn item_contains_tokio(item: &Item) -> bool {
    match item.kind() {
        ItemKind::Import(imp) => {
            let path = flatten_import_tree(&imp.tree);
            path.starts_with("tokio") || path.starts_with("futures")
        }
        ItemKind::DefFunction(f) => is_async_tokio_fn(&f.body.stmts),
        _ => false,
    }
}

fn expr_contains_tokio(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Invoke(inv) => {
            let method = match &inv.target {
                ExprInvokeTarget::Method(sel) => sel.field.name.as_str().to_string(),
                ExprInvokeTarget::Function(name) => {
                    let s = name.to_string();
                    s.rsplit("::").next().unwrap_or(&s).to_string()
                }
                _ => return false,
            };
            if method == "await" { return true; }
            for arg in &inv.args { if expr_contains_tokio(arg) { return true; } }
            false
        }
        ExprKind::Await(_) => true,
        ExprKind::Select(sel) => {
            sel.field.name.as_str() == "await" || expr_contains_tokio(&sel.obj)
        }
        ExprKind::Closure(cl) => expr_contains_tokio(&cl.body),
        ExprKind::Block(block) => {
            for s in &block.stmts { if stmt_contains_tokio(s) { return true; } }
            false
        }
        ExprKind::BinOp(bin) => expr_contains_tokio(&bin.lhs) || expr_contains_tokio(&bin.rhs),
        ExprKind::UnOp(un) => expr_contains_tokio(&un.val),
        ExprKind::If(if_expr) => {
            expr_contains_tokio(&if_expr.cond)
                || expr_contains_tokio(&if_expr.then)
                || if_expr.elze.as_ref().map_or(false, |e| expr_contains_tokio(e))
        }
        ExprKind::Match(mt) => {
            mt.scrutinee.as_ref().map_or(false, |s| expr_contains_tokio(s))
                || mt.cases.iter().any(|c| expr_contains_tokio(&c.body))
        }
        ExprKind::While(wh) => expr_contains_tokio(&wh.cond) || expr_contains_tokio(&wh.body),
        ExprKind::For(fr) => expr_contains_tokio(&fr.iter) || expr_contains_tokio(&fr.body),
        ExprKind::Loop(lp) => expr_contains_tokio(&lp.body),
        ExprKind::Assign(a) => expr_contains_tokio(&a.value) || expr_contains_tokio(&a.target),
        ExprKind::Let(l) => expr_contains_tokio(&l.expr),
        ExprKind::Return(r) => r.value.as_ref().map_or(false, |v| expr_contains_tokio(v)),
        ExprKind::Async(_) => true,
        _ => false,
    }
}

/// Detect parser combinator functions that rely on winnow/nom patterns.
fn is_winnow_parser(stmts: &[BlockStmt]) -> bool {
    for stmt in stmts {
        if stmt_contains_winnow(stmt) { return true; }
    }
    false
}

fn stmt_contains_winnow(stmt: &BlockStmt) -> bool {
    match stmt {
        BlockStmt::Expr(se) => expr_contains_winnow(&se.expr),
        BlockStmt::Let(l) => {
            if let Some(init) = &l.init { expr_contains_winnow(init) } else { false }
        }
        BlockStmt::Item(item) => item_contains_winnow(item),
        _ => false,
    }
}

fn item_contains_winnow(item: &Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(f) => is_winnow_parser(&f.body.stmts),
        _ => false,
    }
}

fn expr_contains_winnow(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Invoke(inv) => {
            let method = match &inv.target {
                ExprInvokeTarget::Method(sel) => sel.field.name.as_str().to_string(),
                ExprInvokeTarget::Function(name) => {
                    let s = name.to_string();
                    s.rsplit("::").next().unwrap_or(&s).to_string()
                }
                _ => return false,
            };
            let winnow_methods = &["parse_next", "take_while", "verify", "alt", "preceded",
                "delimited", "terminated", "separated_pair", "tuple", "many0", "many1"];
            if winnow_methods.contains(&method.as_str()) { return true; }
            // A winnow call can be the *receiver* of a further chained call
            // (`"...".parse_next(input).map(...)`), not just an argument.
            if let ExprInvokeTarget::Method(sel) = &inv.target {
                if expr_contains_winnow(&sel.obj) { return true; }
            }
            for arg in &inv.args { if expr_contains_winnow(arg) { return true; } }
            false
        }
        ExprKind::Select(sel) => expr_contains_winnow(&sel.obj),
        ExprKind::Closure(cl) => expr_contains_winnow(&cl.body),
        ExprKind::Block(block) => {
            for s in &block.stmts { if stmt_contains_winnow(s) { return true; } }
            false
        }
        ExprKind::BinOp(bin) => expr_contains_winnow(&bin.lhs) || expr_contains_winnow(&bin.rhs),
        ExprKind::UnOp(un) => expr_contains_winnow(&un.val),
        ExprKind::If(if_expr) => {
            expr_contains_winnow(&if_expr.cond)
                || expr_contains_winnow(&if_expr.then)
                || if_expr.elze.as_ref().map_or(false, |e| expr_contains_winnow(e))
        }
        ExprKind::Match(mt) => {
            mt.scrutinee.as_ref().map_or(false, |s| expr_contains_winnow(s))
                || mt.cases.iter().any(|c| expr_contains_winnow(&c.body))
        }
        _ => false,
    }
}

// ── Import ───────────────────────────────────────────────────────────────────

fn emit_import(imp: &ItemImport, e: &mut KotlinEmitter) -> Result<()> {
    let path = flatten_import_tree(&imp.tree);
    if path.is_empty() { return Ok(()); }

    // Handle multi-name group imports: Rust `use foo::{A, B, C}` → `import foo.*`
    let effective = if path.contains(",") {
        let first = path.split(",").next().unwrap_or(&path);
        // Drop the last segment (the specific name) to get the parent module
        let parent = first.rsplitn(2, ".").nth(1).unwrap_or(first);
        if parent.is_empty() { ".*".to_string() } else { format!("{}.*", parent) }
    } else {
        path.clone()
    };

    let pkg = known_package(&effective);
    let kt = kt_import_for(pkg, &effective);
    if let Some(import) = kt {
        // A single logical import can expand to multiple Kotlin import lines
        // (e.g. StdPath needs both `Path` and `Paths`).
        // Every generated file across every workspace package lives in the same
        // default (unnamed) Kotlin package, so a sibling *workspace* crate's
        // symbols are already visible without an import too — and there's no
        // package literally named after the crate to import from, since none
        // of these files declare one. `e.workspace_packages` is the real set
        // of sibling package names for this compile, not a hardcoded guess.
        for import in import.split('\n') {
            let first_segment = import.split('.').next().unwrap_or(import);
            if pkg == KnownPackage::Other
                && (e.local_modules.contains(first_segment)
                    || e.workspace_packages.contains(first_segment)
                    || e.workspace_packages.contains(&first_segment.replace('_', "-")))
            {
                continue;
            }
            e.push_line(&format!("import {}", import));
        }
    }
    Ok(())
}

fn known_package(path: &str) -> KnownPackage {
    use fp_core::intrinsics::calls::KnownPackage::*;
    match path {
        p if p.starts_with("std.collections") => StdCollections,
        p if p.starts_with("std.path") => StdPath,
        p if p.starts_with("std.process") => StdProcess,
        p if p.starts_with("std.sync") => StdSync,
        p if p.starts_with("std.fs") => StdFs,
        p if p.starts_with("std.io") => StdIo,
        p if p.starts_with("std.str") => StdStr,
        p if p.starts_with("std.option") => StdOption,
        p if p.starts_with("std.time") => StdSync, // skip Duration/Instant in expressions
        // Dot-boundary match so "serde_json" isn't misclassified as the "serde" derive crate.
        p if p == "serde" || p.starts_with("serde.") => Serde,
        p if p.starts_with("winnow") => Winnow,
        p if p.starts_with("thiserror") => ThisError,
        p if p.starts_with("tracing") => Tracing,
        p if p.starts_with("async_trait") => AsyncTrait,
        p if p.starts_with("anyhow") => Anyhow,
        p if p.starts_with("toml") || p.starts_with("serde_json") || p.starts_with("tokio") => Unsupported,
        _ => Other,
    }
}

fn kt_import_for(pkg: KnownPackage, path: &str) -> Option<String> {
    use fp_core::intrinsics::calls::KnownPackage::*;
    // Silent skip for language-internal packages
    if matches!(pkg, ThisError | Tracing | AsyncTrait | Anyhow) { return None; }
    // Relative imports not valid in Kotlin
    if path.starts_with('.') { return None; }
    match pkg {
        StdCollections | StdSync | StdStr | StdOption | Serde | Winnow
        | ThisError | Tracing | AsyncTrait | Anyhow | Unsupported => None,
        // `Path::from`/`new` renders as `Paths.get(...)` (see map_kt_path), so both
        // classes need to be in scope.
        StdPath => Some("java.nio.file.Path\njava.nio.file.Paths".into()),
        StdProcess => Some("java.lang.ProcessBuilder".into()),
        StdFs => Some("java.nio.file.Path".into()),
        StdIo => Some("java.io.*".into()),
        Other => {
            let clean = path.trim_start_matches("crate.").trim_start_matches("self.");
            if clean.is_empty() { None } else { Some(clean.to_string()) }
        }
    }
}

fn flatten_import_tree(tree: &fp_core::ast::ItemImportTree) -> String {
    use fp_core::ast::ItemImportTree::*;
    match tree {
        Path(p) => p.segments.iter().map(|s| flatten_import_tree(s)).collect::<Vec<_>>().join("."),
        Ident(id) => id.name.clone(),
        Rename(r) => format!("{} as {}", r.from.name, r.to.name),
        Glob => "*".to_string(),
        Group(g) => g.items.iter().map(|i| flatten_import_tree(i)).collect::<Vec<_>>().join(", "),
        _ => String::new(),
    }
}

// ── Statements ───────────────────────────────────────────────────────────────

fn emit_stmt(stmt: &BlockStmt, e: &mut KotlinEmitter, is_tail: bool) -> Result<()> {
    match stmt {
        BlockStmt::Let(l) => {
            let var_name = ident_from_pattern(&l.pat);
            let type_ann = extract_type_annotation(&l.pat, e);
            let decl_kw = if is_mut_pattern(&l.pat) { "var" } else { "val" };
            if var_name != "_" {
                e.declare_name(&var_name);
            }
            // `.len()` needs `.size` on a List but `.length` on a String — record
            // this name as list-typed (reusing `field_element_types`, which the
            // `.len()` call site below checks by name) so it renders correctly.
            if let Some(elem) = type_ann.as_deref().and_then(|t|
                t.strip_prefix("MutableList<").or_else(|| t.strip_prefix("List<")).and_then(|s| s.strip_suffix('>'))
            ) {
                e.field_element_types.insert(var_name.clone(), elem.to_string());
            }
            // `let mut parts = s.split(sep);` — Kotlin's `.split()` returns a `List`,
            // not a stateful iterator; remember `parts` so subsequent `.next()?` calls
            // on it (see below) can be modeled as indexed access instead of erased.
            if let Some(init) = &l.init {
                if let ExprKind::Invoke(inv) = init.kind() {
                    if let ExprInvokeTarget::Method(sel) = &inv.target {
                        if sel.field.name.as_str() == "split" {
                            e.split_iter_vars.insert(var_name.clone());
                        }
                    }
                }
            }
            if var_name == "_" {
                if let Some(init) = &l.init {
                    let val = render_expr(init, e)?;
                    e.push_line(&val);
                }
            } else if let Some(init) = &l.init {
                // Rust's manual-iterator `.next()?` extraction (e.g. `parts.next()?`
                // after `let mut parts = s.split(sep)`) — render as an indexed access
                // with an early return on exhaustion, matching `?`'s None-propagation.
                if let ExprKind::Try(t) = init.kind() {
                    if let ExprKind::Invoke(inv) = t.expr.kind() {
                        if let ExprInvokeTarget::Method(sel) = &inv.target {
                            if sel.field.name.as_str() == "next" && inv.args.is_empty() {
                                if let ExprKind::Name(name) = sel.obj.kind() {
                                    let obj_name = name.to_string();
                                    if e.split_iter_vars.contains(&obj_name) {
                                        let idx = *e.next_call_counters.get(&obj_name).unwrap_or(&0);
                                        e.next_call_counters.insert(obj_name.clone(), idx + 1);
                                        let obj_rendered = render_expr(&sel.obj, e)?;
                                        let val = format!("{}.getOrNull({}) ?: return null", obj_rendered, idx);
                                        if let Some(ref ty) = type_ann {
                                            e.push_line(&format!("{} {} : {} = {}", decl_kw, var_name, ty, val));
                                        } else {
                                            e.push_line(&format!("{} {} = {}", decl_kw, var_name, val));
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }
                }
                // `assert_eq!`/`assert_ne!` lift each side of the comparison into its own
                // `let`, discarding the field type the literal is compared against — track
                // it across the pair so a `Long`-typed field's literal gets an `L` suffix.
                if var_name == "__fp_assert_left" {
                    e.pending_assert_long = matches!(init.kind(), ExprKind::Select(sel)
                        if e.long_field_names.contains(sel.field.name.as_str()));
                }
                let mut val = render_expr(init, e)?;
                if var_name == "__fp_assert_right" {
                    if e.pending_assert_long && matches!(init.kind(), ExprKind::Value(v) if matches!(v.as_ref(), Value::Int(_) | Value::UInt(_))) {
                        val.push('L');
                    }
                    e.pending_assert_long = false;
                }
                if let Some(ref ty) = type_ann {
                    e.push_line(&format!("{} {} : {} = {}", decl_kw, var_name, ty, val));
                } else {
                    e.push_line(&format!("{} {} = {}", decl_kw, var_name, val));
                }
            } else {
                if let Some(ref ty) = type_ann {
                    e.push_line(&format!("{} {} : {} = null", decl_kw, var_name, ty));
                } else {
                    e.push_line(&format!("{} {} = null", decl_kw, var_name));
                }
            }
        }
        BlockStmt::Expr(se) => emit_stmt_expr(&se.expr, e, is_tail)?,
        BlockStmt::Item(item) => return emit_item(item, e),
        BlockStmt::Noop => {}
        _ => {}
    }
    Ok(())
}

fn emit_stmt_expr(expr: &Expr, e: &mut KotlinEmitter, is_tail: bool) -> Result<()> {
    match expr.kind() {
        ExprKind::Block(block) => {
            // A bare `{ ... }` immediately after a preceding statement gets glued to
            // it as a trailing lambda argument by Kotlin's parser — wrap in `run` so
            // it's unambiguously its own statement.
            e.push_line("run {");
            e.indent += 1;
            for s in &block.stmts { emit_stmt(s, e, false)?; }
            e.indent -= 1;
            e.push_line("}");
        }
        ExprKind::If(if_expr) => emit_if_stmt(if_expr, e, is_tail)?,
        ExprKind::While(wh) => {
            let cond = render_expr(&wh.cond, e)?;
            e.push_line(&format!("while ({}) {{", cond));
            e.indent += 1;
            emit_box_body(&wh.body, e)?;
            e.indent -= 1;
            e.push_line("}");
        }
        ExprKind::Loop(lp) => {
            e.push_line("while (true) {");
            e.indent += 1;
            emit_box_body(&lp.body, e)?;
            e.indent -= 1;
            e.push_line("}");
        }
        ExprKind::For(fr) => {
            let iter_expr = render_expr(&fr.iter, e)?;
            let var = ident_from_pattern(&fr.pat);
            // Rust `for` patterns can't carry an explicit type annotation (unlike
            // closure params) — infer the element type when iterating a struct
            // field we know is `List<T>`/`MutableList<T>` (e.g. `for hunk in &f.hunks`).
            let field_name = match fr.iter.kind() {
                ExprKind::Select(sel) => Some(sel.field.name.as_str()),
                ExprKind::Reference(r) => match r.referee.kind() {
                    ExprKind::Select(sel) => Some(sel.field.name.as_str()),
                    _ => None,
                },
                _ => None,
            };
            let var = match field_name.and_then(|f| e.field_element_types.get(f)) {
                Some(ty) if var != "_" && !var.starts_with('(') => format!("{}: {}", var, ty),
                _ => var,
            };
            e.push_line(&format!("for ({} in {}) {{", var, iter_expr));
            e.indent += 1;
            emit_box_body(&fr.body, e)?;
            e.indent -= 1;
            e.push_line("}");
        }
        ExprKind::Return(ret) => {
            if let Some(val) = &ret.value {
                let v = render_expr(val, e)?;
                e.push_line(&format!("return {}", v));
            } else { e.push_line("return"); }
        }
        ExprKind::Break(_) => { e.push_line("break"); }
        ExprKind::Continue(_) => { e.push_line("continue"); }
        _ => {
            let rendered = render_expr(expr, e)?;
            if is_tail { e.push_line(&format!("return {}", rendered)); }
            else { e.push_line(&rendered); }
        }
    }
    Ok(())
}

fn emit_if_stmt(if_expr: &fp_core::ast::ExprIf, e: &mut KotlinEmitter, is_tail: bool) -> Result<()> {
    let cond = render_expr(&if_expr.cond, e)?;
    e.push_line(&format!("if ({}) {{", cond));
    e.indent += 1;
    emit_box_body(&if_expr.then, e)?;
    e.indent -= 1;
    if let Some(elze) = &if_expr.elze {
        e.push_line("} else {");
        e.indent += 1;
        emit_box_body(elze, e)?;
        e.indent -= 1;
    }
    e.push_line("}");
    Ok(())
}

fn emit_box_body(body: &BExpr, e: &mut KotlinEmitter) -> Result<()> {
    if let ExprKind::Block(block) = body.kind() {
        for s in &block.stmts { emit_stmt(s, e, false)?; }
    } else {
        let val = render_expr(body, e)?;
        e.push_line(&val);
    }
    Ok(())
}

/// Extract a Kotlin type string from a pattern's type annotation (PatternKind::Type).
fn extract_type_annotation(pat: &Pattern, e: &KotlinEmitter) -> Option<String> {
    match &pat.kind {
        PatternKind::Type(pt) => {
            Some(kotlin_type_from_ty(&pt.ty, e))
        }
        _ => None,
    }
}

fn ident_from_pattern(pat: &Pattern) -> String {
    match &pat.kind {
        PatternKind::Ident(id) => {
            let name = id.ident.name.as_str();
            if matches!(name, "else" | "when" | "in" | "is" | "as" | "object") {
                format!("`{}`", name)
            } else if name == "_" {
                "__p".to_string()
            } else {
                name.to_string()
            }
        }
        PatternKind::Type(pt) => ident_from_pattern(&pt.pat),
        PatternKind::Tuple(t) => {
            let names: Vec<String> = t.patterns.iter().map(|p| ident_from_pattern(p)).collect();
            format!("({})", names.join(", "))
        }
        PatternKind::Ref(r) => ident_from_pattern(&r.pattern),
        _ => "_".to_string(),
    }
}

/// True if this pattern (looking through `PatternKind::Type`) is a `let mut`/`mut` binding.
fn is_mut_pattern(pat: &Pattern) -> bool {
    match &pat.kind {
        PatternKind::Ident(id) => id.mutability.unwrap_or(false),
        PatternKind::Type(pt) => is_mut_pattern(&pt.pat),
        PatternKind::Ref(r) => r.mutability.unwrap_or(false) || is_mut_pattern(&r.pattern),
        // Rust marks mutability per-binding (`let (mut a, mut b) = ...`), but
        // Kotlin destructuring declarations are all-or-nothing (`var (a, b)`
        // makes both `var`) — "any element mutable" is the right merge.
        PatternKind::Tuple(t) => t.patterns.iter().any(is_mut_pattern),
        _ => false,
    }
}

// ── Expressions ──────────────────────────────────────────────────────────────

/// True if `expr` is `<obj>.as_bytes()[<idx>]` — indexing into a byte array,
/// which renders to Kotlin `Byte`, not `Char`.
fn is_byte_array_index(expr: &Expr) -> bool {
    if let ExprKind::Index(idx) = expr.kind() {
        if let ExprKind::Invoke(inv) = idx.obj.kind() {
            if let ExprInvokeTarget::Method(sel) = &inv.target {
                return sel.field.name.as_str() == "as_bytes";
            }
        }
    }
    false
}

fn render_expr(expr: &Expr, e: &mut KotlinEmitter) -> Result<String> {
    match expr.kind() {
        ExprKind::Value(val) => {
            let rendered = render_value(val);
            // Kotlin has no byte-literal syntax — a `u8` value (e.g. from a Rust
            // byte literal `b':'`) needs an explicit `.toByte()` conversion to be
            // usable where an actual `Byte` (not `Int`) is expected.
            let is_u8 = matches!(
                expr.ty(),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U8)))
            );
            if is_u8 && matches!(val.as_ref(), Value::Int(_) | Value::UInt(_)) {
                Ok(format!("{}.toByte()", rendered))
            } else {
                Ok(rendered)
            }
        }
        ExprKind::Name(name) => {
            let raw = name.to_string();
            let dotted = raw.replace("::", ".");
            // Uppercase enum variant references in qualified paths
            Ok(uppercase_last_segment(&dotted))
        }
        ExprKind::Id(id) => Ok(id.to_string()),

        ExprKind::Invoke(inv) => {
            match &inv.target {
                ExprInvokeTarget::Method(sel) => {
                    // `.or_else(|_| fallback)` / `.unwrap_or_else(|_| fallback)` on a
                    // nullable value — Kotlin's `?:` already lazily evaluates its RHS
                    // only when the LHS is null, which matches these methods' fallback
                    // semantics exactly (the error/ignored-value closure param is never
                    // used in this codebase). `.run`/`.let` (the generic method-name
                    // mapping used elsewhere) would evaluate the fallback unconditionally.
                    if matches!(sel.field.name.as_str(), "or_else" | "unwrap_or_else") && inv.args.len() == 1 {
                        if let ExprKind::Closure(cl) = inv.args[0].kind() {
                            let obj = render_expr(&sel.obj, e)?;
                            let body = render_expr(&cl.body, e)?;
                            let rhs = if body.contains('\n') {
                                format!("run {{\n{}\n}}", body)
                            } else {
                                body
                            };
                            return Ok(format!("{} ?: {}", obj, rhs));
                        }
                    }
                    // `Option::take()` — replaces a `var` with `None`/`null`, returning
                    // the old value. Kotlin has no equivalent method; model it directly.
                    if sel.field.name.as_str() == "take" && inv.args.is_empty() {
                        if let ExprKind::Name(_) = sel.obj.kind() {
                            let obj = render_expr(&sel.obj, e)?;
                            return Ok(format!("run {{ val __t = {0}; {0} = null; __t }}", obj));
                        }
                    }
                    let obj = render_expr(&sel.obj, e)?;
                    // `.map` is ambiguous: `Option::map`/`Result::map` need Kotlin's
                    // `.let { }` (no built-in `.map` on nullable types), but
                    // `Iterator::map` needs Kotlin's own (identically-named) `.map { }`,
                    // unchanged — the generic table below assumes the former. Detect the
                    // latter structurally: an iterator-producing receiver just before it.
                    let is_iterator_map = sel.field.name.as_str() == "map" && {
                        if let ExprKind::Invoke(recv_inv) = sel.obj.kind() {
                            if let ExprInvokeTarget::Method(recv_sel) = &recv_inv.target {
                                matches!(recv_sel.field.name.as_str(),
                                    "iter" | "iter_mut" | "into_iter" | "lines" | "chars")
                            } else { false }
                        } else { false }
                    };
                    // `.len()` needs `.size` on a List but `.length` on a String —
                    // `map_kt_method` alone can't tell which, so check whether the
                    // receiver's name is a known List (see `field_element_types`).
                    let is_len_on_list = sel.field.name.as_str() == "len" && {
                        let recv_name = match sel.obj.kind() {
                            ExprKind::Name(n) => Some(name_to_string(n)),
                            ExprKind::Select(inner) => Some(inner.field.name.to_string()),
                            _ => None,
                        };
                        recv_name.is_some_and(|n| e.field_element_types.contains_key(&n))
                    };
                    let method_name = if is_iterator_map {
                        "map".to_string()
                    } else if is_len_on_list {
                        "size".to_string()
                    } else {
                        map_kt_method(sel.field.name.as_str())
                    };
                    // `is_ascii_alphabetic`/etc. map to Kotlin `Char` methods (`isLetter()`),
                    // but their receiver here is a `Byte` (indexed out of `.as_bytes()`) —
                    // bridge it to a `Char` first.
                    let obj = if matches!(method_name.as_str(), "isLetter()" | "isDigit()" | "isWhitespace()" | "isLetterOrDigit()")
                        && is_byte_array_index(&sel.obj)
                    {
                        format!("{}.toInt().toChar()", obj)
                    } else {
                        obj
                    };
                    // Kotlin's `String.replace` only overloads `(Char, Char)` or
                    // `(String, String)` — Rust's `str::replace` allows a char pattern
                    // with a string replacement, so a `Char` arg here needs coercing to
                    // a one-character string to match the mixed-type call.
                    let is_replace = sel.field.name.as_str() == "replace";
                    // `removePrefix`/`removeSuffix` (mapped from Rust's
                    // `strip_prefix`/`strip_suffix`/`trim_end_matches`) take a
                    // `CharSequence`, not `Char` — Rust's char-pattern overloads
                    // need the same char-to-one-character-string coercion `replace` does.
                    let needs_char_as_string = is_replace
                        || matches!(sel.field.name.as_str(), "strip_prefix" | "strip_suffix" | "trim_end_matches");
                    let args: Vec<String> = inv.args.iter()
                        .map(|a| {
                            if needs_char_as_string {
                                if let ExprKind::Value(v) = a.kind() {
                                    if let Value::Char(c) = v.as_ref() {
                                        return Ok(format!("\"{}\"", escape_str_for_kt(&c.value.to_string())));
                                    }
                                }
                            }
                            render_expr(a, e)
                        })
                        .collect::<Result<Vec<_>>>()?;
                    if method_name.is_empty() {
                        Ok(obj)
                    } else if method_name == "!!" {
                        Ok(format!("{}!!", obj))
                    } else if args.is_empty() {
                        Ok(format!("{}.{}", obj, method_name))
                    } else if method_name.ends_with("()") {
                        let base = &method_name[..method_name.len() - 2];
                        Ok(format!("{}.{}({})", obj, base, args.join(", ")))
                    } else {
                        Ok(format!("{}.{}({})", obj, method_name, args.join(", ")))
                    }
                }
                _ => {
                    let name = invoke_name(&inv.target);
                    // `std::env::current_dir()` — a zero-arg free function whose Kotlin
                    // equivalent needs one arg (`System.getProperty("user.dir")`), which
                    // the generic `map_kt_path` + "always append (args)" pipeline below
                    // can't express without producing a spurious trailing `()`.
                    if name == "std::env::current_dir" && inv.args.is_empty() {
                        return Ok("System.getProperty(\"user.dir\")".to_string());
                    }
                    // Crates with no safe target-language equivalent (toml, serde_json,
                    // tokio, ...) — render as an explicit stub instead of a broken
                    // identifier reference. `TODO()` is typed `Nothing`, so it compiles
                    // in any expression position.
                    if let Some(pos) = name.rfind("::") {
                        let prefix = name[..pos].replace("::", ".");
                        if known_package(&prefix) == KnownPackage::Unsupported {
                            return Ok(format!("TODO(\"unsupported: {}\")", name));
                        }
                    }
                    // Rewrite type prefix in function paths like `PathBuf::from` → `Path.of`
                    let mapped = map_kt_path(&name);
                    let args: Vec<String> = inv.args.iter()
                        .map(|a| render_expr(a, e))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(format!("{}({})", mapped, args.join(", ")))
                }
            }
        }

        ExprKind::Select(sel) => {
            let obj = render_expr(&sel.obj, e)?;
            if obj == "self" {
                let field = map_kt_field(sel.field.name.as_str());
                Ok(field)
            } else {
                let field = map_kt_field(sel.field.name.as_str());
                Ok(format!("{}.{}", obj, field))
            }
        }

        ExprKind::Index(idx) => {
            // Rust's `&s[..end]`/`s[start..]`/`&s[start..end]` (slicing with an
            // omitted bound is common) has no direct Kotlin equivalent —
            // `obj[range]` isn't valid indexing syntax. `String.substring`
            // takes the same start/end-exclusive semantics `..` already has.
            if let ExprKind::Range(r) = idx.index.kind() {
                let obj = render_expr(&idx.obj, e)?;
                let start = match &r.start {
                    Some(s) => render_expr(s, e)?,
                    None => "0".to_string(),
                };
                return match &r.end {
                    Some(end) => {
                        let end = render_expr(end, e)?;
                        let end = if matches!(r.limit, fp_core::ast::ExprRangeLimit::Inclusive) {
                            format!("({} + 1)", end)
                        } else {
                            end
                        };
                        Ok(format!("{}.substring({}, {})", obj, start, end))
                    }
                    None => Ok(format!("{}.substring({})", obj, start)),
                };
            }
            Ok(format!("{}[{}]", render_expr(&idx.obj, e)?, render_expr(&idx.index, e)?))
        }

        ExprKind::BinOp(bin) => {
            Ok(format!("({} {} {})", render_expr(&bin.lhs, e)?, kotlin_bin_op(&bin.kind), render_expr(&bin.rhs, e)?))
        }

        ExprKind::UnOp(un) => {
            Ok(format!("{}({})", kotlin_un_op(&un.op), render_expr(&un.val, e)?))
        }

        ExprKind::If(if_expr) => {
            let cond = render_expr(&if_expr.cond, e)?;
            let then_val = render_expr_single(&if_expr.then, e)?;
            if let Some(elze) = &if_expr.elze {
                Ok(format!("if ({}) {} else {}", cond, then_val, render_expr_single(elze, e)?))
            } else {
                Ok(format!("if ({}) {}", cond, then_val))
            }
        }

        ExprKind::Match(mt) => {
            let scrutinee = mt.scrutinee.as_ref()
                .map(|s| render_expr(s, e)).transpose()?
                .unwrap_or_else(|| "null".to_string());

            // Single non-wildcard arm OR 2-arm match (Some/Ok + else) → val + if
            let is_single_arm = mt.cases.len() == 1 && !matches!(mt.cases[0].pat.as_ref().map(|p| &p.kind), Some(PatternKind::Wildcard(_)));
            let is_two_arm = mt.cases.len() == 2 && is_else_arm(&mt.cases[1].pat);

            if is_single_arm || is_two_arm {
                let case = &mt.cases[0];

                // Determine the binding shape via pure pattern inspection (no rendering
                // yet), so any names it introduces can be pre-seeded into a scope before
                // the arm body renders — a same-named `let`-shadow inside that body (see
                // the `ExprKind::Block` arm) needs to see these as already declared.
                let non_monadic = if is_two_arm { non_monadic_tuple_variant(&case.pat) } else { None };
                let effective_var = if non_monadic.is_some() {
                    None
                } else if is_two_arm {
                    stripped_tuple_binding(&case.pat).or_else(|| match_case_binding(&case.pat))
                } else {
                    match_case_binding(&case.pat)
                };

                e.push_scope();
                if let Some((_, ref binding)) = non_monadic {
                    e.declare_name(binding);
                }
                if let Some(ref var) = effective_var {
                    if let Some(names) = var.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
                        for name in names.split(", ") { e.declare_name(name); }
                    }
                }
                let body = render_expr(&case.body, e)?;
                e.pop_scope();
                let multiline = body.contains('\n');

                // A 2-arm match on an ordinary enum variant (not Some/Ok/Err/None) isn't a
                // null-check equivalent — the scrutinee itself isn't interchangeable with its
                // payload, so it needs a smart-cast + field access, not `val x = scrutinee`.
                if let Some((variant_path, binding)) = non_monadic {
                    let else_body = render_expr(&mt.cases[1].body, e)?;
                    let else_trimmed = else_body.trim();
                    let body_lines: String = body.lines()
                        .map(|l| format!("        {}", l))
                        .collect::<Vec<_>>().join("\n");
                    let else_clause = if else_trimmed.is_empty() {
                        " else {\n        null\n    }".to_string()
                    } else {
                        format!(" else {{\n        {}\n    }}", else_body)
                    };
                    return Ok(format!(
                        "if ({0} is {1}) {{\n    val {2} = {0}.__data\n{3}\n}}{4}",
                        scrutinee, variant_path, binding, body_lines, else_clause
                    ));
                }
                let else_body = if mt.cases.len() > 1 {
                    Some(render_expr(&mt.cases[1].body, e)?)
                } else {
                    None
                };
                match effective_var {
                    Some(var) if var.starts_with('(') => {
                        // Tuple destructuring inside the wrapper, e.g. `Some((host, path))`
                        // from `raw.split_once(':')` — null-check a temp, then destructure.
                        let tmp = "__m";
                        let else_trimmed = else_body.as_deref().map(str::trim).filter(|s| !s.is_empty());
                        let body_lines: String = body.lines()
                            .map(|l| format!("        {}", l))
                            .collect::<Vec<_>>().join("\n");
                        let else_clause = match else_trimmed {
                            Some(eb) => format!(" else {{\n            {}\n        }}", eb),
                            None => " else {\n            null\n        }".to_string(),
                        };
                        Ok(format!(
                            "run {{\n    val {0} = {1}\n    if ({0} != null) {{\n        val {2} = {0}!!\n{3}\n    }}{4}\n}}",
                            tmp, scrutinee, var, body_lines, else_clause
                        ))
                    }
                    Some(var) => {
                        if let Some(ref eb) = else_body {
                            let eb_trim = eb.trim();
                            let rendered_body = if multiline {
                                let indented: String = body.lines()
                                    .map(|l| format!("            {}", l))
                                    .collect::<Vec<_>>().join("\n");
                                format!("{{\n{}\n        }}", indented)
                            } else {
                                body.clone()
                            };
                            if eb_trim.is_empty() {
                                Ok(format!("run {{\n    val {0} = {1}\n    if ({0} != null) {2}\n}}",
                                    var, scrutinee, rendered_body))
                            } else {
                                Ok(format!("run {{\n    val {0} = {1}\n    if ({0} != null) {2} else {{\n            {3}\n        }}\n}}",
                                    var, scrutinee, rendered_body, eb))
                            }
                        } else {
                            if multiline {
                                let indented: String = body.lines()
                                    .map(|l| format!("        {}", l))
                                    .collect::<Vec<_>>().join("\n");
                                Ok(format!("run {{\n    val {0} = {1}\n    if ({0} != null) {{\n{2}\n    }} else {{\n        null\n    }}\n}}",
                                    var, scrutinee, indented))
                            } else {
                                Ok(format!("run {{\n    val {0} = {1}\n    if ({0} != null) {{ {2} }} else {{ null }}\n}}", var, scrutinee, body))
                            }
                        }
                    }
                    None => {
                        if multiline {
                            let indented: String = body.lines()
                                .map(|l| format!("        {}", l))
                                .collect::<Vec<_>>().join("\n");
                            Ok(format!("if ({0}) {{\n{1}\n}}", scrutinee, indented))
                        } else {
                            Ok(format!("if ({0}) {{ {1} }}", scrutinee, body))
                        }
                    }
                }
            } else {
                let mut buf = format!("when ({}) {{\n", scrutinee);
                for case in &mt.cases {
                    let pat = render_match_pat(&case.pat);
                    let body = render_expr(&case.body, e)?;
                    if body.contains('\n') {
                        let indented: String = body.lines()
                            .map(|l| format!("            {}", l))
                            .collect::<Vec<_>>().join("\n");
                        let _ = writeln!(buf, "        {} -> {{\n{}\n        }}", pat, indented);
                    } else {
                        let _ = writeln!(buf, "        {} -> {}", pat, body);
                    }
                }
                for _ in 0..e.indent { buf.push_str("    "); }
                buf.push('}');
                Ok(buf)
            }
        }

        ExprKind::Block(block) => {
            e.push_scope();
            let mut buf = String::new();
            // Rust allows re-`let`-ing a name in the same scope (shadowing); Kotlin
            // doesn't allow a flat re-declaration. When that would happen, nest the
            // remainder of the block in a fresh `run { }` scope instead — each nested
            // `val` then legitimately shadows the outer one.
            let mut nest_depth: usize = 0;
            for stmt in &block.stmts {
                if let BlockStmt::Let(l) = stmt {
                    let name = ident_from_pattern(&l.pat);
                    if name != "_" && !name.starts_with('(') && e.is_declared(&name) {
                        for _ in 0..=nest_depth { buf.push_str("    "); }
                        buf.push_str("run {\n");
                        nest_depth += 1;
                        e.push_scope();
                    }
                }
                let saved = std::mem::take(&mut e.code);
                e.indent += 1 + nest_depth;
                emit_stmt(stmt, e, false)?;
                let stmt_text = std::mem::replace(&mut e.code, saved);
                buf.push_str(&stmt_text);
                e.indent -= 1 + nest_depth;
            }
            for d in (0..nest_depth).rev() {
                for _ in 0..=d { buf.push_str("    "); }
                buf.push_str("}\n");
                e.pop_scope();
            }
            e.pop_scope();
            Ok(buf)
        }

        ExprKind::Assign(assign) => {
            Ok(format!("{} = {}", render_expr(&assign.target, e)?, render_expr(&assign.value, e)?))
        }

        ExprKind::Struct(st) => {
            let name = render_expr(&st.name, e)?;
            let variant_name = uppercase_last_segment(&name);
            let fields: Vec<String> = st.fields.iter().map(|f| {
                // `None` means Rust field-init shorthand (`Field { name }` ≡ `Field { name: name }`),
                // not an explicit null value.
                let val = match &f.value { Some(v) => render_expr(v, e)?, None => f.name.name.clone() };
                Ok(format!("{} = {}", f.name.name, val))
            }).collect::<Result<Vec<_>>>()?;
            Ok(format!("{}({})", variant_name, fields.join(", ")))
        }

        ExprKind::Array(arr) => {
            let items: Vec<String> = arr.values.iter().map(|v| render_expr(v, e)).collect::<Result<Vec<_>>>()?;
            Ok(format!("listOf({})", items.join(", ")))
        }

        ExprKind::Tuple(tup) => {
            let items: Vec<String> = tup.values.iter().map(|v| render_expr(v, e)).collect::<Result<Vec<_>>>()?;
            // Kotlin's built-in tuple constructors only go up to 3 elements.
            let ctor = match items.len() {
                3 => "Triple",
                _ => "Pair",
            };
            Ok(format!("{}({})", ctor, items.join(", ")))
        }

        ExprKind::Reference(r) => render_expr(&r.referee, e),
        ExprKind::Dereference(d) => render_expr(&d.referee, e),
        ExprKind::Cast(c) => {
            let inner = render_expr(&c.expr, e)?;
            let conv = match kotlin_type_from_ty(&c.ty, e).as_str() {
                "Byte" => Some("toByte()"),
                "Short" => Some("toShort()"),
                "Int" => Some("toInt()"),
                "Long" => Some("toLong()"),
                "Float" => Some("toFloat()"),
                "Double" => Some("toDouble()"),
                "Char" => Some("toInt().toChar()"),
                _ => None,
            };
            match conv {
                Some(m) => Ok(format!("{}.{}", inner, m)),
                None => Ok(inner),
            }
        }
        ExprKind::Paren(p) => Ok(format!("({})", render_expr(&p.expr, e)?)),

        ExprKind::Closure(cl) => {
            let params: Vec<String> = cl.params.iter().map(|p| {
                let n = ident_from_pattern(p);
                // An explicit `|c: char| ...` annotation parses as `PatternKind::Type`
                // wrapping the ident pattern, not `Pattern.ty` (that slot is populated
                // by type inference/checking, which this untyped pipeline never runs).
                let ty_str = if let PatternKind::Type(pt) = &p.kind {
                    Some(kotlin_type_from_ty(&pt.ty, e))
                } else {
                    kotlin_type_from_ty_slot(&p.ty, e)
                };
                if n.starts_with('(') {
                    // Destructuring lambda param (`{ (a, b) -> ... }`) — Kotlin doesn't
                    // support a blanket type annotation after the whole pattern here.
                    n
                } else if n == "_" {
                    if let Some(ty) = ty_str { format!("it: {}", ty) } else { "it: Any?".to_string() }
                } else if let Some(ty) = ty_str {
                    format!("{}: {}", n, ty)
                } else {
                    format!("{}: Any?", n)
                }
            }).collect();
            Ok(format!("{{ {} -> {} }}", params.join(", "), render_expr_single(&cl.body, e)?))
        }

        ExprKind::Let(l) => {
            Ok(format!("val {} = {}", ident_from_pattern(&l.pat), render_expr(&l.expr, e)?))
        }

        ExprKind::Return(ret) => {
            if let Some(val) = &ret.value {
                Ok(format!("return {}", render_expr(val, e)?))
            } else { Ok("return".to_string()) }
        }

        ExprKind::IntrinsicCall(ic) => {
            use fp_core::intrinsics::calls::OpKind;
            // Render all args first to avoid borrow conflicts
            let args: Vec<String> = ic.args.iter()
                .map(|a| render_expr(a, e))
                .collect::<Result<Vec<_>>>()?;

            match &ic.kind {
                CallKind::Op(OpKind::MapOr) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    let default = args.get(1).cloned().unwrap_or_default();
                    Ok(format!("{} ?: {}", receiver, default))
                }
                CallKind::Op(OpKind::Collect) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    Ok(format!("{}.toList()", receiver))
                }
                CallKind::Op(OpKind::Find) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    let pred = args.get(1).cloned();
                    if let Some(p) = pred {
                        Ok(format!("{}.firstOrNull {{ {} }}", receiver, p))
                    } else {
                        Ok(format!("{}.firstOrNull()", receiver))
                    }
                }
                CallKind::Op(OpKind::UnwrapOr) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    let default = args.get(1).cloned().unwrap_or_default();
                    Ok(format!("{} ?: {}", receiver, default))
                }
                CallKind::Op(OpKind::ToString) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    Ok(format!("{}.toString()", receiver))
                }
                CallKind::Op(OpKind::AndThen) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    Ok(format!("{}.let {{ it }}", receiver))
                }
                CallKind::Op(OpKind::OptionUnwrap) => {
                    let receiver = args.first().cloned().unwrap_or_default();
                    Ok(format!("{}!!", receiver))
                }
                CallKind::Op(op @ (OpKind::Format | OpKind::Print | OpKind::Println)) => {
                    // Resolve each placeholder against its real argument and emit a
                    // genuine Kotlin string template, instead of a fake "arg" literal
                    // fed to `String.format(...)`.
                    let template = match ic.args.first().map(|a| a.kind()) {
                        Some(ExprKind::FormatString(fs)) => {
                            let value_args = &args[1..];
                            let mut next_implicit = 0usize;
                            let mut out = String::new();
                            for part in &fs.parts {
                                match part {
                                    FormatTemplatePart::Literal(lit) => out.push_str(lit),
                                    FormatTemplatePart::Placeholder(ph) => {
                                        let idx = match &ph.arg_ref {
                                            FormatArgRef::Positional(i) => *i,
                                            FormatArgRef::Implicit | FormatArgRef::Named(_) => {
                                                let i = next_implicit;
                                                next_implicit += 1;
                                                i
                                            }
                                        };
                                        let val = value_args.get(idx).cloned().unwrap_or_default();
                                        out.push_str(&format!("${{{}}}", val));
                                    }
                                }
                            }
                            format!("\"{}\"", out)
                        }
                        _ => args.first().cloned().unwrap_or_default(),
                    };
                    match op {
                        OpKind::Format => Ok(template),
                        OpKind::Print => Ok(format!("print({})", template)),
                        OpKind::Println => Ok(format!("println({})", template)),
                        _ => unreachable!(),
                    }
                }
                _ => {
                    let name = intrinsic_name(&ic.kind);
                    Ok(format!("{}({})", name, args.join(", ")))
                }
            }
        }

        ExprKind::Range(r) => {
            let start = r.start.as_ref().map(|s| render_expr(s, e)).transpose()?;
            let end = r.end.as_ref().map(|s| render_expr(s, e)).transpose()?;
            Ok(match (start, end) {
                (Some(s), Some(en)) => format!("{}..{}", s, en),
                (Some(s), None) => format!("{}..", s),
                (None, Some(en)) => format!("..{}", en),
                (None, None) => "..".to_string(),
            })
        }

        ExprKind::FormatString(fs) => {
            let parts = fs.parts.iter().map(|p| match p {
                FormatTemplatePart::Literal(lit) => Ok(lit.clone()),
                FormatTemplatePart::Placeholder(_ph) => {
                    let rendered = "arg".to_string();
                    Ok(format!("${{{}}}", rendered))
                }
            }).collect::<Result<Vec<_>>>()?;
            Ok(format!("\"{}\"", parts.join("")))
        }

        ExprKind::Break(_) => Ok("break".to_string()),
        ExprKind::Continue(_) => Ok("continue".to_string()),

        ExprKind::Try(t) => {
            // `?` operator → just render inner expr (error handling is implicit)
            render_expr(&t.expr, e)
        }
        ExprKind::Macro(_m) => {
            Ok("null".to_string())
        }
        ExprKind::ConstBlock(_) => Ok("null".to_string()),
        ExprKind::ArrayRepeat(ar) => {
            let elem = render_expr(&ar.elem, e)?;
            Ok(format!("listOf({})", elem))
        }
        ExprKind::Await(a) => {
            render_expr(&a.base, e)
        }

        _ => Ok(format!("/* unreachable: {:?} */", std::mem::discriminant(expr.kind()))),
    }
}

fn render_expr_single(body: &BExpr, e: &mut KotlinEmitter) -> Result<String> {
    if let ExprKind::Block(block) = body.kind() {
        let mut parts = Vec::new();
        for stmt in &block.stmts {
            if let BlockStmt::Expr(se) = stmt { parts.push(render_expr(&se.expr, e)?); }
        }
        Ok(parts.join(" "))
    } else { render_expr(body, e) }
}

fn render_invoke_target(target: &ExprInvokeTarget, e: &mut KotlinEmitter) -> Result<String> {
    match target {
        ExprInvokeTarget::Function(name) => Ok(name.to_string().replace("::", ".")),
        ExprInvokeTarget::Method(sel) => Ok(format!("{}.{}", render_expr(&sel.obj, e)?, sel.field.name)),
        ExprInvokeTarget::Expr(bexpr) => render_expr(bexpr, e),
        _ => Ok("call".to_string()),
    }
}

fn invoke_name(target: &ExprInvokeTarget) -> String {
    match target {
        ExprInvokeTarget::Function(name) => name.to_string(),
        ExprInvokeTarget::Method(sel) => {
            format!(".{}", sel.field.name)
        }
        _ => String::new(),
    }
}

/// Map a field/function name in a select expression to Kotlin equivalent.
fn map_kt_field(name: &str) -> String {
    match name {
        "var" => "getenv".into(),
        "current_dir" => "getProperty".into(),
        _ => name.to_string(),
    }
}

/// Map a path-style function call (e.g., `PathBuf::from` or `std::path::PathBuf::from`)
/// to its Kotlin approximation by resolving type prefixes through KnownClass.
fn map_kt_path(name: &str) -> String {
    // Type-qualified paths use `::` or `.` as separators
    if let Some(pos) = name.rfind("::") {
        let (prefix, method) = name.split_at(pos);
        let method = &method[2..];
        let normalized = prefix.replace("::", ".");

        // `Path::from`/`PathBuf::from`/`Path::new` → `java.nio.file.Path.of(...)` isn't
        // resolvable as a static-interface-method call in this position; use
        // `Paths.get(...)` instead (kt_import_for's StdPath arm imports both).
        let prefix_last = prefix.rsplit("::").next().unwrap_or(prefix);
        if matches!(method, "from" | "new") && KnownClass::from_source_type(prefix_last) == Some(KnownClass::Path) {
            return "Paths.get".to_string();
        }
        // `Vec::new()` — there's no Kotlin class named `Vec`; the portable
        // constructor is the top-level `mutableListOf()` function.
        if prefix_last == "Vec" && method == "new" {
            return "mutableListOf".to_string();
        }

        let pkg = known_package(&normalized);
        // Drop prefix only for language-internal crates (not serialization libs)
        let skip_prefix = matches!(pkg,
            KnownPackage::ThisError | KnownPackage::Tracing
            | KnownPackage::AsyncTrait | KnownPackage::Anyhow);
        if skip_prefix {
            let kt_method = map_kt_method(method);
            if method.chars().next().map_or(false, |c| c.is_uppercase()) {
                return method.to_uppercase();
            }
            if kt_method.is_empty() {
                return String::new();
            }
            return kt_method;
        }
        let kt_prefix = map_name_to_kt(prefix);
        // Drop PascalCase type prefix for local-type static methods not in known mappings
        let is_local_type = prefix.chars().next().map_or(false, |c| c.is_uppercase())
            && !prefix.contains("::")
            && method.chars().next().map_or(false, |c| c.is_lowercase());
        let known_method = is_local_type && map_kt_method(method) == method;
        if is_local_type && known_method {
            return method.to_string();
        }
        if method.chars().next().map_or(false, |c| c.is_uppercase()) {
            return format!("{}.{}", kt_prefix, method.to_uppercase());
        }
        let kt_method = map_kt_method(method);
        return format!("{}.{}", kt_prefix, kt_method);
    }
    map_kt_method(name)
}

fn map_kt_method(name: &str) -> String {
    // Portable method mappings (no Rust-specific names)
    match name {
        // Collecion constructors (portable)
        "Vec::new" | "Vec" => "mutableListOf".into(),
        "HashSet::new" => "mutableSetOf".into(),
        "HashMap::new" => "mutableMapOf".into(),
        // Collection operations (portable names)
        "unwrap" | "expect" => "!!".into(),
        "is_empty" => "isEmpty()".into(),
        "push" => "add".into(),
        "pop" => "removeLast()".into(),
        "insert" => "add".into(),
        "len" => "length".into(),
        "lines" => "lines()".into(),
        "split" => "split".into(),
        "contains" => "contains".into(),
        "replace" => "replace".into(),
        "trim" => "trim()".into(),
        "to_uppercase" => "uppercase()".into(),
        "to_lowercase" => "lowercase()".into(),
        "starts_with" => "startsWith".into(),
        "ends_with" => "endsWith".into(),
        "parse" => "toLong()".into(),
        "rfind" => "lastIndexOf".into(),
        "clone" => "copy()".into(),
        "from" => "of".into(),
        "new" => "of".into(),
        "into" => "".into(),
        "var" => "getenv".into(),
        "current_dir" => "currentDir()".into(),
        "to_string_lossy" => "toString()".into(),
        "to_string" => "toString()".into(),
        "unwrap_or_else" => "let".into(),
        "split_once" => "split".into(),
        "or_else" => "run".into(),
        "display" => "toString()".into(),
        // Property access, not a method call — java.nio.file.Path has no
        // zero-arg `fileName()` member (only the synthetic `fileName` property).
        "file_name" => "fileName".into(),
        "to_str" => "toString()".into(),
        "join" => "resolve".into(),
        "strip_prefix" => "removePrefix".into(),
        "strip_suffix" => "removeSuffix".into(),
        "trim_end_matches" => "removeSuffix".into(),
        "unwrap_or" => "".into(),
        "as_bytes" => "toByteArray()".into(),
        "map" => "let".into(),
        "parse_next" => "parse".into(),
        "verify" => "also".into(),
        "take_while" => "filter".into(),
        "is_ascii_alphanumeric" => "isLetterOrDigit()".into(),
        "is_ascii_hexdigit" => "isDigit()".into(),
        "is_whitespace" => "isWhitespace()".into(),
        "all" => "all".into(),
        // Kotlin's `CharSequence` already has `.all { c: Char -> ... }`, `.map`, etc.
        // directly — no `.chars()` step needed (that maps to a Java `IntStream`).
        "chars" => "".into(),
        // Kotlin collections are already directly iterable/chainable — no explicit
        // iterator-producing step needed before `.map`/`.filter`/`.find`/etc.
        "iter" => "".into(),
        "iter_mut" => "".into(),
        "into_iter" => "".into(),
        // Kotlin's `.map`/`.filter`/etc. already return a concrete `List`, unlike
        // Rust's lazy iterators — no separate materializing `.collect()` step needed.
        "collect" => "".into(),
        // `Iterator::sum()` — a genuine zero-arg Kotlin function, needs real parens
        // (unlike the property-style zero-arg methods erased above).
        "sum" => "sum()".into(),
        "from_millis" => "ofMillis".into(),
        "from_secs" => "ofSeconds".into(),
        "is_ascii_alphabetic" => "isLetter()".into(),
        "is_ascii_digit" => "isDigit()".into(),
        "wrapping_mul" => "times".into(),
        "write_all" => "write".into(),
        "read_to_string" => "readText".into(),
        "remove_file" => "delete".into(),
        "is_alive" => "isAlive".into(),
        "kill_process" => "destroy".into(),
        "sleep" => "Thread.sleep".into(),
        "next" => "".into(),
        _ => name.replace("::", "."),
    }
}

/// Check if a pattern is a wildcard/default (else) arm.
fn is_else_arm(pat: &Option<fp_core::ast::BPattern>) -> bool {
    match pat {
        None => true,
        Some(p) => match &p.kind {
            PatternKind::Wildcard(_) => true,
            // Err(_) is also a catch-all arm
            PatternKind::TupleStruct(ts) => {
                let raw = ts.name.to_string();
                let simple = raw.rsplit("::").next().unwrap_or(&raw);
                (simple == "Err" || simple == "None")
                    && ts.patterns.iter().all(|inner| matches!(&inner.kind, PatternKind::Wildcard(_)))
            }
            _ => false,
        },
    }
}

/// Uppercase the last path segment for Kotlin enum constant references.
fn uppercase_last_segment(name: &str) -> String {
    if let Some(pos) = name.rfind('.') {
        let (prefix, variant) = name.split_at(pos + 1);
        format!("{}{}", prefix, variant.to_uppercase())
    } else if let Some(pos) = name.rfind("::") {
        let (prefix, variant) = name.split_at(pos + 2);
        format!("{}{}", prefix, variant.to_uppercase())
    } else {
        name.to_string()
    }
}

fn render_match_pat(pat: &Option<fp_core::ast::BPattern>) -> String {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::Ident(id) => id.ident.name.clone(),
            PatternKind::Wildcard(_) => "else".to_string(),
            PatternKind::Struct(s) => s.fields.iter()
                .map(|f| f.name.name.clone())
                .collect::<Vec<_>>().join(", "),
            PatternKind::Tuple(t) => t.patterns.iter()
                .map(|p| render_match_pat(&Some(Box::new(p.clone()))))
                .collect::<Vec<_>>().join(", "),
            PatternKind::TupleStruct(ts) => {
                let raw_name = ts.name.to_string();
                let simple_name = raw_name.rsplit("::").next().unwrap_or(&raw_name);
                // Portable monadic wrappers (Option/Result) — strip to just the binding
                if matches!(simple_name, "Ok" | "Err" | "Some" | "None") {
                    if ts.patterns.is_empty() {
                        return "null".to_string();
                    }
                    return ts.patterns.iter()
                        .map(|p| render_match_pat(&Some(Box::new(p.clone()))))
                        .collect::<Vec<_>>().join(", ");
                }
                let variant_name = uppercase_last_segment(&raw_name);
                let inner = ts.patterns.iter()
                    .map(|p| render_match_pat(&Some(Box::new(p.clone()))))
                    .collect::<Vec<_>>().join(", ");
                format!("{}({})", variant_name, inner)
            }
            _ => "else".to_string(),
        },
        None => "else".to_string(),
    }
}

/// Check if a pattern is a TupleStruct with a matching name (Some, Ok, Err).
fn is_tuple_struct_binding(pat: &Option<fp_core::ast::BPattern>, names: &[&str]) -> bool {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::TupleStruct(ts) => {
                let raw = ts.name.to_string();
                let simple = raw.rsplit("::").next().unwrap_or(&raw);
                names.contains(&simple) && ts.patterns.len() == 1
                    && matches!(&ts.patterns[0].kind, PatternKind::Ident(_))
            }
            _ => false,
        },
        None => false,
    }
}

/// If this pattern is `Enum::Variant(binding)` for a single-field, NON-monadic
/// variant (not Some/Ok/Err/None — those get the null-check fast path instead),
/// return the Kotlin sealed-subclass path (`Enum.VARIANT`) and the binding name.
/// Requires the pattern to be written with its enum-qualified path.
fn non_monadic_tuple_variant(pat: &Option<fp_core::ast::BPattern>) -> Option<(String, String)> {
    let p = pat.as_ref()?;
    let PatternKind::TupleStruct(ts) = &p.kind else { return None };
    if ts.patterns.len() != 1 {
        return None;
    }
    let raw = ts.name.to_string();
    if !raw.contains("::") {
        return None;
    }
    let simple = raw.rsplit("::").next().unwrap_or(&raw);
    if matches!(simple, "Some" | "Ok" | "Err" | "None") {
        return None;
    }
    let binding = match &unwrap_ref_pattern(&ts.patterns[0]).kind {
        PatternKind::Ident(id) => id.ident.name.clone(),
        _ => return None,
    };
    let variant_path = uppercase_last_segment(&raw).replace("::", ".");
    Some((variant_path, binding))
}

/// Strip `ref`/`ref mut` wrapper patterns (`Some(ref mut file)`) to get at the
/// underlying binding — these carry no Kotlin-relevant information (there's no
/// by-reference-vs-by-value binding distinction), but the binding-extraction
/// helpers below need to see through them to find the `Ident`/`Struct` pattern.
fn unwrap_ref_pattern(pat: &Pattern) -> &Pattern {
    match &pat.kind {
        PatternKind::Ref(r) => unwrap_ref_pattern(&r.pattern),
        _ => pat,
    }
}

/// Extract the inner binding name from a Some/Ok/Err TupleStruct pattern. A nested
/// tuple pattern (`Some((host, path))`) is returned as `"(host, path)"` — callers
/// distinguish this from a single binding by checking for a leading `(`.
fn stripped_tuple_binding(pat: &Option<fp_core::ast::BPattern>) -> Option<String> {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::TupleStruct(ts) => {
                if ts.patterns.len() == 1 {
                    match &unwrap_ref_pattern(&ts.patterns[0]).kind {
                        PatternKind::Ident(id) => Some(id.ident.name.clone()),
                        PatternKind::Tuple(t) => {
                            let names: Vec<String> = t.patterns.iter()
                                .map(|p| match &p.kind {
                                    PatternKind::Ident(id) => Some(id.ident.name.clone()),
                                    _ => None,
                                })
                                .collect::<Option<Vec<_>>>()?;
                            Some(format!("({})", names.join(", ")))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        None => None,
    }
}

/// Extract a binding variable name from the first struct field of a match arm.
fn match_case_binding(pat: &Option<fp_core::ast::BPattern>) -> Option<String> {
    match pat {
        Some(p) => match &unwrap_ref_pattern(p).kind {
            PatternKind::Ident(id) => Some(id.ident.name.clone()),
            PatternKind::Struct(s) => s.fields.first().map(|f| f.name.name.clone()),
            _ => None,
        },
        None => None,
    }
}

// ── Value ────────────────────────────────────────────────────────────────────

/// Render a u64 literal that may exceed `Long.MAX_VALUE`, reinterpreting the bit
/// pattern via Kotlin's unsigned-to-signed conversion (matches Rust's `as i64` cast
/// semantics used by hash/checksum constants).
fn render_int_literal_kt(v: u64) -> String {
    if v > i64::MAX as u64 {
        format!("{}UL.toLong()", v)
    } else {
        v.to_string()
    }
}

fn render_value(val: &Value) -> String {
    match val {
        Value::Bool(v) => v.value.to_string(),
        Value::Int(v) => v.value.to_string(),
        Value::UInt(v) => render_int_literal_kt(v.value),
        Value::BigInt(v) => {
            let s = v.value.to_string();
            if !s.starts_with('-') && s.parse::<i64>().is_err() {
                format!("{}UL.toLong()", s)
            } else {
                s
            }
        }
        Value::Decimal(v) => v.value.to_string(),
        Value::BigDecimal(v) => v.value.to_string(),
        Value::Char(v) => format!("'{}'", escape_char_for_kt(v.value)),
        Value::String(v) => format!("\"{}\"", escape_str_for_kt(&v.value)),
        Value::Unit(_) | Value::Null(_) | Value::None(_) => "null".to_string(),
        Value::Some(v) => render_value(&v.value),
        Value::Option(v) => v.value.as_ref().map(|i| render_value(i)).unwrap_or_else(|| "null".to_string()),
        Value::List(l) => {
            let items: Vec<String> = l.values.iter().map(|v| render_value(v)).collect();
            format!("listOf({})", items.join(", "))
        }
        Value::Map(m) => {
            let entries: Vec<String> = m.entries.iter()
                .map(|e| format!("{} to {}", render_value(&e.key), render_value(&e.value)))
                .collect();
            format!("mapOf({})", entries.join(", "))
        }
        Value::Tuple(t) => {
            let items: Vec<String> = t.values.iter().map(|v| render_value(v)).collect();
            format!("Pair({})", items.join(", "))
        }
        _ => "null".to_string(),
    }
}

fn escape_char_for_kt(c: char) -> String {
    match c {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        c if c.is_ascii_graphic() || c == ' ' => c.to_string(),
        c => format!("\\u{:04X}", c as u32),
    }
}

fn escape_str_for_kt(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            // Unicode escapes: convert raw unicode chars to Kotlin \\uXXXX
            c if !c.is_ascii_graphic() && c != ' ' => {
                out.push_str(&format!("\\u{:04X}", c as u32));
            }
            _ => out.push(c),
        }
    }
    out
}

// ── Operators ────────────────────────────────────────────────────────────────

fn kotlin_bin_op(kind: &BinOpKind) -> &str {
    match kind {
        BinOpKind::Add => "+", BinOpKind::Sub => "-", BinOpKind::Mul => "*", BinOpKind::Div => "/",
        BinOpKind::Mod => "%", BinOpKind::And | BinOpKind::BitAnd => "&&",
        BinOpKind::Or | BinOpKind::BitOr => "||", BinOpKind::Eq => "==", BinOpKind::Ne => "!=",
        BinOpKind::Lt => "<", BinOpKind::Gt => ">", BinOpKind::Le => "<=", BinOpKind::Ge => ">=",
        BinOpKind::Shl => "shl", BinOpKind::Shr => "shr", BinOpKind::BitXor => "xor",
        _ => "?",
    }
}

fn kotlin_un_op(kind: &UnOpKind) -> &str {
    match kind { UnOpKind::Not => "!", UnOpKind::Neg => "-", UnOpKind::Deref => "*", _ => "?" }
}

fn intrinsic_name(kind: &fp_core::intrinsics::calls::CallKind) -> String {
    use fp_core::intrinsics::calls::OpKind;
    use fp_core::intrinsics::calls::IntrinsicKind;
    match kind {
        fp_core::intrinsics::calls::CallKind::Op(op) => match op {
            OpKind::Print => "print".into(),
            OpKind::Println => "println".into(),
            OpKind::Format => "String.format".into(),
            _ => format!("op_{:?}", op).to_lowercase(),
        },
        fp_core::intrinsics::calls::CallKind::Intrinsic(i) => match i {
            IntrinsicKind::Len => "count()".into(),
            IntrinsicKind::Panic => "error".into(),
            _ => format!("intr_{:?}", i).to_lowercase(),
        },
    }
}

// ── Type mapping ─────────────────────────────────────────────────────────────

fn kotlin_type_from_ty(ty: &Ty, _e: &KotlinEmitter) -> String {
    match ty {
        Ty::Primitive(prim) => match prim {
            TypePrimitive::Bool => "Boolean".into(),
            TypePrimitive::Char => "Char".into(),
            TypePrimitive::String => "String".into(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => "Byte".into(), TypeInt::I16 => "Short".into(),
                TypeInt::I32 => "Int".into(), TypeInt::I64 => "Long".into(),
                TypeInt::U8 => "Int".into(), TypeInt::U16 => "Int".into(),
                TypeInt::U32 => "Long".into(), TypeInt::U64 => "Long".into(),
                _ => "Int".into(),
            },
            TypePrimitive::Decimal(d) => match d {
                fp_core::ast::DecimalType::F32 => "Float".into(),
                fp_core::ast::DecimalType::F64 => "Double".into(),
                _ => "Double".into(),
            },
            TypePrimitive::List => "List<Any>".into(),
        },
        Ty::Vec(v) => format!("List<{}>", kotlin_type_from_ty(&v.ty, _e)),
        // Kotlin only has built-in tuple types up to 3 elements (Pair/Triple);
        // anything wider needs a real named type — see ExprKind::Tuple in
        // render_expr for the matching value-construction side.
        Ty::Tuple(t) => match t.types.len() {
            2 => format!("Pair<{}, {}>", kotlin_type_from_ty(&t.types[0], _e), kotlin_type_from_ty(&t.types[1], _e)),
            3 => format!("Triple<{}, {}, {}>", kotlin_type_from_ty(&t.types[0], _e), kotlin_type_from_ty(&t.types[1], _e), kotlin_type_from_ty(&t.types[2], _e)),
            _ => "Any".into(),
        },
        Ty::Struct(s) => s.name.name.clone(),
        Ty::Enum(en) => en.name.name.clone(),
        Ty::Reference(r) => kotlin_type_from_ty(&r.ty, _e),
        Ty::Expr(expr) => map_name_to_kt(&expr_to_name(expr)),
        Ty::Unit(_) => "Unit".into(),
        Ty::Slice(sl) => format!("List<{}>", kotlin_type_from_ty(&sl.elem, _e)),
        Ty::Any(_) | Ty::Unknown(_) => "Any".into(),
        Ty::Nothing(_) => "Nothing".into(),
        _ => "Any".into(),
    }
}

fn expr_to_name(expr: &Expr) -> String {
    match expr.kind() {
        ExprKind::Name(name) => name_to_string(name),
        ExprKind::Select(sel) => {
            format!("{}.{}", expr_to_name(&sel.obj), sel.field.name.as_str())
        }
        _ => format!("Any"),
    }
}

fn name_to_string(name: &fp_core::ast::Name) -> String {
    use fp_core::ast::Name::*;
    match name {
        Ident(id) => id.name.clone(),
        Path(p) => p.segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("."),
        ParameterPath(pp) => {
            let base = pp.segments.iter()
                .map(|s| {
                    let name = s.ident.name.as_str();
                    if s.args.is_empty() { name.to_string() }
                    else {
                        let args = s.args.iter()
                            .map(|ty| kotlin_type_from_ty(ty, &KotlinEmitter::new()))
                            .collect::<Vec<_>>().join(", ");
                        format!("{}<{}>", name, args)
                    }
                })
                .collect::<Vec<_>>().join(".");
            base
        },
    }
}

fn kotlin_type_from_ty_slot(ty: &TySlot, e: &KotlinEmitter) -> Option<String> {
    match ty {
        Some(t) => {
            let raw = kotlin_type_from_ty(t, e);
            if raw == "Any" || raw == "Nothing" || raw == "Unit" { None } else { Some(raw) }
        }
        None => None,
    }
}

/// If `dot_name` is `wrapper<Inner>` or `some.qualifier.wrapper<Inner>` (a generic
/// wrapper at a dot-segment boundary, possibly qualified by a module path), return
/// `Inner`. Handles qualified paths like `std.io.Result<Unit>`, not just bare
/// `Result<...>`.
fn strip_generic_wrapper<'a>(dot_name: &'a str, wrapper: &str) -> Option<&'a str> {
    let pat = format!("{}<", wrapper);
    let idx = dot_name.rfind(&pat)?;
    if idx != 0 && dot_name.as_bytes()[idx - 1] != b'.' {
        return None;
    }
    dot_name[idx + pat.len()..].strip_suffix('>')
}

fn map_name_to_kt(name: &str) -> String {
    // Normalize :: separators to dots for path resolution
    let dot_name = name.replace("::", ".");
    let last_seg = dot_name.rsplit('.').next().unwrap_or(&dot_name);

    if dot_name.starts_with("std.env") {
        return "System".into();
    }

    // Generic wrapper simplifications — match on the last dotted segment before `<`
    // so qualified paths like `std::io::Result<Unit>` also unwrap correctly, not
    // just bare `Result<...>`. Must run before KnownPackage resolution below, or a
    // qualified path like `std::io::Result<()>` gets misclassified as plain `std::io`.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Vec") {
        return format!("MutableList<{}>", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Option") {
        return format!("{}?", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Arc") {
        return map_name_to_kt(inner);
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Box") {
        return map_name_to_kt(inner);
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Result").and_then(|s| {
        // Result<T, E> → just T
        let comma = s.find(',')?;
        Some(&s[..comma])
    }) {
        return map_name_to_kt(inner);
    }
    // winnow's `ModalResult<T>` (≈ `Result<T, ContextError>`) — single type
    // argument, unlike `Result`, so just unwrap to T directly.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "ModalResult") {
        return map_name_to_kt(inner);
    }

    // KnownPackage-based resolution (skips language-internal crates)
    match known_package(&dot_name) {
        KnownPackage::StdPath => return kt_type_for_class(KnownClass::Path),
        KnownPackage::StdProcess => return "ProcessBuilder".into(),
        KnownPackage::StdFs => return "Path".into(),
        // "java.io.*" is a glob import, not a valid type — use a concrete class here.
        KnownPackage::StdIo => return "java.io.IOException".into(),
        KnownPackage::StdCollections | KnownPackage::StdStr | KnownPackage::StdOption
        | KnownPackage::StdSync | KnownPackage::Serde | KnownPackage::Winnow
        | KnownPackage::ThisError | KnownPackage::Tracing | KnownPackage::AsyncTrait
        | KnownPackage::Anyhow | KnownPackage::Unsupported => return "Any".into(),
        _ => {}
    }

    // KnownClass resolution (portable type descriptors from fp-core)
    if let Some(kc) = KnownClass::from_source_type(last_seg) {
        return kt_type_for_class(kc);
    }

    // Primitive type resolution
    match last_seg {
        "str" | "String" => return "String".into(),
        "char" => return "Char".into(),
        "bool" => return "Boolean".into(),
        "i8" => return "Byte".into(), "i16" => return "Short".into(),
        "i32" => return "Int".into(), "i64" => return "Long".into(),
        "u8" => return "Int".into(), "u16" => return "Int".into(),
        "u32" => return "Long".into(), "u64" => return "Long".into(),
        "f32" => return "Float".into(), "f64" => return "Double".into(),
        "usize" => return "Long".into(), "isize" => return "Long".into(),
        _ => {}
    }

    // Preserve path structure for workspace packages and unresolved names
    name.to_string()
}

/// Map a KnownClass descriptor to its Kotlin type representation.
fn kt_type_for_class(kc: KnownClass) -> String {
    use KnownClass::*;
    match kc {
        Path => "Path".into(),
        Instant => "java.time.Instant".into(),
        Duration => "java.time.Duration".into(),
        LocalDateTime => "java.time.LocalDateTime".into(),
        UtcDateTime => "java.time.ZonedDateTime".into(),
        Date => "java.time.LocalDate".into(),
        IpAddr => "java.net.InetAddress".into(),
        TcpStream => "java.net.Socket".into(),
        TcpListener => "java.net.ServerSocket".into(),
        UdpSocket => "java.net.DatagramSocket".into(),
        FileHandle => "Path".into(),
        IoStream => "java.io.InputStream".into(),
        ChildProcess => "java.lang.Process".into(),
        ExitCode => "Int".into(),
    }
}
