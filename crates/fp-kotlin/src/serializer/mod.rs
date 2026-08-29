use eyre::Result;
use fp_core::ast::package::{AstPackage, PackageItem};
use fp_core::ast::{
    BExpr, BlockStmt, Expr, ExprInvokeTarget, ExprKind, File, FormatArgRef, FormatTemplatePart,
    Item, ItemDefEnum, ItemDefFunction, ItemDefStruct, ItemImport, ItemKind, Pattern, PatternKind,
    Ty, TypeInt, TypePrimitive, Value,
};
use fp_core::backend::{BackendConfig, PackageWriter, TargetBackend};
use fp_core::intrinsics::calls::{KnownClass, KnownPackage};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::writer::{IndentStyle, StyledWriter, WriterConfig};
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;

mod backend;
mod collections;
mod expressions;
mod naming;
pub use backend::KotlinBackend;
use collections::sized_collection_element_type;
pub use collections::{
    collect_enum_field_names, collect_enum_variant_names, collect_list_field_names,
    collect_mutated_field_names, collect_string_field_names,
};
use expressions::*;
use naming::*;

// ── Emitter context ──────────────────────────────────────────────────────────

struct KotlinEmitter {
    writer: StyledWriter,
    var_counter: usize,
    /// Names of sibling modules generated into the same (default) Kotlin package —
    /// imports targeting these are skipped since they're already visible.
    local_modules: HashSet<String>,
    /// Names of sibling packages selected for this compile.
    workspace_packages: HashSet<String>,
    /// Kotlin package name for every selected Rust crate, keyed by either its
    /// Cargo spelling (`skln-git`) or Rust path spelling (`skln_git`).
    workspace_kotlin_packages: HashMap<String, String>,
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
    /// Workspace-wide struct field names whose Kotlin type is `String` — Rust's
    /// `.clone()` maps to Kotlin's `.copy()` (a data-class convention) by
    /// default, but `String` has no `.copy()` (already immutable, the call
    /// should just drop). Same workspace-wide-registry shape as
    /// `field_element_types`/`collect_list_field_names`, see
    /// `collect_string_field_names`.
    string_field_names: HashSet<String>,
    /// Workspace-wide struct field names whose declared type is an `enum
    /// class` — same rationale and shape as `string_field_names`, backing
    /// the same `.clone()`-vs-`.copy()` disambiguation (`is_known_enum_receiver`)
    /// for a field whose defining struct lives in a different package than
    /// the file reading it (so `expr.ty()` alone isn't reliably populated).
    enum_field_names: HashSet<String>,
    /// Names of the currently-emitted function's own parameters — Kotlin
    /// parameters are always an implicit `val` (unlike Rust, which allows
    /// `mut`/`&mut` parameters), so `.take()` on one of these (see its
    /// render_expr special case) can't emit its usual reset-to-`null`
    /// side effect; the parameter's Kotlin copy dies at function return
    /// regardless, so dropping the reset is safe as long as nothing later
    /// in the same function body reads the parameter again expecting to
    /// see the reset value — true for every case in this codebase today.
    /// Reset per function (see `emit_function`/`emit_impl_function`).
    current_fn_params: HashSet<String>,
    /// The real class/enum name `Self` refers to inside the body of the
    /// impl method currently being emitted (`None` outside any impl method,
    /// e.g. while emitting a plain top-level function) — Rust's `Self {
    /// field: value }` constructor shorthand and `Self::other_fn()` calls
    /// both need the real name substituted in, since Kotlin has no `Self`
    /// expression-position equivalent. Set by `emit_companion_function`/
    /// `emit_impl_function`, cleared by `emit_function`.
    current_self_name: Option<String>,
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
    /// Dotted qualified paths (e.g. `"std.collections.HashMap"`) this file's
    /// content actually references, computed from typed HIR
    /// (`AstPackage::referenced_paths`) rather than the file's own
    /// pre-existing `use` items — lets `emit_file` add an import for a
    /// std-equivalent type even when the source's own `use` list doesn't
    /// (already-)cover it (e.g. spliced-in content whose path shape
    /// differs from how it was originally written). Same-package,
    /// cross-module references are deliberately excluded before this
    /// reaches the emitter (see `serialize_package`) — those never need a
    /// Kotlin import (no generated file declares a `package`).
    referenced_paths: HashSet<String>,
    /// Workspace-wide `enum_name -> (rust_variant_name -> kotlin_variant_name)`
    /// registry — see `collect_enum_variant_names`'s doc comment. Consulted by
    /// `render_match_pat` instead of re-deriving a variant's Kotlin name from
    /// its own (possibly differently-qualified) pattern text.
    enum_variant_names: HashMap<String, HashMap<String, String>>,
}

fn kotlin_writer_config() -> WriterConfig {
    WriterConfig {
        indent_style: IndentStyle::Spaces(4),
        ..WriterConfig::default()
    }
}

impl KotlinEmitter {
    fn new() -> Self {
        Self {
            writer: StyledWriter::new(kotlin_writer_config()),
            var_counter: 0,
            local_modules: HashSet::new(),
            workspace_packages: HashSet::new(),
            workspace_kotlin_packages: HashMap::new(),
            mutated_fields: HashSet::new(),
            long_field_names: HashSet::new(),
            pending_assert_long: false,
            split_iter_vars: HashSet::new(),
            next_call_counters: HashMap::new(),
            field_element_types: HashMap::new(),
            string_field_names: HashSet::new(),
            enum_field_names: HashSet::new(),
            current_fn_params: HashSet::new(),
            current_self_name: None,
            declared_names: Vec::new(),
            referenced_paths: HashSet::new(),
            enum_variant_names: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.declared_names.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.declared_names.pop();
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
        if self.var_counter == 1 {
            base.to_string()
        } else {
            format!("{}{}", base, self.var_counter)
        }
    }
}

// ── Serializer entry ─────────────────────────────────────────────────────────

/// Cross-package facts the Kotlin backend needs before it can serialize any
/// single package: field mutability (`val` vs `var`) and List-vs-String
/// disambiguation (`.len()` -> `.size` not `.length`, range-index ->
/// `.subList` not `.substring`) are both decided workspace-wide, since a
/// struct's fields can be defined in one package and mutated/read from
/// another — see the individual `collect_*` functions' doc comments for why
/// each field is needed. Kotlin is the only backend that needs anything
/// beyond a single `AstPackage` to serialize a package; this groups what
/// would otherwise be five loose values plus a separately-merged map into
/// one value threaded through `serialize_package`.
#[derive(Default)]
pub struct KotlinWorkspaceContext {
    pub mutated_fields: HashSet<String>,
    pub list_fields: HashMap<String, String>,
    pub string_fields: HashSet<String>,
    pub enum_fields: HashSet<String>,
    pub enum_variant_names: HashMap<String, HashMap<String, String>>,
    /// `AstPackage::referenced_paths` merged across every package in the
    /// workspace — each item's own qualified path (module + name) mapped to
    /// the qualified paths it references.
    pub referenced_paths: HashMap<Vec<String>, Vec<Vec<String>>>,
}

impl KotlinWorkspaceContext {
    /// Collects every workspace-wide fact from every package's items in one
    /// pass. `sources` must be cheaply cloneable (e.g. `sources.iter()` on a
    /// slice) since each fact is collected via its own full traversal.
    pub fn collect<'a>(sources: impl Iterator<Item = &'a AstPackage> + Clone) -> Self {
        let items = || sources.clone().flat_map(|src| src.items.iter());
        let mutated_fields = collect_mutated_field_names(items());
        let list_fields = collect_list_field_names(items());
        let string_fields = collect_string_field_names(items());
        let enum_fields = collect_enum_field_names(items());
        let enum_variant_names = collect_enum_variant_names(items());
        let referenced_paths = sources
            .flat_map(|src| src.referenced_paths.iter())
            .map(|(path, refs)| (path.clone(), refs.clone()))
            .collect();
        Self {
            mutated_fields,
            list_fields,
            string_fields,
            enum_fields,
            enum_variant_names,
            referenced_paths,
        }
    }
}

pub struct KotlinSerializer;

impl KotlinSerializer {
    pub fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = KotlinEmitter::new();
        emitter.emit_file(file)?;
        let mut out = String::from("// Generated by FerroPhase — Kotlin target\n\n");
        out.push_str(&emitter.writer.finish());
        Ok(out)
    }

    /// Serialize a package into per-module Kotlin files with Gradle manifest.
    /// `workspace_packages` is the full set of sibling package names in this
    /// compile (e.g. every other Cargo crate in the same `magnet transpile`
    /// run) — used to recognize cross-package imports within the workspace.
    /// `ctx` is the workspace-wide state described by `KotlinWorkspaceContext`.
    /// Returns `Vec<(relative_path, code)>` — source files + build files.
    pub fn serialize_package(
        &self,
        source: &AstPackage,
        workspace_packages: &HashSet<String>,
        workspace_kotlin_packages: &HashMap<String, String>,
        ctx: &KotlinWorkspaceContext,
    ) -> Result<Vec<(String, String)>> {
        let KotlinWorkspaceContext {
            mutated_fields,
            list_fields,
            string_fields,
            enum_fields,
            enum_variant_names,
            referenced_paths,
        } = ctx;
        let modules = fp_core::ast::package::split_package_into_modules(source);

        let pkg_name = &source.name;
        let mut files = Vec::new();

        // Collect cross-package dependencies from imports
        let deps = collect_workspace_deps(&source.items, pkg_name, workspace_packages);

        // Sibling modules share this crate's generated Kotlin package, so no
        // import is needed for them.
        let local_modules: HashSet<String> = modules
            .iter()
            .map(|module| {
                module
                    .path
                    .segments
                    .last()
                    .cloned()
                    .unwrap_or_else(|| module.relative_path())
            })
            .collect();

        // Gradle manifest
        files.push(("settings.gradle.kts".into(), settings_gradle(pkg_name)));
        files.push(("build.gradle.kts".into(), build_gradle(pkg_name, &deps)));

        // Source files under src/main/kotlin/
        for module in modules {
            let mod_path = module.relative_path();
            let output_name = if mod_path.is_empty() {
                "root".to_string()
            } else {
                mod_path.clone()
            };
            let file = File {
                path: std::path::PathBuf::from(&mod_path),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: module.items,
            };
            // Every referenced-path entry is keyed by the REFERENCING
            // item's own qualified path (module segments + name) — take
            // everything referenced by any item whose containing module
            // is this file, i.e. whose key's segments-minus-last equal
            // this file's own module path.
            let module_segments: Vec<String> = if mod_path.is_empty() {
                Vec::new()
            } else {
                mod_path.split('/').map(str::to_string).collect()
            };
            let file_referenced_paths: HashSet<String> = referenced_paths
                .iter()
                .filter(|(item_path, _)| {
                    item_path.len() == module_segments.len() + 1
                        && item_path[..module_segments.len()] == module_segments[..]
                })
                .flat_map(|(_, refs)| refs.iter())
                .map(|path| path.join("."))
                .collect();
            let mut emitter = KotlinEmitter::new();
            emitter.local_modules = local_modules.clone();
            emitter.workspace_packages = workspace_packages.clone();
            emitter.workspace_kotlin_packages = workspace_kotlin_packages.clone();
            emitter.mutated_fields = mutated_fields.clone();
            emitter.field_element_types = list_fields.clone();
            emitter.string_field_names = string_fields.clone();
            emitter.enum_field_names = enum_fields.clone();
            emitter.referenced_paths = file_referenced_paths;
            emitter.enum_variant_names = enum_variant_names.clone();
            emitter
                .emit_file(&file)
                .map_err(|e| eyre::eyre!("serialize {}: {}", mod_path, e))?;
            let mut code = String::from("// Generated by FerroPhase — Kotlin target\n\n");
            if let Some(package) = workspace_kotlin_packages.get(pkg_name) {
                code.push_str("package ");
                code.push_str(package);
                code.push_str("\n\n");
            }
            code.push_str(&emitter.writer.finish());
            let out_path = format!("src/main/kotlin/{}.kt", output_name);
            files.push((out_path, code));
        }
        Ok(files)
    }
}

/// The workspace-wide facts `KotlinBackend` needs beyond a single
/// package's own `AstPackage` — computed lazily (see `ensure_scan`)
/// from `&AstProgram` on first use and cached, instead of being
/// force-fed at construction time. `workspace_packages` comes from
/// `AstProgram::workspace_packages()` (in turn
/// `PackageProvider::workspace_packages()`) rather than being passed by
/// the caller — the provider is the thing that actually knows which
/// packages are this workspace's own, as opposed to e.g. `std`.
fn settings_gradle(name: &str) -> String {
    format!("rootProject.name = \"{}\"\n", name.replace('-', "_"))
}

fn build_gradle(name: &str, deps: &[String]) -> String {
    let group = format!("com.{}", name.replace('-', "."));
    let dep_lines: String = deps
        .iter()
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
            if d.as_str() == pkg_name {
                return false;
            }
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
            if !path.starts_with("std.")
                && !path.starts_with("serde")
                && !path.starts_with("winnow")
                && !path.starts_with('.')
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
            for child in &m.items {
                collect_deps_from_item(child, deps);
            }
        }
        ItemKind::DefFunction(f) => {
            for stmt in &f.body.stmts {
                collect_deps_from_stmt(stmt, deps);
            }
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

/// Kotlin has no scoped/local imports — a Rust `use` statement written
/// inside a function body (valid, block-scoped Rust syntax) must still be
/// hoisted to the file's top-level `import` list rather than emitted where
/// it's found (see `emit_item`'s `ItemKind::Import` arm, which is a no-op
/// specifically because every import — top-level or nested — is collected
/// here first). Mirrors `collect_deps_from_item`'s recursive shape.
fn collect_nested_imports_from_item(item: &Item, imports: &mut Vec<ItemImport>) {
    match item.kind() {
        ItemKind::Import(imp) => imports.push(imp.clone()),
        ItemKind::Module(m) => {
            for child in &m.items {
                collect_nested_imports_from_item(child, imports);
            }
        }
        ItemKind::DefFunction(f) => {
            for stmt in &f.body.stmts {
                collect_nested_imports_from_stmt(stmt, imports);
            }
        }
        _ => {}
    }
}

fn collect_nested_imports_from_stmt(stmt: &BlockStmt, imports: &mut Vec<ItemImport>) {
    if let BlockStmt::Item(item) = stmt {
        collect_nested_imports_from_item(item, imports);
    }
}

impl KotlinEmitter {
    fn emit_file(&mut self, file: &File) -> Result<()> {
        let mut imports = Vec::new();
        let mut non_imports = Vec::new();
        for item in &file.items {
            if let ItemKind::Import(imp) = item.kind() {
                imports.push(imp.clone());
            } else {
                collect_nested_imports_from_item(item, &mut imports);
                non_imports.push(item);
            }
        }
        let mut emitted_imports: HashSet<String> = HashSet::new();
        for imp in &imports {
            self.emit_import(imp, &mut emitted_imports)?;
        }
        self.emit_referenced_path_imports(&mut emitted_imports);
        if !emitted_imports.is_empty() {
            self.writer.write_line("");
        }

        // `impl` blocks are collected up front, keyed by the struct/enum name
        // they attach to, so a static method can be nested into that type's own
        // `data class`/`sealed class`/`enum class` declaration as a companion
        // object member — Kotlin has no way to attach a companion object to a
        // class from *outside* its declaration, unlike an extension function
        // (used for instance methods instead, emitted after every other item).
        // `impl Trait for Type` instance methods are collected separately from
        // plain inherent-impl instance methods: they become real `override`
        // members nested in the type's own declaration (with `: Trait` added to
        // its header) rather than extension functions, since Kotlin only
        // recognizes actual member overrides as satisfying an interface — see
        // `emit_trait_impl_block`.
        let mut static_methods: HashMap<String, Vec<ItemDefFunction>> = HashMap::new();
        let mut instance_methods: Vec<(ItemDefFunction, String)> = Vec::new();
        let mut trait_impls: HashMap<String, Vec<(String, ItemDefFunction)>> = HashMap::new();
        for item in &non_imports {
            collect_impl_methods(
                item,
                &mut static_methods,
                &mut instance_methods,
                &mut trait_impls,
            );
        }

        for item in non_imports {
            self.emit_item(item, &static_methods, &trait_impls)?;
        }

        for (f, self_name) in &instance_methods {
            self.emit_impl_function(f, self_name)?;
        }
        Ok(())
    }
}

/// True for a Rust standard-library trait with its own dedicated Kotlin
/// convention — `Display`/`Debug` → `toString()`, `Clone` → data-class
/// `.copy()`, `PartialEq`/`Eq` → `equals()`, etc. — rather than a generic
/// "this type implements that interface" relationship. Matched by last
/// path segment (`std::fmt::Display` and a bare `Display` both match).
/// `trait_name` comes from `name_to_string`, which renders a qualified
/// `Name::Path` with `.` separators, not `::` — split on both, since a
/// `Name::ParameterPath` (or a manually-built `Name`) could still use `::`.
fn is_known_std_trait(trait_name: &str) -> bool {
    let last = trait_name.rsplit(['.', ':']).next().unwrap_or(trait_name);
    matches!(
        last,
        "Display"
            | "Debug"
            | "Clone"
            | "Copy"
            | "Default"
            | "PartialEq"
            | "Eq"
            | "PartialOrd"
            | "Ord"
            | "Hash"
            | "From"
            | "Into"
            | "TryFrom"
            | "TryInto"
            | "Send"
            | "Sync"
            | "Drop"
            | "Iterator"
            | "IntoIterator"
            | "AsRef"
            | "AsMut"
            | "Deref"
            | "DerefMut"
            | "Serialize"
            | "Deserialize"
    )
}

fn collect_impl_methods(
    item: &Item,
    static_methods: &mut HashMap<String, Vec<ItemDefFunction>>,
    instance_methods: &mut Vec<(ItemDefFunction, String)>,
    trait_impls: &mut HashMap<String, Vec<(String, ItemDefFunction)>>,
) {
    match item.kind() {
        ItemKind::Impl(impl_block) => {
            let self_name = expr_to_name(&impl_block.self_ty);
            // Strip any generic argument suffix (`Foo<T>` → `Foo`) so this
            // matches the bare name `emit_struct`/`emit_enum` are keyed by.
            let self_name = self_name
                .split('<')
                .next()
                .unwrap_or(&self_name)
                .to_string();
            // Well-known standard traits (`Clone`, `PartialEq`, ...) have
            // their own dedicated Kotlin conventions (data-class `.copy()`,
            // `equals()`, ...), not a generic "implements this interface"
            // relationship — only a genuine custom (locally-defined) trait
            // becomes a Kotlin `interface` its implementors declare.
            // Anything else keeps the prior, already-working behavior (an
            // ordinary instance method, emitted as an extension function).
            // `Display`/`Debug` are the one exception: their Kotlin
            // convention (`toString()`) can ONLY be satisfied by a real
            // class-member override (see `emit_fmt_as_to_string`'s doc
            // comment) — unlike an ordinary custom trait, `implemented_
            // trait_names` still excludes them from the `: Trait1, Trait2`
            // supertype list, since Kotlin has no such interface to name.
            let trait_name = impl_block
                .trait_ty
                .as_ref()
                .map(name_to_string)
                .filter(|name| {
                    let last = name.rsplit(['.', ':']).next().unwrap_or(name);
                    matches!(last, "Display" | "Debug") || !is_known_std_trait(name)
                });
            for item in &impl_block.items {
                if let ItemKind::DefFunction(f) = item.kind() {
                    if f.sig.receiver.is_none() {
                        static_methods
                            .entry(self_name.clone())
                            .or_default()
                            .push(f.clone());
                    } else if let Some(trait_name) = &trait_name {
                        trait_impls
                            .entry(self_name.clone())
                            .or_default()
                            .push((trait_name.clone(), f.clone()));
                    } else {
                        instance_methods.push((f.clone(), self_name.clone()));
                    }
                }
            }
        }
        ItemKind::Module(m) => {
            for child in &m.items {
                collect_impl_methods(child, static_methods, instance_methods, trait_impls);
            }
        }
        _ => {}
    }
}

impl KotlinEmitter {
    fn emit_item(
        &mut self,
        item: &Item,
        static_methods: &HashMap<String, Vec<ItemDefFunction>>,
        trait_impls: &HashMap<String, Vec<(String, ItemDefFunction)>>,
    ) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(s) => {
                let methods = static_methods
                    .get(s.name.name.as_str())
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                let traits = trait_impls
                    .get(s.name.name.as_str())
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                self.emit_struct(s, methods, traits)
            }
            ItemKind::DefEnum(en) => {
                let methods = static_methods
                    .get(en.name.name.as_str())
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                let traits = trait_impls
                    .get(en.name.name.as_str())
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                self.emit_enum(en, methods, traits)
            }
            ItemKind::DefFunction(f) => self.emit_function(f),
            ItemKind::Module(m) => {
                for child in &m.items {
                    self.emit_item(child, static_methods, trait_impls)?;
                }
                Ok(())
            }
            // Only ever reached in statement position (a block-scoped Rust
            // `use`) — `emit_file` already hoisted every import, top-level
            // or nested, to the file's top via `collect_nested_imports_from_item`.
            // Kotlin has no scoped imports, so there is nothing to emit here.
            ItemKind::Import(_) => Ok(()),
            ItemKind::DefConst(c) => {
                let name = c.name.name.as_str();
                let val = self.render_expr(&c.value)?;
                self.writer.write_line(format!("val {} = {}", name, val));
                Ok(())
            }
            ItemKind::DefTrait(t) => self.emit_trait(t),
            ItemKind::Macro(_) | ItemKind::DefStructural(_) => Ok(()),
            // Handled up front by `collect_impl_methods` (see `emit_file`).
            ItemKind::Impl(_) => Ok(()),
            ItemKind::Expr(expr) => {
                if let ExprKind::Block(block) = expr.kind() {
                    for stmt in &block.stmts {
                        self.emit_stmt(stmt, Tail::None)?;
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    // ── Trait ────────────────────────────────────────────────────────────

    /// A Rust trait becomes a Kotlin `interface` — its method *signatures*
    /// become abstract interface methods; a default-bodied method (rare in
    /// this codebase, but real Kotlin interfaces support it) keeps its body.
    /// Implementing types get `: TraitName` added to their own declaration and
    /// their trait-impl methods emitted as real overrides — see
    /// `emit_trait_impl_block`.
    fn emit_trait(&mut self, t: &fp_core::ast::ItemDefTrait) -> Result<()> {
        let name = t.name.name.as_str();
        self.writer.write_line(format!("interface {} {{", name));
        self.writer.increase_indent();
        for item in &t.items {
            match item.kind() {
                ItemKind::DeclFunction(f) => {
                    let params = f
                        .sig
                        .params
                        .iter()
                        .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret = f
                        .sig
                        .ret_ty
                        .as_ref()
                        .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty)))
                        .unwrap_or_else(|| ": Unit".to_string());
                    let fn_kw = if f.is_async { "suspend fun" } else { "fun" };
                    self.writer.write_line(format!(
                        "{} {}({}){}",
                        fn_kw,
                        f.name.name.as_str(),
                        params,
                        ret
                    ));
                }
                ItemKind::DefFunction(f) => {
                    // A default method body — emit like any other instance
                    // method, just nested as an interface member instead of a
                    // free-standing extension function.
                    let name = name.to_string();
                    self.emit_companion_function(f, &name)?;
                }
                _ => {}
            }
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }

    /// Emits every `(trait_name, method)` in `traits` as a real `override fun`
    /// member (grouped so each trait's methods stay together), and returns the
    /// distinct trait names implemented — for the caller to add as `: Trait1,
    /// Trait2` on the type's own declaration. Must be called from inside the
    /// type's already-open body braces, same as `emit_companion_block`.
    fn emit_trait_impl_block(
        &mut self,
        self_name: &str,
        traits: &[(String, ItemDefFunction)],
    ) -> Result<()> {
        for (_, f) in traits {
            self.emit_override_function(f, self_name)?;
        }
        Ok(())
    }

    // ── Struct ───────────────────────────────────────────────────────────

    fn emit_struct(
        &mut self,
        s: &ItemDefStruct,
        static_methods: &[ItemDefFunction],
        traits: &[(String, ItemDefFunction)],
    ) -> Result<()> {
        let name = s.name.name.as_str();
        let fields = &s.value.fields;
        self.writer.write_line(format!("data class {}(", name));
        for (i, field) in fields.iter().enumerate() {
            let comma = if i < fields.len() - 1 { "," } else { "" };
            let kt = self.kotlin_type_from_ty(&field.value);
            if kt == "Long" {
                self.long_field_names.insert(field.name.name.clone());
            }
            if let Some(elem) = sized_collection_element_type(&kt) {
                self.field_element_types
                    .insert(field.name.name.clone(), elem.to_string());
            }
            let mutability = if self.mutated_fields.contains(&field.name.name) {
                "var"
            } else {
                "val"
            };
            self.writer.write_line(format!(
                "    {} {}: {}{}",
                mutability, field.name.name, kt, comma
            ));
        }
        if static_methods.is_empty() && traits.is_empty() {
            self.writer.write_line(")\n");
            return Ok(());
        }
        let implemented = implemented_trait_names(traits);
        let header_suffix = if implemented.is_empty() {
            String::new()
        } else {
            format!(" : {}", implemented.join(", "))
        };
        self.writer.write_line(format!("){} {{", header_suffix));
        self.writer.increase_indent();
        let name = name.to_string();
        self.emit_trait_impl_block(&name, traits)?;
        if !static_methods.is_empty() {
            self.emit_companion_block(&name, static_methods)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }

    // ── Enum ─────────────────────────────────────────────────────────────

    fn emit_enum(
        &mut self,
        en: &ItemDefEnum,
        static_methods: &[ItemDefFunction],
        traits: &[(String, ItemDefFunction)],
    ) -> Result<()> {
        let name = en.name.name.as_str().to_string();
        let variants = &en.value.variants;
        let is_error = derives_rust_error(&en.attrs);
        let has_data = is_error || variants.iter().any(|v| !matches!(v.value, Ty::Unit(_)));
        let implemented_traits = implemented_trait_names(traits);
        let mut supertypes = implemented_traits;
        if is_error {
            supertypes.push("Exception()".to_string());
        }
        let header_suffix = if supertypes.is_empty() {
            String::new()
        } else {
            format!(" : {}", supertypes.join(", "))
        };

        if has_data {
            self.writer
                .write_line(format!("sealed class {}{} {{", name, header_suffix));
            for (i, variant) in variants.iter().enumerate() {
                // Faithful to the Rust source name — Kotlin class/object names
                // are conventionally PascalCase anyway (matching a variant's
                // own casing), and every reference site (match patterns,
                // struct-literal construction, plain value references) reads
                // this same name back verbatim, so there's no separate
                // casing transform for those sites to independently reproduce.
                let vname = variant.name.name.clone();
                match &variant.value {
                    Ty::Unit(_) | Ty::Nothing(_) => {
                        self.writer
                            .write_line(format!("    object {} : {}()", vname, name));
                    }
                    Ty::Struct(s) => {
                        let fields: Vec<String> = s
                            .fields
                            .iter()
                            .map(|f| {
                                format!(
                                    "val {}: {}",
                                    f.name.name,
                                    self.kotlin_type_from_ty(&f.value)
                                )
                            })
                            .collect();
                        self.writer.write_line(format!(
                            "    data class {}({}) : {}()",
                            vname,
                            fields.join(", "),
                            name
                        ));
                    }
                    Ty::Structural(s) => {
                        let fields: Vec<String> = s
                            .fields
                            .iter()
                            .map(|f| {
                                format!(
                                    "val {}: {}",
                                    f.name.name,
                                    self.kotlin_type_from_ty(&f.value)
                                )
                            })
                            .collect();
                        self.writer.write_line(format!(
                            "    data class {}({}) : {}()",
                            vname,
                            fields.join(", "),
                            name
                        ));
                    }
                    Ty::Expr(expr) => {
                        let ty_str = self.kotlin_type_from_ty(&Ty::Expr(expr.clone()));
                        self.writer.write_line(format!(
                            "    data class {}(val __data: {}) : {}()",
                            vname, ty_str, name
                        ));
                    }
                    _ => {
                        self.writer.write_line(format!(
                            "    data class {}(vararg __data: Any?) : {}()",
                            vname, name
                        ));
                    }
                }
                if i < variants.len() - 1 {
                    self.writer.write_line("");
                }
            }
            if static_methods.is_empty() && traits.is_empty() {
                self.writer.write_line("}\n");
            } else {
                self.writer.increase_indent();
                self.emit_trait_impl_block(&name, traits)?;
                if !static_methods.is_empty() {
                    self.emit_companion_block(&name, static_methods)?;
                }
                self.writer.decrease_indent();
                self.writer.write_line("}\n");
            }
            Ok(())
        } else {
            self.writer
                .write_line(format!("enum class {}{} {{", name, header_suffix));
            for (i, variant) in variants.iter().enumerate() {
                let comma =
                    if i < variants.len() - 1 || !static_methods.is_empty() || !traits.is_empty() {
                        ","
                    } else {
                        ""
                    };
                self.writer
                    .write_line(format!("    {}{}", variant.name.name, comma));
            }
            if static_methods.is_empty() && traits.is_empty() {
                self.writer.write_line("}\n");
            } else {
                self.writer.write_line("    ;");
                self.writer.increase_indent();
                self.emit_trait_impl_block(&name, traits)?;
                if !static_methods.is_empty() {
                    self.emit_companion_block(&name, static_methods)?;
                }
                self.writer.decrease_indent();
                self.writer.write_line("}\n");
            }
            Ok(())
        }
    }

    /// Emits `companion object { ... }` with `static_methods` inside, at the
    /// current indent level — the caller must already have opened the enclosing
    /// class body's braces (and is responsible for closing them afterward). See
    /// `emit_file`'s doc comment on why this has to be nested here rather than
    /// emitted from the separate `impl` item.
    fn emit_companion_block(
        &mut self,
        self_name: &str,
        static_methods: &[ItemDefFunction],
    ) -> Result<()> {
        self.writer.write_line("companion object {");
        self.writer.increase_indent();
        for f in static_methods {
            self.emit_companion_function(f, self_name)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}");
        Ok(())
    }
}

/// Rust error enums derive `thiserror::Error` (or an imported `Error`). Kotlin's
/// standard `Result.failure` accepts only a `Throwable`, so preserve that source
/// declaration contract by making the generated sealed base an `Exception`.
/// This inspects the structured derive attribute, never a declaration name.
fn derives_rust_error(attrs: &[fp_core::ast::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        let fp_core::ast::AttrMeta::List(list) = &attr.meta else {
            return false;
        };
        list.name.last().as_str() == "derive"
            && list.items.iter().any(|item| {
                matches!(item, fp_core::ast::AttrMeta::Path(path) if path.last().as_str() == "Error")
            })
    })
}

/// The distinct trait names in `traits`, in first-seen order — for the
/// `: Trait1, Trait2` suffix on a type's own declaration. Must be computed
/// (and the header line written) *before* `emit_trait_impl_block` opens the
/// class body and starts writing member overrides into it. Excludes
/// `Display`/`Debug` — `collect_impl_methods` routes their `fmt` method
/// here too (for a real member override), but Kotlin has no such
/// interface to declare `: Display`/`: Debug` against.
fn implemented_trait_names(traits: &[(String, ItemDefFunction)]) -> Vec<String> {
    let mut seen = Vec::new();
    for (trait_name, _) in traits {
        let last = trait_name.rsplit(['.', ':']).next().unwrap_or(trait_name);
        if matches!(last, "Display" | "Debug") {
            continue;
        }
        if !seen.contains(trait_name) {
            seen.push(trait_name.clone());
        }
    }
    seen
}

// ── Function ─────────────────────────────────────────────────────────────────

/// `"fun"` or `"suspend fun"` — Rust's `async fn` maps to Kotlin's
/// `suspend fun`, and since `.await` already renders as just its inner
/// expression (a suspend function's call site *is* its await point in
/// Kotlin), that's the entire translation needed for the common
/// sequential-await case.
fn fn_kw(f: &ItemDefFunction) -> &'static str {
    if f.is_async { "suspend fun" } else { "fun" }
}

/// What should happen to a statement chain's final expression's value.
/// Threaded through the statement-emission methods instead of an
/// `is_tail: bool` so `if`/`when`/`if let` used as a *value* still emit
/// their branches as ordinary direct statements -- at the real depth, via
/// the same `self.writer.write_line`/`block` calls as everything else --
/// rather than being rendered to a string and re-spliced back in.
#[derive(Clone, Copy)]
enum Tail<'a> {
    /// Plain statement position -- the value (if any) is discarded.
    None,
    /// Function-body tail position -- wrap the value in `return`.
    Return,
    /// Assign the value to this already-declared variable.
    Assign(&'a str),
}

impl KotlinEmitter {
    /// Apply `tail` to an already-rendered value: discard it, `return` it,
    /// or assign it to an already-declared variable.
    fn write_tail(&mut self, tail: Tail, value: &str) {
        match tail {
            Tail::None => {
                self.writer.write_lines(value);
            }
            Tail::Return => {
                self.writer.write_lines(&format!("return {}", value));
            }
            Tail::Assign(name) => {
                self.writer.write_lines(&format!("{} = {}", name, value));
            }
        }
    }

    fn emit_companion_function(&mut self, f: &ItemDefFunction, self_name: &str) -> Result<()> {
        let name = f.name.name.as_str();
        self.current_fn_params = f.sig.params.iter().map(|p| p.name.name.clone()).collect();
        self.current_self_name = Some(self_name.to_string());
        let params = f
            .sig
            .params
            .iter()
            .map(|p| {
                format!(
                    "{}: {}",
                    p.name.name,
                    self.kotlin_type_from_ty(&p.ty).replace("Self", self_name)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        let ret = f
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| {
                format!(
                    ": {}",
                    self.kotlin_type_from_ty(ty).replace("Self", self_name)
                )
            })
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer
            .write_line(&format!("{} {}({}){} {{", fn_kw(f), name, params, ret));
        self.writer.increase_indent();
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let tail = if i == len - 1 && f.sig.ret_ty.is_some() {
                Tail::Return
            } else {
                Tail::None
            };
            self.emit_stmt(stmt, tail)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

impl KotlinEmitter {
    fn emit_impl_function(&mut self, f: &ItemDefFunction, self_name: &str) -> Result<()> {
        let name = f.name.name.as_str();
        if is_fmt_trait_method(f) {
            // Kotlin resolves a real member (`Any.toString()`) over an
            // extension function of the same name, so this inherent-impl
            // path (unlike `emit_override_function`, a genuine `impl Display`
            // always goes through) can't make `toString()` actually dispatch
            // polymorphically — it still emits valid, callable code, just
            // not a true override. See `emit_fmt_as_to_string`.
            self.current_self_name = Some(self_name.to_string());
            self.emit_fmt_as_to_string(f, Some(self_name))?;
            return Ok(());
        }
        self.current_fn_params = f.sig.params.iter().map(|p| p.name.name.clone()).collect();
        self.current_self_name = Some(self_name.to_string());
        // Skip the first param (self) — Kotlin extension functions have implicit receiver
        let params = f
            .sig
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
            .collect::<Vec<_>>()
            .join(", ");
        let ret = f
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| {
                format!(
                    ": {}",
                    self.kotlin_type_from_ty(ty).replace("Self", self_name)
                )
            })
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!(
            "{} {}.{}({}){} {{",
            fn_kw(f),
            self_name,
            name,
            params,
            ret
        ));
        self.writer.increase_indent();
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let tail = if i == len - 1 && f.sig.ret_ty.is_some() {
                Tail::Return
            } else {
                Tail::None
            };
            self.emit_stmt(stmt, tail)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

/// Like `emit_impl_function`, but nested as a real class member
/// (`override fun name(...)`) instead of an extension function — needed
/// for a trait's methods specifically, since Kotlin only recognizes an
/// actual member override as satisfying an interface (an extension
/// function never does, regardless of its name/signature match).
impl KotlinEmitter {
    fn emit_override_function(&mut self, f: &ItemDefFunction, self_name: &str) -> Result<()> {
        let name = f.name.name.as_str();
        if is_fmt_trait_method(f) {
            // `toString()` is the one Kotlin name that must go through a real
            // class-member override (satisfying `Any.toString()`), which is
            // exactly this path — a trait impl's methods are always emitted
            // here. See `emit_fmt_as_to_string`.
            self.current_self_name = Some(self_name.to_string());
            self.emit_fmt_as_to_string(f, None)?;
            return Ok(());
        }
        self.current_fn_params = f.sig.params.iter().map(|p| p.name.name.clone()).collect();
        self.current_self_name = Some(self_name.to_string());
        let params = f
            .sig
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
            .collect::<Vec<_>>()
            .join(", ");
        let ret = f
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| {
                format!(
                    ": {}",
                    self.kotlin_type_from_ty(ty).replace("Self", self_name)
                )
            })
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!(
            "override {} {}({}){} {{",
            fn_kw(f),
            name,
            params,
            ret
        ));
        self.writer.increase_indent();
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let tail = if i == len - 1 && f.sig.ret_ty.is_some() {
                Tail::Return
            } else {
                Tail::None
            };
            self.emit_stmt(stmt, tail)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

impl KotlinEmitter {
    fn emit_function(&mut self, f: &ItemDefFunction) -> Result<()> {
        let name = f.name.name.as_str();
        self.current_fn_params = f.sig.params.iter().map(|p| p.name.name.clone()).collect();
        self.current_self_name = None;
        let params = f
            .sig
            .params
            .iter()
            .map(|p| {
                let kt = self.kotlin_type_from_ty(&p.ty);
                // Same `.len()` vs `.size` tracking as `let`-bound locals (see
                // `field_element_types`'s doc comment) — a List-typed parameter
                // needs to be known by name too.
                if let Some(elem) = sized_collection_element_type(&kt) {
                    self.field_element_types
                        .insert(p.name.name.clone(), elem.to_string());
                }
                format!("{}: {}", p.name.name, kt)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let ret = f
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty)))
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer
            .write_line(&format!("{} {}({}){} {{", fn_kw(f), name, params, ret));
        self.writer.increase_indent();
        let len = f.body.stmts.len();
        for (i, stmt) in f.body.stmts.iter().enumerate() {
            let tail = if i == len - 1 && f.sig.ret_ty.is_some() {
                Tail::Return
            } else {
                Tail::None
            };
            self.emit_stmt(stmt, tail)?;
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

/// `Display`/`Debug`'s `fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result`
/// — detected by name + receiver rather than by inspecting the parameter
/// type (matches even if `Formatter` itself failed to resolve for some
/// reason). `Formatter` is modeled as a real Kotlin `StringBuilder` and
/// `write!`/`writeln!` as real `.append(...)` calls on it (see
/// `kotlin_type_from_ty`, `fp-lang`'s `write`/`writeln` macro handling) —
/// so the body needs NO special rendering at all, only a signature/
/// prelude/epilogue adaptation for the one unavoidable mismatch: Kotlin's
/// polymorphic string conversion hook is a real, no-argument
/// `toString(): String` member, not a `Formatter`-taking, `Result`-
/// returning method by any name. `emit_fmt_as_to_string` bridges that by
/// declaring a local `StringBuilder` where the source's formatter
/// parameter used to be, running the exact same statement list as any
/// other method (its `write!`-turned-`.append()` calls mutate that local
/// same as they mutated the parameter), and returning its `.toString()`.
fn is_fmt_trait_method(f: &ItemDefFunction) -> bool {
    f.name.name.as_str() == "fmt" && f.sig.receiver.is_some()
}

impl KotlinEmitter {
    fn emit_fmt_as_to_string(
        &mut self,
        f: &ItemDefFunction,
        extension_receiver: Option<&str>,
    ) -> Result<()> {
        let formatter_name = f
            .sig
            .params
            .first()
            .map(|p| p.name.name.as_str())
            .unwrap_or("f");
        let header = match extension_receiver {
            Some(receiver) => format!("fun {}.toString(): String", receiver),
            None => "override fun toString(): String".to_string(),
        };
        self.writer.write_line(&format!("{} {{", header));
        self.writer.increase_indent();
        self.writer
            .write_line(&format!("val {} = StringBuilder()", formatter_name));
        for stmt in &f.body.stmts {
            self.emit_stmt(stmt, Tail::None)?;
        }
        self.writer
            .write_line(&format!("return {}.toString()", formatter_name));
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

// ── Import ───────────────────────────────────────────────────────────────────

impl KotlinEmitter {
    fn emit_import(&mut self, imp: &ItemImport, emitted: &mut HashSet<String>) -> Result<()> {
        let path = flatten_import_tree(&imp.tree);
        if path.is_empty() {
            return Ok(());
        }

        // Handle multi-name group imports: Rust `use foo::{A, B, C}` → `import foo.*`
        let effective = if path.contains(",") {
            let first = path.split(",").next().unwrap_or(&path);
            // Drop the last segment (the specific name) to get the parent module
            let parent = first.rsplitn(2, ".").nth(1).unwrap_or(first);
            if parent.is_empty() {
                ".*".to_string()
            } else {
                format!("{}.*", parent)
            }
        } else {
            path.clone()
        };

        let pkg = known_package(&effective);
        let kt = kt_import_for(pkg, &effective);
        if let Some(import) = kt {
            // A single logical import can expand to multiple Kotlin import lines
            // (self.g. StdPath needs both `Path` and `Paths`).
            for import in import.split('\n') {
                let first_segment = import.split('.').next().unwrap_or(import);
                if pkg == KnownPackage::Other && self.local_modules.contains(first_segment) {
                    continue;
                }
                let import = if pkg == KnownPackage::Other {
                    let package = self
                        .workspace_kotlin_packages
                        .get(first_segment)
                        .or_else(|| {
                            self.workspace_kotlin_packages
                                .get(&first_segment.replace('_', "-"))
                        });
                    let Some(package) = package else {
                        continue;
                    };
                    let suffix = import
                        .split_once('.')
                        .map(|(_, suffix)| suffix)
                        .unwrap_or("*");
                    format!("{package}.{suffix}")
                } else {
                    import.to_string()
                };
                if emitted.insert(import.to_string()) {
                    self.writer.write_line(&format!("import {}", import));
                }
            }
        }
        Ok(())
    }

    /// Adds an import for each of `e.referenced_paths` classified as a real
    /// external (std-equivalent) package not already covered by the file's
    /// own `use`-derived imports (`emitted`) — computed from actual typed
    /// usage rather than the source file's own `use` list, which may not
    /// (fully) account for spliced-in content. `KnownPackage::Other` covers
    /// same-package/local references, which need no Kotlin import at all
    /// (no generated file declares a `package`) — deliberately skipped here,
    /// unlike `emit_import`'s handling of the same variant for genuine
    /// external Rust crate dependencies written as an explicit `use`.
    fn emit_referenced_path_imports(&mut self, emitted: &mut HashSet<String>) {
        let paths: Vec<String> = self.referenced_paths.iter().cloned().collect();
        for path in paths {
            let pkg = known_package(&path);
            if pkg == KnownPackage::Other {
                continue;
            }
            let Some(kt) = kt_import_for(pkg, &path) else {
                continue;
            };
            for import in kt.split('\n') {
                if emitted.insert(import.to_string()) {
                    self.writer.write_line(&format!("import {}", import));
                }
            }
        }
    }
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
        p if p.starts_with("toml") || p.starts_with("serde_json") || p.starts_with("tokio") => {
            Unsupported
        }
        _ => Other,
    }
}

fn kt_import_for(pkg: KnownPackage, path: &str) -> Option<String> {
    use fp_core::intrinsics::calls::KnownPackage::*;
    // Silent skip for language-internal packages
    if matches!(pkg, ThisError | Tracing | AsyncTrait | Anyhow) {
        return None;
    }
    // Relative imports not valid in Kotlin
    if path.starts_with('.') {
        return None;
    }
    match pkg {
        StdCollections | StdSync | StdStr | StdOption | Serde | Winnow | ThisError | Tracing
        | AsyncTrait | Anyhow | Unsupported => None,
        // `Path::from`/`new` renders as `Paths.get(...)` (see map_kt_path), so both
        // classes need to be in scope.
        StdPath => Some("java.nio.file.Path\njava.nio.file.Paths".into()),
        StdProcess => Some("java.lang.ProcessBuilder".into()),
        StdFs => Some("java.nio.file.Path".into()),
        StdIo => Some("java.io.*".into()),
        Other => {
            let clean = path
                .trim_start_matches("crate.")
                .trim_start_matches("self.");
            if clean.is_empty() {
                None
            } else {
                Some(clean.to_string())
            }
        }
    }
}

fn flatten_import_tree(tree: &fp_core::ast::ItemImportTree) -> String {
    use fp_core::ast::ItemImportTree::*;
    match tree {
        Path(p) => p
            .segments
            .iter()
            .map(|s| flatten_import_tree(s))
            .collect::<Vec<_>>()
            .join("."),
        Ident(id) => id.name.clone(),
        Rename(r) => format!("{} as {}", r.from.name, r.to.name),
        Glob => "*".to_string(),
        Group(g) => g
            .items
            .iter()
            .map(|i| flatten_import_tree(i))
            .collect::<Vec<_>>()
            .join(", "),
        _ => String::new(),
    }
}

// ── Statements ───────────────────────────────────────────────────────────────

impl KotlinEmitter {
    fn emit_stmt(&mut self, stmt: &BlockStmt, tail: Tail) -> Result<()> {
        match stmt {
            BlockStmt::Let(l) => {
                let var_name = ident_from_pattern(&l.pat);
                let mut type_ann = extract_type_annotation(&l.pat, self);
                // Kotlin's `String.split(...)` returns a plain (immutable) `List`,
                // never a `MutableList` — a Rust `Vec<T>` annotation on a
                // `let x: Vec<T> = s.split(...).collect();` binding (the `.collect()`
                // itself is dropped as redundant, see below) would otherwise be a
                // declared-vs-actual type mismatch.
                let init_is_split = l
                    .init
                    .as_ref()
                    .is_some_and(|init| method_chain_contains(init, "split"));
                if init_is_split {
                    if let Some(t) = type_ann.as_deref().and_then(|t| t.strip_prefix("Mutable")) {
                        type_ann = Some(t.to_string());
                    }
                }
                let decl_kw = if is_mut_pattern(&l.pat) { "var" } else { "val" };
                if var_name != "_" {
                    self.declare_name(&var_name);
                }
                // `.len()` needs `.size` on a List but `.length` on a String — record
                // this name as list-typed (reusing `field_element_types`, which the
                // `.len()` call site below checks by name) so it renders correctly.
                if let Some(elem) = type_ann.as_deref().and_then(sized_collection_element_type) {
                    self.field_element_types
                        .insert(var_name.clone(), elem.to_string());
                }
                // `let mut parts = s.split(sep);` — Kotlin's `.split()` returns a `List`,
                // not a stateful iterator; remember `parts` so subsequent `.next()?` calls
                // on it (see below) can be modeled as indexed access instead of erased.
                if let Some(init) = &l.init {
                    if let ExprKind::Invoke(inv) = init.kind() {
                        if let ExprInvokeTarget::Method(sel) = &inv.target {
                            if sel.field.name.as_str() == "split" {
                                self.split_iter_vars.insert(var_name.clone());
                            }
                        }
                    }
                }
                if var_name == "_" {
                    if let Some(init) = &l.init {
                        let val = self.render_expr(init)?;
                        self.writer.write_lines(&val);
                    }
                } else if let Some(init) = &l.init {
                    // Rust's manual-iterator `.next()?` extraction (self.g. `parts.next()?`
                    // after `let mut parts = s.split(sep)`) — render as an indexed access
                    // with an early return on exhaustion, matching `?`'s None-propagation.
                    if let ExprKind::Try(t) = init.kind() {
                        if let ExprKind::Invoke(inv) = t.expr.kind() {
                            if let ExprInvokeTarget::Method(sel) = &inv.target {
                                if sel.field.name.as_str() == "next" && inv.args.is_empty() {
                                    if let ExprKind::Name(name) = sel.obj.kind() {
                                        let obj_name = name.to_string();
                                        if self.split_iter_vars.contains(&obj_name) {
                                            let idx = *self
                                                .next_call_counters
                                                .get(&obj_name)
                                                .unwrap_or(&0);
                                            self.next_call_counters
                                                .insert(obj_name.clone(), idx + 1);
                                            let obj_rendered = self.render_expr(&sel.obj)?;
                                            let val = format!(
                                                "{}.getOrNull({}) ?: return null",
                                                obj_rendered, idx
                                            );
                                            if let Some(ref ty) = type_ann {
                                                self.writer.write_lines(&format!(
                                                    "{} {} : {} = {}",
                                                    decl_kw, var_name, ty, val
                                                ));
                                            } else {
                                                self.writer.write_lines(&format!(
                                                    "{} {} = {}",
                                                    decl_kw, var_name, val
                                                ));
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
                        self.pending_assert_long = matches!(init.kind(), ExprKind::Select(sel)
                            if self.long_field_names.contains(sel.field.name.as_str()));
                    }
                    let mut val = self.render_expr(init)?;
                    if var_name == "__fp_assert_right" {
                        if self.pending_assert_long
                            && matches!(init.kind(), ExprKind::Value(v) if matches!(v.as_ref(), Value::Int(_) | Value::UInt(_)))
                        {
                            val.push('L');
                        }
                        self.pending_assert_long = false;
                    }
                    if let Some(ref ty) = type_ann {
                        self.writer
                            .write_lines(&format!("{} {} : {} = {}", decl_kw, var_name, ty, val));
                    } else {
                        self.writer
                            .write_lines(&format!("{} {} = {}", decl_kw, var_name, val));
                    }
                } else {
                    if let Some(ref ty) = type_ann {
                        self.writer
                            .write_line(&format!("{} {} : {} = null", decl_kw, var_name, ty));
                    } else {
                        self.writer
                            .write_line(&format!("{} {} = null", decl_kw, var_name));
                    }
                }
            }
            BlockStmt::Expr(se) => self.emit_stmt_expr(&se.expr, tail)?,
            // A block-local item (nested `fn`/`struct`/...) has no file-level
            // pre-pass of its own — local static methods stay unresolved to a
            // companion object here (an edge case not exercised by any current
            // caller; local `impl` blocks inside a function body are rare).
            BlockStmt::Item(item) => return self.emit_item(item, &HashMap::new(), &HashMap::new()),
            BlockStmt::Noop => {}
            _ => {}
        }
        Ok(())
    }

    fn emit_stmt_expr(&mut self, expr: &Expr, tail: Tail) -> Result<()> {
        match expr.kind() {
            ExprKind::Block(block) => {
                // A bare `{ ... }` immediately after a preceding statement gets glued to
                // it as a trailing lambda argument by Kotlin's parser — wrap in `run` so
                // it's unambiguously its own statement.
                let w = self.writer.clone();
                w.block("run", |_| -> Result<()> {
                    for s in &block.stmts {
                        self.emit_stmt(s, Tail::None)?;
                    }
                    Ok(())
                })?;
            }
            ExprKind::If(if_expr) => self.emit_if_stmt(if_expr, tail)?,
            ExprKind::Match(mt) => self.emit_match_stmt(mt, tail)?,
            ExprKind::While(wh) => {
                let cond = self.render_expr(&wh.cond)?;
                self.writer.write_lines(&format!("while ({}) {{", cond));
                self.writer.increase_indent();
                self.emit_box_body(&wh.body, Tail::None)?;
                self.writer.decrease_indent();
                self.writer.write_line("}");
            }
            ExprKind::Loop(lp) => {
                self.writer.write_line("while (true) {");
                self.writer.increase_indent();
                self.emit_box_body(&lp.body, Tail::None)?;
                self.writer.decrease_indent();
                self.writer.write_line("}");
            }
            ExprKind::For(fr) => {
                let iter_expr = self.render_expr(&fr.iter)?;
                let var = ident_from_pattern(&fr.pat);
                // Rust `for` patterns can't carry an explicit type annotation (unlike
                // closure params) — infer the element type when iterating a struct
                // field we know is `List<T>`/`MutableList<T>` (self.g. `for hunk in &f.hunks`).
                let field_name = match fr.iter.kind() {
                    ExprKind::Select(sel) => Some(sel.field.name.as_str()),
                    ExprKind::Reference(r) => match r.referee.kind() {
                        ExprKind::Select(sel) => Some(sel.field.name.as_str()),
                        _ => None,
                    },
                    _ => None,
                };
                let var = match field_name.and_then(|f| self.field_element_types.get(f)) {
                    Some(ty) if var != "_" && !var.starts_with('(') => format!("{}: {}", var, ty),
                    _ => var,
                };
                self.writer
                    .write_lines(&format!("for ({} in {}) {{", var, iter_expr));
                self.writer.increase_indent();
                self.emit_box_body(&fr.body, Tail::None)?;
                self.writer.decrease_indent();
                self.writer.write_line("}");
            }
            ExprKind::Return(ret) => {
                if let Some(val) = &ret.value {
                    let v = self.render_expr(val)?;
                    self.writer.write_lines(&format!("return {}", v));
                } else {
                    self.writer.write_line("return");
                }
            }
            ExprKind::Break(_) => {
                self.writer.write_line("break");
            }
            ExprKind::Continue(_) => {
                self.writer.write_line("continue");
            }
            _ => {
                let rendered = self.render_expr(expr)?;
                self.write_tail(tail, &rendered);
            }
        }
        Ok(())
    }

    fn emit_if_stmt(&mut self, if_expr: &fp_core::ast::ExprIf, tail: Tail) -> Result<()> {
        let cond = self.render_expr(&if_expr.cond)?;
        self.writer.write_lines(&format!("if ({}) {{", cond));
        self.writer.increase_indent();
        self.emit_box_body(&if_expr.then, tail)?;
        self.writer.decrease_indent();
        if let Some(elze) = &if_expr.elze {
            self.writer.write_line("} else {");
            self.writer.increase_indent();
            self.emit_box_body(elze, tail)?;
            self.writer.decrease_indent();
        }
        self.writer.write_line("}");
        Ok(())
    }

    fn emit_box_body(&mut self, body: &BExpr, tail: Tail) -> Result<()> {
        if let ExprKind::Block(block) = body.kind() {
            let last_index = block.stmts.len().checked_sub(1);
            for (i, s) in block.stmts.iter().enumerate() {
                let stmt_tail = if Some(i) == last_index {
                    tail
                } else {
                    Tail::None
                };
                self.emit_stmt(s, stmt_tail)?;
            }
        } else if let ExprKind::If(if_expr) = body.kind() {
            // An "else if" continuation: `elze` is a bare `ExprKind::If`, not a
            // `Block` wrapping one. Recursing through the statement-oriented
            // `emit_if_stmt` (rather than falling through to the generic
            // single-expression `render_expr` branch below) matters whenever
            // that nested if's own body has more than one statement —
            // `render_expr`'s `ExprKind::If` arm renders each branch via
            // `render_expr_single`, which silently drops every `BlockStmt`
            // that isn't a bare `Expr` and joins the survivors with a plain
            // space, no `else`/statement separator at all.
            self.emit_if_stmt(if_expr, tail)?;
        } else if let ExprKind::Match(mt) = body.kind() {
            self.emit_match_stmt(mt, tail)?;
        } else {
            let val = self.render_expr(body)?;
            self.write_tail(tail, &val);
        }
        Ok(())
    }

    /// Direct-write port of an `if let`/`match` used in statement or
    /// function-tail position -- writes straight into `self.writer` at the
    /// real depth, with `tail` deciding what happens to whichever arm's
    /// value ends up chosen (discarded, `return`ed, or assigned to an
    /// already-declared variable). Nested `if let`/`match` compose
    /// correctly at any depth this way since there's never a string to
    /// re-indent: `tail` just flows straight through the recursion.
    fn emit_match_stmt(&mut self, mt: &fp_core::ast::ExprMatch, tail: Tail) -> Result<()> {
        let scrutinee = match &mt.scrutinee {
            Some(s) => self.render_expr(s)?,
            None => "null".to_string(),
        };

        let is_single_arm = mt.cases.len() == 1
            && !matches!(
                mt.cases[0].pat.as_ref().map(|p| &p.kind),
                Some(PatternKind::Wildcard(_))
            );
        let is_two_arm = mt.cases.len() == 2 && is_else_arm(&mt.cases[1].pat);

        if is_single_arm || is_two_arm {
            let case = &mt.cases[0];
            let non_monadic = if is_two_arm {
                non_monadic_tuple_variant(self, &case.pat)
            } else {
                None
            };
            let effective_var = if non_monadic.is_some() {
                None
            } else if is_two_arm {
                stripped_tuple_binding(&case.pat).or_else(|| match_case_binding(&case.pat))
            } else {
                match_case_binding(&case.pat)
            };

            self.push_scope();
            if let Some((_, ref binding)) = non_monadic {
                self.declare_name(binding);
            }
            if let Some(ref var) = effective_var {
                if let Some(names) = var.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
                    for name in names.split(", ") {
                        self.declare_name(name);
                    }
                }
            }

            let has_second_arm = mt.cases.len() > 1;

            // A 2-arm match on an ordinary enum variant (not Some/Ok/Err/None) isn't a
            // null-check equivalent — the scrutinee itself isn't interchangeable with its
            // payload, so it needs a smart-cast + field access, not `val x = scrutinee`.
            if let Some((variant_path, binding)) = non_monadic {
                self.writer
                    .write_line(&format!("if ({} is {}) {{", scrutinee, variant_path));
                self.writer.increase_indent();
                self.writer
                    .write_line(&format!("val {} = {}.__data", binding, scrutinee));
                self.emit_box_body(&case.body, tail)?;
                self.writer.decrease_indent();
                self.writer.write_line("} else {");
                self.writer.increase_indent();
                self.emit_box_body(&mt.cases[1].body, tail)?;
                self.writer.decrease_indent();
                self.writer.write_line("}");
                self.pop_scope();
                return Ok(());
            }

            match effective_var {
                Some(var) if var.starts_with('(') => {
                    // Tuple destructuring inside the wrapper, self.g. `Some((host, path))`
                    // from `raw.split_once(':')` — null-check a temp, then destructure.
                    let tmp = "__m";
                    self.writer.write_line("run {");
                    self.writer.increase_indent();
                    self.writer
                        .write_line(&format!("val {} = {}", tmp, scrutinee));
                    self.writer.write_line(&format!("if ({} != null) {{", tmp));
                    self.writer.increase_indent();
                    self.writer.write_line(&format!("val {} = {}!!", var, tmp));
                    self.emit_box_body(&case.body, tail)?;
                    self.writer.decrease_indent();
                    if has_second_arm {
                        self.writer.write_line("} else {");
                        self.writer.increase_indent();
                        self.emit_box_body(&mt.cases[1].body, tail)?;
                        self.writer.decrease_indent();
                        self.writer.write_line("}");
                    } else if !matches!(tail, Tail::None) {
                        self.writer.write_line("} else {");
                        self.writer.increase_indent();
                        self.write_tail(tail, "null");
                        self.writer.decrease_indent();
                        self.writer.write_line("}");
                    } else {
                        self.writer.write_line("}");
                    }
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                }
                Some(var) => {
                    self.writer.write_line("run {");
                    self.writer.increase_indent();
                    self.writer
                        .write_line(&format!("val {} = {}", var, scrutinee));
                    self.writer.write_line(&format!("if ({} != null) {{", var));
                    self.writer.increase_indent();
                    self.emit_box_body(&case.body, tail)?;
                    self.writer.decrease_indent();
                    if has_second_arm {
                        self.writer.write_line("} else {");
                        self.writer.increase_indent();
                        self.emit_box_body(&mt.cases[1].body, tail)?;
                        self.writer.decrease_indent();
                        self.writer.write_line("}");
                    } else if !matches!(tail, Tail::None) {
                        self.writer.write_line("} else {");
                        self.writer.increase_indent();
                        self.write_tail(tail, "null");
                        self.writer.decrease_indent();
                        self.writer.write_line("}");
                    } else {
                        self.writer.write_line("}");
                    }
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                }
                None => {
                    // No binding: a bare `if (scrutinee) { ... }`, never an `else`
                    // -- matches this shape's pre-existing semantics (a bool guard
                    // or non-binding pattern has nothing to smart-cast, so a second
                    // arm here -- if any -- is unreachable).
                    self.writer.write_line(&format!("if ({}) {{", scrutinee));
                    self.writer.increase_indent();
                    self.emit_box_body(&case.body, tail)?;
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                }
            }
            self.pop_scope();
            Ok(())
        } else {
            self.writer.write_line(&format!("when ({}) {{", scrutinee));
            self.writer.increase_indent();
            for case in &mt.cases {
                if let Some((variant_path, binding)) = non_monadic_tuple_variant(self, &case.pat) {
                    self.writer
                        .write_line(&format!("is {} -> {{", variant_path));
                    self.writer.increase_indent();
                    self.push_scope();
                    self.declare_name(&binding);
                    self.writer
                        .write_line(&format!("val {} = {}.__data", binding, scrutinee));
                    self.emit_box_body(&case.body, tail)?;
                    self.pop_scope();
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                    continue;
                }
                let pat = render_match_pat(&case.pat, self);
                let is_empty_body =
                    matches!(case.body.kind(), ExprKind::Block(b) if b.stmts.is_empty());
                if is_empty_body {
                    self.writer.write_line(&format!("{} -> {{}}", pat));
                } else {
                    self.writer.write_line(&format!("{} -> {{", pat));
                    self.writer.increase_indent();
                    self.emit_box_body(&case.body, tail)?;
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                }
            }
            self.writer.decrease_indent();
            self.writer.write_line("}");
            Ok(())
        }
    }
}

/// Extract a Kotlin type string from a pattern's type annotation (PatternKind::Type).
fn extract_type_annotation(pat: &Pattern, e: &KotlinEmitter) -> Option<String> {
    match &pat.kind {
        PatternKind::Type(pt) => Some(e.kotlin_type_from_ty(&pt.ty)),
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

/// True if `expr` is (or is a `.method()` chain built on top of) a call to
/// `<obj>.<name>(...)`  — e.g. `s.split(' ').collect()` contains `"split"`
/// even though the outermost call is `.collect()`.
fn method_chain_contains(expr: &Expr, name: &str) -> bool {
    let ExprKind::Invoke(inv) = expr.kind() else {
        return false;
    };
    let ExprInvokeTarget::Method(sel) = &inv.target else {
        return false;
    };
    sel.field.name.as_str() == name || method_chain_contains(&sel.obj, name)
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

/// True if `expr` is a known List — checks the real inferred type
/// (`Ty::Vec`/`Ty::Slice`) first, falling back to name-registry lookup
/// (`field_element_types`, populated from struct fields, `let`-bindings
/// with an explicit `Vec<T>`/`List<T>` annotation, and List-typed function
/// parameters) only when no type is available. Used to disambiguate Rust
/// operations that mean different things on a `String` vs. a `List`
/// (`.len()` → `.size` not `.length`; range-indexing → `.subList(...)` not
/// `.substring(...)`).
fn is_known_list_receiver(expr: &Expr, e: &KotlinEmitter) -> bool {
    if matches!(
        fp_core::ast::resolved_expr_type(expr.id()),
        Some(Ty::Vec(_)) | Some(Ty::Slice(_))
    ) {
        return true;
    }
    expr_receiver_name(expr).is_some_and(|n| e.field_element_types.contains_key(&n))
}

/// True if `expr` is a known `String` — checks the real inferred type
/// first, falling back to name-registry lookup (`string_field_names`, see
/// its doc comment) only when no type is available. Used to disambiguate
/// `.clone()`, which needs to drop entirely on a `String` (already
/// immutable, no `.copy()` method) rather than map to Kotlin's data-class
/// `.copy()` convention.
fn is_known_string_receiver(expr: &Expr, e: &KotlinEmitter) -> bool {
    if is_string_like_ty(fp_core::ast::resolved_expr_type(expr.id()).as_ref()) {
        return true;
    }
    expr_receiver_name(expr).is_some_and(|n| e.string_field_names.contains(&n))
}

/// `String`/`&str` both count as "known string" — `str` has no dedicated
/// `Ty::Primitive` variant (real rustc has no distinct HIR `TyKind::Str`
/// either; it round-trips through `hir_ty_to_ast`'s `Adt`/`Ref` handling as
/// a plain named/reference type), so a resolved `&str` shows up here as
/// `Ty::Reference(.. Ty::Expr("str"))` or a bare `Ty::Expr("str")`, not
/// `Ty::Primitive(String)`. Recurses through references so `&str`/`&&str`
/// etc. all match.
fn is_string_like_ty(ty: Option<&Ty>) -> bool {
    match ty {
        Some(Ty::Primitive(TypePrimitive::String)) => true,
        Some(Ty::Reference(reference)) => is_string_like_ty(Some(reference.ty.as_ref())),
        Some(Ty::Expr(expr)) => matches!(expr_to_name(expr).as_str(), "str" | "String"),
        _ => false,
    }
}

/// True if `expr`'s real inferred type is a Kotlin `enum class` — checks
/// the real inferred type first, falling back to name-registry lookup
/// (`enum_field_names`) only when no type is available, same pattern as
/// `is_known_string_receiver`. Used to disambiguate `.clone()`, since an
/// `enum class` has no synthesized `.copy()` either — the call should drop
/// entirely rather than map to Kotlin's data-class `.copy()` convention.
fn is_known_enum_receiver(expr: &Expr, e: &KotlinEmitter) -> bool {
    if matches!(
        fp_core::ast::resolved_expr_type(expr.id()),
        Some(Ty::Enum(_))
    ) {
        return true;
    }
    expr_receiver_name(expr).is_some_and(|n| e.enum_field_names.contains(&n))
}

/// A bare local/param name, or a struct field access's field name — the
/// "name" `field_element_types`/`string_field_names` key by.
fn expr_receiver_name(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Name(n) => Some(name_to_string(n)),
        ExprKind::Select(inner) => Some(inner.field.name.to_string()),
        _ => None,
    }
}

/// Render `body` into a *fresh, depth-0* scratch `StyledWriter` swapped
/// in for `e.writer`'s real one, restoring the original writer afterward
/// (whether `body` errors or not) — used wherever `render_expr` needs to
/// build a self-contained, correctly-nested multi-line Kotlin snippet (e.g.
/// a `run { ... }`/`if (...) { ... }` wrapper for an `if let` pattern) as a
/// plain `String` to return, rather than writing directly to the real
/// output. The result is meant to be embedded elsewhere via
/// `StyledWriter::write_lines`, which stacks the *real* depth at the
/// embedding site on top of whatever relative indentation these lines
/// already carry — so building it through the same `write_line`/`block`/
/// `increase_indent` primitives the rest of the emitter uses (rather than
/// hand-rolled `"    "`/`"        "` literals for each nesting level) keeps

impl KotlinEmitter {
    fn kotlin_type_from_ty(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(prim) => match prim {
                TypePrimitive::Bool => "Boolean".into(),
                TypePrimitive::Char => "Char".into(),
                TypePrimitive::String => "String".into(),
                TypePrimitive::Int(int_ty) => match int_ty {
                    TypeInt::I8 => "Byte".into(),
                    TypeInt::I16 => "Short".into(),
                    TypeInt::I32 => "Int".into(),
                    TypeInt::I64 => "Long".into(),
                    TypeInt::U8 => "Int".into(),
                    TypeInt::U16 => "Int".into(),
                    TypeInt::U32 => "Long".into(),
                    TypeInt::U64 => "Long".into(),
                    _ => "Int".into(),
                },
                TypePrimitive::Decimal(d) => match d {
                    fp_core::ast::DecimalType::F32 => "Float".into(),
                    fp_core::ast::DecimalType::F64 => "Double".into(),
                    _ => "Double".into(),
                },
                TypePrimitive::List => "List<Any>".into(),
            },
            // Rust's `Vec<T>` is the owned, growable collection type — `.push`/
            // `.add`-style mutation is part of its normal API (mutability is
            // tracked separately via the binding's own `let`/`let mut`, not the
            // type), so this always needs Kotlin's `MutableList`, never the
            // read-only `List` (which has no `.add()`).
            Ty::Vec(v) => format!("MutableList<{}>", self.kotlin_type_from_ty(&v.ty)),
            // Kotlin only has built-in tuple types up to 3 elements (Pair/Triple);
            // anything wider needs a real named type — see ExprKind::Tuple in
            // render_expr for the matching value-construction side.
            Ty::Tuple(t) => match t.types.len() {
                0 => "Unit".into(),
                2 => format!(
                    "Pair<{}, {}>",
                    self.kotlin_type_from_ty(&t.types[0]),
                    self.kotlin_type_from_ty(&t.types[1])
                ),
                3 => format!(
                    "Triple<{}, {}, {}>",
                    self.kotlin_type_from_ty(&t.types[0]),
                    self.kotlin_type_from_ty(&t.types[1]),
                    self.kotlin_type_from_ty(&t.types[2])
                ),
                _ => "Any".into(),
            },
            Ty::Struct(s) => s.name.name.clone(),
            Ty::Enum(en) => en.name.name.clone(),
            Ty::Reference(r) => self.kotlin_type_from_ty(&r.ty),
            Ty::Expr(expr) => map_name_to_kt(&expr_to_name(expr)),
            Ty::Unit(_) => "Unit".into(),
            Ty::Slice(sl) => format!("List<{}>", self.kotlin_type_from_ty(&sl.elem)),
            Ty::Any(_) | Ty::Unknown(_) => "Any".into(),
            Ty::Nothing(_) => "Nothing".into(),
            // `dyn Trait` (typically seen inside `Arc<dyn Trait>`/`Box<dyn Trait>`
            // field types) — a single trait bound with no concrete type behind
            // it. The trait becomes a Kotlin `interface` of the same name (see
            // `emit_trait`), so the bound's own name is already the right type.
            Ty::TypeBounds(tb) => tb
                .bounds
                .first()
                .map(|b| map_name_to_kt(&expr_to_name(b)))
                .unwrap_or_else(|| "Any".into()),
            Ty::ImplTraits(it) => it
                .bounds
                .bounds
                .first()
                .map(|b| map_name_to_kt(&expr_to_name(b)))
                .unwrap_or_else(|| "Any".into()),
            _ => "Any".into(),
        }
    }
}

mod tests {
    use fp_core::ast::{
        AttrMeta, AttrMetaList, AttrStyle, Attribute, EnumTypeVariant, ExprBlock, ExprInvoke,
        Ident, ItemDefEnum, Path, ReprOptions, TypeEnum,
    };

    use super::*;

    #[test]
    fn error_derive_emits_throwable_enum_base() {
        let error_attr = Attribute {
            style: AttrStyle::Outer,
            meta: AttrMeta::List(AttrMetaList {
                name: Path::plain(vec![Ident::new("derive")]),
                items: vec![AttrMeta::Path(Path::plain(vec![Ident::new("Error")]))],
            }),
        };
        let error = ItemDefEnum {
            attrs: vec![error_attr],
            visibility: fp_core::ast::Visibility::Public,
            name: Ident::new("Problem"),
            value: TypeEnum {
                name: Ident::new("Problem"),
                generics_params: Vec::new(),
                repr: ReprOptions::default(),
                variants: vec![EnumTypeVariant {
                    attrs: Vec::new(),
                    name: Ident::new("Broken"),
                    value: Ty::unit(),
                    discriminant: None,
                }],
            },
        };
        let file = File {
            path: Default::default(),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![Item::new(ItemKind::DefEnum(error))],
        };

        let rendered = KotlinSerializer
            .serialize_file(&file)
            .expect("serialize error enum");
        assert!(rendered.contains("sealed class Problem : Exception()"));
        assert!(rendered.contains("object Broken : Problem()"));
    }

    #[test]
    fn unsupported_invoke_target_errors_instead_of_producing_a_callee_less_call() {
        // `ExprInvokeTarget::Type` (calling a type value directly, e.g. a
        // reflection-driven `SomeType(args)`-shaped construction) has no
        // Kotlin rendering yet. Before this fix, `invoke_name`'s silent
        // empty-string fallback let this flow all the way to
        // `format!("{}({})", mapped, args)`, emitting bare `(args)` with no
        // callee at all — a real, silently-wrong-Kotlin bug, not just an
        // unreachable one (unlike the dead `render_invoke_target`, which
        // this cleanup deleted instead of "fixing").
        let invoke = ExprInvoke {
            span: fp_core::span::Span::null(),
            target: ExprInvokeTarget::Type(Ty::unit()),
            args: Vec::new(),
            kwargs: Vec::new(),
        };
        let body = ExprBlock::new_stmts(vec![BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(
            Expr::new(ExprKind::Invoke(invoke)),
        ))]);
        let main_fn = ItemDefFunction::new_simple(Ident::new("main"), body);
        let file = File {
            path: Default::default(),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![Item::new(ItemKind::DefFunction(main_fn))],
        };

        let serializer = KotlinSerializer;
        let result = serializer.serialize_file(&file);
        assert!(
            result.is_err(),
            "an invoke target with no Kotlin rendering must be a real error, not a \
             silent callee-less `(args)` call"
        );
    }
}
