use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write as _;

use fp_core::ast::{
    BlockStmt, Expr, ExprKind, File, Item,
    ItemDefEnum, ItemDefFunction, ItemDefStruct, ItemImport, ItemKind, ItemDefConst,
    Ty, TypeInt, TypePrimitive, StructuralField, TySlot,
    EnumTypeVariant, FunctionParam, FormatTemplatePart, FormatArgRef,
    Value, ExprInvokeTarget,
    StmtLet, BExpr, Pattern, PatternKind,
};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::intrinsics::calls::{CallKind, KnownClass, KnownPackage};
use fp_core::package::{PackageItem, PackageSource};
use fp_core::diagnostics::report_warning_with_context;
use fp_core::backend::{BackendConfig, PackageWriter, TargetBackend};
use fp_core::writer::{IndentStyle, StyledWriter, WriterConfig};
use eyre::{bail, Result};

// ── Emitter context ──────────────────────────────────────────────────────────

struct KotlinEmitter {
    writer: StyledWriter,
    var_counter: usize,
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
    /// (`PackageSource::referenced_paths`) rather than the file's own
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

}

// ── Serializer entry ─────────────────────────────────────────────────────────

/// Cross-package facts the Kotlin backend needs before it can serialize any
/// single package: field mutability (`val` vs `var`) and List-vs-String
/// disambiguation (`.len()` -> `.size` not `.length`, range-index ->
/// `.subList` not `.substring`) are both decided workspace-wide, since a
/// struct's fields can be defined in one package and mutated/read from
/// another — see the individual `collect_*` functions' doc comments for why
/// each field is needed. Kotlin is the only backend that needs anything
/// beyond a single `PackageSource` to serialize a package; this groups what
/// would otherwise be five loose values plus a separately-merged map into
/// one value threaded through `serialize_package`.
#[derive(Default)]
pub struct KotlinWorkspaceContext {
    pub mutated_fields: HashSet<String>,
    pub list_fields: HashMap<String, String>,
    pub string_fields: HashSet<String>,
    pub enum_fields: HashSet<String>,
    pub enum_variant_names: HashMap<String, HashMap<String, String>>,
    /// `PackageSource::referenced_paths` merged across every package in the
    /// workspace — each item's own qualified path (module + name) mapped to
    /// the qualified paths it references.
    pub referenced_paths: HashMap<Vec<String>, Vec<Vec<String>>>,
}

impl KotlinWorkspaceContext {
    /// Collects every workspace-wide fact from every package's items in one
    /// pass. `sources` must be cheaply cloneable (e.g. `sources.iter()` on a
    /// slice) since each fact is collected via its own full traversal.
    pub fn collect<'a>(sources: impl Iterator<Item = &'a PackageSource> + Clone) -> Self {
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
    /// run) — used to recognize cross-package imports within the workspace
    /// and to skip emitting an (unresolvable) Kotlin import for them.
    /// `ctx` is the workspace-wide state described by `KotlinWorkspaceContext`.
    /// Returns `Vec<(relative_path, code)>` — source files + build files.
    pub fn serialize_package(
        &self,
        source: &PackageSource,
        workspace_packages: &HashSet<String>,
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
        let modules = fp_core::package::split_package_into_modules(source);

        let pkg_name = &source.name;
        let mut files = Vec::new();

        // Collect cross-package dependencies from imports
        let deps = collect_workspace_deps(&source.items, pkg_name, workspace_packages);

        // Every generated file lives in the default (unnamed) Kotlin package, so
        // imports of sibling modules within this package are both unnecessary and
        // unresolvable (there's no package literally named e.g. "config").
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
            let module_segments: Vec<String> = mod_path.split('/').map(str::to_string).collect();
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
            emitter.mutated_fields = mutated_fields.clone();
            emitter.field_element_types = list_fields.clone();
            emitter.string_field_names = string_fields.clone();
            emitter.enum_field_names = enum_fields.clone();
            emitter.referenced_paths = file_referenced_paths;
            emitter.enum_variant_names = enum_variant_names.clone();
            emitter.emit_file(&file)
                .map_err(|e| eyre::eyre!("serialize {}: {}", mod_path, e))?;
            let mut code = String::from("// Generated by FerroPhase — Kotlin target\n\n");
            code.push_str(&emitter.writer.finish());
            let out_path = format!("src/main/kotlin/{}.kt", mod_path);
            files.push((out_path, code));
        }
        Ok(files)
    }
}

/// The workspace-wide facts `KotlinBackend` needs beyond a single
/// package's own `PackageSource` — computed lazily (see `ensure_scan`)
/// from `&WorkspaceContext` on first use and cached, instead of being
/// force-fed at construction time. `workspace_packages` comes from
/// `WorkspaceContext::workspace_packages()` (in turn
/// `PackageProvider::workspace_packages()`) rather than being passed by
/// the caller — the provider is the thing that actually knows which
/// packages are this workspace's own, as opposed to e.g. `std`.
struct KotlinScan {
    ctx: KotlinWorkspaceContext,
    workspace_packages: HashSet<String>,
    /// Every package name in this workspace compile, sorted — used only
    /// by `write_workspace_files` for `settings.gradle.kts`'s
    /// `include(...)` lines.
    package_names: Vec<String>,
}

/// `TargetBackend` wrapper around [`KotlinSerializer`]. Kotlin needs
/// workspace-wide context beyond what `BackendConfig` carries — the
/// workspace-wide `KotlinScan` is read lazily from `&WorkspaceContext` on
/// first `compile_package`/`write_workspace_files` call, same as every
/// other backend gets its input. `config.root_name` (the *source* project
/// directory's name, not `config.workspace_root`, the output directory)
/// is read straight off `self.config` — `WorkspaceContext` has no way to
/// reconstruct it, it isn't package data at all.
pub struct KotlinBackend {
    serializer: KotlinSerializer,
    config: BackendConfig,
    scan: std::sync::OnceLock<KotlinScan>,
}

impl KotlinBackend {
    pub fn new(config: BackendConfig) -> Self {
        Self {
            serializer: KotlinSerializer,
            config,
            scan: std::sync::OnceLock::new(),
        }
    }

    /// Builds and caches the workspace-wide scan from `&WorkspaceContext`
    /// on first call. Safe to call from any package's `compile_package` —
    /// including the very first — since `run_named_target`'s typecheck
    /// phase already ran for every package in the workspace before any
    /// `compile_package` call happens.
    fn ensure_scan(&self, workspace: &fp_core::workspace::WorkspaceContext) -> fp_core::error::Result<&KotlinScan> {
        if let Some(scan) = self.scan.get() {
            return Ok(scan);
        }
        let workspace_packages: HashSet<String> = workspace.workspace_packages().into_iter().collect();
        let sources: Vec<PackageSource> = workspace_packages
            .iter()
            .map(|name| workspace.package_source(&fp_core::package::PackageId::new(name.clone())))
            .collect::<fp_core::error::Result<_>>()?;
        let ctx = KotlinWorkspaceContext::collect(sources.iter());
        let mut package_names: Vec<String> = sources.iter().map(|s| s.name.clone()).collect();
        package_names.sort();
        let _ = self.scan.set(KotlinScan {
            ctx,
            workspace_packages,
            package_names,
        });
        Ok(self.scan.get().expect("just set above"))
    }
}

impl TargetBackend for KotlinBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let scan = self.ensure_scan(workspace)?;
        // Materialize portable ops (`IntrinsicCall(CallKind::Op(_))`) into
        // Kotlin's real shape (`Some(x)` -> `x`, `Vec::new()` -> an empty
        // list literal, ...) directly on the compiled package's items in
        // place, immediately before reading `package_source` below —
        // `package_source` derives from the same `compiled_package`, so
        // the mutation is visible to the read that follows.
        {
            let compiled = workspace.compiled_package(package_id).ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "package `{package_id}` is unavailable for materialization"
                ))
            })?;
            let mut compiled = compiled.borrow_mut();
            for pkg_item in &mut compiled.items {
                pkg_item.item = fp_core::intrinsics::materialize_item(
                    pkg_item.item.clone(),
                    &crate::KotlinMaterializer,
                )?;
            }
        }
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = self
            .serializer
            .serialize_package(package, &scan.workspace_packages, &scan.ctx)?;
        let writer = PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (mod_path, code) in files {
            let rel = if mod_path.contains('.') {
                mod_path
            } else {
                format!("{}.kt", mod_path)
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }

    fn write_workspace_files(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
    ) -> fp_core::error::Result<()> {
        let scan = self.ensure_scan(workspace)?;
        let root_name = self.config.root_name.replace('-', "_");
        let settings = format!(
            "rootProject.name = \"{root_name}\"\n\n{}\n",
            scan.package_names
                .iter()
                .map(|n| format!("include(\":{}\")", n))
                .collect::<Vec<_>>()
                .join("\n")
        );
        let writer = PackageWriter::new(self.config.workspace_root.clone());
        writer.write_file("settings.gradle.kts", settings)?;
        writer.write_file(
            "build.gradle.kts",
            "plugins {\n    kotlin(\"jvm\") version \"2.1.0\" apply false\n}\n\n\
             allprojects {\n    repositories { mavenCentral() }\n}\n",
        )?;
        Ok(())
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
pub fn collect_string_field_names<'a>(items: impl Iterator<Item = &'a PackageItem>) -> HashSet<String> {
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
pub fn collect_enum_field_names<'a>(items: impl Iterator<Item = &'a PackageItem>) -> HashSet<String> {
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

fn collect_enum_fields_in_item(item: &Item, enum_type_names: &HashSet<String>, out: &mut HashSet<String>) {
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
fn sized_collection_element_type(kt: &str) -> Option<&str> {
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
            for child in &m.items { collect_nested_imports_from_item(child, imports); }
        }
        ItemKind::DefFunction(f) => {
            for stmt in &f.body.stmts { collect_nested_imports_from_stmt(stmt, imports); }
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
            if let ItemKind::Import(imp) = item.kind() { imports.push(imp.clone()); }
            else {
                collect_nested_imports_from_item(item, &mut imports);
                non_imports.push(item);
            }
        }
        let mut emitted_imports: HashSet<String> = HashSet::new();
        for imp in &imports { self.emit_import(imp, &mut emitted_imports)?; }
        self.emit_referenced_path_imports(&mut emitted_imports);
        if !emitted_imports.is_empty() { self.writer.write_line(""); }

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
            collect_impl_methods(item, &mut static_methods, &mut instance_methods, &mut trait_impls);
        }

        for item in non_imports { self.emit_item(item, &static_methods, &trait_impls)?; }

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
        "Display" | "Debug" | "Clone" | "Copy" | "Default" | "PartialEq" | "Eq"
            | "PartialOrd" | "Ord" | "Hash" | "From" | "Into" | "TryFrom" | "TryInto"
            | "Send" | "Sync" | "Drop" | "Iterator" | "IntoIterator" | "AsRef" | "AsMut"
            | "Deref" | "DerefMut" | "Serialize" | "Deserialize"
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
            let self_name = self_name.split('<').next().unwrap_or(&self_name).to_string();
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
            let trait_name = impl_block.trait_ty.as_ref().map(name_to_string).filter(|name| {
                let last = name.rsplit(['.', ':']).next().unwrap_or(name);
                matches!(last, "Display" | "Debug") || !is_known_std_trait(name)
            });
            for item in &impl_block.items {
                if let ItemKind::DefFunction(f) = item.kind() {
                    if f.sig.receiver.is_none() {
                        static_methods.entry(self_name.clone()).or_default().push(f.clone());
                    } else if let Some(trait_name) = &trait_name {
                        trait_impls.entry(self_name.clone()).or_default()
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

// ── Items ────────────────────────────────────────────────────────────────────

impl KotlinEmitter {
    fn emit_item(
        &mut self,
        item: &Item,
        static_methods: &HashMap<String, Vec<ItemDefFunction>>,
        trait_impls: &HashMap<String, Vec<(String, ItemDefFunction)>>,
    ) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(s) => {
                let methods = static_methods.get(s.name.name.as_str()).map(Vec::as_slice).unwrap_or(&[]);
                let traits = trait_impls.get(s.name.name.as_str()).map(Vec::as_slice).unwrap_or(&[]);
                self.emit_struct(s, methods, traits)
            }
            ItemKind::DefEnum(en) => {
                let methods = static_methods.get(en.name.name.as_str()).map(Vec::as_slice).unwrap_or(&[]);
                let traits = trait_impls.get(en.name.name.as_str()).map(Vec::as_slice).unwrap_or(&[]);
                self.emit_enum(en, methods, traits)
            }
            ItemKind::DefFunction(f) => self.emit_function(f),
            ItemKind::Module(m) => {
                for child in &m.items { self.emit_item(child, static_methods, trait_impls)?; }
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
                    for stmt in &block.stmts { self.emit_stmt(stmt, Tail::None)?; }
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
                    let params = f.sig.params.iter()
                        .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
                        .collect::<Vec<_>>().join(", ");
                    let ret = f.sig.ret_ty.as_ref()
                        .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty)))
                        .unwrap_or_else(|| ": Unit".to_string());
                    self.writer.write_line(format!("fun {}({}){}", f.name.name.as_str(), params, ret));
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
                self.field_element_types.insert(field.name.name.clone(), elem.to_string());
            }
            let mutability = if self.mutated_fields.contains(&field.name.name) { "var" } else { "val" };
            self.writer.write_line(format!("    {} {}: {}{}", mutability, field.name.name, kt, comma));
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
        let has_data = variants.iter().any(|v| !matches!(v.value, Ty::Unit(_)));
        let implemented_traits = implemented_trait_names(traits);
        let header_suffix = if implemented_traits.is_empty() {
            String::new()
        } else {
            format!(" : {}", implemented_traits.join(", "))
        };

        if has_data {
            self.writer.write_line(format!("sealed class {}{} {{", name, header_suffix));
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
                        self.writer.write_line(format!("    object {} : {}()", vname, name));
                    }
                    Ty::Struct(s) => {
                        let fields: Vec<String> = s.fields.iter()
                            .map(|f| format!("val {}: {}", f.name.name, self.kotlin_type_from_ty(&f.value)))
                            .collect();
                        self.writer.write_line(format!("    data class {}({}) : {}()",
                            vname, fields.join(", "), name));
                    }
                    Ty::Structural(s) => {
                        let fields: Vec<String> = s.fields.iter()
                            .map(|f| format!("val {}: {}", f.name.name, self.kotlin_type_from_ty(&f.value)))
                            .collect();
                        self.writer.write_line(format!("    data class {}({}) : {}()",
                            vname, fields.join(", "), name));
                    }
                    Ty::Expr(expr) => {
                        let ty_str = self.kotlin_type_from_ty(&Ty::Expr(expr.clone()));
                        self.writer.write_line(format!("    data class {}(val __data: {}) : {}()",
                            vname, ty_str, name));
                    }
                    _ => {
                        self.writer.write_line(format!("    data class {}(vararg __data: Any?) : {}()", vname, name));
                    }
                }
                if i < variants.len() - 1 { self.writer.write_line(""); }
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
            self.writer.write_line(format!("enum class {}{} {{", name, header_suffix));
            for (i, variant) in variants.iter().enumerate() {
                let comma = if i < variants.len() - 1 || !static_methods.is_empty() || !traits.is_empty() { "," } else { "" };
                self.writer.write_line(format!("    {}{}", variant.name.name, comma));
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
            Tail::None => { self.writer.write_lines(value); }
            Tail::Return => { self.writer.write_lines(&format!("return {}", value)); }
            Tail::Assign(name) => { self.writer.write_lines(&format!("{} = {}", name, value)); }
        }
    }

    fn emit_companion_function(&mut self, f: &ItemDefFunction, self_name: &str) -> Result<()> {
        let name = f.name.name.as_str();
        self.current_fn_params = f.sig.params.iter().map(|p| p.name.name.clone()).collect();
        self.current_self_name = Some(self_name.to_string());
        let params = f.sig.params.iter()
            .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty).replace("Self", self_name)))
            .collect::<Vec<_>>().join(", ");
        let ret = f.sig.ret_ty.as_ref()
            .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty).replace("Self", self_name)))
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!("{} {}({}){} {{", fn_kw(f), name, params, ret));
        self.writer.increase_indent();
        match untranspilable_reason(f) {
            Some(reason) => self.emit_stub_body(&format!("{self_name}::{name}"), reason),
            None => {
                let len = f.body.stmts.len();
                for (i, stmt) in f.body.stmts.iter().enumerate() {
                    let tail = if i == len - 1 && f.sig.ret_ty.is_some() { Tail::Return } else { Tail::None };
                    self.emit_stmt(stmt, tail)?;
                }
            }
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
        let params = f.sig.params.iter()
            .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
            .collect::<Vec<_>>().join(", ");
        let ret = f.sig.ret_ty.as_ref()
            .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty).replace("Self", self_name)))
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!("{} {}.{}({}){} {{", fn_kw(f), self_name, name, params, ret));
        self.writer.increase_indent();
        match untranspilable_reason(f) {
            Some(reason) => self.emit_stub_body(&format!("{self_name}::{name}"), reason),
            None => {
                let len = f.body.stmts.len();
                for (i, stmt) in f.body.stmts.iter().enumerate() {
                    let tail = if i == len - 1 && f.sig.ret_ty.is_some() { Tail::Return } else { Tail::None };
                    self.emit_stmt(stmt, tail)?;
                }
            }
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
        let params = f.sig.params.iter()
            .map(|p| format!("{}: {}", p.name.name, self.kotlin_type_from_ty(&p.ty)))
            .collect::<Vec<_>>().join(", ");
        let ret = f.sig.ret_ty.as_ref()
            .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty).replace("Self", self_name)))
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!("override {} {}({}){} {{", fn_kw(f), name, params, ret));
        self.writer.increase_indent();
        match untranspilable_reason(f) {
            Some(reason) => self.emit_stub_body(&format!("{self_name}::{name}"), reason),
            None => {
                let len = f.body.stmts.len();
                for (i, stmt) in f.body.stmts.iter().enumerate() {
                    let tail = if i == len - 1 && f.sig.ret_ty.is_some() { Tail::Return } else { Tail::None };
                    self.emit_stmt(stmt, tail)?;
                }
            }
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
        let params = f.sig.params.iter()
            .map(|p| {
                let kt = self.kotlin_type_from_ty(&p.ty);
                // Same `.len()` vs `.size` tracking as `let`-bound locals (see
                // `field_element_types`'s doc comment) — a List-typed parameter
                // needs to be known by name too.
                if let Some(elem) = sized_collection_element_type(&kt) {
                    self.field_element_types.insert(p.name.name.clone(), elem.to_string());
                }
                format!("{}: {}", p.name.name, kt)
            })
            .collect::<Vec<_>>().join(", ");
        let ret = f.sig.ret_ty.as_ref()
            .map(|ty| format!(": {}", self.kotlin_type_from_ty(ty)))
            .unwrap_or_else(|| ": Unit".to_string());

        self.writer.write_line(&format!("{} {}({}){} {{", fn_kw(f), name, params, ret));
        self.writer.increase_indent();
        match untranspilable_reason(f) {
            Some(reason) => self.emit_stub_body(name, reason),
            None => {
                let len = f.body.stmts.len();
                for (i, stmt) in f.body.stmts.iter().enumerate() {
                    let tail = if i == len - 1 && f.sig.ret_ty.is_some() { Tail::Return } else { Tail::None };
                    self.emit_stmt(stmt, tail)?;
                }
            }
        }
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
    }
}

/// The single reason a function's real body isn't attempted (checked before
/// `emit_function`/`emit_impl_function`/`emit_companion_function` try to
/// render it) — `None` means go ahead and emit the real body. Kept as one
/// shared check (see `emit_stub_body`) rather than each caller repeating
/// its own `if is_x { stub } else if is_y { stub } else { ... }` chain.
fn untranspilable_reason(f: &ItemDefFunction) -> Option<&'static str> {
    if is_winnow_parser(&f.body.stmts) {
        Some("parser function not transpilable (winnow combinator)")
    } else {
        None
    }
}

/// Records that `context` (a qualified function name, e.g. `Foo::bar`)
/// couldn't be transpiled and why — via `fp_core::diagnostics`, so this is
/// visible in `fp compile`'s own output (like the "skipping impl with
/// unresolvable self-type" warnings emitted during HIR generation) instead
/// of only surfacing much later as a Gradle compile error nobody connects
/// back to this specific, already-known cause.
fn report_untranspilable(context: &str, reason: &str) {
    report_warning_with_context(context.to_string(), format!("Kotlin codegen: {reason}"));
}

/// Emits `throw NotImplementedError(reason)` as a function's body and
/// reports it (see `report_untranspilable`) — the one place both of those
/// happen, so every stub call site does both instead of some only doing
/// the first.
impl KotlinEmitter {
    fn emit_stub_body(&mut self, context: &str, reason: &str) {
        report_untranspilable(context, reason);
        self.writer.write_line(&format!("throw NotImplementedError(\"{}\")", reason));
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
        self.writer.write_line(&format!("val {} = StringBuilder()", formatter_name));
        for stmt in &f.body.stmts {
            self.emit_stmt(stmt, Tail::None)?;
        }
        self.writer.write_line(&format!("return {}.toString()", formatter_name));
        self.writer.decrease_indent();
        self.writer.write_line("}\n");
        Ok(())
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

impl KotlinEmitter {
    fn emit_import(&mut self, imp: &ItemImport, emitted: &mut HashSet<String>) -> Result<()> {
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
            // (self.g. StdPath needs both `Path` and `Paths`).
            // Every generated file across every workspace package lives in the same
            // default (unnamed) Kotlin package, so a sibling *workspace* crate's
            // symbols are already visible without an import too — and there's no
            // package literally named after the crate to import from, since none
            // of these files declare one. `self.workspace_packages` is the real set
            // of sibling package names for this compile, not a hardcoded guess.
            for import in import.split('\n') {
                let first_segment = import.split('.').next().unwrap_or(import);
                if pkg == KnownPackage::Other
                    && (self.local_modules.contains(first_segment)
                        || self.workspace_packages.contains(first_segment)
                        || self.workspace_packages.contains(&first_segment.replace('_', "-")))
                {
                    continue;
                }
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
                let init_is_split = l.init.as_ref().is_some_and(|init| method_chain_contains(init, "split"));
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
                    self.field_element_types.insert(var_name.clone(), elem.to_string());
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
                                            let idx = *self.next_call_counters.get(&obj_name).unwrap_or(&0);
                                            self.next_call_counters.insert(obj_name.clone(), idx + 1);
                                            let obj_rendered = self.render_expr(&sel.obj)?;
                                            let val = format!("{}.getOrNull({}) ?: return null", obj_rendered, idx);
                                            if let Some(ref ty) = type_ann {
                                                self.writer.write_lines(&format!("{} {} : {} = {}", decl_kw, var_name, ty, val));
                                            } else {
                                                self.writer.write_lines(&format!("{} {} = {}", decl_kw, var_name, val));
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
                        if self.pending_assert_long && matches!(init.kind(), ExprKind::Value(v) if matches!(v.as_ref(), Value::Int(_) | Value::UInt(_))) {
                            val.push('L');
                        }
                        self.pending_assert_long = false;
                    }
                    if let Some(ref ty) = type_ann {
                        self.writer.write_lines(&format!("{} {} : {} = {}", decl_kw, var_name, ty, val));
                    } else {
                        self.writer.write_lines(&format!("{} {} = {}", decl_kw, var_name, val));
                    }
                } else {
                    if let Some(ref ty) = type_ann {
                        self.writer.write_line(&format!("{} {} : {} = null", decl_kw, var_name, ty));
                    } else {
                        self.writer.write_line(&format!("{} {} = null", decl_kw, var_name));
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
                    for s in &block.stmts { self.emit_stmt(s, Tail::None)?; }
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
                self.writer.write_lines(&format!("for ({} in {}) {{", var, iter_expr));
                self.writer.increase_indent();
                self.emit_box_body(&fr.body, Tail::None)?;
                self.writer.decrease_indent();
                self.writer.write_line("}");
            }
            ExprKind::Return(ret) => {
                if let Some(val) = &ret.value {
                    let v = self.render_expr(val)?;
                    self.writer.write_lines(&format!("return {}", v));
                } else { self.writer.write_line("return"); }
            }
            ExprKind::Break(_) => { self.writer.write_line("break"); }
            ExprKind::Continue(_) => { self.writer.write_line("continue"); }
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
                let stmt_tail = if Some(i) == last_index { tail } else { Tail::None };
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
            && !matches!(mt.cases[0].pat.as_ref().map(|p| &p.kind), Some(PatternKind::Wildcard(_)));
        let is_two_arm = mt.cases.len() == 2 && is_else_arm(&mt.cases[1].pat);

        if is_single_arm || is_two_arm {
            let case = &mt.cases[0];
            let non_monadic = if is_two_arm { non_monadic_tuple_variant(self, &case.pat) } else { None };
            let effective_var = if non_monadic.is_some() {
                None
            } else if is_two_arm {
                stripped_tuple_binding(&case.pat).or_else(|| match_case_binding(&case.pat))
            } else {
                match_case_binding(&case.pat)
            };

            self.push_scope();
            if let Some((_, ref binding)) = non_monadic { self.declare_name(binding); }
            if let Some(ref var) = effective_var {
                if let Some(names) = var.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
                    for name in names.split(", ") { self.declare_name(name); }
                }
            }

            let has_second_arm = mt.cases.len() > 1;

            // A 2-arm match on an ordinary enum variant (not Some/Ok/Err/None) isn't a
            // null-check equivalent — the scrutinee itself isn't interchangeable with its
            // payload, so it needs a smart-cast + field access, not `val x = scrutinee`.
            if let Some((variant_path, binding)) = non_monadic {
                self.writer.write_line(&format!("if ({} is {}) {{", scrutinee, variant_path));
                self.writer.increase_indent();
                self.writer.write_line(&format!("val {} = {}.__data", binding, scrutinee));
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
                    self.writer.write_line(&format!("val {} = {}", tmp, scrutinee));
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
                    self.writer.write_line(&format!("val {} = {}", var, scrutinee));
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
                    self.writer.write_line(&format!("is {} -> {{", variant_path));
                    self.writer.increase_indent();
                    self.push_scope();
                    self.declare_name(&binding);
                    self.writer.write_line(&format!("val {} = {}.__data", binding, scrutinee));
                    self.emit_box_body(&case.body, tail)?;
                    self.pop_scope();
                    self.writer.decrease_indent();
                    self.writer.write_line("}");
                    continue;
                }
                let pat = render_match_pat(&case.pat, self);
                let is_empty_body = matches!(case.body.kind(), ExprKind::Block(b) if b.stmts.is_empty());
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
        PatternKind::Type(pt) => {
            Some(e.kotlin_type_from_ty(&pt.ty))
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
    if matches!(fp_core::ast::resolved_expr_type(expr.id()), Some(Ty::Enum(_))) {
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
/// nested snippets self-consistent no matter how deep they end up embedded.
impl KotlinEmitter {
    fn render_expr(&mut self, expr: &Expr) -> Result<String> {
        match expr.kind() {
            ExprKind::Value(val) => {
                let rendered = render_value(val);
                // Kotlin has no byte-literal syntax — a `u8` value (self.g. from a Rust
                // byte literal `b':'`) needs an explicit `.toByte()` conversion to be
                // usable where an actual `Byte` (not `Int`) is expected.
                let is_u8 = matches!(
                    fp_core::ast::resolved_expr_type(expr.id()),
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
                // `FerroIntrinsicNormalizer` already rewrites a bare `None` to
                // `Value::Null` during normalization, and pattern rendering
                // special-cases it too, but a bare `None` used directly as an
                // expression (self.g. a tuple-constructor argument) can still reach
                // here unrewritten — handle it defensively at the backend level.
                if raw == "None" {
                    return Ok("null".to_string());
                }
                // A bare `self` used as a call receiver (`self.other_method()`,
                // rendered here since `render_expr` sees just the receiver
                // expression, not the surrounding `Select`) — Kotlin has no
                // implicit local named `self`; `this` is the equivalent and,
                // unlike the field-access case (`ExprKind::Select` below, which
                // drops it entirely), stays valid written out explicitly even
                // inside an extension function body.
                if raw == "self" {
                    return Ok("this".to_string());
                }
                // A module-qualified enum-variant VALUE (self.g. Rust
                // `types::FileStatus::Modified` used as a plain value, not a
                // match pattern) — resolve the declaring enum's real name from
                // this expression's own resolved type (`Ty::Expr` wrapping the
                // real `DefPath`-derived path built by
                // `HirToAstLifter::def_id_to_ty`) rather than guessing from the
                // path text: a `DefPath`'s own declaring segment is
                // structurally always last, so `ty()`'s last path segment is
                // the real enum name regardless of how many module segments
                // precede it. Look the exact Kotlin spelling up in the same
                // `enum_variant_names` registry `render_match_pat` already
                // uses for the pattern case.
                if let Some(enum_name) = enum_name_from_ty(fp_core::ast::resolved_expr_type(expr.id()).as_ref()) {
                    let variant_name = match name {
                        fp_core::ast::Name::Path(p) => {
                            p.segments.last().map(|s| s.name.clone())
                        }
                        fp_core::ast::Name::Ident(id) => Some(id.name.clone()),
                        _ => None,
                    };
                    if let Some(kotlin_variant) = variant_name
                        .as_deref()
                        .and_then(|v| self.enum_variant_names.get(&enum_name).and_then(|m| m.get(v)))
                    {
                        return Ok(format!("{}.{}", enum_name, kotlin_variant));
                    }
                }
                // Not a resolved enum-variant value (plain function/const
                // reference, `Self::`-shorthand path, or type info genuinely
                // unavailable) — an ordinary qualified name, rendered from its
                // real segments with no enum-specific guessing.
                Ok(qualified_name_with_self(name, self.current_self_name.as_deref()))
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
                                let obj = self.render_expr(&sel.obj)?;
                                let body = self.render_expr(&cl.body)?;
                                let rhs = if body.contains('\n') {
                                    format!("run {{\n{}\n}}", body)
                                } else {
                                    body
                                };
                                return Ok(format!("{} ?: {}", obj, rhs));
                            }
                        }
                        // `Option::map_or(default, |x| body)` has no Kotlin equivalent method —
                        // rewrite structurally as `obj?.let { x -> body } ?: default`.
                        if sel.field.name.as_str() == "map_or" && inv.args.len() == 2 {
                            if let ExprKind::Closure(cl) = inv.args[1].kind() {
                                let obj = self.render_expr(&sel.obj)?;
                                let default = self.render_expr(&inv.args[0])?;
                                let param = cl.params.first().map(ident_from_pattern).unwrap_or_else(|| "it".to_string());
                                let body = self.render_expr(&cl.body)?;
                                return Ok(format!("{}?.let {{ {} -> {} }} ?: {}", obj, param, body, default));
                            }
                        }
                        // `.map_err(SomeError::Variant)` / `.map_err(some_fn)` — Rust
                        // lets a tuple-variant constructor (or any named function) be
                        // passed as a bare value where a closure is expected, since
                        // it's itself a first-class `Fn(T) -> U` item. Kotlin's
                        // equivalent (a variant's own constructor, referenced through
                        // a dotted qualifier) isn't usable as a bare value the same
                        // way — `CoreError.IO` isn't a value, only `CoreError.IO(x)`
                        // is — so wrap it in an explicit one-arg lambda instead.
                        // `map_err` itself has no dedicated Result-mapping support
                        // here; this only needs to compile, matching every other
                        // Result-shaped call in this file.
                        if sel.field.name.as_str() == "map_err" && inv.args.len() == 1
                            && !matches!(inv.args[0].kind(), ExprKind::Closure(_))
                        {
                            let obj = self.render_expr(&sel.obj)?;
                            let ctor = self.render_expr(&inv.args[0])?;
                            return Ok(format!("{}.map_err {{ __e -> {}(__e) }}", obj, ctor));
                        }
                        // `Option::take()` — replaces a `var` with `None`/`null`, returning
                        // the old value. Kotlin has no equivalent method; model it directly.
                        // A function *parameter* receiver can't be reassigned at all in
                        // Kotlin (always an implicit `val`, unlike a `let`-bound local) —
                        // drop the reset for those (see `current_fn_params`'s doc comment).
                        if sel.field.name.as_str() == "take" && inv.args.is_empty() {
                            if let ExprKind::Name(name) = sel.obj.kind() {
                                let obj = self.render_expr(&sel.obj)?;
                                if self.current_fn_params.contains(&name_to_string(name)) {
                                    return Ok(obj);
                                }
                                return Ok(format!("run {{ val __t = {0}; {0} = null; __t }}", obj));
                            }
                        }
                        let obj = self.render_expr(&sel.obj)?;
                        // `round`/`log2` have no Kotlin member-method equivalent — both are
                        // top-level `kotlin.math` functions taking the receiver as an
                        // argument (`kotlin.math.round(x)`, not `x.round()`).
                        if matches!(sel.field.name.as_str(), "round" | "log2") && inv.args.is_empty() {
                            return Ok(format!("kotlin.math.{}({})", sel.field.name.as_str(), obj));
                        }
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
                        let is_len_on_list = sel.field.name.as_str() == "len" && is_known_list_receiver(&sel.obj, self);
                        // `.find` is also ambiguous: Rust's `str::find(pat: &str)` (a
                        // substring search, needs Kotlin's `indexOf`, which takes a
                        // `String` and returns `Int`) vs. `Iterator::find(predicate)`
                        // (needs Kotlin's own identically-named, closure-taking
                        // `find` — unchanged). Disambiguate by the argument's shape.
                        let is_string_find = sel.field.name.as_str() == "find"
                            && !matches!(inv.args.first().map(|a| a.kind()), Some(ExprKind::Closure(_)));
                        // `.clone()` maps to `.copy()` (Kotlin's data-class convention)
                        // by default, but a `String`/`enum class` has no `.copy()` —
                        // already immutable, the call should just drop (see
                        // `is_known_string_receiver`/`is_known_enum_receiver`).
                        let is_clone_dropped = sel.field.name.as_str() == "clone"
                            && (is_known_string_receiver(&sel.obj, self) || is_known_enum_receiver(&sel.obj, self));
                        // `str::parse::<T>()` needs Kotlin's `.toLong()` — but a user's own
                        // inherent/associated method that happens to be named `parse` (self.g. a
                        // winnow-combinator-style `RefNode::parse`) is a different, unrelated
                        // function that must stay `.parse(...)` unchanged. Only the real
                        // `String`-receiver case gets the numeric-string mapping.
                        let is_string_parse = sel.field.name.as_str() == "parse"
                            && is_known_string_receiver(&sel.obj, self);
                        // `map_kt_method`'s fallthrough (no entry for this Rust
                        // name) returns the name unchanged — that's fine for a
                        // known bare-property mapping (`len` → `length`, no
                        // parens needed), but a genuine unmapped method (any
                        // user-defined trait/inherent method, self.g. a custom
                        // `RepoBackend::workdir()`) still needs real Kotlin call
                        // parens even with zero args, unlike an actual property.
                        let mut is_unmapped_passthrough = false;
                        let method_name = if is_iterator_map {
                            "map".to_string()
                        } else if is_len_on_list {
                            // Rust's `.len()` always returns `usize`, which this
                            // workspace's type registry always maps to Kotlin
                            // `Long` (see `kotlin_type_from_ty`) — but Kotlin's
                            // `List.size` is natively `Int`. Coerce here so every
                            // `.len()` call is `Long`-typed like every other
                            // `usize` value, matching the convention rather than
                            // Kotlin's native collection API.
                            "size.toLong()".to_string()
                        } else if is_string_find {
                            "indexOf".to_string()
                        } else if is_clone_dropped {
                            "".to_string()
                        } else if is_string_parse {
                            "toLong()".to_string()
                        } else {
                            let mapped = map_kt_method(sel.field.name.as_str());
                            is_unmapped_passthrough = mapped == sel.field.name.as_str();
                            mapped
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
                                self.render_expr(a)
                            })
                            .collect::<Result<Vec<_>>>()?;
                        if method_name.is_empty() {
                            Ok(obj)
                        } else if method_name == "!!" {
                            Ok(format!("{}!!", obj))
                        } else if args.is_empty() {
                            if is_unmapped_passthrough {
                                Ok(format!("{}.{}()", obj, method_name))
                            } else {
                                Ok(format!("{}.{}", obj, method_name))
                            }
                        } else if method_name.ends_with("()") {
                            let base = &method_name[..method_name.len() - 2];
                            Ok(format!("{}.{}({})", obj, base, args.join(", ")))
                        } else {
                            Ok(format!("{}.{}({})", obj, method_name, args.join(", ")))
                        }
                    }
                    _ => {
                        let name = invoke_name(&inv.target)?;
                        // `Self::other_fn(...)` — Kotlin has no `Self` expression-
                        // position equivalent; swap in the real class/enum name
                        // (see `current_self_name`'s doc comment).
                        let name = if let Some(self_name) = &self.current_self_name {
                            substitute_self_prefix(&name, self_name)
                        } else {
                            name
                        };
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
                            .map(|a| self.render_expr(a))
                            .collect::<Result<Vec<_>>>()?;
                        Ok(format!("{}({})", mapped, args.join(", ")))
                    }
                }
            }

            ExprKind::Select(sel) => {
                let obj = self.render_expr(&sel.obj)?;
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
                // shares `..`'s start-inclusive/end-exclusive semantics; a slice/
                // `Vec` (see `is_known_list_receiver`) needs `List.subList` instead
                // — both require an explicit end, so an omitted one becomes `.size`.
                if let ExprKind::Range(r) = idx.index.kind() {
                    let is_list = is_known_list_receiver(&idx.obj, self);
                    let obj = self.render_expr(&idx.obj)?;
                    let start = match &r.start {
                        Some(s) => self.render_expr(s)?,
                        None => "0".to_string(),
                    };
                    if !is_list && r.end.is_none() {
                        return Ok(format!("{}.substring({})", obj, start));
                    }
                    let end = match &r.end {
                        Some(end) => {
                            let end = self.render_expr(end)?;
                            if matches!(r.limit, fp_core::ast::ExprRangeLimit::Inclusive) {
                                format!("({} + 1)", end)
                            } else {
                                end
                            }
                        }
                        None => format!("{}.size", obj),
                    };
                    return if is_list {
                        Ok(format!("{}.subList({}, {})", obj, start, end))
                    } else {
                        Ok(format!("{}.substring({}, {})", obj, start, end))
                    };
                }
                Ok(format!("{}[{}]", self.render_expr(&idx.obj)?, self.render_expr(&idx.index)?))
            }

            ExprKind::BinOp(bin) => {
                let mut lhs = self.render_expr(&bin.lhs)?;
                let mut rhs = self.render_expr(&bin.rhs)?;
                // Kotlin's `==`/`!=` (unlike `<`/`>`, which have cross-type
                // `compareTo` overloads) require matching numeric types — a
                // `.len()`-derived `Long` (see `is_len_on_list`'s `.toLong()`)
                // compared against a bare `Int` literal needs the literal
                // suffixed to match.
                if matches!(bin.kind, BinOpKind::Eq | BinOpKind::Ne) {
                    if lhs.ends_with(".toLong()") {
                        if let Some(suffixed) = int_literal_as_long(&rhs) {
                            rhs = suffixed;
                        }
                    } else if rhs.ends_with(".toLong()") {
                        if let Some(suffixed) = int_literal_as_long(&lhs) {
                            lhs = suffixed;
                        }
                    }
                }
                Ok(format!("({} {} {})", lhs, kotlin_bin_op(&bin.kind), rhs))
            }

            ExprKind::UnOp(un) => {
                Ok(format!("{}({})", kotlin_un_op(&un.op), self.render_expr(&un.val)?))
            }

            ExprKind::If(if_expr) => {
                let cond = self.render_expr(&if_expr.cond)?;
                // Always brace-wrap: `then`/`elze` can be a multi-statement block
                // (render_expr_single/render_expr on an ExprKind::Block renders
                // just the inner statements, no braces of its own), and Kotlin
                // accepts `if (c) { x } else { y }` as an expression too, so
                // wrapping unconditionally is safe even for the single-expression
                // case.
                let then_val = render_expr_single(&if_expr.then, self)?;
                if let Some(elze) = &if_expr.elze {
                    Ok(format!("if ({}) {{ {} }} else {{ {} }}", cond, then_val, render_expr_single(elze, self)?))
                } else {
                    Ok(format!("if ({}) {{ {} }}", cond, then_val))
                }
            }

            ExprKind::Match(mt) => {
                let tmp = self.fresh_var("__m");
                self.writer.write_line(&format!("var {}: Any? = null", tmp));
                self.emit_match_stmt(mt, Tail::Assign(&tmp))?;
                Ok(tmp)
            }

            ExprKind::Block(block) => {
                let tmp = self.fresh_var("__b");
                self.writer.write_line(&format!("var {}: Any? = null", tmp));
                self.push_scope();
                let len = block.stmts.len();
                for (i, stmt) in block.stmts.iter().enumerate() {
                    let stmt_tail = if i == len - 1 { Tail::Assign(&tmp) } else { Tail::None };
                    self.emit_stmt(stmt, stmt_tail)?;
                }
                self.pop_scope();
                Ok(tmp)
            }

            ExprKind::Assign(assign) => {
                Ok(format!("{} = {}", self.render_expr(&assign.target)?, self.render_expr(&assign.value)?))
            }

            ExprKind::Struct(st) => {
                // `st.name` is an `ExprKind::Name` for every real enum-variant
                // constructor call — `render_expr`'s own `Name` arm already
                // resolves it fully (typed registry lookup, then the plain
                // structural join for anything else), so no separate
                // post-processing is needed here at all.
                let variant_name = self.render_expr(&st.name)?;
                let fields: Vec<String> = st.fields.iter().map(|f| {
                    // `None` means Rust field-init shorthand (`Field { name }` ≡ `Field { name: name }`),
                    // not an explicit null value.
                    let val = match &f.value { Some(v) => self.render_expr(v)?, None => f.name.name.clone() };
                    Ok(format!("{} = {}", f.name.name, val))
                }).collect::<Result<Vec<_>>>()?;
                Ok(format!("{}({})", variant_name, fields.join(", ")))
            }

            ExprKind::Array(arr) => {
                let items: Vec<String> = arr.values.iter().map(|v| self.render_expr(v)).collect::<Result<Vec<_>>>()?;
                Ok(format!("listOf({})", items.join(", ")))
            }

            ExprKind::Tuple(tup) => {
                let items: Vec<String> = tup.values.iter().map(|v| self.render_expr(v)).collect::<Result<Vec<_>>>()?;
                // Kotlin's built-in tuple constructors only go up to 3 elements.
                let ctor = match items.len() {
                    3 => "Triple",
                    _ => "Pair",
                };
                Ok(format!("{}({})", ctor, items.join(", ")))
            }

            ExprKind::Reference(r) => self.render_expr(&r.referee),
            ExprKind::Dereference(d) => self.render_expr(&d.referee),
            ExprKind::Cast(c) => {
                let inner = self.render_expr(&c.expr)?;
                let conv = match self.kotlin_type_from_ty(&c.ty).as_str() {
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
            ExprKind::Paren(p) => Ok(format!("({})", self.render_expr(&p.expr)?)),

            ExprKind::Closure(cl) => {
                let params: Vec<String> = cl.params.iter().map(|p| {
                    let n = ident_from_pattern(p);
                    // An explicit `|c: char| ...` annotation, or a typechecker-resolved
                    // parameter type promoted by `HirToAstLifter`'s closure-lifting arm,
                    // both parse/lift as `PatternKind::Type` wrapping the ident pattern —
                    // there's no other `Pattern`-level type slot to fall back to anymore
                    // (the old ad hoc `Pattern.ty` cache field has been removed).
                    let ty_str = if let PatternKind::Type(pt) = &p.kind {
                        Some(self.kotlin_type_from_ty(&pt.ty))
                    } else {
                        None
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
                // `render_expr_single` on a `Block`/`Match` body has a side
                // effect: it writes hoisted `var __bN = ...` statements
                // straight to `self.writer`, the single shared output
                // stream, at whatever statement position is currently
                // open — normally correct for a body rendered directly
                // into the enclosing statement list, but wrong here, since
                // this closure's `{ params -> ... }` is built as an
                // in-memory string and spliced in later. Left alone, a
                // nested closure's own hoisted statements leak out into
                // the *enclosing* function/closure's statement stream,
                // landing outside this closure's braces entirely (and
                // ahead of the closure literal itself), rather than
                // inside them. Redirect the writer into a scratch buffer
                // for the duration of this one body's render so anything
                // hoisted stays scoped to this closure.
                self.writer.increase_indent();
                let saved = self.writer.swap_buffer(String::new());
                let value = render_expr_single(&cl.body, self);
                let hoisted = self.writer.swap_buffer(saved);
                self.writer.decrease_indent();
                let value = value?;
                let params = params.join(", ");
                if hoisted.trim().is_empty() {
                    Ok(format!("{{ {} -> {} }}", params, value))
                } else {
                    Ok(format!("{{ {} ->\n{}\n{} }}", params, hoisted.trim_end_matches('\n'), value))
                }
            }

            ExprKind::Let(l) => {
                Ok(format!("val {} = {}", ident_from_pattern(&l.pat), self.render_expr(&l.expr)?))
            }

            ExprKind::Return(ret) => {
                if let Some(val) = &ret.value {
                    Ok(format!("return {}", self.render_expr(val)?))
                } else { Ok("return".to_string()) }
            }

            ExprKind::IntrinsicCall(ic) => {
                use fp_core::intrinsics::calls::IntrinsicKind;
                // Render all args first to avoid borrow conflicts
                let args: Vec<String> = ic.args.iter()
                    .map(|a| self.render_expr(a))
                    .collect::<Result<Vec<_>>>()?;

                match &ic.kind {
                    // A method-style intrinsic (`receiver.count()`, from self.g. a
                    // desugared `for` loop's length check) — NOT a plain
                    // function call. The generic fallback below (`name(args)`)
                    // would double up the parens `intrinsic_name` already
                    // includes for this one (`"count()"`), producing malformed
                    // `count()(receiver)`.
                    CallKind::Intrinsic(IntrinsicKind::Len) => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        Ok(format!("{}.count()", receiver))
                    }
                    CallKind::Op(op) if op.name() == "map_or" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        let default = args.get(1).cloned().unwrap_or_default();
                        Ok(format!("{} ?: {}", receiver, default))
                    }
                    CallKind::Op(op) if op.name() == "collect" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        Ok(format!("{}.toList()", receiver))
                    }
                    CallKind::Op(op) if op.name() == "find" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        let pred = args.get(1).cloned();
                        if let Some(p) = pred {
                            Ok(format!("{}.firstOrNull {{ {} }}", receiver, p))
                        } else {
                            Ok(format!("{}.firstOrNull()", receiver))
                        }
                    }
                    CallKind::Op(op) if op.name() == "unwrap_or" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        let default = args.get(1).cloned().unwrap_or_default();
                        Ok(format!("{} ?: {}", receiver, default))
                    }
                    CallKind::Op(op) if op.name() == "to_string" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        Ok(format!("{}.toString()", receiver))
                    }
                    CallKind::Op(op) if op.name() == "and_then" => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        Ok(format!("{}.let {{ it }}", receiver))
                    }
                    // `OptionUnwrap`/`OptionSome`/`OptionNone`/`VecNew`/`AsRef`/
                    // `Iter`/`ToOwned`/`AsStr`/`Clone` never reach here:
                    // `KotlinMaterializer::materialize_call` (run over the
                    // lifted AST before serialization, see `compile_project`'s
                    // phase 2 in `fp-cli`) already rewrites those into their
                    // real Kotlin-shaped `Expr` upstream. The arms below stay
                    // here rather than in the materializer because they render
                    // straight to a Kotlin-specific string form
                    // (`?:`/`.toList()`/a string-template literal) that has no
                    // generic `ast::Expr` equivalent to return instead.
                    CallKind::Intrinsic(
                        kind @ (IntrinsicKind::Format | IntrinsicKind::Print | IntrinsicKind::Println),
                    ) => {
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
                                        FormatTemplatePart::Literal(lit) => {
                                            out.push_str(&escape_str_for_kt(lit))
                                        }
                                        FormatTemplatePart::Placeholder(ph) => {
                                            match &ph.arg_ref {
                                                // `{name}` with no separate trailing
                                                // argument at all — Rust's inline-
                                                // captured-identifier format syntax,
                                                // which refers to a local variable
                                                // directly rather than indexing into
                                                // the macro's own argument list.
                                                FormatArgRef::Named(name) if value_args.is_empty() => {
                                                    out.push_str(&format!("${{{}}}", name));
                                                }
                                                FormatArgRef::Positional(i) => {
                                                    let val = value_args.get(*i).cloned().unwrap_or_default();
                                                    out.push_str(&format!("${{{}}}", val));
                                                }
                                                FormatArgRef::Implicit | FormatArgRef::Named(_) => {
                                                    let i = next_implicit;
                                                    next_implicit += 1;
                                                    let val = value_args.get(i).cloned().unwrap_or_default();
                                                    out.push_str(&format!("${{{}}}", val));
                                                }
                                            }
                                        }
                                    }
                                }
                                format!("\"{}\"", out)
                            }
                            _ => args.first().cloned().unwrap_or_default(),
                        };
                        match kind {
                            IntrinsicKind::Format => Ok(template),
                            IntrinsicKind::Print => Ok(format!("print({})", template)),
                            IntrinsicKind::Println => Ok(format!("println({})", template)),
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
                let start = r.start.as_ref().map(|s| self.render_expr(s)).transpose()?;
                let end = r.end.as_ref().map(|s| self.render_expr(s)).transpose()?;
                Ok(match (start, end) {
                    (Some(s), Some(en)) => format!("{}..{}", s, en),
                    (Some(s), None) => format!("{}..", s),
                    (None, Some(en)) => format!("..{}", en),
                    (None, None) => "..".to_string(),
                })
            }

            ExprKind::FormatString(fs) => {
                let parts = fs.parts.iter().map(|p| match p {
                    FormatTemplatePart::Literal(lit) => Ok(escape_str_for_kt(lit)),
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
                self.render_expr(&t.expr)
            }
            ExprKind::Macro(_m) => {
                Ok("null".to_string())
            }
            ExprKind::ConstBlock(_) => Ok("null".to_string()),
            ExprKind::ArrayRepeat(ar) => {
                let elem = self.render_expr(&ar.elem)?;
                Ok(format!("listOf({})", elem))
            }
            ExprKind::Await(a) => {
                self.render_expr(&a.base)
            }

            _ => Ok(format!("/* unreachable: {:?} */", std::mem::discriminant(expr.kind()))),
        }
    }
}

/// `render_expr` already handles `ExprKind::Block` correctly (via `emit_stmt`,
/// preserving every statement kind — `let`s, `for`-loops, everything, not
/// just bare `Expr` statements) — this is a thin alias kept for call-site
/// clarity at "this body must render as a single value" positions (an
/// if/else branch used as an expression), not a distinct implementation.
fn render_expr_single(body: &BExpr, e: &mut KotlinEmitter) -> Result<String> {
    e.render_expr(body)
}

/// `Self` (constructor shorthand, `Self::other_fn`) or a leading `Self::`
/// path segment, swapped for the real class/enum name — see
/// `current_self_name`'s doc comment.
fn substitute_self_prefix(raw: &str, self_name: &str) -> String {
    if raw == "Self" {
        self_name.to_string()
    } else if let Some(rest) = raw.strip_prefix("Self::") {
        format!("{self_name}::{rest}")
    } else {
        raw.to_string()
    }
}

/// The `name` extracted here is used purely to match against known
/// path-shaped special cases (e.g. `std::env::current_dir`, crate-prefix
/// stripping via `map_kt_path`) before falling through to a generic
/// `{name}({args})` call rendering — so a target this can't name has no
/// honest generic rendering either; erroring here is what stops that
/// fallthrough from silently emitting a callee-less `(args)`.
fn invoke_name(target: &ExprInvokeTarget) -> Result<String> {
    match target {
        ExprInvokeTarget::Function(name) => Ok(name.to_string()),
        ExprInvokeTarget::Method(sel) => Ok(format!(".{}", sel.field.name)),
        other => Err(eyre::eyre!(
            "call target {other:?} is not yet supported in Kotlin output"
        )),
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
        // `Vec::new()`/`HashSet::new()`/`HashMap::new()` — there's no Kotlin
        // class named `Vec`, and `HashSet`/`HashMap` don't have a portable
        // no-arg factory reachable this way (this falls through to generic
        // path resolution otherwise, producing unresolvable `HashSet.of()`);
        // the portable constructors are these top-level functions. (The
        // `"HashSet::new" => "mutableSetOf"` entry in `map_kt_method` below
        // is unreachable from here — this function always splits `::` before
        // calling it, so it only ever sees the bare method name.)
        if method == "new" && matches!(prefix_last, "Vec" | "HashSet" | "HashMap") {
            return match prefix_last {
                "Vec" => "mutableListOf",
                "HashSet" => "mutableSetOf",
                _ => "mutableMapOf",
            }.to_string();
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
        // `Type::new(...)` — a real constructor call (JDK type like
        // `ProcessBuilder`/`Arc`, or any user-defined struct's own `new`).
        // `Vec`/`HashSet`/`HashMap` (the only real static-factory-shaped
        // `new`s) are already intercepted above, so anything reaching here
        // is a genuine constructor — render it as `Type(args)`, not
        // `Type.new(args)`/`Type.of(args)` (neither of which exist).
        if method == "new" {
            return kt_prefix;
        }
        // Drop PascalCase type prefix for local-type static methods not in known mappings
        let is_local_type = prefix.chars().next().map_or(false, |c| c.is_uppercase())
            && !prefix.contains("::")
            && method.chars().next().map_or(false, |c| c.is_lowercase());
        let known_method = is_local_type && map_kt_method(method) == method;
        if is_local_type && known_method {
            return method.to_string();
        }
        if method.chars().next().map_or(false, |c| c.is_uppercase()) {
            // Enum-variant tuple-struct constructor (e.g.
            // `CoreError::GitCommand(...)`) — pass the variant name through
            // unchanged. This branch used to uppercase it, intending to
            // handle Rust unit-struct-as-constant calls, but that also fired
            // on ordinary enum-variant constructors, which have no all-caps
            // Kotlin convention.
            return format!("{}.{}", kt_prefix, method);
        }
        let kt_method = map_kt_method(method);
        return format!("{}.{}", kt_prefix, kt_method);
    }
    map_kt_method(name)
}

fn map_kt_method(name: &str) -> String {
    // Portable method mappings (no Rust-specific names)
    match name {
        // Collecion constructors (portable). Note: a qualified `X::new` never
        // reaches here — `map_kt_path` always splits on `::` first and only
        // passes the bare method name — so this only matters for a bare
        // "Vec" (no `::`) reaching this function directly.
        "Vec" => "mutableListOf".into(),
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
        // `Option<T>::as_ref()` -> `Option<&T>` has no Kotlin equivalent step —
        // nullable types don't need one — so drop the call entirely.
        "as_ref" => "".into(),
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
        "clamp" => "coerceIn".into(),
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
            // `None` with no parens (a unit variant, not a tuple-struct
            // shape) parses as a bare `Variant` pattern instead.
            PatternKind::Variant(v) if v.pattern.is_none() => match v.name.kind() {
                ExprKind::Name(name) => {
                    let raw = name.to_string();
                    raw.rsplit("::").next().unwrap_or(&raw) == "None"
                }
                _ => false,
            },
            _ => false,
        },
    }
}


/// The enum's own bare declared name for an enum-variant VALUE expression,
/// derived from the expression's real resolved type (`Ty::Expr` wrapping the
/// real, `DefPath`-derived path `HirToAstLifter::def_id_to_ty` builds from
/// the type-checker's own resolved `DefId` — never derived from this
/// particular use's own path text). A `DefPath`'s own declaring segment is
/// structurally always last (module segments only ever precede it), so the
/// last path segment reliably names the real enum regardless of how many
/// module segments precede it — no position-counting/guessing needed.
fn enum_name_from_ty(ty: Option<&Ty>) -> Option<String> {
    match ty? {
        Ty::Enum(en) => Some(en.name.name.clone()),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(fp_core::ast::Name::Path(p)) => {
                p.segments.last().map(|s| s.name.clone())
            }
            ExprKind::Name(fp_core::ast::Name::Ident(id)) => Some(id.name.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// Renders an ordinary (non-enum-variant) qualified name from its real
/// segments — no `::`/`.` text search-and-replace — swapping a literal
/// `Self` first segment for the real class/enum name (see
/// `current_self_name`'s doc comment) by checking/replacing that one real
/// `Ident`, not by substring-searching the joined text.
fn qualified_name_with_self(name: &fp_core::ast::Name, self_name: Option<&str>) -> String {
    match name {
        fp_core::ast::Name::Ident(id) => {
            if id.name == "Self" {
                self_name.unwrap_or(id.name.as_str()).to_string()
            } else {
                id.name.clone()
            }
        }
        fp_core::ast::Name::Path(p) => {
            let mut segments: Vec<String> = p.segments.iter().map(|s| s.name.clone()).collect();
            if let (Some(first), Some(sn)) = (segments.first_mut(), self_name) {
                if first == "Self" {
                    *first = sn.to_string();
                }
            }
            segments.join(".")
        }
        _ => name_to_string(name),
    }
}

/// Resolves `raw_name` (a pattern's own qualified source path, e.g.
/// `"GitRefNode::Branch"` or a bare `"Branch"` when the variant was
/// brought into scope via `use`) to its real Kotlin sealed-subclass path,
/// by looking up `e.enum_variant_names` — the registry `emit_enum` itself
/// populated from the enum's own definition — instead of re-deriving a
/// name via string manipulation of the pattern text. Falls back to a
/// best-effort dotted-path guess only when the registry has no entry
/// (e.g. the defining enum lives in a package this file's workspace-wide
/// scan didn't cover), so a genuinely unresolvable case still degrades
/// gracefully rather than panicking.
fn resolve_variant_kotlin_path(e: &KotlinEmitter, raw_name: &str) -> String {
    let segments: Vec<&str> = raw_name.split("::").flat_map(|s| s.split('.')).collect();
    let variant_name = segments.last().copied().unwrap_or(raw_name);
    let enum_name = if segments.len() >= 2 {
        segments[segments.len() - 2]
    } else {
        e.current_self_name.as_deref().unwrap_or("")
    };
    if let Some(kotlin_variant) = e
        .enum_variant_names
        .get(enum_name)
        .and_then(|variants| variants.get(variant_name))
    {
        return format!("{}.{}", enum_name, kotlin_variant);
    }
    // No registry entry — join the segments already split out above; no
    // separate re-derivation from the raw, `::`-joined string.
    segments.join(".")
}

fn render_match_pat(pat: &Option<fp_core::ast::BPattern>, e: &KotlinEmitter) -> String {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::Ident(id) => id.ident.name.clone(),
            PatternKind::Wildcard(_) => "else".to_string(),
            PatternKind::Struct(s) => s.fields.iter()
                .map(|f| f.name.name.clone())
                .collect::<Vec<_>>().join(", "),
            PatternKind::Tuple(t) => t.patterns.iter()
                .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
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
                        .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
                        .collect::<Vec<_>>().join(", ");
                }
                let variant_name = resolve_variant_kotlin_path(e, &raw_name);
                let inner = ts.patterns.iter()
                    .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
                    .collect::<Vec<_>>().join(", ");
                format!("{}({})", variant_name, inner)
            }
            // A bare qualified path pattern (`ChangesLineKind::Add`, no `(...)`)
            // parses as a "literal" `Variant` pattern (see
            // `parse_literal_pattern_expr` in fp-lang), not `TupleStruct` —
            // render it the same way `ExprKind::Name` renders a path
            // expression elsewhere (dotted + uppercased last segment, e.g.
            // `ChangesLineKind.ADD`). Plain literal values (ints/strings/etc,
            // also routed through `Variant` by the same parser rule) render
            // via `render_value` instead.
            PatternKind::Variant(v) => {
                let variant = match v.name.kind() {
                    ExprKind::Name(name) => resolve_variant_kotlin_path(e, &name.to_string()),
                    ExprKind::Value(val) => render_value(val),
                    _ => return "else".to_string(),
                };
                match &v.pattern {
                    Some(inner) => format!("{}({})", variant, render_match_pat(&Some(inner.clone()), e)),
                    None => variant,
                }
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
fn non_monadic_tuple_variant(e: &KotlinEmitter, pat: &Option<fp_core::ast::BPattern>) -> Option<(String, String)> {
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
    let variant_path = resolve_variant_kotlin_path(e, &raw);
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
/// If `s` is a bare integer literal (optionally negative), returns it with
/// a Kotlin `L` (`Long`) suffix — used to match a `.len()`-derived `Long`
/// on the other side of an `==`/`!=` comparison. Only a plain literal
/// qualifies; any other rendered expression is left alone.
fn int_literal_as_long(s: &str) -> Option<String> {
    let digits = s.strip_prefix('-').unwrap_or(s);
    if !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()) {
        Some(format!("{s}L"))
    } else {
        None
    }
}

fn render_int_literal_kt(v: u64) -> String {
    if v > i64::MAX as u64 {
        format!("{}UL.toLong()", v)
    } else {
        v.to_string()
    }
}

/// Rust's default `f64`/decimal formatting drops the fractional part for
/// whole-number floats (`0.0.to_string() == "0"`), which Kotlin then parses
/// as an `Int` literal instead of a `Double` — cascading into type
/// mismatches everywhere that value flows. Force a decimal point onto any
/// formatted value that would otherwise read as an integer literal.
fn format_kt_decimal_literal(s: String) -> String {
    if s.contains('.') || s.contains('e') || s.contains('E') || s.contains("NaN") || s.contains("inf") {
        s
    } else {
        format!("{}.0", s)
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
        Value::Decimal(v) => format_kt_decimal_literal(v.value.to_string()),
        Value::BigDecimal(v) => format_kt_decimal_literal(v.value.to_string()),
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
    use fp_core::intrinsics::calls::IntrinsicKind;
    match kind {
        fp_core::intrinsics::calls::CallKind::Op(op) => format!("op_{}", op.name()),
        fp_core::intrinsics::calls::CallKind::Intrinsic(i) => match i {
            IntrinsicKind::Print => "print".into(),
            IntrinsicKind::Println => "println".into(),
            IntrinsicKind::Format => "String.format".into(),
            IntrinsicKind::Len => "count()".into(),
            IntrinsicKind::Panic => "error".into(),
            _ => format!("intr_{:?}", i).to_lowercase(),
        },
    }
}

// ── Type mapping ─────────────────────────────────────────────────────────────

impl KotlinEmitter {
    fn kotlin_type_from_ty(&self, ty: &Ty) -> String {
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
                2 => format!("Pair<{}, {}>", self.kotlin_type_from_ty(&t.types[0]), self.kotlin_type_from_ty(&t.types[1])),
                3 => format!("Triple<{}, {}, {}>", self.kotlin_type_from_ty(&t.types[0]), self.kotlin_type_from_ty(&t.types[1]), self.kotlin_type_from_ty(&t.types[2])),
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
            Ty::TypeBounds(tb) => tb.bounds.first()
                .map(|b| map_name_to_kt(&expr_to_name(b)))
                .unwrap_or_else(|| "Any".into()),
            Ty::ImplTraits(it) => it.bounds.bounds.first()
                .map(|b| map_name_to_kt(&expr_to_name(b)))
                .unwrap_or_else(|| "Any".into()),
            _ => "Any".into(),
        }
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
                            .map(|ty| KotlinEmitter::new().kotlin_type_from_ty(ty))
                            .collect::<Vec<_>>().join(", ");
                        format!("{}<{}>", name, args)
                    }
                })
                .collect::<Vec<_>>().join(".");
            base
        },
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

/// Splits `s` on `sep`, ignoring any `sep` nested inside `<...>`/`(...)` —
/// e.g. `split_top_level("String, Vec<Int>", ',')` → `["String", " Vec<Int>"]`,
/// not a bogus 3-way split on the inner comma.
fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '<' | '(' => depth += 1,
            '>' | ')' => depth -= 1,
            c if c == sep && depth == 0 => {
                parts.push(&s[start..i]);
                start = i + c.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(&s[start..]);
    parts
}

fn map_name_to_kt(name: &str) -> String {
    // `dyn Trait` — a trait object's type-position name still carries the
    // `dyn` keyword this far (there's no dedicated `Ty` shape for it; it's
    // parsed as a plain type-expression string). A Rust trait is emitted as
    // a Kotlin `interface` of the same name (see `emit_trait`), so `dyn`
    // just needs dropping — the trait name alone is already the right
    // Kotlin type.
    if let Some(inner) = name.strip_prefix("dyn ") {
        return map_name_to_kt(inner);
    }
    // A bare tuple type spelled out as text (`(String, bool)`) — reachable
    // for a trait method's declared return/param type, which (unlike a
    // struct field or a `let`'s inferred type) goes through this
    // string-based path rather than the structured `Ty::Tuple` one. Only
    // top-level commas count as separators — a nested generic's own comma
    // (`(String, Vec<Int>)`) must not split there.
    if let Some(inner) = name.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
        let parts = split_top_level(inner, ',');
        let mapped: Vec<String> = parts.iter().map(|p| map_name_to_kt(p.trim())).collect();
        return match mapped.len() {
            2 => format!("Pair<{}, {}>", mapped[0], mapped[1]),
            3 => format!("Triple<{}, {}, {}>", mapped[0], mapped[1], mapped[2]),
            _ => "Any".into(),
        };
    }
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
    // `HashSet<T>`/`HashMap<K, V>` as a type annotation need to agree with
    // `HashSet::new()`/`HashMap::new()`'s constructor mapping (`map_kt_path`,
    // `mutableSetOf`/`mutableMapOf` — which return `MutableSet`/`MutableMap`,
    // not `HashSet`/`HashMap`) or a `let x: HashSet<T> = HashSet::new();`
    // binding is a declared-vs-actual type mismatch.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "HashSet") {
        return format!("MutableSet<{}>", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "HashMap") {
        return format!("MutableMap<{}>", map_name_to_kt(inner));
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
        // Result<T, E> → just T. Only the first *top-level* comma marks the
        // T/E boundary — T can itself contain commas (`Result<Vec<(A, B)>, E>`),
        // which a naive `s.find(',')` would wrongly split on instead.
        let parts = split_top_level(s, ',');
        (parts.len() > 1).then_some(parts[0])
    }) {
        return map_name_to_kt(inner);
    }
    // `std::fmt::Result` — a plain type ALIAS for `Result<(), fmt::Error>`
    // (never written with `<...>` at the use site, unlike a real generic
    // instantiation), so the generic-wrapper strip above never matches
    // it. Same "unwrap to the success type" rule applies: `() → Unit`.
    if last_seg == "Result" && !dot_name.contains('<') {
        return "Unit".into();
    }
    // `std::fmt::Formatter` — the one parameter type `Display`/`Debug`'s
    // `fmt` method takes, always by `&mut` reference. Modeled directly as
    // Kotlin's `StringBuilder`: `write!(f, ..)` normalizes to `f.append(..)`
    // (see `fp-lang`'s `write`/`writeln` macro handling), which is a real,
    // valid `StringBuilder` method call — no synthetic Formatter type or
    // fmt-specific codegen needed anywhere else.
    if last_seg == "Formatter" {
        return "StringBuilder".into();
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

    // A module-qualified name with no other match (a workspace-local
    // struct/enum reference, e.g. `crate::config::GlobalConfig`) — Kotlin
    // has no nested package hierarchy mirroring Rust's module tree (every
    // struct/enum is emitted as a flat top-level class/companion object
    // per generated file), so only the type's own last segment is ever a
    // real Kotlin identifier; earlier module segments would render as
    // literal `::`/`.`-joined garbage (`crate.config.GlobalConfig`, not a
    // resolvable reference) rather than the intended type name. Falls
    // back to the last segment alone for exactly the same reason
    // `is_local_type` in `map_kt_path` already special-cases a
    // *single*-segment PascalCase prefix — this is that same rule
    // extended to a prefix that still carries its module qualification.
    last_seg.to_string()
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

#[cfg(test)]
mod tests {
    use fp_core::ast::{ExprBlock, ExprInvoke, Ident};

    use super::*;

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
        let body = ExprBlock::new_stmts(vec![BlockStmt::Expr(
            fp_core::ast::BlockStmtExpr::new(Expr::new(ExprKind::Invoke(invoke))),
        )]);
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
