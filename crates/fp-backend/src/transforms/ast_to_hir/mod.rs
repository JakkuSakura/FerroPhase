use fp_core::ast::Name;
use fp_core::ast::Pattern;
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicKind, IntrinsicNormalizer};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::query::{
    QueryDocument, QueryIrDocument, QueryKind, QueryOrigin, lower_fp_expr_to_query,
    statement_to_query_ir,
};
use fp_core::span::{FileId, Span};
use fp_core::{ast, ast::ItemKind, ast::attrs_repr, cfg::TargetEnv, hir};
use fp_sql::sql_ast::parse_sql_ast;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::Path;

mod closure;
pub(crate) use closure::strip_doc_attrs_in_items;
use closure::*;
mod config;
mod exprs; // expression lowering
mod helpers;
mod imports;
mod items; // item/impl helpers
mod macro_expansion;
mod patterns; // pattern lowering // shared path/name helpers
mod predeclare;
mod quote_detection;
mod structural;
use quote_detection::*;

#[cfg(test)]
mod tests;

pub use config::{HirLoweringConfig, ResolvedName, ResolvedNameNamespace, ResolvedNameTable};

use fp_core::diagnostics::{Diagnostic, DiagnosticManager};

const DIAGNOSTIC_CONTEXT: &str = "ast_to_hir";

fn query_origin(document: &QueryDocument) -> QueryOrigin {
    document.origin.clone()
}
// TOOD: split into multiple files?
/// Generator for transforming AST to HIR (High-level IR)
///
/// NOTE: This is transitioning from stateful to share-nothing architecture.
/// The generator now supports lossy mode and will gradually become more pure.
pub struct AstToHirLowerer {
    package_id: hir::PackageId,
    /// The closest enclosing item-like definition, used as the `owner` of
    /// every `HirId` minted while lowering it (see `HirId`'s doc comment).
    /// `None` outside any item (root owner used instead).
    current_owner: Option<hir::DefId>,
    /// Per-owner `HirId` counter, reset to zero on entering each owner.
    local_id: u32,
    current_file: FileId,
    current_position: u32,
    type_scopes: Vec<HashMap<String, hir::Res>>,
    value_scopes: Vec<HashMap<String, hir::Res>>,
    module_path: fp_core::ast::path::QualifiedPath,
    module_visibility: Vec<bool>,
    /// Reverse index from a `DefId` to every qualified-path key bound in
    /// the type namespace that resolves to it (a type can have more than
    /// one, e.g. via re-exports) — built incrementally in
    /// `record_type_symbol`, its only insertion site. Lets
    /// `name_to_hir_path_with_scope`'s cross-module associated-call
    /// resolution (`Vec::new()`, `String::from(...)`, any std/libc
    /// associated-function call) go straight to the handful of paths that
    /// could possibly match a given type, instead of linear-scanning every
    /// type binding in the package (potentially thousands once vendored
    /// std is loaded) with a `format!` allocation per candidate.
    global_type_defs_by_def_id: HashMap<hir::DefId, Vec<String>>,
    /// Memoized results for the ambiguous bare-type export query. The HIR
    /// program is immutable during one lowering pass, while this query is
    /// reached repeatedly for generic arguments in bundled std.
    preassigned_def_ids: HashMap<u64, hir::DefId>,
    enum_variant_def_ids: HashMap<String, hir::DefId>,
    type_aliases: HashMap<String, ast::Ty>,
    /// Lexical declaration module for each transparent alias, keyed by the
    /// same canonical alias key as `type_aliases`.
    type_alias_defining_modules: HashMap<String, fp_core::ast::path::QualifiedPath>,
    type_alias_children: HashMap<String, Vec<String>>,
    global_symbol_cache: RefCell<HashMap<(String, hir::Namespace, String), Option<hir::Res>>>,
    struct_field_defs: HashMap<hir::DefId, Vec<ast::StructuralField>>,
    trait_defs: HashMap<String, ast::ItemDefTrait>,
    /// The module a trait was declared in, keyed the same way as
    /// `trait_defs` — needed so synthesizing one of its default methods
    /// into an *implementing* type's impl block (in `transform_impl`) can
    /// temporarily resolve names as if still in the trait's own module,
    /// not the impl's. A default method's body/signature is copied
    /// verbatim from the trait declaration (`Iterator::try_fold`
    /// returning `ControlFlow`, `Any::type_id` returning `TypeId`, ...)
    /// and can reference names only that file's own `use` imports bring
    /// into scope — imports are persisted keyed by the importing
    /// module's own path (`record_type_symbol`'s `self.module_path.
    /// with_segment(name)`), so resolving them from the *impl's* module
    /// path instead finds nothing, exactly the same shape of bug
    /// `trait_generic_scope_bindings` fixed for a trait's own generic
    /// parameters.
    trait_def_modules: HashMap<String, fp_core::ast::path::QualifiedPath>,
    structural_value_defs: HashMap<String, StructuralValueDef>,
    const_list_length_scopes: Vec<HashMap<String, usize>>,
    synthetic_items: Vec<hir::Item>,
    /// This package's own HIR content — `items`/`def_map`/`def_paths`/
    /// `op_defs`/`intrinsic_defs`/`type_alias_targets`/`placeholder_defs`,
    /// plus `module_tree` (see `fp_core::hir::resolve::ModuleTree`'s doc
    /// comment). Written into directly throughout lowering — no private
    /// scratch copy, no mirror/extend step at `transform_package`'s return
    /// points (the earlier design this replaced kept separate `def_paths`/
    /// `op_defs`/`intrinsic_defs`/`type_alias_targets`/`placeholder_defs`
    /// fields here and copied them into a freshly-built `hir::HirPackage` at
    /// several return points instead).
    ///
    /// `module_tree` specifically replaces the old `module_defs:
    /// HashSet<QualifiedPath>` (module *existence*, `module_exists`/
    /// `ensure_module`) and `crate_roots: HashMap<String, Vec<String>>`
    /// (a sub-crate root is just a child of the tree's crate-root node,
    /// not a separate table), and — since `ModuleTree`'s bindings now
    /// carry the full `hir::SymbolEntry` shape (visibility/canonical
    /// path, not just a bare `Res`) — the former `global_type_defs`/
    /// `global_value_defs`/`prelude_type_defs`/`prelude_value_defs` flat
    /// maps too. See `docs/Resolution.md`.
    package: hir::HirPackage,
    program_def_map: HashMap<hir::DefId, hir::Item>,
    local_dispatch_items: Vec<hir::Item>,
    /// Nonzero while lowering a function-local item statement (an
    /// `ast::BlockStmt::Item` — e.g. a `const`/`struct` declared inside a
    /// function body, via `transform_item_to_hir_stmt`'s fallthrough arm).
    /// `record_value_symbol`/`record_type_symbol` (the two, and only,
    /// insertion sites for the module tree's value/type bindings) check
    /// this and skip the module-qualified/global registration while it's
    /// set, since such an item is only ever visible through the enclosing
    /// block's lexical scope (`current_value_scope`/`current_type_scope`,
    /// already pushed/popped correctly) — never via a module-qualified/
    /// `self::`-style lookup. Without this, a function-local item whose
    /// name happens to match a real module-level item of the same name
    /// (e.g. `core/time.rs`'s module-level `NANOS_PER_SEC` const and an
    /// unrelated function-local `const NANOS_PER_SEC` shadowing it)
    /// clobbers that module item's global registration *before* the local
    /// item's own body is lowered, so a `self::`-qualified reference in
    /// that very body (meant to reach past its own shadow to the module
    /// item) resolves back to itself — a genuine, silent self-reference
    /// baked directly into the HIR, which the per-item typecheck task
    /// executor then (correctly, if confusingly) reports as a stalled
    /// dependency cycle. A counter (not a bool) since local item
    /// statements can nest (an item inside a block inside another local
    /// item's body).
    suppress_global_registration_depth: u32,
    /// Debug-only labels for function-local items (see
    /// `suppress_global_registration_depth`'s doc comment) — deliberately
    /// distinct from any real module-qualified path (an
    /// `"<local>"`-tagged segment can never collide with a real path
    /// segment) so it's safe to populate even though the whole point of
    /// `suppress_global_registration_depth` is to keep these items out of
    /// the real, lookup-relevant symbol tables. Exists purely so a stalled
    /// task's `DefId` can still be resolved to a human-readable name for
    /// diagnostics (see `driver.rs`'s stall printout) — reading this map
    /// is never part of any actual name-resolution decision.
    local_item_debug_labels: HashMap<hir::DefId, String>,
    unimplemented_type_def_ids: HashSet<hir::DefId>,
    resolving_type_aliases: HashSet<String>,
    resolved_names: ResolvedNameTable,
    target_env: TargetEnv,
    respect_cfg: bool,
    lowering_config: HirLoweringConfig,
    intrinsic_normalizer: Option<Box<dyn IntrinsicNormalizer>>,
    workspace: Option<std::rc::Rc<fp_core::ast::program::AstProgram>>,
    /// The whole workspace's HIR (every already-published dependency
    /// package, plus this package once transformed) — required upfront for
    /// cross-package name/export resolution (`hir::HirProgram::find_export*`/
    /// `hir_definitions`). Separate from `workspace` (AST-only data) since
    /// `AstProgram` no longer carries HIR content itself.
    hir_program: fp_core::hir::SharedHirProgram,
    /// Same idea as `pending_impls`, for a transparent type alias
    /// (`type Result = result::Result<(), Error>;`) whose RHS names a
    /// module-qualified path (`result::Result`) that isn't reachable yet
    /// on a tolerant `predeclare_items` pass because the `use` bringing
    /// that module into scope hasn't been processed. Eagerly resolving
    /// the RHS here (`transform_type_to_hir`) runs during STEP 1,
    /// strictly before STEP 2's import resolution — see
    /// `transform_package`'s own step comments — so this defers exactly
    /// the same way `pending_impls` does, retried once imports exist.
    pending_type_aliases: Vec<(fp_core::ast::path::QualifiedPath, ast::Item)>,
    /// Impl items deferred until imports have been resolved.
    pending_impls: Vec<(fp_core::ast::path::QualifiedPath, ast::Item)>,
    /// `(module_path, alias)` pairs already registered by
    /// `register_import_binding`, so re-running it (e.g. `append_item`'s
    /// own `ItemKind::Import` handling, after `transform_package`'s
    /// upfront import worklist already ran) is a guaranteed no-op instead
    /// of an assumed-safe duplicate.
    resolved_import_aliases: HashSet<(fp_core::ast::path::QualifiedPath, String)>,
    diagnostics: DiagnosticManager,
}

enum MaterializedTypeAlias {
    Struct(ast::TypeStruct),
    Structural(ast::TypeStructural),
    Enum(ast::TypeEnum),
}

#[derive(Debug, Clone)]
struct StructuralValueDef {
    name: String,
    def_id: hir::DefId,
    fields: Vec<StructuralFieldSpec>,
}

#[derive(Debug, Clone, PartialEq)]
struct StructuralFieldSpec {
    name: String,
    ty: LiteralTypeKind,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PathResolutionScope {
    Value,
    Type,
    Trait,
}

impl PathResolutionScope {
    fn namespace(self) -> hir::Namespace {
        match self {
            PathResolutionScope::Value => hir::Namespace::Value,
            PathResolutionScope::Type | PathResolutionScope::Trait => hir::Namespace::Type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralTypeKind {
    Primitive(ast::TypePrimitive),
    Unit,
    Null,
}

#[derive(Debug, Clone)]
struct ImportBinding {
    target: Vec<String>,
    alias: Option<String>,
    /// `target` names a module prefix to expand (`use target::*;`),
    /// re-evaluated fresh on every fixed-point sweep in
    /// `resolve_pending_imports` instead of once at collection time — see
    /// that field's own doc comment for why a one-shot expansion is wrong
    /// for a glob whose source module's own bindings are still pending
    /// (e.g. real `core::prelude::rust_2024`'s `pub use super::v1::*;`,
    /// where `v1` itself has no bindings of its own until its `pub use
    /// crate::option::Option::{self, None, Some};` resolves).
    is_glob: bool,
}

impl AstToHirLowerer {
    fn add_error(&mut self, diag: Diagnostic) {
        self.diagnostics.add_diagnostic(diag);
    }

    pub fn take_diagnostics(&mut self) -> DiagnosticManager {
        std::mem::replace(&mut self.diagnostics, DiagnosticManager::new())
    }

    /// Lowers `f` with `owner` as the current `HirId` owner, resetting the
    /// per-owner `local_id` counter to zero for its duration and restoring
    /// the previous owner/counter afterward (so nested items don't leak
    /// their local-id space into the enclosing item).
    fn with_owner<T>(
        &mut self,
        owner: hir::DefId,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let previous_owner = self.current_owner.replace(owner);
        let previous_local = std::mem::replace(&mut self.local_id, 0);
        let result = f(self);
        self.current_owner = previous_owner;
        self.local_id = previous_local;
        result
    }

    fn item_enabled_by_cfg(&self, item: &ast::Item) -> bool {
        !self.respect_cfg || fp_core::cfg::item_enabled_by_cfg(item, &self.target_env)
    }

    fn normalize_span(&self, span: Span) -> Span {
        span
    }

    fn handle_import(&mut self, _import: &ast::ItemImport) -> Result<()> {
        let mut bindings = Vec::new();
        self.collect_imports(Vec::new(), &_import.tree, &mut bindings)?;
        for binding in bindings {
            self.register_import_binding(binding, &_import.visibility);
        }
        Ok(())
    }

    pub fn with_file<P: AsRef<Path>>(
        hir_program: fp_core::hir::SharedHirProgram,
        package_id: hir::PackageId,
        path: P,
    ) -> Self {
        let mut generator = Self::new(hir_program, package_id);
        generator.reset_file_context(path);
        generator
    }

    /// Create a new HIR generator for `package_id` — required upfront (not
    /// filled in later via a builder method) so `self.package`'s own id is
    /// correct from construction, never a placeholder default that a
    /// caller might forget to override (see `HirPackage::new`'s doc
    /// comment for the bug this class of mistake caused). `hir_program`
    /// is likewise required upfront (the workspace's HIR), so
    /// cross-package name resolution is always available.
    pub fn new(hir_program: fp_core::hir::SharedHirProgram, package_id: hir::PackageId) -> Self {
        Self {
            package_id: package_id.clone(),
            current_owner: None,
            local_id: 0,
            current_file: 0, // Default file ID
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            module_visibility: vec![true],
            global_type_defs_by_def_id: HashMap::new(),
            preassigned_def_ids: HashMap::new(),
            enum_variant_def_ids: HashMap::new(),
            type_aliases: HashMap::new(),
            type_alias_defining_modules: HashMap::new(),
            type_alias_children: HashMap::new(),
            global_symbol_cache: RefCell::new(HashMap::new()),
            struct_field_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            trait_def_modules: HashMap::new(),
            structural_value_defs: HashMap::new(),
            const_list_length_scopes: vec![HashMap::new()],
            synthetic_items: Vec::new(),
            package: hir::HirPackage::new(package_id),
            program_def_map: HashMap::new(),
            local_dispatch_items: Vec::new(),
            suppress_global_registration_depth: 0,
            local_item_debug_labels: HashMap::new(),
            unimplemented_type_def_ids: HashSet::new(),
            resolving_type_aliases: HashSet::new(),
            resolved_names: ResolvedNameTable::new(),
            target_env: TargetEnv::host(),
            respect_cfg: true,
            lowering_config: HirLoweringConfig::default(),
            intrinsic_normalizer: None,
            workspace: None,
            hir_program,
            pending_type_aliases: Vec::new(),
            pending_impls: Vec::new(),
            resolved_import_aliases: HashSet::new(),
            diagnostics: DiagnosticManager::new(),
        }
    }

    pub fn with_lowering_config(mut self, config: HirLoweringConfig) -> Self {
        self.lowering_config = config;
        self
    }

    pub fn with_workspace(
        mut self,
        workspace: std::rc::Rc<fp_core::ast::program::AstProgram>,
    ) -> Self {
        self.workspace = Some(workspace);
        self
    }

    pub fn with_preassigned_def_ids(mut self, ids: HashMap<u64, hir::DefId>) -> Self {
        self.preassigned_def_ids = ids;
        self
    }

    pub fn preassigned_def_ids(&self) -> HashMap<u64, hir::DefId> {
        self.preassigned_def_ids.clone()
    }

    pub fn exported_symbols(&self) -> HashMap<String, hir::Res> {
        self.package
            .module_tree
            .public_bindings()
            .into_iter()
            .map(|(path, _namespace, entry)| {
                let key =
                    hir::HirProgram::canonical_external_path(&self.package.id, &path.to_key());
                (key, entry.res)
            })
            .collect()
    }

    /// `type_aliases` (unlike the module tree's value/type bindings) has no
    /// per-entry visibility tracking — `register_type_alias` is called for
    /// every `type X = Y;` regardless of `pub`. Export all of them; a
    /// dependent package can only ever reach one by spelling out its exact
    /// qualified path (e.g. `::libc::char`), so there's no meaningful
    /// privacy leak from exporting private aliases too.
    pub fn exported_type_aliases(&self) -> HashMap<String, ast::package::TypeAliasExport> {
        self.type_aliases
            .iter()
            .filter_map(|(key, target)| {
                self.type_alias_defining_modules
                    .get(key)
                    .cloned()
                    .map(|defining_module| {
                        (
                            key.clone(),
                            ast::package::TypeAliasExport {
                                target: target.clone(),
                                defining_module,
                            },
                        )
                    })
            })
            .collect()
    }

    pub fn with_intrinsic_normalizer<N>(mut self, normalizer: N) -> Self
    where
        N: IntrinsicNormalizer + 'static,
    {
        self.intrinsic_normalizer = Some(Box::new(normalizer));
        self
    }

    pub fn with_resolved_names(mut self, resolved_names: ResolvedNameTable) -> Self {
        self.resolved_names = resolved_names;
        self
    }

    pub fn set_target_triple(&mut self, target_triple: Option<&str>) {
        self.target_env = TargetEnv::from_triple(target_triple);
    }

    pub fn set_target_lang(&mut self, target_lang: Option<&str>) {
        self.target_env.lang = target_lang.map(str::to_owned);
    }

    pub fn set_cfg_filtering(&mut self, enabled: bool) {
        self.respect_cfg = enabled;
    }

    fn reset_file_context<P: AsRef<Path>>(&mut self, file_path: P) {
        self.current_file = fp_core::source_map::source_map()
            .file_id(file_path.as_ref())
            .unwrap_or(0);
        self.current_position = 0;
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        self.module_visibility.clear();
        self.module_visibility.push(true);
        self.enum_variant_def_ids.clear();
        self.struct_field_defs.clear();
        self.package.module_tree = hir::resolve::ModuleTree::new();
        self.unimplemented_type_def_ids.clear();
        self.resolving_type_aliases.clear();
    }

    fn current_type_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.type_scopes
            .last_mut()
            .expect("at least one type scope must exist")
    }

    fn current_value_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.value_scopes
            .last_mut()
            .expect("at least one value scope must exist")
    }

    fn register_type_generic(&mut self, name: &str, def_id: hir::DefId) {
        self.current_type_scope()
            .insert(name.to_string(), hir::Res::Def(def_id));
    }

    fn register_value_def(&mut self, name: &str, def_id: hir::DefId, visibility: &ast::Visibility) {
        let res = hir::Res::Def(def_id);
        self.current_value_scope()
            .insert(name.to_string(), res.clone());
        self.record_value_symbol(name, res, visibility);
    }

    fn register_value_local(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_value_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    /// Type-namespace counterpart of `register_value_local` — binds `name`
    /// (lexically, this scope only) to the `HirId` of the expression whose
    /// checked/comptime-resolved value defines its shape, instead of a real
    /// `DefId`/`def_map` entry. Used for a local `type X = const { .. };`
    /// whose RHS needs compile-time evaluation to produce a concrete type
    /// (e.g. a `TypeBuilder`-constructed struct) — unlike a real
    /// `struct`/`enum` item (`register_type_def`, backed by `Res::Def` and a
    /// `def_map` entry known up front), the shape here is only known once
    /// the checker actually evaluates the tagged expression, so `path_ty`
    /// reads it out of the package's own typed results via this `HirId`
    /// instead.
    fn register_type_local(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_type_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    fn record_module_def(&mut self, name: &str) {
        let path = self.module_path.with_segment(name.to_string());
        self.package.module_tree.ensure_module(&path);
    }

    fn register_type_def(&mut self, name: &str, def_id: hir::DefId, visibility: &ast::Visibility) {
        let res = hir::Res::Def(def_id);
        self.current_type_scope()
            .insert(name.to_string(), res.clone());
        self.record_type_symbol(name, res, visibility);
    }

    /// Binds `path`'s last segment as `res` in namespace `ns`, at the
    /// tree node for `path`'s remaining prefix (created via
    /// `ensure_namespace` if it doesn't exist yet) — the module tree's
    /// equivalent of the old flat `global_type_defs`/`global_value_defs`
    /// insertion, now carrying the same `SymbolEntry` shape directly on
    /// the tree node instead of a second, parallel lookup table.
    ///
    /// `ensure_namespace`, not `ensure_module`: `path`'s prefix is often
    /// a real module (an ordinary item's own enclosing module, already
    /// marked real by `transform_package`'s file-based `ensure_module`
    /// pass), but just as often isn't — a struct/enum's own qualified
    /// path, here purely as the parent for one of *its* associated items
    /// (an impl method, an enum variant). Marking that non-module prefix
    /// `is_module = true` would make `module_exists` — and therefore
    /// `register_import_binding`'s "is this name actually a module?"
    /// check — treat `use crate::marker::PhantomData;` as importing a
    /// module instead of the struct itself, resolving to
    /// `Res::Module(["core","marker","PhantomData"])` rather than the
    /// struct's own `Res::Def`, the moment `PhantomData` gained even one
    /// associated item (which every real one does).
    fn bind_symbol(
        &mut self,
        path: &fp_core::ast::path::QualifiedPath,
        ns: hir::Namespace,
        entry: hir::SymbolEntry,
    ) {
        self.global_symbol_cache.borrow_mut().clear();
        let mut segments = path.segments.clone();
        let Some(leaf) = segments.pop() else {
            return;
        };
        let prefix = fp_core::ast::path::QualifiedPath::new(segments);
        let module_id = self.package.module_tree.ensure_namespace(&prefix);
        if let Some(previous) = self
            .package
            .module_tree
            .lookup_res(module_id, ns, &leaf)
            .cloned()
            && previous != entry.res
        {
            self.emit_error(
                Span::new(self.current_file, 0, 0),
                format!("duplicate definition `{leaf}` in the same namespace"),
            );
        }
        self.package
            .module_tree
            .bind(module_id, ns, &leaf, entry.clone());
        self.package
            .module_tree
            // Keep the complete path here. The reserved prelude node is
            // populated from the `prelude::<edition>::<name>` suffix, so
            // passing only `prefix` would discard the leaf and leave crate
            // metadata with an empty implicit prelude.
            .bind_prelude(&prefix, &leaf, ns, entry);
    }

    fn record_value_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let path = self.qualify_path(name);
        self.record_def_path(&res, &path);
        // See `suppress_global_registration_depth`'s doc comment: a
        // function-local item statement's name must never enter the
        // module-qualified global table, only its own lexical scope
        // (already handled by `register_value_def`'s separate
        // `current_value_scope()` insert, unaffected by this guard).
        if self.suppress_global_registration_depth > 0 {
            return;
        }
        let export = self.symbol_export_marker(visibility);
        self.bind_symbol(
            &path,
            hir::Namespace::Value,
            hir::SymbolEntry {
                res,
                export,
                path: Some(path.clone()),
            },
        );
    }

    fn record_value_path(
        &mut self,
        path: &fp_core::ast::path::QualifiedPath,
        res: hir::Res,
        visibility: &ast::Visibility,
    ) {
        self.record_def_path(&res, path);
        let export = self.symbol_export_marker(visibility);
        self.bind_symbol(
            path,
            hir::Namespace::Value,
            hir::SymbolEntry {
                res,
                export,
                path: Some(path.clone()),
            },
        );
    }

    fn record_type_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let path = self.qualify_path(name);
        self.record_def_path(&res, &path);
        // See `suppress_global_registration_depth`'s doc comment.
        if self.suppress_global_registration_depth > 0 {
            return;
        }
        let qualified = path.to_key();
        let export = self.symbol_export_marker(visibility);
        if let hir::Res::Def(def_id) = &res {
            self.global_type_defs_by_def_id
                .entry(def_id.clone())
                .or_default()
                .push(qualified);
        }
        self.bind_symbol(
            &path,
            hir::Namespace::Type,
            hir::SymbolEntry {
                res,
                export,
                path: Some(path.clone()),
            },
        );
    }

    /// Records a definition's qualified path the first time its `DefId` is
    /// registered (see `hir::HirPackage::def_paths`). Uses `entry().or_insert`
    /// rather than overwriting so that later re-registrations under an
    /// alias (`register_import_binding` re-registers an existing
    /// `Res::Def` under a `use ... as` name through these same helpers)
    /// never clobber a def's one true canonical path.
    fn record_def_path(&mut self, res: &hir::Res, path: &fp_core::ast::path::QualifiedPath) {
        if let hir::Res::Def(def_id) = res {
            self.package
                .def_paths
                .entry(def_id.clone())
                .or_insert_with(|| hir::DefPath::from_qualified_path(path));
        }
    }

    fn symbol_export_marker(&self, visibility: &ast::Visibility) -> hir::SymbolExport {
        if self.should_export(visibility) {
            hir::SymbolExport::Public
        } else if matches!(visibility, ast::Visibility::Crate) {
            // `pub(crate)` is visible to the *whole crate*, not just this
            // item's own declaring module and its descendants (what a
            // plain `Scoped(self.module_path)` means) — without this, a
            // `pub(crate)` item declared in one module (e.g. real
            // vendored std's `core::ops::index_range::IndexRange`,
            // re-exported as `core::ops::IndexRange`) is invisible to
            // every unrelated module elsewhere in the same crate that
            // legitimately imports it (`core::array::iter::iter_inner`'s
            // `use crate::ops::IndexRange;`), a real resolution failure
            // this crate's own `SymbolExport::can_access`'s descendant-of
            // check would otherwise silently produce. Scope to just the
            // first module-path segment — real vendored std's one
            // exception (bundling `core`/`alloc`/`std` as three separate
            // real crates under one FerroPhase package) already uses
            // this same single-segment convention as its own crate-root
            // boundary elsewhere (`register_import_binding`'s `roots`).
            hir::SymbolExport::Scoped(
                self.module_path
                    .segments
                    .first()
                    .cloned()
                    .into_iter()
                    .collect(),
            )
        } else if let ast::Visibility::Restricted(path) = visibility {
            // `pub(super)`/`pub(in some::path)` — same scoping bug as
            // `pub(crate)` above, generalized: visible starting from
            // whatever *ancestor* module the restriction names (the
            // parent, for `pub(super)`; an arbitrary explicit ancestor —
            // real vendored std's own `pub(in crate::num)`, granting
            // every module under `core::num` access to a helper declared
            // several levels deeper in `core::num::imp::overflow_panic`
            // — for `pub(in ..)`), not just this item's own declaring
            // module and its descendants. A plain `Scoped(self.
            // module_path)` makes such an item invisible to every module
            // the restriction explicitly grants access to except its own
            // declaring subtree, since the named ancestor is never a
            // descendant of it.
            hir::SymbolExport::Scoped(self.resolve_restricted_visibility_scope(path))
        } else {
            hir::SymbolExport::Scoped(self.module_path.segments.clone())
        }
    }

    /// Resolves a `pub(super)`/`pub(in path)` restriction's own path
    /// (`super`, `crate::num`, `self`, ...) to the absolute module path
    /// segments it names, relative to this item's own declaring module —
    /// the same handful of path-prefix keywords a plain `use` path
    /// recognizes, just walked directly here since a visibility
    /// restriction is never itself re-exported/aliased the way an import
    /// target can be.
    fn resolve_restricted_visibility_scope(&self, path: &ast::ItemImportPath) -> Vec<String> {
        let mut scope: Vec<String> = Vec::new();
        let mut started = false;
        for segment in &path.segments {
            match segment {
                ast::ItemImportTree::Crate => {
                    scope = self
                        .module_path
                        .segments
                        .first()
                        .cloned()
                        .into_iter()
                        .collect();
                    started = true;
                }
                ast::ItemImportTree::Root => {
                    scope = Vec::new();
                    started = true;
                }
                ast::ItemImportTree::SelfMod => {
                    scope = self.module_path.segments.clone();
                    started = true;
                }
                ast::ItemImportTree::SuperMod => {
                    if !started {
                        scope = self.module_path.segments.clone();
                        started = true;
                    }
                    scope.pop();
                }
                ast::ItemImportTree::Ident(ident) => {
                    if !started {
                        // A restriction path with no leading `crate`/
                        // `self`/`super`/`::` keyword (uncommon; every
                        // real vendored std case seen so far starts with
                        // `crate::` or is a bare `super`) — treat as
                        // relative to this item's own declaring module,
                        // the same convention an ordinary relative `use`
                        // path uses.
                        scope = self.module_path.segments.clone();
                        started = true;
                    }
                    scope.push(ident.name.to_string());
                }
                _ => {}
            }
        }
        if !started {
            scope = self.module_path.segments.clone();
        }
        scope
    }

    fn canonical_type_path(
        &self,
        self_path: &hir::Path,
    ) -> Result<fp_core::ast::path::QualifiedPath> {
        // Non-nominal self-type shapes (`&T`, `[T]`, `[T; N]`) carry a typed
        // `Res::Builtin` tag rather than resolving to a `DefId` — mirrors
        // rustc's `SimplifiedType` fast-reject bucketing. Check this first,
        // via the tag rather than sniffing the segment name.
        if let Some(hir::Res::Builtin(kind)) = &self_path.res {
            return Ok(fp_core::ast::path::QualifiedPath::new(vec![
                kind.bucket_key().to_string(),
            ]));
        }
        let self_def_id = match self_path.res {
            Some(hir::Res::Def(ref def_id)) => Some(def_id.clone()),
            _ => None,
        };
        let self_def_id = self_def_id.as_ref();
        let self_def_id = match self_def_id {
            Some(id) => id,
            None => {
                let name = self_path
                    .segments
                    .first()
                    .map(|s| s.name.as_str())
                    .unwrap_or("");
                if is_primitive_type_name(name) {
                    return Ok(fp_core::ast::path::QualifiedPath::new(vec![
                        name.to_string(),
                    ]));
                }
                return Err(fp_core::Error::from("unresolved impl self type"));
            }
        };

        // An impl may be declared in this package while its self type is
        // owned by a dependency (`alloc::vec` implements traits for
        // `core::option::Option`, for example). The local reverse index only
        // contains definitions predeclared by this lowerer; route foreign
        // identities through the owning package's canonical path instead of
        // treating them as unresolved and repeatedly retrying/skipping them.
        if self_def_id.package_id != self.package.id {
            if let Some(path) = self.hir_program.def_path(self_def_id.clone()) {
                return Ok(fp_core::ast::path::QualifiedPath::new(path.to_segments()));
            }
        }

        // During predeclaration, impl generic parameters are entered into a
        // temporary lexical type scope so `impl<T> Trait for T` can resolve
        // its self path. They are not nominal workspace definitions and
        // therefore have no entry in `global_type_defs_by_def_id`; preserve
        // the lexical path as the canonical blanket-impl key instead.
        if self.type_scopes.iter().rev().any(|scope| {
            scope
                .values()
                .any(|res| matches!(res, hir::Res::Def(id) if id == self_def_id))
        }) {
            return Ok(fp_core::ast::path::QualifiedPath::new(
                self_path
                    .segments
                    .iter()
                    .map(|segment| segment.name.as_str().to_owned())
                    .collect(),
            ));
        }

        let relative = self.module_path.join(
            &self_path
                .segments
                .iter()
                .map(|segment| segment.name.as_str().to_owned())
                .collect::<Vec<_>>(),
        );
        // `global_type_defs_by_def_id` narrows straight to the qualified
        // paths that could possibly resolve to `self_def_id`, instead of
        // scanning every type binding in the package on every impl block.
        // Deliberately bypasses `lookup_symbol`'s visibility filtering —
        // a definition's own canonical path is always resolvable from its
        // own impl block, regardless of privacy.
        let mut paths: Vec<_> = self
            .global_type_defs_by_def_id
            .get(&self_def_id)
            .into_iter()
            .flatten()
            .filter_map(|key| self.tree_lookup_raw(key, hir::Namespace::Type))
            .filter_map(|entry| entry.path.clone())
            .collect();
        paths.sort_by_key(|path| path.to_key());
        if paths.iter().any(|path| path == &relative) {
            return Ok(relative);
        }
        paths.into_iter().next().ok_or_else(|| {
            fp_core::Error::from(format!(
                "type definition `{self_def_id}` has no canonical path"
            ))
        })
    }

    fn should_export(&self, visibility: &ast::Visibility) -> bool {
        matches!(visibility, ast::Visibility::Public) && self.current_module_visibility_flag()
    }

    fn map_visibility(&self, visibility: &ast::Visibility) -> hir::Visibility {
        match visibility {
            ast::Visibility::Public => hir::Visibility::Public,
            ast::Visibility::Crate => hir::Visibility::Private,
            ast::Visibility::Restricted(_) => hir::Visibility::Private,
            ast::Visibility::Inherited => hir::Visibility::Private,
            ast::Visibility::Private => hir::Visibility::Private,
        }
    }

    fn map_abi(&self, abi: &ast::Abi) -> hir::Abi {
        match abi {
            ast::Abi::Rust => hir::Abi::Rust,
            ast::Abi::Named(name) if name == "C" => hir::Abi::C { unwind: false },
            ast::Abi::Named(name) => hir::Abi::Named(name.clone()),
        }
    }

    fn item_key(item: &ast::Item) -> u64 {
        item.id()
    }

    fn allocate_def_id_for_item(&mut self, item: &ast::Item) -> hir::DefId {
        let key = Self::item_key(item);
        if let Some(existing) = self.preassigned_def_ids.get(&key) {
            existing.clone()
        } else {
            let def_id = self.next_def_id();
            self.preassigned_def_ids.insert(key, def_id.clone());
            def_id
        }
    }

    fn def_id_for_item(&mut self, item: &ast::Item) -> hir::DefId {
        let key = Self::item_key(item);
        if let Some(id) = self.preassigned_def_ids.get(&key) {
            id.clone()
        } else {
            self.allocate_def_id_for_item(item)
        }
    }

    fn current_module_visibility_flag(&self) -> bool {
        *self.module_visibility.last().unwrap_or(&true)
    }

    fn compute_child_visibility(&self, visibility: &ast::Visibility) -> bool {
        match visibility {
            ast::Visibility::Public => self.current_module_visibility_flag(),
            _ => false,
        }
    }

    fn push_module_scope(&mut self, name: &str, visibility: &ast::Visibility) {
        self.module_path.push(name.to_string());
        let child_visibility = self.compute_child_visibility(visibility);
        self.module_visibility.push(child_visibility);
        self.push_type_scope();
        self.push_value_scope();
    }

    fn pop_module_scope(&mut self) {
        self.pop_value_scope();
        self.pop_type_scope();
        self.module_path.pop();
        self.module_visibility.pop();
        if self.module_visibility.is_empty() {
            self.module_visibility.push(true);
        }
    }

    fn qualify_name(&self, name: &str) -> String {
        self.qualify_path(name).to_key()
    }

    fn qualify_path(&self, name: &str) -> fp_core::ast::path::QualifiedPath {
        if self.module_path.is_empty() {
            fp_core::ast::path::QualifiedPath::new(vec![name.to_string()])
        } else {
            self.module_path.with_segment(name.to_string())
        }
    }

    /// A qualified-path `key` (`"a::b::c"`, or a bare name at the crate
    /// root) resolved against the module tree's bindings in namespace
    /// `ns`, with no visibility filtering — the raw lookup `lookup_symbol`
    /// applies `SymbolExport::can_access` on top of, and `canonical_type_path`
    /// deliberately uses unfiltered instead (a definition's own canonical
    /// path is always resolvable from its own impl block).
    fn tree_lookup_raw(&self, key: &str, ns: hir::Namespace) -> Option<&hir::SymbolEntry> {
        let mut segments: Vec<String> = key.split("::").map(|s| s.to_string()).collect();
        let leaf = segments.pop()?;
        let prefix = fp_core::ast::path::QualifiedPath::new(segments);
        // The crate root is represented by `ModuleTree::root()`, not by an
        // entry in `by_path`. Top-level definitions are bound there, so an
        // empty prefix must select the root directly rather than attempting
        // an impossible empty-path lookup.
        let module_id = if prefix.is_empty() {
            self.package.module_tree.root()
        } else {
            self.package.module_tree.module_id(&prefix)?
        };
        self.package.module_tree.lookup(module_id, ns, &leaf)
    }

    fn lookup_symbol(&self, key: &str, ns: hir::Namespace) -> Option<hir::Res> {
        self.tree_lookup_raw(key, ns).and_then(|entry| {
            if entry.export.can_access(&self.module_path.segments) {
                Some(entry.res.clone())
            } else {
                None
            }
        })
    }

    fn lookup_prelude_symbol(&self, name: &str, ns: hir::Namespace) -> Option<hir::Res> {
        self.package
            .module_tree
            .prelude_bindings(ns)
            .find_map(|(bound_name, entry)| (bound_name == name).then(|| entry.res.clone()))
    }

    /// Lexical scope only (generic parameters, locals pushed by
    /// `push_type_scope`/`push_value_scope`) — distinct from the
    /// module/prelude/global tiers `resolve_type_symbol`/`resolve_value_symbol`
    /// also consult. Used to tell a true lexical binding (an identity, not a
    /// module path — must not be canonicalized) apart from a same-named
    /// resolution that came from one of the other tiers (a real path that
    /// canonicalization should expand).
    fn resolve_lexical_type_symbol(&self, name: &str) -> Option<hir::Res> {
        self.type_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    fn resolve_lexical_value_symbol(&self, name: &str) -> Option<hir::Res> {
        self.value_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    /// The module-qualified/global tiers only — no lexical/local scope.
    /// Factored out of `resolve_type_symbol` so `self::`-prefixed paths
    /// (see `name_to_hir_path_with_scope`'s `PathPrefix::SelfMod` arm) can
    /// use it directly: `self::` is an explicit module path, semantically
    /// distinct from a bare name, and must never resolve to a same-named
    /// local/function-scoped shadow the way a bare reference correctly
    /// does.
    fn resolve_global_type_symbol(&self, name: &str) -> Option<hir::Res> {
        // A primitive type name is never shadowable by a same-named
        // module or re-export in type position — real Rust's own rule
        // (`isize::MAX` always means the primitive, even where vendored
        // std's own crate-root `pub use legacy_int_modules::{isize, ..}`
        // re-export makes a real, reachable module named `isize` exist
        // too). Without this, `lookup_symbol`/the ambiguous-export
        // fallback below would happily resolve the re-exported module
        // instead, and every ordinary `isize::`-prefixed UFCS/associated-
        // item reference elsewhere in the corpus would silently mean the
        // wrong thing.
        if is_primitive_type_name(name) {
            return None;
        }
        let cache_key = (
            self.module_path.to_key(),
            hir::Namespace::Type,
            name.to_string(),
        );
        if let Some(cached) = self.global_symbol_cache.borrow().get(&cache_key) {
            return cached.clone();
        }
        let qualified = self.module_path.with_segment(name.to_string()).to_key();
        let resolved = self
            .lookup_symbol(&qualified, hir::Namespace::Type)
            .or_else(|| self.lookup_prelude_symbol(name, hir::Namespace::Type))
            .or_else(|| self.lookup_symbol(name, hir::Namespace::Type));
        self.global_symbol_cache
            .borrow_mut()
            .insert(cache_key, resolved.clone());
        resolved
    }

    fn resolve_type_symbol(&self, name: &str) -> Option<hir::Res> {
        self.resolve_lexical_type_symbol(name)
            .or_else(|| self.resolve_global_type_symbol(name))
    }

    /// Trait bounds use the type namespace, but an ambiguous bare bound must
    /// select a trait declaration rather than a nominal type with the same
    /// name. Keep this context-sensitive choice in AST->HIR resolution.
    fn is_trait_definition(&self, def_id: &hir::DefId) -> bool {
        self.package
            .def_map
            .get(def_id)
            .cloned()
            .or_else(|| self.hir_program.item(def_id.clone()))
            .is_some_and(|item| matches!(item.kind, hir::ItemKind::Trait(_)))
    }

    fn resolve_trait_symbol(&self, name: &str) -> Option<hir::Res> {
        let is_trait = |res: Option<hir::Res>| match res {
            Some(hir::Res::Def(def_id)) if self.is_trait_definition(&def_id) => {
                Some(hir::Res::Def(def_id))
            }
            _ => None,
        };

        // Trait bounds use the same lexical/module/prelude scopes as ordinary
        // type paths. The expected trait namespace affects the interpretation
        // of an already-resolved binding; it does not authorize a workspace
        // suffix search for an arbitrary declaration with the same name.
        is_trait(self.resolve_lexical_type_symbol(name))
            .or_else(|| {
                let qualified = self.module_path.with_segment(name.to_string()).to_key();
                is_trait(self.lookup_symbol(&qualified, hir::Namespace::Type))
            })
            .or_else(|| is_trait(self.lookup_prelude_symbol(name, hir::Namespace::Type)))
            .or_else(|| is_trait(self.lookup_symbol(name, hir::Namespace::Type)))
    }

    fn resolve_value_symbol(&self, name: &str) -> Option<hir::Res> {
        self.resolve_lexical_value_symbol(name)
            .or_else(|| self.resolve_global_value_symbol(name))
    }

    /// Tagged operations are owned by the package that declares their
    /// definition. Foreign operations are queried through the program with
    /// the definition's package identity, never copied into this package.
    fn op_kind_for_def(&self, def_id: hir::DefId) -> Option<fp_core::intrinsics::PortableOp> {
        self.package
            .op_defs
            .get(&def_id)
            .cloned()
            .or_else(|| self.hir_program.op_def(def_id))
    }

    /// Same tiers as `resolve_value_symbol`, minus the lexical-scope tier —
    /// used to answer "does this bare identifier already name something at
    /// module/prelude/workspace scope?" without that answer being masked by
    /// a shadowing local. Used to disambiguate a bare-identifier *pattern*
    /// (`None`, a unit variant) from an ordinary new-binding pattern of the
    /// same syntax — the same ambiguity real Rust resolves via name
    /// resolution, not parser syntax.
    fn resolve_global_value_symbol(&self, name: &str) -> Option<hir::Res> {
        // Mirrors `resolve_global_type_symbol`'s identical guard: a
        // primitive type name is never shadowable by a same-named module
        // or re-export — real Rust's own rule (`u8::MAX` always means the
        // primitive, even where vendored std's own crate-root `pub use
        // legacy_int_modules::{u8, ..}` re-export makes a real, reachable
        // module named `u8` exist too). Guarded here (the value
        // namespace) too, not just the type namespace, since `u8::MAX` is
        // itself a *value*-position type-relative access whose base
        // segment resolves through this exact function.
        if is_primitive_type_name(name) {
            return None;
        }
        let cache_key = (
            self.module_path.to_key(),
            hir::Namespace::Value,
            name.to_string(),
        );
        if let Some(cached) = self.global_symbol_cache.borrow().get(&cache_key) {
            return cached.clone();
        }
        let qualified = self.module_path.with_segment(name.to_string()).to_key();
        let resolved = self
            .lookup_symbol(&qualified, hir::Namespace::Value)
            .or_else(|| self.lookup_prelude_symbol(name, hir::Namespace::Value))
            .or_else(|| self.lookup_symbol(name, hir::Namespace::Value));
        self.global_symbol_cache
            .borrow_mut()
            .insert(cache_key, resolved.clone());
        resolved
    }

    fn push_value_scope(&mut self) {
        self.value_scopes.push(HashMap::new());
        self.const_list_length_scopes.push(HashMap::new());
    }

    fn pop_value_scope(&mut self) {
        self.value_scopes.pop();
        if self.value_scopes.is_empty() {
            self.value_scopes.push(HashMap::new());
        }
        self.const_list_length_scopes.pop();
        if self.const_list_length_scopes.is_empty() {
            self.const_list_length_scopes.push(HashMap::new());
        }
    }

    fn push_type_scope(&mut self) {
        self.type_scopes.push(HashMap::new());
    }

    fn pop_type_scope(&mut self) {
        self.type_scopes.pop();
        if self.type_scopes.is_empty() {
            self.type_scopes.push(HashMap::new());
        }
    }

    /// Create a span for the current position
    fn create_span(&mut self, length: u32) -> Span {
        let span = Span::new(
            self.current_file,
            self.current_position,
            self.current_position + length,
        );
        self.current_position += length;
        span
    }

    /// Transform an AST expression tree to HIR
    pub fn transform_expr(&mut self, ast_expr: &ast::Expr) -> Result<hir::HirPackage> {
        let mut lowered_expr = ast_expr.clone();
        let (generated_items, closure_diagnostics) = lower_closures_in_expr(&mut lowered_expr)?;
        self.diagnostics.add_diagnostics(closure_diagnostics);
        if let Some(query) = lower_fp_expr_to_query(&lowered_expr, None) {
            return self.transform_query_document(&query);
        }
        self.reset_file_context("<expr>");
        self.prepare_lowering_state();
        self.load_default_prelude_defs();
        self.predeclare_items(&generated_items, false)?;

        let mut hir_program = hir::HirPackage::new(self.package_id.clone());
        self.program_def_map = HashMap::new();
        self.local_dispatch_items.clear();

        for item in &generated_items {
            self.append_item(&mut hir_program, item)?;
        }

        // Transform the root expression into a main function. The
        // synthesized `main`'s return type must match the expression's own
        // type — callers that lower this program all the way through MIR
        // (e.g. a comptime block that needs the expression's value) rely on
        // the declared output type matching the body, which HIR→MIR lowering
        // checks; a hardcoded `()` output would then be a mismatch for any
        // non-unit expression.
        let output = match fp_core::ast::resolved_expr_type(ast_expr.id()) {
            Some(ty) => self.transform_type_to_hir(&ty)?,
            None => self.create_unit_type(),
        };
        let main_body_expr = self.transform_expr_to_hir(&lowered_expr)?;
        let main_body = match main_body_expr.kind {
            hir::ExprKind::Block(block) => block,
            _ => hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: Some(Box::new(main_body_expr)),
            },
        };
        let main_fn = self.create_main_function(main_body, output)?;

        // Add main function to program
        let main_item = hir::Item {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            visibility: hir::Visibility::Public,
            kind: hir::ItemKind::Function(main_fn),
            span: self.create_span(4), // Span for "main" function
        };

        hir_program.items.push(main_item);

        if !self.synthetic_items.is_empty() {
            let mut synthetic = std::mem::take(&mut self.synthetic_items);
            for item in &synthetic {
                hir_program
                    .def_map
                    .insert(item.def_id.clone(), item.clone());
                self.program_def_map
                    .insert(item.def_id.clone(), item.clone());
            }
            hir_program.items.extend(synthetic.drain(..));
        }

        Ok(hir_program)
    }

    /// Transform a module's items into HIR directly, without an `ast::File`
    /// wrapper — used for on-demand compilation of workspace-crate modules
    /// (e.g. `std::meta`), where the driver already has
    /// `(QualifiedPath, Vec<Item>)` in hand. Unlike `transform_package`, this
    /// sets `module_path` to the real module identity rather than always
    /// leaving it empty.
    pub fn transform_module(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        items: &[ast::Item],
    ) -> Result<hir::HirPackage> {
        self.transform_module_inner(module_path, module_path.to_key(), items)
    }

    /// Lower a module after the driver has made its package dependencies
    /// available in the typing context.
    pub async fn transform_module_async(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        items: &[ast::Item],
        typing_shared: std::rc::Rc<std::cell::RefCell<fp_typing::HirTypeChecker>>,
    ) -> Result<hir::HirPackage> {
        let _ = typing_shared;
        self.transform_module(module_path, items)
    }

    /// Struct field types (name -> [(field name, field type)]) from
    /// whatever packages `self.workspace`'s package-scoped `crates()` map
    /// currently holds — used to seed `ClosureLowering`'s structural
    /// closure-argument-type derivation with definitions that live outside
    /// the package currently being lowered (see its call site in
    /// `transform_package`).
    ///
    /// This relies on a package's *ordinary* declared dependencies (e.g.
    /// skln-git's dependency on skln-core) actually being present in
    /// `crates()` — which in turn relies on `RustPackageProvider::
    /// load_package_metadata` (fp-rust/src/provider.rs) reporting them at
    /// all. It previously always returned empty `PackageMetadata`, so
    /// `CompilerDriver::compile_package`'s dependency loop had nothing to
    /// recurse into for a real project's own path dependencies (only
    /// `std`/`libc` were ever wired up) — the analogue of rustc never
    /// being told about an `--extern` crate. Fixed by
    /// `workspace_path_dependencies` parsing each package's own
    /// `Cargo.toml` `[dependencies]` table and resolving `path = ".."`
    /// entries against the discovered workspace members.
    fn workspace_struct_field_types(&self) -> HashMap<String, Vec<(String, ast::Ty)>> {
        let mut result = HashMap::new();
        let Some(workspace) = &self.workspace else {
            return result;
        };
        for package in workspace.crates().values() {
            for package_item in &package.borrow().items {
                if let ItemKind::DefStruct(def) = package_item.item.kind() {
                    let fields = def
                        .value
                        .fields
                        .iter()
                        .map(|field| (field.name.as_str().to_string(), field.value.clone()))
                        .collect();
                    result.insert(def.name.as_str().to_string(), fields);
                }
            }
        }
        result
    }

    pub fn transform_package(
        &mut self,
        package: &fp_core::ast::package::AstPackage,
    ) -> Result<hir::HirPackage> {
        self.reset_file_context("<package>");
        self.prepare_lowering_state();
        // The module tree otherwise only ever gains a node via an explicit
        // `mod X { .. }` AST node (`record_module_def`, common for
        // `.fp`-dialect source) or another package's own tree
        // own module tree when a provider represents it implicitly, one
        // module per source file with no literal `Module` wrapper item at
        // all (e.g. `fp-rust`'s real-std provider). Without this, a bare
        // `use crate::sibling_module;`-style import can never resolve as
        // a module alias for such a package, no matter how its target
        // path is computed. This also seeds every real "extern prelude"
        // entry `name_to_hir_path_with_scope`'s plain-multi-segment branch
        // relies on (a sub-crate root, e.g. `"core"`/`"std"` under the
        // vendored real Rust `std` package, is just a child of the
        // package-root node — no separate table needed).
        for path in &package.module_paths {
            self.package.module_tree.ensure_module(path);
        }

        // Unlike `transform_file` (the single-file path), `transform_package`
        // never ran the `lower_closures_in_file` pre-pass that decomposes a
        // closure literal into an ordinary struct+function pair before HIR
        // lowering sees it — see `lower_closures_in_items`'s doc comment.
        // Run it here, once, on a local mutable copy; its generated
        // `__ClosureN`/`__closureN_call` items are synthetic and not tied to
        // any one source module, so they're scoped to the package root.
        let mut lowered_items: Vec<ast::Item> =
            package.items.iter().map(|pi| pi.item.clone()).collect();
        let original_len = lowered_items.len();
        // A closure argument's receiver (e.g. `node.stats` in
        // `node.stats.as_ref().map_or(..)`) is frequently a struct defined
        // in a *dependency* package, not this one — collect every already
        // -compiled package's struct field types too, so
        // `closure_param_ty_for_invoke`'s structural lookup isn't blind to
        // them (see `ClosureLowering::collect_struct_field_types`).
        // Targets with `capabilities.first_class_closures` set (Kotlin/etc.)
        // lower a closure literal directly into a real
        // `hir::ExprKind::Closure` node instead (see
        // `transform_expr_to_hir_inner`'s `Closure` arm) — running this
        // pre-pass too would defunctionalize it first, defeating that
        // entirely.
        if !self.lowering_config.capabilities.first_class_closures {
            let dependency_struct_field_types = self.workspace_struct_field_types();
            lower_closures_in_items(
                &mut lowered_items,
                &dependency_struct_field_types,
                self.package_id.as_str(),
            )?;
        }
        let generated_count = lowered_items.len() - original_len;
        let root_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        let package_items: Vec<fp_core::ast::package::PackageItem> = lowered_items
            .into_iter()
            .enumerate()
            .map(|(i, item)| {
                let path = if i < generated_count {
                    root_path.clone()
                } else {
                    package.items[i - generated_count].module_path.clone()
                };
                fp_core::ast::package::PackageItem {
                    module_path: path,
                    item,
                }
            })
            .collect();
        // Item-position `macro_rules!` invocations (real std's own idiom for
        // generating a batch of items — e.g. `std/os/raw/mod.rs`'s
        // `alias_core_ffi! { c_int c_uint .. }`, expanding to `pub type
        // c_int = core::ffi::c_int;` etc.) previously reached
        // `predeclare_items`'s `ItemKind::Macro` arm unexpanded and were
        // silently dropped with a warning — meaning every item such a
        // macro generates (across real std, primarily C-FFI type aliases)
        // was simply never defined at all, not a resolution gap. Expand
        // them for real here, before any definition/import pass runs, the
        // same way `normalize_macro` (`fp_lang::normalization`) already
        // expands an *expression*-position invocation: match each rule in
        // declaration order, substitute the bindings, re-parse the result.
        let package_items = self.expand_item_macros(package_items);

        // The Rust provider flattens inline modules into package items while
        // retaining each item's owning module path. Those paths are real
        // modules even when no separate provider descriptor exists, such as
        // `alloc::collections::btree::node::marker`.
        for package_item in &package_items {
            self.package
                .module_tree
                .ensure_module(&package_item.module_path);
        }

        let mut program = hir::HirPackage::new(self.package.id.clone());

        // 1: definitions (tolerant — impls whose self-type isn't resolvable
        // yet, because it's only reachable through an import that hasn't
        // been processed, get deferred into `pending_impls` instead of
        // failing immediately; see `predeclare_items`'s `ItemKind::Impl` arm).
        for package_item in &package_items {
            self.with_module_scope(&package_item.module_path, |this| {
                this.predeclare_items(std::slice::from_ref(&package_item.item), true)
            })?;
        }

        // 2: imports — needs every definition above to already exist,
        // crate-wide, since an import can reference any file's item
        // regardless of processing order. Never attempted before append
        // until now; this fixed-point worklist also makes re-export
        // chains resolve, not just direct single-hop imports.
        self.resolve_pending_imports(package)?;

        // Predeclaration may have probed a transparent alias RHS before its
        // imports existed and cached that miss (`type Result = result::Result`
        // in core::io::error is the standard-library case). Imports have now
        // changed the module symbol tables, so those negative cache entries
        // are no longer valid when deferred aliases are lowered below.
        self.global_symbol_cache.borrow_mut().clear();

        // 3: type aliases whose module-qualified RHS
        // (`type Result = result::Result<(), Error>;`) was deferred above.
        for (module_path, item) in std::mem::take(&mut self.pending_type_aliases) {
            self.with_module_scope(&module_path, |this| {
                this.predeclare_items(std::slice::from_ref(&item), false)
            })?;
        }
        self.program_def_map = program.def_map.clone();

        // `load_default_prelude_defs` scans *this* package's own module
        // tree for prelude-tagged entries (see its doc comment) — those
        // bindings are only populated by `predeclare_items` (steps 1/3
        // above), so this must run after both, never before. Running it
        // before predeclare meant a package's own prelude
        // module scanned an always-empty tree,
        // silently hiding every one of its own prelude items (`Vec`,
        // `String`, `Option`, `Box`, `Rc`, ...) from its own unqualified
        // uses — exactly the case when compiling `std` itself, whose source
        // relies on its own prelude bare names throughout. Cross-package
        // prelude (a *dependency*'s prelude, via `workspace.hir_definitions()`
        // below) was never affected by this ordering bug since it doesn't
        // depend on this package's own tables at all.
        self.load_default_prelude_defs();

        // Deferred impl headers are lowered after the package prelude is
        // installed as well as after imports, since source impls commonly
        // use bare prelude types such as `Option`.
        for (module_path, item) in std::mem::take(&mut self.pending_impls) {
            self.with_module_scope(&module_path, |this| {
                this.predeclare_items(std::slice::from_ref(&item), false)
            })?;
        }

        // 5: append — unchanged.
        for package_item in &package_items {
            self.with_module_scope(&package_item.module_path, |this| {
                this.append_item(&mut program, &package_item.item)
            })?;
        }

        if !self.synthetic_items.is_empty() {
            let mut synthetic = std::mem::take(&mut self.synthetic_items);
            for item in &synthetic {
                program.def_map.insert(item.def_id.clone(), item.clone());
                self.program_def_map
                    .insert(item.def_id.clone(), item.clone());
            }
            program.items.extend(synthetic.drain(..));
        }
        // The real Rust primitive documentation module declares these
        // methods, but it is not part of the provider's lowered item graph.
        // Materialize its two raw-pointer `cast` declarations here, before
        // rebuilding the normal impl indices, so raw pointers participate in
        // method lookup through the same shape bucket as every other
        // non-nominal primitive.
        for item in self.raw_pointer_cast_impls()? {
            self.program_def_map
                .insert(item.def_id.clone(), item.clone());
            program.items.push(item);
        }
        program
            .items
            .extend(std::mem::take(&mut self.local_dispatch_items));
        program.def_map = self.program_def_map.clone();
        program.def_paths = self.package.def_paths.clone();
        for (def_id, block) in self.package.anonymous_consts() {
            program.add_anonymous_const(def_id, block);
        }
        program.placeholder_defs = self.package.placeholder_defs.clone();
        program.op_defs.extend(self.package.op_defs.clone());
        program
            .intrinsic_defs
            .extend(self.package.intrinsic_defs.clone());
        program
            .type_alias_targets
            .extend(self.package.type_alias_targets.clone());
        // Crate metadata must travel with the published HIR snapshot. The
        // consumer lowerer uses this edge set to select the implicit prelude;
        // deriving it again from a transient package workspace makes the
        // result depend on recursive-scope lifetime.
        program.dependencies = self.package.dependencies.clone();
        // Dependency resolution walks the returned HIR package's module tree
        // segment by segment for imports. The lowerer records bindings in its
        // package-owned tree while transforming, so publish that completed
        // tree alongside the other HIR indexes before returning the package.
        program.module_tree = self.package.module_tree.clone();
        // Every item above was appended straight to `program.items`, not
        // through `add_item`, so the derived impl-candidate indices
        // (`impls_by_self_did`/`impls_by_shape`/`blanket_impls`) are
        // still empty at this point — see `HirPackage::
        // index_derived_lookups`'s own doc comment for why this
        // bulk-construction path needs this explicit rebuild call.
        // Without it, every method/associated-type candidate search
        // (`impls_for_adt`/`impls_for_shape`/`blanket_impls`,
        // `fp-typing`'s `shape_and_blanket_candidates`) finds nothing,
        // for every package this lowerer ever produces.
        program.index_derived_lookups();
        Ok(program)
    }

    fn raw_pointer_cast_impls(&mut self) -> Result<Vec<hir::Item>> {
        [false, true]
            .into_iter()
            .map(|mutable| {
                let impl_def_id = self.next_def_id();
                self.with_owner(impl_def_id.clone(), |this| {
                    let span = Span::new(this.current_file, 0, 0);
                    let t_def_id = this.next_def_id();
                    let u_def_id = this.next_def_id();
                    let method_def_id = this.next_def_id();

                    let generic_param =
                        |this: &mut Self, name: &str, def_id: hir::DefId| hir::GenericParam {
                            hir_id: this.next_id(),
                            def_id,
                            name: hir::Symbol::new(name),
                            kind: hir::GenericParamKind::Type { default: None },
                            bounds: Vec::new(),
                            explicit_bindings: Vec::new(),
                            projection_bounds: Vec::new(),
                        };
                    let type_param = |this: &mut Self, name: &str, def_id: hir::DefId| {
                        hir::TypeExpr::new(
                            this.next_id(),
                            hir::TypeExprKind::Path(hir::Path {
                                segments: vec![hir::PathSegment {
                                    name: hir::Symbol::new(name),
                                    args: None,
                                }],
                                res: Some(hir::Res::Def(def_id)),
                            }),
                            span,
                        )
                    };
                    let pointer = |this: &mut Self, inner: hir::TypeExpr| {
                        hir::TypeExpr::new(
                            this.next_id(),
                            hir::TypeExprKind::Ptr {
                                inner: Box::new(inner),
                                mutable,
                            },
                            span,
                        )
                    };

                    let t_ty = type_param(this, "T", t_def_id.clone());
                    let u_ty = type_param(this, "U", u_def_id.clone());
                    let self_ty = pointer(this, t_ty);
                    let output = pointer(this, u_ty);
                    let self_param = hir::Param {
                        hir_id: this.next_id(),
                        pat: hir::Pat {
                            hir_id: this.next_id(),
                            kind: hir::PatKind::Binding {
                                name: hir::Symbol::new("self"),
                                mutable: false,
                            },
                        },
                        ty: self_ty.clone(),
                        is_context: false,
                        as_tuple: false,
                        as_dict: false,
                        default: None,
                    };
                    let method = hir::Function::new(
                        hir::FunctionSig {
                            name: hir::Symbol::new("cast"),
                            inputs: vec![self_param],
                            output,
                            generics: hir::Generics {
                                params: vec![generic_param(this, "U", u_def_id)],
                                where_clause: None,
                            },
                            abi: hir::Abi::Rust,
                        },
                        None,
                        true,
                        false,
                    );

                    Ok(hir::Item {
                        hir_id: this.next_id(),
                        def_id: impl_def_id,
                        visibility: hir::Visibility::Private,
                        kind: hir::ItemKind::Impl(hir::Impl {
                            generics: hir::Generics {
                                params: vec![generic_param(this, "T", t_def_id)],
                                where_clause: None,
                            },
                            trait_ty: None,
                            self_ty,
                            items: vec![hir::ImplItem {
                                def_id: method_def_id,
                                hir_id: this.next_id(),
                                name: hir::Symbol::new("cast"),
                                kind: hir::ImplItemKind::Method(method),
                            }],
                        }),
                        span,
                    })
                })
            })
            .collect()
    }

    /// Resolves every `ItemKind::Import` item in `package` as a small
    /// fixed-point worklist: collect all bindings up front (this needs no
    /// global state, just each import's own `module_path` context), then
    /// keep sweeping whatever's still unresolved until a full sweep makes
    /// no further progress. This is what makes re-export chains resolve
    /// (`pub use` re-exporting another `pub use`), not just direct,
    /// single-hop imports — a single sweep would only catch the latter.
    /// Whatever's left unresolved after the fixed point is left as-is,
    /// exactly like today's single-sweep behavior — not a new error
    /// surface, genuinely-unresolvable imports behave the same as before.
    fn resolve_pending_imports(
        &mut self,
        package: &fp_core::ast::package::AstPackage,
    ) -> Result<()> {
        let mut pending: Vec<(
            fp_core::ast::path::QualifiedPath,
            ImportBinding,
            ast::Visibility,
        )> = Vec::new();
        // Collects every `use` in `item`, at whatever nesting depth —
        // `package.items`' own `module_path` only reflects each *file's*
        // location (`RustPackageProvider` gives every `.rs` file's
        // top-level items that file's own module path), but a source file
        // can still write `mod foo { use ...; }` inline rather than as a
        // separate `mod foo;` file declaration (real std's own
        // `std::prelude::v1`'s `mod ambiguous_macros_only { pub use crate
        // ::*; }`), nesting the `use` one or more `ast::ItemKind::Module`
        // levels below the file's own path. A single flat scan over
        // `package.items` never looks inside those, so an import written
        // this way was silently never collected as pending at all.
        fn collect_pending_imports_recursive(
            this: &mut AstToHirLowerer,
            module_path: &fp_core::ast::path::QualifiedPath,
            item: &ast::Item,
            pending: &mut Vec<(
                fp_core::ast::path::QualifiedPath,
                ImportBinding,
                ast::Visibility,
            )>,
        ) -> Result<()> {
            match item.kind() {
                ItemKind::Import(import) => {
                    this.with_module_scope(module_path, |this| {
                        let mut bindings = Vec::new();
                        this.collect_imports(Vec::new(), &import.tree, &mut bindings)?;
                        for binding in bindings {
                            pending.push((module_path.clone(), binding, import.visibility.clone()));
                        }
                        Ok(())
                    })?;
                }
                ItemKind::Module(module) => {
                    let nested_path = module_path.with_segment(module.name.name.clone());
                    for child in &module.items {
                        collect_pending_imports_recursive(this, &nested_path, child, pending)?;
                    }
                }
                _ => {}
            }
            Ok(())
        }
        for package_item in &package.items {
            collect_pending_imports_recursive(
                self,
                &package_item.module_path,
                &package_item.item,
                &mut pending,
            )?;
        }

        // A named import is only ever retried while it keeps making
        // progress and is removed from `pending` the moment it resolves,
        // so it can never itself loop forever. A glob is different (see
        // its branch below): it's deliberately never removed, since its
        // expansion can keep growing as sibling imports resolve. Both are
        // bounded by the same real invariant — a finite crate has a finite
        // number of (module, namespace, name) bindings, so genuine
        // progress can only happen finitely many times — but a defensive
        // cap still guards against a resolver bug (or a pathological
        // re-export cycle this fixed point doesn't actually converge on)
        // turning into a silent hang instead of a loud failure.
        const MAX_SWEEPS: usize = 1000;
        let mut sweeps = 0usize;
        loop {
            sweeps += 1;
            if sweeps > MAX_SWEEPS {
                return Err(fp_core::error::Error::from(format!(
                    "internal compiler error: import resolution did not reach a fixed point after {MAX_SWEEPS} sweeps (likely an unbounded re-export cycle)"
                )));
            }
            let mut progressed = false;
            let mut still_pending = Vec::with_capacity(pending.len());
            for (module_path, binding, visibility) in pending {
                if binding.is_glob {
                    // Re-expand from scratch every sweep instead of once at
                    // collection time — the source module's own bindings
                    // (e.g. `v1`'s re-exported `Some`/`None`) may still be
                    // unresolved on earlier sweeps, so a one-shot expansion
                    // would permanently see an empty module. A glob is
                    // never removed from `pending`: its expansion can keep
                    // growing as sibling imports resolve, so it's retried
                    // every sweep until the whole worklist reaches a fixed
                    // point (tracked via `resolved_import_aliases` growth
                    // below, since `register_import_binding` itself returns
                    // `true` for an already-resolved alias too).
                    let aliases_before = self.resolved_import_aliases.len();
                    self.with_module_scope(&module_path, |this| {
                        let mut fresh = Vec::new();
                        this.expand_glob_import(binding.target.clone(), &mut fresh);
                        for fresh_binding in fresh {
                            this.register_import_binding(fresh_binding, &visibility);
                        }
                        Ok(())
                    })?;
                    if self.resolved_import_aliases.len() > aliases_before {
                        progressed = true;
                    }
                    still_pending.push((module_path, binding, visibility));
                    continue;
                }
                let resolved = self.with_module_scope(&module_path, |this| {
                    Ok(this.register_import_binding(binding.clone(), &visibility))
                })?;
                if resolved {
                    progressed = true;
                } else {
                    still_pending.push((module_path, binding, visibility));
                }
            }
            pending = still_pending;
            if !progressed || pending.is_empty() {
                break;
            }
        }
        Ok(())
    }

    /// Transform a query document node into HIR.
    pub fn transform_query_document(&mut self, query: &QueryDocument) -> Result<hir::HirPackage> {
        let file_name = query.name.as_deref().unwrap_or("<query>");
        self.reset_file_context(file_name);
        self.prepare_lowering_state();
        self.load_default_prelude_defs();
        self.program_def_map = HashMap::new();
        self.local_dispatch_items.clear();

        let ir = self.resolve_query_ir(query)?;
        let span = self.create_span(query.source_len_hint() as u32);
        let item = hir::Item {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Query(hir::Query {
                origin: query_origin(query),
                ir,
                span,
            }),
            span,
        };

        let mut program = hir::HirPackage::new(self.package.id.clone());
        program.def_map.insert(item.def_id.clone(), item.clone());
        self.program_def_map
            .insert(item.def_id.clone(), item.clone());
        program.items.push(item);
        program.def_paths = self.package.def_paths.clone();
        program.placeholder_defs = self.package.placeholder_defs.clone();
        program.op_defs.extend(self.package.op_defs.clone());
        program
            .intrinsic_defs
            .extend(self.package.intrinsic_defs.clone());
        program
            .type_alias_targets
            .extend(self.package.type_alias_targets.clone());
        Ok(program)
    }

    fn transform_module_inner<P: AsRef<Path>>(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        file_label: P,
        items: &[ast::Item],
    ) -> Result<hir::HirPackage> {
        self.reset_file_context(file_label);
        self.prepare_lowering_state();

        self.module_path = module_path.clone();
        let mut program = hir::HirPackage::new(self.package.id.clone());
        self.load_default_prelude_defs();
        self.predeclare_items(items, false)?;
        self.program_def_map = program.def_map.clone();
        for item in &self.synthetic_items {
            self.program_def_map
                .insert(item.def_id.clone(), item.clone());
        }

        // Append in the same order: extra-module items (impls in particular)
        // must land in `program.items` *before* the caller's own functions —
        // MIR lowering processes `program.items` in a single linear pass, so
        // an `impl` block appearing after the function that calls into it
        // would still be unregistered (`struct_methods` empty) when that
        // call is lowered.
        self.module_path = module_path.clone();
        for item in items {
            self.append_item(&mut program, item)?;
        }

        if !self.synthetic_items.is_empty() {
            let mut synthetic = std::mem::take(&mut self.synthetic_items);
            for item in &synthetic {
                program.def_map.insert(item.def_id.clone(), item.clone());
                self.program_def_map
                    .insert(item.def_id.clone(), item.clone());
            }
            program.items.extend(synthetic.drain(..));
        }

        // Nested const items generated for const blocks are referenced by
        // their DefId when the type checker requests comptime evaluation.
        // Keep them in the program index even though they are not top-level
        // program items.
        program.def_map = self.program_def_map.clone();
        program.def_paths = self.package.def_paths.clone();
        program.placeholder_defs = self.package.placeholder_defs.clone();
        program.op_defs.extend(self.package.op_defs.clone());
        program
            .intrinsic_defs
            .extend(self.package.intrinsic_defs.clone());
        program
            .type_alias_targets
            .extend(self.package.type_alias_targets.clone());

        program.index_derived_lookups();
        Ok(program)
    }

    fn with_module_scope<T>(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        action: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let depth = module_path.segments.len();
        for segment in &module_path.segments {
            self.push_module_scope(segment, &ast::Visibility::Public);
        }
        let result = action(self);
        for _ in 0..depth {
            self.pop_module_scope();
        }
        result
    }

    fn resolve_query_ir(&mut self, query: &QueryDocument) -> Result<QueryIrDocument> {
        if let Some(semantic) = &query.semantic {
            if !semantic.is_empty() {
                return Ok(semantic.clone());
            }
            return Err(fp_core::error::Error::from(
                "query item missing semantic IR",
            ));
        }
        let statements = match &query.kind {
            QueryKind::Sql(sql) => {
                let source = sql.raw.clone().unwrap_or_else(|| sql.to_string());
                parse_sql_ast(&source, sql.dialect.clone()).map_err(|err| {
                    fp_core::error::Error::from(format!("failed to normalize SQL query: {err}"))
                })?
            }
            QueryKind::Prql(_prql) => {
                return Err(fp_core::error::Error::from("PRQL query has no semantic IR"));
            }
        };
        let mut lowered = Vec::with_capacity(statements.len());
        for statement in &statements {
            let Some(stmt) = statement_to_query_ir(statement) else {
                return Err(fp_core::error::Error::from(
                    "SQL query could not be lowered into semantic query IR",
                ));
            };
            lowered.push(stmt);
        }
        if lowered.is_empty() {
            return Err(fp_core::error::Error::from(
                "query item missing semantic IR",
            ));
        }
        Ok(QueryIrDocument {
            name: query.name.clone(),
            statements: lowered,
        })
    }

    fn append_item(&mut self, program: &mut hir::HirPackage, item: &ast::Item) -> Result<()> {
        if !self.item_enabled_by_cfg(item) {
            return Ok(());
        }
        if should_drop_quote_item(item) {
            return Ok(());
        }
        if should_drop_const_type_item(item) {
            return Ok(());
        }
        match item.kind() {
            ItemKind::Module(module) => {
                self.push_module_scope(&module.name.name, &module.visibility);
                for child in &module.items {
                    self.append_item(program, child)?;
                }
                self.pop_module_scope();
                Ok(())
            }
            ItemKind::Import(import) => {
                self.handle_import(import)?;
                Ok(())
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                if let Some(hir_item) = self.materialize_def_type_item(item, def_type)? {
                    program
                        .def_map
                        .insert(hir_item.def_id.clone(), hir_item.clone());
                    self.program_def_map
                        .insert(hir_item.def_id.clone(), hir_item.clone());
                    program.items.push(hir_item);
                }
                Ok(())
            }
            ItemKind::Expr(expr) => {
                if let ast::ExprKind::Value(value) = expr.kind() {
                    if matches!(value.as_ref(), ast::Value::Unit(_)) {
                        return Ok(());
                    }
                }
                let def_id = self.allocate_def_id_for_item(item);
                self.with_owner(def_id.clone(), |this| {
                    let hir_expr = this.transform_expr_to_hir(expr)?;
                    let hir_item = hir::Item {
                        hir_id: this.next_id(),
                        def_id: def_id.clone(),
                        visibility: hir::Visibility::Private,
                        kind: hir::ItemKind::Expr(hir_expr),
                        span: item.span(),
                    };
                    program
                        .def_map
                        .insert(hir_item.def_id.clone(), hir_item.clone());
                    this.program_def_map
                        .insert(hir_item.def_id.clone(), hir_item.clone());
                    program.items.push(hir_item);
                    Ok(())
                })
            }
            ItemKind::DeclFunction(decl) => {
                let hir_item = self.transform_decl_function(item, decl)?;
                program
                    .def_map
                    .insert(hir_item.def_id.clone(), hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id.clone(), hir_item.clone());
                program.items.push(hir_item);
                Ok(())
            }
            ItemKind::Macro(_) => {
                self.add_error(
                    Diagnostic::warning(
                        "dropping macro item during AST→HIR in lossy mode".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                Ok(())
            }
            _ => {
                let hir_item = self.transform_item_to_hir(item)?;
                program
                    .def_map
                    .insert(hir_item.def_id.clone(), hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id.clone(), hir_item.clone());
                program.items.push(hir_item);
                Ok(())
            }
        }
    }

    // Expression lowering helpers live in expressions.rs
    /// Transform an AST item into a HIR statement
    fn transform_item_to_hir_stmt(&mut self, item: &ast::BItem) -> Result<hir::StmtKind> {
        if should_drop_quote_item(item.as_ref()) {
            let unit_block = hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: None,
            };
            let unit_expr = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Block(unit_block),
                span: self.create_span(1),
            };
            return Ok(hir::StmtKind::Expr(unit_expr));
        }
        if should_drop_const_type_item(item.as_ref()) {
            let unit_block = hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: None,
            };
            let unit_expr = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Block(unit_block),
                span: self.create_span(1),
            };
            return Ok(hir::StmtKind::Expr(unit_expr));
        }

        match item.as_ref().kind() {
            ItemKind::Import(import) => {
                self.handle_import(import)?;
                let unit_block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Block(unit_block),
                    span: self.create_span(1),
                };
                Ok(hir::StmtKind::Expr(unit_expr))
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                if let Some(hir_item) = self.materialize_def_type_item(item.as_ref(), def_type)? {
                    Ok(hir::StmtKind::Item(hir_item))
                } else if let Some(inner) = comptime_type_alias_rhs(&def_type.value) {
                    // `type X = const { .. };` / `type X = EXPR;` (where
                    // `EXPR` needs compile-time evaluation to produce a
                    // concrete type, e.g. a `TypeBuilder`-constructed
                    // struct) has no `def_map` entry to give — a real
                    // struct/enum's shape is known up front, this one only
                    // once the checker evaluates `inner`. Lower it as an
                    // ordinary, eagerly-checked expression-position
                    // `ConstBlock` statement (per Part B: `const { .. }` in
                    // this position is transparent sugar, so both syntaxes
                    // collapse to the same node here), and bind `X`'s name
                    // to that const block's own `DefId` via `Res::Def` —
                    // scope-local only (like `register_type_generic`'s
                    // generics binding), not exported through
                    // `record_value_symbol`/`record_type_symbol`, since this
                    // name is lexically scoped to this statement, not a real
                    // module-level definition. `path_ty`/`field_ty` read the
                    // resolved shape straight out of the package's own
                    // `const_block_values` by that `DefId` once this
                    // statement has been checked.
                    let alias_def_id = self.next_def_id();
                    // Lower the declared RHS as a type expression.  An
                    // explicit `const { ... }` remains a const-block query,
                    // but it is the alias target rather than the alias item.
                    let target = self.transform_type_to_hir(&def_type.value)?;
                    self.package.type_alias_targets.insert(
                        alias_def_id.clone(),
                        target,
                    );
                    self.current_value_scope()
                        .insert(
                            def_type.name.name.clone(),
                            hir::Res::Def(alias_def_id.clone()),
                        );
                    self.current_type_scope()
                        .insert(
                            def_type.name.name.clone(),
                            hir::Res::Def(alias_def_id.clone()),
                        );
                    Ok(hir::StmtKind::Item(hir::Item {
                        hir_id: self.next_id(),
                        visibility: hir::Visibility::Private,
                        def_id: alias_def_id.clone(),
                        kind: hir::ItemKind::TypeAlias(hir::TypeAlias {
                            name: hir::Symbol::new(def_type.name.name.clone()),
                            target: self
                                .package
                                .type_alias_targets
                                .get(&alias_def_id)
                                .cloned()
                                .expect("local comptime type alias target was registered"),
                        }),
                        span: item.span(),
                    }))
                } else {
                    let unit_block = hir::Block {
                        hir_id: self.next_id(),
                        stmts: Vec::new(),
                        expr: None,
                    };
                    let unit_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Block(unit_block),
                        span: self.create_span(1),
                    };
                    Ok(hir::StmtKind::Expr(unit_expr))
                }
            }
            ItemKind::Macro(_) => {
                self.add_error(
                    Diagnostic::warning(
                        "dropping macro item in statement position during AST→HIR".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                let unit_block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Block(unit_block),
                    span: self.create_span(1),
                };
                Ok(hir::StmtKind::Expr(unit_expr))
            }
            _ => {
                // `transform_item_to_hir` is shared with real top-level
                // items, so it unconditionally registers `item`'s name into
                // the module-qualified global symbol tables
                // (`register_value_def`/`register_type_def` ->
                // `record_value_symbol`/`record_type_symbol`) as a side
                // effect. That's correct for a real module item, but this
                // call site handles a function-local item statement (e.g.
                // a `const`/`struct` declared inside a function body) —
                // such an item is only visible via the enclosing block's
                // lexical scope (`current_value_scope`/`current_type_scope`,
                // already pushed/popped per block, and unaffected by this
                // guard), never via a module-qualified/`self::`-style
                // lookup. `suppress_global_registration_depth` (see its
                // doc comment) makes `record_value_symbol`/
                // `record_type_symbol` skip the global registration while
                // this item (and anything nested inside its own body) is
                // being lowered — guarding the whole call, not just this
                // item's own registration, since the same collision can
                // recur for any item nested inside its body. Without this,
                // a function-local item whose name happens to match a real
                // module-level item (e.g. `core/time.rs`'s module-level
                // `NANOS_PER_SEC` const and an unrelated function-local
                // `const NANOS_PER_SEC` shadowing it) clobbers that module
                // item's global registration *before* the local item's own
                // body is lowered, so a `self::`-qualified reference in
                // that very body (meant to reach past its own shadow to
                // the module item) resolves back to itself — a genuine,
                // silent self-reference baked directly into the HIR, which
                // the per-item typecheck task executor then (correctly, if
                // confusingly) reports as a stalled dependency cycle.
                self.suppress_global_registration_depth += 1;
                let hir_item = self.transform_item_to_hir(item.as_ref());
                self.suppress_global_registration_depth -= 1;
                let hir_item = hir_item?;
                self.program_def_map
                    .insert(hir_item.def_id.clone(), hir_item.clone());
                if matches!(
                    hir_item.kind,
                    hir::ItemKind::Trait(_) | hir::ItemKind::Impl(_)
                ) {
                    self.local_dispatch_items.push(hir_item.clone());
                }
                if let Some(ident) = item.as_ref().get_ident() {
                    let label = format!(
                        "{}::<local>::{}",
                        self.module_path.to_key(),
                        ident.name.as_str()
                    );
                    self.local_item_debug_labels
                        .insert(hir_item.def_id.clone(), label);
                }
                Ok(hir::StmtKind::Item(hir_item))
            }
        }
    }

    /// Transform an AST item into a HIR item
    fn transform_item_to_hir(&mut self, item: &ast::Item) -> Result<hir::Item> {
        let def_id = self.def_id_for_item(item);
        self.with_owner(def_id.clone(), |this| {
            this.transform_item_to_hir_inner(item, def_id)
        })
    }

    fn transform_item_to_hir_inner(
        &mut self,
        item: &ast::Item,
        def_id: hir::DefId,
    ) -> Result<hir::Item> {
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let (kind, visibility) = match item.kind() {
            ItemKind::DefConst(const_def) => {
                self.register_value_def(
                    &const_def.name.name,
                    def_id.clone(),
                    &const_def.visibility,
                );
                let hir_const = self.transform_const_def(const_def)?;
                (
                    hir::ItemKind::Const(hir_const),
                    self.map_visibility(&const_def.visibility),
                )
            }
            ItemKind::DefStatic(static_def) if attrs_has_name(&static_def.attrs, "host") => {
                self.register_value_def(
                    &static_def.name.name,
                    def_id.clone(),
                    &static_def.visibility,
                );
                let konst = self.transform_static_def(static_def)?;
                (
                    hir::ItemKind::Const(konst),
                    self.map_visibility(&static_def.visibility),
                )
            }
            ItemKind::DefStruct(struct_def) => {
                self.register_type_def(
                    &struct_def.name.name,
                    def_id.clone(),
                    &struct_def.visibility,
                );
                self.register_value_def(
                    &struct_def.name.name,
                    def_id.clone(),
                    &struct_def.visibility,
                );
                self.push_type_scope();
                let generics = self.transform_generics(&struct_def.value.generics_params);
                let name = hir::Symbol::new(struct_def.name.name.clone());
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics,
                        repr: attrs_repr(&struct_def.attrs),
                    }),
                    self.map_visibility(&struct_def.visibility),
                )
            }
            ItemKind::DefStructural(struct_def) => {
                self.register_type_def(
                    &struct_def.name.name,
                    def_id.clone(),
                    &struct_def.visibility,
                );
                self.register_value_def(
                    &struct_def.name.name,
                    def_id.clone(),
                    &struct_def.visibility,
                );
                let name = hir::Symbol::new(struct_def.name.name.clone());
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics: hir::Generics::default(),
                        repr: attrs_repr(&struct_def.attrs),
                    }),
                    self.map_visibility(&struct_def.visibility),
                )
            }
            ItemKind::OpaqueType(opaque_def) => {
                self.register_type_def(
                    &opaque_def.name.name,
                    def_id.clone(),
                    &opaque_def.visibility,
                );
                let name = hir::Symbol::new(opaque_def.name.name.clone());
                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields: Vec::new(),
                        generics: hir::Generics::default(),
                        repr: attrs_repr(&opaque_def.attrs),
                    }),
                    self.map_visibility(&opaque_def.visibility),
                )
            }
            ItemKind::DefEnum(enum_def) => {
                self.register_type_def(&enum_def.name.name, def_id.clone(), &enum_def.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&enum_def.value.generics_params);
                let qualified_enum_name = hir::Symbol::new(enum_def.name.name.clone());

                let variants = enum_def
                    .value
                    .variants
                    .iter()
                    .map(|variant| {
                        let variant_path = fp_core::ast::path::QualifiedPath::new(vec![
                            enum_def.name.name.clone(),
                            variant.name.name.clone(),
                        ]);
                        let qualified_variant = variant_path.to_key();
                        let fully_qualified = if self.module_path.is_empty() {
                            qualified_variant.clone()
                        } else {
                            self.module_path.join(&variant_path.segments).to_key()
                        };

                        let variant_def_id = if let Some(def_id) =
                            self.enum_variant_def_ids.get(&fully_qualified).cloned()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id.clone());
                            new_id
                        };

                        // Record the `Enum::Variant`-qualified registration
                        // first so its more complete path (including the
                        // enum name segment) wins the `def_paths` entry —
                        // `register_value_def` below re-registers the same
                        // `def_id` under the bare variant name alone (for
                        // unqualified in-scope lookup), which must not
                        // clobber the canonical path.
                        //
                        // `record_value_path` (already-split segments), not
                        // `record_value_symbol` (bare `&str` re-split as a
                        // single malformed segment) — see the matching
                        // `predeclare_items` `DefEnum` arm's doc comment.
                        self.record_value_path(
                            &self.module_path.join(&variant_path.segments),
                            hir::Res::Def(variant_def_id.clone()),
                            &enum_def.visibility,
                        );
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id.clone(),
                            &enum_def.visibility,
                        );

                        let discriminant = variant
                            .discriminant
                            .as_ref()
                            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                            .transpose()?;
                        let payload = match &variant.value {
                            ast::Ty::Unit(_) => {
                                if let Some(alias) =
                                    self.lookup_type_alias(&[variant.name.name.clone()])
                                {
                                    let alias = alias.clone();
                                    Some(self.transform_type_to_hir(&alias)?)
                                } else {
                                    None
                                }
                            }
                            ast::Ty::Structural(structural) => {
                                Some(self.materialize_enum_struct_payload(
                                    &enum_def.name.name,
                                    &variant.name.name,
                                    structural,
                                )?)
                            }
                            other => Some(self.transform_type_to_hir(other)?),
                        };

                        Ok(hir::EnumVariant {
                            attrs: variant.attrs.clone(),
                            hir_id: self.next_id(),
                            def_id: variant_def_id,
                            name: hir::Symbol::new(variant.name.name.clone()),
                            discriminant,
                            payload,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Enum(hir::Enum {
                        attrs: enum_def.attrs.clone(),
                        name: qualified_enum_name,
                        variants,
                        generics,
                        repr: attrs_repr(&enum_def.attrs),
                    }),
                    self.map_visibility(&enum_def.visibility),
                )
            }
            ItemKind::DefFunction(func_def) => {
                self.register_value_def(&func_def.name.name, def_id.clone(), &func_def.visibility);
                if let Some(tag) = fp_core::intrinsics::extract_op_attr(&func_def.attrs, "func") {
                    if let Some(op) = fp_core::lang::resolve_portable_op_tag(&tag) {
                        self.package.op_defs.insert(def_id.clone(), op);
                    }
                }
                if let Some(tag) = fp_core::lang::extract_intrinsic_item(&func_def.attrs) {
                    if let Some(kind) = fp_core::intrinsics::lang_intrinsic_for_lang_item(&tag)
                        .and_then(fp_core::intrinsics::lang_intrinsic_call_kind)
                    {
                        self.package.intrinsic_defs.insert(def_id.clone(), kind);
                    }
                }
                let lower_body = !attrs_has_name(&func_def.attrs, "unimplemented");
                let mut function = self.transform_function_with_body(func_def, None, lower_body)?;
                // Many `std` functions have a fake body whose sole purpose is
                // satisfying the type checker's signature requirements — the
                // compiler synthesizes the real implementation elsewhere,
                // e.g. `impl str { pub fn len(&self) -> usize {
                // compile_error!("compiler intrinsic") } }`. Type-checking
                // such a body for real would hard-error (see hir_typeck's
                // `IntrinsicKind::CompileError` handling), so drop it back
                // to a stub here — but only for genuine markers like this,
                // not for every function that merely happens to live under
                // `std::**` (a real, hand-written function such as
                // `std::bench::run_benches` or `std::json::parse` must keep
                // its real body).
                if function_body_is_compiler_intrinsic_marker(&function) {
                    function.body = None;
                }
                (
                    hir::ItemKind::Function(function),
                    self.map_visibility(&func_def.visibility),
                )
            }
            ItemKind::DeclFunction(func_decl) => {
                self.register_value_def(
                    &func_decl.name.name,
                    def_id.clone(),
                    &ast::Visibility::Public,
                );
                let function = self.transform_decl_function_sig(func_decl, None)?;
                (hir::ItemKind::Function(function), hir::Visibility::Public)
            }
            ItemKind::DeclStatic(decl) => {
                // An external symbol declaration (real `std::sys::alloc::
                // vexos`'s linkerscript-provided `__heap_start`) — its
                // real value comes from the linker at link time, not from
                // any expression this compiler could evaluate. HIR has no
                // declaration-only static/const shape (`hir::ItemKind::
                // Const` always carries a body), so this fabricates an
                // integer-literal placeholder body — deliberately picked
                // so `expr_path_ty`'s literal fast path (matching on
                // `Literal(Integer(_))`) reports this const's type as its
                // own *declared* type to callers, instead of the
                // placeholder body's actual (irrelevant) inferred type.
                self.register_value_def(&decl.name.name, def_id.clone(), &ast::Visibility::Public);
                let ty = self.transform_type_to_hir(&decl.ty)?;
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
                        span: self.create_span(1),
                    },
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(decl.name.name.clone()),
                    ty,
                    body,
                    mutable: decl.mutable,
                    is_host: decl.is_host,
                };
                (hir::ItemKind::Const(konst), hir::Visibility::Public)
            }
            ItemKind::Impl(impl_block) => {
                let hir_impl = self.transform_impl(impl_block)?;
                (hir::ItemKind::Impl(hir_impl), hir::Visibility::Private)
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Bool(false)),
                    span: self.create_span(1),
                };
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: unit_expr,
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(def_type.name.name.clone()),
                    ty: self.create_simple_type("bool"),
                    body,
                    mutable: false,
                    is_host: false,
                };
                (hir::ItemKind::Const(konst), hir::Visibility::Private)
            }
            ItemKind::DefTrait(def_trait) => {
                // Backends that model traits as real interfaces (e.g.
                // fp-kotlin) still work off the original, pristine
                // `ast::Item` instead of anything lifted from this HIR
                // shape — recording `def_id` in `placeholder_defs`
                // (mirrored into `hir::HirPackage::placeholder_defs`) lets
                // `HirToAstLifter::lift_items_by_path` skip lifting this
                // item, so typed-splice (`typecheck_package`) falls back to
                // the real trait declaration for codegen. This real
                // `hir::ItemKind::Trait` shape exists purely so HIR
                // typechecking's method resolution (`HirTypeChecker::
                // method_output`'s trait-default-method fallback) has
                // somewhere to find a trait's default-method signatures
                // and associated-type declarations — see that function's
                // doc comment.
                self.package.placeholder_defs.insert(def_id.clone());
                let hir_trait = self.transform_trait(def_trait)?;
                (
                    hir::ItemKind::Trait(hir_trait),
                    self.map_visibility(&def_trait.visibility),
                )
            }
            _ => {
                self.add_error(
                    Diagnostic::error(format!(
                        "Unimplemented AST item type for HIR transformation: {:?}",
                        item
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Bool(false)),
                    span: self.create_span(1),
                };
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: unit_expr,
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(format!("__fp_error_{def_id}")),
                    ty: self.create_simple_type("bool"),
                    body,
                    mutable: false,
                    is_host: false,
                };
                return Ok(hir::Item {
                    hir_id,
                    def_id,
                    visibility: hir::Visibility::Private,
                    kind: hir::ItemKind::Const(konst),
                    span,
                });
            }
        };

        Ok(hir::Item {
            hir_id,
            def_id,
            visibility,
            kind,
            span,
        })
    }

    fn transform_decl_function(
        &mut self,
        item: &ast::Item,
        decl: &ast::ItemDeclFunction,
    ) -> Result<hir::Item> {
        let def_id = self.def_id_for_item(item);
        self.with_owner(def_id.clone(), |this| {
            let hir_id = this.next_id();
            let span = this.create_span(1);
            this.register_value_def(&decl.name.name, def_id.clone(), &ast::Visibility::Public);
            let function = this.transform_decl_function_sig(decl, None)?;
            Ok(hir::Item {
                hir_id,
                def_id,
                visibility: hir::Visibility::Public,
                kind: hir::ItemKind::Function(function),
                span,
            })
        })
    }

    fn transform_const_def(&mut self, const_def: &ast::ItemDefConst) -> Result<hir::Const> {
        let list_len = self.const_list_length_from_expr(&const_def.value);
        if let Some(len) = list_len {
            self.record_const_list_length(&const_def.name.name, len);
        }
        let ty = if let Some(ty) = &const_def.ty {
            if let (ast::Ty::Vec(vec_ty), Some(len)) = (ty, list_len) {
                let len_expr =
                    ast::Expr::new(ast::ExprKind::Value(Box::new(ast::Value::int(len as i64))));
                let array_ty = ast::Ty::Array(ast::TypeArray {
                    elem: vec_ty.ty.clone(),
                    len: Box::new(len_expr),
                });
                self.transform_type_to_hir(&array_ty)?
            } else {
                self.transform_type_to_hir(ty)?
            }
        } else {
            self.create_unit_type()
        };

        // A `const` item's initializer is already an implicitly const-
        // evaluated position — an explicit `const { EXPR }` wrapper here
        // is redundant sugar for `EXPR` itself, not a distinct construct
        // (unlike inside a `fn` body, where it's the one place `const {
        // .. }` carves out a compile-time-evaluated island in otherwise-
        // runtime code, and keeps its own `hir::ExprKind::ConstBlock`
        // handling untouched). Unwrapping here means `lower_const_expr`
        // only ever needs to constant-fold "an expression in const
        // position," never a `ConstBlock` shape on top of that.
        let value_expr = match const_def.value.kind() {
            ast::ExprKind::ConstBlock(const_block) => const_block.expr.as_ref(),
            _ => &const_def.value,
        };
        let value = self.transform_expr_to_hir(value_expr)?;
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value,
        };

        Ok(hir::Const {
            name: hir::Symbol::new(const_def.name.name.clone()),
            ty,
            body,
            mutable: const_def.mutable.unwrap_or(false),
            is_host: false,
        })
    }

    fn transform_static_def(&mut self, def: &ast::ItemDefStatic) -> Result<hir::Const> {
        let ty = self.transform_type_to_hir(&def.ty)?;
        let is_host = attrs_has_name(&def.attrs, "host");
        // A host global has no Ferro-owned initializer. Keep a typed dummy
        // body for the HIR shape while avoiding recursive lowering of the
        // source initializer during native compilation.
        let value = if is_host {
            self.create_simple_literal(0)
        } else {
            self.transform_expr_to_hir(def.value.as_ref())?
        };
        Ok(hir::Const {
            name: hir::Symbol::new(def.name.name.clone()),
            ty,
            body: hir::Body {
                hir_id: self.next_id(),
                params: Vec::new(),
                value,
            },
            mutable: def.mutable.unwrap_or(false),
            is_host,
        })
    }

    fn const_list_length_from_expr(&self, expr: &ast::Expr) -> Option<usize> {
        match expr.kind() {
            ast::ExprKind::Array(array) => Some(array.values.len()),
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::List(list) => Some(list.values.len()),
                _ => None,
            },
            _ => None,
        }
    }

    fn record_const_list_length(&mut self, name: &str, len: usize) {
        if let Some(scope) = self.const_list_length_scopes.last_mut() {
            scope.insert(name.to_string(), len);
        }
    }

    fn lookup_const_list_length(&self, segments: &[ast::Ident]) -> Option<usize> {
        if segments.len() != 1 {
            return None;
        }
        let name = segments[0].name.as_str();
        self.const_list_length_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    /// Transform an AST type into a HIR type
    fn transform_type_to_hir(&mut self, ty: &ast::Ty) -> Result<hir::TypeExpr> {
        match ty {
            ast::Ty::Primitive(prim) => Ok(self.primitive_type_to_hir(*prim)),
            ast::Ty::Struct(struct_ty) => {
                let alias_info = self
                    .lookup_type_alias_with_key(&[struct_ty.name.name.to_string()])
                    .map(|(key, alias)| (key, alias.clone()));
                if let Some((key, alias)) = alias_info {
                    let span = self.normalize_span(ty.span());
                    if !self.enter_type_alias(&key, span) {
                        return Ok(self.error_type_expr(span));
                    }
                    let result = self.with_type_alias_definition_scope(&key, |this| {
                        this.transform_type_to_hir(&alias)
                    });
                    self.exit_type_alias(&key);
                    return result;
                }
                let path = self.name_to_hir_path_with_scope(
                    &Name::Ident(struct_ty.name.clone()),
                    PathResolutionScope::Type,
                )?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Reference(reference) => {
                let inner = self.transform_type_to_hir(&reference.ty)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Ref(Box::new(inner)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::RawPtr(raw_ptr) => {
                let inner = self.transform_type_to_hir(&raw_ptr.ty)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Ptr {
                        inner: Box::new(inner),
                        mutable: raw_ptr.mutability == Some(true),
                    },
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Unit(_) => Ok(self.create_unit_type()),
            ast::Ty::Nothing(_) => Ok(self.create_null_type()),
            ast::Ty::Any(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Any,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::TypeBounds(bounds) => {
                // `dyn Fn(..) -> ..`/`FnMut`/`FnOnce` bounds (`dyn FnMut(&T)
                // -> bool`, commonly boxed as `Box<dyn FnMut(..) -> ..>`)
                // parse through the same `FnMut(..) -> ..` sugar-folding as
                // a bare `fn(..) -> ..` type (`ast::Value::Type(ast::Ty::
                // Function(fn_ty))`, per `fp_lang::ast::type_to_expr`) —
                // the exact same shape the `ast::Ty::ImplTraits` arm above
                // already special-cases for `impl Fn(..)`. Without this,
                // the generic `ExprKind::Name` match below never matches
                // (a `Value::Type(Function)` bound has no `Name` at all),
                // `primary_trait_name` falls through to `None`, and the
                // whole `dyn FnMut(..)` type erases to `Infer`, losing its
                // signature entirely. Build the same real `FnPtr` HIR type
                // here too.
                if let Some(bound) = bounds.bounds.first() {
                    if let ast::ExprKind::Value(value) = bound.kind() {
                        if let ast::Value::Type(ast::Ty::Function(fn_ty)) = value.as_ref() {
                            let inputs = fn_ty
                                .params
                                .iter()
                                .map(|ty| self.transform_type_to_hir(ty).map(Box::new))
                                .collect::<Result<Vec<_>>>()?;
                            let output = if let Some(ret_ty) = &fn_ty.ret_ty {
                                Box::new(self.transform_type_to_hir(ret_ty)?)
                            } else {
                                Box::new(self.create_unit_type())
                            };
                            return Ok(hir::TypeExpr::new(
                                self.next_id(),
                                hir::TypeExprKind::FnPtr(hir::FnPtrType { inputs, output }),
                                self.normalize_span(ty.span()),
                            ));
                        }
                    }
                }
                let dynamic_bounds = bounds
                    .bounds
                    .iter()
                    .filter_map(|bound| {
                        self.ast_expr_to_hir_path(bound, PathResolutionScope::Trait)
                            .ok()
                    })
                    .collect::<Vec<_>>();
                if dynamic_bounds.is_empty() {
                    Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Infer,
                        Span::new(self.current_file, 0, 0),
                    ))
                } else {
                    Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Dynamic(dynamic_bounds),
                        Span::new(self.current_file, 0, 0),
                    ))
                }
            }
            ast::Ty::Unknown(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Tuple(tuple) => {
                let elements = tuple
                    .types
                    .iter()
                    .map(|ty| Ok(Box::new(self.transform_type_to_hir(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Tuple(elements),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::TypeStructuralField {
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: Box::new(self.transform_type_to_hir(&field.value)?),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Structural(hir::TypeStructural { fields }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Vec(vec_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&vec_ty.ty)?);
                let args = hir::GenericArgs {
                    args: vec![hir::GenericArg::Type(elem)],
                };
                // `Vec<T>` is a nominal ADT type, even though the parser has
                // a dedicated AST variant for its surface spelling. Resolve
                // the nominal head through the ordinary type namespace before
                // attaching the argument. Leaving `res` empty makes an impl
                // such as `impl<T> Trait for Vec<T>` indistinguishable from
                // an unresolved path to the HIR impl index, so it cannot be
                // placed in rustc's ADT dispatch bucket.
                let mut path = self.name_to_hir_path_with_scope(
                    &Name::Ident(ast::Ident::new("Vec")),
                    PathResolutionScope::Type,
                )?;
                if let Some(last) = path.segments.last_mut() {
                    last.args = Some(args);
                }
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Array(array_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&array_ty.elem)?);
                let len_expr = Box::new(self.transform_expr_to_hir(array_ty.len.as_ref())?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Array(elem, Some(len_expr)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Slice(slice_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&slice_ty.elem)?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Slice(elem),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::TypeBinaryOp(type_op) => {
                if let Some(kind) = self.literal_type_kind(ty) {
                    let expr = match kind {
                        LiteralTypeKind::Primitive(prim) => self.primitive_type_to_hir(prim),
                        LiteralTypeKind::Unit => self.create_unit_type(),
                        LiteralTypeKind::Null => self.create_null_type(),
                    };
                    return Ok(expr);
                }
                let lhs = self.transform_type_to_hir(&type_op.lhs)?;
                let rhs = self.transform_type_to_hir(&type_op.rhs)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::TypeBinaryOp(hir::TypeBinaryOp {
                        kind: type_op.kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Projection(projection) => {
                let self_ty = Box::new(self.transform_type_to_hir(&projection.self_ty)?);
                let trait_ty = self.transform_type_to_hir(&projection.trait_ty)?;
                let hir::TypeExprKind::Path(trait_path) = trait_ty.kind else {
                    return Ok(self.error_type_expr(ty.span()));
                };
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Projection(hir::TypeProjection {
                        self_ty,
                        trait_path,
                        assoc: projection.assoc.name.clone().into(),
                    }),
                    self.normalize_span(ty.span()),
                ))
            }
            ast::Ty::Value(type_value) => {
                let expr = match type_value.value.as_ref() {
                    ast::Value::Int(_) => {
                        self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))
                    }
                    ast::Value::Bool(_) => self.primitive_type_to_hir(ast::TypePrimitive::Bool),
                    ast::Value::Decimal(_) => self
                        .primitive_type_to_hir(ast::TypePrimitive::Decimal(ast::DecimalType::F64)),
                    ast::Value::String(_) => self.primitive_type_to_hir(ast::TypePrimitive::String),
                    ast::Value::Char(_) => self.primitive_type_to_hir(ast::TypePrimitive::Char),
                    ast::Value::Unit(_) => self.create_unit_type(),
                    ast::Value::Null(_) | ast::Value::None(_) => self.create_null_type(),
                    ast::Value::Type(ty) => {
                        return self.transform_type_to_hir(ty);
                    }
                    other => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "unsupported literal type in AST→HIR lowering: {:?}",
                                other
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(ty.span()),
                        );
                        return Ok(hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Error,
                            Span::new(self.current_file, 0, 0),
                        ));
                    }
                };
                Ok(expr)
            }
            ast::Ty::Quote(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Never,
                self.normalize_span(ty.span()),
            )),
            ast::Ty::Type(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Type,
                ty.span(),
            )),
            ast::Ty::ConstBlock(block) => {
                if let ast::ExprKind::Value(value) = block.expr.kind() {
                    match value.as_ref() {
                        ast::Value::Type(ty) => return self.transform_type_to_hir(ty),
                        _ => {}
                    }
                }
                // Only try path resolution for expressions that are
                // actually path-shaped (`const { SomeType }`,
                // `const { module::Type }`, ...) — `ast_expr_to_hir_path`
                // falls back to an `__fp_error` placeholder `Ok(..)` rather
                // than `Err` for anything else, which would otherwise steer
                // every non-path const-block body (literals, arithmetic,
                // blocks, ...) away from the comptime-resolving fallback
                // below and into a silently-wrong error path.
                if matches!(
                    block.expr.kind(),
                    ast::ExprKind::Name(_) | ast::ExprKind::Select(_)
                ) {
                    if let Ok(path) =
                        self.ast_expr_to_hir_path(block.expr.as_ref(), PathResolutionScope::Type)
                    {
                        return Ok(hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Path(path),
                            Span::new(self.current_file, 0, 0),
                        ));
                    }
                }
                // Fall through — the const block produces a type at comptime;
                // the type checker resolves it via `HirTypeChecker::request_comptime`
                // when it encounters this node.
                let body = Box::new(self.transform_expr_to_hir(block.expr.as_ref())?);
                let def_id = self.next_def_id();
                let hir_id = self.next_id();
                // Recorded once, unconditionally, right here — see
                // `hir::HirPackage::const_block_defs`'s doc comment.
                self.package.add_anonymous_const(
                    def_id.clone(),
                    hir::Block {
                        hir_id: hir_id.clone(),
                        stmts: Vec::new(),
                        expr: Some(body.clone()),
                    },
                );
                Ok(hir::TypeExpr::new(
                    hir_id,
                    hir::TypeExprKind::ConstBlock(def_id, body),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Refinement(refinement) => {
                let base = Box::new(self.transform_type_to_hir(&refinement.base)?);
                let binder = hir::Symbol::new(refinement.binder.name.clone());
                let predicate = Box::new(self.transform_expr_to_hir(&refinement.predicate)?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Refinement {
                        base,
                        binder,
                        predicate,
                    },
                    self.normalize_span(ty.span()),
                ))
            }
            ast::Ty::Literal(lit) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::LiteralString(lit.value.clone()),
                self.normalize_span(ty.span()),
            )),
            ast::Ty::Expr(expr) => {
                // The resolver result belongs to this expression node even
                // when the type is wrapped as `Value::Expr`.  Consult it
                // before unwrapping the value; otherwise the nested path
                // construction below can produce an unresolved HIR path for
                // a resolved bare type such as `String`.
                if let Some(path) = self.resolved_type_path(expr)? {
                    return Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Path(path),
                        self.normalize_span(ty.span()),
                    ));
                }
                // `_` in type position (`Vec<_>`, a turbofish arg, ...) —
                // real inference-placeholder syntax, not a real path to
                // resolve. Reaches here as a bare `Name::Ident("_")`
                // expression (fp-lang parses it as an ordinary identifier,
                // not a dedicated `ast::Ty::Wildcard` node, in every
                // position this crate's own `parse_type_arg` builds a
                // `Ty::Expr` from) — without this check it falls all the
                // way through to `ast_expr_to_hir_path`, which has no
                // declaration named `_` to resolve, producing a genuine
                // "unresolved type path `_`" error for what should just
                // silently infer.
                if matches!(
                    expr.kind(),
                    ast::ExprKind::Name(fp_core::ast::Name::Ident(ident)) if ident.name.as_str() == "_"
                ) {
                    return Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Infer,
                        self.normalize_span(ty.span()),
                    ));
                }
                if let ast::ExprKind::Value(value) = expr.kind() {
                    match value.as_ref() {
                        ast::Value::Type(ty) => {
                            return self.transform_type_to_hir(ty);
                        }
                        ast::Value::Expr(inner) => {
                            if let Ok(path) =
                                self.ast_expr_to_hir_path(inner, PathResolutionScope::Type)
                            {
                                return Ok(hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(path),
                                    Span::new(self.current_file, 0, 0),
                                ));
                            }
                        }
                        _ => {}
                    }
                }
                // A bare `f"...{...}..."` in type position is an implicit
                // const block — type position is already a const-eval
                // context, so there's no need for the explicit `const { }`
                // wrapper this otherwise mirrors exactly (see the
                // `ast::Ty::ConstBlock` arm above). Checked explicitly by
                // shape (an intrinsic call can never be a type path) rather
                // than folded into the generic path-resolution-failed
                // fallback below, which exists to produce a real
                // "unresolved type" error for genuine mistakes.
                if let ast::ExprKind::IntrinsicCall(call) = expr.kind() {
                    if call.kind == fp_core::intrinsics::CallKind::Format {
                        let body = Box::new(self.transform_expr_to_hir(expr)?);
                        let def_id = self.next_def_id();
                        let hir_id = self.next_id();
                        self.package.add_anonymous_const(
                            def_id.clone(),
                            hir::Block {
                                hir_id: hir_id.clone(),
                                stmts: Vec::new(),
                                expr: Some(body.clone()),
                            },
                        );
                        return Ok(hir::TypeExpr::new(
                            hir_id,
                            hir::TypeExprKind::ConstBlock(def_id, body),
                            self.normalize_span(ty.span()),
                        ));
                    }
                }
                // Transparent aliases intentionally have no nominal HIR item
                // or module-tree binding. Resolve their source path before
                // asking the nominal path resolver: a re-export chain such
                // as `core::io::Result -> alloc::io::Result ->
                // std::io::Result` otherwise reaches no binding at all and
                // can never get to the alias expansion below.
                let alias_segments = match expr.kind() {
                    ast::ExprKind::Name(ast::Name::Ident(ident)) => Some(vec![ident.name.clone()]),
                    ast::ExprKind::Name(ast::Name::Path(path)) => Some(
                        path.segments
                            .iter()
                            .map(|segment| segment.name.clone())
                            .collect(),
                    ),
                    ast::ExprKind::Name(ast::Name::ParameterPath(path)) => Some(
                        path.segments
                            .iter()
                            .map(|segment| segment.ident.name.clone())
                            .collect(),
                    ),
                    _ => None,
                };
                if let Some(segments) = alias_segments {
                    if let Some((key, alias)) = self.lookup_type_alias_with_key(&segments) {
                        let span = self.normalize_span(ty.span());
                        if !self.enter_type_alias(&key, span) {
                            return Ok(self.error_type_expr(span));
                        }
                        let result = self.with_type_alias_definition_scope(&key, |this| {
                            this.transform_type_to_hir(&alias)
                        });
                        self.exit_type_alias(&key);
                        return result;
                    }
                }
                if let Ok(path) = self.ast_expr_to_hir_path(expr, PathResolutionScope::Type) {
                    let segments = path
                        .segments
                        .iter()
                        .map(|seg| seg.name.as_str().to_string())
                        .collect::<Vec<_>>();
                    let alias_info = self
                        .lookup_type_alias_with_key(&segments)
                        .map(|(key, alias)| (key, alias.clone()));
                    if let Some((key, alias)) = alias_info {
                        let span = self.normalize_span(ty.span());
                        if !self.enter_type_alias(&key, span) {
                            return Ok(self.error_type_expr(span));
                        }
                        let result = self.with_type_alias_definition_scope(&key, |this| {
                            this.transform_type_to_hir(&alias)
                        });
                        self.exit_type_alias(&key);
                        return result;
                    }
                    return Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Path(path),
                        Span::new(self.current_file, 0, 0),
                    ));
                }
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::ImplTraits(impl_traits) => {
                if let Some(bound) = impl_traits.bounds.bounds.first() {
                    // `impl Fn(..) -> ..`/`FnMut`/`FnOnce` bounds parse the
                    // same way a bare `fn(..) -> ..` type does (`ast::
                    // Value::Type(ast::Ty::Function(fn_ty))`, per `fp_lang::
                    // ast::type_to_expr`) — but routing that through
                    // `ast_expr_to_hir_path` (the generic path below, for
                    // ordinary trait bounds like `impl Display`) only ever
                    // produces a placeholder `Res::Builtin(BuiltinSelfType::
                    // Function)` path with the param/return types discarded
                    // (see that function's own `Value::Type(Ty::Function)`
                    // arm). Build the same real `FnPtr` HIR type the plain
                    // `ast::Ty::Function(fn_ty)` arm below constructs,
                    // instead, so the closure-hint machinery downstream
                    // (`fp-typing`) has an actual signature to match against.
                    if let ast::ExprKind::Value(value) = bound.kind() {
                        if let ast::Value::Type(ast::Ty::Function(fn_ty)) = value.as_ref() {
                            let inputs = fn_ty
                                .params
                                .iter()
                                .map(|ty| self.transform_type_to_hir(ty).map(Box::new))
                                .collect::<Result<Vec<_>>>()?;
                            let output = if let Some(ret_ty) = &fn_ty.ret_ty {
                                Box::new(self.transform_type_to_hir(ret_ty)?)
                            } else {
                                Box::new(self.create_unit_type())
                            };
                            return Ok(hir::TypeExpr::new(
                                self.next_id(),
                                hir::TypeExprKind::FnPtr(hir::FnPtrType { inputs, output }),
                                self.normalize_span(ty.span()),
                            ));
                        }
                    }
                    if let Ok(path) = self.ast_expr_to_hir_path(bound, PathResolutionScope::Trait) {
                        return Ok(hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Path(path),
                            self.normalize_span(ty.span()),
                        ));
                    }
                }
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Infer,
                    self.normalize_span(ty.span()),
                ))
            }
            ast::Ty::Function(fn_ty) => {
                let inputs = fn_ty
                    .params
                    .iter()
                    .map(|ty| self.transform_type_to_hir(ty).map(Box::new))
                    .collect::<Result<Vec<_>>>()?;

                let output = if let Some(ret_ty) = &fn_ty.ret_ty {
                    Box::new(self.transform_type_to_hir(ret_ty)?)
                } else {
                    Box::new(self.create_unit_type())
                };

                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::FnPtr(hir::FnPtrType { inputs, output }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            unsupported => {
                self.add_error(
                    Diagnostic::warning(format!(
                        "unsupported type in AST→HIR lowering: {:?}",
                        unsupported
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(self.normalize_span(unsupported.span())),
                );
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    Span::new(self.current_file, 0, 0),
                ))
            }
        }
    }

    /// Create a simple HIR literal expression
    pub fn create_simple_literal(&mut self, value: i64) -> hir::Expr {
        hir::Expr::new(
            self.next_id(),
            hir::ExprKind::Literal(hir::Lit::Integer(value)),
            Span::new(0, 0, 0),
        )
    }

    /// Create a simple HIR type
    pub fn create_simple_type(&mut self, type_name: &str) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: hir::Symbol::new(type_name),
                    args: None,
                }],
                res: None,
            }),
            Span::new(0, 0, 0),
        )
    }

    fn error_type_expr(&mut self, span: Span) -> hir::TypeExpr {
        hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Error, span)
    }

    fn create_unit_type(&mut self) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Tuple(Vec::new()),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn create_null_type(&mut self) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: hir::Symbol::new("null"),
                    args: None,
                }],
                res: None,
            }),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn literal_type_kind(&self, ty: &ast::Ty) -> Option<LiteralTypeKind> {
        match ty {
            ast::Ty::Value(type_value) => match type_value.value.as_ref() {
                ast::Value::Int(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Int(
                    ast::TypeInt::I64,
                ))),
                ast::Value::Bool(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Bool)),
                ast::Value::Decimal(_) => Some(LiteralTypeKind::Primitive(
                    ast::TypePrimitive::Decimal(ast::DecimalType::F64),
                )),
                ast::Value::String(_) => {
                    Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String))
                }
                ast::Value::Bytes(bytes)
                    if bytes.value.as_ref().strip_suffix(&[0]).is_some()
                        && std::str::from_utf8(
                            bytes.value.as_ref().strip_suffix(&[0]).unwrap_or_default(),
                        )
                        .is_ok() =>
                {
                    Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String))
                }
                ast::Value::Char(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Char)),
                ast::Value::Unit(_) => Some(LiteralTypeKind::Unit),
                ast::Value::Null(_) | ast::Value::None(_) => Some(LiteralTypeKind::Null),
                _ => None,
            },
            ast::Ty::TypeBinaryOp(op) if matches!(op.kind, ast::TypeBinaryOpKind::Union) => {
                let lhs = self.literal_type_kind(&op.lhs)?;
                let rhs = self.literal_type_kind(&op.rhs)?;
                if lhs == rhs { Some(lhs) } else { None }
            }
            _ => None,
        }
    }

    fn literal_type_kind_from_value(&self, value: &ast::Value) -> Option<LiteralTypeKind> {
        match value {
            ast::Value::Int(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Int(
                ast::TypeInt::I64,
            ))),
            ast::Value::Bool(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Bool)),
            ast::Value::Decimal(_) => Some(LiteralTypeKind::Primitive(
                ast::TypePrimitive::Decimal(ast::DecimalType::F64),
            )),
            ast::Value::String(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String)),
            ast::Value::Bytes(bytes)
                if bytes.value.as_ref().strip_suffix(&[0]).is_some()
                    && std::str::from_utf8(
                        bytes.value.as_ref().strip_suffix(&[0]).unwrap_or_default(),
                    )
                    .is_ok() =>
            {
                Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String))
            }
            ast::Value::Char(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Char)),
            ast::Value::Unit(_) => Some(LiteralTypeKind::Unit),
            ast::Value::Null(_) | ast::Value::None(_) => Some(LiteralTypeKind::Null),
            ast::Value::Type(ty) => self.literal_type_kind(ty),
            _ => None,
        }
    }

    fn structural_field_value_to_ty(&mut self, value: &ast::Value) -> ast::Ty {
        if let ast::Value::Type(ty) = value {
            return ty.clone();
        }
        if let Some(ty) = self
            .literal_type_kind_from_value(value)
            .and_then(|kind| match kind {
                LiteralTypeKind::Primitive(prim) => Some(ast::Ty::Primitive(prim)),
                LiteralTypeKind::Unit => Some(ast::Ty::Unit(ast::TypeUnit)),
                LiteralTypeKind::Null => Some(ast::Ty::Nothing(ast::TypeNothing)),
            })
        {
            return ty;
        }
        self.add_error(
            Diagnostic::error(format!(
                "unsupported structural field value type: {:?}",
                value
            ))
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(value.span()),
        );
        ast::Ty::Unknown(ast::TypeUnknown)
    }

    fn structural_specs_compatible(
        &self,
        existing: &[StructuralFieldSpec],
        incoming: &[StructuralFieldSpec],
    ) -> bool {
        if existing.len() != incoming.len() {
            return false;
        }

        existing.iter().zip(incoming.iter()).all(|(lhs, rhs)| {
            if lhs.name != rhs.name {
                return false;
            }
            lhs.ty == rhs.ty
        })
    }

    fn structural_value_key(&self, fields: &[StructuralFieldSpec]) -> String {
        let mut parts = Vec::with_capacity(fields.len());
        for field in fields {
            let ty_key = match field.ty {
                LiteralTypeKind::Primitive(prim) => format!("{:?}", prim),
                LiteralTypeKind::Unit => "unit".to_string(),
                LiteralTypeKind::Null => "null".to_string(),
            };
            parts.push(format!("{}:{}", field.name, ty_key));
        }
        parts.join("|")
    }

    fn structural_value_name(&self, key: &str) -> String {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        format!("__structural_value_{:x}", hasher.finish())
    }

    fn find_compatible_structural_value_def(
        &self,
        fields: &[StructuralFieldSpec],
    ) -> Option<StructuralValueDef> {
        self.structural_value_defs
            .values()
            .find(|candidate| self.structural_specs_compatible(&candidate.fields, fields))
            .cloned()
    }

    fn register_structural_value_def(
        &mut self,
        name: String,
        fields: Vec<StructuralFieldSpec>,
        hir_fields: Vec<hir::StructField>,
        ast_fields: Vec<ast::StructuralField>,
    ) -> StructuralValueDef {
        let def_id = self.next_def_id();
        let name_symbol = hir::Symbol::new(name.clone());
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let struct_item = hir::Item {
            hir_id,
            def_id: def_id.clone(),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Struct(hir::Struct {
                name: name_symbol,
                fields: hir_fields,
                generics: hir::Generics::default(),
                repr: ast::ReprOptions::default(),
            }),
            span,
        };

        self.register_type_def(&name, def_id.clone(), &ast::Visibility::Private);
        self.struct_field_defs.insert(def_id.clone(), ast_fields);
        self.synthetic_items.push(struct_item);

        StructuralValueDef {
            name,
            def_id,
            fields,
        }
    }

    /// Give a struct-like enum payload a nominal HIR identity. The payload is
    /// still represented as a struct all the way through MIR; it is not
    /// coerced to a tuple. A stable synthetic name keeps the generated DefId
    /// addressable by the normal type checker and later lowering stages.
    fn materialize_enum_struct_payload(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        structural: &ast::TypeStructural,
    ) -> Result<hir::TypeExpr> {
        use std::hash::{Hash, Hasher};

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.qualify_name(enum_name).hash(&mut hasher);
        variant_name.hash(&mut hasher);
        let name = format!("__enum_payload_{:x}", hasher.finish());
        let def_id = self.next_def_id();
        let fields = structural
            .fields
            .iter()
            .map(|field| {
                Ok(hir::StructField {
                    hir_id: self.next_id(),
                    name: hir::Symbol::new(field.name.name.clone()),
                    ty: self.transform_type_to_hir(&field.value)?,
                    vis: hir::Visibility::Public,
                })
            })
            .collect::<Result<Vec<_>>>()?;
        let item = hir::Item {
            hir_id: self.next_id(),
            def_id: def_id.clone(),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Struct(hir::Struct {
                name: hir::Symbol::new(name.clone()),
                fields,
                generics: hir::Generics::default(),
                repr: ast::ReprOptions::default(),
            }),
            span: self.create_span(1),
        };
        self.register_type_def(&name, def_id.clone(), &ast::Visibility::Private);
        self.synthetic_items.push(item);
        let path = hir::Path {
            segments: vec![hir::PathSegment {
                name: hir::Symbol::new(name.clone()),
                args: None,
            }],
            res: Some(hir::Res::Def(def_id)),
        };
        Ok(hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Path(path),
            Span::new(self.current_file, 0, 0),
        ))
    }

    fn should_update_structural_def(&self, def_id: hir::DefId) -> bool {
        let Some(fields) = self.struct_field_defs.get(&def_id) else {
            return false;
        };
        fields
            .iter()
            .all(|field| matches!(field.value, ast::Ty::Any(_) | ast::Ty::Unknown(_)))
    }

    fn update_structural_def_fields(
        &mut self,
        def_id: hir::DefId,
        hir_fields: Vec<hir::StructField>,
        ast_fields: Vec<ast::StructuralField>,
    ) {
        if let Some(item) = self
            .synthetic_items
            .iter_mut()
            .find(|item| item.def_id == def_id)
        {
            if let hir::ItemKind::Struct(strukt) = &mut item.kind {
                strukt.fields = hir_fields;
            }
        }
        self.struct_field_defs.insert(def_id, ast_fields);
    }

    fn structural_fields_from_value(
        &mut self,
        structural: &ast::ValueStructural,
    ) -> Vec<StructuralFieldSpec> {
        structural
            .fields
            .iter()
            .map(|field| {
                let ty = match self.literal_type_kind_from_value(&field.value) {
                    Some(ty) => ty,
                    None => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "unsupported structural field value for HIR materialization: {:?}",
                                field.value
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(field.value.span()),
                        );
                        LiteralTypeKind::Null
                    }
                };
                StructuralFieldSpec {
                    name: field.name.name.clone(),
                    ty,
                }
            })
            .collect()
    }

    fn path_for_structural_def(&mut self, def: &StructuralValueDef) -> hir::Path {
        hir::Path {
            segments: vec![hir::PathSegment {
                name: hir::Symbol::new(def.name.clone()),
                args: None,
            }],
            res: Some(hir::Res::Def(def.def_id.clone())),
        }
    }

    fn hir_type_for_value(&mut self, value: &ast::Value) -> Result<hir::TypeExpr> {
        let span = Span::new(self.current_file, 0, 0);
        let expr = match value {
            ast::Value::Int(_) => {
                self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))
            }
            ast::Value::Bool(_) => self.primitive_type_to_hir(ast::TypePrimitive::Bool),
            ast::Value::Decimal(_) => {
                self.primitive_type_to_hir(ast::TypePrimitive::Decimal(ast::DecimalType::F64))
            }
            ast::Value::String(_) => self.primitive_type_to_hir(ast::TypePrimitive::String),
            ast::Value::Bytes(bytes)
                if bytes.value.as_ref().strip_suffix(&[0]).is_some()
                    && std::str::from_utf8(
                        bytes.value.as_ref().strip_suffix(&[0]).unwrap_or_default(),
                    )
                    .is_ok() =>
            {
                self.primitive_type_to_hir(ast::TypePrimitive::String)
            }
            ast::Value::Char(_) => self.primitive_type_to_hir(ast::TypePrimitive::Char),
            ast::Value::Unit(_) => self.create_unit_type(),
            ast::Value::Null(_) | ast::Value::None(_) => {
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Infer, span)
            }
            ast::Value::Struct(struct_val) => {
                let path = self.name_to_hir_path_with_scope(
                    &Name::Ident(struct_val.ty.name.clone()),
                    PathResolutionScope::Type,
                )?;
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Path(path), span)
            }
            ast::Value::Structural(structural) => {
                let def = self.materialize_structural_value_def(structural)?;
                let path = self.path_for_structural_def(&def);
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Path(path), span)
            }
            ast::Value::Type(ty) => return self.transform_type_to_hir(ty),
            other => {
                self.add_error(
                    Diagnostic::error(format!(
                        "unsupported structural field value type: {:?}",
                        other
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(value.span()),
                );
                return Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    span,
                ));
            }
        };
        Ok(expr)
    }

    fn materialize_structural_value_def(
        &mut self,
        structural: &ast::ValueStructural,
    ) -> Result<StructuralValueDef> {
        let specs = self.structural_fields_from_value(structural);
        if let Some(def) = self.find_compatible_structural_value_def(&specs) {
            if self.should_update_structural_def(def.def_id.clone()) {
                let hir_fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.hir_type_for_value(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let ast_fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        let ty = self.structural_field_value_to_ty(&field.value);
                        Ok(ast::StructuralField::new(field.name.clone(), ty))
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.update_structural_def_fields(def.def_id.clone(), hir_fields, ast_fields);
            }
            return Ok(def);
        }

        let key = self.structural_value_key(&specs);
        if let Some(def) = self.structural_value_defs.get(&key).cloned() {
            return Ok(def);
        }

        let hir_fields = structural
            .fields
            .iter()
            .map(|field| {
                Ok(hir::StructField {
                    hir_id: self.next_id(),
                    name: hir::Symbol::new(field.name.name.clone()),
                    ty: self.hir_type_for_value(&field.value)?,
                    vis: hir::Visibility::Public,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let ast_fields = structural
            .fields
            .iter()
            .map(|field| {
                let ty = self.structural_field_value_to_ty(&field.value);
                Ok(ast::StructuralField::new(field.name.clone(), ty))
            })
            .collect::<Result<Vec<_>>>()?;

        let name = self.structural_value_name(&key);
        let def = self.register_structural_value_def(name, specs, hir_fields, ast_fields);
        self.structural_value_defs.insert(key, def.clone());
        Ok(def)
    }

    fn register_type_alias(&mut self, name: &str, ty: &ast::Ty) {
        let qualified = self.qualify_name(name);
        self.type_alias_children
            .entry(self.module_path.to_key())
            .or_default()
            .push(name.to_string());
        self.type_aliases.insert(qualified.clone(), ty.clone());
        self.type_alias_defining_modules
            .insert(qualified, self.module_path.clone());
    }

    /// Look up `name` as if resolved via `use super::name` from every
    /// enclosing module, walking from the immediate parent up to the
    /// package root. Plain `type X = Y;` aliases (e.g. `libc`'s `void`)
    /// are stored keyed by their *defining* module's qualified path, not
    /// by `Res`, so they don't benefit from the normal import machinery —
    /// this lets a submodule (e.g. `libc::macos`) find an alias declared
    /// in an ancestor module (`libc::void`) without re-declaring it.
    fn qualify_name_in_ancestor(&self, name: &str) -> Option<String> {
        let segments = &self.module_path.segments;
        for len in (0..segments.len()).rev() {
            let candidate = fp_core::ast::path::QualifiedPath::new(segments[..len].to_vec())
                .with_segment(name.to_string())
                .to_key();
            if self.type_aliases.contains_key(&candidate) {
                return Some(candidate);
            }
        }
        None
    }

    /// Lazily consult `self.workspace` for a cross-package alias (e.g.
    /// `libc::char`) on a local-lookup miss, instead of relying on it
    /// having been eagerly copied into `self.type_aliases` up front.
    fn find_workspace_type_alias(&self, key: &str) -> Option<ast::package::TypeAliasExport> {
        self.workspace.as_ref()?.find_type_alias(key)
    }

    fn lookup_type_alias(&self, segments: &[String]) -> Option<ast::Ty> {
        let qualified = if segments.len() == 1 {
            self.qualify_name(&segments[0])
        } else {
            fp_core::ast::path::QualifiedPath::new(segments.to_vec()).to_key()
        };
        if let Some(alias) = self.type_aliases.get(&qualified) {
            return Some(alias.clone());
        }
        if segments.len() == 1 {
            if let Some(ancestor_key) = self.qualify_name_in_ancestor(&segments[0]) {
                if let Some(alias) = self.type_aliases.get(&ancestor_key) {
                    return Some(alias.clone());
                }
            }
        }
        if let Some(alias) = segments.get(0).and_then(|name| self.type_aliases.get(name)) {
            return Some(alias.clone());
        }
        self.find_workspace_type_alias(&qualified)
            .map(|alias| alias.target)
            .or_else(|| {
                segments
                    .get(0)
                    .and_then(|name| self.find_workspace_type_alias(name))
                    .map(|alias| alias.target)
            })
    }

    fn lookup_type_alias_with_key(&mut self, segments: &[String]) -> Option<(String, ast::Ty)> {
        let qualified = if segments.len() == 1 {
            self.qualify_name(&segments[0])
        } else {
            fp_core::ast::path::QualifiedPath::new(segments.to_vec()).to_key()
        };
        if let Some(alias) = self.type_aliases.get(&qualified) {
            if self.ty_is_simple_path(alias, segments) {
                return None;
            }
            return Some((qualified, alias.clone()));
        }
        // A multi-segment reference to a *sibling* alias (`c_char_definition
        // ::c_char`, from within `core::ffi::primitives`, referencing
        // `pub(super) type c_char` inside its own nested `mod
        // c_char_definition`) is registered under its *fully* qualified
        // key (`core::ffi::primitives::c_char_definition::c_char` — see
        // `register_type_alias`'s own `self.qualify_name`, always run
        // relative to the *declaring* module). The plain segments-joined
        // key above never carries that module prefix, so it can never
        // match here — walk the same current-module/ancestor prefixes
        // `qualify_name_in_ancestor` already uses for the single-segment
        // case, just with the *entire* multi-segment tail appended instead
        // of one name.
        if segments.len() > 1 {
            let mut prefix = self.module_path.segments.clone();
            loop {
                let candidate = fp_core::ast::path::QualifiedPath::new(prefix.clone())
                    .join(segments)
                    .to_key();
                if let Some(alias) = self.type_aliases.get(&candidate) {
                    if self.ty_is_simple_path(alias, segments) {
                        return None;
                    }
                    return Some((candidate, alias.clone()));
                }
                if prefix.is_empty() {
                    break;
                }
                prefix.pop();
            }
        }
        if segments.len() == 1 {
            if let Some(ancestor_key) = self.qualify_name_in_ancestor(&segments[0]) {
                if let Some(alias) = self.type_aliases.get(&ancestor_key) {
                    if self.ty_is_simple_path(alias, segments) {
                        return None;
                    }
                    return Some((ancestor_key, alias.clone()));
                }
            }
        }
        if let Some(name) = segments.get(0) {
            if let Some(alias) = self.type_aliases.get(name) {
                if self.ty_is_simple_path(alias, segments) {
                    return None;
                }
                return Some((name.clone(), alias.clone()));
            }
        }
        if let Some(alias) = self.find_workspace_type_alias(&qualified) {
            if self.ty_is_simple_path(&alias.target, segments) {
                return None;
            }
            self.type_aliases
                .insert(qualified.clone(), alias.target.clone());
            self.type_alias_defining_modules
                .insert(qualified.clone(), alias.defining_module);
            return Some((qualified, alias.target));
        }
        if let Some(name) = segments.get(0) {
            if let Some(alias) = self.find_workspace_type_alias(name) {
                if self.ty_is_simple_path(&alias.target, segments) {
                    return None;
                }
                self.type_aliases.insert(name.clone(), alias.target.clone());
                self.type_alias_defining_modules
                    .insert(name.clone(), alias.defining_module);
                return Some((name.clone(), alias.target));
            }
        }
        None
    }

    pub(super) fn ty_is_simple_path(&self, ty: &ast::Ty, segments: &[String]) -> bool {
        match ty {
            ast::Ty::Expr(expr) => self.expr_is_simple_path(expr, segments),
            ast::Ty::Value(type_value) => match type_value.value.as_ref() {
                ast::Value::Type(inner) => self.ty_is_simple_path(inner, segments),
                ast::Value::Expr(inner) => self.expr_is_simple_path(inner, segments),
                _ => false,
            },
            _ => false,
        }
    }

    pub(super) fn expr_is_simple_path(&self, expr: &ast::Expr, segments: &[String]) -> bool {
        match expr.kind() {
            ast::ExprKind::Name(name) => self.name_matches_segments(name, segments),
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Expr(inner) => self.expr_is_simple_path(inner, segments),
                ast::Value::Type(ty) => self.ty_is_simple_path(ty, segments),
                _ => false,
            },
            ast::ExprKind::Paren(paren) => self.expr_is_simple_path(&paren.expr, segments),
            _ => false,
        }
    }

    pub(super) fn name_matches_segments(&self, name: &Name, segments: &[String]) -> bool {
        match name {
            Name::Ident(ident) => segments.len() == 1 && ident.name == segments[0],
            Name::Path(path) => self.path_matches_segments(path, segments),
            Name::ParameterPath(path) => {
                if path.segments.len() != segments.len() {
                    return false;
                }
                path.segments
                    .iter()
                    .zip(segments.iter())
                    .all(|(seg, expected)| seg.ident.name == *expected)
            }
        }
    }

    pub(super) fn path_matches_segments(&self, path: &ast::Path, segments: &[String]) -> bool {
        if path.segments.len() != segments.len() {
            return false;
        }
        path.segments
            .iter()
            .zip(segments.iter())
            .all(|(seg, expected)| seg.name == *expected)
    }

    pub(super) fn enter_type_alias(&mut self, key: &str, span: Span) -> bool {
        if self.resolving_type_aliases.contains(key) {
            self.add_error(
                Diagnostic::error(format!("type alias cycle detected: {}", key))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
            );
            return false;
        }
        self.resolving_type_aliases.insert(key.to_string());
        true
    }

    pub(super) fn exit_type_alias(&mut self, key: &str) {
        self.resolving_type_aliases.remove(key);
    }

    /// Transparent aliases retain the lexical module context in which their
    /// RHS was declared. Re-exporting `core::io::Result` as
    /// `std::io::Result` must not make its `result::Result` RHS resolve as if
    /// it had been written inside `std::io`.
    fn with_type_alias_definition_scope<T>(
        &mut self,
        key: &str,
        action: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let Some(defining_module) = self.type_alias_defining_modules.get(key).cloned() else {
            return action(self);
        };
        let previous = std::mem::replace(&mut self.module_path, defining_module);
        let result = action(self);
        self.module_path = previous;
        result
    }

    pub(super) fn materialized_type_alias(
        &self,
        def_type: &ast::ItemDefType,
    ) -> Option<MaterializedTypeAlias> {
        match &def_type.value {
            ast::Ty::Struct(struct_ty) => Some(MaterializedTypeAlias::Struct(struct_ty.clone())),
            ast::Ty::Structural(structural) => {
                Some(MaterializedTypeAlias::Structural(structural.clone()))
            }
            ast::Ty::Enum(enum_ty) => Some(MaterializedTypeAlias::Enum(enum_ty.clone())),
            ast::Ty::ConstBlock(_) | ast::Ty::Expr(_) => None,
            _ => None,
        }
    }

    pub(super) fn materialize_def_type_item(
        &mut self,
        item: &ast::Item,
        def_type: &ast::ItemDefType,
    ) -> Result<Option<hir::Item>> {
        let def_id = self.def_id_for_item(item);
        self.with_owner(def_id.clone(), |this| {
            this.materialize_def_type_item_inner(item, def_type, def_id)
        })
    }

    pub(super) fn materialize_def_type_item_inner(
        &mut self,
        item: &ast::Item,
        def_type: &ast::ItemDefType,
        def_id: hir::DefId,
    ) -> Result<Option<hir::Item>> {
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let (kind, visibility) = match self.materialized_type_alias(def_type) {
            Some(MaterializedTypeAlias::Struct(struct_ty)) => {
                self.register_type_def(&def_type.name.name, def_id.clone(), &def_type.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&struct_ty.generics_params);
                let name = hir::Symbol::new(def_type.name.name.clone());

                // Merge fields from source struct for TypeBuilder::from(Type)
                let fields: Vec<ast::StructuralField> = if struct_ty.name != def_type.name {
                    // Look up source struct fields
                    let source_name = struct_ty.name.as_str();
                    // Pre-existing quirk, preserved as-is: `source_name` is
                    // a raw, unqualified name, not run through
                    // `qualify_path`/`to_key()` — so this only succeeds if
                    // the source struct happens to be registered at the
                    // crate root.
                    let source_def_id = self.lookup_symbol(source_name, hir::Namespace::Type);
                    let source_fields: Vec<ast::StructuralField> = source_def_id
                        .and_then(|res| match res {
                            hir::Res::Def(def_id) => self.struct_field_defs.get(&def_id).cloned(),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let mut merged = source_fields;
                    for f in &struct_ty.fields {
                        if !merged.iter().any(|m| m.name == f.name) {
                            merged.push(f.clone());
                        }
                    }
                    merged
                } else {
                    struct_ty.fields.clone()
                };

                let fields = fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics,
                        repr: def_type
                            .value
                            .as_struct()
                            .map(|struct_ty| struct_ty.repr.clone())
                            .unwrap_or_default(),
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            Some(MaterializedTypeAlias::Structural(structural)) => {
                self.register_type_def(&def_type.name.name, def_id.clone(), &def_type.visibility);
                let name = hir::Symbol::new(def_type.name.name.clone());
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics: hir::Generics::default(),
                        repr: ast::ReprOptions::default(),
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            Some(MaterializedTypeAlias::Enum(enum_ty)) => {
                self.register_type_def(&def_type.name.name, def_id.clone(), &def_type.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&enum_ty.generics_params);
                let qualified_enum_name = hir::Symbol::new(def_type.name.name.clone());

                let variants = enum_ty
                    .variants
                    .iter()
                    .map(|variant| {
                        let variant_path = fp_core::ast::path::QualifiedPath::new(vec![
                            def_type.name.name.clone(),
                            variant.name.name.clone(),
                        ]);
                        let qualified_variant = variant_path.to_key();
                        let fully_qualified = if self.module_path.is_empty() {
                            qualified_variant.clone()
                        } else {
                            self.module_path.join(&variant_path.segments).to_key()
                        };

                        let variant_def_id = if let Some(def_id) =
                            self.enum_variant_def_ids.get(&fully_qualified).cloned()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id.clone());
                            new_id
                        };

                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id.clone()),
                            &def_type.visibility,
                        );
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id.clone(),
                            &def_type.visibility,
                        );

                        let discriminant = variant
                            .discriminant
                            .as_ref()
                            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                            .transpose()?;
                        let payload = match &variant.value {
                            ast::Ty::Unit(_) => None,
                            ast::Ty::Structural(structural) => {
                                Some(self.materialize_enum_struct_payload(
                                    &def_type.name.name,
                                    &variant.name.name,
                                    structural,
                                )?)
                            }
                            other => Some(self.transform_type_to_hir(other)?),
                        };

                        Ok(hir::EnumVariant {
                            attrs: variant.attrs.clone(),
                            hir_id: self.next_id(),
                            def_id: variant_def_id,
                            name: hir::Symbol::new(variant.name.name.clone()),
                            discriminant,
                            payload,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Enum(hir::Enum {
                        attrs: def_type.attrs.clone(),
                        name: qualified_enum_name,
                        variants,
                        generics,
                        repr: attrs_repr(&def_type.attrs),
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            // `Ty::ConstBlock` (`type X = const { ... };`) and `Ty::Expr`
            // (bare name-as-type aliases) don't materialize into a HIR item:
            // uses of `X` are resolved by substituting `type_aliases[X]`
            // directly (see `lookup_type_alias`), so no item — real or
            // synthetic — is needed for either to work.
            None => return Ok(None),
        };

        Ok(Some(hir::Item {
            hir_id,
            def_id,
            kind,
            visibility,
            span,
        }))
    }
}

/// Decomposes every `ExprKind::Closure` reachable from `items` into an
/// ordinary `__ClosureN` struct + `__closureN_call` function pair
/// (`ClosureLowering`) — run once, up front, over a package's flattened
/// item list (`transform_package`). Without this pre-pass, a closure literal
/// reaching
/// `transform_expr_to_hir_inner`'s `ExprKind::Closure` arm has no other
/// lowering support and gets discarded entirely (see that arm's explicit
/// "closure lowering not implemented" placeholder) — previously
/// unnoticed for `transform_package`'s callers (whole-package/typed
/// compiles) since typed content never exercised this path for a real
/// multi-file package before.
fn lower_closures_in_items(
    items: &mut Vec<ast::Item>,
    dependency_struct_field_types: &HashMap<String, Vec<(String, ast::Ty)>>,
    package_name: &str,
) -> Result<Vec<Diagnostic>> {
    let mut pass = ClosureLowering::new(sanitize_generated_symbol_prefix(package_name));
    pass.reserve_generated_names(items);
    pass.struct_field_types = dependency_struct_field_types.clone();
    pass.collect_struct_field_types(items);
    pass.find_and_transform_functions(items)?;
    pass.rewrite_usage(items)?;

    if !pass.generated_items.is_empty() {
        let mut new_items = pass.generated_items;
        new_items.append(items);
        *items = new_items;
    }
    Ok(pass.diagnostics)
}

fn lower_closures_in_expr(expr: &mut ast::Expr) -> Result<(Vec<ast::Item>, Vec<Diagnostic>)> {
    let mut pass = ClosureLowering::new("expr".to_string());
    pass.rewrite_in_expr(expr)?;
    Ok((pass.generated_items, pass.diagnostics))
}

fn sanitize_generated_symbol_prefix(value: &str) -> String {
    value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() {
                character
            } else {
                '_'
            }
        })
        .collect()
}

const DUMMY_CAPTURE_NAME: &str = "__fp_no_capture";

fn expand_intrinsic_collection(expr: &mut ast::Expr) -> bool {
    let id = expr.id();
    if let ast::ExprKind::IntrinsicContainer(collection) = expr.kind_mut() {
        let mut new_expr = collection.take_into_const_expr();
        new_expr.id = id;
        *expr = new_expr;
        true
    } else {
        false
    }
}
