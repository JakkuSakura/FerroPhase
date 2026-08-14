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
use fp_typing::ResolvedNameTable;
use std::collections::{HashMap, HashSet};
use std::path::Path;

mod exprs; // expression lowering
mod helpers;
mod items; // item/impl helpers
mod patterns; // pattern lowering // shared path/name helpers

#[cfg(test)]
mod tests;

use fp_core::diagnostics::{Diagnostic, diagnostic_manager};

const DIAGNOSTIC_CONTEXT: &str = "ast_to_hir";

#[derive(Clone, Debug, Default)]
pub struct HirLoweringConfig {
    /// When `false` (the default, matching every existing caller), a
    /// closure literal is defunctionalized (decomposed into an ordinary
    /// struct + function pair) by `ClosureLowering` *before* HIR
    /// generation even runs — needed by pipelines that lower to MIR
    /// (`PipelineMode::Native`), since MIR has no closure representation
    /// of its own yet.
    ///
    /// When `true`, that pre-pass is skipped entirely and a closure
    /// literal instead survives HIR generation as a real, first-class
    /// `hir::ExprKind::Closure` node — mirroring rustc's own ordering
    /// (a closure stays a rich, typed expression throughout type
    /// checking, with its signature resolved via ordinary expected-type
    /// propagation from its call site, and is only "compiled away" as a
    /// later lowering concern). Used by `PipelineMode::TypecheckedTranspile`
    /// (the Kotlin/etc. backends), which never lowers to MIR and whose
    /// backends want a genuine closure literal to render as an idiomatic
    /// target-language lambda.
    pub keep_closures_first_class: bool,
}

fn query_origin(document: &QueryDocument) -> QueryOrigin {
    document.origin.clone()
}
// TOOD: split into multiple files?
/// Generator for transforming AST to HIR (High-level IR)
///
/// NOTE: This is transitioning from stateful to share-nothing architecture.
/// The generator now supports lossy mode and will gradually become more pure.
pub struct HirGenerator {
    package_id: hir::PackageId,
    next_hir_id: hir::HirId,
    next_def_id: u32,
    current_file: FileId,
    current_position: u32,
    type_scopes: Vec<HashMap<String, hir::Res>>,
    value_scopes: Vec<HashMap<String, hir::Res>>,
    module_path: fp_core::ast::path::QualifiedPath,
    module_visibility: Vec<bool>,
    global_value_defs: HashMap<String, SymbolEntry>,
    global_type_defs: HashMap<String, SymbolEntry>,
    /// `DefId -> qualified path` table mirrored into the final
    /// `hir::Program::def_paths` (see its doc comment). Populated
    /// centrally by `record_def_path`, called from every symbol
    /// registration helper; never cleared per-file since `DefId`s are
    /// unique for the lifetime of this generator.
    def_paths: HashMap<hir::DefId, hir::DefPath>,
    prelude_value_defs: HashMap<String, hir::Res>,
    prelude_type_defs: HashMap<String, hir::Res>,
    preassigned_def_ids: HashMap<u64, hir::DefId>,
    enum_variant_def_ids: HashMap<String, hir::DefId>,
    type_aliases: HashMap<String, ast::Ty>,
    struct_field_defs: HashMap<hir::DefId, Vec<ast::StructuralField>>,
    trait_defs: HashMap<String, ast::ItemDefTrait>,
    structural_value_defs: HashMap<String, StructuralValueDef>,
    const_list_length_scopes: Vec<HashMap<String, usize>>,
    synthetic_items: Vec<hir::Item>,
    module_defs: HashSet<fp_core::ast::path::QualifiedPath>,
    program_def_map: HashMap<hir::DefId, hir::Item>,
    unimplemented_type_def_ids: HashSet<hir::DefId>,
    /// Mirrored into the final `hir::Program::placeholder_defs` (see its
    /// doc comment) the same way `def_paths` is — `DefId`s whose HIR item
    /// is a structural stand-in (currently: trait declarations, which HIR
    /// has no first-class representation for) rather than a real lowering.
    placeholder_defs: HashSet<hir::DefId>,
    resolving_type_aliases: HashSet<String>,
    resolved_names: ResolvedNameTable,
    target_env: TargetEnv,
    respect_cfg: bool,
    lowering_config: HirLoweringConfig,
    intrinsic_normalizer: Option<Box<dyn IntrinsicNormalizer>>,
    workspace: Option<std::rc::Rc<fp_core::workspace::WorkspaceContext>>,
    /// `impl` items whose self-type didn't resolve on a *tolerant*
    /// `predeclare_items` pass because the name is only reachable through
    /// an import that hadn't been processed yet — see `transform_package`,
    /// which retries these once imports are resolved.
    pending_impls: Vec<(fp_core::ast::path::QualifiedPath, ast::Item)>,
    /// `(module_path, alias)` pairs already registered by
    /// `register_import_binding`, so re-running it (e.g. `append_item`'s
    /// own `ItemKind::Import` handling, after `transform_package`'s
    /// upfront import worklist already ran) is a guaranteed no-op instead
    /// of an assumed-safe duplicate.
    resolved_import_aliases: HashSet<(fp_core::ast::path::QualifiedPath, String)>,
    /// Memoized result of `cached_root_modules` — see its doc comment.
    /// `(module_defs.len(), global_type_defs.len(), global_value_defs.len())`
    /// at computation time, paired with the computed set; recomputed only
    /// when one of those sizes has grown since.
    root_modules_cache: Option<(usize, usize, usize, HashSet<String>)>,
}

enum MaterializedTypeAlias {
    Struct(ast::TypeStruct),
    Structural(ast::TypeStructural),
    Enum(ast::TypeEnum),
}

#[derive(Debug, Clone)]
struct SymbolEntry {
    res: hir::Res,
    export: SymbolExport,
    path: Option<fp_core::ast::path::QualifiedPath>,
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

#[derive(Debug, Clone)]
enum SymbolExport {
    Public,
    Scoped(Vec<String>),
}

impl SymbolExport {
    fn can_access(&self, current_module: &[String]) -> bool {
        match self {
            SymbolExport::Public => true,
            SymbolExport::Scoped(scope) => current_module.starts_with(scope.as_slice()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PathResolutionScope {
    Value,
    Type,
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
}

impl HirGenerator {
    fn add_error(&mut self, diag: Diagnostic) {
        diagnostic_manager().add_diagnostic(diag);
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

    fn collect_imports(
        &self,
        base: Vec<String>,
        tree: &ast::ItemImportTree,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        match tree {
            ast::ItemImportTree::Path(path) => self.collect_imports_from_path(base, path, out),
            ast::ItemImportTree::Ident(ident) => {
                let mut target = base;
                target.push(ident.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: None,
                });
                Ok(())
            }
            ast::ItemImportTree::Rename(rename) => {
                let mut target = base;
                target.push(rename.from.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: Some(rename.to.name.clone()),
                });
                Ok(())
            }
            ast::ItemImportTree::Group(group) => {
                for item in &group.items {
                    self.collect_imports(base.clone(), item, out)?;
                }
                Ok(())
            }
            ast::ItemImportTree::Root
            | ast::ItemImportTree::SelfMod
            | ast::ItemImportTree::SuperMod
            | ast::ItemImportTree::Crate
            | ast::ItemImportTree::Glob => Ok(()),
        }
    }

    fn collect_imports_from_path(
        &self,
        base: Vec<String>,
        path: &ast::ItemImportPath,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        let mut prefix = base;
        for seg in &path.segments {
            match seg {
                ast::ItemImportTree::Root | ast::ItemImportTree::Crate => {
                    prefix.clear();
                }
                ast::ItemImportTree::SelfMod => {
                    prefix = self.module_path.segments.clone();
                }
                ast::ItemImportTree::SuperMod => {
                    prefix = self.module_path.segments.clone();
                    prefix.pop();
                }
                ast::ItemImportTree::Ident(ident) => {
                    prefix.push(ident.name.clone());
                }
                ast::ItemImportTree::Rename(rename) => {
                    let mut target = prefix.clone();
                    target.push(rename.from.name.clone());
                    out.push(ImportBinding {
                        target,
                        alias: Some(rename.to.name.clone()),
                    });
                    return Ok(());
                }
                ast::ItemImportTree::Group(group) => {
                    for item in &group.items {
                        self.collect_imports(prefix.clone(), item, out)?;
                    }
                    return Ok(());
                }
                ast::ItemImportTree::Path(nested) => {
                    self.collect_imports_from_path(prefix.clone(), nested, out)?;
                    return Ok(());
                }
                ast::ItemImportTree::Glob => {
                    self.expand_glob_import(prefix, out);
                    return Ok(());
                }
            }
        }

        if !prefix.is_empty() {
            out.push(ImportBinding {
                target: prefix,
                alias: None,
            });
        }
        Ok(())
    }

    /// Expand `use <prefix>::*;` into one `ImportBinding` per direct member
    /// (value, type, or submodule) of the target module, so glob re-exports
    /// like `pub use macos::*;` actually make the re-exported module's
    /// contents resolvable under the importing module's own path — this
    /// pass previously treated every glob import as a silent no-op.
    fn expand_glob_import(&self, prefix: Vec<String>, out: &mut Vec<ImportBinding>) {
        let target_path = fp_core::ast::path::QualifiedPath::new(prefix.clone());
        let mut candidates = vec![target_path.clone()];
        if !self.module_path.is_empty() {
            let relative = self.module_path.join(&prefix);
            if relative != target_path {
                candidates.push(relative);
            }
        }
        for candidate in candidates {
            if !self.module_defs.contains(&candidate) {
                continue;
            }
            let mut seen = HashSet::new();
            for key in self
                .global_value_defs
                .keys()
                .chain(self.global_type_defs.keys())
                .chain(self.type_aliases.keys())
            {
                let segments: Vec<&str> = key.split("::").collect();
                if segments.len() != candidate.segments.len() + 1 {
                    continue;
                }
                if !segments
                    .iter()
                    .zip(candidate.segments.iter())
                    .all(|(a, b)| *a == b.as_str())
                {
                    continue;
                }
                let child = segments[candidate.segments.len()].to_string();
                if !seen.insert(child.clone()) {
                    continue;
                }
                let mut full = candidate.segments.clone();
                full.push(child);
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                });
            }
            for module in &self.module_defs {
                if module.segments.len() != candidate.segments.len() + 1 {
                    continue;
                }
                if module.segments[..candidate.segments.len()] != candidate.segments[..] {
                    continue;
                }
                let child = module.segments[candidate.segments.len()].clone();
                if !seen.insert(child.clone()) {
                    continue;
                }
                let mut full = candidate.segments.clone();
                full.push(child);
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                });
            }
            return;
        }
    }

    /// Returns whether `binding` actually resolved to something (module,
    /// value, or type). Idempotent *by construction*, not by assumption:
    /// once `(module_path, alias)` has resolved once, every later call
    /// (e.g. `append_item`'s own `ItemKind::Import` handling re-running
    /// after `transform_package`'s upfront import worklist already
    /// resolved it) is a guaranteed no-op — see `resolved_import_aliases`.
    fn register_import_binding(&mut self, binding: ImportBinding, visibility: &ast::Visibility) -> bool {
        let alias = binding
            .alias
            .clone()
            .unwrap_or_else(|| binding.target.last().cloned().unwrap_or_default());
        if alias.is_empty() {
            return false;
        }
        let resolved_key = (self.module_path.clone(), alias.clone());
        if self.resolved_import_aliases.contains(&resolved_key) {
            return true;
        }
        let target_path = fp_core::ast::path::QualifiedPath::new(binding.target.clone());
        let mut candidates = vec![target_path.clone()];
        if !self.module_path.is_empty() {
            let relative = self.module_path.join(&target_path.segments);
            if relative != target_path {
                candidates.push(relative);
            }
        }
        // `use crate::X`/`use ::X` (an absolute import) reaches here with
        // its "crate::"/root prefix already stripped by
        // `collect_imports_from_path` — which, not knowing the current
        // crate's own root depth, always strips to nothing. For an
        // ordinary single-crate package the crate root is just the
        // package name (module_path's first segment); the vendored real
        // Rust `std` library is the one exception, bundling three real
        // crates (`core`/`alloc`/`std`) under one FerroPhase package, so a
        // file belonging to one of those needs its sub-crate name kept
        // too (module_path's first two segments — see
        // `rs_relative_to_module_segments` in fp-rust's provider). Try
        // both possible crate roots as additional candidates; harmless
        // for ordinary packages, where the two either coincide or the
        // second candidate simply never matches anything.
        let root = &self.module_path.segments;
        if !root.is_empty() {
            let mut with_root = root[..1].to_vec();
            with_root.extend(target_path.segments.iter().cloned());
            let with_root = fp_core::ast::path::QualifiedPath::new(with_root);
            if !candidates.contains(&with_root) {
                candidates.push(with_root);
            }
        }
        if root.len() >= 2 {
            let mut with_root = root[..2].to_vec();
            with_root.extend(target_path.segments.iter().cloned());
            let with_root = fp_core::ast::path::QualifiedPath::new(with_root);
            if !candidates.contains(&with_root) {
                candidates.push(with_root);
            }
        }

        for candidate in candidates {
            if self.module_defs.contains(&candidate) {
                let res = hir::Res::Module(candidate.segments.clone());
                self.current_value_scope().insert(alias.clone(), res.clone());
                self.current_type_scope().insert(alias.clone(), res.clone());
                // Every top-level item gets its own transient
                // `with_module_scope` push/pop cycle (see
                // `transform_package`), so a scope-only insert here is
                // invisible to sibling items processed afterward (e.g. a
                // `use std::json;` above `fn main() {}` would otherwise
                // never be visible inside `main`'s body). Persist the
                // alias the same way value/type re-exports already do
                // below, so `module::item()` resolves regardless of
                // which sibling item introduced the `use`.
                self.record_value_symbol(&alias, res.clone(), visibility);
                self.record_type_symbol(&alias, res, visibility);
                self.resolved_import_aliases.insert(resolved_key);
                return true;
            }

            let key = candidate.to_key();
            let value = self.lookup_symbol(&key, &self.global_value_defs);
            let ty = self.lookup_symbol(&key, &self.global_type_defs);
            // `type X = Y;` aliases (e.g. `libc::macos::useconds_t`) live in
            // a separate table from `global_value_defs`/`global_type_defs`
            // (see `register_type_alias`) — an import/glob-re-export needs
            // its own explicit copy step here, or a re-exported alias (e.g.
            // via `libc::mod.fp`'s `pub use macos::*;`) never becomes
            // resolvable under the shorter path at all.
            let type_alias = self.type_aliases.get(&key).cloned();
            if value.is_none() && ty.is_none() && type_alias.is_none() {
                continue;
            }

            if let Some(res) = value {
                self.current_value_scope()
                    .insert(alias.clone(), res.clone());
                self.record_value_symbol(&alias, res, visibility);
            }
            if let Some(res) = ty {
                self.current_type_scope().insert(alias.clone(), res.clone());
                self.record_type_symbol(&alias, res, visibility);
            }
            if let Some(alias_ty) = type_alias {
                let new_key = self.qualify_name(&alias);
                self.type_aliases.insert(new_key, alias_ty);
            }
            self.resolved_import_aliases.insert(resolved_key);
            return true;
        }
        false
    }

    pub fn with_file<P: AsRef<Path>>(path: P) -> Self {
        let mut generator = Self::new();
        generator.reset_file_context(path);
        generator
    }

    /// Create a new HIR generator
    pub fn new() -> Self {
        Self {
            package_id: hir::PackageId(0),
            next_hir_id: 0,
            next_def_id: 0,
            current_file: 0, // Default file ID
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            module_visibility: vec![true],
            global_value_defs: HashMap::new(),
            global_type_defs: HashMap::new(),
            def_paths: HashMap::new(),
            prelude_value_defs: HashMap::new(),
            prelude_type_defs: HashMap::new(),
            preassigned_def_ids: HashMap::new(),
            enum_variant_def_ids: HashMap::new(),
            type_aliases: HashMap::new(),
            struct_field_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            structural_value_defs: HashMap::new(),
            const_list_length_scopes: vec![HashMap::new()],
            synthetic_items: Vec::new(),
            module_defs: HashSet::new(),
            program_def_map: HashMap::new(),
            unimplemented_type_def_ids: HashSet::new(),
            placeholder_defs: HashSet::new(),
            resolving_type_aliases: HashSet::new(),
            resolved_names: ResolvedNameTable::new(),
            target_env: TargetEnv::host(),
            respect_cfg: true,
            lowering_config: HirLoweringConfig::default(),
            intrinsic_normalizer: None,
            workspace: None,
            pending_impls: Vec::new(),
            resolved_import_aliases: HashSet::new(),
            root_modules_cache: None,
        }
    }

    pub fn with_package_id(mut self, package_id: hir::PackageId) -> Self {
        self.package_id = package_id;
        self
    }

    pub fn with_def_id_start(mut self, start: u32) -> Self {
        self.next_def_id = start;
        self
    }

    pub fn next_def_id_value(&self) -> u32 {
        self.next_def_id
    }

    pub fn with_lowering_config(mut self, config: HirLoweringConfig) -> Self {
        self.lowering_config = config;
        self
    }

    pub fn with_workspace(
        mut self,
        workspace: std::rc::Rc<fp_core::workspace::WorkspaceContext>,
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
        self.global_value_defs
            .iter()
            .chain(self.global_type_defs.iter())
            .filter(|(_, entry)| matches!(entry.export, SymbolExport::Public))
            .map(|(path, entry)| (path.clone(), entry.res.clone()))
            .collect()
    }

    /// `type_aliases` (unlike `global_value_defs`/`global_type_defs`) has no
    /// per-entry visibility tracking — `register_type_alias` is called for
    /// every `type X = Y;` regardless of `pub`. Export all of them; a
    /// dependent package can only ever reach one by spelling out its exact
    /// qualified path (e.g. `::libc::char`), so there's no meaningful
    /// privacy leak from exporting private aliases too.
    pub fn exported_type_aliases(&self) -> HashMap<String, ast::Ty> {
        self.type_aliases.clone()
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
        self.global_value_defs.clear();
        self.global_type_defs.clear();
        self.enum_variant_def_ids.clear();
        self.struct_field_defs.clear();
        self.module_defs.clear();
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

    fn record_module_def(&mut self, name: &str) {
        let path = self.module_path.with_segment(name.to_string());
        self.module_defs.insert(path);
    }

    fn register_type_def(&mut self, name: &str, def_id: hir::DefId, visibility: &ast::Visibility) {
        let res = hir::Res::Def(def_id);
        self.current_type_scope()
            .insert(name.to_string(), res.clone());
        self.record_type_symbol(name, res, visibility);
    }

    fn record_value_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let path = self.qualify_path(name);
        self.record_def_path(&res, &path);
        let qualified = path.to_key();
        let export = self.symbol_export_marker(visibility);
        self.global_value_defs.insert(
            qualified,
            SymbolEntry {
                res,
                export,
                path: Some(path),
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
        self.global_value_defs.insert(
            path.to_key(),
            SymbolEntry {
                res,
                export,
                path: Some(path.clone()),
            },
        );
    }

    fn record_type_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let path = self.qualify_path(name);
        self.record_def_path(&res, &path);
        let qualified = path.to_key();
        let export = self.symbol_export_marker(visibility);
        self.global_type_defs.insert(
            qualified,
            SymbolEntry {
                res,
                export,
                path: Some(path),
            },
        );
    }

    /// Records a definition's qualified path the first time its `DefId` is
    /// registered (see `hir::Program::def_paths`). Uses `entry().or_insert`
    /// rather than overwriting so that later re-registrations under an
    /// alias (`register_import_binding` re-registers an existing
    /// `Res::Def` under a `use ... as` name through these same helpers)
    /// never clobber a def's one true canonical path.
    fn record_def_path(&mut self, res: &hir::Res, path: &fp_core::ast::path::QualifiedPath) {
        if let hir::Res::Def(def_id) = res {
            self.def_paths
                .entry(*def_id)
                .or_insert_with(|| hir::DefPath::from_qualified_path(path));
        }
    }

    fn symbol_export_marker(&self, visibility: &ast::Visibility) -> SymbolExport {
        if self.should_export(visibility) {
            SymbolExport::Public
        } else {
            SymbolExport::Scoped(self.module_path.segments.clone())
        }
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
            Some(hir::Res::Def(def_id)) => Some(def_id),
            _ => {
                let relative = self.module_path.join(
                    &self_path
                        .segments
                        .iter()
                        .map(|segment| segment.name.as_str().to_owned())
                        .collect::<Vec<_>>(),
                );
                match self.lookup_global_res(&relative, PathResolutionScope::Type) {
                    Some(hir::Res::Def(def_id)) => Some(def_id),
                    _ => None,
                }
            }
        };
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

        let relative = self.module_path.join(
            &self_path
                .segments
                .iter()
                .map(|segment| segment.name.as_str().to_owned())
                .collect::<Vec<_>>(),
        );
        let mut paths: Vec<_> = self
            .global_type_defs
            .iter()
            .filter(|(_, entry)| entry.res == hir::Res::Def(self_def_id))
            .filter_map(|(_, entry)| entry.path.clone())
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
            *existing
        } else {
            let def_id = self.next_def_id();
            self.preassigned_def_ids.insert(key, def_id);
            def_id
        }
    }

    fn def_id_for_item(&mut self, item: &ast::Item) -> hir::DefId {
        let key = Self::item_key(item);
        if let Some(id) = self.preassigned_def_ids.get(&key) {
            *id
        } else {
            self.allocate_def_id_for_item(item)
        }
    }

    fn prepare_lowering_state(&mut self) {
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        self.module_visibility.clear();
        self.module_visibility.push(true);
        self.next_hir_id = 0;
        self.current_position = 0;
        self.type_aliases.clear();
        self.trait_defs.clear();
        self.structural_value_defs.clear();
        self.const_list_length_scopes.clear();
        self.const_list_length_scopes.push(HashMap::new());
        self.synthetic_items.clear();
        self.module_defs.clear();
        self.prelude_value_defs.clear();
        self.prelude_type_defs.clear();
        self.pending_impls.clear();
        self.resolved_import_aliases.clear();
        // Keep predeclared struct fields available for struct update lowering.
    }

    fn load_default_prelude_defs(&mut self) {
        let prelude_prefix = "std::prelude::";
        let type_aliases: Vec<_> = self
            .global_type_defs
            .iter()
            .filter_map(|(key, entry)| {
                key.strip_prefix(prelude_prefix)
                    .filter(|name| !name.contains("::"))
                    .map(|name| (name.to_owned(), entry.res.clone()))
            })
            .collect();
        let value_aliases: Vec<_> = self
            .global_value_defs
            .iter()
            .filter_map(|(key, entry)| {
                key.strip_prefix(prelude_prefix)
                    .filter(|name| !name.contains("::"))
                    .map(|name| (name.to_owned(), entry.res.clone()))
            })
            .collect();
        self.prelude_type_defs = type_aliases.into_iter().collect();
        self.prelude_value_defs = value_aliases.into_iter().collect();
    }

    fn seed_workspace_definitions(&mut self, program: &mut hir::Program) {
        let Some(ref workspace) = self.workspace else {
            return;
        };
        for (_module_path, hir_program, _exports) in workspace.hir_definitions() {
            // Deliberately *not* pushed into `program.items` — that would
            // duplicate every dependency's struct/enum into this package's
            // own output/lifted AST regardless of whether anything here
            // actually references them. `def_map` (populated below) is the
            // registry; `hir_to_mir::MirLowering::compute_adt_layout` looks
            // up and lazily registers a foreign struct/enum from it only
            // when something concrete actually needs one.
            for item in &hir_program.items {
                program.def_map.insert(item.def_id, item.clone());
            }
            program.def_map.extend(hir_program.def_map);
            program.def_paths.extend(hir_program.def_paths);
            // Cross-package exported value/type symbols (`_exports`) are
            // *not* eagerly copied into `global_value_defs`/
            // `global_type_defs` here — `resolve_type_symbol`/
            // `resolve_value_symbol`/`lookup_global_res`/`item_exists` all
            // fall back to `workspace.find_export` lazily on a
            // local-lookup miss instead.
        }
        self.module_defs
            .extend(workspace.module_paths().into_iter());
        // Cross-package `type X = Y;` aliases (e.g. `libc::char`) are
        // *not* eagerly copied in here either — `lookup_type_alias`/
        // `lookup_type_alias_with_key` fall back to `workspace.
        // find_type_alias` lazily on a local-lookup miss instead.
    }

    fn predeclare_items(&mut self, items: &[ast::Item], tolerant: bool) -> Result<()> {
        for item in items {
            if !self.item_enabled_by_cfg(item) {
                continue;
            }
            if should_drop_quote_item(item) {
                continue;
            }
            if should_drop_const_type_item(item) {
                continue;
            }
            match item.kind() {
                ItemKind::Module(module) => {
                    self.allocate_def_id_for_item(item);
                    self.record_module_def(module.name.as_str());
                    self.push_module_scope(&module.name.name, &module.visibility);
                    self.predeclare_items(&module.items, tolerant)?;
                    self.pop_module_scope();
                }
                ItemKind::DefConst(def_const) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_const.name.name, def_id, &def_const.visibility);
                }
                ItemKind::DefStruct(def_struct) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_struct.name.name, def_id, &def_struct.visibility);
                    self.register_value_def(&def_struct.name.name, def_id, &def_struct.visibility);
                    if attrs_has_name(&def_struct.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }
                    self.struct_field_defs
                        .insert(def_id, def_struct.value.fields.clone());
                }
                ItemKind::DefStructural(def_structural) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_structural.name.name,
                        def_id,
                        &def_structural.visibility,
                    );
                    self.register_value_def(
                        &def_structural.name.name,
                        def_id,
                        &def_structural.visibility,
                    );
                    if attrs_has_name(&def_structural.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }
                    self.struct_field_defs
                        .insert(def_id, def_structural.value.fields.clone());
                }
                ItemKind::OpaqueType(opaque_def) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&opaque_def.name.name, def_id, &opaque_def.visibility);
                    self.struct_field_defs.insert(def_id, Vec::new());
                }
                ItemKind::DefEnum(def_enum) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_enum.name.name, def_id, &def_enum.visibility);
                    if attrs_has_name(&def_enum.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }

                    for variant in &def_enum.value.variants {
                        let variant_def_id = self.next_def_id();

                        let variant_path = fp_core::ast::path::QualifiedPath::new(vec![
                            def_enum.name.name.clone(),
                            variant.name.name.clone(),
                        ]);
                        let qualified_variant = variant_path.to_key();
                        let fully_qualified = if self.module_path.is_empty() {
                            qualified_variant.clone()
                        } else {
                            self.module_path.join(&variant_path.segments).to_key()
                        };
                        // Record the `Enum::Variant`-qualified registration
                        // first so its more complete path wins the
                        // `def_paths` entry over the bare-name
                        // registration below (see the analogous comment in
                        // `transform_item_to_hir`'s `DefEnum` arm).
                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &def_enum.visibility,
                        );
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
                            &def_enum.visibility,
                        );
                        self.enum_variant_def_ids
                            .insert(fully_qualified, variant_def_id);
                    }
                }
                ItemKind::DefFunction(def_fn) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_fn.name.name, def_id, &def_fn.visibility);
                }
                ItemKind::DeclFunction(decl_fn) => {
                    // Body-less `extern "C" fn foo(...);` declarations (e.g.
                    // the embedded libc package's platform bindings) must be
                    // registered here, not left to `append_item` (STEP 4),
                    // so that STEP 2's import resolution (in particular glob
                    // re-exports like `pub use macos::*;`) can already see
                    // them when it enumerates `global_value_defs`.
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&decl_fn.name.name, def_id, &ast::Visibility::Public);
                }
                ItemKind::DefTrait(def_trait) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_trait.name.name, def_id, &def_trait.visibility);
                    if attrs_has_name(&def_trait.attrs, "unimplemented") {
                        self.unimplemented_type_def_ids.insert(def_id);
                    }
                    self.trait_defs
                        .insert(def_trait.name.name.clone(), def_trait.clone());
                }
                ItemKind::DefType(def_type) => {
                    self.register_type_alias(&def_type.name.name, &def_type.value);
                    if let Some(materialized) = self.materialized_type_alias(def_type) {
                        let def_id = self.allocate_def_id_for_item(item);
                        self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                        if attrs_has_name(&def_type.attrs, "unimplemented") {
                            self.unimplemented_type_def_ids.insert(def_id);
                        }
                        match materialized {
                            MaterializedTypeAlias::Struct(struct_ty) => {
                                self.struct_field_defs
                                    .insert(def_id, struct_ty.fields.clone());
                            }
                            MaterializedTypeAlias::Structural(structural) => {
                                self.struct_field_defs
                                    .insert(def_id, structural.fields.clone());
                            }
                            MaterializedTypeAlias::Enum(enum_ty) => {
                                for variant in &enum_ty.variants {
                                    let variant_def_id = self.next_def_id();

                                    let variant_path =
                                        fp_core::ast::path::QualifiedPath::new(vec![
                                            def_type.name.name.clone(),
                                            variant.name.name.clone(),
                                        ]);
                                    let qualified_variant = variant_path.to_key();
                                    let fully_qualified = if self.module_path.is_empty() {
                                        qualified_variant.clone()
                                    } else {
                                        self.module_path.join(&variant_path.segments).to_key()
                                    };
                                    self.record_value_symbol(
                                        &qualified_variant,
                                        hir::Res::Def(variant_def_id),
                                        &def_type.visibility,
                                    );
                                    self.register_value_def(
                                        &variant.name.name,
                                        variant_def_id,
                                        &def_type.visibility,
                                    );
                                    self.enum_variant_def_ids
                                        .insert(fully_qualified, variant_def_id);
                                }
                            }
                        }
                    }
                }
                ItemKind::Impl(_) => {
                    let ItemKind::Impl(impl_block) = item.kind() else {
                        unreachable!();
                    };
                    // Only single-segment bare names (`Vec`, not
                    // `crate::vec::Vec` or a blanket `T`) can plausibly be
                    // waiting on an import that hasn't been processed yet
                    // — everything else keeps today's immediate behavior,
                    // including its immediate failure modes. Checked via
                    // `resolve_type_symbol` (non-mutating) *before* the
                    // first mutation below (`allocate_def_id_for_item`),
                    // so a deferred item has made zero state changes and
                    // is safe to fully re-run later, unmodified.
                    let defer = tolerant
                        && self_type_first_segment_name(&impl_block.self_ty)
                            .map(|name| {
                                self.resolve_type_symbol(name).is_none()
                                    && !is_primitive_type_name(name)
                            })
                            .unwrap_or(false);
                    if defer {
                        self.pending_impls
                            .push((self.module_path.clone(), item.clone()));
                    } else {
                        self.allocate_def_id_for_item(item);
                        // A self-type can be permanently unresolvable — not a
                        // timing issue an import-order retry would fix, but a
                        // genuine dead end (e.g. its target type lives in a
                        // module that failed to parse in the first place, so
                        // no amount of import resolution will ever find it).
                        // Skip just this one impl rather than aborting HIR
                        // generation for the whole package — the same
                        // "tolerate what's broken, keep what isn't" policy
                        // already applied at the file level (parse errors).
                        let self_path = match self
                            .ast_expr_to_hir_path(&impl_block.self_ty, PathResolutionScope::Type)
                        {
                            Ok(path) => path,
                            Err(error) => {
                                tracing::warn!(
                                    "skipping impl with unresolvable self-type in {}: {error}",
                                    self.module_path.to_key(),
                                );
                                continue;
                            }
                        };
                        let mut method_path = match self.canonical_type_path(&self_path) {
                            Ok(path) => path.segments,
                            Err(error) => {
                                tracing::warn!(
                                    "skipping impl with unresolvable self-type in {}: {error}",
                                    self.module_path.to_key(),
                                );
                                continue;
                            }
                        };
                        for impl_item in &impl_block.items {
                            let ast::ItemKind::DefFunction(function) = impl_item.kind() else {
                                continue;
                            };
                            let method_def_id = self.allocate_def_id_for_item(impl_item);
                            method_path.push(function.name.name.clone());
                            self.record_value_path(
                                &fp_core::ast::path::QualifiedPath::new(method_path.clone()),
                                hir::Res::Def(method_def_id),
                                &function.visibility,
                            );
                            method_path.pop();
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
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

    fn lookup_symbol(&self, key: &str, map: &HashMap<String, SymbolEntry>) -> Option<hir::Res> {
        map.get(key).and_then(|entry| {
            if entry.export.can_access(&self.module_path.segments) {
                Some(entry.res.clone())
            } else {
                None
            }
        })
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

    fn resolve_type_symbol(&self, name: &str) -> Option<hir::Res> {
        let qualified = self.module_path.with_segment(name.to_string()).to_key();
        self.resolve_lexical_type_symbol(name)
            .or_else(|| self.lookup_symbol(&qualified, &self.global_type_defs))
            .or_else(|| self.prelude_type_defs.get(name).cloned())
            .or_else(|| self.lookup_symbol(name, &self.global_type_defs))
            // Cross-package export (e.g. `libc::char`), looked up lazily
            // against the workspace on a local-lookup miss — see
            // `lookup_global_res`'s identical fallback.
            .or_else(|| self.workspace.as_ref()?.find_export(&qualified))
            .or_else(|| self.workspace.as_ref()?.find_export(name))
    }

    fn resolve_value_symbol(&self, name: &str) -> Option<hir::Res> {
        let qualified = self.module_path.with_segment(name.to_string()).to_key();
        self.resolve_lexical_value_symbol(name)
            .or_else(|| self.lookup_symbol(&qualified, &self.global_value_defs))
            .or_else(|| self.prelude_value_defs.get(name).cloned())
            .or_else(|| self.lookup_symbol(name, &self.global_value_defs))
            .or_else(|| self.workspace.as_ref()?.find_export(&qualified))
            .or_else(|| self.workspace.as_ref()?.find_export(name))
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
    pub fn transform_expr(&mut self, ast_expr: &ast::Expr) -> Result<hir::Program> {
        let mut lowered_expr = ast_expr.clone();
        let (generated_items, closure_diagnostics) = lower_closures_in_expr(&mut lowered_expr)?;
        diagnostic_manager().add_diagnostics(closure_diagnostics);
        if let Some(query) = lower_fp_expr_to_query(&lowered_expr, None) {
            return self.transform_query_document(&query);
        }
        self.reset_file_context("<expr>");
        self.prepare_lowering_state();
        self.load_default_prelude_defs();
        self.predeclare_items(&generated_items, false)?;

        let mut hir_program = hir::Program::new();
        self.program_def_map = HashMap::new();

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
        let output = match ast_expr.ty() {
            Some(ty) => self.transform_type_to_hir(ty)?,
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
                hir_program.def_map.insert(item.def_id, item.clone());
                self.program_def_map.insert(item.def_id, item.clone());
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
    ) -> Result<hir::Program> {
        self.transform_module_inner(module_path, module_path.to_key(), items)
    }

    /// Lower a module after the driver has made its package dependencies
    /// available in the typing context.
    pub async fn transform_module_async(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        items: &[ast::Item],
        typing_context: std::rc::Rc<fp_typing::TypingContext>,
    ) -> Result<hir::Program> {
        let _ = typing_context;
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
        package: &fp_core::package::CompiledPackage,
    ) -> Result<hir::Program> {
        self.reset_file_context("<package>");
        self.prepare_lowering_state();
        // `module_defs` otherwise only ever gains an entry via an explicit
        // `mod X { .. }` AST node (`record_module_def`, common for
        // `.fp`-dialect source) or another package's own tree
        // (`seed_workspace_definitions`, below) — never *this* package's
        // own module tree when a provider represents it implicitly, one
        // module per source file with no literal `Module` wrapper item at
        // all (e.g. `fp-rust`'s real-std provider). Without this, a bare
        // `use crate::sibling_module;`-style import can never resolve as
        // a module alias for such a package, no matter how its target
        // path is computed.
        self.module_defs.extend(package.module_paths.iter().cloned());

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
        // `keep_closures_first_class` pipelines (Kotlin/etc.) lower a
        // closure literal directly into a real `hir::ExprKind::Closure`
        // node instead (see `transform_expr_to_hir_inner`'s `Closure` arm)
        // — running this pre-pass too would defunctionalize it first,
        // defeating that entirely.
        if !self.lowering_config.keep_closures_first_class {
            let dependency_struct_field_types = self.workspace_struct_field_types();
            lower_closures_in_items(&mut lowered_items, &dependency_struct_field_types)?;
        }
        let generated_count = lowered_items.len() - original_len;
        let root_path = fp_core::ast::path::QualifiedPath::new(Vec::new());
        let package_items: Vec<fp_core::package::PackageItem> = lowered_items
            .into_iter()
            .enumerate()
            .map(|(i, item)| {
                let path = if i < generated_count {
                    root_path.clone()
                } else {
                    package.items[i - generated_count].path.clone()
                };
                fp_core::package::PackageItem { path, item }
            })
            .collect();

        let mut program = hir::Program::new();
        self.seed_workspace_definitions(&mut program);
        self.load_default_prelude_defs();

        // 1: definitions (tolerant — impls whose self-type isn't resolvable
        // yet, because it's only reachable through an import that hasn't
        // been processed, get deferred into `pending_impls` instead of
        // failing immediately; see `predeclare_items`'s `ItemKind::Impl` arm).
        for package_item in &package_items {
            self.with_module_scope(&package_item.path, |this| {
                this.predeclare_items(std::slice::from_ref(&package_item.item), true)
            })?;
        }

        // 2: imports — needs every definition above to already exist,
        // crate-wide, since an import can reference any file's item
        // regardless of processing order. Never attempted before append
        // until now; this fixed-point worklist also makes re-export
        // chains resolve, not just direct single-hop imports.
        self.resolve_pending_imports(package)?;

        // 3: retry deferred impls, now strict — imports are resolved, so
        // anything that still fails here is a genuine error, not a
        // forward-reference timing issue.
        for (module_path, item) in std::mem::take(&mut self.pending_impls) {
            self.with_module_scope(&module_path, |this| {
                this.predeclare_items(std::slice::from_ref(&item), false)
            })?;
        }
        self.program_def_map = program.def_map.clone();

        // 4: append — unchanged.
        for package_item in &package_items {
            self.with_module_scope(&package_item.path, |this| {
                this.append_item(&mut program, &package_item.item)
            })?;
        }

        if !self.synthetic_items.is_empty() {
            let mut synthetic = std::mem::take(&mut self.synthetic_items);
            for item in &synthetic {
                program.def_map.insert(item.def_id, item.clone());
                self.program_def_map.insert(item.def_id, item.clone());
            }
            program.items.extend(synthetic.drain(..));
        }
        program.def_map = self.program_def_map.clone();
        program.def_paths = self.def_paths.clone();
        program.placeholder_defs = self.placeholder_defs.clone();
        Ok(program)
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
    fn resolve_pending_imports(&mut self, package: &fp_core::package::CompiledPackage) -> Result<()> {
        let mut pending: Vec<(
            fp_core::ast::path::QualifiedPath,
            ImportBinding,
            ast::Visibility,
        )> = Vec::new();
        for package_item in &package.items {
            if let ItemKind::Import(import) = package_item.item.kind() {
                self.with_module_scope(&package_item.path, |this| {
                    let mut bindings = Vec::new();
                    this.collect_imports(Vec::new(), &import.tree, &mut bindings)?;
                    for binding in bindings {
                        pending.push((
                            package_item.path.clone(),
                            binding,
                            import.visibility.clone(),
                        ));
                    }
                    Ok(())
                })?;
            }
        }

        loop {
            let mut progressed = false;
            let mut still_pending = Vec::with_capacity(pending.len());
            for (module_path, binding, visibility) in pending {
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
    pub fn transform_query_document(&mut self, query: &QueryDocument) -> Result<hir::Program> {
        let file_name = query.name.as_deref().unwrap_or("<query>");
        self.reset_file_context(file_name);
        self.prepare_lowering_state();
        self.load_default_prelude_defs();
        self.program_def_map = HashMap::new();

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

        let mut program = hir::Program::new();
        program.def_map.insert(item.def_id, item.clone());
        self.program_def_map.insert(item.def_id, item.clone());
        program.items.push(item);
        program.def_paths = self.def_paths.clone();
        program.placeholder_defs = self.placeholder_defs.clone();
        Ok(program)
    }

    fn transform_module_inner<P: AsRef<Path>>(
        &mut self,
        module_path: &fp_core::ast::path::QualifiedPath,
        file_label: P,
        items: &[ast::Item],
    ) -> Result<hir::Program> {
        self.reset_file_context(file_label);
        self.prepare_lowering_state();

        self.module_path = module_path.clone();
        let mut program = hir::Program::new();
        self.seed_workspace_definitions(&mut program);
        self.load_default_prelude_defs();
        self.predeclare_items(items, false)?;
        self.program_def_map = program.def_map.clone();
        for item in &self.synthetic_items {
            self.program_def_map.insert(item.def_id, item.clone());
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
                program.def_map.insert(item.def_id, item.clone());
                self.program_def_map.insert(item.def_id, item.clone());
            }
            program.items.extend(synthetic.drain(..));
        }

        // Nested const items generated for const blocks are referenced by
        // their DefId when the type checker requests comptime evaluation.
        // Keep them in the program index even though they are not top-level
        // program items.
        program.def_map = self.program_def_map.clone();
        program.def_paths = self.def_paths.clone();
        program.placeholder_defs = self.placeholder_defs.clone();

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
            QueryKind::Any(_) => {
                return Err(fp_core::error::Error::from(
                    "unsupported opaque query document in AST→HIR",
                ));
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

    fn append_item(&mut self, program: &mut hir::Program, item: &ast::Item) -> Result<()> {
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
                    program.def_map.insert(hir_item.def_id, hir_item.clone());
                    self.program_def_map
                        .insert(hir_item.def_id, hir_item.clone());
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
                let hir_expr = self.transform_expr_to_hir(expr)?;
                let hir_item = hir::Item {
                    hir_id: self.next_id(),
                    def_id: self.allocate_def_id_for_item(item),
                    visibility: hir::Visibility::Private,
                    kind: hir::ItemKind::Expr(hir_expr),
                    span: item.span(),
                };
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id, hir_item.clone());
                program.items.push(hir_item);
                Ok(())
            }
            ItemKind::DeclFunction(decl) => {
                let hir_item = self.transform_decl_function(item, decl)?;
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id, hir_item.clone());
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
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id, hir_item.clone());
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
                let hir_item = self.transform_item_to_hir(item.as_ref())?;
                self.program_def_map
                    .insert(hir_item.def_id, hir_item.clone());
                Ok(hir::StmtKind::Item(hir_item))
            }
        }
    }

    /// Transform an AST item into a HIR item
    fn transform_item_to_hir(&mut self, item: &ast::Item) -> Result<hir::Item> {
        let hir_id = self.next_id();
        let def_id = self.def_id_for_item(item);
        let span = self.create_span(1);

        let (kind, visibility) = match item.kind() {
            ItemKind::DefConst(const_def) => {
                self.register_value_def(&const_def.name.name, def_id, &const_def.visibility);
                let hir_const = self.transform_const_def(const_def)?;
                (
                    hir::ItemKind::Const(hir_const),
                    self.map_visibility(&const_def.visibility),
                )
            }
            ItemKind::DefStruct(struct_def) => {
                self.register_type_def(&struct_def.name.name, def_id, &struct_def.visibility);
                self.register_value_def(&struct_def.name.name, def_id, &struct_def.visibility);
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
                self.register_type_def(&struct_def.name.name, def_id, &struct_def.visibility);
                self.register_value_def(&struct_def.name.name, def_id, &struct_def.visibility);
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
                self.register_type_def(&opaque_def.name.name, def_id, &opaque_def.visibility);
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
                self.register_type_def(&enum_def.name.name, def_id, &enum_def.visibility);
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
                            self.enum_variant_def_ids.get(&fully_qualified).copied()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id);
                            new_id
                        };

                        // Record the `Enum::Variant`-qualified registration
                        // first so its more complete path (including the
                        // enum name segment) wins the `def_paths` entry —
                        // `register_value_def` below re-registers the same
                        // `def_id` under the bare variant name alone (for
                        // unqualified in-scope lookup), which must not
                        // clobber the canonical path.
                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &enum_def.visibility,
                        );
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
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
                        name: qualified_enum_name,
                        variants,
                        generics,
                        repr: attrs_repr(&enum_def.attrs),
                    }),
                    self.map_visibility(&enum_def.visibility),
                )
            }
            ItemKind::DefFunction(func_def) => {
                self.register_value_def(&func_def.name.name, def_id, &func_def.visibility);
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
                self.register_value_def(&func_decl.name.name, def_id, &ast::Visibility::Public);
                let function = self.transform_decl_function_sig(func_decl, None)?;
                (hir::ItemKind::Function(function), hir::Visibility::Public)
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
                };
                (hir::ItemKind::Const(konst), hir::Visibility::Private)
            }
            ItemKind::DefTrait(def_trait) => {
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
                // HIR has no first-class trait item — this placeholder only
                // exists so the item has *some* HIR shape to type-check.
                // Backends that model traits as real interfaces (e.g.
                // fp-kotlin) work off the original, pristine `ast::Item`
                // instead — recording `def_id` in `placeholder_defs` (mirrored
                // into `hir::Program::placeholder_defs`) lets
                // `HirToAstLifter::lift_items_by_path` skip lifting this
                // stand-in, so typed-splice (`typecheck_package`) falls back
                // to the real trait declaration instead of overwriting it
                // with this bogus `const NAME = false`.
                self.placeholder_defs.insert(def_id);
                let konst = hir::Const {
                    name: hir::Symbol::new(def_trait.name.name.clone()),
                    ty: self.create_simple_type("bool"),
                    body,
                };
                (
                    hir::ItemKind::Const(konst),
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
        let hir_id = self.next_id();
        let def_id = self.def_id_for_item(item);
        let span = self.create_span(1);
        self.register_value_def(&decl.name.name, def_id, &ast::Visibility::Public);
        let function = self.transform_decl_function_sig(decl, None)?;
        Ok(hir::Item {
            hir_id,
            def_id,
            visibility: hir::Visibility::Public,
            kind: hir::ItemKind::Function(function),
            span,
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

        let value = self.transform_expr_to_hir(&const_def.value)?;
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value,
        };

        Ok(hir::Const {
            name: hir::Symbol::new(const_def.name.name.clone()),
            ty,
            body,
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
                    let result = self.transform_type_to_hir(&alias);
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
                    hir::TypeExprKind::Ptr(Box::new(inner)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Unit(_) => Ok(self.create_unit_type()),
            ast::Ty::Nothing(_) => Ok(self.create_null_type()),
            ast::Ty::Any(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::TypeBounds(bounds) => {
                // `dyn Trait` (extra `+ Send`/`Sync`-style bounds carry no
                // separate identity, so only the primary trait matters).
                // Resolve it to a real path the same way a concrete struct
                // type name is resolved above — leaving this as `Infer`
                // erases the trait name before typechecking even starts,
                // which then collapses `Arc<dyn Trait>` down to a bare
                // `Arc` once hir_to_ast tries to render the generic arg.
                let primary_trait_name = bounds.bounds.first().and_then(|expr| match expr.kind() {
                    ast::ExprKind::Name(name) => Some(name.clone()),
                    _ => None,
                });
                match primary_trait_name {
                    Some(name) => {
                        let path = self
                            .name_to_hir_path_with_scope(&name, PathResolutionScope::Type)?;
                        Ok(hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Path(path),
                            Span::new(self.current_file, 0, 0),
                        ))
                    }
                    None => Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Infer,
                        Span::new(self.current_file, 0, 0),
                    )),
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
                let path = hir::Path {
                    segments: vec![self.make_path_segment("Vec", Some(args))],
                    res: None,
                };
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
            // FIXME: Ty::Type lowered as I64 is a pragmatic hack — the
            // comptime type handle is stored as u64, so I64 allows struct
            // construction without tripping over Never/Infer→error_ty().
            // Should be a dedicated HIR/MIR variant for comp time type values.
            ast::Ty::Type(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)),
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
                    ast::ExprKind::Name(_) | ast::ExprKind::Select(_) | ast::ExprKind::Invoke(_)
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
                // the type checker resolves it via `TypingContext::request_comptime`
                // when it encounters this node.
                let body = Box::new(self.transform_expr_to_hir(block.expr.as_ref())?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::ConstBlock(body),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Expr(expr) => {
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
                        let result = self.transform_type_to_hir(&alias);
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
                    if let Ok(path) = self.ast_expr_to_hir_path(bound, PathResolutionScope::Type) {
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
            def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Struct(hir::Struct {
                name: name_symbol,
                fields: hir_fields,
                generics: hir::Generics::default(),
                repr: ast::ReprOptions::default(),
            }),
            span,
        };

        self.register_type_def(&name, def_id, &ast::Visibility::Private);
        self.struct_field_defs.insert(def_id, ast_fields);
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
            def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Struct(hir::Struct {
                name: hir::Symbol::new(name.clone()),
                fields,
                generics: hir::Generics::default(),
                repr: ast::ReprOptions::default(),
            }),
            span: self.create_span(1),
        };
        self.register_type_def(&name, def_id, &ast::Visibility::Private);
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
            res: Some(hir::Res::Def(def.def_id)),
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
            if self.should_update_structural_def(def.def_id) {
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

                self.update_structural_def_fields(def.def_id, hir_fields, ast_fields);
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
        self.type_aliases.insert(qualified, ty.clone());
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
    fn find_workspace_type_alias(&self, key: &str) -> Option<ast::Ty> {
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
            .or_else(|| segments.get(0).and_then(|name| self.find_workspace_type_alias(name)))
    }

    fn lookup_type_alias_with_key(&self, segments: &[String]) -> Option<(String, ast::Ty)> {
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
            if self.ty_is_simple_path(&alias, segments) {
                return None;
            }
            return Some((qualified, alias));
        }
        if let Some(name) = segments.get(0) {
            if let Some(alias) = self.find_workspace_type_alias(name) {
                if self.ty_is_simple_path(&alias, segments) {
                    return None;
                }
                return Some((name.clone(), alias));
            }
        }
        None
    }

    fn ty_is_simple_path(&self, ty: &ast::Ty, segments: &[String]) -> bool {
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

    fn expr_is_simple_path(&self, expr: &ast::Expr, segments: &[String]) -> bool {
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

    fn name_matches_segments(&self, name: &Name, segments: &[String]) -> bool {
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

    fn path_matches_segments(&self, path: &ast::Path, segments: &[String]) -> bool {
        if path.segments.len() != segments.len() {
            return false;
        }
        path.segments
            .iter()
            .zip(segments.iter())
            .all(|(seg, expected)| seg.name == *expected)
    }

    fn enter_type_alias(&mut self, key: &str, span: Span) -> bool {
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

    fn exit_type_alias(&mut self, key: &str) {
        self.resolving_type_aliases.remove(key);
    }

    fn materialized_type_alias(
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

    fn materialize_def_type_item(
        &mut self,
        item: &ast::Item,
        def_type: &ast::ItemDefType,
    ) -> Result<Option<hir::Item>> {
        let def_id = self.def_id_for_item(item);
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let (kind, visibility) = match self.materialized_type_alias(def_type) {
            Some(MaterializedTypeAlias::Struct(struct_ty)) => {
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&struct_ty.generics_params);
                let name = hir::Symbol::new(def_type.name.name.clone());

                // Merge fields from source struct for TypeBuilder::from(Type)
                let fields: Vec<ast::StructuralField> = if struct_ty.name != def_type.name {
                    // Look up source struct fields
                    let source_name = struct_ty.name.as_str();
                    let source_def_id = self.lookup_symbol(source_name, &self.global_type_defs);
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
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
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
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
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
                            self.enum_variant_def_ids.get(&fully_qualified).copied()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id);
                            new_id
                        };

                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &def_type.visibility,
                        );
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
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

fn should_drop_quote_item(item: &ast::Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(func) => signature_contains_quote(&func.sig),
        ItemKind::DeclFunction(func) => signature_contains_quote(&func.sig),
        ItemKind::DefConst(def) => {
            def.ty_annotation()
                .or_else(|| def.ty.as_ref())
                .is_some_and(ty_contains_quote)
                || expr_contains_quote_value(def.value.as_ref())
        }
        _ => false,
    }
}

fn should_drop_const_type_item(item: &ast::Item) -> bool {
    let _ = item;
    false
}

/// Shared with `canonical_type_path`'s own primitive-name check, and with
/// the tolerant-predeclare deferral check below, so both places recognize
/// the same set of names as "not a real registered type, don't bother
/// looking it up."
fn is_primitive_type_name(name: &str) -> bool {
    matches!(
        name,
        "str" | "char"
            | "bool"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
    )
}

/// Returns the self-type's head (first) segment name when it's a plain,
/// unprefixed name-based path — a bare single segment (`Vec`, or
/// `Vec<u8>` via a single-segment `Name::ParameterPath`), or the first
/// segment of a bare multi-segment path (`ops::RangeFull`, where `ops`
/// is a module brought into scope by a plain `use crate::{..., ops};`).
/// Either shape could plausibly still be waiting on an import that
/// hasn't been processed yet. Already-anchored paths (`crate::vec::Vec`,
/// `self::Foo`, `super::Foo`) and non-name self-types (blanket
/// `impl<T> Trait for T`) all return `None` — those are never deferred,
/// they fall straight through to today's immediate resolution/failure.
fn self_type_first_segment_name(self_ty: &ast::Expr) -> Option<&str> {
    let ast::ExprKind::Name(name) = self_ty.kind() else {
        return None;
    };
    match name {
        Name::Ident(ident) => Some(ident.name.as_str()),
        Name::Path(path) if path.prefix == fp_core::ast::path::PathPrefix::Plain => {
            path.segments.first().map(|seg| seg.name.as_str())
        }
        Name::ParameterPath(param_path)
            if param_path.prefix == fp_core::ast::path::PathPrefix::Plain =>
        {
            param_path
                .segments
                .first()
                .map(|seg| seg.ident.name.as_str())
        }
        _ => None,
    }
}

fn signature_contains_quote(sig: &ast::FunctionSignature) -> bool {
    sig.params.iter().any(|param| ty_contains_quote(&param.ty))
        || sig.ret_ty.as_ref().is_some_and(ty_contains_quote)
}

#[allow(dead_code)]
fn signature_contains_type_type(sig: &ast::FunctionSignature) -> bool {
    sig.params.iter().any(|param| {
        ty_contains_type_type(&param.ty)
            || param
                .ty_annotation
                .as_ref()
                .is_some_and(ty_contains_type_type)
    }) || sig.ret_ty.as_ref().is_some_and(ty_contains_type_type)
}

fn ty_contains_quote(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Quote(_) => true,
        ast::Ty::Tuple(tuple) => tuple.types.iter().any(ty_contains_quote),
        ast::Ty::Array(array) => ty_contains_quote(&array.elem),
        ast::Ty::Vec(vec) => ty_contains_quote(&vec.ty),
        ast::Ty::Reference(reference) => ty_contains_quote(&reference.ty),
        ast::Ty::RawPtr(raw_ptr) => ty_contains_quote(&raw_ptr.ty),
        ast::Ty::Slice(slice) => ty_contains_quote(&slice.elem),
        ast::Ty::Struct(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Structural(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Enum(def) => def
            .variants
            .iter()
            .any(|variant| ty_contains_quote(&variant.value)),
        ast::Ty::Function(func) => {
            func.params.iter().any(ty_contains_quote)
                || func
                    .ret_ty
                    .as_ref()
                    .is_some_and(|ty| ty_contains_quote(ty.as_ref()))
        }
        ast::Ty::TypeBinaryOp(op) => ty_contains_quote(&op.lhs) || ty_contains_quote(&op.rhs),
        ast::Ty::TypeBounds(bounds) => bounds
            .bounds
            .iter()
            .any(|expr| expr_contains_quote_value(expr)),
        ast::Ty::Value(value) => value_contains_quote(value.value.as_ref()),
        ast::Ty::Expr(expr) => expr_contains_quote_value(expr.as_ref()),
        ast::Ty::ConstBlock(block) => expr_contains_quote_value(block.expr.as_ref()),
        ast::Ty::Primitive(_)
        | ast::Ty::TokenStream(_)
        | ast::Ty::ImplTraits(_)
        | ast::Ty::Any(_)
        | ast::Ty::GenericVar(_)
        | ast::Ty::ErrorType(_)
        | ast::Ty::InferVar(_)
        | ast::Ty::Unit(_)
        | ast::Ty::Unknown(_)
        | ast::Ty::Nothing(_)
        | ast::Ty::Type(_)
        | ast::Ty::RequestedType(_)
        | ast::Ty::AnyBox(_)
        | ast::Ty::Wildcard(_) => false,
    }
}

#[allow(dead_code)]
fn ty_contains_type_type(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Type(_) | ast::Ty::RequestedType(_) | ast::Ty::ConstBlock(_) => true,
        ast::Ty::Tuple(tuple) => tuple.types.iter().any(ty_contains_type_type),
        ast::Ty::Array(array) => ty_contains_type_type(&array.elem),
        ast::Ty::Vec(vec) => ty_contains_type_type(&vec.ty),
        ast::Ty::Reference(reference) => ty_contains_type_type(&reference.ty),
        ast::Ty::RawPtr(raw_ptr) => ty_contains_type_type(&raw_ptr.ty),
        ast::Ty::Slice(slice) => ty_contains_type_type(&slice.elem),
        ast::Ty::Struct(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_type_type(&field.value)),
        ast::Ty::Structural(def) => def
            .fields
            .iter()
            .any(|field| ty_contains_type_type(&field.value)),
        ast::Ty::Enum(def) => def
            .variants
            .iter()
            .any(|variant| ty_contains_type_type(&variant.value)),
        ast::Ty::Function(func) => type_function_contains_type_type(func),
        ast::Ty::TypeBinaryOp(op) => {
            ty_contains_type_type(&op.lhs) || ty_contains_type_type(&op.rhs)
        }
        ast::Ty::TypeBounds(bounds) => bounds
            .bounds
            .iter()
            .any(|expr| expr_contains_type_type(expr)),
        ast::Ty::Value(value) => value_contains_type_type(value.value.as_ref()),
        ast::Ty::Expr(expr) => expr_contains_type_type(expr.as_ref()),
        ast::Ty::Primitive(_)
        | ast::Ty::TokenStream(_)
        | ast::Ty::ImplTraits(_)
        | ast::Ty::Any(_)
        | ast::Ty::GenericVar(_)
        | ast::Ty::ErrorType(_)
        | ast::Ty::InferVar(_)
        | ast::Ty::Unit(_)
        | ast::Ty::Unknown(_)
        | ast::Ty::Nothing(_)
        | ast::Ty::Quote(_)
        | ast::Ty::AnyBox(_)
        | ast::Ty::Wildcard(_) => false,
    }
}

#[allow(dead_code)]
fn type_function_contains_type_type(func: &ast::TypeFunction) -> bool {
    func.params.iter().any(ty_contains_type_type)
        || func
            .ret_ty
            .as_ref()
            .is_some_and(|ty| ty_contains_type_type(ty.as_ref()))
}

fn expr_contains_quote_value(expr: &ast::Expr) -> bool {
    if let ast::ExprKind::Value(value) = expr.kind() {
        return value_contains_quote(value.as_ref());
    }
    false
}

#[allow(dead_code)]
fn expr_contains_type_type(expr: &ast::Expr) -> bool {
    expr.ty()
        .as_ref()
        .is_some_and(|ty| ty_contains_type_type(ty))
}

fn value_contains_quote(value: &ast::Value) -> bool {
    match value {
        ast::Value::QuoteToken(_) => true,
        ast::Value::List(list) => {
            !list.values.is_empty() && list.values.iter().all(|value| value_contains_quote(value))
        }
        _ => false,
    }
}

#[allow(dead_code)]
fn value_contains_type_type(value: &ast::Value) -> bool {
    match value {
        ast::Value::Type(ty) => ty_contains_type_type(ty),
        ast::Value::Expr(expr) => expr_contains_type_type(expr.as_ref()),
        ast::Value::List(list) => list.values.iter().any(value_contains_type_type),
        ast::Value::Struct(value) => value
            .structural
            .fields
            .iter()
            .any(|field| value_contains_type_type(&field.value)),
        ast::Value::Structural(value) => value
            .fields
            .iter()
            .any(|field| value_contains_type_type(&field.value)),
        ast::Value::Tuple(value) => value
            .values
            .iter()
            .any(|value| value_contains_type_type(value)),
        _ => false,
    }
}

impl Default for HirGenerator {
    fn default() -> Self {
        Self::new()
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
) -> Result<Vec<Diagnostic>> {
    let mut pass = ClosureLowering::new();
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
    let mut pass = ClosureLowering::new();
    pass.rewrite_in_expr(expr)?;
    Ok((pass.generated_items, pass.diagnostics))
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

#[derive(Clone)]
struct ClosureInfo {
    env_struct_ident: ast::Ident,
    env_struct_ty: ast::Ty,
    call_fn_ident: ast::Ident,
    call_ret_ty: ast::Ty,
}

#[derive(Clone)]
struct Capture {
    name: ast::Ident,
    ty: ast::Ty,
}

struct ClosureLowering {
    counter: usize,
    function_infos: HashMap<String, ClosureInfo>,
    struct_infos: HashMap<String, ClosureInfo>,
    variable_infos: HashMap<String, ClosureInfo>,
    generated_items: Vec<ast::Item>,
    diagnostics: Vec<Diagnostic>,
    /// Struct name -> (field name, declared field type), collected once up
    /// front over the whole package — used only to derive a closure
    /// argument's real parameter type at its call site (see
    /// `closure_param_ty_for_invoke`), never mutated afterward.
    struct_field_types: HashMap<String, Vec<(String, ast::Ty)>>,
    /// The enclosing top-level function's own parameter name -> declared
    /// type, while rewriting its body (see `rewrite_usage`) — the other
    /// half of the same closure-argument-type derivation. Does not cover
    /// `impl` method receivers/params or `let`-bound locals; a receiver
    /// expression built from those simply doesn't resolve here, same as
    /// any other unhandled shape.
    current_param_types: HashMap<String, ast::Ty>,
}
// TODO: move to new file
impl ClosureLowering {
    fn new() -> Self {
        Self {
            counter: 0,
            function_infos: HashMap::new(),
            struct_infos: HashMap::new(),
            variable_infos: HashMap::new(),
            generated_items: Vec::new(),
            diagnostics: Vec::new(),
            struct_field_types: HashMap::new(),
            current_param_types: HashMap::new(),
        }
    }

    /// One-time pre-pass collecting every struct's declared field types,
    /// so `closure_param_ty_for_invoke` can resolve a field-access chain
    /// (`node.stats`) back to its real type without a full type checker.
    fn collect_struct_field_types(&mut self, items: &[ast::Item]) {
        for item in items {
            match item.kind() {
                ast::ItemKind::Module(module) => self.collect_struct_field_types(&module.items),
                ast::ItemKind::DefStruct(def) => {
                    let fields = def
                        .value
                        .fields
                        .iter()
                        .map(|field| (field.name.as_str().to_string(), field.value.clone()))
                        .collect();
                    self.struct_field_types
                        .insert(def.name.as_str().to_string(), fields);
                }
                _ => {}
            }
        }
    }

    /// Best-effort, deliberately narrow structural type lookup for a
    /// receiver expression — not a general type checker, just enough to
    /// resolve the two shapes real call sites need: a tracked function
    /// parameter's own declared type, and field access through a known
    /// struct definition. Returns `None` for anything else.
    fn infer_static_expr_ty(&self, expr: &ast::Expr) -> Option<ast::Ty> {
        match expr.kind() {
            ast::ExprKind::Name(name) => self
                .current_param_types
                .get(name.as_ident()?.as_str())
                .cloned(),
            ast::ExprKind::Select(select) => {
                let base_ty = self.infer_static_expr_ty(&select.obj)?;
                let struct_name = Self::struct_name_of(&base_ty)?;
                self.struct_field_types
                    .get(&struct_name)?
                    .iter()
                    .find(|(name, _)| name == select.field.as_str())
                    .map(|(_, ty)| ty.clone())
            }
            _ => None,
        }
    }

    /// The struct name a type ultimately names, stripping reference
    /// wrappers and unwrapping the `Ty::Expr(Name(..))` shape a bare
    /// (non-generic) struct reference parses as.
    fn struct_name_of(ty: &ast::Ty) -> Option<String> {
        match ty {
            ast::Ty::Reference(r) => Self::struct_name_of(&r.ty),
            ast::Ty::Struct(s) => Some(s.name.as_str().to_string()),
            ast::Ty::Expr(expr) => match expr.kind() {
                ast::ExprKind::Name(name) => name.as_ident().map(|i| i.as_str().to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    /// The `index`-th generic type argument of a parameterized type
    /// reference (`Option<T>`'s `T` is index 0, `Result<T, E>`'s `E` is
    /// index 1) — generic types parse as `Ty::Expr` wrapping a
    /// `Name::ParameterPath` whose segment carries the type args
    /// directly (see `fp-lang/src/ast/types.rs`'s `parse_simple_type`).
    fn generic_type_arg_at(ty: &ast::Ty, index: usize) -> Option<ast::Ty> {
        let ast::Ty::Expr(expr) = ty else {
            return None;
        };
        let ast::ExprKind::Name(ast::Name::ParameterPath(path)) = expr.kind() else {
            return None;
        };
        path.segments.last()?.args.get(index).cloned()
    }

    /// Resolves a call receiver's static type, peeling through at most one
    /// trailing `.as_ref()`/`.as_mut()` — common right before a
    /// closure-taking method (`opt.as_ref().map_or(..)`) — and reporting
    /// whether the generic argument later extracted from it should be
    /// reference-wrapped to match (`.as_ref()` turns `Option<T>` access
    /// into effectively `Option<&T>` for the closure's purposes).
    fn receiver_ty_for_closure_arg(&self, expr: &ast::Expr) -> (Option<ast::Ty>, bool) {
        if let ast::ExprKind::Invoke(invoke) = expr.kind() {
            if let ast::ExprInvokeTarget::Method(sel) = &invoke.target {
                if invoke.args.is_empty() && matches!(sel.field.name.as_str(), "as_ref" | "as_mut")
                {
                    return (self.infer_static_expr_ty(&sel.obj), true);
                }
            }
        }
        (self.infer_static_expr_ty(expr), false)
    }

    /// Derives the real parameter type for a closure passed to one of the
    /// handful of `Option`/`Result` methods whose Kotlin codegen needs a
    /// literal closure (see `fp-kotlin`'s `map_or`/`map_err` special
    /// cases) — `None` if the receiver's type isn't structurally
    /// resolvable, or the method isn't one of these.
    /// Returns `(param_ty, ret_ty)` for the closure argument of a
    /// `map_or`/`map`/`map_err`/`and_then` call — the closure's own return
    /// type also needs to be a real type, not `Unknown`: leaving it
    /// `Unknown` reproduces the exact same "silently resolves to a null
    /// placeholder" failure mode this whole derivation exists to avoid,
    /// just one step later (at the synthetic `__closureN_call` function's
    /// own return position instead of its parameter). The full body
    /// wouldn't need type inference to get this right in general, but
    /// `map_or`'s `default` argument is frequently a literal with an
    /// obvious static type, which covers the common case cheaply.
    fn closure_param_ty_for_invoke(&self, invoke: &ast::ExprInvoke) -> (Option<ast::Ty>, Option<ast::Ty>) {
        let ast::ExprInvokeTarget::Method(sel) = &invoke.target else {
            return (None, None);
        };
        let arg_index = match sel.field.name.as_str() {
            "map_or" | "map" | "and_then" => 0,
            "map_err" => 1,
            _ => return (None, None),
        };
        let (receiver_ty, by_ref) = self.receiver_ty_for_closure_arg(&sel.obj);
        let Some(inner) = receiver_ty.and_then(|ty| Self::generic_type_arg_at(&ty, arg_index)) else {
            return (None, None);
        };
        let param_ty = if by_ref {
            ast::Ty::Reference(
                ast::TypeReference {
                    ty: Box::new(inner),
                    mutability: None,
                    lifetime: None,
                }
                .into(),
            )
        } else {
            inner
        };
        let ret_ty = if sel.field.name.as_str() == "map_or" {
            invoke.args.first().and_then(Self::literal_expr_ty)
        } else {
            None
        };
        (Some(param_ty), ret_ty)
    }

    /// The static type of an integer/float/bool/string literal expression
    /// — used only as a best-effort return-type hint (see
    /// `closure_param_ty_for_invoke`), not a general literal-type table.
    fn literal_expr_ty(expr: &ast::Expr) -> Option<ast::Ty> {
        let ast::ExprKind::Value(value) = expr.kind() else {
            return None;
        };
        Some(match value.as_ref() {
            ast::Value::Int(_) => ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)),
            ast::Value::Decimal(_) => {
                ast::Ty::Primitive(ast::TypePrimitive::Decimal(ast::DecimalType::F64))
            }
            ast::Value::Bool(_) => ast::Ty::Primitive(ast::TypePrimitive::Bool),
            ast::Value::String(_) => ast::Ty::Primitive(ast::TypePrimitive::String),
            _ => return None,
        })
    }

    fn add_error(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    fn block_stmt_expr(expr: ast::Expr, has_value: bool) -> ast::BlockStmt {
        ast::BlockStmt::Expr(ast::BlockStmtExpr::new(expr).with_semicolon(!has_value))
    }

    fn desugar_block_defer(&mut self, block: &mut ast::ExprBlock) -> bool {
        let defer_index = block
            .stmts
            .iter()
            .position(|stmt| matches!(stmt, ast::BlockStmt::Defer(_)));
        let Some(index) = defer_index else {
            return false;
        };
        let ast::BlockStmt::Defer(stmt_defer) = block.stmts.remove(index) else {
            return false;
        };
        let suffix = block.stmts.split_off(index);
        let has_value = match suffix.last() {
            Some(ast::BlockStmt::Expr(expr_stmt)) => expr_stmt.has_value(),
            _ => false,
        };
        let wrapped = ast::Expr::new(ast::ExprKind::Try(ast::ExprTry {
            span: stmt_defer.span(),
            expr: Box::new(ast::Expr::new(ast::ExprKind::Block(
                ast::ExprBlock::new_stmts(suffix),
            ))),
            catches: Vec::new(),
            elze: None,
            finally: Some(stmt_defer.expr),
        }));
        block.stmts.push(Self::block_stmt_expr(wrapped, has_value));
        true
    }

    fn find_and_transform_functions(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => {
                    self.find_and_transform_functions(&mut module.items)?;
                }
                ast::ItemKind::DefFunction(func) => {
                    if let Some(info) = self.transform_function(func)? {
                        self.function_infos
                            .insert(func.name.as_str().to_string(), info.clone());
                        self.struct_infos
                            .insert(info.env_struct_ident.as_str().to_string(), info);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn transform_function(
        &mut self,
        func: &mut ast::ItemDefFunction,
    ) -> Result<Option<ClosureInfo>> {
        if let Some(last_expr) = func.body.last_expr_mut()
            && let Some(info) = self.transform_closure_expr(last_expr)?
        {
            let env_ret_ty = info.env_struct_ty.clone();

            if let Some(ty_fn) = func.ty.as_mut() {
                ty_fn.ret_ty = Some(Box::new(env_ret_ty.clone()));
            }

            if func.ty.is_none() {
                func.ty = Some(ast::TypeFunction {
                    params: func
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    generics_params: func.sig.generics_params.clone(),
                    ret_ty: Some(Box::new(env_ret_ty.clone())),
                });
            }

            if func.ty_annotation.is_some() || func.ty.is_some() {
                func.ty_annotation = func
                    .ty
                    .as_ref()
                    .map(|ty_fn| ast::Ty::Function(ty_fn.clone()));
            }

            if let Some(ret_slot) = func.sig.ret_ty.as_mut() {
                *ret_slot = env_ret_ty.clone();
            } else {
                func.sig.ret_ty = Some(env_ret_ty.clone());
            }

            return Ok(Some(info));
        }

        Ok(None)
    }

    /// `transform_closure_expr` only decomposes a closure literal that
    /// already carries a `Ty::Function` type — true for a function's own
    /// tail expression (its declared return-type annotation is copied
    /// onto the tail by an earlier pass), but never true for a closure
    /// passed as a call *argument*: this pre-pass runs before typecheck,
    /// so the callee's parameter type isn't resolved yet, and previously
    /// nothing else ever gave the closure a type here either. Left
    /// unaddressed, such a closure falls through every other lowering
    /// path all the way to `transform_expr_to_hir_inner`'s
    /// `ExprKind::Closure` arm, which has no implementation and silently
    /// discards it (an empty HIR block, plus an error diagnostic nothing
    /// currently surfaces).
    ///
    /// The real parameter/return types aren't needed to decompose the
    /// closure correctly — only its *arity* is, and that's already known
    /// from the closure literal itself, with no inference required.
    /// `transform_closure_expr` already tolerates missing per-parameter
    /// and return types gracefully (falling back to `Any`/`Unknown`), so
    /// synthesizing a same-arity placeholder `Ty::Function` here is
    /// sufficient to let it decompose the closure like any other.
    fn ensure_closure_has_function_ty(
        expr: &mut ast::Expr,
        param_ty: Option<&ast::Ty>,
        ret_ty: Option<&ast::Ty>,
    ) {
        let ast::ExprKind::Closure(closure) = expr.kind() else {
            return;
        };
        if matches!(expr.ty(), Some(ast::Ty::Function(_))) {
            return;
        }
        // Prefer the real, structurally-derived parameter type
        // (`closure_param_ty_for_invoke`) when the closure takes exactly
        // one parameter (true for every method this derivation currently
        // covers) — falling back to an `Any`-typed, arity-only
        // placeholder otherwise. `Any` is only safe for a closure body
        // that does nothing type-dependent with its parameter (e.g. it's
        // ignored, or just returned) — a body doing real field/method
        // access on an `Any`-typed parameter would silently resolve to an
        // error placeholder instead of erroring loudly, so callers should
        // supply a real type whenever one is derivable.
        let params = match (param_ty, closure.params.len()) {
            (Some(ty), 1) => vec![ty.clone()],
            _ => vec![ast::Ty::Any(ast::TypeAny); closure.params.len()],
        };
        // Same reasoning applies to the closure's own return type — left
        // `Unknown`, it reproduces the identical "silently becomes a null
        // placeholder" failure one step later, now at the synthetic
        // `__closureN_call` function's return position.
        let ret_ty = ret_ty.cloned().unwrap_or(ast::Ty::Unknown(ast::TypeUnknown));
        expr.set_ty(ast::Ty::Function(ast::TypeFunction {
            params,
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(ret_ty)),
        }));
    }

    fn transform_closure_expr(&mut self, expr: &mut ast::Expr) -> Result<Option<ClosureInfo>> {
        let Some(expr_ty) = expr.ty().cloned() else {
            return Ok(None);
        };
        let ast::Ty::Function(fn_ty) = expr_ty.clone() else {
            return Ok(None);
        };

        let ast::ExprKind::Closure(closure) = expr.kind_mut() else {
            return Ok(None);
        };

        let mut param_names = Vec::new();
        let mut param_set = HashSet::new();
        for param in &closure.params {
            if let ast::PatternKind::Ident(ident) = param.kind() {
                let name = ident.ident.name.as_str().to_string();
                param_set.insert(name.clone());
                param_names.push(name);
            } else {
                self.add_error(
                    Diagnostic::error(
                        "only simple identifier parameters are supported in closures".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(param.span()),
                );
                return Ok(None);
            }
        }

        let captures = self.collect_captures(closure.body.as_ref(), &param_set)?;

        let struct_ident = ast::Ident::new(format!("__Closure{}", self.counter));
        let call_ident = ast::Ident::new(format!("__closure{}_call", self.counter));
        self.counter += 1;

        let mut struct_fields: Vec<ast::StructuralField> = captures
            .iter()
            .map(|capture| ast::StructuralField::new(capture.name.clone(), capture.ty.clone()))
            .collect();
        if struct_fields.is_empty() {
            struct_fields.push(ast::StructuralField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I8)),
            ));
        }
        let struct_decl = ast::TypeStruct {
            name: struct_ident.clone(),
            generics_params: Vec::new(),
            repr: ast::ReprOptions::default(),
            fields: struct_fields,
        };
        let env_struct_ty = ast::Ty::Struct(struct_decl.clone());

        let mut struct_item = ast::Item::new(ast::ItemKind::DefStruct(ast::ItemDefStruct {
            attrs: Vec::new(),
            visibility: ast::Visibility::Private,
            name: struct_ident.clone(),
            value: struct_decl.clone(),
        }));
        struct_item.set_ty(ast::Ty::Struct(struct_decl.clone()));
        let env_param_ident = ast::Ident::new("__env");
        let mut fn_params = Vec::new();
        let mut fn_param_tys = Vec::new();
        fn_params.push(ast::FunctionParam::new(
            env_param_ident.clone(),
            env_struct_ty.clone(),
        ));
        fn_param_tys.push(env_struct_ty.clone());
        for (idx, name) in param_names.iter().enumerate() {
            let ty = fn_ty
                .params
                .get(idx)
                .cloned()
                .unwrap_or_else(|| ast::Ty::Any(ast::TypeAny));
            fn_params.push(ast::FunctionParam::new(
                ast::Ident::new(name.clone()),
                ty.clone(),
            ));
            fn_param_tys.push(ty);
        }

        let mut rewritten_body = (*closure.body).clone();
        let inferred_ret_ty = fn_ty
            .ret_ty
            .as_ref()
            .and_then(|ty| {
                if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                    None
                } else {
                    Some(ty.as_ref().clone())
                }
            })
            .or_else(|| {
                closure
                    .body
                    .ty()
                    .cloned()
                    .or_else(|| rewritten_body.ty().cloned())
                    .and_then(|ty| {
                        if matches!(ty, ast::Ty::Unknown(_)) {
                            None
                        } else {
                            Some(ty)
                        }
                    })
            });
        let fallback_ret_ty = fn_ty.ret_ty.as_ref().and_then(|ty| {
            if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                None
            } else {
                Some(ty.as_ref().clone())
            }
        });
        let call_ret_ty = inferred_ret_ty
            .clone()
            .or(fallback_ret_ty)
            .unwrap_or_else(|| ast::Ty::Unknown(ast::TypeUnknown));

        self.rewrite_captured_usage(&mut rewritten_body, &captures, &env_param_ident);

        let mut fn_item_ast = ast::ItemDefFunction::new_simple(
            call_ident.clone(),
            ast::ExprBlock::new_expr(rewritten_body),
        );
        fn_item_ast.visibility = ast::Visibility::Private;
        fn_item_ast.sig.params = fn_params;
        fn_item_ast.sig.ret_ty = Some(call_ret_ty.clone());
        fn_item_ast.ty = Some(ast::TypeFunction {
            params: fn_param_tys.clone(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(call_ret_ty.clone())),
        });
        fn_item_ast.ty_annotation = fn_item_ast.ty.clone().map(|ty_fn| ast::Ty::Function(ty_fn));

        let fn_item = ast::Item::new(ast::ItemKind::DefFunction(fn_item_ast));

        self.generated_items.push(struct_item);
        self.generated_items.push(fn_item);

        let mut fields = Vec::new();
        for capture in &captures {
            let mut value_expr = ast::Expr::ident(capture.name.clone());
            value_expr.set_ty(capture.ty.clone());
            fields.push(ast::ExprField::new(capture.name.clone(), value_expr));
        }
        if fields.is_empty() {
            let mut value_expr = ast::Expr::value(ast::Value::int(0));
            value_expr.set_ty(ast::Ty::Primitive(ast::TypePrimitive::Int(
                ast::TypeInt::I8,
            )));
            fields.push(ast::ExprField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                value_expr,
            ));
        }

        let struct_name_expr = ast::Expr::ident(struct_ident.clone());

        let mut struct_expr = ast::Expr::new(ast::ExprKind::Struct(ast::ExprStruct {
            span: fp_core::span::Span::null(),
            name: struct_name_expr.into(),
            fields,
            update: None,
        }));
        struct_expr.set_ty(env_struct_ty.clone());
        struct_expr.id = expr.id();

        *expr = struct_expr;

        let info = ClosureInfo {
            env_struct_ident: struct_ident,
            env_struct_ty,
            call_fn_ident: call_ident,
            call_ret_ty: call_ret_ty.clone(),
        };

        Ok(Some(info))
    }

    fn rewrite_usage(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
                ast::ItemKind::DefFunction(func) => {
                    let previous = std::mem::replace(
                        &mut self.current_param_types,
                        func.sig
                            .params
                            .iter()
                            .map(|param| (param.name.as_str().to_string(), param.ty.clone()))
                            .collect(),
                    );
                    self.rewrite_in_block(&mut func.body)?;
                    self.current_param_types = previous;
                }
                ast::ItemKind::DefConst(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ast::ItemKind::DefStatic(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
                _ => {}
            }
        }
        Ok(())
    }
    // FIXME: rewrite things is sus, you should be finishing this during a pas
    fn rewrite_in_expr(&mut self, expr: &mut ast::Expr) -> Result<()> {
        if expand_intrinsic_collection(expr) {
            return self.rewrite_in_expr(expr);
        }

        if let Some(info) = self.transform_closure_expr(expr)? {
            self.struct_infos
                .insert(info.env_struct_ident.as_str().to_string(), info);
            return self.rewrite_in_expr(expr);
        }

        match expr.kind_mut() {
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                while self.desugar_block_defer(block) {
                    self.rewrite_in_expr(expr)?;
                    return Ok(());
                }
                if let Some(last) = block.last_expr_mut() {
                    self.rewrite_in_expr(last)?;
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.rewrite_in_expr(expr_if.cond.as_mut())?;
                self.rewrite_in_expr(expr_if.then.as_mut())?;
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_in_expr(elze)?;
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.rewrite_in_expr(expr_loop.body.as_mut())?,
            ast::ExprKind::While(expr_while) => {
                self.rewrite_in_expr(expr_while.cond.as_mut())?;
                self.rewrite_in_expr(expr_while.body.as_mut())?;
            }
            ast::ExprKind::With(expr_with) => {
                self.rewrite_in_expr(expr_with.context.as_mut())?;
                self.rewrite_in_expr(expr_with.body.as_mut())?;
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.rewrite_in_expr(const_block.expr.as_mut())?;
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_in_expr(case.cond.as_mut())?;
                    self.rewrite_in_expr(case.body.as_mut())?;
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.rewrite_in_expr(expr_for.iter.as_mut())?;
                self.rewrite_in_expr(expr_for.body.as_mut())?;
            }
            ast::ExprKind::Let(expr_let) => self.rewrite_in_expr(expr_let.expr.as_mut())?,
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                if let Some(last) = q.block.clone().last_expr_mut() {
                    let mut last_clone = last.clone();
                    self.rewrite_in_expr(&mut last_clone)?;
                }
            }
            ast::ExprKind::Splice(s) => {
                self.rewrite_in_expr(s.token.as_mut())?;
            }
            ast::ExprKind::SplicePending(p) => {
                self.rewrite_in_expr(p.token.as_mut())?;
            }
            ast::ExprKind::Invoke(invoke) => {
                // A closure literal passed as a call argument (as opposed
                // to a function's own tail expression, whose declared
                // return-type annotation an earlier pass already copies
                // onto it) never carries a `Ty::Function` type at this
                // pre-typecheck stage — give it one so
                // `transform_closure_expr` (called from `rewrite_in_expr`
                // below) can still decompose it instead of silently
                // discarding it later. Prefer the real, structurally
                // derived parameter type when this call is one
                // `closure_param_ty_for_invoke` covers; computed once per
                // invoke (not per arg, since it depends on the whole call,
                // not any individual argument).
                let (closure_param_ty, closure_ret_ty) = self.closure_param_ty_for_invoke(invoke);
                for arg in &mut invoke.args {
                    // Scoped to exactly this position (not applied to
                    // every closure `rewrite_in_expr` visits) since
                    // closures still nested inside an unexpanded macro's
                    // argument tokens must not be touched here.
                    Self::ensure_closure_has_function_ty(
                        arg,
                        closure_param_ty.as_ref(),
                        closure_ret_ty.as_ref(),
                    );
                    self.rewrite_in_expr(arg)?;
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.rewrite_in_expr(target.as_mut())?;
                        if let Some(info) = self.closure_info_from_expr(target.as_ref()) {
                            let call_name = ast::Name::ident(info.call_fn_ident.clone());
                            let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                            new_args.push(*target.clone());
                            new_args.extend(invoke.args.iter().cloned());
                            invoke.target = ast::ExprInvokeTarget::Function(call_name);
                            invoke.args = new_args;
                            expr.set_ty(info.call_ret_ty.clone());
                        }
                    }
                    ast::ExprInvokeTarget::Function(name) => {
                        if let Some(ident) = name.as_ident() {
                            let info = self
                                .variable_infos
                                .get(ident.as_str())
                                .cloned()
                                .or_else(|| self.struct_infos.get(ident.as_str()).cloned());
                            if let Some(info) = info {
                                let mut env_expr =
                                    ast::Expr::new(ast::ExprKind::Name(name.clone()));
                                env_expr.set_ty(info.env_struct_ty.clone());
                                let call_name = ast::Name::ident(info.call_fn_ident.clone());
                                let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                                new_args.push(env_expr);
                                new_args.extend(invoke.args.iter().cloned());
                                invoke.target = ast::ExprInvokeTarget::Function(call_name);
                                invoke.args = new_args;
                                expr.set_ty(info.call_ret_ty.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.rewrite_in_expr(await_expr.base.as_mut())?;
            }
            ast::ExprKind::Async(async_expr) => {
                self.rewrite_in_expr(async_expr.expr.as_mut())?;
            }
            ast::ExprKind::Assign(assign) => {
                self.rewrite_in_expr(assign.target.as_mut())?;
                self.rewrite_in_expr(assign.value.as_mut())?;
            }
            ast::ExprKind::Select(select) => self.rewrite_in_expr(select.obj.as_mut())?,
            ast::ExprKind::Struct(struct_expr) => {
                self.rewrite_in_expr(struct_expr.name.as_mut())?;
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_in_expr(array_repeat.elem.as_mut())?;
                self.rewrite_in_expr(array_repeat.len.as_mut())?;
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Reference(reference) => {
                self.rewrite_in_expr(reference.referee.as_mut())?;
            }
            ast::ExprKind::Dereference(deref) => {
                self.rewrite_in_expr(deref.referee.as_mut())?;
            }
            ast::ExprKind::Cast(cast) => self.rewrite_in_expr(cast.expr.as_mut())?,
            ast::ExprKind::Index(index) => {
                self.rewrite_in_expr(index.obj.as_mut())?;
                self.rewrite_in_expr(index.index.as_mut())?;
            }
            ast::ExprKind::BinOp(binop) => {
                self.rewrite_in_expr(binop.lhs.as_mut())?;
                self.rewrite_in_expr(binop.rhs.as_mut())?;
            }
            ast::ExprKind::UnOp(unop) => self.rewrite_in_expr(unop.val.as_mut())?,
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.rewrite_in_expr(start.as_mut())?;
                }
                if let Some(end) = range.end.as_mut() {
                    self.rewrite_in_expr(end.as_mut())?;
                }
                if let Some(step) = range.step.as_mut() {
                    self.rewrite_in_expr(step.as_mut())?;
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => {
                self.rewrite_in_expr(expr_try.expr.as_mut())?;
                for catch in &mut expr_try.catches {
                    self.rewrite_in_expr(catch.body.as_mut())?;
                }
                if let Some(elze) = expr_try.elze.as_mut() {
                    self.rewrite_in_expr(elze.as_mut())?;
                }
                if let Some(finally) = expr_try.finally.as_mut() {
                    self.rewrite_in_expr(finally.as_mut())?;
                }
            }
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.rewrite_in_expr(expr.as_mut())?,
                ast::Value::Function(func) => self.rewrite_in_expr(func.body.as_mut())?,
                _ => {}
            },
            ast::ExprKind::Splat(splat) => self.rewrite_in_expr(splat.iter.as_mut())?,
            ast::ExprKind::SplatDict(dict) => self.rewrite_in_expr(dict.dict.as_mut())?,
            ast::ExprKind::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.rewrite_in_expr(arg)?;
                }
                for kwarg in &mut call.kwargs {
                    self.rewrite_in_expr(&mut kwarg.value)?;
                }
            }
            ast::ExprKind::Paren(paren) => self.rewrite_in_expr(paren.expr.as_mut())?,
            ast::ExprKind::IntrinsicContainer(_) => {
                unreachable!("intrinsic collections should have been expanded")
            }
            ast::ExprKind::Name(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::Closure(_) | ast::ExprKind::Any(_) | ast::ExprKind::Id(_) => {}
        }
        Ok(())
    }

    fn rewrite_in_block(&mut self, block: &mut ast::ExprBlock) -> Result<()> {
        for stmt in &mut block.stmts {
            self.rewrite_in_stmt(stmt)?;
        }
        while self.desugar_block_defer(block) {
            for stmt in &mut block.stmts {
                self.rewrite_in_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn rewrite_in_stmt(&mut self, stmt: &mut ast::BlockStmt) -> Result<()> {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.rewrite_in_expr(expr_stmt.expr.as_mut())?,
            ast::BlockStmt::Defer(stmt_defer) => self.rewrite_in_expr(stmt_defer.expr.as_mut())?,
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.rewrite_in_expr(init)?;
                    if let Some(info) = self.closure_info_from_expr(init) {
                        let mut names = Vec::new();
                        collect_pattern_idents(&stmt_let.pat, &mut names);
                        for name in names {
                            self.variable_infos.insert(name, info.clone());
                        }
                        stmt_let.pat.set_ty(info.env_struct_ty.clone());
                        init.set_ty(info.env_struct_ty.clone());
                    }
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.rewrite_in_expr(diverge)?;
                }
            }
            ast::BlockStmt::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
        Ok(())
    }

    fn rewrite_in_item(&mut self, item: &mut ast::Item) -> Result<()> {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
            ast::ItemKind::DefConst(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = Some(info.env_struct_ty.clone());
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefStatic(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = info.env_struct_ty.clone();
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefFunction(func) => self.rewrite_in_block(&mut func.body)?,
            ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
            _ => {}
        }
        Ok(())
    }

    fn closure_info_from_expr(&self, expr: &ast::Expr) -> Option<ClosureInfo> {
        match expr.kind() {
            ast::ExprKind::Struct(struct_expr) => extract_ident(struct_expr.name.as_ref())
                .and_then(|ident| self.struct_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Invoke(invoke) => {
                if let ast::ExprInvokeTarget::Function(name) = &invoke.target {
                    name.as_ident()
                        .and_then(|ident| self.function_infos.get(ident.as_str()).cloned())
                } else {
                    None
                }
            }
            ast::ExprKind::Name(name) => name
                .as_ident()
                .and_then(|ident| self.variable_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Paren(paren) => self.closure_info_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn collect_captures(&self, expr: &ast::Expr, params: &HashSet<String>) -> Result<Vec<Capture>> {
        let mut collector = CaptureCollector::new(params.clone());
        collector.visit(expr);
        Ok(collector.into_captures())
    }

    fn rewrite_captured_usage(
        &self,
        expr: &mut ast::Expr,
        captures: &[Capture],
        env_ident: &ast::Ident,
    ) {
        let mut replacer = CaptureReplacer::new(captures, env_ident.clone());
        replacer.visit(expr);
    }
}

struct CaptureCollector {
    scope: Vec<HashSet<String>>,
    captures: Vec<(String, ast::Ty)>,
    seen: HashSet<String>,
}

impl CaptureCollector {
    fn new(params: HashSet<String>) -> Self {
        Self {
            scope: vec![params],
            captures: Vec::new(),
            seen: HashSet::new(),
        }
    }

    fn visit(&mut self, expr: &ast::Expr) {
        match expr.kind() {
            ast::ExprKind::Quote(q) => {
                self.scope.push(HashSet::new());
                for stmt in &q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_ref());
            }
            ast::ExprKind::SplicePending(p) => {
                self.visit(p.token.as_ref());
            }
            ast::ExprKind::Closure(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::IntrinsicContainer(collection) => {
                let expanded = collection.clone().into_const_expr();
                self.visit(&expanded);
            }
            ast::ExprKind::Block(block) => {
                self.scope.push(HashSet::new());
                for stmt in &block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Let(expr_let) => {
                self.visit(expr_let.expr.as_ref());
                let mut names = Vec::new();
                collect_pattern_idents(&expr_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                match &invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => self.visit(target.as_ref()),
                    ast::ExprInvokeTarget::Method(select) => self.visit(select.obj.as_ref()),
                    _ => {}
                }
                for arg in &invoke.args {
                    self.visit(arg);
                }
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_ref());
                self.visit(assign.value.as_ref());
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_ref());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_ref());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_ref());
                self.visit(binop.rhs.as_ref());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_ref()),
            ast::ExprKind::Select(select) => self.visit(select.obj.as_ref()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_ref());
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_ref());
                self.visit(array_repeat.len.as_ref());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_ref()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_ref()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_ref()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_ref());
                self.visit(index.index.as_ref());
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_ref());
                self.visit(expr_if.then.as_ref());
                if let Some(elze) = expr_if.elze.as_ref() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_ref()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_ref());
                self.visit(expr_while.body.as_ref());
            }
            ast::ExprKind::With(expr_with) => {
                self.visit(expr_with.context.as_ref());
                self.visit(expr_with.body.as_ref());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_ref());
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_ref());
                self.visit(expr_for.body.as_ref());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &expr_match.cases {
                    self.visit(case.cond.as_ref());
                    self.visit(case.body.as_ref());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_ref() {
                    self.visit(start.as_ref());
                }
                if let Some(end) = range.end.as_ref() {
                    self.visit(end.as_ref());
                }
                if let Some(step) = range.step.as_ref() {
                    self.visit(step.as_ref());
                }
            }
            ast::ExprKind::Try(expr_try) => {
                self.visit(expr_try.expr.as_ref());
                for catch in &expr_try.catches {
                    self.visit(catch.body.as_ref());
                }
                if let Some(elze) = expr_try.elze.as_ref() {
                    self.visit(elze.as_ref());
                }
                if let Some(finally) = expr_try.finally.as_ref() {
                    self.visit(finally.as_ref());
                }
            }
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Expr(expr) => self.visit(expr.as_ref()),
                ast::Value::Function(func) => self.visit(func.body.as_ref()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_ref()),
            ast::ExprKind::Name(name) => {
                if let Some(ident) = name.as_ident() {
                    let name = ident.as_str();
                    if !self.is_in_scope(name) && !self.seen.contains(name) {
                        let ty = expr
                            .ty()
                            .cloned()
                            .unwrap_or_else(|| ast::Ty::Any(ast::TypeAny));
                        self.seen.insert(name.to_string());
                        self.captures.push((name.to_string(), ty));
                    }
                }
            }
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_ref()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_ref()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_ref()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &call.args {
                    self.visit(arg);
                }
                for kwarg in &call.kwargs {
                    self.visit(&kwarg.value);
                }
            }
            ast::ExprKind::Any(_) | ast::ExprKind::Id(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_ref()),
            ast::BlockStmt::Defer(stmt_defer) => self.visit(stmt_defer.expr.as_ref()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_ref() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_ref() {
                    self.visit(diverge);
                }
                let mut names = Vec::new();
                collect_pattern_idents(&stmt_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_ref()),
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
    }

    fn visit_block(&mut self, block: &ast::ExprBlock) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_item(&mut self, item: &ast::Item) {
        match item.kind() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_ref()),
            ast::ItemKind::DefStatic(def) => self.visit(def.value.as_ref()),
            ast::ItemKind::DefFunction(func) => self.visit_block(&func.body),
            ast::ItemKind::Module(module) => {
                for item in &module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }

    fn is_in_scope(&self, name: &str) -> bool {
        self.scope.iter().rev().any(|scope| scope.contains(name))
    }

    fn into_captures(self) -> Vec<Capture> {
        self.captures
            .into_iter()
            .map(|(name, ty)| Capture {
                name: ast::Ident::new(name),
                ty,
            })
            .collect()
    }
}

fn collect_pattern_idents(pat: &ast::Pattern, out: &mut Vec<String>) {
    match pat.kind() {
        ast::PatternKind::Ident(ident) => out.push(ident.ident.name.as_str().to_string()),
        ast::PatternKind::Bind(bind) => {
            out.push(bind.ident.ident.name.as_str().to_string());
            collect_pattern_idents(&bind.pattern, out);
        }
        ast::PatternKind::Tuple(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        ast::PatternKind::Struct(pat_struct) => {
            for field in &pat_struct.fields {
                if let Some(rename) = field.rename.as_ref() {
                    collect_pattern_idents(rename.as_ref(), out);
                } else {
                    out.push(field.name.as_str().to_string());
                }
            }
        }
        ast::PatternKind::TupleStruct(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        _ => {}
    }
}

struct CaptureReplacer {
    captures: HashMap<String, ast::Ty>,
    env_ident: ast::Ident,
}

impl CaptureReplacer {
    fn new(captures: &[Capture], env_ident: ast::Ident) -> Self {
        let mut capture_map = HashMap::new();
        for capture in captures {
            capture_map.insert(capture.name.as_str().to_string(), capture.ty.clone());
        }
        Self {
            captures: capture_map,
            env_ident,
        }
    }

    fn visit(&mut self, expr: &mut ast::Expr) {
        match expr.kind_mut() {
            ast::ExprKind::Name(name) => {
                if let Some(ident) = name.as_ident() {
                    if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                        let mut expr_struct =
                            ast::Expr::new(ast::ExprKind::Select(ast::ExprSelect {
                                span: fp_core::span::Span::null(),
                                obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                field: ident.clone(),
                                select: ast::ExprSelectType::Field,
                            }));
                        expr_struct.set_ty(capture_ty.clone());
                        expr_struct.id = expr.id();
                        *expr = expr_struct;
                    }
                }
            }
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_mut());
                self.visit(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_mut()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_mut());
                self.visit(expr_while.body.as_mut());
            }
            ast::ExprKind::With(expr_with) => {
                self.visit(expr_with.context.as_mut());
                self.visit(expr_with.body.as_mut());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_mut());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.visit(case.cond.as_mut());
                    self.visit(case.body.as_mut());
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_mut());
                self.visit(expr_for.body.as_mut());
            }
            ast::ExprKind::Let(expr_let) => self.visit(expr_let.expr.as_mut()),
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.visit(arg);
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.visit(target.as_mut());
                    }
                    ast::ExprInvokeTarget::Function(name) => {
                        if let Some(ident) = name.as_ident() {
                            if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                                let mut expr_struct =
                                    ast::Expr::new(ast::ExprKind::Select(ast::ExprSelect {
                                        span: fp_core::span::Span::null(),
                                        obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                        field: ident.clone(),
                                        select: ast::ExprSelectType::Field,
                                    }));
                                expr_struct.set_ty(capture_ty.clone());
                                invoke.target = ast::ExprInvokeTarget::Expr(expr_struct.into());
                            }
                        }
                    }
                    ast::ExprInvokeTarget::Method(select) => {
                        self.visit(select.obj.as_mut());
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_mut());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_mut());
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_mut());
                self.visit(assign.value.as_mut());
            }
            ast::ExprKind::Select(select) => self.visit(select.obj.as_mut()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_mut());
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_mut());
                self.visit(array_repeat.len.as_mut());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_mut()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_mut()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_mut()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_mut());
                self.visit(index.index.as_mut());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_mut());
                self.visit(binop.rhs.as_mut());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_mut()),
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.visit(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.visit(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.visit(step.as_mut());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => {
                self.visit(expr_try.expr.as_mut());
                for catch in &mut expr_try.catches {
                    self.visit(catch.body.as_mut());
                }
                if let Some(elze) = expr_try.elze.as_mut() {
                    self.visit(elze.as_mut());
                }
                if let Some(finally) = expr_try.finally.as_mut() {
                    self.visit(finally.as_mut());
                }
            }
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.visit(expr.as_mut()),
                ast::Value::Function(func) => self.visit(func.body.as_mut()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_mut()),
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_mut()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_mut()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_mut()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.visit(arg);
                }
                for kwarg in &mut call.kwargs {
                    self.visit(&mut kwarg.value);
                }
            }
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_mut());
            }
            ast::ExprKind::SplicePending(p) => {
                self.visit(p.token.as_mut());
            }
            ast::ExprKind::IntrinsicContainer(container) => {
                let mut new_expr = container.take_into_const_expr();
                self.visit(&mut new_expr);
                new_expr.id = expr.id();
                *expr = new_expr;
            }
            ast::ExprKind::Any(_)
            | ast::ExprKind::Id(_)
            | ast::ExprKind::Closure(_)
            | ast::ExprKind::Closured(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_mut()),
            ast::BlockStmt::Defer(stmt_defer) => self.visit(stmt_defer.expr.as_mut()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.visit(diverge);
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_mut()),
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
    }

    fn visit_block(&mut self, block: &mut ast::ExprBlock) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_item(&mut self, item: &mut ast::Item) {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_mut()),
            ast::ItemKind::DefStatic(def) => self.visit(def.value.as_mut()),
            ast::ItemKind::DefFunction(func) => self.visit_block(&mut func.body),
            ast::ItemKind::Module(module) => {
                for item in &mut module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }
}

fn extract_ident(expr: &ast::Expr) -> Option<&ast::Ident> {
    if let ast::ExprKind::Name(name) = expr.kind() {
        name.as_ident()
    } else {
        None
    }
}

/// Strips `#[doc = "..."]`/`///` attributes from every item (recursing
/// into modules and impl blocks) — HIR carries no doc-comment concept, so
/// backends that lower through it never see these; only callers that skip
/// HIR-based typechecking and hand items to a renderer more directly
/// (`fp-shell`'s roundtrip) need to strip them explicitly first.
pub(crate) fn strip_doc_attrs_in_items(items: &mut [ast::Item]) {
    for item in items {
        strip_doc_attrs_in_item(item);
    }
}

fn strip_doc_attrs_in_item(item: &mut ast::Item) {
    if let Some(attrs) = item_attrs_mut(item) {
        attrs.retain(|attr| !is_doc_attr(attr));
    }

    match item.kind_mut() {
        ItemKind::Module(module) => strip_doc_attrs_in_items(&mut module.items),
        ItemKind::Impl(impl_block) => strip_doc_attrs_in_items(&mut impl_block.items),
        _ => {}
    }
}

fn item_attrs_mut(item: &mut ast::Item) -> Option<&mut Vec<ast::Attribute>> {
    match item.kind_mut() {
        ItemKind::Module(module) => Some(&mut module.attrs),
        ItemKind::DefStruct(def) => Some(&mut def.attrs),
        ItemKind::DefStructural(def) => Some(&mut def.attrs),
        ItemKind::DefEnum(def) => Some(&mut def.attrs),
        ItemKind::DefType(def) => Some(&mut def.attrs),
        ItemKind::DefConst(def) => Some(&mut def.attrs),
        ItemKind::DefStatic(def) => Some(&mut def.attrs),
        ItemKind::DefFunction(def) => Some(&mut def.attrs),
        ItemKind::DefTrait(def) => Some(&mut def.attrs),
        ItemKind::Import(import) => Some(&mut import.attrs),
        ItemKind::Impl(impl_block) => Some(&mut impl_block.attrs),
        _ => None,
    }
}

fn attrs_has_name(attrs: &[ast::Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| attr_has_name(attr, name))
}

/// True if `function`'s lowered body is nothing but a bare
/// `compile_error!(...)` call — the established convention (throughout
/// `crates/fp-lang/src/std/**/*.fp`) for a function whose real
/// implementation the compiler synthesizes elsewhere, with the `.fp`-level
/// body existing only to satisfy the type checker's signature
/// requirements. See the `ItemKind::DefFunction` caller for why this can't
/// just be type-checked/lowered normally.
fn function_body_is_compiler_intrinsic_marker(function: &hir::Function) -> bool {
    let Some(body) = &function.body else {
        return false;
    };
    // Marker bodies are allowed any number of leading `let _ = param;`
    // statements (silencing "unused parameter" for params only meaningful
    // to the real, compiler-synthesized implementation) before the bare
    // `compile_error!(...)` marker call itself.
    let all_leading_stmts_are_discards = body.stmts.iter().all(|stmt| {
        matches!(
            &stmt.kind,
            hir::StmtKind::Local(local) if matches!(local.pat.kind, hir::PatKind::Wild)
        )
    });
    if !all_leading_stmts_are_discards {
        return false;
    }
    matches!(
        body.expr.as_deref().map(|expr| &expr.kind),
        Some(hir::ExprKind::IntrinsicCall(call)) if call.kind == IntrinsicKind::CompileError
    )
}

fn attr_has_name(attr: &ast::Attribute, name: &str) -> bool {
    match &attr.meta {
        ast::AttrMeta::Path(path) => path.last().as_str() == name,
        ast::AttrMeta::List(list) => list.name.last().as_str() == name,
        ast::AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
    }
}

fn is_doc_attr(attr: &ast::Attribute) -> bool {
    match &attr.meta {
        ast::AttrMeta::Path(path) => path.last().as_str() == "doc",
        ast::AttrMeta::List(list) => list.name.last().as_str() == "doc",
        ast::AttrMeta::NameValue(nv) => nv.name.last().as_str() == "doc",
    }
}
