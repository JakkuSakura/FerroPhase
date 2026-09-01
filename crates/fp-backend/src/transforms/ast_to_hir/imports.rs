use super::*;

impl AstToHirLowerer {
    fn glob_shadowed_by_definition(
        &self,
        alias: &str,
        namespace: fp_core::ast::resolve::Namespace,
        res: &hir::Res,
        is_glob: bool,
    ) -> bool {
        let resolve_ast = |path: &fp_core::ast::path::QualifiedPath,
                           namespace: fp_core::ast::resolve::Namespace| {
            match self.workspace.resolve_module_path(
                &self.package_id,
                &self.module_path,
                path,
                namespace,
            ) {
                fp_core::ast::resolve::ResolutionResult::Found(
                    hir::Res::Def(id),
                ) => Some(hir::Res::Def(id)),
                _ => None,
            }
        };
        if !is_glob {
            return false;
        }
        let _ = res;
        matches!(
            self.workspace.resolve_local(alias, namespace),
            fp_core::ast::resolve::ResolutionResult::Found(_)
        ) && !self.glob_import_bindings.contains(&(
            self.module_path.clone(),
            namespace,
            alias.to_string(),
        ))
    }

    fn check_import_collision(
        &mut self,
        alias: &str,
        namespace: fp_core::ast::resolve::Namespace,
        res: &hir::Res,
        span: Span,
    ) {
        let existing = self.workspace.resolve_local(alias, namespace);
        if matches!(existing, fp_core::ast::resolve::ResolutionResult::Found(_)) {
            self.add_error(
                Diagnostic::error(format!("ambiguous import `{alias}`"))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
            );
        }
    }

    pub(super) fn collect_imports(
        &self,
        base: Vec<String>,
        tree: &ast::ItemImportTree,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        match tree {
            // A group member's bare `self` (`use path::Item::{self, ..};`)
            // parses as `Path` wrapping a *single* `SelfMod` segment, not
            // the bare `SelfMod` variant itself — `parse_use_path`'s loop
            // always wraps whatever it parses in a `Path`, and `self` in
            // group position hits the same `SelfMod` branch as `self::`
            // at the start of an ordinary path, with no syntactic way to
            // tell the two apart at parse time. Handle it here, before
            // `collect_imports_from_path`, which would otherwise treat a
            // lone `SelfMod` segment as "current module" prefix semantics
            // (`self::`) — overwriting `base` with `self.module_path`
            // instead of using `base` as-is — and silently produce no
            // binding at all, since a single-segment path with no `::`
            // separator never reaches that function's `out.push`.
            ast::ItemImportTree::Path(path)
                if matches!(path.segments.as_slice(), [ast::ItemImportTree::SelfMod]) =>
            {
                self.collect_imports(base, &ast::ItemImportTree::SelfMod, out)
            }
            ast::ItemImportTree::Path(path) => self.collect_imports_from_path(base, path, out),
            ast::ItemImportTree::Ident(ident) => {
                let mut target = base;
                target.push(ident.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: None,
                    is_glob: false,
                });
                Ok(())
            }
            ast::ItemImportTree::Rename(rename) => {
                let mut target = base;
                target.push(rename.from.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: Some(rename.to.name.clone()),
                    is_glob: false,
                });
                Ok(())
            }
            ast::ItemImportTree::Group(group) => {
                for item in &group.items {
                    self.collect_imports(base.clone(), item, out)?;
                }
                Ok(())
            }
            // A bare `self` reached here only ever comes from a *group*
            // member (`use path::Item::{self, Variant1, Variant2};`, real
            // core::prelude::v1's own `pub use crate::option::Option::
            // {self, None, Some};`/`crate::result::Result::{self, Err,
            // Ok};`) — `self::` as the *first* segment of a path (`use
            // self::foo;`, "current module") is consumed directly by
            // `collect_imports_from_path`'s own per-segment loop and never
            // delegates to this function for that segment. In the group
            // position, `self` means "the enclosing path itself" (`Item`,
            // not just its variants) — dropping it here (as a no-op)
            // silently imported every variant but never the type/enum
            // itself, so `use ...::Option::{self, ..}` never actually
            // brought `Option` into scope, only `None`/`Some`.
            ast::ItemImportTree::SelfMod => {
                if !base.is_empty() {
                    out.push(ImportBinding {
                        target: base,
                        alias: None,
                        is_glob: false,
                    });
                }
                Ok(())
            }
            ast::ItemImportTree::Root
            | ast::ItemImportTree::SuperMod
            | ast::ItemImportTree::Crate
            | ast::ItemImportTree::Glob => Ok(()),
        }
    }

    pub(super) fn collect_imports_from_path(
        &self,
        base: Vec<String>,
        path: &ast::ItemImportPath,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        let mut prefix = base;
        // Each `super` climbs one level *from wherever the previous
        // segment left off* — a repeated `SuperMod` must pop from the
        // already-adjusted `prefix`, not re-derive from `self.module_path`
        // every time, or `super::super::X` collapses to the same result as
        // a single `super::X` (confirmed: real `core::iter`'s own `use
        // super::super::{Enumerate, Map, ...};` resolved one level too
        // shallow because of this, leaving every adapter type unresolved).
        let mut super_climbed = false;
        for seg in &path.segments {
            match seg {
                ast::ItemImportTree::Root => {
                    prefix.clear();
                    super_climbed = false;
                }
                ast::ItemImportTree::Crate => {
                    // `crate::` is the defining crate's root, not the
                    // process-wide empty root used by `::`. Ordinary Cargo
                    // packages are represented crate-relatively, while the
                    // bundled sysroot packages retain their real crate root
                    // (`core::`, `alloc::`, `std::`) in the module tree.
                    // Derive the representation from that tree boundary so
                    // the same import resolver handles both layouts.
                    prefix = self.package_crate_root();
                    super_climbed = false;
                }
                ast::ItemImportTree::SelfMod => {
                    prefix = self.module_path.segments.clone();
                    super_climbed = false;
                }
                ast::ItemImportTree::SuperMod => {
                    if !super_climbed {
                        prefix = self.module_path.segments.clone();
                        super_climbed = true;
                    }
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
                        is_glob: false,
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
                    out.push(ImportBinding {
                        target: prefix,
                        alias: None,
                        is_glob: true,
                    });
                    return Ok(());
                }
            }
        }

        if !prefix.is_empty() {
            out.push(ImportBinding {
                target: prefix,
                alias: None,
                is_glob: false,
            });
        }
        Ok(())
    }

    pub(super) fn package_crate_root(&self) -> Vec<String> {
        // The vendored Rust sysroot is intentionally one FerroPhase package
        // containing Rust's three distinct crates. Inside `core::io`,
        // `crate::result` therefore means `core::result`, never the outer
        // package's `std` root. Keep this decision at package metadata/module
        // boundaries rather than rewriting paths at individual call sites.
        if let Some(current_root) = self.module_path.segments.first()
            && matches!(current_root.as_str(), "core" | "alloc" | "std")
            && self.workspace.module_exists(&self.package_id, &fp_core::ast::path::QualifiedPath::new(vec![current_root.clone()]))
        {
            return vec![current_root.clone()];
        }
        let root = hir::HirProgram::external_crate_name(&self.package_id);
        let candidate = fp_core::ast::path::QualifiedPath::new(vec![root]);
        if self.workspace.module_exists(&self.package_id, &candidate) {
            return candidate.segments;
        }
        Vec::new()
    }

    /// Expand `use <prefix>::*;` into one `ImportBinding` per direct member
    /// (value, type, or submodule) of the target module, so glob re-exports
    /// like `pub use macos::*;` actually make the re-exported module's
    /// contents resolvable under the importing module's own path — this
    /// pass previously treated every glob import as a silent no-op.
    pub(super) fn expand_glob_import(&self, prefix: Vec<String>, out: &mut Vec<ImportBinding>) {
        let target_path = fp_core::ast::path::QualifiedPath::new(prefix.clone());
        let mut candidates = vec![target_path.clone()];
        if !self.module_path.is_empty() {
            let relative = self.module_path.join(&prefix);
            if relative != target_path {
                candidates.push(relative);
            }
        }
        for candidate in candidates {
            let mut seen = HashSet::new();
            if let Some(members) = self.workspace.external_module_member_names(&candidate) {
                for child_name in members {
                    if !seen.insert(child_name.clone()) {
                        continue;
                    }
                    let mut full = candidate.segments.clone();
                    full.push(child_name);
                    out.push(ImportBinding {
                        target: full,
                        alias: None,
                        // Preserve the originating glob's precedence: this
                        // binding may be shadowed by a local definition, but
                        // must still conflict with another glob target.
                        is_glob: true,
                    });
                }
                return;
            }
            let Some(members) = self.workspace.module_member_names(&self.package_id, &candidate) else { continue };
            for child_name in members {
                if !seen.insert(child_name.to_string()) { continue; }
                let mut full = candidate.segments.clone();
                full.push(child_name.to_string());
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                    is_glob: true,
                });
            }
            // Type aliases have their own table, so use the parent-module
            // index populated by `register_type_alias` instead of scanning
            // every alias and splitting every qualified key for each glob.
            for child in self
                .type_alias_children
                .get(&candidate.to_key())
                .into_iter()
                .flatten()
            {
                let child = child.clone();
                if !seen.insert(child.clone()) {
                    continue;
                }
                let mut full = candidate.segments.clone();
                full.push(child);
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                    is_glob: true,
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
    pub(super) fn register_import_binding(
        &mut self,
        binding: ImportBinding,
        visibility: &ast::Visibility,
        span: Span,
    ) -> bool {
        let alias = binding
            .alias
            .clone()
            .unwrap_or_else(|| binding.target.last().cloned().unwrap_or_default());
        if alias.is_empty() {
            return false;
        }
        let resolved_key = (
            self.module_path.clone(),
            alias.clone(),
            binding.target.clone(),
        );
        if self.resolved_import_aliases.contains(&resolved_key) {
            return true;
        }
        let Some((last, prefix)) = binding.target.split_last() else {
            return false;
        };

        // Candidate starting points for the segment walk below, in
        // priority order (first match wins) — same crate-root reasoning
        // this always had: `use crate::X`/`use ::X` (an absolute import)
        // reaches here with its "crate::"/root prefix already stripped
        // by `collect_imports_from_path`, which doesn't know the current
        // crate's own root depth. For an ordinary single-crate package
        // the crate root is just the package name (`module_path`'s first
        // segment); the vendored real Rust `std` library is the one
        // exception, bundling three real crates (`core`/`alloc`/`std`)
        // under one FerroPhase package, so a file belonging to one of
        // those needs its sub-crate name kept too (`module_path`'s first
        // two segments — see `rs_relative_to_module_segments` in
        // fp-rust's provider). Trying each possible root is harmless for
        // ordinary packages, where they either coincide or a root simply
        // never resolves anything.
        let start = fp_core::ast::path::QualifiedPath::new(Vec::new());
        {
            // Phase 1, mirrors rustc's `resolve_import`/`maybe_resolve_path`
            // (`compiler/rustc_resolve/src/imports.rs`): walk every segment
            // except the last one at a time, looking each up in the
            // *current* module's own binding table and continuing from
            // whatever module that binding actually resolves to — so a
            // re-exported/aliased module (`pub use core::option;`) is
            // followed transparently, the same way rustc's resolver does,
            // rather than re-deriving one flat guessed string key from the
            // literal path text.
            let Some(module_path) = self.resolve_module_path_through_aliases(&start, prefix) else {
                return false;
            };
            let candidate = module_path.with_segment(last.clone());
            let resolve_ast = |path: &fp_core::ast::path::QualifiedPath,
                               namespace: fp_core::ast::resolve::Namespace| {
                match self.workspace.resolve_module_path_final(
                    &self.package_id,
                    &self.module_path,
                    path,
                    namespace,
                ) {
                    fp_core::ast::resolve::ResolutionResult::Found(
                        hir::Res::Def(id),
                    ) => Some(hir::Res::Def(id)),
                    _ => None,
                }
            };

            // Whole-module import (`use std::json;`) — the last segment
            // itself names a module, not an item within one. A namespace-only
            // node can share the same path as a nominal item with associated
            // members; resolve the item namespaces first so that node is not
            // mistaken for a real module (the same final-segment precedence
            // used by rustc's resolver).
            let candidate_value = resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Value);
            let candidate_type = resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Type);
            let candidate_alias = self.type_aliases.get(&candidate.to_key()).cloned();
            let dependency_value =
                self.lookup_dependency_binding(&candidate, fp_core::ast::resolve::Namespace::Value);
            let dependency_type = self.lookup_dependency_binding(&candidate, fp_core::ast::resolve::Namespace::Type);
            if self.workspace.module_exists(&self.package_id, &candidate)
                && candidate_value.is_none()
                && candidate_type.is_none()
                && dependency_value.is_none()
                && dependency_type.is_none()
                && candidate_alias.is_none()
            {
                let res = hir::Res::Module(candidate.segments.clone());
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Value,
                    &res,
                    binding.is_glob,
                ) {
                    self.check_import_collision(&alias, fp_core::ast::resolve::Namespace::Value, &res, span);
                    if binding.is_glob {
                        self.glob_import_bindings.insert((
                            self.module_path.clone(),
                            fp_core::ast::resolve::Namespace::Value,
                            alias.clone(),
                        ));
                    }
                }
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Type,
                    &res,
                    binding.is_glob,
                ) {
                    self.check_import_collision(&alias, fp_core::ast::resolve::Namespace::Type, &res, span);
                    if binding.is_glob {
                        self.glob_import_bindings.insert((
                            self.module_path.clone(),
                            fp_core::ast::resolve::Namespace::Type,
                            alias.clone(),
                        ));
                    }
                }
                // Every top-level item gets its own transient
                // `with_module_scope` push/pop cycle (see
                // `transform_package`), so a scope-only insert here is
                // invisible to sibling items processed afterward (e.g. a
                // `use std::json;` above `fn main() {}` would otherwise
                // never be visible inside `main`'s body). Persist the
                // alias the same way value/type re-exports already do
                // below, so `module::item()` resolves regardless of
                // which sibling item introduced the `use`.
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Value,
                    &res,
                    binding.is_glob,
                ) {
                    self.record_import_symbol(
                        &alias,
                        fp_core::ast::resolve::Namespace::Value,
                        res.clone(),
                        visibility,
                    );
                }
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Type,
                    &res,
                    binding.is_glob,
                ) {
                    self.record_import_symbol(&alias, fp_core::ast::resolve::Namespace::Type, res, visibility);
                }
                self.resolved_import_aliases.insert(resolved_key);
                return true;
            }

            // Phase 2, mirrors rustc's `maybe_resolve_ident_in_module`:
            // resolve the final identifier against the walked module's
            // own bindings.
            let value = resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Value)
                .or(dependency_value);
            let ty = resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Type)
                .or(dependency_type);
            // `type X = Y;` aliases (e.g. `libc::macos::useconds_t`) live in
            // a separate table from the module tree's value/type bindings
            // (see `register_type_alias`) — an import/glob-re-export needs
            // its own explicit copy step here, or a re-exported alias (e.g.
            // via `libc::mod.fp`'s `pub use macos::*;`) never becomes
            // resolvable under the shorter path at all.
            let type_alias = self.type_aliases.get(&candidate.to_key()).cloned().or_else(|| {
                self.find_workspace_type_alias(&candidate.to_key()).map(|alias| {
                    self.type_alias_defining_modules
                        .insert(candidate.to_key(), alias.defining_module);
                    alias.target
                })
            });
            if value.is_none() && ty.is_none() && type_alias.is_none() {
                return false;
            }

            if let Some(res) = value {
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Value,
                    &res,
                    binding.is_glob,
                ) {
                    self.check_import_collision(&alias, fp_core::ast::resolve::Namespace::Value, &res, span);
                    if binding.is_glob {
                        self.glob_import_bindings.insert((
                            self.module_path.clone(),
                            fp_core::ast::resolve::Namespace::Value,
                            alias.clone(),
                        ));
                    }
                    self.record_import_symbol(&alias, fp_core::ast::resolve::Namespace::Value, res, visibility);
                }
            }
            if let Some(res) = ty {
                if !self.glob_shadowed_by_definition(
                    &alias,
                    fp_core::ast::resolve::Namespace::Type,
                    &res,
                    binding.is_glob,
                ) {
                    self.check_import_collision(&alias, fp_core::ast::resolve::Namespace::Type, &res, span);
                    if binding.is_glob {
                        self.glob_import_bindings.insert((
                            self.module_path.clone(),
                            fp_core::ast::resolve::Namespace::Type,
                            alias.clone(),
                        ));
                    }
                    self.record_import_symbol(&alias, fp_core::ast::resolve::Namespace::Type, res, visibility);
                }
            }
            if let Some(alias_ty) = type_alias {
                let new_key = self.qualify_name(&alias);
                self.type_alias_children
                    .entry(self.module_path.to_key())
                    .or_default()
                    .push(alias.clone());
                self.type_aliases.insert(new_key.clone(), alias_ty);
                if let Some(defining_module) = self.type_alias_defining_modules.get(&candidate.to_key()).cloned() {
                    self.type_alias_defining_modules
                        .insert(new_key, defining_module);
                }
            }
            self.resolved_import_aliases.insert(resolved_key);
            return true;
        }
    }

    /// Walks `segments` one at a time starting from `start`, looking up
    /// each name against the *current* module's own binding table and
    /// following any resolved module alias's real canonical path as the
    /// scope for the next segment — mirrors rustc's `resolve_import`/
    /// `maybe_resolve_path` (`compiler/rustc_resolve/src/imports.rs`):
    /// resolution walks forward from whatever a segment's binding
    /// actually points at, never re-deriving a flat string key from the
    /// literal path text. Returns the final resolved module path once
    /// every segment has been consumed as a module hop, or `None` if a
    /// step doesn't resolve to a module at all. Terminates naturally —
    /// each step consumes exactly one segment of a finite input path, so
    /// (unlike following an *alias* recursively) this walk can't loop.
    pub(super) fn resolve_module_path_through_aliases(
        &self,
        start: &fp_core::ast::path::QualifiedPath,
        segments: &[String],
    ) -> Option<fp_core::ast::path::QualifiedPath> {
        let resolve_ast = |path: &fp_core::ast::path::QualifiedPath,
                           namespace: fp_core::ast::resolve::Namespace| {
            match self.workspace.resolve_module_path_final(
                &self.package_id, &self.module_path, path, namespace,
            ) {
                fp_core::ast::resolve::ResolutionResult::Found(
                    hir::Res::Def(id),
                ) => Some(hir::Res::Def(id)),
                _ => None,
            }
        };
        let mut current = start.clone();
        for segment in segments {
            // An unqualified first segment is resolved in the current module
            // before the extern prelude. This is the path rustc takes for an
            // `extern crate alloc as alloc_crate;` binding declared inside
            // `std`: `alloc_crate::vec` must follow the local alias, while a
            // bare `alloc::vec` may still enter through the extern prelude.
            if current.segments.is_empty() {
                // A bundled sysroot package has a real crate-root segment in
                // its HIR tree (`std::prelude`, `alloc::vec`, ...), whereas
                // an ordinary provider package is crate-relative. Resolve a
                // plain import from the current module against that owning
                // root exactly once; otherwise `use prelude::rust_2024::*`
                // in std is searched as an unrooted `prelude` path and the
                // crate's own nominal re-exports never enter its prelude.
                let local_bases = if self.module_path.is_empty() {
                    vec![fp_core::ast::path::QualifiedPath::new(
                        self.package_crate_root(),
                    )]
                } else {
                    // An import declared in a child module resolves an
                    // unqualified first segment through enclosing module
                    // scopes. Real std relies on this for `std::io`'s
                    // `alloc_crate::io` re-export: `alloc_crate` is an
                    // `extern crate` alias declared in the parent `std`
                    // module, not a child of `std::io`.
                    (0..=self.module_path.segments.len())
                        .rev()
                        .map(|len| {
                            fp_core::ast::path::QualifiedPath::new(
                                self.module_path.segments[..len].to_vec(),
                            )
                        })
                        .collect()
                };
                for local_base in local_bases {
                    let local_path = local_base.with_segment(segment.clone());
                    let local_alias = resolve_ast(&local_path, fp_core::ast::resolve::Namespace::Value)
                        .or_else(|| resolve_ast(&local_path, fp_core::ast::resolve::Namespace::Type));
                    if let Some(hir::Res::Module(real_path)) = local_alias {
                        current = fp_core::ast::path::QualifiedPath::new(real_path);
                        break;
                    }
                    if self.workspace.module_exists(&self.package_id, &local_path) {
                        current = local_path;
                        break;
                    }
                }
                if !current.segments.is_empty() {
                    continue;
                }
            }
            if current.segments.is_empty()
                && self.hir_program.with(|program| {
                    program.packages.values().any(|package| {
                        hir::HirProgram::external_crate_name(&package.borrow().id) == *segment
                    })
                })
            {
                current.segments.push(segment.clone());
                continue;
            }
            let candidate = current.with_segment(segment.clone());
            if current.segments.len() == 1 && current.head() == candidate.head() {
                if let Some(module) = self
                    .workspace
                    .resolve_external_module_path_from(&self.package_id, &candidate)
                {
                    current = module;
                    continue;
                }
            }
            let module_alias = resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Value)
                .or_else(|| resolve_ast(&candidate, fp_core::ast::resolve::Namespace::Type))
                .or_else(|| self.lookup_dependency_binding(&candidate, fp_core::ast::resolve::Namespace::Value))
                .or_else(|| self.lookup_dependency_binding(&candidate, fp_core::ast::resolve::Namespace::Type));
            match module_alias {
                Some(hir::Res::Module(real_path)) => {
                    current = fp_core::ast::path::QualifiedPath::new(real_path);
                }
                // Intermediate path segments are resolved in the module
                // namespace. A dependency can publish a type/value binding
                // with the same spelling as a real module, so do not let
                // that other namespace shadow the module child while walking
                // toward the final identifier (`std::fmt::Formatter`, for
                // example).
                Some(_) if self.path_is_definition_namespace(&candidate) => {
                    current = candidate;
                }
                Some(_) => return None,
                None if self.path_is_definition_namespace(&candidate) => {
                    current = candidate;
                }
                None => return None,
            }
        }
        Some(current)
    }

    /// Resolve one external-prelude segment from the package that owns it.
    /// The consumer's copied tree is an optimization, but it cannot represent
    /// every transparent alias: a `type` alias is recorded by DefId/DefPath,
    /// and a public module re-export may point into another sysroot package.
    /// rustc keeps both facts in the defining crate's resolver tables, so use
    /// those authoritative package tables when the copied tree has no entry.
    fn lookup_dependency_binding(
        &self,
        path: &fp_core::ast::path::QualifiedPath,
        namespace: fp_core::ast::resolve::Namespace,
    ) -> Option<hir::Res> {
        if let Some(res) = self.workspace.resolve_external_path_from(&self.package_id, path, namespace) {
            if let hir::Res::Def(def_id) = res {
                return Some(hir::Res::Def(def_id));
            }
        }
        self.workspace
            .resolve_external_module_path_from(&self.package_id, path)
            .map(|_| hir::Res::Module(path.segments.clone()))
    }

    /// A type definition owns a child namespace for associated items. Enum
    /// variants are the important case here, but using the item kind keeps
    /// this aligned with rustc's definition namespace rather than encoding
    /// individual constructor names in the resolver.
    fn path_is_definition_namespace(&self, path: &fp_core::ast::path::QualifiedPath) -> bool {
        if self.workspace.module_exists(&self.package_id, path) {
            return true;
        }
        let resolved = self
            .lookup_dependency_binding(path, fp_core::ast::resolve::Namespace::Type)
            .or_else(|| self.lookup_dependency_binding(path, fp_core::ast::resolve::Namespace::Value));
        let Some(hir::Res::Def(def_id)) = resolved else {
            return false;
        };
        self.package
            .def_map
            .get(&def_id)
            .cloned()
            .or_else(|| self.hir_program.item(def_id))
            .is_some_and(|item| {
                matches!(
                    item.kind,
                    hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) | hir::ItemKind::Trait(_)
                )
            })
    }
}
