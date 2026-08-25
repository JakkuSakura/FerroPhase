use super::*;

impl AstToHirLowerer {
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
                ast::ItemImportTree::Root | ast::ItemImportTree::Crate => {
                    prefix.clear();
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
            let Some(module_id) = self.package.module_tree.module_id(&candidate) else {
                continue;
            };
            let mut seen = HashSet::new();
            // Item children (values/types) — a direct lookup of this
            // module's own bindings instead of a flat scan over every
            // global definition in the package filtered by key prefix.
            for (child_name, _) in self
                .package
                .module_tree
                .bindings(module_id, hir::Namespace::Value)
                .chain(
                    self.package
                        .module_tree
                        .bindings(module_id, hir::Namespace::Type),
                )
            {
                if !seen.insert(child_name.to_string()) {
                    continue;
                }
                let mut full = candidate.segments.clone();
                full.push(child_name.to_string());
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                    is_glob: false,
                });
            }
            // `type X = Y;` aliases live in their own table (see
            // `register_type_alias`), not modeled by `ModuleTree` —
            // still a scan over that one flat map, filtered by key prefix.
            for key in self.type_aliases.keys() {
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
                    is_glob: false,
                });
            }
            // Module children — a direct tree lookup instead of the old
            // linear scan over every module path in the package.
            for (child_name, _) in self.package.module_tree.children(module_id) {
                if !seen.insert(child_name.to_string()) {
                    continue;
                }
                let mut full = candidate.segments.clone();
                full.push(child_name.to_string());
                out.push(ImportBinding {
                    target: full,
                    alias: None,
                    is_glob: false,
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
    ) -> bool {
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
        let mut roots = vec![fp_core::ast::path::QualifiedPath::new(Vec::new())];
        if !self.module_path.is_empty() {
            roots.push(self.module_path.clone());
        }
        let root_segs = self.module_path.segments.clone();
        if !root_segs.is_empty() {
            roots.push(fp_core::ast::path::QualifiedPath::new(
                root_segs[..1].to_vec(),
            ));
        }
        if root_segs.len() >= 2 {
            roots.push(fp_core::ast::path::QualifiedPath::new(
                root_segs[..2].to_vec(),
            ));
        }

        for start in roots {
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
                continue;
            };
            let candidate = module_path.with_segment(last.clone());

            // Whole-module import (`use std::json;`) — the last segment
            // itself names a module, not an item within one.
            if self.package.module_tree.module_exists(&candidate) {
                let res = hir::Res::Module(candidate.segments.clone());
                self.current_value_scope()
                    .insert(alias.clone(), res.clone());
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

            // Phase 2, mirrors rustc's `maybe_resolve_ident_in_module`:
            // resolve the final identifier against the walked module's
            // own bindings.
            let key = candidate.to_key();
            let value = self.lookup_symbol(&key, hir::Namespace::Value);
            let ty = self.lookup_symbol(&key, hir::Namespace::Type);
            // `type X = Y;` aliases (e.g. `libc::macos::useconds_t`) live in
            // a separate table from the module tree's value/type bindings
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
        let mut current = start.clone();
        for segment in segments {
            let candidate = current.with_segment(segment.clone());
            if self.package.module_tree.module_exists(&candidate) {
                current = candidate;
                continue;
            }
            let key = candidate.to_key();
            let module_alias = self
                .lookup_symbol(&key, hir::Namespace::Value)
                .or_else(|| self.lookup_symbol(&key, hir::Namespace::Type));
            match module_alias {
                Some(hir::Res::Module(real_path)) => {
                    current = fp_core::ast::path::QualifiedPath::new(real_path);
                }
                // Not a module alias — could still be a legitimate
                // non-module path component (e.g. an enum type name in
                // `result::Result::Ok`, where `Result` is a type, not a
                // module, but its variants are still addressed through
                // it). Rustc's resolver treats a type's own namespace as
                // a valid intermediate hop for exactly this shape; here,
                // simply keep walking literally — the final identifier
                // lookup still gets a fair chance either way, and this
                // never regresses a path that previously only worked via
                // pure literal splicing.
                _ => current = candidate,
            }
        }
        Some(current)
    }
}
