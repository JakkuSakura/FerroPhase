use super::*;
use fp_core::ast::path::{ParsedPath, PathPrefix, QualifiedPath};

impl AstToHirLowerer {
    pub(super) fn lookup_enum_variant(&self, base: &hir::Path, name: &str) -> Option<hir::Res> {
        let hir::Res::Def(def_id) = base.res.as_ref()? else {
            return None;
        };
        let item = self
            .package
            .def_map
            .get(def_id)
            .cloned()
            .or_else(|| self.hir_program.item(def_id.clone()))?;
        let hir::ItemKind::Enum(enum_def) = &item.kind else {
            return None;
        };
        enum_def
            .variants
            .iter()
            .find(|variant| variant.name.as_str() == name)
            .map(|variant| hir::Res::Def(variant.def_id.clone()))
    }

    /// Preserve the HIR shape used for rustc's `QPath::TypeRelative` when a
    /// projection is rooted at a lexical type parameter. This HIR has no
    /// separate `QPath` node, so the parameter remains in the first segment,
    /// all projection segments remain in order, and `res` stays unresolved for
    /// type checking to resolve structurally from the parameter's bounds.
    fn preserve_lexical_projection_path(
        &self,
        path: hir::Path,
        scope: PathResolutionScope,
    ) -> hir::Path {
        let rooted_at_type_param = path.segments.first().is_some_and(|segment| {
            self.resolve_lexical_type_symbol(segment.name.as_str())
                .is_some()
        });
        if scope == PathResolutionScope::Type && path.segments.len() > 1 && rooted_at_type_param {
            hir::Path {
                segments: path.segments,
                res: None,
            }
        } else {
            path
        }
    }

    fn resolved_name_to_hir_path(
        &mut self,
        resolved_name: &ResolvedName,
        name: &Name,
        scope: PathResolutionScope,
    ) -> Result<Option<hir::Path>> {
        let resolution_scope = match resolved_name.namespace {
            ResolvedNameNamespace::Value => PathResolutionScope::Value,
            ResolvedNameNamespace::Type => PathResolutionScope::Type,
            ResolvedNameNamespace::Module => {
                return Ok(Some(hir::Path {
                    segments: resolved_name
                        .path
                        .segments
                        .iter()
                        .map(|segment| self.make_path_segment(segment, None))
                        .collect(),
                    res: Some(hir::Res::Module(resolved_name.path.segments.clone())),
                }));
            }
        };

        if resolution_scope != scope {
            return Ok(None);
        }

        let name_args = self.name_segment_args(name)?;
        let offset = resolved_name
            .path
            .segments
            .len()
            .saturating_sub(name_args.len());
        let segments = resolved_name
            .path
            .segments
            .iter()
            .enumerate()
            .map(|(idx, segment)| {
                let args = if idx >= offset {
                    name_args[idx - offset].clone()
                } else {
                    None
                };
                self.make_path_segment(segment, args)
            })
            .collect();
        // A resolved bare name still needs the ordinary lexical/module/
        // implicit-prelude lookup tiers. `lookup_global_res` is deliberately
        // a qualified-path lookup: its root node has no entry for the
        // reserved implicit prelude, so using it for `String`/`Vec`/`Arc`
        // leaves `res` empty even when the published HIR prelude metadata is
        // correct. This is the same distinction rustc makes between a
        // single identifier resolved in a scope and a qualified path walked
        // through module namespaces.
        let mut res = if resolved_name.path.segments.len() == 1 {
            match scope {
                PathResolutionScope::Value => {
                    self.resolve_value_symbol(&resolved_name.path.segments[0])
                }
                PathResolutionScope::Type => {
                    self.resolve_type_symbol(&resolved_name.path.segments[0])
                }
                PathResolutionScope::Trait => {
                    self.resolve_trait_symbol(&resolved_name.path.segments[0])
                }
            }
        } else {
            self.lookup_global_res(&resolved_name.path, scope)
        };
        if res.is_none() && self.package.module_tree.module_exists(&resolved_name.path) {
            res = Some(hir::Res::Module(resolved_name.path.segments.clone()));
        }
        Ok(Some(self.preserve_lexical_projection_path(
            hir::Path { segments, res },
            scope,
        )))
    }

    /// Resolve the expression node that owns a type reference before
    /// inspecting any expression wrapper around it.  Type syntax can be
    /// represented as `Ty::Expr(Value::Expr(..))`; the frontend's resolver
    /// records the namespace result on the owning expression node, and
    /// dropping that node in favour of the nested expression loses the
    /// result before HIR construction.  This is the AST-to-HIR equivalent of
    /// rustc carrying the resolved `Res` on the path node rather than
    /// reconstructing it from the spelling later.
    pub(super) fn resolved_type_path(&mut self, expr: &ast::Expr) -> Result<Option<hir::Path>> {
        let Some(resolved_name) = self.resolved_names.get(&expr.id()).cloned() else {
            return Ok(None);
        };
        if resolved_name.namespace != ResolvedNameNamespace::Type {
            return Ok(None);
        }
        let name = match expr.kind() {
            ast::ExprKind::Name(name) => name.clone(),
            // `Value::Expr` is a type wrapper, not a new source path. Build a
            // name from the resolver's path so the owning node's namespace
            // and canonical identity remain authoritative.
            _ => ast::Name::path(ast::Path::plain(
                resolved_name
                    .path
                    .segments
                    .iter()
                    .map(|segment| ast::Ident::new(segment.clone()))
                    .collect(),
            )),
        };
        let path =
            self.resolved_name_to_hir_path(&resolved_name, &name, PathResolutionScope::Type)?;
        // Frontend resolution can run before imports and deferred aliases
        // are installed. Preserve an actual resolution, but do not let an
        // unresolved snapshot suppress the authoritative AST-to-HIR resolver
        // after those module tables have been completed.
        Ok(path.filter(|path| path.res.is_some()))
    }

    fn name_segment_args(&mut self, name: &Name) -> Result<Vec<Option<hir::GenericArgs>>> {
        match name {
            Name::Ident(_) => Ok(vec![None]),
            Name::Path(path) => Ok(path.segments.iter().map(|_| None).collect()),
            Name::ParameterPath(path) => path
                .segments
                .iter()
                .map(|segment| {
                    if segment.args.is_empty() {
                        Ok(None)
                    } else {
                        self.convert_generic_args(&segment.args).map(Some)
                    }
                })
                .collect(),
        }
    }

    pub(super) fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            // An explicit associated-type binding (`Iterator<Item = U>` —
            // fp-lang's `parse_type_arg` turns `Item = U` into a
            // `Ty::Expr(Assign { target: Item, value: U })` entry among a
            // `ParameterPath` segment's own `args`, per this same crate's
            // `items.rs`' `explicit_bindings` extraction, which already
            // handles this shape on its own dedicated path) is not an
            // ordinary positional type argument — passing it through to
            // `transform_type_to_hir`/`ast_expr_to_hir_path` here (which
            // has no notion of a binding, only plain type references)
            // always fails as "not path-like" and produces a synthetic
            // `__fp_error` placeholder. Every real trait-bound-with-
            // binding reaches here as one of `args`, so skip it — the
            // binding itself is recovered separately by whichever caller
            // already extracts `explicit_bindings`.
            if let ast::Ty::Expr(expr) = arg {
                if matches!(expr.kind(), ast::ExprKind::Assign(_)) {
                    continue;
                }
                // A const generic argument (`Simd<f32, 4>`, `[T; N]`'s own
                // `N` reused as a generic arg elsewhere, ...) parses as a
                // plain integer-literal `Ty::Expr`, not a type at all —
                // passing it to `transform_type_to_hir`/`ast_expr_to_hir_path`
                // (which only knows how to build a *type* path) always
                // fails as "not path-like", producing a `__fp_error`
                // placeholder that then cascades into unrelated
                // "unresolved type path" noise downstream. `hir::
                // GenericArg` already has a dedicated `Const` variant for
                // exactly this shape (see `fp-typing`'s `check_type_expr`,
                // which already reports a clean, accurate "const generic
                // arguments are not supported" for it) — route it there
                // instead of forcing it through the type-path builder.
                if matches!(
                    expr.kind(),
                    ast::ExprKind::Value(value)
                        if matches!(value.as_ref(), ast::Value::Int(_) | ast::Value::UInt(_))
                ) {
                    let hir_expr = self.transform_expr_to_hir(expr)?;
                    hir_args.push(hir::GenericArg::Const(Box::new(hir_expr)));
                    continue;
                }
            }
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    /// `root_modules` (used by `resolve_item_path`'s root-module heuristic
    /// in `name_to_hir_path_with_scope`) is every top-level module name —
    /// now a direct O(children) tree lookup: every item's owning module is
    /// `ensure_module`d at the point the item itself is bound (see
    /// `bind_symbol`), so a root-level module is always a direct child of
    /// `module_tree.root()`, with no need to scan every item's qualified
    /// path (as the old flat-map-based version did) or cache the result.
    fn cached_root_modules(&self) -> HashSet<String> {
        let root = self.package.module_tree.root();
        self.package
            .module_tree
            .children(root)
            .map(|(name, _)| name.to_string())
            .collect()
    }

    /// Parses a `"a::b::c"`-shaped textual path spec into a `ParsedPath`
    /// (prefix + segments) — moved here from `fp_core::ast::path` since
    /// this resolver is its only real caller (`resolve_item_path`'s own
    /// associated-type-path handling in `name_to_hir_path_with_scope`);
    /// keeping it as a free-standing "shared kernel" function in `fp-core`
    /// implied a generality it never actually had.
    fn parse_path(spec: &str) -> std::result::Result<ParsedPath, fp_core::ast::path::PathError> {
        use fp_core::ast::path::PathError;
        let trimmed = spec.trim();
        if trimmed.is_empty() {
            return Err(PathError::EmptyPath);
        }
        let mut raw = trimmed;
        let mut prefix = PathPrefix::Plain;
        if raw.starts_with("::") {
            prefix = PathPrefix::Root;
            raw = raw.trim_start_matches("::");
        }
        let mut segments: Vec<String> = raw
            .split("::")
            .filter(|seg| !seg.is_empty())
            .map(|seg| seg.trim().to_string())
            .filter(|seg| !seg.is_empty())
            .collect();
        if segments.is_empty() {
            return Err(PathError::InvalidPath(spec.to_string()));
        }
        if matches!(prefix, PathPrefix::Plain) {
            match segments[0].as_str() {
                "crate" => {
                    prefix = PathPrefix::Crate;
                    segments.remove(0);
                }
                "self" => {
                    prefix = PathPrefix::SelfMod;
                    segments.remove(0);
                }
                "super" => {
                    let mut depth = 0;
                    while segments.first().map(|seg| seg.as_str()) == Some("super") {
                        segments.remove(0);
                        depth += 1;
                    }
                    prefix = PathPrefix::Super(depth);
                }
                _ => {}
            }
        }
        Ok(ParsedPath { prefix, segments })
    }

    /// Resolves a parsed path against this resolver's own state directly
    /// (module path, module tree, symbol tables, workspace) — moved here
    /// from `fp_core::ast::path::resolve_item_path` (a free function that
    /// needed three separate closures just to reach into `self`) since
    /// this resolver is its only real caller; folds `item_exists`/
    /// `scope_contains`/`module_exists` (previously closures built at the
    /// call site) directly into the method body instead.
    fn resolve_item_path(
        &self,
        parsed: &ParsedPath,
        scope: PathResolutionScope,
    ) -> Option<QualifiedPath> {
        if parsed.segments.is_empty() {
            return None;
        }
        let item_exists = |candidate: &QualifiedPath| {
            let key = candidate.to_key();
            if self.tree_lookup_raw(&key, scope.namespace()).is_some() {
                return true;
            }
            // Dependency trees participate only after the extern-prelude has
            // identified the first segment as a crate root. Rustc does not
            // consult every dependency when probing an ordinary lexical path;
            // doing so would change shadowing and module-relative lookup.
            let is_extern_crate_root = candidate.segments.first().is_some_and(|root| {
                self.hir_program.packages.values().any(|package| {
                    hir::HirProgram::external_crate_name(&package.borrow().id) == root.as_str()
                })
            });
            if is_extern_crate_root
                && self
                    .lookup_dependency_module_tree(candidate, scope)
                    .is_some()
            {
                return true;
            }
            // Cross-package export (e.g. `libc::macos::getenv`), looked
            // up lazily against the workspace on a local-lookup miss —
            // see `lookup_global_res`'s identical fallback.
            self.hir_program.find_export(&key).is_some()
        };
        let scope_contains = |name: &str| match scope {
            PathResolutionScope::Value => self.resolve_value_symbol(name).is_some(),
            PathResolutionScope::Type => self.resolve_type_symbol(name).is_some(),
            PathResolutionScope::Trait => self.resolve_trait_symbol(name).is_some(),
        };
        let module_exists = |p: &QualifiedPath| self.package.module_tree.module_exists(p);

        match parsed.prefix {
            PathPrefix::Root | PathPrefix::Crate => {
                // Unlike every other prefix arm here, this used to return
                // the literal segments unconditionally, with no check that
                // the resulting path actually resolves to anything. The
                // caller (`name_to_hir_path_with_scope`) treats *any*
                // `Some` from this function as authoritative and returns
                // immediately with whatever `lookup_global_res` finds for
                // it (or `None` if it finds nothing) — so an unconditional
                // `Some` here permanently short-circuited that caller's own
                // later `crate_root_candidates` fallback, which is the only
                // place that knows about the vendored real `std` package's
                // two-segment sub-crate root (`["std", "core"]`, not just
                // `["std"]` or the bare literal segments). A `crate::`
                // path written from inside `core`/`alloc` (e.g.
                // `crate::panic::Location` from `core::cell`) needs that
                // root prepended to resolve at all; falling through to
                // literal segments here made it fail before the caller
                // ever got a chance to try the correct root. Mirror
                // `name_to_hir_path_with_scope`'s own candidate order
                // (bare literal segments, for an ordinary single-crate
                // package where they need no root at all; then the
                // package's own one- and two-segment roots) and verify
                // each with `item_exists`/`module_exists` before
                // committing, so a candidate that doesn't actually resolve
                // falls through to `None` instead of masking the caller's
                // real fallback.
                let literal = QualifiedPath::new(parsed.segments.clone());
                if item_exists(&literal) || module_exists(&literal) {
                    return Some(literal);
                }
                let root_segs = &self.module_path.segments;
                for root_len in [1usize, 2usize] {
                    if root_segs.len() < root_len {
                        continue;
                    }
                    let mut candidate_segments = root_segs[..root_len].to_vec();
                    candidate_segments.extend(parsed.segments.iter().cloned());
                    let candidate = QualifiedPath::new(candidate_segments);
                    if item_exists(&candidate) || module_exists(&candidate) {
                        return Some(candidate);
                    }
                }
                None
            }
            PathPrefix::SelfMod => Some(self.module_path.join(&parsed.segments)),
            PathPrefix::Super(depth) => self
                .module_path
                .parent_n(depth)
                .map(|parent| parent.join(&parsed.segments)),
            PathPrefix::Plain => {
                let first = parsed.segments.first()?;
                let base = if self.module_path.head() == Some("bin") {
                    QualifiedPath::new(Vec::new())
                } else {
                    self.module_path.clone()
                };
                // `root_modules` is the resolver's extern-prelude/module
                // source of truth. Dependency roots are resolved through
                // `HirProgram`; do not maintain a second hardcoded list for
                // bundled standard-library roots.
                let root_modules = self.cached_root_modules();

                if parsed.segments.len() == 1 {
                    if scope_contains(first) {
                        return Some(QualifiedPath::new(vec![first.clone()]));
                    }
                    if !base.is_empty() {
                        let local = base.with_segment(first.clone());
                        if item_exists(&local) || module_exists(&local) {
                            return Some(local);
                        }
                    } else {
                        let local = QualifiedPath::new(parsed.segments.clone());
                        if item_exists(&local) {
                            return Some(local);
                        }
                    }
                    if root_modules.contains(first) {
                        return Some(QualifiedPath::new(parsed.segments.clone()));
                    }
                    return None;
                }

                if !base.is_empty() {
                    let local = base.join(&parsed.segments);
                    if item_exists(&local) {
                        return Some(local);
                    }
                    let module_candidate = base.with_segment(first.clone());
                    if module_exists(&module_candidate) {
                        return Some(local);
                    }
                } else {
                    let local = QualifiedPath::new(parsed.segments.clone());
                    if item_exists(&local) {
                        return Some(local);
                    }
                    let module_candidate = QualifiedPath::new(vec![first.clone()]);
                    if module_exists(&module_candidate) {
                        return Some(local);
                    }
                }

                if root_modules.contains(first) {
                    return Some(QualifiedPath::new(parsed.segments.clone()));
                }
                None
            }
        }
    }

    pub(super) fn name_to_hir_path_with_scope(
        &mut self,
        name: &Name,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        // Build segments from the name.
        let (mut segments, mut path_prefix) = match name {
            Name::Ident(ident) => (
                vec![self.make_path_segment(&ident.name, None)],
                PathPrefix::Plain,
            ),
            Name::Path(path) => (
                path.segments
                    .iter()
                    .map(|seg| self.make_path_segment(&seg.name, None))
                    .collect(),
                path.prefix,
            ),
            Name::ParameterPath(param_path) => {
                let mut segs = Vec::new();
                for seg in &param_path.segments {
                    let args = if seg.args.is_empty() {
                        None
                    } else {
                        Some(self.convert_generic_args(&seg.args)?)
                    };
                    segs.push(self.make_path_segment(&seg.ident.name, args));
                }
                (segs, param_path.prefix)
            }
        };

        if path_prefix == PathPrefix::Plain && !segments.is_empty() {
            let first = segments[0].name.as_str();
            if first == "crate" {
                path_prefix = PathPrefix::Crate;
                segments.remove(0);
            } else if first == "self" && (scope == PathResolutionScope::Type || segments.len() > 1)
            {
                path_prefix = PathPrefix::SelfMod;
                segments.remove(0);
            } else if first == "super" {
                let mut depth = 0usize;
                while depth < segments.len() && segments[depth].name.as_str() == "super" {
                    depth += 1;
                }
                path_prefix = PathPrefix::Super(depth);
                segments.drain(0..depth);
            }
        }

        let mut resolved = None;

        // A primitive type name (`u8`, `bool`, ...) takes absolute
        // priority over every other resolution tier below when it's the
        // *entire* bare name being resolved — real Rust's own rule (a
        // primitive is never shadowable by an import/re-export/module of
        // the same name), and specifically what a type-relative value
        // access (`u8::MAX`, built by resolving `u8` alone first and
        // appending `::MAX` afterward) depends on: without this priority
        // check, several *later* tiers below (e.g. `resolve_item_path`'s
        // own independent item-existence scan) can still resolve a bare
        // primitive name to an unrelated same-named item (vendored std's
        // own crate-root `pub use legacy_int_modules::{u8, ..}`
        // re-export) before ever reaching the primitive fallback that
        // used to be the *last* resort at the bottom of this function.
        if segments.len() == 1
            && path_prefix == PathPrefix::Plain
            && is_primitive_type_name(segments[0].name.as_str())
        {
            resolved = Some(hir::Res::Builtin(hir::BuiltinSelfType::Primitive(
                segments[0].name.as_str().to_string(),
            )));
        }

        if resolved.is_none() && segments.len() == 1 {
            if path_prefix == PathPrefix::Plain {
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
                    PathResolutionScope::Trait => self.resolve_trait_symbol(&segment.name),
                });
            } else if path_prefix == PathPrefix::SelfMod {
                // `self::` is an explicit module path — unlike a bare
                // name, it must never resolve to a lexically-scoped local
                // shadow (e.g. a function-local `const` of the same name
                // as a module-level item it's initialized from), so this
                // skips straight to the module-qualified/global tiers.
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_global_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_global_type_symbol(&segment.name),
                    PathResolutionScope::Trait => self.resolve_trait_symbol(&segment.name),
                });
            } else if matches!(path_prefix, PathPrefix::Super(_)) {
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
                    PathResolutionScope::Trait => self.resolve_trait_symbol(&segment.name),
                });
            }
        }

        // Lexical bindings (especially generic parameters) are identities, not
        // module paths. Keep their DefId intact and do not pass them through
        // global canonicalization. A same-named resolution from the
        // module/prelude/global tiers instead (e.g. a prelude alias like
        // `Result`) is a real path and should still be canonicalized below.
        if segments.len() == 1
            && matches!(
                resolved,
                Some(hir::Res::Def(_)) | Some(hir::Res::Local(_)) | Some(hir::Res::SelfTy)
            )
        {
            let is_lexical = segments.last().is_some_and(|segment| {
                match scope {
                    PathResolutionScope::Value => self.resolve_lexical_value_symbol(&segment.name),
                    PathResolutionScope::Type | PathResolutionScope::Trait => {
                        self.resolve_lexical_type_symbol(&segment.name)
                    }
                }
                .is_some()
            });
            if is_lexical || matches!(resolved, Some(hir::Res::Local(_)) | Some(hir::Res::SelfTy)) {
                return Ok(hir::Path {
                    segments,
                    res: resolved,
                });
            }
        }

        // `Self::Target` (an associated-type path) — only the single-
        // segment bare `Self` case above (line ~139-150, via
        // `resolve_type_symbol`) is otherwise recognized; a multi-segment
        // path starting with `Self` would never resolve through the
        // module-path/global-lookup logic below (there's no real module
        // named "Self"), so short-circuit here instead of falling through
        // to certain failure. Keeps all segments (including `Self`
        // itself) so `path_ty` can see both the root and the assoc-type
        // name it's projecting. Also the correct shape for a *value*-scope
        // `Self::method_name(..)` call (an associated function called via
        // `Self::`) — `fp-typing`'s `expr_path_ty` has its own dedicated
        // `Res::SelfTy` handling for exactly that case, resolving the
        // trailing segment against the current `self_types` scope; see
        // its own doc comment for why that handler, not this one, turned
        // out to be where the real bug was.
        if segments.len() > 1
            && path_prefix == PathPrefix::Plain
            && segments[0].name.as_str() == "Self"
        {
            return Ok(hir::Path {
                segments,
                res: Some(hir::Res::SelfTy),
            });
        }

        // Rustc represents a concrete `Type::associated_item` expression as
        // a type-relative qualified path: name resolution resolves only the
        // type (or trait/generic parameter) head, while type checking selects
        // the applicable impl item. Do this before any full-path value lookup
        // below, because impl members are also published for import and
        // metadata purposes and a flat `Type::item` lookup would otherwise
        // select one arbitrary generic impl. Enum variants are the one
        // exception: their variant namespace is resolved immediately and the
        // variant `DefId` is needed by constructor typing.
        if scope == PathResolutionScope::Value
            && segments.len() > 1
            && path_prefix == PathPrefix::Plain
        {
            if let Some(base_res @ (hir::Res::Def(_) | hir::Res::Builtin(_) | hir::Res::SelfTy)) =
                self.resolve_type_symbol(segments[0].name.as_str())
            {
                let mut type_relative = hir::Path {
                    segments,
                    res: Some(base_res),
                };
                if let Some(res) = self.lookup_enum_variant(
                    &type_relative,
                    &type_relative
                        .segments
                        .last()
                        .map(|segment| segment.name.as_str().to_owned())
                        .unwrap_or_default(),
                ) {
                    type_relative.res = Some(res);
                }
                return Ok(type_relative);
            }
        }

        // A plain path rooted at a lexical type parameter is rustc's
        // resolved QPath form for an associated-type projection such as
        // `I::Item` (and for chained projections such as
        // `I::Item::IntoIter`). The head remains in segment 0 and the
        // remaining segments are deliberately retained for later
        // associated-item resolution. This HIR has no separate QPath node;
        // leaving `res` empty is the representation consumed by `path_ty`'s
        // structural projection lookup. Marking the whole path with the
        // head's `Res::Def` would make `path_ty` return the generic parameter
        // immediately and discard the projection tail.
        if segments.len() > 1 && path_prefix == PathPrefix::Plain {
            if self
                .resolve_lexical_type_symbol(&segments[0].name)
                .is_some()
            {
                return Ok(self.preserve_lexical_projection_path(
                    hir::Path {
                        segments,
                        res: None,
                    },
                    scope,
                ));
            }
        }

        // `char::EscapeUnicode` (and `EscapeDefault`/`EscapeDebug`) — real
        // vendored std's `core::char` module doubles as both the `char`
        // primitive's own name *and* a real module declaring sibling
        // struct types, an ambiguity the primitive-priority skip just
        // below can't tell apart from `isize::Output`-style UFCS
        // flattening by shape alone (both are a 2-segment type path with
        // a primitive-named first segment). Resolve this narrower,
        // unambiguous case first: only when a real crate-root module is
        // literally named after the primitive *and* that module directly
        // declares a type by the trailing name — `Output` is never such
        // a member of any real `core::isize`-named module (it only ever
        // exists via a trait impl), so this can't misfire into that case.
        if scope == PathResolutionScope::Type
            && segments.len() == 2
            && path_prefix == PathPrefix::Plain
            && is_primitive_type_name(segments[0].name.as_str())
        {
            let tree = &self.package.module_tree;
            if let Some(root_module) = self
                .module_path
                .segments
                .first()
                .and_then(|root| tree.child(tree.root(), root))
            {
                if let Some(prim_module) = tree.child(root_module, segments[0].name.as_str()) {
                    if let Some(res) = tree.lookup_res(
                        prim_module,
                        hir::Namespace::Type,
                        segments[1].name.as_str(),
                    ) {
                        return Ok(hir::Path {
                            segments,
                            res: Some(res.clone()),
                        });
                    }
                }
            }
        }

        // A primitive-named first segment (`isize::Output`, from a UFCS-
        // flattened `<isize as Not>::Output`) must never be treated as a
        // module-relative or absolute path lookup, even where a real,
        // reachable module of that exact name also exists (vendored
        // std's own crate-root `pub use legacy_int_modules::{isize, ..}`
        // re-export) — real Rust's primitive names are never shadowable
        // this way. Skip straight to the generic fallback below (which
        // ultimately reaches `fp-typing`'s own primitive-first UFCS
        // handling) instead of resolving to the wrong module here.
        let first_is_shadowable_primitive = scope == PathResolutionScope::Type
            && segments
                .first()
                .is_some_and(|s| is_primitive_type_name(s.name.as_str()));

        if segments.len() > 1 && path_prefix == PathPrefix::Plain && !first_is_shadowable_primitive
        {
            // Resolve an imported module alias before applying the relative
            // path heuristic below. Rustc resolves `marker` in
            // `marker::Mut` first, then resolves `Mut` inside the module it
            // names. Without this ordering, an alias such as
            // `use super::node::marker` can fall through to suffix lookup
            // and select an unrelated public module (`core::marker`).
            if let Some(hir::Res::Module(module_path)) = match scope {
                PathResolutionScope::Value => self.resolve_value_symbol(&segments[0].name),
                PathResolutionScope::Type => self.resolve_type_symbol(&segments[0].name),
                PathResolutionScope::Trait => self.resolve_trait_symbol(&segments[0].name),
            } {
                let mut aliased = module_path;
                aliased.extend(
                    segments
                        .iter()
                        .skip(1)
                        .map(|segment| segment.name.as_str().to_string()),
                );
                let aliased = QualifiedPath::new(aliased);
                if let Some(res) = self.lookup_global_res(&aliased, scope) {
                    let offset = aliased.segments.len().saturating_sub(segments.len());
                    return Ok(hir::Path {
                        segments: aliased
                            .segments
                            .iter()
                            .enumerate()
                            .map(|(index, name)| {
                                let args = (index >= offset)
                                    .then(|| segments[index - offset].args.clone())
                                    .flatten();
                                self.make_path_segment(name, args)
                            })
                            .collect(),
                        res: Some(res),
                    });
                }
            }
            let suffix = segments
                .iter()
                .map(|segment| segment.name.as_str().to_string())
                .collect::<Vec<_>>();
            // A qualified path with no explicit `crate`/`self` prefix
            // starts in the current module and then searches enclosing
            // module scopes. `core::io::error` uses this for
            // `result::Result`: `result` is a sibling of `io`, not a child
            // of `io::error`. This is namespace resolution, not a retry or
            // a suffix scan; each candidate is a concrete lexical ancestor.
            for depth in (0..=self.module_path.segments.len()).rev() {
                let local_path = fp_core::ast::path::QualifiedPath::new(
                    self.module_path.segments[..depth].to_vec(),
                )
                .join(&suffix);
                if let Some(res) = self.lookup_global_res(&local_path, scope) {
                    return Ok(hir::Path {
                        segments: local_path
                            .segments
                            .iter()
                            .enumerate()
                            .map(|(index, name)| {
                                let offset =
                                    local_path.segments.len().saturating_sub(segments.len());
                                let args = (index >= offset)
                                    .then(|| segments[index - offset].args.clone())
                                    .flatten();
                                self.make_path_segment(name, args)
                            })
                            .collect(),
                        res: Some(res),
                    });
                }
            }
            // A path may also already BE its own real global key exactly
            // as written, with no module prefix at all — the case for a
            // primitive type's own inherent-impl item (`char::MAX`,
            // referenced from inside the *module* `core::char`, whose own
            // canonical impl path is the bare `["char"]` singleton — see
            // `canonical_type_path`'s `is_primitive_type_name` branch, not
            // `core::char`). Without this check, the crate-root guess
            // below fires first: since `core::char` (the module) really
            // does have a child named `char`... no it doesn't, but this
            // module's OWN name is `char`, so prepending the crate root
            // to `first_name` (`"char"`) below reconstructs `core::char::
            // MAX` — this exact const's own module-level sibling, i.e.
            // itself — instead of the primitive's unrelated inherent
            // constant, silently turning `char::MAX = char::MAX;` into a
            // self-referential cycle. A literal, unprefixed lookup is
            // strictly more specific than either the relative-module or
            // crate-root-guess candidates, so it takes precedence over
            // both.
            let literal_path = QualifiedPath::new(
                segments
                    .iter()
                    .map(|segment| segment.name.as_str().to_string())
                    .collect::<Vec<_>>(),
            );
            if let Some(res) = self.lookup_global_res(&literal_path, scope) {
                return Ok(hir::Path {
                    segments: literal_path
                        .segments
                        .iter()
                        .zip(segments.iter())
                        .map(|(name, original)| self.make_path_segment(name, original.args.clone()))
                        .collect(),
                    res: Some(res),
                });
            }
            // The relative-to-current-module lookup above is the ordinary
            // case (a `foo::bar` reference where `foo` is a sibling
            // module). A plain path may also be an *absolute* reference
            // naming a real crate's own root as its first segment (2018+
            // edition style, e.g. `std::os::raw::c_int` written from
            // inside `std` itself) — real rustc resolves this through the
            // extern prelude, an exact name -> crate-root mapping, not by
            // guessing. A sub-crate root is a child of the package-root
            // node in `self.package.module_tree` (ground truth from the
            // loader — every real module path was `ensure_module`d at the
            // start of `transform_package`) — a single deterministic tree
            // descent, no candidate trial-and-error.
            if let (Some(first_name), Some(package_root)) = (
                segments.first().map(|s| s.name.as_str().to_string()),
                self.module_path.segments.first().cloned(),
            ) {
                let tree = &self.package.module_tree;
                if let Some(root_module) = tree.child(tree.root(), &package_root) {
                    if tree.child(root_module, &first_name).is_some() {
                        let mut absolute_segments = vec![package_root, first_name];
                        absolute_segments.extend(
                            segments
                                .iter()
                                .skip(1)
                                .map(|segment| segment.name.as_str().to_string()),
                        );
                        let absolute = QualifiedPath::new(absolute_segments);
                        if let Some(res) = self.lookup_global_res(&absolute, scope) {
                            return Ok(hir::Path {
                                segments,
                                res: Some(res),
                            });
                        }
                    }
                }
            }
            if let Some(first) = segments.first() {
                if scope == PathResolutionScope::Value {
                    if let Some(hir::Res::Def(type_def_id)) =
                        self.resolve_type_symbol(first.name.as_str())
                    {
                        // `global_type_defs_by_def_id` narrows straight to
                        // the (usually one-element) set of qualified paths
                        // that could possibly resolve to `type_def_id`,
                        // instead of scanning every entry in
                        // `global_type_defs` (potentially thousands once
                        // vendored std is loaded) with a `format!`
                        // allocation per candidate.
                        let suffix = format!("::{}", first.name);
                        let mut type_paths: Vec<_> = self
                            .global_type_defs_by_def_id
                            .get(&type_def_id)
                            .into_iter()
                            .flatten()
                            .filter(|path| path.ends_with(&suffix))
                            .cloned()
                            .collect();
                        // `global_type_defs_by_def_id` only ever holds
                        // *this* module's own predeclared types —
                        // `type_def_id` resolved above can just as easily
                        // name a workspace dependency's type (`Option`,
                        // `Vec`, ...), whose own `AstToHirLowerer` instance
                        // (and its local maps) no longer exists. Its real
                        // path survives in that dependency's own lowered
                        // `hir::HirPackage::def_paths` instead — fall back to
                        // scanning those when the local map has nothing.
                        if type_paths.is_empty() {
                            {
                                for package in self.hir_program.packages.values() {
                                    if let Some(def_path) =
                                        package.borrow().def_paths.get(&type_def_id)
                                    {
                                        type_paths.push(def_path.join("::"));
                                    }
                                }
                            }
                        }
                        type_paths.sort();
                        for type_path in type_paths {
                            let mut associated_path = Self::parse_path(&type_path)
                                .map_err(|error| fp_core::Error::from(format!("{error:?}")))?
                                .segments;
                            associated_path.extend(
                                segments
                                    .iter()
                                    .skip(1)
                                    .map(|segment| segment.name.as_str().to_string()),
                            );
                            let associated_path = QualifiedPath::new(associated_path);
                            if let Some(res) = self.lookup_global_res(&associated_path, scope) {
                                return Ok(hir::Path {
                                    segments: associated_path
                                        .segments
                                        .iter()
                                        .enumerate()
                                        .map(|(index, name)| {
                                            let offset = associated_path
                                                .segments
                                                .len()
                                                .saturating_sub(segments.len());
                                            let args = (index >= offset)
                                                .then(|| segments[index - offset].args.clone())
                                                .flatten();
                                            self.make_path_segment(name, args)
                                        })
                                        .collect(),
                                    res: Some(res),
                                });
                            }
                        }
                    }
                }
                if let Some(hir::Res::Module(module_path)) =
                    self.resolve_value_symbol(first.name.as_str())
                {
                    let mut canonical = module_path;
                    canonical.extend(
                        segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.name.as_str().to_string()),
                    );
                    let mut canonical_segments = Vec::with_capacity(canonical.len());
                    let offset = canonical.len().saturating_sub(segments.len());
                    for (idx, seg) in canonical.iter().enumerate() {
                        let args = if idx >= offset {
                            segments[idx - offset].args.clone()
                        } else {
                            None
                        };
                        canonical_segments.push(self.make_path_segment(seg, args));
                    }
                    let canonical_path = QualifiedPath::new(canonical.clone());
                    let mut canonical_res = self.lookup_global_res(&canonical_path, scope);
                    if canonical_res.is_none()
                        && self.package.module_tree.module_exists(&canonical_path)
                    {
                        canonical_res = Some(hir::Res::Module(canonical.clone()));
                    }
                    return Ok(hir::Path {
                        segments: canonical_segments,
                        res: canonical_res,
                    });
                }
            }
        }

        if !matches!(resolved, Some(hir::Res::Local(_))) {
            let segment_names = segments
                .iter()
                .map(|seg| seg.name.as_str().to_string())
                .collect::<Vec<_>>();
            let parsed = ParsedPath {
                prefix: path_prefix,
                segments: segment_names,
            };
            if let Some(canonical) = self.resolve_item_path(&parsed, scope) {
                let mut canonical_segments = Vec::with_capacity(canonical.segments.len());
                let offset = canonical.segments.len().saturating_sub(segments.len());
                for (idx, seg) in canonical.segments.iter().enumerate() {
                    let args = if idx >= offset {
                        segments[idx - offset].args.clone()
                    } else {
                        None
                    };
                    canonical_segments.push(self.make_path_segment(seg, args));
                }
                // `resolved` (above) already went through the real,
                // priority-ordered resolution for this name (lexical,
                // then this package's own qualified declaration, then
                // prelude, then plain global) — the same tiered lookup
                // `scope_contains` uses internally, which is what let
                // `resolve_item_path` confirm this path resolves at all.
                // Once that's settled, it's authoritative: never
                // re-derive `res` via `lookup_global_res`, which has no
                // notion of that priority (it only sees the plain global
                // tier) and can resolve to a completely different,
                // same-named declaration elsewhere in the workspace (this
                // is exactly what broke bare `Ok`/`Err`/`Some`/`None`
                // resolving to their real, tagged declarations — a
                // same-named but unrelated item elsewhere in `std` won
                // instead). `lookup_global_res` is only ever needed to
                // find a `Res` for the cases `resolved` didn't cover in
                // the first place (e.g. a multi-segment path, where the
                // single-segment tiered lookup above never ran) — never
                // as a second opinion on something already answered.
                let canonical_res = if resolved.is_some() {
                    resolved.clone()
                } else {
                    let mut canonical_res = self.lookup_global_res(&canonical, scope);
                    if canonical_res.is_none() && self.package.module_tree.module_exists(&canonical)
                    {
                        canonical_res = Some(hir::Res::Module(canonical.segments.clone()));
                    }
                    canonical_res
                };
                return Ok(hir::Path {
                    segments: canonical_segments,
                    res: canonical_res,
                });
            }
        }

        if resolved.is_none() {
            if path_prefix == PathPrefix::Plain {
                if let Some(first) = segments.first() {
                    let alias = match scope {
                        PathResolutionScope::Value => self.resolve_value_symbol(&first.name),
                        PathResolutionScope::Type => self.resolve_type_symbol(&first.name),
                        PathResolutionScope::Trait => self.resolve_trait_symbol(&first.name),
                    };
                    if let Some(hir::Res::Module(module_path)) = alias {
                        let mut canonical = module_path;
                        canonical.extend(
                            segments
                                .iter()
                                .skip(1)
                                .map(|seg| seg.name.as_str().to_string()),
                        );
                        let canonical_path = QualifiedPath::new(canonical.clone());
                        resolved = self.lookup_global_res(&canonical_path, scope);
                        if resolved.is_none() && segments.len() == 1 {
                            resolved = Some(hir::Res::Module(canonical));
                        }
                    }
                }
            }
        }

        if resolved.is_none() {
            let canonical = self.canonicalize_segments(&segments);
            resolved = self.lookup_global_res(&canonical, scope);
        }

        if resolved.is_none() && path_prefix != PathPrefix::Plain {
            let crate_root_candidates: Vec<Vec<String>> = match path_prefix {
                // `crate::`/an absolute path resolves relative to the
                // current *crate's* own root — for an ordinary single-crate
                // package that's just its own package name (module_path's
                // first segment). The vendored real Rust `std` library is
                // the one exception: it bundles three separate real crates
                // (`core`, `alloc`, `std`) under one FerroPhase package, so
                // a file belonging to one of those needs its sub-crate name
                // kept too (module_path's first two segments — see
                // `rs_relative_to_module_segments` in fp-rust's provider,
                // which is the only place that ever emits a two-segment
                // crate identity like `["std", "std"]`/`["std", "core"]`).
                // Try the ordinary (one-segment) case first, then the
                // vendored-multi-crate (two-segment) case.
                PathPrefix::Root | PathPrefix::Crate => {
                    let root = &self.module_path.segments;
                    let mut candidates = Vec::new();
                    if !root.is_empty() {
                        candidates.push(root[..1].to_vec());
                    }
                    if root.len() >= 2 {
                        candidates.push(root[..2].to_vec());
                    }
                    if candidates.is_empty() {
                        candidates.push(Vec::new());
                    }
                    candidates
                }
                PathPrefix::SelfMod => vec![self.module_path.segments.clone()],
                PathPrefix::Super(depth) => {
                    let keep = self
                        .module_path
                        .segments
                        .len()
                        .saturating_sub(depth as usize);
                    vec![self.module_path.segments[..keep].to_vec()]
                }
                PathPrefix::Plain => vec![Vec::new()],
            };
            for crate_root in crate_root_candidates {
                let mut relative_segments = crate_root;
                relative_segments.extend(segments.iter().map(|seg| seg.name.as_str().to_string()));
                let relative = QualifiedPath::new(relative_segments);
                resolved = self.lookup_global_res(&relative, scope);
                if resolved.is_some() {
                    break;
                }
            }
        }

        // Type-relative value path (`Map::new(..)`, `Add::add(a, b)`,
        // `T::default()`, `u8::MAX`) — mirrors rustc's `QPath::
        // TypeRelative`: name resolution only ever resolves the *base*
        // segment, in the type namespace (structs/enums/traits/generic
        // type parameters all live there and all have a real `DefId` —
        // see `type_scopes`, which already registers a generic param's
        // name this way). The trailing segment (the method/assoc-fn name)
        // is deliberately left unresolved here; only type-checking has
        // enough information (impl/bound probing) to resolve it, exactly
        // like the existing `Self::` case above. Tried only as a last
        // resort, after every value-scope lookup above has already
        // failed, so it can never shadow a genuine value (a real
        // module-qualified constant/function takes priority).
        //
        // Applies even when this call's own `segments` has only *one*
        // entry: a `Select` chain (`u8::MAX`, `<$SelfT>::MAX`) is built
        // incrementally by `ast_expr_to_hir_path`'s own recursion — the
        // base (`u8` alone) is resolved by a separate call to this
        // function *before* the caller appends the trailing segment, so
        // gating this on `segments.len() > 1` here would only ever catch
        // a path whose *entire* multi-segment shape was already known
        // when lowering started (a call's callee, built as one compound
        // `Name` up front) and miss every incrementally-built one. A bare
        // single-segment name that isn't a type either falls through
        // unresolved exactly as before — this is purely additive.
        if resolved.is_none() && path_prefix == PathPrefix::Plain {
            if let Some(hir::Res::Def(def_id)) = self.resolve_type_symbol(segments[0].name.as_str())
            {
                resolved = Some(hir::Res::Def(def_id));
            } else if is_primitive_type_name(segments[0].name.as_str()) {
                // A primitive named directly (`u8::MAX`, `u8::from_str_
                // radix(..)`) is the same type-relative shape, just with
                // no `DefId` at all to resolve through `Res::Def` — real
                // std leans on this constantly for every integer
                // primitive's own inherent consts/methods (`MAX`/`MIN`/
                // `BITS`, `wrapping_add`, ...).
                resolved = Some(hir::Res::Builtin(hir::BuiltinSelfType::Primitive(
                    segments[0].name.as_str().to_string(),
                )));
            }
        }

        Ok(hir::Path {
            segments,
            res: resolved,
        })
    }

    pub(super) fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        match expr.kind() {
            ast::ExprKind::Name(name) => {
                if let Some(resolved_name) = self.resolved_names.get(&expr.id()).cloned() {
                    if !resolved_name.path.segments.is_empty() {
                        if let Some(path) =
                            self.resolved_name_to_hir_path(&resolved_name, name, scope)?
                        {
                            if path.res.is_some() {
                                return Ok(path);
                            }
                        }
                    }
                }
                self.name_to_hir_path_with_scope(name, scope)
            }
            ast::ExprKind::Select(select) => {
                // `T::ASSOC` is a type-relative path. Resolve its base in
                // the type namespace, as rustc does for a qualified path,
                // even when the surrounding expression is in value scope.
                // This applies to associated functions as well as constants:
                // `Vec::from` must resolve `Vec` as a type, never as a value
                // constructor or a same-named lexical binding. Keep a value
                // lookup only as the module-qualified constant fallback below.
                let type_base =
                    self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Type)?;
                let value_base = if matches!(select.select, ast::ExprSelectType::Const) {
                    Some(self.ast_expr_to_hir_path(&select.obj, PathResolutionScope::Value)?)
                } else {
                    None
                };
                // A module-qualified constant (`self::CONST` or
                // `module::CONST`) also uses `::`, but its base is a module,
                // not a type-relative path. Preserve rustc's namespace
                // fallback for that case after the type-relative lookup.
                let mut base = match value_base {
                    Some(value_base) if matches!(value_base.res, Some(hir::Res::Module(_))) => {
                        value_base
                    }
                    _ => type_base,
                };
                let member_args = if select.generic_args.is_empty() {
                    None
                } else {
                    Some(self.convert_generic_args(&select.generic_args)?)
                };
                let seg = self.make_path_segment(&select.field.name, member_args);
                base.segments.push(seg);
                if matches!(
                    select.select,
                    ast::ExprSelectType::Const | ast::ExprSelectType::Function
                ) && !matches!(base.res, Some(hir::Res::Module(_)))
                {
                    if let Some(res) = self.lookup_enum_variant(&base, &select.field.name) {
                        base.res = Some(res);
                    }
                }

                Ok(self.preserve_lexical_projection_path(base, scope))
            }
            ast::ExprKind::Invoke(invoke) => {
                let mut base = match &invoke.target {
                    ast::ExprInvokeTarget::Function(name) => {
                        self.name_to_hir_path_with_scope(name, scope)?
                    }
                    ast::ExprInvokeTarget::Expr(expr) => {
                        self.ast_expr_to_hir_path(expr.as_ref(), scope)?
                    }
                    // A generic-argumented reference to a *type* target
                    // (e.g. a qualified path's base type, or a bare type
                    // reused as a callable-position expression) parses its
                    // base as `ExprInvokeTarget::Type(ty)` rather than
                    // `Function(name)` — previously fell straight through
                    // to the generic "not path-like" error below and got
                    // replaced with a `__fp_error` placeholder path, even
                    // when the type itself resolves to a perfectly real
                    // path (the overwhelmingly common real case). Lower it
                    // the same way any other type reference is lowered,
                    // reusing its own already-resolved path when it has
                    // one; only genuinely non-path-shaped types (a tuple,
                    // a slice, `dyn Trait`, ...) still fall through.
                    ast::ExprInvokeTarget::Type(ty) => match ty {
                        // A type target is already the path head of this
                        // invoke. Resolve that head directly so lowering its
                        // generic arguments cannot re-enter this same
                        // `ExprInvokeTarget::Type` through `transform_type_to_hir`.
                        ast::Ty::Struct(struct_ty) => self.name_to_hir_path_with_scope(
                            &Name::Ident(struct_ty.name.clone()),
                            PathResolutionScope::Type,
                        )?,
                        ast::Ty::Expr(type_expr) => match type_expr.kind() {
                            ast::ExprKind::Name(name) => {
                                self.name_to_hir_path_with_scope(name, PathResolutionScope::Type)?
                            }
                            ast::ExprKind::Value(value) => match value.as_ref() {
                                ast::Value::Type(inner) => match inner {
                                    ast::Ty::Struct(struct_ty) => self
                                        .name_to_hir_path_with_scope(
                                            &Name::Ident(struct_ty.name.clone()),
                                            PathResolutionScope::Type,
                                        )?,
                                    ast::Ty::Expr(inner_expr) => match inner_expr.kind() {
                                        ast::ExprKind::Name(name) => self
                                            .name_to_hir_path_with_scope(
                                                name,
                                                PathResolutionScope::Type,
                                            )?,
                                        _ => {
                                            self.add_error(
                                                Diagnostic::error(
                                                    "expected a path-like type target".to_string(),
                                                )
                                                .with_source_context(DIAGNOSTIC_CONTEXT)
                                                .with_span(expr.span()),
                                            );
                                            hir::Path {
                                                segments: vec![
                                                    self.make_path_segment("__fp_error", None),
                                                ],
                                                res: None,
                                            }
                                        }
                                    },
                                    _ => {
                                        self.add_error(
                                            Diagnostic::error(
                                                "expected a path-like type target".to_string(),
                                            )
                                            .with_source_context(DIAGNOSTIC_CONTEXT)
                                            .with_span(expr.span()),
                                        );
                                        hir::Path {
                                            segments: vec![
                                                self.make_path_segment("__fp_error", None),
                                            ],
                                            res: None,
                                        }
                                    }
                                },
                                _ => {
                                    self.add_error(
                                        Diagnostic::error(
                                            "expected a path-like type target".to_string(),
                                        )
                                        .with_source_context(DIAGNOSTIC_CONTEXT)
                                        .with_span(expr.span()),
                                    );
                                    hir::Path {
                                        segments: vec![self.make_path_segment("__fp_error", None)],
                                        res: None,
                                    }
                                }
                            },
                            _ => {
                                self.add_error(
                                    Diagnostic::error(
                                        "expected a path-like type target".to_string(),
                                    )
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                                );
                                hir::Path {
                                    segments: vec![self.make_path_segment("__fp_error", None)],
                                    res: None,
                                }
                            }
                        },
                        _ => {
                            self.add_error(
                                Diagnostic::error("expected a path-like type target".to_string())
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                            );
                            hir::Path {
                                segments: vec![self.make_path_segment("__fp_error", None)],
                                res: None,
                            }
                        }
                    },
                    ast::ExprInvokeTarget::Method(select) => {
                        let mut base = self.ast_expr_to_hir_path(&select.obj, scope)?;
                        let seg = self.make_path_segment(&select.field.name, None);
                        base.segments.push(seg);
                        base
                    }
                    other => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "expected path-like expression for type path, found {:?}",
                                other
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(expr.span()),
                        );
                        hir::Path {
                            segments: vec![self.make_path_segment("__fp_error", None)],
                            res: None,
                        }
                    }
                };

                if !invoke.args.is_empty() {
                    let args: Vec<ast::Ty> = invoke
                        .args
                        .iter()
                        .map(|arg| match arg.kind() {
                            ast::ExprKind::Value(value) => match value.as_ref() {
                                ast::Value::Type(ty) => ty.clone(),
                                _ => ast::Ty::expr(arg.clone()),
                            },
                            _ => ast::Ty::expr(arg.clone()),
                        })
                        .collect();
                    let hir_args = self.convert_generic_args(&args)?;
                    if let Some(last) = base.segments.last_mut() {
                        if last.args.is_none() {
                            last.args = Some(hir_args);
                        }
                    }
                }

                Ok(base)
            }
            // A self-type like `&'a str`/`[T]`/`[T; N]` parses as a plain
            // `Ty` (not path-like at all — no `Name`/`Select`/`Invoke`
            // shape exists for it), wrapped as `Value::Type` by
            // `fp_lang::ast::type_to_expr`. These aren't nameable the way
            // `canonical_type_path` expects — real rustc doesn't register
            // their impls under a module path either, it keys them by a
            // structural `SimplifiedType` bucket. Mirror that: tag the
            // path with `Res::Builtin(BuiltinSelfType)` (a typed shape
            // tag) instead of relying on the segment name; see
            // `canonical_type_path`'s matching `Res::Builtin` check.
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Type(ast::Ty::Reference(reference)) => {
                    let kind = hir::BuiltinSelfType::Reference {
                        mutable: reference.mutability.unwrap_or(false),
                    };
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Slice(_)) => {
                    let kind = hir::BuiltinSelfType::Slice;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Array(_)) => {
                    let kind = hir::BuiltinSelfType::Array;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::RawPtr(ptr)) => {
                    let kind = hir::BuiltinSelfType::RawPtr {
                        mutable: ptr.mutability.unwrap_or(false),
                    };
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Nothing(_)) => {
                    let kind = hir::BuiltinSelfType::Never;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Unit(_)) => {
                    let kind = hir::BuiltinSelfType::Unit;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Tuple(_)) => {
                    let kind = hir::BuiltinSelfType::Tuple;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                ast::Value::Type(ast::Ty::Function(_)) => {
                    let kind = hir::BuiltinSelfType::Function;
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment(kind.bucket_key(), None)],
                        res: Some(hir::Res::Builtin(kind)),
                    })
                }
                // A multi-bound trait-object/`impl` type used in
                // expression position (`Box<dyn Fn(..) -> X + Send>`,
                // a closure cast, ...) — `+` (`TypeBinaryOpKind::Add`) is
                // the same token this compiler's struct-composition `+`
                // uses, just with no structural fields to merge here
                // either (see `fp-typing`'s own identical `TypeBinaryOp`
                // handling in `check_type_expr` for the type-position
                // counterpart of this exact shape/rationale). No
                // multi-trait `dyn`/`impl` representation exists to
                // build a real path for regardless, so approximate it as
                // its first bound rather than falling through to the
                // generic "not path-like" `__fp_error` placeholder below.
                ast::Value::Type(ast::Ty::TypeBinaryOp(op))
                    if op.kind == fp_core::ast::TypeBinaryOpKind::Add =>
                {
                    let lhs = ast::Expr::value(ast::Value::Type((*op.lhs).clone()));
                    self.ast_expr_to_hir_path(&lhs, scope)
                }
                _ => {
                    self.add_error(
                        Diagnostic::error(format!(
                            "expected path-like expression for type path, found {:?}",
                            value
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(expr.span()),
                    );
                    Ok(hir::Path {
                        segments: vec![self.make_path_segment("__fp_error", None)],
                        res: None,
                    })
                }
            },
            other => {
                self.add_error(
                    Diagnostic::error(format!(
                        "expected path-like expression for type path, found {:?}",
                        other
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(expr.span()),
                );
                Ok(hir::Path {
                    segments: vec![self.make_path_segment("__fp_error", None)],
                    res: None,
                })
            }
        }
    }

    pub(super) fn canonicalize_segments(&self, segments: &[hir::PathSegment]) -> QualifiedPath {
        QualifiedPath::new(
            segments
                .iter()
                .map(|s| s.name.as_str().to_string())
                .collect(),
        )
    }

    pub(super) fn make_path_segment(
        &self,
        name: &str,
        args: Option<hir::GenericArgs>,
    ) -> hir::PathSegment {
        hir::PathSegment {
            name: hir::Symbol::new(name),
            args,
        }
    }
}

/// Every primitive scalar name real Rust reserves — mirrors `fp-typing`'s
/// own `primitive_path_ty` name list (kept in sync deliberately; that one
/// maps the name to a `Ty`, this one only needs to recognize the name at
/// HIR-lowering time, before any `Ty` exists).
fn is_primitive_type_name(name: &str) -> bool {
    matches!(
        name,
        "bool"
            | "char"
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
            | "f16"
            | "f32"
            | "f64"
            | "f128"
            | "str"
    )
}
