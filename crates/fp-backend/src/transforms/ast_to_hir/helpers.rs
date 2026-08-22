use super::*;
use fp_core::ast::path::{ParsedPath, PathPrefix, QualifiedPath, parse_path, resolve_item_path};

impl HirGenerator {
    fn resolved_name_to_hir_path(
        &mut self,
        resolved_name: &fp_typing::ResolvedName,
        name: &Name,
        scope: PathResolutionScope,
    ) -> Result<Option<hir::Path>> {
        let resolution_scope = match resolved_name.namespace {
            fp_typing::ResolvedNameNamespace::Value => PathResolutionScope::Value,
            fp_typing::ResolvedNameNamespace::Type => PathResolutionScope::Type,
            fp_typing::ResolvedNameNamespace::Module => {
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
        let mut res = self.lookup_global_res(&resolved_name.path, scope);
        if res.is_none() && self.package.module_tree.module_exists(&resolved_name.path) {
            res = Some(hir::Res::Module(resolved_name.path.segments.clone()));
        }
        Ok(Some(hir::Path { segments, res }))
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
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    /// `root_modules` (used by `resolve_item_path`'s root-module heuristic
    /// in `name_to_hir_path_with_scope`) is derived from `module_defs`/
    /// `global_type_defs`/`global_value_defs`, which only grow as more
    /// items get processed — but the caller used to rebuild it (including
    /// a `parse_path` string-parse per global def key) from scratch on
    /// *every* unresolved path reference. For a large package (the
    /// vendored std library) with many still-unresolved references, that
    /// made each one pay an O(workspace definition count) cost. Cache it,
    /// keyed by a cheap size snapshot of its three inputs — invalidated
    /// (and rebuilt) only when one of them has actually grown since the
    /// last call.
    fn cached_root_modules(&mut self) -> HashSet<String> {
        let root = self.package.module_tree.root();
        let sizes = (
            self.package.module_tree.children(root).count(),
            self.global_type_defs.len(),
            self.global_value_defs.len(),
        );
        if let Some((a, b, c, cached)) = &self.root_modules_cache {
            if (*a, *b, *c) == sizes {
                return cached.clone();
            }
        }
        let mut root_modules = HashSet::new();
        for (name, _) in self.package.module_tree.children(root) {
            root_modules.insert(name.to_string());
        }
        for key in self
            .global_type_defs
            .keys()
            .chain(self.global_value_defs.keys())
        {
            if let Ok(parsed) = parse_path(key) {
                if let Some(head) = parsed.segments.first() {
                    root_modules.insert(head.clone());
                }
            }
        }
        self.root_modules_cache = Some((sizes.0, sizes.1, sizes.2, root_modules.clone()));
        root_modules
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

        if segments.len() == 1 {
            if path_prefix == PathPrefix::Plain {
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
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
                });
            } else if matches!(path_prefix, PathPrefix::Super(_)) {
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
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
                    PathResolutionScope::Type => self.resolve_lexical_type_symbol(&segment.name),
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
        // name it's projecting.
        if segments.len() > 1 && path_prefix == PathPrefix::Plain && segments[0].name.as_str() == "Self"
        {
            return Ok(hir::Path {
                segments,
                res: Some(hir::Res::SelfTy),
            });
        }

        if segments.len() > 1 && path_prefix == PathPrefix::Plain {
            let local_path = self.module_path.join(
                &segments
                    .iter()
                    .map(|segment| segment.name.as_str().to_string())
                    .collect::<Vec<_>>(),
            );
            if let Some(res) = self.lookup_global_res(&local_path, scope) {
                return Ok(hir::Path {
                    segments: local_path
                        .segments
                        .iter()
                        .enumerate()
                        .map(|(index, name)| {
                            let offset = local_path.segments.len().saturating_sub(segments.len());
                            let args = (index >= offset)
                                .then(|| segments[index - offset].args.clone())
                                .flatten();
                            self.make_path_segment(name, args)
                        })
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
                            return Ok(hir::Path { segments, res: Some(res) });
                        }
                    }
                }
            }
            if let Some(first) = segments.first() {
                let debug = std::env::var("FP_DEBUG_ASSOC").is_ok() && first.name.as_str() == "String";
                if debug {
                    eprintln!(
                        "DEBUG assoc-path first={:?} resolve_type_symbol={:?}",
                        first.name.as_str(),
                        self.resolve_type_symbol(first.name.as_str())
                    );
                }
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
                        // `Vec`, ...), whose own `HirGenerator` instance
                        // (and its local maps) no longer exists. Its real
                        // path survives in that dependency's own lowered
                        // `hir::Package::def_paths` instead — fall back to
                        // scanning those when the local map has nothing.
                        if type_paths.is_empty() {
                            if let Some(ref workspace) = self.workspace {
                                for (_module_path, hir_program, _exports) in
                                    workspace.hir_definitions()
                                {
                                    if let Some(def_path) =
                                        hir_program.def_paths.get(&type_def_id)
                                    {
                                        type_paths.push(def_path.join("::"));
                                    }
                                }
                            }
                        }
                        type_paths.sort();
                        if debug {
                            eprintln!("DEBUG assoc-path type_def_id={type_def_id:?} type_paths={type_paths:?}");
                        }
                        for type_path in type_paths {
                            let mut associated_path = parse_path(&type_path)
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
                    if canonical_res.is_none() && self.package.module_tree.module_exists(&canonical_path) {
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
            let root_modules = self.cached_root_modules();
            let extern_prelude: HashSet<String> = ["std", "core", "alloc"]
                .into_iter()
                .map(|name| name.to_string())
                .collect();
            let segment_names = segments
                .iter()
                .map(|seg| seg.name.as_str().to_string())
                .collect::<Vec<_>>();
            let parsed = ParsedPath {
                prefix: path_prefix,
                segments: segment_names,
            };
            let item_exists = |candidate: &QualifiedPath| {
                let key = candidate.to_key();
                match scope {
                    PathResolutionScope::Value => {
                        if self.global_value_defs.contains_key(&key) {
                            return true;
                        }
                    }
                    PathResolutionScope::Type => {
                        if self.global_type_defs.contains_key(&key) {
                            return true;
                        }
                    }
                }
                // Cross-package export (e.g. `libc::macos::getenv`),
                // looked up lazily against the workspace on a local-lookup
                // miss — see `lookup_global_res`'s identical fallback.
                self.workspace
                    .as_ref()
                    .is_some_and(|ws| ws.find_export(&key).is_some())
            };
            let scope_contains = |name: &str| match scope {
                PathResolutionScope::Value => self.resolve_value_symbol(name).is_some(),
                PathResolutionScope::Type => self.resolve_type_symbol(name).is_some(),
            };
            let module_defs: HashSet<QualifiedPath> =
                self.package.module_tree.all_paths().cloned().collect();
            if let Some(canonical) = resolve_item_path(
                &parsed,
                &self.module_path,
                &root_modules,
                &extern_prelude,
                &module_defs,
                item_exists,
                scope_contains,
            ) {
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
                    if canonical_res.is_none() && self.package.module_tree.module_exists(&canonical) {
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
                            return Ok(path);
                        }
                    }
                }
                self.name_to_hir_path_with_scope(name, scope)
            }
            ast::ExprKind::Select(select) => {
                let mut base = self.ast_expr_to_hir_path(&select.obj, scope)?;
                let seg = self.make_path_segment(&select.field.name, None);
                base.segments.push(seg);
                Ok(base)
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
                    ast::ExprInvokeTarget::Type(ty) => {
                        let type_expr = self.transform_type_to_hir(ty)?;
                        match type_expr.kind {
                            hir::TypeExprKind::Path(path) => path,
                            _ => {
                                self.add_error(
                                    Diagnostic::error(format!(
                                        "expected path-like expression for type path, found type target {:?}",
                                        ty
                                    ))
                                    .with_source_context(DIAGNOSTIC_CONTEXT)
                                    .with_span(expr.span()),
                                );
                                hir::Path {
                                    segments: vec![self.make_path_segment("__fp_error", None)],
                                    res: None,
                                }
                            }
                        }
                    }
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
