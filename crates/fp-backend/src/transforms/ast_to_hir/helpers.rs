use super::*;
use fp_core::module::path::{parse_path, resolve_item_path, ParsedPath, PathPrefix, QualifiedPath};

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
        if res.is_none() && self.module_defs.contains(&resolved_name.path) {
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
            if path_prefix == PathPrefix::Plain || path_prefix == PathPrefix::SelfMod {
                resolved = segments.last().and_then(|segment| match scope {
                    PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                    PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
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
        // global canonicalization.
        if segments.len() == 1
            && matches!(
                resolved,
                Some(hir::Res::Def(_)) | Some(hir::Res::Local(_)) | Some(hir::Res::SelfTy)
            )
        {
            return Ok(hir::Path {
                segments,
                res: resolved,
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
            if let Some(first) = segments.first() {
                if scope == PathResolutionScope::Value {
                    if let Some(hir::Res::Def(type_def_id)) =
                        self.resolve_type_symbol(first.name.as_str())
                    {
                        let mut type_paths: Vec<_> = self
                            .global_type_defs
                            .iter()
                            .filter(|(_, entry)| entry.res == hir::Res::Def(type_def_id))
                            .map(|(path, _)| path)
                            .filter(|path| path.ends_with(&format!("::{}", first.name)))
                            .cloned()
                            .collect();
                        type_paths.sort();
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
                    if canonical_res.is_none() && self.module_defs.contains(&canonical_path) {
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
            let mut root_modules = HashSet::new();
            for path in &self.module_defs {
                if let Some(first) = path.head() {
                    root_modules.insert(first.to_string());
                }
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
                false
            };
            let scope_contains = |name: &str| match scope {
                PathResolutionScope::Value => self.resolve_value_symbol(name).is_some(),
                PathResolutionScope::Type => self.resolve_type_symbol(name).is_some(),
            };
            if let Some(canonical) = resolve_item_path(
                &parsed,
                &self.module_path,
                &root_modules,
                &extern_prelude,
                &self.module_defs,
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
                let mut canonical_res = self.lookup_global_res(&canonical, scope);
                if canonical_res.is_none() && self.module_defs.contains(&canonical) {
                    canonical_res = Some(hir::Res::Module(canonical.segments.clone()));
                }
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
            let mut relative_segments = match path_prefix {
                PathPrefix::Root | PathPrefix::Crate => Vec::new(),
                PathPrefix::SelfMod => self.module_path.segments.clone(),
                PathPrefix::Super(depth) => {
                    let keep = self
                        .module_path
                        .segments
                        .len()
                        .saturating_sub(depth as usize);
                    self.module_path.segments[..keep].to_vec()
                }
                PathPrefix::Plain => Vec::new(),
            };
            relative_segments.extend(segments.iter().map(|seg| seg.name.as_str().to_string()));
            let relative = QualifiedPath::new(relative_segments);
            resolved = self.lookup_global_res(&relative, scope);
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
