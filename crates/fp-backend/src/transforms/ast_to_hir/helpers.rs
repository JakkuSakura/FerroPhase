use super::*;
use fp_core::module::path::{parse_path, resolve_item_path, ParsedPath, PathPrefix, QualifiedPath};
use fp_core::module::resolution::resolve_symbol_path;

impl HirGenerator {
    pub(super) fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    pub(super) fn locator_to_hir_path_with_scope(
        &mut self,
        locator: &Name,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        // Build segments from the locator.
        let (mut segments, mut path_prefix) = match locator {
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
            } else if first == "self" {
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

        if segments.len() > 1 && path_prefix == PathPrefix::Plain {
            if let Some(first) = segments.first() {
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
                        res: canonical_res.or(resolved),
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
            let extern_prelude = HashSet::new();
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
                        if candidate.segments.len() > 1 {
                            if let Some(parent) = candidate.parent_n(1) {
                                if self.global_type_defs.contains_key(&parent.to_key()) {
                                    return true;
                                }
                            }
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
            if let Some(ctx) = &self.module_resolution {
                if let Some(qualified) = resolve_symbol_path(
                    &parsed,
                    ctx,
                    scope_contains,
                    |candidate| {
                        let key = candidate.path.to_key();
                        match scope {
                            PathResolutionScope::Value => {
                                if self.global_value_defs.contains_key(&key) {
                                    return true;
                                }
                                if candidate.path.segments.len() > 1 {
                                    if let Some(parent) = candidate.path.parent_n(1) {
                                        if self.global_type_defs.contains_key(&parent.to_key()) {
                                            return true;
                                        }
                                    }
                                }
                            }
                            PathResolutionScope::Type => {
                                if self.global_type_defs.contains_key(&key) {
                                    return true;
                                }
                            }
                        }
                        false
                    },
                ) {
                    let canonical = qualified.path;
                    let mut canonical_segments =
                        Vec::with_capacity(canonical.segments.len());
                    let offset =
                        canonical.segments.len().saturating_sub(segments.len());
                    for (idx, seg) in canonical.segments.iter().enumerate() {
                        let args = if idx >= offset {
                            segments[idx - offset].args.clone()
                        } else {
                            None
                        };
                        canonical_segments.push(self.make_path_segment(seg, args));
                    }
                    let mut canonical_res =
                        self.lookup_global_res(&canonical, scope);
                    if canonical_res.is_none() && self.module_defs.contains(&canonical) {
                        canonical_res = Some(hir::Res::Module(canonical.segments.clone()));
                    }
                    return Ok(hir::Path {
                        segments: canonical_segments,
                        res: canonical_res.or(resolved),
                    });
                }
            }
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
                    res: canonical_res.or(resolved),
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
                    let keep = self.module_path.segments.len().saturating_sub(depth as usize);
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
            ast::ExprKind::Name(locator) => self.locator_to_hir_path_with_scope(locator, scope),
            ast::ExprKind::Select(select) => {
                let mut base = self.ast_expr_to_hir_path(&select.obj, scope)?;
                let seg = self.make_path_segment(&select.field.name, None);
                base.segments.push(seg);
                Ok(base)
            }
            ast::ExprKind::Invoke(invoke) => {
                let mut base = match &invoke.target {
                    ast::ExprInvokeTarget::Function(locator) => {
                        self.locator_to_hir_path_with_scope(locator, scope)?
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
