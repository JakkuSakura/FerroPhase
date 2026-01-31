use super::*;
use fp_core::module::path::{parse_segments, resolve_item_path};

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
        locator: &Locator,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        // Build segments from the locator
        let segments = match locator {
            Locator::Ident(ident) => vec![self.make_path_segment(&ident.name, None)],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|seg| self.make_path_segment(&seg.name, None))
                .collect(),
            Locator::ParameterPath(param_path) => {
                let mut segs = Vec::new();
                for seg in &param_path.segments {
                    let args = if seg.args.is_empty() {
                        None
                    } else {
                        Some(self.convert_generic_args(&seg.args)?)
                    };
                    segs.push(self.make_path_segment(&seg.ident.name, args));
                }
                segs
            }
        };

        let mut resolved = None;

        if segments.len() == 1 {
            resolved = segments.last().and_then(|segment| match scope {
                PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
                PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
            });
        }

        if segments.len() > 1 {
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
                    let mut canonical_res = self.lookup_global_res(&canonical, scope);
                    if canonical_res.is_none() && self.module_defs.contains(&canonical) {
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
                if let Some(first) = path.first() {
                    root_modules.insert(first.clone());
                }
            }
            for key in self.global_type_defs.keys().chain(self.global_value_defs.keys()) {
                if let Some((head, _)) = key.split_once("::") {
                    root_modules.insert(head.to_string());
                }
            }
            let extern_prelude = HashSet::new();
            let segment_names = self.canonicalize_segments(&segments);
            if let Ok(parsed) = parse_segments(&segment_names) {
                let item_exists = |candidate: &[String]| {
                    let key = candidate.join("::");
                    match scope {
                        PathResolutionScope::Value => {
                            if self.global_value_defs.contains_key(&key) {
                                return true;
                            }
                            if candidate.len() > 1 {
                                let parent = candidate[..candidate.len() - 1].join("::");
                                if self.global_type_defs.contains_key(&parent) {
                                    return true;
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
                if let Some(canonical) = resolve_item_path(
                    &parsed,
                    &self.module_path,
                    &root_modules,
                    &extern_prelude,
                    &self.module_defs,
                    item_exists,
                ) {
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
                    let mut canonical_res = self.lookup_global_res(&canonical, scope);
                    if canonical_res.is_none() && self.module_defs.contains(&canonical) {
                        canonical_res = Some(hir::Res::Module(canonical.clone()));
                    }
                    return Ok(hir::Path {
                        segments: canonical_segments,
                        res: canonical_res.or(resolved),
                    });
                }
            }
        }

        if resolved.is_none() {
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
                    resolved = self.lookup_global_res(&canonical, scope);
                    if resolved.is_none() && segments.len() == 1 {
                        resolved = Some(hir::Res::Module(canonical));
                    }
                }
            }
        }

        if resolved.is_none() {
            let canonical = self.canonicalize_segments(&segments);
            resolved = self.lookup_global_res(&canonical, scope);
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
            ast::ExprKind::Locator(locator) => self.locator_to_hir_path_with_scope(locator, scope),
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

    pub(super) fn canonicalize_segments(&self, segments: &[hir::PathSegment]) -> Vec<String> {
        segments
            .iter()
            .map(|s| s.name.as_str().to_string())
            .collect()
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
