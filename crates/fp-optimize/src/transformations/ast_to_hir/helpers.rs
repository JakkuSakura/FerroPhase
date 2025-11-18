use super::*;

impl HirGenerator {
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

        // Try resolve against current scope using the last segment
        let mut resolved = segments.last().and_then(|segment| match scope {
            PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
            PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
        });

        // Fallback to global symbol tables based on canonicalized segments
        if resolved.is_none() {
            let canonical = self.canonicalize_segments(&segments);
            resolved = self.lookup_global_res(&canonical, scope);
        }

        Ok(hir::Path { segments, res: resolved })
    }

    pub(super) fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        match expr.kind() {
            ast::ExprKind::Locator(locator) =>
                self.locator_to_hir_path_with_scope(locator, scope),
            ast::ExprKind::Select(select) => {
                let mut base = self.ast_expr_to_hir_path(&select.obj, scope)?;
                let seg = self.make_path_segment(&select.field.name, None);
                base.segments.push(seg);
                Ok(base)
            }
            other => Err(crate::error::optimization_error(format!(
                "expected path-like expression for type path, found {:?}",
                other
            )))
        }
    }

    pub(super) fn canonicalize_segments(&self, segments: &[hir::PathSegment]) -> Vec<String> {
        segments.iter().map(|s| s.name.as_str().to_string()).collect()
    }

    pub(super) fn make_path_segment(
        &self,
        name: &str,
        args: Option<hir::GenericArgs>,
    ) -> hir::PathSegment {
        hir::PathSegment { name: hir::Symbol::new(name), args }
    }
}
