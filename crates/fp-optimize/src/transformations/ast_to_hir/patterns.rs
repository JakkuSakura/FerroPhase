use super::*;

impl HirGenerator {
    pub(super) fn transform_pattern_with_metadata(
        &mut self,
        pat: &Pattern,
    ) -> Result<(hir::Pat, Option<hir::TypeExpr>, bool)> {
        use fp_core::ast::PatternKind;
        match pat.kind() {
            PatternKind::Ident(ident) => {
                let mutable = ident.mutability.unwrap_or(false);
                let hir_pat = hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Binding {
                        name: ident.ident.clone().into(),
                        mutable,
                    },
                };
                Ok((hir_pat, None, mutable))
            }
            PatternKind::Wildcard(_) => Ok((
                hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Wild,
                },
                None,
                false,
            )),
            PatternKind::Type(pattern_type) => {
                let ty_expr = self.transform_type_to_hir(&pattern_type.ty)?;
                let (inner_pat, _inner_ty, mutable) =
                    self.transform_pattern_with_metadata(pattern_type.pat.as_ref())?;
                Ok((inner_pat, Some(ty_expr), mutable))
            }
            // Additional pattern kinds can be added here (Tuple, Struct, etc.)
            other => Err(crate::error::optimization_error(format!(
                "unsupported pattern kind in AST→HIR lowering: {:?}",
                other
            ))),
        }
    }

    pub(super) fn transform_pattern(&mut self, pat: &Pattern) -> Result<hir::Pat> {
        self.transform_pattern_with_metadata(pat).map(|(p, _, _)| p)
    }

    pub(super) fn register_pattern_bindings(&mut self, pat: &hir::Pat) {
        match &pat.kind {
            hir::PatKind::Binding { name, .. } => {
                self.register_value_local(name.as_str(), pat.hir_id);
            }
            hir::PatKind::Struct(_, fields) => {
                for field in fields {
                    self.register_pattern_bindings(&field.pat);
                }
            }
            hir::PatKind::Tuple(elements) => {
                for element in elements {
                    self.register_pattern_bindings(element);
                }
            }
            _ => {}
        }
    }
}
