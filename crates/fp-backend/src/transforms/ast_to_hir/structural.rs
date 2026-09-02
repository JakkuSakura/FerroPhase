use super::*;

impl AstToHirLowerer {
    pub(super) fn struct_fields_from_type(
        &mut self,
        ty: &ast::Ty,
        span: Span,
    ) -> Result<Vec<ast::StructuralField>> {
        match ty {
            ast::Ty::Structural(structural) => Ok(structural.fields.clone()),
            ast::Ty::Struct(struct_ty) => Ok(struct_ty.fields.clone()),
            ast::Ty::TypeBinaryOp(op) => {
                let lhs = self.struct_fields_from_type(&op.lhs, span)?;
                let rhs = self.struct_fields_from_type(&op.rhs, span)?;
                match op.kind {
                    ast::TypeBinaryOpKind::Add => self.merge_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Intersect => self.intersect_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Subtract => self.subtract_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Union => {
                        self.add_error(
                            Diagnostic::error(
                                "struct update does not support union type operands".to_string(),
                            )
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(span),
                        );
                        Ok(Vec::new())
                    }
                }
            }
            ast::Ty::Expr(expr) => {
                if let ast::ExprKind::Name(name) = expr.kind() {
                    let path = name.to_path();
                    let qualified = fp_core::ast::path::InPackagePath::new(
                        path.segments.iter().map(|seg| seg.name.clone()).collect(),
                    );
                    if let fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(def_id)) =
                        self.hir_program.resolve_module_path_final(
                            &self.package_id,
                            &self.module_path,
                            &qualified,
                            fp_core::hir::resolve::Namespace::Type,
                        )
                    {
                        if let Some(fields) = self.struct_field_defs.get(&def_id).cloned() {
                            return Ok(fields);
                        }
                    }
                }
                self.add_error(
                    Diagnostic::error(
                        "struct update requires a resolved struct definition".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(Vec::new())
            }
            _ => {
                self.add_error(
                    Diagnostic::error(
                        "struct update requires a resolved struct definition".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(Vec::new())
            }
        }
    }

    pub(super) fn merge_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let mut result = Vec::new();
        let mut seen = HashMap::new();
        for field in lhs {
            seen.insert(field.name.name.clone(), field.value.clone());
            result.push(field);
        }
        for field in rhs {
            if let Some(existing) = seen.get(&field.name.name) {
                if existing != &field.value {
                    self.add_error(
                        Diagnostic::error(format!(
                            "conflicting field types for `{}` in structural merge",
                            field.name.name
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(Span::union([field.value.span(), existing.span()])),
                    );
                    continue;
                }
                continue;
            }
            seen.insert(field.name.name.clone(), field.value.clone());
            result.push(field);
        }
        Ok(result)
    }

    pub(super) fn intersect_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let mut rhs_map = HashMap::new();
        for field in rhs {
            rhs_map.insert(field.name.name.clone(), field.value);
        }
        let mut result = Vec::new();
        for field in lhs {
            if let Some(rhs_ty) = rhs_map.get(&field.name.name) {
                if rhs_ty != &field.value {
                    self.add_error(
                        Diagnostic::error(format!(
                            "conflicting field types for `{}` in structural intersect",
                            field.name.name
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(Span::union([field.value.span(), rhs_ty.span()])),
                    );
                    continue;
                }
                result.push(field);
            }
        }
        Ok(result)
    }

    pub(super) fn subtract_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let rhs_names = rhs
            .into_iter()
            .map(|field| field.name.name)
            .collect::<HashSet<_>>();
        Ok(lhs
            .into_iter()
            .filter(|field| !rhs_names.contains(&field.name.name))
            .collect())
    }
}
