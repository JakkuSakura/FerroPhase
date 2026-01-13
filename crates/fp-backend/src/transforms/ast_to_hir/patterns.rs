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
            PatternKind::Bind(bind) => {
                let mutable = bind.ident.mutability.unwrap_or(false);
                let hir_pat = hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Binding {
                        name: bind.ident.ident.clone().into(),
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
            PatternKind::Quote(_) | PatternKind::QuotePlural(_) => Ok((
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
            PatternKind::Tuple(tuple) => {
                let parts = tuple
                    .patterns
                    .iter()
                    .map(|pat| self.transform_pattern(pat))
                    .collect::<Result<Vec<_>>>()?;
                Ok((
                    hir::Pat {
                        hir_id: self.next_id(),
                        kind: hir::PatKind::Tuple(parts),
                    },
                    None,
                    false,
                ))
            }
            PatternKind::TupleStruct(tuple_struct) => {
                let path = self.locator_to_hir_path_with_scope(
                    &tuple_struct.name,
                    PathResolutionScope::Value,
                )?;
                let parts = tuple_struct
                    .patterns
                    .iter()
                    .map(|pat| self.transform_pattern(pat))
                    .collect::<Result<Vec<_>>>()?;
                Ok((
                    hir::Pat {
                        hir_id: self.next_id(),
                        kind: hir::PatKind::TupleStruct(path, parts),
                    },
                    None,
                    false,
                ))
            }
            PatternKind::Box(box_pat) => {
                let inner = self.transform_pattern(box_pat.pattern.as_ref())?;
                Ok((inner, None, false))
            }
            PatternKind::Struct(struct_pat) => {
                let path = self.locator_to_hir_path_with_scope(
                    &Locator::Ident(struct_pat.name.clone()),
                    PathResolutionScope::Type,
                )?;
                let fields = struct_pat
                    .fields
                    .iter()
                    .map(|field| {
                        let pat = if let Some(rename) = field.rename.as_ref() {
                            self.transform_pattern(rename.as_ref())?
                        } else {
                            hir::Pat {
                                hir_id: self.next_id(),
                                kind: hir::PatKind::Binding {
                                    name: field.name.clone().into(),
                                    mutable: false,
                                },
                            }
                        };
                        Ok(hir::PatField {
                            hir_id: self.next_id(),
                            name: field.name.clone().into(),
                            pat,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok((
                    hir::Pat {
                        hir_id: self.next_id(),
                        kind: hir::PatKind::Struct(path, fields, struct_pat.has_rest),
                    },
                    None,
                    false,
                ))
            }
            PatternKind::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        let pat = if let Some(rename) = field.rename.as_ref() {
                            self.transform_pattern(rename.as_ref())?
                        } else {
                            hir::Pat {
                                hir_id: self.next_id(),
                                kind: hir::PatKind::Binding {
                                    name: field.name.clone().into(),
                                    mutable: false,
                                },
                            }
                        };
                        Ok(hir::PatField {
                            hir_id: self.next_id(),
                            name: field.name.clone().into(),
                            pat,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok((
                    hir::Pat {
                        hir_id: self.next_id(),
                        kind: hir::PatKind::Struct(
                            hir::Path {
                                segments: Vec::new(),
                                res: None,
                            },
                            fields,
                            structural.has_rest,
                        ),
                    },
                    None,
                    false,
                ))
            }
            PatternKind::Variant(variant) => {
                let (hir_pat, ty, mutable) =
                    self.transform_variant_pattern(&variant.name, variant.pattern.as_deref())?;
                Ok((hir_pat, ty, mutable))
            }
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
            hir::PatKind::Struct(_, fields, _) => {
                for field in fields {
                    self.register_pattern_bindings(&field.pat);
                }
            }
            hir::PatKind::TupleStruct(_, parts) => {
                for part in parts {
                    self.register_pattern_bindings(part);
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

    fn transform_variant_pattern(
        &mut self,
        name: &ast::Expr,
        nested: Option<&Pattern>,
    ) -> Result<(hir::Pat, Option<hir::TypeExpr>, bool)> {
        use ast::ExprKind as AstExprKind;

        if let AstExprKind::Value(value) = name.kind() {
            if let Some(lit) = self.value_to_literal(value) {
                return Ok((
                    hir::Pat {
                        hir_id: self.next_id(),
                        kind: hir::PatKind::Lit(lit),
                    },
                    None,
                    false,
                ));
            }
        }

        let path = match name.kind() {
            AstExprKind::Locator(locator) => {
                self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "unsupported variant pattern expression",
                ));
            }
        };

        if let Some(pattern) = nested {
            match pattern.kind() {
                ast::PatternKind::Structural(structural) => {
                    let fields = structural
                        .fields
                        .iter()
                        .map(|field| {
                            let pat = if let Some(rename) = field.rename.as_ref() {
                                self.transform_pattern(rename.as_ref())?
                            } else {
                                hir::Pat {
                                    hir_id: self.next_id(),
                                    kind: hir::PatKind::Binding {
                                        name: field.name.clone().into(),
                                        mutable: false,
                                    },
                                }
                            };
                            Ok(hir::PatField {
                                hir_id: self.next_id(),
                                name: field.name.clone().into(),
                                pat,
                            })
                        })
                        .collect::<Result<Vec<_>>>()?;
                    return Ok((
                        hir::Pat {
                            hir_id: self.next_id(),
                            kind: hir::PatKind::Struct(path, fields, structural.has_rest),
                        },
                        None,
                        false,
                    ));
                }
                ast::PatternKind::Tuple(tuple) => {
                    let parts = tuple
                        .patterns
                        .iter()
                        .map(|pat| self.transform_pattern(pat))
                        .collect::<Result<Vec<_>>>()?;
                    return Ok((
                        hir::Pat {
                            hir_id: self.next_id(),
                            kind: hir::PatKind::TupleStruct(path, parts),
                        },
                        None,
                        false,
                    ));
                }
                _ => {}
            }
        }

        Ok((
            hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Variant(path),
            },
            None,
            false,
        ))
    }

    fn value_to_literal(&self, value: &ast::BValue) -> Option<hir::Lit> {
        match value.as_ref() {
            ast::Value::Int(int_val) => Some(hir::Lit::Integer(int_val.value)),
            ast::Value::Bool(bool_val) => Some(hir::Lit::Bool(bool_val.value)),
            ast::Value::Decimal(decimal_val) => Some(hir::Lit::Float(decimal_val.value)),
            ast::Value::String(string_val) => Some(hir::Lit::Str(string_val.value.clone())),
            ast::Value::Char(char_val) => Some(hir::Lit::Char(char_val.value)),
            ast::Value::Null(_) | ast::Value::None(_) => Some(hir::Lit::Null),
            _ => None,
        }
    }
}
