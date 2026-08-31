use super::body::BodyBuilder;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{Ty, TyKind};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn lower_match_condition(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Result<mir::Operand> {
        if let hir::PatKind::Tuple(items) = &pat.kind {
            let mut tuple_place = scrutinee_place.clone();
            let mut tuple_ty = scrutinee_ty.clone();
            if matches!(tuple_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                tuple_place.projection.push(mir::PlaceElem::Deref);
                tuple_ty = match &tuple_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => tuple_ty,
                };
            }

            let TyKind::Tuple(elem_tys) = &tuple_ty.kind else {
                self.lowering.emit_warning(
                    span,
                    "tuple pattern match requires tuple scrutinee; treating as non-matching",
                );
                return Ok(self.constant_bool_operand(false, span).operand);
            };

            if items.len() != elem_tys.len() {
                self.lowering.emit_warning(
                    span,
                    "tuple pattern length mismatch; treating as non-matching",
                );
                return Ok(self.constant_bool_operand(false, span).operand);
            }

            let mut combined: Option<mir::Operand> = None;
            for (index, item) in items.iter().enumerate() {
                match &item.kind {
                    hir::PatKind::Lit(lit) => {
                        let (literal, ty) = self.lower_literal(lit, None);
                        let mut field_place = tuple_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(index, (*elem_tys[index]).clone()));
                        let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                        let eq_place = mir::Place::from_local(eq_temp);
                        self.push_statement(mir::Statement {
                            source_info: span,
                            kind: mir::StatementKind::Assign(
                                eq_place.clone(),
                                mir::Rvalue::BinaryOp(
                                    mir::BinOp::Eq,
                                    mir::Operand::Copy(field_place),
                                    mir::Operand::Constant(mir::Constant {
                                        span,
                                        ty,
                                        user_ty: None,
                                        literal,
                                    }),
                                ),
                            ),
                        });
                        let eq_operand = mir::Operand::Copy(eq_place);
                        combined = Some(match combined {
                            None => eq_operand,
                            Some(existing) => {
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            existing,
                                            eq_operand,
                                        ),
                                    ),
                                });
                                mir::Operand::Copy(and_place)
                            }
                        });
                    }
                    hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                    _ => {
                        self.lowering.emit_warning(
                            span,
                            "tuple pattern element not supported; treating as non-matching",
                        );
                        return Ok(self.constant_bool_operand(false, span).operand);
                    }
                }
            }

            return Ok(combined.unwrap_or_else(|| self.constant_bool_operand(true, span).operand));
        }
        if let hir::PatKind::Struct(path, fields, _) = &pat.kind {
            if let Some(variant) = self.enum_variant_info_from_path(path) {
                let layout = self
                    .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                    .or_else(|| {
                        self.lowering
                            .enum_layout_for_def(variant.enum_def.clone(), span)
                    });
                if let Some(layout) = layout {
                    let mut base_place = scrutinee_place.clone();
                    if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                        base_place.projection.push(mir::PlaceElem::Deref);
                    }

                    let mut tag_place = base_place.clone();
                    // Every enum has a canonical tag field, including a
                    // fieldless enum.  `payload_tys` describes only the
                    // optional payload slots and is empty for a C-like enum;
                    // it must not make the whole `{ tag }` value participate
                    // in a scalar discriminant comparison.
                    tag_place
                        .projection
                        .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                    let tag_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                    let tag_place_out = mir::Place::from_local(tag_temp);
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            tag_place_out.clone(),
                            mir::Rvalue::BinaryOp(
                                mir::BinOp::Eq,
                                mir::Operand::Copy(tag_place),
                                mir::Operand::Constant(mir::Constant {
                                    span,
                                    ty: layout.tag_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Int(variant.discriminant),
                                }),
                            ),
                        ),
                    });
                    let mut combined = mir::Operand::Copy(tag_place_out);

                    let payload_tys = self.variant_payloads_from_layout_or_ty(
                        &layout,
                        &variant,
                        scrutinee_ty,
                        span,
                    );
                    for (idx, field) in fields.iter().enumerate() {
                        if idx >= payload_tys.len() {
                            break;
                        }
                        match &field.pat.kind {
                            hir::PatKind::Lit(lit) => {
                                let (literal, ty) = self.lower_literal(lit, None);
                                let field_ty = payload_tys[idx].clone();
                                let mut field_place = base_place.clone();
                                field_place
                                    .projection
                                    .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                                let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let eq_place = mir::Place::from_local(eq_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        eq_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::Eq,
                                            mir::Operand::Copy(field_place),
                                            mir::Operand::Constant(mir::Constant {
                                                span,
                                                ty,
                                                user_ty: None,
                                                literal,
                                            }),
                                        ),
                                    ),
                                });
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            combined,
                                            mir::Operand::Copy(eq_place),
                                        ),
                                    ),
                                });
                                combined = mir::Operand::Copy(and_place);
                            }
                            hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                            _ => {
                                self.lowering.emit_warning(
                                    span,
                                    "enum struct pattern field not supported; ignoring",
                                );
                            }
                        }
                    }

                    return Ok(combined);
                }
            }

            let mut base_place = scrutinee_place.clone();
            let mut base_ty = scrutinee_ty.clone();
            if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                base_place.projection.push(mir::PlaceElem::Deref);
                base_ty = match &base_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => base_ty,
                };
            }
            if let Some(struct_def) = self.struct_def_from_ty(&base_ty) {
                let mut combined: Option<mir::Operand> = None;
                for field in fields {
                    match &field.pat.kind {
                        hir::PatKind::Lit(lit) => {
                            let Some((field_index, field_info)) = self.lowering.struct_field(
                                struct_def.clone(),
                                &base_ty,
                                field.name.as_str(),
                                span,
                            ) else {
                                self.lowering.emit_warning(
                                    span,
                                    format!(
                                        "struct pattern field `{}` not found; treating as non-matching",
                                        field.name
                                    ),
                                );
                                return Ok(self.constant_bool_operand(false, span).operand);
                            };
                            let (literal, ty) = self.lower_literal(lit, None);
                            let mut field_place = base_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                            let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                            let eq_place = mir::Place::from_local(eq_temp);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    eq_place.clone(),
                                    mir::Rvalue::BinaryOp(
                                        mir::BinOp::Eq,
                                        mir::Operand::Copy(field_place),
                                        mir::Operand::Constant(mir::Constant {
                                            span,
                                            ty,
                                            user_ty: None,
                                            literal,
                                        }),
                                    ),
                                ),
                            });
                            let eq_operand = mir::Operand::Copy(eq_place);
                            combined = Some(match combined {
                                None => eq_operand,
                                Some(existing) => {
                                    let and_temp =
                                        self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                    let and_place = mir::Place::from_local(and_temp);
                                    self.push_statement(mir::Statement {
                                        source_info: span,
                                        kind: mir::StatementKind::Assign(
                                            and_place.clone(),
                                            mir::Rvalue::BinaryOp(
                                                mir::BinOp::And,
                                                existing,
                                                eq_operand,
                                            ),
                                        ),
                                    });
                                    mir::Operand::Copy(and_place)
                                }
                            });
                        }
                        hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                        _ => {
                            self.lowering.emit_warning(
                                span,
                                "struct pattern field not supported; treating as non-matching",
                            );
                            return Ok(self.constant_bool_operand(false, span).operand);
                        }
                    }
                }

                return Ok(
                    combined.unwrap_or_else(|| self.constant_bool_operand(true, span).operand)
                );
            }
        }

        if let hir::PatKind::TupleStruct(path, parts) = &pat.kind {
            if let Some(variant) = self.enum_variant_info_from_path(path) {
                let layout = self
                    .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                    .or_else(|| {
                        self.lowering
                            .enum_layout_for_def(variant.enum_def.clone(), span)
                    });
                if let Some(layout) = layout {
                    let mut base_place = scrutinee_place.clone();
                    if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                        base_place.projection.push(mir::PlaceElem::Deref);
                    }

                    let mut tag_place = base_place.clone();
                    tag_place
                        .projection
                        .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                    let tag_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                    let tag_place_out = mir::Place::from_local(tag_temp);
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            tag_place_out.clone(),
                            mir::Rvalue::BinaryOp(
                                mir::BinOp::Eq,
                                mir::Operand::Copy(tag_place),
                                mir::Operand::Constant(mir::Constant {
                                    span,
                                    ty: layout.tag_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Int(variant.discriminant),
                                }),
                            ),
                        ),
                    });
                    let mut combined = mir::Operand::Copy(tag_place_out);

                    let payload_tys = self.variant_payloads_from_layout_or_ty(
                        &layout,
                        &variant,
                        scrutinee_ty,
                        span,
                    );
                    for (idx, part) in parts.iter().enumerate() {
                        if idx >= payload_tys.len() {
                            break;
                        }
                        match &part.kind {
                            hir::PatKind::Lit(lit) => {
                                let (literal, ty) = self.lower_literal(lit, None);
                                let field_ty = payload_tys[idx].clone();
                                let mut field_place = base_place.clone();
                                field_place
                                    .projection
                                    .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                                let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let eq_place = mir::Place::from_local(eq_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        eq_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::Eq,
                                            mir::Operand::Copy(field_place),
                                            mir::Operand::Constant(mir::Constant {
                                                span,
                                                ty,
                                                user_ty: None,
                                                literal,
                                            }),
                                        ),
                                    ),
                                });
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            combined,
                                            mir::Operand::Copy(eq_place),
                                        ),
                                    ),
                                });
                                combined = mir::Operand::Copy(and_place);
                            }
                            hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                            _ => {
                                self.lowering.emit_warning(
                                    span,
                                    "tuple-struct pattern element not supported; ignoring",
                                );
                            }
                        }
                    }

                    return Ok(combined);
                }
            }
        }

        let literal = match &pat.kind {
            hir::PatKind::Lit(lit) => {
                let (literal, ty) = self.lower_literal(lit, None);
                mir::Operand::Constant(mir::Constant {
                    span,
                    ty,
                    user_ty: None,
                    literal,
                })
            }
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => {
                if let Some(variant) = self.enum_variant_info_from_path(path) {
                    let tag_ty = self
                        .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                        .or_else(|| {
                            self.lowering
                                .enum_layout_for_def(variant.enum_def.clone(), span)
                        })
                        .ok_or_else(|| {
                            crate::error::optimization_error(
                                "enum pattern has no concrete MIR layout",
                            )
                        })?
                        .tag_ty;
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: tag_ty,
                        user_ty: None,
                        literal: mir::ConstantKind::Int(variant.discriminant),
                    })
                } else {
                    let expr = hir::Expr {
                        hir_id: pat.hir_id.clone(),
                        kind: hir::ExprKind::Path(path.clone()),
                        span,
                    };
                    let operand = self.lower_operand(&expr, None)?;
                    operand.operand
                }
            }
            _ => {
                self.lowering.emit_warning(
                    span,
                    "unsupported pattern in match condition; treating as non-matching",
                );
                self.constant_bool_operand(false, span).operand
            }
        };

        let scrutinee_operand = if matches!(
            pat.kind,
            hir::PatKind::Variant(_)
                | hir::PatKind::Struct(_, _, _)
                | hir::PatKind::TupleStruct(_, _)
        ) {
            let layout = match &pat.kind {
                hir::PatKind::Variant(path)
                | hir::PatKind::Struct(path, _, _)
                | hir::PatKind::TupleStruct(path, _) => self
                    .enum_variant_info_from_path(path)
                    .and_then(|variant| {
                        self.enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                            .or_else(|| {
                                self.lowering
                                    .enum_layout_for_def(variant.enum_def.clone(), span)
                            })
                    })
                    .or_else(|| self.enum_layout_for_ty(scrutinee_ty, span)),
                _ => self.enum_layout_for_ty(scrutinee_ty, span),
            };
            if let Some(layout) = layout {
                let mut tag_place = scrutinee_place.clone();
                if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                    tag_place.projection.push(mir::PlaceElem::Deref);
                }
                tag_place
                    .projection
                    .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                mir::Operand::Copy(tag_place)
            } else {
                mir::Operand::Copy(scrutinee_place.clone())
            }
        } else {
            mir::Operand::Copy(scrutinee_place.clone())
        };

        let temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
        let place = mir::Place::from_local(temp);
        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::BinaryOp(mir::BinOp::Eq, scrutinee_operand, literal),
            ),
        };
        self.push_statement(assign);
        Ok(mir::Operand::Copy(place))
    }

    pub(super) fn bind_match_pattern(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        if let hir::PatKind::Tuple(parts) = &pat.kind {
            let mut base_place = scrutinee_place.clone();
            let mut base_ty = scrutinee_ty.clone();
            if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                base_place.projection.push(mir::PlaceElem::Deref);
                base_ty = match &base_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => base_ty,
                };
            }
            if let TyKind::Tuple(elem_tys) = &base_ty.kind {
                if parts.len() == elem_tys.len() {
                    for (idx, part) in parts.iter().enumerate() {
                        let field_ty = (*elem_tys[idx]).clone();
                        let mut field_place = base_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(idx, field_ty.clone()));
                        self.bind_match_pattern(part, &field_place, &field_ty, span);
                    }
                    return;
                }
            }
        }
        if let hir::PatKind::Struct(path, fields, _) = &pat.kind {
            if self.enum_variant_info_from_path(path).is_none() {
                let mut base_place = scrutinee_place.clone();
                let mut base_ty = scrutinee_ty.clone();
                if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                    base_place.projection.push(mir::PlaceElem::Deref);
                    base_ty = match &base_ty.kind {
                        TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                        TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                        _ => base_ty,
                    };
                }
                if let Some(def_id) = self.struct_def_from_ty(&base_ty) {
                    for field in fields {
                        let Some((field_index, field_info)) = self.lowering.struct_field(
                            def_id.clone(),
                            &base_ty,
                            field.name.as_str(),
                            span,
                        ) else {
                            continue;
                        };
                        let mut field_place = base_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                        self.bind_match_pattern(&field.pat, &field_place, &field_info.ty, span);
                    }
                    return;
                }
            }
        }
        let layout = match &pat.kind {
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => self
                .enum_variant_info_from_path(path)
                .and_then(|variant| {
                    self.enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                        .or_else(|| {
                            self.lowering
                                .enum_layout_for_def(variant.enum_def.clone(), span)
                        })
                })
                .or_else(|| self.enum_layout_for_ty(scrutinee_ty, span)),
            _ => self.enum_layout_for_ty(scrutinee_ty, span),
        };
        if let Some(layout) = layout {
            let mut scrutinee_place = scrutinee_place.clone();
            if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                scrutinee_place.projection.push(mir::PlaceElem::Deref);
            }
            match &pat.kind {
                hir::PatKind::Variant(path) => {
                    if self.enum_variant_info_from_path(path).is_some() {
                        return;
                    }
                }
                hir::PatKind::TupleStruct(path, parts) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = self.variant_payloads_from_layout_or_ty(
                            &layout,
                            &variant,
                            scrutinee_ty,
                            span,
                        );
                        for (idx, part) in parts.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                            // The projection above keeps the flattened
                            // struct shape (matching actual tuple-based
                            // storage), but the *bound local*'s declared
                            // type should be nominal when this payload is a
                            // registered struct — otherwise callers like
                            // `real_indexable_struct_def_id` can't
                            // recognize e.g. a `Vec<Field>` match-bound
                            // payload as indexable.
                            let bound_ty = self.lowering.nominalize_struct_ty(field_ty);
                            self.bind_match_pattern(part, &field_place, &bound_ty, span);
                        }
                        return;
                    }
                }
                hir::PatKind::Struct(path, fields, _) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = self.variant_payloads_from_layout_or_ty(
                            &layout,
                            &variant,
                            scrutinee_ty,
                            span,
                        );
                        for (idx, field) in fields.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                            let bound_ty = self.lowering.nominalize_struct_ty(field_ty);
                            self.bind_match_pattern(&field.pat, &field_place, &bound_ty, span);
                        }
                        return;
                    }
                }
                _ => {}
            }
        } else if let TyKind::Tuple(fields) = &scrutinee_ty.kind {
            // A generic enum's payload is sometimes represented, by this
            // point, as a plain `(discriminant, ...payload)` tuple rather
            // than a `TyKind::Adt` the layout lookup above can recognize
            // (e.g. inside a monomorphized generic method body, where the
            // scrutinee's registered local type is already the flattened
            // tuple form) — `enum_layout_for_variant_ty`/`enum_layout_for_ty`
            // only match `Ref`/`RawPtr`/`Adt`/`Opaque`, so `layout` above is
            // `None` even though the pattern genuinely is an enum-variant
            // destructure. Falling through to the generic tuple-pattern
            // case below would incorrectly bind each part to the *whole*
            // enum value/type instead of projecting into its payload
            // field — extract payload types directly from the tuple shape
            // instead (field 0 is always the discriminant; this mirrors
            // `variant_payloads_from_layout_or_ty`'s own `TyKind::Tuple`
            // fallback for exactly this situation).
            match &pat.kind {
                hir::PatKind::TupleStruct(path, parts)
                    if self.enum_variant_info_from_path(path).is_some() =>
                {
                    let variant = self
                        .enum_variant_info_from_path(path)
                        .expect("checked above");
                    let substituted_payloads = self.payload_types_from_type_substs(&variant, span);
                    for (idx, part) in parts.iter().enumerate() {
                        let field_idx = idx + 1;
                        let field_ty = match substituted_payloads.as_ref() {
                            Some(payloads) if idx < payloads.len() => payloads[idx].clone(),
                            _ if field_idx < fields.len() => (*fields[field_idx]).clone(),
                            _ => break,
                        };
                        let mut field_place = scrutinee_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_idx, field_ty.clone()));
                        self.bind_match_pattern(part, &field_place, &field_ty, span);
                    }
                    return;
                }
                hir::PatKind::Struct(path, pat_fields, _)
                    if self.enum_variant_info_from_path(path).is_some() =>
                {
                    let variant = self
                        .enum_variant_info_from_path(path)
                        .expect("checked above");
                    let substituted_payloads = self.payload_types_from_type_substs(&variant, span);
                    for (idx, field) in pat_fields.iter().enumerate() {
                        let field_idx = idx + 1;
                        let field_ty = match substituted_payloads.as_ref() {
                            Some(payloads) if idx < payloads.len() => payloads[idx].clone(),
                            _ if field_idx < fields.len() => (*fields[field_idx]).clone(),
                            _ => break,
                        };
                        let mut field_place = scrutinee_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_idx, field_ty.clone()));
                        self.bind_match_pattern(&field.pat, &field_place, &field_ty, span);
                    }
                    return;
                }
                _ => {}
            }
        }
        match &pat.kind {
            hir::PatKind::Binding { name, .. } => {
                self.bind_match_binding(name, pat, scrutinee_place, scrutinee_ty, span);
            }
            hir::PatKind::Tuple(parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::TupleStruct(_, parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::Struct(_, fields, _) => {
                for field in fields {
                    self.bind_match_pattern(&field.pat, scrutinee_place, scrutinee_ty, span);
                }
            }
            _ => {}
        }
    }

    pub(super) fn bind_match_binding(
        &mut self,
        name: &hir::Symbol,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        let mut decl = self.lowering.make_local_decl(scrutinee_ty, span);
        decl.mutability = mir::Mutability::Not;
        let local_id = self.push_local(decl);
        let place = mir::Place::from_local(local_id);
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::Use(mir::Operand::Copy(scrutinee_place.clone())),
            ),
        });
        if let Some(log) = self.match_binding_undo_log.as_mut() {
            log.push(MatchBindingUndo::LocalMap(
                pat.hir_id.clone(),
                self.local_map.get(&pat.hir_id).copied(),
            ));
            log.push(MatchBindingUndo::Fallback(
                name.as_str().to_string(),
                self.fallback_locals.get(name.as_str()).copied(),
            ));
        }
        self.local_map.insert(pat.hir_id.clone(), local_id);
        self.fallback_locals
            .insert(name.as_str().to_string(), local_id);
        if let Some(def_id) = self.struct_def_from_ty(scrutinee_ty) {
            self.local_structs.insert(local_id, def_id);
        }
    }
}
