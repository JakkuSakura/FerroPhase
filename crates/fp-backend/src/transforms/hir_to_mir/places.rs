use super::body::BodyBuilder;
use super::*;

fn projections_include_slice_before_index(projections: &[HirAssignTargetProjection]) -> bool {
    let mut saw_slice = false;
    for projection in projections {
        match projection {
            HirAssignTargetProjection::Slice(_) => saw_slice = true,
            HirAssignTargetProjection::Index(_) if saw_slice => return true,
            _ => {}
        }
    }
    false
}
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{Ty, TyKind};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn lower_place_path_base(
        &mut self,
        _expr: &hir::Expr,
        path: &hir::Path,
    ) -> Result<Option<PlaceInfo>> {
        let fallback_local = path
            .segments
            .first()
            .filter(|_| path.segments.len() == 1)
            .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied());
        match &path.res {
            Some(hir::Res::Local(hir_id)) => {
                if let Some(local_id) = self.local_map.get(hir_id) {
                    let local_id = *local_id;
                    let ty = self.locals[local_id as usize].ty.clone();
                    let mut struct_def = self.local_structs.get(&local_id).cloned();
                    if struct_def.is_none() {
                        if let Some(derived) = self.struct_def_from_ty(&ty) {
                            self.local_structs.insert(local_id, derived.clone());
                            struct_def = Some(derived);
                        }
                    }
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
                if let Some(local_id) = fallback_local {
                    let ty = self.locals[local_id as usize].ty.clone();
                    let struct_def = self.struct_def_from_ty(&ty);
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
            }
            // Constants are operands, never assignable places. Let
            // `lower_operand` resolve them to their constant or global form.
            Some(hir::Res::Def(_)) => {}
            _ => {
                if let Some(local_id) = fallback_local {
                    let ty = self.locals[local_id as usize].ty.clone();
                    let struct_def = self.struct_def_from_ty(&ty);
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
            }
        }
        Ok(None)
    }

    pub(super) fn lower_place_expr_base(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        match &expr.kind {
            hir::ExprKind::Unary(hir::UnOp::Deref, inner) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    self.lowering
                        .emit_error(expr.span, "dereference target is not a place expression");
                    return Ok(None);
                };
                let mut base_ty = place_info.ty.clone();
                loop {
                    match &base_ty.kind {
                        TyKind::Ref(_, inner_ty, _) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = inner_ty.as_ref().clone();
                            break;
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = type_and_mut.ty.as_ref().clone();
                            break;
                        }
                        _ if self.boxed_inner_ty(&base_ty).is_some() => {
                            base_ty = self
                                .boxed_inner_ty(&base_ty)
                                .expect("checked boxed inner type above");
                            break;
                        }
                        _ => break,
                    }
                }
                place_info.ty = base_ty;
                place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                Ok(Some(place_info))
            }
            hir::ExprKind::Cast(inner, ty) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    return Ok(None);
                };
                let cast_ty = self.lower_type_expr(ty);
                let place_ok = match (&place_info.ty.kind, &cast_ty.kind) {
                    (TyKind::Ref(_, _, _), TyKind::Ref(_, _, _)) => true,
                    (TyKind::RawPtr(_), TyKind::RawPtr(_)) => true,
                    (TyKind::FnDef(_, _), TyKind::FnPtr(_)) => true,
                    (TyKind::FnPtr(_), TyKind::FnPtr(_)) => true,
                    _ => false,
                };
                if !place_ok {
                    return Ok(None);
                }
                place_info.ty = cast_ty.clone();
                place_info.struct_def = self.struct_def_from_ty(&cast_ty);
                Ok(Some(place_info))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_place_from_projected(
        &mut self,
        expr: &hir::Expr,
    ) -> Result<Option<PlaceInfo>> {
        let Some(projected) = project_hir_assign_target(expr) else {
            return Ok(None);
        };

        let mut place_info = match projected.base {
            HirAssignTargetBase::Name(path) => {
                let Some(place) = self.lower_place_path_base(expr, &path)? else {
                    return Ok(None);
                };
                place
            }
            HirAssignTargetBase::Expr(base_expr) => {
                let Some(place) = self.lower_place_expr_base(base_expr.as_ref())? else {
                    return Ok(None);
                };
                place
            }
        };

        for projection in &projected.projections {
            match projection {
                HirAssignTargetProjection::Deref => {
                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner_ty, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner_ty.as_ref().clone();
                                break;
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                                break;
                            }
                            _ if self.boxed_inner_ty(&base_ty).is_some() => {
                                base_ty = self
                                    .boxed_inner_ty(&base_ty)
                                    .expect("checked boxed inner type above");
                                break;
                            }
                            _ => return Ok(None),
                        }
                    }
                    place_info.ty = base_ty;
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Field(field) => {
                    let mut base_ty = place_info.ty.clone();
                    let mut struct_def = place_info.struct_def;
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    if struct_def.is_none() {
                        struct_def = self.struct_def_from_ty(&base_ty);
                    }
                    let struct_def = match struct_def {
                        Some(def_id) => def_id,
                        None => {
                            self.lowering
                                .emit_error(expr.span, "field access on non-struct value");
                            return Ok(None);
                        }
                    };
                    let (field_index, field_info) = match self.lowering.struct_field(
                        struct_def.clone(),
                        &base_ty,
                        field.as_str(),
                        expr.span,
                    ) {
                        Some(data) => data,
                        None => {
                            let available = self
                                .lowering
                                .struct_def(&struct_def)
                                .map(|definition| {
                                    definition
                                        .fields
                                        .iter()
                                        .map(|field| field.name.as_str())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                })
                                .unwrap_or_default();
                            self.lowering
                                .emit_error(
                                    expr.span,
                                    format!(
                                        "unknown field `{field}` on `{struct_def}`; available fields: [{available}]"
                                    ),
                                );
                            return Ok(None);
                        }
                    };
                    place_info
                        .place
                        .projection
                        .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                    place_info.ty = field_info.ty.clone();
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Index(index) => {
                    if projections_include_slice_before_index(&projected.projections) {
                        return Ok(None);
                    }
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let index_operand = self.lower_operand(index.as_ref(), Some(&index_ty))?;
                    let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                    let index_place = mir::Place::from_local(index_local);
                    let assign = mir::Statement {
                        source_info: index.span,
                        kind: mir::StatementKind::Assign(
                            index_place.clone(),
                            mir::Rvalue::Use(index_operand.operand),
                        ),
                    };
                    self.push_statement(assign);

                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    if self.real_indexable_struct_def_id(&base_ty).is_some()
                        || self.is_list_container(&base_ty)
                        || self.is_map_container(&base_ty)
                    {
                        return Ok(None);
                    }
                    let element_ty = match &base_ty.kind {
                        TyKind::Array(elem, _) => *elem.clone(),
                        TyKind::Slice(elem) => *elem.clone(),
                        _ => {
                            self.lowering
                                .emit_error(expr.span, "index access requires array or slice type");
                            return Ok(None);
                        }
                    };
                    place_info
                        .place
                        .projection
                        .push(mir::PlaceElem::Index(index_local));
                    place_info.ty = element_ty;
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Slice(slice) => {
                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    let element_ty = match &base_ty.kind {
                        TyKind::Array(elem, _) => *elem.clone(),
                        TyKind::Slice(elem) => *elem.clone(),
                        _ => {
                            self.lowering
                                .emit_error(expr.span, "slice access requires array or slice type");
                            return Ok(None);
                        }
                    };
                    let Some(from) = slice
                        .start
                        .as_ref()
                        .map_or(Some(0), |start| self.evaluate_array_length(start.as_ref()))
                    else {
                        return Ok(None);
                    };
                    let Some(mut to) = (match slice.end.as_ref() {
                        Some(end) => self.evaluate_array_length(end.as_ref()),
                        None => match &base_ty.kind {
                            TyKind::Array(_, len) => self.const_kind_to_u64(expr.span, len),
                            _ => None,
                        },
                    }) else {
                        return Ok(None);
                    };
                    if slice.inclusive {
                        to = to.saturating_add(1);
                    }
                    if to < from {
                        self.lowering
                            .emit_error(expr.span, "slice end is before slice start");
                        return Ok(None);
                    }
                    place_info.place.projection.push(mir::PlaceElem::Subslice {
                        from,
                        to,
                        from_end: false,
                    });
                    place_info.ty = Ty {
                        kind: TyKind::Slice(Box::new(element_ty)),
                    };
                    place_info.struct_def = None;
                }
            }
        }

        Ok(Some(place_info))
    }

    pub(super) fn lower_place(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        self.lower_place_from_projected(expr)
    }

    pub(super) fn materialize_expr_place(&mut self, expr: &hir::Expr) -> Result<PlaceInfo> {
        let value = self.lower_operand(expr, None)?;
        let local_id = self.allocate_temp(value.ty.clone(), expr.span);
        let place = mir::Place::from_local(local_id);
        let container_kind = match &value.operand {
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }) => {
                    Some(mir::ContainerKind::List {
                        elem_ty: elem_ty.clone(),
                        len: elements.len() as u64,
                    })
                }
                mir::ConstantKind::Val(mir::ConstValue::Map {
                    entries,
                    key_ty,
                    value_ty,
                }) => Some(mir::ContainerKind::Map {
                    key_ty: key_ty.clone(),
                    value_ty: value_ty.clone(),
                    len: entries.len() as u64,
                }),
                _ => None,
            },
            _ => None,
        };
        let statement = mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(place.clone(), mir::Rvalue::Use(value.operand)),
        };
        self.push_statement(statement);
        self.locals[local_id as usize].ty = value.ty.clone();
        let struct_def = self.struct_def_from_ty(&value.ty);
        if let Some(ref def_id) = struct_def {
            self.local_structs.insert(local_id, def_id.clone());
        }
        if let Some(kind) = container_kind {
            self.container_locals.insert(local_id, kind);
        }
        Ok(PlaceInfo {
            place,
            ty: value.ty.clone(),
            struct_def,
        })
    }

    pub(super) fn lower_expr_into_place(
        &mut self,
        expr: &hir::Expr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Let(pat, ty, init) => {
                self.lower_let_expr(pat, ty, init, expr.span)?;
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place,
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::Query(query) => {
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Query(mir::Query {
                            origin: query.origin.clone(),
                            ir: query.ir.clone(),
                            span: query.span,
                        }),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = expected_ty.clone();
                }
            }
            hir::ExprKind::Literal(_)
            | hir::ExprKind::Path(_)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::FieldAccess(_, _)
            | hir::ExprKind::ConstBlock(_) => {
                let assignment_place = place.clone();
                let value = self.lower_operand(expr, Some(expected_ty))?;
                let container_kind = match &value.operand {
                    mir::Operand::Constant(constant) => match &constant.literal {
                        mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }) => {
                            Some(mir::ContainerKind::List {
                                elem_ty: elem_ty.clone(),
                                len: elements.len() as u64,
                            })
                        }
                        mir::ConstantKind::Val(mir::ConstValue::Map {
                            entries,
                            key_ty,
                            value_ty,
                        }) => Some(mir::ContainerKind::Map {
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                            len: entries.len() as u64,
                        }),
                        _ => None,
                    },
                    _ => None,
                };
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        assignment_place.clone(),
                        mir::Rvalue::Use(value.operand),
                    ),
                };
                self.push_statement(statement);
                if assignment_place.projection.is_empty() {
                    // Prefer the destination's already-known `expected_ty`
                    // (the declared/annotated type this value is being
                    // assigned into) over `value.ty` (the operand's own,
                    // independently-derived type) — a comptime-frozen
                    // constant can lose its ADT identity on the way to a
                    // `mir::Constant` (a struct value degrading to a bare
                    // field tuple, since `mir::ConstValue`/`LirType` are
                    // purely structural and don't carry it through), and
                    // `value.ty` would then wrongly clobber a local whose
                    // real, declared type (`Vec<BenchCase>`, say) is
                    // already known and correct.
                    self.locals[assignment_place.local as usize].ty = expected_ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(expected_ty) {
                        self.local_structs
                            .insert(assignment_place.local, struct_def);
                    }
                    if let Some(kind) = container_kind {
                        self.container_locals.insert(assignment_place.local, kind);
                    }
                }
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = if matches!(ty_expr.kind, hir::TypeExprKind::Infer) {
                    self.lowering
                        .typeck_expr_type(expr.hir_id.clone())
                        .unwrap_or_else(|| expected_ty.clone())
                } else {
                    self.lower_type_expr(ty_expr)
                };
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Cast(mir::CastKind::Misc, operand.operand, target_ty.clone()),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = target_ty;
                }
            }
            hir::ExprKind::Loop(block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::While(cond, block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_while_expr(expr.span, cond, block, Some(destination))?;
            }
            hir::ExprKind::Try(expr_try) => {
                self.lower_try_expr(
                    expr,
                    expr_try,
                    Some((place.clone(), expected_ty.clone())),
                    false,
                )?;
            }
            hir::ExprKind::Break(value) => {
                self.lower_break(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Return(value) => {
                self.lower_return(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Continue => {
                self.lower_continue(expr.span)?;
            }
            hir::ExprKind::Struct(path, fields) => {
                let local_id = place.local;
                self.lower_struct_literal(
                    local_id,
                    Some(expected_ty),
                    expr.hir_id.clone(),
                    path,
                    fields,
                    expr.span,
                )?;
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let left = self.lower_operand(lhs, None)?;
                let right = self.lower_operand(rhs, None)?;

                if HirToMirLowerer::is_unit_ty(&left.ty) || HirToMirLowerer::is_unit_ty(&right.ty) {
                    return Err(fp_core::error::Error::from(format!(
                        "binary operation `{op:?}` received unit operand(s): lhs=`{}`, rhs=`{}`",
                        left.ty, right.ty
                    )));
                }

                let mir_op = Self::convert_bin_op(op);
                let result_ty = Self::binary_result_ty(op, &left.ty);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::BinaryOp(mir_op, left.operand, right.operand),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = result_ty;
                }
            }
            hir::ExprKind::Unary(op, operand_expr) => match op {
                hir::UnOp::Neg | hir::UnOp::Not => {
                    let operand = self.lower_operand(operand_expr, None)?;
                    let mir_op = match Self::convert_un_op(op) {
                        Some(op) => op,
                        None => unreachable!("Neg/Not must convert to MIR op"),
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::UnaryOp(mir_op, operand.operand),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = operand.ty.clone();
                    }
                }
                hir::UnOp::Deref => {
                    let place_info = match self.lower_place(expr)? {
                        Some(info) => info,
                        None => {
                            self.lowering.emit_error(
                                expr.span,
                                "dereference expressions must resolve to a place",
                            );
                            return Ok(());
                        }
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Copy(place_info.place.clone())),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                    }
                }
                hir::UnOp::Box => {
                    let operand = self.lower_operand(operand_expr, None)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(operand.operand),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                    }
                }
            },
            hir::ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    self.lower_stmt(stmt)?;
                }

                if let Some(expr) = &block.expr {
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                } else {
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let bool_ty = Ty { kind: TyKind::Bool };
                let cond_operand = self.lower_condition_operand(cond)?;

                let then_block = self.new_block();
                let else_block = self.new_block();
                let continue_block = self.new_block();

                let switch = mir::Terminator {
                    source_info: cond.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand,
                        switch_ty: bool_ty,
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![then_block],
                            otherwise: else_block,
                        },
                    },
                };
                self.set_current_terminator(switch);

                // Then branch
                self.current_block = then_block;
                self.control_flow_emitted = false;
                self.lower_expr_into_place(then_expr, place.clone(), expected_ty)?;
                if !self.control_flow_emitted
                    && self.blocks[self.current_block as usize]
                        .terminator
                        .is_none()
                {
                    let then_goto = mir::Terminator {
                        source_info: then_expr.span,
                        kind: mir::TerminatorKind::Goto {
                            target: continue_block,
                        },
                    };
                    self.set_current_terminator(then_goto);
                }

                // Else branch (if present)
                self.current_block = else_block;
                if let Some(else_expr) = else_expr {
                    self.control_flow_emitted = false;
                    self.lower_expr_into_place(else_expr, place, expected_ty)?;
                    if !self.control_flow_emitted
                        && self.blocks[self.current_block as usize]
                            .terminator
                            .is_none()
                    {
                        let else_goto = mir::Terminator {
                            source_info: else_expr.span,
                            kind: mir::TerminatorKind::Goto {
                                target: continue_block,
                            },
                        };
                        self.set_current_terminator(else_goto);
                    }
                } else {
                    self.control_flow_emitted = false;
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                    if self.blocks[self.current_block as usize]
                        .terminator
                        .is_none()
                    {
                        let else_goto = mir::Terminator {
                            source_info: expr.span,
                            kind: mir::TerminatorKind::Goto {
                                target: continue_block,
                            },
                        };
                        self.set_current_terminator(else_goto);
                    }
                }

                self.current_block = continue_block;
                self.control_flow_emitted = false;
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.lower_match_expr(expr.span, scrutinee, arms, place, expected_ty)?;
            }
            hir::ExprKind::IntrinsicCall(call) => match call.kind {
                kind => match kind {
                    IntrinsicKind::Print | IntrinsicKind::Println => {
                        self.emit_printf_call(call, expr.span)?;
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(statement);
                        if (place.local as usize) < self.locals.len() {
                            self.locals[place.local as usize].ty = HirToMirLowerer::unit_ty();
                        }
                        return Ok(());
                    }
                    IntrinsicKind::Format => {
                        let (format, args) = self.prepare_format_call(call, expr.span)?;
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::IntrinsicCall {
                                    kind: IntrinsicKind::Format,
                                    format,
                                    args,
                                },
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    IntrinsicKind::Panic => {
                        let unit_assign = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(unit_assign);
                        self.emit_panic_intrinsic(call, expr.span)?;
                        return Ok(());
                    }
                    IntrinsicKind::CatchUnwind => {
                        self.lower_catch_unwind(expr, call, Some(place.clone()))?;
                        return Ok(());
                    }
                    IntrinsicKind::CatchUnwindResult => {
                        self.lower_catch_unwind_result(expr, call, Some(place.clone()))?;
                        return Ok(());
                    }
                    IntrinsicKind::TimeNow => {
                        let args = &call.callargs;
                        if !args.is_empty() {
                            self.lowering
                                .emit_error(expr.span, "time::now intrinsic expects no arguments");
                        }
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::IntrinsicCall {
                                    kind: IntrinsicKind::TimeNow,
                                    format: String::new(),
                                    args: Vec::new(),
                                },
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    IntrinsicKind::FsReadToString => {
                        self.lower_fs_read_to_string_into_place(
                            expr,
                            call,
                            place.clone(),
                            expected_ty,
                        )?;
                        return Ok(());
                    }
                    IntrinsicKind::FsWriteString
                    | IntrinsicKind::FsAppendString
                    | IntrinsicKind::FsIsDir
                    | IntrinsicKind::FsIsFile => {
                        self.lowering.emit_error(
                            expr.span,
                            format!("{:?} is not implemented for compiled backends", kind),
                        );
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(mir::Operand::Constant(
                                    self.lowering.error_constant(expr.span),
                                )),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    IntrinsicKind::FsExists => {
                        self.lower_fs_exists_into_place(expr, call, place.clone(), expected_ty)?;
                        return Ok(());
                    }
                    IntrinsicKind::FsRemoveFile => {
                        self.lower_fs_remove_file_as_statement(expr, call)?;
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    IntrinsicKind::EnvVarExists => {
                        self.lower_env_var_exists_into_place(
                            expr,
                            call,
                            place.clone(),
                            expected_ty,
                        )?;
                        return Ok(());
                    }
                    IntrinsicKind::EnvVar => {
                        self.lower_env_var_into_place(expr, call, place.clone(), expected_ty)?;
                        return Ok(());
                    }
                    IntrinsicKind::Spawn | IntrinsicKind::Select => {
                        if let Some(first) = call.callargs.first() {
                            self.lower_expr_into_place(&first.value, place.clone(), expected_ty)?;
                        } else {
                            self.lowering.emit_error(
                                expr.span,
                                format!("{:?} intrinsic expects at least one argument", kind),
                            );
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                                ),
                            };
                            self.push_statement(statement);
                        }
                        return Ok(());
                    }
                    IntrinsicKind::Join => {
                        let args = &call.callargs;
                        if args.is_empty() {
                            self.lowering
                                .emit_error(expr.span, "join intrinsic expects arguments");
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }

                        if args.len() == 1 {
                            self.lower_expr_into_place(&args[0].value, place.clone(), expected_ty)?;
                            return Ok(());
                        }

                        let mut operands = Vec::with_capacity(args.len());
                        for arg in args {
                            let value = self.lower_operand(&arg.value, None)?;
                            operands.push(value.operand);
                        }
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    IntrinsicKind::ProcMacroTokenStreamFromStr
                    | IntrinsicKind::ProcMacroTokenStreamToString => {
                        let mut operands = Vec::with_capacity(call.callargs.len());
                        for arg in &call.callargs {
                            let value = self.lower_operand(&arg.value, None)?;
                            operands.push(value.operand);
                        }
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::IntrinsicCall {
                                    kind: kind,
                                    format: String::new(),
                                    args: operands,
                                },
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }
                    _ => {
                        // Type-producing comptime intrinsics are expressions,
                        // so a tail expression must assign their handle into
                        // the destination (including a synthetic const
                        // function's return place). Operand lowering has the
                        // same family for nested uses.
                        if matches!(
                            call.kind,
                            IntrinsicKind::CreateStruct
                                | IntrinsicKind::AddField
                                | IntrinsicKind::CloneStruct
                                | IntrinsicKind::BuildType
                                | IntrinsicKind::PrimitiveType
                        ) {
                            let operands = call
                                .callargs
                                .iter()
                                .map(|arg| {
                                    self.lower_operand(&arg.value, None).map(|arg| arg.operand)
                                })
                                .collect::<Result<Vec<_>>>()?;
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::IntrinsicCall {
                                        kind: call.kind,
                                        format: String::new(),
                                        args: operands,
                                    },
                                ),
                            });
                            return Ok(());
                        }
                        if let Some((literal, ty)) = self.lower_intrinsic_constant(call, expr.span)
                        {
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                        span: expr.span,
                                        ty,
                                        user_ty: None,
                                        literal,
                                    })),
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }

                        self.lowering.emit_warning(
                            expr.span,
                            format!(
                                "intrinsic {:?} is not yet supported for MIR assignment",
                                call.kind
                            ),
                        );
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(statement);
                    }
                },
            },
            hir::ExprKind::MethodCall(receiver, method_name, _, args) => {
                let mut resolved_info: Option<(MethodLoweringInfo, Option<PlaceInfo>)> = None;
                let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

                if let Some(def_id) = self.lowering.typeck_method_resolution(expr.hir_id.clone()) {
                    if let Some(kind) = self.lowering.hir_program.intrinsic_def(def_id.clone()) {
                        let mut intrinsic_args = Vec::with_capacity(arg_values.len() + 1);
                        intrinsic_args.push(receiver.as_ref());
                        intrinsic_args.extend(arg_values.iter().copied());
                        if self.lower_resolved_intrinsic_call(
                            expr,
                            kind,
                            &intrinsic_args,
                            Some((place.clone(), expected_ty.clone())),
                        )? {
                            return Ok(());
                        }
                    }
                    // `ensure_method_info` is the uniform lookup, same
                    // shape as `compute_adt_layout` — see `resolve_callee_path`'s
                    // matching comment.
                    if let Some(info) = self.lowering.ensure_method_info(def_id.clone()) {
                        resolved_info = Some((info, None));
                        // Signature presence doesn't imply the body's been
                        // lowered yet — ensure it now.
                        self.lowering.ensure_method_lowered(def_id)?;
                    }
                }

                // `str::len`/`str::as_ptr` (`crates/fp-lang/src/std/string/
                // mod.fp`'s `impl str { .. }`) are `compile_error!("compiler
                // intrinsic")`-marked stubs — `str` has no `.fp`-visible
                // fields to read a real body from, so `function_body_is_
                // compiler_intrinsic_marker` drops their HIR body to `None`
                // (`ast_to_hir/items.rs:294-296`). They're still registered
                // into `method_lookup_by_def` like any other non-generic
                // impl method, so without this check `resolved_info` above
                // would already be `Some`, and the plain-call path below
                // would emit a `Call` to that empty-bodied function (which
                // has no lowered statements — `BodyBuilder::lower` skips
                // lowering entirely when `function.body` is `None` — so its
                // return "value" would be whatever's left in its
                // uninitialized return-place local, not the real length or
                // pointer). Intercept by name here — before `resolved_info`
                // is consumed — and compute the real value directly from
                // the receiver's own slice-shaped place, reusing the same
                // `lower_slice_len_place`/`lower_slice_ptr_place` helpers
                // the hand-written env/fs intrinsics already use.
                let str_intrinsic_name = |name: &str| -> Option<bool> {
                    if name == "len" || name.ends_with("::len") {
                        Some(true)
                    } else if name == "as_ptr" || name.ends_with("::as_ptr") {
                        Some(false)
                    } else {
                        None
                    }
                };
                if let Some(is_len) = str_intrinsic_name(method_name.as_str()) {
                    if let Some(receiver_place) = self.lower_place(receiver)? {
                        let mut base_ty = receiver_place.ty.clone();
                        let mut base_place = receiver_place.place.clone();
                        loop {
                            match &base_ty.kind {
                                TyKind::Ref(_, inner, _) => {
                                    base_place.projection.push(mir::PlaceElem::Deref);
                                    base_ty = inner.as_ref().clone();
                                }
                                TyKind::RawPtr(type_and_mut) => {
                                    base_place.projection.push(mir::PlaceElem::Deref);
                                    base_ty = type_and_mut.ty.as_ref().clone();
                                }
                                _ => break,
                            }
                        }
                        if let TyKind::Slice(_) = &base_ty.kind {
                            let (field_place, declared_ty) = if is_len {
                                (
                                    self.lower_slice_len_place(base_place),
                                    Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    },
                                )
                            } else {
                                (
                                    self.lower_slice_ptr_place(base_place),
                                    self.lowering.raw_string_ptr_ty(),
                                )
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = declared_ty;
                            }
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Use(mir::Operand::copy(field_place)),
                                ),
                            });
                            return Ok(());
                        }
                    }
                }

                if (method_name.as_str() == "get_unchecked"
                    || method_name.as_str().ends_with("::get_unchecked"))
                    && args.len() == 1
                {
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        let mut resolved_path = path.clone();
                        self.resolve_self_path(&mut resolved_path);
                        let mut const_info = None;
                        let mut const_body_len = None;
                        if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                            if let Some(info) = self.lowering.ensure_const_info(def_id.clone()) {
                                const_info = Some(info.clone());
                            } else if let Some(konst) = self
                                .lowering
                                .hir_item(def_id.clone())
                                .and_then(|item| match &item.kind {
                                    hir::ItemKind::Const(konst) => Some(konst.clone()),
                                    _ => None,
                                })
                            {
                                if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                    const_body_len = Some(elements.len() as u64);
                                }
                                self.lowering.ensure_item_lowered(def_id.clone())?;
                                if let Some(info) = self.lowering.ensure_const_info(def_id.clone())
                                {
                                    const_info = Some(info.clone());
                                }
                            }
                        } else if resolved_path.segments.len() == 1 {
                            let name = resolved_path.segments[0].name.as_str();
                            let matching_const =
                                self.lowering
                                    .hir_all_items()
                                    .find_map(|item| match &item.kind {
                                        hir::ItemKind::Const(konst)
                                            if konst.name.as_str() == name =>
                                        {
                                            Some((item.def_id.clone(), konst.clone()))
                                        }
                                        _ => None,
                                    });
                            if let Some((def_id, konst)) = matching_const {
                                if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                    const_body_len = Some(elements.len() as u64);
                                }
                                self.lowering.ensure_item_lowered(def_id.clone())?;
                                if let Some(info) = self.lowering.ensure_const_info(def_id.clone())
                                {
                                    const_info = Some(info.clone());
                                }
                            }
                        }

                        if let Some(const_info) = const_info {
                            if let mir::ConstantKind::Val(value) = &const_info.value.literal {
                                if let Some((constant, ty)) = self.lowering.const_index_value(
                                    expr.span,
                                    &const_info.typed_value(),
                                    &args[0].value,
                                ) {
                                    self.push_statement(mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Constant(constant)),
                                        ),
                                    });
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = ty.clone();
                                    }
                                    return Ok(());
                                }
                                let mut map_len: Option<u64> = None;
                                let mut map_key_ty: Option<Ty> = None;
                                let mut map_value_ty: Option<Ty> = None;
                                match value {
                                    mir::ConstValue::Map {
                                        entries,
                                        key_ty,
                                        value_ty,
                                    } => {
                                        map_len = Some(entries.len() as u64);
                                        map_key_ty = Some(key_ty.clone());
                                        map_value_ty = Some(value_ty.clone());
                                    }
                                    mir::ConstValue::List { elements, elem_ty } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_len = Some(elements.len() as u64);
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                            }
                                        }
                                    }
                                    mir::ConstValue::Array(elements) => {
                                        if let TyKind::Array(elem_ty, _) = &const_info.ty.kind {
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_len = Some(elements.len() as u64);
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                                if map_len.is_none() {
                                    map_len = const_body_len;
                                }

                                if map_key_ty.is_none() {
                                    let key_operand = self.lower_operand(&args[0].value, None)?;
                                    map_key_ty = Some(key_operand.ty);
                                }
                                if map_value_ty.is_none() {
                                    map_value_ty = Some(expected_ty.clone());
                                }

                                if let (Some(key_ty), Some(value_ty), Some(len)) =
                                    (map_key_ty, map_value_ty, map_len)
                                {
                                    if len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len,
                                                    },
                                                    container: mir::Operand::Constant(
                                                        const_info.typed_value(),
                                                    ),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }

                    if let Ok(receiver_info) = self.lower_operand(receiver, None) {
                        if let mir::Operand::Constant(constant) = &receiver_info.operand {
                            if let mir::ConstantKind::Val(mir::ConstValue::Map {
                                entries,
                                key_ty,
                                value_ty,
                            }) = &constant.literal
                            {
                                let key_operand =
                                    self.lower_operand(arg_values[0], Some(key_ty))?;
                                let kind = mir::ContainerKind::Map {
                                    key_ty: key_ty.clone(),
                                    value_ty: value_ty.clone(),
                                    len: entries.len() as u64,
                                };
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind,
                                            container: receiver_info.operand.clone(),
                                            key: key_operand.operand,
                                        },
                                    ),
                                });

                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                        if let Some(local_id) = self.local_id_from_expr(receiver) {
                            if let Some(container_kind) =
                                self.container_locals.get(&local_id).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        let local_place = mir::Place::from_local(local_id);
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: mir::Operand::copy(local_place),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                        if let mir::Operand::Copy(place) = &receiver_info.operand {
                            if let Some(container_kind) =
                                self.container_locals.get(&place.local).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: receiver_info.operand.clone(),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }

                        let mut map_len: Option<u64> = None;
                        let mut map_key_ty: Option<Ty> = None;
                        let mut map_value_ty: Option<Ty> = None;
                        let receiver_ty = match &receiver_info.ty.kind {
                            TyKind::Ref(_, inner, _) => inner.as_ref(),
                            _ => &receiver_info.ty,
                        };
                        match &receiver_ty.kind {
                            TyKind::Array(elem_ty, len) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                        map_len = self.const_kind_to_u64(expr.span, len);
                                    }
                                }
                            }
                            TyKind::Slice(elem_ty) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                    }
                                }
                            }
                            _ => {}
                        }

                        if map_len.is_none() {
                            if let mir::Operand::Constant(constant) = &receiver_info.operand {
                                if let mir::ConstantKind::Val(value) = &constant.literal {
                                    match value {
                                        mir::ConstValue::Map {
                                            entries,
                                            key_ty,
                                            value_ty,
                                        } => {
                                            map_len = Some(entries.len() as u64);
                                            map_key_ty = Some(key_ty.clone());
                                            map_value_ty = Some(value_ty.clone());
                                        }
                                        mir::ConstValue::List { elements, elem_ty } => {
                                            map_len = Some(elements.len() as u64);
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }

                        if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                            let len = map_len.unwrap_or(0);
                            if len != 0 {
                                let key_operand =
                                    self.lower_operand(&args[0].value, Some(&key_ty))?;
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind: mir::ContainerKind::Map {
                                                key_ty: key_ty.clone(),
                                                value_ty: value_ty.clone(),
                                                len,
                                            },
                                            container: receiver_info.operand,
                                            key: key_operand.operand,
                                        },
                                    ),
                                });
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                    }
                }

                if let Some((info, _cached_place)) = resolved_info {
                    let receiver_expected = info.sig.inputs.get(0);
                    let receiver_operand = self.lower_operand(receiver, receiver_expected)?;

                    let mut lowered_args = Vec::with_capacity(args.len() + 1);
                    lowered_args.push(receiver_operand.operand);
                    for (idx, arg) in args.iter().enumerate() {
                        let expected = info.sig.inputs.get(idx + 1);
                        let operand = self.lower_operand(&arg.value, expected)?;
                        lowered_args.push(operand.operand);
                    }

                    let method_def_id = info.def_id.clone().ok_or_else(|| {
                        crate::error::optimization_error(format!(
                            "resolved method `{}` has no definition identity",
                            info.fn_name
                        ))
                    })?;
                    let literal = mir::ConstantKind::FnDef(method_def_id, info.substs.clone());
                    let func_operand = mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: info.fn_ty.clone(),
                        user_ty: None,
                        literal,
                    });

                    let continue_block = self.new_block();
                    let destination = Some((place.clone(), continue_block));
                    let terminator = mir::Terminator {
                        source_info: expr.span,
                        kind: mir::TerminatorKind::Call {
                            func: func_operand,
                            args: lowered_args,
                            destination: destination.clone(),
                            cleanup: self.current_unwind_target,
                            from_hir_call: true,
                            fn_span: expr.span,
                        },
                    };

                    self.blocks[self.current_block as usize].terminator = Some(terminator);
                    self.current_block = continue_block;

                    let result_ty = info.sig.output.clone();
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = result_ty.clone();
                    }
                    if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                        self.local_structs.insert(place.local, struct_def);
                    }

                    return Ok(());
                }

                if let Ok(Some(place_info)) = self.lower_place(receiver) {
                    if let Some(def_id) = place_info
                        .struct_def
                        .or_else(|| self.struct_def_from_ty(&place_info.ty))
                    {
                        if self.lowering.struct_def(&def_id).is_some() {
                            let method_def = self
                                .lowering
                                .typeck_method_resolution(expr.hir_id.clone())
                                .and_then(|def_id| self.lowering.ensure_generic_method_def(def_id));
                            if let Some(def) = method_def {
                                let method_ctx = self
                                    .lowering
                                    .make_method_context(&def.self_ty, &def.assoc_types);
                                let tentative_sig = self
                                    .lowering
                                    .lower_function_sig(&def.function.sig, method_ctx.as_ref());
                                let receiver_expected = tentative_sig.inputs.get(0);
                                let receiver_operand =
                                    self.lower_operand(receiver, receiver_expected)?;

                                let mut call_args = args.to_vec();
                                if let Some(mut param_names) =
                                    self.param_names_from_params(&def.function.sig.inputs)
                                {
                                    if !param_names.is_empty() {
                                        param_names.remove(0);
                                    }
                                    call_args = self.reorder_named_call_args(
                                        args,
                                        &param_names,
                                        expr.span,
                                    )?;
                                }

                                let mut lowered_args = Vec::with_capacity(call_args.len() + 1);
                                let mut arg_types = Vec::with_capacity(call_args.len() + 1);
                                arg_types.push(receiver_operand.ty.clone());
                                lowered_args.push(receiver_operand.operand);
                                for (idx, arg) in call_args.iter().enumerate() {
                                    let expected = tentative_sig.inputs.get(idx + 1);
                                    let operand = self.lower_operand(&arg.value, expected)?;
                                    arg_types.push(operand.ty.clone());
                                    lowered_args.push(operand.operand);
                                }

                                let generic_args = self
                                    .lowering
                                    .typeck_generic_method_arg(expr.hir_id.clone())
                                    .ok_or_else(|| {
                                        crate::error::optimization_error(
                                            "missing HIR generic method substitutions",
                                        )
                                    })?;
                                let info = self.lowering.ensure_method_specialization(
                                    &def,
                                    &generic_args,
                                    &arg_types,
                                    Some(&place_info.ty),
                                    expr.span,
                                )?;

                                let func_operand = mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::FnDef(
                                        info.def_id.clone().ok_or_else(|| {
                                            crate::error::optimization_error(format!(
                                                "specialized method `{}` has no definition identity",
                                                info.fn_name
                                            ))
                                        })?,
                                        info.substs.clone(),
                                    ),
                                });

                                let continue_block = self.new_block();
                                let destination = Some((place.clone(), continue_block));
                                let terminator = mir::Terminator {
                                    source_info: expr.span,
                                    kind: mir::TerminatorKind::Call {
                                        func: func_operand,
                                        args: lowered_args,
                                        destination: destination.clone(),
                                        cleanup: self.current_unwind_target,
                                        from_hir_call: true,
                                        fn_span: expr.span,
                                    },
                                };

                                self.blocks[self.current_block as usize].terminator =
                                    Some(terminator);
                                self.current_block = continue_block;

                                let result_ty = info.sig.output.clone();
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = result_ty.clone();
                                }
                                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                                    self.local_structs.insert(place.local, struct_def);
                                }

                                return Ok(());
                            }
                        }
                    } else if let Some(enum_def) = self.enum_def_from_ty(&place_info.ty) {
                        if self.lowering.has_enum_def(&enum_def) {
                            let method_def = self
                                .lowering
                                .typeck_method_resolution(expr.hir_id.clone())
                                .and_then(|def_id| self.lowering.ensure_generic_method_def(def_id));
                            if let Some(def) = method_def {
                                let method_ctx = self
                                    .lowering
                                    .make_method_context(&def.self_ty, &def.assoc_types);
                                let tentative_sig = self
                                    .lowering
                                    .lower_function_sig(&def.function.sig, method_ctx.as_ref());
                                let receiver_expected = tentative_sig.inputs.get(0);
                                let receiver_operand =
                                    self.lower_operand(receiver, receiver_expected)?;

                                let mut call_args = args.to_vec();
                                if let Some(mut param_names) =
                                    self.param_names_from_params(&def.function.sig.inputs)
                                {
                                    if !param_names.is_empty() {
                                        param_names.remove(0);
                                    }
                                    call_args = self.reorder_named_call_args(
                                        args,
                                        &param_names,
                                        expr.span,
                                    )?;
                                }

                                let mut lowered_args = Vec::with_capacity(call_args.len() + 1);
                                let mut arg_types = Vec::with_capacity(call_args.len() + 1);
                                arg_types.push(receiver_operand.ty.clone());
                                lowered_args.push(receiver_operand.operand);
                                for (idx, arg) in call_args.iter().enumerate() {
                                    let expected = tentative_sig.inputs.get(idx + 1);
                                    let operand = self.lower_operand(&arg.value, expected)?;
                                    arg_types.push(operand.ty.clone());
                                    lowered_args.push(operand.operand);
                                }

                                let generic_args = self
                                    .lowering
                                    .typeck_generic_method_arg(expr.hir_id.clone())
                                    .ok_or_else(|| {
                                        crate::error::optimization_error(
                                            "missing HIR generic method substitutions",
                                        )
                                    })?;
                                let info = self.lowering.ensure_method_specialization(
                                    &def,
                                    &generic_args,
                                    &arg_types,
                                    Some(&place_info.ty),
                                    expr.span,
                                )?;

                                let func_operand = mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::FnDef(
                                        info.def_id.clone().ok_or_else(|| {
                                            crate::error::optimization_error(format!(
                                                "specialized method `{}` has no definition identity",
                                                info.fn_name
                                            ))
                                        })?,
                                        info.substs.clone(),
                                    ),
                                });

                                let continue_block = self.new_block();
                                let destination = Some((place.clone(), continue_block));
                                let terminator = mir::Terminator {
                                    source_info: expr.span,
                                    kind: mir::TerminatorKind::Call {
                                        func: func_operand,
                                        args: lowered_args,
                                        destination: destination.clone(),
                                        cleanup: self.current_unwind_target,
                                        from_hir_call: true,
                                        fn_span: expr.span,
                                    },
                                };

                                self.blocks[self.current_block as usize].terminator =
                                    Some(terminator);
                                self.current_block = continue_block;

                                let result_ty = info.sig.output.clone();
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = result_ty.clone();
                                }
                                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                                    self.local_structs.insert(place.local, struct_def);
                                }

                                return Ok(());
                            }
                        }
                    }
                }

                if method_name.as_str() == "push" && args.len() == 1 {
                    if let Some(receiver_place) = self.lower_place(receiver)? {
                        if self.is_list_container(&receiver_place.ty) {
                            let elem_ty = self
                                .expect_array_element_ty(&receiver_place.ty)
                                .unwrap_or_else(|| self.lowering.error_ty());
                            let value_info = self.lower_operand(&args[0].value, Some(&elem_ty))?;
                            let kind = mir::ContainerKind::List {
                                elem_ty: elem_ty.clone(),
                                len: 0,
                            };
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    receiver_place.place.clone(),
                                    mir::Rvalue::ContainerPush {
                                        kind,
                                        container: mir::Operand::copy(receiver_place.place.clone()),
                                        value: value_info.operand,
                                    },
                                ),
                            });
                            // `push` returns unit; still initialize the call
                            // expression's own (unused) destination place.
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                                ),
                            });
                            return Ok(());
                        }
                    }
                }

                if method_name.as_str() == "len" && args.is_empty() {
                    if let Some(constant) = self.lowering.lower_const_expr(receiver, None, None) {
                        if let Some(len) = self.lowering.const_len_from_constant(&constant) {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = len_ty.clone();
                            }
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                        span: expr.span,
                                        ty: len_ty.clone(),
                                        user_ty: None,
                                        literal: mir::ConstantKind::UInt(len),
                                    })),
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }
                    }
                    if let Some(local_id) = self.local_id_from_expr(receiver) {
                        if let Some(kind) = self.container_locals.get(&local_id).cloned() {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = len_ty.clone();
                            }
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::ContainerLen {
                                        kind,
                                        container: mir::Operand::copy(mir::Place::from_local(
                                            local_id,
                                        )),
                                    },
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }
                        if let Some(local) = self.locals.get(local_id as usize) {
                            if self.is_list_container(&local.ty) {
                                let elem_ty = self
                                    .expect_array_element_ty(&local.ty)
                                    .unwrap_or_else(|| self.lowering.error_ty());
                                let len = self
                                    .container_locals
                                    .get(&local_id)
                                    .and_then(|kind| match kind {
                                        mir::ContainerKind::List { len, .. } => Some(*len),
                                        _ => None,
                                    })
                                    .unwrap_or(0);
                                let kind = mir::ContainerKind::List {
                                    elem_ty: elem_ty.clone(),
                                    len,
                                };
                                let len_ty = Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                };
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = len_ty.clone();
                                }
                                let statement = mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place,
                                        mir::Rvalue::ContainerLen {
                                            kind,
                                            container: mir::Operand::copy(mir::Place::from_local(
                                                local_id,
                                            )),
                                        },
                                    ),
                                };
                                self.push_statement(statement);
                                return Ok(());
                            }
                        }
                        let array_len = self.locals.get(local_id as usize).and_then(|local| {
                            if let TyKind::Array(_, len) = &local.ty.kind {
                                Some(len.clone())
                            } else {
                                None
                            }
                        });
                        if let Some(len) = array_len {
                            if let Some(len) = self.const_kind_to_u64(expr.span, &len) {
                                let len_ty = Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                };
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = len_ty.clone();
                                }
                                let statement = mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place,
                                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                            span: expr.span,
                                            ty: len_ty.clone(),
                                            user_ty: None,
                                            literal: mir::ConstantKind::UInt(len),
                                        })),
                                    ),
                                };
                                self.push_statement(statement);
                                return Ok(());
                            }
                        }
                    }
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        if let Some(hir::Res::Def(def_id)) = &path.res {
                            if let Some(const_info) =
                                self.lowering.ensure_const_info(def_id.clone())
                            {
                                if let Some(len) =
                                    self.lowering.const_len_from_constant(&const_info.value)
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(len),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = &const_info.ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                            if let Some(konst) = self.const_items.get(def_id).cloned() {
                                let ty = self.lower_type_expr(&konst.ty);
                                if let Some(constant) = self.lowering.lower_const_expr(
                                    &konst.body.value,
                                    Some(&ty),
                                    None,
                                ) {
                                    if let Some(len) =
                                        self.lowering.const_len_from_constant(&constant)
                                    {
                                        let len_ty = Ty {
                                            kind: TyKind::Uint(UintTy::Usize),
                                        };
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = len_ty.clone();
                                        }
                                        let statement = mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place,
                                                mir::Rvalue::Use(mir::Operand::Constant(
                                                    mir::Constant {
                                                        span: expr.span,
                                                        ty: len_ty.clone(),
                                                        user_ty: None,
                                                        literal: mir::ConstantKind::UInt(len),
                                                    },
                                                )),
                                            ),
                                        };
                                        self.push_statement(statement);
                                        return Ok(());
                                    }
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                        }
                    }
                    self.lowering.emit_error(
                        expr.span,
                        "len() method is only supported on constant arrays during lowering",
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Use(mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            )),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }

                let receiver_operand = self.lower_operand(receiver, None)?;
                let mut lowered_args = Vec::with_capacity(args.len() + 1);
                let mut input_tys = Vec::with_capacity(args.len() + 1);
                lowered_args.push(receiver_operand.operand.clone());
                input_tys.push(receiver_operand.ty.clone());
                for arg in args {
                    let lowered = self.lower_operand(&arg.value, None)?;
                    input_tys.push(lowered.ty.clone());
                    lowered_args.push(lowered.operand);
                }

                let mut result_ty = expected_ty.clone();
                // `method_name_output_consensus` is maintained incrementally
                // at registration time (see its doc comment) instead of
                // rescanning every struct's whole method table here.
                if let Some(Some(output)) = self
                    .lowering
                    .mir_package
                    .borrow()
                    .method_name_output_consensus
                    .get(method_name.as_str())
                {
                    result_ty = output.clone();
                }
                let sig = mir::FunctionSig {
                    inputs: input_tys,
                    output: result_ty.clone(),
                };
                let sanitized_sig = self.lowering.sanitize_function_sig(&sig);
                let arg_types = sig.inputs.clone();

                for (idx, expected_input) in sanitized_sig.inputs.iter().enumerate() {
                    if let Some(original_ty) = arg_types.get(idx) {
                        if HirToMirLowerer::is_unit_ty(original_ty)
                            && matches!(
                                expected_input.kind,
                                TyKind::Ref(_, _, _) | TyKind::RawPtr(_)
                            )
                        {
                            lowered_args[idx] = mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: expected_input.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Null,
                            });
                        }
                    }

                    if let Some(operand) = lowered_args.get_mut(idx) {
                        match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                                if (place.local as usize) < self.locals.len() {
                                    let existing = self.locals[place.local as usize].ty.clone();
                                    if HirToMirLowerer::is_unit_ty(&existing)
                                        || matches!(
                                            existing.kind,
                                            TyKind::Infer(_) | TyKind::Error(_)
                                        )
                                    {
                                        self.locals[place.local as usize].ty =
                                            expected_input.clone();
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }

                let Some(def_id) = self.lowering.typeck_method_resolution(expr.hir_id.clone())
                else {
                    self.lowering
                        .emit_error(expr.span, "method call has no resolved definition");
                    return Ok(());
                };
                let literal = mir::ConstantKind::FnDef(def_id, Vec::new());
                let func_operand = mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: self.lowering.function_pointer_ty(&sanitized_sig),
                    user_ty: None,
                    literal,
                });

                let continue_block = self.new_block();
                let destination = Some((place.clone(), continue_block));
                self.blocks[self.current_block as usize].terminator = Some(mir::Terminator {
                    source_info: expr.span,
                    kind: mir::TerminatorKind::Call {
                        func: func_operand,
                        args: lowered_args,
                        destination: destination.clone(),
                        cleanup: self.current_unwind_target,
                        from_hir_call: true,
                        fn_span: expr.span,
                    },
                });

                self.current_block = continue_block;
                if (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                }
                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                    self.local_structs.insert(place.local, struct_def);
                }

                return Ok(());
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, Some((place, expected_ty.clone())))?;
            }
            hir::ExprKind::Array(elements) => {
                if self.is_map_container(expected_ty) {
                    let mut entries = Vec::with_capacity(elements.len());
                    let mut key_ty: Option<Ty> = None;
                    let mut value_ty: Option<Ty> = None;

                    for element in elements {
                        let hir::ExprKind::Array(entry) = &element.kind else {
                            self.lowering
                                .emit_error(element.span, "HashMap literal expects array entries");
                            continue;
                        };
                        if entry.len() != 2 {
                            self.lowering.emit_error(
                                element.span,
                                "HashMap literal expects array entries of length 2",
                            );
                            continue;
                        }
                        let key_operand = self.lower_operand(&entry[0], None)?;
                        let value_operand = self.lower_operand(&entry[1], None)?;
                        if key_ty.is_none() {
                            key_ty = Some(key_operand.ty.clone());
                        }
                        if value_ty.is_none() {
                            value_ty = Some(value_operand.ty.clone());
                        }
                        entries.push((key_operand.operand, value_operand.operand));
                    }

                    let key_ty = key_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let value_ty = value_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::Map {
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                        len: entries.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerMapLiteral {
                                kind: kind.clone(),
                                entries,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                if self.is_list_container(expected_ty) {
                    // Derive the element type from the annotated destination
                    // type up front (mirroring the fixed-size-array fallback
                    // below), instead of letting every element default to
                    // whatever type its own literal infers to regardless of
                    // what `expected_ty`'s element type actually is.
                    let declared_elem_ty = self.expect_array_element_ty(expected_ty);
                    let mut operands = Vec::with_capacity(elements.len());
                    let mut elem_ty: Option<Ty> = declared_elem_ty.clone();
                    for element in elements {
                        let lowered = self.lower_operand(element, declared_elem_ty.as_ref())?;
                        if elem_ty.is_none() {
                            elem_ty = Some(lowered.ty.clone());
                        }
                        operands.push(lowered.operand);
                    }

                    let elem_ty = elem_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::List {
                        elem_ty: elem_ty.clone(),
                        len: operands.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerLiteral {
                                kind: kind.clone(),
                                elements: operands,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                let mut element_ty = self.expect_array_element_ty(expected_ty);
                let mut operands = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());
                let mut heterogeneous = false;
                if let Some(elem_ty) = element_ty.clone() {
                    for element in elements {
                        let lowered = self.lower_operand(element, Some(&elem_ty))?;
                        if lowered.ty != elem_ty {
                            heterogeneous = true;
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                } else {
                    for element in elements {
                        let lowered = self.lower_operand(element, None)?;
                        if element_ty.is_none() {
                            element_ty = Some(lowered.ty.clone());
                        } else if let Some(existing) = element_ty.as_ref() {
                            if &lowered.ty != existing {
                                heterogeneous = true;
                            }
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                }

                let expected_is_array = matches!(&expected_ty.kind, TyKind::Array(_, _))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _) if matches!(inner.kind, TyKind::Array(_, _))
                    );
                if heterogeneous && expected_is_array {
                    self.lowering
                        .emit_error(expr.span, "array literal elements have mismatched types");
                }

                let element_ty = element_ty.unwrap_or_else(|| {
                    self.lowering
                        .emit_error(expr.span, "array expression expected array type");
                    self.lowering.error_ty()
                });

                let expected_is_slice = matches!(&expected_ty.kind, TyKind::Slice(_))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _)
                            if matches!(inner.kind, TyKind::Slice(_))
                    );
                if (expected_is_slice || matches!(expected_ty.kind, TyKind::Error(_)))
                    && place.projection.is_empty()
                {
                    let array_ty = Ty {
                        kind: TyKind::Array(
                            Box::new(element_ty.clone()),
                            ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                                data: elements.len() as u128,
                                size: 8,
                            }))),
                        ),
                    };
                    if let Some(local) = self.locals.get_mut(place.local as usize) {
                        local.ty = array_ty;
                    }
                }

                let aggregate_kind = if heterogeneous && !expected_is_array {
                    if place.projection.is_empty() {
                        let tuple_ty = Ty {
                            kind: TyKind::Tuple(element_types.into_iter().map(Box::new).collect()),
                        };
                        if let Some(local) = self.locals.get_mut(place.local as usize) {
                            local.ty = tuple_ty;
                        }
                    }
                    mir::AggregateKind::Tuple
                } else {
                    mir::AggregateKind::Array(element_ty.clone())
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(aggregate_kind, operands),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let element_ty = self
                    .expect_array_element_ty(expected_ty)
                    .unwrap_or_else(|| {
                        self.lowering
                            .emit_error(expr.span, "array repeat expression expected array type");
                        self.lowering.error_ty()
                    });

                let lowered_elem = self.lower_operand(elem, Some(&element_ty))?;
                let repeat_len = match self.evaluate_array_length(len) {
                    Some(len) => len,
                    None => {
                        self.lowering
                            .emit_error(len.span, "array repeat length must be a constant integer");
                        0
                    }
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Repeat(lowered_elem.operand, repeat_len),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::Tuple(elements) => {
                let mut operands = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());
                for element in elements {
                    let lowered = self.lower_operand(element, None)?;
                    element_types.push(lowered.ty.clone());
                    operands.push(lowered.operand);
                }
                if place.projection.is_empty() {
                    let tuple_ty = Ty {
                        kind: TyKind::Tuple(element_types.into_iter().map(Box::new).collect()),
                    };
                    if let Some(local) = self.locals.get_mut(place.local as usize) {
                        local.ty = tuple_ty;
                    }
                }
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                    ),
                };
                self.push_statement(statement);
            }
            _ => {
                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating expression {:?} as unit during MIR assignment",
                        expr.kind
                    ),
                );
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                };
                self.push_statement(statement);
            }
        }

        Ok(())
    }
}
