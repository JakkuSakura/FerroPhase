use super::body::BodyBuilder;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{Ty, TyKind};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn lower_local(&mut self, local: &hir::Local) -> Result<()> {
        let init_span = local
            .init
            .as_ref()
            .map(|expr| expr.span)
            .unwrap_or(self.span);

        let mut declared_ty = local
            .ty
            .as_ref()
            .filter(|ty_expr| {
                !matches!(
                    ty_expr.kind,
                    hir::TypeExprKind::Infer | hir::TypeExprKind::Error
                )
            })
            .map(|ty_expr| self.lower_type_expr(ty_expr));
        let annotated_enum_def = local.ty.as_ref().and_then(|ty_expr| {
            let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
                return None;
            };
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self
                    .lowering
                    .mir_package
                    .borrow()
                    .enum_defs
                    .contains_key(def_id)
                {
                    return Some(def_id.clone());
                }
            }
            let name = path.segments.last()?.name.as_str();
            self.lowering
                .mir_package
                .borrow()
                .enum_defs
                .values()
                .find(|enm| enm.name == name)
                .map(|enm| enm.def_id.clone())
        });
        if let Some(ty_expr) = local.ty.as_ref() {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if self
                        .lowering
                        .mir_package
                        .borrow()
                        .enum_defs
                        .contains_key(def_id)
                    {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| self.lowering.lower_generic_args(Some(args), init_span))
                            .unwrap_or_default();
                        let layout = if args.is_empty() {
                            self.lowering.enum_layout_for_def(def_id.clone(), init_span)
                        } else {
                            self.lowering
                                .enum_layout_for_instance(def_id.clone(), &args, init_span)
                        };
                        if let Some(layout) = layout {
                            declared_ty = Some(self.lowering.nominal_enum_ty(&layout));
                        }
                    }
                }
            }
        }

        let implicit_ty = if declared_ty.is_none() {
            local
                .init
                .as_ref()
                .map(|expr| self.implicit_local_init_ty(expr))
                .transpose()?
        } else {
            None
        };
        let local_ty = declared_ty
            .as_ref()
            .or(implicit_ty.as_ref())
            .ok_or_else(|| fp_core::error::Error::from("local declaration has no type"))?;
        let mut decl = self.lowering.make_local_decl(local_ty, init_span);
        decl.local_info = mir::LocalInfo::User(());

        if let hir::PatKind::Binding { mutable, .. } = &local.pat.kind {
            if *mutable {
                decl.mutability = mir::Mutability::Mut;
            }
        }

        let local_id = self.push_local(decl);
        self.bind_pattern(&local.pat, local_id, Some(local_ty));

        if let Some(init_expr) = &local.init {
            self.update_null_tracking(
                mir::Place::from_local(local_id),
                declared_ty.as_ref(),
                init_expr,
            );
            self.lower_assignment(
                local_id,
                declared_ty.as_ref(),
                annotated_enum_def,
                init_expr,
            )?;
        }

        Ok(())
    }

    pub(super) fn implicit_local_init_ty(&mut self, expr: &hir::Expr) -> Result<Ty> {
        let hir_ty = self
            .lowering
            .hir_program
            .expr_type(expr.hir_id.clone())
            .ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "missing HIR type for local initializer {} ({:?})",
                    expr.hir_id, expr.kind
                ))
            })?;
        let ty = super::expr::lower_hir_ty(&hir_ty).map_err(|error| {
            fp_core::error::Error::from(format!(
                "cannot lower cached HIR type `{hir_ty:?}` for local initializer {} ({:?}): {error}",
                expr.hir_id,
                expr.kind,
            ))
        })?;
        // Same concern as `lower_type_expr`'s typeck-cache check: the type
        // checker's cached type for this initializer expression comes from
        // type-checking the generic body once, abstractly — inside a
        // monomorphized specialization (`type_substs` populated), a bare
        // generic-param reference in that cached type (e.g. `*mut T`) is
        // unresolved and must be substituted, not returned as-is.
        if !self.type_substs.is_empty() && self.lowering.has_unresolved_ty(&ty) {
            return Ok(self.lowering.substitute_ty(&ty, &self.type_substs));
        }
        Ok(ty)
    }

    pub(super) fn lower_inner_item(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Struct(def) => {
                self.lowering
                    .register_struct(item.def_id.clone(), def, item.span);
            }
            hir::ItemKind::Enum(enm) => {
                self.lowering
                    .register_enum(item.def_id.clone(), enm, item.span);
            }
            hir::ItemKind::TypeAlias(_) => {}
            hir::ItemKind::Const(konst) => {
                if konst.is_host {
                    let ty = self.lowering.lower_type_expr(&konst.ty);
                    self.lowering.extra_items.push(mir::Item {
                        mir_id: self.lowering.mir_package.borrow_mut().fresh_mir_id(),
                        kind: mir::ItemKind::Static(mir::Static {
                            name: konst.name.clone().into(),
                            ty: ty.clone(),
                            init: mir::Operand::Constant(mir::Constant {
                                span: konst.body.value.span,
                                ty,
                                user_ty: None,
                                literal: mir::ConstantKind::Undef,
                            }),
                            mutability: if konst.mutable {
                                mir::Mutability::Mut
                            } else {
                                mir::Mutability::Not
                            },
                        }),
                    });
                    return Ok(());
                }
                self.lowering.ensure_item_lowered(item.def_id.clone())?;
                self.const_items.insert(item.def_id.clone(), konst.clone());
                // Emit a Static/ExecutableConst MIR item for every
                // non-unit const so cross-references between consts
                // work correctly in the interpreter and native codegen.
                let ty = self.lowering.lower_type_expr(&konst.ty);
                if !HirToMirLowerer::is_unit_ty(&ty) {
                    let mir_item = self.lowering.lower_const(item.def_id.clone(), konst)?;
                    self.lowering.extra_items.push(mir_item);
                }
            }
            hir::ItemKind::Impl(impl_block) => {
                self.lowering.lower_impl(item, impl_block, None)?;
            }
            hir::ItemKind::Function(function) => {
                let (mir_item, body_id, body) = self.lowering.lower_function(item, function)?;
                self.lowering.extra_items.push(mir_item);
                self.lowering.extra_bodies.push((body_id, body));
            }
            hir::ItemKind::Query(_) => {}
            hir::ItemKind::Trait(_) => {}
            hir::ItemKind::Expr(expr) => {
                self.lower_expr_statement(expr)?;
            }
        }
        Ok(())
    }

    pub(super) fn lower_expr_statement(&mut self, expr: &hir::Expr) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Let(pat, ty, init) => {
                self.lower_let_expr(pat, ty, init, expr.span)?;
            }
            hir::ExprKind::Block(block) => {
                self.lower_block_as_statement(block)?;
            }
            hir::ExprKind::Assign(place_expr, value_expr) => {
                // `x[i] = v` where `x`'s type has a real `index_set`
                // method (e.g. `Vec<T>`'s `Index`-trait-style impl) isn't a
                // real addressable MIR place at all — the struct's memory
                // is behind a raw pointer field, reachable only through a
                // method call, not a field/array projection — so it can't
                // go through `lower_place` (see its `Index` projection
                // case bailing out for exactly this reason). Detect it
                // here, before `lower_place` gets a chance to report
                // "assignment target is not addressable", and dispatch to
                // a real method call instead. `typeck_expr_type` gives the
                // receiver's type on demand, without eagerly lowering every
                // other expr in the package up front.
                if let hir::ExprKind::Index(receiver, index) = &place_expr.kind {
                    let receiver_ty = self.lowering.typeck_expr_type(receiver.hir_id.clone());
                    if let Some(struct_def_id) = receiver_ty
                        .as_ref()
                        .and_then(|ty| self.real_indexable_struct_def_id(ty))
                    {
                        let unit_ty = Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        };
                        let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.call_real_method_into_place(
                            struct_def_id,
                            "index_set",
                            receiver,
                            &[index, value_expr],
                            place,
                            Some(&unit_ty),
                            expr.span,
                        )?;
                        return Ok(());
                    }
                }
                let place_info = match self.lower_place(place_expr)? {
                    Some(info) => info,
                    None => {
                        self.lowering
                            .emit_error(place_expr.span, "assignment target is not addressable");
                        return Ok(());
                    }
                };

                self.update_null_tracking(
                    place_info.place.clone(),
                    Some(&place_info.ty),
                    value_expr,
                );
                let expected_ty = place_info.ty.clone();
                self.lower_expr_into_place(value_expr, place_info.place, &expected_ty)?;
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, None)?;
            }
            hir::ExprKind::Loop(block) => {
                let temp_unit = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
                let temp_local = self.allocate_temp(temp_unit.clone(), expr.span);
                let destination = LoopDestination {
                    place: mir::Place::from_local(temp_local),
                    ty: temp_unit,
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                self.lower_if_statement(expr.span, cond, then_expr, else_expr)?;
            }
            hir::ExprKind::While(cond, block) => {
                self.lower_while_expr(expr.span, cond, block, None)?;
            }
            hir::ExprKind::Try(expr_try) => {
                self.lower_try_expr(expr, expr_try, None, true)?;
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
            _ => {
                // Evaluate then drop result
                let _ = self.lower_operand(expr, None)?;
            }
        }
        Ok(())
    }

    pub(super) fn lower_expr_as_statement(&mut self, expr: &hir::Expr) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Block(block) => self.lower_block_as_statement(block),
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                self.lower_if_statement(expr.span, cond, then_expr, else_expr)
            }
            _ => self.lower_expr_statement(expr),
        }
    }

    pub(super) fn lower_if_statement(
        &mut self,
        span: Span,
        cond: &hir::Expr,
        then_expr: &hir::Expr,
        else_expr: &Option<Box<hir::Expr>>,
    ) -> Result<()> {
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

        self.current_block = then_block;
        self.control_flow_emitted = false;
        self.lower_expr_as_statement(then_expr)?;
        if !self.control_flow_emitted
            && self.blocks[self.current_block as usize]
                .terminator
                .is_none()
        {
            self.set_current_terminator(mir::Terminator {
                source_info: then_expr.span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = else_block;
        if let Some(else_expr) = else_expr {
            self.control_flow_emitted = false;
            self.lower_expr_as_statement(else_expr)?;
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: else_expr.span,
                    kind: mir::TerminatorKind::Goto {
                        target: continue_block,
                    },
                });
            }
        } else {
            self.control_flow_emitted = false;
            self.set_current_terminator(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = continue_block;
        self.control_flow_emitted = false;
        Ok(())
    }

    pub(super) fn lower_assignment(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        annotated_enum_def: Option<hir::DefId>,
        expr: &hir::Expr,
    ) -> Result<()> {
        // Coerce enum payloads into their tagged layout when assigning from a place.
        let place_info = self.lower_place(expr)?;
        if let Some(place_info) = place_info {
            if let Some(enum_def) = annotated_enum_def {
                if let Some(layout) = self.lowering.enum_layout_for_def(enum_def, expr.span) {
                    if let Some((variant, layout)) = self.enum_variant_for_payload(
                        &layout.enum_ty,
                        &place_info.ty,
                        place_info.struct_def.clone(),
                    ) {
                        self.assign_enum_variant_from_place(
                            mir::Place::from_local(local_id),
                            &variant,
                            &layout,
                            Some(&layout.enum_ty),
                            place_info.place,
                            expr.span,
                        )?;
                        self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                        return Ok(());
                    }
                }
            }
            if let Some(expected_ty) = annotated_ty {
                if let Some((variant, layout)) = self.enum_variant_for_payload(
                    expected_ty,
                    &place_info.ty,
                    place_info.struct_def,
                ) {
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        place_info.place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
            }
        }
        if let Some(expected_ty) = annotated_ty {
            if self.enum_layout_for_ty(expected_ty, expr.span).is_some()
                && matches!(
                    expr.kind,
                    hir::ExprKind::Literal(_)
                        | hir::ExprKind::Index(_, _)
                        | hir::ExprKind::Cast(_, _)
                )
            {
                let value = self.lower_operand(expr, None)?;
                let payload_def = self.struct_def_from_ty(&value.ty);
                if let Some((variant, layout)) =
                    self.enum_variant_for_payload(expected_ty, &value.ty, payload_def)
                {
                    let payload_local = self.allocate_temp(value.ty.clone(), expr.span);
                    let payload_place = mir::Place::from_local(payload_local);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            payload_place.clone(),
                            mir::Rvalue::Use(value.operand),
                        ),
                    });
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        payload_place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
            }
        }
        if let hir::ExprKind::Struct(path, fields) = &expr.kind {
            self.lower_struct_literal(
                local_id,
                annotated_ty,
                expr.hir_id.clone(),
                path,
                fields,
                expr.span,
            )
        } else if let hir::ExprKind::Call(callee, args) = &expr.kind {
            let place = mir::Place::from_local(local_id);
            let ty = annotated_ty
                .cloned()
                .unwrap_or_else(|| self.locals[local_id as usize].ty.clone());
            if let Some(info) = self.lower_call(expr, callee, args, Some((place, ty.clone())))? {
                self.locals[local_id as usize].ty = info.ty.clone();
                if let Some(def_id) = info.struct_def {
                    self.local_structs.insert(local_id, def_id);
                }
            }
            Ok(())
        } else {
            let expected_ty = annotated_ty
                .cloned()
                .or_else(|| Some(self.locals[local_id as usize].ty.clone()));
            if let (
                Some(expected_ty),
                hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. },
            ) = (expected_ty.as_ref(), &expr.kind)
            {
                if self.is_list_container(expected_ty) || self.is_map_container(expected_ty) {
                    let place = mir::Place::from_local(local_id);
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                    return Ok(());
                }
            }
            let expected_ty = annotated_ty
                .cloned()
                .or_else(|| Some(self.locals[local_id as usize].ty.clone()));
            let value = self.lower_operand(expr, expected_ty.as_ref())?;
            let statement = mir::Statement {
                source_info: expr.span,
                kind: mir::StatementKind::Assign(
                    mir::Place::from_local(local_id),
                    mir::Rvalue::Use(value.operand),
                ),
            };
            self.push_statement(statement);
            let struct_def = expected_ty
                .as_ref()
                .and_then(|ty| self.struct_def_from_ty(ty))
                .or_else(|| self.struct_def_from_ty(&value.ty));
            if let Some(def_id) = struct_def {
                self.local_structs.insert(local_id, def_id);
            }
            // Prefer the destination's already-known `expected_ty` (always
            // populated above, either from the explicit annotation or the
            // local's own prior declared type) over `value.ty` — see the
            // identical reasoning in `lower_expr_into_place`'s
            // `Literal|Path|Index|FieldAccess|ConstBlock` group: a
            // comptime-frozen constant can lose its ADT identity on the
            // way to a `mir::Constant`, and `value.ty` would then wrongly
            // clobber a local whose real, declared type is already known.
            self.locals[local_id as usize].ty = expected_ty.unwrap_or_else(|| value.ty.clone());
            Ok(())
        }
    }
}
