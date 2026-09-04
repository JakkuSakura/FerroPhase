use super::body::BodyBuilder;
use super::body::LoopDestination;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{Ty, TyKind};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn lower_block(&mut self, block: &hir::Block) -> Result<()> {
        self.lower_block_impl(block, true)
    }

    pub(super) fn lower_block_as_statement(&mut self, block: &hir::Block) -> Result<()> {
        self.lower_block_impl(block, false)
    }

    pub(super) fn lower_block_impl(&mut self, block: &hir::Block, is_tail: bool) -> Result<()> {
        let scope_depth = self.defer_scopes.len();
        // Name-based fallback resolution is only needed while lowering this
        // lexical block.  Preserve the outer bindings and restore them when
        // the block is complete so an inner shadow does not leak into later
        // expressions (HIR paths normally carry a Local resolution, but a
        // few synthesized paths rely on this fallback map).
        let fallback_locals_before = self.fallback_locals.clone();
        self.defer_scopes.push(DeferScope {
            deferred: Vec::new(),
        });

        let mut tail_expr = block.expr.as_deref();
        let mut stmt_slice = block.stmts.as_slice();
        if tail_expr.is_none() {
            if let Some(last) = block.stmts.last() {
                if let hir::StmtKind::Expr(expr) = &last.kind {
                    tail_expr = Some(expr);
                    stmt_slice = &block.stmts[..block.stmts.len().saturating_sub(1)];
                }
            }
        }

        for stmt in stmt_slice {
            self.lower_stmt(stmt)?;
            if self.control_flow_emitted {
                break;
            }
        }

        if !self.control_flow_emitted {
            if let Some(expr) = tail_expr {
                if is_tail {
                    if let hir::ExprKind::Block(inner) = &expr.kind {
                        self.lower_block(inner)?;
                    } else {
                        self.lower_tail_expr(expr)?;
                    }
                } else {
                    self.lower_expr_as_statement(expr)?;
                }
            }
        }

        if self.defer_scopes.len() > scope_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
        }

        self.fallback_locals = fallback_locals_before;

        Ok(())
    }

    pub(super) fn run_popped_deferred(&mut self, scope: DeferScope) -> Result<()> {
        for deferred in scope.deferred.into_iter().rev() {
            self.control_flow_emitted = false;
            self.lower_expr_as_statement(&deferred)?;
            if self.control_flow_emitted {
                break;
            }
        }
        Ok(())
    }

    pub(super) fn unwind_defer_scopes_to(&mut self, target_depth: usize) -> Result<()> {
        while self.defer_scopes.len() > target_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
            if self.control_flow_emitted {
                return Ok(());
            }
        }
        Ok(())
    }

    pub(super) fn with_unwind_target<T>(
        &mut self,
        unwind_target: Option<mir::BasicBlockId>,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let saved = self.current_unwind_target;
        self.current_unwind_target = unwind_target;
        let result = f(self);
        self.current_unwind_target = saved;
        result
    }

    pub(super) fn lower_try_expr(
        &mut self,
        expr: &hir::Expr,
        expr_try: &hir::TryExpr,
        destination: Option<(mir::Place, Ty)>,
        as_statement: bool,
    ) -> Result<()> {
        let outer_scope_depth = self.defer_scopes.len();
        if let Some(finally_expr) = expr_try.finally.as_ref() {
            self.defer_scopes.push(DeferScope {
                deferred: vec![finally_expr.as_ref().clone()],
            });
        }

        let join_block = self.new_block();
        let panic_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(panic_block as usize) {
            block.is_cleanup = true;
        }

        self.control_flow_emitted = false;
        self.with_unwind_target(Some(panic_block), |this| match &destination {
            Some((place, ty)) if !as_statement && expr_try.elze.is_none() => {
                this.lower_expr_into_place(&expr_try.expr, place.clone(), ty)
            }
            _ => this.lower_expr_as_statement(&expr_try.expr),
        })?;

        if !self.control_flow_emitted {
            if let Some(elze) = expr_try.elze.as_ref() {
                self.control_flow_emitted = false;
                match &destination {
                    Some((place, ty)) if !as_statement => {
                        self.lower_expr_into_place(elze, place.clone(), ty)?;
                    }
                    _ => self.lower_expr_as_statement(elze)?,
                }
            }
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: expr.span,
                    kind: mir::TerminatorKind::Goto { target: join_block },
                });
            }
        }

        let outer_unwind = self.current_unwind_target;
        let mut next_catch_block = panic_block;
        for (idx, catch) in expr_try.catches.iter().enumerate() {
            self.current_block = next_catch_block;
            let fallback_block = if idx + 1 < expr_try.catches.len() {
                let block = self.new_block();
                if let Some(data) = self.blocks.get_mut(block as usize) {
                    data.is_cleanup = true;
                }
                Some(block)
            } else {
                None
            };

            if let Some(pat) = &catch.pat {
                let panic_value_local =
                    self.allocate_temp(self.lowering.raw_string_ptr_ty(), catch.body.span);
                self.push_statement(mir::Statement {
                    source_info: catch.body.span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(panic_value_local),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: catch.body.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Str(
                                "<panic payload unavailable>".to_string(),
                            ),
                        })),
                    ),
                });
                self.bind_pattern(
                    pat,
                    panic_value_local,
                    Some(&self.lowering.raw_string_ptr_ty()),
                );
            }

            self.control_flow_emitted = false;
            self.with_unwind_target(fallback_block, |this| match &destination {
                Some((place, ty)) if !as_statement => {
                    this.lower_expr_into_place(&catch.body, place.clone(), ty)
                }
                _ => this.lower_expr_as_statement(&catch.body),
            })?;
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: catch.body.span,
                    kind: mir::TerminatorKind::Goto { target: join_block },
                });
            }

            if let Some(block) = fallback_block {
                next_catch_block = block;
            }
        }

        self.current_block = next_catch_block;
        if expr_try.catches.is_empty()
            || self.blocks[self.current_block as usize]
                .terminator
                .is_none()
        {
            self.with_unwind_target(outer_unwind, |this| this.lower_panic(expr.span, &[]))?;
        }

        self.current_block = join_block;
        self.control_flow_emitted = false;
        if self.defer_scopes.len() > outer_scope_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
        }

        Ok(())
    }

    pub(super) fn lower_let_expr(
        &mut self,
        pat: &hir::Pat,
        ty: &hir::TypeExpr,
        init: &Option<Box<hir::Expr>>,
        span: Span,
    ) -> Result<()> {
        let init_span = init.as_ref().map(|expr| expr.span).unwrap_or(span);
        let ty_is_infer = matches!(ty.kind, hir::TypeExprKind::Infer | hir::TypeExprKind::Error);
        let declared_ty = if ty_is_infer {
            None
        } else {
            Some(self.lower_type_expr(ty))
        };
        let mut storage_ty = declared_ty.clone();
        let annotated_enum_def = if ty_is_infer {
            None
        } else if let hir::TypeExprKind::Path(path) = &ty.kind {
            if let hir::Res::Def(def_id) = &path.res_ref() {
                if self
                    .lowering
                    .mir_package
                    .borrow()
                    .enum_defs
                    .contains_key(def_id)
                {
                    Some(def_id.clone())
                } else {
                    None
                }
            } else {
                if let Some(seg) = path.segments().last() {
                    let name = seg.ident.as_str();
                    self.lowering
                        .mir_package
                        .borrow()
                        .enum_defs_by_name
                        .get(name)
                        .cloned()
                } else {
                    None
                }
            }
        } else {
            None
        };
        if !ty_is_infer {
            if let hir::TypeExprKind::Path(path) = &ty.kind {
                if let hir::Res::Def(def_id) = &path.res_ref() {
                    if self
                        .lowering
                        .mir_package
                        .borrow()
                        .enum_defs
                        .contains_key(def_id)
                    {
                        let args = path
                            .path()
                            .and_then(|path| {
                                path.segments
                                    .iter()
                                    .find_map(|segment| segment.args.as_ref())
                            })
                            .map(|args| self.lowering.lower_generic_args(Some(args), init_span))
                            .unwrap_or_default();
                        let layout = if args.is_empty() {
                            self.lowering.enum_layout_for_def(def_id.clone(), init_span)
                        } else {
                            self.lowering
                                .enum_layout_for_instance(def_id.clone(), &args, init_span)
                        };
                        if let Some(layout) = layout {
                            storage_ty = Some(self.lowering.nominal_enum_ty(&layout));
                        }
                    }
                }
            }
        }

        let implicit_ty = init
            .as_deref()
            .map(|expr| self.implicit_local_init_ty(expr))
            .transpose()?;
        let local_ty = storage_ty
            .as_ref()
            .or(implicit_ty.as_ref())
            .ok_or_else(|| fp_core::error::Error::from("local declaration has no type"))?;
        let mut decl = self.lowering.make_local_decl(local_ty, init_span);
        decl.local_info = mir::LocalInfo::User(());

        if let hir::PatKind::Binding { mutable, .. } = &pat.kind {
            if *mutable {
                decl.mutability = mir::Mutability::Mut;
            }
        }

        let local_id = self.push_local(decl);
        self.bind_pattern(pat, local_id, Some(local_ty));

        if let Some(init_expr) = init {
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

    pub(super) fn lower_loop_expr(
        &mut self,
        span: Span,
        block: &hir::Block,
        destination: Option<LoopDestination>,
        break_value_allowed: bool,
    ) -> Result<()> {
        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_header = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: header_block,
            },
        };
        self.set_current_terminator(goto_header);

        self.current_block = header_block;
        let goto_body = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: body_block },
        };
        self.set_current_terminator(goto_body);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: header_block,
            break_destination: context_destination,
            break_value_allowed,
            defer_scope_depth: self.defer_scopes.len(),
        });

        self.current_block = body_block;
        self.lower_block_as_statement(block)?;

        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: header_block,
                },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        Ok(())
    }

    pub(super) fn lower_while_expr(
        &mut self,
        span: Span,
        cond: &hir::Expr,
        block: &hir::Block,
        destination: Option<LoopDestination>,
    ) -> Result<()> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_cond = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: cond_block },
        };
        self.set_current_terminator(goto_cond);

        self.current_block = cond_block;
        let bool_ty = Ty { kind: TyKind::Bool };
        let cond_operand = self.lower_condition_operand(cond)?;
        let switch = mir::Terminator {
            source_info: cond.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: cond_operand,
                switch_ty: bool_ty.clone(),
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![body_block],
                    otherwise: exit_block,
                },
            },
        };
        self.set_current_terminator(switch);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: cond_block,
            break_destination: context_destination,
            break_value_allowed: false,
            defer_scope_depth: self.defer_scopes.len(),
        });

        self.current_block = body_block;
        self.lower_block(block)?;
        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto { target: cond_block },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        if let Some(dest) = destination.as_ref() {
            let assign_unit = mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    dest.place.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            };
            self.push_statement(assign_unit);
            if dest.place.projection.is_empty() {
                self.locals[dest.place.local as usize].ty = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
            }
        }

        Ok(())
    }

    pub(super) fn lower_break(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`break` used outside of a loop");
                return Ok(());
            }
        };
        let break_value = if let Some(value_expr) = value {
            let expected =
                context
                    .break_destination
                    .as_ref()
                    .and_then(|dest| match &dest.ty.kind {
                        TyKind::Tuple(elements) if elements.is_empty() => None,
                        TyKind::Error(_) => None,
                        _ => Some(&dest.ty),
                    });
            let (temp_place, temp_ty) = if let Some(expected_ty) = expected {
                let temp_local = self.allocate_temp(expected_ty.clone(), value_expr.span);
                let temp_place = mir::Place::from_local(temp_local);
                self.lower_expr_into_place(value_expr, temp_place.clone(), expected_ty)?;
                (temp_place, expected_ty.clone())
            } else {
                let operand = self.lower_operand(value_expr, None)?;
                let temp_local = self.allocate_temp(operand.ty.clone(), value_expr.span);
                let temp_place = mir::Place::from_local(temp_local);
                self.push_statement(mir::Statement {
                    source_info: value_expr.span,
                    kind: mir::StatementKind::Assign(
                        temp_place.clone(),
                        mir::Rvalue::Use(operand.operand),
                    ),
                });
                (temp_place, operand.ty)
            };
            Some((temp_place, temp_ty))
        } else {
            None
        };
        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(context.defer_scope_depth)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        if let Some((value_place, value_ty)) = break_value {
            if !context.break_value_allowed {
                self.lowering.emit_error(
                    span,
                    "`break` with a value is only supported inside `loop` expressions",
                );
            } else if let Some(dest) = context.break_destination.as_ref() {
                let statement = mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        dest.place.clone(),
                        mir::Rvalue::Use(mir::Operand::Copy(value_place)),
                    ),
                };
                self.push_statement(statement);
                if dest.place.projection.is_empty() {
                    self.locals[dest.place.local as usize].ty = value_ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(&value_ty) {
                        self.local_structs.insert(dest.place.local, struct_def);
                    }
                }
            } else {
                self.lowering.emit_error(
                    span,
                    "`break` with a value requires the surrounding loop to produce a value",
                );
            }
        } else if context.break_value_allowed {
            if let Some(dest) = context.break_destination.as_ref() {
                match &dest.ty.kind {
                    TyKind::Tuple(elements) if elements.is_empty() => {}
                    TyKind::Never => {}
                    _ => {
                        self.lowering.emit_error(
                            span,
                            "`break` without a value in a value-producing loop is not supported",
                        );
                    }
                }
            }
        }

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.break_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    pub(super) fn lower_continue(&mut self, span: Span) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`continue` used outside of a loop");
                return Ok(());
            }
        };
        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(context.defer_scope_depth)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.continue_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    pub(super) fn lower_return(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let return_place = mir::Place::from_local(0);
        let return_value = if let Some(value_expr) = value {
            let temp_local = self.allocate_temp(return_ty.clone(), value_expr.span);
            let temp_place = mir::Place::from_local(temp_local);
            self.lower_expr_into_place(value_expr, temp_place.clone(), &return_ty)?;
            Some(temp_place)
        } else {
            None
        };

        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(0)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        if let Some(value_place) = return_value {
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    return_place.clone(),
                    mir::Rvalue::Use(mir::Operand::Copy(value_place)),
                ),
            });
        } else {
            if !matches!(return_ty.kind, TyKind::Tuple(ref elems) if elems.is_empty()) {
                self.lowering
                    .emit_error(span, "`return` without a value requires unit return type");
            }
        }

        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Return,
        };
        self.set_current_terminator(terminator);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    pub(super) fn lower_stmt(&mut self, stmt: &hir::Stmt) -> Result<()> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => self.lower_local(local),
            hir::StmtKind::Item(item) => self.lower_inner_item(item),
            hir::StmtKind::Semi(expr) | hir::StmtKind::Expr(expr) => {
                self.lower_expr_statement(expr)
            }
        }
    }

    pub(super) fn lower_tail_expr(&mut self, expr: &hir::Expr) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let place = mir::Place::from_local(0);
        if HirToMirLowerer::is_unit_ty(&return_ty) {
            self.lower_expr_as_statement(expr)?;
            self.push_statement(mir::Statement {
                source_info: expr.span,
                kind: mir::StatementKind::Assign(
                    place,
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            });
            Ok(())
        } else {
            self.lower_expr_into_place(expr, place, &return_ty)
        }
    }

    pub(super) fn lower_match_expr(
        &mut self,
        span: Span,
        scrutinee: &hir::Expr,
        arms: &[hir::MatchArm],
        destination: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let scrutinee_info = self.lower_operand(scrutinee, None)?;
        let scrutinee_local = self.allocate_temp(scrutinee_info.ty.clone(), scrutinee.span);
        let scrutinee_place = mir::Place::from_local(scrutinee_local);
        self.push_statement(mir::Statement {
            source_info: scrutinee.span,
            kind: mir::StatementKind::Assign(
                scrutinee_place.clone(),
                mir::Rvalue::Use(scrutinee_info.operand),
            ),
        });

        let continue_block = self.new_block();
        let mut next_block = self.current_block;
        let mut fallthrough_block = None;

        for (idx, arm) in arms.iter().enumerate() {
            let body_block = self.new_block();
            let is_last = idx == arms.len() - 1;
            let mut next_arm_block = self.new_block();
            let always_matches = self.pattern_always_matches(&arm.pat);
            if is_last && always_matches {
                next_arm_block = continue_block;
            } else if is_last {
                fallthrough_block = Some(next_arm_block);
            }

            self.current_block = next_block;
            if always_matches {
                self.set_current_terminator(mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::Goto { target: body_block },
                });
            } else {
                let cond_operand = self.lower_match_condition(
                    &arm.pat,
                    &scrutinee_place,
                    &scrutinee_info.ty,
                    span,
                )?;
                let switch = mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![body_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(switch);
            }

            self.current_block = body_block;
            self.match_binding_undo_log = Some(Vec::new());
            self.bind_match_pattern(&arm.pat, &scrutinee_place, &scrutinee_info.ty, span);
            let undo_log = self.match_binding_undo_log.take().unwrap_or_default();

            if let Some(guard) = &arm.guard {
                let guard_operand = self.lower_condition_operand(guard)?;
                let guard_block = self.new_block();
                let guard_switch = mir::Terminator {
                    source_info: guard.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: guard_operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![guard_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(guard_switch);
                self.current_block = guard_block;
            }

            self.lower_expr_into_place(&arm.body, destination.clone(), expected_ty)?;
            self.set_current_terminator(mir::Terminator {
                source_info: arm.body.span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
            for entry in undo_log.into_iter().rev() {
                match entry {
                    MatchBindingUndo::LocalMap(hir_id, Some(prev)) => {
                        self.local_map.insert(hir_id, prev);
                    }
                    MatchBindingUndo::LocalMap(hir_id, None) => {
                        self.local_map.remove(&hir_id);
                    }
                    MatchBindingUndo::Fallback(name, Some(prev)) => {
                        self.fallback_locals.insert(name, prev);
                    }
                    MatchBindingUndo::Fallback(name, None) => {
                        self.fallback_locals.remove(&name);
                    }
                }
            }

            next_block = next_arm_block;
        }

        if let Some(fallthrough) = fallthrough_block {
            self.current_block = fallthrough;
            self.lowering
                .emit_warning(span, "match arms did not cover all cases");
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    destination.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            });
            self.set_current_terminator(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = continue_block;
        Ok(())
    }
}
