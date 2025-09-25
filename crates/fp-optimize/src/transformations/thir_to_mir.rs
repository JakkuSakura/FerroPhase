use fp_core::error::Result;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use fp_core::types::{Ty, TyKind};
use fp_core::{mir, thir};
// use fp_core::ast::Visibility; // not used here
use std::collections::HashMap;

use super::IrTransform;

/// Generator for transforming THIR to MIR (Mid-level IR)
pub struct MirGenerator {
    next_mir_id: mir::MirId,
    next_local_id: u32,
    next_bb_id: u32,
    current_locals: HashMap<thir::ThirId, mir::LocalId>,
    current_blocks: Vec<mir::BasicBlockData>,
    local_decls: Vec<mir::LocalDecl>,
    loop_stack: Vec<LoopContext>,
    current_return_local: Option<mir::LocalId>,
}

struct ExprOutcome {
    place: mir::Place,
    block: mir::BasicBlockId,
}

#[derive(Clone, Copy)]
struct LoopContext {
    continue_block: mir::BasicBlockId,
    break_block: mir::BasicBlockId,
    break_result: Option<mir::LocalId>,
}

impl MirGenerator {
    /// Create a new MIR generator
    pub fn new() -> Self {
        Self {
            next_mir_id: 0,
            next_local_id: 0,
            next_bb_id: 0,
            current_locals: HashMap::new(),
            current_blocks: Vec::new(),
            local_decls: Vec::new(),
            loop_stack: Vec::new(),
            current_return_local: None,
        }
    }

    /// Transform a THIR program to MIR
    pub fn transform(&mut self, thir_program: thir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();

        for item in &thir_program.items {
            match &item.kind {
                thir::ItemKind::Function(func) => {
                    let (mir_func, maybe_body) =
                        self.transform_function_with_body(func.clone(), &thir_program)?;
                    if let Some((body_id, body)) = maybe_body {
                        mir_program.bodies.insert(body_id, body);
                    }
                    mir_program.items.push(mir::Item {
                        mir_id: self.next_id(),
                        kind: mir::ItemKind::Function(mir_func),
                    });
                }
                thir::ItemKind::Struct(_) => {
                    // Structs don't need MIR representation - they're handled at type level
                }
                thir::ItemKind::Const(_const_item) => {
                    // Skip const items for now
                }
                thir::ItemKind::Impl(_) => {
                    // Skip impl items for now
                }
            }
        }

        Ok(mir_program)
    }

    /// Transform a THIR function to MIR
    fn transform_function_with_body(
        &mut self,
        func: thir::Function,
        thir_program: &thir::Program,
    ) -> Result<(mir::Function, Option<(mir::BodyId, mir::Body)>)> {
        // Reset generator state for new function
        self.reset_for_new_function();

        // Create locals for parameters and return value
        let return_local = self.create_local(func.sig.output.clone());
        let mut param_locals = Vec::new();

        for param in &func.sig.inputs {
            let local_id = self.create_local(param.clone());
            param_locals.push(local_id);
        }

        // Track the return slot so `return` expressions can write into it.
        self.current_return_local = Some(return_local);

        let (body_id, body_opt) = if let Some(body_id) = func.body_id {
            if let Some(thir_body) = thir_program.bodies.get(&body_id) {
                let mir_body =
                    self.transform_body(thir_body.clone(), return_local, &param_locals)?;
                (mir::BodyId::new(body_id.0), Some(mir_body))
            } else {
                let mir_body =
                    self.create_external_function_body(return_local, param_locals.len())?;
                (mir::BodyId::new(0), Some(mir_body))
            }
        } else {
            // External function - minimal body
            let mir_body = self.create_external_function_body(return_local, param_locals.len())?;
            (mir::BodyId::new(0), Some(mir_body))
        };
        let mir_ty = self.transform_type(&func.sig.output);
        let inputs = param_locals
            .iter()
            .map(|&local_id| self.local_decls[local_id as usize].ty.clone())
            .collect();

        let func = mir::Function {
            sig: mir::FunctionSig {
                inputs,
                output: mir_ty,
            },
            body_id,
        };

        // Clear return slot tracking before leaving the function.
        self.current_return_local = None;

        Ok((func, body_opt.map(|b| (body_id, b))))
    }

    /// Transform a THIR body to MIR body
    fn transform_body(
        &mut self,
        thir_body: thir::Body,
        return_local: mir::LocalId,
        param_locals: &[mir::LocalId],
    ) -> Result<mir::Body> {
        // Create entry basic block
        let entry_bb = self.create_basic_block();

        for (idx, &local) in param_locals.iter().enumerate() {
            self.current_locals.insert(idx as thir::ThirId, local);
        }

        let base_thir_local = param_locals.len() as thir::ThirId;
        for (offset, local_decl) in thir_body.locals.iter().enumerate() {
            let mir_local = self.create_local(local_decl.ty.clone());
            self.current_locals
                .insert(base_thir_local + offset as thir::ThirId, mir_local);
        }

        self.current_return_local = Some(return_local);

        tracing::debug!(
            "Transforming THIR body with expression kind: {:?}",
            thir_body.value.kind
        );

        // Transform the body expression
        let ExprOutcome {
            place: result_place,
            block: current_block,
        } = self.transform_expr(thir_body.value, entry_bb)?;

        // Add return statement
        self.add_statement_to_block(
            current_block,
            mir::Statement {
                kind: mir::StatementKind::Assign(
                    mir::Place::from_local(return_local),
                    mir::Rvalue::Use(mir::Operand::Move(result_place)),
                ),
                source_info: Span::new(0, 0, 0),
            },
        );

        // Set terminator for entry block
        self.set_block_terminator(
            current_block,
            mir::Terminator {
                kind: mir::TerminatorKind::Return,
                source_info: Span::new(0, 0, 0),
            },
        );

        // let locals: Vec<mir::LocalDecl> = (0..self.next_local_id)
        //     .map(|_| mir::LocalDecl {
        //         mutability: mir::Mutability::Not,
        //         local_info: mir::LocalInfo::Other,
        //         internal: false,
        //         is_block_tail: None,
        //         ty: todo!(),
        //         user_ty: None,
        //         source_info: Span::new(0, 0, 0),
        //     })
        //     .collect();

        Ok(mir::Body {
            basic_blocks: self.current_blocks.clone(),
            locals: self.local_decls.clone(),
            arg_count: param_locals.len(),
            return_local,
            var_debug_info: Vec::new(),
            span: Span::new(0, 0, 0),
        })
    }

    /// Transform a THIR expression to MIR, returning the place where result is stored and the current block
    fn transform_expr(
        &mut self,
        expr: thir::Expr,
        current_bb: mir::BasicBlockId,
    ) -> Result<ExprOutcome> {
        match expr.kind {
            thir::ExprKind::Field { base, field_idx: _ } => {
                // Lower field selection by first lowering the base
                // For now, if the base lowered to a constant struct literal, extract constant field.
                // Since our types are placeholders, treat base as producing its fields directly when possible.
                let ExprOutcome {
                    place: base_place,
                    block,
                } = self.transform_expr(*base, current_bb)?;
                // Without a real aggregate model, emit a move of base and rely on earlier inlining to have simplified this.
                let temp_local = self.create_local(expr.ty);
                let place = mir::Place::from_local(temp_local);
                self.add_statement_to_block(
                    block,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Move(base_place)),
                        ),
                        source_info: expr.span,
                    },
                );
                Ok(ExprOutcome { place, block })
            }
            thir::ExprKind::Literal(lit) => {
                let temp_local = self.create_local(expr.ty);
                let place = mir::Place::from_local(temp_local);

                self.add_statement_to_block(
                    current_bb,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(self.literal_to_operand(lit, expr.span)),
                        ),
                        source_info: expr.span,
                    },
                );

                Ok(ExprOutcome {
                    place,
                    block: current_bb,
                })
            }
            thir::ExprKind::Path(item_ref) => {
                let mir_ty = self.transform_type(&expr.ty);
                // Use type information to distinguish between function references and global constants
                let constant_kind = if self.is_function_type(&expr.ty) {
                    mir::ConstantKind::Fn(item_ref.name.clone(), mir_ty)
                } else {
                    // If it's not a function type, treat it as a global constant
                    // Use unit type as placeholder since MIR Ty is defined as ()
                    mir::ConstantKind::Global(item_ref.name.clone(), mir_ty)
                };

                let temp_local = self.create_local(expr.ty);
                let place = mir::Place::from_local(temp_local);
                self.add_statement_to_block(
                    current_bb,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                user_ty: None,
                                literal: constant_kind,
                            })),
                        ),
                        source_info: expr.span,
                    },
                );
                Ok(ExprOutcome {
                    place,
                    block: current_bb,
                })
            }
            thir::ExprKind::Binary(op, lhs, rhs) => {
                let ExprOutcome {
                    place: lhs_place,
                    block: after_lhs,
                } = self.transform_expr(*lhs, current_bb)?;
                let ExprOutcome {
                    place: rhs_place,
                    block: after_rhs,
                } = self.transform_expr(*rhs, after_lhs)?;
                let result_local = self.create_local(expr.ty);
                let result_place = mir::Place::from_local(result_local);

                // Convert THIR BinOp to BinOpKind
                let binop_kind = self.convert_thir_binop_to_kind(op);

                self.add_statement_to_block(
                    after_rhs,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            result_place.clone(),
                            mir::Rvalue::BinaryOp(
                                self.transform_binary_op(binop_kind)?,
                                mir::Operand::Move(lhs_place),
                                mir::Operand::Move(rhs_place),
                            ),
                        ),
                        source_info: expr.span,
                    },
                );
                Ok(ExprOutcome {
                    place: result_place,
                    block: after_rhs,
                })
            }
            thir::ExprKind::Call { fun, args, .. } => {
                tracing::debug!("Processing Call with {} args", args.len());

                let callee_expr = *fun;
                let ExprOutcome {
                    place: callee_place,
                    block: mut current_block,
                } = self.transform_expr(callee_expr, current_bb)?;

                let mut arg_operands = Vec::new();
                for arg in args {
                    match &arg.kind {
                        thir::ExprKind::Literal(lit) => {
                            arg_operands.push(self.literal_to_operand(lit.clone(), arg.span));
                        }
                        _ => {
                            let ExprOutcome {
                                place: arg_place,
                                block,
                            } = self.transform_expr(arg, current_block)?;
                            current_block = block;
                            arg_operands.push(mir::Operand::Move(arg_place));
                        }
                    }
                }

                let result_local = self.create_local(expr.ty);
                let result_place = mir::Place::from_local(result_local);
                let cont_bb = self.create_basic_block();

                self.set_block_terminator(
                    current_block,
                    mir::Terminator {
                        kind: mir::TerminatorKind::Call {
                            func: mir::Operand::Move(callee_place),
                            args: arg_operands,
                            destination: Some((result_place.clone(), cont_bb)),
                            cleanup: None,
                            from_hir_call: true,
                            fn_span: expr.span,
                        },
                        source_info: expr.span,
                    },
                );

                Ok(ExprOutcome {
                    place: result_place,
                    block: cont_bb,
                })
            }
            thir::ExprKind::LogicalOp { op, lhs, rhs } => {
                let ExprOutcome {
                    place: lhs_place,
                    block: lhs_block,
                } = self.transform_expr(*lhs, current_bb)?;

                if self.block_has_terminator(lhs_block) {
                    return Ok(ExprOutcome {
                        place: lhs_place,
                        block: lhs_block,
                    });
                }

                let rhs_entry = self.create_basic_block();
                let short_block = self.create_basic_block();
                let join_block = self.create_basic_block();

                let result_local = self.create_local(expr.ty.clone());
                let result_place = mir::Place::from_local(result_local);

                let (switch_values, switch_targets, short_value) = match op {
                    thir::LogicalOp::And => (vec![1], vec![rhs_entry], false),
                    thir::LogicalOp::Or => (vec![0], vec![rhs_entry], true),
                };

                self.add_statement_to_block(
                    short_block,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            result_place.clone(),
                            mir::Rvalue::Use(self.bool_operand(short_value, expr.span)),
                        ),
                        source_info: expr.span,
                    },
                );
                self.ensure_goto(short_block, join_block, expr.span);

                self.set_block_terminator(
                    lhs_block,
                    mir::Terminator {
                        kind: mir::TerminatorKind::SwitchInt {
                            discr: mir::Operand::Move(lhs_place),
                            switch_ty: self.transform_type(&expr.ty),
                            targets: mir::SwitchTargets {
                                values: switch_values,
                                targets: switch_targets,
                                otherwise: short_block,
                            },
                        },
                        source_info: expr.span,
                    },
                );

                let ExprOutcome {
                    place: rhs_place,
                    block: rhs_block_end,
                } = self.transform_expr(*rhs, rhs_entry)?;

                if !self.block_has_terminator(rhs_block_end) {
                    self.add_statement_to_block(
                        rhs_block_end,
                        mir::Statement {
                            kind: mir::StatementKind::Assign(
                                result_place.clone(),
                                mir::Rvalue::Use(mir::Operand::Move(rhs_place)),
                            ),
                            source_info: expr.span,
                        },
                    );
                    self.ensure_goto(rhs_block_end, join_block, expr.span);
                }

                Ok(ExprOutcome {
                    place: result_place,
                    block: join_block,
                })
            }
            thir::ExprKind::If {
                cond,
                then,
                else_opt,
            } => {
                let cond_expr = *cond;
                let cond_ty = cond_expr.ty.clone();
                let ExprOutcome {
                    place: cond_place,
                    block: cond_block,
                } = self.transform_expr(cond_expr, current_bb)?;

                let then_block = self.create_basic_block();
                let else_block = self.create_basic_block();
                let join_block = self.create_basic_block();

                let result_local = self.create_local(expr.ty.clone());
                let result_place = mir::Place::from_local(result_local);

                self.set_block_terminator(
                    cond_block,
                    mir::Terminator {
                        kind: mir::TerminatorKind::SwitchInt {
                            discr: mir::Operand::Move(cond_place),
                            switch_ty: self.transform_type(&cond_ty),
                            targets: mir::SwitchTargets {
                                values: vec![1],
                                targets: vec![then_block],
                                otherwise: else_block,
                            },
                        },
                        source_info: expr.span,
                    },
                );

                let ExprOutcome {
                    place: then_place,
                    block: end_then_block,
                } = self.transform_expr(*then, then_block)?;
                if !self.block_has_terminator(end_then_block) {
                    self.add_statement_to_block(
                        end_then_block,
                        mir::Statement {
                            kind: mir::StatementKind::Assign(
                                result_place.clone(),
                                mir::Rvalue::Use(mir::Operand::Move(then_place)),
                            ),
                            source_info: expr.span,
                        },
                    );
                    self.ensure_goto(end_then_block, join_block, expr.span);
                }

                if let Some(else_expr) = else_opt {
                    let ExprOutcome {
                        place: else_place,
                        block: end_else_block,
                    } = self.transform_expr(*else_expr, else_block)?;
                    if !self.block_has_terminator(end_else_block) {
                        self.add_statement_to_block(
                            end_else_block,
                            mir::Statement {
                                kind: mir::StatementKind::Assign(
                                    result_place.clone(),
                                    mir::Rvalue::Use(mir::Operand::Move(else_place)),
                                ),
                                source_info: expr.span,
                            },
                        );
                        self.ensure_goto(end_else_block, join_block, expr.span);
                    }
                } else {
                    if !self.block_has_terminator(else_block) && self.is_unit_type(&expr.ty) {
                        self.add_statement_to_block(
                            else_block,
                            mir::Statement {
                                kind: mir::StatementKind::Assign(
                                    result_place.clone(),
                                    self.unit_rvalue(),
                                ),
                                source_info: expr.span,
                            },
                        );
                    }
                    self.ensure_goto(else_block, join_block, expr.span);
                }

                Ok(ExprOutcome {
                    place: result_place,
                    block: join_block,
                })
            }
            thir::ExprKind::Loop { body } => {
                let loop_body_block = self.create_basic_block();
                let exit_block = self.create_basic_block();

                let result_local = self.create_local(expr.ty.clone());
                let break_result = if self.is_never_type(&expr.ty) {
                    None
                } else {
                    Some(result_local)
                };

                self.loop_stack.push(LoopContext {
                    continue_block: loop_body_block,
                    break_block: exit_block,
                    break_result,
                });

                self.ensure_goto(current_bb, loop_body_block, expr.span);

                let ExprOutcome {
                    block: end_block, ..
                } = self.transform_expr(*body, loop_body_block)?;
                if end_block != exit_block && !self.block_has_terminator(end_block) {
                    self.ensure_goto(end_block, loop_body_block, expr.span);
                }

                self.loop_stack.pop();

                Ok(ExprOutcome {
                    place: mir::Place::from_local(result_local),
                    block: exit_block,
                })
            }
            thir::ExprKind::Break { value } => {
                if let Some(loop_ctx) = self.loop_stack.last().cloned() {
                    let mut block = current_bb;

                    if let Some(val_expr) = value {
                        let ExprOutcome {
                            place: break_place,
                            block: after_value,
                        } = self.transform_expr(*val_expr, block)?;
                        block = after_value;
                        if let Some(local) = loop_ctx.break_result {
                            self.add_statement_to_block(
                                block,
                                mir::Statement {
                                    kind: mir::StatementKind::Assign(
                                        mir::Place::from_local(local),
                                        mir::Rvalue::Use(mir::Operand::Move(break_place)),
                                    ),
                                    source_info: expr.span,
                                },
                            );
                        }
                    } else if let Some(local) = loop_ctx.break_result {
                        self.add_statement_to_block(
                            block,
                            mir::Statement {
                                kind: mir::StatementKind::Assign(
                                    mir::Place::from_local(local),
                                    self.unit_rvalue(),
                                ),
                                source_info: expr.span,
                            },
                        );
                    }

                    self.ensure_goto(block, loop_ctx.break_block, expr.span);

                    Ok(ExprOutcome {
                        place: loop_ctx
                            .break_result
                            .map(mir::Place::from_local)
                            .unwrap_or_else(|| {
                                mir::Place::from_local(self.create_local(expr.ty.clone()))
                            }),
                        block: loop_ctx.break_block,
                    })
                } else {
                    let temp_local = self.create_local(expr.ty);
                    Ok(ExprOutcome {
                        place: mir::Place::from_local(temp_local),
                        block: current_bb,
                    })
                }
            }
            thir::ExprKind::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last().cloned() {
                    let continue_block = loop_ctx.continue_block;
                    let break_result = loop_ctx.break_result;

                    self.ensure_goto(current_bb, continue_block, expr.span);

                    let place = match break_result {
                        Some(local) => mir::Place::from_local(local),
                        None => mir::Place::from_local(self.create_local(expr.ty.clone())),
                    };

                    Ok(ExprOutcome {
                        place,
                        block: continue_block,
                    })
                } else {
                    let temp_local = self.create_local(expr.ty);
                    Ok(ExprOutcome {
                        place: mir::Place::from_local(temp_local),
                        block: current_bb,
                    })
                }
            }
            thir::ExprKind::Return { value } => {
                let mut block = current_bb;

                if let Some(ret_expr) = value {
                    let ExprOutcome {
                        place: ret_place,
                        block: after_ret,
                    } = self.transform_expr(*ret_expr, block)?;
                    block = after_ret;
                    if let Some(return_local) = self.current_return_local {
                        self.add_statement_to_block(
                            block,
                            mir::Statement {
                                kind: mir::StatementKind::Assign(
                                    mir::Place::from_local(return_local),
                                    mir::Rvalue::Use(mir::Operand::Move(ret_place)),
                                ),
                                source_info: expr.span,
                            },
                        );
                    }
                }

                self.set_block_terminator(
                    block,
                    mir::Terminator {
                        kind: mir::TerminatorKind::Return,
                        source_info: expr.span,
                    },
                );

                let return_place = self
                    .current_return_local
                    .map(mir::Place::from_local)
                    .unwrap_or_else(|| mir::Place::from_local(self.create_local(expr.ty.clone())));

                Ok(ExprOutcome {
                    place: return_place,
                    block,
                })
            }
            thir::ExprKind::Match { scrutinee, arms } => {
                let ExprOutcome {
                    place: scrutinee_place,
                    block: mut current_block,
                } = self.transform_expr(*scrutinee, current_bb)?;

                let result_local = self.create_local(expr.ty.clone());
                let result_place = mir::Place::from_local(result_local);
                let join_block = self.create_basic_block();

                let mut handled = false;

                for arm in arms {
                    if arm.guard.is_some() {
                        tracing::warn!("THIR→MIR: match guards not yet supported");
                    }

                    match arm.pattern.kind {
                        thir::PatKind::Wild => {
                            let ExprOutcome {
                                place: arm_place,
                                block: arm_block,
                            } = self.transform_expr(arm.body, current_block)?;

                            if !self.block_has_terminator(arm_block) {
                                self.add_statement_to_block(
                                    arm_block,
                                    mir::Statement {
                                        kind: mir::StatementKind::Assign(
                                            result_place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Move(arm_place)),
                                        ),
                                        source_info: arm.span,
                                    },
                                );
                                self.ensure_goto(arm_block, join_block, arm.span);
                            }

                            handled = true;
                            current_block = join_block;
                            break;
                        }
                        thir::PatKind::Range { .. } => {
                            tracing::warn!(
                                "THIR→MIR: range patterns lowered as wildcard (no bounds check)"
                            );
                            let ExprOutcome {
                                place: arm_place,
                                block: arm_block,
                            } = self.transform_expr(arm.body, current_block)?;

                            if !self.block_has_terminator(arm_block) {
                                self.add_statement_to_block(
                                    arm_block,
                                    mir::Statement {
                                        kind: mir::StatementKind::Assign(
                                            result_place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Move(arm_place)),
                                        ),
                                        source_info: arm.span,
                                    },
                                );
                                self.ensure_goto(arm_block, join_block, arm.span);
                            }

                            handled = true;
                            current_block = join_block;
                            break;
                        }
                        thir::PatKind::Constant { ref value } => {
                            // Create blocks for this arm's success and the next candidate.
                            let arm_block_id = self.create_basic_block();
                            let next_block_id = self.create_basic_block();

                            self.add_statement_to_block(
                                current_block,
                                mir::Statement {
                                    kind: mir::StatementKind::Assign(
                                        result_place.clone(),
                                        self.default_rvalue_for_type(&expr.ty, arm.span),
                                    ),
                                    source_info: arm.span,
                                },
                            );

                            self.set_block_terminator(
                                current_block,
                                mir::Terminator {
                                    kind: mir::TerminatorKind::SwitchInt {
                                        discr: mir::Operand::Copy(scrutinee_place.clone()),
                                        switch_ty: self.transform_type(&arm.pattern.ty),
                                        targets: mir::SwitchTargets {
                                            values: vec![self.constant_to_u128(value)],
                                            targets: vec![arm_block_id],
                                            otherwise: next_block_id,
                                        },
                                    },
                                    source_info: arm.span,
                                },
                            );

                            let ExprOutcome {
                                place: arm_place,
                                block: block_after_arm,
                            } = self.transform_expr(arm.body, arm_block_id)?;

                            if !self.block_has_terminator(block_after_arm) {
                                self.add_statement_to_block(
                                    block_after_arm,
                                    mir::Statement {
                                        kind: mir::StatementKind::Assign(
                                            result_place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Move(arm_place)),
                                        ),
                                        source_info: arm.span,
                                    },
                                );
                                self.ensure_goto(block_after_arm, join_block, arm.span);
                            }

                            current_block = next_block_id;
                        }
                        thir::PatKind::Binding { var, .. } => {
                            // Bind scrutinee into the pattern local before evaluating the body.
                            let bound_local = self.get_or_create_local(var, arm.pattern.ty.clone());
                            self.add_statement_to_block(
                                current_block,
                                mir::Statement {
                                    kind: mir::StatementKind::Assign(
                                        mir::Place::from_local(bound_local),
                                        mir::Rvalue::Use(mir::Operand::Copy(
                                            scrutinee_place.clone(),
                                        )),
                                    ),
                                    source_info: arm.span,
                                },
                            );

                            let ExprOutcome {
                                place: arm_place,
                                block: arm_block,
                            } = self.transform_expr(arm.body, current_block)?;

                            if !self.block_has_terminator(arm_block) {
                                self.add_statement_to_block(
                                    arm_block,
                                    mir::Statement {
                                        kind: mir::StatementKind::Assign(
                                            result_place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Move(arm_place)),
                                        ),
                                        source_info: arm.span,
                                    },
                                );
                                self.ensure_goto(arm_block, join_block, arm.span);
                            }

                            handled = true;
                            current_block = join_block;
                            break;
                        }
                        _ => {
                            tracing::warn!(
                                "THIR→MIR: pattern kind {:?} not supported; falling back to wildcard handling",
                                arm.pattern.kind
                            );
                            let ExprOutcome {
                                place: arm_place,
                                block: arm_block,
                            } = self.transform_expr(arm.body, current_block)?;

                            if !self.block_has_terminator(arm_block) {
                                self.add_statement_to_block(
                                    arm_block,
                                    mir::Statement {
                                        kind: mir::StatementKind::Assign(
                                            result_place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Move(arm_place)),
                                        ),
                                        source_info: arm.span,
                                    },
                                );
                                self.ensure_goto(arm_block, join_block, arm.span);
                            }

                            handled = true;
                            current_block = join_block;
                            break;
                        }
                    }
                }

                if !handled && !self.block_has_terminator(current_block) {
                    self.add_statement_to_block(
                        current_block,
                        mir::Statement {
                            kind: mir::StatementKind::Assign(
                                result_place.clone(),
                                self.default_rvalue_for_type(&expr.ty, expr.span),
                            ),
                            source_info: expr.span,
                        },
                    );
                    self.ensure_goto(current_block, join_block, expr.span);
                }

                Ok(ExprOutcome {
                    place: result_place,
                    block: join_block,
                })
            }
            thir::ExprKind::Scope { value, .. } => self.transform_expr(*value, current_bb),
            thir::ExprKind::Use(inner) => self.transform_expr(*inner, current_bb),
            thir::ExprKind::Let {
                expr: value_expr,
                pat,
            } => {
                let ExprOutcome {
                    place: value_place,
                    block: current_block,
                } = self.transform_expr(*value_expr, current_bb)?;

                if let Some(local_id) = self.binding_local_from_pattern(&pat) {
                    let local = self.get_or_create_local(local_id, pat.ty.clone());
                    self.add_statement_to_block(
                        current_block,
                        mir::Statement {
                            kind: mir::StatementKind::Assign(
                                mir::Place::from_local(local),
                                mir::Rvalue::Use(mir::Operand::Move(value_place)),
                            ),
                            source_info: pat.span,
                        },
                    );
                }

                let result_local = self.create_local(expr.ty.clone());
                let result_place = mir::Place::from_local(result_local);

                let assign_rvalue = match expr.ty.kind {
                    TyKind::Bool => mir::Rvalue::Use(self.bool_operand(true, expr.span)),
                    TyKind::Tuple(ref elems) if elems.is_empty() => self.unit_rvalue(),
                    _ => mir::Rvalue::Use(self.bool_operand(true, expr.span)),
                };

                self.add_statement_to_block(
                    current_block,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(result_place.clone(), assign_rvalue),
                        source_info: expr.span,
                    },
                );

                Ok(ExprOutcome {
                    place: result_place,
                    block: current_block,
                })
            }
            thir::ExprKind::VarRef { id } => {
                let local = self.get_or_create_local(id, expr.ty.clone());
                Ok(ExprOutcome {
                    place: mir::Place::from_local(local),
                    block: current_bb,
                })
            }
            thir::ExprKind::Block(block) => {
                let thir::Block {
                    stmts,
                    expr: tail_expr,
                    ..
                } = block;

                let mut current_block = current_bb;
                let stmt_count = stmts.len();
                let has_tail_expr = tail_expr.is_some();

                tracing::debug!("Processing block with {} statements", stmt_count);
                for (index, stmt) in stmts.into_iter().enumerate() {
                    current_block = self.transform_stmt(stmt, current_block)?;

                    let more_to_process = index + 1 < stmt_count || has_tail_expr;
                    if more_to_process && self.block_has_terminator(current_block) {
                        current_block = self.create_basic_block();
                    }
                }

                if let Some(final_expr) = tail_expr {
                    self.transform_expr(*final_expr, current_block)
                } else {
                    let temp_local = self.create_local(expr.ty);
                    Ok(ExprOutcome {
                        place: mir::Place::from_local(temp_local),
                        block: current_block,
                    })
                }
            }
            _ => {
                // Default case for unhandled expressions
                let temp_local = self.create_local(expr.ty);
                Ok(ExprOutcome {
                    place: mir::Place::from_local(temp_local),
                    block: current_bb,
                })
            }
        }
    }

    fn transform_stmt(
        &mut self,
        stmt: thir::Stmt,
        current_bb: mir::BasicBlockId,
    ) -> Result<mir::BasicBlockId> {
        match stmt.kind {
            thir::StmtKind::Expr(expr) => {
                let ExprOutcome { block, .. } = self.transform_expr(expr, current_bb)?;
                Ok(block)
            }
            thir::StmtKind::Let {
                initializer,
                pattern,
                ..
            } => {
                let mut block = current_bb;

                if let Some(init_expr) = initializer {
                    let ExprOutcome {
                        place: init_place,
                        block: after_init,
                    } = self.transform_expr(init_expr, block)?;
                    block = after_init;

                    if let Some(local_id) = self.binding_local_from_pattern(&pattern) {
                        let mir_local = self.get_or_create_local(local_id, pattern.ty.clone());
                        self.add_statement_to_block(
                            block,
                            mir::Statement {
                                kind: mir::StatementKind::Assign(
                                    mir::Place::from_local(mir_local),
                                    mir::Rvalue::Use(mir::Operand::Move(init_place)),
                                ),
                                source_info: pattern.span,
                            },
                        );
                    }
                }

                Ok(block)
            }
        }
    }

    fn binding_local_from_pattern(&self, pattern: &thir::Pat) -> Option<thir::LocalId> {
        match &pattern.kind {
            thir::PatKind::Binding { var, .. } => Some(*var),
            thir::PatKind::Deref { subpattern } => self.binding_local_from_pattern(subpattern),
            _ => None,
        }
    }

    /// Helper methods
    fn reset_for_new_function(&mut self) {
        self.next_local_id = 0;
        self.next_bb_id = 0;
        self.current_locals.clear();
        self.current_blocks.clear();
        self.local_decls.clear();
        self.loop_stack.clear();
        self.current_return_local = None;
    }

    fn create_local(&mut self, ty: Ty) -> mir::LocalId {
        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.local_decls.push(mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty,
            user_ty: None,
            source_info: Span::new(0, 0, 0),
        });
        local_id
    }

    fn get_or_create_local(&mut self, thir_id: thir::ThirId, ty: Ty) -> mir::LocalId {
        if let Some(&local) = self.current_locals.get(&thir_id) {
            local
        } else {
            let local = self.create_local(ty);
            self.current_locals.insert(thir_id, local);
            local
        }
    }

    fn create_basic_block(&mut self) -> mir::BasicBlockId {
        let bb_id = self.next_bb_id;
        self.next_bb_id += 1;

        self.current_blocks.push(mir::BasicBlockData {
            statements: Vec::new(),
            terminator: None,
            is_cleanup: false,
        });

        bb_id
    }

    fn add_statement_to_block(&mut self, bb_id: mir::BasicBlockId, stmt: mir::Statement) {
        if let Some(block) = self.current_blocks.get_mut(bb_id as usize) {
            block.statements.push(stmt);
        }
    }

    fn set_block_terminator(&mut self, bb_id: mir::BasicBlockId, terminator: mir::Terminator) {
        if let Some(block) = self.current_blocks.get_mut(bb_id as usize) {
            block.terminator = Some(terminator);
        }
    }

    fn block_has_terminator(&self, bb_id: mir::BasicBlockId) -> bool {
        self.current_blocks
            .get(bb_id as usize)
            .and_then(|block| block.terminator.as_ref())
            .is_some()
    }

    fn ensure_goto(&mut self, bb_id: mir::BasicBlockId, target: mir::BasicBlockId, span: Span) {
        if !self.block_has_terminator(bb_id) {
            self.set_block_terminator(
                bb_id,
                mir::Terminator {
                    kind: mir::TerminatorKind::Goto { target },
                    source_info: span,
                },
            );
        }
    }

    fn is_unit_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Tuple(ref elems) if elems.is_empty())
    }

    fn is_never_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Never)
    }

    fn unit_rvalue(&self) -> mir::Rvalue {
        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new())
    }

    fn bool_operand(&self, value: bool, span: Span) -> mir::Operand {
        mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Bool(value),
        })
    }

    fn default_rvalue_for_type(&self, ty: &Ty, span: Span) -> mir::Rvalue {
        match &ty.kind {
            TyKind::Bool => mir::Rvalue::Use(self.bool_operand(false, span)),
            TyKind::Tuple(elems) if elems.is_empty() => self.unit_rvalue(),
            TyKind::Never => mir::Rvalue::Use(self.bool_operand(false, span)),
            _ => mir::Rvalue::Use(self.bool_operand(false, span)),
        }
    }

    fn constant_to_u128(&self, _value: &thir::ConstValue) -> u128 {
        tracing::warn!("THIR→MIR: const pattern lowering uses placeholder value; treating as zero");
        0
    }

    fn create_external_function_body(
        &mut self,
        return_local: mir::LocalId,
        param_count: usize,
    ) -> Result<mir::Body> {
        let entry_bb = self.create_basic_block();

        self.set_block_terminator(
            entry_bb,
            mir::Terminator {
                kind: mir::TerminatorKind::Return,
                source_info: Span::new(0, 0, 0),
            },
        );

        Ok(mir::Body {
            basic_blocks: self.current_blocks.clone(),
            locals: self.local_decls.clone(),
            arg_count: param_count,
            return_local,
            var_debug_info: Vec::new(),
            span: Span::new(0, 0, 0),
        })
    }

    fn transform_binary_op(&self, op: BinOpKind) -> Result<mir::BinOp> {
        match op {
            BinOpKind::Add => Ok(mir::BinOp::Add),
            BinOpKind::Sub => Ok(mir::BinOp::Sub),
            BinOpKind::Mul => Ok(mir::BinOp::Mul),
            BinOpKind::Div => Ok(mir::BinOp::Div),
            BinOpKind::Eq => Ok(mir::BinOp::Eq),
            BinOpKind::Ne => Ok(mir::BinOp::Ne),
            BinOpKind::Lt => Ok(mir::BinOp::Lt),
            BinOpKind::Le => Ok(mir::BinOp::Le),
            BinOpKind::Gt => Ok(mir::BinOp::Gt),
            BinOpKind::Ge => Ok(mir::BinOp::Ge),
            _ => Ok(mir::BinOp::Add), // Default fallback
        }
    }

    fn literal_to_operand(&self, lit: thir::Lit, span: Span) -> mir::Operand {
        let literal = match lit {
            thir::Lit::Bool(b) => mir::ConstantKind::Bool(b),
            thir::Lit::Int(v, _) => mir::ConstantKind::Int(v as i64),
            thir::Lit::Uint(v, _) => mir::ConstantKind::UInt(v as u64),
            thir::Lit::Float(v, _) => mir::ConstantKind::Float(v as f64),
            thir::Lit::Str(s) => mir::ConstantKind::Str(s),
            thir::Lit::Char(c) => mir::ConstantKind::Int(c as i64),
            thir::Lit::Byte(b) => mir::ConstantKind::UInt(b as u64),
            thir::Lit::ByteStr(bytes) => {
                // Represent as string for now
                let s = String::from_utf8_lossy(&bytes).into_owned();
                mir::ConstantKind::Str(s)
            }
        };
        mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal,
        })
    }

    /// Check if a type is a function type
    fn is_function_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::FnDef(..) | TyKind::FnPtr(..))
    }

    fn convert_thir_binop_to_kind(&self, op: thir::BinOp) -> BinOpKind {
        match op {
            thir::BinOp::Add => BinOpKind::Add,
            thir::BinOp::Sub => BinOpKind::Sub,
            thir::BinOp::Mul => BinOpKind::Mul,
            thir::BinOp::Div => BinOpKind::Div,
            thir::BinOp::Eq => BinOpKind::Eq,
            thir::BinOp::Ne => BinOpKind::Ne,
            thir::BinOp::Lt => BinOpKind::Lt,
            thir::BinOp::Le => BinOpKind::Le,
            thir::BinOp::Gt => BinOpKind::Gt,
            thir::BinOp::Ge => BinOpKind::Ge,
            _ => BinOpKind::Add, // Default fallback for other ops
        }
    }

    fn next_id(&mut self) -> mir::MirId {
        let id = self.next_mir_id;
        self.next_mir_id += 1;
        id
    }

    /// Transform THIR type to MIR type (simplified mapping)
    fn transform_type(&self, ty: &fp_core::types::Ty) -> fp_core::types::Ty {
        // For now, just clone the THIR type since MIR can use the same type system
        // In a full implementation, this might transform types to MIR-specific representations
        ty.clone()
    }
}

impl IrTransform<thir::Program, mir::Program> for MirGenerator {
    fn transform(&mut self, source: thir::Program) -> Result<mir::Program> {
        MirGenerator::transform(self, source)
    }
}

impl Default for MirGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::id::Ident;

    #[test]
    fn test_mir_generator_creation() {
        let generator = MirGenerator::new();
        assert_eq!(generator.next_mir_id, 0);
        assert_eq!(generator.next_local_id, 0);
        assert_eq!(generator.next_bb_id, 0);
    }

    #[test]
    fn test_local_creation() {
        let mut generator = MirGenerator::new();
        let ty = Ty::new(TyKind::Tuple(Vec::new()));

        let local1 = generator.create_local(ty.clone());
        let local2 = generator.create_local(ty.clone());

        assert_eq!(local1, 0);
        assert_eq!(local2, 1);
        assert_eq!(generator.next_local_id, 2);
    }

    #[test]
    fn test_basic_block_creation() {
        let mut generator = MirGenerator::new();

        let bb1 = generator.create_basic_block();
        let bb2 = generator.create_basic_block();

        assert_eq!(bb1, 0);
        assert_eq!(bb2, 1);
        assert_eq!(generator.current_blocks.len(), 2);
    }

    #[test]
    fn test_binary_op_transformation() {
        let generator = MirGenerator::new();

        assert_eq!(
            generator.transform_binary_op(BinOpKind::Add).unwrap(),
            mir::BinOp::Add
        );
        assert_eq!(
            generator.transform_binary_op(BinOpKind::Eq).unwrap(),
            mir::BinOp::Eq
        );
    }

    #[test]
    fn test_empty_thir_program_transformation() {
        let mut generator = MirGenerator::new();
        let thir_program = thir::Program::new();

        let result = generator.transform(thir_program);
        assert!(result.is_ok());

        let mir_program = result.unwrap();
        assert_eq!(mir_program.items.len(), 0);
    }
}
