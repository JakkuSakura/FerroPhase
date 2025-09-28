use super::*;

impl MirGenerator {
    pub(super) fn transform_function_with_body(
        &mut self,
        func: thir::Function,
        thir_program: &thir::Program,
    ) -> Result<(mir::Function, Option<(mir::BodyId, mir::Body)>)> {
        // Reset generator state for new function
        self.reset_for_new_function();

        // Create locals for parameters and return value
        let return_local = self.create_local(self.transform_type(&func.sig.output));
        let mut param_locals = Vec::new();

        for param in &func.sig.inputs {
            let local_id = self.create_local(self.transform_type(param));
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
            name: func.name.clone(),
            path: func.path.clone(),
            def_id: func.def_id.map(|id| id as mir_types::DefId),
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

    pub(super) fn transform_body(
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
            let mir_local = self.create_local(self.transform_type(&local_decl.ty));
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

    pub(super) fn transform_stmt(
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
                        let mir_local =
                            self.get_or_create_local(local_id, self.transform_type(&pattern.ty));
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

    pub(super) fn binding_local_from_pattern(&self, pattern: &thir::Pat) -> Option<thir::LocalId> {
        match &pattern.kind {
            thir::PatKind::Binding { var, .. } => Some(*var),
            thir::PatKind::Deref { subpattern } => self.binding_local_from_pattern(subpattern),
            _ => None,
        }
    }

    /// Helper methods

    pub(super) fn create_external_function_body(
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
}
