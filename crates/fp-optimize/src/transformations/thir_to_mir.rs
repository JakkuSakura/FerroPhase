use fp_core::error::Result;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use fp_core::types::{Ty, TyKind};
use fp_core::{mir, thir};
// use fp_core::ast::Visibility; // not used here
use std::collections::HashMap;

/// Generator for transforming THIR to MIR (Mid-level IR)
pub struct MirGenerator {
    next_mir_id: mir::MirId,
    next_local_id: u32,
    next_bb_id: u32,
    current_locals: HashMap<thir::ThirId, mir::LocalId>,
    current_blocks: Vec<mir::BasicBlockData>,
    local_decls: Vec<mir::LocalDecl>,
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
        }
    }

    /// Transform a THIR program to MIR
    pub fn transform(&mut self, thir_program: thir::ThirProgram) -> Result<mir::MirProgram> {
        let mut mir_program = mir::MirProgram::new();

        for item in &thir_program.items {
            match &item.kind {
                thir::ThirItemKind::Function(func) => {
                    let (mir_func, maybe_body) =
                        self.transform_function_with_body(func.clone(), &thir_program)?;
                    if let Some((body_id, body)) = maybe_body {
                        mir_program.bodies.insert(body_id, body);
                    }
                    mir_program.items.push(mir::MirItem {
                        mir_id: self.next_id(),
                        kind: mir::MirItemKind::Function(mir_func),
                    });
                }
                thir::ThirItemKind::Struct(_) => {
                    // Structs don't need MIR representation - they're handled at type level
                }
                thir::ThirItemKind::Const(_const_item) => {
                    // Skip const items for now
                }
                thir::ThirItemKind::Impl(_) => {
                    // Skip impl items for now
                }
            }
        }

        Ok(mir_program)
    }

    /// Transform a THIR function to MIR
    fn transform_function_with_body(
        &mut self,
        func: thir::ThirFunction,
        thir_program: &thir::ThirProgram,
    ) -> Result<(mir::MirFunction, Option<(mir::BodyId, mir::MirBody)>)> {
        // Reset generator state for new function
        self.reset_for_new_function();

        // Create locals for parameters and return value
        let return_local = self.create_local(func.sig.output.clone());
        let mut param_locals = Vec::new();

        for param in &func.sig.inputs {
            let local_id = self.create_local(param.clone());
            param_locals.push(local_id);
        }

        // Create a body. If THIR has one, transform it and store in MIR program bodies.
        let param_count = param_locals.len();

        let (body_id, body_opt) = if let Some(body_id) = func.body_id {
            if let Some(thir_body) = thir_program.bodies.get(&body_id) {
                let mir_body =
                    self.transform_body(thir_body.clone(), return_local, param_count)?;
                (mir::BodyId(body_id.0), Some(mir_body))
            } else {
                let mir_body = self.create_external_function_body(return_local, param_count)?;
                (mir::BodyId(0), Some(mir_body))
            }
        } else {
            // External function - minimal body
            let mir_body = self.create_external_function_body(return_local, param_count)?;
            (mir::BodyId(0), Some(mir_body))
        };
        let mir_ty = self.transform_type(&func.sig.output);
        let func = mir::MirFunction {
            sig: mir::MirFunctionSig {
                inputs: vec![], // Convert later properly
                output: mir_ty, // Use unit type for MIR
            },
            body_id,
        };

        Ok((func, body_opt.map(|b| (body_id, b))))
    }

    /// Transform a THIR body to MIR body
    fn transform_body(
        &mut self,
        thir_body: thir::ThirBody,
        return_local: mir::LocalId,
        param_count: usize,
    ) -> Result<mir::MirBody> {
        // Create entry basic block
        let entry_bb = self.create_basic_block();

        tracing::debug!(
            "Transforming THIR body with expression kind: {:?}",
            thir_body.value.kind
        );

        // Transform the body expression
        let result_place = self.transform_expr(thir_body.value, entry_bb)?;

        // Add return statement
        self.add_statement_to_block(
            entry_bb,
            mir::Statement {
                kind: mir::StatementKind::Assign(
                    mir::Place::from_local(return_local),
                    mir::Rvalue::Use(mir::MirOperand::Move(result_place)),
                ),
                source_info: Span::new(0, 0, 0),
            },
        );

        // Set terminator for entry block
        self.set_block_terminator(
            entry_bb,
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

        Ok(mir::MirBody {
            basic_blocks: self.current_blocks.clone(),
            locals: self.local_decls.clone(),
            arg_count: param_count,
            return_local,
            var_debug_info: Vec::new(),
            span: Span::new(0, 0, 0),
        })
    }

    /// Transform a THIR expression to MIR, returning the place where result is stored
    fn transform_expr(
        &mut self,
        expr: thir::ThirExpr,
        current_bb: mir::BasicBlockId,
    ) -> Result<mir::Place> {
        match expr.kind {
            thir::ThirExprKind::Field { base, field_idx: _ } => {
                // Lower field selection by first lowering the base
                // For now, if the base lowered to a constant struct literal, extract constant field.
                // Since our types are placeholders, treat base as producing its fields directly when possible.
                let base_place = self.transform_expr(*base, current_bb)?;
                // Without a real aggregate model, emit a move of base and rely on earlier inlining to have simplified this.
                let temp_local = self.create_local(expr.ty);
                let place = mir::Place::from_local(temp_local);
                self.add_statement_to_block(
                    current_bb,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::MirOperand::Move(base_place)),
                        ),
                        source_info: expr.span,
                    },
                );
                Ok(place)
            }
            thir::ThirExprKind::Literal(lit) => {
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

                Ok(place)
            }
            thir::ThirExprKind::Path(name) => {
                let mir_ty = self.transform_type(&expr.ty);
                // Use type information to distinguish between function references and global constants
                let constant_kind = if self.is_function_type(&expr.ty) {
                    mir::ConstantKind::Fn(name.clone(), mir_ty)
                } else {
                    // If it's not a function type, treat it as a global constant
                    // Use unit type as placeholder since MIR Ty is defined as ()
                    mir::ConstantKind::Global(name.clone(), mir_ty)
                };

                let temp_local = self.create_local(expr.ty);
                let place = mir::Place::from_local(temp_local);
                self.add_statement_to_block(
                    current_bb,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::MirOperand::Constant(mir::Constant {
                                span: expr.span,
                                user_ty: None,
                                literal: constant_kind,
                            })),
                        ),
                        source_info: expr.span,
                    },
                );
                Ok(place)
            }
            thir::ThirExprKind::Binary(op, lhs, rhs) => {
                let lhs_place = self.transform_expr(*lhs, current_bb)?;
                let rhs_place = self.transform_expr(*rhs, current_bb)?;
                let result_local = self.create_local(expr.ty);
                let result_place = mir::Place::from_local(result_local);

                // Convert THIR BinOp to BinOpKind
                let binop_kind = self.convert_thir_binop_to_kind(op);

                self.add_statement_to_block(
                    current_bb,
                    mir::Statement {
                        kind: mir::StatementKind::Assign(
                            result_place.clone(),
                            mir::Rvalue::BinaryOp(
                                self.transform_binary_op(binop_kind)?,
                                mir::MirOperand::Move(lhs_place),
                                mir::MirOperand::Move(rhs_place),
                            ),
                        ),
                        source_info: expr.span,
                    },
                );

                Ok(result_place)
            }
            thir::ThirExprKind::Call { fun, args, .. } => {
                // Transform callee and arguments
                // Get callee name if it's a Path; otherwise lower callee to operand
                let func_name_opt = match fun.kind {
                    thir::ThirExprKind::Path(ref n) => Some(n.clone()),
                    _ => None,
                };

                tracing::debug!(
                    "Processing Call to function: {:?} with {} args",
                    func_name_opt,
                    args.len()
                );
                // Lower function expression; we don't need the place for a direct Fn(...) constant
                let _ = self.transform_expr(*fun, current_bb)?;
                let mut arg_operands = Vec::new();
                for arg in args {
                    // If the argument is a literal, pass it directly as a MIR constant operand
                    match &arg.kind {
                        thir::ThirExprKind::Literal(lit) => {
                            arg_operands.push(self.literal_to_operand(lit.clone(), arg.span));
                        }
                        thir::ThirExprKind::Field { .. } => {
                            // Attempt to lower field to a value (const/copy). As a fallback, treat as Move of lowered place.
                            let arg_place = self.transform_expr(arg.clone(), current_bb)?;
                            arg_operands.push(mir::MirOperand::Move(arg_place));
                        }
                        _ => {
                            let arg_place = self.transform_expr(arg, current_bb)?;
                            arg_operands.push(mir::MirOperand::Move(arg_place));
                        }
                    }
                }
                // Allocate a result local and emit a Call terminator returning to a new block
                let result_local = self.create_local(expr.ty);
                let result_place = mir::Place::from_local(result_local);

                // Create a continuation block
                let cont_bb = self.create_basic_block();
                self.set_block_terminator(
                    current_bb,
                    mir::Terminator {
                        kind: mir::TerminatorKind::Call {
                            func: if let Some(fname) = func_name_opt {
                                mir::MirOperand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(
                                        fname,
                                        // TODO: use proper type
                                        Ty::new(TyKind::Never),
                                    ),
                                })
                            } else {
                                mir::MirOperand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::Ty(()),
                                })
                            },
                            args: arg_operands,
                            destination: Some((result_place.clone(), cont_bb)),
                            cleanup: None,
                            from_hir_call: true,
                            fn_span: expr.span,
                        },
                        source_info: expr.span,
                    },
                );
                Ok(result_place)
            }
            thir::ThirExprKind::VarRef { .. } => {
                // Variable reference - create proper local and place
                let temp_local = self.create_local(expr.ty.clone());

                // Add the local to current basic block if needed
                let place = mir::Place::from_local(temp_local);
                Ok(place)
            }
            thir::ThirExprKind::Block(block) => {
                // Process all statements in the block
                let mut current_bb = current_bb;

                tracing::debug!("Processing block with {} statements", block.stmts.len());
                for (i, stmt) in block.stmts.iter().enumerate() {
                    match &stmt.kind {
                        thir::ThirStmtKind::Expr(expr) => {
                            tracing::debug!("Processing statement {}: {:?}", i, expr.kind);

                            // Transform expression as a statement (result is discarded)
                            let _place = self.transform_expr(expr.clone(), current_bb)?;

                            // Check if the current block has a terminator set
                            // If so, we need to continue in a new basic block
                            if let Some(block_data) = self.current_blocks.get(current_bb as usize) {
                                if block_data.terminator.is_some() {
                                    // Current block has a terminator, create a new block for subsequent statements
                                    current_bb = self.create_basic_block();
                                    tracing::debug!(
                                        "Created new basic block {} after statement {}",
                                        current_bb,
                                        i
                                    );
                                }
                            }
                        }
                        thir::ThirStmtKind::Let { .. } => {
                            // TODO: Handle let statements
                        }
                    }
                }

                // If block has a final expression, transform it and return its place
                if let Some(final_expr) = &block.expr {
                    self.transform_expr(*final_expr.clone(), current_bb)
                } else {
                    // Block has no final expression, return unit
                    let temp_local = self.create_local(expr.ty);
                    Ok(mir::Place::from_local(temp_local))
                }
            }
            _ => {
                // Default case for unhandled expressions
                let temp_local = self.create_local(expr.ty);
                Ok(mir::Place::from_local(temp_local))
            }
        }
    }

    /// Helper methods
    fn reset_for_new_function(&mut self) {
        self.next_local_id = 0;
        self.next_bb_id = 0;
        self.current_locals.clear();
        self.current_blocks.clear();
        self.local_decls.clear();
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

    fn create_external_function_body(
        &mut self,
        return_local: mir::LocalId,
        param_count: usize,
    ) -> Result<mir::MirBody> {
        let entry_bb = self.create_basic_block();

        self.set_block_terminator(
            entry_bb,
            mir::Terminator {
                kind: mir::TerminatorKind::Return,
                source_info: Span::new(0, 0, 0),
            },
        );

        Ok(mir::MirBody {
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

    fn literal_to_operand(&self, lit: thir::ThirLit, span: Span) -> mir::MirOperand {
        let literal = match lit {
            thir::ThirLit::Bool(b) => mir::ConstantKind::Bool(b),
            thir::ThirLit::Int(v, _) => mir::ConstantKind::Int(v as i64),
            thir::ThirLit::Uint(v, _) => mir::ConstantKind::UInt(v as u64),
            thir::ThirLit::Float(v, _) => mir::ConstantKind::Float(v as f64),
            thir::ThirLit::Str(s) => mir::ConstantKind::Str(s),
            thir::ThirLit::Char(c) => mir::ConstantKind::Int(c as i64),
            thir::ThirLit::Byte(b) => mir::ConstantKind::UInt(b as u64),
            thir::ThirLit::ByteStr(bytes) => {
                // Represent as string for now
                let s = String::from_utf8_lossy(&bytes).into_owned();
                mir::ConstantKind::Str(s)
            }
        };
        mir::MirOperand::Constant(mir::Constant {
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
        let thir_program = thir::ThirProgram::new();

        let result = generator.transform(thir_program);
        assert!(result.is_ok());

        let mir_program = result.unwrap();
        assert_eq!(mir_program.items.len(), 0);
    }
}
