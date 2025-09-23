use fp_core::error::Result;
use fp_core::types::{IntTy, Ty};
use fp_core::{lir, mir};
use std::collections::HashMap;

mod const_eval;
mod println;

// Internal submodules; items are used via inherent methods

use super::IrTransform;

/// Generator for transforming MIR to LIR (Low-level IR)
pub struct LirGenerator {
    next_lir_id: lir::LirId,
    next_virtual_reg: u32,
    next_label: u32,
    register_map: HashMap<mir::LocalId, lir::LirValue>,
    current_function: Option<lir::LirFunction>,
    pub(crate) const_values: HashMap<mir::LocalId, lir::LirConstant>,
    format_string_map: HashMap<mir::LocalId, (String, Vec<lir::LirValue>)>,
}

impl LirGenerator {
    /// Create a new LIR generator
    pub fn new() -> Self {
        Self {
            next_lir_id: 0,
            next_virtual_reg: 0,
            next_label: 0,
            register_map: HashMap::new(),
            current_function: None,
            const_values: HashMap::new(),
            format_string_map: HashMap::new(),
        }
    }

    /// Transform a MIR program to LIR
    pub fn transform(&mut self, mir_program: mir::MirProgram) -> Result<lir::LirProgram> {
        let mut lir_program = lir::LirProgram::new();

        for mir_item in mir_program.items {
            match mir_item.kind {
                mir::MirItemKind::Function(mir_func) => {
                    let lir_func =
                        self.transform_function_with_bodies(mir_func, &mir_program.bodies)?;
                    lir_program.functions.push(lir_func);
                }
                mir::MirItemKind::Static(mir_static) => {
                    let lir_static = self.transform_static(mir_static)?;
                    lir_program.globals.push(lir_static);
                }
            }
        }

        Ok(lir_program)
    }

    /// Transform a MIR function to LIR
    fn transform_function_with_bodies(
        &mut self,
        mir_func: mir::MirFunction,
        bodies: &std::collections::HashMap<mir::BodyId, mir::MirBody>,
    ) -> Result<lir::LirFunction> {
        // Reset generator state for new function
        self.reset_for_new_function();

        let mut lir_func = lir::LirFunction {
            name: format!("func_{}", self.next_lir_id),
            signature: lir::LirFunctionSignature {
                params: Vec::new(),
                return_type: lir::LirType::Void,
                is_variadic: false,
            },
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: lir::CallingConvention::C,
            linkage: lir::Linkage::Internal,
        };

        // Transform MIR body if present
        if let Some(mir_body) = bodies.get(&mir_func.body_id) {
            // First pass: analyze const values
            self.analyze_const_values(mir_body)?;

            for (bb_idx, bb) in mir_body.basic_blocks.iter().enumerate() {
                let lir_block = self.transform_basic_block(bb_idx as u32, bb)?;
                lir_func.basic_blocks.push(lir_block);
            }
            // Ensure at least one block exists
            if lir_func.basic_blocks.is_empty() {
                lir_func.basic_blocks.push(lir::LirBasicBlock {
                    id: 0,
                    label: Some("entry".to_string()),
                    instructions: Vec::new(),
                    terminator: lir::LirTerminator::Return(None),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
            }
        } else {
            // Fallback: create a minimal function with a return
            lir_func.basic_blocks.push(lir::LirBasicBlock {
                id: 0,
                label: Some("entry".to_string()),
                instructions: Vec::new(),
                terminator: lir::LirTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }

        self.current_function = Some(lir_func.clone());
        Ok(lir_func)
    }

    /// Transform a MIR static to LIR global
    fn transform_static(&mut self, _mir_static: mir::MirStatic) -> Result<lir::LirGlobal> {
        Ok(lir::LirGlobal {
            name: format!("global_{}", self.next_lir_id),
            ty: lir::LirType::I32,
            initializer: None,
            linkage: lir::Linkage::Internal,
            visibility: lir::Visibility::Hidden,
            is_constant: false,
            alignment: None,
            section: None,
        })
    }

    /// Transform a basic block
    fn transform_basic_block(
        &mut self,
        bb_id: u32,
        bb_data: &mir::BasicBlockData,
    ) -> Result<lir::LirBasicBlock> {
        let mut lir_block = lir::LirBasicBlock {
            id: bb_id,
            label: Some(format!("bb{}", bb_id)),
            instructions: Vec::new(),
            terminator: lir::LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        // Transform all MIR statements into LIR instructions
        for stmt in &bb_data.statements {
            let lir_insts = self.transform_statement(stmt)?;
            for inst in lir_insts {
                lir_block.instructions.push(inst);
            }
        }

        // Transform the terminator
        let terminator = if let Some(terminator) = &bb_data.terminator {
            self.transform_terminator(terminator, &mut lir_block)?
        } else {
            // Default terminator if none present
            lir::LirTerminator::Return(None)
        };

        lir_block.terminator = terminator;
        Ok(lir_block)
    }

    /// Transform a MIR statement to LIR instructions
    fn transform_statement(&mut self, stmt: &mir::Statement) -> Result<Vec<lir::LirInstruction>> {
        match &stmt.kind {
            mir::StatementKind::Assign(place, rvalue) => self.transform_assign(place, rvalue),
            mir::StatementKind::StorageLive(_) => Ok(Vec::new()),
            mir::StatementKind::StorageDead(_) => Ok(Vec::new()),
            _ => Ok(vec![lir::LirInstruction {
                id: self.next_id(),
                kind: lir::LirInstructionKind::Unreachable,
                type_hint: None,
                debug_info: None,
            }]),
        }
    }

    /// Transform an assignment
    fn transform_assign(
        &mut self,
        place: &mir::Place,
        rvalue: &mir::Rvalue,
    ) -> Result<Vec<lir::LirInstruction>> {
        let _target_reg = self.get_or_create_register_for_place(place);

        match rvalue {
            mir::Rvalue::Use(operand) => {
                // For Use operations, directly map the operand to the target place
                let source_value = self.transform_operand(operand)?;

                // Always update the register mapping to point to the source value
                // This avoids creating unnecessary instructions
                self.register_map.insert(place.local, source_value);
                Ok(Vec::new())
            }
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let _lhs_value = self.transform_operand(lhs)?;
                let _rhs_value = self.transform_operand(rhs)?;
                let lir_op = self.transform_binary_op(bin_op.clone())?;

                Ok(vec![lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir_op,
                    type_hint: Some(lir::LirType::I32),
                    debug_info: None,
                }])
            }
            _ => Ok(vec![lir::LirInstruction {
                id: self.next_id(),
                kind: lir::LirInstructionKind::Unreachable,
                type_hint: None,
                debug_info: None,
            }]),
        }
    }

    /// Transform a MIR terminator to LIR terminator
    fn transform_terminator(
        &mut self,
        terminator: &mir::Terminator,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        match &terminator.kind {
            mir::TerminatorKind::Return => Ok(lir::LirTerminator::Return(None)),
            mir::TerminatorKind::Goto { target } => Ok(lir::LirTerminator::Br(*target)),
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } => {
                // Map MIR func operand; if it is a Constant(Fn(name)) and equals "println",
                // emit a call to printf with a naive format string.
                let mut term = lir::LirTerminator::Return(None);

                if let mir::MirOperand::Constant(mir::Constant {
                    literal: mir::ConstantKind::Fn(name, _ty),
                    ..
                }) = func
                {
                    if name == "println" {
                        // Basic lowering supporting optional format string + args
                        let mut fmt = "\n".to_string();
                        let mut call_args: Vec<lir::LirValue> = Vec::new();

                        if args.len() == 1 {
                            // Transform single argument println
                            self.transform_println_single_arg(&args[0], &mut fmt, &mut call_args)?;
                        } else if args.len() >= 2 {
                            // Transform multi-argument println
                            self.transform_println_multi_arg(args, &mut fmt, &mut call_args)?;
                        }
                        // Push printf(fmt, ...)
                        let call = lir::LirInstruction {
                            id: self.next_id(),
                            kind: lir::LirInstructionKind::Call {
                                // FIXME: fill in the proper type here
                                function: lir::LirValue::Global(
                                    "printf".to_string(),
                                    Ty::int(IntTy::I32),
                                ),
                                args: {
                                    let mut v = vec![lir::LirValue::Constant(
                                        lir::LirConstant::String(fmt),
                                    )];
                                    v.append(&mut call_args);
                                    v
                                },
                                calling_convention: lir::CallingConvention::C,
                                tail_call: false,
                            },
                            type_hint: Some(lir::LirType::I32),
                            debug_info: None,
                        };
                        block.instructions.push(call);
                        // Continue to destination if present
                        if let Some((_dest_place, dest_bb)) = destination {
                            term = lir::LirTerminator::Br(*dest_bb);
                        }
                    }
                }
                Ok(term)
            }
            _ => Ok(lir::LirTerminator::Return(None)),
        }
    }

    /// Transform a MIR operand to LIR value
    fn transform_operand(&mut self, operand: &mir::MirOperand) -> Result<lir::LirValue> {
        match operand {
            mir::MirOperand::Move(place) => Ok(self.get_or_create_register_for_place(place)),
            mir::MirOperand::Copy(place) => Ok(self.get_or_create_register_for_place(place)),
            mir::MirOperand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Fn(name, ty) => {
                    Ok(lir::LirValue::Global(name.clone(), ty.clone()))
                }
                mir::ConstantKind::Global(name, ty) => {
                    Ok(lir::LirValue::Global(name.clone(), ty.clone()))
                }
                mir::ConstantKind::Str(s) => {
                    Ok(lir::LirValue::Constant(lir::LirConstant::String(s.clone())))
                }
                mir::ConstantKind::Int(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::Int(*value, lir::LirType::I32),
                )),
                mir::ConstantKind::Bool(b) => {
                    Ok(lir::LirValue::Constant(lir::LirConstant::Bool(*b)))
                }
                mir::ConstantKind::Val(_cv, _ty) => {
                    return Err(crate::error::optimization_error(
                        "Unsupported complex constant in MIR operand",
                    ));
                }
                _ => {
                    return Err(crate::error::optimization_error(
                        "Unsupported constant kind for MIR→LIR",
                    ))
                }
            },
        }
    }

    /// Transform a MIR binary operation to LIR instruction kind
    fn transform_binary_op(&mut self, bin_op: mir::BinOp) -> Result<lir::LirInstructionKind> {
        match bin_op {
            mir::BinOp::Add => Ok(lir::LirInstructionKind::Add(
                lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I32)),
                lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I32)),
            )),
            mir::BinOp::Sub => Ok(lir::LirInstructionKind::Sub(
                lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I32)),
                lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I32)),
            )),
            _ => Ok(lir::LirInstructionKind::Unreachable),
        }
    }

    /// Helper methods
    fn reset_for_new_function(&mut self) {
        self.next_virtual_reg = 0;
        self.next_label = 0;
        self.register_map.clear();
        self.current_function = None;
        self.const_values.clear();
        self.format_string_map.clear();
    }

    fn allocate_virtual_register(&mut self) -> lir::LirValue {
        let reg_id = self.next_virtual_reg;
        self.next_virtual_reg += 1;
        lir::LirValue::Register(reg_id)
    }

    fn get_or_create_register_for_place(&mut self, place: &mir::Place) -> lir::LirValue {
        if let Some(existing_reg) = self.register_map.get(&place.local) {
            existing_reg.clone()
        } else {
            let new_reg = self.allocate_virtual_register();
            self.register_map.insert(place.local, new_reg.clone());
            new_reg
        }
    }

    fn next_id(&mut self) -> lir::LirId {
        let id = self.next_lir_id;
        self.next_lir_id += 1;
        id
    }
}

impl IrTransform<mir::MirProgram, lir::LirProgram> for LirGenerator {
    fn transform(&mut self, source: mir::MirProgram) -> Result<lir::LirProgram> {
        LirGenerator::transform(self, source)
    }
}

impl Default for LirGenerator {
    fn default() -> Self {
        Self::new()
    }
}
