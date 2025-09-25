use fp_core::error::Result;
use fp_core::types::{
    ConstKind, ConstValue, FloatTy, IntTy, Scalar, Ty, TyKind, UintTy,
};
use fp_core::{lir, mir};
use std::collections::HashMap;

mod const_eval;
mod println;

// Internal submodules; items are used via inherent methods

use super::IrTransform;

/// Generator for transforming MIR to LIR (Low-level IR)
pub struct LirGenerator {
    next_lir_id: lir::LirId,
    next_label: u32,
    register_map: HashMap<mir::LocalId, lir::LirValue>,
    current_function: Option<lir::LirFunction>,
    pub(crate) const_values: HashMap<mir::LocalId, lir::LirConstant>,
    format_string_map: HashMap<mir::LocalId, (String, Vec<lir::LirValue>)>,
    local_types: Vec<Ty>,
}

impl LirGenerator {
    /// Create a new LIR generator
    pub fn new() -> Self {
        Self {
            next_lir_id: 0,
            next_label: 0,
            register_map: HashMap::new(),
            current_function: None,
            const_values: HashMap::new(),
            format_string_map: HashMap::new(),
            local_types: Vec::new(),
        }
    }

    /// Transform a MIR program to LIR
    pub fn transform(&mut self, mir_program: mir::Program) -> Result<lir::LirProgram> {
        let mut lir_program = lir::LirProgram::new();

        for mir_item in mir_program.items {
            match mir_item.kind {
                mir::ItemKind::Function(mir_func) => {
                    let lir_func =
                        self.transform_function_with_bodies(mir_func, &mir_program.bodies)?;
                    lir_program.functions.push(lir_func);
                }
                mir::ItemKind::Static(mir_static) => {
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
        mir_func: mir::Function,
        bodies: &std::collections::HashMap<mir::BodyId, mir::Body>,
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
            self.local_types = mir_body.locals.iter().map(|decl| decl.ty.clone()).collect();

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
    fn transform_static(&mut self, _mir_static: mir::Static) -> Result<lir::LirGlobal> {
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
        match rvalue {
            mir::Rvalue::Use(operand) => {
                let source_value = self.transform_operand(operand)?;
                self.register_map.insert(place.local, source_value);
                Ok(Vec::new())
            }
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let lhs_value = self.transform_operand(lhs)?;
                let rhs_value = self.transform_operand(rhs)?;

                let instr_id = self.next_id();
                let lir_kind = self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
                let type_hint = self
                    .lookup_place_type(place)
                    .map(|ty| self.lir_type_from_ty(ty))
                    .or_else(|| Some(lir::LirType::I32));

                self.register_map
                    .insert(place.local, lir::LirValue::Register(instr_id));

                Ok(vec![lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint,
                    debug_info: None,
                }])
            }
            mir::Rvalue::Aggregate(kind, fields) => {
                self.handle_aggregate(place, kind, fields)
            }
            mir::Rvalue::Ref(_, _, borrowed_place) => {
                let pointer = lir::LirValue::Local(borrowed_place.local);
                self.register_map.insert(place.local, pointer);
                Ok(Vec::new())
            }
            mir::Rvalue::AddressOf(_, borrowed_place) => {
                let pointer = lir::LirValue::Local(borrowed_place.local);
                self.register_map.insert(place.local, pointer);
                Ok(Vec::new())
            }
            mir::Rvalue::Len(_place) => {
                // TODO: compute length when len(place) metadata is available
                self.register_map.insert(
                    place.local,
                    lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I64)),
                );
                Ok(Vec::new())
            }
            mir::Rvalue::Cast(cast_kind, operand, _ty) => {
                let operand_value = self.transform_operand(operand)?;
                let target_ty = self
                    .lookup_place_type(place)
                    .map(|ty| self.lir_type_from_ty(ty))
                    .unwrap_or(lir::LirType::I64);

                let instr_id = self.next_id();
                let instr_kind = self.lower_cast(cast_kind.clone(), operand_value.clone(), target_ty.clone());

                self.register_map
                    .insert(place.local, lir::LirValue::Register(instr_id));

                Ok(vec![lir::LirInstruction {
                    id: instr_id,
                    kind: instr_kind,
                    type_hint: Some(target_ty),
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

                if let mir::Operand::Constant(mir::Constant {
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
                        let call_id = self.next_id();
                        let call = lir::LirInstruction {
                            id: call_id,
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
                        if let Some((dest_place, dest_bb)) = destination {
                            self.register_map
                                .insert(dest_place.local, lir::LirValue::Register(call_id));
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
    fn transform_operand(&mut self, operand: &mir::Operand) -> Result<lir::LirValue> {
        match operand {
            mir::Operand::Move(place) => Ok(self.get_or_create_register_for_place(place)),
            mir::Operand::Copy(place) => Ok(self.get_or_create_register_for_place(place)),
            mir::Operand::Constant(constant) => match &constant.literal {
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
                    lir::LirConstant::Int(*value, lir::LirType::I64),
                )),
                mir::ConstantKind::UInt(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::UInt(*value, lir::LirType::I64),
                )),
                mir::ConstantKind::Float(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::Float(*value, lir::LirType::F64),
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

    /// Helper methods
    fn reset_for_new_function(&mut self) {
        self.next_label = 0;
        self.register_map.clear();
        self.current_function = None;
        self.const_values.clear();
        self.format_string_map.clear();
        self.local_types.clear();
    }

    fn get_or_create_register_for_place(&mut self, place: &mir::Place) -> lir::LirValue {
        if let Some(existing_reg) = self.register_map.get(&place.local) {
            existing_reg.clone()
        } else {
            tracing::warn!(
                "MIR→LIR: missing value for local {}; defaulting to zero",
                place.local
            );
            let default = lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I32));
            self.register_map.insert(place.local, default.clone());
            default
        }
    }

    fn next_id(&mut self) -> lir::LirId {
        let id = self.next_lir_id;
        self.next_lir_id += 1;
        id
    }

    fn handle_aggregate(
        &mut self,
        place: &mir::Place,
        kind: &mir::AggregateKind,
        fields: &[mir::Operand],
    ) -> Result<Vec<lir::LirInstruction>> {
        let mut lir_values = Vec::with_capacity(fields.len());
        let mut constants = Vec::with_capacity(fields.len());
        let mut all_constants = true;

        for operand in fields {
            let value = self.transform_operand(operand)?;
            all_constants &= matches!(value, lir::LirValue::Constant(_));
            if let lir::LirValue::Constant(ref c) = value {
                constants.push(c.clone());
            }
            lir_values.push(value);
        }

        if fields.is_empty() {
            if let Some(place_ty) = self.lookup_place_type(place) {
                let lir_ty = self.lir_type_from_ty(place_ty);
                let value = lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), lir_ty.clone()));
                self.register_map.insert(place.local, value);
                return Ok(Vec::new());
            }
        }

        if all_constants {
            if let Some(place_ty) = self.lookup_place_type(place) {
                if let Some(constant) = self.constant_from_aggregate(kind, constants, place_ty) {
                    self.register_map
                        .insert(place.local, lir::LirValue::Constant(constant));
                    return Ok(Vec::new());
                }
            }
        }

        if let Some(place_ty) = self.lookup_place_type(place) {
            let aggregate_ty = self.lir_type_from_ty(place_ty);
            let mut instructions = Vec::new();
            let mut current_value =
                lir::LirValue::Constant(lir::LirConstant::Undef(aggregate_ty.clone()));

            for (index, value) in lir_values.into_iter().enumerate() {
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::InsertValue {
                        aggregate: current_value.clone(),
                        element: value,
                        indices: vec![index as u32],
                    },
                    type_hint: Some(aggregate_ty.clone()),
                    debug_info: None,
                });
                current_value = lir::LirValue::Register(instr_id);
            }

            self.register_map.insert(place.local, current_value);
            return Ok(instructions);
        }

        self.register_map.insert(
            place.local,
            lir::LirValue::Constant(lir::LirConstant::Undef(lir::LirType::Void)),
        );
        Ok(Vec::new())
    }

    fn constant_from_aggregate(
        &self,
        kind: &mir::AggregateKind,
        constants: Vec<lir::LirConstant>,
        place_ty: &Ty,
    ) -> Option<lir::LirConstant> {
        match kind {
            mir::AggregateKind::Tuple => {
                let lir_ty = self.lir_type_from_ty(place_ty);
                Some(lir::LirConstant::Struct(constants, lir_ty))
            }
            mir::AggregateKind::Array(_elem_ty) => {
                if let TyKind::Array(inner_ty, len) = &place_ty.kind {
                    let lir_elem_ty = self.lir_type_from_ty(inner_ty);
                    let expected = self.array_length_from_const(len);
                    if expected != 0 && expected != constants.len() as u64 {
                        tracing::warn!(
                            "MIR→LIR: array constant length {} differs from {} elements",
                            expected,
                            constants.len()
                        );
                    }
                    Some(lir::LirConstant::Array(constants, lir_elem_ty))
                } else {
                    Some(lir::LirConstant::Array(
                        constants,
                        lir::LirType::I64,
                    ))
                }
            }
            _ => None,
        }
    }

    fn lookup_place_type(&self, place: &mir::Place) -> Option<&Ty> {
        self.local_types.get(place.local as usize)
    }

    fn lir_type_from_ty(&self, ty: &Ty) -> lir::LirType {
        match &ty.kind {
            TyKind::Bool => lir::LirType::I1,
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => lir::LirType::I8,
                IntTy::I16 => lir::LirType::I16,
                IntTy::I32 => lir::LirType::I32,
                IntTy::I64 => lir::LirType::I64,
                IntTy::I128 => lir::LirType::I128,
                IntTy::Isize => lir::LirType::I64,
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => lir::LirType::I8,
                UintTy::U16 => lir::LirType::I16,
                UintTy::U32 => lir::LirType::I32,
                UintTy::U64 => lir::LirType::I64,
                UintTy::U128 => lir::LirType::I128,
                UintTy::Usize => lir::LirType::I64,
            },
            TyKind::Float(float_ty) => match float_ty {
                FloatTy::F32 => lir::LirType::F32,
                FloatTy::F64 => lir::LirType::F64,
            },
            TyKind::Tuple(elements) if elements.is_empty() => lir::LirType::Void,
            TyKind::Tuple(elements) => lir::LirType::Struct {
                fields: elements
                    .iter()
                    .map(|elem| self.lir_type_from_ty(elem))
                    .collect(),
                packed: false,
                name: None,
            },
            TyKind::Array(element_ty, len) => lir::LirType::Array(
                Box::new(self.lir_type_from_ty(element_ty)),
                self.array_length_from_const(len),
            ),
            _ => lir::LirType::I64,
        }
    }

    fn lower_binary_op(
        &self,
        bin_op: mir::BinOp,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
    ) -> lir::LirInstructionKind {
        match bin_op {
            mir::BinOp::Add => lir::LirInstructionKind::Add(lhs, rhs),
            mir::BinOp::Sub => lir::LirInstructionKind::Sub(lhs, rhs),
            mir::BinOp::Mul => lir::LirInstructionKind::Mul(lhs, rhs),
            mir::BinOp::Div => lir::LirInstructionKind::Div(lhs, rhs),
            mir::BinOp::Rem => lir::LirInstructionKind::Rem(lhs, rhs),
            mir::BinOp::BitAnd => lir::LirInstructionKind::And(lhs, rhs),
            mir::BinOp::BitOr => lir::LirInstructionKind::Or(lhs, rhs),
            mir::BinOp::BitXor => lir::LirInstructionKind::Xor(lhs, rhs),
            mir::BinOp::Shl => lir::LirInstructionKind::Shl(lhs, rhs),
            mir::BinOp::Shr => lir::LirInstructionKind::Shr(lhs, rhs),
            mir::BinOp::Eq => lir::LirInstructionKind::Eq(lhs, rhs),
            mir::BinOp::Ne => lir::LirInstructionKind::Ne(lhs, rhs),
            mir::BinOp::Lt => lir::LirInstructionKind::Lt(lhs, rhs),
            mir::BinOp::Le => lir::LirInstructionKind::Le(lhs, rhs),
            mir::BinOp::Gt => lir::LirInstructionKind::Gt(lhs, rhs),
            mir::BinOp::Ge => lir::LirInstructionKind::Ge(lhs, rhs),
            _ => lir::LirInstructionKind::Unreachable,
        }
    }

    fn lower_cast(
        &self,
        cast_kind: mir::CastKind,
        source: lir::LirValue,
        target_ty: lir::LirType,
    ) -> lir::LirInstructionKind {
        match cast_kind {
            mir::CastKind::Misc => lir::LirInstructionKind::Bitcast(source, target_ty),
            mir::CastKind::Pointer(pointer_cast) => match pointer_cast {
                mir::PointerCast::ReifyFnPointer
                | mir::PointerCast::UnsafeFnPointer
                | mir::PointerCast::ClosureFnPointer
                | mir::PointerCast::MutToConstPointer
                | mir::PointerCast::ArrayToPointer
                | mir::PointerCast::Unsize => lir::LirInstructionKind::Bitcast(source, target_ty),
            },
        }
    }

    fn array_length_from_const(&self, len: &ConstKind) -> u64 {
        match len {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => int.data as u64,
            other => {
                tracing::warn!("MIR→LIR: array length {:?} not evaluated; defaulting to 0", other);
                0
            }
        }
    }
}

impl IrTransform<mir::Program, lir::LirProgram> for LirGenerator {
    fn transform(&mut self, source: mir::Program) -> Result<lir::LirProgram> {
        LirGenerator::transform(self, source)
    }
}

impl Default for LirGenerator {
    fn default() -> Self {
        Self::new()
    }
}
