use fp_core::error::Result;
use fp_core::types::{ConstKind, ConstValue, FloatTy, IntTy, Scalar, Ty, TyKind, UintTy};
use fp_core::{lir, mir};
use std::collections::HashMap;

mod const_eval;
// Internal submodules; items are used via inherent methods

use super::IrTransform;

/// Generator for transforming MIR to LIR (Low-level IR)
pub struct LirGenerator {
    next_lir_id: lir::LirId,
    next_label: u32,
    register_map: HashMap<mir::LocalId, lir::LirValue>,
    current_function: Option<lir::LirFunction>,
    pub(crate) const_values: HashMap<mir::LocalId, lir::LirConstant>,
    local_types: Vec<Ty>,
    current_return_type: Option<lir::LirType>,
    return_local: Option<mir::LocalId>,
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
            local_types: Vec::new(),
            current_return_type: None,
            return_local: None,
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

        let function_name = format!("func_{}", mir_func.body_id.0);
        let param_types: Vec<lir::LirType> = mir_func
            .sig
            .inputs
            .iter()
            .map(|ty| self.lir_type_from_ty(ty))
            .collect();
        let return_type = self.lir_type_from_ty(&mir_func.sig.output);
        self.current_return_type = Some(return_type.clone());

        let mut lir_func = lir::LirFunction {
            name: function_name,
            signature: lir::LirFunctionSignature {
                params: param_types,
                return_type: return_type.clone(),
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
            self.return_local = Some(mir_body.return_local);
            lir_func.locals = self.build_lir_locals(mir_body);
            self.seed_argument_registers(mir_body);

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

        self.populate_block_edges(&mut lir_func.basic_blocks);

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
                let lir_kind =
                    self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
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
            mir::Rvalue::Aggregate(kind, fields) => self.handle_aggregate(place, kind, fields),
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
                let source_ty = self.type_of_operand(operand);
                let target_ty = self
                    .lookup_place_type(place)
                    .map(|ty| self.lir_type_from_ty(ty))
                    .unwrap_or(lir::LirType::I64);

                let instr_id = self.next_id();
                let instr_kind = self.lower_cast(
                    cast_kind.clone(),
                    operand_value.clone(),
                    source_ty,
                    target_ty.clone(),
                );

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
            mir::TerminatorKind::Return => {
                Ok(lir::LirTerminator::Return(self.prepare_return_value()))
            }
            mir::TerminatorKind::Goto { target } => Ok(lir::LirTerminator::Br(*target)),
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } => self.transform_call_terminator(func, args, destination, block),
            mir::TerminatorKind::SwitchInt {
                discr,
                switch_ty: _,
                targets,
            } => {
                let discr_value = self.transform_operand(discr)?;
                if targets.values.len() == 1 {
                    let true_target = targets.targets[0];
                    let false_target = targets.otherwise;
                    Ok(lir::LirTerminator::CondBr {
                        condition: discr_value,
                        if_true: true_target,
                        if_false: false_target,
                    })
                } else {
                    let cases = targets
                        .values
                        .iter()
                        .zip(targets.targets.iter())
                        .map(|(value, target)| (*value as u64, *target))
                        .collect();
                    Ok(lir::LirTerminator::Switch {
                        value: discr_value,
                        default: targets.otherwise,
                        cases,
                    })
                }
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
        self.local_types.clear();
        self.current_return_type = None;
        self.return_local = None;
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
                let value =
                    lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), lir_ty.clone()));
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

    fn build_lir_locals(&self, mir_body: &mir::Body) -> Vec<lir::LirLocal> {
        let arg_count = mir_body.arg_count;
        mir_body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, decl)| lir::LirLocal {
                id: idx as u32,
                ty: self.lir_type_from_ty(&decl.ty),
                name: None,
                is_argument: idx > 0 && idx <= arg_count,
            })
            .collect()
    }

    fn seed_argument_registers(&mut self, mir_body: &mir::Body) {
        for arg_index in 0..mir_body.arg_count {
            let local_id = (arg_index as mir::LocalId) + 1;
            self.register_map
                .entry(local_id)
                .or_insert_with(|| lir::LirValue::Local(local_id));
        }
    }

    fn populate_block_edges(&self, blocks: &mut Vec<lir::LirBasicBlock>) {
        let mut predecessors: HashMap<lir::BasicBlockId, Vec<lir::BasicBlockId>> = HashMap::new();

        for block in blocks.iter_mut() {
            let successors = Self::successors_from_terminator(&block.terminator);
            block.successors = successors.clone();
            for succ in successors {
                predecessors.entry(succ).or_default().push(block.id);
            }
        }

        for block in blocks.iter_mut() {
            if let Some(preds) = predecessors.remove(&block.id) {
                block.predecessors = preds;
            }
        }
    }

    fn successors_from_terminator(terminator: &lir::LirTerminator) -> Vec<lir::BasicBlockId> {
        match terminator {
            lir::LirTerminator::Br(target) => vec![*target],
            lir::LirTerminator::CondBr {
                if_true, if_false, ..
            } => vec![*if_true, *if_false],
            lir::LirTerminator::Switch { default, cases, .. } => {
                let mut targets: Vec<lir::BasicBlockId> = cases.iter().map(|(_, bb)| *bb).collect();
                targets.push(*default);
                targets.sort_unstable();
                targets.dedup();
                targets
            }
            _ => Vec::new(),
        }
    }

    fn transform_call_terminator(
        &mut self,
        func: &mir::Operand,
        args: &[mir::Operand],
        destination: &Option<(mir::Place, mir::BasicBlockId)>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        let function_value = self.transform_operand(func)?;
        let mut lowered_args = Vec::with_capacity(args.len());
        for arg in args {
            lowered_args.push(self.transform_operand(arg)?);
        }

        let call_id = self.next_id();
        let result_type = destination
            .as_ref()
            .and_then(|(place, _)| self.lookup_place_type(place))
            .map(|ty| self.lir_type_from_ty(ty));

        block.instructions.push(lir::LirInstruction {
            id: call_id,
            kind: lir::LirInstructionKind::Call {
                function: function_value,
                args: lowered_args,
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: result_type
                .clone()
                .filter(|ty| !matches!(ty, lir::LirType::Void)),
            debug_info: None,
        });

        if let Some((dest_place, dest_bb)) = destination.as_ref() {
            match result_type {
                Some(ref ty) if !matches!(ty, lir::LirType::Void) => {
                    self.register_map
                        .insert(dest_place.local, lir::LirValue::Register(call_id));
                }
                Some(ref ty) => {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::Constant(lir::LirConstant::Undef(ty.clone())),
                    );
                }
                None => {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::Constant(lir::LirConstant::Undef(lir::LirType::Void)),
                    );
                }
            }
            return Ok(lir::LirTerminator::Br(*dest_bb));
        }

        Ok(lir::LirTerminator::Return(None))
    }

    fn prepare_return_value(&self) -> Option<lir::LirValue> {
        let return_ty = self.current_return_type.clone()?;
        if matches!(return_ty, lir::LirType::Void) {
            return None;
        }

        if let Some(local) = self.return_local {
            if let Some(value) = self.register_map.get(&local) {
                return Some(value.clone());
            }
        }

        Some(lir::LirValue::Constant(lir::LirConstant::Undef(return_ty)))
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
                    Some(lir::LirConstant::Array(constants, lir::LirType::I64))
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
        source_ty: Option<lir::LirType>,
        target_ty: lir::LirType,
    ) -> lir::LirInstructionKind {
        match cast_kind {
            mir::CastKind::Misc => {
                if let Some(src_ty) = source_ty {
                    if self.is_integral_type(&src_ty) && self.is_integral_type(&target_ty) {
                        let src_w = self.type_bit_width(&src_ty);
                        let dst_w = self.type_bit_width(&target_ty);
                        if src_w == dst_w {
                            lir::LirInstructionKind::Bitcast(source, target_ty)
                        } else {
                            lir::LirInstructionKind::SextOrTrunc(source, target_ty)
                        }
                    } else if self.is_float_type(&src_ty) && self.is_float_type(&target_ty) {
                        let src_w = self.type_bit_width(&src_ty);
                        let dst_w = self.type_bit_width(&target_ty);
                        match (src_w, dst_w) {
                            (Some(s), Some(d)) if d > s => {
                                lir::LirInstructionKind::FPExt(source, target_ty)
                            }
                            (Some(s), Some(d)) if d < s => {
                                lir::LirInstructionKind::FPTrunc(source, target_ty)
                            }
                            _ => lir::LirInstructionKind::Bitcast(source, target_ty),
                        }
                    } else if self.is_float_type(&src_ty) && self.is_integral_type(&target_ty) {
                        lir::LirInstructionKind::FPToSI(source, target_ty)
                    } else if self.is_integral_type(&src_ty) && self.is_float_type(&target_ty) {
                        lir::LirInstructionKind::SIToFP(source, target_ty)
                    } else {
                        lir::LirInstructionKind::Bitcast(source, target_ty)
                    }
                } else {
                    lir::LirInstructionKind::Bitcast(source, target_ty)
                }
            }
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
                tracing::warn!(
                    "MIR→LIR: array length {:?} not evaluated; defaulting to 0",
                    other
                );
                0
            }
        }
    }

    fn type_of_operand(&self, operand: &mir::Operand) -> Option<lir::LirType> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => self
                .lookup_place_type(place)
                .map(|ty| self.lir_type_from_ty(ty)),
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Bool(_) => Some(lir::LirType::I1),
                mir::ConstantKind::Int(_) | mir::ConstantKind::UInt(_) => Some(lir::LirType::I64),
                mir::ConstantKind::Float(_) => Some(lir::LirType::F64),
                mir::ConstantKind::Fn(_, _) | mir::ConstantKind::Global(_, _) => {
                    Some(lir::LirType::Ptr(Box::new(lir::LirType::I8)))
                }
                _ => None,
            },
        }
    }

    fn is_integral_type(&self, ty: &lir::LirType) -> bool {
        matches!(
            ty,
            lir::LirType::I1
                | lir::LirType::I8
                | lir::LirType::I16
                | lir::LirType::I32
                | lir::LirType::I64
                | lir::LirType::I128
        )
    }

    fn is_float_type(&self, ty: &lir::LirType) -> bool {
        matches!(ty, lir::LirType::F32 | lir::LirType::F64)
    }

    fn type_bit_width(&self, ty: &lir::LirType) -> Option<u32> {
        match ty {
            lir::LirType::I1 => Some(1),
            lir::LirType::I8 => Some(8),
            lir::LirType::I16 => Some(16),
            lir::LirType::I32 => Some(32),
            lir::LirType::I64 => Some(64),
            lir::LirType::I128 => Some(128),
            lir::LirType::F32 => Some(32),
            lir::LirType::F64 => Some(64),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::span::Span;
    use std::collections::HashMap;

    fn local_decl(ty: Ty) -> mir::LocalDecl {
        mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty,
            user_ty: None,
            source_info: Span::new(0, 0, 0),
        }
    }

    fn int_constant(value: i64) -> mir::Constant {
        mir::Constant {
            span: Span::new(0, 0, 0),
            user_ty: None,
            literal: mir::ConstantKind::Int(value),
        }
    }

    #[test]
    fn builds_function_signature_and_locals() {
        let mut bodies = HashMap::new();

        let return_ty = Ty::int(IntTy::I32);
        let param_ty = Ty::int(IntTy::I32);

        let mut body = mir::Body::new(
            vec![mir::BasicBlockData::new(Some(mir::Terminator {
                source_info: Span::new(0, 0, 0),
                kind: mir::TerminatorKind::Return,
            }))],
            vec![local_decl(return_ty.clone()), local_decl(param_ty.clone())],
            1,
            Span::new(0, 0, 0),
        );
        body.return_local = 0;

        bodies.insert(mir::BodyId::new(0), body);

        let mir_program = mir::Program {
            items: vec![mir::Item {
                mir_id: 0,
                kind: mir::ItemKind::Function(mir::Function {
                    sig: mir::FunctionSig {
                        inputs: vec![param_ty.clone()],
                        output: return_ty.clone(),
                    },
                    body_id: mir::BodyId::new(0),
                }),
            }],
            bodies,
        };

        let mut generator = LirGenerator::new();
        let lir_program = generator
            .transform(mir_program)
            .expect("lowering should succeed");

        assert_eq!(lir_program.functions.len(), 1);
        let lir_func = &lir_program.functions[0];

        assert_eq!(lir_func.signature.params, vec![lir::LirType::I32]);
        assert_eq!(lir_func.signature.return_type, lir::LirType::I32);
        assert_eq!(lir_func.locals.len(), 2);
        assert!(!lir_func.locals[0].is_argument);
        assert!(lir_func.locals[1].is_argument);
    }

    #[test]
    fn lowers_general_call_and_branches() {
        let mut bodies = HashMap::new();

        let return_ty = Ty::int(IntTy::I32);
        let param_ty = Ty::int(IntTy::I32);
        let temp_ty = Ty::int(IntTy::I32);

        let mut block0 = mir::BasicBlockData::new(None);
        block0.terminator = Some(mir::Terminator {
            source_info: Span::new(0, 0, 0),
            kind: mir::TerminatorKind::Call {
                func: mir::Operand::Constant(mir::Constant {
                    span: Span::new(0, 0, 0),
                    user_ty: None,
                    literal: mir::ConstantKind::Fn("foo".to_string(), return_ty.clone()),
                }),
                args: vec![mir::Operand::Constant(int_constant(1))],
                destination: Some((mir::Place::from_local(2), 1)),
                cleanup: None,
                from_hir_call: false,
                fn_span: Span::new(0, 0, 0),
            },
        });

        let mut block1 = mir::BasicBlockData::new(None);
        block1.statements.push(mir::Statement {
            source_info: Span::new(0, 0, 0),
            kind: mir::StatementKind::Assign(
                mir::Place::from_local(0),
                mir::Rvalue::Use(mir::Operand::Move(mir::Place::from_local(2))),
            ),
        });
        block1.terminator = Some(mir::Terminator {
            source_info: Span::new(0, 0, 0),
            kind: mir::TerminatorKind::Return,
        });

        let mut body = mir::Body::new(
            vec![block0, block1],
            vec![
                local_decl(return_ty.clone()),
                local_decl(param_ty.clone()),
                local_decl(temp_ty),
            ],
            1,
            Span::new(0, 0, 0),
        );
        body.return_local = 0;

        bodies.insert(mir::BodyId::new(0), body);

        let mir_program = mir::Program {
            items: vec![mir::Item {
                mir_id: 0,
                kind: mir::ItemKind::Function(mir::Function {
                    sig: mir::FunctionSig {
                        inputs: vec![param_ty.clone()],
                        output: return_ty.clone(),
                    },
                    body_id: mir::BodyId::new(0),
                }),
            }],
            bodies,
        };

        let mut generator = LirGenerator::new();
        let lir_program = generator
            .transform(mir_program)
            .expect("lowering should succeed");

        let lir_func = &lir_program.functions[0];
        assert_eq!(lir_func.basic_blocks.len(), 2);

        let entry_block = &lir_func.basic_blocks[0];
        assert_eq!(entry_block.successors, vec![1]);
        assert_eq!(entry_block.instructions.len(), 1);
        match &entry_block.instructions[0].kind {
            lir::LirInstructionKind::Call { function, .. } => match function {
                lir::LirValue::Global(name, _) => assert_eq!(name, "foo"),
                other => panic!("expected global call, got {:?}", other),
            },
            other => panic!("expected call instruction, got {:?}", other),
        }
        assert!(matches!(entry_block.terminator, lir::LirTerminator::Br(1)));

        let successor_block = &lir_func.basic_blocks[1];
        assert_eq!(successor_block.predecessors, vec![0]);
        match &successor_block.terminator {
            lir::LirTerminator::Return(Some(lir::LirValue::Register(_))) => {}
            other => panic!("expected return with register value, got {:?}", other),
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
