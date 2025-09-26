use fp_core::ast::Value;
use fp_core::error::Result;
use fp_core::mir::ty::{ConstKind, ConstValue, FloatTy, IntTy, Scalar, Ty, TyKind, UintTy};
use fp_core::{lir, mir};
use std::collections::{HashMap, HashSet, VecDeque};

mod const_eval;
#[cfg(test)]
mod tests;
// Internal submodules; items are used via inherent methods

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
    const_globals: HashMap<String, Value>,
    mutable_locals: HashSet<mir::LocalId>,
    local_storage: HashMap<mir::LocalId, LocalStorage>,
    entry_allocas: Vec<lir::LirInstruction>,
    queued_instructions: Vec<lir::LirInstruction>,
}

#[derive(Clone)]
struct LocalStorage {
    ptr_value: lir::LirValue,
    element_type: lir::LirType,
    alignment: u32,
}

impl LirGenerator {
    /// Create a new LIR generator
    pub fn new(const_globals: HashMap<String, Value>) -> Self {
        Self {
            next_lir_id: 0,
            next_label: 0,
            register_map: HashMap::new(),
            current_function: None,
            const_values: HashMap::new(),
            local_types: Vec::new(),
            current_return_type: None,
            return_local: None,
            const_globals,
            mutable_locals: HashSet::new(),
            local_storage: HashMap::new(),
            entry_allocas: Vec::new(),
            queued_instructions: Vec::new(),
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
            self.mutable_locals = self.compute_mutable_locals(mir_body);
            self.initialize_local_storage();
            lir_func.locals = self.build_lir_locals(mir_body);
            self.seed_argument_registers(mir_body);

            let block_order = self.compute_block_order(mir_body);
            for &bb_idx in &block_order {
                let bb = &mir_body.basic_blocks[bb_idx];
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

        if bb_id == 0 && !self.entry_allocas.is_empty() {
            lir_block.instructions.extend(self.entry_allocas.clone());
            self.entry_allocas.clear();
        }

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
        let mut instructions = Vec::new();
        let has_storage = self.local_storage.contains_key(&place.local);

        match rvalue {
            mir::Rvalue::Use(operand) => {
                let value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());

                if has_storage {
                    if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                        let store_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: store_id,
                            kind: lir::LirInstructionKind::Store {
                                value: value.clone(),
                                address: storage.ptr_value.clone(),
                                alignment: Some(storage.alignment),
                                volatile: false,
                            },
                            type_hint: None,
                            debug_info: None,
                        });
                    }
                }

                self.register_map.insert(place.local, value);
                Ok(instructions)
            }
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let lhs_value = self.transform_operand(lhs)?;
                instructions.extend(self.take_queued_instructions());
                let rhs_value = self.transform_operand(rhs)?;
                instructions.extend(self.take_queued_instructions());

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
                let type_hint = self
                    .lookup_place_type(place)
                    .map(|ty| self.lir_type_from_ty(ty))
                    .or_else(|| Some(lir::LirType::I32));

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint,
                    debug_info: None,
                });

                let result_value = lir::LirValue::Register(instr_id);
                if has_storage {
                    if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                        let store_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: store_id,
                            kind: lir::LirInstructionKind::Store {
                                value: result_value.clone(),
                                address: storage.ptr_value.clone(),
                                alignment: Some(storage.alignment),
                                volatile: false,
                            },
                            type_hint: None,
                            debug_info: None,
                        });
                    }
                }

                self.register_map.insert(place.local, result_value);
                Ok(instructions)
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
                self.register_map.insert(
                    place.local,
                    lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I64)),
                );
                Ok(Vec::new())
            }
            mir::Rvalue::Cast(cast_kind, operand, _ty) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());
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

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: instr_kind,
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });

                let result_value = lir::LirValue::Register(instr_id);
                if has_storage {
                    if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                        let store_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: store_id,
                            kind: lir::LirInstructionKind::Store {
                                value: result_value.clone(),
                                address: storage.ptr_value.clone(),
                                alignment: Some(storage.alignment),
                                volatile: false,
                            },
                            type_hint: None,
                            debug_info: None,
                        });
                    }
                }

                self.register_map.insert(place.local, result_value);
                Ok(instructions)
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
                block.instructions.extend(self.take_queued_instructions());
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
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                    let load_id = self.next_id();
                    let load_instruction = lir::LirInstruction {
                        id: load_id,
                        kind: lir::LirInstructionKind::Load {
                            address: storage.ptr_value.clone(),
                            alignment: Some(storage.alignment),
                            volatile: false,
                        },
                        type_hint: Some(storage.element_type.clone()),
                        debug_info: None,
                    };
                    self.queued_instructions.push(load_instruction);
                    let value = lir::LirValue::Register(load_id);
                    self.register_map.insert(place.local, value.clone());
                    Ok(value)
                } else {
                    self.get_or_create_register_for_place(place)
                }
            }
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Fn(name, ty) => Ok(lir::LirValue::Global(
                    name.clone(),
                    self.lir_type_from_ty(ty),
                )),
                mir::ConstantKind::Global(name, ty) => {
                    if let Some(constant) = self.const_global_to_lir_constant(name, ty) {
                        Ok(lir::LirValue::Constant(constant))
                    } else {
                        Ok(lir::LirValue::Global(
                            name.clone(),
                            self.lir_type_from_ty(ty),
                        ))
                    }
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

    fn const_global_to_lir_constant(&self, name: &str, ty: &Ty) -> Option<lir::LirConstant> {
        let value = self.const_globals.get(name)?;
        match (&ty.kind, value) {
            (TyKind::Int(_), Value::Int(v)) => {
                let lir_ty = self.lir_type_from_ty(ty);
                Some(lir::LirConstant::Int(v.value, lir_ty))
            }
            (TyKind::Uint(_), Value::Int(v)) => {
                let lir_ty = self.lir_type_from_ty(ty);
                Some(lir::LirConstant::UInt(v.value as u64, lir_ty))
            }
            (TyKind::Bool, Value::Bool(b)) => Some(lir::LirConstant::Bool(b.value)),
            (TyKind::Float(_), Value::Decimal(d)) => {
                let lir_ty = self.lir_type_from_ty(ty);
                Some(lir::LirConstant::Float(d.value, lir_ty))
            }
            (TyKind::Tuple(elements), Value::Unit(_)) if elements.is_empty() => {
                Some(lir::LirConstant::Struct(Vec::new(), lir::LirType::Void))
            }
            _ => None,
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
        self.mutable_locals.clear();
        self.local_storage.clear();
        self.entry_allocas.clear();
        self.queued_instructions.clear();
    }

    fn compute_mutable_locals(&self, mir_body: &mir::Body) -> HashSet<mir::LocalId> {
        let mut assignment_counts: HashMap<mir::LocalId, usize> = HashMap::new();
        for basic_block in &mir_body.basic_blocks {
            for stmt in &basic_block.statements {
                if let mir::StatementKind::Assign(place, _) = &stmt.kind {
                    *assignment_counts.entry(place.local).or_insert(0) += 1;
                }
            }
        }

        assignment_counts
            .into_iter()
            .filter_map(|(local, count)| if count > 1 { Some(local) } else { None })
            .collect()
    }

    fn initialize_local_storage(&mut self) {
        self.entry_allocas.clear();
        self.local_storage.clear();

        let locals: Vec<_> = self.mutable_locals.clone().into_iter().collect();
        for local in locals {
            if let Some(return_local) = self.return_local {
                if local == return_local {
                    continue;
                }
            }
            let local_index = local as usize;
            if local_index >= self.local_types.len() {
                continue;
            }

            let ty = &self.local_types[local_index];
            if Self::is_zero_sized(ty) {
                continue;
            }

            let lir_ty = self.lir_type_from_ty(ty);
            let alignment = Self::alignment_for_lir_type(&lir_ty);
            if alignment == 0 {
                continue;
            }

            let alloca_id = self.next_id();
            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
            let size_value = lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
            self.entry_allocas.push(lir::LirInstruction {
                id: alloca_id,
                kind: lir::LirInstructionKind::Alloca {
                    size: size_value,
                    alignment,
                },
                type_hint: Some(pointer_type.clone()),
                debug_info: None,
            });

            self.local_storage.insert(
                local,
                LocalStorage {
                    ptr_value: lir::LirValue::Register(alloca_id),
                    element_type: lir_ty,
                    alignment,
                },
            );
        }

        // Ensure entry allocas appear once at the top of the entry block
        if self.entry_allocas.is_empty() {
            return;
        }
    }

    fn get_or_create_register_for_place(&mut self, place: &mir::Place) -> Result<lir::LirValue> {
        if let Some(storage) = self.local_storage.get(&place.local) {
            return Ok(storage.ptr_value.clone());
        }
        if let Some(existing_reg) = self.register_map.get(&place.local) {
            Ok(existing_reg.clone())
        } else {
            if let Some(place_ty) = self.lookup_place_type(place) {
                if Self::is_zero_sized(&place_ty) {
                    let lir_ty = self.lir_type_from_ty(&place_ty);
                    let value =
                        lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), lir_ty));
                    self.register_map.insert(place.local, value.clone());
                    return Ok(value);
                }
            }
            Err(crate::error::optimization_error(format!(
                "MIR→LIR: missing value for local {} (place={:?}); cannot lower MIR",
                place.local, place
            )))
        }
    }

    fn alignment_for_lir_type(ty: &lir::LirType) -> u32 {
        match ty {
            lir::LirType::I1 => 1,
            lir::LirType::I8 => 1,
            lir::LirType::I16 => 2,
            lir::LirType::I32 => 4,
            lir::LirType::I64 => 8,
            lir::LirType::I128 => 16,
            lir::LirType::F32 => 4,
            lir::LirType::F64 => 8,
            lir::LirType::Ptr(_) => 8,
            lir::LirType::Array(element_type, _) => Self::alignment_for_lir_type(element_type),
            lir::LirType::Struct { fields, .. } => fields
                .iter()
                .map(Self::alignment_for_lir_type)
                .max()
                .unwrap_or(1),
            _ => 8,
        }
    }

    fn take_queued_instructions(&mut self) -> Vec<lir::LirInstruction> {
        std::mem::take(&mut self.queued_instructions)
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
        let mut instructions = Vec::new();
        let mut lir_values = Vec::with_capacity(fields.len());
        let mut constants = Vec::with_capacity(fields.len());
        let mut all_constants = true;

        for operand in fields {
            let value = self.transform_operand(operand)?;
            instructions.extend(self.take_queued_instructions());
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
                return Ok(instructions);
            }
        }

        if all_constants {
            if let Some(place_ty) = self.lookup_place_type(place) {
                if let Some(constant) = self.constant_from_aggregate(kind, constants, place_ty) {
                    self.register_map
                        .insert(place.local, lir::LirValue::Constant(constant));
                    return Ok(instructions);
                }
            }
        }

        if let Some(place_ty) = self.lookup_place_type(place) {
            let aggregate_ty = self.lir_type_from_ty(place_ty);
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
        Ok(instructions)
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
        block.instructions.extend(self.take_queued_instructions());
        let mut lowered_args = Vec::with_capacity(args.len());
        for arg in args {
            lowered_args.push(self.transform_operand(arg)?);
            block.instructions.extend(self.take_queued_instructions());
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

    fn compute_block_order(&self, mir_body: &mir::Body) -> Vec<usize> {
        let mut order = Vec::new();
        let block_count = mir_body.basic_blocks.len();
        if block_count == 0 {
            return order;
        }

        let mut visited = vec![false; block_count];
        let mut queue = VecDeque::new();
        queue.push_back(0usize);
        visited[0] = true;

        while let Some(bb_idx) = queue.pop_front() {
            order.push(bb_idx);
            let successors = Self::mir_successors(&mir_body.basic_blocks[bb_idx]);
            for succ in successors {
                let succ_idx = succ as usize;
                if succ_idx < block_count && !visited[succ_idx] {
                    visited[succ_idx] = true;
                    queue.push_back(succ_idx);
                }
            }
        }

        // Append any unreachable blocks deterministically to maintain coverage
        for idx in 0..block_count {
            if !visited[idx] {
                order.push(idx);
            }
        }

        order
    }

    fn mir_successors(bb: &mir::BasicBlockData) -> Vec<mir::BasicBlockId> {
        let mut successors = Vec::new();
        if let Some(terminator) = &bb.terminator {
            match &terminator.kind {
                mir::TerminatorKind::Goto { target } => successors.push(*target),
                mir::TerminatorKind::SwitchInt { targets, .. } => {
                    successors.extend(targets.targets.iter().copied());
                    successors.push(targets.otherwise);
                }
                mir::TerminatorKind::Call {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some((_, dest_bb)) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Drop { target, unwind, .. }
                | mir::TerminatorKind::DropAndReplace { target, unwind, .. } => {
                    successors.push(*target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::Assert {
                    target, cleanup, ..
                } => {
                    successors.push(*target);
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Yield { resume, drop, .. } => {
                    successors.push(*resume);
                    if let Some(drop_bb) = drop {
                        successors.push(*drop_bb);
                    }
                }
                mir::TerminatorKind::FalseEdge {
                    real_target,
                    imaginary_target,
                } => {
                    successors.push(*real_target);
                    successors.push(*imaginary_target);
                }
                mir::TerminatorKind::FalseUnwind {
                    real_target,
                    unwind,
                } => {
                    successors.push(*real_target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::InlineAsm {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some(dest_bb) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                _ => {}
            }
        }

        successors.sort_unstable();
        successors.dedup();
        successors
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

    fn is_zero_sized(ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Tuple(ref elements) if elements.is_empty())
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

impl super::IrTransform<mir::Program, lir::LirProgram> for LirGenerator {
    fn transform(&mut self, source: mir::Program) -> Result<lir::LirProgram> {
        LirGenerator::transform(self, source)
    }
}

impl Default for LirGenerator {
    fn default() -> Self {
        Self::new(HashMap::new())
    }
}
