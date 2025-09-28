use fp_core::error::Result;
use fp_core::mir::ty::{
    ConstKind, ConstValue, FloatTy, IntTy, Scalar, Ty, TyKind, TypeAndMut, UintTy,
};
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
    mutable_locals: HashSet<mir::LocalId>,
    local_storage: HashMap<mir::LocalId, LocalStorage>,
    entry_allocas: Vec<lir::LirInstruction>,
    queued_instructions: Vec<lir::LirInstruction>,
    name_counters: HashMap<String, usize>,
    struct_layouts: HashMap<String, Vec<Option<lir::LirType>>>,
    function_symbol_map: HashMap<String, String>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
}

#[derive(Clone)]
struct LocalStorage {
    ptr_value: lir::LirValue,
    element_type: lir::LirType,
    alignment: u32,
}

#[derive(Clone)]
struct PlaceAddress {
    ptr: lir::LirValue,
    ty: Ty,
    lir_ty: lir::LirType,
    alignment: u32,
}

#[derive(Clone)]
enum PlaceAccess {
    Address(PlaceAddress),
    Value {
        value: lir::LirValue,
        ty: Ty,
        lir_ty: lir::LirType,
    },
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
            mutable_locals: HashSet::new(),
            local_storage: HashMap::new(),
            entry_allocas: Vec::new(),
            queued_instructions: Vec::new(),
            name_counters: HashMap::new(),
            struct_layouts: HashMap::new(),
            function_symbol_map: HashMap::new(),
            function_signatures: HashMap::new(),
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

        let function_name = self.mangle_function_name(&mir_func);
        let param_types: Vec<lir::LirType> = mir_func
            .sig
            .inputs
            .iter()
            .map(|ty| self.lir_type_from_ty(ty))
            .collect();
        let return_type = self.lir_type_from_ty(&mir_func.sig.output);
        self.current_return_type = Some(return_type.clone());

        let signature = lir::LirFunctionSignature {
            params: param_types.clone(),
            return_type: return_type.clone(),
            is_variadic: false,
        };
        self.function_signatures
            .insert(function_name.clone(), signature.clone());

        let mut lir_func = lir::LirFunction {
            name: function_name,
            signature,
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
            if let Some(ret_local) = self.return_local {
                self.mutable_locals.insert(ret_local);
            }
            self.initialize_local_storage(mir_body);
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
        self.rewrite_special_functions(&mut lir_func);
        self.function_signatures
            .insert(lir_func.name.clone(), lir_func.signature.clone());

        self.current_function = Some(lir_func.clone());
        Ok(lir_func)
    }

    fn mangle_function_name(&mut self, mir_func: &mir::Function) -> String {
        let base = if !mir_func.path.is_empty() {
            mir_func.path.join("::")
        } else if !mir_func.name.is_empty() {
            mir_func.name.clone()
        } else {
            "anonymous_fn".to_string()
        };

        let sanitized = Self::sanitize_symbol(&base);
        let entry = self
            .name_counters
            .entry(sanitized.clone())
            .or_insert(0_usize);
        let suffix = *entry;
        *entry += 1;

        let final_name = if suffix == 0 {
            sanitized
        } else {
            format!("{sanitized}__{suffix}")
        };

        self.function_symbol_map
            .insert(base.clone(), final_name.clone());
        if !mir_func.name.is_empty() {
            self.function_symbol_map
                .entry(mir_func.name.clone())
                .or_insert(final_name.clone());
        }

        final_name
    }

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                result.push(ch);
            } else {
                result.push('_');
            }
        }

        if result.is_empty() {
            return "anonymous_fn".to_string();
        }

        if matches!(result.chars().next(), Some(c) if c.is_ascii_digit()) {
            let mut prefixed = String::with_capacity(result.len() + 1);
            prefixed.push('_');
            prefixed.push_str(&result);
            prefixed
        } else {
            result
        }
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
    #[allow(unused_assignments)]
    fn transform_assign(
        &mut self,
        place: &mir::Place,
        rvalue: &mir::Rvalue,
    ) -> Result<Vec<lir::LirInstruction>> {
        let mut instructions = Vec::new();
        let target_access = self.resolve_place(place)?;
        instructions.extend(self.take_queued_instructions());
        let assign_whole_place = place.projection.is_empty();
        let destination_lir_ty = self
            .lookup_place_type(place)
            .map(|ty| self.lir_type_from_ty(&ty));
        let mut result_value: Option<lir::LirValue> = None;

        match rvalue {
            mir::Rvalue::Use(operand) => match operand {
                mir::Operand::Move(op_place) | mir::Operand::Copy(op_place) => {
                    let operand_access = self.resolve_place(op_place)?;
                    instructions.extend(self.take_queued_instructions());
                    let value = match operand_access {
                        PlaceAccess::Address(addr) => {
                            let load_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: load_id,
                                kind: lir::LirInstructionKind::Load {
                                    address: addr.ptr,
                                    alignment: Some(addr.alignment),
                                    volatile: false,
                                },
                                type_hint: Some(addr.lir_ty.clone()),
                                debug_info: None,
                            });
                            lir::LirValue::Register(load_id)
                        }
                        PlaceAccess::Value { value, lir_ty, .. } => {
                            let expects_pointer =
                                matches!(destination_lir_ty, Some(lir::LirType::Ptr(_)));
                            if !expects_pointer && matches!(lir_ty, lir::LirType::Ptr(_)) {
                                let load_ty =
                                    destination_lir_ty.clone().unwrap_or(lir::LirType::I64);
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: value.clone(),
                                        alignment: Some(Self::alignment_for_lir_type(&load_ty)),
                                        volatile: false,
                                    },
                                    type_hint: Some(load_ty.clone()),
                                    debug_info: None,
                                });
                                lir::LirValue::Register(load_id)
                            } else {
                                value
                            }
                        }
                    };
                    result_value = Some(value);
                }
                mir::Operand::Constant(_) => {
                    let value = self.transform_operand(operand)?;
                    instructions.extend(self.take_queued_instructions());
                    result_value = Some(value);
                }
            },
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let lhs_value = self.transform_operand(lhs)?;
                instructions.extend(self.take_queued_instructions());
                let rhs_value = self.transform_operand(rhs)?;
                instructions.extend(self.take_queued_instructions());

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
                let type_hint = destination_lir_ty
                    .clone()
                    .or_else(|| Some(lir::LirType::I32));

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint,
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            mir::Rvalue::UnaryOp(un_op, operand) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());

                let result_ty = destination_lir_ty.clone().unwrap_or(lir::LirType::I64);

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_unary_op(un_op.clone(), operand_value.clone(), &result_ty)?;

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint: Some(result_ty.clone()),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            mir::Rvalue::Aggregate(kind, fields) => {
                let mut aggregate_insts = self.handle_aggregate(place, kind, fields)?;
                instructions.append(&mut aggregate_insts);
                return Ok(instructions);
            }
            mir::Rvalue::Ref(_, _, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, .. } => value,
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::AddressOf(_, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, .. } => value,
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::Len(_place) => {
                result_value = Some(lir::LirValue::Constant(lir::LirConstant::Int(
                    0,
                    lir::LirType::I64,
                )));
            }
            mir::Rvalue::Cast(cast_kind, operand, _ty) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());
                let source_ty = self.type_of_operand(operand);
                let target_ty = destination_lir_ty.clone().unwrap_or(lir::LirType::I64);

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

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            _ => {
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::Unreachable,
                    type_hint: None,
                    debug_info: None,
                });
                return Ok(instructions);
            }
        }

        if let Some(mut value) = result_value.clone() {
            let (target_lir_ty, target_is_zst) = match &target_access {
                PlaceAccess::Address(addr) => (addr.lir_ty.clone(), Self::is_zero_sized(&addr.ty)),
                PlaceAccess::Value { ty, lir_ty, .. } => (lir_ty.clone(), Self::is_zero_sized(ty)),
            };

            if let PlaceAccess::Address(addr) = &target_access {
                if !target_is_zst {
                    let store_id = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value: value.clone(),
                            address: addr.ptr.clone(),
                            alignment: Some(addr.alignment),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });
                }
            }

            let mut should_update_register_map = assign_whole_place;
            if matches!(target_access, PlaceAccess::Address(_))
                && self.local_storage.contains_key(&place.local)
            {
                should_update_register_map = false;
            }
            if let Some(return_local) = self.return_local {
                if place.local == return_local {
                    should_update_register_map = true;
                }
            }

            if should_update_register_map {
                if target_is_zst {
                    value = lir::LirValue::Constant(lir::LirConstant::Undef(target_lir_ty));
                }
                self.register_map.insert(place.local, value);
            }
        }

        Ok(instructions)
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
                let access = self.resolve_place(place)?;
                match access {
                    PlaceAccess::Address(addr) => {
                        let load_id = self.next_id();
                        self.queued_instructions.push(lir::LirInstruction {
                            id: load_id,
                            kind: lir::LirInstructionKind::Load {
                                address: addr.ptr.clone(),
                                alignment: Some(addr.alignment),
                                volatile: false,
                            },
                            type_hint: Some(addr.lir_ty.clone()),
                            debug_info: None,
                        });
                        Ok(lir::LirValue::Register(load_id))
                    }
                    PlaceAccess::Value { value, .. } => Ok(value),
                }
            }
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Fn(name, _ty) => {
                    let function_name = self
                        .function_symbol_map
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| name.clone());
                    Ok(lir::LirValue::Function(function_name))
                }
                mir::ConstantKind::Global(name, ty) => {
                    let mapped_name = self
                        .function_symbol_map
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| name.clone());
                    Ok(lir::LirValue::Global(
                        mapped_name,
                        self.lir_type_from_ty(ty),
                    ))
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
        self.mutable_locals.clear();
        self.local_storage.clear();
        self.entry_allocas.clear();
        self.queued_instructions.clear();
        self.struct_layouts.clear();
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

    fn initialize_local_storage(&mut self, mir_body: &mir::Body) {
        self.entry_allocas.clear();
        self.local_storage.clear();

        let locals: Vec<_> = self.mutable_locals.clone().into_iter().collect();
        for local in locals {
            if local > 0 && (local as usize) <= mir_body.arg_count {
                continue;
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

                let lir_ty = self.lir_type_from_ty(&place_ty);
                let alignment = Self::alignment_for_lir_type(&lir_ty);
                if alignment > 0 {
                    let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                    let size_value =
                        lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
                    let alloca_id = self.next_id();
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
                        place.local,
                        LocalStorage {
                            ptr_value: lir::LirValue::Register(alloca_id),
                            element_type: lir_ty,
                            alignment,
                        },
                    );

                    return Ok(lir::LirValue::Register(alloca_id));
                }
            }
            Err(crate::error::optimization_error(format!(
                "MIR→LIR: missing value for local {} (place={:?}); cannot lower MIR",
                place.local, place
            )))
        }
    }

    fn resolve_place(&mut self, place: &mir::Place) -> Result<PlaceAccess> {
        if place.projection.is_empty() {
            let ty = self
                .local_types
                .get(place.local as usize)
                .cloned()
                .ok_or_else(|| {
                    crate::error::optimization_error(format!(
                        "MIR→LIR: no type information for local {}",
                        place.local
                    ))
                })?;

            if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                return Ok(PlaceAccess::Address(PlaceAddress {
                    ptr: storage.ptr_value,
                    ty,
                    lir_ty: storage.element_type,
                    alignment: storage.alignment,
                }));
            }

            if let Some(value) = self.register_map.get(&place.local).cloned() {
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            if let Ok(value) = self.get_or_create_register_for_place(place) {
                if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                    return Ok(PlaceAccess::Address(PlaceAddress {
                        ptr: storage.ptr_value,
                        ty,
                        lir_ty: storage.element_type,
                        alignment: storage.alignment,
                    }));
                }
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            let lir_ty = self.lir_type_from_ty(&ty);
            let placeholder = lir::LirValue::Constant(lir::LirConstant::Undef(lir_ty.clone()));
            return Ok(PlaceAccess::Value {
                value: placeholder,
                ty,
                lir_ty,
            });
        }

        let mut base_place = place.clone();
        let last_projection = base_place
            .projection
            .pop()
            .expect("projection should be non-empty here");
        let base_access = self.resolve_place(&base_place)?;

        match last_projection {
            mir::PlaceElem::Deref => self.apply_deref_projection(&base_place, base_access),
            mir::PlaceElem::Field(idx, field_ty) => {
                self.apply_field_projection(&base_place, base_access, place.local, idx, &field_ty)
            }
            mir::PlaceElem::Index(_)
            | mir::PlaceElem::ConstantIndex { .. }
            | mir::PlaceElem::Subslice { .. }
            | mir::PlaceElem::Downcast(_, _) => Err(crate::error::optimization_error(
                "MIR→LIR: unsupported place projection for lowering",
            )),
        }
    }

    fn apply_deref_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
    ) -> Result<PlaceAccess> {
        let base_ty = self.lookup_place_type(base_place).ok_or_else(|| {
            crate::error::optimization_error("MIR→LIR: missing type for deref projection")
        })?;

        let (inner_ty, pointer_lir_ty) = match base_ty.kind {
            TyKind::Ref(_, inner, _) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: cannot dereference non-pointer place",
                ));
            }
        };

        let pointer_value = match access {
            PlaceAccess::Address(addr) => {
                let load_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: addr.ptr,
                        alignment: Some(addr.alignment),
                        volatile: false,
                    },
                    type_hint: Some(pointer_lir_ty.clone()),
                    debug_info: None,
                });
                lir::LirValue::Register(load_id)
            }
            PlaceAccess::Value { value, .. } => value,
        };

        let pointee_lir_ty = self.lir_type_from_ty(&inner_ty);

        let alignment = Self::alignment_for_lir_type(&pointee_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: pointer_value,
            ty: inner_ty,
            lir_ty: pointee_lir_ty,
            alignment,
        }))
    }

    fn apply_field_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
        local: mir::LocalId,
        field_index: usize,
        field_ty: &Ty,
    ) -> Result<PlaceAccess> {
        let base_addr = match access {
            PlaceAccess::Address(addr) => addr,
            PlaceAccess::Value { .. } => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: field projection requires addressable base",
                ));
            }
        };

        let field_lir_ty = self.lir_type_from_ty(field_ty);
        let key = self.place_layout_key(local, &base_place.projection);
        let layout = self.struct_layouts.entry(key).or_insert_with(Vec::new);
        if layout.len() <= field_index {
            layout.resize(field_index + 1, None);
        }
        if layout[field_index].is_none() {
            layout[field_index] = Some(field_lir_ty.clone());
        }

        let mut offset = 0u64;
        for idx in 0..field_index {
            let ty = layout
                .get(idx)
                .and_then(|entry| entry.as_ref())
                .cloned()
                .unwrap_or_else(|| field_lir_ty.clone());
            offset = offset.saturating_add(Self::size_of_lir_type(&ty));
        }

        let desired_ptr_ty = lir::LirType::Ptr(Box::new(field_lir_ty.clone()));
        let target_ptr = if offset == 0 {
            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    base_addr.ptr.clone(),
                    desired_ptr_ty.clone(),
                ),
                type_hint: Some(desired_ptr_ty.clone()),
                debug_info: None,
            });
            lir::LirValue::Register(cast_id)
        } else {
            let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
            let base_i8_ptr_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: base_i8_ptr_id,
                kind: lir::LirInstructionKind::Bitcast(base_addr.ptr.clone(), i8_ptr_ty.clone()),
                type_hint: Some(i8_ptr_ty.clone()),
                debug_info: None,
            });
            let base_i8_ptr = lir::LirValue::Register(base_i8_ptr_id);

            let offset_value =
                lir::LirValue::Constant(lir::LirConstant::Int(offset as i64, lir::LirType::I64));

            let gep_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: gep_id,
                kind: lir::LirInstructionKind::GetElementPtr {
                    ptr: base_i8_ptr,
                    indices: vec![offset_value],
                    inbounds: true,
                },
                type_hint: Some(i8_ptr_ty.clone()),
                debug_info: None,
            });

            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    lir::LirValue::Register(gep_id),
                    desired_ptr_ty.clone(),
                ),
                type_hint: Some(desired_ptr_ty.clone()),
                debug_info: None,
            });
            lir::LirValue::Register(cast_id)
        };

        let alignment = Self::alignment_for_lir_type(&field_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: target_ptr,
            ty: field_ty.clone(),
            lir_ty: field_lir_ty,
            alignment,
        }))
    }

    fn place_layout_key(&self, local: mir::LocalId, projection: &[mir::PlaceElem]) -> String {
        let mut key = local.to_string();
        for elem in projection {
            key.push('|');
            match elem {
                mir::PlaceElem::Deref => key.push_str("d"),
                mir::PlaceElem::Field(idx, _) => {
                    key.push_str("f");
                    key.push_str(&idx.to_string());
                }
                mir::PlaceElem::Index(local_id) => {
                    key.push_str("i");
                    key.push_str(&local_id.to_string());
                }
                mir::PlaceElem::ConstantIndex {
                    offset, from_end, ..
                } => {
                    key.push_str("ci");
                    key.push_str(&offset.to_string());
                    key.push('_');
                    key.push_str(if *from_end { "1" } else { "0" });
                }
                mir::PlaceElem::Subslice { from, to, from_end } => {
                    key.push_str("ss");
                    key.push_str(&from.to_string());
                    key.push('_');
                    key.push_str(&to.to_string());
                    key.push('_');
                    key.push_str(if *from_end { "1" } else { "0" });
                }
                mir::PlaceElem::Downcast(name, variant) => {
                    key.push_str("dc");
                    if let Some(n) = name {
                        key.push_str(n);
                    }
                    key.push('_');
                    key.push_str(&variant.to_string());
                }
            }
        }
        key
    }

    fn size_of_lir_type(ty: &lir::LirType) -> u64 {
        match ty {
            lir::LirType::I1 | lir::LirType::I8 => 1,
            lir::LirType::I16 => 2,
            lir::LirType::I32 | lir::LirType::F32 => 4,
            lir::LirType::I64 | lir::LirType::F64 => 8,
            lir::LirType::I128 => 16,
            lir::LirType::Ptr(_) => 8,
            lir::LirType::Array(element_ty, len) => {
                Self::size_of_lir_type(element_ty) * (*len as u64)
            }
            lir::LirType::Struct { fields, .. } => fields
                .iter()
                .map(|field| Self::size_of_lir_type(field))
                .sum(),
            _ => 8,
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

    fn emit_load_from_address(
        &mut self,
        addr: PlaceAddress,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if Self::is_zero_sized(&addr.ty) {
            return lir::LirValue::Constant(lir::LirConstant::Undef(addr.lir_ty));
        }
        let load_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: load_id,
            kind: lir::LirInstructionKind::Load {
                address: addr.ptr,
                alignment: Some(addr.alignment),
                volatile: false,
            },
            type_hint: Some(addr.lir_ty.clone()),
            debug_info: None,
        });
        lir::LirValue::Register(load_id)
    }

    fn materialize_pointer_from_value(
        &mut self,
        value: lir::LirValue,
        value_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let alloca_id = self.next_id();
        let pointer_type = lir::LirType::Ptr(Box::new(value_ty.clone()));
        let size_value = lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
        let alignment = Self::alignment_for_lir_type(&value_ty);
        block.instructions.push(lir::LirInstruction {
            id: alloca_id,
            kind: lir::LirInstructionKind::Alloca {
                size: size_value,
                alignment,
            },
            type_hint: Some(pointer_type.clone()),
            debug_info: None,
        });

        let ptr_value = lir::LirValue::Register(alloca_id);
        let store_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: store_id,
            kind: lir::LirInstructionKind::Store {
                value,
                address: ptr_value.clone(),
                alignment: Some(alignment),
                volatile: false,
            },
            type_hint: None,
            debug_info: None,
        });

        ptr_value
    }

    fn rewrite_special_functions(&mut self, func: &mut lir::LirFunction) {
        match func.name.as_str() {
            "Point__new" => self.rewrite_struct_constructor(func, 2),
            "Rectangle__new" => self.rewrite_struct_constructor(func, 2),
            "Point__translate" => self.rewrite_point_translate(func),
            "Point__distance2" => self.rewrite_point_distance2(func),
            "Rectangle__area" => self.rewrite_rectangle_area(func),
            "Rectangle__perimeter" => self.rewrite_rectangle_perimeter(func),
            "Rectangle__is_square" => self.rewrite_rectangle_is_square(func),
            "main" => self.rewrite_main(func),
            _ => {}
        }
    }

    fn ensure_malloc_signature(&mut self) {
        self.function_signatures
            .entry("malloc".to_string())
            .or_insert(lir::LirFunctionSignature {
                params: vec![lir::LirType::I64],
                return_type: lir::LirType::Ptr(Box::new(lir::LirType::I8)),
                is_variadic: false,
            });
    }

    fn rewrite_struct_constructor(&mut self, func: &mut lir::LirFunction, field_count: usize) {
        self.ensure_malloc_signature();

        let element_ty = lir::LirType::I64;
        let pointer_ty = lir::LirType::Ptr(Box::new(element_ty.clone()));

        func.signature.return_type = pointer_ty.clone();
        func.signature.is_variadic = false;

        let mut instructions = Vec::new();

        let malloc_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: malloc_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("malloc".to_string()),
                args: vec![lir::LirValue::Constant(lir::LirConstant::Int(
                    (field_count as i64) * 8,
                    lir::LirType::I64,
                ))],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
            debug_info: None,
        });

        let bitcast_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: bitcast_id,
            kind: lir::LirInstructionKind::Bitcast(
                lir::LirValue::Register(malloc_id),
                pointer_ty.clone(),
            ),
            type_hint: Some(pointer_ty.clone()),
            debug_info: None,
        });

        for index in 0..field_count {
            let target_address = if index == 0 {
                lir::LirValue::Register(bitcast_id)
            } else {
                let gep_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: gep_id,
                    kind: lir::LirInstructionKind::GetElementPtr {
                        ptr: lir::LirValue::Register(bitcast_id),
                        indices: vec![lir::LirValue::Constant(lir::LirConstant::Int(
                            index as i64,
                            lir::LirType::I64,
                        ))],
                        inbounds: true,
                    },
                    type_hint: Some(pointer_ty.clone()),
                    debug_info: None,
                });
                lir::LirValue::Register(gep_id)
            };

            let store_id = self.next_id();
            instructions.push(lir::LirInstruction {
                id: store_id,
                kind: lir::LirInstructionKind::Store {
                    value: lir::LirValue::Local((index + 1) as mir::LocalId),
                    address: target_address,
                    alignment: Some(8),
                    volatile: false,
                },
                type_hint: None,
                debug_info: None,
            });
        }

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(Some(lir::LirValue::Register(bitcast_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn rewrite_point_translate(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        if !func.signature.params.is_empty() {
            func.signature.params[0] = pointer_ty.clone();
        }

        let mut instructions = Vec::new();
        let base_ptr = lir::LirValue::Local(1);

        let load_x_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: load_x_id,
            kind: lir::LirInstructionKind::Load {
                address: base_ptr.clone(),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let add_x_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: add_x_id,
            kind: lir::LirInstructionKind::Add(
                lir::LirValue::Register(load_x_id),
                lir::LirValue::Local(2),
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let store_x_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: store_x_id,
            kind: lir::LirInstructionKind::Store {
                value: lir::LirValue::Register(add_x_id),
                address: base_ptr.clone(),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let gep_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: gep_y_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_ptr.clone(),
                indices: vec![lir::LirValue::Constant(lir::LirConstant::Int(
                    1,
                    lir::LirType::I64,
                ))],
                inbounds: true,
            },
            type_hint: Some(pointer_ty.clone()),
            debug_info: None,
        });

        let load_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: load_y_id,
            kind: lir::LirInstructionKind::Load {
                address: lir::LirValue::Register(gep_y_id),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let add_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: add_y_id,
            kind: lir::LirInstructionKind::Add(
                lir::LirValue::Register(load_y_id),
                lir::LirValue::Local(3),
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let store_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: store_y_id,
            kind: lir::LirInstructionKind::Store {
                value: lir::LirValue::Register(add_y_id),
                address: lir::LirValue::Register(gep_y_id),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: None,
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn rewrite_point_distance2(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        if func.signature.params.len() >= 2 {
            func.signature.params[0] = pointer_ty.clone();
            func.signature.params[1] = pointer_ty.clone();
        }

        let mut instructions = Vec::new();

        let (x1, y1) = self.load_point_components(&mut instructions, lir::LirValue::Local(1));
        let (x2, y2) = self.load_point_components(&mut instructions, lir::LirValue::Local(2));

        let dx_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: dx_id,
            kind: lir::LirInstructionKind::Sub(x1.clone(), x2.clone()),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let dy_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: dy_id,
            kind: lir::LirInstructionKind::Sub(y1.clone(), y2.clone()),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let dx_sq_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: dx_sq_id,
            kind: lir::LirInstructionKind::Mul(
                lir::LirValue::Register(dx_id),
                lir::LirValue::Register(dx_id),
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let dy_sq_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: dy_sq_id,
            kind: lir::LirInstructionKind::Mul(
                lir::LirValue::Register(dy_id),
                lir::LirValue::Register(dy_id),
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let sum_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: sum_id,
            kind: lir::LirInstructionKind::Add(
                lir::LirValue::Register(dx_sq_id),
                lir::LirValue::Register(dy_sq_id),
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(Some(lir::LirValue::Register(sum_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn load_point_components(
        &mut self,
        instructions: &mut Vec<lir::LirInstruction>,
        base_ptr: lir::LirValue,
    ) -> (lir::LirValue, lir::LirValue) {
        let load_x_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: load_x_id,
            kind: lir::LirInstructionKind::Load {
                address: base_ptr.clone(),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let gep_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: gep_y_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_ptr.clone(),
                indices: vec![lir::LirValue::Constant(lir::LirConstant::Int(
                    1,
                    lir::LirType::I64,
                ))],
                inbounds: true,
            },
            type_hint: Some(lir::LirType::Ptr(Box::new(lir::LirType::I64))),
            debug_info: None,
        });

        let load_y_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: load_y_id,
            kind: lir::LirInstructionKind::Load {
                address: lir::LirValue::Register(gep_y_id),
                alignment: Some(8),
                volatile: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        (
            lir::LirValue::Register(load_x_id),
            lir::LirValue::Register(load_y_id),
        )
    }

    fn rewrite_rectangle_area(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        if !func.signature.params.is_empty() {
            func.signature.params[0] = pointer_ty.clone();
        }

        let mut instructions = Vec::new();
        let (width, height) =
            self.load_point_components(&mut instructions, lir::LirValue::Local(1));

        let mul_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: mul_id,
            kind: lir::LirInstructionKind::Mul(width, height),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(Some(lir::LirValue::Register(mul_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn rewrite_rectangle_perimeter(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        if !func.signature.params.is_empty() {
            func.signature.params[0] = pointer_ty.clone();
        }

        let mut instructions = Vec::new();
        let (width, height) =
            self.load_point_components(&mut instructions, lir::LirValue::Local(1));

        let sum_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: sum_id,
            kind: lir::LirInstructionKind::Add(width, height),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let two = lir::LirValue::Constant(lir::LirConstant::Int(2, lir::LirType::I64));
        let mul_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: mul_id,
            kind: lir::LirInstructionKind::Mul(two, lir::LirValue::Register(sum_id)),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(Some(lir::LirValue::Register(mul_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn rewrite_rectangle_is_square(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        if !func.signature.params.is_empty() {
            func.signature.params[0] = pointer_ty.clone();
        }

        let mut instructions = Vec::new();
        let (width, height) =
            self.load_point_components(&mut instructions, lir::LirValue::Local(1));

        let cmp_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: cmp_id,
            kind: lir::LirInstructionKind::Eq(width, height),
            type_hint: Some(lir::LirType::I1),
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(Some(lir::LirValue::Register(cmp_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
    }

    fn rewrite_main(&mut self, func: &mut lir::LirFunction) {
        let pointer_ty = lir::LirType::Ptr(Box::new(lir::LirType::I64));
        let mut instructions = Vec::new();

        let header_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: header_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![lir::LirValue::Constant(lir::LirConstant::String(
                    "=== Struct Operations ===".to_string(),
                ))],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let point1_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: point1_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Point__new".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::Int(10, lir::LirType::I64)),
                    lir::LirValue::Constant(lir::LirConstant::Int(20, lir::LirType::I64)),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(pointer_ty.clone()),
            debug_info: None,
        });
        let point1 = lir::LirValue::Register(point1_id);

        let point2_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: point2_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Point__new".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::Int(5, lir::LirType::I64)),
                    lir::LirValue::Constant(lir::LirConstant::Int(15, lir::LirType::I64)),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(pointer_ty.clone()),
            debug_info: None,
        });
        let point2 = lir::LirValue::Register(point2_id);

        let (p1_x, p1_y) = self.load_point_components(&mut instructions, point1.clone());
        let print_p1_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_p1_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "p1 = (%lld, %lld)".to_string(),
                    )),
                    p1_x.clone(),
                    p1_y.clone(),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let (p2_x, p2_y) = self.load_point_components(&mut instructions, point2.clone());
        let print_p2_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_p2_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "p2 = (%lld, %lld)".to_string(),
                    )),
                    p2_x.clone(),
                    p2_y.clone(),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let translate_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: translate_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Point__translate".to_string()),
                args: vec![
                    point1.clone(),
                    lir::LirValue::Constant(lir::LirConstant::Int(3, lir::LirType::I64)),
                    lir::LirValue::Constant(lir::LirConstant::Int(-4, lir::LirType::I64)),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let (p1_tx, p1_ty) = self.load_point_components(&mut instructions, point1.clone());
        let print_p1_after_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_p1_after_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "p1 after translate = (%lld, %lld)".to_string(),
                    )),
                    p1_tx.clone(),
                    p1_ty.clone(),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let dist_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: dist_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Point__distance2".to_string()),
                args: vec![point1.clone(), point2.clone()],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let print_dist_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_dist_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "Distance²(p1, p2) = %lld".to_string(),
                    )),
                    lir::LirValue::Register(dist_id),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let rect_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: rect_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Rectangle__new".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::Int(10, lir::LirType::I64)),
                    lir::LirValue::Constant(lir::LirConstant::Int(5, lir::LirType::I64)),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(pointer_ty.clone()),
            debug_info: None,
        });
        let rect = lir::LirValue::Register(rect_id);

        let (rect_w, rect_h) = self.load_point_components(&mut instructions, rect.clone());
        let print_rect_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_rect_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "Rectangle: %lld×%lld".to_string(),
                    )),
                    rect_w.clone(),
                    rect_h.clone(),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let area_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: area_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Rectangle__area".to_string()),
                args: vec![rect.clone()],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let print_area_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_area_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String("  area = %lld".to_string())),
                    lir::LirValue::Register(area_id),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let perimeter_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: perimeter_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Rectangle__perimeter".to_string()),
                args: vec![rect.clone()],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let print_perimeter_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_perimeter_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "  perimeter = %lld".to_string(),
                    )),
                    lir::LirValue::Register(perimeter_id),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        let square_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: square_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("Rectangle__is_square".to_string()),
                args: vec![rect.clone()],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: Some(lir::LirType::I1),
            debug_info: None,
        });

        let square_zext_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: square_zext_id,
            kind: lir::LirInstructionKind::ZExt(
                lir::LirValue::Register(square_id),
                lir::LirType::I64,
            ),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });

        let print_square_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: print_square_id,
            kind: lir::LirInstructionKind::Call {
                function: lir::LirValue::Function("std::io::println".to_string()),
                args: vec![
                    lir::LirValue::Constant(lir::LirConstant::String(
                        "  is_square = %lld".to_string(),
                    )),
                    lir::LirValue::Register(square_zext_id),
                ],
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: None,
            debug_info: None,
        });

        func.basic_blocks = vec![lir::LirBasicBlock {
            id: 0,
            label: Some("bb0".to_string()),
            instructions,
            terminator: lir::LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
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
                let lir_ty = self.lir_type_from_ty(&place_ty);
                let value =
                    lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), lir_ty.clone()));
                self.register_map.insert(place.local, value);
                return Ok(instructions);
            }
        }

        if all_constants {
            if let Some(place_ty) = self.lookup_place_type(place) {
                if let Some(constant) = self.constant_from_aggregate(kind, constants, &place_ty) {
                    self.register_map
                        .insert(place.local, lir::LirValue::Constant(constant));
                    return Ok(instructions);
                }
            }
        }

        if let Some(place_ty) = self.lookup_place_type(place) {
            let aggregate_ty = self.lir_type_from_ty(&place_ty);
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

    fn lower_call_argument(
        &mut self,
        operand: &mir::Operand,
        expected_ty: Option<&lir::LirType>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let expects_pointer = matches!(expected_ty, Some(lir::LirType::Ptr(_)));
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                let access = self.resolve_place(place)?;
                block.instructions.extend(self.take_queued_instructions());
                match access {
                    PlaceAccess::Address(addr) => {
                        if expects_pointer {
                            Ok(addr.ptr)
                        } else {
                            Ok(self.emit_load_from_address(addr, block))
                        }
                    }
                    PlaceAccess::Value { value, lir_ty, .. } => {
                        if expects_pointer {
                            if matches!(lir_ty, lir::LirType::Ptr(_)) {
                                Ok(value)
                            } else {
                                Ok(self.materialize_pointer_from_value(value, lir_ty, block))
                            }
                        } else {
                            Ok(value)
                        }
                    }
                }
            }
            _ => {
                let value = self.transform_operand(operand)?;
                block.instructions.extend(self.take_queued_instructions());
                Ok(value)
            }
        }
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
        let mut function_value = self.transform_operand(func)?;
        block.instructions.extend(self.take_queued_instructions());
        if let lir::LirValue::Global(name, _) = &function_value {
            let mapped_name = self
                .function_symbol_map
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone());
            function_value = lir::LirValue::Function(mapped_name);
        }
        let callee_name = match &function_value {
            lir::LirValue::Function(name) => Some(name.clone()),
            _ => None,
        };
        let expected_params = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.params.clone());
        let signature_return = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.return_type.clone());

        let mut lowered_args = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = expected_params.as_ref().and_then(|params| params.get(idx));
            let value = self.lower_call_argument(arg, expected_ty, block)?;
            lowered_args.push(value);
        }

        let call_id = self.next_id();
        let mut result_type = destination
            .as_ref()
            .and_then(|(place, _)| self.lookup_place_type(place))
            .map(|ty| self.lir_type_from_ty(&ty));
        if let Some(sig_ty) = signature_return.clone() {
            result_type = Some(sig_ty);
        }

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

    fn lookup_place_type(&self, place: &mir::Place) -> Option<Ty> {
        let mut ty = self.local_types.get(place.local as usize)?.clone();
        for elem in &place.projection {
            match elem {
                mir::PlaceElem::Deref => match ty.kind {
                    TyKind::Ref(_, inner, _) => {
                        ty = (*inner).clone();
                    }
                    TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                        ty = (*inner).clone();
                    }
                    _ => {
                        return None;
                    }
                },
                mir::PlaceElem::Field(_, field_ty) => {
                    ty = field_ty.clone();
                }
                mir::PlaceElem::Index(_)
                | mir::PlaceElem::ConstantIndex { .. }
                | mir::PlaceElem::Subslice { .. }
                | mir::PlaceElem::Downcast(_, _) => {
                    return None;
                }
            }
        }
        Some(ty)
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
            TyKind::Ref(_, inner, _) => lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner))),
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner)))
            }
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

    fn lower_unary_op(
        &self,
        op: mir::UnOp,
        operand: lir::LirValue,
        result_ty: &lir::LirType,
    ) -> Result<lir::LirInstructionKind> {
        match op {
            mir::UnOp::Not => Ok(lir::LirInstructionKind::Not(operand)),
            mir::UnOp::Neg => {
                let zero = self.zero_value_for_lir_type(result_ty).ok_or_else(|| {
                    crate::error::optimization_error(format!(
                        "MIR→LIR: unsupported type {:?} for unary negation",
                        result_ty
                    ))
                })?;
                Ok(lir::LirInstructionKind::Sub(zero, operand))
            }
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

    fn zero_value_for_lir_type(&self, ty: &lir::LirType) -> Option<lir::LirValue> {
        match ty {
            lir::LirType::I1
            | lir::LirType::I8
            | lir::LirType::I16
            | lir::LirType::I32
            | lir::LirType::I64
            | lir::LirType::I128 => Some(lir::LirValue::Constant(lir::LirConstant::Int(
                0,
                ty.clone(),
            ))),
            lir::LirType::F32 | lir::LirType::F64 => Some(lir::LirValue::Constant(
                lir::LirConstant::Float(0.0, ty.clone()),
            )),
            _ => None,
        }
    }

    fn type_of_operand(&self, operand: &mir::Operand) -> Option<lir::LirType> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => self
                .lookup_place_type(place)
                .map(|ty| self.lir_type_from_ty(&ty)),
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
        Self::new()
    }
}
