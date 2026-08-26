//! Per-function lowering state.
//!
//! [`FunctionLowering`] maintains the simulated operand stack, assigns
//! virtual registers, and dispatches each bytecode instruction to the
//! appropriate sub-lowering in [`ops`][super::ops].

use fp_bytecode::{BytecodeCallee, BytecodeInstr, BytecodeTerminator};
use fp_core::lir::{
    BasicBlockId, CallingConvention, LirBasicBlock, LirFunction, LirFunctionRef, LirInstruction,
    LirInstructionKind, LirTerminator, LirType, LirValue, LirValueKind, RegisterId,
};
use std::collections::HashMap;

use super::LowerError;
use super::LowerResult;

/// State kept during the lowering of a single bytecode function.
pub(crate) struct FunctionLowering<'a> {
    /// Reference to the full bytecode program (for const pool lookups).
    pub bytecode: &'a fp_bytecode::BytecodeProgram,
    /// The LIR function being built.
    pub func: &'a mut LirFunction,
    /// Monotonic counter for allocating fresh [`RegisterId`]s.
    pub next_reg: RegisterId,
    /// Simulated operand stack.  Each entry is the register holding the
    /// value at that stack position.
    pub stack: Vec<RegisterId>,
    /// Types assigned by the bytecode lowering, used for every later
    /// register operand construction.
    pub register_types: HashMap<RegisterId, LirType>,
    pub local_types: Vec<LirType>,
    entry_block_id: BasicBlockId,
    predecessors: HashMap<BasicBlockId, Vec<BasicBlockId>>,
    exit_stacks: HashMap<BasicBlockId, Vec<RegisterId>>,
    pending_phis: Vec<(BasicBlockId, RegisterId, BasicBlockId, usize)>,
}

impl<'a> FunctionLowering<'a> {
    pub fn new(
        bytecode: &'a fp_bytecode::BytecodeProgram,
        func: &'a mut LirFunction,
        entry_block_id: BasicBlockId,
        local_types: Vec<LirType>,
        predecessors: HashMap<BasicBlockId, Vec<BasicBlockId>>,
    ) -> Self {
        Self {
            bytecode,
            func,
            next_reg: 10,
            stack: Vec::new(),
            register_types: HashMap::new(),
            local_types,
            entry_block_id,
            predecessors,
            exit_stacks: HashMap::new(),
            pending_phis: Vec::new(),
        }
    }

    // ---------------------------------------------------------------
    // Register management
    // ---------------------------------------------------------------

    /// Allocate and return the next available virtual register.
    pub fn alloc_reg(&mut self, ty: LirType) -> RegisterId {
        let reg = self.next_reg;
        self.next_reg += 1;
        self.register_types.insert(reg, ty);
        reg
    }

    /// Push a register onto the simulated operand stack.
    pub fn push_reg(&mut self, reg: RegisterId) {
        self.stack.push(reg);
    }

    /// Pop a register from the simulated operand stack.
    pub fn pop_reg(&mut self) -> LowerResult<RegisterId> {
        self.stack
            .pop()
            .ok_or_else(|| LowerError::Internal("stack underflow during lowering".into()))
    }

    /// Convenience: wrap a register in a typed LIR value.
    pub fn reg_val(&self, reg: RegisterId) -> LowerResult<LirValue> {
        let ty =
            self.register_types.get(&reg).cloned().ok_or_else(|| {
                LowerError::Internal(format!("register %{reg} has no lowered type"))
            })?;
        Ok(LirValue::register(reg, ty))
    }

    pub fn local_type(&self, local: u32) -> LowerResult<LirType> {
        self.local_types
            .get(local as usize)
            .cloned()
            .ok_or_else(|| LowerError::Internal(format!("local {local} is out of bounds")))
    }

    pub fn set_local_type(&mut self, local: u32, ty: LirType) -> LowerResult<()> {
        let slot = self
            .local_types
            .get_mut(local as usize)
            .ok_or_else(|| LowerError::Internal(format!("local {local} is out of bounds")))?;
        *slot = ty.clone();
        if let Some(local_info) = self.func.locals.iter_mut().find(|entry| entry.id == local) {
            local_info.ty = ty;
        }
        Ok(())
    }

    // ---------------------------------------------------------------
    // Block management
    // ---------------------------------------------------------------

    /// Ensure a basic block with the given ID exists, creating it if
    /// necessary.
    pub fn ensure_block(&mut self, id: BasicBlockId) {
        if self.func.get_basic_block(id).is_none() {
            self.func.add_basic_block(LirBasicBlock::new(id, None));
        }
    }

    /// Get mutable access to the given block, creating it on demand.
    pub fn current_block_mut(&mut self, id: BasicBlockId) -> &mut LirBasicBlock {
        self.ensure_block(id);
        self.func.get_basic_block_mut(id).unwrap()
    }

    // ---------------------------------------------------------------
    // Instruction emission
    // ---------------------------------------------------------------

    /// Emit a single instruction into `block_id`, returning the
    /// register that holds its result.
    pub fn emit_in_block(
        &mut self,
        block_id: BasicBlockId,
        kind: LirInstructionKind,
    ) -> LowerResult<RegisterId> {
        let result_type = Self::result_type(&kind).ok_or_else(|| {
            LowerError::Internal(format!("instruction {:?} has no result type", kind))
        })?;
        let reg = self.alloc_reg(result_type.clone());
        let instr = LirInstruction::new(reg, kind).with_result(result_type);
        let block = self.current_block_mut(block_id);
        block.add_instruction(instr);
        Ok(reg)
    }

    pub fn emit_void_in_block(
        &mut self,
        block_id: BasicBlockId,
        kind: LirInstructionKind,
    ) -> LowerResult<()> {
        if Self::result_type(&kind).is_some() {
            return Err(LowerError::Internal(format!(
                "instruction {:?} unexpectedly produces a result",
                kind
            )));
        }
        let reg = self.next_reg;
        self.next_reg += 1;
        self.current_block_mut(block_id)
            .add_instruction(LirInstruction::new(reg, kind));
        Ok(())
    }

    pub fn emit_typed_in_block(
        &mut self,
        block_id: BasicBlockId,
        kind: LirInstructionKind,
        result_type: LirType,
    ) -> LowerResult<RegisterId> {
        let reg = self.alloc_reg(result_type.clone());
        let instr = LirInstruction::new(reg, kind).with_result(result_type);
        self.current_block_mut(block_id).add_instruction(instr);
        Ok(reg)
    }

    fn result_type(kind: &LirInstructionKind) -> Option<LirType> {
        use LirInstructionKind::*;
        match kind {
            Store { .. } | Unreachable => None,
            Eq(..) | Ne(..) | Lt(..) | Le(..) | Gt(..) | Ge(..) => Some(LirType::I1),
            Load { address, .. } => Some(address.ty.clone()),
            Add(a, _)
            | Sub(a, _)
            | Mul(a, _)
            | Div(a, _)
            | Rem(a, _)
            | And(a, _)
            | Or(a, _)
            | Xor(a, _)
            | Shl(a, _)
            | Shr(a, _)
            | Not(a) => Some(a.ty.clone()),
            PtrToInt(_) => Some(LirType::I64),
            IntToPtr(_) => Some(LirType::Ptr(Box::new(LirType::I8))),
            Trunc(_, ty)
            | ZExt(_, ty)
            | SExt(_, ty)
            | FPTrunc(_, ty)
            | FPExt(_, ty)
            | FPToUI(_, ty)
            | FPToSI(_, ty)
            | UIToFP(_, ty)
            | SIToFP(_, ty)
            | Bitcast(_, ty)
            | SextOrTrunc(_, ty) => Some(ty.clone()),
            ExtractValue { aggregate, .. } => Some(aggregate.ty.clone()),
            InsertValue { aggregate, .. } => Some(aggregate.ty.clone()),
            Call { function, .. }
                if matches!(
                    &function.kind,
                    LirValueKind::Function(LirFunctionRef::Name(name))
                        if name.as_str() == "__bc_print" || name.as_str() == "__bc_println"
                ) =>
            {
                None
            }
            Call { .. } | IntrinsicCall { .. } | ExecQuery(_) | ComptimeOp(_) => Some(LirType::I64),
            Alloca { .. } | GetElementPtr { .. } => Some(LirType::Ptr(Box::new(LirType::I8))),
            Phi { incoming } => incoming.first().map(|(value, _)| value.ty.clone()),
            Select { if_true, .. } => Some(if_true.ty.clone()),
            InlineAsm { output_type, .. }
            | LandingPad {
                result_type: output_type,
                ..
            } => Some(output_type.clone()),
            Freeze(value) => Some(value.ty.clone()),
        }
    }

    // ---------------------------------------------------------------
    // Block lowering (main dispatch)
    // ---------------------------------------------------------------

    /// Lower a single bytecode basic block into LIR instructions and a
    /// terminator.
    ///
    /// Restore the block's incoming stack, synthesizing typed phis at joins
    /// and deferring unresolved backedge incoming values until their
    /// predecessor is lowered.
    pub fn lower_block(&mut self, block: &fp_bytecode::BytecodeBlock) -> LowerResult<()> {
        let block_id = block.id;
        self.stack = self.entry_stack(block_id)?;

        for instr in &block.code {
            match instr {
                BytecodeInstr::LoadConst(id) => {
                    let bc_const = self
                        .bytecode
                        .const_pool
                        .get(*id as usize)
                        .ok_or_else(|| LowerError::Internal(format!("missing const {id}")))?;
                    let reg = super::ops::lower_load_const(self, block_id, bc_const)?;
                    self.push_reg(reg);
                }
                BytecodeInstr::LoadLocal(local) => {
                    let local_type = self.local_type(*local)?;
                    let val_reg = self.emit_in_block(
                        block_id,
                        LirInstructionKind::Load {
                            address: LirValue::local(*local, local_type),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?;
                    self.push_reg(val_reg);
                }
                BytecodeInstr::StoreLocal(local) => {
                    let val_reg = self.pop_reg()?;
                    let value_type =
                        self.register_types.get(&val_reg).cloned().ok_or_else(|| {
                            LowerError::Internal(format!("register %{val_reg} has no lowered type"))
                        })?;
                    self.set_local_type(*local, value_type.clone())?;
                    self.emit_void_in_block(
                        block_id,
                        LirInstructionKind::Store {
                            value: self.reg_val(val_reg)?,
                            address: LirValue::local(*local, value_type),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?;
                }
                BytecodeInstr::LoadPlace(place) => {
                    let val_reg = super::ops::lower_load_place(self, block_id, place)?;
                    self.push_reg(val_reg);
                }
                BytecodeInstr::StorePlace(place) => {
                    let val_reg = self.pop_reg()?;
                    super::ops::lower_store_place(self, block_id, place, val_reg)?;
                }
                BytecodeInstr::BinaryOp(op) => {
                    let right = self.pop_reg()?;
                    let left = self.pop_reg()?;
                    let result_reg = super::ops::lower_binop(self, block_id, op, left, right)?;
                    self.push_reg(result_reg);
                }
                BytecodeInstr::UnaryOp(op) => {
                    let operand = self.pop_reg()?;
                    let result_reg = super::ops::lower_unop(self, block_id, op, operand)?;
                    self.push_reg(result_reg);
                }
                BytecodeInstr::IntrinsicCall {
                    kind,
                    arg_count,
                    format,
                    result_type,
                } => {
                    let mut args = Vec::with_capacity(*arg_count as usize);
                    for _ in 0..*arg_count {
                        let arg_reg = self.pop_reg()?;
                        args.push(self.reg_val(arg_reg)?);
                    }
                    args.reverse();
                    let result_reg = super::ops::lower_intrinsic(
                        self,
                        block_id,
                        *kind,
                        format.as_deref(),
                        args,
                        result_type.clone(),
                    )?;
                    if let Some(reg) = result_reg {
                        self.push_reg(reg);
                    }
                }
                BytecodeInstr::MakeTuple(count) => {
                    let reg = super::ops::lower_make_compound(
                        self,
                        block_id,
                        super::constants::INTRINSIC_MAKE_TUPLE,
                        *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeArray(count) => {
                    let reg = super::ops::lower_make_compound(
                        self,
                        block_id,
                        super::constants::INTRINSIC_MAKE_ARRAY,
                        *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeList(count) => {
                    let reg = super::ops::lower_make_compound(
                        self,
                        block_id,
                        super::constants::INTRINSIC_MAKE_LIST,
                        *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeMap(count) => {
                    let reg = super::ops::lower_make_compound(
                        self,
                        block_id,
                        super::constants::INTRINSIC_MAKE_MAP,
                        *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::ContainerLen => {
                    let container = self.pop_reg()?;
                    let reg = super::ops::lower_call_intrinsic(
                        self,
                        block_id,
                        super::constants::INTRINSIC_CONTAINER_LEN,
                        &[self.reg_val(container)?],
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::ContainerGet => {
                    let key = self.pop_reg()?;
                    let container = self.pop_reg()?;
                    let reg = super::ops::lower_container_get(self, block_id, container, key)?;
                    self.push_reg(reg);
                }
                BytecodeInstr::Pop => {
                    let _ = self.pop_reg()?;
                }
            }
        }

        // -- Terminator lowering --
        match &block.terminator {
            BytecodeTerminator::Return => {
                // Bytecode VM reads locals[0] as the return value.
                let val_reg = self.emit_in_block(
                    block_id,
                    LirInstructionKind::Load {
                        address: LirValue::local(0, self.local_type(0)?),
                        alignment: Some(8),
                        volatile: false,
                    },
                )?;
                let return_value = self.reg_val(val_reg)?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Return(Some(return_value)));
            }
            BytecodeTerminator::Jump { target } => {
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Br(*target));
            }
            BytecodeTerminator::JumpIfTrue { target, otherwise } => {
                let cond = self.pop_reg()?;
                let condition = self.reg_val(cond)?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::CondBr {
                    condition,
                    if_true: *target,
                    if_false: *otherwise,
                });
            }
            BytecodeTerminator::JumpIfFalse { target, otherwise } => {
                let cond = self.pop_reg()?;
                let condition = self.reg_val(cond)?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::CondBr {
                    condition,
                    if_true: *otherwise,
                    if_false: *target,
                });
            }
            BytecodeTerminator::SwitchInt {
                values,
                targets,
                otherwise,
            } => {
                let discr = self.pop_reg()?;
                let cases: Vec<(u64, BasicBlockId)> = values
                    .iter()
                    .zip(targets.iter())
                    .map(|(v, t)| (*v as u64, *t))
                    .collect();
                let value = self.reg_val(discr)?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Switch {
                    value,
                    default: *otherwise,
                    cases,
                });
            }
            BytecodeTerminator::Call {
                callee,
                arg_count,
                destination,
                target,
                result_type,
            } => {
                let mut args = Vec::with_capacity(*arg_count as usize);
                for _ in 0..*arg_count {
                    let arg_reg = self.pop_reg()?;
                    args.push(self.reg_val(arg_reg)?);
                }
                args.reverse();

                let callee_val = match callee {
                    BytecodeCallee::Function(name) => LirValue::function(
                        fp_core::lir::LirFunctionRef::Name(fp_core::lir::Name::new(name)),
                        LirType::Ptr(Box::new(LirType::I8)),
                    ),
                    BytecodeCallee::Local(place) => {
                        let reg = super::ops::lower_load_place(self, block_id, place)?;
                        self.reg_val(reg)?
                    }
                };

                let void_call = matches!(
                    callee,
                    BytecodeCallee::Function(name)
                        if name == "__bc_print" || name == "__bc_println"
                );
                if void_call {
                    self.emit_void_in_block(
                        block_id,
                        LirInstructionKind::Call {
                            function: callee_val,
                            args,
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                    )?;
                } else {
                    let result_reg = self.emit_typed_in_block(
                        block_id,
                        LirInstructionKind::Call {
                            function: callee_val,
                            args,
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        result_type.clone(),
                    )?;

                    if let Some(place) = destination {
                        super::ops::lower_store_place(self, block_id, place, result_reg)?;
                    }
                }

                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Br(*target));
            }
            BytecodeTerminator::Abort | BytecodeTerminator::Unreachable => {
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Unreachable);
            }
        }

        let exit_stack = std::mem::take(&mut self.stack);
        self.exit_stacks.insert(block_id, exit_stack.clone());
        self.patch_pending_phis(block_id, &exit_stack);

        Ok(())
    }

    fn entry_stack(&mut self, block_id: BasicBlockId) -> LowerResult<Vec<RegisterId>> {
        if block_id == self.entry_block_id {
            return Ok(Vec::new());
        }
        let predecessors = self
            .predecessors
            .get(&block_id)
            .cloned()
            .unwrap_or_default();
        if predecessors.is_empty() {
            return Ok(Vec::new());
        }
        let mut incoming = Vec::new();
        for predecessor in predecessors {
            incoming.push((predecessor, self.exit_stacks.get(&predecessor).cloned()));
        }
        let Some((_, Some(first_stack))) = incoming.iter().find(|(_, stack)| stack.is_some()) else {
            return Err(LowerError::Unsupported(format!(
                "block {block_id} has no resolved operand-stack predecessor"
            )));
        };
        let height = first_stack.len();
        if incoming
            .iter()
            .filter_map(|(_, stack)| stack.as_ref())
            .any(|stack| stack.len() != height)
        {
            return Err(LowerError::Unsupported(format!(
                "block {block_id} has inconsistent incoming operand-stack heights"
            )));
        }
        let mut result = Vec::with_capacity(height);
        for position in 0..height {
            let first = first_stack[position];
            let first_value = self.reg_val(first)?;
            let mut phi_incoming = Vec::with_capacity(incoming.len());
            for (predecessor, stack) in &incoming {
                let value = if let Some(stack) = stack {
                    let value = self.reg_val(stack[position])?;
                    if value.ty != first_value.ty {
                        return Err(LowerError::Unsupported(format!(
                            "block {block_id} has incompatible operand-stack types at position {position}"
                        )));
                    }
                    value
                } else {
                    let placeholder = self.alloc_reg(first_value.ty.clone());
                    self.pending_phis
                        .push((block_id, placeholder, *predecessor, position));
                    self.reg_val(placeholder)?
                };
                phi_incoming.push((value, *predecessor));
            }
            let reg = self.emit_typed_in_block(
                block_id,
                LirInstructionKind::Phi {
                    incoming: phi_incoming,
                },
                first_value.ty,
            )?;
            result.push(reg);
        }
        Ok(result)
    }

    fn patch_pending_phis(&mut self, predecessor: BasicBlockId, stack: &[RegisterId]) {
        let pending = std::mem::take(&mut self.pending_phis);
        let mut remaining = Vec::new();
        for (block_id, placeholder, expected_predecessor, position) in pending {
            if expected_predecessor != predecessor {
                remaining.push((block_id, placeholder, expected_predecessor, position));
                continue;
            }
            let Some(value_reg) = stack.get(position).copied() else {
                remaining.push((block_id, placeholder, expected_predecessor, position));
                continue;
            };
            let Ok(replacement) = self.reg_val(value_reg) else {
                remaining.push((block_id, placeholder, expected_predecessor, position));
                continue;
            };
            let Some(block) = self.func.get_basic_block_mut(block_id) else {
                remaining.push((block_id, placeholder, expected_predecessor, position));
                continue;
            };
            for instruction in &mut block.instructions {
                if let LirInstructionKind::Phi { incoming } = &mut instruction.kind {
                    for (value, incoming_block) in incoming {
                        if *incoming_block == predecessor
                            && matches!(value.kind, LirValueKind::Register(reg) if reg == placeholder)
                        {
                            *value = replacement.clone();
                        }
                    }
                }
            }
        }
        self.pending_phis = remaining;
    }
}
