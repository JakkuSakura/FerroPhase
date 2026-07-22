//! Bytecode → LIR lowering pass.
//!
//! Converts a [`BytecodeProgram`] into an [`LirProgram`] suitable for
//! execution by `fp-interpret`.
//!
//! ## Approach
//!
//! Bytecode is stack-based; LIR is register-based (SSA). This pass
//! simulates the bytecode operand stack at compile time, assigning a
//! fresh virtual register to every produced value.
//!
//! ## Compound value ABI
//!
//! Compound values (Tuple, Array, List, Map, Str) are stored on the
//! managed object heap. The LIR representation carries object handles
//! (u64 indices). The `fp-interpret` VM already maintains an
//! `objects: Vec<Value>` table — we emit intrinsic calls to populate
//! and query it.
//!
//! Scalars (Int, UInt, Float, Bool) flow through registers directly.

use fp_bytecode::{
    BytecodeBinOp, BytecodeCallee, BytecodeConst, BytecodeFunction, BytecodeInstr,
    BytecodePlace, BytecodePlaceElem, BytecodeProgram, BytecodeTerminator, BytecodeUnOp,
    IntrinsicCallKind,
};
use fp_core::lir::{
    BasicBlockId, CallingConvention, LirBasicBlock, LirConstant, LirFunction,
    LirFunctionSignature, LirInstruction, LirInstructionKind, LirLocal, LirProgram, LirTerminator,
    LirType, LirValue, Linkage, Name, RegisterId,
};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum LowerError {
    Unsupported(String),
    Internal(String),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::Unsupported(msg) => write!(f, "unsupported bytecode: {msg}"),
            LowerError::Internal(msg) => write!(f, "internal lowering error: {msg}"),
        }
    }
}

impl std::error::Error for LowerError {}

type LowerResult<T> = Result<T, LowerError>;

// ---------------------------------------------------------------------------
// Object-handle intrinsics
// ---------------------------------------------------------------------------

const INTRINSIC_MAKE_TUPLE: &str = "__bc_make_tuple";
const INTRINSIC_MAKE_ARRAY: &str = "__bc_make_array";
const INTRINSIC_MAKE_LIST: &str = "__bc_make_list";
const INTRINSIC_MAKE_MAP: &str = "__bc_make_map";
const INTRINSIC_TUPLE_GET: &str = "__bc_tuple_get";
const INTRINSIC_TUPLE_SET: &str = "__bc_tuple_set";
const INTRINSIC_ARRAY_GET: &str = "__bc_array_get";
const INTRINSIC_CONTAINER_LEN: &str = "__bc_container_len";
const INTRINSIC_STR_ALLOC: &str = "__bc_str_alloc";

// ---------------------------------------------------------------------------
// Top-level entry point
// ---------------------------------------------------------------------------

pub fn lower_program(program: &BytecodeProgram) -> LowerResult<LirProgram> {
    let mut ctx = LoweringContext::new(program);
    for func in &program.functions {
        let lir_func = ctx.lower_function(func)?;
        ctx.program.add_function(lir_func);
    }
    Ok(ctx.program)
}

// ---------------------------------------------------------------------------
// Lowering context
// ---------------------------------------------------------------------------

struct LoweringContext<'a> {
    program: LirProgram,
    bytecode: &'a BytecodeProgram,
}

impl<'a> LoweringContext<'a> {
    fn new(bytecode: &'a BytecodeProgram) -> Self {
        Self {
            program: LirProgram::new(),
            bytecode,
        }
    }

    fn lower_function(&mut self, func: &BytecodeFunction) -> LowerResult<LirFunction> {
        let entry_block_id = func.blocks.first().map(|b| b.id).unwrap_or(0);
        let sig = LirFunctionSignature {
            params: vec![LirType::I64; func.params as usize],
            return_type: LirType::I64,
            is_variadic: false,
        };

        let locals: Vec<LirLocal> = (0..func.locals)
            .map(|i| LirLocal {
                id: i,
                ty: LirType::I64,
                name: Some(format!("local_{i}")),
                is_argument: i > 0 && i <= func.params,
            })
            .collect();

        let mut lir_func = LirFunction::new(
            Name::new(func.name.clone()),
            sig,
            CallingConvention::C,
            Linkage::Internal,
        );
        lir_func.locals = locals;

        let mut fl = FunctionLowering::new(self.bytecode, &mut lir_func, entry_block_id);

        // Allocate stack slots for all locals in the entry block.
        // Local 0 is the return slot; locals 1..=params are args;
        // locals params+1.. are general-purpose.
        for i in 0..func.locals {
            fl.emit_in_entry_block(LirInstructionKind::Alloca {
                size: LirValue::Constant(LirConstant::Int(8, LirType::I64)),
                alignment: 8,
            })?;
            let slot_reg = fl.last_reg();
            fl.set_local_addr(i, slot_reg);
        }

        // Lower each block
        for block in &func.blocks {
            fl.lower_block(block)?;
        }

        // Compute predecessors and successors
        compute_cfg(&mut lir_func);

        Ok(lir_func)
    }
}

// ---------------------------------------------------------------------------
// Per-function lowering state
// ---------------------------------------------------------------------------

struct FunctionLowering<'a> {
    bytecode: &'a BytecodeProgram,
    func: &'a mut LirFunction,
    next_reg: RegisterId,
    stack: Vec<RegisterId>,
    /// Bytecode local index → LIR register that holds the alloca'd address
    local_addrs: HashMap<u32, RegisterId>,
    entry_block_id: BasicBlockId,
}

impl<'a> FunctionLowering<'a> {
    fn new(
        bytecode: &'a BytecodeProgram,
        func: &'a mut LirFunction,
        entry_block_id: BasicBlockId,
    ) -> Self {
        Self {
            bytecode,
            func,
            next_reg: 1000,
            stack: Vec::new(),
            local_addrs: HashMap::new(),
            entry_block_id,
        }
    }

    // -- register management --

    fn alloc_reg(&mut self) -> RegisterId {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }

    fn last_reg(&self) -> RegisterId {
        self.next_reg - 1
    }

    fn push_reg(&mut self, reg: RegisterId) {
        self.stack.push(reg);
    }

    fn pop_reg(&mut self) -> LowerResult<RegisterId> {
        self.stack
            .pop()
            .ok_or_else(|| LowerError::Internal("stack underflow during lowering".into()))
    }

    fn reg_val(reg: RegisterId) -> LirValue {
        LirValue::Register(reg)
    }

    // -- local address tracking --

    fn set_local_addr(&mut self, local_idx: u32, addr_reg: RegisterId) {
        self.local_addrs.insert(local_idx, addr_reg);
    }

    fn get_local_addr(&self, local_idx: u32) -> LowerResult<RegisterId> {
        self.local_addrs
            .get(&local_idx)
            .copied()
            .ok_or_else(|| LowerError::Internal(format!("no alloca for local {local_idx}")))
    }

    // -- block management --

    fn ensure_block(&mut self, id: BasicBlockId) {
        if self.func.get_basic_block(id).is_none() {
            self.func.add_basic_block(LirBasicBlock::new(id, None));
        }
    }

    fn current_block_mut(&mut self, id: BasicBlockId) -> &mut LirBasicBlock {
        self.ensure_block(id);
        self.func.get_basic_block_mut(id).unwrap()
    }

    // -- instruction emission --

    fn emit_in_block(
        &mut self,
        block_id: BasicBlockId,
        kind: LirInstructionKind,
    ) -> LowerResult<RegisterId> {
        let reg = self.alloc_reg();
        let instr = LirInstruction::new(reg, kind);
        let block = self.current_block_mut(block_id);
        block.add_instruction(instr);
        Ok(reg)
    }

    fn emit_in_entry_block(&mut self, kind: LirInstructionKind) -> LowerResult<RegisterId> {
        self.emit_in_block(self.entry_block_id, kind)
    }

    // -- block lowering --

    fn lower_block(&mut self, block: &fp_bytecode::BytecodeBlock) -> LowerResult<()> {
        let block_id = block.id;

        // Clear simulated stack at block entry.
        // For blocks with multiple predecessors, we'd need phi nodes.
        // For now, assume stack is empty at block start (caller restores it).
        self.stack.clear();

        for instr in &block.code {
            match instr {
                BytecodeInstr::LoadConst(id) => {
                    let bc_const = self.bytecode.const_pool.get(*id as usize).ok_or_else(|| {
                        LowerError::Internal(format!("missing const {id}"))
                    })?;
                    let reg = self.lower_load_const(block_id, bc_const)?;
                    self.push_reg(reg);
                }
                BytecodeInstr::LoadLocal(local) => {
                    let addr_reg = self.get_local_addr(*local)?;
                    let val_reg = self.emit_in_block(
                        block_id,
                        LirInstructionKind::Load {
                            address: Self::reg_val(addr_reg),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?;
                    self.push_reg(val_reg);
                }
                BytecodeInstr::StoreLocal(local) => {
                    let val_reg = self.pop_reg()?;
                    let addr_reg = self.get_local_addr(*local)?;
                    self.emit_in_block(
                        block_id,
                        LirInstructionKind::Store {
                            value: Self::reg_val(val_reg),
                            address: Self::reg_val(addr_reg),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?;
                }
                BytecodeInstr::LoadPlace(place) => {
                    let val_reg = self.lower_load_place(block_id, place)?;
                    self.push_reg(val_reg);
                }
                BytecodeInstr::StorePlace(place) => {
                    let val_reg = self.pop_reg()?;
                    self.lower_store_place(block_id, place, val_reg)?;
                }
                BytecodeInstr::BinaryOp(op) => {
                    let right = self.pop_reg()?;
                    let left = self.pop_reg()?;
                    let result_reg = self.lower_binop(block_id, op, left, right)?;
                    self.push_reg(result_reg);
                }
                BytecodeInstr::UnaryOp(op) => {
                    let operand = self.pop_reg()?;
                    let result_reg = self.lower_unop(block_id, op, operand)?;
                    self.push_reg(result_reg);
                }
                BytecodeInstr::IntrinsicCall {
                    kind,
                    arg_count,
                    format,
                } => {
                    let mut args = Vec::with_capacity(*arg_count as usize);
                    for _ in 0..*arg_count {
                        args.push(Self::reg_val(self.pop_reg()?));
                    }
                    args.reverse();
                    let result_reg = self.lower_intrinsic(block_id, *kind, format.as_deref(), args)?;
                    if let Some(reg) = result_reg {
                        self.push_reg(reg);
                    }
                }
                BytecodeInstr::MakeTuple(count) => {
                    let reg = self.lower_make_compound(
                        block_id, INTRINSIC_MAKE_TUPLE, *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeArray(count) => {
                    let reg = self.lower_make_compound(
                        block_id, INTRINSIC_MAKE_ARRAY, *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeList(count) => {
                    let reg = self.lower_make_compound(
                        block_id, INTRINSIC_MAKE_LIST, *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::MakeMap(count) => {
                    let reg = self.lower_make_compound(
                        block_id, INTRINSIC_MAKE_MAP, *count,
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::ContainerLen => {
                    let container = self.pop_reg()?;
                    let reg = self.lower_call_intrinsic(
                        block_id, INTRINSIC_CONTAINER_LEN,
                        &[Self::reg_val(container)],
                    )?;
                    self.push_reg(reg);
                }
                BytecodeInstr::ContainerGet => {
                    let key = self.pop_reg()?;
                    let container = self.pop_reg()?;
                    let reg = self.lower_container_get(block_id, container, key)?;
                    self.push_reg(reg);
                }
                BytecodeInstr::Pop => {
                    let _ = self.pop_reg()?;
                }
            }
        }

        // -- terminator --
        match &block.terminator {
            BytecodeTerminator::Return => {
                let ret_val = if self.stack.is_empty() {
                    None
                } else {
                    Some(Self::reg_val(self.pop_reg()?))
                };
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Return(ret_val));
            }
            BytecodeTerminator::Jump { target } => {
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Br(*target));
            }
            BytecodeTerminator::JumpIfTrue { target, otherwise } => {
                let cond = self.pop_reg()?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::CondBr {
                    condition: Self::reg_val(cond),
                    if_true: *target,
                    if_false: *otherwise,
                });
            }
            BytecodeTerminator::JumpIfFalse { target, otherwise } => {
                let cond = self.pop_reg()?;
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::CondBr {
                    condition: Self::reg_val(cond),
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
                let block = self.current_block_mut(block_id);
                let cases: Vec<(u64, BasicBlockId)> = values
                    .iter()
                    .zip(targets.iter())
                    .map(|(v, t)| (*v as u64, *t))
                    .collect();
                block.set_terminator(LirTerminator::Switch {
                    value: Self::reg_val(discr),
                    default: *otherwise,
                    cases,
                });
            }
            BytecodeTerminator::Call {
                callee,
                arg_count,
                destination,
                target,
            } => {
                let mut args = Vec::with_capacity(*arg_count as usize);
                for _ in 0..*arg_count {
                    args.push(Self::reg_val(self.pop_reg()?));
                }
                args.reverse();

                let callee_val = match callee {
                    BytecodeCallee::Function(name) => LirValue::Function(name.clone()),
                    BytecodeCallee::Local(place) => {
                        let reg = self.lower_load_place(block_id, place)?;
                        Self::reg_val(reg)
                    }
                };

                let result_reg = self.emit_in_block(
                    block_id,
                    LirInstructionKind::Call {
                        function: callee_val,
                        args,
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                )?;

                if let Some(place) = destination {
                    self.lower_store_place(block_id, place, result_reg)?;
                }

                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Br(*target));
            }
            BytecodeTerminator::Abort | BytecodeTerminator::Unreachable => {
                let block = self.current_block_mut(block_id);
                block.set_terminator(LirTerminator::Unreachable);
            }
        }

        Ok(())
    }

    // -- sub-lowerings --

    fn lower_load_const(
        &mut self,
        block_id: BasicBlockId,
        value: &BytecodeConst,
    ) -> LowerResult<RegisterId> {
        match value {
            BytecodeConst::Unit => {
                self.emit_in_block(
                    block_id,
                    LirInstructionKind::Add(
                        LirValue::Constant(LirConstant::Int(0, LirType::I64)),
                        LirValue::Constant(LirConstant::Int(0, LirType::I64)),
                    ),
                )
            }
            BytecodeConst::Bool(b) => {
                let val = if *b { 1u64 } else { 0u64 };
                self.emit_in_block(
                    block_id,
                    LirInstructionKind::Add(
                        LirValue::Constant(LirConstant::UInt(val, LirType::I1)),
                        LirValue::Constant(LirConstant::UInt(0, LirType::I1)),
                    ),
                )
            }
            BytecodeConst::Int(i) => self.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    LirValue::Constant(LirConstant::Int(*i, LirType::I64)),
                    LirValue::Constant(LirConstant::Int(0, LirType::I64)),
                ),
            ),
            BytecodeConst::UInt(u) => self.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    LirValue::Constant(LirConstant::UInt(*u, LirType::I64)),
                    LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                ),
            ),
            BytecodeConst::Float(f) => self.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    LirValue::Constant(LirConstant::Float(*f, LirType::F64)),
                    LirValue::Constant(LirConstant::Float(0.0, LirType::F64)),
                ),
            ),
            BytecodeConst::Str(s) => {
                let len = s.len() as u64;
                let len_reg = self.emit_in_block(
                    block_id,
                    LirInstructionKind::Add(
                        LirValue::Constant(LirConstant::UInt(len, LirType::I64)),
                        LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                    ),
                )?;
                self.lower_call_intrinsic(
                    block_id,
                    INTRINSIC_STR_ALLOC,
                    &[Self::reg_val(len_reg)],
                )
            }
            BytecodeConst::Function(name) => {
                let len = name.len() as u64;
                let len_reg = self.emit_in_block(
                    block_id,
                    LirInstructionKind::Add(
                        LirValue::Constant(LirConstant::UInt(len, LirType::I64)),
                        LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                    ),
                )?;
                self.lower_call_intrinsic(
                    block_id,
                    INTRINSIC_STR_ALLOC,
                    &[Self::reg_val(len_reg)],
                )
            }
            BytecodeConst::Null => self.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    LirValue::Constant(LirConstant::Null(LirType::Ptr(Box::new(LirType::I64)))),
                    LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                ),
            ),
            BytecodeConst::Tuple(items) => {
                let mut element_regs = Vec::new();
                for item in items {
                    let reg = self.lower_load_const(block_id, item)?;
                    element_regs.push(Self::reg_val(reg));
                }
                let mut args = vec![
                    LirValue::Constant(LirConstant::UInt(element_regs.len() as u64, LirType::I64)),
                ];
                args.extend(element_regs);
                self.lower_call_intrinsic(block_id, INTRINSIC_MAKE_TUPLE, &args)
            }
            BytecodeConst::Array(items) => {
                let mut element_regs = Vec::new();
                for item in items {
                    let reg = self.lower_load_const(block_id, item)?;
                    element_regs.push(Self::reg_val(reg));
                }
                let mut args = vec![
                    LirValue::Constant(LirConstant::UInt(element_regs.len() as u64, LirType::I64)),
                ];
                args.extend(element_regs);
                self.lower_call_intrinsic(block_id, INTRINSIC_MAKE_ARRAY, &args)
            }
            BytecodeConst::List(items) => {
                let mut element_regs = Vec::new();
                for item in items {
                    let reg = self.lower_load_const(block_id, item)?;
                    element_regs.push(Self::reg_val(reg));
                }
                let mut args = vec![
                    LirValue::Constant(LirConstant::UInt(element_regs.len() as u64, LirType::I64)),
                ];
                args.extend(element_regs);
                self.lower_call_intrinsic(block_id, INTRINSIC_MAKE_LIST, &args)
            }
            BytecodeConst::Map(entries) => {
                let mut arg_regs = Vec::new();
                for (key, value) in entries {
                    let k = self.lower_load_const(block_id, key)?;
                    let v = self.lower_load_const(block_id, value)?;
                    arg_regs.push(Self::reg_val(k));
                    arg_regs.push(Self::reg_val(v));
                }
                let mut args = vec![
                    LirValue::Constant(LirConstant::UInt(entries.len() as u64, LirType::I64)),
                ];
                args.extend(arg_regs);
                self.lower_call_intrinsic(block_id, INTRINSIC_MAKE_MAP, &args)
            }
        }
    }

    fn lower_binop(
        &mut self,
        block_id: BasicBlockId,
        op: &BytecodeBinOp,
        left: RegisterId,
        right: RegisterId,
    ) -> LowerResult<RegisterId> {
        let kind = match op {
            BytecodeBinOp::Add => LirInstructionKind::Add(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Sub => LirInstructionKind::Sub(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Mul => LirInstructionKind::Mul(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Div => LirInstructionKind::Div(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Rem => LirInstructionKind::Rem(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::And => LirInstructionKind::And(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Or => LirInstructionKind::Or(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::BitXor => LirInstructionKind::Xor(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::BitAnd => LirInstructionKind::And(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::BitOr => LirInstructionKind::Or(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Shl => LirInstructionKind::Shl(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Shr => LirInstructionKind::Shr(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Eq => LirInstructionKind::Eq(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Ne => LirInstructionKind::Ne(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Lt => LirInstructionKind::Lt(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Le => LirInstructionKind::Le(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Ge => LirInstructionKind::Ge(Self::reg_val(left), Self::reg_val(right)),
            BytecodeBinOp::Gt => LirInstructionKind::Gt(Self::reg_val(left), Self::reg_val(right)),
        };
        self.emit_in_block(block_id, kind)
    }

    fn lower_unop(
        &mut self,
        block_id: BasicBlockId,
        op: &BytecodeUnOp,
        operand: RegisterId,
    ) -> LowerResult<RegisterId> {
        let kind = match op {
            BytecodeUnOp::Not => LirInstructionKind::Not(Self::reg_val(operand)),
            BytecodeUnOp::Neg => {
                LirInstructionKind::Sub(
                    LirValue::Constant(LirConstant::Int(0, LirType::I64)),
                    Self::reg_val(operand),
                )
            }
        };
        self.emit_in_block(block_id, kind)
    }

    fn lower_intrinsic(
        &mut self,
        block_id: BasicBlockId,
        kind: IntrinsicCallKind,
        _format: Option<&str>,
        args: Vec<LirValue>,
    ) -> LowerResult<Option<RegisterId>> {
        match kind {
            IntrinsicCallKind::Println | IntrinsicCallKind::Print | IntrinsicCallKind::Format => {
                let reg = self.lower_call_intrinsic(
                    block_id,
                    intrinsic_to_runtime_name(kind),
                    &args,
                )?;
                Ok(Some(reg))
            }
            IntrinsicCallKind::Len => {
                let reg = self.lower_call_intrinsic(
                    block_id,
                    INTRINSIC_CONTAINER_LEN,
                    &args,
                )?;
                Ok(Some(reg))
            }
            IntrinsicCallKind::TimeNow => {
                let reg = self.lower_call_intrinsic(
                    block_id,
                    "__bc_time_now",
                    &args,
                )?;
                Ok(Some(reg))
            }
            _ => Err(LowerError::Unsupported(format!(
                "intrinsic {kind:?} not yet lowered"
            ))),
        }
    }

    fn lower_make_compound(
        &mut self,
        block_id: BasicBlockId,
        intrinsic_name: &str,
        count: u32,
    ) -> LowerResult<RegisterId> {
        let mut element_regs = Vec::with_capacity(count as usize);
        for _ in 0..count {
            element_regs.push(Self::reg_val(self.pop_reg()?));
        }
        element_regs.reverse();
        let mut args = vec![
            LirValue::Constant(LirConstant::UInt(count as u64, LirType::I64)),
        ];
        args.extend(element_regs);
        self.lower_call_intrinsic(block_id, intrinsic_name, &args)
    }

    fn lower_container_get(
        &mut self,
        block_id: BasicBlockId,
        container: RegisterId,
        key: RegisterId,
    ) -> LowerResult<RegisterId> {
        self.lower_call_intrinsic(
            block_id,
            INTRINSIC_ARRAY_GET,
            &[Self::reg_val(container), Self::reg_val(key)],
        )
    }

    fn lower_load_place(
        &mut self,
        block_id: BasicBlockId,
        place: &BytecodePlace,
    ) -> LowerResult<RegisterId> {
        let addr_reg = self.get_local_addr(place.local)?;
        let mut current_val = self.emit_in_block(
            block_id,
            LirInstructionKind::Load {
                address: Self::reg_val(addr_reg),
                alignment: Some(8),
                volatile: false,
            },
        )?;

        for elem in &place.projection {
            let index_reg = match elem {
                BytecodePlaceElem::Field(idx) => {
                    let reg = self.emit_in_block(
                        block_id,
                        LirInstructionKind::Add(
                            LirValue::Constant(LirConstant::UInt(*idx as u64, LirType::I64)),
                            LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                        ),
                    )?;
                    reg
                }
                BytecodePlaceElem::Index(local_idx) => {
                    let addr_reg = self.get_local_addr(*local_idx)?;
                    self.emit_in_block(
                        block_id,
                        LirInstructionKind::Load {
                            address: Self::reg_val(addr_reg),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?
                }
            };

            let get_intrinsic = match elem {
                BytecodePlaceElem::Field(_) => INTRINSIC_TUPLE_GET,
                BytecodePlaceElem::Index(_) => INTRINSIC_ARRAY_GET,
            };

            current_val = self.lower_call_intrinsic(
                block_id,
                get_intrinsic,
                &[Self::reg_val(current_val), Self::reg_val(index_reg)],
            )?;
        }

        Ok(current_val)
    }

    fn lower_store_place(
        &mut self,
        block_id: BasicBlockId,
        place: &BytecodePlace,
        value_reg: RegisterId,
    ) -> LowerResult<()> {
        if place.projection.is_empty() {
            let addr_reg = self.get_local_addr(place.local)?;
            self.emit_in_block(
                block_id,
                LirInstructionKind::Store {
                    value: Self::reg_val(value_reg),
                    address: Self::reg_val(addr_reg),
                    alignment: Some(8),
                    volatile: false,
                },
            )?;
            return Ok(());
        }

        // With projections: load the base, apply updates through intrinsics, store back
        let addr_reg = self.get_local_addr(place.local)?;
        let mut base_val_reg = self.emit_in_block(
            block_id,
            LirInstructionKind::Load {
                address: Self::reg_val(addr_reg),
                alignment: Some(8),
                volatile: false,
            },
        )?;

        let last = place.projection.len() - 1;
        for (i, elem) in place.projection.iter().enumerate() {
            let index_reg = match elem {
                BytecodePlaceElem::Field(idx) => {
                    let reg = self.emit_in_block(
                        block_id,
                        LirInstructionKind::Add(
                            LirValue::Constant(LirConstant::UInt(*idx as u64, LirType::I64)),
                            LirValue::Constant(LirConstant::UInt(0, LirType::I64)),
                        ),
                    )?;
                    reg
                }
                BytecodePlaceElem::Index(local_idx) => {
                    let idx_addr = self.get_local_addr(*local_idx)?;
                    self.emit_in_block(
                        block_id,
                        LirInstructionKind::Load {
                            address: Self::reg_val(idx_addr),
                            alignment: Some(8),
                            volatile: false,
                        },
                    )?
                }
            };

            if i == last {
                let set_intrinsic = match elem {
                    BytecodePlaceElem::Field(_) => INTRINSIC_TUPLE_SET,
                    BytecodePlaceElem::Index(_) => INTRINSIC_ARRAY_GET,
                };
                let new_handle = self.lower_call_intrinsic(
                    block_id,
                    set_intrinsic,
                    &[
                        Self::reg_val(base_val_reg),
                        Self::reg_val(index_reg),
                        Self::reg_val(value_reg),
                    ],
                )?;
                self.emit_in_block(
                    block_id,
                    LirInstructionKind::Store {
                        value: Self::reg_val(new_handle),
                        address: Self::reg_val(addr_reg),
                        alignment: Some(8),
                        volatile: false,
                    },
                )?;
            } else {
                let get_intrinsic = match elem {
                    BytecodePlaceElem::Field(_) => INTRINSIC_TUPLE_GET,
                    BytecodePlaceElem::Index(_) => INTRINSIC_ARRAY_GET,
                };
                base_val_reg = self.lower_call_intrinsic(
                    block_id,
                    get_intrinsic,
                    &[Self::reg_val(base_val_reg), Self::reg_val(index_reg)],
                )?;
            }
        }

        Ok(())
    }

    fn lower_call_intrinsic(
        &mut self,
        block_id: BasicBlockId,
        name: &str,
        args: &[LirValue],
    ) -> LowerResult<RegisterId> {
        self.emit_in_block(
            block_id,
            LirInstructionKind::Call {
                function: LirValue::Function(name.to_string()),
                args: args.to_vec(),
                calling_convention: CallingConvention::C,
                tail_call: false,
            },
        )
    }
}

// ---------------------------------------------------------------------------
// CFG computation
// ---------------------------------------------------------------------------

fn compute_cfg(func: &mut LirFunction) {
    let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    let mut succs: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();

    for block in &func.basic_blocks {
        let targets = match &block.terminator {
            LirTerminator::Br(dest) => vec![*dest],
            LirTerminator::CondBr { if_true, if_false, .. } => vec![*if_true, *if_false],
            LirTerminator::Switch { default, cases, .. } => {
                let mut v: Vec<BasicBlockId> = cases.iter().map(|(_, t)| *t).collect();
                v.push(*default);
                v
            }
            _ => vec![],
        };
        succs.insert(block.id, targets.clone());
        for t in &targets {
            preds.entry(*t).or_default().push(block.id);
        }
    }

    for block in &mut func.basic_blocks {
        block.predecessors = preds.remove(&block.id).unwrap_or_default();
        block.successors = succs.remove(&block.id).unwrap_or_default();
    }
}

// ---------------------------------------------------------------------------
// Intrinsic name mapping
// ---------------------------------------------------------------------------

fn intrinsic_to_runtime_name(kind: IntrinsicCallKind) -> &'static str {
    match kind {
        IntrinsicCallKind::Println => "__bc_println",
        IntrinsicCallKind::Print => "__bc_print",
        IntrinsicCallKind::Format => "__bc_format",
        IntrinsicCallKind::Len => INTRINSIC_CONTAINER_LEN,
        IntrinsicCallKind::TimeNow => "__bc_time_now",
        _ => "__bc_unknown",
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use fp_bytecode::{
        BytecodeBinOp, BytecodeBlock, BytecodeConst, BytecodeFunction, BytecodeInstr,
        BytecodeProgram, BytecodeTerminator,
    };

    #[test]
    fn lowers_simple_arithmetic() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Add),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        assert_eq!(lir.functions.len(), 1);
        let main = &lir.functions[0];
        assert_eq!(main.name.as_str(), "main");
        assert_eq!(main.basic_blocks.len(), 1);
        let block = &main.basic_blocks[0];
        assert!(!block.instructions.is_empty());
        assert!(matches!(block.terminator, LirTerminator::Return(_)));
    }

    #[test]
    fn lowers_control_flow() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![
                    BytecodeBlock {
                        id: 0,
                        code: vec![BytecodeInstr::LoadConst(0)],
                        terminator: BytecodeTerminator::JumpIfTrue {
                            target: 1,
                            otherwise: 2,
                        },
                    },
                    BytecodeBlock {
                        id: 1,
                        code: vec![],
                        terminator: BytecodeTerminator::Jump { target: 2 },
                    },
                    BytecodeBlock {
                        id: 2,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    },
                ],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let main = &lir.functions[0];
        assert_eq!(main.basic_blocks.len(), 3);

        let bb0 = main.get_basic_block(0).unwrap();
        assert!(matches!(bb0.terminator, LirTerminator::CondBr { .. }));

        let bb1 = main.get_basic_block(1).unwrap();
        assert!(matches!(bb1.terminator, LirTerminator::Br(2)));
        assert_eq!(bb1.predecessors, vec![0]);
        assert_eq!(bb1.successors, vec![2]);
    }

    #[test]
    fn lowers_function_call() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![
                BytecodeFunction {
                    name: "helper".to_string(),
                    params: 0,
                    locals: 1,
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    }],
                },
                BytecodeFunction {
                    name: "main".to_string(),
                    params: 0,
                    locals: 1,
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![],
                        terminator: BytecodeTerminator::Call {
                            callee: BytecodeCallee::Function("helper".into()),
                            arg_count: 0,
                            destination: Some(BytecodePlace {
                                local: 0,
                                projection: vec![],
                            }),
                            target: 1,
                        },
                    }],
                },
            ],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        assert_eq!(lir.functions.len(), 2);
    }
}
