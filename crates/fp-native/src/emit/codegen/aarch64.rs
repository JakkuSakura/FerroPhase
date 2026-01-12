use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirTerminator,
    LirValue,
};
use std::collections::HashMap;

use crate::emit::TargetFormat;

#[derive(Clone, Copy, Debug)]
enum Reg {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X16,
    X29,
    X30,
    X31,
}

impl Reg {
    fn id(self) -> u32 {
        match self {
            Reg::X0 => 0,
            Reg::X1 => 1,
            Reg::X2 => 2,
            Reg::X3 => 3,
            Reg::X4 => 4,
            Reg::X5 => 5,
            Reg::X6 => 6,
            Reg::X7 => 7,
            Reg::X8 => 8,
            Reg::X16 => 16,
            Reg::X29 => 29,
            Reg::X30 => 30,
            Reg::X31 => 31,
        }
    }
}

struct RegFile {
    free: Vec<Reg>,
    map: HashMap<u32, Reg>,
    slot_map: HashMap<u32, u32>,
    next_slot: u32,
}

impl RegFile {
    fn new() -> Self {
        Self {
            free: vec![
                Reg::X7, Reg::X6, Reg::X5, Reg::X4, Reg::X3, Reg::X2, Reg::X1, Reg::X0,
            ],
            map: HashMap::new(),
            slot_map: HashMap::new(),
            next_slot: 0,
        }
    }

    fn alloc(&mut self, id: u32) -> Result<Reg> {
        if let Some(reg) = self.map.get(&id) {
            return Ok(*reg);
        }
        let reg = self
            .free
            .pop()
            .ok_or_else(|| Error::from("out of registers"))?;
        self.map.insert(id, reg);
        Ok(reg)
    }

    fn get(&self, id: u32) -> Result<Reg> {
        self.map
            .get(&id)
            .copied()
            .ok_or_else(|| Error::from("register not allocated"))
    }

    fn slot_offset(&mut self, slot_id: u32) -> i32 {
        if let Some(offset) = self.slot_map.get(&slot_id) {
            return *offset as i32;
        }
        let next = self.next_slot + 1;
        self.next_slot = next;
        self.slot_map.insert(slot_id, next);
        (next as i32) * 8
    }
}

pub fn emit_text(
    func: &LirFunction,
    format: TargetFormat,
    needs_stack: bool,
) -> Result<Vec<u8>> {
    let mut regs = RegFile::new();
    let mut asm = Assembler::new();
    asm.needs_stack = needs_stack;

    if needs_stack {
        emit_prologue(&mut asm, &mut regs)?;
    }
    for block in &func.basic_blocks {
        asm.bind(block.id);
        emit_block(&mut asm, &mut regs, block, format)?;
    }

    asm.finish()
}

enum BinOp {
    Add,
    Sub,
    Mul,
}

fn emit_binop(
    asm: &mut Assembler,
    regs: &mut RegFile,
    dst: Reg,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
) -> Result<()> {
    emit_value_to_reg(asm, regs, dst, lhs)?;
    emit_op_rhs(asm, regs, dst, rhs, op)
}

fn emit_value_to_reg(
    asm: &mut Assembler,
    regs: &mut RegFile,
    dst: Reg,
    value: &LirValue,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let src = regs.get(*id)?;
            emit_mov_reg(asm, dst, src);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                return Err(Error::from("aarch64 immediate out of range"));
            }
            emit_mov_imm16(asm, dst, imm as u16);
            Ok(())
        }
        _ => Err(Error::from("unsupported LIR value for aarch64")),
    }
}

fn emit_load(
    asm: &mut Assembler,
    regs: &mut RegFile,
    dst: Reg,
    address: &LirValue,
) -> Result<()> {
    match address {
        LirValue::StackSlot(id) => {
            let offset = regs.slot_offset(*id);
            emit_load_from_sp(asm, dst, offset);
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for aarch64")),
    }
}

fn emit_store(
    asm: &mut Assembler,
    regs: &mut RegFile,
    value: &LirValue,
    address: &LirValue,
) -> Result<()> {
    let src = match value {
        LirValue::Register(id) => regs.get(*id)?,
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                return Err(Error::from("store immediate out of range"));
            }
            emit_mov_imm16(asm, Reg::X16, imm as u16);
            Reg::X16
        }
        _ => return Err(Error::from("unsupported store value for aarch64")),
    };

    match address {
        LirValue::StackSlot(id) => {
            let offset = regs.slot_offset(*id);
            emit_store_to_sp(asm, src, offset);
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for aarch64")),
    }
}

fn emit_op_rhs(asm: &mut Assembler, regs: &mut RegFile, dst: Reg, rhs: &LirValue, op: BinOp) -> Result<()> {
    match rhs {
        LirValue::Register(id) => {
            let src = regs.get(*id)?;
            match op {
                BinOp::Add => emit_add_reg(asm, dst, dst, src),
                BinOp::Sub => emit_sub_reg(asm, dst, dst, src),
                BinOp::Mul => emit_mul_reg(asm, dst, dst, src),
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm >= 0 && imm <= 4095 {
                match op {
                    BinOp::Add => emit_add_imm12(asm, dst, dst, imm as u32),
                    BinOp::Sub => emit_sub_imm12(asm, dst, dst, imm as u32),
                    BinOp::Mul => {
                        emit_mov_imm16(asm, Reg::X16, imm as u16);
                        emit_mul_reg(asm, dst, dst, Reg::X16);
                    }
                }
                Ok(())
            } else {
                emit_mov_imm16(asm, Reg::X16, imm as u16);
                match op {
                    BinOp::Add => emit_add_reg(asm, dst, dst, Reg::X16),
                    BinOp::Sub => emit_sub_reg(asm, dst, dst, Reg::X16),
                    BinOp::Mul => emit_mul_reg(asm, dst, dst, Reg::X16),
                }
                Ok(())
            }
        }
        _ => Err(Error::from("unsupported RHS for aarch64")),
    }
}

fn constant_to_i64(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        _ => Err(Error::from("unsupported constant for aarch64")),
    }
}

fn emit_mov_reg(asm: &mut Assembler, dst: Reg, src: Reg) {
    if dst.id() == src.id() {
        return;
    }
    let instr = if dst.is_sp() || src.is_sp() {
        0x9100_0000u32 | (src.id() << 5) | dst.id()
    } else {
        0xAA00_03E0u32 | (src.id() << 16) | dst.id()
    };
    asm.extend(&instr.to_le_bytes());
}

fn emit_mov_imm16(asm: &mut Assembler, dst: Reg, imm: u16) {
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8B00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_mul_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9B00_7C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0x9100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0xD100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_ret(asm: &mut Assembler) {
    asm.extend(&0xD65F_03C0u32.to_le_bytes());
}

fn emit_exit_syscall(asm: &mut Assembler, code: u16) -> Result<()> {
    emit_mov_imm16(asm, Reg::X0, code);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_reg(asm, Reg::X0, reg);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_svc(asm: &mut Assembler) {
    asm.extend(&0xD400_0001u32.to_le_bytes());
}

fn emit_sub_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0xD100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0x9100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) {
    let imm12 = ((offset / 8) as u32) & 0xfff;
    let instr = 0xF940_03E0u32 | (imm12 << 10) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_to_sp(asm: &mut Assembler, src: Reg, offset: i32) {
    let imm12 = ((offset / 8) as u32) & 0xfff;
    let instr = 0xF900_03E0u32 | (imm12 << 10) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (a.id() << 5) | 31;
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (a.id() << 5) | 31;
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_reg(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_001F | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_imm12(asm: &mut Assembler, lhs: Reg, imm12: u32) {
    let instr = 0xF100_001F | ((imm12 & 0xfff) << 10) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cset(asm: &mut Assembler, dst: Reg, cond: u32) {
    let instr = 0x9A9F_07E0u32 | ((cond & 0xF) << 12) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_block(
    asm: &mut Assembler,
    regs: &mut RegFile,
    block: &LirBasicBlock,
    format: TargetFormat,
) -> Result<()> {
    for inst in &block.instructions {
        let dst = regs.alloc(inst.id)?;
        match &inst.kind {
            LirInstructionKind::Add(lhs, rhs) => emit_binop(asm, regs, dst, lhs, rhs, BinOp::Add)?,
            LirInstructionKind::Sub(lhs, rhs) => emit_binop(asm, regs, dst, lhs, rhs, BinOp::Sub)?,
            LirInstructionKind::Mul(lhs, rhs) => emit_binop(asm, regs, dst, lhs, rhs, BinOp::Mul)?,
            LirInstructionKind::Eq(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Eq)?,
            LirInstructionKind::Ne(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Ne)?,
            LirInstructionKind::Lt(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Lt)?,
            LirInstructionKind::Le(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Le)?,
            LirInstructionKind::Gt(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Gt)?,
            LirInstructionKind::Ge(lhs, rhs) => emit_cmp(asm, regs, dst, lhs, rhs, CmpKind::Ge)?,
            LirInstructionKind::Alloca { .. } => {
                regs.slot_offset(inst.id);
            }
            LirInstructionKind::Load { address, .. } => {
                emit_load(asm, regs, dst, address)?;
            }
            LirInstructionKind::Store { value, address, .. } => {
                emit_store(asm, regs, value, address)?;
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported LIR instruction for aarch64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if asm.needs_stack {
                emit_epilogue(asm, regs);
            }
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm16(asm, Reg::X0, 0);
                emit_ret(asm);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let ret_reg = Reg::X0;
            emit_value_to_reg(asm, regs, ret_reg, value)?;
            if asm.needs_stack {
                emit_epilogue(asm, regs);
            }
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall_reg(asm, ret_reg)?;
            } else {
                emit_ret(asm);
            }
        }
        LirTerminator::Br(target) => {
            asm.emit_b(*target);
        }
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            emit_cond_branch(asm, regs, condition, *if_true, *if_false)?;
        }
        _ => {}
    }

    Ok(())
}

enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

fn emit_cmp(
    asm: &mut Assembler,
    regs: &mut RegFile,
    dst: Reg,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
) -> Result<()> {
    match (lhs, rhs) {
        (LirValue::Register(lhs_id), LirValue::Register(rhs_id)) => {
            let lhs_reg = regs.get(*lhs_id)?;
            let rhs_reg = regs.get(*rhs_id)?;
            emit_cmp_reg(asm, lhs_reg, rhs_reg);
        }
        (LirValue::Register(lhs_id), LirValue::Constant(constant)) => {
            let lhs_reg = regs.get(*lhs_id)?;
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > 4095 {
                return Err(Error::from("cmp immediate out of range"));
            }
            emit_cmp_imm12(asm, lhs_reg, imm as u32);
        }
        _ => return Err(Error::from("unsupported compare operands")),
    }

    let cond = match kind {
        CmpKind::Eq => 0,
        CmpKind::Ne => 1,
        CmpKind::Lt => 11,
        CmpKind::Le => 13,
        CmpKind::Gt => 12,
        CmpKind::Ge => 10,
    };
    emit_cset(asm, dst, cond);
    Ok(())
}

fn emit_cond_branch(
    asm: &mut Assembler,
    regs: &mut RegFile,
    condition: &LirValue,
    if_true: BasicBlockId,
    if_false: BasicBlockId,
) -> Result<()> {
    match condition {
        LirValue::Constant(LirConstant::Bool(value)) => {
            if *value {
                asm.emit_b(if_true);
            } else {
                asm.emit_b(if_false);
            }
        }
        LirValue::Register(id) => {
            let reg = regs.get(*id)?;
            emit_cmp_imm12(asm, reg, 0);
            asm.emit_b_cond(1, if_true);
            asm.emit_b(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}

#[derive(Clone, Copy, Debug)]
struct Fixup {
    pos: usize,
    target: BasicBlockId,
    kind: FixupKind,
}

#[derive(Clone, Copy, Debug)]
enum FixupKind {
    B,
    BCond(u32),
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<BasicBlockId, usize>,
    fixups: Vec<Fixup>,
    needs_stack: bool,
}

fn emit_prologue(asm: &mut Assembler, regs: &mut RegFile) -> Result<()> {
    let slots = regs.next_slot.max(1) as u32;
    let frame = ((slots * 8 + 15) / 16) * 16;
    emit_sub_sp(asm, frame);
    emit_store_pair(asm, Reg::X29, Reg::X30, frame as i32 - 16);
    emit_mov_reg(asm, Reg::X29, Reg::X31);
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler, regs: &RegFile) {
    emit_mov_reg(asm, Reg::X31, Reg::X29);
    emit_load_pair(asm, Reg::X29, Reg::X30, 0);
    let slots = regs.next_slot.max(1) as u32;
    let frame = ((slots * 8 + 15) / 16) * 16;
    emit_add_sp(asm, frame);
}

impl Assembler {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            needs_stack: false,
        }
    }

    fn bind(&mut self, id: BasicBlockId) {
        self.labels.insert(id, self.buf.len());
    }

    fn emit_b(&mut self, target: BasicBlockId) {
        let pos = self.buf.len();
        self.emit_u32(0x1400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::B,
        });
    }

    fn emit_b_cond(&mut self, cond: u32, target: BasicBlockId) {
        let pos = self.buf.len();
        self.emit_u32(0x5400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::BCond(cond),
        });
    }

    fn emit_u32(&mut self, word: u32) {
        self.buf.extend_from_slice(&word.to_le_bytes());
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<Vec<u8>> {
        let fixups = self.fixups.clone();
        for fixup in fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown branch target"))?;
            let origin = fixup.pos;
            let delta = (*target as i64) - (origin as i64);
            let imm = delta / 4;
            match fixup.kind {
                FixupKind::B => {
                    let imm26 = i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("branch out of range"));
                    }
                    let encoded = 0x1400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::BCond(cond) => {
                    let imm19 = i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm19 < -(1 << 18) || imm19 > (1 << 18) - 1 {
                        return Err(Error::from("conditional branch out of range"));
                    }
                    let encoded = 0x5400_0000u32
                        | (((imm19 as u32) & 0x7FFFF) << 5)
                        | (cond & 0xF);
                    self.patch_u32(origin, encoded);
                }
            }
        }
        Ok(self.buf)
    }

    fn patch_u32(&mut self, pos: usize, word: u32) {
        self.buf[pos..pos + 4].copy_from_slice(&word.to_le_bytes());
    }
}
impl Reg {
    fn is_sp(self) -> bool {
        matches!(self, Reg::X31)
    }
}
