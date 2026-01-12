use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirTerminator,
    LirValue,
};
use std::collections::HashMap;

use crate::emit::TargetFormat;

#[derive(Clone, Copy, Debug)]
enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    Rbp,
    Rsp,
}

impl Reg {
    fn id(self) -> u8 {
        match self {
            Reg::Rax => 0,
            Reg::Rcx => 1,
            Reg::Rdx => 2,
            Reg::Rdi => 7,
            Reg::R8 => 8,
            Reg::R9 => 9,
            Reg::R10 => 10,
            Reg::R11 => 11,
            Reg::Rsp => 4,
            Reg::Rbp => 5,
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
                Reg::R10,
                Reg::R9,
                Reg::R8,
                Reg::Rdi,
                Reg::Rdx,
                Reg::Rcx,
                Reg::Rax,
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
            return -(*offset as i32 * 8);
        }
        let next = self.next_slot + 1;
        self.next_slot = next;
        self.slot_map.insert(slot_id, next);
        -(next as i32 * 8)
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
            emit_mov_rr(asm, dst, src);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            emit_mov_imm64(asm, dst, imm as u64);
            Ok(())
        }
        _ => Err(Error::from("unsupported LIR value for x86_64")),
    }
}

fn emit_op_rhs(asm: &mut Assembler, regs: &mut RegFile, dst: Reg, rhs: &LirValue, op: BinOp) -> Result<()> {
    match rhs {
        LirValue::Register(id) => {
            let src = regs.get(*id)?;
            match op {
                BinOp::Add => emit_add_rr(asm, dst, src),
                BinOp::Sub => emit_sub_rr(asm, dst, src),
                BinOp::Mul => emit_imul_rr(asm, dst, src),
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if let Ok(imm32) = i32::try_from(imm) {
                match op {
                    BinOp::Add => emit_add_ri32(asm, dst, imm32),
                    BinOp::Sub => emit_sub_ri32(asm, dst, imm32),
                    BinOp::Mul => {
                        emit_mov_imm64(asm, Reg::R11, imm as u64);
                        emit_imul_rr(asm, dst, Reg::R11);
                    }
                }
                Ok(())
            } else {
                emit_mov_imm64(asm, Reg::R11, imm as u64);
                match op {
                    BinOp::Add => emit_add_rr(asm, dst, Reg::R11),
                    BinOp::Sub => emit_sub_rr(asm, dst, Reg::R11),
                    BinOp::Mul => emit_imul_rr(asm, dst, Reg::R11),
                }
                Ok(())
            }
        }
        _ => Err(Error::from("unsupported RHS for x86_64")),
    }
}

fn constant_to_i64(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        _ => Err(Error::from("unsupported constant for x86_64")),
    }
}

fn emit_ret(asm: &mut Assembler) {
    asm.push(0xC3);
}

fn emit_exit_syscall(asm: &mut Assembler, code: u32) -> Result<()> {
    if code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    emit_mov_imm64(asm, Reg::Rdi, code as u64);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_rr(asm, Reg::Rdi, reg);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_mov_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xB8 + (dst.id() & 0x7));
    asm.extend(&imm.to_le_bytes());
}

fn emit_add_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x01);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_sub_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x29);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_imul_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.extend(&[0x0F, 0xAF]);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_add_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 0, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_sub_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_rex(asm: &mut Assembler, w: bool, reg: u8, rm: u8) {
    let mut rex = 0x40;
    if w {
        rex |= 0x08;
    }
    if (reg & 0x8) != 0 {
        rex |= 0x04;
    }
    if (rm & 0x8) != 0 {
        rex |= 0x01;
    }
    asm.push(rex);
}

fn emit_modrm(asm: &mut Assembler, mode: u8, reg: u8, rm: u8) {
    let byte = ((mode & 0x3) << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
    asm.push(byte);
}

fn emit_cmp_rr(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    emit_rex(asm, true, rhs.id(), lhs.id());
    asm.push(0x39);
    emit_modrm(asm, 0b11, rhs.id(), lhs.id());
}

fn emit_cmp_imm32(asm: &mut Assembler, lhs: Reg, imm: i32) {
    emit_rex(asm, true, 0, lhs.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 7, lhs.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_setcc(asm: &mut Assembler, cc: u8, dst: Reg) {
    emit_rex(asm, false, 0, dst.id());
    asm.push(0x0F);
    asm.push(0x90 + cc);
    emit_modrm(asm, 0b11, 0, dst.id());
}

fn emit_movzx_r64_rm8(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm(asm, 0b11, dst.id(), src.id());
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
                    "unsupported LIR instruction for x86_64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if asm.needs_stack {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm64(asm, Reg::Rax, 0);
                emit_ret(asm);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let ret_reg = Reg::Rax;
            emit_value_to_reg(asm, regs, ret_reg, value)?;
            if asm.needs_stack {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall_reg(asm, ret_reg)?;
            } else {
                emit_ret(asm);
            }
        }
        LirTerminator::Br(target) => {
            asm.emit_jmp(*target);
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
            emit_cmp_rr(asm, lhs_reg, rhs_reg);
        }
        (LirValue::Register(lhs_id), LirValue::Constant(constant)) => {
            let lhs_reg = regs.get(*lhs_id)?;
            let imm = constant_to_i64(constant)?;
            let imm32 = i32::try_from(imm).map_err(|_| Error::from("cmp immediate out of range"))?;
            emit_cmp_imm32(asm, lhs_reg, imm32);
        }
        _ => return Err(Error::from("unsupported compare operands")),
    }

    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0xC,
        CmpKind::Le => 0xE,
        CmpKind::Gt => 0xF,
        CmpKind::Ge => 0xD,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, dst, Reg::R11);
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
                asm.emit_jmp(if_true);
            } else {
                asm.emit_jmp(if_false);
            }
        }
        LirValue::Register(id) => {
            let reg = regs.get(*id)?;
            emit_cmp_imm32(asm, reg, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}

struct Fixup {
    pos: usize,
    target: BasicBlockId,
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<BasicBlockId, usize>,
    fixups: Vec<Fixup>,
    needs_stack: bool,
}

fn emit_prologue(asm: &mut Assembler, regs: &mut RegFile) -> Result<()> {
    asm.push(0x55);
    emit_rex(asm, true, Reg::Rbp.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, Reg::Rbp.id(), Reg::Rsp.id());

    let slots = regs.next_slot.max(1);
    let stack_size = (slots as i32) * 8;
    emit_sub_ri32(asm, Reg::Rsp, stack_size);
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler) {
    emit_mov_rr(asm, Reg::Rsp, Reg::Rbp);
    asm.push(0x5D);
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

    fn emit_jmp(&mut self, target: BasicBlockId) {
        self.buf.push(0xE9);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn emit_jcc(&mut self, opcode: u8, target: BasicBlockId) {
        self.buf.push(0x0F);
        self.buf.push(opcode);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn push(&mut self, byte: u8) {
        self.buf.push(byte);
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<Vec<u8>> {
        for fixup in &self.fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown jump target"))?;
            let origin = fixup.pos;
            let rel = (*target as i64) - (origin as i64 + 4);
            let rel32 = i32::try_from(rel).map_err(|_| Error::from("jump out of range"))?;
            self.buf[origin..origin + 4].copy_from_slice(&rel32.to_le_bytes());
        }
        Ok(self.buf)
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
            emit_mov_rm64(asm, dst, Reg::Rbp, offset);
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for x86_64")),
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
            emit_mov_imm64(asm, Reg::R11, imm as u64);
            Reg::R11
        }
        _ => return Err(Error::from("unsupported store value for x86_64")),
    };

    match address {
        LirValue::StackSlot(id) => {
            let offset = regs.slot_offset(*id);
            emit_mov_mr64(asm, Reg::Rbp, offset, src);
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for x86_64")),
    }
}

fn emit_mov_rm64(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x8B);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_mov_mr64(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_modrm_disp32(asm: &mut Assembler, reg: u8, rm: u8, disp: i32) {
    emit_modrm(asm, 0b10, reg, rm);
    asm.extend(&disp.to_le_bytes());
}
