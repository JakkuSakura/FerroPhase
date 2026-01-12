use fp_core::error::{Error, Result};
use fp_core::lir::{LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirTerminator, LirValue};
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
        }
    }
}

struct RegFile {
    free: Vec<Reg>,
    map: HashMap<u32, Reg>,
}

impl RegFile {
    fn new() -> Self {
        Self {
            free: vec![
                Reg::R11,
                Reg::R10,
                Reg::R9,
                Reg::R8,
                Reg::Rdi,
                Reg::Rdx,
                Reg::Rcx,
                Reg::Rax,
            ],
            map: HashMap::new(),
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
}

pub fn emit_text(
    _func: &LirFunction,
    block: &LirBasicBlock,
    format: TargetFormat,
) -> Result<Vec<u8>> {
    let mut regs = RegFile::new();
    let mut out = Vec::new();

    for inst in &block.instructions {
        let dst = regs.alloc(inst.id)?;
        match &inst.kind {
            LirInstructionKind::Add(lhs, rhs) => emit_binop(&mut out, &mut regs, dst, lhs, rhs, BinOp::Add)?,
            LirInstructionKind::Sub(lhs, rhs) => emit_binop(&mut out, &mut regs, dst, lhs, rhs, BinOp::Sub)?,
            LirInstructionKind::Mul(lhs, rhs) => emit_binop(&mut out, &mut regs, dst, lhs, rhs, BinOp::Mul)?,
            other => {
                return Err(Error::from(format!(
                    "unsupported LIR instruction for x86_64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall(&mut out, 0)?;
            } else {
                emit_mov_imm64(&mut out, Reg::Rax, 0);
                emit_ret(&mut out);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let ret_reg = Reg::Rax;
            emit_value_to_reg(&mut out, &mut regs, ret_reg, value)?;
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall_reg(&mut out, ret_reg)?;
            } else {
                emit_ret(&mut out);
            }
        }
        _ => {}
    }

    Ok(out)
}

enum BinOp {
    Add,
    Sub,
    Mul,
}

fn emit_binop(
    out: &mut Vec<u8>,
    regs: &mut RegFile,
    dst: Reg,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
) -> Result<()> {
    emit_value_to_reg(out, regs, dst, lhs)?;
    emit_op_rhs(out, regs, dst, rhs, op)
}

fn emit_value_to_reg(out: &mut Vec<u8>, regs: &mut RegFile, dst: Reg, value: &LirValue) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let src = regs.get(*id)?;
            emit_mov_rr(out, dst, src);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            emit_mov_imm64(out, dst, imm as u64);
            Ok(())
        }
        _ => Err(Error::from("unsupported LIR value for x86_64")),
    }
}

fn emit_op_rhs(
    out: &mut Vec<u8>,
    regs: &mut RegFile,
    dst: Reg,
    rhs: &LirValue,
    op: BinOp,
) -> Result<()> {
    match rhs {
        LirValue::Register(id) => {
            let src = regs.get(*id)?;
            match op {
                BinOp::Add => emit_add_rr(out, dst, src),
                BinOp::Sub => emit_sub_rr(out, dst, src),
                BinOp::Mul => emit_imul_rr(out, dst, src),
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if let Ok(imm32) = i32::try_from(imm) {
                match op {
                    BinOp::Add => emit_add_ri32(out, dst, imm32),
                    BinOp::Sub => emit_sub_ri32(out, dst, imm32),
                    BinOp::Mul => {
                        let temp = regs.alloc(alloc_temp_id(rhs))?;
                        emit_mov_imm64(out, temp, imm as u64);
                        emit_imul_rr(out, dst, temp);
                    }
                }
                Ok(())
            } else {
                let temp = regs.alloc(alloc_temp_id(rhs))?;
                emit_mov_imm64(out, temp, imm as u64);
                match op {
                    BinOp::Add => emit_add_rr(out, dst, temp),
                    BinOp::Sub => emit_sub_rr(out, dst, temp),
                    BinOp::Mul => emit_imul_rr(out, dst, temp),
                }
                Ok(())
            }
        }
        _ => Err(Error::from("unsupported RHS for x86_64")),
    }
}

fn alloc_temp_id(value: &LirValue) -> u32 {
    match value {
        LirValue::Register(id) => id.wrapping_add(10_000),
        _ => 10_000,
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

fn emit_ret(out: &mut Vec<u8>) {
    out.push(0xC3);
}

fn emit_exit_syscall(out: &mut Vec<u8>, code: u32) -> Result<()> {
    if code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    emit_mov_imm64(out, Reg::Rdi, code as u64);
    emit_mov_imm64(out, Reg::Rax, 60);
    out.extend_from_slice(&[0x0F, 0x05]);
    Ok(())
}

fn emit_exit_syscall_reg(out: &mut Vec<u8>, reg: Reg) -> Result<()> {
    emit_mov_rr(out, Reg::Rdi, reg);
    emit_mov_imm64(out, Reg::Rax, 60);
    out.extend_from_slice(&[0x0F, 0x05]);
    Ok(())
}

fn emit_mov_rr(out: &mut Vec<u8>, dst: Reg, src: Reg) {
    emit_rex(out, true, src.id(), dst.id());
    out.push(0x89);
    emit_modrm(out, 0b11, src.id(), dst.id());
}

fn emit_mov_imm64(out: &mut Vec<u8>, dst: Reg, imm: u64) {
    emit_rex(out, true, 0, dst.id());
    out.push(0xB8 + (dst.id() & 0x7));
    out.extend_from_slice(&imm.to_le_bytes());
}

fn emit_add_rr(out: &mut Vec<u8>, dst: Reg, src: Reg) {
    emit_rex(out, true, src.id(), dst.id());
    out.push(0x01);
    emit_modrm(out, 0b11, src.id(), dst.id());
}

fn emit_sub_rr(out: &mut Vec<u8>, dst: Reg, src: Reg) {
    emit_rex(out, true, src.id(), dst.id());
    out.push(0x29);
    emit_modrm(out, 0b11, src.id(), dst.id());
}

fn emit_imul_rr(out: &mut Vec<u8>, dst: Reg, src: Reg) {
    emit_rex(out, true, dst.id(), src.id());
    out.extend_from_slice(&[0x0F, 0xAF]);
    emit_modrm(out, 0b11, dst.id(), src.id());
}

fn emit_add_ri32(out: &mut Vec<u8>, dst: Reg, imm: i32) {
    emit_rex(out, true, 0, dst.id());
    out.push(0x81);
    emit_modrm(out, 0b11, 0, dst.id());
    out.extend_from_slice(&imm.to_le_bytes());
}

fn emit_sub_ri32(out: &mut Vec<u8>, dst: Reg, imm: i32) {
    emit_rex(out, true, 0, dst.id());
    out.push(0x81);
    emit_modrm(out, 0b11, 5, dst.id());
    out.extend_from_slice(&imm.to_le_bytes());
}

fn emit_rex(out: &mut Vec<u8>, w: bool, reg: u8, rm: u8) {
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
    out.push(rex);
}

fn emit_modrm(out: &mut Vec<u8>, mode: u8, reg: u8, rm: u8) {
    let byte = ((mode & 0x3) << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
    out.push(byte);
}
