use fp_core::error::{Error, Result};
use fp_core::lir::{LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirTerminator, LirValue};
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
                Reg::X7, Reg::X6, Reg::X5, Reg::X4, Reg::X3, Reg::X2, Reg::X1, Reg::X0,
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
                    "unsupported LIR instruction for aarch64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) {
                emit_exit_syscall(&mut out, 0)?;
            } else {
                emit_mov_imm16(&mut out, Reg::X0, 0);
                emit_ret(&mut out);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let ret_reg = Reg::X0;
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
            emit_mov_reg(out, dst, src);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                return Err(Error::from("aarch64 immediate out of range"));
            }
            emit_mov_imm16(out, dst, imm as u16);
            Ok(())
        }
        _ => Err(Error::from("unsupported LIR value for aarch64")),
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
                BinOp::Add => emit_add_reg(out, dst, dst, src),
                BinOp::Sub => emit_sub_reg(out, dst, dst, src),
                BinOp::Mul => emit_mul_reg(out, dst, dst, src),
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm >= 0 && imm <= 4095 {
                match op {
                    BinOp::Add => emit_add_imm12(out, dst, dst, imm as u32),
                    BinOp::Sub => emit_sub_imm12(out, dst, dst, imm as u32),
                    BinOp::Mul => {
                        let temp = regs.alloc(alloc_temp_id(rhs))?;
                        emit_mov_imm16(out, temp, imm as u16);
                        emit_mul_reg(out, dst, dst, temp);
                    }
                }
                Ok(())
            } else {
                let temp = regs.alloc(alloc_temp_id(rhs))?;
                emit_mov_imm16(out, temp, imm as u16);
                match op {
                    BinOp::Add => emit_add_reg(out, dst, dst, temp),
                    BinOp::Sub => emit_sub_reg(out, dst, dst, temp),
                    BinOp::Mul => emit_mul_reg(out, dst, dst, temp),
                }
                Ok(())
            }
        }
        _ => Err(Error::from("unsupported RHS for aarch64")),
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
        _ => Err(Error::from("unsupported constant for aarch64")),
    }
}

fn emit_mov_reg(out: &mut Vec<u8>, dst: Reg, src: Reg) {
    if dst.id() == src.id() {
        return;
    }
    let instr = 0xAA00_03E0u32 | (src.id() << 16) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_mov_imm16(out: &mut Vec<u8>, dst: Reg, imm: u16) {
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_add_reg(out: &mut Vec<u8>, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8B00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_sub_reg(out: &mut Vec<u8>, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_mul_reg(out: &mut Vec<u8>, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9B00_7C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_add_imm12(out: &mut Vec<u8>, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0x9100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_sub_imm12(out: &mut Vec<u8>, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0xD100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_ret(out: &mut Vec<u8>) {
    out.extend_from_slice(&0xD65F_03C0u32.to_le_bytes());
}

fn emit_exit_syscall(out: &mut Vec<u8>, code: u16) -> Result<()> {
    emit_mov_imm16(out, Reg::X0, code);
    emit_mov_imm16(out, Reg::X8, 93);
    emit_svc(out);
    Ok(())
}

fn emit_exit_syscall_reg(out: &mut Vec<u8>, reg: Reg) -> Result<()> {
    emit_mov_reg(out, Reg::X0, reg);
    emit_mov_imm16(out, Reg::X8, 93);
    emit_svc(out);
    Ok(())
}

fn emit_svc(out: &mut Vec<u8>) {
    out.extend_from_slice(&0xD400_0001u32.to_le_bytes());
}
