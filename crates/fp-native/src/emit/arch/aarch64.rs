use fp_core::error::Result;

use super::TargetFormat;
use super::ensure_u16;

pub fn emit_entry_text(format: TargetFormat, exit_code: u32) -> Result<Vec<u8>> {
    match format {
        TargetFormat::MachO => emit_return(exit_code),
        TargetFormat::Elf | TargetFormat::Coff => emit_exit_syscall(exit_code),
    }
}

fn emit_return(exit_code: u32) -> Result<Vec<u8>> {
    let imm = ensure_u16(exit_code)? as u32;
    let mut text = Vec::new();
    text.extend_from_slice(&mov_w_imm(0, imm));
    text.extend_from_slice(&ret());
    Ok(text)
}

fn emit_exit_syscall(exit_code: u32) -> Result<Vec<u8>> {
    let imm = ensure_u16(exit_code)? as u32;
    let mut text = Vec::new();
    text.extend_from_slice(&mov_w_imm(0, imm));
    text.extend_from_slice(&mov_x_imm(8, 93));
    text.extend_from_slice(&svc());
    Ok(text)
}

fn mov_w_imm(reg: u32, imm: u32) -> [u8; 4] {
    let instr = 0x5280_0000u32 | ((imm & 0xffff) << 5) | (reg & 0x1f);
    instr.to_le_bytes()
}

fn mov_x_imm(reg: u32, imm: u32) -> [u8; 4] {
    let instr = 0xD280_0000u32 | ((imm & 0xffff) << 5) | (reg & 0x1f);
    instr.to_le_bytes()
}

fn ret() -> [u8; 4] {
    0xD65F_03C0u32.to_le_bytes()
}

fn svc() -> [u8; 4] {
    0xD400_0001u32.to_le_bytes()
}
