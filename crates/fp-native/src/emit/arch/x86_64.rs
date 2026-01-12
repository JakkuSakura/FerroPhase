use fp_core::error::{Error, Result};

use super::TargetFormat;

pub fn emit_entry_text(format: TargetFormat, exit_code: u32) -> Result<Vec<u8>> {
    match format {
        TargetFormat::MachO => emit_return(exit_code),
        TargetFormat::Elf | TargetFormat::Coff => emit_exit_syscall(exit_code),
    }
}

fn emit_return(exit_code: u32) -> Result<Vec<u8>> {
    let mut text = Vec::new();
    text.push(0xB8); // mov eax, imm32
    text.extend_from_slice(&exit_code.to_le_bytes());
    text.push(0xC3); // ret
    Ok(text)
}

fn emit_exit_syscall(exit_code: u32) -> Result<Vec<u8>> {
    if exit_code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    let mut text = Vec::new();
    text.push(0xBF); // mov edi, imm32
    text.extend_from_slice(&exit_code.to_le_bytes());
    text.push(0xB8); // mov eax, imm32
    text.extend_from_slice(&60u32.to_le_bytes());
    text.extend_from_slice(&[0x0F, 0x05]); // syscall
    Ok(text)
}
