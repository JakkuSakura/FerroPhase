use fp_core::error::{Error, Result};

use super::{TargetArch, TargetFormat};

mod aarch64;
mod x86_64;

pub fn emit_entry_text(arch: TargetArch, format: TargetFormat, exit_code: u32) -> Result<Vec<u8>> {
    match arch {
        TargetArch::X86_64 => x86_64::emit_entry_text(format, exit_code),
        TargetArch::Aarch64 => aarch64::emit_entry_text(format, exit_code),
    }
}

fn ensure_u16(value: u32) -> Result<u16> {
    u16::try_from(value).map_err(|_| Error::from("exit code exceeds u16 range"))
}
