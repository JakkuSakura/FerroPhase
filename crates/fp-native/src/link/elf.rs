use fp_core::error::Result;
use std::path::Path;

use crate::emit::elf::emit_executable_elf64_minimal;
use crate::emit::TargetArch;

pub fn link_executable_elf64_minimal(path: &Path, arch: TargetArch, text: &[u8]) -> Result<()> {
    emit_executable_elf64_minimal(path, arch, text)
}
