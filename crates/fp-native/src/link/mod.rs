use fp_core::error::Result;
use std::path::Path;

use crate::emit::{TargetArch, TargetFormat};

mod elf;
mod macho;
mod pe;

/// Link an executable for a target format.
///
/// Note: Architecture differences live in the emitters (instruction bytes and
/// object layout). The link step stays format-focused and should not branch
/// on x86_64 vs arm64 unless the format itself requires it.
pub fn link_executable_minimal(
    path: &Path,
    format: TargetFormat,
    arch: TargetArch,
    text: &[u8],
) -> Result<()> {
    match format {
        TargetFormat::MachO => macho::link_executable_macho_minimal(path, arch, text),
        TargetFormat::Elf => elf::link_executable_elf64_minimal(path, arch, text),
        TargetFormat::Coff => pe::link_executable_pe64(path, arch, text),
    }
}
