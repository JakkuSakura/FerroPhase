use fp_core::error::Result;
use std::path::Path;

use crate::emit::{EmitPlan, TargetArch, TargetFormat};

pub(crate) mod coff;
pub(crate) mod elf;
pub(crate) mod macho;
mod pe;

/// Link an executable for a target format.
///
/// Note: Architecture differences live in the emitters (instruction bytes and
/// object layout). The link step stays format-focused and should not branch
/// on x86_64 vs arm64 unless the format itself requires it.
pub fn link_executable(
    path: &Path,
    format: TargetFormat,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    match format {
        TargetFormat::MachO => macho::link_executable_macho(path, arch, plan),
        TargetFormat::Elf => elf::link_executable_elf64(path, arch, plan),
        TargetFormat::Coff => pe::link_executable_pe64(path, arch, plan),
    }
}
