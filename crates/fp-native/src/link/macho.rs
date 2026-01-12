use fp_core::error::Result;
use std::path::Path;

use crate::emit::macho::emit_executable_macho_minimal;
use crate::emit::TargetArch;

pub fn link_executable_macho_minimal(path: &Path, arch: TargetArch, text: &[u8]) -> Result<()> {
    emit_executable_macho_minimal(path, arch, text)
}
