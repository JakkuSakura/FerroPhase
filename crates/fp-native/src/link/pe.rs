use fp_core::error::Result;
use std::path::Path;

use crate::emit::coff::emit_executable_pe64;
use crate::emit::TargetArch;

pub fn link_executable_pe64(path: &Path, arch: TargetArch, text: &[u8]) -> Result<()> {
    emit_executable_pe64(path, arch, text)
}
