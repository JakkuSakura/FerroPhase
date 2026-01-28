use fp_core::error::Result;
use std::path::Path;

use super::coff::emit_executable_pe64;
use crate::emit::{EmitPlan, TargetArch};

pub fn link_executable_pe64(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    emit_executable_pe64(path, arch, plan)
}
