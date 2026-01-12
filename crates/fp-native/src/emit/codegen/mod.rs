use fp_core::error::{Error, Result};
use fp_core::lir::{LirInstructionKind, LirProgram, LirTerminator};

use crate::emit::{TargetArch, TargetFormat};

mod aarch64;
mod x86_64;

pub fn emit_text_from_lir(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<Vec<u8>> {
    let func = lir_program
        .functions
        .first()
        .ok_or_else(|| Error::from("no functions in LIR program"))?;

    if func.basic_blocks.is_empty() {
        return Err(Error::from(
            "native emitter requires at least one basic block",
        ));
    }

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if matches!(inst.kind, LirInstructionKind::Phi { .. }) {
                return Err(Error::from("native emitter does not support Phi yet"));
            }
        }
        match &block.terminator {
            LirTerminator::Return(_)
            | LirTerminator::Br(_)
            | LirTerminator::CondBr { .. } => {}
            other => {
                return Err(Error::from(format!(
                    "native emitter does not support terminator {other:?}"
                )));
            }
        }
    }

    match arch {
        TargetArch::X86_64 => x86_64::emit_text(func, format),
        TargetArch::Aarch64 => aarch64::emit_text(func, format),
    }
}
