use fp_core::error::{Error, Result};
use fp_core::lir::{LirProgram, LirTerminator};

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

    if func.basic_blocks.len() != 1 {
        return Err(Error::from(
            "native emitter only supports a single basic block in this stage",
        ));
    }

    let block = &func.basic_blocks[0];
    match &block.terminator {
        LirTerminator::Return(_) => {}
        other => {
            return Err(Error::from(format!(
                "native emitter only supports Return terminators, got {other:?}"
            )));
        }
    }

    match arch {
        TargetArch::X86_64 => x86_64::emit_text(func, block, format),
        TargetArch::Aarch64 => aarch64::emit_text(func, block, format),
    }
}
