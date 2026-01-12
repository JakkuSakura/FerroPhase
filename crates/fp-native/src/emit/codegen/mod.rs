use fp_core::error::{Error, Result};
use fp_core::lir::{LirInstructionKind, LirProgram, LirTerminator, LirType};

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

    let mut saw_alloca = false;
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if matches!(inst.kind, LirInstructionKind::Alloca { .. }) {
                saw_alloca = true;
            }
            if matches!(inst.kind, LirInstructionKind::Phi { .. }) {
                return Err(Error::from("native emitter does not support Phi yet"));
            }
            if !is_integer_type(inst.type_hint.as_ref()) {
                return Err(Error::from(
                    "native emitter only supports integer/bool instruction types",
                ));
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
        TargetArch::X86_64 => x86_64::emit_text(func, format, saw_alloca),
        TargetArch::Aarch64 => aarch64::emit_text(func, format, saw_alloca),
    }
}

fn is_integer_type(ty: Option<&LirType>) -> bool {
    match ty {
        None => true,
        Some(LirType::I1)
        | Some(LirType::I8)
        | Some(LirType::I16)
        | Some(LirType::I32)
        | Some(LirType::I64)
        | Some(LirType::I128) => true,
        _ => false,
    }
}
