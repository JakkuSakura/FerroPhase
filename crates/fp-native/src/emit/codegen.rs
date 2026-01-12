use fp_core::error::{Error, Result};
use fp_core::lir::{LirInstructionKind, LirProgram, LirTerminator, LirValue};

use crate::emit::{aarch64, x86_64, CodegenOutput, TargetArch, TargetFormat};

pub fn emit_text_from_lir(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<CodegenOutput> {
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
            if let LirInstructionKind::Call { function, args, .. } = &inst.kind {
                if !matches!(function, LirValue::Function(_)) {
                    return Err(Error::from("native emitter only supports direct calls"));
                }
                if !args.iter().all(is_simple_value) {
                    return Err(Error::from(
                        "native emitter only supports register/constant call args",
                    ));
                }
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
        TargetArch::X86_64 => x86_64::emit_text(lir_program, format),
        TargetArch::Aarch64 => aarch64::emit_text(lir_program, format),
    }
}

fn is_simple_value(value: &LirValue) -> bool {
    matches!(value, LirValue::Register(_) | LirValue::Constant(_))
}
