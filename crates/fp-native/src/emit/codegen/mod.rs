use fp_core::error::{Error, Result};
use fp_core::lir::{
    LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue,
};

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
            if !is_integer_type(inst.type_hint.as_ref()) {
                return Err(Error::from(
                    "native emitter only supports integer/bool instruction types",
                ));
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

fn is_simple_value(value: &LirValue) -> bool {
    matches!(value, LirValue::Register(_) | LirValue::Constant(_))
}
