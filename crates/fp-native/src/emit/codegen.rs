use fp_core::asmir::AsmProgram;
use fp_core::error::{Error, Result};
use fp_core::lir::{LirInstruction, LirInstructionKind, LirProgram, LirTerminator, LirValue};

use crate::emit::{CodegenOutput, TargetArch, TargetFormat, aarch64, x86_64};

pub fn emit_text_from_selection(
    lir_program: &LirProgram,
    asmir_program: &AsmProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<CodegenOutput> {
    let func = lir_program
        .functions
        .iter()
        .find(|func| !func.is_declaration)
        .ok_or_else(|| Error::from("no defined functions in LIR program"))?;

    if func.basic_blocks.is_empty() {
        return Err(Error::from(
            "native emitter requires at least one basic block",
        ));
    }

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if let LirInstructionKind::Call {
                function: _, args, ..
            } = &inst.kind
            {
                if !args.iter().all(is_call_arg_value) {
                    return Err(Error::from(
                        "native emitter only supports register/constant/local/stack call args",
                    ));
                }
            }
        }
        match &block.terminator {
            LirTerminator::Return(_)
            | LirTerminator::Br(_)
            | LirTerminator::CondBr { .. }
            | LirTerminator::Switch { .. }
            | LirTerminator::Unreachable
            | LirTerminator::Invoke { .. } => {}
            other => {
                return Err(Error::from(format!(
                    "native emitter does not support terminator {other:?}"
                )));
            }
        }
    }

    match arch {
        TargetArch::X86_64 => x86_64::emit_text_from_asmir(asmir_program, format),
        TargetArch::Aarch64 => aarch64::emit_text_from_asmir(asmir_program, format),
    }
}

pub fn lower_program_for_native(lir_program: &LirProgram) -> Result<LirProgram> {
    let mut lir_program = lir_program.clone();
    lower_phi_in_program(&mut lir_program)?;
    crate::jit::validate_native_program(&lir_program)?;
    Ok(lir_program)
}
fn is_call_arg_value(value: &LirValue) -> bool {
    matches!(
        value,
        LirValue::Register(_)
            | LirValue::Constant(_)
            | LirValue::Local(_)
            | LirValue::StackSlot(_)
            | LirValue::Global(_, _)
            | LirValue::Null(_)
            | LirValue::Undef(_)
    )
}

fn lower_phi_in_program(program: &mut LirProgram) -> Result<()> {
    for function in &mut program.functions {
        lower_phi_in_function(function)?;
    }
    Ok(())
}

fn lower_phi_in_function(function: &mut fp_core::lir::LirFunction) -> Result<()> {
    let mut block_index = std::collections::HashMap::new();
    for (idx, block) in function.basic_blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    let mut copies_per_block: std::collections::HashMap<usize, Vec<LirInstruction>> =
        std::collections::HashMap::new();

    for block in &mut function.basic_blocks {
        let mut retained = Vec::with_capacity(block.instructions.len());
        for instruction in &block.instructions {
            let LirInstructionKind::Phi { incoming } = &instruction.kind else {
                retained.push(instruction.clone());
                continue;
            };

            for (value, predecessor) in incoming {
                let Some(pred_idx) = block_index.get(predecessor).copied() else {
                    return Err(Error::from("phi predecessor block not found"));
                };
                copies_per_block
                    .entry(pred_idx)
                    .or_default()
                    .push(LirInstruction {
                        id: instruction.id,
                        kind: LirInstructionKind::Freeze(value.clone()),
                        type_hint: instruction.type_hint.clone(),
                        debug_info: instruction.debug_info.clone(),
                    });
            }
        }
        block.instructions = retained;
    }

    for (block_idx, mut copies) in copies_per_block {
        function.basic_blocks[block_idx]
            .instructions
            .append(&mut copies);
    }

    Ok(())
}
