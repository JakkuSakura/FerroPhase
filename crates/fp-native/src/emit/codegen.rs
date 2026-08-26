use fp_core::asmir::AsmProgram;
use fp_core::error::{Error, Result};
use fp_core::lir::{LirBlob, LirInstruction, LirInstructionKind, LirTerminator, LirValue};

use crate::emit::{CodegenOutput, TargetArch, TargetFormat, aarch64, x86_64};

pub fn emit_text_from_selection(
    lir_program: &LirBlob,
    asmir_program: &AsmProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<CodegenOutput> {
    // Validated function set: prefer `main` alone, exactly like before —
    // every real FerroPhase compile goes through this same path with
    // (often many) non-`main` functions this validation has never actually
    // exercised, so widening it to *all* defined functions unconditionally
    // would risk newly rejecting programs that compiled fine yesterday for
    // reasons unrelated to what's actually being fixed here. Only fall
    // back to validating every defined function when there's no `main` at
    // all — the one new case that needs to work: `emit_lir_program` (used
    // by the JVM/CIL/goasm/URCL-to-native transpile paths in
    // `container/pipeline.rs`) can hand this a program transpiled from
    // foreign bytecode with no `main`. A defined `main` is only required
    // to produce a runnable executable or JIT-execute a plan — both check
    // for that explicitly at that point (see `EmitPlan::entry_offset`),
    // not unconditionally here for every plan.
    let main_fn = lir_program
        .functions
        .iter()
        .find(|func| func.name.as_str() == "main" && !func.is_declaration);
    let to_validate: Vec<&fp_core::lir::LirFunction> = match main_fn {
        Some(main) => vec![main],
        None => lir_program
            .functions
            .iter()
            .filter(|func| !func.is_declaration)
            .collect(),
    };

    for func in to_validate {
        if func.basic_blocks.is_empty() {
            return Err(Error::from(format!(
                "native emitter requires at least one basic block in function {}",
                func.name
            )));
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
    }

    match arch {
        TargetArch::X86_64 => x86_64::emit_text_from_asmir(asmir_program, format),
        TargetArch::Aarch64 => aarch64::emit_text_from_asmir(asmir_program, format),
    }
}

pub fn lower_program_for_native(lir_program: &LirBlob) -> Result<LirBlob> {
    let mut lir_program = lir_program.clone();
    lower_phi_in_program(&mut lir_program)?;
    crate::jit::validate_native_program(&lir_program)?;
    Ok(lir_program)
}
fn is_call_arg_value(value: &LirValue) -> bool {
    matches!(
        value.kind,
        fp_core::lir::LirValueKind::Register(_)
            | fp_core::lir::LirValueKind::Constant(_)
            | fp_core::lir::LirValueKind::Local(_)
            | fp_core::lir::LirValueKind::StackSlot(_)
            | fp_core::lir::LirValueKind::Global(_)
            | fp_core::lir::LirValueKind::Function(_)
    )
}

fn lower_phi_in_program(program: &mut LirBlob) -> Result<()> {
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
                        result: instruction.result.clone(),
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
