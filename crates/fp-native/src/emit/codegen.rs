use fp_core::error::{Error, Result};
use fp_core::lir::layout::align_of;
use fp_core::lir::{
    LirConstant, LirInstruction, LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue,
};

use crate::emit::{CodegenOutput, TargetArch, TargetFormat, aarch64, x86_64};

pub fn emit_text_from_lir(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<CodegenOutput> {
    let mut lir_program = lir_program.clone();
    lower_phi_in_program(&mut lir_program)?;

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
            if let LirInstructionKind::Call { function: _, args, .. } = &inst.kind {
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
        TargetArch::X86_64 => x86_64::emit_text(&lir_program, format),
        TargetArch::Aarch64 => aarch64::emit_text(&lir_program, format),
    }
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
    for func in &mut program.functions {
        lower_phi_in_function(func)?;
    }
    Ok(())
}

fn lower_phi_in_function(func: &mut fp_core::lir::LirFunction) -> Result<()> {
    let mut max_id = 0u32;
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            max_id = max_id.max(inst.id);
        }
    }

    let mut block_index = std::collections::HashMap::new();
    for (idx, block) in func.basic_blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    let mut entry_allocas = Vec::new();

    for block_idx in 0..func.basic_blocks.len() {
        let mut replacements = Vec::new();
        let mut stores = Vec::new();
        let mut phis = Vec::new();

        {
            let block = &func.basic_blocks[block_idx];
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                if let LirInstructionKind::Phi { incoming } = &inst.kind {
                    phis.push((inst_idx, inst.id, incoming.clone(), inst.type_hint.clone()));
                }
            }
        }

        if phis.is_empty() {
            continue;
        }

        for (inst_idx, phi_id, incoming, phi_ty) in phis {
            let Some(phi_ty) = phi_ty else {
                return Err(Error::from("phi instruction missing type hint"));
            };
            let align = align_of(&phi_ty) as u32;
            max_id += 1;
            let alloca_id = max_id;
            entry_allocas.push(LirInstruction {
                id: alloca_id,
                kind: LirInstructionKind::Alloca {
                    size: LirValue::Constant(LirConstant::Int(1, LirType::I32)),
                    alignment: align,
                },
                type_hint: Some(LirType::Ptr(Box::new(phi_ty.clone()))),
                debug_info: None,
            });

            for (value, pred) in incoming {
                let Some(pred_idx) = block_index.get(&pred).copied() else {
                    return Err(Error::from("phi predecessor block not found"));
                };
                max_id += 1;
                stores.push((
                    pred_idx,
                    LirInstruction {
                        id: max_id,
                        kind: LirInstructionKind::Store {
                            value,
                            address: LirValue::Register(alloca_id),
                            alignment: Some(align),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    },
                ));
            }

            replacements.push((
                inst_idx,
                LirInstruction {
                    id: phi_id,
                    kind: LirInstructionKind::Load {
                        address: LirValue::Register(alloca_id),
                        alignment: Some(align),
                        volatile: false,
                    },
                    type_hint: Some(phi_ty),
                    debug_info: None,
                },
            ));
        }

        if let Some(block) = func.basic_blocks.get_mut(block_idx) {
            for (inst_idx, replacement) in replacements.into_iter().rev() {
                block.instructions[inst_idx] = replacement;
            }
        }

        for (pred_idx, store_inst) in stores {
            if let Some(pred_block) = func.basic_blocks.get_mut(pred_idx) {
                pred_block.instructions.push(store_inst);
            }
        }
    }

    if !entry_allocas.is_empty() {
        if let Some(entry_block) = func.basic_blocks.first_mut() {
            let mut new_insts =
                Vec::with_capacity(entry_allocas.len() + entry_block.instructions.len());
            new_insts.extend(entry_allocas);
            new_insts.extend(entry_block.instructions.drain(..));
            entry_block.instructions = new_insts;
        }
    }

    Ok(())
}
