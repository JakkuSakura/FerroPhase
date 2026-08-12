use fp_core::asmir::{
    AsmArchitecture, AsmAttr, AsmConstant, AsmGenericOpcode, AsmOpcode, AsmOperand, AsmProgram,
    AsmRegister, AsmSyscallConvention, AsmTarget, AsmVirtualRegId, OperandAccess,
};
use std::collections::HashMap;

/// Fixes up syscall numbers/conventions in an already-selected `AsmProgram`
/// after its target has changed out from under it.
///
/// This is *not* part of the LIR-to-native compile pipeline's construction
/// path: `system_api::rewrite_program_for_target` already picks the correct
/// convention and syscall number from a per-(OS, arch) table at the point it
/// synthesizes each `Syscall` instruction (see `target_syscall_convention`
/// and `lower_system_api_to_syscall` in `crate::system_api`), so this pass
/// is a no-op there. It exists for `fp-cli`'s assembly transpile command,
/// which lifts machine code for one target, reassigns `AsmProgram::target`
/// to a different one, and needs existing (already-synthesized) `Syscall`
/// instructions corrected for the new target -- there is no single
/// "construction site" to move that correction into, since the syscalls
/// were constructed for a different target entirely.
pub fn normalize_for_target(program: &mut AsmProgram) {
    normalize_syscall_conventions_for_target(program);
}

fn normalize_syscall_conventions_for_target(program: &mut AsmProgram) {
    let Some(convention) = syscall_convention_for_target(&program.target) else {
        return;
    };

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            // `Syscall`'s own vreg id isn't visible here as a HashMap key
            // convention anymore, so track "last constant frozen into each
            // virtual register" directly off the register table id.
            let mut last_constants: HashMap<AsmVirtualRegId, AsmConstant> = HashMap::new();
            for instruction in &mut block.instructions {
                if matches!(instruction.opcode, AsmOpcode::Generic(AsmGenericOpcode::Freeze)) {
                    let result_id = instruction.operands.iter().find_map(|op| match op {
                        AsmOperand::Register {
                            reg: AsmRegister::Virtual(id),
                            access,
                        } if *access != OperandAccess::Read => Some(*id),
                        _ => None,
                    });
                    let constant = instruction.operands.iter().find_map(|op| match op {
                        AsmOperand::Constant(constant) => Some(constant.clone()),
                        _ => None,
                    });
                    if let (Some(id), Some(constant)) = (result_id, constant) {
                        last_constants.insert(id, constant);
                    }
                }

                if !matches!(instruction.opcode, AsmOpcode::Generic(AsmGenericOpcode::Syscall)) {
                    continue;
                }
                let Some(attr_idx) = instruction.operands.iter().position(|op| {
                    matches!(op, AsmOperand::Attr(AsmAttr::SyscallConvention(_)))
                }) else {
                    continue;
                };
                let old_convention = match &instruction.operands[attr_idx] {
                    AsmOperand::Attr(AsmAttr::SyscallConvention(cc)) => *cc,
                    _ => unreachable!(),
                };
                instruction.operands[attr_idx] =
                    AsmOperand::Attr(AsmAttr::SyscallConvention(convention));

                if !matches!(
                    (old_convention, convention),
                    (
                        AsmSyscallConvention::DarwinX86_64,
                        AsmSyscallConvention::DarwinAarch64
                    ) | (
                        AsmSyscallConvention::DarwinAarch64,
                        AsmSyscallConvention::DarwinX86_64
                    )
                ) {
                    continue;
                }

                // `number` is the operand immediately following the
                // SyscallConvention attr (see the Syscall operand schema in
                // `select_instruction`).
                let Some(number_operand) = instruction.operands.get_mut(attr_idx + 1) else {
                    continue;
                };
                let constant_number = match number_operand {
                    AsmOperand::Constant(AsmConstant::UInt(value, ty)) => {
                        Some((*value as i64, ty.clone()))
                    }
                    AsmOperand::Constant(AsmConstant::Int(value, ty)) => Some((*value, ty.clone())),
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(id),
                        ..
                    } => last_constants.get(id).and_then(|constant| match constant {
                        AsmConstant::UInt(value, ty) => Some((*value as i64, ty.clone())),
                        AsmConstant::Int(value, ty) => Some((*value, ty.clone())),
                        _ => None,
                    }),
                    _ => None,
                };

                if let Some((value, ty)) = constant_number {
                    let translated = match (old_convention, convention) {
                        (
                            AsmSyscallConvention::DarwinX86_64,
                            AsmSyscallConvention::DarwinAarch64,
                        ) => value.saturating_sub(0x0200_0000),
                        (
                            AsmSyscallConvention::DarwinAarch64,
                            AsmSyscallConvention::DarwinX86_64,
                        ) => value.saturating_add(0x0200_0000),
                        _ => value,
                    };

                    if translated != value {
                        *number_operand = AsmOperand::Constant(AsmConstant::Int(translated, ty));
                    }
                }
            }
        }
    }
}

fn syscall_convention_for_target(target: &AsmTarget) -> Option<AsmSyscallConvention> {
    use fp_core::asmir::AsmObjectFormat;

    match (&target.architecture, &target.object_format) {
        (AsmArchitecture::X86_64, AsmObjectFormat::Elf) => Some(AsmSyscallConvention::LinuxX86_64),
        (AsmArchitecture::X86_64, AsmObjectFormat::MachO) => {
            Some(AsmSyscallConvention::DarwinX86_64)
        }
        (AsmArchitecture::Aarch64, AsmObjectFormat::Elf) => {
            Some(AsmSyscallConvention::LinuxAarch64)
        }
        (AsmArchitecture::Aarch64, AsmObjectFormat::MachO) => {
            Some(AsmSyscallConvention::DarwinAarch64)
        }
        _ => None,
    }
}
