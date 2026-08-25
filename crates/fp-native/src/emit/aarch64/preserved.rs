use super::*;

pub(super) fn is_synthesized_instruction(inst: &fp_core::asmir::AsmInstruction) -> bool {
    inst.annotations
        .iter()
        .any(|annotation| annotation.key == "fp.synthesized")
}

pub(super) fn collect_preserved_single_block_bytes(
    _program: &AsmProgram,
    func: &AsmFunction,
) -> Option<Vec<u8>> {
    if func.basic_blocks.len() != 1 {
        return None;
    }
    let block = func.basic_blocks.first()?;
    let default_ret = 0xD65F03C0u32.to_le_bytes();
    let default_nop = 0xD503201Fu32.to_le_bytes();
    let terminator_encoding: &[u8] = match &block.terminator {
        AsmTerminator::Return(_) => default_ret.as_slice(),
        _ => return None,
    };

    let mut out = Vec::new();
    for inst in &block.instructions {
        if matches!(inst.kind, AsmInstructionKind::Nop) {
            out.extend_from_slice(default_nop.as_slice());
            continue;
        }
        if let AsmInstructionKind::Syscall { convention, .. } = inst.kind {
            let imm16 = match convention {
                AsmSyscallConvention::LinuxAarch64 => 0u16,
                AsmSyscallConvention::DarwinAarch64 => 0x80u16,
                _ => return None,
            };
            let word = 0xD400_0001u32 | ((imm16 as u32) << 5);
            out.extend_from_slice(word.to_le_bytes().as_slice());
            continue;
        }

        if matches!(
            inst.kind,
            AsmInstructionKind::Add(_, _) | AsmInstructionKind::Sub(_, _)
        ) {
            let dst = annotation_value(&inst.annotations, "fp.preserve.aarch64.dst_gpr").and_then(
                |value| {
                    value
                        .parse::<u8>()
                        .map_err(|e| {
                            eprintln!("[fp-native] preserved-instruction parse error: {e}");
                            e
                        })
                        .ok()
                },
            );
            let src = annotation_value(&inst.annotations, "fp.preserve.aarch64.src_gpr").and_then(
                |value| {
                    value
                        .parse::<u8>()
                        .map_err(|e| {
                            eprintln!("[fp-native] preserved-instruction parse error: {e}");
                            e
                        })
                        .ok()
                },
            );
            let imm =
                annotation_value(&inst.annotations, "fp.preserve.aarch64.imm").and_then(|value| {
                    value
                        .parse::<u16>()
                        .map_err(|e| {
                            eprintln!("[fp-native] preserved-instruction parse error: {e}");
                            e
                        })
                        .ok()
                });
            if let (Some(dst), Some(src), Some(imm)) = (dst, src, imm) {
                if imm > 4095 {
                    return None;
                }
                let opcode_base = match inst.kind {
                    AsmInstructionKind::Add(_, _) => 0x9100_0000u32,
                    AsmInstructionKind::Sub(_, _) => 0xD100_0000u32,
                    _ => return None,
                };
                let word = opcode_base | ((imm as u32) << 10) | ((src as u32) << 5) | (dst as u32);
                out.extend_from_slice(word.to_le_bytes().as_slice());
                continue;
            }
        }

        if is_synthesized_instruction(inst) {
            continue;
        }
        return None;
    }
    out.extend_from_slice(terminator_encoding);
    Some(out)
}
