use crate::binary::LiftedFunction;
use crate::binary::TextRelocation;
use crate::binary::cfg::wire_block_edges;
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmSyscallConvention, AsmType,
    AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LastCompare {
    id: u32,
    index: usize,
}

fn synthesized_annotations(reason: &str) -> Vec<fp_core::asmir::AsmAnnotation> {
    vec![fp_core::asmir::AsmAnnotation {
        key: "fp.synthesized".to_string(),
        value: reason.to_string(),
    }]
}

fn decode_b_cond_immediate(word: u32, offset: u64) -> Result<Option<(u8, u64)>> {
    // B.cond immediate.
    if (word & 0xFF000010) != 0x54000000 {
        return Ok(None);
    }
    let imm19 = ((word >> 5) & 0x7FFFF) as i32;
    let imm19 = (imm19 << 13) >> 13;
    let target = (offset as i64)
        .saturating_add(4)
        .saturating_add((imm19 as i64) << 2);
    if target < 0 {
        return Err(Error::from("aarch64 conditional branch target underflow"));
    }
    let condition = (word & 0x0F) as u8;
    Ok(Some((condition, target as u64)))
}

fn decode_cmp_register(word: u32) -> Option<(u8, u8)> {
    // Alias: `cmp Xn, Xm` -> `subs xzr, Xn, Xm`.
    // Match the opcode bits and the XZR destination.
    if (word & 0xFF00001F) != 0xEB00001F {
        return None;
    }
    let rm = ((word >> 16) & 0x1F) as u8;
    let rn = ((word >> 5) & 0x1F) as u8;
    Some((rn, rm))
}

fn decode_cmp_immediate(word: u32) -> Option<(u8, i64)> {
    // Alias: `cmp Xn, #imm` -> `subs xzr, Xn, #imm`.
    if (word & 0xFF00001F) != 0xF100001F {
        return None;
    }
    let shift = (word >> 22) & 0x3;
    if shift != 0 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    Some((rn, imm12))
}

fn compare_instruction(
    id: u32,
    kind: AsmInstructionKind,
    opcode: fp_core::asmir::AsmGenericOpcode,
) -> AsmInstruction {
    AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(opcode),
        kind,
        type_hint: None,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn patch_compare_kind(
    instructions: &mut [AsmInstruction],
    compare: &LastCompare,
    condition: u8,
) -> Result<()> {
    let inst = instructions
        .get_mut(compare.index)
        .ok_or_else(|| Error::from("missing comparison instruction"))?;
    if inst.id != compare.id {
        return Err(Error::from("comparison instruction id mismatch"));
    }
    let (lhs, rhs) = match &inst.kind {
        AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => (lhs.clone(), rhs.clone()),
        _ => {
            return Err(Error::from(
                "comparison instruction has unexpected kind for patching",
            ));
        }
    };
    let (kind, opcode) = compare_kind_from_cond(condition, lhs, rhs)?;
    inst.kind = kind;
    inst.opcode = AsmOpcode::Generic(opcode);
    inst.type_hint = None;
    Ok(())
}

fn compare_kind_from_cond(
    condition: u8,
    lhs: AsmValue,
    rhs: AsmValue,
) -> Result<(AsmInstructionKind, fp_core::asmir::AsmGenericOpcode)> {
    Ok(match condition {
        0 => (
            AsmInstructionKind::Eq(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Eq,
        ),
        1 => (
            AsmInstructionKind::Ne(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ne,
        ),
        10 => (
            AsmInstructionKind::Ge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ge,
        ),
        11 => (
            AsmInstructionKind::Lt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Lt,
        ),
        12 => (
            AsmInstructionKind::Gt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Gt,
        ),
        13 => (
            AsmInstructionKind::Le(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Le,
        ),
        2 => (
            AsmInstructionKind::Uge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Uge,
        ),
        3 => (
            AsmInstructionKind::Ult(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ult,
        ),
        8 => (
            AsmInstructionKind::Ugt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ugt,
        ),
        9 => (
            AsmInstructionKind::Ule(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ule,
        ),
        other => {
            return Err(Error::from(format!(
                "unsupported aarch64 condition code: {other}"
            )));
        }
    })
}

pub fn lift_function_bytes(
    bytes: &[u8],
    relocs: &[TextRelocation],
    syscall_convention: Option<AsmSyscallConvention>,
) -> Result<LiftedFunction> {
    if bytes.len() % 4 != 0 {
        return Err(Error::from("aarch64 function size is not 4-byte aligned"));
    }

    let instruction_count = bytes.len() / 4;
    let mut block_starts = vec![0u64];
    for inst_index in 0..instruction_count {
        let offset = (inst_index * 4) as u64;
        let word = u32::from_le_bytes(
            bytes[inst_index * 4..inst_index * 4 + 4]
                .try_into()
                .unwrap(),
        );
        if word == 0xD65F03C0 {
            let fallthrough = offset + 4;
            if fallthrough < bytes.len() as u64 {
                block_starts.push(fallthrough);
            }
            continue;
        }
        if let Some(target) = decode_b_immediate(word, offset)? {
            block_starts.push(target);
            let fallthrough = offset + 4;
            if fallthrough < bytes.len() as u64 {
                block_starts.push(fallthrough);
            }
            continue;
        }
        if let Some((_, target)) = decode_b_cond_immediate(word, offset)? {
            block_starts.push(target);
            let fallthrough = offset + 4;
            if fallthrough < bytes.len() as u64 {
                block_starts.push(fallthrough);
            }
        }
    }
    block_starts.sort_unstable();
    block_starts.dedup();

    let offset_to_block = block_starts
        .iter()
        .enumerate()
        .map(|(idx, offset)| (*offset, idx as u32))
        .collect::<std::collections::HashMap<_, _>>();

    let mut ctx = RegisterLiftContext::new();
    let mut next_id = 0u32;
    let mut basic_blocks = Vec::new();

    for (block_index, &block_offset) in block_starts.iter().enumerate() {
        let block_id = block_index as u32;
        let next_block_offset = block_starts
            .get(block_index + 1)
            .copied()
            .unwrap_or(bytes.len() as u64);

        let mut instructions = Vec::new();
        let mut terminated = false;
        let mut last_compare: Option<LastCompare> = None;

        let mut cursor = block_offset;
        while cursor < next_block_offset {
            let inst_index = (cursor / 4) as usize;
            let word = u32::from_le_bytes(
                bytes[inst_index * 4..inst_index * 4 + 4]
                    .try_into()
                    .unwrap(),
            );

            if word == 0xD503201F {
                let nop_id = next_id;
                instructions.push(AsmInstruction {
                    id: nop_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Nop),
                    kind: AsmInstructionKind::Nop,
                    type_hint: None,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                next_id += 1;
                cursor += 4;
                continue;
            }

            if word == 0xD65F03C0 {
                let return_value = ctx.read_return_value();
                basic_blocks.push(fp_core::asmir::AsmBlock {
                    id: block_id,
                    label: None,
                    instructions: std::mem::take(&mut instructions),
                    terminator: fp_core::asmir::AsmTerminator::Return(return_value),
                    terminator_encoding: None,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
                terminated = true;
                break;
            }

            if let Some((condition, target)) = decode_b_cond_immediate(word, cursor)? {
                let if_true = offset_to_block
                    .get(&target)
                    .copied()
                    .ok_or_else(|| Error::from("missing aarch64 conditional target block"))?;
                let fallthrough = cursor + 4;
                let if_false = offset_to_block
                    .get(&fallthrough)
                    .copied()
                    .ok_or_else(|| Error::from("missing aarch64 conditional fallthrough block"))?;
                let compare = last_compare
                    .as_ref()
                    .ok_or_else(|| Error::from("conditional branch without comparison"))?;
                patch_compare_kind(&mut instructions, compare, condition)?;
                basic_blocks.push(fp_core::asmir::AsmBlock {
                    id: block_id,
                    label: None,
                    instructions: std::mem::take(&mut instructions),
                    terminator: fp_core::asmir::AsmTerminator::CondBr {
                        condition: AsmValue::Flags(compare.id),
                        if_true,
                        if_false,
                    },
                    terminator_encoding: None,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
                terminated = true;
                break;
            }

            if let Some(target) = decode_b_immediate(word, cursor)? {
                let dest = offset_to_block
                    .get(&target)
                    .copied()
                    .ok_or_else(|| Error::from("missing aarch64 branch target block"))?;
                basic_blocks.push(fp_core::asmir::AsmBlock {
                    id: block_id,
                    label: None,
                    instructions: std::mem::take(&mut instructions),
                    terminator: fp_core::asmir::AsmTerminator::Br(dest),
                    terminator_encoding: None,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
                terminated = true;
                break;
            }

            if (word & 0xFC000000) == 0x94000000 {
                let reloc = relocation_at(relocs, cursor)
                    .ok_or_else(|| Error::from("unsupported aarch64 bl without relocation"))?;
                let id = next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function(reloc.symbol.clone()),
                        args: Vec::new(),
                        calling_convention: CallingConvention::AAPCS,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Void),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                next_id += 1;
                cursor += 4;
                continue;
            }

            if (word & 0x9F000000) == 0x90000000 {
                // ADRP Xd, label@PAGE
                //
                // The immediate encoding is PC-relative and page-based. For semantic lifting we
                // rely on the relocation target rather than reconstructing the page delta.
                let rd = (word & 0x1F) as u8;
                let reloc = relocation_at(relocs, cursor)
                    .ok_or_else(|| Error::from("unsupported aarch64 adrp without relocation"))?;
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(reloc.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                next_id += 1;

                let mut value = AsmValue::Register(symbol_id);
                if reloc.addend != 0 {
                    value = pointer_add_immediate(
                        value,
                        reloc.addend,
                        &mut instructions,
                        &mut next_id,
                    )?;
                }
                ctx.write_gpr(rd, value);
                cursor += 4;
                continue;
            }

            if (word & 0xFFE0_001F) == 0xD400_0001 {
                // SVC #imm16
                let syscall_convention = syscall_convention.ok_or_else(|| {
                    Error::from("aarch64 syscall lifting is disabled for COFF/PE")
                })?;
                let imm16 = ((word >> 5) & 0xFFFF) as u16;
                match (syscall_convention, imm16) {
                    (AsmSyscallConvention::LinuxAarch64, 0)
                    | (AsmSyscallConvention::DarwinAarch64, 0x80) => {}
                    _ => {
                        return Err(Error::from(
                            "unsupported aarch64 svc immediate for syscall convention",
                        ));
                    }
                }

                let number_reg = match syscall_convention {
                    AsmSyscallConvention::LinuxAarch64 => 8,
                    AsmSyscallConvention::DarwinAarch64 => 16,
                    _ => {
                        return Err(Error::from(
                            "unsupported syscall convention for aarch64 lifter",
                        ));
                    }
                };
                let number = ctx.read_gpr(number_reg)?;
                let args = vec![
                    ctx.read_gpr(0)?,
                    ctx.read_gpr(1)?,
                    ctx.read_gpr(2)?,
                    ctx.read_gpr(3)?,
                    ctx.read_gpr(4)?,
                    ctx.read_gpr(5)?,
                ];

                let id = next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: syscall_convention,
                        number,
                        args,
                    },
                    type_hint: Some(AsmType::I64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                next_id += 1;
                ctx.write_gpr(0, AsmValue::Register(id));
                cursor += 4;
                continue;
            }

            if let Some((lhs, rhs)) = decode_cmp_register(word) {
                let lhs_value = ctx.read_gpr(lhs)?;
                let rhs_value = ctx.read_gpr(rhs)?;
                let id = next_id;
                instructions.push(compare_instruction(
                    id,
                    AsmInstructionKind::Eq(lhs_value, rhs_value),
                    fp_core::asmir::AsmGenericOpcode::Eq,
                ));
                next_id += 1;
                last_compare = Some(LastCompare {
                    id,
                    index: instructions.len() - 1,
                });
                cursor += 4;
                continue;
            }

            if let Some((lhs, imm)) = decode_cmp_immediate(word) {
                let lhs_value = ctx.read_gpr(lhs)?;
                let rhs_value = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
                let id = next_id;
                instructions.push(compare_instruction(
                    id,
                    AsmInstructionKind::Eq(lhs_value, rhs_value),
                    fp_core::asmir::AsmGenericOpcode::Eq,
                ));
                next_id += 1;
                last_compare = Some(LastCompare {
                    id,
                    index: instructions.len() - 1,
                });
                cursor += 4;
                continue;
            }

            if let Some((dst, src, imm)) = decode_add_immediate(word) {
                if let Some(reloc) = relocation_at(relocs, cursor) {
                    let lhs = ctx.read_gpr(src)?;
                    let rhs = AsmValue::Constant(AsmConstant::Int(
                        reloc.addend.saturating_add(imm),
                        AsmType::I64,
                    ));
                    let id = next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs, rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(id));
                    cursor += 4;
                    continue;
                }
            }

            if let Some((dst, base, disp)) = decode_ldr_immediate(word) {
                if let Some(reloc) = relocation_at(relocs, cursor) {
                    let base_value = ctx.read_gpr(base)?;
                    let addr = pointer_add_immediate(
                        base_value,
                        disp.saturating_add(reloc.addend),
                        &mut instructions,
                        &mut next_id,
                    )?;
                    let id = next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(id));
                    cursor += 4;
                    continue;
                }
            }

            if let Some((value, base, disp)) = decode_str_immediate(word) {
                if let Some(reloc) = relocation_at(relocs, cursor) {
                    let base_value = ctx.read_gpr(base)?;
                    let addr = pointer_add_immediate(
                        base_value,
                        disp.saturating_add(reloc.addend),
                        &mut instructions,
                        &mut next_id,
                    )?;
                    let stored = ctx.read_gpr(value)?;
                    let id = next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        type_hint: Some(AsmType::Void),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    next_id += 1;
                    cursor += 4;
                    continue;
                }
            }

            lift_instruction(word, &mut ctx, &mut instructions, &mut next_id)?;
            cursor += 4;
        }

        if !terminated {
            let terminator = if block_index + 1 < block_starts.len() {
                fp_core::asmir::AsmTerminator::Br((block_index + 1) as u32)
            } else {
                fp_core::asmir::AsmTerminator::Return(None)
            };
            basic_blocks.push(fp_core::asmir::AsmBlock {
                id: block_id,
                label: None,
                instructions,
                terminator,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }
    }

    wire_block_edges(&mut basic_blocks);

    Ok(LiftedFunction {
        basic_blocks,
        locals: ctx.locals,
        stack_slots: Vec::new(),
        direct_call_targets: Vec::new(),
    })
}

fn relocation_at<'a>(relocs: &'a [TextRelocation], offset: u64) -> Option<&'a TextRelocation> {
    relocs.iter().find(|reloc| reloc.offset == offset)
}

fn decode_b_immediate(word: u32, offset: u64) -> Result<Option<u64>> {
    // B immediate.
    if (word & 0xFC000000) != 0x14000000 {
        return Ok(None);
    }
    let imm26 = (word & 0x03FF_FFFF) as i32;
    let imm26 = (imm26 << 6) >> 6;
    let target = (offset as i64)
        .saturating_add(4)
        .saturating_add((imm26 as i64) << 2);
    if target < 0 {
        return Err(Error::from("aarch64 branch target underflow"));
    }
    if target % 4 != 0 {
        return Err(Error::from("aarch64 branch target is not aligned"));
    }
    Ok(Some(target as u64))
}

fn lift_instruction(
    word: u32,
    ctx: &mut RegisterLiftContext,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<()> {
    if let Some((dst, src, imm)) = decode_add_immediate(word) {
        let lhs = ctx.read_gpr(src)?;
        let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
        let id = *next_id;
        let mut inst = build_binop(
            id,
            AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        );
        inst.annotations.extend([
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.dst_gpr".to_string(),
                value: dst.to_string(),
            },
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.src_gpr".to_string(),
                value: src.to_string(),
            },
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.imm".to_string(),
                value: imm.to_string(),
            },
        ]);
        instructions.push(inst);
        *next_id += 1;
        ctx.write_gpr(dst, AsmValue::Register(id));
        return Ok(());
    }

    if let Some((dst, src, imm)) = decode_sub_immediate(word) {
        let lhs = ctx.read_gpr(src)?;
        let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
        let id = *next_id;
        let mut inst = build_binop(
            id,
            AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
        );
        inst.annotations.extend([
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.dst_gpr".to_string(),
                value: dst.to_string(),
            },
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.src_gpr".to_string(),
                value: src.to_string(),
            },
            fp_core::asmir::AsmAnnotation {
                key: "fp.preserve.aarch64.imm".to_string(),
                value: imm.to_string(),
            },
        ]);
        instructions.push(inst);
        *next_id += 1;
        ctx.write_gpr(dst, AsmValue::Register(id));
        return Ok(());
    }

    if let Some((dst, base, disp)) = decode_ldr_immediate(word) {
        let base_value = ctx.read_gpr(base)?;
        let addr = pointer_add_immediate(base_value, disp, instructions, next_id)?;
        let id = *next_id;
        instructions.push(AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
            kind: AsmInstructionKind::Load {
                address: addr,
                alignment: None,
                volatile: false,
            },
            type_hint: Some(AsmType::I64),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        });
        *next_id += 1;
        ctx.write_gpr(dst, AsmValue::Register(id));
        return Ok(());
    }

    if let Some((value, base, disp)) = decode_str_immediate(word) {
        let base_value = ctx.read_gpr(base)?;
        let addr = pointer_add_immediate(base_value, disp, instructions, next_id)?;
        let stored = ctx.read_gpr(value)?;
        let id = *next_id;
        instructions.push(AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
            kind: AsmInstructionKind::Store {
                value: stored,
                address: addr,
                alignment: None,
                volatile: false,
            },
            type_hint: Some(AsmType::Void),
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        });
        *next_id += 1;
        return Ok(());
    }

    Err(Error::from(format!(
        "unsupported aarch64 instruction: 0x{word:08x}"
    )))
}

fn build_binop(id: u32, kind: AsmInstructionKind, opcode: AsmOpcode) -> AsmInstruction {
    AsmInstruction {
        id,
        opcode,
        kind,
        type_hint: Some(AsmType::I64),
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn decode_add_immediate(word: u32) -> Option<(u8, u8, i64)> {
    // ADD (immediate) 64-bit: sf=1, op=0, S=0, fixed 0b10001 at bits 28..24.
    if (word & 0x1F000000) != 0x11000000 {
        return None;
    }
    let sf = (word >> 31) & 1;
    let op = (word >> 30) & 1;
    let s = (word >> 29) & 1;
    if sf != 1 || op != 0 || s != 0 {
        return None;
    }
    let shift = (word >> 22) & 0x3;
    if shift != 0 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rd = (word & 0x1F) as u8;
    Some((rd, rn, imm12))
}

fn decode_sub_immediate(word: u32) -> Option<(u8, u8, i64)> {
    // SUB (immediate) 64-bit: sf=1, op=1, S=0, fixed 0b10001.
    if (word & 0x1F000000) != 0x11000000 {
        return None;
    }
    let sf = (word >> 31) & 1;
    let op = (word >> 30) & 1;
    let s = (word >> 29) & 1;
    if sf != 1 || op != 1 || s != 0 {
        return None;
    }
    let shift = (word >> 22) & 0x3;
    if shift != 0 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rd = (word & 0x1F) as u8;
    Some((rd, rn, imm12))
}

fn decode_ldr_immediate(word: u32) -> Option<(u8, u8, i64)> {
    // LDR Xt, [Xn, #imm] (unsigned immediate), 64-bit.
    if (word & 0xFFC00000) != 0xF9400000 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rt = (word & 0x1F) as u8;
    if rt == 31 {
        return None;
    }
    let disp = imm12 * 8;
    Some((rt, rn, disp))
}

fn decode_str_immediate(word: u32) -> Option<(u8, u8, i64)> {
    // STR Xt, [Xn, #imm] (unsigned immediate), 64-bit.
    if (word & 0xFFC00000) != 0xF9000000 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rt = (word & 0x1F) as u8;
    if rt == 31 {
        return None;
    }
    let disp = imm12 * 8;
    Some((rt, rn, disp))
}

struct RegisterLiftContext {
    locals: Vec<AsmLocal>,
    locals_by_register: std::collections::HashMap<u8, u32>,
    registers: std::collections::HashMap<u8, AsmValue>,
    next_local_id: u32,
}

impl RegisterLiftContext {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            locals_by_register: std::collections::HashMap::new(),
            registers: std::collections::HashMap::new(),
            next_local_id: 0,
        }
    }

    fn read_return_value(&mut self) -> Option<AsmValue> {
        self.registers.get(&0).cloned().or_else(|| {
            self.ensure_local(0, true);
            Some(AsmValue::Local(*self.locals_by_register.get(&0)?))
        })
    }

    fn read_gpr(&mut self, reg: u8) -> Result<AsmValue> {
        if let Some(value) = self.registers.get(&reg).cloned() {
            return Ok(value);
        }
        let is_argument = reg <= 7;
        self.ensure_local(reg, is_argument);
        let local_id = *self
            .locals_by_register
            .get(&reg)
            .ok_or_else(|| Error::from("missing local"))?;
        let value = AsmValue::Local(local_id);
        self.registers.insert(reg, value.clone());
        Ok(value)
    }

    fn write_gpr(&mut self, reg: u8, value: AsmValue) {
        self.registers.insert(reg, value);
    }

    fn ensure_local(&mut self, reg: u8, is_argument: bool) {
        if let Some(local_id) = self.locals_by_register.get(&reg).copied() {
            if is_argument {
                if let Some(local) = self.locals.iter_mut().find(|local| local.id == local_id) {
                    local.is_argument = true;
                }
            }
            return;
        }

        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.locals_by_register.insert(reg, local_id);
        self.locals.push(AsmLocal {
            id: local_id,
            ty: AsmType::I64,
            name: Some(match reg {
                31 => "sp".to_string(),
                _ => format!("x{reg}"),
            }),
            is_argument,
        });
    }
}

fn pointer_add_immediate(
    base: AsmValue,
    displacement: i64,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    if displacement == 0 {
        return Ok(base);
    }
    let rhs = AsmValue::Constant(AsmConstant::Int(displacement, AsmType::I64));
    let id = *next_id;
    let mut inst = build_binop(
        id,
        AsmInstructionKind::Add(base, rhs),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    );
    inst.annotations = synthesized_annotations("aarch64.addr");
    instructions.push(inst);
    *next_id += 1;
    Ok(AsmValue::Register(id))
}
