use crate::binary::LiftedFunction;
use crate::binary::TextRelocation;
use crate::binary::cfg::wire_block_edges;
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmAttr, AsmConstant, AsmFunction, AsmGenericOpcode, AsmInstruction, AsmOpcode, AsmOperand,
    AsmRegister, AsmRegisterBank, AsmSyscallConvention, AsmType, AsmVirtualRegId, OperandAccess,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LastCompare {
    id: u32,
    index: usize,
    vreg: AsmVirtualRegId,
}

fn synthesized_annotations(reason: &str) -> Vec<fp_core::asmir::AsmAnnotation> {
    vec![fp_core::asmir::AsmAnnotation {
        key: "fp.synthesized".to_string(),
        value: reason.to_string(),
    }]
}

fn write_operand(reg: AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(reg),
        access: OperandAccess::Write,
    }
}

fn read_operand(reg: AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(reg),
        access: OperandAccess::Read,
    }
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

/// Builds a placeholder comparison instruction (`Eq lhs, rhs`) that is later
/// patched (see `patch_compare_kind`) to the real comparison opcode once the
/// following `b.cond`'s condition code is known. Returns the instruction and
/// the freshly allocated destination register (of `Flags` bank) that the
/// eventual conditional branch will read as its condition.
fn compare_instruction(
    id: u32,
    function: &mut AsmFunction,
    lhs: AsmOperand,
    rhs: AsmOperand,
) -> (AsmInstruction, AsmVirtualRegId) {
    let dest = function.alloc_virtual_register(AsmType::I1, AsmRegisterBank::Flags, 1);
    let inst = AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Eq),
        vec![write_operand(dest), lhs, rhs],
    );
    (inst, dest)
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
    if inst.operands.len() != 3 {
        return Err(Error::from(
            "comparison instruction has unexpected operand shape for patching",
        ));
    }
    inst.opcode = AsmOpcode::Generic(condition_opcode(condition)?);
    Ok(())
}

fn condition_opcode(condition: u8) -> Result<AsmGenericOpcode> {
    Ok(match condition {
        0 => AsmGenericOpcode::Eq,
        1 => AsmGenericOpcode::Ne,
        10 => AsmGenericOpcode::Ge,
        11 => AsmGenericOpcode::Lt,
        12 => AsmGenericOpcode::Gt,
        13 => AsmGenericOpcode::Le,
        2 => AsmGenericOpcode::Uge,
        3 => AsmGenericOpcode::Ult,
        8 => AsmGenericOpcode::Ugt,
        9 => AsmGenericOpcode::Ule,
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
    function: &mut AsmFunction,
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
                instructions.push(AsmInstruction::new(
                    nop_id,
                    AsmOpcode::Generic(AsmGenericOpcode::Nop),
                    Vec::new(),
                ));
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
                        condition: read_operand(compare.vreg),
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
                instructions.push(AsmInstruction::new(
                    id,
                    AsmOpcode::Generic(AsmGenericOpcode::Call),
                    vec![
                        AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::AAPCS)),
                        AsmOperand::Symbol(Name::new(reloc.symbol.clone())),
                    ],
                ));
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
                let symbol_const = AsmOperand::Constant(AsmConstant::GlobalRef(
                    Name::new(reloc.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let dest = function.alloc_virtual_register(
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmRegisterBank::General,
                    64,
                );
                let symbol_id = next_id;
                instructions.push(AsmInstruction::new(
                    symbol_id,
                    AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                    vec![write_operand(dest), symbol_const],
                ));
                next_id += 1;

                let mut value = read_operand(dest);
                if reloc.addend != 0 {
                    value = pointer_add_immediate(
                        value,
                        reloc.addend,
                        &mut instructions,
                        &mut next_id,
                        function,
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

                let dest = function.alloc_virtual_register(
                    AsmType::I64,
                    AsmRegisterBank::General,
                    64,
                );
                let id = next_id;
                let mut operands = vec![
                    write_operand(dest),
                    AsmOperand::Attr(AsmAttr::SyscallConvention(syscall_convention)),
                    number,
                ];
                operands.extend(args);
                instructions.push(AsmInstruction::new(
                    id,
                    AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    operands,
                ));
                next_id += 1;
                ctx.write_gpr(0, read_operand(dest));
                cursor += 4;
                continue;
            }

            if let Some((lhs, rhs)) = decode_cmp_register(word) {
                let lhs_value = ctx.read_gpr(lhs)?;
                let rhs_value = ctx.read_gpr(rhs)?;
                let id = next_id;
                let (inst, vreg) = compare_instruction(id, function, lhs_value, rhs_value);
                instructions.push(inst);
                next_id += 1;
                last_compare = Some(LastCompare {
                    id,
                    index: instructions.len() - 1,
                    vreg,
                });
                cursor += 4;
                continue;
            }

            if let Some((lhs, imm)) = decode_cmp_immediate(word) {
                let lhs_value = ctx.read_gpr(lhs)?;
                let rhs_value = AsmOperand::Constant(AsmConstant::Int(imm, AsmType::I64));
                let id = next_id;
                let (inst, vreg) = compare_instruction(id, function, lhs_value, rhs_value);
                instructions.push(inst);
                next_id += 1;
                last_compare = Some(LastCompare {
                    id,
                    index: instructions.len() - 1,
                    vreg,
                });
                cursor += 4;
                continue;
            }

            if let Some((dst, src, imm)) = decode_add_immediate(word) {
                if let Some(reloc) = relocation_at(relocs, cursor) {
                    let lhs = ctx.read_gpr(src)?;
                    let rhs = AsmOperand::Constant(AsmConstant::Int(
                        reloc.addend.saturating_add(imm),
                        AsmType::I64,
                    ));
                    let id = next_id;
                    let (inst, dest) =
                        build_binop(id, function, AsmGenericOpcode::Add, lhs, rhs);
                    instructions.push(inst);
                    next_id += 1;
                    ctx.write_gpr(dst, read_operand(dest));
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
                        function,
                    )?;
                    let dest = function.alloc_virtual_register(
                        AsmType::I64,
                        AsmRegisterBank::General,
                        64,
                    );
                    let id = next_id;
                    instructions.push(AsmInstruction::new(
                        id,
                        AsmOpcode::Generic(AsmGenericOpcode::Load),
                        vec![write_operand(dest), addr],
                    ));
                    next_id += 1;
                    ctx.write_gpr(dst, read_operand(dest));
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
                        function,
                    )?;
                    let stored = ctx.read_gpr(value)?;
                    let id = next_id;
                    instructions.push(AsmInstruction::new(
                        id,
                        AsmOpcode::Generic(AsmGenericOpcode::Store),
                        vec![stored, addr],
                    ));
                    next_id += 1;
                    cursor += 4;
                    continue;
                }
            }

            lift_instruction(word, &mut ctx, &mut instructions, &mut next_id, function)?;
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
    function: &mut AsmFunction,
) -> Result<()> {
    if let Some((dst, src, imm)) = decode_add_immediate(word) {
        let lhs = ctx.read_gpr(src)?;
        let rhs = AsmOperand::Constant(AsmConstant::Int(imm, AsmType::I64));
        let id = *next_id;
        let (mut inst, dest) = build_binop(id, function, AsmGenericOpcode::Add, lhs, rhs);
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
        ctx.write_gpr(dst, read_operand(dest));
        return Ok(());
    }

    if let Some((dst, src, imm)) = decode_sub_immediate(word) {
        let lhs = ctx.read_gpr(src)?;
        let rhs = AsmOperand::Constant(AsmConstant::Int(imm, AsmType::I64));
        let id = *next_id;
        let (mut inst, dest) = build_binop(id, function, AsmGenericOpcode::Sub, lhs, rhs);
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
        ctx.write_gpr(dst, read_operand(dest));
        return Ok(());
    }

    if let Some((dst, base, disp)) = decode_ldr_immediate(word) {
        let base_value = ctx.read_gpr(base)?;
        let addr = pointer_add_immediate(base_value, disp, instructions, next_id, function)?;
        let dest = function.alloc_virtual_register(AsmType::I64, AsmRegisterBank::General, 64);
        let id = *next_id;
        instructions.push(AsmInstruction::new(
            id,
            AsmOpcode::Generic(AsmGenericOpcode::Load),
            vec![write_operand(dest), addr],
        ));
        *next_id += 1;
        ctx.write_gpr(dst, read_operand(dest));
        return Ok(());
    }

    if let Some((value, base, disp)) = decode_str_immediate(word) {
        let base_value = ctx.read_gpr(base)?;
        let addr = pointer_add_immediate(base_value, disp, instructions, next_id, function)?;
        let stored = ctx.read_gpr(value)?;
        let id = *next_id;
        instructions.push(AsmInstruction::new(
            id,
            AsmOpcode::Generic(AsmGenericOpcode::Store),
            vec![stored, addr],
        ));
        *next_id += 1;
        return Ok(());
    }

    Err(Error::from(format!(
        "unsupported aarch64 instruction: 0x{word:08x}"
    )))
}

/// Builds a binary-op instruction (`opcode dest, lhs, rhs`), allocating a
/// fresh 64-bit general-purpose destination register. Every aarch64 GPR op
/// lifted here operates on 64-bit values, so the destination type is always
/// `I64`.
fn build_binop(
    id: u32,
    function: &mut AsmFunction,
    opcode: AsmGenericOpcode,
    lhs: AsmOperand,
    rhs: AsmOperand,
) -> (AsmInstruction, AsmVirtualRegId) {
    let dest = function.alloc_virtual_register(AsmType::I64, AsmRegisterBank::General, 64);
    let inst = AsmInstruction::new(
        id,
        AsmOpcode::Generic(opcode),
        vec![write_operand(dest), lhs, rhs],
    );
    (inst, dest)
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
    registers: std::collections::HashMap<u8, AsmOperand>,
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

    fn read_return_value(&mut self) -> Option<AsmOperand> {
        self.registers.get(&0).cloned().or_else(|| {
            self.ensure_local(0, true);
            Some(AsmOperand::Local(*self.locals_by_register.get(&0)?))
        })
    }

    fn read_gpr(&mut self, reg: u8) -> Result<AsmOperand> {
        if let Some(value) = self.registers.get(&reg).cloned() {
            return Ok(value);
        }
        let is_argument = reg <= 7;
        self.ensure_local(reg, is_argument);
        let local_id = *self
            .locals_by_register
            .get(&reg)
            .ok_or_else(|| Error::from("missing local"))?;
        let value = AsmOperand::Local(local_id);
        self.registers.insert(reg, value.clone());
        Ok(value)
    }

    fn write_gpr(&mut self, reg: u8, value: AsmOperand) {
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
    base: AsmOperand,
    displacement: i64,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    function: &mut AsmFunction,
) -> Result<AsmOperand> {
    if displacement == 0 {
        return Ok(base);
    }
    let rhs = AsmOperand::Constant(AsmConstant::Int(displacement, AsmType::I64));
    let id = *next_id;
    let (mut inst, dest) = build_binop(id, function, AsmGenericOpcode::Add, base, rhs);
    inst.annotations = synthesized_annotations("aarch64.addr");
    instructions.push(inst);
    *next_id += 1;
    Ok(read_operand(dest))
}
