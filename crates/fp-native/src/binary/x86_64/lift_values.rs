use super::*;

pub(super) fn parse_st_register(token: &str) -> Result<u8> {
    let token = token.trim();
    if token == "st" || token == "st(0)" {
        return Ok(0);
    }
    let inner = token
        .strip_prefix("st(")
        .and_then(|rest| rest.strip_suffix(')'))
        .ok_or_else(|| Error::from(format!("expected st register, got: {token}")))?;
    inner
        .parse::<u8>()
        .map_err(|e| Error::from(format!("invalid st register: {token}: {e}")))
}

pub(super) fn read_u16(bytes: &[u8], index: usize) -> Result<u16> {
    let imm = bytes
        .get(index..index + 2)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(u16::from_le_bytes(imm.try_into().unwrap()))
}

pub(super) fn parse_capstone_operands(op_str: &str) -> Vec<&str> {
    op_str
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .collect()
}

pub(super) fn parse_capstone_memory_operand(op_str: &str) -> Result<X86Memory> {
    fn strip_ptr_qualifier(mut text: &str) -> &str {
        // Capstone uses Intel syntax and may prefix memory operands with size qualifiers.
        // We strip the common ones so the remaining string begins with `[`.
        loop {
            let lower = text.to_ascii_lowercase();
            let prefixes = [
                "byte ptr",
                "word ptr",
                "dword ptr",
                "qword ptr",
                "tbyte ptr",
                "xword ptr",
                "oword ptr",
                "xmmword ptr",
                "ymmword ptr",
                "zmmword ptr",
                "ptr",
            ];

            let Some(prefix) = prefixes.iter().find(|prefix| lower.starts_with(*prefix)) else {
                return text;
            };
            text = text[prefix.len()..].trim_start();
        }
    }

    fn parse_i64_token(token: &str) -> Result<i64> {
        let token = token.trim();
        let (sign, rest) = if let Some(rest) = token.strip_prefix('-') {
            (-1i64, rest)
        } else {
            (1i64, token)
        };

        let value = if let Some(rest) = rest.strip_prefix("0x") {
            i64::from_str_radix(rest, 16)
                .map_err(|e| Error::from(format!("invalid hex displacement: {token}: {e}")))?
        } else {
            rest.parse::<i64>()
                .map_err(|e| Error::from(format!("invalid displacement: {token}: {e}")))?
        };
        Ok(sign * value)
    }

    let text = strip_ptr_qualifier(op_str.trim());

    let (inner, segment) = if let Some((seg, rest)) = text.split_once(":") {
        (rest.trim(), Some(seg.trim()))
    } else {
        (text, None)
    };

    let segment = match segment {
        Some("fs") => Some(X86Segment::Fs),
        Some("gs") => Some(X86Segment::Gs),
        Some(_) => return Err(Error::from("unsupported x86 segment override")),
        None => None,
    };

    let inner = inner.trim();
    let normalized = inner.replace(' ', "");

    // AT&T style used by some Capstone configurations: `*0x123(%rip)`.
    if let Some((disp_text, rest)) = normalized.split_once("(%rip)") {
        if rest.is_empty() {
            let disp_text = disp_text.trim_start_matches('*');
            let disp = if disp_text.is_empty() {
                0
            } else {
                parse_i64_token(disp_text)?
            };
            return Ok(X86Memory {
                base: Some(16),
                index: None,
                scale: 1,
                displacement: disp,
                displacement_offset: Some(0),
                segment,
            });
        }
    }

    let bracketed = inner
        .strip_prefix('[')
        .and_then(|inner| inner.strip_suffix(']'))
        .ok_or_else(|| Error::from(format!("unsupported memory operand syntax: {op_str}")))?
        .trim();

    let normalized = bracketed.replace(' ', "");
    if let Some(rest) = normalized.strip_prefix("rip+") {
        let disp = parse_i64_token(rest)?;
        return Ok(X86Memory {
            base: Some(16),
            index: None,
            scale: 1,
            displacement: disp,
            displacement_offset: Some(0),
            segment,
        });
    }
    if let Some(rest) = normalized.strip_prefix("rip-") {
        let disp = parse_i64_token(rest)?;
        return Ok(X86Memory {
            base: Some(16),
            index: None,
            scale: 1,
            displacement: -disp,
            displacement_offset: Some(0),
            segment,
        });
    }

    // Generic base/index/disp parse (still intentionally conservative).
    // Examples:
    // - `rbp-0x20`
    // - `rsp+0x30`
    // - `rax+rcx*8+0x10`
    // - `r12+r13*2-0x8`
    let mut base: Option<u8> = None;
    let mut index: Option<u8> = None;
    let mut scale: u8 = 1;
    let mut displacement: i64 = 0;

    let expression = normalized.replace('-', "+-");
    for term in expression.split('+').filter(|term| !term.is_empty()) {
        if let Some((reg, scale_text)) = term.split_once('*') {
            let reg_id = parse_gpr_register(reg)?;
            if index.is_some() {
                return Err(Error::from(format!(
                    "unsupported indexed memory operand (multiple indices): {op_str}"
                )));
            }
            index = Some(reg_id);
            scale = scale_text
                .parse::<u8>()
                .map_err(|e| Error::from(format!("invalid index scale: {op_str}: {e}")))?;
            if !matches!(scale, 1 | 2 | 4 | 8) {
                return Err(Error::from(format!(
                    "unsupported index scale {scale} in memory operand: {op_str}"
                )));
            }
            continue;
        }

        if term.chars().next().is_some_and(|c| c.is_ascii_alphabetic()) {
            let reg_id = parse_gpr_register(term)?;
            if base.is_none() {
                base = Some(reg_id);
            } else if index.is_none() {
                index = Some(reg_id);
            } else {
                return Err(Error::from(format!(
                    "unsupported memory operand (too many registers): {op_str}"
                )));
            }
            continue;
        }

        displacement = displacement
            .checked_add(parse_i64_token(term)?)
            .ok_or_else(|| Error::from("x86_64 displacement overflow"))?;
    }

    Ok(X86Memory {
        base,
        index,
        scale,
        displacement,
        displacement_offset: None,
        segment,
    })
}

pub(super) fn x86_64_sysv_call_args(ctx: &mut RegisterLiftContext) -> Result<Vec<AsmValue>> {
    // SysV integer arguments: rdi, rsi, rdx, rcx, r8, r9.
    Ok(vec![
        ctx.read_gpr(7)?,
        ctx.read_gpr(6)?,
        ctx.read_gpr(2)?,
        ctx.read_gpr(1)?,
        ctx.read_gpr(8)?,
        ctx.read_gpr(9)?,
    ])
}

pub(super) fn value_from_operand_with_width(
    ctx: &mut RegisterLiftContext,
    operand: Operand,
    width_bits: u16,
    inst: DecodedInstruction,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        Operand::Imm(imm) => {
            if width_bits >= 64 {
                return Ok(AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64)));
            }
            let mask: i128 = (1i128 << width_bits) - 1;
            let truncated = (imm as i128) & mask;
            Ok(AsmValue::Constant(AsmConstant::UInt(
                truncated as u64,
                AsmType::I64,
            )))
        }
        Operand::Rm(rm) => {
            value_from_rm_with_width(ctx, rm, width_bits, inst, relocs, instructions, next_id)
        }
    }
}

pub(super) fn value_from_rm_with_width(
    ctx: &mut RegisterLiftContext,
    rm: RmOperand,
    width_bits: u16,
    inst: DecodedInstruction,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match rm {
        RmOperand::Reg(reg) => Ok(ctx.read_gpr(reg)?),
        RmOperand::Mem(memory) => {
            if memory.segment.is_some() {
                return Ok(AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)));
            }

            if width_bits == 64 {
                if let Some(displacement_offset) = memory.displacement_offset {
                    let relocation_offset = inst
                        .offset
                        .checked_add(displacement_offset as u64)
                        .ok_or_else(|| Error::from("x86_64 relocation offset overflow"))?;
                    if let Some(reloc) = relocation_at(relocs, relocation_offset) {
                        if reloc.is_got {
                            let id = *next_id;
                            instructions.push(AsmInstruction {
                                id,
                                opcode: AsmOpcode::Generic(
                                    fp_core::asmir::AsmGenericOpcode::SymbolAddress,
                                ),
                                kind: AsmInstructionKind::SymbolAddress {
                                    symbol: reloc.symbol.clone(),
                                    kind: fp_core::asmir::AsmSymbolAddressKind::Got,
                                },
                                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                                operands: Vec::new(),
                                implicit_uses: Vec::new(),
                                implicit_defs: Vec::new(),
                                encoding: None,
                                debug_info: None,
                                annotations: Vec::new(),
                            });
                            *next_id += 1;
                            return Ok(AsmValue::Register(id));
                        }
                    }
                }

                if let Some(symbol) = ctx.resolve_rip_symbol(&memory, inst.offset, inst.len) {
                    if symbol.is_got {
                        let id = *next_id;
                        let target = symbol.import.clone().unwrap_or_else(|| symbol.name.clone());
                        instructions.push(AsmInstruction {
                            id,
                            opcode: AsmOpcode::Generic(
                                fp_core::asmir::AsmGenericOpcode::SymbolAddress,
                            ),
                            kind: AsmInstructionKind::SymbolAddress {
                                symbol: target,
                                kind: fp_core::asmir::AsmSymbolAddressKind::Got,
                            },
                            ty: AsmType::Ptr(Box::new(AsmType::I8)),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        return Ok(AsmValue::Register(id));
                    }
                }

                if let Some(symbol) = ctx.resolve_disp32_symbol(&memory, inst.offset, inst.len) {
                    if symbol.is_got {
                        let id = *next_id;
                        let target = symbol.import.clone().unwrap_or_else(|| symbol.name.clone());
                        instructions.push(AsmInstruction {
                            id,
                            opcode: AsmOpcode::Generic(
                                fp_core::asmir::AsmGenericOpcode::SymbolAddress,
                            ),
                            kind: AsmInstructionKind::SymbolAddress {
                                symbol: target,
                                kind: fp_core::asmir::AsmSymbolAddressKind::Got,
                            },
                            ty: AsmType::Ptr(Box::new(AsmType::I8)),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        return Ok(AsmValue::Register(id));
                    }
                }
            }

            let addr = compute_address(
                ctx,
                memory,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: match width_bits {
                    8 => AsmType::I8,
                    16 => AsmType::I16,
                    32 => AsmType::I32,
                    _ => AsmType::I64,
                },
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile_init_store"),
            });
            *next_id += 1;

            if width_bits == 64 {
                return Ok(AsmValue::Register(load_id));
            }

            let zext_id = *next_id;
            instructions.push(AsmInstruction {
                id: zext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(load_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile_load"),
            });
            *next_id += 1;

            Ok(AsmValue::Register(zext_id))
        }
    }
}

pub(super) fn freeze_i64(
    value: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let id = *next_id;
    instructions.push(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
        kind: AsmInstructionKind::Freeze(value),
        ty: AsmType::I64,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    });
    *next_id += 1;
    AsmValue::Register(id)
}

pub(super) fn value_for_store(
    width_bits: u16,
    value: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    let target_ty = match width_bits {
        8 => AsmType::I8,
        16 => AsmType::I16,
        32 => AsmType::I32,
        64 => return Ok(value),
        _ => return Err(Error::from("unsupported x86_64 store width")),
    };

    let id = *next_id;
    instructions.push(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
        kind: AsmInstructionKind::Trunc(value, target_ty.clone()),
        ty: target_ty,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    });
    *next_id += 1;
    Ok(AsmValue::Register(id))
}

pub(super) fn byte_swap_value(
    width_bits: u16,
    value: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    let (mask, shift_bits) = match width_bits {
        16 => (0xFFFFu64, 8u16),
        32 => (0xFFFF_FFFFu64, 8u16),
        64 => (u64::MAX, 8u16),
        _ => return Err(Error::from("unsupported x86_64 movbe width")),
    };

    let value = freeze_i64(value, instructions, next_id);
    let masked = if mask == u64::MAX {
        value
    } else {
        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::And(
                value,
                AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
            ),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
        ));
        *next_id += 1;
        AsmValue::Register(id)
    };

    match width_bits {
        16 => {
            let shl_id = *next_id;
            instructions.push(build_binop(
                shl_id,
                AsmInstructionKind::Shl(
                    masked.clone(),
                    AsmValue::Constant(AsmConstant::UInt(shift_bits as u64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let shr_id = *next_id;
            instructions.push(build_binop(
                shr_id,
                AsmInstructionKind::Shr(
                    masked,
                    AsmValue::Constant(AsmConstant::UInt(shift_bits as u64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(shl_id), AsmValue::Register(shr_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let final_id = *next_id;
            instructions.push(build_binop(
                final_id,
                AsmInstructionKind::And(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            Ok(AsmValue::Register(final_id))
        }
        32 => {
            let left_mask = 0x00FF_00FFu64;
            let right_mask = 0xFF00_FF00u64;

            let left_id = *next_id;
            instructions.push(build_binop(
                left_id,
                AsmInstructionKind::And(
                    masked.clone(),
                    AsmValue::Constant(AsmConstant::UInt(left_mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let left_shift_id = *next_id;
            instructions.push(build_binop(
                left_shift_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(left_id),
                    AsmValue::Constant(AsmConstant::UInt(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let right_id = *next_id;
            instructions.push(build_binop(
                right_id,
                AsmInstructionKind::And(
                    masked.clone(),
                    AsmValue::Constant(AsmConstant::UInt(right_mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let right_shift_id = *next_id;
            instructions.push(build_binop(
                right_shift_id,
                AsmInstructionKind::Shr(
                    AsmValue::Register(right_id),
                    AsmValue::Constant(AsmConstant::UInt(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(left_shift_id),
                    AsmValue::Register(right_shift_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let lo_id = *next_id;
            instructions.push(build_binop(
                lo_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::UInt(16, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let hi_id = *next_id;
            instructions.push(build_binop(
                hi_id,
                AsmInstructionKind::Shr(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::UInt(16, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let final_or_id = *next_id;
            instructions.push(build_binop(
                final_or_id,
                AsmInstructionKind::Or(AsmValue::Register(lo_id), AsmValue::Register(hi_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let final_id = *next_id;
            instructions.push(build_binop(
                final_id,
                AsmInstructionKind::And(
                    AsmValue::Register(final_or_id),
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            Ok(AsmValue::Register(final_id))
        }
        64 => {
            let step1_left = 0x00FF_00FF_00FF_00FFu64;
            let step1_right = 0xFF00_FF00_FF00_FF00u64;
            let step2_left = 0x0000_FFFF_0000_FFFFu64;
            let step2_right = 0xFFFF_0000_FFFF_0000u64;

            let left_id = *next_id;
            instructions.push(build_binop(
                left_id,
                AsmInstructionKind::And(
                    masked.clone(),
                    AsmValue::Constant(AsmConstant::UInt(step1_left, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let left_shift_id = *next_id;
            instructions.push(build_binop(
                left_shift_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(left_id),
                    AsmValue::Constant(AsmConstant::UInt(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let right_id = *next_id;
            instructions.push(build_binop(
                right_id,
                AsmInstructionKind::And(
                    masked.clone(),
                    AsmValue::Constant(AsmConstant::UInt(step1_right, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let right_shift_id = *next_id;
            instructions.push(build_binop(
                right_shift_id,
                AsmInstructionKind::Shr(
                    AsmValue::Register(right_id),
                    AsmValue::Constant(AsmConstant::UInt(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let step1_id = *next_id;
            instructions.push(build_binop(
                step1_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(left_shift_id),
                    AsmValue::Register(right_shift_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let left2_id = *next_id;
            instructions.push(build_binop(
                left2_id,
                AsmInstructionKind::And(
                    AsmValue::Register(step1_id),
                    AsmValue::Constant(AsmConstant::UInt(step2_left, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let left2_shift_id = *next_id;
            instructions.push(build_binop(
                left2_shift_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(left2_id),
                    AsmValue::Constant(AsmConstant::UInt(16, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let right2_id = *next_id;
            instructions.push(build_binop(
                right2_id,
                AsmInstructionKind::And(
                    AsmValue::Register(step1_id),
                    AsmValue::Constant(AsmConstant::UInt(step2_right, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let right2_shift_id = *next_id;
            instructions.push(build_binop(
                right2_shift_id,
                AsmInstructionKind::Shr(
                    AsmValue::Register(right2_id),
                    AsmValue::Constant(AsmConstant::UInt(16, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let step2_id = *next_id;
            instructions.push(build_binop(
                step2_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(left2_shift_id),
                    AsmValue::Register(right2_shift_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let left3_id = *next_id;
            instructions.push(build_binop(
                left3_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(step2_id),
                    AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let right3_id = *next_id;
            instructions.push(build_binop(
                right3_id,
                AsmInstructionKind::Shr(
                    AsmValue::Register(step2_id),
                    AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let final_id = *next_id;
            instructions.push(build_binop(
                final_id,
                AsmInstructionKind::Or(AsmValue::Register(left3_id), AsmValue::Register(right3_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;
            Ok(AsmValue::Register(final_id))
        }
        _ => Err(Error::from("unsupported x86_64 movbe width")),
    }
}

pub(super) fn write_gpr_with_width(
    ctx: &mut RegisterLiftContext,
    dst: u8,
    value: AsmValue,
    width_bits: u16,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<()> {
    let value = freeze_i64(value, instructions, next_id);
    match width_bits {
        64 => {
            ctx.write_gpr(dst, value);
            Ok(())
        }
        32 => {
            let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::And(value, mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        16 | 8 => {
            let old = ctx.read_gpr(dst)?;
            let low_mask = if width_bits == 8 { 0xFF } else { 0xFFFF };
            let low_mask_value =
                AsmValue::Constant(AsmConstant::UInt(low_mask as u64, AsmType::I64));
            let high_mask_value =
                AsmValue::Constant(AsmConstant::UInt((!low_mask) as u64, AsmType::I64));

            let low_id = *next_id;
            instructions.push(build_binop(
                low_id,
                AsmInstructionKind::And(value, low_mask_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(old, high_mask_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let result_id = *next_id;
            instructions.push(build_binop(
                result_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(low_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            ctx.write_gpr(dst, AsmValue::Register(result_id));
            Ok(())
        }
        _ => Err(Error::from("unsupported x86_64 register write width")),
    }
}

pub(super) fn compare_instruction(
    id: u32,
    kind: AsmInstructionKind,
    opcode: fp_core::asmir::AsmGenericOpcode,
) -> AsmInstruction {
    AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(opcode),
        kind,
        ty: AsmType::I1,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

pub(super) fn value_from_operand(
    ctx: &mut RegisterLiftContext,
    operand: Operand,
    instruction_offset: u64,
    instruction_len: usize,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        Operand::Imm(value) => {
            if value >= 0 {
                let addr = value as u64;
                if let Some(symbol) = ctx.rip_symbols.get(&addr) {
                    if symbol.kind == RipSymbolKind::Function {
                        return Ok(AsmValue::Function(symbol.name.clone()));
                    }
                }
            }
            Ok(AsmValue::Constant(AsmConstant::Int(value, AsmType::I64)))
        }
        Operand::Rm(rm) => match rm {
            RmOperand::Reg(reg) => ctx.read_gpr(reg),
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    // Treat segment-based loads as stable zero for now.
                    return Ok(AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)));
                }

                if let Some(symbol) =
                    ctx.resolve_disp32_symbol(&memory, instruction_offset, instruction_len)
                {
                    if symbol.kind == RipSymbolKind::Function {
                        return Ok(AsmValue::Function(symbol.name.clone()));
                    }
                }
                let addr = compute_address(
                    ctx,
                    memory,
                    instruction_offset,
                    instruction_len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                    kind: AsmInstructionKind::Load {
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                Ok(AsmValue::Register(id))
            }
        },
    }
}

pub(super) fn patch_compare_kind(
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
    let (lhs, rhs) = compare_operands(&inst.kind)
        .ok_or_else(|| Error::from("comparison instruction has unexpected kind"))?;
    if let Ok((kind, opcode)) = compare_kind_from_condition(condition, lhs, rhs, compare.is_float) {
        inst.kind = kind;
        inst.opcode = AsmOpcode::Generic(opcode);
        inst.ty = AsmType::Void;
    }
    Ok(())
}

pub(super) fn compare_operands(kind: &AsmInstructionKind) -> Option<(AsmValue, AsmValue)> {
    match kind {
        AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => Some((lhs.clone(), rhs.clone())),
        _ => None,
    }
}

pub(super) fn compare_kind_from_condition(
    condition: u8,
    lhs: AsmValue,
    rhs: AsmValue,
    is_float: bool,
) -> Result<(AsmInstructionKind, fp_core::asmir::AsmGenericOpcode)> {
    Ok(match (condition, is_float) {
        (0x4, _) => (
            AsmInstructionKind::Eq(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Eq,
        ),
        (0x5, _) => (
            AsmInstructionKind::Ne(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ne,
        ),
        (0xC, _) => (
            AsmInstructionKind::Lt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Lt,
        ),
        (0xD, _) => (
            AsmInstructionKind::Ge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ge,
        ),
        (0xE, _) => (
            AsmInstructionKind::Le(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Le,
        ),
        (0xF, _) => (
            AsmInstructionKind::Gt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Gt,
        ),
        (0x2, true) => (
            AsmInstructionKind::Lt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Lt,
        ),
        (0x3, true) => (
            AsmInstructionKind::Ge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ge,
        ),
        (0x6, true) => (
            AsmInstructionKind::Le(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Le,
        ),
        (0x7, true) => (
            AsmInstructionKind::Gt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Gt,
        ),
        (0x2, false) => (
            AsmInstructionKind::Ult(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ult,
        ),
        (0x3, false) => (
            AsmInstructionKind::Uge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Uge,
        ),
        (0x6, false) => (
            AsmInstructionKind::Ule(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ule,
        ),
        (0x7, false) => (
            AsmInstructionKind::Ugt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ugt,
        ),
        // JS/JNS depend on the sign flag, which is not precisely modeled today.
        // Approximate them via the signed comparison result.
        (0x8, _) => (
            AsmInstructionKind::Lt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Lt,
        ),
        (0x9, _) => (
            AsmInstructionKind::Ge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ge,
        ),
        (other, _) => {
            return Err(Error::from(format!(
                "unsupported x86_64 conditional jump: 0x{other:02x}"
            )));
        }
    })
}
