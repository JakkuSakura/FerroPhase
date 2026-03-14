use crate::binary::cfg::wire_block_edges;
use crate::binary::{DataRegion, LiftedFunction, RipSymbol, RipSymbolKind, TextRelocation};
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmSyscallConvention, AsmType,
    AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};
use std::collections::HashMap;

pub fn lift_function_bytes(
    bytes: &[u8],
    relocs: &[TextRelocation],
    syscall_convention: Option<AsmSyscallConvention>,
) -> Result<LiftedFunction> {
    lift_function_bytes_with_symbols(
        bytes,
        relocs,
        syscall_convention,
        0,
        None,
        None,
        None,
        None,
        0,
    )
}

pub(super) fn lift_function_bytes_with_symbols(
    bytes: &[u8],
    relocs: &[TextRelocation],
    syscall_convention: Option<AsmSyscallConvention>,
    code_base_address: u64,
    rip_symbols: Option<&HashMap<u64, RipSymbol>>,
    rodata_cstrings: Option<&HashMap<String, String>>,
    rodata_cstrings_by_addr: Option<&HashMap<u64, String>>,
    data_regions: Option<&[DataRegion]>,
    entry_offset: u64,
) -> Result<LiftedFunction> {
    let decoded = decode_stream(bytes)?;
    let sorted_starts = determine_block_starts(&decoded, bytes.len() as u64, entry_offset)?;
    let offset_to_sorted_index = sorted_starts
        .iter()
        .enumerate()
        .map(|(idx, offset)| (*offset, idx))
        .collect::<std::collections::HashMap<_, _>>();

    // Codegen expects the first basic block to be the function entry.
    // Keep block boundaries in ascending address order, but assign IDs and
    // emit order with the entry block first.
    let mut block_starts = Vec::with_capacity(sorted_starts.len());
    block_starts.push(entry_offset);
    for &offset in &sorted_starts {
        if offset != entry_offset {
            block_starts.push(offset);
        }
    }

    let offset_to_block = block_starts
        .iter()
        .enumerate()
        .map(|(idx, offset)| (*offset, idx as u32))
        .collect::<std::collections::HashMap<_, _>>();

    let mut ctx = RegisterLiftContext::new(
        code_base_address,
        rip_symbols,
        rodata_cstrings,
        rodata_cstrings_by_addr,
        data_regions,
    );
    let mut next_id = 0u32;
    let mut basic_blocks = Vec::new();

    for (block_index, &block_offset) in block_starts.iter().enumerate() {
        let block_id = block_index as u32;
        let sorted_index = *offset_to_sorted_index
            .get(&block_offset)
            .ok_or_else(|| Error::from("missing block offset index"))?;
        let next_block_offset = sorted_starts
            .get(sorted_index + 1)
            .copied()
            .unwrap_or(bytes.len() as u64);
        let mut instructions = Vec::new();
        let mut terminated = false;
        let mut last_compare: Option<LastCompare> = None;

        let mut cursor = block_offset;
        while cursor < next_block_offset {
            let inst = decoded
                .iter()
                .find(|inst| inst.offset == cursor)
                .ok_or_else(|| Error::from("missing decoded instruction"))?;

            if is_terminator(&inst.kind) {
                let terminator = lift_terminator(
                    &mut ctx,
                    inst,
                    &mut instructions,
                    &mut next_id,
                    &offset_to_block,
                    &mut last_compare,
                )?;
                basic_blocks.push(fp_core::asmir::AsmBlock {
                    id: block_id,
                    label: None,
                    instructions: std::mem::take(&mut instructions),
                    terminator,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
                terminated = true;
                break;
            }

            lift_non_terminator(
                &mut ctx,
                inst,
                relocs,
                &mut instructions,
                &mut next_id,
                &mut last_compare,
                syscall_convention,
            )?;
            cursor = cursor
                .checked_add(inst.len as u64)
                .ok_or_else(|| Error::from("x86_64 lift overflow"))?;
        }

        if !terminated {
            let terminator = if let Some(&fallthrough_offset) =
                sorted_starts.get(sorted_index + 1)
            {
                let fallthrough = *offset_to_block
                    .get(&fallthrough_offset)
                    .ok_or_else(|| Error::from("missing fallthrough block"))?;
                fp_core::asmir::AsmTerminator::Br(fallthrough)
            } else {
                fp_core::asmir::AsmTerminator::Return(None)
            };
            basic_blocks.push(fp_core::asmir::AsmBlock {
                id: block_id,
                label: None,
                instructions,
                terminator,
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }
    }

    wire_block_edges(&mut basic_blocks);

    Ok(LiftedFunction {
        basic_blocks,
        locals: ctx.locals,
        direct_call_targets: ctx.direct_call_targets,
    })
}

fn packed_add_i32x2(
    lhs: AsmValue,
    rhs: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let lhs = freeze_i64(lhs, instructions, next_id);
    let rhs = freeze_i64(rhs, instructions, next_id);

    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
    let shift_32 = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

    let lhs_low_id = *next_id;
    instructions.push(build_binop(
        lhs_low_id,
        AsmInstructionKind::And(lhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_low_id = *next_id;
    instructions.push(build_binop(
        rhs_low_id,
        AsmInstructionKind::And(rhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let low_sum_id = *next_id;
    instructions.push(build_binop(
        low_sum_id,
        AsmInstructionKind::Add(AsmValue::Register(lhs_low_id), AsmValue::Register(rhs_low_id)),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    ));
    *next_id += 1;
    let low_masked_id = *next_id;
    instructions.push(build_binop(
        low_masked_id,
        AsmInstructionKind::And(AsmValue::Register(low_sum_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let lhs_hi_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_id,
        AsmInstructionKind::Shr(lhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;
    let rhs_hi_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_id,
        AsmInstructionKind::Shr(rhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;

    let hi_sum_id = *next_id;
    instructions.push(build_binop(
        hi_sum_id,
        AsmInstructionKind::Add(AsmValue::Register(lhs_hi_id), AsmValue::Register(rhs_hi_id)),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    ));
    *next_id += 1;
    let hi_masked_id = *next_id;
    instructions.push(build_binop(
        hi_masked_id,
        AsmInstructionKind::And(AsmValue::Register(hi_sum_id), mask),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let hi_shifted_id = *next_id;
    instructions.push(build_binop(
        hi_shifted_id,
        AsmInstructionKind::Shl(AsmValue::Register(hi_masked_id), shift_32),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
    ));
    *next_id += 1;

    let out_id = *next_id;
    instructions.push(build_binop(
        out_id,
        AsmInstructionKind::Or(
            AsmValue::Register(hi_shifted_id),
            AsmValue::Register(low_masked_id),
        ),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
    ));
    *next_id += 1;

    AsmValue::Register(out_id)
}

fn packed_umax_i32x2(
    lhs: AsmValue,
    rhs: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let lhs = freeze_i64(lhs, instructions, next_id);
    let rhs = freeze_i64(rhs, instructions, next_id);

    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
    let shift_32 = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

    let lhs_low_id = *next_id;
    instructions.push(build_binop(
        lhs_low_id,
        AsmInstructionKind::And(lhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_low_id = *next_id;
    instructions.push(build_binop(
        rhs_low_id,
        AsmInstructionKind::And(rhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let cmp_low_id = *next_id;
    instructions.push(compare_instruction(
        cmp_low_id,
        AsmInstructionKind::Ugt(AsmValue::Register(lhs_low_id), AsmValue::Register(rhs_low_id)),
        fp_core::asmir::AsmGenericOpcode::Ugt,
    ));
    *next_id += 1;
    let sel_low_id = *next_id;
    instructions.push(AsmInstruction {
        id: sel_low_id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
        kind: AsmInstructionKind::Select {
            condition: AsmValue::Register(cmp_low_id),
            if_true: AsmValue::Register(lhs_low_id),
            if_false: AsmValue::Register(rhs_low_id),
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

    let lhs_hi_shift_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_shift_id,
        AsmInstructionKind::Shr(lhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;
    let rhs_hi_shift_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_shift_id,
        AsmInstructionKind::Shr(rhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;

    let lhs_hi_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_id,
        AsmInstructionKind::And(AsmValue::Register(lhs_hi_shift_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_hi_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_id,
        AsmInstructionKind::And(AsmValue::Register(rhs_hi_shift_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let cmp_hi_id = *next_id;
    instructions.push(compare_instruction(
        cmp_hi_id,
        AsmInstructionKind::Ugt(AsmValue::Register(lhs_hi_id), AsmValue::Register(rhs_hi_id)),
        fp_core::asmir::AsmGenericOpcode::Ugt,
    ));
    *next_id += 1;
    let sel_hi_id = *next_id;
    instructions.push(AsmInstruction {
        id: sel_hi_id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
        kind: AsmInstructionKind::Select {
            condition: AsmValue::Register(cmp_hi_id),
            if_true: AsmValue::Register(lhs_hi_id),
            if_false: AsmValue::Register(rhs_hi_id),
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

    let hi_shifted_id = *next_id;
    instructions.push(build_binop(
        hi_shifted_id,
        AsmInstructionKind::Shl(AsmValue::Register(sel_hi_id), shift_32),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
    ));
    *next_id += 1;

    let out_id = *next_id;
    instructions.push(build_binop(
        out_id,
        AsmInstructionKind::Or(AsmValue::Register(hi_shifted_id), AsmValue::Register(sel_low_id)),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
    ));
    *next_id += 1;
    AsmValue::Register(out_id)
}

fn parse_st_register(token: &str) -> Result<u8> {
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
        .map_err(|_| Error::from(format!("invalid st register: {token}")))
}

fn read_u16(bytes: &[u8], index: usize) -> Result<u16> {
    let imm = bytes
        .get(index..index + 2)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(u16::from_le_bytes(imm.try_into().unwrap()))
}

fn parse_capstone_operands(op_str: &str) -> Vec<&str> {
    op_str
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .collect()
}

fn parse_capstone_memory_operand(op_str: &str) -> Result<X86Memory> {
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
                .map_err(|_| Error::from(format!("invalid hex displacement: {token}")))?
        } else {
            rest.parse::<i64>()
                .map_err(|_| Error::from(format!("invalid displacement: {token}")))?
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
                .map_err(|_| Error::from(format!("invalid index scale: {op_str}")))?;
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

fn x86_64_sysv_call_args(ctx: &mut RegisterLiftContext) -> Result<Vec<AsmValue>> {
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

fn value_from_operand_with_width(
    ctx: &mut RegisterLiftContext,
    operand: Operand,
    width_bits: u16,
    inst: DecodedInstruction,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        Operand::Imm(imm) => Ok(AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64))),
        Operand::Rm(rm) => value_from_rm_with_width(
            ctx,
            rm,
            width_bits,
            inst,
            relocs,
            instructions,
            next_id,
        ),
    }
}

fn value_from_rm_with_width(
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
                type_hint: Some(match width_bits {
                    8 => AsmType::I8,
                    16 => AsmType::I16,
                    32 => AsmType::I32,
                    _ => AsmType::I64,
                }),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
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
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            Ok(AsmValue::Register(zext_id))
        }
    }
}

fn freeze_i64(
    value: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let id = *next_id;
    instructions.push(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
        kind: AsmInstructionKind::Freeze(value),
        type_hint: Some(AsmType::I64),
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

fn value_for_store(
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
        type_hint: Some(target_ty),
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

fn byte_swap_value(
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

fn write_gpr_with_width(
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
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(low_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            ctx.write_gpr(dst, AsmValue::Register(result_id));
            Ok(())
        }
        _ => Err(Error::from("unsupported x86_64 register write width")),
    }
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
        type_hint: Some(AsmType::I1),
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn value_from_operand(
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
                    type_hint: Some(AsmType::I64),
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
    let (lhs, rhs) = compare_operands(&inst.kind)
        .ok_or_else(|| Error::from("comparison instruction has unexpected kind"))?;
    if let Ok((kind, opcode)) =
        compare_kind_from_condition(condition, lhs, rhs, compare.is_float)
    {
        inst.kind = kind;
        inst.opcode = AsmOpcode::Generic(opcode);
        inst.type_hint = None;
    }
    Ok(())
}

fn compare_operands(kind: &AsmInstructionKind) -> Option<(AsmValue, AsmValue)> {
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

fn compare_kind_from_condition(
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DecodedInstruction {
    offset: u64,
    len: usize,
    kind: Decoded,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LastCompare {
    id: u32,
    index: usize,
    is_float: bool,
}

fn parse_capstone_two_operands(op_str: &str) -> Result<(&str, &str)> {
    let (lhs, rhs) = op_str
        .split_once(',')
        .ok_or_else(|| Error::from("expected two capstone operands"))?;
    Ok((lhs.trim(), rhs.trim()))
}

fn parse_capstone_immediate(token: &str) -> Result<i64> {
    let token = token.trim();
    let (sign, rest) = if let Some(rest) = token.strip_prefix('-') {
        (-1i64, rest.trim())
    } else {
        (1i64, token)
    };

    let value = if let Some(rest) = rest.strip_prefix("0x") {
        i64::from_str_radix(rest, 16)
            .map_err(|_| Error::from(format!("invalid immediate: {token}")))?
    } else {
        rest.parse::<i64>()
            .map_err(|_| Error::from(format!("invalid immediate: {token}")))?
    };
    Ok(sign * value)
}

fn capstone_operand_width_bits(token: &str) -> Option<u16> {
    let lower = token.to_ascii_lowercase();
    if lower.contains("byte ptr") {
        return Some(8);
    }
    if lower.contains("tbyte ptr") || lower.contains("tword ptr") || lower.contains("xword ptr") {
        return Some(80);
    }
    if lower.contains("qword ptr") {
        return Some(64);
    }
    if lower.contains("dword ptr") {
        return Some(32);
    }
    if lower.contains("word ptr") {
        return Some(16);
    }

    let token = token.trim();
    let lower = token.to_ascii_lowercase();
    if matches!(
        lower.as_str(),
        "al" | "cl" | "dl" | "bl" | "spl" | "bpl" | "sil" | "dil"
    ) {
        return Some(8);
    }
    if matches!(lower.as_str(), "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di") {
        return Some(16);
    }
    if matches!(
        lower.as_str(),
        "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
    ) {
        return Some(32);
    }
    if matches!(lower.as_str(), "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi") {
        return Some(64);
    }
    if let Some(rest) = lower.strip_prefix('r') {
        if rest.ends_with('b') {
            return Some(8);
        }
        if rest.ends_with('w') {
            return Some(16);
        }
        if rest.ends_with('d') {
            return Some(32);
        }
        if rest.parse::<u8>().is_ok() {
            return Some(64);
        }
    }
    None
}

fn parse_xmm_register(token: &str) -> Result<u8> {
    let id = token
        .trim()
        .strip_prefix("xmm")
        .ok_or_else(|| Error::from(format!("expected xmm register, got: {token}")))?
        .parse::<u8>()
        .map_err(|_| Error::from(format!("invalid xmm register: {token}")))?;
    Ok(id)
}

fn parse_gpr_register(token: &str) -> Result<u8> {
    let token = token.trim();
    let normalized = token.trim_end_matches(['d', 'w', 'b']);
    let mapped = match normalized {
        "rax" | "eax" | "ax" | "al" => 0,
        "rcx" | "ecx" | "cx" | "cl" => 1,
        "rdx" | "edx" | "dx" | "dl" => 2,
        "rbx" | "ebx" | "bx" | "bl" => 3,
        "rsp" | "esp" | "sp" | "spl" => 4,
        "rbp" | "ebp" | "bp" | "bpl" => 5,
        "rsi" | "esi" | "si" | "sil" => 6,
        "rdi" | "edi" | "di" | "dil" => 7,
        _ => {
            if let Some(rest) = normalized.strip_prefix('r') {
                let id = rest
                    .parse::<u8>()
                    .map_err(|_| Error::from(format!("invalid gpr register: {token}")))?;
                if id >= 8 {
                    return Ok(id);
                }
            }
            return Err(Error::from(format!("unsupported gpr register: {token}")));
        }
    };
    Ok(mapped)
}

fn decode_stream(bytes: &[u8]) -> Result<Vec<DecodedInstruction>> {
    use capstone::prelude::*;
    use capstone::Syntax;

    let mut capstone = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .map_err(|err| Error::from(format!("failed to initialize capstone: {err}")))?;

    capstone
        .set_syntax(Syntax::Intel)
        .map_err(|err| Error::from(format!("failed to set capstone intel syntax: {err}")))?;

    let instructions = capstone
        .disasm_all(bytes, 0)
        .map_err(|err| Error::from(format!("failed to disassemble x86_64: {err}")))?;

    let mut decoded = Vec::with_capacity(instructions.len());
    for inst in instructions.iter() {
        let offset = inst.address();
        let len = inst.bytes().len();
        let offset_usize = usize::try_from(offset)
            .map_err(|_| Error::from("x86_64 instruction offset overflow"))?;
        let end = offset_usize
            .checked_add(len)
            .ok_or_else(|| Error::from("x86_64 instruction length overflow"))?;
        let slice = bytes
            .get(offset_usize..end)
            .ok_or_else(|| Error::from("x86_64 instruction slice out of bounds"))?;

        let decode_error = match decode_instruction(slice, offset) {
            Ok(Some((kind, consumed))) => {
                decoded.push(DecodedInstruction { offset, len, kind });
                if consumed != len {
                    return Err(Error::from(format!(
                        "x86_64 decode length mismatch at 0x{offset:x}: capstone={len} custom={consumed}"
                    )));
                }
                continue;
            }
            Ok(None) => None,
            Err(err) => Some(err),
        };

        let (kind, consumed) = {
                let mnemonic = inst.mnemonic().unwrap_or("<unknown>");
                let op_str = inst.op_str().unwrap_or("");
                if (op_str.contains("zmm") || op_str.contains("ymm")) && mnemonic.starts_with('v') {
                    // Many real-world x86_64 binaries ship multiple SIMD-optimized variants
                    // of helper routines (often behind CPUID dispatch). We currently lift a
                    // scalar subset, so treat unsupported wide-vector instructions as NOP to
                    // keep exploring the executable.
                    (Decoded::Nop, len)
                } else if mnemonic.starts_with('k') {
                    // AVX-512 mask register operations. Treat as NOP for now.
                    (Decoded::Nop, len)
                } else if op_str
                    .split(|c: char| !c.is_ascii_alphanumeric())
                    .any(|token| matches!(token, "ah" | "bh" | "ch" | "dh"))
                {
                    // High 8-bit registers require subregister modeling; skip for now.
                    (Decoded::Nop, len)
                } else if mnemonic == "movbe" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 movbe operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }

                    let (kind, width_bits) = if parts[0].contains('[') {
                        let memory = parse_capstone_memory_operand(parts[0])?;
                        let src = parts[1];
                        let width_bits = capstone_operand_width_bits(src).ok_or_else(|| {
                            Error::from(format!(
                                "unsupported movbe register width at 0x{offset:x}: {src}"
                            ))
                        })?;
                        let src = parse_gpr_register(src)?;
                        (
                            Decoded::MovbeMemFromReg {
                                dst: memory,
                                src,
                                width_bits,
                            },
                            width_bits,
                        )
                    } else if parts[1].contains('[') {
                        let dst = parts[0];
                        let memory = parse_capstone_memory_operand(parts[1])?;
                        let width_bits = capstone_operand_width_bits(dst).ok_or_else(|| {
                            Error::from(format!(
                                "unsupported movbe register width at 0x{offset:x}: {dst}"
                            ))
                        })?;
                        let dst = parse_gpr_register(dst)?;
                        (
                            Decoded::MovbeRegFromMem {
                                dst,
                                src: memory,
                                width_bits,
                            },
                            width_bits,
                        )
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 movbe form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    };

                    if !matches!(width_bits, 16 | 32 | 64) {
                        return Err(Error::from(format!(
                            "unsupported x86_64 movbe width at 0x{offset:x}: {width_bits}"
                        )));
                    }

                    (kind, len)
                } else if mnemonic == "bswap" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 bswap operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst_token = parts[0];
                    let width_bits = capstone_operand_width_bits(dst_token).unwrap_or(64);
                    if !matches!(width_bits, 32 | 64) {
                        return Err(Error::from(format!(
                            "unsupported x86_64 bswap width at 0x{offset:x}: {width_bits}"
                        )));
                    }
                    let dst = parse_gpr_register(dst_token)?;
                    (Decoded::Bswap { dst, width_bits }, len)
                } else if mnemonic == "vpbroadcastq" {
                    let (dst, src) = parse_capstone_two_operands(op_str)?;
                    let dst = parse_xmm_register(dst)?;
                    let src = parse_gpr_register(src)?;
                    (Decoded::Vpbroadcastq { dst, src }, len)
                } else if mnemonic == "vpxorq" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpxorq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[2])?;
                        (Decoded::VpxorqXmmMem { dst, lhs, rhs }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpxorq form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vptest" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let lhs = parse_xmm_register(lhs)?;
                    if rhs.contains('[') {
                        let rhs = parse_capstone_memory_operand(rhs)?;
                        (Decoded::VptestMem { lhs, rhs }, len)
                    } else {
                        let rhs = parse_xmm_register(rhs)?;
                        (Decoded::Vptest { lhs, rhs }, len)
                    }
                } else if matches!(mnemonic, "vpcmpeqd" | "pcmpeqd") {
                    let parts = parse_capstone_operands(op_str);
                    let (dst, lhs, rhs) = if parts.len() == 3 {
                        (
                            parse_xmm_register(parts[0])?,
                            parse_xmm_register(parts[1])?,
                            parse_xmm_register(parts[2])?,
                        )
                    } else if parts.len() == 2 {
                        let dst = parse_xmm_register(parts[0])?;
                        (dst, dst, parse_xmm_register(parts[1])?)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 pcmpeqd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    };

                    if lhs == rhs {
                        (Decoded::OnesXmm { dst }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 pcmpeqd form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vpalignr" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 4 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpalignr operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    let imm = parts[3]
                        .parse::<u8>()
                        .map_err(|_| Error::from("invalid vpalignr immediate"))?;
                    (Decoded::Vpalignr { dst, lhs, rhs, imm }, len)
                } else if matches!(mnemonic, "vpmaxsq" | "vpmaxuq") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpmaxsq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    if mnemonic == "vpmaxsq" {
                        (Decoded::Vpmaxsq { dst, lhs, rhs }, len)
                    } else {
                        (Decoded::Vpmaxuq { dst, lhs, rhs }, len)
                    }
                } else if mnemonic == "vpmaxud" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpmaxud operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    (Decoded::Vpmaxud { dst, lhs, rhs }, len)
                } else if mnemonic == "vpminuq" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpminuq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    (Decoded::Vpminuq { dst, lhs, rhs }, len)
                } else if mnemonic == "vpsubq" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpsubq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    (Decoded::Vpsubq { dst, lhs, rhs }, len)
                } else if mnemonic == "vpaddd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpaddd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    (Decoded::Vpaddd { dst, lhs, rhs }, len)
                } else if mnemonic == "vpaddq" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpaddq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    (Decoded::Vpaddq { dst, lhs, rhs }, len)
                } else if mnemonic == "vpsrldq" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpsrldq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let src = parse_xmm_register(parts[1])?;
                    let imm = parts[2]
                        .parse::<u8>()
                        .map_err(|_| Error::from("invalid vpsrldq immediate"))?;
                    (Decoded::Vpsrldq { dst, src, imm }, len)
                } else if matches!(mnemonic, "vpandq" | "vporq") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 {mnemonic} operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    if mnemonic == "vpandq" {
                        (Decoded::Vpandq { dst, lhs, rhs }, len)
                    } else {
                        (Decoded::Vporq { dst, lhs, rhs }, len)
                    }
                } else if matches!(mnemonic, "vpunpcklwd" | "vpunpckldq" | "vpunpcklqdq") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpunpcklwd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = if parts[2].contains('[') {
                        VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        VecOperand::Reg(parse_xmm_register(parts[2])?)
                    };
                    if mnemonic == "vpunpcklwd" {
                        (Decoded::Vpunpcklwd { dst, lhs, rhs }, len)
                    } else if mnemonic == "vpunpckldq" {
                        (Decoded::Vpunpckldq { dst, lhs, rhs }, len)
                    } else {
                        (Decoded::Vpunpcklqdq { dst, lhs, rhs }, len)
                    }
                } else if mnemonic == "vpinsrd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 4 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpinsrd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let vector = parse_xmm_register(parts[1])?;
                    let value = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    let lane = parts[3]
                        .parse::<u8>()
                        .map_err(|_| Error::from("invalid vpinsrd lane immediate"))?;
                    (Decoded::Pinsrd {
                        dst,
                        vector,
                        value,
                        lane,
                    }, len)
                } else if mnemonic == "vpinsrb" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 4 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vpinsrb operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let vector = parse_xmm_register(parts[1])?;
                    let value = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    let lane = parts[3]
                        .parse::<u8>()
                        .map_err(|_| Error::from("invalid vpinsrb lane immediate"))?;
                    (Decoded::Pinsrb {
                        dst,
                        vector,
                        value,
                        lane,
                    }, len)
                } else if matches!(mnemonic, "vmovd" | "movd") {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    if lhs.starts_with("xmm") {
                        let dst = parse_xmm_register(lhs)?;
                        if rhs.contains('[') {
                            let src = parse_capstone_memory_operand(rhs)?;
                            (Decoded::MovdXmmFromMem32 { dst, src }, len)
                        } else {
                            let src = parse_gpr_register(rhs)?;
                            (Decoded::MovdXmmFromGpr32 { dst, src }, len)
                        }
                    } else if rhs.starts_with("xmm") {
                        let src = parse_xmm_register(rhs)?;
                        if lhs.contains('[') {
                            let dst = parse_capstone_memory_operand(lhs)?;
                            (Decoded::MovdMem32FromXmm { dst, src }, len)
                        } else {
                            let dst = parse_gpr_register(lhs)?;
                            let width_bits = capstone_operand_width_bits(lhs).unwrap_or(32);
                            (Decoded::MovdGpr32FromXmm { dst, src, width_bits }, len)
                        }
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmovd form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if matches!(mnemonic, "vpxor" | "vxorps" | "vxorpd") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vxor operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    let rhs = parse_xmm_register(parts[2])?;
                    if lhs == rhs {
                        (Decoded::ZeroXmm { dst }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vxor form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vcvtusi2sd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vcvtusi2sd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let src_vec = parse_xmm_register(parts[1])?;
                    let src_gpr = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    let width_bits = capstone_operand_width_bits(parts[2]).unwrap_or(64);
                    (Decoded::Vcvtusi2sd {
                        dst,
                        src_vec,
                        src_gpr,
                        width_bits,
                    }, len)
                } else if mnemonic == "vcvtusi2ss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vcvtusi2ss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let src_vec = parse_xmm_register(parts[1])?;
                    let src_gpr = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    let width_bits = capstone_operand_width_bits(parts[2]).unwrap_or(64);
                    (Decoded::Vcvtusi2ss {
                        dst,
                        src_vec,
                        src_gpr,
                        width_bits,
                    }, len)
                } else if mnemonic == "vmulsd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmulsd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[2])?;
                        (Decoded::VmulsdMem { dst, lhs, rhs }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmulsd form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vdivsd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vdivsd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vdivsd memory form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let rhs = parse_xmm_register(parts[2])?;
                    (Decoded::Vdivsd { dst, lhs, rhs }, len)
                } else if mnemonic == "vmovups" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmovups operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    if parts[0].contains('[') {
                        let dst = parse_capstone_memory_operand(parts[0])?;
                        let src = parse_xmm_register(parts[1])?;
                        (Decoded::VmovupsStore { dst, src }, len)
                    } else if parts[1].contains('[') {
                        let dst = parse_xmm_register(parts[0])?;
                        let src = parse_capstone_memory_operand(parts[1])?;
                        (Decoded::VmovupsLoad { dst, src }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmovups form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vmovss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmovss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    if parts[0].contains('[') {
                        let dst = parse_capstone_memory_operand(parts[0])?;
                        let src = parse_xmm_register(parts[1])?;
                        (Decoded::VmovssStore { dst, src }, len)
                    } else if parts[1].contains('[') {
                        let dst = parse_xmm_register(parts[0])?;
                        let src = parse_capstone_memory_operand(parts[1])?;
                        (Decoded::VmovssLoad { dst, src }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmovss form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vcomiss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vcomiss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let lhs = parse_xmm_register(parts[0])?;
                    if parts[1].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[1])?;
                        (Decoded::VcomissMem { lhs, rhs }, len)
                    } else {
                        let rhs = parse_xmm_register(parts[1])?;
                        (Decoded::VcomissReg { lhs, rhs }, len)
                    }
                } else if mnemonic == "vaddss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vaddss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[2])?;
                        (Decoded::VaddssMem { dst, lhs, rhs }, len)
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vaddss form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vdivss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vdivss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[2])?;
                        (Decoded::VdivssMem { dst, lhs, rhs }, len)
                    }
                    else {
                        let rhs = parse_xmm_register(parts[2])?;
                        (Decoded::Vdivss { dst, lhs, rhs }, len)
                    }
                } else if mnemonic == "vcvttss2usi" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vcvttss2usi operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_gpr_register(parts[0])?;
                    let src = parse_xmm_register(parts[1])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                    (Decoded::Vcvttss2usi { dst, src, width_bits }, len)
                } else if mnemonic == "vmulss" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 vmulss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let lhs = parse_xmm_register(parts[1])?;
                    if parts[2].contains('[') {
                        let rhs = parse_capstone_memory_operand(parts[2])?;
                        (Decoded::VmulssMem { dst, lhs, rhs }, len)
                    }
                    else {
                        let rhs = parse_xmm_register(parts[2])?;
                        (Decoded::Vmulss { dst, lhs, rhs }, len)
                    }
                } else if matches!(mnemonic, "vpinsrq" | "pinsrq") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 4 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 pinsrq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_xmm_register(parts[0])?;
                    let vector = parse_xmm_register(parts[1])?;
                    let value = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    let lane = parts[3]
                        .parse::<u8>()
                        .map_err(|_| Error::from("invalid pinsrq lane immediate"))?;
                    (Decoded::Pinsrq {
                        dst,
                        vector,
                        value,
                        lane,
                    }, len)
                } else if matches!(mnemonic, "vpextrq" | "pextrq") {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 pextrq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_gpr_register(parts[0])?;
                    let src = parse_xmm_register(parts[1])?;
                    let lane = parse_capstone_immediate(parts[2])?;
                    if !(0..=1).contains(&lane) {
                        return Err(Error::from("invalid pextrq lane immediate"));
                    }
                    (Decoded::Pextrq {
                        dst,
                        src,
                        lane: lane as u8,
                    }, len)
                } else if matches!(mnemonic, "movq" | "vmovq") {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    if lhs.starts_with("xmm") {
                        let dst = parse_xmm_register(lhs)?;
                        if rhs.contains('[') {
                            let src = parse_capstone_memory_operand(rhs)?;
                            (Decoded::MovqXmmFromMem { dst, src }, len)
                        } else {
                            let src = parse_gpr_register(rhs)?;
                            (Decoded::MovqXmmFromGpr { dst, src }, len)
                        }
                    } else if rhs.starts_with("xmm") {
                        let src = parse_xmm_register(rhs)?;
                        if lhs.contains('[') {
                            let dst = parse_capstone_memory_operand(lhs)?;
                            (Decoded::MovqMemFromXmm { dst, src }, len)
                        } else {
                            let dst = parse_gpr_register(lhs)?;
                            (Decoded::MovqGprFromXmm { dst, src }, len)
                        }
                    } else {
                        return Err(Error::from(format!(
                            "unsupported x86_64 movq form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                } else if mnemonic == "vzeroupper"
                    || mnemonic.starts_with("vmovaps")
                    || mnemonic.starts_with("vmovdqa")
                    || mnemonic.starts_with("vmovdqu")
                {
                    // TODO: Lift SIMD moves/spills properly.
                    (Decoded::Nop, len)
                } else if mnemonic == "cmp" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);

                    let lhs_operand = if lhs.contains('[') {
                        Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(lhs)?))
                    } else {
                        Operand::Rm(RmOperand::Reg(parse_gpr_register(lhs)?))
                    };

                    let rhs_operand = if rhs.contains('[') {
                        Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(rhs)?))
                    } else if rhs.starts_with("0x")
                        || rhs.starts_with('-')
                        || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                    {
                        Operand::Imm(parse_capstone_immediate(rhs)?)
                    } else {
                        Operand::Rm(RmOperand::Reg(parse_gpr_register(rhs)?))
                    };

                    (
                        Decoded::Cmp {
                            lhs: lhs_operand,
                            rhs: rhs_operand,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "test" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);

                    let lhs_operand = if lhs.contains('[') {
                        Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(lhs)?))
                    } else {
                        Operand::Rm(RmOperand::Reg(parse_gpr_register(lhs)?))
                    };

                    let rhs_operand = if rhs.contains('[') {
                        Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(rhs)?))
                    } else if rhs.starts_with("0x")
                        || rhs.starts_with('-')
                        || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                    {
                        Operand::Imm(parse_capstone_immediate(rhs)?)
                    } else {
                        Operand::Rm(RmOperand::Reg(parse_gpr_register(rhs)?))
                    };

                    (
                        Decoded::Test {
                            lhs: lhs_operand,
                            rhs: rhs_operand,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "bt" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    if lhs.contains('[') || rhs.contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 bt memory form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let value = parse_gpr_register(lhs)?;
                    if rhs.starts_with("0x") || rhs.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        let imm = parse_capstone_immediate(rhs)?;
                        let imm = u8::try_from(imm)
                            .map_err(|_| Error::from("unsupported bt immediate"))?;
                        (Decoded::BtImm { value, imm }, len)
                    } else {
                        let bit = parse_gpr_register(rhs)?;
                        (Decoded::BtReg { value, bit }, len)
                    }
                } else if mnemonic == "btc" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs).unwrap_or(64);
                    let dst = if lhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(lhs)?)
                    };
                    let imm = parse_capstone_immediate(rhs)?;
                    let imm = u8::try_from(imm).map_err(|_| Error::from("unsupported btc immediate"))?;
                    (Decoded::BtcImm { dst, imm, width_bits }, len)
                } else if mnemonic == "cqo" {
                    (Decoded::Cqo, len)
                } else if mnemonic == "cdq" {
                    (Decoded::Cdq, len)
                } else if mnemonic == "cdqe" {
                    (Decoded::Cdqe, len)
                } else if matches!(mnemonic, "shl" | "sal" | "shr" | "sar") {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    let dst = if lhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(lhs)?)
                    };
                    let imm = parse_capstone_immediate(rhs)?;
                    let imm = u8::try_from(imm)
                        .map_err(|_| Error::from("unsupported x86_64 shift immediate"))?;
                    if matches!(mnemonic, "shl" | "sal") {
                        (
                            Decoded::ShlImm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    } else if mnemonic == "sar" {
                        (
                            Decoded::SarImm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    } else {
                        (
                            Decoded::ShrImm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    }
                } else if mnemonic == "shrx" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 shrx form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(parts[0])
                        .or_else(|| capstone_operand_width_bits(parts[1]))
                        .unwrap_or(64);
                    let dst = parse_gpr_register(parts[0])?;
                    let src = if parts[1].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[1])?)
                    };
                    let shift = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    (
                        Decoded::Shrx {
                            dst,
                            src,
                            shift,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "shlx" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 shlx form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(parts[0])
                        .or_else(|| capstone_operand_width_bits(parts[1]))
                        .unwrap_or(64);
                    let dst = parse_gpr_register(parts[0])?;
                    let src = if parts[1].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[1])?)
                    };
                    let shift = if parts[2].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[2])?)
                    };
                    (
                        Decoded::Shlx {
                            dst,
                            src,
                            shift,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "rorx" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 3 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 rorx form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(parts[0])
                        .or_else(|| capstone_operand_width_bits(parts[1]))
                        .unwrap_or(64);
                    if !matches!(width_bits, 32 | 64) {
                        return Err(Error::from(format!(
                            "unsupported x86_64 rorx width at 0x{offset:x}: {width_bits}"
                        )));
                    }
                    let dst = parse_gpr_register(parts[0])?;
                    let src = if parts[1].contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(parts[1])?)
                    };
                    let imm = parse_capstone_immediate(parts[2])?;
                    let imm = u16::try_from(imm)
                        .map_err(|_| Error::from("unsupported rorx immediate"))?;
                    (
                        Decoded::Rorx {
                            dst,
                            src,
                            imm,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "blsr" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    if lhs.contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 blsr destination at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_gpr_register(lhs)?;
                    let src = if rhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(rhs)?)
                    };
                    (
                        Decoded::Blsr {
                            dst,
                            src,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "not" {
                    let operand = op_str.trim();
                    if operand.is_empty() {
                        return Err(Error::from(format!(
                            "unsupported x86_64 not form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(operand).unwrap_or(64);
                    let dst = if operand.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(operand)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(operand)?)
                    };
                    (
                        Decoded::NotRm {
                            dst,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "neg" {
                    let operand = op_str.trim();
                    if operand.is_empty() {
                        return Err(Error::from(format!(
                            "unsupported x86_64 neg form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(operand).unwrap_or(64);
                    let dst = if operand.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(operand)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(operand)?)
                    };
                    (
                        Decoded::NegRm {
                            dst,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "sbb" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    if lhs.contains('[') || rhs.contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 sbb memory form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_gpr_register(lhs)?;
                    let src = parse_gpr_register(rhs)?;
                    if dst != src {
                        return Err(Error::from(format!(
                            "unsupported x86_64 sbb form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    (Decoded::SbbSelf { reg: dst, width_bits }, len)
                } else if mnemonic == "sub" {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    if lhs.contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 sub memory-destination form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_gpr_register(lhs)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    let src = if rhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(rhs)?)
                    };
                    (Decoded::SubRegRmWidth { dst, src, width_bits }, len)
                } else if matches!(mnemonic, "or" | "and" | "xor") {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    let dst = if lhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(lhs)?)
                    };
                    if rhs.starts_with("0x")
                        || rhs.starts_with('-')
                        || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                    {
                        let imm = parse_capstone_immediate(rhs)?;
                        if mnemonic == "or" {
                            (
                                Decoded::OrImmRm {
                                    dst,
                                    imm,
                                    width_bits,
                                },
                                len,
                            )
                        } else if mnemonic == "and" {
                            (
                                Decoded::AndImmRm {
                                    dst,
                                    imm,
                                    width_bits,
                                },
                                len,
                            )
                        } else {
                            (
                                Decoded::XorImm {
                                    dst,
                                    imm,
                                    width_bits,
                                },
                                len,
                            )
                        }
                    } else if rhs.contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 {mnemonic} memory-source form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    } else {
                        let src = parse_gpr_register(rhs)?;
                        match (mnemonic, dst) {
                            ("or", RmOperand::Reg(dst)) => {
                                (Decoded::OrReg { dst, src, width_bits }, len)
                            }
                            ("and", RmOperand::Reg(dst)) => {
                                (Decoded::AndReg { dst, src, width_bits }, len)
                            }
                            ("xor", RmOperand::Reg(dst)) => {
                                (Decoded::XorReg { dst, src, width_bits }, len)
                            }
                            ("or", dst) => (Decoded::OrRmReg { dst, src, width_bits }, len),
                            ("and", dst) => (Decoded::AndRmReg { dst, src, width_bits }, len),
                            _ => {
                                return Err(Error::from(format!(
                                    "unsupported x86_64 xor destination at 0x{offset:x}: {mnemonic} {op_str}"
                                )));
                            }
                        }
                    }
                } else if mnemonic == "imul" {
                    let parts = parse_capstone_operands(op_str);
                    match parts.len() {
                        1 => {
                            let src_text = parts[0];
                            let width_bits = capstone_operand_width_bits(src_text).unwrap_or(64);
                            let src = if src_text.contains('[') {
                                RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                            } else {
                                RmOperand::Reg(parse_gpr_register(src_text)?)
                            };
                            (Decoded::ImulRmWide { src, width_bits }, len)
                        }
                        2 => {
                            let lhs = parts[0];
                            let rhs = parts[1];
                            if lhs.contains('[') || rhs.contains('[') {
                                return Err(Error::from(format!(
                                    "unsupported x86_64 imul memory form at 0x{offset:x}: {mnemonic} {op_str}"
                                )));
                            }
                            let dst = parse_gpr_register(lhs)?;
                            let src = parse_gpr_register(rhs)?;
                            let width_bits = capstone_operand_width_bits(lhs)
                                .or_else(|| capstone_operand_width_bits(rhs))
                                .unwrap_or(64);
                            (Decoded::ImulReg { dst, src, width_bits }, len)
                        }
                        3 => {
                            let dst_text = parts[0];
                            let src_text = parts[1];
                            let imm_text = parts[2];
                            if dst_text.contains('[') {
                                return Err(Error::from(format!(
                                    "unsupported x86_64 imul memory form at 0x{offset:x}: {mnemonic} {op_str}"
                                )));
                            }
                            let dst = parse_gpr_register(dst_text)?;
                            let src = if src_text.contains('[') {
                                RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                            } else {
                                RmOperand::Reg(parse_gpr_register(src_text)?)
                            };
                            let imm = parse_capstone_immediate(imm_text)?;
                            let width_bits = capstone_operand_width_bits(dst_text)
                                .or_else(|| capstone_operand_width_bits(src_text))
                                .unwrap_or(64);
                            (
                                Decoded::ImulRegImm {
                                    dst,
                                    src,
                                    imm,
                                    width_bits,
                                },
                                len,
                            )
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 imul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    }
                } else if mnemonic == "mul" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 mul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let src_text = parts[0];
                    let width_bits = capstone_operand_width_bits(src_text).unwrap_or(64);
                    let src = if src_text.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(src_text)?)
                    };
                    (Decoded::MulRm { src, width_bits }, len)
                } else if mnemonic == "fild" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 || !parts[0].contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fild form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let src = parse_capstone_memory_operand(parts[0])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                    (Decoded::Fild { src, width_bits }, len)
                } else if mnemonic == "fld" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fld operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    if parts[0].contains('[') {
                        let src = parse_capstone_memory_operand(parts[0])?;
                        let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(80);
                        (Decoded::FldMem { src, width_bits }, len)
                    } else {
                        let index = parse_st_register(parts[0])?;
                        (Decoded::FldSt { index }, len)
                    }
                } else if mnemonic == "fxch" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fxch operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let index = parse_st_register(parts[0])?;
                    (Decoded::Fxch { index }, len)
                } else if mnemonic == "fdivrp" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fdivrp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let a = parse_st_register(parts[0])?;
                    let b = parse_st_register(parts[1])?;
                    let index = if a != 0 { a } else { b };
                    (Decoded::Fdivrp { index }, len)
                } else if mnemonic == "fdivp" {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fdivp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::Fdivp { index }, len)
                } else if mnemonic == "fmulp" {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fmulp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::Fmulp { index }, len)
                } else if mnemonic == "fmul" {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fmul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::FmulSt0St { index }, len)
                } else if mnemonic == "fcomi" {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fcomi operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::Fcomi { index }, len)
                } else if mnemonic == "fcomip"
                    || mnemonic == "fcompi"
                    || mnemonic == "fucomip"
                    || mnemonic == "fucompi"
                {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fcomip operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::Fcomip { index }, len)
                } else if mnemonic == "fstp" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fstp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    if parts[0].contains('[') {
                        let dst = parse_capstone_memory_operand(parts[0])?;
                        let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                        (Decoded::FstpMem { dst, width_bits }, len)
                    } else {
                        let index = parse_st_register(parts[0])?;
                        (Decoded::FstpSt { index }, len)
                    }
                } else if mnemonic == "fisttp" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 || !parts[0].contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fisttp form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_capstone_memory_operand(parts[0])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                    (Decoded::Fisttp { dst, width_bits }, len)
                } else if let Some(condition) = mnemonic.strip_prefix("fcmov").and_then(|suffix| {
                    Some(match suffix {
                        "b" | "c" | "nae" => 0x2,
                        "ae" | "nb" | "nc" => 0x3,
                        "be" | "na" => 0x6,
                        "a" | "nbe" => 0x7,
                        "e" | "z" => 0x4,
                        "ne" | "nz" => 0x5,
                        _ => return None,
                    })
                }) {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 2 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fcmov operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let dst = parse_st_register(parts[0])?;
                    if dst != 0 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fcmov dst at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let src = parse_st_register(parts[1])?;
                    (
                        Decoded::Fcmovcc {
                            condition,
                            src,
                        },
                        len,
                    )
                } else if mnemonic == "fadd" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 || !parts[0].contains('[') {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fadd form at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let src = parse_capstone_memory_operand(parts[0])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                    (Decoded::FaddMem { src, width_bits }, len)
                } else if mnemonic == "ffreep" {
                    let parts = parse_capstone_operands(op_str);
                    if parts.len() != 1 {
                        return Err(Error::from(format!(
                            "unsupported x86_64 ffreep operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                    let index = parse_st_register(parts[0])?;
                    (Decoded::Ffreep { index }, len)
                } else if mnemonic == "fsubr" {
                    let parts = parse_capstone_operands(op_str);
                    let index = match parts.as_slice() {
                        [single] => parse_st_register(single)?,
                        [a, b] => {
                            let a = parse_st_register(a)?;
                            let b = parse_st_register(b)?;
                            if a != 0 { a } else { b }
                        }
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 fsubr operand count at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    };
                    (Decoded::FsubrSt0St { index }, len)
                } else if let Some(condition) = mnemonic.strip_prefix("cmov").and_then(|suffix| {
                    Some(match suffix {
                        "e" | "z" => 0x4,
                        "ne" | "nz" => 0x5,
                        "b" | "c" | "nae" => 0x2,
                        "ae" | "nb" | "nc" => 0x3,
                        "be" | "na" => 0x6,
                        "a" | "nbe" => 0x7,
                        // `s/ns` are based on the sign flag. We approximate them as
                        // `lt/ge` (valid for common patterns where OF=0, e.g. `test`).
                        "s" => 0xC,
                        "ns" => 0xD,
                        "l" => 0xC,
                        "ge" => 0xD,
                        "le" => 0xE,
                        "g" => 0xF,
                        _ => return None,
                    })
                }) {
                    let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                    let dst = parse_gpr_register(lhs)?;
                    let src = if rhs.contains('[') {
                        RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                    } else {
                        RmOperand::Reg(parse_gpr_register(rhs)?)
                    };
                    let width_bits = capstone_operand_width_bits(lhs)
                        .or_else(|| capstone_operand_width_bits(rhs))
                        .unwrap_or(64);
                    (
                        Decoded::Cmovcc {
                            dst,
                            src,
                            condition,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "hlt" {
                    (Decoded::Hlt, len)
                } else if mnemonic == "leave" {
                    (Decoded::Leave, len)
                } else {
                    if let Some(err) = decode_error {
                        return Err(Error::from(format!(
                            "unsupported x86_64 instruction at 0x{offset:x}: {mnemonic} {op_str} (custom decode failed: {err})"
                        )));
                    }
                    return Err(Error::from(format!(
                        "unsupported x86_64 instruction at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
        };
        if consumed != len {
            return Err(Error::from(format!(
                "x86_64 decode length mismatch at 0x{offset:x}: capstone={len} custom={consumed}"
            )));
        }

        decoded.push(DecodedInstruction { offset, len, kind });
    }

    Ok(decoded)
}

pub(super) fn find_elf_sysv_main_offset(
    text_bytes: &[u8],
    text_address: u64,
    entry_address: u64,
    rip_symbols: &HashMap<u64, crate::binary::RipSymbol>,
) -> Option<usize> {
    use capstone::prelude::*;
    use capstone::Syntax;

    let entry_offset = entry_address.checked_sub(text_address)?;
    let entry_offset = usize::try_from(entry_offset).ok()?;
    if entry_offset >= text_bytes.len() {
        return None;
    }

    // Heuristic: glibc-style `_start` loads `main` into `rdi` before calling
    // `__libc_start_main`. We disassemble a small window from the entrypoint,
    // remember the last `lea rdi, [rip + disp]`, and confirm the subsequent
    // `call` targets `__libc_start_main` via its GOT relocation.
    let window_len = (text_bytes.len() - entry_offset).min(512);
    let window = &text_bytes[entry_offset..entry_offset + window_len];

    let mut capstone = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .ok()?;
    capstone.set_syntax(Syntax::Intel).ok()?;
    let instructions = capstone.disasm_all(window, entry_address).ok()?;

    let trace = std::env::var_os("FP_LIFT_MAIN_TRACE").is_some();

    fn is_rip_relative(mem: &X86Memory) -> bool {
        mem.base == Some(16) && mem.index.is_none() && mem.displacement_offset.is_some()
    }

    let mut candidate_main: Option<u64> = None;
    for inst in instructions.iter() {
        let mnemonic = inst.mnemonic().unwrap_or("");
        match mnemonic {
            "lea" => {
                let op_str = inst.op_str().unwrap_or("");
                let mut parts = op_str.splitn(2, ',');
                let dst = parts.next().unwrap_or("").trim().to_ascii_lowercase();
                let src = parts.next().unwrap_or("").trim();
                if dst != "rdi" {
                    continue;
                }
                let Ok(mem) = parse_capstone_memory_operand(src) else {
                    continue;
                };
                if !is_rip_relative(&mem) {
                    continue;
                }
                let next_addr = inst.address().checked_add(inst.bytes().len() as u64)?;
                let target = (next_addr as i64).checked_add(mem.displacement)? as u64;
                if target >= text_address {
                    candidate_main = Some(target);
                    if trace {
                        eprintln!(
                            "[fp-native] ELF main candidate: 0x{target:x} (text_base=0x{text_address:x})"
                        );
                    }
                }
            }
            "call" => {
                let Some(candidate) = candidate_main else {
                    continue;
                };
                let op_str = inst.op_str().unwrap_or("");
                let Ok(mem) = parse_capstone_memory_operand(op_str) else {
                    continue;
                };
                if !is_rip_relative(&mem) {
                    continue;
                }
                let next_addr = inst.address().checked_add(inst.bytes().len() as u64)?;
                let got_target = (next_addr as i64).checked_add(mem.displacement)? as u64;
                let symbol = rip_symbols.get(&got_target)?;
                if trace {
                    eprintln!(
                        "[fp-native] ELF start call GOT 0x{got_target:x} -> {} ({:?})",
                        symbol.name,
                        symbol.kind
                    );
                }
                if symbol.name != "__libc_start_main" {
                    continue;
                }
                let offset = usize::try_from(candidate.checked_sub(text_address)?).ok()?;
                if offset < text_bytes.len() {
                    if trace {
                        eprintln!("[fp-native] ELF selected main offset: 0x{offset:x}");
                    }
                    return Some(offset);
                }
            }
            _ => {}
        }
    }

    None
}

fn determine_block_starts(
    decoded: &[DecodedInstruction],
    bytes_len: u64,
    entry_offset: u64,
) -> Result<Vec<u64>> {
    let inst_map = decoded
        .iter()
        .map(|inst| (inst.offset, inst))
        .collect::<std::collections::HashMap<_, _>>();

    if !inst_map.contains_key(&entry_offset) {
        return Err(Error::from(
            "x86_64 entrypoint is not on an instruction boundary",
        ));
    }

    let mut starts = std::collections::HashSet::new();
    starts.insert(entry_offset);

    let mut queue = std::collections::VecDeque::new();
    queue.push_back(entry_offset);
    let mut visited = std::collections::HashSet::new();

    while let Some(entry) = queue.pop_front() {
        if !visited.insert(entry) {
            continue;
        }
        let mut cursor = entry;
        loop {
            let inst = inst_map
                .get(&cursor)
                .ok_or_else(|| Error::from("missing decoded instruction"))?;

            match inst.kind {
                Decoded::JccRel { target, .. } => {
                    if !inst_map.contains_key(&target) {
                        return Err(Error::from("x86_64 jcc target not on instruction boundary"));
                    }
                    let fallthrough = cursor + inst.len as u64;
                    starts.insert(target);
                    starts.insert(fallthrough);
                    queue.push_back(target);
                    queue.push_back(fallthrough);
                    break;
                }
                Decoded::JmpRel { target } => {
                    if !inst_map.contains_key(&target) {
                        return Err(Error::from("x86_64 jmp target not on instruction boundary"));
                    }
                    starts.insert(target);
                    queue.push_back(target);
                    break;
                }
                Decoded::Ret | Decoded::Hlt | Decoded::JmpRm { .. } => {
                    break;
                }
                _ => {
                    let next = cursor + inst.len as u64;
                    if next >= bytes_len {
                        break;
                    }
                    if starts.contains(&next) {
                        queue.push_back(next);
                        break;
                    }
                    cursor = next;
                }
            }
        }
    }

    let mut starts: Vec<u64> = starts.into_iter().collect();
    starts.sort_unstable();
    Ok(starts)
}

fn is_terminator(kind: &Decoded) -> bool {
    matches!(
        kind,
        Decoded::Ret
            | Decoded::Hlt
            | Decoded::JmpRel { .. }
            | Decoded::JmpRm { .. }
            | Decoded::JccRel { .. }
    )
}

fn synthesize_fallthrough_compare(
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> LastCompare {
    let id = *next_id;
    instructions.push(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Eq),
        kind: AsmInstructionKind::Eq(
            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
            AsmValue::Constant(AsmConstant::Int(1, AsmType::I64)),
        ),
        type_hint: None,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    });
    *next_id += 1;
    LastCompare {
        id,
        index: instructions.len() - 1,
        is_float: false,
    }
}

fn lift_terminator(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    offset_to_block: &std::collections::HashMap<u64, u32>,
    last_compare: &mut Option<LastCompare>,
) -> Result<fp_core::asmir::AsmTerminator> {
    match inst.kind {
        Decoded::Ret => Ok(fp_core::asmir::AsmTerminator::Return(
            ctx.read_return_value(),
        )),
        Decoded::Hlt => Ok(fp_core::asmir::AsmTerminator::Unreachable),
        Decoded::JccRel { condition, target } => {
            let if_true = offset_to_block
                .get(&target)
                .copied()
                .ok_or_else(|| Error::from("missing conditional jump target block"))?;
            let fallthrough = inst.offset + inst.len as u64;
            let if_false = offset_to_block
                .get(&fallthrough)
                .copied()
                .ok_or_else(|| Error::from("missing conditional jump fallthrough block"))?;

            let compare = if let Some(compare) = last_compare.as_ref() {
                match patch_compare_kind(instructions, compare, condition) {
                    Ok(()) => *compare,
                    Err(_) => synthesize_fallthrough_compare(instructions, next_id),
                }
            } else {
                synthesize_fallthrough_compare(instructions, next_id)
            };

            // The branch consumes the flags produced by the compare.
            // This keeps the terminator independent from ISA-specific condition codes.
            Ok(fp_core::asmir::AsmTerminator::CondBr {
                condition: AsmValue::Flags(compare.id),
                if_true,
                if_false,
            })
        }
        Decoded::JmpRel { target } => {
            let dest = offset_to_block
                .get(&target)
                .copied()
                .ok_or_else(|| Error::from("missing jump target block"))?;
            Ok(fp_core::asmir::AsmTerminator::Br(dest))
        }
        Decoded::JmpRm { target } => {
            // Indirect branches show up in jump tables / PLT stubs.
            let address = match target {
                RmOperand::Reg(reg) => ctx.read_gpr(reg)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            &[],
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
                                volatile: true,
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
                        AsmValue::Register(load_id)
                    }
                }
            };
            Ok(fp_core::asmir::AsmTerminator::IndirectBr {
                address,
                destinations: Vec::new(),
            })
        }
        _ => Err(Error::from("internal error: expected terminator")),
    }
}

fn lift_non_terminator(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    last_compare: &mut Option<LastCompare>,
    syscall_convention: Option<AsmSyscallConvention>,
) -> Result<()> {
    match inst.kind {
        Decoded::Nop => Ok(()),
        Decoded::Hlt => Err(Error::from("internal error: unexpected hlt in non-terminator")),
        Decoded::Leave => {
            let rbp = ctx.read_gpr(5)?;
            ctx.write_gpr(4, rbp.clone());

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: rbp.clone(),
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

            let rsp_id = *next_id;
            instructions.push(build_binop(
                rsp_id,
                AsmInstructionKind::Add(
                    rbp,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(4, AsmValue::Register(rsp_id));
            ctx.write_gpr(5, AsmValue::Register(load_id));
            Ok(())
        }
        Decoded::PushReg { src } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = ctx.read_gpr(src)?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
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
            Ok(())
        }
        Decoded::PushImm { imm } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
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
            Ok(())
        }
        Decoded::CallRm { target } => {
            let args = x86_64_sysv_call_args(ctx)?;
            let function = match target {
                RmOperand::Reg(reg) => ctx.read_gpr(reg)?,
                RmOperand::Mem(memory) => {
                    if let Some(displacement_offset) = memory.displacement_offset {
                        let relocation_offset = inst
                            .offset
                            .checked_add(displacement_offset as u64)
                            .ok_or_else(|| Error::from("x86_64 call relocation overflow"))?;
                        if let Some(reloc) = relocation_at(relocs, relocation_offset) {
                            AsmValue::Function(reloc.symbol.clone())
                        } else {
                            if let Some(symbol) =
                                ctx.resolve_rip_symbol(&memory, inst.offset, inst.len)
                            {
                                if symbol.kind == RipSymbolKind::Function {
                                    AsmValue::Function(symbol.name.clone())
                                } else {
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
                                        type_hint: Some(AsmType::I64),
                                        operands: Vec::new(),
                                        implicit_uses: Vec::new(),
                                        implicit_defs: Vec::new(),
                                        encoding: None,
                                        debug_info: None,
                                        annotations: Vec::new(),
                                    });
                                    *next_id += 1;
                                    AsmValue::Register(load_id)
                                }
                            } else if let Some(symbol) =
                                ctx.resolve_disp32_symbol(&memory, inst.offset, inst.len)
                            {
                                if symbol.kind == RipSymbolKind::Function {
                                    AsmValue::Function(symbol.name.clone())
                                } else {
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
                                        type_hint: Some(AsmType::I64),
                                        operands: Vec::new(),
                                        implicit_uses: Vec::new(),
                                        implicit_defs: Vec::new(),
                                        encoding: None,
                                        debug_info: None,
                                        annotations: Vec::new(),
                                    });
                                    *next_id += 1;
                                    AsmValue::Register(load_id)
                                }
                            } else {
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
                                type_hint: Some(AsmType::I64),
                                operands: Vec::new(),
                                implicit_uses: Vec::new(),
                                implicit_defs: Vec::new(),
                                encoding: None,
                                debug_info: None,
                                annotations: Vec::new(),
                            });
                            *next_id += 1;
                            AsmValue::Register(load_id)
                            }
                        }
                    } else {
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
                            type_hint: Some(AsmType::I64),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function,
                    args,
                    calling_convention: CallingConvention::X86_64SysV,
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
            *next_id += 1;
            Ok(())
        }
        Decoded::PushRm { src } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
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
            Ok(())
        }
        Decoded::PopReg { dst } => {
            let rsp = ctx.read_gpr(4)?;
            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: rsp.clone(),
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
            ctx.write_gpr(dst, AsmValue::Register(load_id));

            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Add(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(4, AsmValue::Register(new_rsp_id));
            Ok(())
        }
        Decoded::XorReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Xor(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )?;
            let value = ctx.read_gpr(dst)?;
            let mask_bits = match width_bits {
                64 => None,
                32 => Some(0xFFFF_FFFFu64),
                16 => Some(0xFFFFu64),
                8 => Some(0xFFu64),
                _ => None,
            };
            let masked = if let Some(mask_bits) = mask_bits {
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let masked_id = *next_id;
                instructions.push(build_binop(
                    masked_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                AsmValue::Register(masked_id)
            } else {
                value
            };

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    masked,
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::XorImm {
            dst,
            imm,
            width_bits,
        } => {
            lift_rm_imm_binop(
                ctx,
                dst,
                imm,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
                fp_core::asmir::AsmGenericOpcode::Xor,
            )?;

            if let RmOperand::Reg(reg) = dst {
                let value = ctx.read_gpr(reg)?;
                let mask_bits = match width_bits {
                    64 => None,
                    32 => Some(0xFFFF_FFFFu64),
                    16 => Some(0xFFFFu64),
                    8 => Some(0xFFu64),
                    _ => None,
                };
                let masked = if let Some(mask_bits) = mask_bits {
                    let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                    let masked_id = *next_id;
                    instructions.push(build_binop(
                        masked_id,
                        AsmInstructionKind::And(value, mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    AsmValue::Register(masked_id)
                } else {
                    value
                };

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Eq(
                        masked,
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ),
                    fp_core::asmir::AsmGenericOpcode::Eq,
                ));
                *next_id += 1;
                *last_compare = Some(LastCompare {
                    id: cmp_id,
                    index: instructions.len() - 1,
                    is_float: false,
                });
            }

            Ok(())
        }
        Decoded::OrImm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Or,
        ),
        Decoded::AndReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::And(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::AndRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::And(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::OrReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Or(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::OrRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Or(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::OrRmReg {
            dst,
            src,
            width_bits,
        }
        | Decoded::AndRmReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            let (kind, opcode) = match inst.kind {
                Decoded::OrRmReg { .. } => (
                    AsmInstructionKind::Or(lhs, rhs),
                    fp_core::asmir::AsmGenericOpcode::Or,
                ),
                _ => (
                    AsmInstructionKind::And(lhs, rhs),
                    fp_core::asmir::AsmGenericOpcode::And,
                ),
            };
            instructions.push(build_binop(id, kind, AsmOpcode::Generic(opcode)));
            *next_id += 1;

            match dst {
                RmOperand::Reg(dst_reg) => write_gpr_with_width(
                    ctx,
                    dst_reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
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
                    Ok(())
                }
            }
        }
        Decoded::XorRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Xor(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::AndImm {
            dst,
            imm,
            width_bits,
        } => {
            lift_rm_imm_binop(
                ctx,
                dst,
                imm,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
                fp_core::asmir::AsmGenericOpcode::And,
            )
        }
        Decoded::AdcImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::SbbImm {
            dst,
            imm,
            width_bits,
        } => {
            let compare = last_compare
                .as_ref()
                .ok_or_else(|| Error::from("adc/sbb without comparison"))?;
            // Carry flag corresponds to unsigned borrow from the last compare.
            // Force the compare to be treated as `ult`.
            patch_compare_kind(instructions, compare, 0x2)?;

            let carry_id = *next_id;
            instructions.push(AsmInstruction {
                id: carry_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            let carry = AsmValue::Register(carry_id);

            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let is_sbb = matches!(inst.kind, Decoded::SbbImm { .. });

            let (addr, current_value) = match dst {
                RmOperand::Reg(reg) => (None, ctx.read_gpr(reg)?),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
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
                            address: addr.clone(),
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
                    (Some(addr), AsmValue::Register(load_id))
                }
            };

            let id1 = *next_id;
            let first_kind = if is_sbb {
                AsmInstructionKind::Sub(current_value.clone(), rhs)
            } else {
                AsmInstructionKind::Add(current_value.clone(), rhs)
            };
            let first_op = if is_sbb {
                fp_core::asmir::AsmGenericOpcode::Sub
            } else {
                fp_core::asmir::AsmGenericOpcode::Add
            };
            instructions.push(build_binop(
                id1,
                first_kind,
                AsmOpcode::Generic(first_op.clone()),
            ));
            *next_id += 1;

            let id2 = *next_id;
            let second_kind = if is_sbb {
                AsmInstructionKind::Sub(AsmValue::Register(id1), carry)
            } else {
                AsmInstructionKind::Add(AsmValue::Register(id1), carry)
            };
            instructions.push(build_binop(
                id2,
                second_kind,
                AsmOpcode::Generic(first_op),
            ));
            *next_id += 1;

            let mut value = AsmValue::Register(id2);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }

            match (dst, addr) {
                (RmOperand::Reg(reg), _) => {
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                (RmOperand::Mem(_), Some(addr)) => {
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
                _ => Err(Error::from("internal error: adc/sbb address missing")),
            }
        }
        Decoded::MovImm32ToRm {
            dst,
            imm_offset: _,
            imm,
        } => {
            let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I32));
            match dst {
                RmOperand::Reg(dst) => {
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value),
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    // x86_64 zero-extends 32-bit register writes.
                    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let and_id = *next_id;
                    instructions.push(build_binop(
                        and_id,
                        AsmInstructionKind::And(AsmValue::Register(id), mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(and_id));
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
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
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::MovImm32ToMem64 {
            dst,
            imm_offset: _,
            imm,
        } => {
            if dst.segment.is_some() {
                return Ok(());
            }
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I64));
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
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
            Ok(())
        }
        Decoded::MovImm8ToRm { dst, imm } => {
            match dst {
                RmOperand::Reg(dst) => {
                    let value = AsmValue::Constant(AsmConstant::UInt(imm as u8 as u64, AsmType::I64));
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value),
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
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I8));
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::MovImm16ToRm {
            dst,
            imm_offset: _,
            imm,
        } => match dst {
            RmOperand::Reg(dst) => {
                let current = ctx.read_gpr(dst)?;
                let mask = AsmValue::Constant(AsmConstant::UInt(
                    0xFFFF_FFFF_FFFF_0000,
                    AsmType::I64,
                ));
                let masked_id = *next_id;
                instructions.push(build_binop(
                    masked_id,
                    AsmInstructionKind::And(current, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;

                let imm_val = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I64));
                let merged_id = *next_id;
                instructions.push(build_binop(
                    merged_id,
                    AsmInstructionKind::Or(AsmValue::Register(masked_id), imm_val),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                ));
                *next_id += 1;
                ctx.write_gpr(dst, AsmValue::Register(merged_id));
                Ok(())
            }
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    return Ok(());
                }
                let value = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I16));
                let addr = compute_address(
                    ctx,
                    memory,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value,
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
                Ok(())
            }
        },
        Decoded::AddRegRm { dst, src } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Add(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::AddRmReg {
            dst,
            src,
            width_bits,
        } => {
            let rhs = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    let lhs = ctx.read_gpr(dst)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs.clone(), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(dst, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
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
                            address: addr.clone(),
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
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(AsmValue::Register(load_id), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::SubRegRm { dst, src } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::SubRegRmWidth {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::SubRmReg {
            dst,
            src,
            width_bits,
        } => {
            let rhs = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    let lhs = ctx.read_gpr(dst)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Sub(lhs.clone(), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(dst, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
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
                            address: addr.clone(),
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
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Sub(AsmValue::Register(load_id), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::AddImm {
            dst,
            imm,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            let mut value = AsmValue::Register(id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, value);
            Ok(())
        }
        Decoded::AddImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Add,
        ),
        Decoded::SubImm {
            dst,
            imm,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let mut value = AsmValue::Register(id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, value);
            Ok(())
        }
        Decoded::SubImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Sub,
        ),
        Decoded::Cmp {
            lhs,
            rhs,
            width_bits,
        } => {
            let lhs_value = value_from_operand_with_width(
                ctx,
                lhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_value = value_from_operand_with_width(
                ctx,
                rhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let (lhs_value, rhs_value) = if width_bits != 64 {
                let mask_bits = if width_bits == 32 { 0xFFFF_FFFF } else { 0xFF };
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let lhs_id = *next_id;
                instructions.push(build_binop(
                    lhs_id,
                    AsmInstructionKind::And(lhs_value, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                let rhs_id = *next_id;
                instructions.push(build_binop(
                    rhs_id,
                    AsmInstructionKind::And(rhs_value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                (AsmValue::Register(lhs_id), AsmValue::Register(rhs_id))
            } else {
                (lhs_value, rhs_value)
            };
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(lhs_value, rhs_value),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::Test {
            lhs,
            rhs,
            width_bits,
        } => {
            let lhs_value = value_from_operand_with_width(
                ctx,
                lhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_value = value_from_operand_with_width(
                ctx,
                rhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let (lhs_value, rhs_value) = if width_bits != 64 {
                let mask_bits = if width_bits == 32 { 0xFFFF_FFFF } else { 0xFF };
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let lhs_id = *next_id;
                instructions.push(build_binop(
                    lhs_id,
                    AsmInstructionKind::And(lhs_value, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                let rhs_id = *next_id;
                instructions.push(build_binop(
                    rhs_id,
                    AsmInstructionKind::And(rhs_value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                (AsmValue::Register(lhs_id), AsmValue::Register(rhs_id))
            } else {
                (lhs_value, rhs_value)
            };
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    AsmValue::Register(and_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::VptestMem { lhs, rhs } => {
            if rhs.segment.is_some() {
                return Ok(());
            }

            let lhs_vec = ctx.read_vec(lhs)?;

            let lhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
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
            let lhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 1,
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

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr.clone(),
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

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let rhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(addr1_id),
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

            let and0_id = *next_id;
            instructions.push(build_binop(
                and0_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lhs0_id),
                    AsmValue::Register(rhs0_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            let and1_id = *next_id;
            instructions.push(build_binop(
                and1_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lhs1_id),
                    AsmValue::Register(rhs1_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(and0_id), AsmValue::Register(and1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtReg { value, bit } => {
            let lhs_value = ctx.read_gpr(value)?;
            let rhs_value = ctx.read_gpr(bit)?;
            let shift_id = *next_id;
            instructions.push(build_binop(
                shift_id,
                AsmInstructionKind::Shr(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let mask = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(AsmValue::Register(shift_id), mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            // `bt` writes the carry flag; represent that as a comparison between
            // `0` and the tested bit, which allows `jc/jnc` to patch the predicate
            // into `ult/uge`.
            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtImm { value, imm } => {
            let lhs_value = ctx.read_gpr(value)?;
            let rhs_value = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I64));
            let shift_id = *next_id;
            instructions.push(build_binop(
                shift_id,
                AsmInstructionKind::Shr(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let mask = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(AsmValue::Register(shift_id), mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtcImm {
            dst,
            imm,
            width_bits,
        } => {
            let value = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let mask_value =
                AsmValue::Constant(AsmConstant::UInt(1u64 << (imm as u64), AsmType::I64));

            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value.clone(), mask_value.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });

            let xor_id = *next_id;
            instructions.push(build_binop(
                xor_id,
                AsmInstructionKind::Xor(value, mask_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            match dst {
                RmOperand::Reg(reg) => write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(xor_id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(xor_id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
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
                    Ok(())
                }
            }
        }
        Decoded::Cqo => {
            let rax = ctx.read_gpr(0)?;
            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Lt(
                    rax,
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Lt,
            ));
            *next_id += 1;

            let select_id = *next_id;
            instructions.push(AsmInstruction {
                id: select_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
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
            ctx.write_gpr(2, AsmValue::Register(select_id));
            Ok(())
        }
        Decoded::Cdq => {
            let rax = ctx.read_gpr(0)?;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(rax, AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Lt(
                    AsmValue::Register(sext_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Lt,
            ));
            *next_id += 1;

            let select_id = *next_id;
            instructions.push(AsmInstruction {
                id: select_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
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
            ctx.write_gpr(2, AsmValue::Register(select_id));
            Ok(())
        }
        Decoded::Cdqe => {
            let value = ctx.read_gpr(0)?;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(value, AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(0, AsmValue::Register(sext_id));
            Ok(())
        }
        Decoded::ShlImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::ShrImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::SarImm {
            dst,
            imm,
            width_bits,
        } => {
            let lhs =
                value_from_rm_with_width(ctx, dst, width_bits, *inst, relocs, instructions, next_id)?;

            let id = if matches!(inst.kind, Decoded::SarImm { .. }) {
                let sign_shift = match width_bits {
                    64 => 63u8,
                    32 => 31u8,
                    _ => return Err(Error::from("unsupported x86_64 sar width")),
                };
                if imm == 0 {
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    id
                } else {
                    let shift = AsmValue::Constant(AsmConstant::Int(i64::from(imm), AsmType::I64));
                    let logical_id = *next_id;
                    instructions.push(build_binop(
                        logical_id,
                        AsmInstructionKind::Shr(lhs.clone(), shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let sign_id = *next_id;
                    instructions.push(build_binop(
                        sign_id,
                        AsmInstructionKind::Shr(
                            lhs,
                            AsmValue::Constant(AsmConstant::Int(i64::from(sign_shift), AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let neg_id = *next_id;
                    instructions.push(build_binop(
                        neg_id,
                        AsmInstructionKind::Sub(
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            AsmValue::Register(sign_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;

                    let fill_shift = width_bits
                        .try_into()
                        .ok()
                        .and_then(|bits: u8| bits.checked_sub(imm))
                        .ok_or_else(|| Error::from("unsupported x86_64 sar shift"))?;
                    let fill_id = *next_id;
                    instructions.push(build_binop(
                        fill_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(neg_id),
                            AsmValue::Constant(AsmConstant::Int(i64::from(fill_shift), AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let result_id = *next_id;
                    instructions.push(build_binop(
                        result_id,
                        AsmInstructionKind::Or(
                            AsmValue::Register(logical_id),
                            AsmValue::Register(fill_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                    ));
                    *next_id += 1;
                    result_id
                }
            } else {
                let rhs = AsmValue::Constant(AsmConstant::Int(i64::from(imm), AsmType::I64));
                let id = *next_id;
                let (kind, opcode) = match inst.kind {
                    Decoded::ShlImm { .. } => (
                        AsmInstructionKind::Shl(lhs, rhs),
                        fp_core::asmir::AsmGenericOpcode::Shl,
                    ),
                    Decoded::ShrImm { .. } => (
                        AsmInstructionKind::Shr(lhs, rhs),
                        fp_core::asmir::AsmGenericOpcode::Shr,
                    ),
                    _ => return Err(Error::from("internal error: expected shift kind")),
                };
                instructions.push(build_binop(id, kind, AsmOpcode::Generic(opcode)));
                *next_id += 1;
                id
            };

            match dst {
                RmOperand::Reg(dst_reg) => write_gpr_with_width(
                    ctx,
                    dst_reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
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
                    Ok(())
                }
            }
        }
        Decoded::Shrx {
            dst,
            src,
            shift,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = value_from_rm_with_width(
                ctx,
                shift,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = freeze_i64(rhs, instructions, next_id);

            let mask = if width_bits == 64 { 0x3F } else { 0x1F };
            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    rhs,
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Shr(lhs, AsmValue::Register(rhs_mask_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::Shlx {
            dst,
            src,
            shift,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = value_from_rm_with_width(
                ctx,
                shift,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = freeze_i64(rhs, instructions, next_id);

            let mask = if width_bits == 64 { 0x3F } else { 0x1F };
            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    rhs,
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Shl(lhs, AsmValue::Register(rhs_mask_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::Rorx {
            dst,
            src,
            imm,
            width_bits,
        } => {
            let width = width_bits as u64;
            let imm = (imm as u64) % width;
            if imm == 0 {
                let value = value_from_rm_with_width(
                    ctx,
                    src,
                    width_bits,
                    *inst,
                    relocs,
                    instructions,
                    next_id,
                )?;
                return write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id);
            }

            let value = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let imm_value = AsmValue::Constant(AsmConstant::UInt(imm, AsmType::I64));
            let inv_value = AsmValue::Constant(AsmConstant::UInt(width - imm, AsmType::I64));

            let shr_id = *next_id;
            instructions.push(build_binop(
                shr_id,
                AsmInstructionKind::Shr(value.clone(), imm_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let shl_id = *next_id;
            instructions.push(build_binop(
                shl_id,
                AsmInstructionKind::Shl(value, inv_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(shr_id), AsmValue::Register(shl_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            write_gpr_with_width(ctx, dst, AsmValue::Register(or_id), width_bits, instructions, next_id)
        }
        Decoded::Blsr {
            dst,
            src,
            width_bits,
        } => {
            let value = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let sub_id = *next_id;
            instructions.push(build_binop(
                sub_id,
                AsmInstructionKind::Sub(
                    value.clone(),
                    AsmValue::Constant(AsmConstant::Int(1, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;

            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value, AsmValue::Register(sub_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            write_gpr_with_width(ctx, dst, AsmValue::Register(and_id), width_bits, instructions, next_id)
        }
        Decoded::NotRm { dst, width_bits } => {
            let value = value_from_rm_with_width(ctx, dst, width_bits, *inst, relocs, instructions, next_id)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Not),
                kind: AsmInstructionKind::Not(value),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            match dst {
                RmOperand::Reg(reg) => {
                    write_gpr_with_width(ctx, reg, AsmValue::Register(id), width_bits, instructions, next_id)
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored = value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
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
                    Ok(())
                }
            }
        }
        Decoded::NegRm { dst, width_bits } => {
            let value = value_from_rm_with_width(ctx, dst, width_bits, *inst, relocs, instructions, next_id)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    value,
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            match dst {
                RmOperand::Reg(reg) => {
                    write_gpr_with_width(ctx, reg, AsmValue::Register(id), width_bits, instructions, next_id)
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored = value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
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
                    Ok(())
                }
            }
        }
        Decoded::SbbSelf { reg, width_bits } => {
            if let Some(compare) = last_compare.as_ref() {
                // Interpret carry as `ult` from the last comparison.
                patch_compare_kind(instructions, compare, 0x2)?;

                let zext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                    kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I64),
                    type_hint: Some(AsmType::I64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Sub(
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Register(zext_id),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                ));
                *next_id += 1;
                write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                )
            } else {
                // If we do not know the carry flag, conservatively emit `0`.
                write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    width_bits,
                    instructions,
                    next_id,
                )
            }
        }
        Decoded::OrImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Or,
        ),
        Decoded::AndImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::And,
        ),
        Decoded::ImulReg { dst, src, width_bits } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::ImulRegImm {
            dst,
            src,
            imm,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::ImulRmWide { src, width_bits } => {
            let lhs = ctx.read_gpr(0)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;

            match width_bits {
                32 => {
                    let low_mask =
                        AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::And(AsmValue::Register(id), low_mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    write_gpr_with_width(
                        ctx,
                        0,
                        AsmValue::Register(low_id),
                        32,
                        instructions,
                        next_id,
                    )?;
                    write_gpr_with_width(
                        ctx,
                        2,
                        AsmValue::Register(high_id),
                        32,
                        instructions,
                        next_id,
                    )?;
                    Ok(())
                }
                64 => {
                    ctx.write_gpr(0, AsmValue::Register(id));
                    ctx.write_gpr(2, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 imul width")),
            }
        }
        Decoded::MulRm { src, width_bits } => {
            let lhs = ctx.read_gpr(0)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            match width_bits {
                32 => {
                    let mask =
                        AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));

                    let lhs32_id = *next_id;
                    instructions.push(build_binop(
                        lhs32_id,
                        AsmInstructionKind::And(lhs, mask.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let rhs32_id = *next_id;
                    instructions.push(build_binop(
                        rhs32_id,
                        AsmInstructionKind::And(rhs, mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let product_id = *next_id;
                    instructions.push(build_binop(
                        product_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(lhs32_id),
                            AsmValue::Register(rhs32_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;

                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::And(
                            AsmValue::Register(product_id),
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(product_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    write_gpr_with_width(ctx, 0, AsmValue::Register(low_id), 32, instructions, next_id)?;
                    write_gpr_with_width(ctx, 2, AsmValue::Register(high_id), 32, instructions, next_id)
                }
                64 => {
                    let low_mask =
                        AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let high_shift = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

                    let a0_id = *next_id;
                    instructions.push(build_binop(
                        a0_id,
                        AsmInstructionKind::And(lhs.clone(), low_mask.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    let a1_id = *next_id;
                    instructions.push(build_binop(
                        a1_id,
                        AsmInstructionKind::Shr(lhs, high_shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let b0_id = *next_id;
                    instructions.push(build_binop(
                        b0_id,
                        AsmInstructionKind::And(rhs.clone(), low_mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    let b1_id = *next_id;
                    instructions.push(build_binop(
                        b1_id,
                        AsmInstructionKind::Shr(rhs, high_shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let p0_id = *next_id;
                    instructions.push(build_binop(
                        p0_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a0_id),
                            AsmValue::Register(b0_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p1_id = *next_id;
                    instructions.push(build_binop(
                        p1_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a0_id),
                            AsmValue::Register(b1_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p2_id = *next_id;
                    instructions.push(build_binop(
                        p2_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a1_id),
                            AsmValue::Register(b0_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p3_id = *next_id;
                    instructions.push(build_binop(
                        p3_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a1_id),
                            AsmValue::Register(b1_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;

                    let mid_sum_id = *next_id;
                    instructions.push(build_binop(
                        mid_sum_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p1_id),
                            AsmValue::Register(p2_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let mid_carry_flag_id = *next_id;
                    instructions.push(compare_instruction(
                        mid_carry_flag_id,
                        AsmInstructionKind::Ult(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Register(p1_id),
                        ),
                        fp_core::asmir::AsmGenericOpcode::Ult,
                    ));
                    *next_id += 1;

                    let mid_carry_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: mid_carry_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                        kind: AsmInstructionKind::ZExt(
                            AsmValue::Register(mid_carry_flag_id),
                            AsmType::I64,
                        ),
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let mid_shifted_id = *next_id;
                    instructions.push(build_binop(
                        mid_shifted_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p0_id),
                            AsmValue::Register(mid_shifted_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let low_carry_flag_id = *next_id;
                    instructions.push(compare_instruction(
                        low_carry_flag_id,
                        AsmInstructionKind::Ult(
                            AsmValue::Register(low_id),
                            AsmValue::Register(p0_id),
                        ),
                        fp_core::asmir::AsmGenericOpcode::Ult,
                    ));
                    *next_id += 1;

                    let low_carry_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: low_carry_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                        kind: AsmInstructionKind::ZExt(
                            AsmValue::Register(low_carry_flag_id),
                            AsmType::I64,
                        ),
                        type_hint: Some(AsmType::I64),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let mid_high_id = *next_id;
                    instructions.push(build_binop(
                        mid_high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let mid_carry_shifted_id = *next_id;
                    instructions.push(build_binop(
                        mid_carry_shifted_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(mid_carry_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let high_partial1_id = *next_id;
                    instructions.push(build_binop(
                        high_partial1_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p3_id),
                            AsmValue::Register(mid_high_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let high_partial2_id = *next_id;
                    instructions.push(build_binop(
                        high_partial2_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(high_partial1_id),
                            AsmValue::Register(mid_carry_shifted_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(high_partial2_id),
                            AsmValue::Register(low_carry_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    ctx.write_gpr(0, AsmValue::Register(low_id));
                    ctx.write_gpr(2, AsmValue::Register(high_id));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 mul width")),
            }
        }
        Decoded::Fild { src, width_bits } => {
            if src.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(0.0, AsmType::F64))),
                    type_hint: Some(AsmType::F64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(id))?;
                return Ok(());
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let load_ty = match width_bits {
                32 => AsmType::I32,
                64 => AsmType::I64,
                _ => return Err(Error::from("unsupported x86_64 fild width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(load_ty.clone()),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let conv_id = *next_id;
            instructions.push(AsmInstruction {
                id: conv_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SIToFP),
                kind: AsmInstructionKind::SIToFP(AsmValue::Register(load_id), AsmType::F64),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_push(AsmValue::Register(conv_id))?;
            Ok(())
        }
        Decoded::FldSt { index } => {
            let value = ctx.x87_peek(index)?;
            ctx.x87_push(value)
        }
        Decoded::FldMem { src, width_bits } => {
            if src.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(0.0, AsmType::F64))),
                    type_hint: Some(AsmType::F64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(id))?;
                return Ok(());
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let (load_ty, need_extend) = match width_bits {
                32 => (AsmType::F32, true),
                64 | 80 => (AsmType::F64, false),
                _ => return Err(Error::from("unsupported x86_64 fld width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(load_ty),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            if need_extend {
                let ext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: ext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPExt),
                    kind: AsmInstructionKind::FPExt(AsmValue::Register(load_id), AsmType::F64),
                    type_hint: Some(AsmType::F64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(ext_id))
            } else {
                ctx.x87_push(AsmValue::Register(load_id))
            }
        }
        Decoded::Fxch { index } => ctx.x87_swap(index),
        Decoded::FmulSt0St { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(st0, rhs),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Fmulp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(lhs, st0),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fdivrp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let denom = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(st0, denom),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fdivp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(lhs, st0),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fcomi { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(st0, rhs),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: true,
            });
            Ok(())
        }
        Decoded::Fcomip { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(st0, rhs),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: true,
            });
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FstpSt { index } => {
            let st0 = ctx.x87_peek(0)?;
            if index != 0 {
                ctx.x87_set(index, st0)?;
            }
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FstpMem { dst, width_bits } => {
            let st0 = ctx.x87_peek(0)?;
            let stored_value = match width_bits {
                32 => {
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPTrunc),
                        kind: AsmInstructionKind::FPTrunc(st0, AsmType::F32),
                        type_hint: Some(AsmType::F32),
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
                64 => st0,
                // x87 `fstp tbyte ptr` stores 80-bit extended precision.
                // The current AsmIR model does not represent 80-bit floats,
                // so we conservatively truncate to f64.
                80 => st0,
                _ => {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fstp width_bits={width_bits}"
                    )))
                }
            };

            if dst.segment.is_none() {
                let addr = compute_address(
                    ctx,
                    dst,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let store_id = *next_id;
                instructions.push(AsmInstruction {
                    id: store_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value: stored_value,
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
            }

            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fisttp { dst, width_bits } => {
            let st0 = ctx.x87_peek(0)?;
            if dst.segment.is_none() {
                let addr = compute_address(
                    ctx,
                    dst,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let int_ty = match width_bits {
                    32 => AsmType::I32,
                    64 => AsmType::I64,
                    _ => return Err(Error::from("unsupported x86_64 fisttp width")),
                };

                let conv_id = *next_id;
                instructions.push(AsmInstruction {
                    id: conv_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPToSI),
                    kind: AsmInstructionKind::FPToSI(st0, int_ty.clone()),
                    type_hint: Some(int_ty),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                let store_id = *next_id;
                instructions.push(AsmInstruction {
                    id: store_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value: AsmValue::Register(conv_id),
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
            }

            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FaddMem { src, width_bits } => {
            if src.segment.is_some() {
                return Ok(());
            }
            let st0 = ctx.x87_peek(0)?;
            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let (load_ty, need_extend) = match width_bits {
                32 => (AsmType::F32, true),
                64 | 80 => (AsmType::F64, false),
                _ => return Err(Error::from("unsupported x86_64 fadd width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(load_ty),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs = if need_extend {
                let ext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: ext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPExt),
                    kind: AsmInstructionKind::FPExt(AsmValue::Register(load_id), AsmType::F64),
                    type_hint: Some(AsmType::F64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                AsmValue::Register(ext_id)
            } else {
                AsmValue::Register(load_id)
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                kind: AsmInstructionKind::Add(st0, rhs),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Ffreep { index } => {
            if index != 0 {
                let zero_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zero_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(0.0, AsmType::F64))),
                    type_hint: Some(AsmType::F64),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_set(index, AsmValue::Register(zero_id))?;
            }
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FsubrSt0St { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                kind: AsmInstructionKind::Sub(lhs, st0),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Fcmovcc { condition, src } => {
            let old = ctx.x87_peek(0)?;
            let new = ctx.x87_peek(src)?;

            let Some(compare) = last_compare.as_ref() else {
                return ctx.x87_set(0, new);
            };
            patch_compare_kind(instructions, compare, condition)?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(compare.id),
                    if_true: new,
                    if_false: old,
                },
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Cmovcc {
            dst,
            src,
            condition,
            width_bits,
        } => {
            let old = ctx.read_gpr(dst)?;
            let new = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let Some(compare) = last_compare.as_ref() else {
                return write_gpr_with_width(ctx, dst, new, width_bits, instructions, next_id);
            };
            patch_compare_kind(instructions, compare, condition)?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(compare.id),
                    if_true: new,
                    if_false: old,
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
            write_gpr_with_width(ctx, dst, AsmValue::Register(id), width_bits, instructions, next_id)
        }
        Decoded::MovRmToReg { dst, src, width_bits } => match src {
            RmOperand::Reg(src) => {
                let value = ctx.read_gpr(src)?;
                write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
            }
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    // Most real-world ELF executables use this for stack canary loads.
                    // We do not model TLS/segments yet, so treat it as a stable zero value.
                    let value = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value),
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

                if let Some(symbol) = ctx
                    .resolve_disp32_symbol(&memory, inst.offset, inst.len)
                    .or_else(|| ctx.resolve_rip_symbol(&memory, inst.offset, inst.len))
                {
                    if symbol.kind == RipSymbolKind::Function {
                        ctx.write_gpr(dst, AsmValue::Function(symbol.name.clone()));
                        return Ok(());
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
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                    kind: AsmInstructionKind::Load {
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    type_hint: Some(match width_bits {
                        8 => AsmType::I8,
                        16 => AsmType::I16,
                        32 => AsmType::I32,
                        _ => AsmType::I64,
                    }),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                let value = match width_bits {
                    64 => AsmValue::Register(id),
                    _ => {
                        let zext_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: zext_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                            kind: AsmInstructionKind::ZExt(AsmValue::Register(id), AsmType::I64),
                            type_hint: Some(AsmType::I64),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(zext_id)
                    }
                };
                write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
            }
        },
        Decoded::MovRegToRm { dst, src, width_bits } => {
            let value = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
                }
                RmOperand::Mem(memory) => {
                    let value = value_for_store(width_bits, value, instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::MovbeRegFromMem { dst, src, width_bits } => {
            let value = value_from_rm_with_width(
                ctx,
                RmOperand::Mem(src),
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            write_gpr_with_width(ctx, dst, swapped, width_bits, instructions, next_id)
        }
        Decoded::MovbeMemFromReg { dst, src, width_bits } => {
            let value = ctx.read_gpr(src)?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            let value = value_for_store(width_bits, swapped, instructions, next_id)?;
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
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
            Ok(())
        }
        Decoded::Bswap { dst, width_bits } => {
            let value = ctx.read_gpr(dst)?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            write_gpr_with_width(ctx, dst, swapped, width_bits, instructions, next_id)
        }
        Decoded::MovImm64 {
            dst,
            imm_offset,
            imm,
        } => {
            let reloc_offset = inst
                .offset
                .checked_add(imm_offset as u64)
                .ok_or_else(|| Error::from("x86_64 mov imm64 relocation overflow"))?;
            if let Some(reloc) = relocation_at(relocs, reloc_offset) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(reloc.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
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
                *next_id += 1;

                let mut value = AsmValue::Register(symbol_id);
                let addend = reloc.addend.saturating_add(imm);
                if addend != 0 {
                    let rhs = AsmValue::Constant(AsmConstant::Int(addend, AsmType::I64));
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(value, rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    value = AsmValue::Register(id);
                }
                ctx.write_gpr(dst, value);
                return Ok(());
            }

            let value = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(value),
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
            Ok(())
        }
        Decoded::Lea {
            dst,
            src,
            width_bits,
        } => {
            if src.segment.is_none() && src.index.is_none() {
                if src.base == Some(16)
                    || (src.base.is_none() && src.displacement_offset.is_some())
                {
                    let next_ip = (ctx.code_base_address as i64)
                        .saturating_add(inst.offset as i64)
                        .saturating_add(inst.len as i64);
                    let target = next_ip.saturating_add(src.displacement);
                    if target >= 0 {
                        if let Some(text) = ctx.rodata_cstrings_by_addr.get(&(target as u64)) {
                            return write_gpr_with_width(
                                ctx,
                                dst,
                                AsmValue::Constant(AsmConstant::String(text.clone())),
                                width_bits,
                                instructions,
                                next_id,
                            );
                        }
                    }
                }
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            write_gpr_with_width(ctx, dst, addr, width_bits, instructions, next_id)
        }
        Decoded::CallRel32 { imm_offset, target } => {
            let reloc_offset = inst
                .offset
                .checked_add(imm_offset as u64)
                .ok_or_else(|| Error::from("x86_64 call relocation overflow"))?;

            let function = if let Some(reloc) = relocation_at(relocs, reloc_offset) {
                if reloc.kind != object::RelocationKind::Relative
                    && reloc.kind != object::RelocationKind::PltRelative
                {
                    return Err(Error::from("unsupported x86_64 call relocation kind"));
                }
                if reloc.encoding != object::RelocationEncoding::X86Branch
                    && reloc.encoding != object::RelocationEncoding::Generic
                    && reloc.encoding != object::RelocationEncoding::Unknown
                {
                    return Err(Error::from("unsupported x86_64 call relocation encoding"));
                }
                AsmValue::Function(reloc.symbol.clone())
            } else {
                // Executables frequently use direct calls that do not carry
                // relocations. We represent the callee as a synthetic symbol
                // rooted at the target offset within the text slice so the
                // object lifter can pull in the corresponding function body.
                ctx.direct_call_targets.push(target);
                AsmValue::Function(format!("sub_{target:x}"))
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function,
                    args: x86_64_sysv_call_args(ctx)?,
                    calling_convention: CallingConvention::X86_64SysV,
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
            *next_id += 1;
            Ok(())
        }
        Decoded::IncRm { target, width_bits } => {
            let one = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            match target {
                RmOperand::Reg(reg) => {
                    let lhs = ctx.read_gpr(reg)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs.clone(), one),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
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
                            address: addr.clone(),
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

                    let add_id = *next_id;
                    instructions.push(build_binop(
                        add_id,
                        AsmInstructionKind::Add(AsmValue::Register(load_id), one),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(add_id);
                    if width_bits == 32 {
                        let mask =
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }

                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::DecRm { target, width_bits } => lift_rm_imm_binop(
            ctx,
            target,
            1,
            width_bits,
            *inst,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Sub,
        ),
        Decoded::MovSxd { dst, src } => {
            let value = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
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
                            type_hint: Some(AsmType::I32),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(value, AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_gpr(dst, AsmValue::Register(sext_id));
            Ok(())
        }
        Decoded::MovSx {
            dst,
            src,
            src_width_bits,
            dst_width_bits,
        } => {
            let raw = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
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
                            type_hint: Some(match src_width_bits {
                                8 => AsmType::I8,
                                _ => AsmType::I16,
                            }),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let truncated_type = match src_width_bits {
                8 => AsmType::I8,
                _ => AsmType::I16,
            };
            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(raw, truncated_type.clone()),
                type_hint: Some(truncated_type),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(sext_id),
                dst_width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::DivRm {
            src,
            signed,
            width_bits,
        } => {
            let _signed = signed;

            let rax = ctx.read_gpr(0)?;
            // `div` uses the 128-bit dividend (rdx:rax). Modeling the full
            // wide dividend would require multi-precision arithmetic in AsmIR.
            // For now, approximate by using only the low 64 bits (rax).
            let _rdx = ctx.read_gpr(2)?;

            let divisor = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let quot_id = *next_id;
            instructions.push(build_binop(
                quot_id,
                AsmInstructionKind::Div(rax.clone(), divisor.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
            ));
            *next_id += 1;
            let rem_id = *next_id;
            instructions.push(build_binop(
                rem_id,
                AsmInstructionKind::Rem(rax, divisor),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Rem),
            ));
            *next_id += 1;

            let mut quot = AsmValue::Register(quot_id);
            let mut rem = AsmValue::Register(rem_id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let q_id = *next_id;
                instructions.push(build_binop(
                    q_id,
                    AsmInstructionKind::And(quot, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                quot = AsmValue::Register(q_id);
                let r_id = *next_id;
                instructions.push(build_binop(
                    r_id,
                    AsmInstructionKind::And(rem, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                rem = AsmValue::Register(r_id);
            }

            ctx.write_gpr(0, quot);
            ctx.write_gpr(2, rem);
            Ok(())
        }
        Decoded::Syscall => {
            let syscall_convention = syscall_convention.ok_or_else(|| {
                Error::from("x86_64 syscall lifting is disabled for COFF/PE targets")
            })?;
            let number = ctx.read_gpr(0)?;
            let args = vec![
                ctx.read_gpr(7)?,
                ctx.read_gpr(6)?,
                ctx.read_gpr(2)?,
                ctx.read_gpr(10)?,
                ctx.read_gpr(8)?,
                ctx.read_gpr(9)?,
            ];
            let id = *next_id;
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
            *next_id += 1;
            ctx.write_gpr(0, AsmValue::Register(id));
            ctx.write_gpr(1, AsmValue::Undef(AsmType::I64));
            ctx.write_gpr(11, AsmValue::Undef(AsmType::I64));
            Ok(())
        }
        Decoded::Vpbroadcastq { dst, src } => {
            let value = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Splat),
                kind: AsmInstructionKind::Splat {
                    value,
                    lane_bits: 64,
                    lanes: 2,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::ZeroXmm { dst } => {
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::OnesXmm { dst } => {
            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let ones = AsmValue::Constant(AsmConstant::UInt(u64::MAX, AsmType::I64));

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: ones.clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: ones,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vcvtusi2sd {
            dst,
            src_vec,
            src_gpr,
            width_bits,
        } => {
            let src_vec_value = ctx.read_vec(src_vec)?;
            let int_value = value_from_rm_with_width(
                ctx,
                src_gpr,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::UIToFP),
                kind: AsmInstructionKind::UIToFP(int_value, AsmType::F64),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(fp_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: src_vec_value,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert0_id));
            Ok(())
        }
        Decoded::Vcvtusi2ss {
            dst,
            src_vec,
            src_gpr,
            width_bits,
        } => {
            let base_vec = ctx.read_vec(src_vec)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let int_value = value_from_rm_with_width(
                ctx,
                src_gpr,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::UIToFP),
                kind: AsmInstructionKind::UIToFP(int_value, AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(fp_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VmulsdMem { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;

            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
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

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lane0_id), AsmType::F64),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_id)),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: lhs_vec,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::Vdivsd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let lhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
                    lane: 0,
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

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_lane0_id), AsmType::F64),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_lane0_id), AsmType::F64),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: lhs_vec,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VmovupsStore { dst, src } => {
            let src_vec = ctx.read_vec(src)?;

            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec.clone(),
                    lane: 0,
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

            let lane1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
                    lane: 1,
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

            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store0_id = *next_id;
            instructions.push(AsmInstruction {
                id: store0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Register(lane0_id),
                    address: addr.clone(),
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

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let store1_id = *next_id;
            instructions.push(AsmInstruction {
                id: store1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Register(lane1_id),
                    address: AsmValue::Register(addr1_id),
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
            Ok(())
        }
        Decoded::VmovupsLoad { dst, src } => {
            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let load0_id = *next_id;
            instructions.push(AsmInstruction {
                id: load0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr.clone(),
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

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let load1_id = *next_id;
            instructions.push(AsmInstruction {
                id: load1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(addr1_id),
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

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: AsmValue::Register(load0_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: AsmValue::Register(load1_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::VmovssLoad { dst, src } => {
            let dst_vec = ctx.read_vec(dst)?;
            let new_low = value_from_rm_with_width(
                ctx,
                RmOperand::Mem(src),
                32,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: dst_vec.clone(),
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), new_low),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: dst_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VmovssStore { dst, src } => {
            let src_vec = ctx.read_vec(src)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
                    lane: 0,
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

            let stored =
                value_for_store(32, AsmValue::Register(lane0_id), instructions, next_id)?;
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
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
            Ok(())
        }
        Decoded::VcomissMem { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 0,
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

            let mask_id = *next_id;
            instructions.push(build_binop(
                mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(trunc_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::VcomissReg { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let lhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 0,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
                    lane: 0,
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

            let lhs_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_trunc_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_trunc_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: true,
            });
            Ok(())
        }
        Decoded::VaddssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let low_mask_id = *next_id;
            instructions.push(build_binop(
                low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let add_id = *next_id;
            instructions.push(AsmInstruction {
                id: add_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                kind: AsmInstructionKind::Add(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(add_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(bits_i64_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::Vdivss { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(bits_i64_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VdivssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(bits_i64_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::Vcvttss2usi {
            dst,
            src,
            width_bits,
        } => {
            let src_vec = ctx.read_vec(src)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
                    lane: 0,
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

            let mask_id = *next_id;
            instructions.push(build_binop(
                mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(trunc_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let int_id = *next_id;
            instructions.push(AsmInstruction {
                id: int_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPToUI),
                kind: AsmInstructionKind::FPToUI(AsmValue::Register(fp_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            write_gpr_with_width(ctx, dst, AsmValue::Register(int_id), width_bits, instructions, next_id)
        }
        Decoded::Vmulss { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(bits_i64_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VmulssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: 0,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_fp_id)),
                type_hint: Some(AsmType::F32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I32),
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), AsmValue::Register(bits_i64_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::VpxorqXmmMem { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;

            let lhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
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

            let lhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 1,
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

            let base_addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: base_addr.clone(),
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

            let rhs1_addr_id = *next_id;
            instructions.push(build_binop(
                rhs1_addr_id,
                AsmInstructionKind::Add(
                    base_addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let rhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(rhs1_addr_id),
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

            let out0_id = *next_id;
            instructions.push(build_binop(
                out0_id,
                AsmInstructionKind::Xor(AsmValue::Register(lhs0_id), AsmValue::Register(rhs0_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            let out1_id = *next_id;
            instructions.push(build_binop(
                out1_id,
                AsmInstructionKind::Xor(AsmValue::Register(lhs1_id), AsmValue::Register(rhs1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: AsmValue::Register(out0_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: AsmValue::Register(out1_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpmaxuq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Ugt(
                        AsmValue::Register(lhs_id),
                        AsmValue::Register(rhs_id),
                    ),
                    fp_core::asmir::AsmGenericOpcode::Ugt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(lhs_id),
                        if_false: AsmValue::Register(rhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpmaxud { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out = packed_umax_i32x2(
                    AsmValue::Register(lhs_id),
                    AsmValue::Register(rhs_id),
                    instructions,
                    next_id,
                );
                lane_values.push(out);
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpminuq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Ugt(
                        AsmValue::Register(lhs_id),
                        AsmValue::Register(rhs_id),
                    ),
                    fp_core::asmir::AsmGenericOpcode::Ugt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(rhs_id),
                        if_false: AsmValue::Register(lhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpsubq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Sub(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpunpcklwd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 16,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Vpunpckldq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 32,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Vpunpcklqdq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 64,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Vpaddd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out = packed_add_i32x2(
                    AsmValue::Register(lhs_id),
                    AsmValue::Register(rhs_id),
                    instructions,
                    next_id,
                );
                lane_values.push(out);
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpaddq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Add(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vpsrldq { dst, src, imm } => {
            let src_vec = ctx.read_vec(src)?;
            match imm {
                0 => {
                    ctx.write_vec(dst, src_vec);
                    Ok(())
                }
                8 => {
                    let lane1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: lane1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: src_vec,
                            lane: 1,
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

                    let vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Register(lane1_id),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(vec_id));
                    Ok(())
                }
                imm if imm >= 16 => {
                    let zero_vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: zero_vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(zero_vec_id));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 vpsrldq shift amount")),
            }
        }
        Decoded::Vpandq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::And(
                        AsmValue::Register(lhs_id),
                        AsmValue::Register(rhs_id),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vporq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Or(
                        AsmValue::Register(lhs_id),
                        AsmValue::Register(rhs_id),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::Vptest { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let lhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
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
            let lhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 1,
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

            let rhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec.clone(),
                    lane: 0,
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
            let rhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
                    lane: 1,
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

            let and0_id = *next_id;
            instructions.push(build_binop(
                and0_id,
                AsmInstructionKind::And(AsmValue::Register(lhs0_id), AsmValue::Register(rhs0_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            let and1_id = *next_id;
            instructions.push(build_binop(
                and1_id,
                AsmInstructionKind::And(AsmValue::Register(lhs1_id), AsmValue::Register(rhs1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(and0_id), AsmValue::Register(and1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: true,
            });
            Ok(())
        }
        Decoded::Vpalignr { dst, lhs, rhs, imm } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            match imm {
                0 => {
                    ctx.write_vec(dst, lhs_vec);
                    Ok(())
                }
                8 => {
                    let lhs_lane1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: lhs_lane1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: lhs_vec.clone(),
                            lane: 1,
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

                    let rhs_lane0_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: rhs_lane0_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: rhs_vec,
                            lane: 0,
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

                    let insert0_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: insert0_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                        kind: AsmInstructionKind::InsertLane {
                            vector: lhs_vec,
                            lane: 0,
                            value: AsmValue::Register(lhs_lane1_id),
                        },
                        type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let insert1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: insert1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                        kind: AsmInstructionKind::InsertLane {
                            vector: AsmValue::Register(insert0_id),
                            lane: 1,
                            value: AsmValue::Register(rhs_lane0_id),
                        },
                        type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    ctx.write_vec(dst, AsmValue::Register(insert1_id));
                    Ok(())
                }
                16 => {
                    ctx.write_vec(dst, rhs_vec);
                    Ok(())
                }
                imm if imm > 16 => {
                    let zero_vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: zero_vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(zero_vec_id));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 vpalignr immediate")),
            }
        }
        Decoded::Vpmaxsq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Gt(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    fp_core::asmir::AsmGenericOpcode::Gt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(lhs_id),
                        if_false: AsmValue::Register(rhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(())
        }
        Decoded::MovdXmmFromGpr32 { dst, src } => {
            let raw = ctx.read_gpr(src)?;
            let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
            let masked_id = *next_id;
            instructions.push(build_binop(
                masked_id,
                AsmInstructionKind::And(raw, mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(masked_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(())
        }
        Decoded::MovdXmmFromMem32 { dst, src } => {
            if src.segment.is_some() {
                return Ok(());
            }

            let addr = compute_address(
                ctx,
                src,
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
                type_hint: Some(AsmType::I32),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let zext_id = *next_id;
            instructions.push(AsmInstruction {
                id: zext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(load_id), AsmType::I64),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(zext_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(())
        }
        Decoded::MovdMem32FromXmm { dst, src } => {
            if dst.segment.is_some() {
                return Ok(());
            }

            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let stored = value_for_store(32, AsmValue::Register(extract_id), instructions, next_id)?;
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
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
            Ok(())
        }
        Decoded::MovdGpr32FromXmm {
            dst,
            src,
            width_bits,
        } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(extract_id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::Pinsrd {
            dst,
            vector,
            value,
            lane,
        } => {
            if lane > 3 {
                return Err(Error::from("unsupported vpinsrd lane"));
            }
            let base_vec = ctx.read_vec(vector)?;
            let scalar = value_from_rm_with_width(
                ctx,
                value,
                32,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let half = u16::from(lane / 2);
            let part = lane % 2;

            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: half,
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

            let scalar_masked_id = *next_id;
            instructions.push(build_binop(
                scalar_masked_id,
                AsmInstructionKind::And(
                    scalar,
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let (preserve_mask, shifted_scalar) = if part == 0 {
                (
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF_0000_0000, AsmType::I64)),
                    AsmValue::Register(scalar_masked_id),
                )
            } else {
                let shifted_id = *next_id;
                instructions.push(build_binop(
                    shifted_id,
                    AsmInstructionKind::Shl(
                        AsmValue::Register(scalar_masked_id),
                        AsmValue::Constant(AsmConstant::Int(32, AsmType::I64)),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                ));
                *next_id += 1;
                (
                    AsmValue::Constant(AsmConstant::UInt(0x0000_0000_FFFF_FFFF, AsmType::I64)),
                    AsmValue::Register(shifted_id),
                )
            };

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(AsmValue::Register(extract_id), preserve_mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), shifted_scalar),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: half,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::Pinsrb {
            dst,
            vector,
            value,
            lane,
        } => {
            if lane >= 16 {
                return Err(Error::from("unsupported vpinsrb lane"));
            }

            let word_lane = if lane < 8 { 0u16 } else { 1u16 };
            let byte_lane = (lane % 8) as u32;
            let shift_bits = (byte_lane * 8) as i64;

            let base_vec = ctx.read_vec(vector)?;

            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: word_lane,
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

            let raw = value_from_rm_with_width(
                ctx,
                value,
                8,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let masked_byte_id = *next_id;
            instructions.push(build_binop(
                masked_byte_id,
                AsmInstructionKind::And(
                    raw,
                    AsmValue::Constant(AsmConstant::UInt(0xFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let shifted_id = *next_id;
            instructions.push(build_binop(
                shifted_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(masked_byte_id),
                    AsmValue::Constant(AsmConstant::Int(shift_bits, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let mask = !(0xFFu64 << (byte_lane * 8));
            let cleared_id = *next_id;
            instructions.push(build_binop(
                cleared_id,
                AsmInstructionKind::And(
                    AsmValue::Register(extract_id),
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(cleared_id),
                    AsmValue::Register(shifted_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: word_lane,
                    value: AsmValue::Register(merged_id),
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(())
        }
        Decoded::MovqXmmFromMem { dst, src } => {
            if src.segment.is_some() {
                return Ok(());
            }
            let addr = compute_address(
                ctx,
                src,
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
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(load_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(())
        }
        Decoded::MovqXmmFromGpr { dst, src } => {
            let value = ctx.read_gpr(src)?;
            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        value,
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(())
        }
        Decoded::MovqMemFromXmm { dst, src } => {
            if dst.segment.is_some() {
                return Ok(());
            }
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Register(extract_id),
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
            Ok(())
        }
        Decoded::MovqGprFromXmm { dst, src } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(extract_id));
            Ok(())
        }
        Decoded::Pinsrq {
            dst,
            vector,
            value,
            lane,
        } => {
            let base = ctx.read_vec(vector)?;
            let scalar = value_from_rm_with_width(
                ctx,
                value,
                64,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base,
                    lane: lane as u16,
                    value: scalar,
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Pextrq { dst, src, lane } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector,
                    lane: lane as u16,
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
            ctx.write_gpr(dst, AsmValue::Register(extract_id));
            Ok(())
        }
        Decoded::Setcc { dst, condition } => {
            let value = if let Some(compare) = last_compare.as_ref() {
                patch_compare_kind(instructions, compare, condition)?;
                let zext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                    kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I8),
                    type_hint: Some(AsmType::I8),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                AsmValue::Register(zext_id)
            } else {
                // `setcc` can observe flags from arithmetic instructions we
                // do not model yet; conservatively synthesize `0`.
                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I8))
            };
            match dst {
                RmOperand::Reg(reg) => {
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr =
                        compute_address(ctx, memory, inst.offset, inst.len, relocs, instructions, next_id)?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
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
                    Ok(())
                }
            }
        }
        Decoded::MovZx {
            dst,
            src,
            src_width_bits,
            dst_width_bits,
        } => {
            let value = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
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
                            type_hint: Some(match src_width_bits {
                                8 => AsmType::I8,
                                16 => AsmType::I16,
                                _ => AsmType::I32,
                            }),
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };
            let mask_bits = match src_width_bits {
                8 => 0xFF,
                16 => 0xFFFF,
                _ => 0xFFFF_FFFF,
            };
            let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value, mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            let mut result = AsmValue::Register(and_id);
            if dst_width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(result, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                result = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, result);
            Ok(())
        }
        Decoded::Ret
        | Decoded::JmpRel { .. }
        | Decoded::JmpRm { .. }
        | Decoded::JccRel { .. } => Ok(()),
    }
}

fn relocation_at<'a>(relocs: &'a [TextRelocation], offset: u64) -> Option<&'a TextRelocation> {
    relocs.iter().find(|reloc| reloc.offset == offset)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Decoded {
    Nop,
    Ret,
    Hlt,
    Leave,
    PushReg {
        src: u8,
    },
    PushImm {
        imm: i64,
    },
    PushRm {
        src: RmOperand,
    },
    PopReg {
        dst: u8,
    },
    XorReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    XorImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    AndRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    OrRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    AndRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    XorRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AdcImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    SbbImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AddRegRm {
        dst: u8,
        src: RmOperand,
    },
    AddRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    SubRegRm {
        dst: u8,
        src: RmOperand,
    },
    SubRegRmWidth {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    SubRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    AddImm {
        dst: u8,
        imm: i64,
        width_bits: u16,
    },
    AddImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    SubImm {
        dst: u8,
        imm: i64,
        width_bits: u16,
    },
    SubImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    Cmp {
        lhs: Operand,
        rhs: Operand,
        width_bits: u16,
    },
    Test {
        lhs: Operand,
        rhs: Operand,
        width_bits: u16,
    },
    MovImm64 {
        dst: u8,
        imm_offset: usize,
        imm: i64,
    },
    MovImm32ToRm {
        dst: RmOperand,
        imm_offset: usize,
        imm: i32,
    },
    MovImm32ToMem64 {
        dst: X86Memory,
        imm_offset: usize,
        imm: i32,
    },
    MovImm8ToRm {
        dst: RmOperand,
        imm: i8,
    },
    MovImm16ToRm {
        dst: RmOperand,
        imm_offset: usize,
        imm: u16,
    },
    MovSxd {
        dst: u8,
        src: RmOperand,
    },
    MovSx {
        dst: u8,
        src: RmOperand,
        src_width_bits: u16,
        dst_width_bits: u16,
    },
    DivRm {
        src: RmOperand,
        signed: bool,
        width_bits: u16,
    },
    Lea {
        dst: u8,
        src: X86Memory,
        width_bits: u16,
    },
    MovRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    MovRegToRm {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    MovbeRegFromMem {
        dst: u8,
        src: X86Memory,
        width_bits: u16,
    },
    MovbeMemFromReg {
        dst: X86Memory,
        src: u8,
        width_bits: u16,
    },
    Bswap {
        dst: u8,
        width_bits: u16,
    },
    CallRel32 {
        imm_offset: usize,
        target: u64,
    },
    CallRm {
        target: RmOperand,
    },
    IncRm {
        target: RmOperand,
        width_bits: u16,
    },
    DecRm {
        target: RmOperand,
        width_bits: u16,
    },
    JmpRel {
        target: u64,
    },
    JmpRm {
        target: RmOperand,
    },
    JccRel {
        condition: u8,
        target: u64,
    },
    Syscall,
    Vpbroadcastq {
        dst: u8,
        src: u8,
    },
    ZeroXmm {
        dst: u8,
    },
    OnesXmm {
        dst: u8,
    },
    Vcvtusi2sd {
        dst: u8,
        src_vec: u8,
        src_gpr: RmOperand,
        width_bits: u16,
    },
    Vcvtusi2ss {
        dst: u8,
        src_vec: u8,
        src_gpr: RmOperand,
        width_bits: u16,
    },
    VmulsdMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vdivsd {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VmovupsStore {
        dst: X86Memory,
        src: u8,
    },
    VmovupsLoad {
        dst: u8,
        src: X86Memory,
    },
    VmovssLoad {
        dst: u8,
        src: X86Memory,
    },
    VmovssStore {
        dst: X86Memory,
        src: u8,
    },
    VcomissMem {
        lhs: u8,
        rhs: X86Memory,
    },
    VcomissReg {
        lhs: u8,
        rhs: u8,
    },
    VaddssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vdivss {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VdivssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vcvttss2usi {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    Vmulss {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VmulssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    VpxorqXmmMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vptest {
        lhs: u8,
        rhs: u8,
    },
    VptestMem {
        lhs: u8,
        rhs: X86Memory,
    },
    Vpalignr {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
        imm: u8,
    },
    Vpmaxsq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpmaxuq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpmaxud {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpminuq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpsubq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpaddd {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpaddq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpsrldq {
        dst: u8,
        src: u8,
        imm: u8,
    },
    Vpandq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vporq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpcklwd {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpckldq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpcklqdq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    MovdXmmFromGpr32 {
        dst: u8,
        src: u8,
    },
    MovdXmmFromMem32 {
        dst: u8,
        src: X86Memory,
    },
    MovdMem32FromXmm {
        dst: X86Memory,
        src: u8,
    },
    MovdGpr32FromXmm {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    Pinsrd {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    Pinsrb {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    MovqXmmFromMem {
        dst: u8,
        src: X86Memory,
    },
    MovqXmmFromGpr {
        dst: u8,
        src: u8,
    },
    MovqMemFromXmm {
        dst: X86Memory,
        src: u8,
    },
    MovqGprFromXmm {
        dst: u8,
        src: u8,
    },
    Pinsrq {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    Pextrq {
        dst: u8,
        src: u8,
        lane: u8,
    },
    BtReg {
        value: u8,
        bit: u8,
    },
    BtImm {
        value: u8,
        imm: u8,
    },
    BtcImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    Cqo,
    Cdq,
    Cdqe,
    ShlImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    ShrImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    Shrx {
        dst: u8,
        src: RmOperand,
        shift: RmOperand,
        width_bits: u16,
    },
    Shlx {
        dst: u8,
        src: RmOperand,
        shift: RmOperand,
        width_bits: u16,
    },
    Rorx {
        dst: u8,
        src: RmOperand,
        imm: u16,
        width_bits: u16,
    },
    Blsr {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    SarImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    NotRm {
        dst: RmOperand,
        width_bits: u16,
    },
    NegRm {
        dst: RmOperand,
        width_bits: u16,
    },
    SbbSelf {
        reg: u8,
        width_bits: u16,
    },
    OrImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    ImulReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    ImulRegImm {
        dst: u8,
        src: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    ImulRmWide {
        src: RmOperand,
        width_bits: u16,
    },
    MulRm {
        src: RmOperand,
        width_bits: u16,
    },
    Fild {
        src: X86Memory,
        width_bits: u16,
    },
    FldSt {
        index: u8,
    },
    FldMem {
        src: X86Memory,
        width_bits: u16,
    },
    Fxch {
        index: u8,
    },
    Fdivrp {
        index: u8,
    },
    Fdivp {
        index: u8,
    },
    Fmulp {
        index: u8,
    },
    FmulSt0St {
        index: u8,
    },
    Fcomi {
        index: u8,
    },
    Fcomip {
        index: u8,
    },
    FstpSt {
        index: u8,
    },
    FstpMem {
        dst: X86Memory,
        width_bits: u16,
    },
    Fisttp {
        dst: X86Memory,
        width_bits: u16,
    },
    FaddMem {
        src: X86Memory,
        width_bits: u16,
    },
    Ffreep {
        index: u8,
    },
    FsubrSt0St {
        index: u8,
    },
    Fcmovcc {
        condition: u8,
        src: u8,
    },
    Cmovcc {
        dst: u8,
        src: RmOperand,
        condition: u8,
        width_bits: u16,
    },
    Setcc {
        dst: RmOperand,
        condition: u8,
    },
    MovZx {
        dst: u8,
        src: RmOperand,
        src_width_bits: u16,
        dst_width_bits: u16,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operand {
    Rm(RmOperand),
    Imm(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RmOperand {
    Reg(u8),
    Mem(X86Memory),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VecOperand {
    Reg(u8),
    Mem(X86Memory),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct X86Memory {
    base: Option<u8>,
    index: Option<u8>,
    scale: u8,
    displacement: i64,
    displacement_offset: Option<usize>,
    segment: Option<X86Segment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum X86Segment {
    Fs,
    Gs,
}

fn decode_instruction(bytes: &[u8], offset: u64) -> Result<Option<(Decoded, usize)>> {
    if bytes.is_empty() {
        return Ok(None);
    }

    // Intel CET indirect branch tracking marker.
    // Treat as a NOP for lifting purposes.
    if bytes.starts_with(&[0xF3, 0x0F, 0x1E, 0xFA]) {
        return Ok(Some((Decoded::Nop, 4)));
    }

    let mut segment = None;
    let mut operand_size_override = false;
    let mut opcode_index = 0usize;
    while let Some(prefix) = bytes.get(opcode_index).copied() {
        match prefix {
            // Branch hints / segment overrides that show up in CET-enabled code.
            0x2E | 0x3E | 0x26 | 0x36 => {
                opcode_index += 1;
            }
            0x64 => {
                segment = Some(X86Segment::Fs);
                opcode_index += 1;
            }
            0x65 => {
                segment = Some(X86Segment::Gs);
                opcode_index += 1;
            }
            0x66 => {
                operand_size_override = true;
                opcode_index += 1;
            }
            0xF0 | 0xF2 | 0xF3 | 0x67 => {
                opcode_index += 1;
            }
            _ => break,
        }
    }

    // Minimal prefix handling: accept a REX prefix (0x40..0x4F).
    let (rex_w, rex_r, rex_x, rex_b, opcode_index) = match bytes.get(opcode_index).copied() {
        Some(rex @ 0x40..=0x4F) => (
            ((rex >> 3) & 1) != 0,
            ((rex >> 2) & 1) != 0,
            ((rex >> 1) & 1) != 0,
            (rex & 1) != 0,
            opcode_index + 1,
        ),
        _ => (false, false, false, false, opcode_index),
    };
    let Some(opcode) = bytes.get(opcode_index).copied() else {
        return Ok(None);
    };

    if opcode == 0x90 {
        return Ok(Some((Decoded::Nop, opcode_index + 1)));
    }
    if opcode == 0xC3 {
        return Ok(Some((Decoded::Ret, opcode_index + 1)));
    }

    // PUSH imm8/imm32.
    if opcode == 0x6A {
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated push imm8"))? as i8;
        return Ok(Some((
            Decoded::PushImm { imm: imm as i64 },
            opcode_index + 2,
        )));
    }
    if opcode == 0x68 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::PushImm { imm: imm as i64 },
            opcode_index + 5,
        )));
    }

    // Unsigned/signed division: F7 /6 (div) and F7 /7 (idiv).
    if opcode == 0xF7 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if ext == 6 || ext == 7 {
            return Ok(Some((
                Decoded::DivRm {
                    src: rm,
                    signed: ext == 7,
                    width_bits: if rex_w { 64 } else { 32 },
                },
                opcode_index + 1 + consumed,
            )));
        }
    }

    // PUSH/POP r64.
    if (0x50..=0x57).contains(&opcode) {
        let mut src = opcode - 0x50;
        if rex_b {
            src = src.saturating_add(8);
        }
        return Ok(Some((Decoded::PushReg { src }, opcode_index + 1)));
    }
    if (0x58..=0x5F).contains(&opcode) {
        let mut dst = opcode - 0x58;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        return Ok(Some((Decoded::PopReg { dst }, opcode_index + 1)));
    }

    if (0x70..=0x7F).contains(&opcode) {
        // Jcc rel8.
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated rel8"))? as i8;
        let len = opcode_index + 2;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm as i64);
        if target < 0 {
            return Err(Error::from("x86_64 jcc target underflow"));
        }
        return Ok(Some((
            Decoded::JccRel {
                condition: opcode & 0x0F,
                target: target as u64,
            },
            len,
        )));
    }

    // MOV r/m8, imm8: C6 /0 imm8.
    if opcode == 0xC6 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if reg != 0 {
            return Err(Error::from("unsupported x86_64 C6 opcode extension"));
        }
        let imm_offset = opcode_index + 1 + consumed;
        let imm = *bytes
            .get(imm_offset)
            .ok_or_else(|| Error::from("truncated imm8"))? as i8;
        return Ok(Some((
            Decoded::MovImm8ToRm { dst: rm, imm },
            imm_offset + 1,
        )));
    }

    if opcode == 0x0F {
        let ext = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated 0f opcode"))?;
        if ext == 0x1F {
            // Multi-byte NOP: 0F 1F /0.
            let (_ext_reg, _rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            return Ok(Some((Decoded::Nop, opcode_index + 2 + consumed)));
        }
        if ext == 0x05 {
            return Ok(Some((Decoded::Syscall, opcode_index + 2)));
        }
        if (0x80..=0x8F).contains(&ext) {
            // Jcc rel32.
            let imm = read_i32(bytes, opcode_index + 2)? as i64;
            let len = opcode_index + 2 + 4;
            let target = (offset as i64)
                .saturating_add(len as i64)
                .saturating_add(imm);
            if target < 0 {
                return Err(Error::from("x86_64 jcc target underflow"));
            }
            return Ok(Some((
                Decoded::JccRel {
                    condition: ext & 0x0F,
                    target: target as u64,
                },
                len,
            )));
        }
        if (0x90..=0x9F).contains(&ext) {
            // SETcc r/m8.
            let (ext_reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            // ext_reg is the ModRM.reg field; for SETcc it is part of the encoding
            // but should be 0.
            let _ = ext_reg;
            return Ok(Some((
                Decoded::Setcc {
                    dst: rm,
                    condition: ext & 0x0F,
                },
                opcode_index + 2 + consumed,
            )));
        }
        if ext == 0xB6 || ext == 0xB7 {
            // MOVZX r, r/m8|r/m16.
            let (reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            let src_width_bits = if ext == 0xB6 { 8 } else { 16 };
            let dst_width_bits = if rex_w { 64 } else { 32 };
            return Ok(Some((
                Decoded::MovZx {
                    dst: reg,
                    src: rm,
                    src_width_bits,
                    dst_width_bits,
                },
                opcode_index + 2 + consumed,
            )));
        }
        if ext == 0xBE || ext == 0xBF {
            // MOVSX r, r/m8|r/m16.
            let (reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            let src_width_bits = if ext == 0xBE { 8 } else { 16 };
            let dst_width_bits = if rex_w { 64 } else { 32 };
            return Ok(Some((
                Decoded::MovSx {
                    dst: reg,
                    src: rm,
                    src_width_bits,
                    dst_width_bits,
                },
                opcode_index + 2 + consumed,
            )));
        }
    }

    if opcode == 0xEB {
        // JMP rel8.
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated rel8"))? as i8;
        let len = opcode_index + 2;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm as i64);
        if target < 0 {
            return Err(Error::from("x86_64 jmp target underflow"));
        }
        return Ok(Some((
            Decoded::JmpRel {
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xE9 {
        // JMP rel32.
        let imm = read_i32(bytes, opcode_index + 1)? as i64;
        let len = opcode_index + 1 + 4;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm);
        if target < 0 {
            return Err(Error::from("x86_64 jmp target underflow"));
        }
        return Ok(Some((
            Decoded::JmpRel {
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xE8 {
        // CALL rel32.
        let imm = read_i32(bytes, opcode_index + 1)? as i64;
        let len = opcode_index + 1 + 4;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm);
        if target < 0 {
            return Err(Error::from("x86_64 call target underflow"));
        }
        return Ok(Some((
            Decoded::CallRel32 {
                imm_offset: opcode_index + 1,
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xFF {
        // Group 5.
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        match ext {
            0 => {
                // INC r/m.
                let width_bits = if rex_w { 64 } else { 32 };
                return Ok(Some((
                    Decoded::IncRm { target: rm, width_bits },
                    opcode_index + 1 + consumed,
                )));
            }
            1 => {
                // DEC r/m.
                let width_bits = if rex_w { 64 } else { 32 };
                return Ok(Some((
                    Decoded::DecRm { target: rm, width_bits },
                    opcode_index + 1 + consumed,
                )));
            }
            2 => {
                // CALL r/m64.
                return Ok(Some((
                    Decoded::CallRm { target: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            4 => {
                // JMP r/m64.
                return Ok(Some((
                    Decoded::JmpRm { target: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            6 => {
                // PUSH r/m.
                return Ok(Some((
                    Decoded::PushRm { src: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            _ => {}
        }
    }

    // MOV r64, imm64: REX.W B8+rd imm64.
    if rex_w && (0xB8..=0xBF).contains(&opcode) {
        let mut dst = opcode - 0xB8;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        let imm = read_i64(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::MovImm64 {
                dst,
                imm_offset: opcode_index + 1,
                imm,
            },
            opcode_index + 1 + 8,
        )));
    }

    // MOV r32, imm32: B8+rd imm32 (zero-extended).
    if !rex_w && (0xB8..=0xBF).contains(&opcode) {
        let mut dst = opcode - 0xB8;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        let imm = read_i32(bytes, opcode_index + 1)? as u32 as i64;
        return Ok(Some((
            Decoded::MovImm64 {
                dst,
                imm_offset: opcode_index + 1,
                imm,
            },
            opcode_index + 1 + 4,
        )));
    }

    // MOV r/m, imm32: C7 /0 imm32 (sign-extended for 64-bit destinations).
    if opcode == 0xC7 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if reg != 0 {
            return Err(Error::from("unsupported x86_64 C7 opcode extension"));
        }
        let imm_offset = opcode_index + 1 + consumed;
        if !rex_w && operand_size_override {
            let imm = read_u16(bytes, imm_offset)?;
            return Ok(Some((
                Decoded::MovImm16ToRm {
                    dst: rm,
                    imm_offset,
                    imm,
                },
                imm_offset + 2,
            )));
        }
        let imm = read_i32(bytes, imm_offset)?;
        if rex_w {
            // Treat this as a sign-extending write to the full 64-bit register.
            match rm {
                RmOperand::Reg(dst) => {
                    return Ok(Some((
                        Decoded::MovImm64 {
                            dst,
                            imm_offset,
                            imm: imm as i64,
                        },
                        imm_offset + 4,
                    )));
                }
                RmOperand::Mem(memory) => {
                    return Ok(Some((
                        Decoded::MovImm32ToMem64 {
                            dst: memory,
                            imm_offset,
                            imm,
                        },
                        imm_offset + 4,
                    )));
                }
            }
        }
        return Ok(Some((
            Decoded::MovImm32ToRm {
                dst: rm,
                imm_offset,
                imm,
            },
            imm_offset + 4,
        )));
    }

    // LEA r, m: 8D /r.
    if opcode == 0x8D {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let RmOperand::Mem(memory) = rm else {
            return Err(Error::from("unsupported x86_64 lea with register operand"));
        };
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::Lea {
                dst: reg,
                src: memory,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD rax, imm32: REX.W 05 imm32.
    if rex_w && opcode == 0x05 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::AddImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // ADD eax, imm32: 05 imm32.
    if !rex_w && opcode == 0x05 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::AddImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // SUB rax, imm32: REX.W 2D imm32.
    if rex_w && opcode == 0x2D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::SubImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // SUB eax, imm32: 2D imm32.
    if !rex_w && opcode == 0x2D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::SubImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // ADD r64, r/m64: REX.W 03 /r.
    if rex_w && opcode == 0x03 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::AddRegRm { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // SUB r64, r/m64: REX.W 2B /r.
    if rex_w && opcode == 0x2B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::SubRegRm { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // MOVSXD r64, r/m32: 63 /r.
    if opcode == 0x63 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovSxd { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD r/m, r: 01 /r.
    if opcode == 0x01 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::AddRmReg {
                dst: rm,
                src: reg,
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // SUB r/m, r: 29 /r.
    if opcode == 0x29 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::SubRmReg {
                dst: rm,
                src: reg,
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // CMP rax, imm32: REX.W 3D imm32.
    if rex_w && opcode == 0x3D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(0)),
                rhs: Operand::Imm(imm as i64),
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP eax, imm32: 3D imm32.
    if !rex_w && opcode == 0x3D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(0)),
                rhs: Operand::Imm(imm as i64),
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP r/m, r: 39 /r.
    if opcode == 0x39 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // CMP r, r/m: 3B /r.
    if opcode == 0x3B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(reg)),
                rhs: Operand::Rm(rm),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // TEST r/m, r: 85 /r.
    if opcode == 0x85 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Test {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // TEST r/m8, r8: 84 /r.
    if opcode == 0x84 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Test {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // XOR r/m, r.
    if opcode == 0x30 || opcode == 0x31 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x30 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 xor to memory"));
        };
        return Ok(Some((
            Decoded::XorReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // XOR r, r/m.
    if opcode == 0x32 || opcode == 0x33 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x32 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::XorRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // AND r/m, r.
    if opcode == 0x20 || opcode == 0x21 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x20 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 and to memory"));
        };
        return Ok(Some((
            Decoded::AndReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // AND r, r/m.
    if opcode == 0x22 || opcode == 0x23 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x22 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::AndRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // OR r/m, r.
    if opcode == 0x08 || opcode == 0x09 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x08 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 or to memory"));
        };
        return Ok(Some((
            Decoded::OrReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // OR r, r/m.
    if opcode == 0x0A || opcode == 0x0B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x0A {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::OrRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD/SUB/CMP r/m, imm8|imm32: 83/81 /0, /5, /7.
    if opcode == 0x83 || opcode == 0x81 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let imm_offset = opcode_index + 1 + consumed;
        let (imm, imm_len) = if opcode == 0x83 {
            let imm8 = *bytes
                .get(imm_offset)
                .ok_or_else(|| Error::from("truncated imm8"))? as i8;
            (imm8 as i64, 1usize)
        } else {
            (read_i32(bytes, imm_offset)? as i64, 4usize)
        };
        let len = imm_offset + imm_len;
        let width_bits = if rex_w { 64 } else { 32 };
        match (ext, rm) {
            (0, RmOperand::Reg(dst)) => {
                return Ok(Some((Decoded::AddImm { dst, imm, width_bits }, len)));
            }
            (0, rm) => {
                return Ok(Some((Decoded::AddImmRm { dst: rm, imm, width_bits }, len)));
            }
            (5, RmOperand::Reg(dst)) => {
                return Ok(Some((Decoded::SubImm { dst, imm, width_bits }, len)));
            }
            (5, rm) => {
                return Ok(Some((Decoded::SubImmRm { dst: rm, imm, width_bits }, len)));
            }
            (2, rm) => {
                return Ok(Some((
                    Decoded::AdcImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (3, rm) => {
                return Ok(Some((
                    Decoded::SbbImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (1, rm) => {
                return Ok(Some((Decoded::OrImm { dst: rm, imm, width_bits }, len)));
            }
            (4, rm) => {
                return Ok(Some((Decoded::AndImm { dst: rm, imm, width_bits }, len)));
            }
            (6, rm) => {
                return Ok(Some((Decoded::XorImm { dst: rm, imm, width_bits }, len)));
            }
            (7, rm) => {
                return Ok(Some((
                    Decoded::Cmp {
                        lhs: Operand::Rm(rm),
                        rhs: Operand::Imm(imm),
                        width_bits,
                    },
                    len,
                )));
            }
            _ => {}
        }
    }

    // Group1 r/m8, imm8: 80 /ext imm8.
    if opcode == 0x80 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let imm_offset = opcode_index + 1 + consumed;
        let imm = *bytes
            .get(imm_offset)
            .ok_or_else(|| Error::from("truncated imm8"))? as i8;
        let len = imm_offset + 1;
        match ext {
            0 => return Ok(Some((Decoded::AddImmRm { dst: rm, imm: imm as i64, width_bits: 8 }, len))),
            1 => return Ok(Some((Decoded::OrImm { dst: rm, imm: imm as i64, width_bits: 8 }, len))),
            4 => return Ok(Some((Decoded::AndImm { dst: rm, imm: imm as i64, width_bits: 8 }, len))),
            5 => return Ok(Some((Decoded::SubImmRm { dst: rm, imm: imm as i64, width_bits: 8 }, len))),
            6 => return Ok(Some((Decoded::XorImm { dst: rm, imm: imm as i64, width_bits: 8 }, len))),
            7 => {
                return Ok(Some((
                    Decoded::Cmp {
                        lhs: Operand::Rm(rm),
                        rhs: Operand::Imm(imm as i64),
                        width_bits: 8,
                    },
                    len,
                )))
            }
            _ => {}
        }
    }

    // MOV r, r/m: 8B /r (32-bit by default, 64-bit with REX.W).
    if opcode == 0x8B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::MovRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r8, r/m8: 8A /r.
    if opcode == 0x8A {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovRmToReg {
                dst: reg,
                src: rm,
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r/m, r: 89 /r (32-bit by default, 64-bit with REX.W).
    if opcode == 0x89 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::MovRegToRm {
                dst: rm,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r/m8, r8: 88 /r.
    if opcode == 0x88 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovRegToRm {
                dst: rm,
                src: reg,
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    Ok(None)
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

fn lift_rm_imm_binop(
    ctx: &mut RegisterLiftContext,
    dst: RmOperand,
    imm: i64,
    width_bits: u16,
    inst: DecodedInstruction,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    opcode: fp_core::asmir::AsmGenericOpcode,
) -> Result<()> {
    let lhs = value_from_rm_with_width(
        ctx,
        dst,
        width_bits,
        inst,
        relocs,
        instructions,
        next_id,
    )?;
    let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));

    let id = *next_id;
    let kind = match opcode {
        fp_core::asmir::AsmGenericOpcode::Add => AsmInstructionKind::Add(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Sub => AsmInstructionKind::Sub(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::And => AsmInstructionKind::And(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Or => AsmInstructionKind::Or(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Xor => AsmInstructionKind::Xor(lhs, rhs),
        _ => return Err(Error::from("unsupported rm+imm binop opcode")),
    };
    instructions.push(build_binop(id, kind, AsmOpcode::Generic(opcode)));
    *next_id += 1;

    match dst {
        RmOperand::Reg(dst_reg) => write_gpr_with_width(
            ctx,
            dst_reg,
            AsmValue::Register(id),
            width_bits,
            instructions,
            next_id,
        ),
        RmOperand::Mem(memory) => {
            if memory.segment.is_some() {
                return Ok(());
            }
            let stored = value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
            let addr = compute_address(
                ctx,
                memory,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
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
            Ok(())
        }
    }
}

fn read_i32(bytes: &[u8], index: usize) -> Result<i32> {
    let imm = bytes
        .get(index..index + 4)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(i32::from_le_bytes(imm.try_into().unwrap()))
}

fn read_i64(bytes: &[u8], index: usize) -> Result<i64> {
    let imm = bytes
        .get(index..index + 8)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(i64::from_le_bytes(imm.try_into().unwrap()))
}

fn decode_modrm(
    bytes: &[u8],
    index: usize,
    rex_r: bool,
    rex_x: bool,
    rex_b: bool,
    segment: Option<X86Segment>,
) -> Result<(u8, RmOperand, usize)> {
    let modrm = *bytes
        .get(index)
        .ok_or_else(|| Error::from("missing modrm"))?;
    let mode = (modrm >> 6) & 0b11;
    let reg3 = (modrm >> 3) & 0b111;
    let rm3 = modrm & 0b111;
    let mut reg = reg3;
    if rex_r {
        reg = reg.saturating_add(8);
    }
    let mut consumed = 1usize;

    if mode == 0b11 {
        let mut rm = rm3;
        if rex_b {
            rm = rm.saturating_add(8);
        }
        return Ok((reg, RmOperand::Reg(rm), consumed));
    }

    let mut base = if mode == 0b00 && rm3 == 0b101 {
        None
    } else {
        let mut rm = rm3;
        if rex_b {
            rm = rm.saturating_add(8);
        }
        Some(rm)
    };
    let mut index_reg = None;
    let mut scale = 1u8;
    let mut displacement = 0i64;
    let mut displacement_offset = None;

    if rm3 == 0b100 {
        let sib = *bytes
            .get(index + consumed)
            .ok_or_else(|| Error::from("missing sib"))?;
        consumed += 1;
        let scale_bits = (sib >> 6) & 0b11;
        scale = 1u8 << scale_bits;
        let mut index_bits = (sib >> 3) & 0b111;
        let base_bits = sib & 0b111;
        if rex_x {
            index_bits = index_bits.saturating_add(8);
        }
        if index_bits != 0b100 {
            index_reg = Some(index_bits);
        }
        base = if mode == 0b00 && base_bits == 0b101 {
            None
        } else {
            let mut base_reg = base_bits;
            if rex_b {
                base_reg = base_reg.saturating_add(8);
            }
            Some(base_reg)
        };
    }

    match mode {
        0b00 => {
            if base.is_none() {
                displacement_offset = Some(index + consumed);
                displacement = read_i32(bytes, index + consumed)? as i64;
                consumed += 4;
            }
        }
        0b01 => {
            let disp8 = *bytes
                .get(index + consumed)
                .ok_or_else(|| Error::from("missing disp8"))? as i8;
            displacement = disp8 as i64;
            consumed += 1;
        }
        0b10 => {
            displacement = read_i32(bytes, index + consumed)? as i64;
            consumed += 4;
        }
        _ => {}
    }

    Ok((
        reg,
        RmOperand::Mem(X86Memory {
            base,
            index: index_reg,
            scale,
            displacement,
            displacement_offset,
            segment,
        }),
        consumed,
    ))
}

fn vec_operand_value(
    ctx: &mut RegisterLiftContext,
    operand: VecOperand,
    instruction_offset: u64,
    instruction_len: usize,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        VecOperand::Reg(reg) => ctx.read_vec(reg),
        VecOperand::Mem(memory) => {
            if memory.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                    kind: AsmInstructionKind::BuildVector {
                        elements: vec![
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        ],
                    },
                    type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
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

            let addr = compute_address(
                ctx,
                memory,
                instruction_offset,
                instruction_len,
                relocs,
                instructions,
                next_id,
            )?;

            let load0_id = *next_id;
            instructions.push(AsmInstruction {
                id: load0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr.clone(),
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

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let load1_id = *next_id;
            instructions.push(AsmInstruction {
                id: load1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(addr1_id),
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

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![AsmValue::Register(load0_id), AsmValue::Register(load1_id)],
                },
                type_hint: Some(AsmType::Vector(Box::new(AsmType::I64), 2)),
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
    }
}

fn compute_address(
    ctx: &mut RegisterLiftContext,
    memory: X86Memory,
    instruction_offset: u64,
    instruction_len: usize,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    if let Some(displacement_offset) = memory.displacement_offset {
        let relocation_offset = instruction_offset
            .checked_add(displacement_offset as u64)
            .ok_or_else(|| Error::from("x86_64 relocation offset overflow"))?;
        if let Some(reloc) = relocation_at(relocs, relocation_offset) {
            if reloc.addend == 0 && memory.displacement == 0 {
                if let Some(text) = ctx.rodata_cstrings.get(&reloc.symbol) {
                    return Ok(AsmValue::Constant(AsmConstant::String(text.clone())));
                }
            }
            if reloc.kind != object::RelocationKind::Relative
                && reloc.kind != object::RelocationKind::Absolute
            {
                return Err(Error::from(
                    "unsupported x86_64 relocation kind for address",
                ));
            }
            if reloc.encoding != object::RelocationEncoding::X86RipRelative
                && reloc.encoding != object::RelocationEncoding::X86RipRelativeMovq
                && reloc.encoding != object::RelocationEncoding::Generic
                && reloc.encoding != object::RelocationEncoding::Unknown
            {
                return Err(Error::from(
                    "unsupported x86_64 relocation encoding for address",
                ));
            }
            let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                Name::new(reloc.symbol.clone()),
                AsmType::Ptr(Box::new(AsmType::I8)),
                vec![0],
            ));
            let symbol_id = *next_id;
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
            *next_id += 1;

            let mut addr = AsmValue::Register(symbol_id);
            let mut addend = reloc.addend.saturating_add(memory.displacement);
            if reloc.kind == object::RelocationKind::Relative
                || reloc.kind == object::RelocationKind::PltRelative
            {
                // x86_64 RIP-relative address computations are based on the
                // next-instruction address, but relocation addends are defined
                // relative to the relocation field itself. Adjust by the
                // delta between the instruction end and the relocation field.
                let correction = instruction_len as i64 - displacement_offset as i64;
                addend = addend.saturating_add(correction);
            }
            if addend != 0 {
                let rhs = AsmValue::Constant(AsmConstant::Int(addend, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(addr, rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                addr = AsmValue::Register(id);
            }

            return Ok(addr);
        }

        if memory.base == Some(16) && memory.index.is_none() {
            if let Some(symbol) =
                ctx.resolve_rip_symbol(&memory, instruction_offset, instruction_len)
            {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(symbol.name.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
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
                *next_id += 1;
                return Ok(AsmValue::Register(symbol_id));
            }

            let next_ip = (ctx.code_base_address as i64)
                .saturating_add(instruction_offset as i64)
                .saturating_add(instruction_len as i64);
            let absolute = next_ip.saturating_add(memory.displacement);
            if absolute < 0 {
                return Err(Error::from("x86_64 RIP-relative address underflow"));
            }

            if let Some((region, offset)) = ctx.resolve_data_region(absolute as u64) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(region.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
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
                *next_id += 1;

                if offset == 0 {
                    return Ok(AsmValue::Register(symbol_id));
                }

                let rhs = AsmValue::Constant(AsmConstant::Int(offset as i64, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(AsmValue::Register(symbol_id), rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                return Ok(AsmValue::Register(id));
            }

            let addr_const = AsmValue::Constant(AsmConstant::UInt(absolute as u64, AsmType::I64));
            let addr_id = *next_id;
            instructions.push(AsmInstruction {
                id: addr_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(addr_const),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            return Ok(AsmValue::Register(addr_id));
        }

        // `mod=00 rm=101 disp32` is RIP-relative addressing on x86_64.
        // ELF executables frequently use it without a relocation (e.g., for
        // local data). For now, treat the computed absolute address as an
        // immediate pointer value. This avoids inventing a fake symbol that
        // later stages cannot resolve when producing fully-linked executables.
        if memory.base.is_none() && memory.index.is_none() {
            let next_ip = (ctx.code_base_address as i64)
                .saturating_add(instruction_offset as i64)
                .saturating_add(instruction_len as i64);
            let absolute = next_ip.saturating_add(memory.displacement);

            if let Some(symbol) =
                ctx.resolve_disp32_symbol(&memory, instruction_offset, instruction_len)
            {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(symbol.name.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
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
                *next_id += 1;
                return Ok(AsmValue::Register(symbol_id));
            }

            if absolute < 0 {
                return Err(Error::from("x86_64 RIP-relative address underflow"));
            }

            if let Some((region, offset)) = ctx.resolve_data_region(absolute as u64) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(region.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
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
                *next_id += 1;

                if offset == 0 {
                    return Ok(AsmValue::Register(symbol_id));
                }

                let rhs = AsmValue::Constant(AsmConstant::Int(offset as i64, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(AsmValue::Register(symbol_id), rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                return Ok(AsmValue::Register(id));
            }
            let addr_const = AsmValue::Constant(AsmConstant::UInt(absolute as u64, AsmType::I64));
            let addr_id = *next_id;
            instructions.push(AsmInstruction {
                id: addr_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(addr_const),
                type_hint: Some(AsmType::I64),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            return Ok(AsmValue::Register(addr_id));
        }
    }

    let mut addr = match memory.base {
        Some(base) => ctx.read_gpr(base)?,
        None => AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
    };

    if let Some(index_reg) = memory.index {
        let mut index_value = ctx.read_gpr(index_reg)?;
        if memory.scale != 1 {
            let rhs = AsmValue::Constant(AsmConstant::Int(memory.scale as i64, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(index_value.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            index_value = AsmValue::Register(id);
        }

        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Add(addr, index_value),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        ));
        *next_id += 1;
        addr = AsmValue::Register(id);
    }

    if memory.displacement != 0 {
        let rhs = AsmValue::Constant(AsmConstant::Int(memory.displacement, AsmType::I64));
        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Add(addr, rhs),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        ));
        *next_id += 1;
        addr = AsmValue::Register(id);
    }

    Ok(addr)
}

struct RegisterLiftContext {
    locals: Vec<AsmLocal>,
    locals_by_register: std::collections::HashMap<u8, u32>,
    registers: std::collections::HashMap<u8, AsmValue>,
    vec_locals_by_register: std::collections::HashMap<u8, u32>,
    vec_registers: std::collections::HashMap<u8, AsmValue>,
    x87_stack: Vec<AsmValue>,
    next_local_id: u32,
    code_base_address: u64,
    rip_symbols: HashMap<u64, RipSymbol>,
    rodata_cstrings: HashMap<String, String>,
    rodata_cstrings_by_addr: HashMap<u64, String>,
    data_regions: Vec<DataRegion>,
    direct_call_targets: Vec<u64>,
}

impl RegisterLiftContext {
    fn new(
        code_base_address: u64,
        rip_symbols: Option<&HashMap<u64, RipSymbol>>,
        rodata_cstrings: Option<&HashMap<String, String>>,
        rodata_cstrings_by_addr: Option<&HashMap<u64, String>>,
        data_regions: Option<&[DataRegion]>,
    ) -> Self {
        Self {
            locals: Vec::new(),
            locals_by_register: std::collections::HashMap::new(),
            registers: std::collections::HashMap::new(),
            vec_locals_by_register: std::collections::HashMap::new(),
            vec_registers: std::collections::HashMap::new(),
            x87_stack: Vec::new(),
            next_local_id: 0,
            code_base_address,
            rip_symbols: rip_symbols.cloned().unwrap_or_default(),
            rodata_cstrings: rodata_cstrings.cloned().unwrap_or_default(),
            rodata_cstrings_by_addr: rodata_cstrings_by_addr.cloned().unwrap_or_default(),
            data_regions: data_regions.map(|regions| regions.to_vec()).unwrap_or_default(),
            direct_call_targets: Vec::new(),
        }
    }

    fn resolve_data_region(&self, address: u64) -> Option<(&DataRegion, u64)> {
        self.data_regions.iter().find_map(|region| {
            if address >= region.start && address < region.end {
                Some((region, address - region.start))
            } else {
                None
            }
        })
    }

    fn resolve_rip_symbol(
        &self,
        memory: &X86Memory,
        inst_offset: u64,
        inst_len: usize,
    ) -> Option<&RipSymbol> {
        if memory.base != Some(16) || memory.index.is_some() {
            return None;
        }
        let pc = (self.code_base_address as i64)
            .checked_add(inst_offset as i64)?
            .checked_add(inst_len as i64)?;
        let target = pc.checked_add(memory.displacement)? as u64;
        self.rip_symbols.get(&target)
    }

    fn resolve_disp32_symbol(
        &self,
        memory: &X86Memory,
        inst_offset: u64,
        inst_len: usize,
    ) -> Option<&RipSymbol> {
        if memory.base.is_some() || memory.index.is_some() {
            return None;
        }
        let pc = (self.code_base_address as i64)
            .checked_add(inst_offset as i64)?
            .checked_add(inst_len as i64)?;
        let target = pc.checked_add(memory.displacement)? as u64;
        self.rip_symbols.get(&target)
    }

    fn x87_push(&mut self, value: AsmValue) -> Result<()> {
        if self.x87_stack.len() >= 8 {
            return Err(Error::from("x87 stack overflow"));
        }
        self.x87_stack.push(value);
        Ok(())
    }

    fn x87_pop(&mut self) -> Result<AsmValue> {
        Ok(self
            .x87_stack
            .pop()
            .unwrap_or_else(|| AsmValue::Undef(AsmType::F64)))
    }

    fn x87_peek(&self, index: u8) -> Result<AsmValue> {
        let index = usize::from(index);
        if index >= self.x87_stack.len() {
            return Ok(AsmValue::Undef(AsmType::F64));
        }
        Ok(self.x87_stack[self.x87_stack.len() - 1 - index].clone())
    }

    fn x87_set(&mut self, index: u8, value: AsmValue) -> Result<()> {
        let index = usize::from(index);
        while self.x87_stack.len() <= index {
            self.x87_stack.push(AsmValue::Undef(AsmType::F64));
        }
        let slot = self.x87_stack.len() - 1 - index;
        self.x87_stack[slot] = value;
        Ok(())
    }

    fn x87_swap(&mut self, index: u8) -> Result<()> {
        let index = usize::from(index);
        while self.x87_stack.len() <= index {
            self.x87_stack.push(AsmValue::Undef(AsmType::F64));
        }
        let top = self.x87_stack.len() - 1;
        let other = self.x87_stack.len() - 1 - index;
        self.x87_stack.swap(top, other);
        Ok(())
    }

    fn read_return_value(&mut self) -> Option<AsmValue> {
        self.registers.get(&0).cloned().or_else(|| {
            self.ensure_local(0, false);
            Some(AsmValue::Local(*self.locals_by_register.get(&0)?))
        })
    }

    fn read_gpr(&mut self, reg: u8) -> Result<AsmValue> {
        if let Some(value) = self.registers.get(&reg).cloned() {
            return Ok(value);
        }
        let is_argument = matches!(reg, 7 | 6 | 2 | 1 | 8 | 9);
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

    fn read_vec(&mut self, reg: u8) -> Result<AsmValue> {
        if let Some(value) = self.vec_registers.get(&reg).cloned() {
            return Ok(value);
        }

        self.ensure_vec_local(reg);
        let local_id = *self
            .vec_locals_by_register
            .get(&reg)
            .ok_or_else(|| Error::from("missing x86_64 vector local"))?;
        let value = AsmValue::Local(local_id);
        self.vec_registers.insert(reg, value.clone());
        Ok(value)
    }

    fn write_vec(&mut self, reg: u8, value: AsmValue) {
        self.vec_registers.insert(reg, value);
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
            name: Some(reg_name(reg).to_string()),
            is_argument,
        });
    }

    fn ensure_vec_local(&mut self, reg: u8) {
        if self.vec_locals_by_register.contains_key(&reg) {
            return;
        }

        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.vec_locals_by_register.insert(reg, local_id);
        self.locals.push(AsmLocal {
            id: local_id,
            ty: AsmType::Vector(Box::new(AsmType::I64), 2),
            name: Some(format!("xmm{reg}")),
            is_argument: false,
        });
    }
}

fn reg_name(index: u8) -> &'static str {
    match index {
        0 => "rax",
        1 => "rcx",
        2 => "rdx",
        3 => "rbx",
        4 => "rsp",
        5 => "rbp",
        6 => "rsi",
        7 => "rdi",
        8 => "r8",
        9 => "r9",
        10 => "r10",
        11 => "r11",
        12 => "r12",
        13 => "r13",
        14 => "r14",
        15 => "r15",
        _ => "r0",
    }
}
