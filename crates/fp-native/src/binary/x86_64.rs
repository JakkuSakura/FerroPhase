use crate::binary::cfg::wire_block_edges;
use crate::binary::{LiftedFunction, TextRelocation};
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmType, AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};

pub fn lift_function_bytes(bytes: &[u8], relocs: &[TextRelocation]) -> Result<LiftedFunction> {
    let decoded = decode_stream(bytes)?;
    let block_starts = determine_block_starts(&decoded)?;
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
            let inst = decoded
                .iter()
                .find(|inst| inst.offset == cursor)
                .ok_or_else(|| Error::from("missing decoded instruction"))?;

            if is_terminator(&inst.kind) {
                let terminator = lift_terminator(
                    &mut ctx,
                    inst,
                    &mut instructions,
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
            )?;
            cursor = cursor
                .checked_add(inst.len as u64)
                .ok_or_else(|| Error::from("x86_64 lift overflow"))?;
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
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }
    }

    wire_block_edges(&mut basic_blocks);

    Ok(LiftedFunction {
        basic_blocks,
        locals: ctx.locals,
    })
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

fn value_from_operand(
    ctx: &mut RegisterLiftContext,
    operand: Operand,
    instruction_offset: u64,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        Operand::Imm(value) => Ok(AsmValue::Constant(AsmConstant::Int(value, AsmType::I64))),
        Operand::Rm(rm) => match rm {
            RmOperand::Reg(reg) => ctx.read_gpr(reg),
            RmOperand::Mem(memory) => {
                let addr = compute_address(
                    ctx,
                    memory,
                    instruction_offset,
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
    let (kind, opcode) = compare_kind_from_condition(condition, lhs, rhs)?;
    inst.kind = kind;
    inst.opcode = AsmOpcode::Generic(opcode);
    inst.type_hint = None;
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
) -> Result<(AsmInstructionKind, fp_core::asmir::AsmGenericOpcode)> {
    Ok(match condition {
        0x4 => (
            AsmInstructionKind::Eq(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Eq,
        ),
        0x5 => (
            AsmInstructionKind::Ne(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ne,
        ),
        0xC => (
            AsmInstructionKind::Lt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Lt,
        ),
        0xD => (
            AsmInstructionKind::Ge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ge,
        ),
        0xE => (
            AsmInstructionKind::Le(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Le,
        ),
        0xF => (
            AsmInstructionKind::Gt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Gt,
        ),
        0x2 => (
            AsmInstructionKind::Ult(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ult,
        ),
        0x3 => (
            AsmInstructionKind::Uge(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Uge,
        ),
        0x6 => (
            AsmInstructionKind::Ule(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ule,
        ),
        0x7 => (
            AsmInstructionKind::Ugt(lhs, rhs),
            fp_core::asmir::AsmGenericOpcode::Ugt,
        ),
        other => {
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
}

fn decode_stream(bytes: &[u8]) -> Result<Vec<DecodedInstruction>> {
    let mut decoded = Vec::new();
    let mut index = 0usize;
    while index < bytes.len() {
        let Some((kind, len)) = decode_instruction(&bytes[index..], index as u64)? else {
            return Err(Error::from("unsupported x86_64 instruction"));
        };
        decoded.push(DecodedInstruction {
            offset: index as u64,
            len,
            kind,
        });
        index = index
            .checked_add(len)
            .ok_or_else(|| Error::from("x86_64 lift overflow"))?;
    }
    Ok(decoded)
}

fn determine_block_starts(decoded: &[DecodedInstruction]) -> Result<Vec<u64>> {
    let mut starts = vec![0u64];
    let instruction_starts = decoded
        .iter()
        .map(|inst| inst.offset)
        .collect::<std::collections::HashSet<_>>();

    for inst in decoded {
        match inst.kind {
            Decoded::JccRel { target, .. } => {
                if !instruction_starts.contains(&target) {
                    return Err(Error::from("x86_64 jcc target not on instruction boundary"));
                }
                starts.push(target);
                let fallthrough = inst.offset + inst.len as u64;
                if instruction_starts.contains(&fallthrough) {
                    starts.push(fallthrough);
                }
            }
            Decoded::JmpRel { target } => {
                if !instruction_starts.contains(&target) {
                    return Err(Error::from("x86_64 jmp target not on instruction boundary"));
                }
                starts.push(target);
                let fallthrough = inst.offset + inst.len as u64;
                if instruction_starts.contains(&fallthrough) {
                    starts.push(fallthrough);
                }
            }
            Decoded::Ret => {
                let fallthrough = inst.offset + inst.len as u64;
                if instruction_starts.contains(&fallthrough) {
                    starts.push(fallthrough);
                }
            }
            _ => {}
        }
    }

    starts.sort_unstable();
    starts.dedup();
    Ok(starts)
}

fn is_terminator(kind: &Decoded) -> bool {
    matches!(
        kind,
        Decoded::Ret | Decoded::JmpRel { .. } | Decoded::JccRel { .. }
    )
}

fn lift_terminator(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    instructions: &mut Vec<AsmInstruction>,
    offset_to_block: &std::collections::HashMap<u64, u32>,
    last_compare: &mut Option<LastCompare>,
) -> Result<fp_core::asmir::AsmTerminator> {
    match inst.kind {
        Decoded::Ret => Ok(fp_core::asmir::AsmTerminator::Return(
            ctx.read_return_value(),
        )),
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

            let compare = last_compare
                .as_ref()
                .ok_or_else(|| Error::from("conditional branch without comparison"))?;
            patch_compare_kind(instructions, compare, condition)?;

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
) -> Result<()> {
    match inst.kind {
        Decoded::Nop => Ok(()),
        Decoded::AddImm { dst, imm } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::SubImm { dst, imm } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Cmp { lhs, rhs } => {
            let lhs_value =
                value_from_operand(ctx, lhs, inst.offset, relocs, instructions, next_id)?;
            let rhs_value =
                value_from_operand(ctx, rhs, inst.offset, relocs, instructions, next_id)?;
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
            });
            Ok(())
        }
        Decoded::Test { lhs, rhs } => {
            let lhs_value =
                value_from_operand(ctx, lhs, inst.offset, relocs, instructions, next_id)?;
            let rhs_value =
                value_from_operand(ctx, rhs, inst.offset, relocs, instructions, next_id)?;
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
            });
            Ok(())
        }
        Decoded::MovRmToReg { dst, src } => match src {
            RmOperand::Reg(src) => {
                let value = ctx.read_gpr(src)?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(value.clone()),
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
                let addr =
                    compute_address(ctx, memory, inst.offset, relocs, instructions, next_id)?;
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
                Ok(())
            }
        },
        Decoded::MovRegToRm { dst, src } => {
            let value = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value.clone()),
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
                    let addr =
                        compute_address(ctx, memory, inst.offset, relocs, instructions, next_id)?;
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
        Decoded::CallRel32 { imm_offset } => {
            let reloc_offset = inst
                .offset
                .checked_add(imm_offset as u64)
                .ok_or_else(|| Error::from("x86_64 call relocation overflow"))?;
            let reloc = relocation_at(relocs, reloc_offset)
                .ok_or_else(|| Error::from("unsupported x86_64 call without relocation"))?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function(reloc.symbol.clone()),
                    args: Vec::new(),
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
        Decoded::Ret | Decoded::JmpRel { .. } | Decoded::JccRel { .. } => Ok(()),
    }
}

fn relocation_at<'a>(relocs: &'a [TextRelocation], offset: u64) -> Option<&'a TextRelocation> {
    relocs.iter().find(|reloc| reloc.offset == offset)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Decoded {
    Nop,
    Ret,
    AddImm { dst: u8, imm: i64 },
    SubImm { dst: u8, imm: i64 },
    Cmp { lhs: Operand, rhs: Operand },
    Test { lhs: Operand, rhs: Operand },
    MovRmToReg { dst: u8, src: RmOperand },
    MovRegToRm { dst: RmOperand, src: u8 },
    CallRel32 { imm_offset: usize },
    JmpRel { target: u64 },
    JccRel { condition: u8, target: u64 },
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
struct X86Memory {
    base: Option<u8>,
    index: Option<u8>,
    scale: u8,
    displacement: i64,
    displacement_offset: Option<usize>,
}

fn decode_instruction(bytes: &[u8], offset: u64) -> Result<Option<(Decoded, usize)>> {
    if bytes.is_empty() {
        return Ok(None);
    }

    // Minimal prefix handling: accept `REX.W`.
    let (rex_w, opcode_index) = match bytes[0] {
        0x48 => (true, 1usize),
        _ => (false, 0usize),
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

    if opcode == 0x0F {
        let ext = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated 0f opcode"))?;
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
        read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::CallRel32 {
                imm_offset: opcode_index + 1,
            },
            opcode_index + 1 + 4,
        )));
    }

    // ADD rax, imm32: REX.W 05 imm32.
    if rex_w && opcode == 0x05 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::AddImm {
                dst: 0,
                imm: imm as i64,
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
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP rax, imm32: REX.W 3D imm32.
    if rex_w && opcode == 0x3D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(0)),
                rhs: Operand::Imm(imm as i64),
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP r/m64, r64: REX.W 39 /r.
    if rex_w && opcode == 0x39 {
        let (reg, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
            },
            opcode_index + 1 + consumed,
        )));
    }

    // CMP r64, r/m64: REX.W 3B /r.
    if rex_w && opcode == 0x3B {
        let (reg, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(reg)),
                rhs: Operand::Rm(rm),
            },
            opcode_index + 1 + consumed,
        )));
    }

    // TEST r/m64, r64: REX.W 85 /r.
    if rex_w && opcode == 0x85 {
        let (reg, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Test {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
            },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD/SUB/CMP r/m64, imm8|imm32: REX.W 83/81 /0, /5, /7.
    if rex_w && (opcode == 0x83 || opcode == 0x81) {
        let (ext, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
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
        match (ext, rm) {
            (0, RmOperand::Reg(0b100)) => return Ok(Some((Decoded::AddImm { dst: 4, imm }, len))),
            (5, RmOperand::Reg(0b100)) => return Ok(Some((Decoded::SubImm { dst: 4, imm }, len))),
            (7, rm) => {
                return Ok(Some((
                    Decoded::Cmp {
                        lhs: Operand::Rm(rm),
                        rhs: Operand::Imm(imm),
                    },
                    len,
                )));
            }
            _ => {}
        }
    }

    // MOV r64, r/m64: REX.W 8B /r.
    if rex_w && opcode == 0x8B {
        let (reg, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::MovRmToReg { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r/m64, r64: REX.W 89 /r.
    if rex_w && opcode == 0x89 {
        let (reg, rm, consumed) = decode_modrm(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::MovRegToRm { dst: rm, src: reg },
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

fn read_i32(bytes: &[u8], index: usize) -> Result<i32> {
    let imm = bytes
        .get(index..index + 4)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(i32::from_le_bytes(imm.try_into().unwrap()))
}

fn decode_modrm(bytes: &[u8], index: usize) -> Result<(u8, RmOperand, usize)> {
    let modrm = *bytes
        .get(index)
        .ok_or_else(|| Error::from("missing modrm"))?;
    let mode = (modrm >> 6) & 0b11;
    let reg = (modrm >> 3) & 0b111;
    let rm = modrm & 0b111;
    let mut consumed = 1usize;

    if mode == 0b11 {
        return Ok((reg, RmOperand::Reg(rm), consumed));
    }

    let mut base = Some(rm);
    let mut index_reg = None;
    let mut scale = 1u8;
    let mut displacement = 0i64;
    let mut displacement_offset = None;

    if rm == 0b100 {
        let sib = *bytes
            .get(index + consumed)
            .ok_or_else(|| Error::from("missing sib"))?;
        consumed += 1;
        let scale_bits = (sib >> 6) & 0b11;
        scale = 1u8 << scale_bits;
        let index_bits = (sib >> 3) & 0b111;
        let base_bits = sib & 0b111;
        if index_bits != 0b100 {
            index_reg = Some(index_bits);
        }
        base = Some(base_bits);
    }

    match mode {
        0b00 => {
            if base == Some(0b101) {
                base = None;
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
        }),
        consumed,
    ))
}

fn compute_address(
    ctx: &mut RegisterLiftContext,
    memory: X86Memory,
    instruction_offset: u64,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    if let Some(displacement_offset) = memory.displacement_offset {
        let relocation_offset = instruction_offset
            .checked_add(displacement_offset as u64)
            .ok_or_else(|| Error::from("x86_64 relocation offset overflow"))?;
        if let Some(reloc) = relocation_at(relocs, relocation_offset) {
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
            let addend = reloc.addend.saturating_add(memory.displacement);
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
