use crate::binary::LiftedFunction;
use crate::binary::TextRelocation;
use crate::binary::cfg::wire_block_edges;
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmType, AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::CallingConvention;

pub fn lift_function_bytes(bytes: &[u8], relocs: &[TextRelocation]) -> Result<LiftedFunction> {
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

        let mut cursor = block_offset;
        while cursor < next_block_offset {
            let inst_index = (cursor / 4) as usize;
            let word = u32::from_le_bytes(
                bytes[inst_index * 4..inst_index * 4 + 4]
                    .try_into()
                    .unwrap(),
            );

            if word == 0xD503201F {
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
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        ));
        *next_id += 1;
        ctx.write_gpr(dst, AsmValue::Register(id));
        return Ok(());
    }

    if let Some((dst, src, imm)) = decode_sub_immediate(word) {
        let lhs = ctx.read_gpr(src)?;
        let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
        ));
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
    instructions.push(build_binop(
        id,
        AsmInstructionKind::Add(base, rhs),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    ));
    *next_id += 1;
    Ok(AsmValue::Register(id))
}
