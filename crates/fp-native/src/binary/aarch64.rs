use crate::binary::LiftedFunction;
use crate::binary::TextRelocation;
use fp_core::asmir::AsmLocal;
use fp_core::asmir::AsmTerminator;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmType, AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::CallingConvention;

pub fn lift_function_bytes(bytes: &[u8], relocs: &[TextRelocation]) -> Result<LiftedFunction> {
    if bytes.len() % 4 != 0 {
        return Err(Error::from("aarch64 function size is not 4-byte aligned"));
    }

    let mut ctx = RegisterLiftContext::new();
    let mut instructions = Vec::new();
    let mut next_id = 0u32;
    let mut index = 0usize;
    while index < bytes.len() {
        let word = u32::from_le_bytes(bytes[index..index + 4].try_into().unwrap());
        if word == 0xD65F03C0 {
            let return_value = ctx.read_return_value();
            return Ok(LiftedFunction {
                instructions,
                terminator: Some(AsmTerminator::Return(return_value)),
                locals: ctx.locals,
            });
        }
        if word == 0xD503201F {
            index += 4;
            continue;
        }

        if (word & 0xFC000000) == 0x94000000 {
            let reloc = relocation_at(relocs, index as u64)
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
            index += 4;
            continue;
        }

        lift_instruction(word, &mut ctx, &mut instructions, &mut next_id)?;
        index += 4;
    }

    Ok(LiftedFunction {
        instructions,
        terminator: None,
        locals: ctx.locals,
    })
}

fn relocation_at<'a>(relocs: &'a [TextRelocation], offset: u64) -> Option<&'a TextRelocation> {
    relocs.iter().find(|reloc| reloc.offset == offset)
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
    if rn == 31 || rd == 31 {
        return None;
    }
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
    if rn == 31 || rd == 31 {
        return None;
    }
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
    if rn == 31 || rt == 31 {
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
    if rn == 31 || rt == 31 {
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
            name: Some(format!("x{reg}")),
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
