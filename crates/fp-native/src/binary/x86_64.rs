use crate::binary::{LiftedFunction, TextRelocation};
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmType, AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};

pub fn lift_function_bytes(bytes: &[u8], relocs: &[TextRelocation]) -> Result<LiftedFunction> {
    let mut ctx = RegisterLiftContext::new();
    let mut instructions = Vec::new();
    let mut next_id = 0u32;
    let mut index = 0usize;

    while index < bytes.len() {
        let Some((decoded, consumed)) = decode_instruction(&bytes[index..])? else {
            return Err(Error::from("unsupported x86_64 instruction"));
        };

        match decoded {
            Decoded::Nop => {}
            Decoded::Ret => {
                let return_value = ctx.read_return_value();
                return Ok(LiftedFunction {
                    instructions,
                    terminator: Some(fp_core::asmir::AsmTerminator::Return(return_value)),
                    locals: ctx.locals,
                });
            }
            Decoded::AddImm32 { dst, imm } => {
                let lhs = ctx.read_gpr(dst)?;
                let rhs = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I64));
                let id = next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                next_id += 1;
                ctx.write_gpr(dst, AsmValue::Register(id));
            }
            Decoded::SubImm32 { dst, imm } => {
                let lhs = ctx.read_gpr(dst)?;
                let rhs = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I64));
                let id = next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                ));
                next_id += 1;
                ctx.write_gpr(dst, AsmValue::Register(id));
            }
            Decoded::MovRmToReg { dst, src } => match src {
                RmOperand::Reg(src) => {
                    let value = ctx.read_gpr(src)?;
                    let id = next_id;
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
                    next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(id));
                }
                RmOperand::Mem(memory) => {
                    let addr = compute_address(
                        &mut ctx,
                        memory,
                        index as u64,
                        relocs,
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
                }
            },
            Decoded::MovRegToRm { dst, src } => {
                let value = ctx.read_gpr(src)?;
                match dst {
                    RmOperand::Reg(dst) => {
                        let id = next_id;
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
                        next_id += 1;
                        ctx.write_gpr(dst, AsmValue::Register(id));
                    }
                    RmOperand::Mem(memory) => {
                        let addr = compute_address(
                            &mut ctx,
                            memory,
                            index as u64,
                            relocs,
                            &mut instructions,
                            &mut next_id,
                        )?;
                        let id = next_id;
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
                        next_id += 1;
                    }
                }
            }
            Decoded::CallRel32 { imm_offset } => {
                let reloc_offset = (index + imm_offset) as u64;
                let reloc = relocation_at(relocs, reloc_offset)
                    .ok_or_else(|| Error::from("unsupported x86_64 call without relocation"))?;
                let id = next_id;
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
                next_id += 1;
            }
        }

        index = index
            .checked_add(consumed)
            .ok_or_else(|| Error::from("x86_64 lift overflow"))?;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Decoded {
    Nop,
    Ret,
    AddImm32 { dst: u8, imm: i32 },
    SubImm32 { dst: u8, imm: i32 },
    MovRmToReg { dst: u8, src: RmOperand },
    MovRegToRm { dst: RmOperand, src: u8 },
    CallRel32 { imm_offset: usize },
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

fn decode_instruction(bytes: &[u8]) -> Result<Option<(Decoded, usize)>> {
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
            Decoded::AddImm32 { dst: 0, imm },
            opcode_index + 1 + 4,
        )));
    }

    // SUB rax, imm32: REX.W 2D imm32.
    if rex_w && opcode == 0x2D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::SubImm32 { dst: 0, imm },
            opcode_index + 1 + 4,
        )));
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
