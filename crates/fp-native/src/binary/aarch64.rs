use fp_core::asmir::{
    AsmAddressValue, AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode, AsmOperand,
    AsmPhysicalRegister, AsmRegister, AsmRegisterBank, AsmType, AsmValue,
    OperandAccess,
};
use fp_core::error::{Error, Result};
use fp_core::lir::Name;

pub fn lift_function_bytes(bytes: &[u8]) -> Result<(Vec<AsmInstruction>, Option<fp_core::asmir::AsmTerminator>)> {
    if bytes.len() % 4 != 0 {
        return Err(Error::from("aarch64 function size is not 4-byte aligned"));
    }

    let mut instructions = Vec::new();
    let mut next_id = 0u32;
    let mut index = 0usize;
    while index < bytes.len() {
        let word = u32::from_le_bytes(bytes[index..index + 4].try_into().unwrap());
        if word == 0xD65F03C0 {
            return Ok((instructions, Some(fp_core::asmir::AsmTerminator::Return(None))));
        }
        if word == 0xD503201F {
            index += 4;
            continue;
        }

        let lifted = lift_instruction(word, next_id)?;
        instructions.push(lifted);
        next_id += 1;
        index += 4;
    }

    Ok((instructions, None))
}

fn lift_instruction(word: u32, id: u32) -> Result<AsmInstruction> {
    if let Some((dst, src, imm)) = decode_add_immediate(word) {
        return Ok(build_binary(
            id,
            AsmInstructionKind::Add(src.clone(), imm.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            dst,
            src,
            imm,
        ));
    }
    if let Some((dst, src, imm)) = decode_sub_immediate(word) {
        return Ok(build_binary(
            id,
            AsmInstructionKind::Sub(src.clone(), imm.clone()),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            dst,
            src,
            imm,
        ));
    }
    if let Some((dst, base, disp, size_bytes)) = decode_ldr_immediate(word) {
        let address = AsmValue::Address(Box::new(AsmAddressValue {
            base: Some(Box::new(base.clone())),
            index: None,
            scale: 1,
            displacement: disp,
            segment: None,
            size_bytes: Some(size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }));
        let memory = AsmOperand::Memory(fp_core::asmir::AsmMemoryOperand {
            base: Some(value_to_register(&base)?),
            index: None,
            scale: 1,
            displacement: disp,
            segment: None,
            size_bytes: Some(size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        });

        return Ok(AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
            kind: AsmInstructionKind::Load {
                address,
                alignment: None,
                volatile: false,
            },
            type_hint: Some(AsmType::I64),
            operands: vec![register_operand(value_to_register(&dst)?, OperandAccess::Write), memory],
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        });
    }
    if let Some((value, base, disp, size_bytes)) = decode_str_immediate(word) {
        let address = AsmValue::Address(Box::new(AsmAddressValue {
            base: Some(Box::new(base.clone())),
            index: None,
            scale: 1,
            displacement: disp,
            segment: None,
            size_bytes: Some(size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }));
        let memory = AsmOperand::Memory(fp_core::asmir::AsmMemoryOperand {
            base: Some(value_to_register(&base)?),
            index: None,
            scale: 1,
            displacement: disp,
            segment: None,
            size_bytes: Some(size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        });
        let value_operand = register_operand(value_to_register(&value)?, OperandAccess::Read);

        return Ok(AsmInstruction {
            id,
            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
            kind: AsmInstructionKind::Store {
                value,
                address,
                alignment: None,
                volatile: false,
            },
            type_hint: Some(AsmType::Void),
            operands: vec![memory, value_operand],
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        });
    }

    Err(Error::from(format!("unsupported aarch64 instruction: 0x{word:08x}")))
}

fn build_binary(
    id: u32,
    kind: AsmInstructionKind,
    opcode: AsmOpcode,
    dst: AsmValue,
    lhs: AsmValue,
    rhs: AsmValue,
) -> AsmInstruction {
    AsmInstruction {
        id,
        opcode,
        kind,
        type_hint: Some(AsmType::I64),
        operands: vec![
            register_operand(value_to_register(&dst).unwrap(), OperandAccess::Write),
            register_operand(value_to_register(&lhs).unwrap(), OperandAccess::Read),
            value_operand(rhs),
        ],
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn decode_add_immediate(word: u32) -> Option<(AsmValue, AsmValue, AsmValue)> {
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
    Some((
        reg_value(rd),
        reg_value(rn),
        AsmValue::Constant(AsmConstant::Int(imm12, AsmType::I64)),
    ))
}

fn decode_sub_immediate(word: u32) -> Option<(AsmValue, AsmValue, AsmValue)> {
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
    Some((
        reg_value(rd),
        reg_value(rn),
        AsmValue::Constant(AsmConstant::Int(imm12, AsmType::I64)),
    ))
}

fn decode_ldr_immediate(word: u32) -> Option<(AsmValue, AsmValue, i64, u16)> {
    // LDR Xt, [Xn, #imm] (unsigned immediate), 64-bit.
    if (word & 0xFFC00000) != 0xF9400000 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rt = (word & 0x1F) as u8;
    let disp = imm12 * 8;
    Some((reg_value(rt), reg_value(rn), disp, 8))
}

fn decode_str_immediate(word: u32) -> Option<(AsmValue, AsmValue, i64, u16)> {
    // STR Xt, [Xn, #imm] (unsigned immediate), 64-bit.
    if (word & 0xFFC00000) != 0xF9000000 {
        return None;
    }
    let imm12 = ((word >> 10) & 0xFFF) as i64;
    let rn = ((word >> 5) & 0x1F) as u8;
    let rt = (word & 0x1F) as u8;
    let disp = imm12 * 8;
    Some((reg_value(rt), reg_value(rn), disp, 8))
}

fn reg_value(index: u8) -> AsmValue {
    let name = if index == 31 {
        "sp".to_string()
    } else {
        format!("x{index}")
    };
    AsmValue::PhysicalRegister(AsmPhysicalRegister {
        name,
        bank: AsmRegisterBank::General,
        size_bits: 64,
    })
}

fn value_to_register(value: &AsmValue) -> Result<AsmRegister> {
    match value {
        AsmValue::PhysicalRegister(register) => Ok(AsmRegister::Physical(register.clone())),
        AsmValue::Register(id) => Ok(AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: 64,
        }),
        other => Err(Error::from(format!("expected register value, got {other:?}"))),
    }
}

fn register_operand(reg: AsmRegister, access: OperandAccess) -> AsmOperand {
    AsmOperand::Register { reg, access }
}

fn value_operand(value: AsmValue) -> AsmOperand {
    match value {
        AsmValue::Constant(AsmConstant::Int(value, _)) => AsmOperand::Immediate(value as i128),
        AsmValue::Constant(AsmConstant::UInt(value, _)) => AsmOperand::Immediate(value as i128),
        AsmValue::Constant(AsmConstant::Bool(value)) => AsmOperand::Immediate(if value { 1 } else { 0 }),
        AsmValue::PhysicalRegister(register) => AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            access: OperandAccess::Read,
        },
        other => AsmOperand::Symbol(Name::new(format!("value.{other:?}"))),
    }
}
