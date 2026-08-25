use super::*;

pub(super) fn lift_x86_instruction(
    instruction: &X86InstructionDetail,
    id: u32,
) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(x86_operand_to_asm)
        .collect::<Vec<_>>();
    let ty = output_type_from_asm_operands(&operands).unwrap_or(AsmType::Void);
    let kind = semanticize_x86_detail(instruction, &operands)?;
    Ok(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        ty,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    })
}

pub(super) fn lift_aarch64_instruction(
    instruction: &Aarch64InstructionDetail,
    id: u32,
) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(aarch64_operand_to_asm)
        .collect::<Vec<_>>();
    let ty = output_type_from_asm_operands(&operands).unwrap_or(AsmType::Void);
    let kind = semanticize_aarch64_detail(instruction, &operands)?;
    Ok(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        ty,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    })
}

pub(super) fn output_type_from_asm_operands(operands: &[AsmOperand]) -> Option<AsmType> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Register {
            reg: AsmRegister::Virtual { size_bits, .. },
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => {
            Some(type_from_bits(*size_bits))
        }
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => {
            Some(type_from_bits(register.size_bits))
        }
        _ => None,
    })
}

pub(super) fn type_from_bits(size_bits: u16) -> AsmType {
    match size_bits {
        1 => AsmType::I1,
        8 => AsmType::I8,
        16 => AsmType::I16,
        32 => AsmType::I32,
        64 => AsmType::I64,
        128 => AsmType::I128,
        _ => AsmType::I64,
    }
}

pub(super) fn x86_custom_opcode_name(instruction: &X86InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode, X86Opcode::Cmp | X86Opcode::CMov) => {
            format!(
                "{}.{}",
                instruction.opcode.mnemonic(),
                x86_condition_suffix(condition)
            )
        }
        _ => instruction.opcode.mnemonic().to_string(),
    }
}

pub(super) fn aarch64_custom_opcode_name(instruction: &Aarch64InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode.as_str(), "cmp" | "csel") => {
            format!(
                "{}.{}",
                instruction.opcode,
                aarch64_condition_suffix(condition)
            )
        }
        _ => instruction.opcode.clone(),
    }
}

pub(super) fn x86_condition_suffix(condition: &X86ConditionCode) -> &'static str {
    match condition {
        X86ConditionCode::Equal => "eq",
        X86ConditionCode::NotEqual => "ne",
        X86ConditionCode::Less => "lt",
        X86ConditionCode::LessEqual => "le",
        X86ConditionCode::Greater => "gt",
        X86ConditionCode::GreaterEqual => "ge",
        X86ConditionCode::Below => "ult",
        X86ConditionCode::BelowEqual => "ule",
        X86ConditionCode::Above => "ugt",
        X86ConditionCode::AboveEqual => "uge",
        X86ConditionCode::NonZero => "nz",
    }
}

pub(super) fn aarch64_condition_suffix(condition: &Aarch64ConditionCode) -> &'static str {
    match condition {
        Aarch64ConditionCode::Eq => "eq",
        Aarch64ConditionCode::Ne => "ne",
        Aarch64ConditionCode::Lt => "lt",
        Aarch64ConditionCode::Le => "le",
        Aarch64ConditionCode::Gt => "gt",
        Aarch64ConditionCode::Ge => "ge",
        Aarch64ConditionCode::Lo => "ult",
        Aarch64ConditionCode::Ls => "ule",
        Aarch64ConditionCode::Hi => "ugt",
        Aarch64ConditionCode::Hs => "uge",
        Aarch64ConditionCode::NonZero => "nz",
    }
}

pub(super) fn asm_condition_suffix(condition: &AsmConditionCode) -> &'static str {
    match condition {
        AsmConditionCode::Eq => "eq",
        AsmConditionCode::Ne => "ne",
        AsmConditionCode::Lt => "lt",
        AsmConditionCode::Le => "le",
        AsmConditionCode::Gt => "gt",
        AsmConditionCode::Ge => "ge",
        AsmConditionCode::Ult => "ult",
        AsmConditionCode::Ule => "ule",
        AsmConditionCode::Ugt => "ugt",
        AsmConditionCode::Uge => "uge",
        AsmConditionCode::Nz => "nz",
    }
}

pub(super) fn x86_operand_to_asm(operand: &X86Operand) -> AsmOperand {
    match operand {
        X86Operand::Register { reg, access } => AsmOperand::Register {
            reg: x86_register_to_asm(reg),
            access: access.clone(),
        },
        X86Operand::Immediate(value) => AsmOperand::Immediate(*value),
        X86Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(x86_register_to_asm),
            index: mem.index.as_ref().map(x86_register_to_asm),
            scale: mem.scale,
            displacement: mem.displacement,
            segment: None,
            size_bytes: mem.size_bytes,
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        X86Operand::Block(id) => AsmOperand::Block(*id),
        X86Operand::Symbol(name) => AsmOperand::Symbol(name.clone()),
    }
}

pub(super) fn aarch64_operand_to_asm(operand: &Aarch64Operand) -> AsmOperand {
    match operand {
        Aarch64Operand::Register { reg, access } => AsmOperand::Register {
            reg: aarch64_register_to_asm(reg),
            access: access.clone(),
        },
        Aarch64Operand::Immediate(value) => AsmOperand::Immediate(*value),
        Aarch64Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(aarch64_register_to_asm),
            index: mem.index.as_ref().map(aarch64_register_to_asm),
            scale: mem.scale,
            displacement: mem.displacement,
            segment: None,
            size_bytes: mem.size_bytes,
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        Aarch64Operand::Block(id) => AsmOperand::Block(*id),
        Aarch64Operand::Symbol(name) => AsmOperand::Symbol(name.clone()),
    }
}

pub(super) fn x86_register_to_asm(register: &X86Register) -> AsmRegister {
    match register {
        X86Register::Physical { name, size_bits } => {
            AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
                name: name.clone(),
                bank: if name.starts_with("xmm") {
                    AsmRegisterBank::Float
                } else {
                    AsmRegisterBank::General
                },
                size_bits: *size_bits,
            })
        }
        X86Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

pub(super) fn aarch64_register_to_asm(register: &Aarch64Register) -> AsmRegister {
    match register {
        Aarch64Register::Physical { name, size_bits } => {
            AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
                name: name.clone(),
                bank: if matches!(name.chars().next(), Some('s' | 'd' | 'q' | 'v')) {
                    AsmRegisterBank::Float
                } else {
                    AsmRegisterBank::General
                },
                size_bits: *size_bits,
            })
        }
        Aarch64Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

pub(super) fn asm_operand_to_x86(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match operand {
        AsmOperand::Register { reg, access } => X86Operand::Register {
            reg: asm_register_to_x86(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => X86Operand::Immediate(*value),
        AsmOperand::Memory(mem) => X86Operand::Memory(X86MemoryOperand {
            base: mem
                .base
                .as_ref()
                .map(|register| asm_register_to_x86(register, ctx)),
            index: mem
                .index
                .as_ref()
                .map(|register| asm_register_to_x86(register, ctx)),
            scale: mem.scale,
            displacement: mem.displacement,
            size_bytes: mem.size_bytes,
        }),
        AsmOperand::Block(id) => X86Operand::Block(*id),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => X86Operand::Symbol(name.clone()),
        AsmOperand::Relocation(relocation) => X86Operand::Symbol(relocation.symbol.clone()),
        AsmOperand::Predicate { reg, .. } => X86Operand::Register {
            reg: asm_register_to_x86(reg, ctx),
            access: OperandAccess::Read,
        },
    }
}

pub(super) fn asm_operand_to_aarch64(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match operand {
        AsmOperand::Register { reg, access } => Aarch64Operand::Register {
            reg: asm_register_to_aarch64(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => Aarch64Operand::Immediate(*value),
        AsmOperand::Memory(mem) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: mem
                .base
                .as_ref()
                .map(|register| asm_register_to_aarch64(register, ctx)),
            index: mem
                .index
                .as_ref()
                .map(|register| asm_register_to_aarch64(register, ctx)),
            scale: mem.scale,
            displacement: mem.displacement,
            size_bytes: mem.size_bytes,
        }),
        AsmOperand::Block(id) => Aarch64Operand::Block(*id),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => Aarch64Operand::Symbol(name.clone()),
        AsmOperand::Relocation(relocation) => Aarch64Operand::Symbol(relocation.symbol.clone()),
        AsmOperand::Predicate { reg, .. } => Aarch64Operand::Register {
            reg: asm_register_to_aarch64(reg, ctx),
            access: OperandAccess::Read,
        },
    }
}

pub(super) fn asm_register_to_x86(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_x86(physical, ctx),
        AsmRegister::Virtual { id, size_bits, .. } => X86Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        },
    }
}

pub(super) fn asm_register_to_aarch64(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_aarch64(physical, ctx),
        AsmRegister::Virtual { id, size_bits, .. } => Aarch64Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        },
    }
}

pub(super) fn map_physical_register_to_x86(
    register: &fp_core::asmir::AsmPhysicalRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Register {
    if is_x86_physical_register_name(&register.name) {
        return X86Register::Physical {
            name: register.name.clone(),
            size_bits: register.size_bits,
        };
    }

    let size_bits = register.size_bits.max(8);
    let name = register.name.as_str();
    if matches!(name, "sp" | "rsp" | "esp" | "fp" | "rbp" | "ebp" | "bp") {
        return X86Register::Physical {
            name: map_general_register_name_to_x86(name, size_bits),
            size_bits,
        };
    }

    X86Register::Virtual {
        id: ctx.virtual_id_for(register),
        size_bits,
    }
}

pub(super) fn map_physical_register_to_aarch64(
    register: &fp_core::asmir::AsmPhysicalRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Register {
    if is_aarch64_physical_register_name(&register.name) || register.name == "sp" {
        return Aarch64Register::Physical {
            name: register.name.clone(),
            size_bits: register.size_bits,
        };
    }

    let size_bits = register.size_bits.max(8);
    let name = register.name.as_str();
    if matches!(name, "sp" | "rsp" | "esp" | "fp" | "rbp" | "ebp" | "bp") {
        return Aarch64Register::Physical {
            name: map_general_register_name_to_aarch64(name, size_bits),
            size_bits,
        };
    }

    Aarch64Register::Virtual {
        id: ctx.virtual_id_for(register),
        size_bits,
    }
}

pub(super) fn physical_register_index(name: &str) -> Option<u8> {
    let digits = name
        .chars()
        .skip_while(|ch| !ch.is_ascii_digit())
        .collect::<String>();
    if digits.is_empty() {
        None
    } else {
        digits.parse::<u8>().ok()
    }
}

pub(super) fn map_general_register_name_to_x86(name: &str, size_bits: u16) -> String {
    if name == "sp" {
        return x86_general_register_name(4, size_bits);
    }
    if name == "fp" || name == "x29" || name == "w29" {
        return x86_general_register_name(5, size_bits);
    }
    let index = physical_register_index(name)
        .unwrap_or_else(|| x86_general_register_index(name).unwrap_or(0));
    x86_general_register_name(index, size_bits)
}

pub(super) fn map_general_register_name_to_aarch64(name: &str, size_bits: u16) -> String {
    if matches!(name, "rsp" | "esp" | "sp") {
        return "sp".to_string();
    }
    if matches!(name, "rbp" | "ebp" | "bp") {
        return if size_bits <= 32 {
            "w29".to_string()
        } else {
            "x29".to_string()
        };
    }
    let index = x86_general_register_index(name)
        .or_else(|| physical_register_index(name))
        .unwrap_or(0);
    if size_bits <= 32 {
        format!("w{index}")
    } else {
        format!("x{index}")
    }
}

pub(super) fn x86_general_register_index(name: &str) -> Option<u8> {
    Some(match name {
        "rax" | "eax" | "ax" | "al" | "ah" => 0,
        "rcx" | "ecx" | "cx" | "cl" | "ch" => 1,
        "rdx" | "edx" | "dx" | "dl" | "dh" => 2,
        "rbx" | "ebx" | "bx" | "bl" | "bh" => 3,
        "rsp" | "esp" | "sp" => 4,
        "rbp" | "ebp" | "bp" => 5,
        "rsi" | "esi" | "si" => 6,
        "rdi" | "edi" | "di" => 7,
        "r8" | "r8d" | "r8w" | "r8b" => 8,
        "r9" | "r9d" | "r9w" | "r9b" => 9,
        "r10" | "r10d" | "r10w" | "r10b" => 10,
        "r11" | "r11d" | "r11w" | "r11b" => 11,
        "r12" | "r12d" | "r12w" | "r12b" => 12,
        "r13" | "r13d" | "r13w" | "r13b" => 13,
        "r14" | "r14d" | "r14w" | "r14b" => 14,
        "r15" | "r15d" | "r15w" | "r15b" => 15,
        _ => return None,
    })
}

pub(super) fn x86_general_register_name(index: u8, size_bits: u16) -> String {
    match size_bits {
        0..=8 => match index {
            0 => "al".to_string(),
            1 => "cl".to_string(),
            2 => "dl".to_string(),
            3 => "bl".to_string(),
            4 => "spl".to_string(),
            5 => "bpl".to_string(),
            6 => "sil".to_string(),
            7 => "dil".to_string(),
            _ => format!("r{index}b"),
        },
        9..=16 => match index {
            0 => "ax".to_string(),
            1 => "cx".to_string(),
            2 => "dx".to_string(),
            3 => "bx".to_string(),
            4 => "sp".to_string(),
            5 => "bp".to_string(),
            6 => "si".to_string(),
            7 => "di".to_string(),
            _ => format!("r{index}w"),
        },
        17..=32 => match index {
            0 => "eax".to_string(),
            1 => "ecx".to_string(),
            2 => "edx".to_string(),
            3 => "ebx".to_string(),
            4 => "esp".to_string(),
            5 => "ebp".to_string(),
            6 => "esi".to_string(),
            7 => "edi".to_string(),
            _ => format!("r{index}d"),
        },
        _ => match index {
            0 => "rax".to_string(),
            1 => "rcx".to_string(),
            2 => "rdx".to_string(),
            3 => "rbx".to_string(),
            4 => "rsp".to_string(),
            5 => "rbp".to_string(),
            6 => "rsi".to_string(),
            7 => "rdi".to_string(),
            _ => format!("r{index}"),
        },
    }
}

pub(super) fn x86_call_target_from_operand(operand: &X86Operand) -> X86CallTarget {
    match operand {
        X86Operand::Symbol(name) => X86CallTarget::Symbol(name.clone()),
        X86Operand::Register { reg, .. } => X86CallTarget::Register(reg.clone()),
        _ => X86CallTarget::Symbol(Name::new("indirect.call")),
    }
}

pub(super) fn aarch64_call_target_from_operand(operand: &Aarch64Operand) -> Aarch64CallTarget {
    match operand {
        Aarch64Operand::Symbol(name) => Aarch64CallTarget::Symbol(name.clone()),
        Aarch64Operand::Register { reg, .. } => Aarch64CallTarget::Register(reg.clone()),
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

pub(super) fn lift_x86_terminator(terminator: &X86TerminatorDetail) -> Result<AsmTerminator> {
    match terminator.opcode {
        X86TerminatorOpcode::Ret => Ok(AsmTerminator::Return(None)),
        X86TerminatorOpcode::Jmp => Ok(AsmTerminator::Br(
            terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("direct branch is missing its target"))?,
        )),
        X86TerminatorOpcode::Jcc => {
            let condition = terminator
                .condition
                .as_ref()
                .ok_or_else(|| Error::from("conditional branch is missing its condition"))?;
            let if_true = terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its true target"))?;
            let if_false = terminator
                .targets
                .get(1)
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its false target"))?;
            Ok(AsmTerminator::CondBr {
                condition: AsmValue::Condition(asm_condition_from_x86(condition)),
                if_true,
                if_false,
            })
        }
        X86TerminatorOpcode::Ud2 => Ok(AsmTerminator::Unreachable),
        X86TerminatorOpcode::Switch
        | X86TerminatorOpcode::IndirectJmp
        | X86TerminatorOpcode::Invoke
        | X86TerminatorOpcode::Resume
        | X86TerminatorOpcode::CleanupRet
        | X86TerminatorOpcode::CatchRet
        | X86TerminatorOpcode::CatchSwitch => Err(Error::from(
            "raw x86 terminator lacks typed operands required by AsmIR",
        )),
    }
}

pub(super) fn lift_aarch64_terminator(
    terminator: &Aarch64TerminatorDetail,
) -> Result<AsmTerminator> {
    match terminator.opcode {
        Aarch64TerminatorOpcode::Ret => Ok(AsmTerminator::Return(None)),
        Aarch64TerminatorOpcode::B => Ok(AsmTerminator::Br(
            terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("direct branch is missing its target"))?,
        )),
        Aarch64TerminatorOpcode::BCond => {
            let condition = terminator
                .condition
                .as_ref()
                .ok_or_else(|| Error::from("conditional branch is missing its condition"))?;
            let if_true = terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its true target"))?;
            let if_false = terminator
                .targets
                .get(1)
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its false target"))?;
            Ok(AsmTerminator::CondBr {
                condition: AsmValue::Condition(asm_condition_from_aarch64(condition)),
                if_true,
                if_false,
            })
        }
        Aarch64TerminatorOpcode::Brk => Ok(AsmTerminator::Unreachable),
        Aarch64TerminatorOpcode::Br
        | Aarch64TerminatorOpcode::Switch
        | Aarch64TerminatorOpcode::Invoke
        | Aarch64TerminatorOpcode::Resume
        | Aarch64TerminatorOpcode::CleanupRet
        | Aarch64TerminatorOpcode::CatchRet
        | Aarch64TerminatorOpcode::CatchSwitch => Err(Error::from(
            "raw AArch64 terminator lacks typed operands required by AsmIR",
        )),
    }
}

pub(super) fn semanticize_x86_detail(
    instruction: &X86InstructionDetail,
    operands: &[AsmOperand],
) -> Result<AsmInstructionKind> {
    let opcode_name = x86_custom_opcode_name(instruction);
    let (base, condition) = parse_x86_custom_opcode(&opcode_name);
    let values = collect_machine_values(operands)?;
    match base {
        "syscall" => Ok(AsmInstructionKind::Syscall {
            convention: AsmSyscallConvention::LinuxX86_64,
            number: AsmValue::PhysicalRegister(AsmPhysicalRegister {
                name: "rax".to_string(),
                bank: AsmRegisterBank::General,
                size_bits: 64,
            }),
            args: vec![
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rdi".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rsi".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rdx".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r10".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r8".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r9".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
            ],
        }),
        "add" => binary_value_kind(operands, &values, AsmInstructionKind::Add),
        "sub" => binary_value_kind(operands, &values, AsmInstructionKind::Sub),
        "imul" | "mulss" | "mulsd" => binary_value_kind(operands, &values, AsmInstructionKind::Mul),
        "idiv" | "divss" | "divsd" => binary_value_kind(operands, &values, AsmInstructionKind::Div),
        "and" => binary_value_kind(operands, &values, AsmInstructionKind::And),
        "or" => binary_value_kind(operands, &values, AsmInstructionKind::Or),
        "xor" => binary_value_kind(operands, &values, AsmInstructionKind::Xor),
        "shl" => binary_value_kind(operands, &values, AsmInstructionKind::Shl),
        "sar" => binary_value_kind(operands, &values, AsmInstructionKind::Shr),
        "not" => unary_value_kind(operands, &values, AsmInstructionKind::Not),
        "cmp" => compare_value_kind(operands, &values, condition),
        "mov" => x86_mov_kind(operands, &values),
        "lea" | "lea.frame" => address_kind(operands),
        "call" => call_value_kind(operands, &values),
        "cmov" => select_value_kind(operands, &values),
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported x86 opcode for transpile: {base}"
        ))),
    }
}

pub(super) fn semanticize_aarch64_detail(
    instruction: &Aarch64InstructionDetail,
    operands: &[AsmOperand],
) -> Result<AsmInstructionKind> {
    let opcode_name = aarch64_custom_opcode_name(instruction);
    let (base, condition) = parse_aarch64_custom_opcode(&opcode_name);
    let values = collect_machine_values(operands)?;
    match base {
        "svc" => {
            let imm = operands
                .iter()
                .find_map(|operand| match operand {
                    AsmOperand::Immediate(value) => Some(*value),
                    _ => None,
                })
                .unwrap_or(0);
            let convention = match imm {
                0 => AsmSyscallConvention::LinuxAarch64,
                0x80 => AsmSyscallConvention::DarwinAarch64,
                _ => AsmSyscallConvention::LinuxAarch64,
            };
            let number_reg = match convention {
                AsmSyscallConvention::DarwinAarch64 => "x16",
                _ => "x8",
            };
            Ok(AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: number_reg.to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                args: (0..6)
                    .map(|idx| {
                        AsmValue::PhysicalRegister(AsmPhysicalRegister {
                            name: format!("x{idx}"),
                            bank: AsmRegisterBank::General,
                            size_bits: 64,
                        })
                    })
                    .collect(),
            })
        }
        "add" => binary_value_kind(operands, &values, AsmInstructionKind::Add),
        "sub" => binary_value_kind(operands, &values, AsmInstructionKind::Sub),
        "mul" | "fmul.s" | "fmul.d" => {
            binary_value_kind(operands, &values, AsmInstructionKind::Mul)
        }
        "sdiv" | "fdiv.s" | "fdiv.d" => {
            binary_value_kind(operands, &values, AsmInstructionKind::Div)
        }
        "and" => binary_value_kind(operands, &values, AsmInstructionKind::And),
        "orr" => binary_value_kind(operands, &values, AsmInstructionKind::Or),
        "eor" => binary_value_kind(operands, &values, AsmInstructionKind::Xor),
        "lsl" => binary_value_kind(operands, &values, AsmInstructionKind::Shl),
        "asr" => binary_value_kind(operands, &values, AsmInstructionKind::Shr),
        "mvn" => unary_value_kind(operands, &values, AsmInstructionKind::Not),
        "cmp" => compare_value_kind(
            operands,
            &values,
            condition.map(aarch64_condition_to_x86_equivalent),
        ),
        "ldr" => load_kind(operands),
        "str" => store_kind(operands),
        "add.addr" | "add.sp" => address_kind(operands),
        "bl" => call_value_kind(operands, &values),
        "csel" => select_value_kind(operands, &values),
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported aarch64 opcode for transpile: {base}"
        ))),
    }
}
