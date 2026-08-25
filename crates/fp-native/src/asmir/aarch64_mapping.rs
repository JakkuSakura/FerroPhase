use super::*;

pub(super) fn aarch64_detail(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64InstructionDetail {
    Aarch64InstructionDetail {
        opcode: aarch64_opcode_name(kind, ty).to_string(),
        operands: aarch64_typed_operands(id, kind, ty, ctx),
        condition: aarch64_condition(kind),
        call_target: aarch64_call_target(kind, ctx),
    }
}

pub(super) fn aarch64_opcode_name(kind: &AsmInstructionKind, ty: Option<&AsmType>) -> &'static str {
    match kind {
        AsmInstructionKind::Nop => "nop",
        AsmInstructionKind::Add(..) => "add",
        AsmInstructionKind::Sub(..) => "sub",
        AsmInstructionKind::Mul(..) if is_float_type_opt(ty) => {
            if matches!(ty, Some(AsmType::F32)) {
                "fmul.s"
            } else {
                "fmul.d"
            }
        }
        AsmInstructionKind::Mul(..) => "mul",
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) if is_float_type_opt(ty) => {
            if matches!(ty, Some(AsmType::F32)) {
                "fdiv.s"
            } else {
                "fdiv.d"
            }
        }
        AsmInstructionKind::Div(..) => "sdiv",
        AsmInstructionKind::Rem(..) => "msub.rem",
        AsmInstructionKind::And(..) => "and",
        AsmInstructionKind::Or(..) => "orr",
        AsmInstructionKind::Xor(..) => "eor",
        AsmInstructionKind::Shl(..) => "lsl",
        AsmInstructionKind::Shr(..) => "asr",
        AsmInstructionKind::Not(..) => "mvn",
        AsmInstructionKind::Eq(..)
        | AsmInstructionKind::Ne(..)
        | AsmInstructionKind::Lt(..)
        | AsmInstructionKind::Le(..)
        | AsmInstructionKind::Gt(..)
        | AsmInstructionKind::Ge(..)
        | AsmInstructionKind::Ult(..)
        | AsmInstructionKind::Ule(..)
        | AsmInstructionKind::Ugt(..)
        | AsmInstructionKind::Uge(..) => "cmp",
        AsmInstructionKind::Load { .. } => "ldr",
        AsmInstructionKind::Store { .. } => "str",
        AsmInstructionKind::Alloca { .. } => "add.sp",
        AsmInstructionKind::GetElementPtr { .. } => "add.addr",
        AsmInstructionKind::Bitcast(..)
        | AsmInstructionKind::PtrToInt(..)
        | AsmInstructionKind::IntToPtr(..)
        | AsmInstructionKind::Trunc(..)
        | AsmInstructionKind::ZExt(..)
        | AsmInstructionKind::SExt(..)
        | AsmInstructionKind::SextOrTrunc(..)
        | AsmInstructionKind::Freeze(..) => "mov",
        AsmInstructionKind::FPExt(..) => "fcvt.d.s",
        AsmInstructionKind::FPTrunc(..) => "fcvt.s.d",
        AsmInstructionKind::FPToUI(..) | AsmInstructionKind::FPToSI(..) => "fcvtzs",
        AsmInstructionKind::UIToFP(..) | AsmInstructionKind::SIToFP(..) => "scvtf",
        AsmInstructionKind::ExtractValue { .. } => "ldr.extract",
        AsmInstructionKind::InsertValue { .. } => "str.insert",
        AsmInstructionKind::Call { .. } | AsmInstructionKind::IntrinsicCall { .. } => "bl",
        AsmInstructionKind::Phi { .. } => "phi.copy",
        AsmInstructionKind::Select { .. } => "csel",
        AsmInstructionKind::InlineAsm { .. } => "inlineasm",
        AsmInstructionKind::LandingPad { .. } => "landingpad",
        AsmInstructionKind::Syscall { .. } => "svc",
        AsmInstructionKind::Splat { .. } => "dup",
        AsmInstructionKind::BuildVector { .. } => "build_vector",
        AsmInstructionKind::ExtractLane { .. } => "extract_lane",
        AsmInstructionKind::InsertLane { .. } => "insert_lane",
        AsmInstructionKind::ZipLow { .. } => "zip1",
        AsmInstructionKind::SymbolAddress { kind, .. } => match kind {
            fp_core::asmir::AsmSymbolAddressKind::Direct => "symaddr.direct",
            fp_core::asmir::AsmSymbolAddressKind::Got => "symaddr.got",
        },
        AsmInstructionKind::SysOp(_) => "sysop",
        AsmInstructionKind::Unreachable => "brk",
    }
}

pub(super) fn aarch64_typed_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Vec<Aarch64Operand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(Aarch64Operand::Register {
                reg: aarch64_virtual_register(id, ty),
                access: OperandAccess::Write,
            });
        }
    }
    match kind {
        AsmInstructionKind::Nop => {}
        AsmInstructionKind::Add(lhs, rhs)
        | AsmInstructionKind::Sub(lhs, rhs)
        | AsmInstructionKind::Mul(lhs, rhs)
        | AsmInstructionKind::Div(lhs, rhs)
        | AsmInstructionKind::Rem(lhs, rhs)
        | AsmInstructionKind::And(lhs, rhs)
        | AsmInstructionKind::Or(lhs, rhs)
        | AsmInstructionKind::Xor(lhs, rhs)
        | AsmInstructionKind::Shl(lhs, rhs)
        | AsmInstructionKind::Shr(lhs, rhs)
        | AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => {
            operands.push(aarch64_operand(lhs, ctx));
            operands.push(aarch64_operand(rhs, ctx));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(aarch64_operand(value, ctx)),
        AsmInstructionKind::Load { address, .. } => {
            operands.push(aarch64_address_operand(address, ty, ctx))
        }
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(aarch64_address_operand(address, None, ctx));
            operands.push(aarch64_operand(value, ctx));
        }
        AsmInstructionKind::Alloca { size, .. } => operands.push(aarch64_operand(size, ctx)),
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(aarch64_operand(ptr, ctx));
            operands.extend(indices.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::Bitcast(value, _)
        | AsmInstructionKind::Trunc(value, _)
        | AsmInstructionKind::ZExt(value, _)
        | AsmInstructionKind::SExt(value, _)
        | AsmInstructionKind::FPExt(value, _)
        | AsmInstructionKind::FPTrunc(value, _)
        | AsmInstructionKind::FPToUI(value, _)
        | AsmInstructionKind::FPToSI(value, _)
        | AsmInstructionKind::UIToFP(value, _)
        | AsmInstructionKind::SIToFP(value, _)
        | AsmInstructionKind::SextOrTrunc(value, _) => operands.push(aarch64_operand(value, ctx)),
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(aarch64_operand(aggregate, ctx));
            operands.extend(
                indices
                    .iter()
                    .map(|index| Aarch64Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(aarch64_operand(aggregate, ctx));
            operands.push(aarch64_operand(element, ctx));
            operands.extend(
                indices
                    .iter()
                    .map(|index| Aarch64Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::Call { function, .. } => {
            operands.push(match aarch64_call_target_from_value(function, ctx) {
                Aarch64CallTarget::Symbol(name) => Aarch64Operand::Symbol(name),
                Aarch64CallTarget::Register(reg) => Aarch64Operand::Register {
                    reg,
                    access: OperandAccess::Read,
                },
            });
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(Aarch64Operand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(args.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(aarch64_operand(value, ctx));
                operands.push(Aarch64Operand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            operands.push(aarch64_operand(condition, ctx));
            operands.push(aarch64_operand(if_true, ctx));
            operands.push(aarch64_operand(if_false, ctx));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(inputs.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(aarch64_operand(personality, ctx));
            }
        }
        AsmInstructionKind::Syscall { convention, .. } => {
            let imm = match convention {
                AsmSyscallConvention::LinuxAarch64 => 0,
                AsmSyscallConvention::DarwinAarch64 => 0x80,
                _ => 0,
            };
            operands.push(Aarch64Operand::Immediate(imm));
        }
        AsmInstructionKind::SysOp(_) => {}
        AsmInstructionKind::Splat { value, .. } => {
            operands.push(aarch64_operand(value, ctx));
        }
        AsmInstructionKind::BuildVector { elements } => {
            operands.extend(elements.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::ExtractLane { vector, lane } => {
            operands.push(aarch64_operand(vector, ctx));
            operands.push(Aarch64Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::InsertLane {
            vector,
            value,
            lane,
        } => {
            operands.push(aarch64_operand(vector, ctx));
            operands.push(aarch64_operand(value, ctx));
            operands.push(Aarch64Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            operands.push(aarch64_operand(lhs, ctx));
            operands.push(aarch64_operand(rhs, ctx));
        }
        AsmInstructionKind::SymbolAddress { symbol, .. } => {
            operands.push(Aarch64Operand::Symbol(Name::new(symbol.clone())));
        }
        AsmInstructionKind::Unreachable => {}
    }
    operands
}

pub(super) fn aarch64_condition(kind: &AsmInstructionKind) -> Option<Aarch64ConditionCode> {
    match kind {
        AsmInstructionKind::Eq(..) => Some(Aarch64ConditionCode::Eq),
        AsmInstructionKind::Ne(..) => Some(Aarch64ConditionCode::Ne),
        AsmInstructionKind::Lt(..) => Some(Aarch64ConditionCode::Lt),
        AsmInstructionKind::Le(..) => Some(Aarch64ConditionCode::Le),
        AsmInstructionKind::Gt(..) => Some(Aarch64ConditionCode::Gt),
        AsmInstructionKind::Ge(..) => Some(Aarch64ConditionCode::Ge),
        AsmInstructionKind::Ult(..) => Some(Aarch64ConditionCode::Lo),
        AsmInstructionKind::Ule(..) => Some(Aarch64ConditionCode::Ls),
        AsmInstructionKind::Ugt(..) => Some(Aarch64ConditionCode::Hi),
        AsmInstructionKind::Uge(..) => Some(Aarch64ConditionCode::Hs),
        AsmInstructionKind::Select { .. } => Some(Aarch64ConditionCode::NonZero),
        _ => None,
    }
}

pub(super) fn aarch64_call_target(
    kind: &AsmInstructionKind,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64CallTarget> {
    match kind {
        AsmInstructionKind::Call { function, .. } => {
            Some(aarch64_call_target_from_value(function, ctx))
        }
        AsmInstructionKind::IntrinsicCall { kind, .. } => Some(Aarch64CallTarget::Symbol(
            Name::new(format!("intrinsic.{kind:?}").to_ascii_lowercase()),
        )),
        _ => None,
    }
}

pub(super) fn aarch64_call_target_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64CallTarget {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => {
            Aarch64CallTarget::Symbol(Name::new(name.clone()))
        }
        AsmValue::Register(id) => {
            Aarch64CallTarget::Register(aarch64_virtual_register(*id, &AsmType::I64))
        }
        AsmValue::PhysicalRegister(register) => {
            Aarch64CallTarget::Register(map_physical_register_to_aarch64(register, ctx))
        }
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

pub(super) fn aarch64_terminator_detail(
    term: &AsmTerminator,
    instructions: &[AsmInstruction],
) -> Aarch64TerminatorDetail {
    match term {
        AsmTerminator::Return(_) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Ret,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Br(target) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::B,
            condition: None,
            targets: vec![*target],
        },
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::BCond,
            condition: resolve_aarch64_branch_condition(condition, instructions)
                .or(Some(Aarch64ConditionCode::NonZero)),
            targets: vec![*if_true, *if_false],
        },
        AsmTerminator::Switch { default, cases, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Switch,
            condition: None,
            targets: cases
                .iter()
                .map(|(_, target)| *target)
                .chain(std::iter::once(*default))
                .collect(),
        },
        AsmTerminator::IndirectBr { destinations, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Br,
            condition: None,
            targets: destinations.clone(),
        },
        AsmTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Invoke,
            condition: None,
            targets: vec![*normal_dest, *unwind_dest],
        },
        AsmTerminator::Resume(_) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Resume,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Unreachable => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Brk,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::CleanupRet { unwind_dest, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CleanupRet,
            condition: None,
            targets: unwind_dest.iter().copied().collect(),
        },
        AsmTerminator::CatchRet { successor, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CatchRet,
            condition: None,
            targets: vec![*successor],
        },
        AsmTerminator::CatchSwitch {
            handlers,
            unwind_dest,
            ..
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CatchSwitch,
            condition: None,
            targets: handlers
                .iter()
                .copied()
                .chain(unwind_dest.iter().copied())
                .collect(),
        },
    }
}

pub(super) fn resolve_aarch64_branch_condition(
    condition: &AsmValue,
    instructions: &[AsmInstruction],
) -> Option<Aarch64ConditionCode> {
    match condition {
        AsmValue::Flags(id) => instructions
            .iter()
            .find(|instruction| instruction.id == *id)
            .and_then(|instruction| comparison_code_from_kind(&instruction.kind))
            .map(|code| aarch64_condition_from_asm(&code)),
        other => aarch64_branch_condition(other),
    }
}

pub(super) fn collect_machine_values(operands: &[AsmOperand]) -> Result<Vec<AsmValue>> {
    operands.iter().map(machine_operand_to_value).collect()
}

pub(super) fn machine_operand_to_value(operand: &AsmOperand) -> Result<AsmValue> {
    match operand {
        AsmOperand::Register {
            reg: AsmRegister::Virtual { id, .. },
            ..
        } => Ok(AsmValue::Register(*id)),
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            ..
        } => Ok(AsmValue::PhysicalRegister(register.clone())),
        AsmOperand::Immediate(value) => Ok(AsmValue::Constant(AsmConstant::Int(
            *value as i64,
            AsmType::I64,
        ))),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => {
            Ok(AsmValue::Function(name.to_string()))
        }
        AsmOperand::Block(id) => Ok(AsmValue::Constant(AsmConstant::UInt(
            *id as u64,
            AsmType::I32,
        ))),
        AsmOperand::Memory(memory) => memory_address_value(memory),
        _ => Err(fp_core::error::Error::from(
            "machine transpile currently supports only register, immediate, symbol, block, and memory operands",
        )),
    }
}

pub(super) fn memory_address_value(memory: &AsmMemoryOperand) -> Result<AsmValue> {
    Ok(AsmValue::Address(Box::new(address_value_from_memory(
        memory,
    ))))
}

pub(super) fn binary_value_kind<F>(
    operands: &[AsmOperand],
    values: &[AsmValue],
    build: F,
) -> Result<AsmInstructionKind>
where
    F: Fn(AsmValue, AsmValue) -> AsmInstructionKind,
{
    let first_read = first_read_operand_index(operands);
    Ok(build(
        values
            .get(first_read)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing lhs operand"))?,
        values
            .get(first_read + 1)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing rhs operand"))?,
    ))
}

pub(super) fn unary_value_kind<F>(
    operands: &[AsmOperand],
    values: &[AsmValue],
    build: F,
) -> Result<AsmInstructionKind>
where
    F: Fn(AsmValue) -> AsmInstructionKind,
{
    let first_read = first_read_operand_index(operands);
    Ok(build(values.get(first_read).cloned().ok_or_else(|| {
        fp_core::error::Error::from("missing operand")
    })?))
}

pub(super) fn compare_value_kind(
    operands: &[AsmOperand],
    values: &[AsmValue],
    condition: Option<X86ConditionCode>,
) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    let lhs = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare lhs"))?;
    let rhs = values
        .get(first_read + 1)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare rhs"))?;
    Ok(match condition.unwrap_or(X86ConditionCode::NonZero) {
        X86ConditionCode::Equal => AsmInstructionKind::Eq(lhs, rhs),
        X86ConditionCode::NotEqual => AsmInstructionKind::Ne(lhs, rhs),
        X86ConditionCode::Less => AsmInstructionKind::Lt(lhs, rhs),
        X86ConditionCode::LessEqual => AsmInstructionKind::Le(lhs, rhs),
        X86ConditionCode::Greater => AsmInstructionKind::Gt(lhs, rhs),
        X86ConditionCode::GreaterEqual => AsmInstructionKind::Ge(lhs, rhs),
        X86ConditionCode::Below => AsmInstructionKind::Ult(lhs, rhs),
        X86ConditionCode::BelowEqual => AsmInstructionKind::Ule(lhs, rhs),
        X86ConditionCode::Above => AsmInstructionKind::Ugt(lhs, rhs),
        X86ConditionCode::AboveEqual => AsmInstructionKind::Uge(lhs, rhs),
        X86ConditionCode::NonZero => {
            AsmInstructionKind::Ne(lhs, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)))
        }
    })
}

pub(super) fn call_value_kind(
    operands: &[AsmOperand],
    values: &[AsmValue],
) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    let function = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing call target"))?;
    let args = values.iter().skip(first_read + 1).cloned().collect();
    Ok(AsmInstructionKind::Call {
        function,
        args,
        calling_convention: fp_core::lir::CallingConvention::C,
        tail_call: false,
    })
}

pub(super) fn select_value_kind(
    operands: &[AsmOperand],
    values: &[AsmValue],
) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    let condition = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing select condition"))?;
    Ok(AsmInstructionKind::Select {
        condition,
        if_true: values
            .get(first_read + 1)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing select if_true"))?,
        if_false: values
            .get(first_read + 2)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing select if_false"))?,
    })
}

pub(super) fn x86_mov_kind(
    operands: &[AsmOperand],
    values: &[AsmValue],
) -> Result<AsmInstructionKind> {
    match (operands.first(), operands.get(1)) {
        (Some(AsmOperand::Register { .. }), Some(AsmOperand::Memory(_))) => load_kind(operands),
        (Some(AsmOperand::Memory(_)), Some(_)) => store_kind(operands),
        _ => unary_value_kind(operands, values, |value| AsmInstructionKind::Freeze(value)),
    }
}

pub(super) fn load_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let address = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing load memory operand"))?;
    Ok(AsmInstructionKind::Load {
        address,
        alignment: None,
        volatile: false,
    })
}

pub(super) fn store_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let address = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing store memory operand"))?;
    let value = operands
        .iter()
        .find(|operand| !matches!(operand, AsmOperand::Memory(_)))
        .ok_or_else(|| fp_core::error::Error::from("missing store value operand"))
        .and_then(machine_operand_to_value)?;
    Ok(AsmInstructionKind::Store {
        value,
        address,
        alignment: None,
        volatile: false,
    })
}

pub(super) fn address_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let ptr = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            AsmOperand::Register { .. } | AsmOperand::Symbol(_) | AsmOperand::Label(_) => {
                Some(machine_operand_to_value(operand))
            }
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing address operand"))?;
    Ok(AsmInstructionKind::GetElementPtr {
        ptr,
        indices: Vec::new(),
        inbounds: false,
    })
}

pub(super) fn first_read_operand_index(operands: &[AsmOperand]) -> usize {
    operands
        .iter()
        .position(|operand| {
            !matches!(
                operand,
                AsmOperand::Register {
                    access: OperandAccess::Write,
                    ..
                }
            )
        })
        .unwrap_or(0)
}
