use super::*;

pub(super) fn x86_opcode(kind: &AsmInstructionKind, ty: Option<&AsmType>) -> X86Opcode {
    match kind {
        AsmInstructionKind::Nop => X86Opcode::Nop,
        AsmInstructionKind::Add(..) => X86Opcode::Add,
        AsmInstructionKind::Sub(..) => X86Opcode::Sub,
        AsmInstructionKind::Mul(..) if is_float_type_opt(ty) => float_binop_opcode("mul", ty),
        AsmInstructionKind::Mul(..) => X86Opcode::IMul,
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) if is_float_type_opt(ty) => {
            float_binop_opcode("div", ty)
        }
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) => X86Opcode::IDiv,
        AsmInstructionKind::And(..) => X86Opcode::And,
        AsmInstructionKind::Or(..) => X86Opcode::Or,
        AsmInstructionKind::Xor(..) => X86Opcode::Xor,
        AsmInstructionKind::Shl(..) => X86Opcode::Shl,
        AsmInstructionKind::Shr(..) => X86Opcode::Sar,
        AsmInstructionKind::Not(..) => X86Opcode::Not,
        AsmInstructionKind::Eq(..)
        | AsmInstructionKind::Ne(..)
        | AsmInstructionKind::Lt(..)
        | AsmInstructionKind::Le(..)
        | AsmInstructionKind::Gt(..)
        | AsmInstructionKind::Ge(..)
        | AsmInstructionKind::Ult(..)
        | AsmInstructionKind::Ule(..)
        | AsmInstructionKind::Ugt(..)
        | AsmInstructionKind::Uge(..) => X86Opcode::Cmp,
        AsmInstructionKind::Load { .. } | AsmInstructionKind::Store { .. } => X86Opcode::Mov,
        AsmInstructionKind::Alloca { .. } => X86Opcode::LeaFrame,
        AsmInstructionKind::GetElementPtr { .. } => X86Opcode::Lea,
        AsmInstructionKind::Bitcast(..)
        | AsmInstructionKind::PtrToInt(..)
        | AsmInstructionKind::IntToPtr(..)
        | AsmInstructionKind::Trunc(..)
        | AsmInstructionKind::ZExt(..)
        | AsmInstructionKind::SExt(..)
        | AsmInstructionKind::SextOrTrunc(..)
        | AsmInstructionKind::Freeze(..) => X86Opcode::Mov,
        AsmInstructionKind::FPExt(..) => X86Opcode::Cvtss2sd,
        AsmInstructionKind::FPTrunc(..) => X86Opcode::Cvtsd2ss,
        AsmInstructionKind::FPToUI(..) | AsmInstructionKind::FPToSI(..) => X86Opcode::Cvttsd2si,
        AsmInstructionKind::UIToFP(..) | AsmInstructionKind::SIToFP(..) => X86Opcode::Cvtsi2sd,
        AsmInstructionKind::ExtractValue { .. } => X86Opcode::MovExtract,
        AsmInstructionKind::InsertValue { .. } => X86Opcode::MovInsert,
        AsmInstructionKind::Call { .. } | AsmInstructionKind::IntrinsicCall { .. } => {
            X86Opcode::Call
        }
        AsmInstructionKind::Phi { .. } => X86Opcode::PhiCopy,
        AsmInstructionKind::Select { .. } => X86Opcode::CMov,
        AsmInstructionKind::InlineAsm { .. } => X86Opcode::InlineAsm,
        AsmInstructionKind::LandingPad { .. } => X86Opcode::LandingPad,
        AsmInstructionKind::Syscall { .. } => X86Opcode::Syscall,
        AsmInstructionKind::SysOp(_) => X86Opcode::InlineAsm,
        AsmInstructionKind::Splat { .. } => X86Opcode::Mov,
        AsmInstructionKind::BuildVector { .. }
        | AsmInstructionKind::ExtractLane { .. }
        | AsmInstructionKind::InsertLane { .. }
        | AsmInstructionKind::ZipLow { .. } => X86Opcode::Mov,
        AsmInstructionKind::SymbolAddress { .. } => X86Opcode::Mov,
        AsmInstructionKind::Unreachable => X86Opcode::Ud2,
    }
}

pub(super) fn float_binop_opcode(base: &str, ty: Option<&AsmType>) -> X86Opcode {
    match ty {
        Some(AsmType::F32) => match base {
            "mul" => X86Opcode::Mulss,
            "div" => X86Opcode::Divss,
            _ => X86Opcode::Mov,
        },
        Some(AsmType::F64) => match base {
            "mul" => X86Opcode::Mulsd,
            "div" => X86Opcode::Divsd,
            _ => X86Opcode::Mov,
        },
        _ => X86Opcode::Mov,
    }
}

pub(super) fn x86_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> Vec<AsmOperand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(register_operand(
                virtual_register(id, &backend_operand_type(ty)),
                OperandAccess::Write,
            ));
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
            operands.push(value_operand(lhs, register_types));
            operands.push(value_operand(rhs, register_types));
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            operands.push(value_operand(lhs, register_types));
            operands.push(value_operand(rhs, register_types));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(value_operand(value, register_types)),
        AsmInstructionKind::Load { address, .. } => {
            operands.push(address_operand(address, ty, register_types))
        }
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(address_operand(address, None, register_types));
            operands.push(value_operand(value, register_types));
        }
        AsmInstructionKind::Alloca { size, .. } => {
            operands.push(value_operand(size, register_types))
        }
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(value_operand(ptr, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
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
        | AsmInstructionKind::SextOrTrunc(value, _) => {
            operands.push(value_operand(value, register_types))
        }
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(value_operand(aggregate, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|index| AsmOperand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(value_operand(aggregate, register_types));
            operands.push(value_operand(element, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|index| AsmOperand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::Call { function, .. } => {
            // Call arguments are semantic (ABI-lowered), not textual operands.
            operands.push(call_target_operand(function, register_types));
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(AsmOperand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(
                args.iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(value_operand(value, register_types));
                operands.push(AsmOperand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            operands.push(value_operand(condition, register_types));
            operands.push(value_operand(if_true, register_types));
            operands.push(value_operand(if_false, register_types));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(
                inputs
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(value_operand(personality, register_types));
            }
        }
        AsmInstructionKind::Syscall { .. } => {}
        AsmInstructionKind::SysOp(_) => {}
        AsmInstructionKind::Splat { value, .. } => {
            operands.push(value_operand(value, register_types))
        }
        AsmInstructionKind::BuildVector { elements } => {
            operands.extend(
                elements
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::ExtractLane { vector, lane } => {
            operands.push(value_operand(vector, register_types));
            operands.push(AsmOperand::Immediate((*lane).into()));
        }
        AsmInstructionKind::InsertLane {
            vector,
            lane,
            value,
        } => {
            operands.push(value_operand(vector, register_types));
            operands.push(value_operand(value, register_types));
            operands.push(AsmOperand::Immediate((*lane).into()));
        }
        AsmInstructionKind::SymbolAddress { symbol, .. } => {
            operands.push(AsmOperand::Symbol(Name::new(symbol.clone())));
        }
        AsmInstructionKind::Unreachable => {}
    }

    operands
}

pub(super) fn generic_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> Vec<AsmOperand> {
    x86_operands(id, kind, ty, register_types)
}

pub(super) fn instruction_produces_value(kind: &AsmInstructionKind) -> bool {
    !matches!(
        kind,
        AsmInstructionKind::Store { .. }
            | AsmInstructionKind::Call { .. }
            | AsmInstructionKind::IntrinsicCall { .. }
            | AsmInstructionKind::Unreachable
    )
}

pub(super) fn value_operand(
    value: &AsmValue,
    register_types: &HashMap<u32, AsmType>,
) -> AsmOperand {
    match value {
        AsmValue::Register(id) => register_operand(
            virtual_register(
                *id,
                &register_types
                    .get(id)
                    .map(backend_operand_type)
                    .unwrap_or_else(|| panic!("missing type for virtual register {id}")),
            ),
            OperandAccess::Read,
        ),
        AsmValue::PhysicalRegister(register) => {
            register_operand(AsmRegister::Physical(register.clone()), OperandAccess::Read)
        }
        AsmValue::Address(address) => AsmOperand::Memory(memory_from_address_value(address)),
        AsmValue::Condition(condition) => {
            AsmOperand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => AsmOperand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => AsmOperand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) => AsmOperand::Immediate(0),
        AsmValue::Null(_) => AsmOperand::Immediate(0),
    }
}

pub(super) fn x86_operand(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match value {
        AsmValue::Register(id) => X86Operand::Register {
            reg: x86_virtual_register(*id, &ctx.register_type(*id)),
            access: OperandAccess::Read,
        },
        AsmValue::PhysicalRegister(register) => X86Operand::Register {
            reg: map_physical_register_to_x86(register, ctx),
            access: OperandAccess::Read,
        },
        AsmValue::Address(address) => x86_address_value_operand(address, ctx),
        AsmValue::Condition(condition) => {
            X86Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => X86Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => X86Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => x86_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            X86Operand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => X86Operand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => X86Operand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) | AsmValue::Null(_) => X86Operand::Immediate(0),
    }
}

pub(super) fn x86_address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match address {
        AsmValue::Address(address) => x86_memory_or_symbol_from_address(address, ty, ctx),
        AsmValue::Register(id) => X86Operand::Memory(X86MemoryOperand {
            base: Some(x86_virtual_register(
                *id,
                &AsmType::Ptr(Box::new(AsmType::I8)),
            )),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::PhysicalRegister(register) => X86Operand::Memory(X86MemoryOperand {
            base: Some(map_physical_register_to_x86(register, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            X86Operand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => X86Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => X86Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => x86_operand(address, ctx),
    }
}

pub(super) fn x86_constant_operand(constant: &AsmConstant) -> X86Operand {
    match constant {
        AsmConstant::Int(value, _) => X86Operand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => X86Operand::Immediate(*value as i128),
        AsmConstant::Bool(value) => X86Operand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => X86Operand::Immediate(0),
        AsmConstant::Float(value, ty) => X86Operand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => {
            X86Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => X86Operand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            X86Operand::Symbol(name.clone())
        }
        AsmConstant::Array(..) => X86Operand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => X86Operand::Symbol(Name::new("const.struct")),
    }
}

pub(super) fn call_target_operand(
    value: &AsmValue,
    register_types: &HashMap<u32, AsmType>,
) -> AsmOperand {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        _ => value_operand(value, register_types),
    }
}

pub(super) fn address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> AsmOperand {
    match address {
        AsmValue::Address(address) => {
            let mut memory = memory_from_address_value(address);
            if memory.size_bytes.is_none() {
                memory.size_bytes = ty.map(type_size_bytes);
            }
            AsmOperand::Memory(memory)
        }
        AsmValue::Register(id) => AsmOperand::Memory(AsmMemoryOperand {
            base: Some(virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
            index: None,
            scale: 1,
            displacement: 0,
            segment: None,
            size_bytes: ty.map(type_size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        AsmValue::PhysicalRegister(register) => AsmOperand::Memory(AsmMemoryOperand {
            base: Some(AsmRegister::Physical(register.clone())),
            index: None,
            scale: 1,
            displacement: 0,
            segment: None,
            size_bytes: ty.map(type_size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => value_operand(address, register_types),
    }
}

pub(super) fn address_value_from_memory(memory: &AsmMemoryOperand) -> AsmAddressValue {
    AsmAddressValue {
        base: memory
            .base
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        index: memory
            .index
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        scale: memory.scale,
        displacement: memory.displacement,
        segment: memory
            .segment
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        size_bytes: memory.size_bytes,
        address_space: memory.address_space,
        pre_indexed: memory.pre_indexed,
        post_indexed: memory.post_indexed,
    }
}

pub(super) fn memory_from_address_value(address: &AsmAddressValue) -> AsmMemoryOperand {
    AsmMemoryOperand {
        base: address.base.as_deref().and_then(address_component_register),
        index: address
            .index
            .as_deref()
            .and_then(address_component_register),
        scale: address.scale,
        displacement: address.displacement,
        segment: address
            .segment
            .as_deref()
            .and_then(address_component_register),
        size_bytes: address.size_bytes,
        address_space: address.address_space,
        pre_indexed: address.pre_indexed,
        post_indexed: address.post_indexed,
    }
}

pub(super) fn register_value_from_asm(register: &AsmRegister) -> AsmValue {
    match register {
        AsmRegister::Physical(register) => AsmValue::PhysicalRegister(register.clone()),
        AsmRegister::Virtual { id, .. } => AsmValue::Register(*id),
    }
}

pub(super) fn address_component_register(value: &AsmValue) -> Option<AsmRegister> {
    match value {
        AsmValue::Register(id) => Some(virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
        AsmValue::PhysicalRegister(register) => Some(AsmRegister::Physical(register.clone())),
        _ => None,
    }
}

pub(super) fn x86_address_value_operand(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    x86_memory_or_symbol_from_address(address, None, ctx)
}

pub(super) fn x86_memory_or_symbol_from_address(
    address: &AsmAddressValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    if let Some(symbol) = address_symbol_name(address) {
        return X86Operand::Symbol(Name::new(symbol));
    }
    let mut memory = x86_memory_from_address(address, ctx);
    if memory.size_bytes.is_none() {
        memory.size_bytes = ty.map(type_size_bytes);
    }
    X86Operand::Memory(memory)
}

pub(super) fn x86_memory_from_address(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86MemoryOperand {
    X86MemoryOperand {
        base: address
            .base
            .as_deref()
            .and_then(|value| x86_register_from_value(value, ctx)),
        index: address
            .index
            .as_deref()
            .and_then(|value| x86_register_from_value(value, ctx)),
        scale: address.scale,
        displacement: address.displacement,
        size_bytes: address.size_bytes,
    }
}

pub(super) fn aarch64_address_value_operand(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    aarch64_memory_or_symbol_from_address(address, None, ctx)
}

pub(super) fn aarch64_memory_or_symbol_from_address(
    address: &AsmAddressValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    if let Some(symbol) = address_symbol_name(address) {
        return Aarch64Operand::Symbol(Name::new(symbol));
    }
    let mut memory = aarch64_memory_from_address(address, ctx);
    if memory.size_bytes.is_none() {
        memory.size_bytes = ty.map(type_size_bytes);
    }
    Aarch64Operand::Memory(memory)
}

pub(super) fn aarch64_memory_from_address(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64MemoryOperand {
    Aarch64MemoryOperand {
        base: address
            .base
            .as_deref()
            .and_then(|value| aarch64_register_from_value(value, ctx)),
        index: address
            .index
            .as_deref()
            .and_then(|value| aarch64_register_from_value(value, ctx)),
        scale: address.scale,
        displacement: address.displacement,
        size_bytes: address.size_bytes,
    }
}

pub(super) fn x86_register_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<X86Register> {
    match value {
        AsmValue::Register(id) => Some(x86_virtual_register(
            *id,
            &AsmType::Ptr(Box::new(AsmType::I8)),
        )),
        AsmValue::PhysicalRegister(register) => Some(map_physical_register_to_x86(register, ctx)),
        _ => None,
    }
}

pub(super) fn aarch64_register_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64Register> {
    match value {
        AsmValue::Register(id) => Some(aarch64_virtual_register(
            *id,
            &AsmType::Ptr(Box::new(AsmType::I8)),
        )),
        AsmValue::PhysicalRegister(register) => {
            Some(map_physical_register_to_aarch64(register, ctx))
        }
        _ => None,
    }
}

pub(super) fn address_symbol_name(address: &AsmAddressValue) -> Option<String> {
    if address.index.is_some() || address.segment.is_some() || address.displacement != 0 {
        return None;
    }
    match address.base.as_deref() {
        Some(AsmValue::Global(name, _)) | Some(AsmValue::Function(name)) => Some(name.clone()),
        _ => None,
    }
}

pub(super) fn constant_operand(constant: &AsmConstant) -> AsmOperand {
    match constant {
        AsmConstant::Int(value, _) => AsmOperand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => AsmOperand::Immediate(*value as i128),
        AsmConstant::Bool(value) => AsmOperand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => AsmOperand::Immediate(0),
        AsmConstant::Float(value, ty) => AsmOperand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => {
            AsmOperand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => AsmOperand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            AsmOperand::Symbol(name.clone())
        }
        AsmConstant::Array(..) => AsmOperand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => AsmOperand::Symbol(Name::new("const.struct")),
    }
}

pub(super) fn register_operand(reg: AsmRegister, access: OperandAccess) -> AsmOperand {
    AsmOperand::Register { reg, access }
}

/// Native aggregate values are represented by addresses of their storage.
/// ABI expansion is handled later by the target-specific call lowering.
pub(super) fn backend_operand_type(ty: &AsmType) -> AsmType {
    match ty {
        AsmType::Struct { .. } | AsmType::Array(_, _) => AsmType::Ptr(Box::new(ty.clone())),
        _ => ty.clone(),
    }
}

pub(super) fn virtual_register(id: u32, ty: &AsmType) -> AsmRegister {
    AsmRegister::Virtual {
        id,
        bank: register_bank(ty),
        size_bits: type_size_bits(ty),
    }
}

pub(super) fn x86_virtual_register(id: u32, ty: &AsmType) -> X86Register {
    X86Register::Virtual {
        id,
        size_bits: type_size_bits(ty),
    }
}

pub(super) fn x86_branch_condition(value: &AsmValue) -> Option<X86ConditionCode> {
    match value {
        AsmValue::Condition(condition) => Some(x86_condition_from_asm(condition)),
        AsmValue::Comparison(comparison) => Some(x86_condition_from_asm(&comparison.condition)),
        AsmValue::Flags(_) => None,
        _ => branch_condition_name(value).and_then(parse_x86_condition_token),
    }
}

pub(super) fn aarch64_branch_condition(value: &AsmValue) -> Option<Aarch64ConditionCode> {
    match value {
        AsmValue::Condition(condition) => Some(aarch64_condition_from_asm(condition)),
        AsmValue::Comparison(comparison) => Some(aarch64_condition_from_asm(&comparison.condition)),
        AsmValue::Flags(_) => None,
        _ => branch_condition_name(value).and_then(parse_aarch64_condition_token),
    }
}

pub(super) fn branch_condition_name(value: &AsmValue) -> Option<&str> {
    match value {
        AsmValue::Global(name, _) | AsmValue::Function(name) => name.strip_prefix("cc."),
        _ => None,
    }
}

pub(super) fn x86_condition_from_asm(condition: &AsmConditionCode) -> X86ConditionCode {
    match condition {
        AsmConditionCode::Eq => X86ConditionCode::Equal,
        AsmConditionCode::Ne => X86ConditionCode::NotEqual,
        AsmConditionCode::Lt => X86ConditionCode::Less,
        AsmConditionCode::Le => X86ConditionCode::LessEqual,
        AsmConditionCode::Gt => X86ConditionCode::Greater,
        AsmConditionCode::Ge => X86ConditionCode::GreaterEqual,
        AsmConditionCode::Ult => X86ConditionCode::Below,
        AsmConditionCode::Ule => X86ConditionCode::BelowEqual,
        AsmConditionCode::Ugt => X86ConditionCode::Above,
        AsmConditionCode::Uge => X86ConditionCode::AboveEqual,
        AsmConditionCode::Nz => X86ConditionCode::NonZero,
    }
}

pub(super) fn aarch64_condition_from_asm(condition: &AsmConditionCode) -> Aarch64ConditionCode {
    match condition {
        AsmConditionCode::Eq => Aarch64ConditionCode::Eq,
        AsmConditionCode::Ne => Aarch64ConditionCode::Ne,
        AsmConditionCode::Lt => Aarch64ConditionCode::Lt,
        AsmConditionCode::Le => Aarch64ConditionCode::Le,
        AsmConditionCode::Gt => Aarch64ConditionCode::Gt,
        AsmConditionCode::Ge => Aarch64ConditionCode::Ge,
        AsmConditionCode::Ult => Aarch64ConditionCode::Lo,
        AsmConditionCode::Ule => Aarch64ConditionCode::Ls,
        AsmConditionCode::Ugt => Aarch64ConditionCode::Hi,
        AsmConditionCode::Uge => Aarch64ConditionCode::Hs,
        AsmConditionCode::Nz => Aarch64ConditionCode::NonZero,
    }
}

pub(super) fn asm_condition_from_x86(condition: &X86ConditionCode) -> AsmConditionCode {
    match condition {
        X86ConditionCode::Equal => AsmConditionCode::Eq,
        X86ConditionCode::NotEqual => AsmConditionCode::Ne,
        X86ConditionCode::Less => AsmConditionCode::Lt,
        X86ConditionCode::LessEqual => AsmConditionCode::Le,
        X86ConditionCode::Greater => AsmConditionCode::Gt,
        X86ConditionCode::GreaterEqual => AsmConditionCode::Ge,
        X86ConditionCode::Below => AsmConditionCode::Ult,
        X86ConditionCode::BelowEqual => AsmConditionCode::Ule,
        X86ConditionCode::Above => AsmConditionCode::Ugt,
        X86ConditionCode::AboveEqual => AsmConditionCode::Uge,
        X86ConditionCode::NonZero => AsmConditionCode::Nz,
    }
}

pub(super) fn asm_condition_from_aarch64(condition: &Aarch64ConditionCode) -> AsmConditionCode {
    match condition {
        Aarch64ConditionCode::Eq => AsmConditionCode::Eq,
        Aarch64ConditionCode::Ne => AsmConditionCode::Ne,
        Aarch64ConditionCode::Lt => AsmConditionCode::Lt,
        Aarch64ConditionCode::Le => AsmConditionCode::Le,
        Aarch64ConditionCode::Gt => AsmConditionCode::Gt,
        Aarch64ConditionCode::Ge => AsmConditionCode::Ge,
        Aarch64ConditionCode::Lo => AsmConditionCode::Ult,
        Aarch64ConditionCode::Ls => AsmConditionCode::Ule,
        Aarch64ConditionCode::Hi => AsmConditionCode::Ugt,
        Aarch64ConditionCode::Hs => AsmConditionCode::Uge,
        Aarch64ConditionCode::NonZero => AsmConditionCode::Nz,
    }
}
