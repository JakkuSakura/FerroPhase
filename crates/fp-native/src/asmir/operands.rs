use super::*;

pub(super) fn aarch64_operand(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match value {
        AsmValue::Register(id) => Aarch64Operand::Register {
            reg: aarch64_virtual_register(*id, &AsmType::I64),
            access: OperandAccess::Read,
        },
        AsmValue::PhysicalRegister(register) => Aarch64Operand::Register {
            reg: map_physical_register_to_aarch64(register, ctx),
            access: OperandAccess::Read,
        },
        AsmValue::Address(address) => aarch64_address_value_operand(address, ctx),
        AsmValue::Condition(condition) => {
            Aarch64Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => Aarch64Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => Aarch64Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => aarch64_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            Aarch64Operand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => Aarch64Operand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => Aarch64Operand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) | AsmValue::Null(_) => Aarch64Operand::Immediate(0),
    }
}

pub(super) fn aarch64_address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match address {
        AsmValue::Address(address) => aarch64_memory_or_symbol_from_address(address, ty, ctx),
        AsmValue::Register(id) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: Some(aarch64_virtual_register(
                *id,
                &AsmType::Ptr(Box::new(AsmType::I8)),
            )),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::PhysicalRegister(register) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: Some(map_physical_register_to_aarch64(register, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            Aarch64Operand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => Aarch64Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => Aarch64Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => aarch64_operand(address, ctx),
    }
}

fn aarch64_constant_operand(constant: &AsmConstant) -> Aarch64Operand {
    match constant {
        AsmConstant::Int(value, _) => Aarch64Operand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => Aarch64Operand::Immediate(*value as i128),
        AsmConstant::Bool(value) => Aarch64Operand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Aarch64Operand::Immediate(0),
        AsmConstant::Float(value, ty) => Aarch64Operand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => {
            Aarch64Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => Aarch64Operand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            Aarch64Operand::Symbol(name.clone())
        }
        AsmConstant::Array(..) => Aarch64Operand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => Aarch64Operand::Symbol(Name::new("const.struct")),
    }
}

pub(super) fn aarch64_virtual_register(id: u32, ty: &AsmType) -> Aarch64Register {
    Aarch64Register::Virtual {
        id,
        size_bits: type_size_bits(ty),
    }
}

pub(super) fn register_bank(ty: &AsmType) -> AsmRegisterBank {
    match ty {
        AsmType::F32 | AsmType::F64 => AsmRegisterBank::Float,
        AsmType::Vector(..) => AsmRegisterBank::Vector,
        _ => AsmRegisterBank::General,
    }
}

pub(super) fn type_size_bits(ty: &AsmType) -> u16 {
    let bytes = type_size_bytes(ty);
    if bytes == 0 {
        64
    } else {
        bytes.saturating_mul(8)
    }
}

pub(super) fn type_size_bytes(ty: &AsmType) -> u16 {
    let size = match ty {
        AsmType::I1 | AsmType::I8 => 1,
        AsmType::I16 => 2,
        AsmType::I32 | AsmType::F32 => 4,
        AsmType::I64 | AsmType::F64 | AsmType::Ptr(_) | AsmType::Function { .. } => 8,
        AsmType::I128 => 16,
        AsmType::Integer(width) => u64::from(width.div_ceil(8)),
        AsmType::Array(element, count) => u64::from(type_size_bytes(element)) * *count,
        AsmType::Vector(element, count) => u64::from(type_size_bytes(element)) * u64::from(*count),
        AsmType::Struct { fields, .. } => fields.iter().map(type_size_bytes).map(u64::from).sum(),
        AsmType::Void | AsmType::Label | AsmType::Token | AsmType::Metadata => 0,
        AsmType::Error => 0,
    };
    size.min(u64::from(u16::MAX)) as u16
}

pub(super) fn is_float_type_opt(ty: Option<&AsmType>) -> bool {
    matches!(ty, Some(AsmType::F32 | AsmType::F64))
}

pub(super) fn float_bits(value: f64, ty: &AsmType) -> u64 {
    match ty {
        AsmType::F32 => (value as f32).to_bits() as u64,
        _ => value.to_bits(),
    }
}

pub(super) fn sanitize_symbol(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    out.truncate(24);
    if out.is_empty() {
        "literal".to_string()
    } else {
        out
    }
}

pub(super) fn generic_opcode(kind: &AsmInstructionKind) -> AsmGenericOpcode {
    match kind {
        AsmInstructionKind::Nop => AsmGenericOpcode::Nop,
        AsmInstructionKind::Add(..) => AsmGenericOpcode::Add,
        AsmInstructionKind::Sub(..) => AsmGenericOpcode::Sub,
        AsmInstructionKind::Mul(..) => AsmGenericOpcode::Mul,
        AsmInstructionKind::Div(..) => AsmGenericOpcode::Div,
        AsmInstructionKind::Rem(..) => AsmGenericOpcode::Rem,
        AsmInstructionKind::And(..) => AsmGenericOpcode::And,
        AsmInstructionKind::Or(..) => AsmGenericOpcode::Or,
        AsmInstructionKind::Xor(..) => AsmGenericOpcode::Xor,
        AsmInstructionKind::Shl(..) => AsmGenericOpcode::Shl,
        AsmInstructionKind::Shr(..) => AsmGenericOpcode::Shr,
        AsmInstructionKind::Not(..) => AsmGenericOpcode::Not,
        AsmInstructionKind::Eq(..) => AsmGenericOpcode::Eq,
        AsmInstructionKind::Ne(..) => AsmGenericOpcode::Ne,
        AsmInstructionKind::Lt(..) => AsmGenericOpcode::Lt,
        AsmInstructionKind::Le(..) => AsmGenericOpcode::Le,
        AsmInstructionKind::Gt(..) => AsmGenericOpcode::Gt,
        AsmInstructionKind::Ge(..) => AsmGenericOpcode::Ge,
        AsmInstructionKind::Ult(..) => AsmGenericOpcode::Ult,
        AsmInstructionKind::Ule(..) => AsmGenericOpcode::Ule,
        AsmInstructionKind::Ugt(..) => AsmGenericOpcode::Ugt,
        AsmInstructionKind::Uge(..) => AsmGenericOpcode::Uge,
        AsmInstructionKind::Load { .. } => AsmGenericOpcode::Load,
        AsmInstructionKind::Store { .. } => AsmGenericOpcode::Store,
        AsmInstructionKind::Alloca { .. } => AsmGenericOpcode::Alloca,
        AsmInstructionKind::GetElementPtr { .. } => AsmGenericOpcode::GetElementPtr,
        AsmInstructionKind::Bitcast(..) => AsmGenericOpcode::Bitcast,
        AsmInstructionKind::PtrToInt(..) => AsmGenericOpcode::PtrToInt,
        AsmInstructionKind::IntToPtr(..) => AsmGenericOpcode::IntToPtr,
        AsmInstructionKind::Trunc(..) => AsmGenericOpcode::Trunc,
        AsmInstructionKind::ZExt(..) => AsmGenericOpcode::ZExt,
        AsmInstructionKind::SExt(..) => AsmGenericOpcode::SExt,
        AsmInstructionKind::FPExt(..) => AsmGenericOpcode::FPExt,
        AsmInstructionKind::FPTrunc(..) => AsmGenericOpcode::FPTrunc,
        AsmInstructionKind::FPToUI(..) => AsmGenericOpcode::FPToUI,
        AsmInstructionKind::FPToSI(..) => AsmGenericOpcode::FPToSI,
        AsmInstructionKind::UIToFP(..) => AsmGenericOpcode::UIToFP,
        AsmInstructionKind::SIToFP(..) => AsmGenericOpcode::SIToFP,
        AsmInstructionKind::ExtractValue { .. } => AsmGenericOpcode::ExtractValue,
        AsmInstructionKind::InsertValue { .. } => AsmGenericOpcode::InsertValue,
        AsmInstructionKind::Call { .. } => AsmGenericOpcode::Call,
        AsmInstructionKind::IntrinsicCall { .. } => AsmGenericOpcode::IntrinsicCall,
        AsmInstructionKind::SextOrTrunc(..) => AsmGenericOpcode::SextOrTrunc,
        AsmInstructionKind::Phi { .. } => AsmGenericOpcode::Phi,
        AsmInstructionKind::Select { .. } => AsmGenericOpcode::Select,
        AsmInstructionKind::InlineAsm { .. } => AsmGenericOpcode::InlineAsm,
        AsmInstructionKind::LandingPad { .. } => AsmGenericOpcode::LandingPad,
        AsmInstructionKind::Unreachable => AsmGenericOpcode::Unreachable,
        AsmInstructionKind::Freeze(..) => AsmGenericOpcode::Freeze,
        AsmInstructionKind::Syscall { .. } => AsmGenericOpcode::Syscall,
        AsmInstructionKind::SysOp(..) => AsmGenericOpcode::SysOp,
        AsmInstructionKind::Splat { .. } => AsmGenericOpcode::Splat,
        AsmInstructionKind::BuildVector { .. } => AsmGenericOpcode::BuildVector,
        AsmInstructionKind::ExtractLane { .. } => AsmGenericOpcode::ExtractLane,
        AsmInstructionKind::InsertLane { .. } => AsmGenericOpcode::InsertLane,
        AsmInstructionKind::ZipLow { .. } => AsmGenericOpcode::ZipLow,
        AsmInstructionKind::SymbolAddress { .. } => AsmGenericOpcode::SymbolAddress,
    }
}
