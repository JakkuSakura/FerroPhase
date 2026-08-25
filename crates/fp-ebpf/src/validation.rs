use crate::{Error, FrameLayout, constant_non_negative_u32};
use fp_core::error::Result;
use fp_core::lir::{
    LirConstantData, LirConstantKind, LirFunction, LirInstruction, LirInstructionKind,
    LirTerminator, LirType, LirValue, LirValueKind,
};

pub(super) fn validate_function(
    function: &LirFunction,
    data_layout: &fp_core::lir::LirDataLayout,
    errors: &mut Vec<String>,
) {
    if function.signature.is_variadic {
        errors.push(format!(
            "function {}: variadic signatures are not supported",
            function.name
        ));
    }

    let arg_count = function
        .locals
        .iter()
        .filter(|local| local.is_argument)
        .count();
    if arg_count > 5 {
        errors.push(format!(
            "function {}: more than 5 arguments is not supported",
            function.name
        ));
    }

    if let Err(err) = validate_type(&function.signature.return_type) {
        errors.push(format!(
            "function {}: invalid return type: {}",
            function.name, err
        ));
    }

    for local in &function.locals {
        if let Err(err) = validate_type(&local.ty) {
            errors.push(format!(
                "function {} local {}: {}",
                function.name, local.id, err
            ));
        }
    }

    for slot in &function.stack_slots {
        if slot.size > 512 {
            errors.push(format!(
                "function {} stack slot {} exceeds 512 bytes",
                function.name, slot.id
            ));
        }
    }

    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            validate_instruction(function, instruction, errors);
        }
        validate_terminator(function, &block.terminator, errors);
    }

    match FrameLayout::build(function, data_layout) {
        Ok(layout) if layout.frame_size > 512 => errors.push(format!(
            "function {} requires {} bytes of stack, exceeds eBPF 512-byte limit",
            function.name, layout.frame_size
        )),
        Err(err) => errors.push(format!("function {}: {}", function.name, err)),
        _ => {}
    }
}

fn validate_type(ty: &LirType) -> Result<()> {
    match ty {
        LirType::I1
        | LirType::I8
        | LirType::I16
        | LirType::I32
        | LirType::I64
        | LirType::Ptr(_)
        | LirType::Void => Ok(()),
        _ => Err(Error::from(format!(
            "type {:?} is not supported by fp-ebpf",
            ty
        ))),
    }
}

fn validate_instruction(
    function: &LirFunction,
    instruction: &LirInstruction,
    errors: &mut Vec<String>,
) {
    use LirInstructionKind::*;

    let result = match &instruction.kind {
        Add(lhs, rhs)
        | Sub(lhs, rhs)
        | Mul(lhs, rhs)
        | Div(lhs, rhs)
        | Rem(lhs, rhs)
        | And(lhs, rhs)
        | Or(lhs, rhs)
        | Xor(lhs, rhs)
        | Shl(lhs, rhs)
        | Shr(lhs, rhs)
        | Eq(lhs, rhs)
        | Ne(lhs, rhs)
        | Lt(lhs, rhs)
        | Le(lhs, rhs)
        | Gt(lhs, rhs)
        | Ge(lhs, rhs) => validate_scalar_pair(lhs, rhs),
        Not(value)
        | PtrToInt(value)
        | IntToPtr(value)
        | Trunc(value, _)
        | ZExt(value, _)
        | SExt(value, _)
        | FPTrunc(value, _)
        | FPExt(value, _)
        | FPToUI(value, _)
        | FPToSI(value, _)
        | UIToFP(value, _)
        | SIToFP(value, _)
        | Bitcast(value, _)
        | SextOrTrunc(value, _)
        | Freeze(value) => validate_scalar_value(value),
        Load { address, .. } => validate_address_value(address),
        Store { value, address, .. } => {
            validate_scalar_value(value).and_then(|_| validate_address_value(address))
        }
        Alloca { size, .. } => constant_non_negative_u32(size).map(|_| ()),
        GetElementPtr { ptr, indices, .. } => validate_address_value(ptr).and_then(|_| {
            if indices.iter().all(|index| {
                matches!(
                    &index.kind,
                    LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(_)))
                )
            }) {
                Ok(())
            } else {
                Err(Error::from(
                    "getelementptr requires constant integer indices",
                ))
            }
        }),
        ExecQuery(_) => Err(Error::from(
            "LIR ExecQuery is only supported by pxc whole-file lowering",
        )),
        Call { .. } => Err(Error::from("calls are not supported in fp-ebpf yet")),
        IntrinsicCall { kind, args, .. } => match kind {
            fp_core::lir::LirIntrinsicKind::TimeNow => {
                if args.is_empty() {
                    Ok(())
                } else {
                    Err(Error::from("TimeNow does not accept arguments"))
                }
            }
            fp_core::lir::LirIntrinsicKind::Print | fp_core::lir::LirIntrinsicKind::Println => {
                if args.len() > 4 {
                    Err(Error::from(
                        "print helpers support at most 4 scalar arguments",
                    ))
                } else {
                    for arg in args {
                        if let Err(err) = validate_scalar_value(arg) {
                            return errors.push(format!(
                                "function {} instruction {}: {}",
                                function.name, instruction.id, err
                            ));
                        }
                    }
                    Ok(())
                }
            }
            fp_core::lir::LirIntrinsicKind::Format => Err(Error::from(
                "Format is not supported by the current fp-ebpf runtime ABI",
            )),
            fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamFromStr
            | fp_core::lir::LirIntrinsicKind::ProcMacroTokenStreamToString => Err(Error::from(
                "proc-macro token stream parsing/printing is not supported on the eBPF backend",
            )),
        },
        ExtractValue { .. } | InsertValue { .. } => Err(Error::from(
            "aggregate operations are not supported in fp-ebpf",
        )),
        Phi { .. } => Err(Error::from("phi nodes must be lowered before fp-ebpf")),
        Select { .. } => Err(Error::from("select must be lowered before fp-ebpf")),
        InlineAsm { .. } => Err(Error::from("inline asm is not supported in fp-ebpf")),
        LandingPad { .. } | Unreachable | LirInstructionKind::ComptimeOp(_) => Err(Error::from(
            "exception/unreachable instructions are not supported in fp-ebpf",
        )),
    };

    if let Err(err) = result {
        errors.push(format!(
            "function {} instruction {}: {}",
            function.name, instruction.id, err
        ));
    }
}

fn validate_terminator(
    function: &LirFunction,
    terminator: &LirTerminator,
    errors: &mut Vec<String>,
) {
    let result = match terminator {
        LirTerminator::Return(Some(value)) => validate_scalar_value(value),
        LirTerminator::Return(None) | LirTerminator::Br(_) => Ok(()),
        LirTerminator::CondBr { condition, .. } => validate_scalar_value(condition),
        LirTerminator::Switch { value, .. } => validate_scalar_value(value),
        _ => Err(Error::from("terminator is not supported in fp-ebpf")),
    };

    if let Err(err) = result {
        errors.push(format!(
            "function {} terminator {:?}: {}",
            function.name, terminator, err
        ));
    }
}

fn validate_scalar_pair(lhs: &LirValue, rhs: &LirValue) -> Result<()> {
    validate_scalar_value(lhs)?;
    validate_scalar_value(rhs)
}

fn validate_scalar_value(value: &LirValue) -> Result<()> {
    match &value.kind {
        LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(_)))
        | LirValueKind::Constant(LirConstantKind::Null)
        | LirValueKind::Constant(LirConstantKind::Undef) => validate_type(&value.ty),
        LirValueKind::Register(_) | LirValueKind::Local(_) | LirValueKind::StackSlot(_) => {
            validate_type(&value.ty)
        }
        _ => Err(Error::from(format!(
            "value {:?} is not a supported scalar fp-ebpf operand",
            value
        ))),
    }
}

fn validate_address_value(value: &LirValue) -> Result<()> {
    match value.kind {
        LirValueKind::Register(_) | LirValueKind::Local(_) | LirValueKind::StackSlot(_) => Ok(()),
        _ => Err(Error::from(format!(
            "value {:?} is not a supported stack-backed address",
            value
        ))),
    }
}
