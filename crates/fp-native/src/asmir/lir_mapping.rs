use super::*;

pub(super) fn map_instruction_kind(kind: &LirInstructionKind) -> AsmInstructionKind {
    match kind {
        LirInstructionKind::Add(lhs, rhs) => {
            AsmInstructionKind::Add(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Sub(lhs, rhs) => {
            AsmInstructionKind::Sub(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Mul(lhs, rhs) => {
            AsmInstructionKind::Mul(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Div(lhs, rhs) => {
            AsmInstructionKind::Div(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Rem(lhs, rhs) => {
            AsmInstructionKind::Rem(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::And(lhs, rhs) => {
            AsmInstructionKind::And(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Or(lhs, rhs) => AsmInstructionKind::Or(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Xor(lhs, rhs) => {
            AsmInstructionKind::Xor(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Shl(lhs, rhs) => {
            AsmInstructionKind::Shl(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Shr(lhs, rhs) => {
            AsmInstructionKind::Shr(map_value(lhs), map_value(rhs))
        }
        LirInstructionKind::Not(value) => AsmInstructionKind::Not(map_value(value)),
        LirInstructionKind::Eq(lhs, rhs) => AsmInstructionKind::Eq(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Ne(lhs, rhs) => AsmInstructionKind::Ne(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Lt(lhs, rhs) => AsmInstructionKind::Lt(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Le(lhs, rhs) => AsmInstructionKind::Le(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Gt(lhs, rhs) => AsmInstructionKind::Gt(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Ge(lhs, rhs) => AsmInstructionKind::Ge(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Load {
            address,
            volatile,
            alignment,
        } => AsmInstructionKind::Load {
            address: map_value(address),
            alignment: *alignment,
            volatile: *volatile,
        },
        LirInstructionKind::Store {
            value,
            address,
            volatile,
            alignment,
        } => AsmInstructionKind::Store {
            value: map_value(value),
            address: map_value(address),
            alignment: *alignment,
            volatile: *volatile,
        },
        LirInstructionKind::Alloca { size, alignment } => AsmInstructionKind::Alloca {
            size: map_value(size),
            alignment: *alignment,
        },
        LirInstructionKind::GetElementPtr {
            ptr,
            indices,
            inbounds,
        } => AsmInstructionKind::GetElementPtr {
            ptr: map_value(ptr),
            indices: indices.iter().map(map_value).collect(),
            inbounds: *inbounds,
        },
        LirInstructionKind::Bitcast(value, ty) => {
            AsmInstructionKind::Bitcast(map_value(value), ty.clone())
        }
        LirInstructionKind::PtrToInt(value) => AsmInstructionKind::PtrToInt(map_value(value)),
        LirInstructionKind::IntToPtr(value) => AsmInstructionKind::IntToPtr(map_value(value)),
        LirInstructionKind::Trunc(value, ty) => {
            AsmInstructionKind::Trunc(map_value(value), ty.clone())
        }
        LirInstructionKind::ZExt(value, ty) => {
            AsmInstructionKind::ZExt(map_value(value), ty.clone())
        }
        LirInstructionKind::SExt(value, ty) => {
            AsmInstructionKind::SExt(map_value(value), ty.clone())
        }
        LirInstructionKind::FPExt(value, ty) => {
            AsmInstructionKind::FPExt(map_value(value), ty.clone())
        }
        LirInstructionKind::FPTrunc(value, ty) => {
            AsmInstructionKind::FPTrunc(map_value(value), ty.clone())
        }
        LirInstructionKind::FPToUI(value, ty) => {
            AsmInstructionKind::FPToUI(map_value(value), ty.clone())
        }
        LirInstructionKind::FPToSI(value, ty) => {
            AsmInstructionKind::FPToSI(map_value(value), ty.clone())
        }
        LirInstructionKind::UIToFP(value, ty) => {
            AsmInstructionKind::UIToFP(map_value(value), ty.clone())
        }
        LirInstructionKind::SIToFP(value, ty) => {
            AsmInstructionKind::SIToFP(map_value(value), ty.clone())
        }
        LirInstructionKind::ExtractValue { aggregate, indices } => {
            AsmInstructionKind::ExtractValue {
                aggregate: map_value(aggregate),
                indices: indices.clone(),
            }
        }
        LirInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => AsmInstructionKind::InsertValue {
            aggregate: map_value(aggregate),
            element: map_value(element),
            indices: indices.clone(),
        },
        LirInstructionKind::Call {
            function,
            args,
            calling_convention,
            tail_call,
        } => AsmInstructionKind::Call {
            function: map_value(function),
            args: args.iter().map(map_value).collect(),
            calling_convention: calling_convention.clone(),
            tail_call: *tail_call,
        },
        LirInstructionKind::ExecQuery(_) => {
            panic!("LIR ExecQuery is only supported by pxc whole-file lowering")
        }
        LirInstructionKind::IntrinsicCall { kind, format, args } => {
            AsmInstructionKind::IntrinsicCall {
                kind: map_intrinsic(kind),
                format: format.clone(),
                args: args.iter().map(map_value).collect(),
            }
        }
        LirInstructionKind::SextOrTrunc(value, ty) => {
            AsmInstructionKind::SextOrTrunc(map_value(value), ty.clone())
        }
        LirInstructionKind::Phi { incoming } => AsmInstructionKind::Phi {
            incoming: incoming
                .iter()
                .map(|(value, block)| (map_value(value), *block))
                .collect(),
        },
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => AsmInstructionKind::Select {
            condition: map_value(condition),
            if_true: map_value(if_true),
            if_false: map_value(if_false),
        },
        LirInstructionKind::InlineAsm {
            asm_string,
            constraints,
            inputs,
            output_type,
            side_effects,
            align_stack,
        } => AsmInstructionKind::InlineAsm {
            asm_string: asm_string.clone(),
            constraints: constraints.clone(),
            inputs: inputs.iter().map(map_value).collect(),
            output_type: output_type.clone(),
            side_effects: *side_effects,
            align_stack: *align_stack,
        },
        LirInstructionKind::LandingPad {
            result_type,
            personality,
            cleanup,
            clauses,
        } => AsmInstructionKind::LandingPad {
            result_type: result_type.clone(),
            personality: personality.as_ref().map(map_value),
            cleanup: *cleanup,
            clauses: clauses.iter().map(map_clause).collect(),
        },
        LirInstructionKind::Unreachable => AsmInstructionKind::Unreachable,
        LirInstructionKind::Freeze(value) => AsmInstructionKind::Freeze(map_value(value)),
        LirInstructionKind::ComptimeOp(_) => AsmInstructionKind::Nop,
    }
}

pub(super) fn map_terminator(term: &LirTerminator) -> AsmTerminator {
    match term {
        LirTerminator::Return(value) => AsmTerminator::Return(value.as_ref().map(map_value)),
        LirTerminator::Br(target) => AsmTerminator::Br(*target),
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => AsmTerminator::CondBr {
            condition: map_value(condition),
            if_true: *if_true,
            if_false: *if_false,
        },
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => AsmTerminator::Switch {
            value: map_value(value),
            default: *default,
            cases: cases.clone(),
        },
        LirTerminator::IndirectBr {
            address,
            destinations,
        } => AsmTerminator::IndirectBr {
            address: map_value(address),
            destinations: destinations.clone(),
        },
        LirTerminator::Invoke {
            function,
            args,
            normal_dest,
            unwind_dest,
            calling_convention,
        } => AsmTerminator::Invoke {
            function: map_value(function),
            args: args.iter().map(map_value).collect(),
            normal_dest: *normal_dest,
            unwind_dest: *unwind_dest,
            calling_convention: calling_convention.clone(),
        },
        LirTerminator::Resume(value) => AsmTerminator::Resume(map_value(value)),
        LirTerminator::Unreachable => AsmTerminator::Unreachable,
        LirTerminator::CleanupRet {
            cleanup_pad,
            unwind_dest,
        } => AsmTerminator::CleanupRet {
            cleanup_pad: map_value(cleanup_pad),
            unwind_dest: *unwind_dest,
        },
        LirTerminator::CatchRet {
            catch_pad,
            successor,
        } => AsmTerminator::CatchRet {
            catch_pad: map_value(catch_pad),
            successor: *successor,
        },
        LirTerminator::CatchSwitch {
            parent_pad,
            handlers,
            unwind_dest,
        } => AsmTerminator::CatchSwitch {
            parent_pad: parent_pad.as_ref().map(map_value),
            handlers: handlers.clone(),
            unwind_dest: *unwind_dest,
        },
    }
}

pub(super) fn map_value(value: &LirValue) -> AsmValue {
    match &value.kind {
        LirValueKind::Register(id) => AsmValue::Register(*id),
        LirValueKind::Constant(constant) => {
            AsmValue::Constant(map_constant_kind(constant, &value.ty))
        }
        LirValueKind::Global(name) => AsmValue::Global(name.to_string(), value.ty.clone()),
        LirValueKind::Function(function) => AsmValue::Function(function_name(function)),
        LirValueKind::Local(id) => AsmValue::Local(*id),
        LirValueKind::StackSlot(id) => AsmValue::StackSlot(*id),
    }
}

pub(super) fn map_constant(constant: &LirConstant) -> AsmConstant {
    map_constant_kind(&constant.kind, &constant.ty)
}

pub(super) fn function_name(function: &fp_core::lir::LirFunctionRef) -> String {
    match function {
        fp_core::lir::LirFunctionRef::Name(name) => name.to_string(),
        fp_core::lir::LirFunctionRef::Package { name, .. } => name.to_string(),
        fp_core::lir::LirFunctionRef::Definition(def_id) => def_id.to_string(),
    }
}

pub(super) fn map_constant_kind(kind: &LirConstantKind, ty: &fp_core::lir::LirType) -> AsmConstant {
    match kind {
        LirConstantKind::Data(LirConstantData::Integer(integer)) => match integer {
            LirInteger::I1(value) => AsmConstant::Bool(*value),
            LirInteger::I8(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I16(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I32(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I64(value) => AsmConstant::Int(*value as i64, ty.clone()),
            LirInteger::I128(value) => AsmConstant::UInt(*value as u64, ty.clone()),
            LirInteger::Arbitrary(_) => panic!("arbitrary-width native constant is unsupported"),
        },
        LirConstantKind::Data(LirConstantData::Float(float)) => match float {
            LirFloat::F32(value) => AsmConstant::Float(f32::from_bits(*value) as f64, ty.clone()),
            LirFloat::F64(value) => AsmConstant::Float(f64::from_bits(*value), ty.clone()),
        },
        LirConstantKind::Data(LirConstantData::Bytes(bytes)) => AsmConstant::Bytes(bytes.clone()),
        LirConstantKind::Aggregate(LirConstantAggregate::Array(values)) => {
            AsmConstant::Array(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Struct(values)) => {
            AsmConstant::Struct(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Vector(values)) => {
            AsmConstant::Array(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::GlobalAddress { global } => {
            AsmConstant::GlobalRef(global.clone(), ty.clone(), Vec::new())
        }
        LirConstantKind::FunctionAddress(function) => {
            AsmConstant::FunctionRef(Name::new(function_name(function)), ty.clone())
        }
        LirConstantKind::Null => AsmConstant::Null(ty.clone()),
        LirConstantKind::Undef | LirConstantKind::Poison => AsmConstant::Undef(ty.clone()),
        LirConstantKind::Expr(LirConstantExpr::GetElementPtr { base, indices, .. }) => {
            let (global, mut base_indices) = global_ref_components(base)
                .unwrap_or_else(|| panic!("constant GEP requires a global-address base"));
            for index in indices {
                let value = constant_integer(index)
                    .unwrap_or_else(|| panic!("constant GEP index must be an integer"));
                base_indices.push(value);
            }
            AsmConstant::GlobalRef(global, ty.clone(), base_indices)
        }
    }
}

pub(super) fn global_ref_components(constant: &LirConstant) -> Option<(Name, Vec<u64>)> {
    match &constant.kind {
        LirConstantKind::GlobalAddress { global } => Some((global.clone(), Vec::new())),
        LirConstantKind::Expr(LirConstantExpr::GetElementPtr { base, indices, .. }) => {
            let (global, mut base_indices) = global_ref_components(base)?;
            for index in indices {
                base_indices.push(constant_integer(index)?);
            }
            Some((global, base_indices))
        }
        _ => None,
    }
}

pub(super) fn constant_integer(constant: &LirConstant) -> Option<u64> {
    let LirConstantKind::Data(LirConstantData::Integer(integer)) = &constant.kind else {
        return None;
    };
    Some(match integer {
        LirInteger::I1(value) => u64::from(*value),
        LirInteger::I8(value) => u64::from(*value),
        LirInteger::I16(value) => u64::from(*value),
        LirInteger::I32(value) => u64::from(*value),
        LirInteger::I64(value) => *value,
        LirInteger::I128(value) => *value as u64,
        LirInteger::Arbitrary(_) => {
            return None;
        }
    })
}

pub(super) fn map_global(global: &fp_core::lir::LirGlobal) -> AsmGlobal {
    AsmGlobal {
        name: global.name.clone(),
        ty: global.ty.clone(),
        initializer: global.initializer.as_ref().map(map_constant),
        relocations: global
            .relocations
            .iter()
            .filter_map(|reloc| {
                let symbol = match &reloc.target {
                    fp_core::lir::LirRelocationTarget::Global(name)
                    | fp_core::lir::LirRelocationTarget::Function(name) => name.clone(),
                };
                Some(fp_core::asmir::AsmGlobalRelocation {
                    offset: reloc.offset,
                    kind: match reloc.kind {
                        fp_core::lir::LirRelocationKind::Abs64 => {
                            fp_core::asmir::AsmRelocationKind::Abs64
                        }
                        fp_core::lir::LirRelocationKind::PcRel32 => {
                            fp_core::asmir::AsmRelocationKind::PcRel32
                        }
                    },
                    symbol,
                    addend: reloc.addend,
                })
            })
            .collect(),
        section: global.section.clone(),
        linkage: global.linkage.clone(),
        visibility: global.visibility.clone(),
        alignment: global.alignment,
        is_constant: global.is_constant,
    }
}

pub(super) fn map_intrinsic(kind: &LirIntrinsicKind) -> AsmIntrinsicKind {
    match kind {
        LirIntrinsicKind::Print => AsmIntrinsicKind::Print,
        LirIntrinsicKind::Println => AsmIntrinsicKind::Println,
        LirIntrinsicKind::Format => AsmIntrinsicKind::Format,
        LirIntrinsicKind::TimeNow => AsmIntrinsicKind::TimeNow,
        LirIntrinsicKind::ProcMacroTokenStreamFromStr => {
            AsmIntrinsicKind::ProcMacroTokenStreamFromStr
        }
        LirIntrinsicKind::ProcMacroTokenStreamToString => {
            AsmIntrinsicKind::ProcMacroTokenStreamToString
        }
    }
}

pub(super) fn map_clause(clause: &fp_core::lir::LandingPadClause) -> AsmLandingPadClause {
    match clause {
        fp_core::lir::LandingPadClause::Catch(value) => {
            AsmLandingPadClause::Catch(map_value(value))
        }
        fp_core::lir::LandingPadClause::Filter(values) => {
            AsmLandingPadClause::Filter(values.iter().map(map_value).collect())
        }
    }
}

pub(super) fn map_arch(arch: TargetArch) -> AsmArchitecture {
    match arch {
        TargetArch::X86_64 => AsmArchitecture::X86_64,
        TargetArch::Aarch64 => AsmArchitecture::Aarch64,
    }
}

pub(super) fn map_format(format: TargetFormat) -> AsmObjectFormat {
    match format {
        TargetFormat::MachO => AsmObjectFormat::MachO,
        TargetFormat::Elf => AsmObjectFormat::Elf,
        TargetFormat::Coff => AsmObjectFormat::Coff,
    }
}
