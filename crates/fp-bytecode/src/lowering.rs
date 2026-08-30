use super::*;
use std::collections::HashMap;

pub(super) fn lower_function(
    func: &mir::Function,
    body: &mir::Body,
    const_pool: &mut Vec<BytecodeConst>,
    function_names: &HashMap<mir::ty::DefId, String>,
) -> Result<BytecodeFunction, BytecodeError> {
    let local_types = body
        .locals
        .iter()
        .map(|local| lower_type(&local.ty))
        .collect::<Result<Vec<_>, _>>()?;
    let mut blocks = Vec::new();
    for (block_id, block) in body.basic_blocks.iter().enumerate() {
        let mut code = Vec::new();
        for stmt in &block.statements {
            lower_statement(stmt, &local_types, &mut code, const_pool)?;
        }
        let lowered_term = match block.terminator.as_ref() {
            Some(terminator) => lower_terminator(
                terminator,
                &local_types,
                &mut code,
                const_pool,
                function_names,
            )?,
            None => {
                return Err(BytecodeError::Lowering {
                    message: format!("function {} has a block without a terminator", func.name),
                });
            }
        };
        blocks.push(BytecodeBlock {
            id: block_id as u32,
            code,
            terminator: lowered_term,
        });
    }

    Ok(BytecodeFunction {
        name: func.name.as_str().to_string(),
        param_types: func
            .sig
            .inputs
            .iter()
            .map(lower_type)
            .collect::<Result<Vec<_>, _>>()?,
        return_type: lower_type(&func.sig.output)?,
        local_types,
        blocks,
    })
}

fn lower_type(ty: &mir::Ty) -> Result<fp_core::lir::LirType, BytecodeError> {
    use fp_core::lir::LirType;
    use mir::ty::{FloatTy, IntTy, TyKind, UintTy};
    match &ty.kind {
        TyKind::Bool => Ok(LirType::I1),
        TyKind::Char => Ok(LirType::I32),
        TyKind::Int(IntTy::I8) => Ok(LirType::I8),
        TyKind::Int(IntTy::I16) => Ok(LirType::I16),
        TyKind::Int(IntTy::I32) => Ok(LirType::I32),
        TyKind::Int(IntTy::I64) | TyKind::Int(IntTy::Isize) => Ok(LirType::I64),
        TyKind::Int(IntTy::I128) => Ok(LirType::I128),
        TyKind::Uint(UintTy::U8) => Ok(LirType::I8),
        TyKind::Uint(UintTy::U16) => Ok(LirType::I16),
        TyKind::Uint(UintTy::U32) => Ok(LirType::I32),
        TyKind::Uint(UintTy::U64) | TyKind::Uint(UintTy::Usize) => Ok(LirType::I64),
        TyKind::Uint(UintTy::U128) => Ok(LirType::I128),
        TyKind::Float(FloatTy::F32) => Ok(LirType::F32),
        TyKind::Float(FloatTy::F64) => Ok(LirType::F64),
        TyKind::RawPtr(_) | TyKind::Ref(..) | TyKind::Slice(_) => {
            Ok(LirType::Ptr(Box::new(LirType::I8)))
        }
        TyKind::Tuple(elements) => Ok(LirType::Struct {
            fields: elements
                .iter()
                .map(|element| lower_type(element))
                .collect::<Result<Vec<_>, _>>()?,
            packed: false,
            name: None,
        }),
        TyKind::Array(element, mir::ty::ConstKind::Value(mir::ty::ConstValue::Scalar(scalar))) => {
            let mir::ty::Scalar::Int(value) = scalar else {
                return Err(BytecodeError::Lowering {
                    message: "array length is not an integer constant".into(),
                });
            };
            let count = value.data as u64;
            Ok(LirType::Array(Box::new(lower_type(element)?), count))
        }
        TyKind::Adt(adt, _)
            if adt.flags.contains(mir::ty::AdtFlags::IS_ENUM)
                && adt.variants.iter().all(|variant| variant.fields.is_empty()) =>
        {
            // Tag-only enums have the same bytecode representation as their
            // MIR discriminant: a single integer value. Payload-bearing
            // enums still require an aggregate representation.
            Ok(LirType::I64)
        }
        TyKind::Never => Ok(LirType::Void),
        other => Err(BytecodeError::Lowering {
            message: format!("unsupported MIR type in bytecode: {other:?}"),
        }),
    }
}

fn place_type(
    place: &mir::Place,
    local_types: &[fp_core::lir::LirType],
) -> Result<fp_core::lir::LirType, BytecodeError> {
    let mut ty = local_types
        .get(place.local as usize)
        .cloned()
        .ok_or_else(|| BytecodeError::Lowering {
            message: format!("place local {} is out of bounds", place.local),
        })?;
    for projection in &place.projection {
        match projection {
            mir::PlaceElem::Field(_, field_ty) => ty = lower_type(field_ty)?,
            mir::PlaceElem::Index(_) => match ty {
                fp_core::lir::LirType::Array(element, _) => ty = *element,
                _ => {
                    return Err(BytecodeError::Lowering {
                        message: format!("index projection on non-array type {ty:?}"),
                    });
                }
            },
            mir::PlaceElem::Deref => match ty {
                fp_core::lir::LirType::Ptr(inner) => ty = *inner,
                _ => {
                    return Err(BytecodeError::Lowering {
                        message: format!("deref projection on non-pointer type {ty:?}"),
                    });
                }
            },
            unsupported => {
                return Err(BytecodeError::Lowering {
                    message: format!("unsupported place projection: {unsupported:?}"),
                });
            }
        }
    }
    Ok(ty)
}

fn lower_statement(
    stmt: &mir::Statement,
    local_types: &[fp_core::lir::LirType],
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match &stmt.kind {
        mir::StatementKind::Assign(place, rvalue) => {
            let result_type = place_type(place, local_types)?;
            lower_rvalue(rvalue, &result_type, local_types, code, const_pool)?;
            code.push(BytecodeInstr::StorePlace(lower_place(place, local_types)?));
            Ok(())
        }
        mir::StatementKind::IntrinsicCall { kind, format, args } => {
            for arg in args {
                lower_operand(arg, local_types, code, const_pool)?;
            }
            code.push(BytecodeInstr::IntrinsicCall {
                kind: *kind,
                arg_count: args.len() as u32,
                format: if format.is_empty() {
                    None
                } else {
                    Some(format.clone())
                },
                result_type: fp_core::lir::LirType::Void,
            });
            Ok(())
        }
        mir::StatementKind::StorageLive(_)
        | mir::StatementKind::StorageDead(_)
        | mir::StatementKind::Retag(_, _)
        | mir::StatementKind::AscribeUserType(_, _, _)
        | mir::StatementKind::Nop
        | mir::StatementKind::SetDiscriminant { .. } => Ok(()),
    }
}

fn lower_terminator(
    term: &mir::Terminator,
    local_types: &[fp_core::lir::LirType],
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
    function_names: &HashMap<mir::ty::DefId, String>,
) -> Result<BytecodeTerminator, BytecodeError> {
    match &term.kind {
        mir::TerminatorKind::Return => Ok(BytecodeTerminator::Return),
        mir::TerminatorKind::Goto { target } => Ok(BytecodeTerminator::Jump { target: *target }),
        mir::TerminatorKind::Assert {
            cond,
            expected,
            target,
            ..
        } => {
            lower_operand(cond, local_types, code, const_pool)?;
            let otherwise =
                terminator_otherwise(term).map_err(|error| BytecodeError::Lowering {
                    message: error.to_string(),
                })?;
            let terminator = if *expected {
                BytecodeTerminator::JumpIfTrue {
                    target: *target,
                    otherwise,
                }
            } else {
                BytecodeTerminator::JumpIfFalse {
                    target: *target,
                    otherwise,
                }
            };
            Ok(terminator)
        }
        mir::TerminatorKind::SwitchInt { discr, targets, .. } => {
            lower_operand(discr, local_types, code, const_pool)?;
            Ok(BytecodeTerminator::SwitchInt {
                values: targets.values.clone(),
                targets: targets.targets.clone(),
                otherwise: targets.otherwise,
            })
        }
        mir::TerminatorKind::Call {
            func,
            args,
            destination,
            ..
        } => {
            for arg in args {
                lower_operand(arg, local_types, code, const_pool)?;
            }
            let callee = lower_callee(func, function_names, local_types)?;
            let dest = destination
                .as_ref()
                .map(|(place, _)| lower_place(place, local_types))
                .transpose()?;
            let (_, target) = destination
                .as_ref()
                .ok_or_else(|| BytecodeError::Lowering {
                    message: "call terminator without a destination is unsupported".into(),
                })?;
            let result_type = place_type(&destination.as_ref().unwrap().0, local_types)?;
            Ok(BytecodeTerminator::Call {
                callee,
                arg_count: args.len() as u32,
                destination: dest,
                result_type,
                target: *target,
            })
        }
        mir::TerminatorKind::FalseEdge {
            real_target,
            imaginary_target,
        } => Ok(BytecodeTerminator::JumpIfTrue {
            target: *real_target,
            otherwise: *imaginary_target,
        }),
        mir::TerminatorKind::FalseUnwind { real_target, .. } => Err(BytecodeError::Lowering {
            message: format!(
                "false-unwind terminator at target {} is not representable in bytecode",
                real_target
            ),
        }),
        mir::TerminatorKind::Abort => Ok(BytecodeTerminator::Abort),
        mir::TerminatorKind::Unreachable => Ok(BytecodeTerminator::Unreachable),
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported terminator: {:?}", term.kind),
        }),
    }
}

fn terminator_otherwise(term: &mir::Terminator) -> Result<u32, LoweringFallbackError> {
    match &term.kind {
        mir::TerminatorKind::Assert {
            cleanup, target, ..
        } => match cleanup {
            Some(otherwise) => Ok(*otherwise),
            None => Err(LoweringFallbackError::MissingAssertCleanup(*target)),
        },
        _ => Err(LoweringFallbackError::InvalidOtherwiseTerminator),
    }
}

fn lower_rvalue(
    rvalue: &mir::Rvalue,
    result_type: &fp_core::lir::LirType,
    local_types: &[fp_core::lir::LirType],
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match rvalue {
        mir::Rvalue::Use(op) => lower_operand(op, local_types, code, const_pool),
        mir::Rvalue::Query(_) => Err(BytecodeError::Lowering {
            message: "MIR query rvalue is not supported by fp-bytecode".into(),
        }),
        mir::Rvalue::Ref(_, _, place) => lower_operand(
            &mir::Operand::Copy(place.clone()),
            local_types,
            code,
            const_pool,
        ),
        mir::Rvalue::BinaryOp(op, lhs, rhs) => {
            lower_operand(lhs, local_types, code, const_pool)?;
            lower_operand(rhs, local_types, code, const_pool)?;
            match lower_binop(op) {
                Ok(bin_op) => code.push(BytecodeInstr::BinaryOp(bin_op)),
                Err(error) => {
                    return Err(BytecodeError::Lowering {
                        message: error.to_string(),
                    });
                }
            }
            Ok(())
        }
        mir::Rvalue::UnaryOp(op, value) => {
            lower_operand(value, local_types, code, const_pool)?;
            code.push(BytecodeInstr::UnaryOp(lower_unop(op)?));
            Ok(())
        }
        mir::Rvalue::Cast(_, operand, _) => lower_operand(operand, local_types, code, const_pool),
        mir::Rvalue::IntrinsicCall { kind, format, args } => {
            for arg in args {
                lower_operand(arg, local_types, code, const_pool)?;
            }
            code.push(BytecodeInstr::IntrinsicCall {
                kind: *kind,
                arg_count: args.len() as u32,
                format: if format.is_empty() {
                    None
                } else {
                    Some(format.clone())
                },
                result_type: result_type.clone(),
            });
            Ok(())
        }
        mir::Rvalue::Repeat(operand, len) => {
            if *len > u32::MAX as u64 {
                return Err(BytecodeError::Lowering {
                    message: format!("repeat length {} exceeds bytecode limits", len),
                });
            }
            for _ in 0..*len {
                lower_operand(operand, local_types, code, const_pool)?;
            }
            code.push(BytecodeInstr::MakeArray(*len as u32));
            Ok(())
        }
        mir::Rvalue::Aggregate(kind, operands) => {
            for op in operands {
                lower_operand(op, local_types, code, const_pool)?;
            }
            match kind {
                mir::AggregateKind::Tuple => {
                    if matches!(result_type, fp_core::lir::LirType::I64) && operands.len() == 1 {
                        // A tag-only enum is lowered by MIR as a one-element
                        // tuple containing its discriminant, but bytecode
                        // stores that enum as the scalar tag itself.
                    } else {
                        code.push(BytecodeInstr::MakeTuple(operands.len() as u32));
                    }
                    Ok(())
                }
                mir::AggregateKind::Array(_) => {
                    code.push(BytecodeInstr::MakeArray(operands.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported aggregate: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerLiteral { kind, elements } => {
            for op in elements {
                lower_operand(op, local_types, code, const_pool)?;
            }
            match kind {
                mir::ContainerKind::List { .. } => {
                    code.push(BytecodeInstr::MakeList(elements.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported container literal: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerMapLiteral { kind, entries } => {
            for (key, value) in entries {
                lower_operand(key, local_types, code, const_pool)?;
                lower_operand(value, local_types, code, const_pool)?;
            }
            match kind {
                mir::ContainerKind::Map { .. } => {
                    code.push(BytecodeInstr::MakeMap(entries.len() as u32));
                    Ok(())
                }
                _ => Err(BytecodeError::Lowering {
                    message: format!("unsupported container map literal: {:?}", kind),
                }),
            }
        }
        mir::Rvalue::ContainerLen { container, .. } => {
            lower_operand(container, local_types, code, const_pool)?;
            code.push(BytecodeInstr::ContainerLen);
            Ok(())
        }
        mir::Rvalue::ContainerGet { container, key, .. } => {
            lower_operand(container, local_types, code, const_pool)?;
            lower_operand(key, local_types, code, const_pool)?;
            code.push(BytecodeInstr::ContainerGet);
            Ok(())
        }
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported rvalue: {:?}", rvalue),
        }),
    }
}

fn lower_operand(
    operand: &mir::Operand,
    local_types: &[fp_core::lir::LirType],
    code: &mut Vec<BytecodeInstr>,
    const_pool: &mut Vec<BytecodeConst>,
) -> Result<(), BytecodeError> {
    match operand {
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            place_type(place, local_types)?;
            code.push(BytecodeInstr::LoadPlace(lower_place(place, local_types)?));
            Ok(())
        }
        mir::Operand::Constant(constant) => {
            let value = lower_constant(constant)?;
            let id = push_const(const_pool, value);
            code.push(BytecodeInstr::LoadConst(id));
            Ok(())
        }
    }
}

fn lower_constant(constant: &mir::Constant) -> Result<BytecodeConst, BytecodeError> {
    match &constant.literal {
        mir::ConstantKind::Null => Ok(BytecodeConst::Null),
        mir::ConstantKind::Undef => Ok(BytecodeConst::Undef),
        mir::ConstantKind::Int(value) => Ok(BytecodeConst::Int(*value)),
        mir::ConstantKind::UInt(value) => Ok(BytecodeConst::UInt(*value)),
        mir::ConstantKind::Float(value) => Ok(BytecodeConst::Float(*value)),
        mir::ConstantKind::Bool(value) => Ok(BytecodeConst::Bool(*value)),
        mir::ConstantKind::Str(value) => Ok(BytecodeConst::Str(value.clone())),
        mir::ConstantKind::ExternFn(symbol) => Ok(BytecodeConst::Function(symbol.as_str().to_string())),
        mir::ConstantKind::FnDef(def_id, substs) => Err(BytecodeError::Lowering {
            message: format!(
                "function definition reference {:?} with substitutions {:?} cannot be represented in bytecode",
                def_id, substs
            ),
        }),
        mir::ConstantKind::Global(symbol) => Ok(BytecodeConst::Global(symbol.to_string())),
        mir::ConstantKind::Val(value) => lower_const_value(value),
        mir::ConstantKind::Ty(_) => Err(BytecodeError::Lowering {
            message: format!(
                "type constant is not representable in bytecode: {:?}",
                constant.literal
            ),
        }),
        mir::ConstantKind::TokenStream { kind, .. } => {
            let _ = DiagnosticManager::report_error_with_context(
                BYTECODE_LOWERING_CONTEXT,
                format!(
                    "token stream constant ({:?}) should not appear in bytecode — must be resolved at comptime",
                    kind
                ),
            );
            Err(BytecodeError::Lowering {
                message: "token stream in bytecode".into(),
            })
        }
    }
}

fn lower_const_value(value: &mir::ConstValue) -> Result<BytecodeConst, BytecodeError> {
    match value {
        mir::ConstValue::Unit => Ok(BytecodeConst::Unit),
        mir::ConstValue::Bool(value) => Ok(BytecodeConst::Bool(*value)),
        mir::ConstValue::Int(value) => Ok(BytecodeConst::Int(*value)),
        mir::ConstValue::UInt(value) => Ok(BytecodeConst::UInt(*value)),
        mir::ConstValue::Float(value) => Ok(BytecodeConst::Float(*value)),
        mir::ConstValue::Str(value) => Ok(BytecodeConst::Str(value.clone())),
        mir::ConstValue::Null => Ok(BytecodeConst::Null),
        mir::ConstValue::Tuple(items) => items
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::Tuple),
        mir::ConstValue::Array(items) => items
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::Array),
        mir::ConstValue::List { elements, .. } => elements
            .iter()
            .map(lower_const_value)
            .collect::<Result<Vec<_>, _>>()
            .map(BytecodeConst::List),
        mir::ConstValue::Map { entries, .. } => {
            let mut lowered = Vec::with_capacity(entries.len());
            for (key, value) in entries {
                lowered.push((lower_const_value(key)?, lower_const_value(value)?));
            }
            Ok(BytecodeConst::Map(lowered))
        }
        _ => Err(BytecodeError::Lowering {
            message: format!("unsupported const value: {:?}", value),
        }),
    }
}

fn lower_place(
    place: &mir::Place,
    _local_types: &[fp_core::lir::LirType],
) -> Result<BytecodePlace, BytecodeError> {
    let mut projection = Vec::new();
    for elem in &place.projection {
        match elem {
            mir::PlaceElem::Field(index, _) => {
                projection.push(BytecodePlaceElem::Field(*index as u32));
            }
            mir::PlaceElem::Index(local) => {
                projection.push(BytecodePlaceElem::Index(*local));
            }
            mir::PlaceElem::Deref => {}
            _ => {
                return Err(BytecodeError::Lowering {
                    message: format!("unsupported place projection: {:?}", elem),
                });
            }
        }
    }

    Ok(BytecodePlace {
        local: place.local,
        projection,
    })
}

fn lower_callee(
    operand: &mir::Operand,
    function_names: &HashMap<mir::ty::DefId, String>,
    local_types: &[fp_core::lir::LirType],
) -> Result<BytecodeCallee, BytecodeError> {
    match operand {
        mir::Operand::Constant(constant) => match &constant.literal {
            mir::ConstantKind::ExternFn(symbol) => Ok(BytecodeCallee::Function(symbol.to_string())),
            mir::ConstantKind::FnDef(def_id, substs) => {
                if !substs.is_empty() {
                    return Err(BytecodeError::Lowering {
                        message: format!(
                            "generic function definition reference {:?} with substitutions {:?} cannot be called from bytecode",
                            def_id, substs
                        ),
                    });
                }
                let Some(name) = function_names.get(def_id) else {
                    return Err(BytecodeError::Lowering {
                        message: format!(
                            "function definition {:?} is not present in bytecode unit",
                            def_id
                        ),
                    });
                };
                Ok(BytecodeCallee::Function(name.clone()))
            }
            mir::ConstantKind::Global(symbol) => Ok(BytecodeCallee::Function(symbol.to_string())),
            _ => Err(BytecodeError::Lowering {
                message: format!("unsupported call operand: {:?}", constant.literal),
            }),
        },
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            Ok(BytecodeCallee::Local(lower_place(place, local_types)?))
        }
    }
}

fn push_const(pool: &mut Vec<BytecodeConst>, value: BytecodeConst) -> u32 {
    pool.push(value);
    (pool.len() - 1) as u32
}

fn lower_binop(op: &mir::BinOp) -> Result<BytecodeBinOp, LoweringFallbackError> {
    let lowered = match op {
        mir::BinOp::Add => BytecodeBinOp::Add,
        mir::BinOp::Sub => BytecodeBinOp::Sub,
        mir::BinOp::Mul => BytecodeBinOp::Mul,
        mir::BinOp::Div => BytecodeBinOp::Div,
        mir::BinOp::Rem => BytecodeBinOp::Rem,
        mir::BinOp::And => BytecodeBinOp::And,
        mir::BinOp::Or => BytecodeBinOp::Or,
        mir::BinOp::BitXor => BytecodeBinOp::BitXor,
        mir::BinOp::BitAnd => BytecodeBinOp::BitAnd,
        mir::BinOp::BitOr => BytecodeBinOp::BitOr,
        mir::BinOp::Shl => BytecodeBinOp::Shl,
        mir::BinOp::Shr => BytecodeBinOp::Shr,
        mir::BinOp::Eq => BytecodeBinOp::Eq,
        mir::BinOp::Lt => BytecodeBinOp::Lt,
        mir::BinOp::Le => BytecodeBinOp::Le,
        mir::BinOp::Ne => BytecodeBinOp::Ne,
        mir::BinOp::Ge => BytecodeBinOp::Ge,
        mir::BinOp::Gt => BytecodeBinOp::Gt,
        _ => {
            return Err(LoweringFallbackError::UnsupportedBinaryOp(op.clone()));
        }
    };
    Ok(lowered)
}

fn lower_unop(op: &mir::UnOp) -> Result<BytecodeUnOp, BytecodeError> {
    let lowered = match op {
        mir::UnOp::Not => BytecodeUnOp::Not,
        mir::UnOp::Neg => BytecodeUnOp::Neg,
    };
    Ok(lowered)
}
