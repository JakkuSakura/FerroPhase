use super::*;
pub(super) fn emit_block(
    asm: &mut Assembler,
    block: &AsmBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    signatures: &HashMap<String, AsmFunctionSignature>,
    layout: &FrameLayout,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    return_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    for inst in &block.instructions {
        match &inst.kind {
            AsmInstructionKind::Nop => {
                asm.push(0x90);
            }
            AsmInstructionKind::Add(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("add requires a concrete type"));
                }
                if matches!(ty, AsmType::Ptr(_)) {
                    if let (
                        AsmValue::Constant(AsmConstant::String(lhs_text)),
                        AsmValue::Constant(AsmConstant::String(rhs_text)),
                    ) = (lhs, rhs)
                    {
                        let mut combined = String::with_capacity(lhs_text.len() + rhs_text.len());
                        combined.push_str(lhs_text);
                        combined.push_str(rhs_text);
                        let offset = intern_cstring(rodata, rodata_pool, &combined);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                        store_vreg(asm, layout, inst.id, Reg::R10)?;
                        continue;
                    }
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Add,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Sub(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("sub requires a concrete type"));
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Sub,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Mul(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("mul requires a concrete type"));
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Mul,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Splat {
                value,
                lane_bits,
                lanes,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("splat requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("splat expects 128-bit vector result"));
                }
                if *lane_bits != 64 || *lanes != 2 {
                    return Err(Error::from("x86_64 splat only supports 2x64 lanes for now"));
                }
                load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
                emit_movq_xmm_r64(asm, FReg::Xmm0, Reg::R10);
                emit_punpcklqdq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm0);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, &result_ty)?;
            }
            AsmInstructionKind::BuildVector { elements } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("build_vector requires a concrete result type"));
                }
                let AsmType::Vector(elem_ty, lanes) = &result_ty else {
                    return Err(Error::from("build_vector expects vector result type"));
                };
                if size_of(&result_ty) != 16 {
                    return Err(Error::from("build_vector only supports 128-bit vectors"));
                }
                if *elem_ty.as_ref() != AsmType::I64 || *lanes != 2 {
                    return Err(Error::from(
                        "build_vector currently only supports <2 x i64> on x86_64",
                    ));
                }
                if elements.len() != 2 {
                    return Err(Error::from("build_vector lane count mismatch"));
                }
                if !matches!(
                    elements[1],
                    AsmValue::Constant(AsmConstant::Int(0, _))
                        | AsmValue::Constant(AsmConstant::UInt(0, _))
                        | AsmValue::Null(_)
                ) {
                    return Err(Error::from(
                        "build_vector currently requires lane1=0 for x86_64",
                    ));
                }
                load_value(asm, layout, &elements[0], Reg::R10, reg_types, local_types)?;
                emit_movq_xmm_r64(asm, FReg::Xmm0, Reg::R10);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, &result_ty)?;
            }
            AsmInstructionKind::ExtractLane { vector, lane } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("extract_lane requires a concrete result type"));
                }
                if result_ty != AsmType::I64 {
                    return Err(Error::from("extract_lane only supports i64 for now"));
                }

                let vector_ty = value_type(vector, reg_types, local_types)?;
                if !matches!(vector_ty, AsmType::Vector(_, _) if size_of(&vector_ty) == 16) {
                    return Err(Error::from("extract_lane expects 128-bit vector input"));
                }
                if *lane > 1 {
                    return Err(Error::from("extract_lane lane out of range"));
                }

                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::Xmm0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                if *lane == 0 {
                    emit_movq_r64_xmm(asm, Reg::R10, FReg::Xmm0);
                } else {
                    emit_pextrq_r64_xmm_imm8(asm, Reg::R10, FReg::Xmm0, *lane as u8);
                }
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            AsmInstructionKind::InsertLane {
                vector,
                lane,
                value,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("insert_lane requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("insert_lane expects 128-bit vector result"));
                }
                if *lane > 1 {
                    return Err(Error::from("insert_lane lane out of range"));
                }

                let vector_ty = value_type(vector, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::Xmm0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
                emit_pinsrq_xmm_r64_imm8(asm, FReg::Xmm0, Reg::R10, *lane as u8);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, &result_ty)?;
            }
            AsmInstructionKind::ZipLow {
                lhs,
                rhs,
                lane_bits,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("zip_low requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("zip_low expects 128-bit vector result"));
                }
                if !matches!(*lane_bits, 16 | 32 | 64) {
                    return Err(Error::from(
                        "x86_64 zip_low only supports 16/32/64-bit lanes for now",
                    ));
                }

                let lhs_ty = value_type(lhs, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    lhs,
                    FReg::Xmm0,
                    &lhs_ty,
                    reg_types,
                    local_types,
                )?;
                let rhs_ty = value_type(rhs, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    rhs,
                    FReg::Xmm1,
                    &rhs_ty,
                    reg_types,
                    local_types,
                )?;
                match *lane_bits {
                    16 => emit_punpcklwd_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                    32 => emit_punpckldq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                    _ => emit_punpcklqdq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                }
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, &result_ty)?;
            }
            AsmInstructionKind::And(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::And,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Or(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Or,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Xor(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Xor,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Shl(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Left,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Shr(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Right,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Eq(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Eq,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ne(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ne,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Lt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Lt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Le(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Le,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Gt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Gt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ge(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ge,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Div(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                false,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Rem(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                true,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Not(value) => {
                emit_not(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::Alloca { .. } => {
                let offset = alloca_offset(layout, inst.id)?;
                emit_mov_rr(asm, Reg::R10, Reg::Rbp);
                emit_add_ri32(asm, Reg::R10, offset);
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            AsmInstructionKind::Load { address, .. } => {
                if matches!(inst.ty, AsmType::Void) {
                    return Err(Error::from("load requires a concrete type"));
                }
                emit_load(
                    asm,
                    layout,
                    inst.id,
                    address,
                    &inst.ty,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::Store { value, address, .. } => {
                emit_store(
                    asm,
                    layout,
                    value,
                    address,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
                emit_gep(asm, layout, inst.id, ptr, indices, reg_types, local_types)?;
            }
            AsmInstructionKind::SymbolAddress { symbol, .. } => {
                asm.emit_mov_imm64_reloc(Reg::R10, symbol.as_str(), 0);
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            AsmInstructionKind::Call { function, args, .. } => {
                let ty = inst.ty.clone();
                emit_call(
                    asm,
                    layout,
                    inst.id,
                    function,
                    args,
                    func_map,
                    signatures,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::Syscall {
                convention,
                number,
                args,
            } => {
                let ty = inst.ty.clone();
                emit_syscall(
                    asm,
                    layout,
                    inst.id,
                    *convention,
                    number,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?;
            }
            AsmInstructionKind::IntrinsicCall {
                kind,
                format: format_str,
                args,
            } => {
                let ty = inst.ty.clone();
                emit_intrinsic_call(
                    asm,
                    layout,
                    inst.id,
                    kind,
                    format_str,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                    format,
                )?;
            }
            AsmInstructionKind::SIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::UIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::Trunc(value, ty) => {
                emit_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::ZExt(value, ty) => {
                emit_zext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPToSI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::FPToUI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::FPTrunc(value, ty) => {
                emit_fp_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPExt(value, ty) => {
                emit_fp_ext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SExt(value, ty) => {
                emit_sext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SextOrTrunc(value, ty) => {
                emit_sext_or_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::Bitcast(value, ty) => {
                emit_bitcast(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::PtrToInt(value) => {
                emit_ptr_to_int(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::IntToPtr(value) => {
                emit_int_to_ptr(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                emit_insert_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    element,
                    indices,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::ExtractValue { aggregate, indices } => {
                emit_extract_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    indices,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                emit_select(
                    asm,
                    layout,
                    inst.id,
                    condition,
                    if_true,
                    if_false,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::LandingPad { result_type, .. } => {
                emit_landingpad(asm, layout, inst.id, result_type)?;
            }
            AsmInstructionKind::Freeze(value) => {
                emit_freeze(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InlineAsm { output_type, .. } => {
                emit_inline_asm(asm, layout, inst.id, output_type)?;
            }
            AsmInstructionKind::Unreachable => {
                emit_trap(asm);
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported LIR instruction for x86_64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        AsmTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if asm.entry_returns_exit && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm64(asm, Reg::Rax, 0);
                emit_ret(asm);
            }
        }
        AsmTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if matches!(
                abi_pass_mode(return_ty, &layout.data_layout)?,
                AbiPassMode::Indirect
            ) {
                let sret_offset = layout
                    .sret_offset
                    .ok_or_else(|| Error::from("missing sret pointer for aggregate return"))?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, sret_offset);
                match value {
                    AsmValue::Register(id) => {
                        let src_offset = agg_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::R11, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Local(id) => {
                        let src_offset = local_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::R11, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Constant(constant) => {
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::R11,
                            constant,
                            return_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                    _ => return Err(Error::from("unsupported aggregate return value")),
                }
                if asm.needs_frame {
                    emit_epilogue(asm);
                }
                emit_ret(asm);
                return Ok(());
            }
            if matches!(
                abi_pass_mode(return_ty, &layout.data_layout)?,
                AbiPassMode::Pair
            ) && is_aggregate_type(return_ty)
            {
                load_aggregate_pair(asm, layout, value, Reg::Rax, Reg::Rdx)?;
                exit_reg = Some(Reg::Rax);
            } else if matches!(return_ty, AsmType::I128) {
                load_i128_value(
                    asm,
                    layout,
                    value,
                    Reg::Rax,
                    Reg::Rdx,
                    reg_types,
                    local_types,
                )?;
                exit_reg = Some(Reg::Rax);
            } else if is_float_type(return_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::Xmm0,
                    return_ty,
                    reg_types,
                    local_types,
                )?;
            } else {
                load_value(asm, layout, value, Reg::Rax, reg_types, local_types)?;
                exit_reg = Some(Reg::Rax);
            }
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if asm.entry_returns_exit && asm.is_entry() {
                if let Some(reg) = exit_reg {
                    emit_exit_syscall_reg(asm, reg)?;
                } else {
                    emit_exit_syscall(asm, 0)?;
                }
            } else {
                emit_ret(asm);
            }
        }
        AsmTerminator::Br(target) => {
            asm.emit_jmp(Label::Block(asm.current_function, *target));
        }
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            emit_cond_branch(
                asm,
                layout,
                condition,
                Label::Block(asm.current_function, *if_true),
                Label::Block(asm.current_function, *if_false),
            )?;
        }
        AsmTerminator::Invoke {
            function,
            args,
            normal_dest,
            ..
        } => {
            emit_call(
                asm,
                layout,
                0,
                function,
                args,
                func_map,
                signatures,
                &AsmType::Void,
                reg_types,
                local_types,
                format,
                rodata,
                rodata_pool,
            )?;
            asm.emit_jmp(Label::Block(asm.current_function, *normal_dest));
        }
        AsmTerminator::Switch {
            value,
            default,
            cases,
        } => {
            emit_switch(asm, layout, value, *default, cases, reg_types, local_types)?;
        }
        AsmTerminator::Unreachable => {
            emit_trap(asm);
        }
        _ => {
            return Err(Error::from("unsupported terminator for x86_64"));
        }
    }

    Ok(())
}

pub(super) fn emit_switch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    default: BasicBlockId,
    cases: &[(u64, BasicBlockId)],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    for (case_val, target) in cases {
        if *case_val <= i32::MAX as u64 {
            emit_cmp_imm32(asm, Reg::R10, *case_val as i32);
        } else {
            emit_mov_imm64(asm, Reg::R11, *case_val);
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
        asm.emit_jcc(0x84, Label::Block(asm.current_function, *target));
    }
    asm.emit_jmp(Label::Block(asm.current_function, default));
    Ok(())
}
