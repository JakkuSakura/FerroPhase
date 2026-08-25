use super::*;

pub(super) enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub(super) fn emit_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_cmp(asm, layout, dst_id, lhs, rhs, kind, reg_types, local_types);
    }
    if is_float_type(&lhs_ty) {
        emit_float_cmp(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            kind,
            &lhs_ty,
            reg_types,
            local_types,
        )?;
        return Ok(());
    }
    let rhs_ty = value_type(rhs, reg_types, local_types)?;
    let is_scalar = |ty: &AsmType| {
        is_integer_type(ty) || matches!(ty, AsmType::Ptr(_) | AsmType::Function { .. })
    };
    if !is_scalar(&lhs_ty) || !is_scalar(&rhs_ty) {
        return Err(Error::from(format!(
            "unsupported compare operand types: lhs={lhs_ty:?}, rhs={rhs_ty:?}"
        )));
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        AsmValue::Constant(constant) => {
            if let Ok(imm) = constant_to_i64(constant, &layout.data_layout) {
                if let Ok(imm32) = i32::try_from(imm) {
                    emit_cmp_imm32(asm, Reg::R10, imm32);
                } else {
                    load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
                    emit_cmp_rr(asm, Reg::R10, Reg::R11);
                }
            } else {
                load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
                emit_cmp_rr(asm, Reg::R10, Reg::R11);
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
    }

    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0xC,
        CmpKind::Le => 0xE,
        CmpKind::Gt => 0xF,
        CmpKind::Ge => 0xD,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, Reg::R10, Reg::R11);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

pub(super) fn emit_i128_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
    load_i128_value(asm, layout, rhs, Reg::Rax, Reg::Rdx, reg_types, local_types)?;

    emit_cmp_rr(asm, Reg::R11, Reg::Rdx);
    emit_setcc(asm, 0xC, Reg::R8); // signed lt
    emit_setcc(asm, 0xF, Reg::R9); // signed gt
    emit_setcc(asm, 0x4, Reg::Rcx); // eq

    emit_cmp_rr(asm, Reg::R10, Reg::Rax);
    emit_setcc(asm, 0x2, Reg::R10); // unsigned lt
    emit_setcc(asm, 0x7, Reg::R11); // unsigned gt
    emit_setcc(asm, 0x4, Reg::Rdx); // eq

    match kind {
        CmpKind::Eq => {
            emit_and_rr(asm, Reg::Rcx, Reg::Rdx);
            store_vreg(asm, layout, dst_id, Reg::Rcx)?;
        }
        CmpKind::Ne => {
            emit_and_rr(asm, Reg::Rcx, Reg::Rdx);
            emit_mov_imm64(asm, Reg::R8, 1);
            emit_xor_rr(asm, Reg::Rcx, Reg::R8);
            store_vreg(asm, layout, dst_id, Reg::Rcx)?;
        }
        CmpKind::Lt => {
            emit_and_rr(asm, Reg::Rcx, Reg::R10);
            emit_or_rr(asm, Reg::R8, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R8)?;
        }
        CmpKind::Gt => {
            emit_and_rr(asm, Reg::Rcx, Reg::R11);
            emit_or_rr(asm, Reg::R9, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R9)?;
        }
        CmpKind::Le => {
            emit_or_rr(asm, Reg::R10, Reg::Rdx);
            emit_and_rr(asm, Reg::Rcx, Reg::R10);
            emit_or_rr(asm, Reg::R8, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R8)?;
        }
        CmpKind::Ge => {
            emit_or_rr(asm, Reg::R11, Reg::Rdx);
            emit_and_rr(asm, Reg::Rcx, Reg::R11);
            emit_or_rr(asm, Reg::R9, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R9)?;
        }
    }
    Ok(())
}

pub(super) fn emit_select(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    condition: &AsmValue,
    if_true: &AsmValue,
    if_false: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let result_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for select"))?;
    if is_float_type(&result_ty) {
        return Err(Error::from(
            "Select does not support float values on x86_64",
        ));
    }

    load_value(asm, layout, condition, Reg::R11, reg_types, local_types)?;
    emit_cmp_imm32(asm, Reg::R11, 0);
    load_value(asm, layout, if_true, Reg::R10, reg_types, local_types)?;
    load_value(asm, layout, if_false, Reg::Rax, reg_types, local_types)?;
    emit_cmovcc(asm, 0x4, Reg::R10, Reg::Rax);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

pub(super) fn emit_cond_branch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    condition: &AsmValue,
    if_true: Label,
    if_false: Label,
) -> Result<()> {
    match condition {
        AsmValue::Constant(AsmConstant::Bool(value)) => {
            if *value {
                asm.emit_jmp(if_true);
            } else {
                asm.emit_jmp(if_false);
            }
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            emit_cmp_imm32(asm, Reg::R10, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        AsmValue::Flags(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            emit_cmp_imm32(asm, Reg::R10, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}
