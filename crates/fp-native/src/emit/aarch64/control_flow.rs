use super::*;

pub(super) enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
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
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
        emit_fcmp(asm, FReg::V0, FReg::V1, &lhs_ty);
        let cond = match kind {
            CmpKind::Eq => 0,
            CmpKind::Ne => 1,
            CmpKind::Lt => 11,
            CmpKind::Le => 13,
            CmpKind::Gt => 12,
            CmpKind::Ge => 10,
            CmpKind::Ult => 11,
            CmpKind::Ule => 13,
            CmpKind::Ugt => 12,
            CmpKind::Uge => 10,
        };
        emit_cset(asm, Reg::X16, cond);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    match rhs {
        AsmValue::Constant(constant) => {
            if matches!(
                constant,
                AsmConstant::GlobalRef(_, _, _) | AsmConstant::FunctionRef(_, _)
            ) {
                load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
                emit_cmp_reg(asm, Reg::X16, Reg::X17);
            } else {
                let imm = constant_to_i64(constant, &layout.data_layout)?;
                if (0..=4095).contains(&imm) {
                    emit_cmp_imm12(asm, Reg::X16, imm as u32);
                } else {
                    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
                    emit_cmp_reg(asm, Reg::X16, Reg::X17);
                }
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            emit_cmp_reg(asm, Reg::X16, Reg::X17);
        }
    }

    let cond = match kind {
        CmpKind::Eq => 0,
        CmpKind::Ne => 1,
        CmpKind::Lt => 11,
        CmpKind::Le => 13,
        CmpKind::Gt => 12,
        CmpKind::Ge => 10,
        CmpKind::Ult => 3,
        CmpKind::Ule => 9,
        CmpKind::Ugt => 8,
        CmpKind::Uge => 2,
    };
    emit_cset(asm, Reg::X16, cond);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
    load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;

    // Compare high parts.
    emit_cmp_reg(asm, Reg::X17, Reg::X10);
    emit_cset(asm, Reg::X11, 11); // signed lt
    emit_cset(asm, Reg::X12, 12); // signed gt
    emit_cset(asm, Reg::X13, 0); // eq
    emit_cset(asm, Reg::X14, 3); // unsigned lt (LO)
    emit_cset(asm, Reg::X15, 8); // unsigned gt (HI)

    // Compare low parts.
    emit_cmp_reg(asm, Reg::X16, Reg::X9);
    emit_cset(asm, Reg::X9, 3); // unsigned lt (LO)
    emit_cset(asm, Reg::X10, 8); // unsigned gt (HI)
    emit_cset(asm, Reg::X16, 0); // eq

    // overall_eq = hi_eq & lo_eq
    emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X16);

    match kind {
        CmpKind::Eq => {
            store_vreg(asm, layout, dst_id, Reg::X17)?;
        }
        CmpKind::Ne => {
            emit_mov_imm16(asm, Reg::X10, 1);
            emit_eor_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            store_vreg(asm, layout, dst_id, Reg::X17)?;
        }
        CmpKind::Lt => {
            // hi_lt_signed | (hi_eq & lo_lt_unsigned)
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X9);
            emit_or_reg(asm, Reg::X11, Reg::X11, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X11)?;
        }
        CmpKind::Gt => {
            // hi_gt_signed | (hi_eq & lo_gt_unsigned)
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X10);
            emit_or_reg(asm, Reg::X12, Reg::X12, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X12)?;
        }
        CmpKind::Le => {
            // hi_lt_signed | (hi_eq & (lo_lt_unsigned | lo_eq))
            emit_or_reg(asm, Reg::X17, Reg::X9, Reg::X16);
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X17);
            emit_or_reg(asm, Reg::X11, Reg::X11, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X11)?;
        }
        CmpKind::Ge => {
            // hi_gt_signed | (hi_eq & (lo_gt_unsigned | lo_eq))
            emit_or_reg(asm, Reg::X17, Reg::X10, Reg::X16);
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X17);
            emit_or_reg(asm, Reg::X12, Reg::X12, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X12)?;
        }
        CmpKind::Ult => {
            // hi_lt_unsigned | (hi_eq & lo_lt_unsigned)
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X9);
            emit_or_reg(asm, Reg::X16, Reg::X14, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
        CmpKind::Ugt => {
            // hi_gt_unsigned | (hi_eq & lo_gt_unsigned)
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X10);
            emit_or_reg(asm, Reg::X16, Reg::X15, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
        CmpKind::Ule => {
            // hi_lt_unsigned | (hi_eq & (lo_lt_unsigned | lo_eq))
            emit_or_reg(asm, Reg::X17, Reg::X9, Reg::X16);
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X17);
            emit_or_reg(asm, Reg::X16, Reg::X14, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
        CmpKind::Uge => {
            // hi_gt_unsigned | (hi_eq & (lo_gt_unsigned | lo_eq))
            emit_or_reg(asm, Reg::X17, Reg::X10, Reg::X16);
            emit_and_reg(asm, Reg::X17, Reg::X13, Reg::X17);
            emit_or_reg(asm, Reg::X16, Reg::X15, Reg::X17);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        load_value(asm, layout, condition, Reg::X16, reg_types, local_types)?;
        emit_cmp_imm12(asm, Reg::X16, 0);
        load_value_float(
            asm,
            layout,
            if_true,
            FReg::V0,
            &result_ty,
            reg_types,
            local_types,
        )?;
        load_value_float(
            asm,
            layout,
            if_false,
            FReg::V1,
            &result_ty,
            reg_types,
            local_types,
        )?;
        emit_fcsel(asm, FReg::V0, FReg::V0, FReg::V1, 1, &result_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &result_ty)?;
        return Ok(());
    }

    load_value(asm, layout, condition, Reg::X16, reg_types, local_types)?;
    emit_cmp_imm12(asm, Reg::X16, 0);
    load_value(asm, layout, if_true, Reg::X17, reg_types, local_types)?;
    load_value(asm, layout, if_false, Reg::X9, reg_types, local_types)?;
    emit_csel(asm, Reg::X16, Reg::X17, Reg::X9, 1);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
                asm.emit_b(if_true);
            } else {
                asm.emit_b(if_false);
            }
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            emit_cmp_imm12(asm, Reg::X16, 0);
            asm.emit_b_cond(1, if_true);
            asm.emit_b(if_false);
        }
        AsmValue::Flags(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            emit_cmp_imm12(asm, Reg::X16, 0);
            asm.emit_b_cond(1, if_true);
            asm.emit_b(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}
