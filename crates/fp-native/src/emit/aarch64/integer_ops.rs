use super::*;

pub(super) fn emit_bitwise_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BitOp,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
        load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
        match op {
            BitOp::And => {
                emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_and_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
            BitOp::Or => {
                emit_or_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_or_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
            BitOp::Xor => {
                emit_eor_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_eor_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
        }
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
    match op {
        BitOp::And => emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17),
        BitOp::Or => emit_or_reg(asm, Reg::X16, Reg::X16, Reg::X17),
        BitOp::Xor => emit_eor_reg(asm, Reg::X16, Reg::X16, Reg::X17),
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_shift(asm, layout, dst_id, lhs, rhs, kind, reg_types, local_types);
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
    match kind {
        ShiftKind::Left => emit_lslv(asm, Reg::X16, Reg::X16, Reg::X17),
        ShiftKind::Right => emit_lsrv(asm, Reg::X16, Reg::X16, Reg::X17),
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_not(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let ty = value_type(value, reg_types, local_types)?;
    if matches!(ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        emit_mov_imm16(asm, Reg::X9, 0);
        emit_sub_imm12(asm, Reg::X9, Reg::X9, 1);
        emit_sub_reg(asm, Reg::X16, Reg::X9, Reg::X16);
        emit_sub_reg(asm, Reg::X17, Reg::X9, Reg::X17);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    emit_mov_imm16(asm, Reg::X17, 0);
    emit_sub_imm12(asm, Reg::X17, Reg::X17, 1);
    emit_sub_reg(asm, Reg::X16, Reg::X17, Reg::X16);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_zext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits > dst_bits {
        return Err(Error::from("zext expects wider destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        if src_bits < 64 {
            let mask = (1u64 << src_bits) - 1;
            emit_mov_imm64(asm, Reg::X17, mask);
            emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
        }
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    if src_bits < 64 {
        let mask = if src_bits == 64 {
            u64::MAX
        } else {
            (1u64 << src_bits) - 1
        };
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let dst_bits = int_bits(dst_ty)?;
    let src_ty = value_type(value, reg_types, local_types)?;
    if matches!(src_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    }
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_sext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits > dst_bits {
        return Err(Error::from("sext expects wider destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        emit_mov_reg(asm, Reg::X17, Reg::X16);
        emit_mov_imm16(asm, Reg::X9, 63);
        emit_asrv(asm, Reg::X17, Reg::X17, Reg::X9);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    if src_bits < 64 {
        let shift = 64 - src_bits;
        emit_mov_imm16(asm, Reg::X17, shift as u16);
        emit_lslv(asm, Reg::X16, Reg::X16, Reg::X17);
        emit_asrv(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_sext_or_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if matches!(dst_ty, AsmType::I128) {
        return emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
    if src_bits >= dst_bits {
        return emit_trunc(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
    emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types)
}

pub(super) fn emit_ptr_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    let dst_ty = reg_types
        .get(&dst_id)
        .ok_or_else(|| Error::from("missing type for ptrtoint"))?;
    let dst_bits = int_bits(dst_ty)?;
    if matches!(dst_ty, AsmType::I128) {
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_int_to_ptr(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    if matches!(src_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    }
    if src_bits < 64 {
        let mask = (1u64 << src_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_freeze(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let ty = value_type(value, reg_types, local_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::V0, &ty, reg_types, local_types)?;
        store_vreg_float(asm, layout, dst_id, FReg::V0, &ty)?;
        return Ok(());
    }
    if matches!(ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}
