use super::*;

pub(super) fn emit_int_to_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    signed: bool,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_integer_type(&src_ty) {
        return Err(Error::from("int to float expects integer source"));
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    emit_scvtf(asm, FReg::V0, Reg::X16, dst_ty, signed);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
    Ok(())
}

pub(super) fn emit_float_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvtzs(asm, Reg::X16, FReg::V0, &src_ty, signed);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

pub(super) fn emit_fp_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (AsmType::F64, AsmType::F32)) {
        return Err(Error::from("unsupported FPTrunc on aarch64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvt_sd(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
    Ok(())
}

pub(super) fn emit_fp_ext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (AsmType::F32, AsmType::F64)) {
        return Err(Error::from("unsupported FPExt on aarch64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvt_ds(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
    Ok(())
}
