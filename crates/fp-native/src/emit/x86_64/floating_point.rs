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
    if matches!(src_ty, AsmType::I128) {
        return Err(Error::from("i128 to float is not supported on x86_64"));
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if signed {
        emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R10, dst_ty);
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
        return Ok(());
    }
    emit_uint_to_float(asm, Reg::R10, dst_ty)?;
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

pub(super) fn emit_uint_to_float(asm: &mut Assembler, src: Reg, dst_ty: &AsmType) -> Result<()> {
    emit_mov_rr(asm, Reg::R11, src);
    emit_shr_imm8(asm, Reg::R11, 1);
    emit_and_ri32(asm, src, 1);
    emit_or_rr(asm, Reg::R11, src);
    emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R11, &AsmType::F64);
    emit_addsd(asm, FReg::Xmm0, FReg::Xmm0, &AsmType::F64);
    if matches!(dst_ty, AsmType::F32) {
        emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    }
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
    _signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        return Err(Error::from("i128 from float is not supported on x86_64"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvttsd2si(asm, Reg::R10, FReg::Xmm0, &src_ty);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
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
        return Err(Error::from("unsupported FPTrunc on x86_64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
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
        return Err(Error::from("unsupported FPExt on x86_64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvtss2sd(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}
