use super::*;

pub(super) fn emit_gep(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    ptr: &AsmValue,
    indices: &[AsmValue],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let ptr_ty = value_type(ptr, reg_types, local_types)?;
    let mut current_ty = match ptr_ty {
        AsmType::Ptr(inner) => *inner,
        _ => return Err(Error::from("GEP expects pointer base type")),
    };
    let mut const_offset = if let AsmValue::Register(id) = ptr {
        asm.vreg_sp_offset(*id).map(|offset| offset as i64)
    } else {
        None
    };

    load_value(asm, layout, ptr, Reg::X16, reg_types, local_types)?;
    for index in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                let idx = match index {
                    AsmValue::Constant(constant) => {
                        let raw = constant_to_i64(constant, &layout.data_layout)?;
                        usize::try_from(raw).map_err(|e| {
                            Error::from(format!("GEP struct index out of range: {e}"))
                        })?
                    }
                    _ => return Err(Error::from("GEP struct index must be constant")),
                };
                let layout = struct_layout(&current_ty)
                    .ok_or_else(|| Error::from("missing struct layout for GEP"))?;
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
                add_immediate_offset(asm, Reg::X16, field_offset as i64);
                if let Some(base) = const_offset.as_mut() {
                    *base += field_offset as i64;
                }
                current_ty = fields
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                match index {
                    AsmValue::Constant(constant) => {
                        if let Some(base) = const_offset.as_mut() {
                            let idx = constant_to_i64(constant, &layout.data_layout)?;
                            *base += idx * size_of(elem) as i64;
                        }
                    }
                    _ => const_offset = None,
                }
                emit_scaled_index(
                    asm,
                    layout,
                    index,
                    size_of(elem) as u64,
                    reg_types,
                    local_types,
                )?;
                current_ty = *elem.clone();
            }
            _ => {
                match index {
                    AsmValue::Constant(constant) => {
                        if let Some(base) = const_offset.as_mut() {
                            let idx = constant_to_i64(constant, &layout.data_layout)?;
                            *base += idx * size_of(&current_ty) as i64;
                        }
                    }
                    _ => const_offset = None,
                }
                emit_scaled_index(
                    asm,
                    layout,
                    index,
                    size_of(&current_ty) as u64,
                    reg_types,
                    local_types,
                )?;
            }
        }
    }

    store_vreg(asm, layout, dst_id, Reg::X16)?;
    if let Some(offset) = const_offset {
        if let Ok(offset) = i32::try_from(offset) {
            asm.record_vreg_sp_offset(dst_id, offset);
        }
    }
    Ok(())
}

pub(super) fn emit_scaled_index(
    asm: &mut Assembler,
    layout: &FrameLayout,
    index: &AsmValue,
    elem_size: u64,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if elem_size == 0 {
        return Ok(());
    }
    load_value(asm, layout, index, Reg::X17, reg_types, local_types)?;
    if elem_size != 1 {
        let imm = u16::try_from(elem_size)
            .map_err(|e| Error::from(format!("GEP element size too large for aarch64: {e}")))?;
        emit_mov_imm16(asm, Reg::X9, imm);
        emit_mul_reg(asm, Reg::X17, Reg::X17, Reg::X9);
    }
    emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    Ok(())
}

pub(super) fn add_immediate_offset(asm: &mut Assembler, base: Reg, offset: i64) {
    if offset == 0 {
        return;
    }
    let scratch = if base == Reg::X17 { Reg::X9 } else { Reg::X17 };
    if offset < 0 {
        let abs = (-offset) as u64;
        if abs <= 4095 {
            emit_sub_imm12(asm, base, base, abs as u32);
            return;
        }
        if let Ok(imm) = u16::try_from(abs) {
            emit_mov_imm16(asm, scratch, imm);
            emit_sub_reg(asm, base, base, scratch);
        } else {
            emit_mov_imm64(asm, scratch, abs as u64);
            emit_sub_reg(asm, base, base, scratch);
        }
        return;
    }
    if offset <= 4095 {
        emit_add_imm12(asm, base, base, offset as u32);
        return;
    }
    if let Ok(imm) = u16::try_from(offset) {
        emit_mov_imm16(asm, scratch, imm);
        emit_add_reg(asm, base, base, scratch);
    } else {
        emit_mov_imm64(asm, scratch, offset as u64);
        emit_add_reg(asm, base, base, scratch);
    }
}
