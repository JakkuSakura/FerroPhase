use super::*;

pub(super) fn emit_bitcast(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
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
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_size = size_of(&src_ty);
    let dst_size = size_of(dst_ty);
    if src_size != dst_size || src_size > 8 {
        return Err(Error::from("unsupported bitcast size for aarch64"));
    }
    if src_size == 0 {
        return Ok(());
    }

    match (is_float_type(&src_ty), is_float_type(dst_ty), src_size) {
        (true, false, 4) => {
            load_value_float(
                asm,
                layout,
                value,
                FReg::V0,
                &src_ty,
                reg_types,
                local_types,
            )?;
            emit_fmov_w_from_s(asm, Reg::X16, FReg::V0);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
        (true, false, 8) => {
            load_value_float(
                asm,
                layout,
                value,
                FReg::V0,
                &src_ty,
                reg_types,
                local_types,
            )?;
            emit_fmov_x_from_d(asm, Reg::X16, FReg::V0);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
        (false, true, 4) => {
            load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
            emit_fmov_s_from_w(asm, FReg::V0, Reg::X16);
            store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
        }
        (false, true, 8) => {
            load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
            emit_fmov_d_from_x(asm, FReg::V0, Reg::X16);
            store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
        }
        _ => {
            load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
    }
    Ok(())
}

pub(super) fn emit_insert_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    aggregate: &AsmValue,
    element: &AsmValue,
    indices: &[u32],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
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
    let struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let agg_ty = value_type(aggregate, reg_types, local_types)?;
    if !is_large_aggregate(&agg_ty, &layout.data_layout) {
        let size = size_of(&agg_ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let (field_offset, field_ty) =
            aggregate_field_offset(&agg_ty, indices, &layout.data_layout)?;
        if field_offset != 0 || size_of(&field_ty) as i32 != size {
            return Err(Error::from("unsupported InsertValue for small aggregate"));
        }
        if is_float_type(&field_ty) {
            return Err(Error::from(
                "unsupported float InsertValue for small aggregate",
            ));
        }
        load_value(asm, layout, element, Reg::X16, reg_types, local_types)?;
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    let size = size_of(&agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    let dst_offset = agg_offset(layout, dst_id)?;

    match aggregate {
        AsmValue::Register(id) => {
            let src_offset = agg_offset(layout, *id)?;
            copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
        }
        AsmValue::Constant(AsmConstant::Undef(_)) => {
            zero_sp_range(asm, dst_offset, size)?;
        }
        _ => return Err(Error::from("unsupported InsertValue aggregate source")),
    }

    let (field_offset, field_ty) = aggregate_field_offset(&agg_ty, indices, &layout.data_layout)?;
    let store_offset = dst_offset + field_offset as i32;
    if is_large_aggregate(&field_ty, &layout.data_layout) {
        let field_size = size_of(&field_ty) as i32;
        if field_size == 0 {
            return Ok(());
        }
        match element {
            AsmValue::Register(id) => {
                let src_offset = agg_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            AsmValue::Local(id) => {
                let src_offset = local_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            AsmValue::Constant(AsmConstant::Struct(values, ty)) => {
                let fields = match ty {
                    AsmType::Struct { fields, .. } => fields,
                    _ => return Err(Error::from("expected struct type for InsertValue")),
                };
                let struct_layout = struct_layout(ty)
                    .ok_or_else(|| Error::from("missing struct layout for InsertValue"))?;
                for (idx, field) in values.iter().enumerate() {
                    let field_offset = *struct_layout
                        .field_offsets
                        .get(idx)
                        .ok_or_else(|| Error::from("aggregate field out of range"))?;
                    let field_ty = fields
                        .get(idx)
                        .ok_or_else(|| Error::from("aggregate field out of range"))?;
                    let field_size = size_of(field_ty);
                    match field {
                        AsmConstant::String(text) => {
                            let offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    let dst = store_offset + field_offset as i32;
                    match field_size {
                        1 => emit_store8_to_sp(asm, Reg::X16, dst)?,
                        2 => emit_store16_to_sp(asm, Reg::X16, dst)?,
                        4 => emit_store32_to_sp(asm, Reg::X16, dst)?,
                        8 => emit_store_to_sp(asm, Reg::X16, dst),
                        _ => {
                            return Err(Error::from(
                                "unsupported aggregate field size in InsertValue",
                            ));
                        }
                    }
                }
            }
            AsmValue::Constant(AsmConstant::Undef(_)) => {
                zero_sp_range(asm, store_offset, field_size)?;
            }
            _ => return Err(Error::from("unsupported InsertValue aggregate element")),
        }
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    if is_float_type(&field_ty) {
        load_value_float(
            asm,
            layout,
            element,
            FReg::V0,
            &field_ty,
            reg_types,
            local_types,
        )?;
        emit_store_float_to_sp(asm, FReg::V0, store_offset, &field_ty);
    } else {
        if let AsmValue::Constant(AsmConstant::String(text)) = element {
            let offset = intern_cstring(rodata, rodata_pool, text);
            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I16 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I32 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for aarch64: {:?}",
                        field_ty
                    )));
                }
            }
        } else {
            load_value(asm, layout, element, Reg::X16, reg_types, local_types)?;
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I16 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I32 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for aarch64: {:?}",
                        field_ty
                    )));
                }
            }
        }
    }

    emit_mov_reg(asm, Reg::X16, Reg::X31);
    add_immediate_offset(asm, Reg::X16, dst_offset as i64);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    asm.record_vreg_sp_offset(dst_id, dst_offset);
    Ok(())
}

pub(super) fn emit_extract_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    aggregate: &AsmValue,
    indices: &[u32],
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
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let agg_ty = value_type(aggregate, reg_types, local_types)?;
    if !is_large_aggregate(&agg_ty, &layout.data_layout) {
        return Err(Error::from("ExtractValue expects aggregate"));
    }
    let size = size_of(&agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    let src_offset = match aggregate {
        AsmValue::Register(id) => agg_offset(layout, *id)?,
        AsmValue::Local(id) => local_offset(layout, *id)?,
        _ => return Err(Error::from("unsupported ExtractValue aggregate source")),
    };
    let (field_offset, _field_ty) = aggregate_field_offset(&agg_ty, indices, &layout.data_layout)?;
    let result_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for extractvalue"))?;
    if is_large_aggregate(&result_ty, &layout.data_layout) {
        let field_size = size_of(&result_ty) as i32;
        if field_size == 0 {
            return Ok(());
        }
        if let Ok(dst_offset) = agg_offset(layout, dst_id) {
            emit_load_from_sp(asm, Reg::X9, src_offset);
            add_immediate_offset(asm, Reg::X9, field_offset as i64);
            copy_reg_to_sp(asm, Reg::X9, dst_offset, field_size)?;
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            add_immediate_offset(asm, Reg::X16, dst_offset as i64);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
            asm.record_vreg_sp_offset(dst_id, dst_offset);
        } else {
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            emit_load_from_sp(asm, Reg::X9, src_offset);
            add_immediate_offset(asm, Reg::X9, field_offset as i64);
            emit_mov_reg(asm, Reg::X16, Reg::X9);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
            asm.record_vreg_sp_offset(dst_id, src_offset + field_offset as i32);
        }
        return Ok(());
    }
    if is_float_type(&result_ty) {
        emit_load_from_sp(asm, Reg::X9, src_offset);
        add_immediate_offset(asm, Reg::X9, field_offset as i64);
        emit_load_float_from_reg(asm, FReg::V0, Reg::X9, &result_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &result_ty)?;
    } else {
        emit_load_from_sp(asm, Reg::X9, src_offset);
        add_immediate_offset(asm, Reg::X9, field_offset as i64);
        match result_ty {
            AsmType::I1 => emit_load8u_from_reg(asm, Reg::X16, Reg::X9),
            AsmType::I8 => emit_load8s_from_reg(asm, Reg::X16, Reg::X9),
            AsmType::I16 => emit_load16s_from_reg(asm, Reg::X16, Reg::X9),
            AsmType::I32 => emit_load32s_from_reg(asm, Reg::X16, Reg::X9),
            AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
            }
            _ if is_aggregate_type(&result_ty) && size_of(&result_ty) <= 8 => {
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
            }
            _ => {
                return Err(Error::from(format!(
                    "unsupported ExtractValue element type for aarch64: {:?}",
                    result_ty
                )));
            }
        }
        store_vreg(asm, layout, dst_id, Reg::X16)?;
    }
    Ok(())
}
