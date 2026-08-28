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
    if src_size != dst_size
        || !matches!(
            abi_pass_mode(&src_ty, &layout.data_layout)?,
            AbiPassMode::Direct
        )
    {
        return Err(Error::from("unsupported bitcast size for x86_64"));
    }
    if src_size == 0 {
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::R10)?;
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
    if !is_aggregate_storage(&agg_ty, &layout.data_layout) {
        return Err(Error::from("InsertValue expects aggregate"));
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
    if is_aggregate_storage(&field_ty, &layout.data_layout) {
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
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    let dst = store_offset + field_offset as i32;
                    match field_size {
                        1 => emit_mov_mr8(asm, Reg::Rbp, dst, Reg::R10),
                        2 => emit_mov_mr16(asm, Reg::Rbp, dst, Reg::R10),
                        4 => emit_mov_mr32(asm, Reg::Rbp, dst, Reg::R10),
                        8 => emit_mov_mr64(asm, Reg::Rbp, dst, Reg::R10),
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
        emit_mov_rr(asm, Reg::R10, Reg::Rbp);
        emit_add_ri32(asm, Reg::R10, dst_offset);
        store_vreg(asm, layout, dst_id, Reg::R10)?;
        return Ok(());
    }
    if is_float_type(&field_ty) {
        load_value_float(
            asm,
            layout,
            element,
            FReg::Xmm0,
            &field_ty,
            reg_types,
            local_types,
        )?;
        emit_movsd_m64x(asm, Reg::Rbp, store_offset, FReg::Xmm0, &field_ty);
    } else {
        if let AsmValue::Constant(AsmConstant::String(text)) = element {
            let offset = intern_cstring(rodata, rodata_pool, text);
            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )));
                }
            }
        } else {
            load_value(asm, layout, element, Reg::R10, reg_types, local_types)?;
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )));
                }
            }
        }
    }

    emit_mov_rr(asm, Reg::R10, Reg::Rbp);
    emit_add_ri32(asm, Reg::R10, dst_offset);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
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
    if !is_aggregate_storage(&agg_ty, &layout.data_layout) {
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
    let load_offset = src_offset + field_offset as i32;
    let result_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for extractvalue"))?;
    if is_aggregate_storage(&result_ty, &layout.data_layout) {
        let field_size = size_of(&result_ty) as i32;
        if field_size == 0 {
            return Ok(());
        }
        if let Ok(dst_offset) = agg_offset(layout, dst_id) {
            copy_sp_to_sp(asm, load_offset, dst_offset, field_size)?;
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, dst_offset);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        } else {
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, load_offset);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
        return Ok(());
    }
    if is_float_type(&result_ty) {
        emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, load_offset, &result_ty);
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &result_ty)?;
    } else {
        match result_ty {
            AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, load_offset);
            }
            _ => {
                return Err(Error::from(format!(
                    "unsupported ExtractValue element type for x86_64: {:?}",
                    result_ty
                )));
            }
        }
        store_vreg(asm, layout, dst_id, Reg::R10)?;
    }
    Ok(())
}
