use super::*;

pub(super) fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &AsmValue,
    ty: &AsmType,
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
    if matches!(ty, AsmType::I128) {
        match address {
            AsmValue::StackSlot(id) => {
                let (base, offset) = stack_slot_base_and_offset(layout, *id)?;
                emit_load_from_base(asm, Reg::X16, base, offset);
                emit_load_from_base(asm, Reg::X17, base, offset + 8);
            }
            AsmValue::Register(id) => {
                let offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, offset);
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8);
                emit_load_from_reg(asm, Reg::X17, Reg::X9);
            }
            AsmValue::Local(id) => {
                let offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, offset);
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8);
                emit_load_from_reg(asm, Reg::X17, Reg::X9);
            }
            _ => return Err(Error::from("unsupported load address for i128 on aarch64")),
        }
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    if is_large_aggregate(ty, &layout.data_layout) {
        let size = size_of(ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let dst_offset = agg_offset(layout, dst_id)?;
        match address {
            AsmValue::StackSlot(id) => {
                let (base, src_offset) = stack_slot_base_and_offset(layout, *id)?;
                if base != Reg::X31 {
                    return Err(Error::from("unsupported aggregate load from regfile slot"));
                }
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_reg_to_sp(asm, Reg::X17, dst_offset, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_reg_to_sp(asm, Reg::X17, dst_offset, size)?;
            }
            _ => return Err(Error::from("unsupported load address for aarch64")),
        }
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        asm.record_vreg_sp_offset(dst_id, dst_offset);
        return Ok(());
    }
    match address {
        AsmValue::StackSlot(id) => {
            let (base, offset) = stack_slot_base_and_offset(layout, *id)?;
            if is_freg_type(ty, &layout.data_layout) {
                if base != Reg::X31 {
                    return Err(Error::from("unsupported float load from regfile slot"));
                }
                emit_load_float_from_sp(asm, FReg::V0, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_load8u_from_base(asm, Reg::X16, base, offset)?,
                    AsmType::I8 => emit_load8s_from_base(asm, Reg::X16, base, offset)?,
                    AsmType::I16 => emit_load16s_from_base(asm, Reg::X16, base, offset)?,
                    AsmType::I32 => emit_load32s_from_base(asm, Reg::X16, base, offset)?,
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_load_from_base(asm, Reg::X16, base, offset);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_base(asm, Reg::X16, base, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X16)?;
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            if is_freg_type(ty, &layout.data_layout) {
                emit_load_float_from_reg(asm, FReg::V0, Reg::X16, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_load8u_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I8 => emit_load8s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I16 => emit_load16s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I32 => emit_load32s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X17)?;
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            if is_freg_type(ty, &layout.data_layout) {
                emit_load_float_from_reg(asm, FReg::V0, Reg::X16, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_load8u_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I8 => emit_load8s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I16 => emit_load16s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I32 => emit_load32s_from_reg(asm, Reg::X17, Reg::X16),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X17)?;
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for aarch64")),
    }
}

pub(super) fn emit_store(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    address: &AsmValue,
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
    if let AsmValue::Constant(AsmConstant::String(text)) = value {
        let offset = intern_cstring(rodata, rodata_pool, text);
        match address {
            AsmValue::StackSlot(id) => {
                let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_base(asm, Reg::X16, base, dst_offset);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X17);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X17);
            }
            AsmValue::Global(name, _) => {
                emit_load_symbol_addr(asm, Reg::X17, name, 0)?;
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X17);
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
        }
        return Ok(());
    }
    if let AsmValue::Constant(AsmConstant::Array(values, elem_ty)) = value {
        if values.is_empty() {
            return Ok(());
        }
        let elem_ty = match elem_ty {
            AsmType::Array(elem, _) => elem.as_ref(),
            other => other,
        };
        let elem_size = size_of(elem_ty) as i32;
        let store_elem_sp = |asm: &mut Assembler, offset: i32| -> Result<()> {
            match elem_size {
                1 => emit_store8_to_sp(asm, Reg::X16, offset),
                2 => emit_store16_to_sp(asm, Reg::X16, offset),
                4 => emit_store32_to_sp(asm, Reg::X16, offset),
                8 => {
                    emit_store_to_sp(asm, Reg::X16, offset);
                    Ok(())
                }
                _ => Err(Error::from(
                    "unsupported array element size in constant store",
                )),
            }
        };
        let store_elem_reg = |asm: &mut Assembler| -> Result<()> {
            match elem_size {
                1 => {
                    emit_store8_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                2 => {
                    emit_store16_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                4 => {
                    emit_store32_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                8 => {
                    emit_store_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                _ => Err(Error::from(
                    "unsupported array element size in constant store",
                )),
            }
        };
        match address {
            AsmValue::StackSlot(id) => {
                let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                if base != Reg::X31 {
                    return Err(Error::from("unsupported array store into regfile slot"));
                }
                for (idx, elem) in values.iter().enumerate() {
                    let offset = dst_offset + (idx as i32) * elem_size;
                    if matches!(elem, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                        emit_mov_reg(asm, Reg::X9, Reg::X31);
                        add_immediate_offset(asm, Reg::X9, offset as i64);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::X9,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                        continue;
                    }
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    store_elem_sp(asm, offset)?;
                }
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    if matches!(elem, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                        emit_mov_reg(asm, Reg::X9, Reg::X17);
                        add_immediate_offset(asm, Reg::X9, offset as i64);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::X9,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                        continue;
                    }
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    emit_mov_reg(asm, Reg::X9, Reg::X17);
                    add_immediate_offset(asm, Reg::X9, offset as i64);
                    store_elem_reg(asm)?;
                }
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    if matches!(elem, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                        emit_mov_reg(asm, Reg::X9, Reg::X17);
                        add_immediate_offset(asm, Reg::X9, offset as i64);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::X9,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                        continue;
                    }
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    emit_mov_reg(asm, Reg::X9, Reg::X17);
                    add_immediate_offset(asm, Reg::X9, offset as i64);
                    store_elem_reg(asm)?;
                }
            }
            AsmValue::Global(name, _) => {
                emit_load_symbol_addr(asm, Reg::X17, name, 0)?;
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    if matches!(elem, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                        emit_mov_reg(asm, Reg::X9, Reg::X17);
                        add_immediate_offset(asm, Reg::X9, offset as i64);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::X9,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                        continue;
                    }
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    emit_mov_reg(asm, Reg::X9, Reg::X17);
                    add_immediate_offset(asm, Reg::X9, offset as i64);
                    store_elem_reg(asm)?;
                }
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
        }
        return Ok(());
    }
    let value_ty = value_type(value, reg_types, local_types)?;
    if matches!(value_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        match address {
            AsmValue::StackSlot(id) => {
                let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                emit_store_to_base(asm, Reg::X16, base, dst_offset);
                emit_store_to_base(asm, Reg::X17, base, dst_offset + 8);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, addr_offset);
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8);
                emit_store_to_reg(asm, Reg::X17, Reg::X9);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, addr_offset);
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8);
                emit_store_to_reg(asm, Reg::X17, Reg::X9);
            }
            AsmValue::Global(name, _) => {
                emit_load_symbol_addr(asm, Reg::X9, name, 0)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8);
                emit_store_to_reg(asm, Reg::X17, Reg::X9);
            }
            _ => return Err(Error::from("unsupported store address for i128 on aarch64")),
        }
        return Ok(());
    }
    if matches!(value, AsmValue::Constant(AsmConstant::Array(values, _)) if values.is_empty()) {
        return Ok(());
    }
    if size_of(&value_ty) == 0 {
        return Ok(());
    }
    if let AsmValue::Constant(constant) = value {
        if matches!(
            constant,
            AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)
        ) && size_of(&value_ty) <= 8
        {
            let bits = pack_small_aggregate(constant, &value_ty, &layout.data_layout)?;
            emit_mov_imm64(asm, Reg::X16, bits);
            match address {
                AsmValue::StackSlot(id) => {
                    let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                    emit_store_to_base(asm, Reg::X16, base, dst_offset);
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_store_to_reg(asm, Reg::X16, Reg::X17);
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_store_to_reg(asm, Reg::X16, Reg::X17);
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
            }
            return Ok(());
        }
    }
    if is_large_aggregate(&value_ty, &layout.data_layout) {
        let size = size_of(&value_ty) as i32;
        if let AsmValue::Constant(AsmConstant::Struct(values, ty)) = value {
            let fields = match ty {
                AsmType::Struct { fields, .. } => fields,
                _ => return Err(Error::from("expected struct type for constant store")),
            };
            let struct_layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            match address {
                AsmValue::StackSlot(id) => {
                    let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                    if base != Reg::X31 {
                        return Err(Error::from("unsupported aggregate store into regfile slot"));
                    }
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        if matches!(field, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                            let store_offset = dst_offset + field_offset as i32;
                            emit_mov_reg(asm, Reg::X9, Reg::X31);
                            add_immediate_offset(asm, Reg::X9, store_offset as i64);
                            store_constant_aggregate_to_reg(
                                asm,
                                &layout.data_layout,
                                Reg::X9,
                                field,
                                field_ty,
                                rodata,
                                rodata_pool,
                            )?;
                            continue;
                        }
                        match field {
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), 0)?;
                            }
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
                        let store_offset = dst_offset + field_offset as i32;
                        match field_size {
                            1 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                            2 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                            4 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                            8 => emit_store_to_sp(asm, Reg::X16, store_offset),
                            16 => {
                                emit_mov_reg(asm, Reg::X10, Reg::X31);
                                add_immediate_offset(asm, Reg::X10, store_offset as i64);
                                let len: u64 = if let AsmConstant::String(text) = field {
                                    text.len() as u64
                                } else {
                                    0
                                };
                                emit_mov_imm64(asm, Reg::X17, len);
                                emit_store_pair_base(asm, Reg::X10, Reg::X16, Reg::X17, 0);
                            }
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_mov_reg(asm, Reg::X11, Reg::X17);
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        if matches!(field, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                            emit_mov_reg(asm, Reg::X9, Reg::X11);
                            add_immediate_offset(asm, Reg::X9, field_offset as i64);
                            store_constant_aggregate_to_reg(
                                asm,
                                &layout.data_layout,
                                Reg::X9,
                                field,
                                field_ty,
                                rodata,
                                rodata_pool,
                            )?;
                            continue;
                        }
                        match field {
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), 0)?;
                            }
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
                        emit_mov_reg(asm, Reg::X10, Reg::X11);
                        add_immediate_offset(asm, Reg::X10, field_offset as i64);
                        match field_size {
                            1 => emit_store8_to_reg(asm, Reg::X16, Reg::X10),
                            2 => emit_store16_to_reg(asm, Reg::X16, Reg::X10),
                            4 => emit_store32_to_reg(asm, Reg::X16, Reg::X10),
                            8 => emit_store_to_reg(asm, Reg::X16, Reg::X10),
                            16 => {
                                let len: u64 = if let AsmConstant::String(text) = field {
                                    text.len() as u64
                                } else {
                                    0
                                };
                                emit_mov_imm64(asm, Reg::X17, len);
                                emit_store_pair_base(asm, Reg::X10, Reg::X16, Reg::X17, 0);
                            }
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_mov_reg(asm, Reg::X11, Reg::X17);
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        if matches!(field, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                            emit_mov_reg(asm, Reg::X9, Reg::X11);
                            add_immediate_offset(asm, Reg::X9, field_offset as i64);
                            store_constant_aggregate_to_reg(
                                asm,
                                &layout.data_layout,
                                Reg::X9,
                                field,
                                field_ty,
                                rodata,
                                rodata_pool,
                            )?;
                            continue;
                        }
                        match field {
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_load_symbol_addr(asm, Reg::X16, name.as_str(), 0)?;
                            }
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
                        emit_mov_reg(asm, Reg::X10, Reg::X11);
                        add_immediate_offset(asm, Reg::X10, field_offset as i64);
                        match field_size {
                            1 => emit_store8_to_reg(asm, Reg::X16, Reg::X10),
                            2 => emit_store16_to_reg(asm, Reg::X16, Reg::X10),
                            4 => emit_store32_to_reg(asm, Reg::X16, Reg::X10),
                            8 => emit_store_to_reg(asm, Reg::X16, Reg::X10),
                            16 => {
                                let len: u64 = if let AsmConstant::String(text) = field {
                                    text.len() as u64
                                } else {
                                    0
                                };
                                emit_mov_imm64(asm, Reg::X17, len);
                                emit_store_pair_base(asm, Reg::X10, Reg::X16, Reg::X17, 0);
                            }
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
            }
            return Ok(());
        }
        if matches!(value, AsmValue::Constant(AsmConstant::Undef(_))) {
            match address {
                AsmValue::StackSlot(id) => {
                    let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                    if base != Reg::X31 {
                        return Err(Error::from("unsupported undef store into regfile slot"));
                    }
                    zero_sp_range(asm, dst_offset, size)?;
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    zero_reg_range(asm, Reg::X17, size)?;
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    zero_reg_range(asm, Reg::X17, size)?;
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
            }
            return Ok(());
        }
        if let AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) = value {
            let addend = indices.iter().map(|index| *index as i64).sum();
            emit_load_symbol_addr(asm, Reg::X16, name.as_str(), addend)?;
            match address {
                AsmValue::StackSlot(id) => {
                    let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                    if base != Reg::X31 {
                        return Err(Error::from("unsupported aggregate store into regfile slot"));
                    }
                    if size != 16 {
                        return Err(Error::from("unsupported global aggregate store size"));
                    }
                    emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    emit_store_to_sp(asm, Reg::X17, dst_offset);
                    add_immediate_offset(asm, Reg::X16, 8);
                    emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    emit_store_to_sp(asm, Reg::X17, dst_offset + 8);
                }
                AsmValue::Register(_) | AsmValue::Local(_) => {
                    let dst_offset = match address {
                        AsmValue::Register(id) => vreg_offset(layout, *id)?,
                        AsmValue::Local(id) => local_offset(layout, *id)?,
                        _ => return Err(Error::from("unsupported store address for aarch64")),
                    };
                    if size != 16 {
                        return Err(Error::from("unsupported global aggregate store size"));
                    }
                    emit_load_from_sp(asm, Reg::X9, dst_offset);
                    emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    emit_store_to_reg(asm, Reg::X17, Reg::X9);
                    add_immediate_offset(asm, Reg::X16, 8);
                    add_immediate_offset(asm, Reg::X9, 8);
                    emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    emit_store_to_reg(asm, Reg::X17, Reg::X9);
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
            }
            return Ok(());
        }
        let src_offset = match value {
            AsmValue::Register(id) => agg_offset(layout, *id)?,
            AsmValue::Local(id) => local_offset(layout, *id)?,
            _ => {
                return Err(Error::from(format!(
                    "unsupported aggregate store value: {:?}",
                    value
                )));
            }
        };
        match address {
            AsmValue::StackSlot(id) => {
                let (base, dst_offset) = stack_slot_base_and_offset(layout, *id)?;
                if base != Reg::X31 {
                    return Err(Error::from("unsupported aggregate store into regfile slot"));
                }
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                if let Some(offset) = asm.vreg_sp_offset(*id) {
                    asm.log_stack_write(offset, size, "store-agg-via-reg");
                }
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::X17, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                asm.log_stack_write(addr_offset, size, "store-agg-via-local");
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::X17, size)?;
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
        }
        return Ok(());
    }
    match address {
        AsmValue::StackSlot(id) => {
            let (base, offset) = stack_slot_base_and_offset(layout, *id)?;
            if is_freg_type(&value_ty, &layout.data_layout) {
                if base != Reg::X31 {
                    return Err(Error::from("unsupported float store into regfile slot"));
                }
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_sp(asm, FReg::V0, offset, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    AsmType::I1 | AsmType::I8 => {
                        if base != Reg::X31 {
                            let mask = if matches!(value_ty, AsmType::I1) {
                                1
                            } else {
                                0xff
                            };
                            emit_mov_imm16(asm, Reg::X17, mask);
                            emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                            emit_store_to_base(asm, Reg::X16, base, offset);
                            return Ok(());
                        }
                        emit_store8_to_sp(asm, Reg::X16, offset)?;
                    }
                    AsmType::I16 => {
                        if base != Reg::X31 {
                            emit_mov_imm64(asm, Reg::X17, 0xffff);
                            emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                            emit_store_to_base(asm, Reg::X16, base, offset);
                            return Ok(());
                        }
                        emit_store16_to_sp(asm, Reg::X16, offset)?;
                    }
                    AsmType::I32 => {
                        if base != Reg::X31 {
                            emit_mov_imm64(asm, Reg::X17, 0xffff_ffff);
                            emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                            emit_store_to_base(asm, Reg::X16, base, offset);
                            return Ok(());
                        }
                        emit_store32_to_sp(asm, Reg::X16, offset)?;
                    }
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_store_to_base(asm, Reg::X16, base, offset);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_base(asm, Reg::X16, base, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let store_size = size_of(&value_ty) as i32;
            if let Some(offset) = asm.vreg_sp_offset(*id) {
                asm.log_stack_write(offset, store_size, "store-via-reg");
            }
            let addr_offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X17, addr_offset);
            if is_freg_type(&value_ty, &layout.data_layout) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_reg(asm, FReg::V0, Reg::X17, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_store8_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I16 => emit_store16_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I32 => emit_store32_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let store_size = size_of(&value_ty) as i32;
            let addr_offset = local_offset(layout, *id)?;
            asm.log_stack_write(addr_offset, store_size, "store-via-local");
            emit_load_from_sp(asm, Reg::X17, addr_offset);
            if is_freg_type(&value_ty, &layout.data_layout) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_reg(asm, FReg::V0, Reg::X17, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_store8_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I16 => emit_store16_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I32 => emit_store32_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Global(name, _) => {
            emit_load_symbol_addr(asm, Reg::X17, name, 0)?;
            if is_freg_type(&value_ty, &layout.data_layout) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_reg(asm, FReg::V0, Reg::X17, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_store8_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I16 => emit_store16_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I32 => emit_store32_to_reg(asm, Reg::X16, Reg::X17),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for aarch64")),
    }
}
