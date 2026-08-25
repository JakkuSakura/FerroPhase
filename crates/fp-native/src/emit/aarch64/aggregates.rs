use super::*;

pub(super) fn extract_value_type(ty: &AsmType, indices: &[u32]) -> Result<AsmType> {
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                current_ty = fields
                    .get(*idx as usize)
                    .cloned()
                    .ok_or_else(|| Error::from("ExtractValue field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                current_ty = *elem.clone();
            }
            _ => return Err(Error::from("ExtractValue expects aggregate type")),
        }
    }
    Ok(current_ty)
}

pub(super) fn aggregate_field_offset(
    ty: &AsmType,
    indices: &[u32],
    data_layout: &LirDataLayout,
) -> Result<(i64, AsmType)> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    let mut offset = 0i64;
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                let layout = struct_layout(&current_ty)
                    .ok_or_else(|| Error::from("missing struct layout for aggregate"))?;
                let field_offset = *layout
                    .field_offsets
                    .get(*idx as usize)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                offset += field_offset as i64;
                current_ty = fields
                    .get(*idx as usize)
                    .cloned()
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                let elem_size = size_of(elem) as i64;
                offset += elem_size * (*idx as i64);
                current_ty = *elem.clone();
            }
            _ => return Err(Error::from("unsupported aggregate type for indices")),
        }
    }
    Ok((offset, current_ty))
}

pub(super) fn copy_sp_to_sp(asm: &mut Assembler, src: i32, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_load_from_sp(asm, Reg::X16, src + offset);
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_load32u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_load16u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_load8u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

pub(super) fn copy_sp_to_reg(asm: &mut Assembler, src: i32, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_load_from_sp(asm, Reg::X16, src + offset);
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_store_to_reg(asm, Reg::X16, Reg::X9);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_load32u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_store32_to_reg(asm, Reg::X16, Reg::X9);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_load16u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_store16_to_reg(asm, Reg::X16, Reg::X9);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_load8u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_store8_to_reg(asm, Reg::X16, Reg::X9);
    }
    Ok(())
}

pub(super) fn copy_reg_to_sp(asm: &mut Assembler, src: Reg, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load_from_reg(asm, Reg::X16, Reg::X9);
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load32u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load16u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load8u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

#[allow(dead_code)]
pub(super) fn copy_reg_to_reg(asm: &mut Assembler, src: Reg, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store_to_reg(asm, Reg::X16, Reg::X17);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load32u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store32_to_reg(asm, Reg::X16, Reg::X17);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load16u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store16_to_reg(asm, Reg::X16, Reg::X17);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64);
        emit_load8u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store8_to_reg(asm, Reg::X16, Reg::X17);
    }
    Ok(())
}

pub(super) fn zero_sp_range(asm: &mut Assembler, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm16(asm, Reg::X16, 0);
    while offset + 8 <= size {
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

pub(super) fn zero_reg_range(asm: &mut Assembler, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm16(asm, Reg::X16, 0);
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store_to_reg(asm, Reg::X16, Reg::X17);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store32_to_reg(asm, Reg::X16, Reg::X17);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store16_to_reg(asm, Reg::X16, Reg::X17);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store8_to_reg(asm, Reg::X16, Reg::X17);
    }
    Ok(())
}

pub(super) fn store_constant_aggregate_to_reg(
    asm: &mut Assembler,
    data_layout: &LirDataLayout,
    base: Reg,
    constant: &AsmConstant,
    agg_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| data_layout.align_of(ty).expect("layout query failed");
    let struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    let size = size_of(agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    match constant {
        AsmConstant::Undef(_) | AsmConstant::Null(_) => return zero_reg_range(asm, base, size),
        AsmConstant::Int(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::UInt(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::Struct(values, _) => {
            let AsmType::Struct { fields, .. } = agg_ty else {
                return Err(Error::from("expected struct type for aggregate return"));
            };
            let layout = struct_layout(agg_ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate return"))?;
            for (idx, field) in values.iter().enumerate() {
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_ty = fields
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_size = size_of(field_ty);
                if matches!(field, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                    emit_mov_reg(asm, Reg::X10, base);
                    add_immediate_offset(asm, Reg::X10, field_offset as i64);
                    store_constant_aggregate_to_reg(
                        asm,
                        data_layout,
                        Reg::X10,
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
                emit_mov_reg(asm, Reg::X9, base);
                add_immediate_offset(asm, Reg::X9, field_offset as i64);
                match field_size {
                    1 => emit_store8_to_reg(asm, Reg::X16, Reg::X9),
                    2 => emit_store16_to_reg(asm, Reg::X16, Reg::X9),
                    4 => emit_store32_to_reg(asm, Reg::X16, Reg::X9),
                    8 => emit_store_to_reg(asm, Reg::X16, Reg::X9),
                    _ => {
                        return Err(Error::from("unsupported aggregate field size in return"));
                    }
                }
            }
            Ok(())
        }
        AsmConstant::Array(values, elem_ty) => {
            let elem_ty = match agg_ty {
                AsmType::Array(elem, _) => elem.as_ref(),
                _ => elem_ty,
            };
            let elem_size = size_of(elem_ty) as i32;
            if elem_size == 0 {
                return Ok(());
            }
            for (idx, elem) in values.iter().enumerate() {
                let offset = (idx as i32) * elem_size;
                if matches!(elem, AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)) {
                    emit_mov_reg(asm, Reg::X9, base);
                    add_immediate_offset(asm, Reg::X9, offset as i64);
                    store_constant_aggregate_to_reg(
                        asm,
                        data_layout,
                        Reg::X10,
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
                emit_mov_reg(asm, Reg::X9, base);
                add_immediate_offset(asm, Reg::X9, offset as i64);
                match elem_size {
                    1 => emit_store8_to_reg(asm, Reg::X16, Reg::X9),
                    2 => emit_store16_to_reg(asm, Reg::X16, Reg::X9),
                    4 => emit_store32_to_reg(asm, Reg::X16, Reg::X9),
                    8 => emit_store_to_reg(asm, Reg::X16, Reg::X9),
                    _ => {
                        return Err(Error::from("unsupported array element size in return"));
                    }
                }
            }
            Ok(())
        }
        // String-data GlobalRef used as &str return: emit {ptr, len=0}.
        AsmConstant::GlobalRef(name, _, indices) if matches!(agg_ty, AsmType::Struct { .. }) => {
            let layout = struct_layout(agg_ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate return"))?;
            let addend = indices.iter().map(|i| *i as i64).sum();
            // field 0: pointer
            emit_mov_reg(asm, Reg::X9, base);
            add_immediate_offset(asm, Reg::X9, layout.field_offsets[0] as i64);
            emit_load_symbol_addr(asm, Reg::X16, name.as_str(), addend)?;
            emit_store_to_reg(asm, Reg::X16, Reg::X9);
            // field 1: length (0)
            if layout.field_offsets.len() > 1 {
                emit_mov_reg(asm, Reg::X9, base);
                add_immediate_offset(asm, Reg::X9, layout.field_offsets[1] as i64);
                emit_mov_imm64(asm, Reg::X16, 0);
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
            }
            Ok(())
        }
        // Scalar constant (Bool/Int/etc) with a struct destination:
        // zero-fill the struct.  This can happen when a comptime-evaluated
        // const has a different type than the use-site expects.
        _ if matches!(agg_ty, AsmType::Struct { .. }) => {
            let sz = size_of(agg_ty) as i32;
            zero_reg_range(asm, base, sz)
        }
        _ => Err(Error::from(format!(
            "unsupported aggregate constant for return: constant={:?} ty={:?}",
            constant, agg_ty
        ))),
    }
}
