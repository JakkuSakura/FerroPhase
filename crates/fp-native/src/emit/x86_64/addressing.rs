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

    load_value(asm, layout, ptr, Reg::R11, reg_types, local_types)?;
    for index in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                let idx = match index {
                    AsmValue::Constant(constant) => {
                        let raw = constant_to_i64(constant, &layout.data_layout)?;
                        usize::try_from(raw)
                            .map_err(|_| Error::from("GEP struct index out of range"))?
                    }
                    _ => return Err(Error::from("GEP struct index must be constant")),
                };
                let layout = struct_layout(&current_ty)
                    .ok_or_else(|| Error::from("missing struct layout for GEP"))?;
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
                add_immediate_offset(asm, Reg::R11, field_offset as i64)?;
                current_ty = fields
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
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

    store_vreg(asm, layout, dst_id, Reg::R11)?;
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
    load_value(asm, layout, index, Reg::R10, reg_types, local_types)?;
    if elem_size != 1 {
        emit_mov_imm64(asm, Reg::Rax, elem_size);
        emit_imul_rr(asm, Reg::R10, Reg::Rax);
    }
    emit_add_rr(asm, Reg::R11, Reg::R10);
    Ok(())
}

pub(super) fn add_immediate_offset(asm: &mut Assembler, base: Reg, offset: i64) -> Result<()> {
    if offset == 0 {
        return Ok(());
    }
    let imm = i32::try_from(offset)
        .map_err(|_| Error::from(format!("GEP offset too large for x86_64: {offset}")))?;
    emit_add_ri32(asm, base, imm);
    Ok(())
}
