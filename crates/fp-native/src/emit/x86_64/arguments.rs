use super::*;

pub(super) fn push_stack_qword(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    src: Reg,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    emit_mov_mr64_sp(asm, offset, src);
    Ok(())
}

pub(super) fn push_int_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: i64,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        emit_mov_imm64(asm, arg_regs[*int_idx], value as u64);
        *int_idx += 1;
    } else {
        emit_mov_imm64(asm, Reg::R10, value as u64);
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, Reg::R10)?;
        *stack_idx += 1;
    }
    Ok(())
}

pub(super) fn push_rodata_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: u64,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        asm.emit_mov_imm64_reloc(arg_regs[*int_idx], ".rodata", offset as i64);
        *int_idx += 1;
    } else {
        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, Reg::R10)?;
        *stack_idx += 1;
    }
    Ok(())
}

pub(super) fn push_reg_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    reg: Reg,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        if arg_regs[*int_idx] != reg {
            emit_mov_rr(asm, arg_regs[*int_idx], reg);
        }
        *int_idx += 1;
    } else {
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, reg)?;
        *stack_idx += 1;
    }
    Ok(())
}

pub(super) fn push_aggregate_constant_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    arg: &AsmValue,
    scratch_cursor: &mut i32,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<bool> {
    let ty = value_type(arg, reg_types, local_types)?;
    if !is_aggregate_storage(&ty, &layout.data_layout)
        || !matches!(
            arg,
            AsmValue::Constant(AsmConstant::Struct(_, _) | AsmConstant::Array(_, _))
        )
    {
        return Ok(false);
    }
    let AsmValue::Constant(constant) = arg else {
        return Ok(false);
    };
    let size = layout
        .data_layout
        .size_of(&ty)
        .map_err(|error| Error::from(error.to_string()))? as i32;
    let storage_size = align8(size);
    let scratch_offset = layout
        .aggregate_scratch_offset
        .ok_or_else(|| Error::from("missing aggregate constant scratch storage"))?;
    if *scratch_cursor + storage_size > -scratch_offset {
        return Err(Error::from("aggregate constant scratch storage exhausted"));
    }
    emit_mov_rr(asm, Reg::R8, Reg::Rbp);
    emit_add_ri32(asm, Reg::R8, scratch_offset + *scratch_cursor);
    store_constant_aggregate_to_reg(
        asm,
        &layout.data_layout,
        Reg::R8,
        constant,
        &ty,
        rodata,
        rodata_pool,
    )?;
    push_reg_arg(asm, layout, Reg::R8, int_idx, stack_idx, arg_regs)?;
    *scratch_cursor += storage_size;
    Ok(true)
}

pub(super) fn push_value_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    arg: &AsmValue,
    int_idx: &mut usize,
    float_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
    float_regs: &[FReg],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
    aggregate_scratch_cursor: &mut i32,
) -> Result<()> {
    if let AsmValue::Constant(AsmConstant::String(text)) = arg {
        let offset = intern_cstring(rodata, rodata_pool, text);
        return push_rodata_arg(asm, layout, offset, int_idx, stack_idx, arg_regs);
    }
    if push_aggregate_constant_arg(
        asm,
        layout,
        arg,
        aggregate_scratch_cursor,
        int_idx,
        stack_idx,
        arg_regs,
        reg_types,
        local_types,
        rodata,
        rodata_pool,
    )? {
        return Ok(());
    }

    let arg_ty = value_type(arg, reg_types, local_types)?;
    if is_float_type(&arg_ty) {
        if *float_idx < float_regs.len() {
            load_value_float(
                asm,
                layout,
                arg,
                float_regs[*float_idx],
                &arg_ty,
                reg_types,
                local_types,
            )?;
            *float_idx += 1;
        } else {
            let offset = layout.shadow_space + (*stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
            *stack_idx += 1;
        }
    } else if *int_idx < arg_regs.len() {
        load_value(asm, layout, arg, arg_regs[*int_idx], reg_types, local_types)?;
        *int_idx += 1;
    } else {
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
        *stack_idx += 1;
    }

    Ok(())
}

pub(super) fn store_outgoing_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    let ty = value_type(value, reg_types, local_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &ty, reg_types, local_types)?;
        emit_movsd_m64x_sp(asm, offset, FReg::Xmm0, &ty);
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
        emit_mov_mr64_sp(asm, offset, Reg::R10);
    }
    Ok(())
}

pub(super) fn intern_cstring(
    rodata: &mut Vec<u8>,
    pool: &mut HashMap<String, u64>,
    text: &str,
) -> u64 {
    if let Some(offset) = pool.get(text) {
        return *offset;
    }
    align_rodata(rodata, 8);
    let offset = rodata.len() as u64;
    rodata.extend_from_slice(text.as_bytes());
    rodata.push(0);
    pool.insert(text.to_string(), offset);
    offset
}

pub(super) fn align_rodata(rodata: &mut Vec<u8>, align: usize) {
    while rodata.len() % align != 0 {
        rodata.push(0);
    }
}
