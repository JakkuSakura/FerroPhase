use super::*;

pub(super) fn load_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    dst: Reg,
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
    let ty = value_type(value, reg_types, local_types)?;
    if matches!(ty, AsmType::Void) {
        emit_mov_imm16(asm, dst, 0);
        return Ok(());
    }
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
                emit_load_from_sp(asm, dst, offset);
                return Ok(());
            }
            if matches!(ty, AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                AsmType::I1 => emit_load8u_from_sp(asm, dst, offset)?,
                AsmType::I8 => emit_load8s_from_sp(asm, dst, offset)?,
                AsmType::I16 => emit_load16s_from_sp(asm, dst, offset)?,
                AsmType::I32 => emit_load32s_from_sp(asm, dst, offset)?,
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for aarch64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
                emit_mov_reg(asm, dst, Reg::X31);
                add_immediate_offset(asm, dst, offset as i64);
                return Ok(());
            }
            if matches!(ty, AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                AsmType::I1 => emit_load8u_from_sp(asm, dst, offset)?,
                AsmType::I8 => emit_load8s_from_sp(asm, dst, offset)?,
                AsmType::I16 => emit_load16s_from_sp(asm, dst, offset)?,
                AsmType::I32 => emit_load32s_from_sp(asm, dst, offset)?,
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for aarch64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        AsmValue::Constant(constant) => {
            if size_of(&ty) == 0 {
                emit_mov_imm16(asm, dst, 0);
                return Ok(());
            }
            if matches!(ty, AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            if matches!(
                constant,
                AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)
            ) && is_large_aggregate(&ty, &layout.data_layout)
            {
                let scratch = layout.const_agg_scratch_offset.ok_or_else(|| {
                    Error::from("missing scratch slot for constant aggregate load")
                })?;
                emit_mov_reg(asm, dst, Reg::X31);
                add_immediate_offset(asm, dst, scratch as i64);
                store_constant_aggregate_to_reg(
                    asm,
                    &layout.data_layout,
                    dst,
                    constant,
                    &ty,
                    &mut Vec::new(),
                    &mut HashMap::new(),
                )?;
                return Ok(());
            }
            if let AsmConstant::GlobalRef(name, _, indices) = constant {
                let addend = indices.iter().map(|idx| *idx as i64).sum();
                emit_load_symbol_addr(asm, dst, name.as_str(), addend)?;
                return Ok(());
            }
            let imm = constant_to_i64(constant, &layout.data_layout)?;
            if imm < 0 || imm > u16::MAX as i64 {
                emit_mov_imm64(asm, dst, imm as u64);
            } else {
                emit_mov_imm16(asm, dst, imm as u16);
            }
            Ok(())
        }
        AsmValue::Null(_) | AsmValue::Undef(_) => {
            emit_mov_imm16(asm, dst, 0);
            Ok(())
        }
        AsmValue::Global(name, _) => {
            emit_load_symbol_addr(asm, dst, name, 0)?;
            Ok(())
        }
        AsmValue::Function(name) => {
            emit_load_symbol_addr(asm, dst, name, 0)?;
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported value for aarch64: {:?}",
                ty
            )))
        }
    }
}

pub(super) fn i128_parts_from_const(constant: &AsmConstant) -> Result<(u64, u64)> {
    match constant {
        AsmConstant::Int(value, ty) if matches!(ty, AsmType::I128) => {
            let lo = *value as u64;
            let hi = if *value < 0 { u64::MAX } else { 0 };
            Ok((lo, hi))
        }
        AsmConstant::UInt(value, ty) if matches!(ty, AsmType::I128) => Ok((*value as u64, 0)),
        AsmConstant::Bool(value) => Ok((if *value { 1 } else { 0 }, 0)),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok((0, 0)),
        other => Err(Error::from(format!(
            "unsupported i128 constant: {:?}",
            other
        ))),
    }
}

pub(super) fn load_i128_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    lo: Reg,
    hi: Reg,
    _reg_types: &HashMap<u32, AsmType>,
    _local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, lo, offset);
            emit_load_from_sp(asm, hi, offset + 8);
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_from_sp(asm, lo, offset);
            emit_load_from_sp(asm, hi, offset + 8);
            Ok(())
        }
        AsmValue::StackSlot(id) => {
            let (base, offset) = stack_slot_base_and_offset(layout, *id)?;
            emit_load_from_base(asm, lo, base, offset);
            emit_load_from_base(asm, hi, base, offset + 8);
            Ok(())
        }
        AsmValue::Constant(constant) => {
            let (lo_val, hi_val) = i128_parts_from_const(constant)?;
            emit_mov_imm64(asm, lo, lo_val);
            emit_mov_imm64(asm, hi, hi_val);
            Ok(())
        }
        AsmValue::Null(_) | AsmValue::Undef(_) => {
            emit_mov_imm16(asm, lo, 0);
            emit_mov_imm16(asm, hi, 0);
            Ok(())
        }
        _ => Err(Error::from("unsupported i128 value")),
    }
}

pub(super) fn store_i128_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lo: Reg,
    hi: Reg,
) -> Result<()> {
    let offset = vreg_offset(layout, dst_id)?;
    emit_store_to_sp(asm, lo, offset);
    emit_store_to_sp(asm, hi, offset + 8);
    Ok(())
}

pub(super) fn emit_i128_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BinOp,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    match op {
        BinOp::Add => {
            load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
            emit_adds_reg(asm, Reg::X16, Reg::X16, Reg::X9);
            emit_adc_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        }
        BinOp::Sub => {
            load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
            emit_subs_reg(asm, Reg::X16, Reg::X16, Reg::X9);
            emit_sbc_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        }
        BinOp::Mul => {
            emit_i128_libcall(
                asm,
                layout,
                dst_id,
                "__multi3",
                lhs,
                Some(rhs),
                None,
                reg_types,
                local_types,
            )?;
        }
    }
    Ok(())
}

pub(super) fn emit_i128_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let symbol = match kind {
        ShiftKind::Left => "__ashlti3",
        ShiftKind::Right => "__lshrti3",
    };
    emit_i128_libcall(
        asm,
        layout,
        dst_id,
        symbol,
        lhs,
        None,
        Some(rhs),
        reg_types,
        local_types,
    )
}

pub(super) fn emit_i128_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    want_rem: bool,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let symbol = if want_rem { "__modti3" } else { "__divti3" };
    emit_i128_libcall(
        asm,
        layout,
        dst_id,
        symbol,
        lhs,
        Some(rhs),
        None,
        reg_types,
        local_types,
    )
}

pub(super) fn emit_i128_libcall(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    symbol: &str,
    lhs: &AsmValue,
    rhs: Option<&AsmValue>,
    shift: Option<&AsmValue>,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let mut int_idx = 0usize;
    let mut stack_idx = 0usize;

    load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
    push_int_arg(
        asm,
        layout,
        Reg::X16,
        &mut int_idx,
        &mut stack_idx,
        &arg_regs,
    )?;
    push_int_arg(
        asm,
        layout,
        Reg::X17,
        &mut int_idx,
        &mut stack_idx,
        &arg_regs,
    )?;

    if let Some(rhs) = rhs {
        load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
        push_int_arg(
            asm,
            layout,
            Reg::X9,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
        push_int_arg(
            asm,
            layout,
            Reg::X10,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
    }

    if let Some(shift) = shift {
        load_value(asm, layout, shift, Reg::X9, reg_types, local_types)?;
        push_int_arg(
            asm,
            layout,
            Reg::X9,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
    }

    asm.emit_bl_external(symbol);
    store_i128_value(asm, layout, dst_id, Reg::X0, Reg::X1)?;
    Ok(())
}

pub(super) fn push_int_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: Reg,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        emit_mov_reg(asm, arg_regs[*int_idx], value);
        *int_idx += 1;
    } else {
        let offset = (*stack_idx as i32) * 8;
        if offset + 8 > layout.outgoing_size {
            return Err(Error::from("outgoing arg offset out of range"));
        }
        emit_store_to_sp(asm, value, offset);
        *stack_idx += 1;
    }
    Ok(())
}

pub(super) fn load_value_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    dst: FReg,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let _size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
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
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_float_from_sp(asm, dst, offset, ty);
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_float_from_sp(asm, dst, offset, ty);
            Ok(())
        }
        AsmValue::Undef(_) | AsmValue::Null(_) => {
            emit_mov_imm16(asm, Reg::X16, 0);
            if matches!(ty, AsmType::F32) {
                emit_fmov_s_from_w(asm, dst, Reg::X16);
            } else {
                emit_fmov_d_from_x(asm, dst, Reg::X16);
            }
            Ok(())
        }
        AsmValue::Constant(AsmConstant::Float(value, _)) => {
            if matches!(ty, AsmType::F32) {
                let bits = (*value as f32).to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_fmov_s_from_w(asm, dst, Reg::X16);
            } else {
                let bits = value.to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 32) & 0xffff) as u16, 32);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 48) & 0xffff) as u16, 48);
                emit_fmov_d_from_x(asm, dst, Reg::X16);
            }
            Ok(())
        }
        AsmValue::Constant(AsmConstant::Null(_)) | AsmValue::Constant(AsmConstant::Undef(_)) => {
            emit_mov_imm16(asm, Reg::X16, 0);
            if matches!(ty, AsmType::F32) {
                emit_fmov_s_from_w(asm, dst, Reg::X16);
            } else {
                emit_fmov_d_from_x(asm, dst, Reg::X16);
            }
            Ok(())
        }
        _ => {
            let actual = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported float value for aarch64: {:?}",
                actual
            )))
        }
    }
}

pub(super) fn store_vreg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    id: u32,
    src: Reg,
) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_store_to_sp(asm, src, offset);
    Ok(())
}

pub(super) fn store_vreg_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    id: u32,
    src: FReg,
    ty: &AsmType,
) -> Result<()> {
    let _size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
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
    let offset = vreg_offset(layout, id)?;
    emit_store_float_to_sp(asm, src, offset, ty);
    Ok(())
}

pub(super) fn is_freg_type(ty: &AsmType, data_layout: &LirDataLayout) -> bool {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    is_float_type(ty) || matches!(ty, AsmType::Vector(_, _) if size_of(ty) == 16)
}
