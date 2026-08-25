use super::*;

pub(super) fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &AsmValue,
    ty: &AsmType,
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
    if matches!(ty, AsmType::I128) {
        match address {
            AsmValue::StackSlot(id) => {
                let offset = stack_slot_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset + 8);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                emit_mov_rm64(asm, Reg::R11, Reg::R11, 8);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                emit_mov_rm64(asm, Reg::R11, Reg::R11, 8);
            }
            _ => return Err(Error::from("unsupported load address for i128 on x86_64")),
        }
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    if is_aggregate_storage(ty, &layout.data_layout) {
        let size = size_of(ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let dst_offset = agg_offset(layout, dst_id)?;
        match address {
            AsmValue::StackSlot(id) => {
                let src_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_reg_to_sp(asm, Reg::R11, dst_offset, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_reg_to_sp(asm, Reg::R11, dst_offset, size)?;
            }
            _ => return Err(Error::from("unsupported load address for x86_64")),
        }
        emit_mov_rr(asm, Reg::R10, Reg::Rbp);
        emit_add_ri32(asm, Reg::R10, dst_offset);
        store_vreg(asm, layout, dst_id, Reg::R10)?;
        return Ok(());
    }
    match address {
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        _ => {
            let addr_ty = value_type(address, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported load address for x86_64: {:?}",
                addr_ty
            )))
        }
    }
}

pub(super) fn emit_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    want_rem: bool,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if is_float_type(&lhs_ty) {
        if want_rem {
            load_value_float(
                asm,
                layout,
                lhs,
                FReg::Xmm0,
                &lhs_ty,
                reg_types,
                local_types,
            )?;
            load_value_float(
                asm,
                layout,
                rhs,
                FReg::Xmm1,
                &lhs_ty,
                reg_types,
                local_types,
            )?;
            let symbol = if matches!(lhs_ty, AsmType::F32) {
                "fmodf"
            } else {
                "fmod"
            };
            asm.emit_call_external(symbol);
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &lhs_ty)?;
            return Ok(());
        }
        emit_float_div(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            &lhs_ty,
            reg_types,
            local_types,
        )?;
        return Ok(());
    }
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_divrem(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            want_rem,
            reg_types,
            local_types,
            format,
        );
    }

    load_value(asm, layout, lhs, Reg::Rax, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;

    emit_cqo(asm);
    emit_idiv_reg(asm, Reg::R11);

    let src = if want_rem { Reg::Rdx } else { Reg::Rax };
    store_vreg(asm, layout, dst_id, src)?;

    Ok(())
}

pub(super) enum CallTarget {
    Internal(u32),
    External(String),
    Indirect,
}

pub(super) const SYSV_INT_ARGS: [Reg; 6] =
    [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];
pub(super) const SYSV_FLOAT_ARGS: [FReg; 8] = [
    FReg::Xmm0,
    FReg::Xmm1,
    FReg::Xmm2,
    FReg::Xmm3,
    FReg::Xmm4,
    FReg::Xmm5,
    FReg::Xmm6,
    FReg::Xmm7,
];
pub(super) const WIN_INT_ARGS: [Reg; 4] = [Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9];
pub(super) const WIN_FLOAT_ARGS: [FReg; 4] = [FReg::Xmm0, FReg::Xmm1, FReg::Xmm2, FReg::Xmm3];

pub(super) const SYSCALL_ARGS: [Reg; 6] =
    [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::R10, Reg::R8, Reg::R9];

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
                let dst_offset = stack_slot_offset(layout, *id)?;
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
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
        if is_aggregate_storage(elem_ty, &layout.data_layout) {
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    for (idx, elem) in values.iter().enumerate() {
                        emit_mov_rr(asm, Reg::R8, Reg::Rbp);
                        emit_add_ri32(asm, Reg::R8, dst_offset + idx as i32 * elem_size);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::R8,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    for (idx, elem) in values.iter().enumerate() {
                        emit_mov_rr(asm, Reg::R8, Reg::R11);
                        emit_add_ri32(asm, Reg::R8, idx as i32 * elem_size);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::R8,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    for (idx, elem) in values.iter().enumerate() {
                        emit_mov_rr(asm, Reg::R8, Reg::R11);
                        emit_add_ri32(asm, Reg::R8, idx as i32 * elem_size);
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::R8,
                            elem,
                            elem_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        let store_elem = |asm: &mut Assembler, base: Reg, offset: i32| -> Result<()> {
            match elem_size {
                1 => emit_mov_mr8(asm, base, offset, Reg::R10),
                2 => emit_mov_mr16(asm, base, offset, Reg::R10),
                4 => emit_mov_mr32(asm, base, offset, Reg::R10),
                8 => emit_mov_mr64(asm, base, offset, Reg::R10),
                _ => {
                    return Err(Error::from(
                        "unsupported array element size in constant store",
                    ));
                }
            }
            Ok(())
        };
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                for (idx, elem) in values.iter().enumerate() {
                    let offset = dst_offset + (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::GlobalRef(name, _, indices) => {
                            let addend = indices.iter().map(|index| *index as i64).sum();
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), addend);
                        }
                        AsmConstant::FunctionRef(name, _) => {
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), 0);
                        }
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    store_elem(asm, Reg::Rbp, offset)?;
                }
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::GlobalRef(name, _, indices) => {
                            let addend = indices.iter().map(|index| *index as i64).sum();
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), addend);
                        }
                        AsmConstant::FunctionRef(name, _) => {
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), 0);
                        }
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    store_elem(asm, Reg::Rax, 0)?;
                }
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::GlobalRef(name, _, indices) => {
                            let addend = indices.iter().map(|index| *index as i64).sum();
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), addend);
                        }
                        AsmConstant::FunctionRef(name, _) => {
                            asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), 0);
                        }
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    store_elem(asm, Reg::Rax, 0)?;
                }
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if matches!(value, AsmValue::Constant(AsmConstant::Array(values, _)) if values.is_empty()) {
        return Ok(());
    }
    let value_ty = value_type(value, reg_types, local_types)?;
    if size_of(&value_ty) == 0 {
        return Ok(());
    }
    if matches!(value_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::R10,
            Reg::R11,
            reg_types,
            local_types,
        )?;
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
                emit_mov_mr64(asm, Reg::Rbp, dst_offset + 8, Reg::R11);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::Rcx, Reg::Rbp, addr_offset);
                emit_mov_mr64(asm, Reg::Rcx, 0, Reg::R10);
                emit_mov_mr64(asm, Reg::Rcx, 8, Reg::R11);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::Rcx, Reg::Rbp, addr_offset);
                emit_mov_mr64(asm, Reg::Rcx, 0, Reg::R10);
                emit_mov_mr64(asm, Reg::Rcx, 8, Reg::R11);
            }
            _ => return Err(Error::from("unsupported store address for i128 on x86_64")),
        }
        return Ok(());
    }
    if let AsmValue::Constant(constant) = value {
        if matches!(
            constant,
            AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)
        ) && matches!(
            abi_pass_mode(&value_ty, &layout.data_layout)?,
            AbiPassMode::Direct
        ) {
            let bits = pack_small_aggregate(constant, &value_ty, &layout.data_layout)?;
            emit_mov_imm64(asm, Reg::R10, bits);
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
    }
    if is_aggregate_storage(&value_ty, &layout.data_layout) {
        let size = size_of(&value_ty) as i32;
        if let AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) = value {
            let addend = indices.iter().map(|index| *index as i64).sum();
            emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), addend)?;
            store_aggregate_from_reg(asm, layout, Reg::R10, address, size)?;
            return Ok(());
        }
        if let AsmValue::Constant(AsmConstant::Struct(values, ty)) = value {
            let fields = match ty {
                AsmType::Struct { fields, .. } => fields,
                _ => return Err(Error::from("expected struct type for constant store")),
            };
            let struct_layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
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
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), 0)?;
                            }
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
                        let store_offset = dst_offset + field_offset as i32;
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10),
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
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
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
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), 0)?;
                            }
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
                        emit_mov_rr(asm, Reg::Rax, Reg::R11);
                        emit_add_ri32(asm, Reg::Rax, field_offset as i32);
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rax, 0, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rax, 0, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rax, 0, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10),
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
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
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
                            AsmConstant::GlobalRef(name, _, indices) => {
                                let addend = indices.iter().map(|index| *index as i64).sum();
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), addend)?;
                            }
                            AsmConstant::FunctionRef(name, _) => {
                                emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), 0)?;
                            }
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
                        emit_mov_rr(asm, Reg::Rax, Reg::R11);
                        emit_add_ri32(asm, Reg::Rax, field_offset as i32);
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rax, 0, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rax, 0, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rax, 0, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        if matches!(value, AsmValue::Constant(AsmConstant::Undef(_))) {
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    zero_sp_range(asm, dst_offset, size)?;
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
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
                let dst_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if is_float_type(&value_ty) {
        load_value_float(
            asm,
            layout,
            value,
            FReg::Xmm0,
            &value_ty,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    }

    match address {
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::Rbp, offset, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let addr_offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let addr_offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for x86_64")),
    }
}
