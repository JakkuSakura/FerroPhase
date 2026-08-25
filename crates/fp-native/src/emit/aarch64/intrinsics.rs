use super::*;

pub(super) fn emit_intrinsic_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    kind: &AsmIntrinsicKind,
    format: &str,
    args: &[AsmValue],
    result_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
    target_format: TargetFormat,
) -> Result<()> {
    match kind {
        AsmIntrinsicKind::Print | AsmIntrinsicKind::Println => {}
        AsmIntrinsicKind::Format => {
            let fat_ptr_dst = if matches!(result_ty, AsmType::Ptr(_)) {
                None
            } else if is_fat_ptr_layout(result_ty) {
                Some(agg_offset(layout, dst_id)?)
            } else {
                return Err(Error::from("Format expects pointer or {ptr, len} result"));
            };
            let format_offset = intern_cstring(rodata, rodata_pool, format);
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
            let float_regs = [
                FReg::V0,
                FReg::V1,
                FReg::V2,
                FReg::V3,
                FReg::V4,
                FReg::V5,
                FReg::V6,
                FReg::V7,
            ];

            emit_mov_imm16(asm, Reg::X0, 0);
            emit_mov_imm16(asm, Reg::X1, 0);
            emit_load_rodata_addr(asm, Reg::X2, format_offset as i64)?;

            let mut int_idx = 3usize;
            let mut float_idx = 0usize;
            let mut stack_offset = 0i32;
            for arg in args {
                let arg_ty = value_type(arg, reg_types, local_types)?;
                if is_large_aggregate(&arg_ty, &layout.data_layout) {
                    let size = store_vararg_value(
                        asm,
                        layout,
                        stack_offset,
                        arg,
                        &arg_ty,
                        reg_types,
                        local_types,
                    )?;
                    stack_offset += size;
                    continue;
                }
                if let AsmValue::Constant(AsmConstant::String(text)) = arg {
                    let offset = intern_cstring(rodata, rodata_pool, text);
                    if int_idx < arg_regs.len() {
                        emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                        int_idx += 1;
                    }
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    emit_store_to_sp(asm, Reg::X16, stack_offset);
                    stack_offset += 8;
                    continue;
                }
                if is_float_type(&arg_ty) {
                    if float_idx < float_regs.len() {
                        load_value_float(
                            asm,
                            layout,
                            arg,
                            float_regs[float_idx],
                            &arg_ty,
                            reg_types,
                            local_types,
                        )?;
                        float_idx += 1;
                    }
                } else if int_idx < arg_regs.len() {
                    load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                    int_idx += 1;
                }
                let size = store_vararg_value(
                    asm,
                    layout,
                    stack_offset,
                    arg,
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                stack_offset += size;
            }

            asm.emit_bl_external("snprintf");

            // `X9`/`X16`/`X17` are caller-saved (and `X16`/`X17` doubly so,
            // as the linker's intra-call scratch registers for PLT stubs) —
            // none of them survive an external `bl`. Every value that needs
            // to outlive `malloc`/the second `snprintf` below is spilled to
            // this instruction's own stack slot (always reserved, 8 bytes,
            // unused by aggregates otherwise) and reloaded after.
            let scratch_offset = vreg_offset(layout, dst_id)?;
            emit_mov_reg(asm, Reg::X16, Reg::X0);
            if let Some(offset) = fat_ptr_dst {
                // Persist the raw (no-NUL) length directly into the
                // destination's own len field now — that memory is stable
                // across both remaining external calls.
                emit_store_to_sp(asm, Reg::X16, offset + 8);
            }
            emit_add_imm12(asm, Reg::X16, Reg::X16, 1);
            emit_store_to_sp(asm, Reg::X16, scratch_offset);
            emit_mov_reg(asm, Reg::X0, Reg::X16);
            asm.emit_bl_external("malloc");
            emit_mov_reg(asm, Reg::X9, Reg::X0);

            emit_mov_reg(asm, Reg::X0, Reg::X9);
            emit_load_from_sp(asm, Reg::X1, scratch_offset);
            // `scratch_offset`'s length value has now been consumed; reuse
            // the same slot to carry the buffer pointer across the second
            // `snprintf` call below.
            emit_store_to_sp(asm, Reg::X9, scratch_offset);
            emit_load_rodata_addr(asm, Reg::X2, format_offset as i64)?;

            int_idx = 3usize;
            float_idx = 0usize;
            stack_offset = 0i32;
            for arg in args {
                let arg_ty = value_type(arg, reg_types, local_types)?;
                if is_large_aggregate(&arg_ty, &layout.data_layout) {
                    let size = store_vararg_value(
                        asm,
                        layout,
                        stack_offset,
                        arg,
                        &arg_ty,
                        reg_types,
                        local_types,
                    )?;
                    stack_offset += size;
                    continue;
                }
                if let AsmValue::Constant(AsmConstant::String(text)) = arg {
                    let offset = intern_cstring(rodata, rodata_pool, text);
                    if int_idx < arg_regs.len() {
                        emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                        int_idx += 1;
                    }
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    emit_store_to_sp(asm, Reg::X16, stack_offset);
                    stack_offset += 8;
                    continue;
                }
                if is_float_type(&arg_ty) {
                    if float_idx < float_regs.len() {
                        load_value_float(
                            asm,
                            layout,
                            arg,
                            float_regs[float_idx],
                            &arg_ty,
                            reg_types,
                            local_types,
                        )?;
                        float_idx += 1;
                    }
                } else if int_idx < arg_regs.len() {
                    load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                    int_idx += 1;
                }
                let size = store_vararg_value(
                    asm,
                    layout,
                    stack_offset,
                    arg,
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                stack_offset += size;
            }

            asm.emit_bl_external("snprintf");
            emit_load_from_sp(asm, Reg::X9, scratch_offset);
            match fat_ptr_dst {
                Some(offset) => emit_store_to_sp(asm, Reg::X9, offset),
                None => {
                    emit_mov_reg(asm, Reg::X0, Reg::X9);
                    store_vreg(asm, layout, dst_id, Reg::X0)?;
                }
            }
            return Ok(());
        }
        AsmIntrinsicKind::TimeNow => {
            if !is_float_type(result_ty) {
                return Err(Error::from("TimeNow expects floating-point result"));
            }
            emit_mov_imm16(asm, Reg::X0, 0);
            asm.emit_bl_external("time");
            emit_scvtf(asm, FReg::V0, Reg::X0, result_ty, true);
            store_vreg_float(asm, layout, dst_id, FReg::V0, result_ty)?;
            return Ok(());
        }
        AsmIntrinsicKind::ProcMacroTokenStreamFromStr
        | AsmIntrinsicKind::ProcMacroTokenStreamToString => {
            return Err(Error::from(
                "proc-macro token stream parsing/printing is not supported by the aarch64 backend",
            ));
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    emit_load_rodata_addr(asm, Reg::X0, format_offset as i64)?;
    if abi_debug_enabled() {
        let sp_aligned = layout.frame_size % 16 == 0;
        abi_log(&format!(
            "call printf (varargs): sp_align16={} frame_size={} outgoing_size={} format=rodata+{}",
            sp_aligned, layout.frame_size, layout.outgoing_size, format_offset
        ));
        if !sp_aligned {
            abi_log("warning: SP is not 16-byte aligned at call boundary");
        }
        abi_log("  arg format -> x0");
    }

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
    let float_regs = [
        FReg::V0,
        FReg::V1,
        FReg::V2,
        FReg::V3,
        FReg::V4,
        FReg::V5,
        FReg::V6,
        FReg::V7,
    ];

    let mut int_idx = 1usize;
    let mut float_idx = 0usize;
    let mut stack_offset = 0i32;
    for arg in args {
        let arg_ty = value_type(arg, reg_types, local_types)?;
        if is_large_aggregate(&arg_ty, &layout.data_layout) {
            let size = store_vararg_value(
                asm,
                layout,
                stack_offset,
                arg,
                &arg_ty,
                reg_types,
                local_types,
            )?;
            if abi_debug_enabled() {
                abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
            }
            stack_offset += size;
            continue;
        }
        if let AsmValue::Constant(AsmConstant::String(text)) = arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            if int_idx < arg_regs.len() {
                emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                int_idx += 1;
            }
            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
            emit_store_to_sp(asm, Reg::X16, stack_offset);
            if abi_debug_enabled() {
                abi_log(&format!("  vararg string -> [sp+{}]", stack_offset));
            }
            stack_offset += 8;
            continue;
        }
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(
                    asm,
                    layout,
                    arg,
                    float_regs[float_idx],
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                float_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
            int_idx += 1;
        }
        let size = store_vararg_value(
            asm,
            layout,
            stack_offset,
            arg,
            &arg_ty,
            reg_types,
            local_types,
        )?;
        if abi_debug_enabled() {
            abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
        }
        stack_offset += size;
    }
    let stack_bytes = stack_offset;

    if abi_debug_enabled() {
        abi_log(&format!(
            "  stack_args={} bytes={} outgoing_cap={}",
            (stack_bytes / 8),
            stack_bytes,
            layout.outgoing_size
        ));
        if stack_bytes > layout.outgoing_size {
            abi_log("warning: outgoing stack arguments exceed reserved frame size");
        }
        if stack_bytes % 16 != 0 {
            abi_log("note: stack argument area size is not 16-byte aligned");
        }
    }

    if matches!(target_format, TargetFormat::Coff) {
        asm.emit_bl_external("printf");
    } else {
        asm.emit_bl_external("printf");
    }
    Ok(())
}
