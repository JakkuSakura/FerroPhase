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
            let (arg_regs, float_regs, use_al) = call_abi(target_format);

            let mut int_idx = 0usize;
            let mut float_idx = 0usize;
            let mut stack_idx = 0usize;
            let mut aggregate_scratch_cursor = 0i32;

            push_int_arg(asm, layout, 0, &mut int_idx, &mut stack_idx, arg_regs)?;
            push_int_arg(asm, layout, 0, &mut int_idx, &mut stack_idx, arg_regs)?;
            push_rodata_arg(
                asm,
                layout,
                format_offset,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            for arg in args {
                push_value_arg(
                    asm,
                    layout,
                    arg,
                    &mut int_idx,
                    &mut float_idx,
                    &mut stack_idx,
                    arg_regs,
                    float_regs,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                    &mut aggregate_scratch_cursor,
                )?;
            }

            if use_al {
                emit_mov_al_imm8(asm, float_idx as u8);
            }

            asm.emit_call_external("snprintf");

            store_vreg(asm, layout, dst_id, Reg::Rax)?;
            if let Some(offset) = fat_ptr_dst {
                // Persist the raw (no-NUL) length directly into the
                // destination's own len field now — real stack memory,
                // stable across `malloc`/the second `snprintf` below
                // (unlike a register, which neither call preserves).
                emit_mov_mr64(asm, Reg::Rbp, offset + 8, Reg::Rax);
            }
            emit_mov_rr(asm, Reg::R10, Reg::Rax);
            emit_add_ri32(asm, Reg::R10, 1);
            if arg_regs[0] != Reg::R10 {
                emit_mov_rr(asm, arg_regs[0], Reg::R10);
            }
            asm.emit_call_external("malloc");

            let len_offset = vreg_offset(layout, dst_id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, len_offset);
            emit_add_ri32(asm, Reg::R10, 1);
            store_vreg(asm, layout, dst_id, Reg::Rax)?;

            int_idx = 0usize;
            float_idx = 0usize;
            stack_idx = 0usize;
            aggregate_scratch_cursor = 0;
            push_value_arg(
                asm,
                layout,
                &AsmValue::Register(dst_id),
                &mut int_idx,
                &mut float_idx,
                &mut stack_idx,
                arg_regs,
                float_regs,
                reg_types,
                local_types,
                rodata,
                rodata_pool,
                &mut aggregate_scratch_cursor,
            )?;
            push_reg_arg(
                asm,
                layout,
                Reg::R10,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            push_rodata_arg(
                asm,
                layout,
                format_offset,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            for arg in args {
                push_value_arg(
                    asm,
                    layout,
                    arg,
                    &mut int_idx,
                    &mut float_idx,
                    &mut stack_idx,
                    arg_regs,
                    float_regs,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                    &mut aggregate_scratch_cursor,
                )?;
            }

            if use_al {
                emit_mov_al_imm8(asm, float_idx as u8);
            }

            asm.emit_call_external("snprintf");
            if let Some(offset) = fat_ptr_dst {
                let ptr_offset = vreg_offset(layout, dst_id)?;
                emit_mov_rm64(asm, Reg::Rax, Reg::Rbp, ptr_offset);
                emit_mov_mr64(asm, Reg::Rbp, offset, Reg::Rax);
            }
            return Ok(());
        }
        AsmIntrinsicKind::TimeNow => {
            if !is_float_type(result_ty) {
                return Err(Error::from("TimeNow expects floating-point result"));
            }
            let (arg_regs, _, _) = call_abi(target_format);
            emit_mov_imm64(asm, arg_regs[0], 0);
            asm.emit_call_external("time");
            emit_cvtsi2sd(asm, FReg::Xmm0, Reg::Rax, result_ty);
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, result_ty)?;
            return Ok(());
        }
        AsmIntrinsicKind::ProcMacroTokenStreamFromStr
        | AsmIntrinsicKind::ProcMacroTokenStreamToString => {
            return Err(Error::from(
                "proc-macro token stream parsing/printing is not supported by the x86_64 backend",
            ));
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    let (arg_regs, float_regs, use_al) = call_abi(target_format);

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let mut aggregate_scratch_cursor = 0i32;

    push_rodata_arg(
        asm,
        layout,
        format_offset,
        &mut int_idx,
        &mut stack_idx,
        arg_regs,
    )?;
    for arg in args {
        push_value_arg(
            asm,
            layout,
            arg,
            &mut int_idx,
            &mut float_idx,
            &mut stack_idx,
            arg_regs,
            float_regs,
            reg_types,
            local_types,
            rodata,
            rodata_pool,
            &mut aggregate_scratch_cursor,
        )?;
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    asm.emit_call_external("printf");
    Ok(())
}
