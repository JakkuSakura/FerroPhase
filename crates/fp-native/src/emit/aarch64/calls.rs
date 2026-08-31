use super::*;

pub(super) fn emit_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    function: &AsmValue,
    args: &[AsmValue],
    calling_convention: &CallingConvention,
    func_map: &HashMap<String, u32>,
    ret_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    if matches!(calling_convention, CallingConvention::FpLiftedX86_64RegFile) {
        // Preserve x86_64 SysV callee-saved registers across internal lifted calls.
        //
        // We cannot rely on every lifted callee to correctly preserve these yet,
        // especially while stack semantics are still being incrementally filled
        // out.
        const PRESERVED: &[(u32, i32)] = &[
            (3, 0),   // rbx
            (5, 8),   // rbp
            (12, 16), // r12
            (13, 24), // r13
            (14, 32), // r14
            (15, 40), // r15
        ];
        for (slot_id, scratch_off) in PRESERVED {
            if let Some(regfile_off) = layout.x86_regfile_offsets.get(slot_id).copied() {
                emit_load_from_base(asm, Reg::X16, Reg::X19, regfile_off);
                emit_store_to_sp(asm, Reg::X16, *scratch_off);
            }
        }

        // Emulate the x86_64 `call` / `ret` stack effect so the callee sees the
        // expected return-address slot at `[rsp]` and the correct 16-byte stack
        // alignment. The lifted callee does not currently pop the return
        // address on `ret`, so we restore `rsp` after returning.
        if let Some(rsp_offset) = layout.x86_regfile_offsets.get(&4).copied() {
            emit_load_from_base(asm, Reg::X16, Reg::X19, rsp_offset);
            add_immediate_offset(asm, Reg::X16, -8);
            emit_store_to_base(asm, Reg::X16, Reg::X19, rsp_offset);
            // Store the AArch64 return address as a best-effort stand-in.
            emit_store_to_base(asm, Reg::X30, Reg::X16, 0);
        }

        emit_mov_reg(asm, Reg::X0, Reg::X19);
        match function {
            AsmValue::Function(name) => {
                if let Some(id) = func_map.get(name).copied() {
                    asm.emit_bl(Label::Function(id));
                } else {
                    return Err(Error::from("lifted regfile calls must be internal"));
                }
            }
            _ => return Err(Error::from("lifted regfile calls require direct function")),
        }

        if let Some(rsp_offset) = layout.x86_regfile_offsets.get(&4).copied() {
            emit_load_from_base(asm, Reg::X16, Reg::X19, rsp_offset);
            add_immediate_offset(asm, Reg::X16, 8);
            emit_store_to_base(asm, Reg::X16, Reg::X19, rsp_offset);
        }

        for (slot_id, scratch_off) in PRESERVED {
            if let Some(regfile_off) = layout.x86_regfile_offsets.get(slot_id).copied() {
                emit_load_from_sp(asm, Reg::X16, *scratch_off);
                emit_store_to_base(asm, Reg::X16, Reg::X19, regfile_off);
            }
        }
        return Ok(());
    }

    let target = match function {
        AsmValue::Function(name) => func_map
            .get(name)
            .copied()
            .map(CallTarget::Internal)
            .unwrap_or_else(|| CallTarget::External(name.clone())),
        _ => CallTarget::Indirect,
    };

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

    let needs_sret = returns_aggregate(ret_ty, &layout.data_layout);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let mut stack_bytes = 0i32;

    let mut sret_offset = None;
    if needs_sret {
        let agg_off = agg_offset(layout, dst_id)?;
        emit_mov_reg(asm, arg_regs[0], Reg::X31);
        add_immediate_offset(asm, arg_regs[0], agg_off as i64);
        int_idx = 1;
        sret_offset = Some(agg_off);
    }

    let is_darwin_variadic = matches!(format, TargetFormat::MachO)
        && darwin_variadic_format_start(function, args).is_some();

    if abi_debug_enabled() {
        let name = match function {
            AsmValue::Function(name) => name.as_str(),
            _ => "<unknown>",
        };
        let sp_aligned = layout.frame_size % 16 == 0;
        abi_log(&format!(
            "call {}: sp_align16={} frame_size={} outgoing_size={} needs_sret={}",
            name, sp_aligned, layout.frame_size, layout.outgoing_size, needs_sret
        ));
        if !sp_aligned {
            abi_log("warning: SP is not 16-byte aligned at call boundary");
        }
    }

    if is_darwin_variadic {
        stack_bytes = emit_darwin_variadic_format_call(
            asm,
            layout,
            function,
            args,
            reg_types,
            local_types,
            rodata,
            rodata_pool,
        )?;
    } else {
        let mut const_agg_index = 0i32;
        for arg in args {
            if let AsmValue::Constant(AsmConstant::String(text)) = arg {
                let offset = intern_cstring(rodata, rodata_pool, text);
                if int_idx < arg_regs.len() {
                    emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                    if abi_debug_enabled() {
                        abi_log(&format!(
                            "  arg string -> {} (rodata+{})",
                            reg_name(arg_regs[int_idx]),
                            offset
                        ));
                    }
                    int_idx += 1;
                } else {
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    let offset = (stack_idx as i32) * 8;
                    emit_store_to_sp(asm, Reg::X16, offset);
                    if abi_debug_enabled() {
                        abi_log(&format!("  arg string -> [sp+{}]", offset));
                    }
                    stack_idx += 1;
                }
                continue;
            }
            let arg_ty = value_type(arg, reg_types, local_types)?;
            if matches!(arg_ty, AsmType::I128) {
                load_i128_value(asm, layout, arg, Reg::X16, Reg::X17, reg_types, local_types)?;
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
                if abi_debug_enabled() {
                    abi_log("  arg i128 -> pair");
                }
                continue;
            }
            // Every >8-byte aggregate value in this backend's calling
            // convention (see `load_value`'s `Register`/`Local` cases, and
            // the callee prologue's `copy_reg_to_sp`) is passed *by
            // address*: the argument register holds a pointer to memory
            // holding the aggregate's bytes, never the bytes themselves. A
            // bare aggregate constant (e.g. a `&str`'s `{ptr, len}` slice)
            // has no such backing memory of its own — materialize it into
            // this function's reserved scratch slot, then pass that slot's
            // address like any other large-aggregate argument.
            if let AsmValue::Constant(
                constant @ (AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)),
            ) = arg
            {
                if is_large_aggregate(&arg_ty, &layout.data_layout) {
                    let scratch_base = layout.const_agg_scratch_offset.ok_or_else(|| {
                        Error::from("missing scratch slot for constant aggregate argument")
                    })?;
                    let scratch_off =
                        scratch_base + const_agg_index * layout.const_agg_scratch_stride;
                    const_agg_index += 1;
                    // X17 (not X16/X9/X10) because `store_constant_aggregate_to_reg`
                    // uses those internally as scratch while materializing each
                    // field — passing one of them as `base` would alias and
                    // clobber the address mid-store (see its other call sites,
                    // which all pass X17 for the same reason).
                    emit_mov_reg(asm, Reg::X17, Reg::X31);
                    add_immediate_offset(asm, Reg::X17, scratch_off as i64);
                    store_constant_aggregate_to_reg(
                        asm,
                        &layout.data_layout,
                        Reg::X17,
                        constant,
                        &arg_ty,
                        rodata,
                        rodata_pool,
                    )?;
                    push_int_arg(
                        asm,
                        layout,
                        Reg::X17,
                        &mut int_idx,
                        &mut stack_idx,
                        &arg_regs,
                    )?;
                    if abi_debug_enabled() {
                        abi_log(&format!("  arg {:?} -> scratch+{}", arg_ty, scratch_off));
                    }
                    continue;
                }
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
                    if abi_debug_enabled() {
                        abi_log(&format!(
                            "  arg {:?} -> {}",
                            arg_ty,
                            freg_name(float_regs[float_idx])
                        ));
                    }
                    float_idx += 1;
                } else {
                    let offset = (stack_idx as i32) * 8;
                    store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                    if abi_debug_enabled() {
                        abi_log(&format!("  arg {:?} -> [sp+{}]", arg_ty, offset));
                    }
                    stack_idx += 1;
                }
            } else if int_idx < arg_regs.len() {
                load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                if abi_debug_enabled() {
                    abi_log(&format!(
                        "  arg {:?} -> {}",
                        arg_ty,
                        reg_name(arg_regs[int_idx])
                    ));
                }
                int_idx += 1;
            } else {
                let offset = (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                if abi_debug_enabled() {
                    abi_log(&format!("  arg {:?} -> [sp+{}]", arg_ty, offset));
                }
                stack_idx += 1;
            }
        }
    }

    if abi_debug_enabled() {
        if !is_darwin_variadic {
            stack_bytes = (stack_idx as i32) * 8;
        }
        abi_log(&format!(
            "  stack_args={} bytes={} outgoing_cap={}",
            stack_idx, stack_bytes, layout.outgoing_size
        ));
        if stack_bytes > layout.outgoing_size {
            abi_log("warning: outgoing stack arguments exceed reserved frame size");
        }
        if stack_bytes % 16 != 0 {
            abi_log("note: stack argument area size is not 16-byte aligned");
        }
    }

    match target {
        CallTarget::Internal(id) => asm.emit_bl(Label::Function(id)),
        CallTarget::External(name) => {
            if matches!(format, TargetFormat::Coff) {
                asm.emit_bl_external(&name);
            } else {
                asm.emit_bl_external(&name);
            }
        }
        CallTarget::Indirect => {
            load_value(asm, layout, function, Reg::X16, reg_types, local_types)?;
            emit_bl_reg(asm, Reg::X16);
        }
    }

    if needs_sret {
        if let Some(agg_off) = sret_offset {
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            add_immediate_offset(asm, Reg::X16, agg_off as i64);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
    } else if matches!(ret_ty, AsmType::I128) {
        store_i128_value(asm, layout, dst_id, Reg::X0, Reg::X1)?;
    } else if !matches!(ret_ty, AsmType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::V0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::X0)?;
        }
    }

    Ok(())
}
