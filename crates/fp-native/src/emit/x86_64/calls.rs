use super::*;

pub(super) fn call_abi(format: TargetFormat) -> (&'static [Reg], &'static [FReg], bool) {
    match format {
        TargetFormat::Coff => (&WIN_INT_ARGS, &WIN_FLOAT_ARGS, false),
        _ => (&SYSV_INT_ARGS, &SYSV_FLOAT_ARGS, true),
    }
}

pub(super) fn emit_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    function: &AsmValue,
    args: &[AsmValue],
    func_map: &HashMap<String, u32>,
    signatures: &HashMap<String, AsmFunctionSignature>,
    ret_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let target = match function {
        AsmValue::Function(name) => func_map
            .get(name)
            .copied()
            .map(CallTarget::Internal)
            .unwrap_or_else(|| CallTarget::External(name.clone())),
        AsmValue::Register(_) | AsmValue::Local(_) | AsmValue::StackSlot(_) => CallTarget::Indirect,
        _ => return Err(Error::from("unsupported callee for x86_64")),
    };

    let (arg_regs, float_regs, use_al) = call_abi(format);

    let effective_ret_ty = match function {
        AsmValue::Function(name) => signatures
            .get(name)
            .map(|signature| &signature.return_type)
            .unwrap_or(ret_ty),
        _ => ret_ty,
    };
    let return_mode = abi_pass_mode(effective_ret_ty, &layout.data_layout)?;
    let needs_sret = matches!(return_mode, AbiPassMode::Indirect);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let mut aggregate_scratch_cursor = 0i32;

    let mut sret_offset = None;
    if needs_sret {
        let agg_off = agg_offset(layout, dst_id)?;
        emit_mov_rr(asm, arg_regs[0], Reg::Rbp);
        emit_add_ri32(asm, arg_regs[0], agg_off);
        int_idx = 1;
        sret_offset = Some(agg_off);
    }

    for arg in args {
        if let AsmValue::Constant(AsmConstant::String(text)) = arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            if int_idx < arg_regs.len() {
                asm.emit_mov_imm64_reloc(arg_regs[int_idx], ".rodata", offset as i64);
                int_idx += 1;
            } else {
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                emit_mov_mr64_sp(asm, offset, Reg::R10);
                stack_idx += 1;
            }
            continue;
        }
        if push_aggregate_constant_arg(
            asm,
            layout,
            arg,
            &mut aggregate_scratch_cursor,
            &mut int_idx,
            &mut stack_idx,
            arg_regs,
            reg_types,
            local_types,
            rodata,
            rodata_pool,
        )? {
            continue;
        }
        let arg_ty = value_type(arg, reg_types, local_types)?;
        if matches!(
            abi_pass_mode(&arg_ty, &layout.data_layout)?,
            AbiPassMode::Pair
        ) {
            let source = match arg {
                AsmValue::Register(id) => {
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, vreg_offset(layout, *id)?);
                    Reg::R11
                }
                AsmValue::Local(id) => {
                    emit_mov_rr(asm, Reg::R11, Reg::Rbp);
                    emit_add_ri32(asm, Reg::R11, local_offset(layout, *id)?);
                    Reg::R11
                }
                _ => return Err(Error::from("pair ABI argument requires aggregate storage")),
            };
            emit_mov_rm64(asm, Reg::R10, source, 0);
            emit_mov_rm64(asm, Reg::R8, source, 8);
            push_reg_arg(
                asm,
                layout,
                Reg::R10,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            push_reg_arg(asm, layout, Reg::R8, &mut int_idx, &mut stack_idx, arg_regs)?;
            continue;
        }
        if matches!(arg_ty, AsmType::I128) {
            load_i128_value(asm, layout, arg, Reg::R10, Reg::R11, reg_types, local_types)?;
            push_reg_arg(
                asm,
                layout,
                Reg::R10,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            push_reg_arg(
                asm,
                layout,
                Reg::R11,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
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
            } else {
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
            int_idx += 1;
        } else {
            let offset = layout.shadow_space + (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
            stack_idx += 1;
        }
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    match target {
        CallTarget::Internal(target) => asm.emit_call(Label::Function(target)),
        CallTarget::External(symbol) => asm.emit_call_external(&symbol),
        CallTarget::Indirect => {
            load_value(asm, layout, function, Reg::R11, reg_types, local_types)?;
            asm.emit_call_reg(Reg::R11);
        }
    }

    if needs_sret {
        if let Some(agg_off) = sret_offset {
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, agg_off);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
    } else if matches!(return_mode, AbiPassMode::Pair) && is_aggregate_type(effective_ret_ty) {
        store_aggregate_pair(asm, layout, dst_id, Reg::Rax, Reg::Rdx)?;
    } else if matches!(ret_ty, AsmType::I128) {
        store_i128_value(asm, layout, dst_id, Reg::Rax, Reg::Rdx)?;
    } else if !matches!(ret_ty, AsmType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::Rax)?;
        }
    }

    Ok(())
}
