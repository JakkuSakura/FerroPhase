use super::*;

pub(super) fn lift_non_terminator(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    bytes: &[u8],
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    last_compare: &mut Option<LastCompare>,
    syscall_convention: Option<AsmSyscallConvention>,
    jump_table_by_capture_offset: &std::collections::HashMap<u64, (u64, u8)>,
) -> Result<()> {
    if let Some((jmp_offset, index_reg)) = jump_table_by_capture_offset.get(&inst.offset) {
        // Capture the jump-table index *before* it is overwritten by the
        // `movsxd` load.
        if matches!(&inst.kind, Decoded::MovSxd { .. }) {
            if let Ok(value) = ctx.read_gpr(*index_reg) {
                ctx.pending_jump_table_index.insert(*jmp_offset, value);
            }
        }
    }

    if let Some(result) = simd_instructions::lift_simd_instructions(
        ctx,
        inst,
        bytes,
        relocs,
        instructions,
        next_id,
        last_compare,
        syscall_convention,
        jump_table_by_capture_offset,
    )? {
        return Ok(result);
    }

    match inst.kind {
        Decoded::Nop => {
            let nop_id = *next_id;
            instructions.push(AsmInstruction {
                id: nop_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Nop),
                kind: AsmInstructionKind::Nop,
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: vec![AsmAnnotation {
                    key: "fp.preserve.x86_64.nop_len".to_string(),
                    value: inst.len.to_string(),
                }],
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::Hlt => Err(Error::from(
            "internal error: unexpected hlt in non-terminator",
        )),
        Decoded::Leave => {
            let rbp = ctx.read_gpr(5)?;
            ctx.write_gpr(4, rbp.clone());

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: rbp.clone(),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile_store"),
            });
            *next_id += 1;

            let rsp_id = *next_id;
            instructions.push(build_binop(
                rsp_id,
                AsmInstructionKind::Add(rbp, AsmValue::Constant(AsmConstant::Int(8, AsmType::I64))),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(4, AsmValue::Register(rsp_id));
            ctx.write_gpr(5, AsmValue::Register(load_id));
            Ok(())
        }
        Decoded::PushReg { src } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = ctx.read_gpr(src)?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::PushImm { imm } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::CallRm { target } => {
            let mut call_is_external = false;
            let function = match target {
                RmOperand::Reg(reg) => ctx.read_gpr(reg)?,
                RmOperand::Mem(memory) => {
                    if let Some(displacement_offset) = memory.displacement_offset {
                        let relocation_offset = inst
                            .offset
                            .checked_add(displacement_offset as u64)
                            .ok_or_else(|| Error::from("x86_64 call relocation overflow"))?;
                        if let Some(reloc) = relocation_at(relocs, relocation_offset) {
                            call_is_external = relocation_is_external_call(reloc);
                            AsmValue::Function(reloc.symbol.clone())
                        } else {
                            if let Some(symbol) =
                                ctx.resolve_rip_symbol(&memory, inst.offset, inst.len)
                            {
                                if let Some(import) = symbol.import.as_ref() {
                                    call_is_external = true;
                                    AsmValue::Function(import.clone())
                                } else if symbol.kind == RipSymbolKind::Function {
                                    AsmValue::Function(symbol.name.clone())
                                } else {
                                    let addr = compute_address(
                                        ctx,
                                        memory,
                                        inst.offset,
                                        inst.len,
                                        relocs,
                                        instructions,
                                        next_id,
                                    )?;
                                    let load_id = *next_id;
                                    instructions.push(AsmInstruction {
                                        id: load_id,
                                        opcode: AsmOpcode::Generic(
                                            fp_core::asmir::AsmGenericOpcode::Load,
                                        ),
                                        kind: AsmInstructionKind::Load {
                                            address: addr,
                                            alignment: None,
                                            volatile: false,
                                        },
                                        ty: AsmType::I64,
                                        operands: Vec::new(),
                                        implicit_uses: Vec::new(),
                                        implicit_defs: Vec::new(),
                                        encoding: None,
                                        debug_info: None,
                                        annotations: Vec::new(),
                                    });
                                    *next_id += 1;
                                    AsmValue::Register(load_id)
                                }
                            } else if let Some(symbol) =
                                ctx.resolve_disp32_symbol(&memory, inst.offset, inst.len)
                            {
                                if let Some(import) = symbol.import.as_ref() {
                                    call_is_external = true;
                                    AsmValue::Function(import.clone())
                                } else if symbol.kind == RipSymbolKind::Function {
                                    AsmValue::Function(symbol.name.clone())
                                } else {
                                    let addr = compute_address(
                                        ctx,
                                        memory,
                                        inst.offset,
                                        inst.len,
                                        relocs,
                                        instructions,
                                        next_id,
                                    )?;
                                    let load_id = *next_id;
                                    instructions.push(AsmInstruction {
                                        id: load_id,
                                        opcode: AsmOpcode::Generic(
                                            fp_core::asmir::AsmGenericOpcode::Load,
                                        ),
                                        kind: AsmInstructionKind::Load {
                                            address: addr,
                                            alignment: None,
                                            volatile: false,
                                        },
                                        ty: AsmType::I64,
                                        operands: Vec::new(),
                                        implicit_uses: Vec::new(),
                                        implicit_defs: Vec::new(),
                                        encoding: None,
                                        debug_info: None,
                                        annotations: Vec::new(),
                                    });
                                    *next_id += 1;
                                    AsmValue::Register(load_id)
                                }
                            } else {
                                let addr = compute_address(
                                    ctx,
                                    memory,
                                    inst.offset,
                                    inst.len,
                                    relocs,
                                    instructions,
                                    next_id,
                                )?;
                                let load_id = *next_id;
                                instructions.push(AsmInstruction {
                                    id: load_id,
                                    opcode: AsmOpcode::Generic(
                                        fp_core::asmir::AsmGenericOpcode::Load,
                                    ),
                                    kind: AsmInstructionKind::Load {
                                        address: addr,
                                        alignment: None,
                                        volatile: false,
                                    },
                                    ty: AsmType::I64,
                                    operands: Vec::new(),
                                    implicit_uses: Vec::new(),
                                    implicit_defs: Vec::new(),
                                    encoding: None,
                                    debug_info: None,
                                    annotations: Vec::new(),
                                });
                                *next_id += 1;
                                AsmValue::Register(load_id)
                            }
                        }
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            relocs,
                            instructions,
                            next_id,
                        )?;
                        let load_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: load_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                            kind: AsmInstructionKind::Load {
                                address: addr,
                                alignment: None,
                                volatile: false,
                            },
                            ty: AsmType::I64,
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let call_return_model = if call_is_external {
                match &function {
                    AsmValue::Function(name) => external_call_return_model(name),
                    _ => None,
                }
            } else {
                None
            };

            let is_lifted_internal = ctx.use_lifted_regfile_calls
                && !call_is_external
                && matches!(&function, AsmValue::Function(_));

            if is_lifted_internal {
                ctx.end_block(instructions, next_id)?;
            }

            let args = if is_lifted_internal {
                Vec::new()
            } else {
                x86_64_sysv_call_args(ctx)?
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function,
                    args,
                    calling_convention: if is_lifted_internal {
                        CallingConvention::FpLiftedX86_64RegFile
                    } else {
                        CallingConvention::X86_64SysV
                    },
                    tail_call: false,
                },
                ty: if is_lifted_internal {
                    AsmType::Void
                } else if call_is_external {
                    AsmType::I64
                } else {
                    AsmType::I64
                },
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            if is_lifted_internal {
                ctx.begin_block(instructions, next_id)?;
            } else if call_is_external {
                if let Some(model) = call_return_model {
                    match model {
                        ExternalCallReturnModel::I64 => {
                            ctx.write_gpr(0, AsmValue::Register(id));
                        }
                        ExternalCallReturnModel::I32 => {
                            write_gpr_with_width(
                                ctx,
                                0,
                                AsmValue::Register(id),
                                32,
                                instructions,
                                next_id,
                            )?;
                        }
                    }
                } else {
                    ctx.write_gpr(0, AsmValue::Register(id));
                }
            } else {
                ctx.write_gpr(0, AsmValue::Register(id));
            }
            Ok(())
        }
        Decoded::PushRm { src } => {
            let rsp = ctx.read_gpr(4)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Sub(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            let new_rsp = AsmValue::Register(new_rsp_id);
            ctx.write_gpr(4, new_rsp.clone());

            let value = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: new_rsp,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::PopReg { dst } => {
            let rsp = ctx.read_gpr(4)?;
            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: rsp.clone(),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(load_id));

            let rhs = AsmValue::Constant(AsmConstant::Int(8, AsmType::I64));
            let new_rsp_id = *next_id;
            instructions.push(build_binop(
                new_rsp_id,
                AsmInstructionKind::Add(rsp, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(4, AsmValue::Register(new_rsp_id));
            Ok(())
        }
        Decoded::XorReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Xor(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )?;
            let value = ctx.read_gpr(dst)?;
            let mask_bits = match width_bits {
                64 => None,
                32 => Some(0xFFFF_FFFFu64),
                16 => Some(0xFFFFu64),
                8 => Some(0xFFu64),
                _ => None,
            };
            let masked = if let Some(mask_bits) = mask_bits {
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let masked_id = *next_id;
                instructions.push(build_binop(
                    masked_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                AsmValue::Register(masked_id)
            } else {
                value
            };

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    masked,
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::XorImm {
            dst,
            imm,
            width_bits,
        } => {
            lift_rm_imm_binop(
                ctx,
                dst,
                imm,
                width_bits,
                *inst,
                bytes,
                relocs,
                instructions,
                next_id,
                fp_core::asmir::AsmGenericOpcode::Xor,
            )?;

            if let RmOperand::Reg(reg) = dst {
                let value = ctx.read_gpr(reg)?;
                let mask_bits = match width_bits {
                    64 => None,
                    32 => Some(0xFFFF_FFFFu64),
                    16 => Some(0xFFFFu64),
                    8 => Some(0xFFu64),
                    _ => None,
                };
                let masked = if let Some(mask_bits) = mask_bits {
                    let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                    let masked_id = *next_id;
                    instructions.push(build_binop(
                        masked_id,
                        AsmInstructionKind::And(value, mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    AsmValue::Register(masked_id)
                } else {
                    value
                };

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Eq(
                        masked,
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ),
                    fp_core::asmir::AsmGenericOpcode::Eq,
                ));
                *next_id += 1;
                *last_compare = Some(LastCompare {
                    id: cmp_id,
                    index: instructions.len() - 1,
                    is_float: false,
                });
            }

            Ok(())
        }
        Decoded::OrImm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Or,
        ),
        Decoded::AndReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::And(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::AndRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::And(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::OrReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Or(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::OrRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Or(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::OrRmReg {
            dst,
            src,
            width_bits,
        }
        | Decoded::AndRmReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            let (kind, opcode) = match inst.kind {
                Decoded::OrRmReg { .. } => (
                    AsmInstructionKind::Or(lhs, rhs),
                    fp_core::asmir::AsmGenericOpcode::Or,
                ),
                _ => (
                    AsmInstructionKind::And(lhs, rhs),
                    fp_core::asmir::AsmGenericOpcode::And,
                ),
            };
            instructions.push(build_binop(id, kind, AsmOpcode::Generic(opcode)));
            *next_id += 1;

            match dst {
                RmOperand::Reg(dst_reg) => write_gpr_with_width(
                    ctx,
                    dst_reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::XorRmToReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Xor(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::AndImm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::And,
        ),
        Decoded::AdcImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::SbbImm {
            dst,
            imm,
            width_bits,
        } => {
            let compare = last_compare
                .as_ref()
                .ok_or_else(|| Error::from("adc/sbb without comparison"))?;
            // Carry flag corresponds to unsigned borrow from the last compare.
            // Force the compare to be treated as `ult`.
            patch_compare_kind(instructions, compare, 0x2)?;

            let carry_id = *next_id;
            instructions.push(AsmInstruction {
                id: carry_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            let carry = AsmValue::Register(carry_id);

            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let is_sbb = matches!(inst.kind, Decoded::SbbImm { .. });

            let (addr, current_value) = match dst {
                RmOperand::Reg(reg) => (None, ctx.read_gpr(reg)?),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let load_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: load_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: addr.clone(),
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    (Some(addr), AsmValue::Register(load_id))
                }
            };

            let id1 = *next_id;
            let first_kind = if is_sbb {
                AsmInstructionKind::Sub(current_value.clone(), rhs)
            } else {
                AsmInstructionKind::Add(current_value.clone(), rhs)
            };
            let first_op = if is_sbb {
                fp_core::asmir::AsmGenericOpcode::Sub
            } else {
                fp_core::asmir::AsmGenericOpcode::Add
            };
            instructions.push(build_binop(
                id1,
                first_kind,
                AsmOpcode::Generic(first_op.clone()),
            ));
            *next_id += 1;

            let id2 = *next_id;
            let second_kind = if is_sbb {
                AsmInstructionKind::Sub(AsmValue::Register(id1), carry)
            } else {
                AsmInstructionKind::Add(AsmValue::Register(id1), carry)
            };
            instructions.push(build_binop(id2, second_kind, AsmOpcode::Generic(first_op)));
            *next_id += 1;

            let mut value = AsmValue::Register(id2);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }

            match (dst, addr) {
                (RmOperand::Reg(reg), _) => {
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                (RmOperand::Mem(_), Some(addr)) => {
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
                _ => Err(Error::from("internal error: adc/sbb address missing")),
            }
        }
        Decoded::MovImm32ToRm {
            dst,
            imm_offset: _,
            imm,
        } => {
            let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I32));
            match dst {
                RmOperand::Reg(dst) => {
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    // x86_64 zero-extends 32-bit register writes.
                    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let and_id = *next_id;
                    instructions.push(build_binop(
                        and_id,
                        AsmInstructionKind::And(AsmValue::Register(id), mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(and_id));
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::MovImm32ToMem64 {
            dst,
            imm_offset: _,
            imm,
        } => {
            if dst.segment.is_some() {
                return Ok(());
            }
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I64));
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::MovImm8ToRm { dst, imm } => match dst {
            RmOperand::Reg(dst) => {
                let value = AsmValue::Constant(AsmConstant::UInt(imm as u8 as u64, AsmType::I64));
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(value),
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.write_gpr(dst, AsmValue::Register(id));
                Ok(())
            }
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    return Ok(());
                }
                let value = AsmValue::Constant(AsmConstant::Int(imm as i64, AsmType::I8));
                let addr = compute_address(
                    ctx,
                    memory,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value,
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: AsmType::Void,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                Ok(())
            }
        },
        Decoded::MovImm16ToRm {
            dst,
            imm_offset: _,
            imm,
        } => match dst {
            RmOperand::Reg(dst) => {
                let current = ctx.read_gpr(dst)?;
                let mask =
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF_FFFF_0000, AsmType::I64));
                let masked_id = *next_id;
                instructions.push(build_binop(
                    masked_id,
                    AsmInstructionKind::And(current, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;

                let imm_val = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I64));
                let merged_id = *next_id;
                instructions.push(build_binop(
                    merged_id,
                    AsmInstructionKind::Or(AsmValue::Register(masked_id), imm_val),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                ));
                *next_id += 1;
                ctx.write_gpr(dst, AsmValue::Register(merged_id));
                Ok(())
            }
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    return Ok(());
                }
                let value = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I16));
                let addr = compute_address(
                    ctx,
                    memory,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value,
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: AsmType::Void,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                Ok(())
            }
        },
        Decoded::AddRegRm { dst, src } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Add(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::AddRmReg {
            dst,
            src,
            width_bits,
        } => {
            let rhs = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    let lhs = ctx.read_gpr(dst)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs.clone(), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(dst, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let load_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: load_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: addr.clone(),
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(AsmValue::Register(load_id), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::SubRegRm { dst, src } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs.clone(), rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::SubRegRmWidth {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::SubRmReg {
            dst,
            src,
            width_bits,
        } => {
            let rhs = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    let lhs = ctx.read_gpr(dst)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Sub(lhs.clone(), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(dst, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let load_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: load_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: addr.clone(),
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Sub(AsmValue::Register(load_id), rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::AddImm {
            dst,
            imm,
            width_bits,
        } => {
            let opcode = x86_opcode_after_prefixes(bytes, inst)?;
            let imm_width_bits = match opcode {
                0x83 => 8u16,
                0x81 => 32u16,
                0x05 => 32u16,
                _ => {
                    return Err(Error::from(
                        "x86_64 add imm preservation requires 81/83/05 encoding",
                    ));
                }
            };
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            let mut inst = build_binop(
                id,
                AsmInstructionKind::Add(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            );
            inst.annotations.extend([
                AsmAnnotation {
                    key: "fp.preserve.x86_64.dst_gpr".to_string(),
                    value: dst.to_string(),
                },
                AsmAnnotation {
                    key: "fp.preserve.x86_64.imm_width_bits".to_string(),
                    value: imm_width_bits.to_string(),
                },
            ]);
            instructions.push(inst);
            *next_id += 1;
            let mut value = AsmValue::Register(id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                let mut inst = build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                );
                inst.annotations = synthesized_annotations("x86.zeroext32");
                instructions.push(inst);
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, value);
            Ok(())
        }
        Decoded::AddImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Add,
        ),
        Decoded::SubImm {
            dst,
            imm,
            width_bits,
        } => {
            let opcode = x86_opcode_after_prefixes(bytes, inst)?;
            let imm_width_bits = match opcode {
                0x83 => 8u16,
                0x81 => 32u16,
                0x2D => 32u16,
                _ => {
                    return Err(Error::from(
                        "x86_64 sub imm preservation requires 81/83/2D encoding",
                    ));
                }
            };
            let lhs = ctx.read_gpr(dst)?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            let mut inst = build_binop(
                id,
                AsmInstructionKind::Sub(lhs.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            );
            inst.annotations.extend([
                AsmAnnotation {
                    key: "fp.preserve.x86_64.dst_gpr".to_string(),
                    value: dst.to_string(),
                },
                AsmAnnotation {
                    key: "fp.preserve.x86_64.imm_width_bits".to_string(),
                    value: imm_width_bits.to_string(),
                },
            ]);
            instructions.push(inst);
            *next_id += 1;
            let mut value = AsmValue::Register(id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                let mut inst = build_binop(
                    and_id,
                    AsmInstructionKind::And(value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                );
                inst.annotations = synthesized_annotations("x86.zeroext32");
                instructions.push(inst);
                *next_id += 1;
                value = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, value);
            Ok(())
        }
        Decoded::SubImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Sub,
        ),
        Decoded::Cmp {
            lhs,
            rhs,
            width_bits,
        } => {
            let lhs_value = value_from_operand_with_width(
                ctx,
                lhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_value = value_from_operand_with_width(
                ctx,
                rhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let (lhs_value, rhs_value) = if width_bits != 64 {
                let mask_bits = if width_bits == 32 { 0xFFFF_FFFF } else { 0xFF };
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let lhs_id = *next_id;
                instructions.push(build_binop(
                    lhs_id,
                    AsmInstructionKind::And(lhs_value, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                let rhs_id = *next_id;
                instructions.push(build_binop(
                    rhs_id,
                    AsmInstructionKind::And(rhs_value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                (AsmValue::Register(lhs_id), AsmValue::Register(rhs_id))
            } else {
                (lhs_value, rhs_value)
            };
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(lhs_value, rhs_value),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::Test {
            lhs,
            rhs,
            width_bits,
        } => {
            let lhs_value = value_from_operand_with_width(
                ctx,
                lhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs_value = value_from_operand_with_width(
                ctx,
                rhs,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let (lhs_value, rhs_value) = if width_bits != 64 {
                let mask_bits = if width_bits == 32 { 0xFFFF_FFFF } else { 0xFF };
                let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
                let lhs_id = *next_id;
                instructions.push(build_binop(
                    lhs_id,
                    AsmInstructionKind::And(lhs_value, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                let rhs_id = *next_id;
                instructions.push(build_binop(
                    rhs_id,
                    AsmInstructionKind::And(rhs_value, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                (AsmValue::Register(lhs_id), AsmValue::Register(rhs_id))
            } else {
                (lhs_value, rhs_value)
            };
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    AsmValue::Register(and_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::VptestMem { lhs, rhs } => {
            if rhs.segment.is_some() {
                return Ok(());
            }

            let lhs_vec = ctx.read_vec(lhs)?;

            let lhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec.clone(),
                    lane: 0,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            let lhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
                    lane: 1,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr = compute_address(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr.clone(),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let rhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(addr1_id),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let and0_id = *next_id;
            instructions.push(build_binop(
                and0_id,
                AsmInstructionKind::And(AsmValue::Register(lhs0_id), AsmValue::Register(rhs0_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            let and1_id = *next_id;
            instructions.push(build_binop(
                and1_id,
                AsmInstructionKind::And(AsmValue::Register(lhs1_id), AsmValue::Register(rhs1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(and0_id), AsmValue::Register(and1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(
                    AsmValue::Register(or_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtReg { value, bit } => {
            let lhs_value = ctx.read_gpr(value)?;
            let rhs_value = ctx.read_gpr(bit)?;
            let shift_id = *next_id;
            instructions.push(build_binop(
                shift_id,
                AsmInstructionKind::Shr(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let mask = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(AsmValue::Register(shift_id), mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            // `bt` writes the carry flag; represent that as a comparison between
            // `0` and the tested bit, which allows `jc/jnc` to patch the predicate
            // into `ult/uge`.
            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtImm { value, imm } => {
            let lhs_value = ctx.read_gpr(value)?;
            let rhs_value = AsmValue::Constant(AsmConstant::UInt(imm as u64, AsmType::I64));
            let shift_id = *next_id;
            instructions.push(build_binop(
                shift_id,
                AsmInstructionKind::Shr(lhs_value, rhs_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let mask = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(AsmValue::Register(shift_id), mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(())
        }
        Decoded::BtcImm {
            dst,
            imm,
            width_bits,
        } => {
            let value = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let mask_value =
                AsmValue::Constant(AsmConstant::UInt(1u64 << (imm as u64), AsmType::I64));

            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value.clone(), mask_value.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let zero = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Eq(zero, AsmValue::Register(and_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });

            let xor_id = *next_id;
            instructions.push(build_binop(
                xor_id,
                AsmInstructionKind::Xor(value, mask_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            match dst {
                RmOperand::Reg(reg) => write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(xor_id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored = value_for_store(
                        width_bits,
                        AsmValue::Register(xor_id),
                        instructions,
                        next_id,
                    )?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::Cqo => {
            let rax = ctx.read_gpr(0)?;
            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Lt(rax, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))),
                fp_core::asmir::AsmGenericOpcode::Lt,
            ));
            *next_id += 1;

            let select_id = *next_id;
            instructions.push(AsmInstruction {
                id: select_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(2, AsmValue::Register(select_id));
            Ok(())
        }
        Decoded::Cdq => {
            let rax = ctx.read_gpr(0)?;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(rax, AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let cmp_id = *next_id;
            instructions.push(compare_instruction(
                cmp_id,
                AsmInstructionKind::Lt(
                    AsmValue::Register(sext_id),
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                fp_core::asmir::AsmGenericOpcode::Lt,
            ));
            *next_id += 1;

            let select_id = *next_id;
            instructions.push(AsmInstruction {
                id: select_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(cmp_id),
                    if_true: AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                    if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(2, AsmValue::Register(select_id));
            Ok(())
        }
        Decoded::Cdqe => {
            let value = ctx.read_gpr(0)?;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(value, AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(0, AsmValue::Register(sext_id));
            Ok(())
        }
        Decoded::ShlImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::ShrImm {
            dst,
            imm,
            width_bits,
        }
        | Decoded::SarImm {
            dst,
            imm,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let id = if matches!(inst.kind, Decoded::SarImm { .. }) {
                let sign_shift = match width_bits {
                    64 => 63u8,
                    32 => 31u8,
                    _ => return Err(Error::from("unsupported x86_64 sar width")),
                };
                if imm == 0 {
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(
                            lhs,
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    id
                } else {
                    let shift = AsmValue::Constant(AsmConstant::Int(i64::from(imm), AsmType::I64));
                    let logical_id = *next_id;
                    instructions.push(build_binop(
                        logical_id,
                        AsmInstructionKind::Shr(lhs.clone(), shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let sign_id = *next_id;
                    instructions.push(build_binop(
                        sign_id,
                        AsmInstructionKind::Shr(
                            lhs,
                            AsmValue::Constant(AsmConstant::Int(
                                i64::from(sign_shift),
                                AsmType::I64,
                            )),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let neg_id = *next_id;
                    instructions.push(build_binop(
                        neg_id,
                        AsmInstructionKind::Sub(
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            AsmValue::Register(sign_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                    ));
                    *next_id += 1;

                    let fill_shift = width_bits
                        .try_into()
                        .ok()
                        .and_then(|bits: u8| bits.checked_sub(imm))
                        .ok_or_else(|| Error::from("unsupported x86_64 sar shift"))?;
                    let fill_id = *next_id;
                    instructions.push(build_binop(
                        fill_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(neg_id),
                            AsmValue::Constant(AsmConstant::Int(
                                i64::from(fill_shift),
                                AsmType::I64,
                            )),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let result_id = *next_id;
                    instructions.push(build_binop(
                        result_id,
                        AsmInstructionKind::Or(
                            AsmValue::Register(logical_id),
                            AsmValue::Register(fill_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                    ));
                    *next_id += 1;
                    result_id
                }
            } else {
                let rhs = AsmValue::Constant(AsmConstant::Int(i64::from(imm), AsmType::I64));
                let id = *next_id;
                let (kind, opcode) = match inst.kind {
                    Decoded::ShlImm { .. } => (
                        AsmInstructionKind::Shl(lhs, rhs),
                        fp_core::asmir::AsmGenericOpcode::Shl,
                    ),
                    Decoded::ShrImm { .. } => (
                        AsmInstructionKind::Shr(lhs, rhs),
                        fp_core::asmir::AsmGenericOpcode::Shr,
                    ),
                    _ => return Err(Error::from("internal error: expected shift kind")),
                };
                instructions.push(build_binop(id, kind, AsmOpcode::Generic(opcode)));
                *next_id += 1;
                id
            };

            match dst {
                RmOperand::Reg(dst_reg) => write_gpr_with_width(
                    ctx,
                    dst_reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::Shrx {
            dst,
            src,
            shift,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = value_from_rm_with_width(
                ctx,
                shift,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = freeze_i64(rhs, instructions, next_id);

            let mask = if width_bits == 64 { 0x3F } else { 0x1F };
            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    rhs,
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Shr(lhs, AsmValue::Register(rhs_mask_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::Shlx {
            dst,
            src,
            shift,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = value_from_rm_with_width(
                ctx,
                shift,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = freeze_i64(rhs, instructions, next_id);

            let mask = if width_bits == 64 { 0x3F } else { 0x1F };
            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    rhs,
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Shl(lhs, AsmValue::Register(rhs_mask_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::Rorx {
            dst,
            src,
            imm,
            width_bits,
        } => {
            let width = width_bits as u64;
            let imm = (imm as u64) % width;
            if imm == 0 {
                let value = value_from_rm_with_width(
                    ctx,
                    src,
                    width_bits,
                    *inst,
                    relocs,
                    instructions,
                    next_id,
                )?;
                return write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id);
            }

            let value = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let imm_value = AsmValue::Constant(AsmConstant::UInt(imm, AsmType::I64));
            let inv_value = AsmValue::Constant(AsmConstant::UInt(width - imm, AsmType::I64));

            let shr_id = *next_id;
            instructions.push(build_binop(
                shr_id,
                AsmInstructionKind::Shr(value.clone(), imm_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
            ));
            *next_id += 1;

            let shl_id = *next_id;
            instructions.push(build_binop(
                shl_id,
                AsmInstructionKind::Shl(value, inv_value),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let or_id = *next_id;
            instructions.push(build_binop(
                or_id,
                AsmInstructionKind::Or(AsmValue::Register(shr_id), AsmValue::Register(shl_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(or_id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::Blsr {
            dst,
            src,
            width_bits,
        } => {
            let value = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let sub_id = *next_id;
            instructions.push(build_binop(
                sub_id,
                AsmInstructionKind::Sub(
                    value.clone(),
                    AsmValue::Constant(AsmConstant::Int(1, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;

            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value, AsmValue::Register(sub_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(and_id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::NotRm { dst, width_bits } => {
            let value = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Not),
                kind: AsmInstructionKind::Not(value),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            match dst {
                RmOperand::Reg(reg) => write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::NegRm { dst, width_bits } => {
            let value = value_from_rm_with_width(
                ctx,
                dst,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Sub(
                    AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    value,
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
            ));
            *next_id += 1;
            match dst {
                RmOperand::Reg(reg) => write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                ),
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let stored =
                        value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value: stored,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::SbbSelf { reg, width_bits } => {
            if let Some(compare) = last_compare.as_ref() {
                // Interpret carry as `ult` from the last comparison.
                patch_compare_kind(instructions, compare, 0x2)?;

                let zext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                    kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I64),
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Sub(
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Register(zext_id),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                ));
                *next_id += 1;
                write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Register(id),
                    width_bits,
                    instructions,
                    next_id,
                )
            } else {
                // If we do not know the carry flag, conservatively emit `0`.
                write_gpr_with_width(
                    ctx,
                    reg,
                    AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    width_bits,
                    instructions,
                    next_id,
                )
            }
        }
        Decoded::OrImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Or,
        ),
        Decoded::AndImmRm {
            dst,
            imm,
            width_bits,
        } => lift_rm_imm_binop(
            ctx,
            dst,
            imm,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::And,
        ),
        Decoded::ImulReg {
            dst,
            src,
            width_bits,
        } => {
            let lhs = ctx.read_gpr(dst)?;
            let rhs = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::ImulRegImm {
            dst,
            src,
            imm,
            width_bits,
        } => {
            let lhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::ImulRmWide { src, width_bits } => {
            let lhs = ctx.read_gpr(0)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(lhs, rhs),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;

            match width_bits {
                32 => {
                    let low_mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::And(AsmValue::Register(id), low_mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    write_gpr_with_width(
                        ctx,
                        0,
                        AsmValue::Register(low_id),
                        32,
                        instructions,
                        next_id,
                    )?;
                    write_gpr_with_width(
                        ctx,
                        2,
                        AsmValue::Register(high_id),
                        32,
                        instructions,
                        next_id,
                    )?;
                    Ok(())
                }
                64 => {
                    ctx.write_gpr(0, AsmValue::Register(id));
                    ctx.write_gpr(2, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 imul width")),
            }
        }
        Decoded::MulRm { src, width_bits } => {
            let lhs = ctx.read_gpr(0)?;
            let rhs = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            match width_bits {
                32 => {
                    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));

                    let lhs32_id = *next_id;
                    instructions.push(build_binop(
                        lhs32_id,
                        AsmInstructionKind::And(lhs, mask.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let rhs32_id = *next_id;
                    instructions.push(build_binop(
                        rhs32_id,
                        AsmInstructionKind::And(rhs, mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let product_id = *next_id;
                    instructions.push(build_binop(
                        product_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(lhs32_id),
                            AsmValue::Register(rhs32_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;

                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::And(
                            AsmValue::Register(product_id),
                            AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(product_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    write_gpr_with_width(
                        ctx,
                        0,
                        AsmValue::Register(low_id),
                        32,
                        instructions,
                        next_id,
                    )?;
                    write_gpr_with_width(
                        ctx,
                        2,
                        AsmValue::Register(high_id),
                        32,
                        instructions,
                        next_id,
                    )
                }
                64 => {
                    let low_mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                    let high_shift = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

                    let a0_id = *next_id;
                    instructions.push(build_binop(
                        a0_id,
                        AsmInstructionKind::And(lhs.clone(), low_mask.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    let a1_id = *next_id;
                    instructions.push(build_binop(
                        a1_id,
                        AsmInstructionKind::Shr(lhs, high_shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let b0_id = *next_id;
                    instructions.push(build_binop(
                        b0_id,
                        AsmInstructionKind::And(rhs.clone(), low_mask),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                    ));
                    *next_id += 1;
                    let b1_id = *next_id;
                    instructions.push(build_binop(
                        b1_id,
                        AsmInstructionKind::Shr(rhs, high_shift.clone()),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let p0_id = *next_id;
                    instructions.push(build_binop(
                        p0_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a0_id),
                            AsmValue::Register(b0_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p1_id = *next_id;
                    instructions.push(build_binop(
                        p1_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a0_id),
                            AsmValue::Register(b1_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p2_id = *next_id;
                    instructions.push(build_binop(
                        p2_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a1_id),
                            AsmValue::Register(b0_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;
                    let p3_id = *next_id;
                    instructions.push(build_binop(
                        p3_id,
                        AsmInstructionKind::Mul(
                            AsmValue::Register(a1_id),
                            AsmValue::Register(b1_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                    ));
                    *next_id += 1;

                    let mid_sum_id = *next_id;
                    instructions.push(build_binop(
                        mid_sum_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p1_id),
                            AsmValue::Register(p2_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let mid_carry_flag_id = *next_id;
                    instructions.push(compare_instruction(
                        mid_carry_flag_id,
                        AsmInstructionKind::Ult(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Register(p1_id),
                        ),
                        fp_core::asmir::AsmGenericOpcode::Ult,
                    ));
                    *next_id += 1;

                    let mid_carry_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: mid_carry_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                        kind: AsmInstructionKind::ZExt(
                            AsmValue::Register(mid_carry_flag_id),
                            AsmType::I64,
                        ),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let mid_shifted_id = *next_id;
                    instructions.push(build_binop(
                        mid_shifted_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let low_id = *next_id;
                    instructions.push(build_binop(
                        low_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p0_id),
                            AsmValue::Register(mid_shifted_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let low_carry_flag_id = *next_id;
                    instructions.push(compare_instruction(
                        low_carry_flag_id,
                        AsmInstructionKind::Ult(
                            AsmValue::Register(low_id),
                            AsmValue::Register(p0_id),
                        ),
                        fp_core::asmir::AsmGenericOpcode::Ult,
                    ));
                    *next_id += 1;

                    let low_carry_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: low_carry_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                        kind: AsmInstructionKind::ZExt(
                            AsmValue::Register(low_carry_flag_id),
                            AsmType::I64,
                        ),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let mid_high_id = *next_id;
                    instructions.push(build_binop(
                        mid_high_id,
                        AsmInstructionKind::Shr(
                            AsmValue::Register(mid_sum_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
                    ));
                    *next_id += 1;

                    let mid_carry_shifted_id = *next_id;
                    instructions.push(build_binop(
                        mid_carry_shifted_id,
                        AsmInstructionKind::Shl(
                            AsmValue::Register(mid_carry_id),
                            AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64)),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                    ));
                    *next_id += 1;

                    let high_partial1_id = *next_id;
                    instructions.push(build_binop(
                        high_partial1_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(p3_id),
                            AsmValue::Register(mid_high_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let high_partial2_id = *next_id;
                    instructions.push(build_binop(
                        high_partial2_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(high_partial1_id),
                            AsmValue::Register(mid_carry_shifted_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    let high_id = *next_id;
                    instructions.push(build_binop(
                        high_id,
                        AsmInstructionKind::Add(
                            AsmValue::Register(high_partial2_id),
                            AsmValue::Register(low_carry_id),
                        ),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;

                    ctx.write_gpr(0, AsmValue::Register(low_id));
                    ctx.write_gpr(2, AsmValue::Register(high_id));
                    Ok(())
                }
                _ => Err(Error::from("unsupported x86_64 mul width")),
            }
        }
        Decoded::Fild { src, width_bits } => {
            if src.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(
                        0.0,
                        AsmType::F64,
                    ))),
                    ty: AsmType::F64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(id))?;
                return Ok(());
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let load_ty = match width_bits {
                32 => AsmType::I32,
                64 => AsmType::I64,
                _ => return Err(Error::from("unsupported x86_64 fild width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: load_ty.clone(),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let conv_id = *next_id;
            instructions.push(AsmInstruction {
                id: conv_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SIToFP),
                kind: AsmInstructionKind::SIToFP(AsmValue::Register(load_id), AsmType::F64),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_push(AsmValue::Register(conv_id))?;
            Ok(())
        }
        Decoded::FldSt { index } => {
            let value = ctx.x87_peek(index)?;
            ctx.x87_push(value)
        }
        Decoded::FldMem { src, width_bits } => {
            if src.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(
                        0.0,
                        AsmType::F64,
                    ))),
                    ty: AsmType::F64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(id))?;
                return Ok(());
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let (load_ty, need_extend) = match width_bits {
                32 => (AsmType::F32, true),
                64 | 80 => (AsmType::F64, false),
                _ => return Err(Error::from("unsupported x86_64 fld width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: load_ty,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            if need_extend {
                let ext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: ext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPExt),
                    kind: AsmInstructionKind::FPExt(AsmValue::Register(load_id), AsmType::F64),
                    ty: AsmType::F64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_push(AsmValue::Register(ext_id))
            } else {
                ctx.x87_push(AsmValue::Register(load_id))
            }
        }
        Decoded::Fxch { index } => ctx.x87_swap(index),
        Decoded::FmulSt0St { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(st0, rhs),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Fmulp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(lhs, st0),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fdivrp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let denom = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(st0, denom),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fdivp { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(lhs, st0),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(index, AsmValue::Register(id))?;
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fcomi { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(st0, rhs),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: true,
            });
            Ok(())
        }
        Decoded::Fcomip { index } => {
            let st0 = ctx.x87_peek(0)?;
            let rhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(compare_instruction(
                id,
                AsmInstructionKind::Eq(st0, rhs),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id,
                index: instructions.len() - 1,
                is_float: true,
            });
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FstpSt { index } => {
            let st0 = ctx.x87_peek(0)?;
            if index != 0 {
                ctx.x87_set(index, st0)?;
            }
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FstpMem { dst, width_bits } => {
            let st0 = ctx.x87_peek(0)?;
            let stored_value = match width_bits {
                32 => {
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPTrunc),
                        kind: AsmInstructionKind::FPTrunc(st0, AsmType::F32),
                        ty: AsmType::F32,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    AsmValue::Register(id)
                }
                64 => st0,
                // x87 `fstp tbyte ptr` stores 80-bit extended precision.
                // The current AsmIR model does not represent 80-bit floats,
                // so we conservatively truncate to f64.
                80 => st0,
                _ => {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fstp width_bits={width_bits}"
                    )));
                }
            };

            if dst.segment.is_none() {
                let addr = compute_address(
                    ctx,
                    dst,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let store_id = *next_id;
                instructions.push(AsmInstruction {
                    id: store_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value: stored_value,
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: AsmType::Void,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
            }

            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::Fisttp { dst, width_bits } => {
            let st0 = ctx.x87_peek(0)?;
            if dst.segment.is_none() {
                let addr = compute_address(
                    ctx,
                    dst,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let int_ty = match width_bits {
                    32 => AsmType::I32,
                    64 => AsmType::I64,
                    _ => return Err(Error::from("unsupported x86_64 fisttp width")),
                };

                let conv_id = *next_id;
                instructions.push(AsmInstruction {
                    id: conv_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPToSI),
                    kind: AsmInstructionKind::FPToSI(st0, int_ty.clone()),
                    ty: int_ty,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                let store_id = *next_id;
                instructions.push(AsmInstruction {
                    id: store_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                    kind: AsmInstructionKind::Store {
                        value: AsmValue::Register(conv_id),
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: AsmType::Void,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
            }

            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FaddMem { src, width_bits } => {
            if src.segment.is_some() {
                return Ok(());
            }
            let st0 = ctx.x87_peek(0)?;
            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let (load_ty, need_extend) = match width_bits {
                32 => (AsmType::F32, true),
                64 | 80 => (AsmType::F64, false),
                _ => return Err(Error::from("unsupported x86_64 fadd width")),
            };

            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: load_ty,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs = if need_extend {
                let ext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: ext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPExt),
                    kind: AsmInstructionKind::FPExt(AsmValue::Register(load_id), AsmType::F64),
                    ty: AsmType::F64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                AsmValue::Register(ext_id)
            } else {
                AsmValue::Register(load_id)
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                kind: AsmInstructionKind::Add(st0, rhs),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Ffreep { index } => {
            if index != 0 {
                let zero_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zero_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Float(
                        0.0,
                        AsmType::F64,
                    ))),
                    ty: AsmType::F64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                ctx.x87_set(index, AsmValue::Register(zero_id))?;
            }
            ctx.x87_pop()?;
            Ok(())
        }
        Decoded::FsubrSt0St { index } => {
            let st0 = ctx.x87_peek(0)?;
            let lhs = ctx.x87_peek(index)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                kind: AsmInstructionKind::Sub(lhs, st0),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Fcmovcc { condition, src } => {
            let old = ctx.x87_peek(0)?;
            let new = ctx.x87_peek(src)?;

            let Some(compare) = last_compare.as_ref() else {
                return ctx.x87_set(0, new);
            };
            patch_compare_kind(instructions, compare, condition)?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(compare.id),
                    if_true: new,
                    if_false: old,
                },
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.x87_set(0, AsmValue::Register(id))
        }
        Decoded::Cmovcc {
            dst,
            src,
            condition,
            width_bits,
        } => {
            let old = ctx.read_gpr(dst)?;
            let new = value_from_rm_with_width(
                ctx,
                src,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let Some(compare) = last_compare.as_ref() else {
                return write_gpr_with_width(ctx, dst, new, width_bits, instructions, next_id);
            };
            patch_compare_kind(instructions, compare, condition)?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                kind: AsmInstructionKind::Select {
                    condition: AsmValue::Register(compare.id),
                    if_true: new,
                    if_false: old,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(id),
                width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::MovRmToReg {
            dst,
            src,
            width_bits,
        } => match src {
            RmOperand::Reg(src) => {
                let value = ctx.read_gpr(src)?;
                write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
            }
            RmOperand::Mem(memory) => {
                if memory.segment.is_some() {
                    // Most real-world ELF executables use this for stack canary loads.
                    // We do not model TLS/segments yet, so treat it as a stable zero value.
                    let value = AsmValue::Constant(AsmConstant::Int(0, AsmType::I64));
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                        kind: AsmInstructionKind::Freeze(value),
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_gpr(dst, AsmValue::Register(id));
                    return Ok(());
                }

                if let Some(symbol) = ctx
                    .resolve_disp32_symbol(&memory, inst.offset, inst.len)
                    .or_else(|| ctx.resolve_rip_symbol(&memory, inst.offset, inst.len))
                {
                    if symbol.kind == RipSymbolKind::Function {
                        ctx.write_gpr(dst, AsmValue::Function(symbol.name.clone()));
                        return Ok(());
                    }
                }
                let addr = compute_address(
                    ctx,
                    memory,
                    inst.offset,
                    inst.len,
                    relocs,
                    instructions,
                    next_id,
                )?;
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                    kind: AsmInstructionKind::Load {
                        address: addr,
                        alignment: None,
                        volatile: false,
                    },
                    ty: match width_bits {
                        8 => AsmType::I8,
                        16 => AsmType::I16,
                        32 => AsmType::I32,
                        _ => AsmType::I64,
                    },
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                let value = match width_bits {
                    64 => AsmValue::Register(id),
                    _ => {
                        let zext_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: zext_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                            kind: AsmInstructionKind::ZExt(AsmValue::Register(id), AsmType::I64),
                            ty: AsmType::I64,
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(zext_id)
                    }
                };
                write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
            }
        },
        Decoded::MovRegToRm {
            dst,
            src,
            width_bits,
        } => {
            let value = ctx.read_gpr(src)?;
            match dst {
                RmOperand::Reg(dst) => {
                    write_gpr_with_width(ctx, dst, value, width_bits, instructions, next_id)
                }
                RmOperand::Mem(memory) => {
                    let value = value_for_store(width_bits, value, instructions, next_id)?;
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::MovbeRegFromMem {
            dst,
            src,
            width_bits,
        } => {
            let value = value_from_rm_with_width(
                ctx,
                RmOperand::Mem(src),
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            write_gpr_with_width(ctx, dst, swapped, width_bits, instructions, next_id)
        }
        Decoded::MovbeMemFromReg {
            dst,
            src,
            width_bits,
        } => {
            let value = ctx.read_gpr(src)?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            let value = value_for_store(width_bits, swapped, instructions, next_id)?;
            let addr = compute_address(
                ctx,
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
        Decoded::Bswap { dst, width_bits } => {
            let value = ctx.read_gpr(dst)?;
            let swapped = byte_swap_value(width_bits, value, instructions, next_id)?;
            write_gpr_with_width(ctx, dst, swapped, width_bits, instructions, next_id)
        }
        Decoded::MovImm64 {
            dst,
            imm_offset,
            imm,
        } => {
            let reloc_offset = inst
                .offset
                .checked_add(imm_offset as u64)
                .ok_or_else(|| Error::from("x86_64 mov imm64 relocation overflow"))?;
            if let Some(reloc) = relocation_at(relocs, reloc_offset) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(reloc.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                let mut value = AsmValue::Register(symbol_id);
                let addend = reloc.addend.saturating_add(imm);
                if addend != 0 {
                    let rhs = AsmValue::Constant(AsmConstant::Int(addend, AsmType::I64));
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(value, rhs),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    value = AsmValue::Register(id);
                }
                ctx.write_gpr(dst, value);
                return Ok(());
            }

            let value = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(value),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(id));
            Ok(())
        }
        Decoded::Lea {
            dst,
            src,
            width_bits,
        } => {
            if src.segment.is_none() && src.index.is_none() {
                if src.base == Some(16) || (src.base.is_none() && src.displacement_offset.is_some())
                {
                    let next_ip = (ctx.code_base_address as i64)
                        .saturating_add(inst.offset as i64)
                        .saturating_add(inst.len as i64);
                    let target = next_ip.saturating_add(src.displacement);
                    if target >= 0 {
                        if let Some(text) = ctx.rodata_cstrings_by_addr.get(&(target as u64)) {
                            return write_gpr_with_width(
                                ctx,
                                dst,
                                AsmValue::Constant(AsmConstant::String(text.clone())),
                                width_bits,
                                instructions,
                                next_id,
                            );
                        }
                    }
                }
            }

            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            write_gpr_with_width(ctx, dst, addr, width_bits, instructions, next_id)
        }
        Decoded::CallRel32 { imm_offset, target } => {
            let reloc_offset = inst
                .offset
                .checked_add(imm_offset as u64)
                .ok_or_else(|| Error::from("x86_64 call relocation overflow"))?;

            let mut call_is_external = false;

            let function = if let Some(reloc) = relocation_at(relocs, reloc_offset) {
                if reloc.kind != object::RelocationKind::Relative
                    && reloc.kind != object::RelocationKind::PltRelative
                {
                    return Err(Error::from("unsupported x86_64 call relocation kind"));
                }
                if reloc.encoding != object::RelocationEncoding::X86Branch
                    && reloc.encoding != object::RelocationEncoding::Generic
                    && reloc.encoding != object::RelocationEncoding::Unknown
                {
                    return Err(Error::from("unsupported x86_64 call relocation encoding"));
                }
                call_is_external = relocation_is_external_call(reloc);
                AsmValue::Function(reloc.symbol.clone())
            } else if let Some(symbol) = ctx.plt_targets.get(&target) {
                call_is_external = true;
                AsmValue::Function(symbol.clone())
            } else {
                // Executables frequently use direct calls that do not carry
                // relocations. We represent the callee as a synthetic symbol
                // rooted at the target offset within the text slice so the
                // object lifter can pull in the corresponding function body.
                ctx.direct_call_targets.push(target);
                AsmValue::Function(format!("sub_{target:x}"))
            };

            let call_return_model = if call_is_external {
                match &function {
                    AsmValue::Function(name) => external_call_return_model(name),
                    _ => None,
                }
            } else {
                None
            };

            let is_lifted_internal = ctx.use_lifted_regfile_calls
                && !call_is_external
                && matches!(&function, AsmValue::Function(_));

            if is_lifted_internal {
                ctx.end_block(instructions, next_id)?;
            }

            let args = if is_lifted_internal {
                Vec::new()
            } else {
                x86_64_sysv_call_args(ctx)?
            };

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function,
                    args,
                    calling_convention: if is_lifted_internal {
                        CallingConvention::FpLiftedX86_64RegFile
                    } else {
                        CallingConvention::X86_64SysV
                    },
                    tail_call: false,
                },
                ty: if is_lifted_internal {
                    AsmType::Void
                } else if call_is_external {
                    AsmType::I64
                } else {
                    AsmType::I64
                },
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            if is_lifted_internal {
                ctx.begin_block(instructions, next_id)?;
            } else if call_is_external {
                if let Some(model) = call_return_model {
                    match model {
                        ExternalCallReturnModel::I64 => {
                            ctx.write_gpr(0, AsmValue::Register(id));
                        }
                        ExternalCallReturnModel::I32 => {
                            write_gpr_with_width(
                                ctx,
                                0,
                                AsmValue::Register(id),
                                32,
                                instructions,
                                next_id,
                            )?;
                        }
                    }
                } else {
                    ctx.write_gpr(0, AsmValue::Register(id));
                }
            } else {
                ctx.write_gpr(0, AsmValue::Register(id));
            }
            Ok(())
        }
        Decoded::IncRm { target, width_bits } => {
            let one = AsmValue::Constant(AsmConstant::Int(1, AsmType::I64));
            match target {
                RmOperand::Reg(reg) => {
                    let lhs = ctx.read_gpr(reg)?;
                    let id = *next_id;
                    instructions.push(build_binop(
                        id,
                        AsmInstructionKind::Add(lhs.clone(), one),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let load_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: load_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: addr.clone(),
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let add_id = *next_id;
                    instructions.push(build_binop(
                        add_id,
                        AsmInstructionKind::Add(AsmValue::Register(load_id), one),
                        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                    ));
                    *next_id += 1;
                    let mut value = AsmValue::Register(add_id);
                    if width_bits == 32 {
                        let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                        let and_id = *next_id;
                        instructions.push(build_binop(
                            and_id,
                            AsmInstructionKind::And(value, mask),
                            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                        ));
                        *next_id += 1;
                        value = AsmValue::Register(and_id);
                    }

                    let store_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: store_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::DecRm { target, width_bits } => lift_rm_imm_binop(
            ctx,
            target,
            1,
            width_bits,
            *inst,
            bytes,
            relocs,
            instructions,
            next_id,
            fp_core::asmir::AsmGenericOpcode::Sub,
        ),
        Decoded::MovSxd { dst, src } => {
            let value = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            relocs,
                            instructions,
                            next_id,
                        )?;
                        let load_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: load_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                            kind: AsmInstructionKind::Load {
                                address: addr,
                                alignment: None,
                                volatile: false,
                            },
                            ty: AsmType::I32,
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(value, AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_gpr(dst, AsmValue::Register(sext_id));
            Ok(())
        }
        Decoded::MovSx {
            dst,
            src,
            src_width_bits,
            dst_width_bits,
        } => {
            let raw = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            relocs,
                            instructions,
                            next_id,
                        )?;
                        let load_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: load_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                            kind: AsmInstructionKind::Load {
                                address: addr,
                                alignment: None,
                                volatile: false,
                            },
                            ty: match src_width_bits {
                                8 => AsmType::I8,
                                _ => AsmType::I16,
                            },
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };

            let truncated_type = match src_width_bits {
                8 => AsmType::I8,
                _ => AsmType::I16,
            };
            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(raw, truncated_type.clone()),
                ty: truncated_type,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let sext_id = *next_id;
            instructions.push(AsmInstruction {
                id: sext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SExt),
                kind: AsmInstructionKind::SExt(AsmValue::Register(trunc_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            write_gpr_with_width(
                ctx,
                dst,
                AsmValue::Register(sext_id),
                dst_width_bits,
                instructions,
                next_id,
            )
        }
        Decoded::DivRm {
            src,
            signed,
            width_bits,
        } => {
            let _signed = signed;

            let rax = ctx.read_gpr(0)?;
            // `div` uses the 128-bit dividend (rdx:rax). Modeling the full
            // wide dividend would require multi-precision arithmetic in AsmIR.
            // For now, approximate by using only the low 64 bits (rax).
            let _rdx = ctx.read_gpr(2)?;

            let divisor = value_from_operand(
                ctx,
                Operand::Rm(src),
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let quot_id = *next_id;
            instructions.push(build_binop(
                quot_id,
                AsmInstructionKind::Div(rax.clone(), divisor.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
            ));
            *next_id += 1;
            let rem_id = *next_id;
            instructions.push(build_binop(
                rem_id,
                AsmInstructionKind::Rem(rax, divisor),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Rem),
            ));
            *next_id += 1;

            let mut quot = AsmValue::Register(quot_id);
            let mut rem = AsmValue::Register(rem_id);
            if width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let q_id = *next_id;
                instructions.push(build_binop(
                    q_id,
                    AsmInstructionKind::And(quot, mask.clone()),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                quot = AsmValue::Register(q_id);
                let r_id = *next_id;
                instructions.push(build_binop(
                    r_id,
                    AsmInstructionKind::And(rem, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                rem = AsmValue::Register(r_id);
            }

            ctx.write_gpr(0, quot);
            ctx.write_gpr(2, rem);
            Ok(())
        }
        Decoded::Syscall => {
            let syscall_convention = syscall_convention.ok_or_else(|| {
                Error::from("x86_64 syscall lifting is disabled for COFF/PE targets")
            })?;
            let number = ctx.read_gpr(0)?;
            let args = vec![
                ctx.read_gpr(7)?,
                ctx.read_gpr(6)?,
                ctx.read_gpr(2)?,
                ctx.read_gpr(10)?,
                ctx.read_gpr(8)?,
                ctx.read_gpr(9)?,
            ];
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Syscall),
                kind: AsmInstructionKind::Syscall {
                    convention: syscall_convention,
                    number,
                    args,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(0, AsmValue::Register(id));
            ctx.write_gpr(1, AsmValue::Undef(AsmType::I64));
            ctx.write_gpr(11, AsmValue::Undef(AsmType::I64));
            Ok(())
        }
        Decoded::Setcc { dst, condition } => {
            let value = if let Some(compare) = last_compare.as_ref() {
                patch_compare_kind(instructions, compare, condition)?;
                let zext_id = *next_id;
                instructions.push(AsmInstruction {
                    id: zext_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                    kind: AsmInstructionKind::ZExt(AsmValue::Register(compare.id), AsmType::I8),
                    ty: AsmType::I8,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                AsmValue::Register(zext_id)
            } else {
                // `setcc` can observe flags from arithmetic instructions we
                // do not model yet; conservatively synthesize `0`.
                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I8))
            };
            match dst {
                RmOperand::Reg(reg) => {
                    ctx.write_gpr(reg, value);
                    Ok(())
                }
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        return Ok(());
                    }
                    let addr = compute_address(
                        ctx,
                        memory,
                        inst.offset,
                        inst.len,
                        relocs,
                        instructions,
                        next_id,
                    )?;
                    let id = *next_id;
                    instructions.push(AsmInstruction {
                        id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                        kind: AsmInstructionKind::Store {
                            value,
                            address: addr,
                            alignment: None,
                            volatile: false,
                        },
                        ty: AsmType::Void,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    Ok(())
                }
            }
        }
        Decoded::MovZx {
            dst,
            src,
            src_width_bits,
            dst_width_bits,
        } => {
            let value = match src {
                RmOperand::Reg(src) => ctx.read_gpr(src)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            relocs,
                            instructions,
                            next_id,
                        )?;
                        let load_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: load_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                            kind: AsmInstructionKind::Load {
                                address: addr,
                                alignment: None,
                                volatile: false,
                            },
                            ty: match src_width_bits {
                                8 => AsmType::I8,
                                16 => AsmType::I16,
                                _ => AsmType::I32,
                            },
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };
            let mask_bits = match src_width_bits {
                8 => 0xFF,
                16 => 0xFFFF,
                _ => 0xFFFF_FFFF,
            };
            let mask = AsmValue::Constant(AsmConstant::UInt(mask_bits, AsmType::I64));
            let and_id = *next_id;
            instructions.push(build_binop(
                and_id,
                AsmInstructionKind::And(value, mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;
            let mut result = AsmValue::Register(and_id);
            if dst_width_bits == 32 {
                let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
                let and_id = *next_id;
                instructions.push(build_binop(
                    and_id,
                    AsmInstructionKind::And(result, mask),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                result = AsmValue::Register(and_id);
            }
            ctx.write_gpr(dst, result);
            Ok(())
        }
        Decoded::Ret | Decoded::JmpRel { .. } | Decoded::JmpRm { .. } | Decoded::JccRel { .. } => {
            Ok(())
        }
        other => Err(Error::from(format!(
            "unsupported x86 instruction: {other:?}"
        ))),
    }
}
