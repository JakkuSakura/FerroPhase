use super::*;

pub(super) fn lift_simd_instructions(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    bytes: &[u8],
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    last_compare: &mut Option<LastCompare>,
    syscall_convention: Option<AsmSyscallConvention>,
    jump_table_by_capture_offset: &std::collections::HashMap<u64, (u64, u8)>,
) -> Result<Option<()>> {
    match inst.kind {
        Decoded::Vpbroadcastq { dst, src } => {
            let value = ctx.read_gpr(src)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Splat),
                kind: AsmInstructionKind::Splat {
                    value,
                    lane_bits: 64,
                    lanes: 2,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::ZeroXmm { dst } => {
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::OnesXmm { dst } => {
            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let ones = AsmValue::Constant(AsmConstant::UInt(u64::MAX, AsmType::I64));

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: ones.clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: ones,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vcvtusi2sd {
            dst,
            src_vec,
            src_gpr,
            width_bits,
        } => {
            let src_vec_value = ctx.read_vec(src_vec)?;
            let int_value = value_from_rm_with_width(
                ctx,
                src_gpr,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::UIToFP),
                kind: AsmInstructionKind::UIToFP(int_value, AsmType::F64),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(fp_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: src_vec_value,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert0_id));
            Ok(Some(()))
        }
        Decoded::Vcvtusi2ss {
            dst,
            src_vec,
            src_gpr,
            width_bits,
        } => {
            let base_vec = ctx.read_vec(src_vec)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let int_value = value_from_rm_with_width(
                ctx,
                src_gpr,
                width_bits,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::UIToFP),
                kind: AsmInstructionKind::UIToFP(int_value, AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(fp_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VmulsdMem { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;

            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
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

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lane0_id), AsmType::F64),
                ty: AsmType::F64,
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
            let rhs_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
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

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_id),
                ),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: lhs_vec,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::Vdivsd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let lhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_lane0_id,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
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

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_lane0_id), AsmType::F64),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_lane0_id), AsmType::F64),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: lhs_vec,
                    lane: 0,
                    value: AsmValue::Register(bits_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VmovupsStore { dst, src } => {
            let src_vec = ctx.read_vec(src)?;

            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec.clone(),
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

            let lane1_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
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
                dst,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store0_id = *next_id;
            instructions.push(AsmInstruction {
                id: store0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Register(lane0_id),
                    address: addr.clone(),
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

            let store1_id = *next_id;
            instructions.push(AsmInstruction {
                id: store1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Register(lane1_id),
                    address: AsmValue::Register(addr1_id),
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
            Ok(Some(()))
        }
        Decoded::VmovupsLoad { dst, src } => {
            let addr = compute_address(
                ctx,
                src,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let load0_id = *next_id;
            instructions.push(AsmInstruction {
                id: load0_id,
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

            let load1_id = *next_id;
            instructions.push(AsmInstruction {
                id: load1_id,
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

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: AsmValue::Register(load0_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: AsmValue::Register(load1_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::VmovssLoad { dst, src } => {
            let dst_vec = ctx.read_vec(dst)?;
            let new_low = value_from_rm_with_width(
                ctx,
                RmOperand::Mem(src),
                32,
                *inst,
                relocs,
                instructions,
                next_id,
            )?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: dst_vec.clone(),
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), new_low),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: dst_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VmovssStore { dst, src } => {
            let src_vec = ctx.read_vec(src)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
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

            let stored = value_for_store(32, AsmValue::Register(lane0_id), instructions, next_id)?;
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
            Ok(Some(()))
        }
        Decoded::VcomissMem { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
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

            let mask_id = *next_id;
            instructions.push(build_binop(
                mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(trunc_id), AsmType::F32),
                ty: AsmType::F32,
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
            let rhs_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::F32,
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
                AsmInstructionKind::Eq(AsmValue::Register(lhs_fp_id), AsmValue::Register(rhs_id)),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: false,
            });
            Ok(Some(()))
        }
        Decoded::VcomissReg { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let lhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: lhs_vec,
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
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

            let lhs_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_trunc_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_trunc_id), AsmType::F32),
                ty: AsmType::F32,
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
                AsmInstructionKind::Eq(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                fp_core::asmir::AsmGenericOpcode::Eq,
            ));
            *next_id += 1;
            *last_compare = Some(LastCompare {
                id: cmp_id,
                index: instructions.len() - 1,
                is_float: true,
            });
            Ok(Some(()))
        }
        Decoded::VaddssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let low_mask_id = *next_id;
            instructions.push(build_binop(
                low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
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
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let add_id = *next_id;
            instructions.push(AsmInstruction {
                id: add_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                kind: AsmInstructionKind::Add(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(add_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::Vdivss { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VdivssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
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
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let div_id = *next_id;
            instructions.push(AsmInstruction {
                id: div_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Div),
                kind: AsmInstructionKind::Div(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(div_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::Vcvttss2usi {
            dst,
            src,
            width_bits,
        } => {
            let src_vec = ctx.read_vec(src)?;
            let lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: src_vec,
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

            let mask_id = *next_id;
            instructions.push(build_binop(
                mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let trunc_id = *next_id;
            instructions.push(AsmInstruction {
                id: trunc_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(trunc_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let int_id = *next_id;
            instructions.push(AsmInstruction {
                id: int_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::FPToUI),
                kind: AsmInstructionKind::FPToUI(AsmValue::Register(fp_id), AsmType::I64),
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
                AsmValue::Register(int_id),
                width_bits,
                instructions,
                next_id,
            )
            .map(Some)
        }
        Decoded::Vmulss { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let rhs_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let rhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                rhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(rhs_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(rhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(rhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VmulssMem { dst, lhs, rhs } => {
            let base_vec = ctx.read_vec(lhs)?;

            let old_lane0_id = *next_id;
            instructions.push(AsmInstruction {
                id: old_lane0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
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

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(!0xFFFF_FFFFu64, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_low_mask_id = *next_id;
            instructions.push(build_binop(
                lhs_low_mask_id,
                AsmInstructionKind::And(
                    AsmValue::Register(old_lane0_id),
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let lhs_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Trunc),
                kind: AsmInstructionKind::Trunc(AsmValue::Register(lhs_low_mask_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let lhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: lhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(lhs_i32_id), AsmType::F32),
                ty: AsmType::F32,
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
            let rhs_fp_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs_fp_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mul_id = *next_id;
            instructions.push(AsmInstruction {
                id: mul_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
                kind: AsmInstructionKind::Mul(
                    AsmValue::Register(lhs_fp_id),
                    AsmValue::Register(rhs_fp_id),
                ),
                ty: AsmType::F32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i32_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i32_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Bitcast),
                kind: AsmInstructionKind::Bitcast(AsmValue::Register(mul_id), AsmType::I32),
                ty: AsmType::I32,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let bits_i64_id = *next_id;
            instructions.push(AsmInstruction {
                id: bits_i64_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(bits_i32_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(preserved_id),
                    AsmValue::Register(bits_i64_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: 0,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::VpxorqXmmMem { dst, lhs, rhs } => {
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

            let base_addr = compute_address(
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
                    address: base_addr.clone(),
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

            let rhs1_addr_id = *next_id;
            instructions.push(build_binop(
                rhs1_addr_id,
                AsmInstructionKind::Add(
                    base_addr,
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
                    address: AsmValue::Register(rhs1_addr_id),
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

            let out0_id = *next_id;
            instructions.push(build_binop(
                out0_id,
                AsmInstructionKind::Xor(AsmValue::Register(lhs0_id), AsmValue::Register(rhs0_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            let out1_id = *next_id;
            instructions.push(build_binop(
                out1_id,
                AsmInstructionKind::Xor(AsmValue::Register(lhs1_id), AsmValue::Register(rhs1_id)),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Xor),
            ));
            *next_id += 1;

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: AsmValue::Register(out0_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: AsmValue::Register(out1_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpmaxuq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Ugt(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    fp_core::asmir::AsmGenericOpcode::Ugt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(lhs_id),
                        if_false: AsmValue::Register(rhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpmaxud { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out = packed_umax_i32x2(
                    AsmValue::Register(lhs_id),
                    AsmValue::Register(rhs_id),
                    instructions,
                    next_id,
                );
                lane_values.push(out);
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpminuq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Ugt(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    fp_core::asmir::AsmGenericOpcode::Ugt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(rhs_id),
                        if_false: AsmValue::Register(lhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpsubq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Sub(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Sub),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpunpcklwd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 16,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::Vpunpckldq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 32,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::Vpunpcklqdq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZipLow),
                kind: AsmInstructionKind::ZipLow {
                    lhs: lhs_vec,
                    rhs: rhs_vec,
                    lane_bits: 64,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::Vpaddd { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out = packed_add_i32x2(
                    AsmValue::Register(lhs_id),
                    AsmValue::Register(rhs_id),
                    instructions,
                    next_id,
                );
                lane_values.push(out);
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpaddq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Add(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vpsrldq { dst, src, imm } => {
            let src_vec = ctx.read_vec(src)?;
            match imm {
                0 => {
                    ctx.write_vec(dst, src_vec);
                    Ok(Some(()))
                }
                8 => {
                    let lane1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: lane1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: src_vec,
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

                    let vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Register(lane1_id),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(vec_id));
                    Ok(Some(()))
                }
                imm if imm >= 16 => {
                    let zero_vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: zero_vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(zero_vec_id));
                    Ok(Some(()))
                }
                _ => Err(Error::from("unsupported x86_64 vpsrldq shift amount")),
            }
        }
        Decoded::Vpandq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::And(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vporq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let out_id = *next_id;
                instructions.push(build_binop(
                    out_id,
                    AsmInstructionKind::Or(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
                ));
                *next_id += 1;
                lane_values.push(AsmValue::Register(out_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::Vptest { lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = ctx.read_vec(rhs)?;

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

            let rhs0_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec.clone(),
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
            let rhs1_id = *next_id;
            instructions.push(AsmInstruction {
                id: rhs1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: rhs_vec,
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
                is_float: true,
            });
            Ok(Some(()))
        }
        Decoded::Vpalignr { dst, lhs, rhs, imm } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            match imm {
                0 => {
                    ctx.write_vec(dst, lhs_vec);
                    Ok(Some(()))
                }
                8 => {
                    let lhs_lane1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: lhs_lane1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: lhs_vec.clone(),
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

                    let rhs_lane0_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: rhs_lane0_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                        kind: AsmInstructionKind::ExtractLane {
                            vector: rhs_vec,
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

                    let insert0_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: insert0_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                        kind: AsmInstructionKind::InsertLane {
                            vector: lhs_vec,
                            lane: 0,
                            value: AsmValue::Register(lhs_lane1_id),
                        },
                        ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    let insert1_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: insert1_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                        kind: AsmInstructionKind::InsertLane {
                            vector: AsmValue::Register(insert0_id),
                            lane: 1,
                            value: AsmValue::Register(rhs_lane0_id),
                        },
                        ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;

                    ctx.write_vec(dst, AsmValue::Register(insert1_id));
                    Ok(Some(()))
                }
                16 => {
                    ctx.write_vec(dst, rhs_vec);
                    Ok(Some(()))
                }
                imm if imm > 16 => {
                    let zero_vec_id = *next_id;
                    instructions.push(AsmInstruction {
                        id: zero_vec_id,
                        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                        kind: AsmInstructionKind::BuildVector {
                            elements: vec![
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                                AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            ],
                        },
                        ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    });
                    *next_id += 1;
                    ctx.write_vec(dst, AsmValue::Register(zero_vec_id));
                    Ok(Some(()))
                }
                _ => Err(Error::from("unsupported x86_64 vpalignr immediate")),
            }
        }
        Decoded::Vpmaxsq { dst, lhs, rhs } => {
            let lhs_vec = ctx.read_vec(lhs)?;
            let rhs_vec = vec_operand_value(
                ctx,
                rhs,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;

            let lanes = [0u16, 1u16];
            let mut lane_values = Vec::with_capacity(2);
            for lane in lanes {
                let lhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: lhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: lhs_vec.clone(),
                        lane,
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

                let rhs_id = *next_id;
                instructions.push(AsmInstruction {
                    id: rhs_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                    kind: AsmInstructionKind::ExtractLane {
                        vector: rhs_vec.clone(),
                        lane,
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

                let cmp_id = *next_id;
                instructions.push(compare_instruction(
                    cmp_id,
                    AsmInstructionKind::Gt(AsmValue::Register(lhs_id), AsmValue::Register(rhs_id)),
                    fp_core::asmir::AsmGenericOpcode::Gt,
                ));
                *next_id += 1;

                let select_id = *next_id;
                instructions.push(AsmInstruction {
                    id: select_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
                    kind: AsmInstructionKind::Select {
                        condition: AsmValue::Register(cmp_id),
                        if_true: AsmValue::Register(lhs_id),
                        if_false: AsmValue::Register(rhs_id),
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
                lane_values.push(AsmValue::Register(select_id));
            }

            let zero_vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: zero_vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert0_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(zero_vec_id),
                    lane: 0,
                    value: lane_values[0].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let insert1_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: AsmValue::Register(insert0_id),
                    lane: 1,
                    value: lane_values[1].clone(),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert1_id));
            Ok(Some(()))
        }
        Decoded::MovdXmmFromGpr32 { dst, src } => {
            let raw = ctx.read_gpr(src)?;
            let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
            let masked_id = *next_id;
            instructions.push(build_binop(
                masked_id,
                AsmInstructionKind::And(raw, mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(masked_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(Some(()))
        }
        Decoded::MovdXmmFromMem32 { dst, src } => {
            if src.segment.is_some() {
                return Ok(Some(()));
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

            let zext_id = *next_id;
            instructions.push(AsmInstruction {
                id: zext_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ZExt),
                kind: AsmInstructionKind::ZExt(AsmValue::Register(load_id), AsmType::I64),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(zext_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(Some(()))
        }
        Decoded::MovdMem32FromXmm { dst, src } => {
            if dst.segment.is_some() {
                return Ok(Some(()));
            }

            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let stored =
                value_for_store(32, AsmValue::Register(extract_id), instructions, next_id)?;
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
            Ok(Some(()))
        }
        Decoded::MovdGpr32FromXmm {
            dst,
            src,
            width_bits,
        } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
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
                AsmValue::Register(extract_id),
                width_bits,
                instructions,
                next_id,
            )
            .map(Some)
        }
        Decoded::Pinsrd {
            dst,
            vector,
            value,
            lane,
        } => {
            if lane > 3 {
                return Err(Error::from("unsupported vpinsrd lane"));
            }
            let base_vec = ctx.read_vec(vector)?;
            let scalar =
                value_from_rm_with_width(ctx, value, 32, *inst, relocs, instructions, next_id)?;

            let half = u16::from(lane / 2);
            let part = lane % 2;

            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: half,
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

            let scalar_masked_id = *next_id;
            instructions.push(build_binop(
                scalar_masked_id,
                AsmInstructionKind::And(
                    scalar,
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let (preserve_mask, shifted_scalar) = if part == 0 {
                (
                    AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF_0000_0000, AsmType::I64)),
                    AsmValue::Register(scalar_masked_id),
                )
            } else {
                let shifted_id = *next_id;
                instructions.push(build_binop(
                    shifted_id,
                    AsmInstructionKind::Shl(
                        AsmValue::Register(scalar_masked_id),
                        AsmValue::Constant(AsmConstant::Int(32, AsmType::I64)),
                    ),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
                ));
                *next_id += 1;
                (
                    AsmValue::Constant(AsmConstant::UInt(0x0000_0000_FFFF_FFFF, AsmType::I64)),
                    AsmValue::Register(shifted_id),
                )
            };

            let preserved_id = *next_id;
            instructions.push(build_binop(
                preserved_id,
                AsmInstructionKind::And(AsmValue::Register(extract_id), preserve_mask),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(AsmValue::Register(preserved_id), shifted_scalar),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: half,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::Pinsrb {
            dst,
            vector,
            value,
            lane,
        } => {
            if lane >= 16 {
                return Err(Error::from("unsupported vpinsrb lane"));
            }

            let word_lane = if lane < 8 { 0u16 } else { 1u16 };
            let byte_lane = (lane % 8) as u32;
            let shift_bits = (byte_lane * 8) as i64;

            let base_vec = ctx.read_vec(vector)?;

            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector: base_vec.clone(),
                    lane: word_lane,
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

            let raw =
                value_from_rm_with_width(ctx, value, 8, *inst, relocs, instructions, next_id)?;

            let masked_byte_id = *next_id;
            instructions.push(build_binop(
                masked_byte_id,
                AsmInstructionKind::And(
                    raw,
                    AsmValue::Constant(AsmConstant::UInt(0xFF, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let shifted_id = *next_id;
            instructions.push(build_binop(
                shifted_id,
                AsmInstructionKind::Shl(
                    AsmValue::Register(masked_byte_id),
                    AsmValue::Constant(AsmConstant::Int(shift_bits, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
            ));
            *next_id += 1;

            let mask = !(0xFFu64 << (byte_lane * 8));
            let cleared_id = *next_id;
            instructions.push(build_binop(
                cleared_id,
                AsmInstructionKind::And(
                    AsmValue::Register(extract_id),
                    AsmValue::Constant(AsmConstant::UInt(mask, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
            ));
            *next_id += 1;

            let merged_id = *next_id;
            instructions.push(build_binop(
                merged_id,
                AsmInstructionKind::Or(
                    AsmValue::Register(cleared_id),
                    AsmValue::Register(shifted_id),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
            ));
            *next_id += 1;

            let insert_id = *next_id;
            instructions.push(AsmInstruction {
                id: insert_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base_vec,
                    lane: word_lane,
                    value: AsmValue::Register(merged_id),
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(insert_id));
            Ok(Some(()))
        }
        Decoded::MovqXmmFromMem { dst, src } => {
            if src.segment.is_some() {
                return Ok(Some(()));
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

            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![
                        AsmValue::Register(load_id),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(Some(()))
        }
        Decoded::MovqXmmFromGpr { dst, src } => {
            let value = ctx.read_gpr(src)?;
            let vec_id = *next_id;
            instructions.push(AsmInstruction {
                id: vec_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![value, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(vec_id));
            Ok(Some(()))
        }
        Decoded::MovqMemFromXmm { dst, src } => {
            if dst.segment.is_some() {
                return Ok(Some(()));
            }
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
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
                    value: AsmValue::Register(extract_id),
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
            Ok(Some(()))
        }
        Decoded::MovqGprFromXmm { dst, src } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane { vector, lane: 0 },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_gpr(dst, AsmValue::Register(extract_id));
            Ok(Some(()))
        }
        Decoded::Pinsrq {
            dst,
            vector,
            value,
            lane,
        } => {
            let base = ctx.read_vec(vector)?;
            let scalar =
                value_from_rm_with_width(ctx, value, 64, *inst, relocs, instructions, next_id)?;
            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::InsertLane),
                kind: AsmInstructionKind::InsertLane {
                    vector: base,
                    lane: lane as u16,
                    value: scalar,
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            ctx.write_vec(dst, AsmValue::Register(id));
            Ok(Some(()))
        }
        Decoded::Pextrq { dst, src, lane } => {
            let vector = ctx.read_vec(src)?;
            let extract_id = *next_id;
            instructions.push(AsmInstruction {
                id: extract_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::ExtractLane),
                kind: AsmInstructionKind::ExtractLane {
                    vector,
                    lane: lane as u16,
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
            ctx.write_gpr(dst, AsmValue::Register(extract_id));
            Ok(Some(()))
        }
        _ => Ok(None),
    }
}
