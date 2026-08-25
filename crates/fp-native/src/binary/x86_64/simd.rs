use super::*;

pub(super) fn packed_add_i32x2(
    lhs: AsmValue,
    rhs: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let lhs = freeze_i64(lhs, instructions, next_id);
    let rhs = freeze_i64(rhs, instructions, next_id);

    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
    let shift_32 = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

    let lhs_low_id = *next_id;
    instructions.push(build_binop(
        lhs_low_id,
        AsmInstructionKind::And(lhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_low_id = *next_id;
    instructions.push(build_binop(
        rhs_low_id,
        AsmInstructionKind::And(rhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let low_sum_id = *next_id;
    instructions.push(build_binop(
        low_sum_id,
        AsmInstructionKind::Add(
            AsmValue::Register(lhs_low_id),
            AsmValue::Register(rhs_low_id),
        ),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    ));
    *next_id += 1;
    let low_masked_id = *next_id;
    instructions.push(build_binop(
        low_masked_id,
        AsmInstructionKind::And(AsmValue::Register(low_sum_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let lhs_hi_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_id,
        AsmInstructionKind::Shr(lhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;
    let rhs_hi_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_id,
        AsmInstructionKind::Shr(rhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;

    let hi_sum_id = *next_id;
    instructions.push(build_binop(
        hi_sum_id,
        AsmInstructionKind::Add(AsmValue::Register(lhs_hi_id), AsmValue::Register(rhs_hi_id)),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
    ));
    *next_id += 1;
    let hi_masked_id = *next_id;
    instructions.push(build_binop(
        hi_masked_id,
        AsmInstructionKind::And(AsmValue::Register(hi_sum_id), mask),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let hi_shifted_id = *next_id;
    instructions.push(build_binop(
        hi_shifted_id,
        AsmInstructionKind::Shl(AsmValue::Register(hi_masked_id), shift_32),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
    ));
    *next_id += 1;

    let out_id = *next_id;
    instructions.push(build_binop(
        out_id,
        AsmInstructionKind::Or(
            AsmValue::Register(hi_shifted_id),
            AsmValue::Register(low_masked_id),
        ),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
    ));
    *next_id += 1;

    AsmValue::Register(out_id)
}

pub(super) fn packed_umax_i32x2(
    lhs: AsmValue,
    rhs: AsmValue,
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> AsmValue {
    let lhs = freeze_i64(lhs, instructions, next_id);
    let rhs = freeze_i64(rhs, instructions, next_id);

    let mask = AsmValue::Constant(AsmConstant::UInt(0xFFFF_FFFF, AsmType::I64));
    let shift_32 = AsmValue::Constant(AsmConstant::UInt(32, AsmType::I64));

    let lhs_low_id = *next_id;
    instructions.push(build_binop(
        lhs_low_id,
        AsmInstructionKind::And(lhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_low_id = *next_id;
    instructions.push(build_binop(
        rhs_low_id,
        AsmInstructionKind::And(rhs.clone(), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let cmp_low_id = *next_id;
    instructions.push(compare_instruction(
        cmp_low_id,
        AsmInstructionKind::Ugt(
            AsmValue::Register(lhs_low_id),
            AsmValue::Register(rhs_low_id),
        ),
        fp_core::asmir::AsmGenericOpcode::Ugt,
    ));
    *next_id += 1;
    let sel_low_id = *next_id;
    instructions.push(AsmInstruction {
        id: sel_low_id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
        kind: AsmInstructionKind::Select {
            condition: AsmValue::Register(cmp_low_id),
            if_true: AsmValue::Register(lhs_low_id),
            if_false: AsmValue::Register(rhs_low_id),
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

    let lhs_hi_shift_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_shift_id,
        AsmInstructionKind::Shr(lhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;
    let rhs_hi_shift_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_shift_id,
        AsmInstructionKind::Shr(rhs, shift_32.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shr),
    ));
    *next_id += 1;

    let lhs_hi_id = *next_id;
    instructions.push(build_binop(
        lhs_hi_id,
        AsmInstructionKind::And(AsmValue::Register(lhs_hi_shift_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;
    let rhs_hi_id = *next_id;
    instructions.push(build_binop(
        rhs_hi_id,
        AsmInstructionKind::And(AsmValue::Register(rhs_hi_shift_id), mask.clone()),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::And),
    ));
    *next_id += 1;

    let cmp_hi_id = *next_id;
    instructions.push(compare_instruction(
        cmp_hi_id,
        AsmInstructionKind::Ugt(AsmValue::Register(lhs_hi_id), AsmValue::Register(rhs_hi_id)),
        fp_core::asmir::AsmGenericOpcode::Ugt,
    ));
    *next_id += 1;
    let sel_hi_id = *next_id;
    instructions.push(AsmInstruction {
        id: sel_hi_id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Select),
        kind: AsmInstructionKind::Select {
            condition: AsmValue::Register(cmp_hi_id),
            if_true: AsmValue::Register(lhs_hi_id),
            if_false: AsmValue::Register(rhs_hi_id),
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

    let hi_shifted_id = *next_id;
    instructions.push(build_binop(
        hi_shifted_id,
        AsmInstructionKind::Shl(AsmValue::Register(sel_hi_id), shift_32),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Shl),
    ));
    *next_id += 1;

    let out_id = *next_id;
    instructions.push(build_binop(
        out_id,
        AsmInstructionKind::Or(
            AsmValue::Register(hi_shifted_id),
            AsmValue::Register(sel_low_id),
        ),
        AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Or),
    ));
    *next_id += 1;
    AsmValue::Register(out_id)
}
