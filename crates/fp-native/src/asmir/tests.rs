use super::{
    lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64, map_constant_kind,
    select_program,
};
use crate::asm::aarch64::{Aarch64CallTarget, Aarch64ConditionCode, Aarch64TerminatorOpcode};
use crate::asm::aarch64::{
    Aarch64InstructionDetail, Aarch64Operand, Aarch64Register, Aarch64TerminatorDetail,
    AsmAarch64Block, AsmAarch64Function, AsmAarch64Program,
};
use crate::asm::x86_64::{
    AsmX86_64Block, AsmX86_64Function, AsmX86_64Program, X86InstructionDetail, X86Operand,
    X86Register, X86TerminatorDetail,
};
use crate::asm::x86_64::{X86CallTarget, X86ConditionCode, X86Opcode, X86TerminatorOpcode};
use crate::emit::{TargetArch, TargetFormat};
use fp_core::asmir::{
    AsmConditionCode, AsmGenericOpcode, AsmInstructionKind, AsmOpcode, AsmOperand, AsmTerminator,
    AsmValue, OperandAccess,
};
use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirBlob, LirConstant, LirFunction, LirFunctionSignature,
    LirInstruction, LirInstructionKind, LirInteger, LirRegister, LirTerminator, LirType, LirValue,
    Name,
};

fn layout() -> fp_core::lir::LirDataLayout {
    fp_core::lir::LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .unwrap()
}

fn i32_value(value: u32) -> LirValue {
    LirValue::constant(LirConstant::integer(LirType::I32, LirInteger::I32(value)).unwrap())
}

#[test]
fn maps_constant_global_gep_to_global_reference() {
    let ptr_ty = LirType::Ptr(Box::new(LirType::I8));
    let constant = LirConstant::get_element_ptr(
        ptr_ty.clone(),
        LirConstant::global_address(ptr_ty.clone(), Name::new("message")),
        vec![LirConstant::integer(LirType::I64, LirInteger::I64(1)).unwrap()],
        true,
    );

    let mapped = map_constant_kind(&constant.kind, &constant.ty);
    assert!(matches!(
        mapped,
        fp_core::asmir::AsmConstant::GlobalRef(name, _, indices)
            if name == Name::new("message") && indices == vec![1]
    ));
}

fn reg(id: u32, ty: LirType) -> LirValue {
    LirValue::register(id, ty)
}

#[test]
fn select_program_builds_semantic_asmir() {
    let lir = LirBlob {
        data_layout: layout(),
        functions: vec![LirFunction {
            def_id: None,
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Freeze(LirValue::constant(LirConstant::undef(
                        LirType::I32,
                    ))),
                    result: Some(LirRegister {
                        id: 1,
                        ty: LirType::I32,
                    }),
                    debug_info: None,
                }],
                terminator: LirTerminator::Return(Some(reg(1, LirType::I32))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::External,
            is_declaration: false,
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),

        queries: Vec::new(),
    };

    let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
    assert_eq!(program.functions.len(), 1);
    assert!(matches!(
        program.functions[0].basic_blocks[0].instructions[0].kind,
        AsmInstructionKind::Freeze(_)
    ));
    assert!(matches!(
        program.functions[0].basic_blocks[0].terminator,
        AsmTerminator::Return(Some(AsmValue::Register(1)))
    ));
}

#[test]
fn select_program_normalizes_x86_opcode_and_operands() {
    let lir = LirBlob {
        data_layout: layout(),
        functions: vec![LirFunction {
            def_id: None,
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![
                    // Defines register 1 before the Add below references
                    // it as a value — a register use with no producing
                    // instruction anywhere in scope (and not a
                    // parameter/local either) isn't well-formed LIR, so
                    // this test shouldn't construct one just to exercise
                    // the Add opcode/operand shape.
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::Add(i32_value(0), i32_value(0)),
                        result: Some(LirRegister {
                            id: 1,
                            ty: LirType::I32,
                        }),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 7,
                        kind: LirInstructionKind::Add(reg(1, LirType::I32), i32_value(4)),
                        result: Some(LirRegister {
                            id: 7,
                            ty: LirType::I32,
                        }),
                        debug_info: None,
                    },
                ],
                terminator: LirTerminator::Return(Some(reg(7, LirType::I32))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::External,
            is_declaration: false,
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),

        queries: Vec::new(),
    };

    let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
    let inst = &program.functions[0].basic_blocks[0].instructions[1];

    assert_eq!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Add));
    assert_eq!(inst.operands.len(), 3);
    assert!(matches!(
        &inst.operands[0],
        AsmOperand::Register {
            access: OperandAccess::Write,
            ..
        }
    ));
    assert!(matches!(&inst.operands[1], AsmOperand::Register { .. }));
    assert!(matches!(&inst.operands[2], AsmOperand::Immediate(4)));

    let x86 = lower_to_x86_64(&program);
    let inst = &x86.functions[0].blocks[0].instructions[1];
    assert_eq!(inst.opcode, X86Opcode::Add);
}

#[test]
fn select_program_records_x86_condition_and_call_target() {
    let lir = LirBlob {
        data_layout: layout(),
        functions: vec![LirFunction {
            def_id: None,
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::Eq(i32_value(1), i32_value(2)),
                        result: Some(LirRegister {
                            id: 1,
                            ty: LirType::I1,
                        }),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 2,
                        kind: LirInstructionKind::Call {
                            function: LirValue::function(
                                fp_core::lir::LirFunctionRef::Name(Name::new("callee")),
                                LirType::Ptr(Box::new(LirType::I8)),
                            ),
                            args: Vec::new(),
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        result: Some(LirRegister {
                            id: 2,
                            ty: LirType::I32,
                        }),
                        debug_info: None,
                    },
                ],
                terminator: LirTerminator::Return(Some(reg(2, LirType::I32))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::External,
            is_declaration: false,
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),

        queries: Vec::new(),
    };

    let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
    let x86 = lower_to_x86_64(&program);
    let eq_inst = &x86.functions[0].blocks[0].instructions[0];
    let call_inst = &x86.functions[0].blocks[0].instructions[1];

    assert!(matches!(eq_inst.condition, Some(X86ConditionCode::Equal)));
    assert!(matches!(
        call_inst.call_target,
        Some(X86CallTarget::Symbol(ref name)) if *name == Name::new("callee")
    ));
}

#[test]
fn lower_to_aarch64_preserves_concrete_branch_and_call_metadata() {
    let lir = LirBlob {
        data_layout: layout(),
        functions: vec![LirFunction {
            def_id: None,
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![
                LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: vec![
                        LirInstruction {
                            id: 1,
                            kind: LirInstructionKind::Eq(i32_value(1), i32_value(2)),
                            result: Some(LirRegister {
                                id: 1,
                                ty: LirType::I1,
                            }),
                            debug_info: None,
                        },
                        LirInstruction {
                            id: 2,
                            kind: LirInstructionKind::Call {
                                function: LirValue::function(
                                    fp_core::lir::LirFunctionRef::Name(Name::new("callee")),
                                    LirType::Ptr(Box::new(LirType::I8)),
                                ),
                                args: Vec::new(),
                                calling_convention: CallingConvention::C,
                                tail_call: false,
                            },
                            result: Some(LirRegister {
                                id: 2,
                                ty: LirType::I32,
                            }),
                            debug_info: None,
                        },
                    ],
                    terminator: LirTerminator::Br(1),
                    predecessors: Vec::new(),
                    successors: vec![1],
                },
                LirBasicBlock {
                    id: 1,
                    label: Some(Name::new("exit")),
                    instructions: Vec::new(),
                    terminator: LirTerminator::Return(Some(reg(2, LirType::I32))),
                    predecessors: vec![0],
                    successors: Vec::new(),
                },
            ],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::External,
            is_declaration: false,
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),

        queries: Vec::new(),
    };

    let program = select_program(&lir, TargetFormat::Elf, TargetArch::Aarch64).unwrap();
    let aarch64 = lower_to_aarch64(&program);
    let eq_inst = &aarch64.functions[0].blocks[0].instructions[0];
    let call_inst = &aarch64.functions[0].blocks[0].instructions[1];
    let terminator = &aarch64.functions[0].blocks[0].terminator;

    assert_eq!(eq_inst.opcode, "cmp");
    assert_eq!(eq_inst.condition, Some(Aarch64ConditionCode::Eq));
    assert!(matches!(
        call_inst.call_target,
        Some(Aarch64CallTarget::Symbol(ref name)) if *name == Name::new("callee")
    ));
    assert_eq!(terminator.opcode, Aarch64TerminatorOpcode::B);
    assert_eq!(terminator.targets, vec![1]);
}

#[test]
fn lower_to_x86_64_skips_declarations_and_maps_terminators() {
    let lir = LirBlob {
        data_layout: layout(),
        functions: vec![
            LirFunction {
                def_id: None,
                name: Name::new("decl"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: Vec::new(),
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: true,
            },
            LirFunction {
                def_id: None,
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: Vec::new(),
                    terminator: LirTerminator::CondBr {
                        condition: reg(1, LirType::I1),
                        if_true: 1,
                        if_false: 2,
                    },
                    predecessors: Vec::new(),
                    successors: vec![1, 2],
                }],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            },
        ],
        globals: Vec::new(),
        type_definitions: Vec::new(),

        queries: Vec::new(),
    };

    let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
    let x86 = lower_to_x86_64(&program);

    assert_eq!(x86.functions.len(), 1);
    assert_eq!(x86.functions[0].name, Name::new("main"));
    assert_eq!(
        x86.functions[0].blocks[0].terminator.opcode,
        X86TerminatorOpcode::Jcc
    );
    assert_eq!(x86.functions[0].blocks[0].terminator.targets, vec![1, 2]);
}

#[test]
fn lift_from_x86_64_roundtrips_through_asmir() {
    let x86 = AsmX86_64Program {
        functions: vec![AsmX86_64Function {
            name: Name::new("main"),
            blocks: vec![AsmX86_64Block {
                id: 0,
                instructions: vec![X86InstructionDetail {
                    opcode: X86Opcode::Add,
                    operands: vec![
                        X86Operand::Register {
                            reg: X86Register::Virtual {
                                id: 1,
                                size_bits: 64,
                            },
                            access: OperandAccess::Write,
                        },
                        X86Operand::Register {
                            reg: X86Register::Virtual {
                                id: 2,
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        },
                        X86Operand::Immediate(4),
                    ],
                    condition: None,
                    call_target: None,
                }],
                terminator: X86TerminatorDetail {
                    opcode: X86TerminatorOpcode::Jcc,
                    condition: Some(X86ConditionCode::NotEqual),
                    targets: vec![1, 2],
                },
            }],
        }],
    };

    let asmir = lift_from_x86_64(&x86).unwrap();
    let lowered = lower_to_x86_64(&asmir);

    let lowered_inst = &lowered.functions[0].blocks[0].instructions[0];
    let original_inst = &x86.functions[0].blocks[0].instructions[0];
    assert_eq!(lowered_inst.opcode, original_inst.opcode);
    assert_eq!(lowered_inst.operands[1..], original_inst.operands[1..]);
    assert!(matches!(
        &asmir.functions[0].basic_blocks[0].terminator,
        AsmTerminator::CondBr {
            condition: AsmValue::Condition(AsmConditionCode::Ne),
            if_true: 1,
            if_false: 2,
        }
    ));
    assert_eq!(
        lowered.functions[0].blocks[0].terminator,
        x86.functions[0].blocks[0].terminator
    );
}

#[test]
fn lift_from_aarch64_roundtrips_through_asmir() {
    let aarch64 = AsmAarch64Program {
        functions: vec![AsmAarch64Function {
            name: Name::new("main"),
            blocks: vec![AsmAarch64Block {
                id: 0,
                instructions: vec![Aarch64InstructionDetail {
                    opcode: "add".to_string(),
                    operands: vec![
                        Aarch64Operand::Register {
                            reg: Aarch64Register::Virtual {
                                id: 1,
                                size_bits: 64,
                            },
                            access: OperandAccess::Write,
                        },
                        Aarch64Operand::Register {
                            reg: Aarch64Register::Virtual {
                                id: 2,
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        },
                        Aarch64Operand::Immediate(7),
                    ],
                    condition: None,
                    call_target: None,
                }],
                terminator: Aarch64TerminatorDetail {
                    opcode: Aarch64TerminatorOpcode::BCond,
                    condition: Some(Aarch64ConditionCode::Ge),
                    targets: vec![1, 2],
                },
            }],
        }],
    };

    let asmir = lift_from_aarch64(&aarch64).unwrap();
    let lowered = lower_to_aarch64(&asmir);

    let lowered_inst = &lowered.functions[0].blocks[0].instructions[0];
    let original_inst = &aarch64.functions[0].blocks[0].instructions[0];
    assert_eq!(lowered_inst.opcode, original_inst.opcode);
    assert_eq!(lowered_inst.operands[1..], original_inst.operands[1..]);
    assert!(matches!(
        &asmir.functions[0].basic_blocks[0].terminator,
        AsmTerminator::CondBr {
            condition: AsmValue::Condition(AsmConditionCode::Ge),
            if_true: 1,
            if_false: 2,
        }
    ));
    assert_eq!(
        lowered.functions[0].blocks[0].terminator,
        aarch64.functions[0].blocks[0].terminator
    );
}

#[test]
fn lift_from_x86_64_links_compare_instruction_into_branch_condition() {
    let x86 = AsmX86_64Program {
        functions: vec![AsmX86_64Function {
            name: Name::new("main"),
            blocks: vec![AsmX86_64Block {
                id: 0,
                instructions: vec![X86InstructionDetail {
                    opcode: X86Opcode::Cmp,
                    operands: vec![
                        X86Operand::Register {
                            reg: X86Register::Virtual {
                                id: 2,
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        },
                        X86Operand::Immediate(4),
                    ],
                    condition: Some(X86ConditionCode::NotEqual),
                    call_target: None,
                }],
                terminator: X86TerminatorDetail {
                    opcode: X86TerminatorOpcode::Jcc,
                    condition: Some(X86ConditionCode::NotEqual),
                    targets: vec![1, 2],
                },
            }],
        }],
    };

    let asmir = lift_from_x86_64(&x86).unwrap();
    assert!(matches!(
        &asmir.functions[0].basic_blocks[0].terminator,
        AsmTerminator::CondBr {
            condition: AsmValue::Flags(0),
            if_true: 1,
            if_false: 2,
        }
    ));
}

#[test]
fn lift_from_aarch64_links_compare_instruction_into_branch_condition() {
    let aarch64 = AsmAarch64Program {
        functions: vec![AsmAarch64Function {
            name: Name::new("main"),
            blocks: vec![AsmAarch64Block {
                id: 0,
                instructions: vec![Aarch64InstructionDetail {
                    opcode: "cmp.ge".to_string(),
                    operands: vec![
                        Aarch64Operand::Register {
                            reg: Aarch64Register::Virtual {
                                id: 2,
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        },
                        Aarch64Operand::Immediate(7),
                    ],
                    condition: Some(Aarch64ConditionCode::Ge),
                    call_target: None,
                }],
                terminator: Aarch64TerminatorDetail {
                    opcode: Aarch64TerminatorOpcode::BCond,
                    condition: Some(Aarch64ConditionCode::Ge),
                    targets: vec![1, 2],
                },
            }],
        }],
    };

    let asmir = lift_from_aarch64(&aarch64).unwrap();
    assert!(matches!(
        &asmir.functions[0].basic_blocks[0].terminator,
        AsmTerminator::CondBr {
            condition: AsmValue::Flags(0),
            if_true: 1,
            if_false: 2,
        }
    ));
}

#[test]
fn lift_from_x86_64_preserves_indirect_calls_and_addressing_shapes() {
    let x86 = AsmX86_64Program {
        functions: vec![AsmX86_64Function {
            name: Name::new("main"),
            blocks: vec![AsmX86_64Block {
                id: 0,
                instructions: vec![
                    X86InstructionDetail {
                        opcode: X86Opcode::Mov,
                        operands: vec![
                            X86Operand::Register {
                                reg: X86Register::Physical {
                                    name: "rax".to_string(),
                                    size_bits: 64,
                                },
                                access: OperandAccess::Write,
                            },
                            X86Operand::Memory(crate::asm::x86_64::X86MemoryOperand {
                                base: Some(X86Register::Physical {
                                    name: "rbx".to_string(),
                                    size_bits: 64,
                                }),
                                index: Some(X86Register::Physical {
                                    name: "rcx".to_string(),
                                    size_bits: 64,
                                }),
                                scale: 2,
                                displacement: 8,
                                size_bytes: Some(8),
                            }),
                        ],
                        condition: None,
                        call_target: None,
                    },
                    X86InstructionDetail {
                        opcode: X86Opcode::Call,
                        operands: vec![X86Operand::Register {
                            reg: X86Register::Physical {
                                name: "rax".to_string(),
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        }],
                        condition: None,
                        call_target: Some(X86CallTarget::Register(X86Register::Physical {
                            name: "rax".to_string(),
                            size_bits: 64,
                        })),
                    },
                ],
                terminator: X86TerminatorDetail {
                    opcode: X86TerminatorOpcode::Ret,
                    condition: None,
                    targets: Vec::new(),
                },
            }],
        }],
    };

    let asmir = lift_from_x86_64(&x86).unwrap();
    let lowered = lower_to_x86_64(&asmir);

    let instructions = &lowered.functions[0].blocks[0].instructions;
    assert_eq!(instructions.len(), 2);

    let X86InstructionDetail {
        operands: mov_operands,
        ..
    } = &instructions[0];
    let [
        X86Operand::Register {
            reg: X86Register::Virtual { id: dst_id, .. },
            ..
        },
        X86Operand::Memory(mem),
    ] = mov_operands.as_slice()
    else {
        panic!("unexpected x86 mov operands: {mov_operands:?}");
    };
    assert_eq!(mem.scale, 2);
    assert_eq!(mem.displacement, 8);
    assert_eq!(mem.size_bytes, Some(8));
    assert!(matches!(mem.base, Some(X86Register::Virtual { .. })));
    assert!(matches!(mem.index, Some(X86Register::Virtual { .. })));

    let X86InstructionDetail {
        operands: call_operands,
        call_target,
        ..
    } = &instructions[1];
    let [
        X86Operand::Register {
            reg: X86Register::Virtual { id: call_id, .. },
            ..
        },
    ] = call_operands.as_slice()
    else {
        panic!("unexpected x86 call operands: {call_operands:?}");
    };
    assert_eq!(dst_id, call_id);
    assert!(matches!(
        call_target,
        Some(X86CallTarget::Register(X86Register::Virtual { id, .. })) if *id == *call_id
    ));
}

#[test]
fn lift_from_aarch64_preserves_indirect_calls_and_addressing_shapes() {
    let aarch64 = AsmAarch64Program {
        functions: vec![AsmAarch64Function {
            name: Name::new("main"),
            blocks: vec![AsmAarch64Block {
                id: 0,
                instructions: vec![
                    Aarch64InstructionDetail {
                        opcode: "str".to_string(),
                        operands: vec![
                            Aarch64Operand::Register {
                                reg: Aarch64Register::Physical {
                                    name: "x3".to_string(),
                                    size_bits: 64,
                                },
                                access: OperandAccess::Read,
                            },
                            Aarch64Operand::Memory(crate::asm::aarch64::Aarch64MemoryOperand {
                                base: Some(Aarch64Register::Physical {
                                    name: "x1".to_string(),
                                    size_bits: 64,
                                }),
                                index: Some(Aarch64Register::Physical {
                                    name: "x2".to_string(),
                                    size_bits: 64,
                                }),
                                scale: 3,
                                displacement: 16,
                                size_bytes: Some(8),
                            }),
                        ],
                        condition: None,
                        call_target: None,
                    },
                    Aarch64InstructionDetail {
                        opcode: "bl".to_string(),
                        operands: vec![Aarch64Operand::Register {
                            reg: Aarch64Register::Physical {
                                name: "x0".to_string(),
                                size_bits: 64,
                            },
                            access: OperandAccess::Read,
                        }],
                        condition: None,
                        call_target: Some(Aarch64CallTarget::Register(Aarch64Register::Physical {
                            name: "x0".to_string(),
                            size_bits: 64,
                        })),
                    },
                ],
                terminator: Aarch64TerminatorDetail {
                    opcode: Aarch64TerminatorOpcode::Ret,
                    condition: None,
                    targets: Vec::new(),
                },
            }],
        }],
    };

    let asmir = lift_from_aarch64(&aarch64).unwrap();
    let lowered = lower_to_aarch64(&asmir);

    let instructions = &lowered.functions[0].blocks[0].instructions;
    assert_eq!(instructions.len(), 2);

    let Aarch64InstructionDetail {
        operands: store_operands,
        ..
    } = &instructions[0];
    let [
        Aarch64Operand::Register {
            reg: Aarch64Register::Virtual { .. },
            ..
        },
        Aarch64Operand::Memory(mem),
    ] = store_operands.as_slice()
    else {
        panic!("unexpected aarch64 store operands: {store_operands:?}");
    };
    assert_eq!(mem.scale, 3);
    assert_eq!(mem.displacement, 16);
    assert_eq!(mem.size_bytes, Some(8));
    assert!(matches!(mem.base, Some(Aarch64Register::Virtual { .. })));
    assert!(matches!(mem.index, Some(Aarch64Register::Virtual { .. })));

    let Aarch64InstructionDetail {
        operands: call_operands,
        call_target,
        ..
    } = &instructions[1];
    let [
        Aarch64Operand::Register {
            reg: Aarch64Register::Virtual { id: call_id, .. },
            ..
        },
    ] = call_operands.as_slice()
    else {
        panic!("unexpected aarch64 call operands: {call_operands:?}");
    };
    assert!(matches!(
        call_target,
        Some(Aarch64CallTarget::Register(Aarch64Register::Virtual { id, .. })) if *id == *call_id
    ));
}
