use super::{emit_assembly, emit_object, read_object_metadata, validate_program};
use fp_core::lir::{
    CallingConvention, Linkage, LirBasicBlock, LirBlob, LirConstant, LirFunction,
    LirFunctionSignature, LirInstruction, LirInstructionKind, LirInteger, LirLocal, LirRegister,
    LirTerminator, LirType, LirValue,
};

fn i64_value(value: u64) -> LirValue {
    LirValue::constant(LirConstant::integer(LirType::I64, LirInteger::I64(value)).unwrap())
}

fn local(id: u32) -> LirValue {
    LirValue::local(id, LirType::I64)
}

fn register(id: u32) -> LirValue {
    LirValue::register(id, LirType::I64)
}

fn data_layout() -> fp_core::lir::LirDataLayout {
    fp_core::lir::LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .unwrap()
}
use object::read::{File, Object as _, ObjectSection as _, ObjectSymbol as _, RelocationTarget};

fn base_function(name: &str) -> LirFunction {
    LirFunction {
        def_id: None,
        name: fp_core::lir::Name::new(name),
        signature: LirFunctionSignature {
            params: vec![LirType::I64, LirType::I64],
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: Vec::new(),
        locals: vec![
            LirLocal {
                id: 0,
                ty: LirType::I64,
                name: Some("lhs".to_string()),
                is_argument: true,
            },
            LirLocal {
                id: 1,
                ty: LirType::I64,
                name: Some("rhs".to_string()),
                is_argument: true,
            },
        ],
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
        is_declaration: false,
    }
}

fn addition_program() -> LirBlob {
    let mut function = base_function("main");
    function.basic_blocks.push(LirBasicBlock {
        id: 0,
        label: Some(fp_core::lir::Name::new("entry")),
        instructions: vec![LirInstruction {
            id: 1,
            kind: LirInstructionKind::Add(local(0), local(1)),
            result: Some(LirRegister {
                id: 1,
                ty: LirType::I64,
            }),
            debug_info: None,
        }],
        terminator: LirTerminator::Return(Some(register(1))),
        predecessors: Vec::new(),
        successors: Vec::new(),
    });

    LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),
        queries: Vec::new(),
    }
}

#[test]
fn emits_addition_with_argument_spills() {
    let assembly = emit_assembly(&addition_program()).unwrap();
    assert!(assembly.contains("*(u64 *)(r10 -8) = r1"));
    assert!(assembly.contains("*(u64 *)(r10 -16) = r2"));
    assert!(assembly.contains("r6 = *(u64 *)(r10 -8)"));
    assert!(assembly.contains("r7 = *(u64 *)(r10 -16)"));
    assert!(assembly.contains("r6 += r7"));
    assert!(assembly.contains("r0 = *(u64 *)(r10 -24)"));
    assert!(assembly.contains("exit"));
}

#[test]
fn emits_compare_and_branch() {
    let mut function = base_function("branchy");
    function.basic_blocks = vec![
        LirBasicBlock {
            id: 0,
            label: Some(fp_core::lir::Name::new("entry")),
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::Eq(local(0), i64_value(0)),
                result: Some(LirRegister {
                    id: 1,
                    ty: LirType::I1,
                }),
                debug_info: None,
            }],
            terminator: LirTerminator::CondBr {
                condition: register(1),
                if_true: 1,
                if_false: 2,
            },
            predecessors: Vec::new(),
            successors: vec![1, 2],
        },
        LirBasicBlock {
            id: 1,
            label: Some(fp_core::lir::Name::new("then")),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(Some(i64_value(1))),
            predecessors: vec![0],
            successors: Vec::new(),
        },
        LirBasicBlock {
            id: 2,
            label: Some(fp_core::lir::Name::new("else")),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(Some(i64_value(2))),
            predecessors: vec![0],
            successors: Vec::new(),
        },
    ];

    let program = LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),

        queries: Vec::new(),
    };

    let assembly = emit_assembly(&program).unwrap();
    assert!(assembly.contains("if r6 == 0 goto .Lbranchy_cmp_true_0"));
    assert!(assembly.contains("if r6 != 0 goto LBB1"));
    assert!(assembly.contains("goto LBB2"));
}

#[test]
fn validates_unsupported_call() {
    let mut function = base_function("bad");
    function.basic_blocks.push(LirBasicBlock {
        id: 0,
        label: None,
        instructions: vec![LirInstruction {
            id: 1,
            kind: LirInstructionKind::Call {
                function: LirValue::global(
                    fp_core::lir::Name::new("helper"),
                    LirType::Ptr(Box::new(LirType::I8)),
                ),
                args: Vec::new(),
                calling_convention: CallingConvention::C,
                tail_call: false,
            },
            result: Some(LirRegister {
                id: 1,
                ty: LirType::I64,
            }),
            debug_info: None,
        }],
        terminator: LirTerminator::Return(None),
        predecessors: Vec::new(),
        successors: Vec::new(),
    });
    let program = LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),

        queries: Vec::new(),
    };

    let err = validate_program(&program).unwrap_err().to_string();
    assert!(err.contains("calls are not supported"));
}

#[test]
fn validates_format_intrinsic_as_unsupported() {
    let mut function = base_function("bad_format");
    function.basic_blocks.push(LirBasicBlock {
        id: 0,
        label: None,
        instructions: vec![LirInstruction {
            id: 1,
            kind: LirInstructionKind::IntrinsicCall {
                kind: fp_core::lir::LirIntrinsicKind::Format,
                format: "value={}".to_string(),
                args: vec![i64_value(1)],
            },
            result: Some(LirRegister {
                id: 1,
                ty: LirType::I64,
            }),
            debug_info: None,
        }],
        terminator: LirTerminator::Return(None),
        predecessors: Vec::new(),
        successors: Vec::new(),
    });
    let program = LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),

        queries: Vec::new(),
    };

    let err = validate_program(&program).unwrap_err().to_string();
    assert!(err.contains("Format is not supported"));
}

#[test]
fn emits_runtime_helper_metadata_sections() {
    let mut function = base_function("main");
    function.basic_blocks.push(LirBasicBlock {
        id: 0,
        label: Some(fp_core::lir::Name::new("entry")),
        instructions: vec![LirInstruction {
            id: 1,
            kind: LirInstructionKind::IntrinsicCall {
                kind: fp_core::lir::LirIntrinsicKind::Println,
                format: "value={}".to_string(),
                args: vec![i64_value(7)],
            },
            result: None,
            debug_info: None,
        }],
        terminator: LirTerminator::Return(Some(i64_value(0))),
        predecessors: Vec::new(),
        successors: Vec::new(),
    });
    let program = LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),

        queries: Vec::new(),
    };

    let bytes = emit_object(&program).unwrap();
    let metadata = read_object_metadata(&bytes).unwrap();
    let file = File::parse(&*bytes).unwrap();
    assert!(file.section_by_name(".fp.ebpf.abi").is_some());
    assert!(file.section_by_name(".fp.ebpf.helpers").is_some());
    assert!(file.section_by_name(".fp.ebpf.fmt").is_some());
    assert!(file.section_by_name(".fp.ebpf.calls").is_some());
    assert!(
        file.symbols()
            .any(|symbol| symbol.name() == Ok("__fp_helper_println"))
    );

    let abi = file
        .section_by_name(".fp.ebpf.abi")
        .unwrap()
        .data()
        .unwrap();
    let abi = std::str::from_utf8(abi).unwrap();
    assert!(abi.contains("helper.println=3"));
    assert!(abi.contains("helper.println.symbol=__fp_helper_println"));

    let calls = file
        .section_by_name(".fp.ebpf.calls")
        .unwrap()
        .data()
        .unwrap();
    assert!(calls.windows(b"main".len()).any(|window| window == b"main"));
    assert!(
        calls
            .windows(b"__fp_helper_println".len())
            .any(|window| window == b"__fp_helper_println")
    );
    assert_eq!(metadata.helpers.len(), 3);
    assert_eq!(metadata.formats.len(), 1);
    assert_eq!(metadata.callsites.len(), 1);
    assert_eq!(metadata.callsites[0].function, "main");
    assert_eq!(metadata.callsites[0].helper_symbol, "__fp_helper_println");

    let program_section = file.section_by_name("prog/main").unwrap();
    let relocations: Vec<_> = program_section.relocations().collect();
    assert_eq!(relocations.len(), 1);
    let (_, relocation) = &relocations[0];
    let RelocationTarget::Symbol(symbol_index) = relocation.target() else {
        panic!("expected symbol relocation");
    };
    let symbol = file.symbol_by_index(symbol_index).unwrap();
    assert_eq!(symbol.name(), Ok("__fp_helper_println"));

    let program_bytes = program_section.data().unwrap();
    let call_offset = metadata.callsites[0].offset as usize;
    assert_eq!(
        i32::from_le_bytes(
            program_bytes[call_offset + 4..call_offset + 8]
                .try_into()
                .unwrap()
        ),
        0
    );
}

#[test]
fn emits_helper_metadata_in_text_assembly() {
    let mut function = base_function("main");
    function.basic_blocks.push(LirBasicBlock {
        id: 0,
        label: Some(fp_core::lir::Name::new("entry")),
        instructions: vec![LirInstruction {
            id: 1,
            kind: LirInstructionKind::IntrinsicCall {
                kind: fp_core::lir::LirIntrinsicKind::TimeNow,
                format: String::new(),
                args: Vec::new(),
            },
            result: Some(LirRegister {
                id: 1,
                ty: LirType::I64,
            }),
            debug_info: None,
        }],
        terminator: LirTerminator::Return(Some(register(1))),
        predecessors: Vec::new(),
        successors: Vec::new(),
    });
    let program = LirBlob {
        functions: vec![function],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: data_layout(),

        queries: Vec::new(),
    };

    let assembly = emit_assembly(&program).unwrap();
    assert!(assembly.contains("helper.time_now=1 symbol=__fp_helper_time_now"));
    assert!(assembly.contains("call helper 1 ; time_now (__fp_helper_time_now)"));
}

#[test]
fn emits_elf_object() {
    let bytes = emit_object(&addition_program()).unwrap();
    let file = File::parse(&*bytes).unwrap();
    assert!(file.section_by_name("prog/main").is_some());
    assert!(file.section_by_name("license").is_some());
    assert!(file.symbols().any(|symbol| symbol.name() == Ok("main")));
}
