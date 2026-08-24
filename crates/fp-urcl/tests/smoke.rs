use fp_core::lir::{
    CallingConvention, Linkage, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
    LirInstruction, LirInstructionKind, LirInteger, LirBlob, LirRegister, LirTerminator,
    LirType, LirValue, Name,
};

fn i64_value(value: u64) -> LirValue {
    LirValue::constant(LirConstant::integer(LirType::I64, LirInteger::I64(value)).unwrap())
}

fn sample_program() -> LirBlob {
    let add_inst = LirInstruction {
        id: 1,
        kind: LirInstructionKind::Add(i64_value(40), i64_value(2)),
        result: Some(LirRegister {
            id: 1,
            ty: LirType::I64,
        }),
        debug_info: None,
    };
    let main = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: vec![add_inst],
            terminator: LirTerminator::Return(Some(LirValue::register(1, LirType::I64))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
        is_declaration: false,
    };
    LirBlob {
        functions: vec![main],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: fp_core::lir::LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .unwrap(),
        queries: Vec::new(),
    }
}

fn compare_program() -> LirBlob {
    let cmp_inst = LirInstruction {
        id: 1,
        kind: LirInstructionKind::Eq(i64_value(1), i64_value(1)),
        result: Some(LirRegister {
            id: 1,
            ty: LirType::I1,
        }),
        debug_info: None,
    };
    let main = LirFunction {
        def_id: None,
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: vec![
            LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![cmp_inst],
                terminator: LirTerminator::CondBr {
                    condition: LirValue::register(1, LirType::I1),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            },
            LirBasicBlock {
                id: 1,
                label: Some(Name::new("then")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(Some(i64_value(1))),
                predecessors: vec![0],
                successors: Vec::new(),
            },
            LirBasicBlock {
                id: 2,
                label: Some(Name::new("else")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(Some(i64_value(0))),
                predecessors: vec![0],
                successors: Vec::new(),
            },
        ],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
        is_declaration: false,
    };
    LirBlob {
        functions: vec![main],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        data_layout: fp_core::lir::LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .unwrap(),
        queries: Vec::new(),
    }
}

#[test]
fn emits_urcl_text() {
    let out_dir = tempfile::tempdir().unwrap();
    let path = out_dir.path().join("main.urcl");
    let cfg = fp_urcl::UrclConfig::new(&path);
    fp_urcl::UrclEmitter::new(cfg)
        .emit(sample_program(), None)
        .unwrap();
    let text = std::fs::read_to_string(&path).unwrap();
    assert!(text.contains("BITS 64"));
    assert!(text.contains(".function main"));
    assert!(text.contains("ADD R3, 40, 2"));
}

#[test]
fn urcl_compare_lowers_to_labels_and_branches() {
    let out_dir = tempfile::tempdir().unwrap();
    let path = out_dir.path().join("compare.urcl");
    let cfg = fp_urcl::UrclConfig::new(&path);
    fp_urcl::UrclEmitter::new(cfg)
        .emit(compare_program(), None)
        .unwrap();
    let text = std::fs::read_to_string(&path).unwrap();
    assert!(text.contains("BRE 1, 1, .L1_cmp_true"));
    assert!(text.contains(".L1_cmp_true:"));
    assert!(text.contains("BNZ R3, main_then"));
    assert!(text.contains("JMP main_else"));
}
