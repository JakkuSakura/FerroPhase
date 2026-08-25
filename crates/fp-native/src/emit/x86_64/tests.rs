use super::{AbiPassMode, abi_pass_mode, emit_text_from_asmir, is_aggregate_storage};
use crate::emit::TargetFormat;
use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmObjectFormat,
    AsmProgram, AsmTarget, AsmTerminator, AsmType,
};
use fp_core::lir::{CallingConvention, Linkage, LirDataLayout, Name, Visibility};

fn layout() -> LirDataLayout {
    LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid test data layout")
}

#[test]
fn x86_64_emitter_rejects_mismatched_asmir_architecture() {
    let error = emit_text_from_asmir(
        &AsmProgram::new(
            AsmTarget {
                architecture: AsmArchitecture::Aarch64,
                object_format: AsmObjectFormat::Elf,
                endianness: AsmEndianness::Little,
                pointer_width: 64,
                default_calling_convention: None,
            },
            layout(),
        ),
        TargetFormat::Elf,
    )
    .err()
    .expect("expected architecture mismatch to fail");

    assert!(
        error.to_string().contains("requires x86_64 AsmIR input"),
        "unexpected error: {error}"
    );
}

#[test]
fn x86_64_emitter_accepts_minimal_asmir_program() {
    let output = emit_text_from_asmir(&minimal_program(), TargetFormat::Elf).unwrap();
    assert!(!output.text.is_empty());
}

#[test]
fn sysv_classifies_two_word_aggregate_as_pair_without_storage_threshold() {
    let point = AsmType::Struct {
        fields: vec![AsmType::I64, AsmType::I64],
        packed: false,
        name: None,
    };
    assert_eq!(abi_pass_mode(&point, &layout()).unwrap(), AbiPassMode::Pair);
    assert!(is_aggregate_storage(&point, &layout()));
}

#[test]
fn sysv_classifies_larger_aggregate_as_indirect() {
    let value = AsmType::Struct {
        fields: vec![AsmType::I64, AsmType::I64, AsmType::I64],
        packed: false,
        name: None,
    };
    assert_eq!(
        abi_pass_mode(&value, &layout()).unwrap(),
        AbiPassMode::Indirect
    );
}

fn minimal_program() -> AsmProgram {
    AsmProgram {
        target: AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: AsmObjectFormat::Elf,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        },
        data_layout: layout(),
        lifted_from: None,
        container: None,
        sections: Vec::new(),
        globals: Vec::new(),
        type_definitions: Vec::new(),
        physical_register_types: std::collections::HashMap::new(),
        functions: vec![AsmFunction {
            name: Name::new("main"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(None),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        }],
    }
}
