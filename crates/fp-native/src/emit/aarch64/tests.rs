use super::emit_text_from_asmir;
use crate::emit::TargetFormat;
use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmConstant, AsmEndianness, AsmFunction, AsmFunctionSignature,
    AsmGenericOpcode, AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode, AsmProgram,
    AsmTarget, AsmTerminator, AsmType, AsmValue,
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
fn aarch64_emitter_accepts_minimal_asmir_program() {
    let output = emit_text_from_asmir(&minimal_program(), TargetFormat::Elf).unwrap();
    assert!(!output.text.is_empty());
}

#[test]
fn aarch64_emitter_supports_unsigned_compares() {
    let output = emit_text_from_asmir(&unsigned_compare_program(), TargetFormat::Elf).unwrap();
    assert!(!output.text.is_empty());
}

fn minimal_program() -> AsmProgram {
    AsmProgram {
        target: AsmTarget {
            architecture: AsmArchitecture::Aarch64,
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
        type_definitions: Vec::new(),
        physical_register_types: std::collections::HashMap::new(),
    }
}

fn unsigned_compare_program() -> AsmProgram {
    AsmProgram {
        target: AsmTarget {
            architecture: AsmArchitecture::Aarch64,
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
        functions: vec![AsmFunction {
            name: Name::new("main"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::I1,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![AsmInstruction {
                    id: 0,
                    kind: AsmInstructionKind::Ugt(
                        AsmValue::Constant(AsmConstant::Int(1, AsmType::I64)),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ),
                    ty: AsmType::I1,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Ugt),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: AsmTerminator::Return(Some(AsmValue::Register(0))),
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
        type_definitions: Vec::new(),
        physical_register_types: std::collections::HashMap::new(),
    }
}
