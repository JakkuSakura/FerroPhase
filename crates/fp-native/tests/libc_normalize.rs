use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGenericOpcode,
    AsmGlobal, AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode, AsmProgram,
    AsmSection, AsmSectionFlag, AsmSectionKind, AsmTarget, AsmTerminator, AsmType, AsmValue,
};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

#[test]
fn normalize_materializes_printf_format_strings_from_elf_rodata() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    program.globals.push(AsmGlobal {
        name: Name::new("fp_elf_rodata_0"),
        ty: AsmType::Array(Box::new(AsmType::I8), 16),
        initializer: Some(AsmConstant::Bytes(b"hello %s\0rest\0".to_vec())),
        relocations: Vec::new(),
        section: Some(".rodata".to_string()),
        linkage: Linkage::Internal,
        visibility: Visibility::Default,
        alignment: Some(1),
        is_constant: true,
    });

    // v0 = &fp_elf_rodata_0
    // v1 = v0 + 0
    // call printf(v1)
    program.functions.push(AsmFunction {
        name: Name::new("fp_lifted_main"),
        signature: AsmFunctionSignature {
            params: Vec::new(),
            return_type: AsmType::I32,
            is_variadic: false,
        },
        basic_blocks: vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::GlobalRef(
                        Name::new("fp_elf_rodata_0"),
                        AsmType::Ptr(Box::new(AsmType::I8)),
                        vec![0],
                    ))),
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 1,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Add),
                    kind: AsmInstructionKind::Add(
                        AsmValue::Register(0),
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                    ),
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 2,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("printf".to_string()),
                        args: vec![AsmValue::Register(1)],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Void),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
            ],
            terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::Int(
                0,
                AsmType::I32,
            )))),
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
    });

    fp_native::libc::normalize(&mut program);

    let call = &program.functions[0].basic_blocks[0].instructions[2];
    let AsmInstructionKind::Call { args, .. } = &call.kind else {
        panic!("expected call");
    };
    assert!(matches!(args[0], AsmValue::Constant(AsmConstant::String(ref s)) if s == "hello %s"));
}
