use fp_core::asmir::{
    AsmArchitecture, AsmAttr, AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature,
    AsmGenericOpcode, AsmGlobal, AsmInstruction, AsmObjectFormat, AsmOpcode, AsmOperand,
    AsmProgram, AsmRegister, AsmRegisterBank, AsmSection, AsmSectionFlag, AsmSectionKind,
    AsmTarget, AsmTerminator, AsmType, OperandAccess,
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
fn normalize_materializes_printf_format_strings_from_elf_rodata() {
    let mut program = AsmProgram::new(
        AsmTarget {
            architecture: AsmArchitecture::Aarch64,
            object_format: AsmObjectFormat::MachO,
            endianness: fp_core::asmir::AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: Some(CallingConvention::C),
        },
        layout(),
    );
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
    let mut function = AsmFunction::new(
        Name::new("fp_lifted_main"),
        AsmFunctionSignature {
            params: Vec::new(),
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    let v1 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    function.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![
            AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                vec![
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(v0),
                        access: OperandAccess::Write,
                    },
                    AsmOperand::Constant(AsmConstant::GlobalRef(
                        Name::new("fp_elf_rodata_0"),
                        ptr_i8.clone(),
                        vec![0],
                    )),
                ],
            ),
            AsmInstruction::new(
                1,
                AsmOpcode::Generic(AsmGenericOpcode::Add),
                vec![
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(v1),
                        access: OperandAccess::Write,
                    },
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(v0),
                        access: OperandAccess::Read,
                    },
                    AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
                ],
            ),
            AsmInstruction::new(
                2,
                AsmOpcode::Generic(AsmGenericOpcode::Call),
                vec![
                    AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                    AsmOperand::Symbol(Name::new("printf")),
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(v1),
                        access: OperandAccess::Read,
                    },
                ],
            ),
        ],
        terminator: AsmTerminator::Return(Some(AsmOperand::Constant(AsmConstant::Int(
            0,
            AsmType::I32,
        )))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    function.linkage = Linkage::External;
    function.visibility = Visibility::Default;
    function.calling_convention = Some(CallingConvention::C);
    function.section = Some(".text".to_string());
    function.is_declaration = false;
    program.functions.push(function);

    fp_native::libc::normalize(&mut program);

    let call = &program.functions[0].basic_blocks[0].instructions[2];
    let (_, args) = call.call_target_and_args().expect("expected call");
    assert!(
        matches!(args[0], AsmOperand::Constant(AsmConstant::String(ref s)) if s == "hello %s")
    );
}
