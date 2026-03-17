use fp_core::asmir::{
    AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGlobal, AsmInstruction,
    AsmInstructionKind, AsmObjectFormat, AsmOpcode, AsmProgram, AsmSection, AsmSectionFlag,
    AsmSectionKind, AsmSymbolAddressKind, AsmTarget, AsmTerminator, AsmType, AsmValue,
};
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind,
};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

#[test]
fn materialize_maps_stderr_to_darwin_global() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
    program.globals.push(AsmGlobal {
        name: Name::new("stderr"),
        ty: AsmType::Ptr(Box::new(AsmType::I8)),
        initializer: Some(AsmConstant::UInt(0, AsmType::I64)),
        relocations: Vec::new(),
        section: Some(".data".to_string()),
        linkage: Linkage::External,
        visibility: Visibility::Default,
        alignment: Some(8),
        is_constant: false,
    });
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
            instructions: vec![AsmInstruction {
                id: 0,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Global("stderr".to_string(), AsmType::Ptr(Box::new(AsmType::I8))),
                    alignment: None,
                    volatile: false,
                },
                type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            }],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    assert_eq!(block.instructions.len(), 1);

    let AsmInstructionKind::Load { address, .. } = &block.instructions[0].kind else {
        panic!("expected load");
    };
    let AsmValue::Global(name, _) = address else {
        panic!("expected global address");
    };
    assert_eq!(name, "__stderrp");
}

#[test]
fn materialize_removes_elf_copy_reloc_getopt_globals_for_darwin() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    program.globals.push(AsmGlobal {
        name: Name::new("optind"),
        ty: AsmType::I32,
        initializer: Some(AsmConstant::Int(1, AsmType::I32)),
        relocations: Vec::new(),
        section: Some(".bss".to_string()),
        linkage: Linkage::External,
        visibility: Visibility::Default,
        alignment: Some(4),
        is_constant: false,
    });

    fp_native::libc::materialize(&mut program);

    let global = program
        .globals
        .iter()
        .find(|global| global.name.as_str() == "optind")
        .unwrap();
    assert!(global.initializer.is_none());
    assert_eq!(global.ty, AsmType::I32);
    assert!(matches!(global.section.as_deref(), None));
}

#[test]
fn materialize_rewrites_indirect_exit_calls_to_exit_on_darwin_cross_materialization() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

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
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SymbolAddress),
                    kind: AsmInstructionKind::SymbolAddress {
                        symbol: "exit".to_string(),
                        kind: AsmSymbolAddressKind::Got,
                    },
                    type_hint: Some(ptr_i8.clone()),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 1,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Register(0),
                        args: vec![AsmValue::Constant(AsmConstant::Int(2, AsmType::I32))],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let call = block
        .instructions
        .iter()
        .find_map(|inst| match &inst.kind {
            AsmInstructionKind::Call { function, .. } => Some(function),
            _ => None,
        })
        .unwrap();
    assert!(matches!(call, AsmValue::Function(name) if name == "_exit"));
}

#[test]
fn materialize_rewrites_exit_to_exit_on_darwin_cross_materialization() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
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
            instructions: vec![AsmInstruction {
                id: 0,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("exit".to_string()),
                    args: vec![AsmValue::Constant(AsmConstant::Int(2, AsmType::I32))],
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
            }],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let call = block
        .instructions
        .iter()
        .find_map(|inst| match &inst.kind {
            AsmInstructionKind::Call { function, .. } => Some(function),
            _ => None,
        })
        .unwrap();
    assert!(matches!(call, AsmValue::Function(name) if name == "_exit"));
}

#[test]
fn materialize_rewrites_indirect_cxa_atexit_calls_to_noop_stub() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

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
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SymbolAddress),
                    kind: AsmInstructionKind::SymbolAddress {
                        symbol: "__cxa_atexit".to_string(),
                        kind: AsmSymbolAddressKind::Got,
                    },
                    type_hint: Some(ptr_i8.clone()),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 1,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Register(0),
                        args: vec![
                            AsmValue::Null(ptr_i8.clone()),
                            AsmValue::Null(ptr_i8.clone()),
                            AsmValue::Null(ptr_i8.clone()),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::I32),
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

    fp_native::libc::materialize(&mut program);

    assert!(program
        .functions
        .iter()
        .any(|func| func.name.as_str() == "fp_noop_cxa_atexit"));

    let block = &program
        .functions
        .iter()
        .find(|func| func.name.as_str() == "fp_lifted_main")
        .unwrap()
        .basic_blocks[0];
    let call = block
        .instructions
        .iter()
        .find_map(|inst| match &inst.kind {
            AsmInstructionKind::Call { function, .. } => Some(function),
            _ => None,
        })
        .unwrap();
    assert!(matches!(
        call,
        AsmValue::Function(name) if name == "fp_noop_cxa_atexit"
    ));
}

#[test]
fn materialize_inserts_getprogname_for_try_help_diagnostics() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
    program.globals.push(AsmGlobal {
        name: Name::new("fp_str_45"),
        ty: AsmType::Array(Box::new(AsmType::I8), 0),
        initializer: Some(AsmConstant::Bytes(
            b"Try '%s --help' for more information.\n\0".to_vec(),
        )),
        relocations: Vec::new(),
        section: Some(".rodata".to_string()),
        linkage: Linkage::External,
        visibility: Visibility::Default,
        alignment: Some(1),
        is_constant: true,
    });

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
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
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::GlobalRef(
                        Name::new("fp_str_45"),
                        ptr_i8.clone(),
                        vec![0],
                    ))),
                    type_hint: Some(ptr_i8.clone()),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 1,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("dcgettext".to_string()),
                        args: vec![
                            AsmValue::Null(ptr_i8.clone()),
                            AsmValue::Register(0),
                            AsmValue::Constant(AsmConstant::Int(5, AsmType::I32)),
                        ],
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(ptr_i8.clone()),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                },
                AsmInstruction {
                    id: 2,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("fprintf".to_string()),
                        args: vec![
                            AsmValue::Null(ptr_i8.clone()),
                            AsmValue::Register(1),
                            AsmValue::Null(ptr_i8.clone()),
                        ],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let call_sites = block
        .instructions
        .iter()
        .filter_map(|inst| match &inst.kind {
            AsmInstructionKind::Call { function, args, .. } => Some((inst.id, function, args)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(call_sites.len(), 3);
    assert!(matches!(call_sites[1].1, AsmValue::Function(name) if name == "getprogname"));
    assert!(matches!(call_sites[2].1, AsmValue::Function(name) if name == "fprintf"));
    assert_eq!(call_sites[2].2.len(), 3);
    assert_eq!(call_sites[2].2[2], AsmValue::Register(call_sites[1].0));
}

#[test]
fn materialize_rewrites_globalref_constants_for_stdio_got_slots() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
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
            instructions: vec![AsmInstruction {
                id: 0,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new("stderr"),
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
            }],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let AsmInstructionKind::Freeze(value) = &block.instructions[0].kind else {
        panic!("expected freeze");
    };
    let AsmValue::Constant(AsmConstant::GlobalRef(name, _, _)) = value else {
        panic!("expected globalref constant");
    };
    assert_eq!(name.as_str(), "__stderrp");
}

#[test]
fn materialize_dereferences_stdio_got_slot_on_darwin() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: fp_core::asmir::AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: fp_core::asmir::AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.container = Some(ContainerFile::new(
        ContainerKind::Executable,
        AsmObjectFormat::Elf,
        ContainerArchitecture::X86_64,
        ContainerEndianness::Little,
    ));
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
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
            instructions: vec![AsmInstruction {
                id: 0,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::SymbolAddress),
                kind: AsmInstructionKind::SymbolAddress {
                    symbol: "stderr".to_string(),
                    kind: AsmSymbolAddressKind::Got,
                },
                type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            }],
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

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    assert_eq!(block.instructions.len(), 1);

    let AsmInstructionKind::SymbolAddress { symbol, .. } = &block.instructions[0].kind else {
        panic!("expected symbol address");
    };
    assert_eq!(symbol, "__stderrp");
}
