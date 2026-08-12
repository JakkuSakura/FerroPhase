use fp_core::asmir::{
    AsmAttr, AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGenericOpcode,
    AsmGlobal, AsmInstruction, AsmObjectFormat, AsmOpcode, AsmOperand, AsmProgram, AsmRegister,
    AsmRegisterBank, AsmSection, AsmSectionFlag, AsmSectionKind, AsmSymbolAddressKind, AsmTarget,
    AsmTerminator, AsmType, OperandAccess,
};
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind,
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

fn base_program() -> AsmProgram {
    let mut program = AsmProgram::new(
        AsmTarget {
            architecture: fp_core::asmir::AsmArchitecture::Aarch64,
            object_format: AsmObjectFormat::MachO,
            endianness: fp_core::asmir::AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: Some(CallingConvention::C),
        },
        layout(),
    );
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
    program
}

fn new_function() -> AsmFunction {
    let mut function = AsmFunction::new(
        Name::new("fp_lifted_main"),
        AsmFunctionSignature {
            params: Vec::new(),
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    function.linkage = Linkage::External;
    function.visibility = Visibility::Default;
    function.calling_convention = Some(CallingConvention::C);
    function.section = Some(".text".to_string());
    function.is_declaration = false;
    function
}

fn finish_block(function: &mut AsmFunction, instructions: Vec<AsmInstruction>) {
    function.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions,
        terminator: AsmTerminator::Return(Some(AsmOperand::Constant(AsmConstant::Int(
            0,
            AsmType::I32,
        )))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
}

fn write_reg(id: fp_core::asmir::AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(id),
        access: OperandAccess::Write,
    }
}

fn read_reg(id: fp_core::asmir::AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(id),
        access: OperandAccess::Read,
    }
}

#[test]
fn materialize_maps_stderr_to_darwin_global() {
    let mut program = base_program();
    program.globals.push(AsmGlobal {
        name: Name::new("stderr"),
        ty: AsmType::Ptr(Box::new(AsmType::I8)),
        initializer: Some(AsmConstant::Bytes(vec![0; 8])),
        relocations: Vec::new(),
        section: Some(".data".to_string()),
        linkage: Linkage::External,
        visibility: Visibility::Default,
        alignment: Some(8),
        is_constant: false,
    });

    let mut function = new_function();
    let v0 = function.alloc_virtual_register(
        AsmType::Ptr(Box::new(AsmType::I8)),
        AsmRegisterBank::General,
        64,
    );
    finish_block(
        &mut function,
        vec![AsmInstruction::new(
            0,
            AsmOpcode::Generic(AsmGenericOpcode::Load),
            vec![write_reg(v0), AsmOperand::Symbol(Name::new("stderr"))],
        )],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    assert_eq!(block.instructions.len(), 1);

    let address = block.instructions[0]
        .operands
        .iter()
        .find(|op| matches!(op, AsmOperand::Symbol(_)))
        .expect("expected symbol address operand");
    let AsmOperand::Symbol(name) = address else {
        unreachable!()
    };
    assert_eq!(name.as_str(), "__stderrp");
}

#[test]
fn materialize_removes_elf_copy_reloc_getopt_globals_for_darwin() {
    let mut program = base_program();

    program.globals.push(AsmGlobal {
        name: Name::new("optind"),
        ty: AsmType::I32,
        initializer: Some(AsmConstant::Bytes(1i32.to_le_bytes().to_vec())),
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
    let mut program = base_program();
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    let mut function = new_function();
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    finish_block(
        &mut function,
        vec![
            AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::SymbolAddress),
                vec![
                    write_reg(v0),
                    AsmOperand::Symbol(Name::new("exit")),
                    AsmOperand::Attr(AsmAttr::SymbolAddressKind(AsmSymbolAddressKind::Got)),
                ],
            ),
            AsmInstruction::new(
                1,
                AsmOpcode::Generic(AsmGenericOpcode::Call),
                vec![
                    AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                    read_reg(v0),
                    AsmOperand::Constant(AsmConstant::Int(2, AsmType::I32)),
                ],
            ),
        ],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let target = block
        .instructions
        .iter()
        .find_map(|inst| inst.call_target_and_args())
        .map(|(target, _)| target)
        .unwrap();
    assert!(matches!(target, AsmOperand::Symbol(name) if name.as_str() == "_exit"));
}

#[test]
fn materialize_rewrites_exit_to_exit_on_darwin_cross_materialization() {
    let mut program = base_program();

    let mut function = new_function();
    finish_block(
        &mut function,
        vec![AsmInstruction::new(
            0,
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            vec![
                AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                AsmOperand::Symbol(Name::new("exit")),
                AsmOperand::Constant(AsmConstant::Int(2, AsmType::I32)),
            ],
        )],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let target = block
        .instructions
        .iter()
        .find_map(|inst| inst.call_target_and_args())
        .map(|(target, _)| target)
        .unwrap();
    assert!(matches!(target, AsmOperand::Symbol(name) if name.as_str() == "_exit"));
}

#[test]
fn materialize_rewrites_indirect_cxa_atexit_calls_to_noop_stub() {
    let mut program = base_program();
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    let mut function = new_function();
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    let v1 = function.alloc_virtual_register(AsmType::I32, AsmRegisterBank::General, 32);
    finish_block(
        &mut function,
        vec![
            AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::SymbolAddress),
                vec![
                    write_reg(v0),
                    AsmOperand::Symbol(Name::new("__cxa_atexit")),
                    AsmOperand::Attr(AsmAttr::SymbolAddressKind(AsmSymbolAddressKind::Got)),
                ],
            ),
            AsmInstruction::new(
                1,
                AsmOpcode::Generic(AsmGenericOpcode::Call),
                vec![
                    write_reg(v1),
                    AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                    read_reg(v0),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                ],
            ),
        ],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    assert!(
        program
            .functions
            .iter()
            .any(|func| func.name.as_str() == "fp_noop_cxa_atexit")
    );

    let block = &program
        .functions
        .iter()
        .find(|func| func.name.as_str() == "fp_lifted_main")
        .unwrap()
        .basic_blocks[0];
    let target = block
        .instructions
        .iter()
        .find_map(|inst| inst.call_target_and_args())
        .map(|(target, _)| target)
        .unwrap();
    assert!(matches!(
        target,
        AsmOperand::Symbol(name) if name.as_str() == "fp_noop_cxa_atexit"
    ));
}

#[test]
fn materialize_inserts_getprogname_for_try_help_diagnostics() {
    let mut program = base_program();
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
    let mut function = new_function();
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    let v1 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    finish_block(
        &mut function,
        vec![
            AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                vec![
                    write_reg(v0),
                    AsmOperand::Constant(AsmConstant::GlobalRef(
                        Name::new("fp_str_45"),
                        ptr_i8.clone(),
                        vec![0],
                    )),
                ],
            ),
            AsmInstruction::new(
                1,
                AsmOpcode::Generic(AsmGenericOpcode::Call),
                vec![
                    write_reg(v1),
                    AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                    AsmOperand::Symbol(Name::new("dcgettext")),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    read_reg(v0),
                    AsmOperand::Constant(AsmConstant::Int(5, AsmType::I32)),
                ],
            ),
            AsmInstruction::new(
                2,
                AsmOpcode::Generic(AsmGenericOpcode::Call),
                vec![
                    AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                    AsmOperand::Symbol(Name::new("fprintf")),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    read_reg(v1),
                    AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                ],
            ),
        ],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let call_sites: Vec<&AsmInstruction> = block
        .instructions
        .iter()
        .filter(|inst| inst.call_target_and_args().is_some())
        .collect();
    assert_eq!(call_sites.len(), 3);

    let (target1, _) = call_sites[1].call_target_and_args().unwrap();
    assert!(matches!(target1, AsmOperand::Symbol(name) if name.as_str() == "getprogname"));

    let (target2, args2) = call_sites[2].call_target_and_args().unwrap();
    assert!(matches!(target2, AsmOperand::Symbol(name) if name.as_str() == "fprintf"));
    assert_eq!(args2.len(), 3);

    let getprogname_result = call_sites[1]
        .result_register()
        .cloned()
        .expect("getprogname call should define a result");
    assert_eq!(
        args2[2],
        AsmOperand::Register {
            reg: getprogname_result,
            access: OperandAccess::Read,
        }
    );
}

#[test]
fn materialize_rewrites_globalref_constants_for_stdio_got_slots() {
    let mut program = base_program();

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let mut function = new_function();
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    finish_block(
        &mut function,
        vec![AsmInstruction::new(
            0,
            AsmOpcode::Generic(AsmGenericOpcode::Freeze),
            vec![
                write_reg(v0),
                AsmOperand::Constant(AsmConstant::GlobalRef(
                    Name::new("stderr"),
                    ptr_i8.clone(),
                    vec![0],
                )),
            ],
        )],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    let src = block.instructions[0]
        .operands
        .iter()
        .find(|op| matches!(op, AsmOperand::Constant(AsmConstant::GlobalRef(..))))
        .expect("expected globalref constant operand");
    let AsmOperand::Constant(AsmConstant::GlobalRef(name, _, _)) = src else {
        unreachable!()
    };
    assert_eq!(name.as_str(), "__stderrp");
}

#[test]
fn materialize_dereferences_stdio_got_slot_on_darwin() {
    let mut program = base_program();

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let mut function = new_function();
    let v0 = function.alloc_virtual_register(ptr_i8.clone(), AsmRegisterBank::General, 64);
    finish_block(
        &mut function,
        vec![AsmInstruction::new(
            0,
            AsmOpcode::Generic(AsmGenericOpcode::SymbolAddress),
            vec![
                write_reg(v0),
                AsmOperand::Symbol(Name::new("stderr")),
                AsmOperand::Attr(AsmAttr::SymbolAddressKind(AsmSymbolAddressKind::Got)),
            ],
        )],
    );
    program.functions.push(function);

    fp_native::libc::materialize(&mut program);

    let block = &program.functions[0].basic_blocks[0];
    assert_eq!(block.instructions.len(), 1);

    let symbol = block.instructions[0]
        .operands
        .iter()
        .find(|op| matches!(op, AsmOperand::Symbol(_)))
        .expect("expected symbol address operand");
    let AsmOperand::Symbol(name) = symbol else {
        unreachable!()
    };
    assert_eq!(name.as_str(), "__stderrp");
}
