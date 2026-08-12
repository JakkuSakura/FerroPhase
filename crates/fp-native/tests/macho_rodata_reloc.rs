use fp_core::asmir::{
    AsmArchitecture, AsmAttr, AsmBlock, AsmConstant, AsmEndianness, AsmFunction,
    AsmFunctionSignature, AsmGenericOpcode, AsmInstruction, AsmObjectFormat, AsmOpcode,
    AsmOperand, AsmProgram, AsmSection, AsmSectionFlag, AsmSectionKind, AsmTarget, AsmTerminator,
    AsmType,
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
use object::{Object, ObjectSection, RelocationFlags, SectionKind, macho};

#[test]
fn macho_aarch64_rodata_addresses_use_adrp_add_relocations() {
    let mut program = AsmProgram::new(
        AsmTarget {
            architecture: AsmArchitecture::Aarch64,
            object_format: AsmObjectFormat::MachO,
            endianness: AsmEndianness::Little,
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

    // puts("hello")
    let mut function = AsmFunction::new(
        Name::new("main"),
        AsmFunctionSignature {
            params: Vec::new(),
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    function.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![AsmInstruction::new(
            0,
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            vec![
                AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                AsmOperand::Symbol(Name::new("puts")),
                AsmOperand::Constant(AsmConstant::String("hello".to_string())),
            ],
        )],
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

    let plan = fp_native::emit::emit_plan_from_asmir(
        program,
        fp_native::emit::TargetFormat::MachO,
        fp_native::emit::TargetArch::Aarch64,
    )
    .expect("emit plan");
    let bytes = fp_native::emit::write_object_bytes(&plan).expect("write object");

    let file = object::File::parse(bytes.as_slice()).expect("parse Mach-O object");
    let text_section = file
        .sections()
        .find(|section| section.kind() == SectionKind::Text)
        .expect("missing text section");

    let mut saw_page21 = false;
    let mut saw_pageoff12 = false;
    for (_, reloc) in text_section.relocations() {
        let RelocationFlags::MachO {
            r_type,
            r_pcrel: _,
            r_length: _,
        } = reloc.flags()
        else {
            continue;
        };

        assert_ne!(
            r_type,
            macho::ARM64_RELOC_UNSIGNED,
            "rodata addresses should not use unsigned (absolute) text relocations"
        );
        if r_type == macho::ARM64_RELOC_PAGE21 {
            saw_page21 = true;
        }
        if r_type == macho::ARM64_RELOC_PAGEOFF12 {
            saw_pageoff12 = true;
        }
    }

    assert!(saw_page21, "expected an ADRP relocation (PAGE21)");
    assert!(saw_pageoff12, "expected an ADD relocation (PAGEOFF12)");
}
