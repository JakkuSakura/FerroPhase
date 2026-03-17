use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmConstant, AsmEndianness, AsmFunction, AsmFunctionSignature,
    AsmGenericOpcode, AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode, AsmProgram,
    AsmSection, AsmSectionFlag, AsmSectionKind, AsmTarget, AsmTerminator, AsmType, AsmValue,
};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};
use object::{macho, Object, ObjectSection, RelocationFlags, SectionKind};

#[test]
fn macho_aarch64_rodata_addresses_use_adrp_add_relocations() {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::MachO,
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: Some(CallingConvention::C),
    });
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    // puts("hello")
    program.functions.push(AsmFunction {
        name: Name::new("main"),
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
                opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                kind: AsmInstructionKind::Call {
                    function: AsmValue::Function("puts".to_string()),
                    args: vec![AsmValue::Constant(AsmConstant::String("hello".to_string()))],
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

