use crate::asmir::{AsmOperand, AsmProgram, AsmTerminator};

pub fn format_program(program: &AsmProgram) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "asmir target={:?} format={:?} endian={:?} ptr={}\n",
        program.target.architecture,
        program.target.object_format,
        program.target.endianness,
        program.target.pointer_width
    ));

    for section in &program.sections {
        out.push_str(&format!(
            "section {} kind={:?} align={:?}\n",
            section.name, section.kind, section.alignment
        ));
    }

    for function in &program.functions {
        out.push_str(&format!("fn {}\n", function.name));
        for block in &function.basic_blocks {
            out.push_str(&format!("  bb{} {}\n", block.id, block.label.as_ref().map(|n| n.as_str()).unwrap_or("")));
            for instruction in &block.instructions {
                out.push_str(&format!(
                    "    {} {}\n",
                    instruction.opcode.mnemonic(),
                    instruction
                        .operands
                        .iter()
                        .map(format_operand)
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
            out.push_str(&format!("    {}\n", terminator_name(&block.terminator)));
        }
    }

    out
}

fn terminator_name(term: &AsmTerminator) -> &'static str {
    match term {
        AsmTerminator::Return(_) => "ret",
        AsmTerminator::Br(_) => "br",
        AsmTerminator::CondBr { .. } => "condbr",
        AsmTerminator::Switch { .. } => "switch",
        AsmTerminator::IndirectBr { .. } => "indirectbr",
        AsmTerminator::Invoke { .. } => "invoke",
        AsmTerminator::Resume(_) => "resume",
        AsmTerminator::Unreachable => "unreachable",
        AsmTerminator::CleanupRet { .. } => "cleanupret",
        AsmTerminator::CatchRet { .. } => "catchret",
        AsmTerminator::CatchSwitch { .. } => "catchswitch",
    }
}

fn format_operand(operand: &AsmOperand) -> String {
    match operand {
        AsmOperand::Register { reg, .. } => format!("{:?}", reg),
        AsmOperand::Immediate(value) => value.to_string(),
        AsmOperand::Memory(mem) => format!("mem({:?})", mem),
        AsmOperand::Label(name) => format!("label({})", name),
        AsmOperand::Symbol(name) => format!("symbol({})", name),
        AsmOperand::Block(id) => format!("bb{}", id),
        AsmOperand::Relocation(reloc) => {
            format!("reloc(kind={}, symbol={}, addend={})", reloc.kind, reloc.symbol, reloc.addend)
        }
        AsmOperand::Predicate { reg, inverted } => {
            format!("pred({:?}, inverted={})", reg, inverted)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::format_program;
    use crate::asmir::{
        AsmArchitecture, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmGenericOpcode,
        AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode, AsmOperand, AsmProgram,
        AsmSection, AsmSectionKind, AsmTarget, AsmTerminator,
    };
    use crate::lir::{Linkage, Name, Visibility};

    #[test]
    fn pretty_print_includes_target_and_opcode() {
        let mut program = AsmProgram::new(AsmTarget {
            architecture: AsmArchitecture::Aarch64,
            object_format: AsmObjectFormat::MachO,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        });
        program.sections.push(AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: Vec::new(),
            alignment: Some(4),
        });
        program.functions.push(AsmFunction {
            name: Name::new("main"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: crate::lir::Ty::I32,
                is_variadic: false,
            },
            basic_blocks: vec![crate::asmir::AsmBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![AsmInstruction {
                    id: 0,
                    kind: AsmInstructionKind::Freeze(crate::asmir::AsmValue::Constant(
                        crate::asmir::AsmConstant::Int(0, crate::lir::Ty::I32),
                    )),
                    type_hint: Some(crate::lir::Ty::I32),
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                    operands: vec![AsmOperand::Immediate(0)],
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: AsmTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: None,
            section: Some(".text".to_string()),
            is_declaration: false,
        });

        let rendered = format_program(&program);
        assert!(rendered.contains("asmir target=Aarch64"));
        assert!(rendered.contains("freeze 0"));
        assert!(rendered.contains("ret"));
    }
}
