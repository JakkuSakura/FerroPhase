use crate::emit::{TargetArch, TargetFormat};
use crate::asm::{aarch64 as aarch64_asm, x86_64 as x86_64_asm};
use crate::asm::aarch64::{
    Aarch64CallTarget, Aarch64ConditionCode, Aarch64InstructionDetail, Aarch64MemoryOperand,
    Aarch64Operand, Aarch64Register, Aarch64TerminatorDetail, Aarch64TerminatorOpcode,
};
use crate::asm::x86_64::{
    X86CallTarget, X86ConditionCode, X86InstructionDetail, X86MemoryOperand, X86Opcode,
    X86Operand, X86Register, X86TerminatorDetail, X86TerminatorOpcode,
};
use fp_core::asmir::{
    AsmAddressValue, AsmArchitecture, AsmBlock, AsmConditionCode,
    AsmConstant, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmGenericOpcode,
    AsmGlobal, AsmInstruction, AsmInstructionKind, AsmIntrinsicKind, AsmLandingPadClause,
    AsmMemoryOperand, AsmObjectFormat, AsmOpcode, AsmOperand, AsmProgram, AsmRegister,
    AsmRegisterBank, AsmSection, AsmSectionFlag, AsmSectionKind, AsmTarget, AsmTerminator,
    AsmType, AsmTypeDefinition, AsmValue,
    OperandAccess,
};
use fp_core::error::Result;
use fp_core::lir::layout::size_of;
use fp_core::lir::{
    LirConstant, LirInstructionKind, LirIntrinsicKind, LirProgram, LirTerminator, LirValue, Name,
    Visibility,
};

pub fn select_program(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<AsmProgram> {
    let mut program = AsmProgram::new(AsmTarget {
        architecture: map_arch(arch),
        object_format: map_format(format),
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    });

    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });
    program.sections.push(AsmSection {
        name: ".rodata".to_string(),
        kind: AsmSectionKind::ReadOnlyData,
        flags: vec![AsmSectionFlag::Allocate],
        alignment: Some(16),
    });

    program.type_definitions = lir_program
        .type_definitions
        .iter()
        .map(|ty| AsmTypeDefinition {
            name: ty.name.clone(),
            ty: ty.ty.clone(),
        })
        .collect();

    program.globals = lir_program
        .globals
        .iter()
        .map(|global| AsmGlobal {
            name: global.name.clone(),
            ty: global.ty.clone(),
            initializer: global.initializer.as_ref().map(map_constant),
            section: global.section.clone(),
            linkage: global.linkage.clone(),
            visibility: global.visibility.clone(),
            alignment: global.alignment,
            is_constant: global.is_constant,
        })
        .collect();

    for function in &lir_program.functions {
        let mut asm_function = AsmFunction {
            name: function.name.clone(),
            signature: AsmFunctionSignature {
                params: function.signature.params.clone(),
                return_type: function.signature.return_type.clone(),
                is_variadic: function.signature.is_variadic,
            },
            basic_blocks: Vec::with_capacity(function.basic_blocks.len()),
            locals: function
                .locals
                .iter()
                .map(|local| fp_core::asmir::AsmLocal {
                    id: local.id,
                    ty: local.ty.clone(),
                    name: local.name.clone(),
                    is_argument: local.is_argument,
                })
                .collect(),
            stack_slots: function.stack_slots.clone(),
            frame: None,
            linkage: function.linkage.clone(),
            visibility: Visibility::Default,
            calling_convention: Some(function.calling_convention.clone()),
            section: Some(".text".to_string()),
            is_declaration: function.is_declaration,
        };

        for block in &function.basic_blocks {
            let mut instructions = Vec::with_capacity(block.instructions.len());
            for instruction in &block.instructions {
                instructions.push(AsmInstruction {
                    id: instruction.id,
                    kind: map_instruction_kind(&instruction.kind),
                    type_hint: instruction.type_hint.clone(),
                    opcode: AsmOpcode::Generic(generic_opcode(&map_instruction_kind(&instruction.kind))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: instruction.debug_info.clone(),
                    annotations: Vec::new(),
                });
            }
            asm_function.basic_blocks.push(AsmBlock {
                id: block.id,
                label: block.label.clone(),
                instructions,
                terminator: map_terminator(&block.terminator),
                predecessors: block.predecessors.clone(),
                successors: block.successors.clone(),
            });
        }

        asm_function.frame = None;
        program.functions.push(asm_function);
    }

    normalize_program_for_target(&mut program);

    Ok(program)
}

pub fn lower_to_x86_64(program: &AsmProgram) -> x86_64_asm::AsmX86_64Program {
    x86_64_asm::AsmX86_64Program {
        functions: program
            .functions
            .iter()
            .filter(|function| !function.is_declaration)
            .map(|function| {
                let next_virtual_id = function
                    .basic_blocks
                    .iter()
                    .flat_map(|block| block.instructions.iter().map(|instruction| instruction.id))
                    .max()
                    .unwrap_or(0)
                    .saturating_add(1);
                let mut ctx = PhysicalRegisterLoweringContext::new(next_virtual_id);

                x86_64_asm::AsmX86_64Function {
                    name: function.name.clone(),
                    blocks: function
                        .basic_blocks
                        .iter()
                        .map(|block| x86_64_asm::AsmX86_64Block {
                            id: block.id,
                            instructions: block
                                .instructions
                                .iter()
                                .map(|instruction| x86_detail_from_instruction(instruction, &mut ctx))
                                .collect(),
                            terminator: x86_terminator_detail(&block.terminator, &block.instructions),
                        })
                        .collect(),
                }
            })
            .collect(),
    }
}

pub fn lower_to_aarch64(program: &AsmProgram) -> aarch64_asm::AsmAarch64Program {
    aarch64_asm::AsmAarch64Program {
        functions: program
            .functions
            .iter()
            .filter(|function| !function.is_declaration)
            .map(|function| {
                let next_virtual_id = function
                    .basic_blocks
                    .iter()
                    .flat_map(|block| block.instructions.iter().map(|instruction| instruction.id))
                    .max()
                    .unwrap_or(0)
                    .saturating_add(1);
                let mut ctx = PhysicalRegisterLoweringContext::new(next_virtual_id);

                aarch64_asm::AsmAarch64Function {
                    name: function.name.clone(),
                    blocks: function
                        .basic_blocks
                        .iter()
                        .map(|block| aarch64_asm::AsmAarch64Block {
                            id: block.id,
                            instructions: block
                                .instructions
                                .iter()
                                .map(|instruction| aarch64_detail_from_instruction(instruction, &mut ctx))
                                .collect(),
                            terminator: aarch64_terminator_detail(&block.terminator, &block.instructions),
                        })
                        .collect(),
                }
            })
            .collect(),
    }
}

pub fn lift_from_x86_64(program: &x86_64_asm::AsmX86_64Program) -> AsmProgram {
    let mut next_instruction_id = 0u32;
    AsmProgram {
        target: AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: AsmObjectFormat::Raw,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        },
        sections: vec![AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        functions: program
            .functions
            .iter()
            .map(|function| AsmFunction {
                name: function.name.clone(),
                signature: AsmFunctionSignature {
                    params: Vec::new(),
                    return_type: AsmType::Void,
                    is_variadic: false,
                },
                basic_blocks: function
                    .blocks
                    .iter()
                    .map(|block| {
                        let instructions = block
                            .instructions
                            .iter()
                            .map(|instruction| {
                                let lifted = lift_x86_instruction(instruction, next_instruction_id);
                                next_instruction_id += 1;
                                lifted
                            })
                            .collect::<Vec<_>>();
                        let terminator = relink_comparison_condition(instructions.as_slice(), lift_x86_terminator(&block.terminator));
                        AsmBlock {
                            id: block.id,
                            label: Some(Name::new(format!("bb{}", block.id))),
                            instructions,
                            terminator,
                            predecessors: Vec::new(),
                            successors: block.terminator.targets.clone(),
                        }
                    })
                    .collect(),
                locals: Vec::new(),
                stack_slots: Vec::new(),
                frame: None,
                linkage: fp_core::lir::Linkage::External,
                visibility: Visibility::Default,
                calling_convention: None,
                section: Some(".text".to_string()),
                is_declaration: false,
            })
            .collect(),
    }
}

pub fn lift_from_aarch64(program: &aarch64_asm::AsmAarch64Program) -> AsmProgram {
    let mut next_instruction_id = 0u32;
    AsmProgram {
        target: AsmTarget {
            architecture: AsmArchitecture::Aarch64,
            object_format: AsmObjectFormat::Raw,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        },
        sections: vec![AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        functions: program
            .functions
            .iter()
            .map(|function| AsmFunction {
                name: function.name.clone(),
                signature: AsmFunctionSignature {
                    params: Vec::new(),
                    return_type: AsmType::Void,
                    is_variadic: false,
                },
                basic_blocks: function
                    .blocks
                    .iter()
                    .map(|block| {
                        let instructions = block
                            .instructions
                            .iter()
                            .map(|instruction| {
                                let lifted = lift_aarch64_instruction(instruction, next_instruction_id);
                                next_instruction_id += 1;
                                lifted
                            })
                            .collect::<Vec<_>>();
                        let terminator = relink_comparison_condition(instructions.as_slice(), lift_aarch64_terminator(&block.terminator));
                        AsmBlock {
                            id: block.id,
                            label: Some(Name::new(format!("bb{}", block.id))),
                            instructions,
                            terminator,
                            predecessors: Vec::new(),
                            successors: block.terminator.targets.clone(),
                        }
                    })
                    .collect(),
                locals: Vec::new(),
                stack_slots: Vec::new(),
                frame: None,
                linkage: fp_core::lir::Linkage::External,
                visibility: Visibility::Default,
                calling_convention: None,
                section: Some(".text".to_string()),
                is_declaration: false,
            })
            .collect(),
    }
}


fn normalize_program_for_target(program: &mut AsmProgram) {
    match program.target.architecture {
        AsmArchitecture::X86_64 => normalize_program_for_x86_64(program),
        AsmArchitecture::Aarch64 => normalize_program_for_aarch64(program),
        _ => normalize_program_generic(program),
    }
}

fn normalize_program_generic(program: &mut AsmProgram) {
    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                instruction.opcode = AsmOpcode::Generic(generic_opcode(&instruction.kind));
                instruction.operands = generic_operands(instruction.id, &instruction.kind, instruction.type_hint.as_ref());
            }
        }
    }
}

fn normalize_program_for_x86_64(program: &mut AsmProgram) {
    normalize_program_generic(program);
}

fn normalize_program_for_aarch64(program: &mut AsmProgram) {
    normalize_program_generic(program);
}

fn relink_comparison_condition(instructions: &[AsmInstruction], terminator: AsmTerminator) -> AsmTerminator {
    match terminator {
        AsmTerminator::CondBr {
            condition: AsmValue::Condition(condition),
            if_true,
            if_false,
        } => {
            let condition = last_comparison_instruction(instructions)
                .filter(|(_, comparison)| comparison == &condition)
                .map(|(id, _)| AsmValue::Flags(id))
                .unwrap_or(AsmValue::Condition(condition));
            AsmTerminator::CondBr {
                condition,
                if_true,
                if_false,
            }
        }
        other => other,
    }
}

fn last_comparison_instruction(instructions: &[AsmInstruction]) -> Option<(u32, AsmConditionCode)> {
    instructions.iter().rev().find_map(|instruction| comparison_code_from_kind(&instruction.kind).map(|code| (instruction.id, code)))
}

fn comparison_code_from_kind(kind: &AsmInstructionKind) -> Option<AsmConditionCode> {
    match kind {
        AsmInstructionKind::Eq(..) => Some(AsmConditionCode::Eq),
        AsmInstructionKind::Ne(..) => Some(AsmConditionCode::Ne),
        AsmInstructionKind::Lt(..) => Some(AsmConditionCode::Lt),
        AsmInstructionKind::Le(..) => Some(AsmConditionCode::Le),
        AsmInstructionKind::Gt(..) => Some(AsmConditionCode::Gt),
        AsmInstructionKind::Ge(..) => Some(AsmConditionCode::Ge),
        _ => None,
    }
}

fn x86_detail_from_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86InstructionDetail {
    match &instruction.opcode {
        AsmOpcode::Custom(opcode) => x86_detail_from_custom(opcode, &instruction.operands, ctx),
        _ => {
            let mut detail =
                x86_detail(instruction.id, &instruction.kind, instruction.type_hint.as_ref(), ctx);
            if let Some(write_operand) = mapped_x86_write_operand(&instruction.operands, ctx) {
                if !detail.operands.is_empty() && instruction_produces_value(&instruction.kind) {
                    detail.operands[0] = write_operand;
                }
            }
            if let Some(operands) = x86_operands_from_asm(&instruction.operands) {
                detail.operands = operands;
                if detail.opcode == X86Opcode::Call {
                    detail.call_target = detail.operands.first().map(x86_call_target_from_operand);
                }
            }
            detail
        }
    }
}

fn aarch64_detail_from_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64InstructionDetail {
    match &instruction.opcode {
        AsmOpcode::Custom(opcode) => aarch64_detail_from_custom(opcode, &instruction.operands, ctx),
        _ => {
            let mut detail = aarch64_detail(
                instruction.id,
                &instruction.kind,
                instruction.type_hint.as_ref(),
                ctx,
            );
            if let Some(write_operand) = mapped_aarch64_write_operand(&instruction.operands, ctx) {
                if !detail.operands.is_empty() && instruction_produces_value(&instruction.kind) {
                    detail.operands[0] = write_operand;
                }
            }
            if let Some(operands) = aarch64_operands_from_asm(&instruction.operands) {
                detail.operands = operands;
                if detail.opcode == "bl" {
                    detail.call_target = detail.operands.first().map(aarch64_call_target_from_operand);
                }
            }
            detail
        }
    }
}

fn x86_operands_from_asm(operands: &[AsmOperand]) -> Option<Vec<X86Operand>> {
    operands.iter().map(x86_operand_from_asm).collect()
}

fn mapped_x86_write_operand(
    operands: &[AsmOperand],
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<X86Operand> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Register { access: OperandAccess::Write | OperandAccess::ReadWrite, .. } => {
            Some(asm_operand_to_x86(operand, ctx))
        }
        _ => None,
    })
}

fn x86_operand_from_asm(operand: &AsmOperand) -> Option<X86Operand> {
    match operand {
        AsmOperand::Register { reg, access } => Some(X86Operand::Register {
            reg: x86_register_from_asm(reg)?,
            access: access.clone(),
        }),
        AsmOperand::Immediate(value) => Some(X86Operand::Immediate(*value)),
        AsmOperand::Memory(memory) => Some(X86Operand::Memory(x86_memory_from_asm(memory)?)),
        AsmOperand::Label(name) | AsmOperand::Symbol(name) => Some(X86Operand::Symbol(name.clone())),
        AsmOperand::Block(id) => Some(X86Operand::Block(*id)),
        AsmOperand::Relocation(relocation) => Some(X86Operand::Symbol(relocation.symbol.clone())),
        AsmOperand::Predicate { .. } => None,
    }
}

fn x86_memory_from_asm(memory: &AsmMemoryOperand) -> Option<X86MemoryOperand> {
    Some(X86MemoryOperand {
        base: match memory.base.as_ref() {
            Some(register) => Some(x86_register_from_asm(register)?),
            None => None,
        },
        index: match memory.index.as_ref() {
            Some(register) => Some(x86_register_from_asm(register)?),
            None => None,
        },
        scale: memory.scale,
        displacement: memory.displacement,
        size_bytes: memory.size_bytes,
    })
}

fn x86_register_from_asm(register: &AsmRegister) -> Option<X86Register> {
    match register {
        AsmRegister::Physical(register) if is_x86_physical_register_name(&register.name) => Some(X86Register::Physical {
            name: register.name.clone(),
            size_bits: register.size_bits,
        }),
        AsmRegister::Physical(_) => None,
        AsmRegister::Virtual { id, size_bits, .. } => Some(X86Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        }),
    }
}

fn aarch64_operands_from_asm(operands: &[AsmOperand]) -> Option<Vec<Aarch64Operand>> {
    operands.iter().map(aarch64_operand_from_asm).collect()
}

fn mapped_aarch64_write_operand(
    operands: &[AsmOperand],
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64Operand> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Register { access: OperandAccess::Write | OperandAccess::ReadWrite, .. } => {
            Some(asm_operand_to_aarch64(operand, ctx))
        }
        _ => None,
    })
}

fn aarch64_operand_from_asm(operand: &AsmOperand) -> Option<Aarch64Operand> {
    match operand {
        AsmOperand::Register { reg, access } => Some(Aarch64Operand::Register {
            reg: aarch64_register_from_asm(reg)?,
            access: access.clone(),
        }),
        AsmOperand::Immediate(value) => Some(Aarch64Operand::Immediate(*value)),
        AsmOperand::Memory(memory) => Some(Aarch64Operand::Memory(aarch64_memory_from_asm(memory)?)),
        AsmOperand::Label(name) | AsmOperand::Symbol(name) => Some(Aarch64Operand::Symbol(name.clone())),
        AsmOperand::Block(id) => Some(Aarch64Operand::Block(*id)),
        AsmOperand::Relocation(relocation) => Some(Aarch64Operand::Symbol(relocation.symbol.clone())),
        AsmOperand::Predicate { .. } => None,
    }
}

fn aarch64_memory_from_asm(memory: &AsmMemoryOperand) -> Option<Aarch64MemoryOperand> {
    Some(Aarch64MemoryOperand {
        base: match memory.base.as_ref() {
            Some(register) => Some(aarch64_register_from_asm(register)?),
            None => None,
        },
        index: match memory.index.as_ref() {
            Some(register) => Some(aarch64_register_from_asm(register)?),
            None => None,
        },
        scale: memory.scale,
        displacement: memory.displacement,
        size_bytes: memory.size_bytes,
    })
}

fn aarch64_register_from_asm(register: &AsmRegister) -> Option<Aarch64Register> {
    match register {
        AsmRegister::Physical(register) if is_aarch64_physical_register_name(&register.name) => {
            Some(Aarch64Register::Physical {
                name: register.name.clone(),
                size_bits: register.size_bits,
            })
        }
        AsmRegister::Physical(_) => None,
        AsmRegister::Virtual { id, size_bits, .. } => Some(Aarch64Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        }),
    }
}

fn is_x86_physical_register_name(name: &str) -> bool {
    name.starts_with('r')
        || name.starts_with('e')
        || name.starts_with("xmm")
        || matches!(
            name,
            "ax" | "bx" | "cx" | "dx" | "si" | "di" | "sp" | "bp" | "al" | "ah" | "bl" | "bh" | "cl" | "ch" | "dl" | "dh"
        )
}

fn is_aarch64_physical_register_name(name: &str) -> bool {
    matches!(name.chars().next(), Some('x' | 'w' | 's' | 'd' | 'q'))
}

fn x86_detail_from_custom(
    opcode: &str,
    operands: &[AsmOperand],
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86InstructionDetail {
    let (opcode_name, condition) = parse_x86_custom_opcode(opcode);
    let concrete_opcode = match opcode_name {
        "add" => X86Opcode::Add,
        "sub" => X86Opcode::Sub,
        "imul" => X86Opcode::IMul,
        "idiv" => X86Opcode::IDiv,
        "and" => X86Opcode::And,
        "or" => X86Opcode::Or,
        "xor" => X86Opcode::Xor,
        "shl" => X86Opcode::Shl,
        "sar" => X86Opcode::Sar,
        "not" => X86Opcode::Not,
        "cmp" => X86Opcode::Cmp,
        "mov" => X86Opcode::Mov,
        "lea" => X86Opcode::Lea,
        "lea.frame" => X86Opcode::LeaFrame,
        "cvtsi2sd" => X86Opcode::Cvtsi2sd,
        "cvttsd2si" => X86Opcode::Cvttsd2si,
        "cvtss2sd" => X86Opcode::Cvtss2sd,
        "cvtsd2ss" => X86Opcode::Cvtsd2ss,
        "mulss" => X86Opcode::Mulss,
        "mulsd" => X86Opcode::Mulsd,
        "divss" => X86Opcode::Divss,
        "divsd" => X86Opcode::Divsd,
        "mov.extract" => X86Opcode::MovExtract,
        "mov.insert" => X86Opcode::MovInsert,
        "call" => X86Opcode::Call,
        "phi.copy" => X86Opcode::PhiCopy,
        "cmov" => X86Opcode::CMov,
        "landingpad" => X86Opcode::LandingPad,
        "ud2" => X86Opcode::Ud2,
        _ => X86Opcode::InlineAsm,
    };
    let operands = operands
        .iter()
        .map(|operand| asm_operand_to_x86(operand, ctx))
        .collect::<Vec<_>>();
    let call_target = if concrete_opcode == X86Opcode::Call {
        operands.first().map(x86_call_target_from_operand)
    } else {
        None
    };
    X86InstructionDetail {
        opcode: concrete_opcode,
        operands,
        condition,
        call_target,
    }
}

fn aarch64_detail_from_custom(
    opcode: &str,
    operands: &[AsmOperand],
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64InstructionDetail {
    let (opcode_name, condition) = parse_aarch64_custom_opcode(opcode);
    let operands = operands
        .iter()
        .map(|operand| asm_operand_to_aarch64(operand, ctx))
        .collect::<Vec<_>>();
    let call_target = if opcode_name == "bl" {
        operands.first().map(aarch64_call_target_from_operand)
    } else {
        None
    };
    Aarch64InstructionDetail {
        opcode: opcode_name.to_string(),
        operands,
        condition,
        call_target,
    }
}

fn parse_x86_custom_opcode(opcode: &str) -> (&str, Option<X86ConditionCode>) {
    match opcode.split_once('.') {
        Some((base, suffix)) if matches!(base, "cmp" | "cmov") => {
            (base, parse_x86_condition_token(suffix))
        }
        _ => (opcode, None),
    }
}

fn parse_aarch64_custom_opcode(opcode: &str) -> (&str, Option<Aarch64ConditionCode>) {
    match opcode.split_once('.') {
        Some((base, suffix)) if matches!(base, "cmp" | "csel") => {
            (base, parse_aarch64_condition_token(suffix))
        }
        _ => (opcode, None),
    }
}

fn parse_x86_condition_token(token: &str) -> Option<X86ConditionCode> {
    match token {
        "eq" => Some(X86ConditionCode::Equal),
        "ne" => Some(X86ConditionCode::NotEqual),
        "lt" => Some(X86ConditionCode::Less),
        "le" => Some(X86ConditionCode::LessEqual),
        "gt" => Some(X86ConditionCode::Greater),
        "ge" => Some(X86ConditionCode::GreaterEqual),
        "nz" => Some(X86ConditionCode::NonZero),
        _ => None,
    }
}

fn parse_aarch64_condition_token(token: &str) -> Option<Aarch64ConditionCode> {
    match token {
        "eq" => Some(Aarch64ConditionCode::Eq),
        "ne" => Some(Aarch64ConditionCode::Ne),
        "lt" => Some(Aarch64ConditionCode::Lt),
        "le" => Some(Aarch64ConditionCode::Le),
        "gt" => Some(Aarch64ConditionCode::Gt),
        "ge" => Some(Aarch64ConditionCode::Ge),
        "nz" => Some(Aarch64ConditionCode::NonZero),
        _ => None,
    }
}

fn x86_detail(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86InstructionDetail {
    X86InstructionDetail {
        opcode: x86_opcode(kind, ty),
        operands: x86_typed_operands(id, kind, ty, ctx),
        condition: x86_condition(kind),
        call_target: x86_call_target(kind, ctx),
    }
}

fn x86_typed_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Vec<X86Operand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(X86Operand::Register {
                reg: x86_virtual_register(id, ty),
                access: OperandAccess::Write,
            });
        }
    }

    match kind {
        AsmInstructionKind::Add(lhs, rhs)
        | AsmInstructionKind::Sub(lhs, rhs)
        | AsmInstructionKind::Mul(lhs, rhs)
        | AsmInstructionKind::Div(lhs, rhs)
        | AsmInstructionKind::Rem(lhs, rhs)
        | AsmInstructionKind::And(lhs, rhs)
        | AsmInstructionKind::Or(lhs, rhs)
        | AsmInstructionKind::Xor(lhs, rhs)
        | AsmInstructionKind::Shl(lhs, rhs)
        | AsmInstructionKind::Shr(lhs, rhs)
        | AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs) => {
            operands.push(x86_operand(lhs, ctx));
            operands.push(x86_operand(rhs, ctx));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(x86_operand(value, ctx)),
        AsmInstructionKind::Load { address, .. } => operands.push(x86_address_operand(address, ty, ctx)),
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(x86_address_operand(address, None, ctx));
            operands.push(x86_operand(value, ctx));
        }
        AsmInstructionKind::Alloca { size, .. } => operands.push(x86_operand(size, ctx)),
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(x86_operand(ptr, ctx));
            operands.extend(indices.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::Bitcast(value, _)
        | AsmInstructionKind::Trunc(value, _)
        | AsmInstructionKind::ZExt(value, _)
        | AsmInstructionKind::SExt(value, _)
        | AsmInstructionKind::FPExt(value, _)
        | AsmInstructionKind::FPTrunc(value, _)
        | AsmInstructionKind::FPToUI(value, _)
        | AsmInstructionKind::FPToSI(value, _)
        | AsmInstructionKind::UIToFP(value, _)
        | AsmInstructionKind::SIToFP(value, _)
        | AsmInstructionKind::SextOrTrunc(value, _) => operands.push(x86_operand(value, ctx)),
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(x86_operand(aggregate, ctx));
            operands.extend(indices.iter().map(|index| X86Operand::Immediate(*index as i128)));
        }
        AsmInstructionKind::InsertValue { aggregate, element, indices } => {
            operands.push(x86_operand(aggregate, ctx));
            operands.push(x86_operand(element, ctx));
            operands.extend(indices.iter().map(|index| X86Operand::Immediate(*index as i128)));
        }
        AsmInstructionKind::Call { function, args, .. } => {
            operands.push(match x86_call_target_from_value(function, ctx) {
                X86CallTarget::Symbol(name) => X86Operand::Symbol(name),
                X86CallTarget::Register(reg) => X86Operand::Register { reg, access: OperandAccess::Read },
            });
            operands.extend(args.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(X86Operand::Symbol(Name::new(format!("intrinsic.{kind:?}").to_ascii_lowercase())));
            operands.extend(args.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(x86_operand(value, ctx));
                operands.push(X86Operand::Block(*block));
            }
        }
        AsmInstructionKind::Select { condition, if_true, if_false } => {
            operands.push(x86_operand(condition, ctx));
            operands.push(x86_operand(if_true, ctx));
            operands.push(x86_operand(if_false, ctx));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(inputs.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(x86_operand(personality, ctx));
            }
        }
        AsmInstructionKind::Unreachable => {}
    }

    operands
}

fn x86_condition(kind: &AsmInstructionKind) -> Option<X86ConditionCode> {
    match kind {
        AsmInstructionKind::Eq(..) => Some(X86ConditionCode::Equal),
        AsmInstructionKind::Ne(..) => Some(X86ConditionCode::NotEqual),
        AsmInstructionKind::Lt(..) => Some(X86ConditionCode::Less),
        AsmInstructionKind::Le(..) => Some(X86ConditionCode::LessEqual),
        AsmInstructionKind::Gt(..) => Some(X86ConditionCode::Greater),
        AsmInstructionKind::Ge(..) => Some(X86ConditionCode::GreaterEqual),
        AsmInstructionKind::Select { .. } => Some(X86ConditionCode::NonZero),
        _ => None,
    }
}

fn x86_call_target(kind: &AsmInstructionKind, ctx: &mut PhysicalRegisterLoweringContext) -> Option<X86CallTarget> {
    match kind {
        AsmInstructionKind::Call { function, .. } => Some(x86_call_target_from_value(function, ctx)),
        AsmInstructionKind::IntrinsicCall { kind, .. } => Some(X86CallTarget::Symbol(Name::new(
            format!("intrinsic.{kind:?}").to_ascii_lowercase(),
        ))),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct PhysicalRegisterLoweringContext {
    next_virtual_id: u32,
    virtual_ids: std::collections::HashMap<(String, u16), u32>,
}

impl PhysicalRegisterLoweringContext {
    fn new(next_virtual_id: u32) -> Self {
        Self {
            next_virtual_id,
            virtual_ids: std::collections::HashMap::new(),
        }
    }

    fn virtual_id_for(&mut self, register: &fp_core::asmir::AsmPhysicalRegister) -> u32 {
        let key = (register.name.clone(), register.size_bits.max(8));
        if let Some(id) = self.virtual_ids.get(&key) {
            return *id;
        }
        let id = self.next_virtual_id;
        self.next_virtual_id = self.next_virtual_id.saturating_add(1);
        self.virtual_ids.insert(key, id);
        id
    }
}

fn x86_call_target_from_value(value: &AsmValue, ctx: &mut PhysicalRegisterLoweringContext) -> X86CallTarget {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => X86CallTarget::Symbol(Name::new(name.clone())),
        AsmValue::Register(id) => X86CallTarget::Register(x86_virtual_register(*id, &AsmType::I64)),
        AsmValue::PhysicalRegister(register) => {
            X86CallTarget::Register(map_physical_register_to_x86(register, ctx))
        }
        _ => X86CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn x86_terminator_detail(term: &AsmTerminator, instructions: &[AsmInstruction]) -> X86TerminatorDetail {
    match term {
        AsmTerminator::Return(_) => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Ret,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Br(target) => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Jmp,
            condition: None,
            targets: vec![*target],
        },
        AsmTerminator::CondBr { condition, if_true, if_false } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Jcc,
            condition: resolve_x86_branch_condition(condition, instructions).or(Some(X86ConditionCode::NonZero)),
            targets: vec![*if_true, *if_false],
        },
        AsmTerminator::Switch { default, cases, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Switch,
            condition: None,
            targets: cases.iter().map(|(_, target)| *target).chain(std::iter::once(*default)).collect(),
        },
        AsmTerminator::IndirectBr { destinations, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::IndirectJmp,
            condition: None,
            targets: destinations.clone(),
        },
        AsmTerminator::Invoke { normal_dest, unwind_dest, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Invoke,
            condition: None,
            targets: vec![*normal_dest, *unwind_dest],
        },
        AsmTerminator::Resume(_) => X86TerminatorDetail { opcode: X86TerminatorOpcode::Resume, condition: None, targets: Vec::new() },
        AsmTerminator::Unreachable => X86TerminatorDetail { opcode: X86TerminatorOpcode::Ud2, condition: None, targets: Vec::new() },
        AsmTerminator::CleanupRet { unwind_dest, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::CleanupRet,
            condition: None,
            targets: unwind_dest.iter().copied().collect(),
        },
        AsmTerminator::CatchRet { successor, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::CatchRet,
            condition: None,
            targets: vec![*successor],
        },
        AsmTerminator::CatchSwitch { handlers, unwind_dest, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::CatchSwitch,
            condition: None,
            targets: handlers.iter().copied().chain(unwind_dest.iter().copied()).collect(),
        },
    }
}

fn resolve_x86_branch_condition(condition: &AsmValue, instructions: &[AsmInstruction]) -> Option<X86ConditionCode> {
    match condition {
        AsmValue::Flags(id) => instructions
            .iter()
            .find(|instruction| instruction.id == *id)
            .and_then(|instruction| comparison_code_from_kind(&instruction.kind))
            .map(|code| x86_condition_from_asm(&code)),
        other => x86_branch_condition(other),
    }
}

fn aarch64_detail(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64InstructionDetail {
    Aarch64InstructionDetail {
        opcode: aarch64_opcode_name(kind, ty).to_string(),
        operands: aarch64_typed_operands(id, kind, ty, ctx),
        condition: aarch64_condition(kind),
        call_target: aarch64_call_target(kind, ctx),
    }
}

fn aarch64_opcode_name(kind: &AsmInstructionKind, ty: Option<&AsmType>) -> &'static str {
    match kind {
        AsmInstructionKind::Add(..) => "add",
        AsmInstructionKind::Sub(..) => "sub",
        AsmInstructionKind::Mul(..) if is_float_type_opt(ty) => if matches!(ty, Some(AsmType::F32)) { "fmul.s" } else { "fmul.d" },
        AsmInstructionKind::Mul(..) => "mul",
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) if is_float_type_opt(ty) => if matches!(ty, Some(AsmType::F32)) { "fdiv.s" } else { "fdiv.d" },
        AsmInstructionKind::Div(..) => "sdiv",
        AsmInstructionKind::Rem(..) => "msub.rem",
        AsmInstructionKind::And(..) => "and",
        AsmInstructionKind::Or(..) => "orr",
        AsmInstructionKind::Xor(..) => "eor",
        AsmInstructionKind::Shl(..) => "lsl",
        AsmInstructionKind::Shr(..) => "asr",
        AsmInstructionKind::Not(..) => "mvn",
        AsmInstructionKind::Eq(..)
        | AsmInstructionKind::Ne(..)
        | AsmInstructionKind::Lt(..)
        | AsmInstructionKind::Le(..)
        | AsmInstructionKind::Gt(..)
        | AsmInstructionKind::Ge(..) => "cmp",
        AsmInstructionKind::Load { .. } => "ldr",
        AsmInstructionKind::Store { .. } => "str",
        AsmInstructionKind::Alloca { .. } => "add.sp",
        AsmInstructionKind::GetElementPtr { .. } => "add.addr",
        AsmInstructionKind::Bitcast(..)
        | AsmInstructionKind::PtrToInt(..)
        | AsmInstructionKind::IntToPtr(..)
        | AsmInstructionKind::Trunc(..)
        | AsmInstructionKind::ZExt(..)
        | AsmInstructionKind::SExt(..)
        | AsmInstructionKind::SextOrTrunc(..)
        | AsmInstructionKind::Freeze(..) => "mov",
        AsmInstructionKind::FPExt(..) => "fcvt.d.s",
        AsmInstructionKind::FPTrunc(..) => "fcvt.s.d",
        AsmInstructionKind::FPToUI(..) | AsmInstructionKind::FPToSI(..) => "fcvtzs",
        AsmInstructionKind::UIToFP(..) | AsmInstructionKind::SIToFP(..) => "scvtf",
        AsmInstructionKind::ExtractValue { .. } => "ldr.extract",
        AsmInstructionKind::InsertValue { .. } => "str.insert",
        AsmInstructionKind::Call { .. } | AsmInstructionKind::IntrinsicCall { .. } => "bl",
        AsmInstructionKind::Phi { .. } => "phi.copy",
        AsmInstructionKind::Select { .. } => "csel",
        AsmInstructionKind::InlineAsm { .. } => "inlineasm",
        AsmInstructionKind::LandingPad { .. } => "landingpad",
        AsmInstructionKind::Unreachable => "brk",
    }
}

fn aarch64_typed_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Vec<Aarch64Operand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(Aarch64Operand::Register {
                reg: aarch64_virtual_register(id, ty),
                access: OperandAccess::Write,
            });
        }
    }
    match kind {
        AsmInstructionKind::Add(lhs, rhs)
        | AsmInstructionKind::Sub(lhs, rhs)
        | AsmInstructionKind::Mul(lhs, rhs)
        | AsmInstructionKind::Div(lhs, rhs)
        | AsmInstructionKind::Rem(lhs, rhs)
        | AsmInstructionKind::And(lhs, rhs)
        | AsmInstructionKind::Or(lhs, rhs)
        | AsmInstructionKind::Xor(lhs, rhs)
        | AsmInstructionKind::Shl(lhs, rhs)
        | AsmInstructionKind::Shr(lhs, rhs)
        | AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs) => {
            operands.push(aarch64_operand(lhs, ctx));
            operands.push(aarch64_operand(rhs, ctx));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(aarch64_operand(value, ctx)),
        AsmInstructionKind::Load { address, .. } => operands.push(aarch64_address_operand(address, ty, ctx)),
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(aarch64_address_operand(address, None, ctx));
            operands.push(aarch64_operand(value, ctx));
        }
        AsmInstructionKind::Alloca { size, .. } => operands.push(aarch64_operand(size, ctx)),
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(aarch64_operand(ptr, ctx));
            operands.extend(indices.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::Bitcast(value, _)
        | AsmInstructionKind::Trunc(value, _)
        | AsmInstructionKind::ZExt(value, _)
        | AsmInstructionKind::SExt(value, _)
        | AsmInstructionKind::FPExt(value, _)
        | AsmInstructionKind::FPTrunc(value, _)
        | AsmInstructionKind::FPToUI(value, _)
        | AsmInstructionKind::FPToSI(value, _)
        | AsmInstructionKind::UIToFP(value, _)
        | AsmInstructionKind::SIToFP(value, _)
        | AsmInstructionKind::SextOrTrunc(value, _) => operands.push(aarch64_operand(value, ctx)),
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(aarch64_operand(aggregate, ctx));
            operands.extend(indices.iter().map(|index| Aarch64Operand::Immediate(*index as i128)));
        }
        AsmInstructionKind::InsertValue { aggregate, element, indices } => {
            operands.push(aarch64_operand(aggregate, ctx));
            operands.push(aarch64_operand(element, ctx));
            operands.extend(indices.iter().map(|index| Aarch64Operand::Immediate(*index as i128)));
        }
        AsmInstructionKind::Call { function, args, .. } => {
            operands.push(match aarch64_call_target_from_value(function, ctx) {
                Aarch64CallTarget::Symbol(name) => Aarch64Operand::Symbol(name),
                Aarch64CallTarget::Register(reg) => Aarch64Operand::Register { reg, access: OperandAccess::Read },
            });
            operands.extend(args.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(Aarch64Operand::Symbol(Name::new(format!("intrinsic.{kind:?}").to_ascii_lowercase())));
            operands.extend(args.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(aarch64_operand(value, ctx));
                operands.push(Aarch64Operand::Block(*block));
            }
        }
        AsmInstructionKind::Select { condition, if_true, if_false } => {
            operands.push(aarch64_operand(condition, ctx));
            operands.push(aarch64_operand(if_true, ctx));
            operands.push(aarch64_operand(if_false, ctx));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(inputs.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(aarch64_operand(personality, ctx));
            }
        }
        AsmInstructionKind::Unreachable => {}
    }
    operands
}

fn aarch64_condition(kind: &AsmInstructionKind) -> Option<Aarch64ConditionCode> {
    match kind {
        AsmInstructionKind::Eq(..) => Some(Aarch64ConditionCode::Eq),
        AsmInstructionKind::Ne(..) => Some(Aarch64ConditionCode::Ne),
        AsmInstructionKind::Lt(..) => Some(Aarch64ConditionCode::Lt),
        AsmInstructionKind::Le(..) => Some(Aarch64ConditionCode::Le),
        AsmInstructionKind::Gt(..) => Some(Aarch64ConditionCode::Gt),
        AsmInstructionKind::Ge(..) => Some(Aarch64ConditionCode::Ge),
        AsmInstructionKind::Select { .. } => Some(Aarch64ConditionCode::NonZero),
        _ => None,
    }
}

fn aarch64_call_target(
    kind: &AsmInstructionKind,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64CallTarget> {
    match kind {
        AsmInstructionKind::Call { function, .. } => Some(aarch64_call_target_from_value(function, ctx)),
        AsmInstructionKind::IntrinsicCall { kind, .. } => Some(Aarch64CallTarget::Symbol(Name::new(
            format!("intrinsic.{kind:?}").to_ascii_lowercase(),
        ))),
        _ => None,
    }
}

fn aarch64_call_target_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64CallTarget {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => Aarch64CallTarget::Symbol(Name::new(name.clone())),
        AsmValue::Register(id) => Aarch64CallTarget::Register(aarch64_virtual_register(*id, &AsmType::I64)),
        AsmValue::PhysicalRegister(register) => {
            Aarch64CallTarget::Register(map_physical_register_to_aarch64(register, ctx))
        }
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn aarch64_terminator_detail(term: &AsmTerminator, instructions: &[AsmInstruction]) -> Aarch64TerminatorDetail {
    match term {
        AsmTerminator::Return(_) => Aarch64TerminatorDetail { opcode: Aarch64TerminatorOpcode::Ret, condition: None, targets: Vec::new() },
        AsmTerminator::Br(target) => Aarch64TerminatorDetail { opcode: Aarch64TerminatorOpcode::B, condition: None, targets: vec![*target] },
        AsmTerminator::CondBr { condition, if_true, if_false } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::BCond,
            condition: resolve_aarch64_branch_condition(condition, instructions).or(Some(Aarch64ConditionCode::NonZero)),
            targets: vec![*if_true, *if_false],
        },
        AsmTerminator::Switch { default, cases, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Switch,
            condition: None,
            targets: cases.iter().map(|(_, target)| *target).chain(std::iter::once(*default)).collect(),
        },
        AsmTerminator::IndirectBr { destinations, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Br,
            condition: None,
            targets: destinations.clone(),
        },
        AsmTerminator::Invoke { normal_dest, unwind_dest, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Invoke,
            condition: None,
            targets: vec![*normal_dest, *unwind_dest],
        },
        AsmTerminator::Resume(_) => Aarch64TerminatorDetail { opcode: Aarch64TerminatorOpcode::Resume, condition: None, targets: Vec::new() },
        AsmTerminator::Unreachable => Aarch64TerminatorDetail { opcode: Aarch64TerminatorOpcode::Brk, condition: None, targets: Vec::new() },
        AsmTerminator::CleanupRet { unwind_dest, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CleanupRet,
            condition: None,
            targets: unwind_dest.iter().copied().collect(),
        },
        AsmTerminator::CatchRet { successor, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CatchRet,
            condition: None,
            targets: vec![*successor],
        },
        AsmTerminator::CatchSwitch { handlers, unwind_dest, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CatchSwitch,
            condition: None,
            targets: handlers.iter().copied().chain(unwind_dest.iter().copied()).collect(),
        },
    }
}

fn resolve_aarch64_branch_condition(condition: &AsmValue, instructions: &[AsmInstruction]) -> Option<Aarch64ConditionCode> {
    match condition {
        AsmValue::Flags(id) => instructions
            .iter()
            .find(|instruction| instruction.id == *id)
            .and_then(|instruction| comparison_code_from_kind(&instruction.kind))
            .map(|code| aarch64_condition_from_asm(&code)),
        other => aarch64_branch_condition(other),
    }
}

fn x86_opcode(kind: &AsmInstructionKind, ty: Option<&AsmType>) -> X86Opcode {
    match kind {
        AsmInstructionKind::Add(..) => X86Opcode::Add,
        AsmInstructionKind::Sub(..) => X86Opcode::Sub,
        AsmInstructionKind::Mul(..) if is_float_type_opt(ty) => float_binop_opcode("mul", ty),
        AsmInstructionKind::Mul(..) => X86Opcode::IMul,
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) if is_float_type_opt(ty) => {
            float_binop_opcode("div", ty)
        }
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) => X86Opcode::IDiv,
        AsmInstructionKind::And(..) => X86Opcode::And,
        AsmInstructionKind::Or(..) => X86Opcode::Or,
        AsmInstructionKind::Xor(..) => X86Opcode::Xor,
        AsmInstructionKind::Shl(..) => X86Opcode::Shl,
        AsmInstructionKind::Shr(..) => X86Opcode::Sar,
        AsmInstructionKind::Not(..) => X86Opcode::Not,
        AsmInstructionKind::Eq(..)
        | AsmInstructionKind::Ne(..)
        | AsmInstructionKind::Lt(..)
        | AsmInstructionKind::Le(..)
        | AsmInstructionKind::Gt(..)
        | AsmInstructionKind::Ge(..) => X86Opcode::Cmp,
        AsmInstructionKind::Load { .. } | AsmInstructionKind::Store { .. } => X86Opcode::Mov,
        AsmInstructionKind::Alloca { .. } => X86Opcode::LeaFrame,
        AsmInstructionKind::GetElementPtr { .. } => X86Opcode::Lea,
        AsmInstructionKind::Bitcast(..)
        | AsmInstructionKind::PtrToInt(..)
        | AsmInstructionKind::IntToPtr(..)
        | AsmInstructionKind::Trunc(..)
        | AsmInstructionKind::ZExt(..)
        | AsmInstructionKind::SExt(..)
        | AsmInstructionKind::SextOrTrunc(..)
        | AsmInstructionKind::Freeze(..) => X86Opcode::Mov,
        AsmInstructionKind::FPExt(..) => X86Opcode::Cvtss2sd,
        AsmInstructionKind::FPTrunc(..) => X86Opcode::Cvtsd2ss,
        AsmInstructionKind::FPToUI(..) | AsmInstructionKind::FPToSI(..) => X86Opcode::Cvttsd2si,
        AsmInstructionKind::UIToFP(..) | AsmInstructionKind::SIToFP(..) => X86Opcode::Cvtsi2sd,
        AsmInstructionKind::ExtractValue { .. } => X86Opcode::MovExtract,
        AsmInstructionKind::InsertValue { .. } => X86Opcode::MovInsert,
        AsmInstructionKind::Call { .. } | AsmInstructionKind::IntrinsicCall { .. } => X86Opcode::Call,
        AsmInstructionKind::Phi { .. } => X86Opcode::PhiCopy,
        AsmInstructionKind::Select { .. } => X86Opcode::CMov,
        AsmInstructionKind::InlineAsm { .. } => X86Opcode::InlineAsm,
        AsmInstructionKind::LandingPad { .. } => X86Opcode::LandingPad,
        AsmInstructionKind::Unreachable => X86Opcode::Ud2,
    }
}

fn float_binop_opcode(base: &str, ty: Option<&AsmType>) -> X86Opcode {
    match ty {
        Some(AsmType::F32) => match base {
            "mul" => X86Opcode::Mulss,
            "div" => X86Opcode::Divss,
            _ => X86Opcode::Mov,
        },
        Some(AsmType::F64) => match base {
            "mul" => X86Opcode::Mulsd,
            "div" => X86Opcode::Divsd,
            _ => X86Opcode::Mov,
        },
        _ => X86Opcode::Mov,
    }
}

fn x86_operands(id: u32, kind: &AsmInstructionKind, ty: Option<&AsmType>) -> Vec<AsmOperand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(register_operand(virtual_register(id, ty), OperandAccess::Write));
        }
    }

    match kind {
        AsmInstructionKind::Add(lhs, rhs)
        | AsmInstructionKind::Sub(lhs, rhs)
        | AsmInstructionKind::Mul(lhs, rhs)
        | AsmInstructionKind::Div(lhs, rhs)
        | AsmInstructionKind::Rem(lhs, rhs)
        | AsmInstructionKind::And(lhs, rhs)
        | AsmInstructionKind::Or(lhs, rhs)
        | AsmInstructionKind::Xor(lhs, rhs)
        | AsmInstructionKind::Shl(lhs, rhs)
        | AsmInstructionKind::Shr(lhs, rhs)
        | AsmInstructionKind::Eq(lhs, rhs)
        | AsmInstructionKind::Ne(lhs, rhs)
        | AsmInstructionKind::Lt(lhs, rhs)
        | AsmInstructionKind::Le(lhs, rhs)
        | AsmInstructionKind::Gt(lhs, rhs)
        | AsmInstructionKind::Ge(lhs, rhs) => {
            operands.push(value_operand(lhs));
            operands.push(value_operand(rhs));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(value_operand(value)),
        AsmInstructionKind::Load { address, .. } => operands.push(address_operand(address, ty)),
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(address_operand(address, None));
            operands.push(value_operand(value));
        }
        AsmInstructionKind::Alloca { size, .. } => operands.push(value_operand(size)),
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(value_operand(ptr));
            operands.extend(indices.iter().map(value_operand));
        }
        AsmInstructionKind::Bitcast(value, _)
        | AsmInstructionKind::Trunc(value, _)
        | AsmInstructionKind::ZExt(value, _)
        | AsmInstructionKind::SExt(value, _)
        | AsmInstructionKind::FPExt(value, _)
        | AsmInstructionKind::FPTrunc(value, _)
        | AsmInstructionKind::FPToUI(value, _)
        | AsmInstructionKind::FPToSI(value, _)
        | AsmInstructionKind::UIToFP(value, _)
        | AsmInstructionKind::SIToFP(value, _)
        | AsmInstructionKind::SextOrTrunc(value, _) => operands.push(value_operand(value)),
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(value_operand(aggregate));
            operands.extend(indices.iter().map(|index| AsmOperand::Immediate(*index as i128)));
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(value_operand(aggregate));
            operands.push(value_operand(element));
            operands.extend(indices.iter().map(|index| AsmOperand::Immediate(*index as i128)));
        }
        AsmInstructionKind::Call { function, args, .. } => {
            operands.push(call_target_operand(function));
            operands.extend(args.iter().map(value_operand));
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(AsmOperand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(args.iter().map(value_operand));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(value_operand(value));
                operands.push(AsmOperand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            operands.push(value_operand(condition));
            operands.push(value_operand(if_true));
            operands.push(value_operand(if_false));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(inputs.iter().map(value_operand));
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(value_operand(personality));
            }
        }
        AsmInstructionKind::Unreachable => {}
    }

    operands
}

fn generic_operands(id: u32, kind: &AsmInstructionKind, ty: Option<&AsmType>) -> Vec<AsmOperand> {
    x86_operands(id, kind, ty)
}

fn instruction_produces_value(kind: &AsmInstructionKind) -> bool {
    !matches!(
        kind,
        AsmInstructionKind::Store { .. }
            | AsmInstructionKind::Call { .. }
            | AsmInstructionKind::IntrinsicCall { .. }
            | AsmInstructionKind::Unreachable
    )
}

fn value_operand(value: &AsmValue) -> AsmOperand {
    match value {
        AsmValue::Register(id) => register_operand(virtual_register(*id, &AsmType::I64), OperandAccess::Read),
        AsmValue::PhysicalRegister(register) => register_operand(AsmRegister::Physical(register.clone()), OperandAccess::Read),
        AsmValue::Address(address) => AsmOperand::Memory(memory_from_address_value(address)),
        AsmValue::Condition(condition) => AsmOperand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition)))),
        AsmValue::Comparison(comparison) => AsmOperand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => AsmOperand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => AsmOperand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) => AsmOperand::Immediate(0),
        AsmValue::Null(_) => AsmOperand::Immediate(0),
    }
}

fn x86_operand(value: &AsmValue, ctx: &mut PhysicalRegisterLoweringContext) -> X86Operand {
    match value {
        AsmValue::Register(id) => X86Operand::Register {
            reg: x86_virtual_register(*id, &AsmType::I64),
            access: OperandAccess::Read,
        },
        AsmValue::PhysicalRegister(register) => X86Operand::Register {
            reg: map_physical_register_to_x86(register, ctx),
            access: OperandAccess::Read,
        },
        AsmValue::Address(address) => x86_address_value_operand(address, ctx),
        AsmValue::Condition(condition) => X86Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition)))),
        AsmValue::Comparison(comparison) => X86Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => X86Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => x86_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => X86Operand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => X86Operand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => X86Operand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) | AsmValue::Null(_) => X86Operand::Immediate(0),
    }
}

fn x86_address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match address {
        AsmValue::Address(address) => x86_memory_or_symbol_from_address(address, ty, ctx),
        AsmValue::Register(id) => X86Operand::Memory(X86MemoryOperand {
            base: Some(x86_virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::PhysicalRegister(register) => X86Operand::Memory(X86MemoryOperand {
            base: Some(map_physical_register_to_x86(register, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => X86Operand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => X86Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => X86Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => x86_operand(address, ctx),
    }
}

fn x86_constant_operand(constant: &AsmConstant) -> X86Operand {
    match constant {
        AsmConstant::Int(value, _) => X86Operand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => X86Operand::Immediate(*value as i128),
        AsmConstant::Bool(value) => X86Operand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => X86Operand::Immediate(0),
        AsmConstant::Float(value, ty) => X86Operand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => X86Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value)))),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => X86Operand::Symbol(name.clone()),
        AsmConstant::Array(..) => X86Operand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => X86Operand::Symbol(Name::new("const.struct")),
    }
}

fn call_target_operand(value: &AsmValue) -> AsmOperand {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => AsmOperand::Symbol(Name::new(name.clone())),
        _ => value_operand(value),
    }
}

fn address_operand(address: &AsmValue, ty: Option<&AsmType>) -> AsmOperand {
    match address {
        AsmValue::Address(address) => {
            let mut memory = memory_from_address_value(address);
            if memory.size_bytes.is_none() {
                memory.size_bytes = ty.map(type_size_bytes);
            }
            AsmOperand::Memory(memory)
        }
        AsmValue::Register(id) => AsmOperand::Memory(AsmMemoryOperand {
            base: Some(virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
            index: None,
            scale: 1,
            displacement: 0,
            segment: None,
            size_bytes: ty.map(type_size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        AsmValue::PhysicalRegister(register) => AsmOperand::Memory(AsmMemoryOperand {
            base: Some(AsmRegister::Physical(register.clone())),
            index: None,
            scale: 1,
            displacement: 0,
            segment: None,
            size_bytes: ty.map(type_size_bytes),
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => AsmOperand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => value_operand(address),
    }
}

fn address_value_from_memory(memory: &AsmMemoryOperand) -> AsmAddressValue {
    AsmAddressValue {
        base: memory.base.as_ref().map(|register| Box::new(register_value_from_asm(register))),
        index: memory.index.as_ref().map(|register| Box::new(register_value_from_asm(register))),
        scale: memory.scale,
        displacement: memory.displacement,
        segment: memory.segment.as_ref().map(|register| Box::new(register_value_from_asm(register))),
        size_bytes: memory.size_bytes,
        address_space: memory.address_space,
        pre_indexed: memory.pre_indexed,
        post_indexed: memory.post_indexed,
    }
}

fn memory_from_address_value(address: &AsmAddressValue) -> AsmMemoryOperand {
    AsmMemoryOperand {
        base: address.base.as_deref().and_then(address_component_register),
        index: address.index.as_deref().and_then(address_component_register),
        scale: address.scale,
        displacement: address.displacement,
        segment: address.segment.as_deref().and_then(address_component_register),
        size_bytes: address.size_bytes,
        address_space: address.address_space,
        pre_indexed: address.pre_indexed,
        post_indexed: address.post_indexed,
    }
}

fn register_value_from_asm(register: &AsmRegister) -> AsmValue {
    match register {
        AsmRegister::Physical(register) => AsmValue::PhysicalRegister(register.clone()),
        AsmRegister::Virtual { id, .. } => AsmValue::Register(*id),
    }
}

fn address_component_register(value: &AsmValue) -> Option<AsmRegister> {
    match value {
        AsmValue::Register(id) => Some(virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
        AsmValue::PhysicalRegister(register) => Some(AsmRegister::Physical(register.clone())),
        _ => None,
    }
}

fn x86_address_value_operand(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    x86_memory_or_symbol_from_address(address, None, ctx)
}

fn x86_memory_or_symbol_from_address(
    address: &AsmAddressValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    if let Some(symbol) = address_symbol_name(address) {
        return X86Operand::Symbol(Name::new(symbol));
    }
    let mut memory = x86_memory_from_address(address, ctx);
    if memory.size_bytes.is_none() {
        memory.size_bytes = ty.map(type_size_bytes);
    }
    X86Operand::Memory(memory)
}

fn x86_memory_from_address(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86MemoryOperand {
    X86MemoryOperand {
        base: address.base.as_deref().and_then(|value| x86_register_from_value(value, ctx)),
        index: address.index.as_deref().and_then(|value| x86_register_from_value(value, ctx)),
        scale: address.scale,
        displacement: address.displacement,
        size_bytes: address.size_bytes,
    }
}

fn aarch64_address_value_operand(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    aarch64_memory_or_symbol_from_address(address, None, ctx)
}

fn aarch64_memory_or_symbol_from_address(
    address: &AsmAddressValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    if let Some(symbol) = address_symbol_name(address) {
        return Aarch64Operand::Symbol(Name::new(symbol));
    }
    let mut memory = aarch64_memory_from_address(address, ctx);
    if memory.size_bytes.is_none() {
        memory.size_bytes = ty.map(type_size_bytes);
    }
    Aarch64Operand::Memory(memory)
}

fn aarch64_memory_from_address(
    address: &AsmAddressValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64MemoryOperand {
    Aarch64MemoryOperand {
        base: address.base.as_deref().and_then(|value| aarch64_register_from_value(value, ctx)),
        index: address.index.as_deref().and_then(|value| aarch64_register_from_value(value, ctx)),
        scale: address.scale,
        displacement: address.displacement,
        size_bytes: address.size_bytes,
    }
}

fn x86_register_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<X86Register> {
    match value {
        AsmValue::Register(id) => Some(x86_virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
        AsmValue::PhysicalRegister(register) => Some(map_physical_register_to_x86(register, ctx)),
        _ => None,
    }
}

fn aarch64_register_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64Register> {
    match value {
        AsmValue::Register(id) => Some(aarch64_virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
        AsmValue::PhysicalRegister(register) => Some(map_physical_register_to_aarch64(register, ctx)),
        _ => None,
    }
}

fn address_symbol_name(address: &AsmAddressValue) -> Option<String> {
    if address.index.is_some() || address.segment.is_some() || address.displacement != 0 {
        return None;
    }
    match address.base.as_deref() {
        Some(AsmValue::Global(name, _)) | Some(AsmValue::Function(name)) => Some(name.clone()),
        _ => None,
    }
}

fn constant_operand(constant: &AsmConstant) -> AsmOperand {
    match constant {
        AsmConstant::Int(value, _) => AsmOperand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => AsmOperand::Immediate(*value as i128),
        AsmConstant::Bool(value) => AsmOperand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => AsmOperand::Immediate(0),
        AsmConstant::Float(value, ty) => AsmOperand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => AsmOperand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value)))),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => AsmOperand::Symbol(name.clone()),
        AsmConstant::Array(..) => AsmOperand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => AsmOperand::Symbol(Name::new("const.struct")),
    }
}

fn register_operand(reg: AsmRegister, access: OperandAccess) -> AsmOperand {
    AsmOperand::Register { reg, access }
}

fn virtual_register(id: u32, ty: &AsmType) -> AsmRegister {
    AsmRegister::Virtual {
        id,
        bank: register_bank(ty),
        size_bits: type_size_bits(ty),
    }
}

fn x86_virtual_register(id: u32, ty: &AsmType) -> X86Register {
    X86Register::Virtual {
        id,
        size_bits: type_size_bits(ty),
    }
}

fn x86_branch_condition(value: &AsmValue) -> Option<X86ConditionCode> {
    match value {
        AsmValue::Condition(condition) => Some(x86_condition_from_asm(condition)),
        AsmValue::Comparison(comparison) => Some(x86_condition_from_asm(&comparison.condition)),
        AsmValue::Flags(_) => None,
        _ => branch_condition_name(value).and_then(parse_x86_condition_token),
    }
}

fn aarch64_branch_condition(value: &AsmValue) -> Option<Aarch64ConditionCode> {
    match value {
        AsmValue::Condition(condition) => Some(aarch64_condition_from_asm(condition)),
        AsmValue::Comparison(comparison) => Some(aarch64_condition_from_asm(&comparison.condition)),
        AsmValue::Flags(_) => None,
        _ => branch_condition_name(value).and_then(parse_aarch64_condition_token),
    }
}

fn branch_condition_name(value: &AsmValue) -> Option<&str> {
    match value {
        AsmValue::Global(name, _) | AsmValue::Function(name) => name.strip_prefix("cc."),
        _ => None,
    }
}

fn x86_condition_from_asm(condition: &AsmConditionCode) -> X86ConditionCode {
    match condition {
        AsmConditionCode::Eq => X86ConditionCode::Equal,
        AsmConditionCode::Ne => X86ConditionCode::NotEqual,
        AsmConditionCode::Lt => X86ConditionCode::Less,
        AsmConditionCode::Le => X86ConditionCode::LessEqual,
        AsmConditionCode::Gt => X86ConditionCode::Greater,
        AsmConditionCode::Ge => X86ConditionCode::GreaterEqual,
        AsmConditionCode::Nz => X86ConditionCode::NonZero,
    }
}

fn aarch64_condition_from_asm(condition: &AsmConditionCode) -> Aarch64ConditionCode {
    match condition {
        AsmConditionCode::Eq => Aarch64ConditionCode::Eq,
        AsmConditionCode::Ne => Aarch64ConditionCode::Ne,
        AsmConditionCode::Lt => Aarch64ConditionCode::Lt,
        AsmConditionCode::Le => Aarch64ConditionCode::Le,
        AsmConditionCode::Gt => Aarch64ConditionCode::Gt,
        AsmConditionCode::Ge => Aarch64ConditionCode::Ge,
        AsmConditionCode::Nz => Aarch64ConditionCode::NonZero,
    }
}

fn asm_condition_from_x86(condition: &X86ConditionCode) -> AsmConditionCode {
    match condition {
        X86ConditionCode::Equal => AsmConditionCode::Eq,
        X86ConditionCode::NotEqual => AsmConditionCode::Ne,
        X86ConditionCode::Less => AsmConditionCode::Lt,
        X86ConditionCode::LessEqual => AsmConditionCode::Le,
        X86ConditionCode::Greater => AsmConditionCode::Gt,
        X86ConditionCode::GreaterEqual => AsmConditionCode::Ge,
        X86ConditionCode::NonZero => AsmConditionCode::Nz,
    }
}

fn asm_condition_from_aarch64(condition: &Aarch64ConditionCode) -> AsmConditionCode {
    match condition {
        Aarch64ConditionCode::Eq => AsmConditionCode::Eq,
        Aarch64ConditionCode::Ne => AsmConditionCode::Ne,
        Aarch64ConditionCode::Lt => AsmConditionCode::Lt,
        Aarch64ConditionCode::Le => AsmConditionCode::Le,
        Aarch64ConditionCode::Gt => AsmConditionCode::Gt,
        Aarch64ConditionCode::Ge => AsmConditionCode::Ge,
        Aarch64ConditionCode::NonZero => AsmConditionCode::Nz,
    }
}

fn lift_x86_instruction(instruction: &X86InstructionDetail, id: u32) -> AsmInstruction {
    let operands = instruction.operands.iter().map(x86_operand_to_asm).collect::<Vec<_>>();
    let type_hint = output_type_from_asm_operands(&operands);
    let kind = semanticize_x86_detail(instruction, &operands).unwrap_or_else(|_| AsmInstructionKind::InlineAsm {
        asm_string: x86_custom_opcode_name(instruction),
        constraints: String::new(),
        inputs: operands.iter().filter_map(asm_operand_to_value).collect(),
        output_type: type_hint.clone().unwrap_or(AsmType::Void),
        side_effects: true,
        align_stack: false,
    });
    AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        type_hint,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn lift_aarch64_instruction(instruction: &Aarch64InstructionDetail, id: u32) -> AsmInstruction {
    let operands = instruction
        .operands
        .iter()
        .map(aarch64_operand_to_asm)
        .collect::<Vec<_>>();
    let type_hint = output_type_from_asm_operands(&operands);
    let kind = semanticize_aarch64_detail(instruction, &operands).unwrap_or_else(|_| AsmInstructionKind::InlineAsm {
        asm_string: aarch64_custom_opcode_name(instruction),
        constraints: String::new(),
        inputs: operands.iter().filter_map(asm_operand_to_value).collect(),
        output_type: type_hint.clone().unwrap_or(AsmType::Void),
        side_effects: true,
        align_stack: false,
    });
    AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        type_hint,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

fn output_type_from_asm_operands(operands: &[AsmOperand]) -> Option<AsmType> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Register {
            reg:
                AsmRegister::Virtual {
                    size_bits, ..
                },
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => Some(type_from_bits(*size_bits)),
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => Some(type_from_bits(register.size_bits)),
        _ => None,
    })
}

fn type_from_bits(size_bits: u16) -> AsmType {
    match size_bits {
        1 => AsmType::I1,
        8 => AsmType::I8,
        16 => AsmType::I16,
        32 => AsmType::I32,
        64 => AsmType::I64,
        128 => AsmType::I128,
        _ => AsmType::I64,
    }
}

fn asm_operand_to_value(operand: &AsmOperand) -> Option<AsmValue> {
    match operand {
        AsmOperand::Register {
            reg: AsmRegister::Virtual { id, .. },
            ..
        } => Some(AsmValue::Register(*id)),
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            ..
        } => Some(AsmValue::PhysicalRegister(register.clone())),
        AsmOperand::Memory(memory) => Some(AsmValue::Address(Box::new(address_value_from_memory(memory)))),
        AsmOperand::Immediate(value) => Some(AsmValue::Constant(AsmConstant::Int(*value as i64, AsmType::I64))),
        AsmOperand::Symbol(name) => Some(AsmValue::Global(name.to_string(), AsmType::Ptr(Box::new(AsmType::I8)))),
        _ => None,
    }
}

fn x86_custom_opcode_name(instruction: &X86InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode, X86Opcode::Cmp | X86Opcode::CMov) => {
            format!("{}.{}", instruction.opcode.mnemonic(), x86_condition_suffix(condition))
        }
        _ => instruction.opcode.mnemonic().to_string(),
    }
}

fn aarch64_custom_opcode_name(instruction: &Aarch64InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode.as_str(), "cmp" | "csel") => {
            format!("{}.{}", instruction.opcode, aarch64_condition_suffix(condition))
        }
        _ => instruction.opcode.clone(),
    }
}

fn x86_condition_suffix(condition: &X86ConditionCode) -> &'static str {
    match condition {
        X86ConditionCode::Equal => "eq",
        X86ConditionCode::NotEqual => "ne",
        X86ConditionCode::Less => "lt",
        X86ConditionCode::LessEqual => "le",
        X86ConditionCode::Greater => "gt",
        X86ConditionCode::GreaterEqual => "ge",
        X86ConditionCode::NonZero => "nz",
    }
}

fn aarch64_condition_suffix(condition: &Aarch64ConditionCode) -> &'static str {
    match condition {
        Aarch64ConditionCode::Eq => "eq",
        Aarch64ConditionCode::Ne => "ne",
        Aarch64ConditionCode::Lt => "lt",
        Aarch64ConditionCode::Le => "le",
        Aarch64ConditionCode::Gt => "gt",
        Aarch64ConditionCode::Ge => "ge",
        Aarch64ConditionCode::NonZero => "nz",
    }
}

fn asm_condition_suffix(condition: &AsmConditionCode) -> &'static str {
    match condition {
        AsmConditionCode::Eq => "eq",
        AsmConditionCode::Ne => "ne",
        AsmConditionCode::Lt => "lt",
        AsmConditionCode::Le => "le",
        AsmConditionCode::Gt => "gt",
        AsmConditionCode::Ge => "ge",
        AsmConditionCode::Nz => "nz",
    }
}

fn x86_operand_to_asm(operand: &X86Operand) -> AsmOperand {
    match operand {
        X86Operand::Register { reg, access } => AsmOperand::Register {
            reg: x86_register_to_asm(reg),
            access: access.clone(),
        },
        X86Operand::Immediate(value) => AsmOperand::Immediate(*value),
        X86Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(x86_register_to_asm),
            index: mem.index.as_ref().map(x86_register_to_asm),
            scale: mem.scale,
            displacement: mem.displacement,
            segment: None,
            size_bytes: mem.size_bytes,
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        X86Operand::Block(id) => AsmOperand::Block(*id),
        X86Operand::Symbol(name) => AsmOperand::Symbol(name.clone()),
    }
}

fn aarch64_operand_to_asm(operand: &Aarch64Operand) -> AsmOperand {
    match operand {
        Aarch64Operand::Register { reg, access } => AsmOperand::Register {
            reg: aarch64_register_to_asm(reg),
            access: access.clone(),
        },
        Aarch64Operand::Immediate(value) => AsmOperand::Immediate(*value),
        Aarch64Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(aarch64_register_to_asm),
            index: mem.index.as_ref().map(aarch64_register_to_asm),
            scale: mem.scale,
            displacement: mem.displacement,
            segment: None,
            size_bytes: mem.size_bytes,
            address_space: None,
            pre_indexed: false,
            post_indexed: false,
        }),
        Aarch64Operand::Block(id) => AsmOperand::Block(*id),
        Aarch64Operand::Symbol(name) => AsmOperand::Symbol(name.clone()),
    }
}

fn x86_register_to_asm(register: &X86Register) -> AsmRegister {
    match register {
        X86Register::Physical { name, size_bits } => AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
            name: name.clone(),
            bank: if name.starts_with("xmm") { AsmRegisterBank::Float } else { AsmRegisterBank::General },
            size_bits: *size_bits,
        }),
        X86Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

fn aarch64_register_to_asm(register: &Aarch64Register) -> AsmRegister {
    match register {
        Aarch64Register::Physical { name, size_bits } => AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
            name: name.clone(),
            bank: if matches!(name.chars().next(), Some('s' | 'd' | 'q' | 'v')) {
                AsmRegisterBank::Float
            } else {
                AsmRegisterBank::General
            },
            size_bits: *size_bits,
        }),
        Aarch64Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

fn asm_operand_to_x86(operand: &AsmOperand, ctx: &mut PhysicalRegisterLoweringContext) -> X86Operand {
    match operand {
        AsmOperand::Register { reg, access } => X86Operand::Register {
            reg: asm_register_to_x86(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => X86Operand::Immediate(*value),
        AsmOperand::Memory(mem) => X86Operand::Memory(X86MemoryOperand {
            base: mem.base.as_ref().map(|register| asm_register_to_x86(register, ctx)),
            index: mem.index.as_ref().map(|register| asm_register_to_x86(register, ctx)),
            scale: mem.scale,
            displacement: mem.displacement,
            size_bytes: mem.size_bytes,
        }),
        AsmOperand::Block(id) => X86Operand::Block(*id),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => X86Operand::Symbol(name.clone()),
        AsmOperand::Relocation(relocation) => X86Operand::Symbol(relocation.symbol.clone()),
        AsmOperand::Predicate { reg, .. } => X86Operand::Register {
            reg: asm_register_to_x86(reg, ctx),
            access: OperandAccess::Read,
        },
    }
}

fn asm_operand_to_aarch64(operand: &AsmOperand, ctx: &mut PhysicalRegisterLoweringContext) -> Aarch64Operand {
    match operand {
        AsmOperand::Register { reg, access } => Aarch64Operand::Register {
            reg: asm_register_to_aarch64(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => Aarch64Operand::Immediate(*value),
        AsmOperand::Memory(mem) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: mem.base.as_ref().map(|register| asm_register_to_aarch64(register, ctx)),
            index: mem.index.as_ref().map(|register| asm_register_to_aarch64(register, ctx)),
            scale: mem.scale,
            displacement: mem.displacement,
            size_bytes: mem.size_bytes,
        }),
        AsmOperand::Block(id) => Aarch64Operand::Block(*id),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => Aarch64Operand::Symbol(name.clone()),
        AsmOperand::Relocation(relocation) => Aarch64Operand::Symbol(relocation.symbol.clone()),
        AsmOperand::Predicate { reg, .. } => Aarch64Operand::Register {
            reg: asm_register_to_aarch64(reg, ctx),
            access: OperandAccess::Read,
        },
    }
}

fn asm_register_to_x86(register: &AsmRegister, ctx: &mut PhysicalRegisterLoweringContext) -> X86Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_x86(physical, ctx),
        AsmRegister::Virtual { id, size_bits, .. } => X86Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        },
    }
}

fn asm_register_to_aarch64(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_aarch64(physical, ctx),
        AsmRegister::Virtual { id, size_bits, .. } => Aarch64Register::Virtual {
            id: *id,
            size_bits: *size_bits,
        },
    }
}

fn map_physical_register_to_x86(
    register: &fp_core::asmir::AsmPhysicalRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Register {
    if is_x86_physical_register_name(&register.name) {
        return X86Register::Physical {
            name: register.name.clone(),
            size_bits: register.size_bits,
        };
    }

    let size_bits = register.size_bits.max(8);
    let name = register.name.as_str();
    if matches!(name, "sp" | "rsp" | "esp" | "fp" | "rbp" | "ebp" | "bp") {
        return X86Register::Physical {
            name: map_general_register_name_to_x86(name, size_bits),
            size_bits,
        };
    }

    X86Register::Virtual {
        id: ctx.virtual_id_for(register),
        size_bits,
    }
}

fn map_physical_register_to_aarch64(
    register: &fp_core::asmir::AsmPhysicalRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Register {
    if is_aarch64_physical_register_name(&register.name) || register.name == "sp" {
        return Aarch64Register::Physical {
            name: register.name.clone(),
            size_bits: register.size_bits,
        };
    }

    let size_bits = register.size_bits.max(8);
    let name = register.name.as_str();
    if matches!(name, "sp" | "rsp" | "esp" | "fp" | "rbp" | "ebp" | "bp") {
        return Aarch64Register::Physical {
            name: map_general_register_name_to_aarch64(name, size_bits),
            size_bits,
        };
    }

    Aarch64Register::Virtual {
        id: ctx.virtual_id_for(register),
        size_bits,
    }
}

fn physical_register_index(name: &str) -> Option<u8> {
    let digits = name.chars().skip_while(|ch| !ch.is_ascii_digit()).collect::<String>();
    if digits.is_empty() {
        None
    } else {
        digits.parse::<u8>().ok()
    }
}

fn map_general_register_name_to_x86(name: &str, size_bits: u16) -> String {
    if name == "sp" {
        return x86_general_register_name(4, size_bits);
    }
    if name == "fp" || name == "x29" || name == "w29" {
        return x86_general_register_name(5, size_bits);
    }
    let index = physical_register_index(name).unwrap_or_else(|| x86_general_register_index(name).unwrap_or(0));
    x86_general_register_name(index, size_bits)
}

fn map_general_register_name_to_aarch64(name: &str, size_bits: u16) -> String {
    if matches!(name, "rsp" | "esp" | "sp") {
        return "sp".to_string();
    }
    if matches!(name, "rbp" | "ebp" | "bp") {
        return if size_bits <= 32 { "w29".to_string() } else { "x29".to_string() };
    }
    let index = x86_general_register_index(name).or_else(|| physical_register_index(name)).unwrap_or(0);
    if size_bits <= 32 {
        format!("w{index}")
    } else {
        format!("x{index}")
    }
}

fn x86_general_register_index(name: &str) -> Option<u8> {
    Some(match name {
        "rax" | "eax" | "ax" | "al" | "ah" => 0,
        "rcx" | "ecx" | "cx" | "cl" | "ch" => 1,
        "rdx" | "edx" | "dx" | "dl" | "dh" => 2,
        "rbx" | "ebx" | "bx" | "bl" | "bh" => 3,
        "rsp" | "esp" | "sp" => 4,
        "rbp" | "ebp" | "bp" => 5,
        "rsi" | "esi" | "si" => 6,
        "rdi" | "edi" | "di" => 7,
        "r8" | "r8d" | "r8w" | "r8b" => 8,
        "r9" | "r9d" | "r9w" | "r9b" => 9,
        "r10" | "r10d" | "r10w" | "r10b" => 10,
        "r11" | "r11d" | "r11w" | "r11b" => 11,
        "r12" | "r12d" | "r12w" | "r12b" => 12,
        "r13" | "r13d" | "r13w" | "r13b" => 13,
        "r14" | "r14d" | "r14w" | "r14b" => 14,
        "r15" | "r15d" | "r15w" | "r15b" => 15,
        _ => return None,
    })
}

fn x86_general_register_name(index: u8, size_bits: u16) -> String {
    match size_bits {
        0..=8 => match index {
            0 => "al".to_string(),
            1 => "cl".to_string(),
            2 => "dl".to_string(),
            3 => "bl".to_string(),
            4 => "spl".to_string(),
            5 => "bpl".to_string(),
            6 => "sil".to_string(),
            7 => "dil".to_string(),
            _ => format!("r{index}b"),
        },
        9..=16 => match index {
            0 => "ax".to_string(),
            1 => "cx".to_string(),
            2 => "dx".to_string(),
            3 => "bx".to_string(),
            4 => "sp".to_string(),
            5 => "bp".to_string(),
            6 => "si".to_string(),
            7 => "di".to_string(),
            _ => format!("r{index}w"),
        },
        17..=32 => match index {
            0 => "eax".to_string(),
            1 => "ecx".to_string(),
            2 => "edx".to_string(),
            3 => "ebx".to_string(),
            4 => "esp".to_string(),
            5 => "ebp".to_string(),
            6 => "esi".to_string(),
            7 => "edi".to_string(),
            _ => format!("r{index}d"),
        },
        _ => match index {
            0 => "rax".to_string(),
            1 => "rcx".to_string(),
            2 => "rdx".to_string(),
            3 => "rbx".to_string(),
            4 => "rsp".to_string(),
            5 => "rbp".to_string(),
            6 => "rsi".to_string(),
            7 => "rdi".to_string(),
            _ => format!("r{index}"),
        },
    }
}

fn x86_call_target_from_operand(operand: &X86Operand) -> X86CallTarget {
    match operand {
        X86Operand::Symbol(name) => X86CallTarget::Symbol(name.clone()),
        X86Operand::Register { reg, .. } => X86CallTarget::Register(reg.clone()),
        _ => X86CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn aarch64_call_target_from_operand(operand: &Aarch64Operand) -> Aarch64CallTarget {
    match operand {
        Aarch64Operand::Symbol(name) => Aarch64CallTarget::Symbol(name.clone()),
        Aarch64Operand::Register { reg, .. } => Aarch64CallTarget::Register(reg.clone()),
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn lift_x86_terminator(terminator: &X86TerminatorDetail) -> AsmTerminator {
    match terminator.opcode {
        X86TerminatorOpcode::Ret => AsmTerminator::Return(None),
        X86TerminatorOpcode::Jmp => AsmTerminator::Br(terminator.targets.first().copied().unwrap_or(0)),
        X86TerminatorOpcode::Jcc => AsmTerminator::CondBr {
            condition: AsmValue::Condition(asm_condition_from_x86(
                terminator.condition.as_ref().unwrap_or(&X86ConditionCode::NonZero),
            )),
            if_true: terminator.targets.first().copied().unwrap_or(0),
            if_false: terminator.targets.get(1).copied().unwrap_or_else(|| terminator.targets.first().copied().unwrap_or(0)),
        },
        X86TerminatorOpcode::Switch => AsmTerminator::Switch {
            value: AsmValue::Undef(AsmType::I64),
            default: terminator.targets.last().copied().unwrap_or(0),
            cases: terminator
                .targets
                .iter()
                .take(terminator.targets.len().saturating_sub(1))
                .enumerate()
                .map(|(index, target)| (index as u64, *target))
                .collect(),
        },
        X86TerminatorOpcode::IndirectJmp => AsmTerminator::IndirectBr {
            address: AsmValue::Global("indirect.branch".to_string(), AsmType::Ptr(Box::new(AsmType::I8))),
            destinations: terminator.targets.clone(),
        },
        X86TerminatorOpcode::Invoke => AsmTerminator::Invoke {
            function: AsmValue::Function("opaque.invoke".to_string()),
            args: Vec::new(),
            normal_dest: terminator.targets.first().copied().unwrap_or(0),
            unwind_dest: terminator.targets.get(1).copied().unwrap_or(0),
            calling_convention: fp_core::lir::CallingConvention::C,
        },
        X86TerminatorOpcode::Resume => AsmTerminator::Resume(AsmValue::Undef(AsmType::I8)),
        X86TerminatorOpcode::CleanupRet => AsmTerminator::CleanupRet {
            cleanup_pad: AsmValue::Undef(AsmType::I8),
            unwind_dest: terminator.targets.first().copied(),
        },
        X86TerminatorOpcode::CatchRet => AsmTerminator::CatchRet {
            catch_pad: AsmValue::Undef(AsmType::I8),
            successor: terminator.targets.first().copied().unwrap_or(0),
        },
        X86TerminatorOpcode::CatchSwitch => AsmTerminator::CatchSwitch {
            parent_pad: None,
            handlers: terminator.targets.clone(),
            unwind_dest: None,
        },
        X86TerminatorOpcode::Ud2 => AsmTerminator::Unreachable,
    }
}

fn lift_aarch64_terminator(terminator: &Aarch64TerminatorDetail) -> AsmTerminator {
    match terminator.opcode {
        Aarch64TerminatorOpcode::Ret => AsmTerminator::Return(None),
        Aarch64TerminatorOpcode::B => AsmTerminator::Br(terminator.targets.first().copied().unwrap_or(0)),
        Aarch64TerminatorOpcode::BCond => AsmTerminator::CondBr {
            condition: AsmValue::Condition(asm_condition_from_aarch64(
                terminator.condition.as_ref().unwrap_or(&Aarch64ConditionCode::NonZero),
            )),
            if_true: terminator.targets.first().copied().unwrap_or(0),
            if_false: terminator.targets.get(1).copied().unwrap_or_else(|| terminator.targets.first().copied().unwrap_or(0)),
        },
        Aarch64TerminatorOpcode::Br => AsmTerminator::IndirectBr {
            address: AsmValue::Global("indirect.branch".to_string(), AsmType::Ptr(Box::new(AsmType::I8))),
            destinations: terminator.targets.clone(),
        },
        Aarch64TerminatorOpcode::Switch => AsmTerminator::Switch {
            value: AsmValue::Undef(AsmType::I64),
            default: terminator.targets.last().copied().unwrap_or(0),
            cases: terminator
                .targets
                .iter()
                .take(terminator.targets.len().saturating_sub(1))
                .enumerate()
                .map(|(index, target)| (index as u64, *target))
                .collect(),
        },
        Aarch64TerminatorOpcode::Invoke => AsmTerminator::Invoke {
            function: AsmValue::Function("opaque.invoke".to_string()),
            args: Vec::new(),
            normal_dest: terminator.targets.first().copied().unwrap_or(0),
            unwind_dest: terminator.targets.get(1).copied().unwrap_or(0),
            calling_convention: fp_core::lir::CallingConvention::C,
        },
        Aarch64TerminatorOpcode::Resume => AsmTerminator::Resume(AsmValue::Undef(AsmType::I8)),
        Aarch64TerminatorOpcode::CleanupRet => AsmTerminator::CleanupRet {
            cleanup_pad: AsmValue::Undef(AsmType::I8),
            unwind_dest: terminator.targets.first().copied(),
        },
        Aarch64TerminatorOpcode::CatchRet => AsmTerminator::CatchRet {
            catch_pad: AsmValue::Undef(AsmType::I8),
            successor: terminator.targets.first().copied().unwrap_or(0),
        },
        Aarch64TerminatorOpcode::CatchSwitch => AsmTerminator::CatchSwitch {
            parent_pad: None,
            handlers: terminator.targets.clone(),
            unwind_dest: None,
        },
        Aarch64TerminatorOpcode::Brk => AsmTerminator::Unreachable,
    }
}

fn semanticize_x86_detail(
    instruction: &X86InstructionDetail,
    operands: &[AsmOperand],
) -> Result<AsmInstructionKind> {
    let opcode_name = x86_custom_opcode_name(instruction);
    let (base, condition) = parse_x86_custom_opcode(&opcode_name);
    let values = collect_machine_values(operands)?;
    match base {
        "add" => binary_value_kind(operands, &values, AsmInstructionKind::Add),
        "sub" => binary_value_kind(operands, &values, AsmInstructionKind::Sub),
        "imul" | "mulss" | "mulsd" => binary_value_kind(operands, &values, AsmInstructionKind::Mul),
        "idiv" | "divss" | "divsd" => binary_value_kind(operands, &values, AsmInstructionKind::Div),
        "and" => binary_value_kind(operands, &values, AsmInstructionKind::And),
        "or" => binary_value_kind(operands, &values, AsmInstructionKind::Or),
        "xor" => binary_value_kind(operands, &values, AsmInstructionKind::Xor),
        "shl" => binary_value_kind(operands, &values, AsmInstructionKind::Shl),
        "sar" => binary_value_kind(operands, &values, AsmInstructionKind::Shr),
        "not" => unary_value_kind(operands, &values, AsmInstructionKind::Not),
        "cmp" => compare_value_kind(operands, &values, condition),
        "mov" => x86_mov_kind(operands, &values),
        "lea" | "lea.frame" => address_kind(operands),
        "call" => call_value_kind(operands, &values),
        "cmov" => select_value_kind(operands, &values),
        _ => Err(fp_core::error::Error::from(format!("unsupported x86 opcode for transpile: {base}"))),
    }
}

fn semanticize_aarch64_detail(
    instruction: &Aarch64InstructionDetail,
    operands: &[AsmOperand],
) -> Result<AsmInstructionKind> {
    let opcode_name = aarch64_custom_opcode_name(instruction);
    let (base, condition) = parse_aarch64_custom_opcode(&opcode_name);
    let values = collect_machine_values(operands)?;
    match base {
        "add" => binary_value_kind(operands, &values, AsmInstructionKind::Add),
        "sub" => binary_value_kind(operands, &values, AsmInstructionKind::Sub),
        "mul" | "fmul.s" | "fmul.d" => binary_value_kind(operands, &values, AsmInstructionKind::Mul),
        "sdiv" | "fdiv.s" | "fdiv.d" => binary_value_kind(operands, &values, AsmInstructionKind::Div),
        "and" => binary_value_kind(operands, &values, AsmInstructionKind::And),
        "orr" => binary_value_kind(operands, &values, AsmInstructionKind::Or),
        "eor" => binary_value_kind(operands, &values, AsmInstructionKind::Xor),
        "lsl" => binary_value_kind(operands, &values, AsmInstructionKind::Shl),
        "asr" => binary_value_kind(operands, &values, AsmInstructionKind::Shr),
        "mvn" => unary_value_kind(operands, &values, AsmInstructionKind::Not),
        "cmp" => compare_value_kind(operands, &values, condition.map(aarch64_condition_to_x86_equivalent)),
        "ldr" => load_kind(operands),
        "str" => store_kind(operands),
        "add.addr" | "add.sp" => address_kind(operands),
        "bl" => call_value_kind(operands, &values),
        "csel" => select_value_kind(operands, &values),
        _ => Err(fp_core::error::Error::from(format!("unsupported aarch64 opcode for transpile: {base}"))),
    }
}

fn collect_machine_values(operands: &[AsmOperand]) -> Result<Vec<AsmValue>> {
    operands
        .iter()
        .map(machine_operand_to_value)
        .collect()
}

fn machine_operand_to_value(operand: &AsmOperand) -> Result<AsmValue> {
    match operand {
        AsmOperand::Register {
            reg: AsmRegister::Virtual { id, .. },
            ..
        } => Ok(AsmValue::Register(*id)),
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            ..
        } => Ok(AsmValue::PhysicalRegister(register.clone())),
        AsmOperand::Immediate(value) => Ok(AsmValue::Constant(AsmConstant::Int(*value as i64, AsmType::I64))),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => Ok(AsmValue::Function(name.to_string())),
        AsmOperand::Block(id) => Ok(AsmValue::Constant(AsmConstant::UInt(*id as u64, AsmType::I32))),
        AsmOperand::Memory(memory) => memory_address_value(memory),
        _ => Err(fp_core::error::Error::from("machine transpile currently supports only register, immediate, symbol, block, and memory operands")),
    }
}

fn memory_address_value(memory: &AsmMemoryOperand) -> Result<AsmValue> {
    Ok(AsmValue::Address(Box::new(address_value_from_memory(memory))))
}

fn binary_value_kind<F>(operands: &[AsmOperand], values: &[AsmValue], build: F) -> Result<AsmInstructionKind>
where
    F: Fn(AsmValue, AsmValue) -> AsmInstructionKind,
{
    let first_read = first_read_operand_index(operands);
    Ok(build(
        values
            .get(first_read)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing lhs operand"))?,
        values
            .get(first_read + 1)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing rhs operand"))?,
    ))
}

fn unary_value_kind<F>(operands: &[AsmOperand], values: &[AsmValue], build: F) -> Result<AsmInstructionKind>
where
    F: Fn(AsmValue) -> AsmInstructionKind,
{
    let first_read = first_read_operand_index(operands);
    Ok(build(
        values
            .get(first_read)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing operand"))?,
    ))
}

fn compare_value_kind(
    operands: &[AsmOperand],
    values: &[AsmValue],
    condition: Option<X86ConditionCode>,
) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    let lhs = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare lhs"))?;
    let rhs = values
        .get(first_read + 1)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare rhs"))?;
    Ok(match condition.unwrap_or(X86ConditionCode::NonZero) {
        X86ConditionCode::Equal => AsmInstructionKind::Eq(lhs, rhs),
        X86ConditionCode::NotEqual => AsmInstructionKind::Ne(lhs, rhs),
        X86ConditionCode::Less => AsmInstructionKind::Lt(lhs, rhs),
        X86ConditionCode::LessEqual => AsmInstructionKind::Le(lhs, rhs),
        X86ConditionCode::Greater => AsmInstructionKind::Gt(lhs, rhs),
        X86ConditionCode::GreaterEqual => AsmInstructionKind::Ge(lhs, rhs),
        X86ConditionCode::NonZero => AsmInstructionKind::Ne(lhs, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))),
    })
}

fn call_value_kind(operands: &[AsmOperand], values: &[AsmValue]) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    let function = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing call target"))?;
    let args = values.iter().skip(first_read + 1).cloned().collect();
    Ok(AsmInstructionKind::Call {
        function,
        args,
        calling_convention: fp_core::lir::CallingConvention::C,
        tail_call: false,
    })
}

fn select_value_kind(operands: &[AsmOperand], values: &[AsmValue]) -> Result<AsmInstructionKind> {
    let first_read = first_read_operand_index(operands);
    Ok(AsmInstructionKind::Select {
        condition: values
            .get(first_read)
            .cloned()
            .unwrap_or_else(|| AsmValue::Constant(AsmConstant::Int(1, AsmType::I1))),
        if_true: values
            .get(first_read + 1)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing select if_true"))?,
        if_false: values
            .get(first_read + 2)
            .cloned()
            .ok_or_else(|| fp_core::error::Error::from("missing select if_false"))?,
    })
}

fn x86_mov_kind(operands: &[AsmOperand], values: &[AsmValue]) -> Result<AsmInstructionKind> {
    match (operands.first(), operands.get(1)) {
        (Some(AsmOperand::Register { .. }), Some(AsmOperand::Memory(_))) => load_kind(operands),
        (Some(AsmOperand::Memory(_)), Some(_)) => store_kind(operands),
        _ => unary_value_kind(operands, values, |value| AsmInstructionKind::Freeze(value)),
    }
}

fn load_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let address = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing load memory operand"))?;
    Ok(AsmInstructionKind::Load {
        address,
        alignment: None,
        volatile: false,
    })
}

fn store_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let address = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing store memory operand"))?;
    let value = operands
        .iter()
        .find(|operand| !matches!(operand, AsmOperand::Memory(_)))
        .ok_or_else(|| fp_core::error::Error::from("missing store value operand"))
        .and_then(machine_operand_to_value)?;
    Ok(AsmInstructionKind::Store {
        value,
        address,
        alignment: None,
        volatile: false,
    })
}

fn address_kind(operands: &[AsmOperand]) -> Result<AsmInstructionKind> {
    let ptr = operands
        .iter()
        .find_map(|operand| match operand {
            AsmOperand::Memory(memory) => Some(memory_address_value(memory)),
            AsmOperand::Register { .. } | AsmOperand::Symbol(_) | AsmOperand::Label(_) => {
                Some(machine_operand_to_value(operand))
            }
            _ => None,
        })
        .transpose()?
        .ok_or_else(|| fp_core::error::Error::from("missing address operand"))?;
    Ok(AsmInstructionKind::GetElementPtr {
        ptr,
        indices: Vec::new(),
        inbounds: false,
    })
}

fn first_read_operand_index(operands: &[AsmOperand]) -> usize {
    operands
        .iter()
        .position(|operand| {
            !matches!(
                operand,
                AsmOperand::Register {
                    access: OperandAccess::Write,
                    ..
                }
            )
        })
        .unwrap_or(0)
}

fn aarch64_condition_to_x86_equivalent(condition: Aarch64ConditionCode) -> X86ConditionCode {
    match condition {
        Aarch64ConditionCode::Eq => X86ConditionCode::Equal,
        Aarch64ConditionCode::Ne => X86ConditionCode::NotEqual,
        Aarch64ConditionCode::Lt => X86ConditionCode::Less,
        Aarch64ConditionCode::Le => X86ConditionCode::LessEqual,
        Aarch64ConditionCode::Gt => X86ConditionCode::Greater,
        Aarch64ConditionCode::Ge => X86ConditionCode::GreaterEqual,
        Aarch64ConditionCode::NonZero => X86ConditionCode::NonZero,
    }
}

fn aarch64_operand(value: &AsmValue, ctx: &mut PhysicalRegisterLoweringContext) -> Aarch64Operand {
    match value {
        AsmValue::Register(id) => Aarch64Operand::Register {
            reg: aarch64_virtual_register(*id, &AsmType::I64),
            access: OperandAccess::Read,
        },
        AsmValue::PhysicalRegister(register) => Aarch64Operand::Register {
            reg: map_physical_register_to_aarch64(register, ctx),
            access: OperandAccess::Read,
        },
        AsmValue::Address(address) => aarch64_address_value_operand(address, ctx),
        AsmValue::Condition(condition) => Aarch64Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition)))),
        AsmValue::Comparison(comparison) => Aarch64Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => Aarch64Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => aarch64_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => Aarch64Operand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => Aarch64Operand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => Aarch64Operand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) | AsmValue::Null(_) => Aarch64Operand::Immediate(0),
    }
}

fn aarch64_address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match address {
        AsmValue::Address(address) => aarch64_memory_or_symbol_from_address(address, ty, ctx),
        AsmValue::Register(id) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: Some(aarch64_virtual_register(*id, &AsmType::Ptr(Box::new(AsmType::I8)))),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::PhysicalRegister(register) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: Some(map_physical_register_to_aarch64(register, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmValue::Global(name, _) | AsmValue::Function(name) => Aarch64Operand::Symbol(Name::new(name.clone())),
        AsmValue::Local(id) => Aarch64Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => Aarch64Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => aarch64_operand(address, ctx),
    }
}

fn aarch64_constant_operand(constant: &AsmConstant) -> Aarch64Operand {
    match constant {
        AsmConstant::Int(value, _) => Aarch64Operand::Immediate(*value as i128),
        AsmConstant::UInt(value, _) => Aarch64Operand::Immediate(*value as i128),
        AsmConstant::Bool(value) => Aarch64Operand::Immediate(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Aarch64Operand::Immediate(0),
        AsmConstant::Float(value, ty) => Aarch64Operand::Immediate(float_bits(*value, ty) as i128),
        AsmConstant::String(value) => Aarch64Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value)))),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => Aarch64Operand::Symbol(name.clone()),
        AsmConstant::Array(..) => Aarch64Operand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => Aarch64Operand::Symbol(Name::new("const.struct")),
    }
}

fn aarch64_virtual_register(id: u32, ty: &AsmType) -> Aarch64Register {
    Aarch64Register::Virtual {
        id,
        size_bits: type_size_bits(ty),
    }
}

fn register_bank(ty: &AsmType) -> AsmRegisterBank {
    match ty {
        AsmType::F32 | AsmType::F64 => AsmRegisterBank::Float,
        AsmType::Vector(..) => AsmRegisterBank::Vector,
        _ => AsmRegisterBank::General,
    }
}

fn type_size_bits(ty: &AsmType) -> u16 {
    let bytes = type_size_bytes(ty);
    if bytes == 0 { 64 } else { bytes.saturating_mul(8) }
}

fn type_size_bytes(ty: &AsmType) -> u16 {
    let size = size_of(ty);
    if size == 0 { 0 } else { size.min(u16::MAX as u64) as u16 }
}

fn is_float_type_opt(ty: Option<&AsmType>) -> bool {
    matches!(ty, Some(AsmType::F32 | AsmType::F64))
}

fn float_bits(value: f64, ty: &AsmType) -> u64 {
    match ty {
        AsmType::F32 => (value as f32).to_bits() as u64,
        _ => value.to_bits(),
    }
}

fn sanitize_symbol(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    out.truncate(24);
    if out.is_empty() {
        "literal".to_string()
    } else {
        out
    }
}

fn generic_opcode(kind: &AsmInstructionKind) -> AsmGenericOpcode {
    match kind {
        AsmInstructionKind::Add(..) => AsmGenericOpcode::Add,
        AsmInstructionKind::Sub(..) => AsmGenericOpcode::Sub,
        AsmInstructionKind::Mul(..) => AsmGenericOpcode::Mul,
        AsmInstructionKind::Div(..) => AsmGenericOpcode::Div,
        AsmInstructionKind::Rem(..) => AsmGenericOpcode::Rem,
        AsmInstructionKind::And(..) => AsmGenericOpcode::And,
        AsmInstructionKind::Or(..) => AsmGenericOpcode::Or,
        AsmInstructionKind::Xor(..) => AsmGenericOpcode::Xor,
        AsmInstructionKind::Shl(..) => AsmGenericOpcode::Shl,
        AsmInstructionKind::Shr(..) => AsmGenericOpcode::Shr,
        AsmInstructionKind::Not(..) => AsmGenericOpcode::Not,
        AsmInstructionKind::Eq(..) => AsmGenericOpcode::Eq,
        AsmInstructionKind::Ne(..) => AsmGenericOpcode::Ne,
        AsmInstructionKind::Lt(..) => AsmGenericOpcode::Lt,
        AsmInstructionKind::Le(..) => AsmGenericOpcode::Le,
        AsmInstructionKind::Gt(..) => AsmGenericOpcode::Gt,
        AsmInstructionKind::Ge(..) => AsmGenericOpcode::Ge,
        AsmInstructionKind::Load { .. } => AsmGenericOpcode::Load,
        AsmInstructionKind::Store { .. } => AsmGenericOpcode::Store,
        AsmInstructionKind::Alloca { .. } => AsmGenericOpcode::Alloca,
        AsmInstructionKind::GetElementPtr { .. } => AsmGenericOpcode::GetElementPtr,
        AsmInstructionKind::Bitcast(..) => AsmGenericOpcode::Bitcast,
        AsmInstructionKind::PtrToInt(..) => AsmGenericOpcode::PtrToInt,
        AsmInstructionKind::IntToPtr(..) => AsmGenericOpcode::IntToPtr,
        AsmInstructionKind::Trunc(..) => AsmGenericOpcode::Trunc,
        AsmInstructionKind::ZExt(..) => AsmGenericOpcode::ZExt,
        AsmInstructionKind::SExt(..) => AsmGenericOpcode::SExt,
        AsmInstructionKind::FPExt(..) => AsmGenericOpcode::FPExt,
        AsmInstructionKind::FPTrunc(..) => AsmGenericOpcode::FPTrunc,
        AsmInstructionKind::FPToUI(..) => AsmGenericOpcode::FPToUI,
        AsmInstructionKind::FPToSI(..) => AsmGenericOpcode::FPToSI,
        AsmInstructionKind::UIToFP(..) => AsmGenericOpcode::UIToFP,
        AsmInstructionKind::SIToFP(..) => AsmGenericOpcode::SIToFP,
        AsmInstructionKind::ExtractValue { .. } => AsmGenericOpcode::ExtractValue,
        AsmInstructionKind::InsertValue { .. } => AsmGenericOpcode::InsertValue,
        AsmInstructionKind::Call { .. } => AsmGenericOpcode::Call,
        AsmInstructionKind::IntrinsicCall { .. } => AsmGenericOpcode::IntrinsicCall,
        AsmInstructionKind::SextOrTrunc(..) => AsmGenericOpcode::SextOrTrunc,
        AsmInstructionKind::Phi { .. } => AsmGenericOpcode::Phi,
        AsmInstructionKind::Select { .. } => AsmGenericOpcode::Select,
        AsmInstructionKind::InlineAsm { .. } => AsmGenericOpcode::InlineAsm,
        AsmInstructionKind::LandingPad { .. } => AsmGenericOpcode::LandingPad,
        AsmInstructionKind::Unreachable => AsmGenericOpcode::Unreachable,
        AsmInstructionKind::Freeze(..) => AsmGenericOpcode::Freeze,
    }
}

fn map_instruction_kind(kind: &LirInstructionKind) -> AsmInstructionKind {
    match kind {
        LirInstructionKind::Add(lhs, rhs) => AsmInstructionKind::Add(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Sub(lhs, rhs) => AsmInstructionKind::Sub(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Mul(lhs, rhs) => AsmInstructionKind::Mul(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Div(lhs, rhs) => AsmInstructionKind::Div(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Rem(lhs, rhs) => AsmInstructionKind::Rem(map_value(lhs), map_value(rhs)),
        LirInstructionKind::And(lhs, rhs) => AsmInstructionKind::And(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Or(lhs, rhs) => AsmInstructionKind::Or(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Xor(lhs, rhs) => AsmInstructionKind::Xor(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Shl(lhs, rhs) => AsmInstructionKind::Shl(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Shr(lhs, rhs) => AsmInstructionKind::Shr(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Not(value) => AsmInstructionKind::Not(map_value(value)),
        LirInstructionKind::Eq(lhs, rhs) => AsmInstructionKind::Eq(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Ne(lhs, rhs) => AsmInstructionKind::Ne(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Lt(lhs, rhs) => AsmInstructionKind::Lt(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Le(lhs, rhs) => AsmInstructionKind::Le(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Gt(lhs, rhs) => AsmInstructionKind::Gt(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Ge(lhs, rhs) => AsmInstructionKind::Ge(map_value(lhs), map_value(rhs)),
        LirInstructionKind::Load {
            address,
            volatile,
            alignment,
        } => AsmInstructionKind::Load {
            address: map_value(address),
            alignment: *alignment,
            volatile: *volatile,
        },
        LirInstructionKind::Store {
            value,
            address,
            volatile,
            alignment,
        } => AsmInstructionKind::Store {
            value: map_value(value),
            address: map_value(address),
            alignment: *alignment,
            volatile: *volatile,
        },
        LirInstructionKind::Alloca { size, alignment } => AsmInstructionKind::Alloca {
            size: map_value(size),
            alignment: *alignment,
        },
        LirInstructionKind::GetElementPtr {
            ptr,
            indices,
            inbounds,
        } => AsmInstructionKind::GetElementPtr {
            ptr: map_value(ptr),
            indices: indices.iter().map(map_value).collect(),
            inbounds: *inbounds,
        },
        LirInstructionKind::Bitcast(value, ty) => AsmInstructionKind::Bitcast(map_value(value), ty.clone()),
        LirInstructionKind::PtrToInt(value) => AsmInstructionKind::PtrToInt(map_value(value)),
        LirInstructionKind::IntToPtr(value) => AsmInstructionKind::IntToPtr(map_value(value)),
        LirInstructionKind::Trunc(value, ty) => AsmInstructionKind::Trunc(map_value(value), ty.clone()),
        LirInstructionKind::ZExt(value, ty) => AsmInstructionKind::ZExt(map_value(value), ty.clone()),
        LirInstructionKind::SExt(value, ty) => AsmInstructionKind::SExt(map_value(value), ty.clone()),
        LirInstructionKind::FPExt(value, ty) => AsmInstructionKind::FPExt(map_value(value), ty.clone()),
        LirInstructionKind::FPTrunc(value, ty) => AsmInstructionKind::FPTrunc(map_value(value), ty.clone()),
        LirInstructionKind::FPToUI(value, ty) => AsmInstructionKind::FPToUI(map_value(value), ty.clone()),
        LirInstructionKind::FPToSI(value, ty) => AsmInstructionKind::FPToSI(map_value(value), ty.clone()),
        LirInstructionKind::UIToFP(value, ty) => AsmInstructionKind::UIToFP(map_value(value), ty.clone()),
        LirInstructionKind::SIToFP(value, ty) => AsmInstructionKind::SIToFP(map_value(value), ty.clone()),
        LirInstructionKind::ExtractValue { aggregate, indices } => AsmInstructionKind::ExtractValue {
            aggregate: map_value(aggregate),
            indices: indices.clone(),
        },
        LirInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => AsmInstructionKind::InsertValue {
            aggregate: map_value(aggregate),
            element: map_value(element),
            indices: indices.clone(),
        },
        LirInstructionKind::Call {
            function,
            args,
            calling_convention,
            tail_call,
        } => AsmInstructionKind::Call {
            function: map_value(function),
            args: args.iter().map(map_value).collect(),
            calling_convention: calling_convention.clone(),
            tail_call: *tail_call,
        },
        LirInstructionKind::IntrinsicCall { kind, format, args } => AsmInstructionKind::IntrinsicCall {
            kind: map_intrinsic(kind),
            format: format.clone(),
            args: args.iter().map(map_value).collect(),
        },
        LirInstructionKind::SextOrTrunc(value, ty) => {
            AsmInstructionKind::SextOrTrunc(map_value(value), ty.clone())
        }
        LirInstructionKind::Phi { incoming } => AsmInstructionKind::Phi {
            incoming: incoming
                .iter()
                .map(|(value, block)| (map_value(value), *block))
                .collect(),
        },
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => AsmInstructionKind::Select {
            condition: map_value(condition),
            if_true: map_value(if_true),
            if_false: map_value(if_false),
        },
        LirInstructionKind::InlineAsm {
            asm_string,
            constraints,
            inputs,
            output_type,
            side_effects,
            align_stack,
        } => AsmInstructionKind::InlineAsm {
            asm_string: asm_string.clone(),
            constraints: constraints.clone(),
            inputs: inputs.iter().map(map_value).collect(),
            output_type: output_type.clone(),
            side_effects: *side_effects,
            align_stack: *align_stack,
        },
        LirInstructionKind::LandingPad {
            result_type,
            personality,
            cleanup,
            clauses,
        } => AsmInstructionKind::LandingPad {
            result_type: result_type.clone(),
            personality: personality.as_ref().map(map_value),
            cleanup: *cleanup,
            clauses: clauses.iter().map(map_clause).collect(),
        },
        LirInstructionKind::Unreachable => AsmInstructionKind::Unreachable,
        LirInstructionKind::Freeze(value) => AsmInstructionKind::Freeze(map_value(value)),
    }
}

fn map_terminator(term: &LirTerminator) -> AsmTerminator {
    match term {
        LirTerminator::Return(value) => AsmTerminator::Return(value.as_ref().map(map_value)),
        LirTerminator::Br(target) => AsmTerminator::Br(*target),
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => AsmTerminator::CondBr {
            condition: map_value(condition),
            if_true: *if_true,
            if_false: *if_false,
        },
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => AsmTerminator::Switch {
            value: map_value(value),
            default: *default,
            cases: cases.clone(),
        },
        LirTerminator::IndirectBr {
            address,
            destinations,
        } => AsmTerminator::IndirectBr {
            address: map_value(address),
            destinations: destinations.clone(),
        },
        LirTerminator::Invoke {
            function,
            args,
            normal_dest,
            unwind_dest,
            calling_convention,
        } => AsmTerminator::Invoke {
            function: map_value(function),
            args: args.iter().map(map_value).collect(),
            normal_dest: *normal_dest,
            unwind_dest: *unwind_dest,
            calling_convention: calling_convention.clone(),
        },
        LirTerminator::Resume(value) => AsmTerminator::Resume(map_value(value)),
        LirTerminator::Unreachable => AsmTerminator::Unreachable,
        LirTerminator::CleanupRet {
            cleanup_pad,
            unwind_dest,
        } => AsmTerminator::CleanupRet {
            cleanup_pad: map_value(cleanup_pad),
            unwind_dest: *unwind_dest,
        },
        LirTerminator::CatchRet {
            catch_pad,
            successor,
        } => AsmTerminator::CatchRet {
            catch_pad: map_value(catch_pad),
            successor: *successor,
        },
        LirTerminator::CatchSwitch {
            parent_pad,
            handlers,
            unwind_dest,
        } => AsmTerminator::CatchSwitch {
            parent_pad: parent_pad.as_ref().map(map_value),
            handlers: handlers.clone(),
            unwind_dest: *unwind_dest,
        },
    }
}

fn map_value(value: &LirValue) -> AsmValue {
    match value {
        LirValue::Register(id) => AsmValue::Register(*id),
        LirValue::Constant(constant) => AsmValue::Constant(map_constant(constant)),
        LirValue::Global(name, ty) => AsmValue::Global(name.clone(), ty.clone()),
        LirValue::Function(name) => AsmValue::Function(name.clone()),
        LirValue::Local(id) => AsmValue::Local(*id),
        LirValue::StackSlot(id) => AsmValue::StackSlot(*id),
        LirValue::Undef(ty) => AsmValue::Undef(ty.clone()),
        LirValue::Null(ty) => AsmValue::Null(ty.clone()),
    }
}

fn map_constant(constant: &LirConstant) -> AsmConstant {
    match constant {
        LirConstant::Int(value, ty) => AsmConstant::Int(*value, ty.clone()),
        LirConstant::UInt(value, ty) => AsmConstant::UInt(*value, ty.clone()),
        LirConstant::Float(value, ty) => AsmConstant::Float(*value, ty.clone()),
        LirConstant::Bool(value) => AsmConstant::Bool(*value),
        LirConstant::String(value) => AsmConstant::String(value.clone()),
        LirConstant::Array(values, ty) => {
            AsmConstant::Array(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstant::Struct(values, ty) => {
            AsmConstant::Struct(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstant::GlobalRef(name, ty, path) => {
            AsmConstant::GlobalRef(name.clone(), ty.clone(), path.clone())
        }
        LirConstant::FunctionRef(name, ty) => AsmConstant::FunctionRef(name.clone(), ty.clone()),
        LirConstant::Null(ty) => AsmConstant::Null(ty.clone()),
        LirConstant::Undef(ty) => AsmConstant::Undef(ty.clone()),
    }
}

fn map_intrinsic(kind: &LirIntrinsicKind) -> AsmIntrinsicKind {
    match kind {
        LirIntrinsicKind::Print => AsmIntrinsicKind::Print,
        LirIntrinsicKind::Println => AsmIntrinsicKind::Println,
        LirIntrinsicKind::Format => AsmIntrinsicKind::Format,
        LirIntrinsicKind::TimeNow => AsmIntrinsicKind::TimeNow,
    }
}

fn map_clause(clause: &fp_core::lir::LandingPadClause) -> AsmLandingPadClause {
    match clause {
        fp_core::lir::LandingPadClause::Catch(value) => AsmLandingPadClause::Catch(map_value(value)),
        fp_core::lir::LandingPadClause::Filter(values) => {
            AsmLandingPadClause::Filter(values.iter().map(map_value).collect())
        }
    }
}

fn map_arch(arch: TargetArch) -> AsmArchitecture {
    match arch {
        TargetArch::X86_64 => AsmArchitecture::X86_64,
        TargetArch::Aarch64 => AsmArchitecture::Aarch64,
    }
}

fn map_format(format: TargetFormat) -> AsmObjectFormat {
    match format {
        TargetFormat::MachO => AsmObjectFormat::MachO,
        TargetFormat::Elf => AsmObjectFormat::Elf,
        TargetFormat::Coff => AsmObjectFormat::Coff,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64, select_program,
    };
    use crate::asm::aarch64::{
        AsmAarch64Block, AsmAarch64Function, AsmAarch64Program, Aarch64InstructionDetail,
        Aarch64Operand, Aarch64Register, Aarch64TerminatorDetail,
    };
    use crate::asm::x86_64::{AsmX86_64Block, AsmX86_64Function, AsmX86_64Program, X86InstructionDetail, X86Operand, X86Register, X86TerminatorDetail};
    use crate::asm::aarch64::{Aarch64CallTarget, Aarch64ConditionCode, Aarch64TerminatorOpcode};
    use crate::asm::x86_64::{X86CallTarget, X86ConditionCode, X86Opcode, X86TerminatorOpcode};
    use crate::emit::{TargetArch, TargetFormat};
    use fp_core::asmir::{
        AsmConditionCode, AsmGenericOpcode, AsmInstructionKind, AsmOpcode, AsmOperand,
        AsmTerminator, AsmValue, OperandAccess,
    };
    use fp_core::lir::{
        CallingConvention, LirBasicBlock, LirFunction, LirFunctionSignature, LirInstruction,
        LirInstructionKind, LirProgram, LirTerminator, LirType, Name,
    };

    #[test]
    fn select_program_builds_semantic_asmir() {
        let lir = LirProgram {
            functions: vec![LirFunction {
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: vec![LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::Freeze(fp_core::lir::LirValue::Undef(LirType::I32)),
                        type_hint: Some(LirType::I32),
                        debug_info: None,
                    }],
                    terminator: LirTerminator::Return(Some(fp_core::lir::LirValue::Register(1))),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                }],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        assert_eq!(program.functions.len(), 1);
        assert!(matches!(
            program.functions[0].basic_blocks[0].instructions[0].kind,
            AsmInstructionKind::Freeze(_)
        ));
        assert!(matches!(
            program.functions[0].basic_blocks[0].terminator,
            AsmTerminator::Return(Some(AsmValue::Register(1)))
        ));
    }

    #[test]
    fn select_program_normalizes_x86_opcode_and_operands() {
        let lir = LirProgram {
            functions: vec![LirFunction {
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: vec![LirInstruction {
                        id: 7,
                        kind: LirInstructionKind::Add(
                            fp_core::lir::LirValue::Register(1),
                            fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(4, LirType::I32)),
                        ),
                        type_hint: Some(LirType::I32),
                        debug_info: None,
                    }],
                    terminator: LirTerminator::Return(Some(fp_core::lir::LirValue::Register(7))),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                }],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let inst = &program.functions[0].basic_blocks[0].instructions[0];

        assert_eq!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Add));
        assert_eq!(inst.operands.len(), 3);
        assert!(matches!(
            &inst.operands[0],
            AsmOperand::Register { access: OperandAccess::Write, .. }
        ));
        assert!(matches!(&inst.operands[1], AsmOperand::Register { .. }));
        assert!(matches!(&inst.operands[2], AsmOperand::Immediate(4)));

        let x86 = lower_to_x86_64(&program);
        let inst = &x86.functions[0].blocks[0].instructions[0];
        assert_eq!(inst.opcode, X86Opcode::Add);
    }

    #[test]
    fn select_program_records_x86_condition_and_call_target() {
        let lir = LirProgram {
            functions: vec![LirFunction {
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: vec![
                        LirInstruction {
                            id: 1,
                            kind: LirInstructionKind::Eq(
                                fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I32)),
                                fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(2, LirType::I32)),
                            ),
                            type_hint: Some(LirType::I1),
                            debug_info: None,
                        },
                        LirInstruction {
                            id: 2,
                            kind: LirInstructionKind::Call {
                                function: fp_core::lir::LirValue::Function("callee".to_string()),
                                args: Vec::new(),
                                calling_convention: CallingConvention::C,
                                tail_call: false,
                            },
                            type_hint: Some(LirType::I32),
                            debug_info: None,
                        },
                    ],
                    terminator: LirTerminator::Return(Some(fp_core::lir::LirValue::Register(2))),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                }],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let x86 = lower_to_x86_64(&program);
        let eq_inst = &x86.functions[0].blocks[0].instructions[0];
        let call_inst = &x86.functions[0].blocks[0].instructions[1];

        assert!(matches!(
            eq_inst.condition,
            Some(X86ConditionCode::Equal)
        ));
        assert!(matches!(
            call_inst.call_target,
            Some(X86CallTarget::Symbol(ref name)) if *name == Name::new("callee")
        ));
    }

    #[test]
    fn lower_to_aarch64_preserves_concrete_branch_and_call_metadata() {
        let lir = LirProgram {
            functions: vec![LirFunction {
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: vec![
                        LirInstruction {
                            id: 1,
                            kind: LirInstructionKind::Eq(
                                fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I32)),
                                fp_core::lir::LirValue::Constant(fp_core::lir::LirConstant::Int(2, LirType::I32)),
                            ),
                            type_hint: Some(LirType::I1),
                            debug_info: None,
                        },
                        LirInstruction {
                            id: 2,
                            kind: LirInstructionKind::Call {
                                function: fp_core::lir::LirValue::Function("callee".to_string()),
                                args: Vec::new(),
                                calling_convention: CallingConvention::C,
                                tail_call: false,
                            },
                            type_hint: Some(LirType::I32),
                            debug_info: None,
                        },
                    ],
                    terminator: LirTerminator::Br(1),
                    predecessors: Vec::new(),
                    successors: vec![1],
                }, LirBasicBlock {
                    id: 1,
                    label: Some(Name::new("exit")),
                    instructions: Vec::new(),
                    terminator: LirTerminator::Return(Some(fp_core::lir::LirValue::Register(2))),
                    predecessors: vec![0],
                    successors: Vec::new(),
                }],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::Aarch64).unwrap();
        let aarch64 = lower_to_aarch64(&program);
        let eq_inst = &aarch64.functions[0].blocks[0].instructions[0];
        let call_inst = &aarch64.functions[0].blocks[0].instructions[1];
        let terminator = &aarch64.functions[0].blocks[0].terminator;

        assert_eq!(eq_inst.opcode, "cmp");
        assert_eq!(eq_inst.condition, Some(Aarch64ConditionCode::Eq));
        assert!(matches!(
            call_inst.call_target,
            Some(Aarch64CallTarget::Symbol(ref name)) if *name == Name::new("callee")
        ));
        assert_eq!(terminator.opcode, Aarch64TerminatorOpcode::B);
        assert_eq!(terminator.targets, vec![1]);
    }

    #[test]
    fn lower_to_x86_64_skips_declarations_and_maps_terminators() {
        let lir = LirProgram {
            functions: vec![
                LirFunction {
                    name: Name::new("decl"),
                    signature: LirFunctionSignature {
                        params: Vec::new(),
                        return_type: LirType::I32,
                        is_variadic: false,
                    },
                    basic_blocks: Vec::new(),
                    locals: Vec::new(),
                    stack_slots: Vec::new(),
                    calling_convention: CallingConvention::C,
                    linkage: fp_core::lir::Linkage::External,
                    is_declaration: true,
                },
                LirFunction {
                    name: Name::new("main"),
                    signature: LirFunctionSignature {
                        params: Vec::new(),
                        return_type: LirType::I32,
                        is_variadic: false,
                    },
                    basic_blocks: vec![LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::CondBr {
                            condition: fp_core::lir::LirValue::Register(1),
                            if_true: 1,
                            if_false: 2,
                        },
                        predecessors: Vec::new(),
                        successors: vec![1, 2],
                    }],
                    locals: Vec::new(),
                    stack_slots: Vec::new(),
                    calling_convention: CallingConvention::C,
                    linkage: fp_core::lir::Linkage::External,
                    is_declaration: false,
                },
            ],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let x86 = lower_to_x86_64(&program);

        assert_eq!(x86.functions.len(), 1);
        assert_eq!(x86.functions[0].name, Name::new("main"));
        assert_eq!(x86.functions[0].blocks[0].terminator.opcode, X86TerminatorOpcode::Jcc);
        assert_eq!(x86.functions[0].blocks[0].terminator.targets, vec![1, 2]);
    }

    #[test]
    fn lift_from_x86_64_roundtrips_through_asmir() {
        let x86 = AsmX86_64Program {
            functions: vec![AsmX86_64Function {
                name: Name::new("main"),
                blocks: vec![AsmX86_64Block {
                    id: 0,
                    instructions: vec![X86InstructionDetail {
                        opcode: X86Opcode::Add,
                        operands: vec![
                            X86Operand::Register {
                                reg: X86Register::Virtual { id: 1, size_bits: 64 },
                                access: OperandAccess::Write,
                            },
                            X86Operand::Register {
                                reg: X86Register::Virtual { id: 2, size_bits: 64 },
                                access: OperandAccess::Read,
                            },
                            X86Operand::Immediate(4),
                        ],
                        condition: None,
                        call_target: None,
                    }],
                    terminator: X86TerminatorDetail {
                        opcode: X86TerminatorOpcode::Jcc,
                        condition: Some(X86ConditionCode::NotEqual),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let asmir = lift_from_x86_64(&x86);
        let lowered = lower_to_x86_64(&asmir);

        let lowered_inst = &lowered.functions[0].blocks[0].instructions[0];
        let original_inst = &x86.functions[0].blocks[0].instructions[0];
        assert_eq!(lowered_inst.opcode, original_inst.opcode);
        assert_eq!(lowered_inst.operands[1..], original_inst.operands[1..]);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmValue::Condition(AsmConditionCode::Ne),
                if_true: 1,
                if_false: 2,
            }
        ));
        assert_eq!(lowered.functions[0].blocks[0].terminator, x86.functions[0].blocks[0].terminator);
    }

    #[test]
    fn lift_from_aarch64_roundtrips_through_asmir() {
        let aarch64 = AsmAarch64Program {
            functions: vec![AsmAarch64Function {
                name: Name::new("main"),
                blocks: vec![AsmAarch64Block {
                    id: 0,
                    instructions: vec![Aarch64InstructionDetail {
                        opcode: "add".to_string(),
                        operands: vec![
                            Aarch64Operand::Register {
                                reg: Aarch64Register::Virtual { id: 1, size_bits: 64 },
                                access: OperandAccess::Write,
                            },
                            Aarch64Operand::Register {
                                reg: Aarch64Register::Virtual { id: 2, size_bits: 64 },
                                access: OperandAccess::Read,
                            },
                            Aarch64Operand::Immediate(7),
                        ],
                        condition: None,
                        call_target: None,
                    }],
                    terminator: Aarch64TerminatorDetail {
                        opcode: Aarch64TerminatorOpcode::BCond,
                        condition: Some(Aarch64ConditionCode::Ge),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let asmir = lift_from_aarch64(&aarch64);
        let lowered = lower_to_aarch64(&asmir);

        let lowered_inst = &lowered.functions[0].blocks[0].instructions[0];
        let original_inst = &aarch64.functions[0].blocks[0].instructions[0];
        assert_eq!(lowered_inst.opcode, original_inst.opcode);
        assert_eq!(lowered_inst.operands[1..], original_inst.operands[1..]);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmValue::Condition(AsmConditionCode::Ge),
                if_true: 1,
                if_false: 2,
            }
        ));
        assert_eq!(lowered.functions[0].blocks[0].terminator, aarch64.functions[0].blocks[0].terminator);
    }

    #[test]
    fn lift_from_x86_64_links_compare_instruction_into_branch_condition() {
        let x86 = AsmX86_64Program {
            functions: vec![AsmX86_64Function {
                name: Name::new("main"),
                blocks: vec![AsmX86_64Block {
                    id: 0,
                    instructions: vec![X86InstructionDetail {
                        opcode: X86Opcode::Cmp,
                        operands: vec![
                            X86Operand::Register {
                                reg: X86Register::Virtual { id: 2, size_bits: 64 },
                                access: OperandAccess::Read,
                            },
                            X86Operand::Immediate(4),
                        ],
                        condition: Some(X86ConditionCode::NotEqual),
                        call_target: None,
                    }],
                    terminator: X86TerminatorDetail {
                        opcode: X86TerminatorOpcode::Jcc,
                        condition: Some(X86ConditionCode::NotEqual),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let asmir = lift_from_x86_64(&x86);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmValue::Flags(0),
                if_true: 1,
                if_false: 2,
            }
        ));
    }

    #[test]
    fn lift_from_aarch64_links_compare_instruction_into_branch_condition() {
        let aarch64 = AsmAarch64Program {
            functions: vec![AsmAarch64Function {
                name: Name::new("main"),
                blocks: vec![AsmAarch64Block {
                    id: 0,
                    instructions: vec![Aarch64InstructionDetail {
                        opcode: "cmp.ge".to_string(),
                        operands: vec![
                            Aarch64Operand::Register {
                                reg: Aarch64Register::Virtual { id: 2, size_bits: 64 },
                                access: OperandAccess::Read,
                            },
                            Aarch64Operand::Immediate(7),
                        ],
                        condition: Some(Aarch64ConditionCode::Ge),
                        call_target: None,
                    }],
                    terminator: Aarch64TerminatorDetail {
                        opcode: Aarch64TerminatorOpcode::BCond,
                        condition: Some(Aarch64ConditionCode::Ge),
                        targets: vec![1, 2],
                    },
                }],
            }],
        };

        let asmir = lift_from_aarch64(&aarch64);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmValue::Flags(0),
                if_true: 1,
                if_false: 2,
            }
        ));
    }

    #[test]
    fn lift_from_x86_64_preserves_indirect_calls_and_addressing_shapes() {
        let x86 = AsmX86_64Program {
            functions: vec![AsmX86_64Function {
                name: Name::new("main"),
                blocks: vec![AsmX86_64Block {
                    id: 0,
                    instructions: vec![
                        X86InstructionDetail {
                            opcode: X86Opcode::Mov,
                            operands: vec![
                                X86Operand::Register {
                                    reg: X86Register::Physical {
                                        name: "rax".to_string(),
                                        size_bits: 64,
                                    },
                                    access: OperandAccess::Write,
                                },
                                X86Operand::Memory(crate::asm::x86_64::X86MemoryOperand {
                                    base: Some(X86Register::Physical {
                                        name: "rbx".to_string(),
                                        size_bits: 64,
                                    }),
                                    index: Some(X86Register::Physical {
                                        name: "rcx".to_string(),
                                        size_bits: 64,
                                    }),
                                    scale: 2,
                                    displacement: 8,
                                    size_bytes: Some(8),
                                }),
                            ],
                            condition: None,
                            call_target: None,
                        },
                        X86InstructionDetail {
                            opcode: X86Opcode::Call,
                            operands: vec![X86Operand::Register {
                                reg: X86Register::Physical {
                                    name: "rax".to_string(),
                                    size_bits: 64,
                                },
                                access: OperandAccess::Read,
                            }],
                            condition: None,
                            call_target: Some(X86CallTarget::Register(X86Register::Physical {
                                name: "rax".to_string(),
                                size_bits: 64,
                            })),
                        },
                    ],
                    terminator: X86TerminatorDetail {
                        opcode: X86TerminatorOpcode::Ret,
                        condition: None,
                        targets: Vec::new(),
                    },
                }],
            }],
        };

        let asmir = lift_from_x86_64(&x86);
        let lowered = lower_to_x86_64(&asmir);

        assert_eq!(lowered.functions[0].blocks[0].instructions, x86.functions[0].blocks[0].instructions);
    }

    #[test]
    fn lift_from_aarch64_preserves_indirect_calls_and_addressing_shapes() {
        let aarch64 = AsmAarch64Program {
            functions: vec![AsmAarch64Function {
                name: Name::new("main"),
                blocks: vec![AsmAarch64Block {
                    id: 0,
                    instructions: vec![
                        Aarch64InstructionDetail {
                            opcode: "str".to_string(),
                            operands: vec![
                                Aarch64Operand::Register {
                                    reg: Aarch64Register::Physical {
                                        name: "x3".to_string(),
                                        size_bits: 64,
                                    },
                                    access: OperandAccess::Read,
                                },
                                Aarch64Operand::Memory(crate::asm::aarch64::Aarch64MemoryOperand {
                                    base: Some(Aarch64Register::Physical {
                                        name: "x1".to_string(),
                                        size_bits: 64,
                                    }),
                                    index: Some(Aarch64Register::Physical {
                                        name: "x2".to_string(),
                                        size_bits: 64,
                                    }),
                                    scale: 3,
                                    displacement: 16,
                                    size_bytes: Some(8),
                                }),
                            ],
                            condition: None,
                            call_target: None,
                        },
                        Aarch64InstructionDetail {
                            opcode: "bl".to_string(),
                            operands: vec![Aarch64Operand::Register {
                                reg: Aarch64Register::Physical {
                                    name: "x0".to_string(),
                                    size_bits: 64,
                                },
                                access: OperandAccess::Read,
                            }],
                            condition: None,
                            call_target: Some(Aarch64CallTarget::Register(Aarch64Register::Physical {
                                name: "x0".to_string(),
                                size_bits: 64,
                            })),
                        },
                    ],
                    terminator: Aarch64TerminatorDetail {
                        opcode: Aarch64TerminatorOpcode::Ret,
                        condition: None,
                        targets: Vec::new(),
                    },
                }],
            }],
        };

        let asmir = lift_from_aarch64(&aarch64);
        let lowered = lower_to_aarch64(&asmir);

        assert_eq!(lowered.functions[0].blocks[0].instructions, aarch64.functions[0].blocks[0].instructions);
    }
}
