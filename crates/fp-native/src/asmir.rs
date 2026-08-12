mod normalize;
pub use normalize::normalize_for_target;

use crate::asm::aarch64::{
    Aarch64CallTarget, Aarch64ConditionCode, Aarch64InstructionDetail, Aarch64MemoryOperand,
    Aarch64Operand, Aarch64Register, Aarch64TerminatorDetail, Aarch64TerminatorOpcode,
};
use crate::asm::x86_64::{
    X86CallTarget, X86ConditionCode, X86InstructionDetail, X86MemoryOperand, X86Opcode, X86Operand,
    X86Register, X86TerminatorDetail, X86TerminatorOpcode,
};
use crate::asm::{aarch64 as aarch64_asm, x86_64 as x86_64_asm};
use crate::emit::{TargetArch, TargetFormat};
use fp_core::asmir::{
    AsmAddressValue, AsmArchitecture, AsmAttr, AsmBlock, AsmConditionCode, AsmConstant,
    AsmEndianness, AsmFunction, AsmFunctionSignature, AsmGenericOpcode, AsmGlobal, AsmInstruction,
    AsmIntrinsicKind, AsmMemoryOperand, AsmObjectFormat, AsmOpcode, AsmOperand,
    AsmPhysicalRegister, AsmProgram, AsmRegister, AsmRegisterBank, AsmSection, AsmSectionFlag,
    AsmSectionKind, AsmSyscallConvention, AsmTarget, AsmTerminator, AsmType, AsmTypeDefinition,
    AsmVirtualRegId, OperandAccess,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{
    LandingPadClause, LirConstant, LirConstantAggregate, LirConstantData, LirConstantExpr,
    LirConstantKind, LirFloat, LirFunction, LirInstruction, LirInstructionKind, LirInteger,
    LirIntrinsicKind, LirProgram, LirTerminator, LirValue, LirValueKind, Name, RegisterId,
    Visibility,
};
use std::collections::HashMap;

pub fn select_program(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<AsmProgram> {
    let mut program = AsmProgram::new(
        AsmTarget {
            architecture: map_arch(arch),
            object_format: map_format(format),
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        },
        lir_program.data_layout.clone(),
    );

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

    program.globals = lir_program.globals.iter().map(map_global).collect();

    for function in &lir_program.functions {
        program.functions.push(select_function(function));
    }

    normalize_for_target(&mut program);

    Ok(program)
}

fn select_function(function: &LirFunction) -> AsmFunction {
    let mut asm_function = AsmFunction::new(
        function.name.clone(),
        AsmFunctionSignature {
            params: function.signature.params.clone(),
            return_type: function.signature.return_type.clone(),
            is_variadic: function.signature.is_variadic,
        },
    );
    asm_function.locals = function
        .locals
        .iter()
        .map(|local| fp_core::asmir::AsmLocal {
            id: local.id,
            ty: local.ty.clone(),
            name: local.name.clone(),
            is_argument: local.is_argument,
        })
        .collect();
    asm_function.stack_slots = function.stack_slots.clone();
    asm_function.linkage = function.linkage.clone();
    asm_function.visibility = Visibility::Default;
    asm_function.calling_convention = Some(function.calling_convention.clone());
    asm_function.section = Some(".text".to_string());
    asm_function.is_declaration = function.is_declaration;

    // Pass 1: allocate a canonical AsmVirtualRegId (independent of any
    // AsmInstrId) for every LIR result, before building any operands, so
    // forward references (e.g. a Phi referring to a register defined later
    // in the same function) resolve correctly regardless of block order.
    let mut reg_map: HashMap<RegisterId, AsmVirtualRegId> = HashMap::new();
    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            if let Some(result) = &instruction.result {
                let vreg = asm_function.alloc_virtual_register(
                    result.ty.clone(),
                    register_bank(&result.ty),
                    type_size_bits(&result.ty),
                );
                reg_map.insert(result.id, vreg);
            }
        }
    }

    // Pass 2: build opcode+operands for every instruction/terminator now
    // that every register a use could reference is already declared.
    for block in &function.basic_blocks {
        let instructions = block
            .instructions
            .iter()
            .map(|instruction| select_instruction(instruction, &reg_map))
            .collect();
        asm_function.basic_blocks.push(AsmBlock {
            id: block.id,
            label: block.label.clone(),
            instructions,
            terminator: select_terminator(&block.terminator, &reg_map),
            terminator_encoding: None,
            predecessors: block.predecessors.clone(),
            successors: block.successors.clone(),
        });
    }

    asm_function
}

/// The `Write` operand for this instruction's result, if it defines one.
fn result_operand(
    instruction: &LirInstruction,
    reg_map: &HashMap<RegisterId, AsmVirtualRegId>,
) -> Option<AsmOperand> {
    let result = instruction.result.as_ref()?;
    let vreg = *reg_map
        .get(&result.id)
        .expect("every LIR result must be allocated in pass 1 before selection");
    Some(AsmOperand::Register {
        reg: AsmRegister::Virtual(vreg),
        access: OperandAccess::Write,
    })
}

fn lir_value_operand(
    value: &LirValue,
    access: OperandAccess,
    reg_map: &HashMap<RegisterId, AsmVirtualRegId>,
) -> AsmOperand {
    match &value.kind {
        LirValueKind::Register(id) => {
            let vreg = *reg_map
                .get(id)
                .expect("LIR register used before its defining instruction was selected");
            AsmOperand::Register {
                reg: AsmRegister::Virtual(vreg),
                access,
            }
        }
        LirValueKind::Constant(constant) => {
            AsmOperand::Constant(map_constant_kind(constant, &value.ty))
        }
        LirValueKind::Global(name) => AsmOperand::Symbol(name.clone()),
        LirValueKind::Function(function_ref) => {
            AsmOperand::Symbol(Name::new(function_name(function_ref)))
        }
        LirValueKind::Local(id) => AsmOperand::Local(*id),
        LirValueKind::StackSlot(id) => AsmOperand::StackSlot(*id),
    }
}

fn unary_instruction(
    id: fp_core::asmir::AsmInstrId,
    opcode: AsmGenericOpcode,
    dest: Option<AsmOperand>,
    src: AsmOperand,
) -> AsmInstruction {
    let mut operands = Vec::with_capacity(2);
    operands.extend(dest);
    operands.push(src);
    AsmInstruction::new(id, AsmOpcode::Generic(opcode), operands)
}

fn select_instruction(
    instruction: &LirInstruction,
    reg_map: &HashMap<RegisterId, AsmVirtualRegId>,
) -> AsmInstruction {
    let read = |value: &LirValue| lir_value_operand(value, OperandAccess::Read, reg_map);
    let dest = result_operand(instruction, reg_map);
    let id = instruction.id;

    macro_rules! binop {
        ($opcode:ident, $lhs:expr, $rhs:expr) => {{
            let mut operands = Vec::with_capacity(3);
            operands.extend(dest);
            operands.push(read($lhs));
            operands.push(read($rhs));
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::$opcode), operands)
        }};
    }

    match &instruction.kind {
        LirInstructionKind::Add(a, b) => binop!(Add, a, b),
        LirInstructionKind::Sub(a, b) => binop!(Sub, a, b),
        LirInstructionKind::Mul(a, b) => binop!(Mul, a, b),
        LirInstructionKind::Div(a, b) => binop!(Div, a, b),
        LirInstructionKind::Rem(a, b) => binop!(Rem, a, b),
        LirInstructionKind::And(a, b) => binop!(And, a, b),
        LirInstructionKind::Or(a, b) => binop!(Or, a, b),
        LirInstructionKind::Xor(a, b) => binop!(Xor, a, b),
        LirInstructionKind::Shl(a, b) => binop!(Shl, a, b),
        LirInstructionKind::Shr(a, b) => binop!(Shr, a, b),
        LirInstructionKind::Eq(a, b) => binop!(Eq, a, b),
        LirInstructionKind::Ne(a, b) => binop!(Ne, a, b),
        LirInstructionKind::Lt(a, b) => binop!(Lt, a, b),
        LirInstructionKind::Le(a, b) => binop!(Le, a, b),
        LirInstructionKind::Gt(a, b) => binop!(Gt, a, b),
        LirInstructionKind::Ge(a, b) => binop!(Ge, a, b),
        LirInstructionKind::Not(v) => unary_instruction(id, AsmGenericOpcode::Not, dest, read(v)),
        LirInstructionKind::PtrToInt(v) => {
            unary_instruction(id, AsmGenericOpcode::PtrToInt, dest, read(v))
        }
        LirInstructionKind::IntToPtr(v) => {
            unary_instruction(id, AsmGenericOpcode::IntToPtr, dest, read(v))
        }
        LirInstructionKind::Freeze(v) => {
            unary_instruction(id, AsmGenericOpcode::Freeze, dest, read(v))
        }
        // Cast target types are not carried as an operand: they are the
        // destination register's type, already recorded in the function's
        // virtual-register table (see `select_function` pass 1).
        LirInstructionKind::Bitcast(v, _) => {
            unary_instruction(id, AsmGenericOpcode::Bitcast, dest, read(v))
        }
        LirInstructionKind::Trunc(v, _) => {
            unary_instruction(id, AsmGenericOpcode::Trunc, dest, read(v))
        }
        LirInstructionKind::ZExt(v, _) => {
            unary_instruction(id, AsmGenericOpcode::ZExt, dest, read(v))
        }
        LirInstructionKind::SExt(v, _) => {
            unary_instruction(id, AsmGenericOpcode::SExt, dest, read(v))
        }
        LirInstructionKind::FPExt(v, _) => {
            unary_instruction(id, AsmGenericOpcode::FPExt, dest, read(v))
        }
        LirInstructionKind::FPTrunc(v, _) => {
            unary_instruction(id, AsmGenericOpcode::FPTrunc, dest, read(v))
        }
        LirInstructionKind::FPToUI(v, _) => {
            unary_instruction(id, AsmGenericOpcode::FPToUI, dest, read(v))
        }
        LirInstructionKind::FPToSI(v, _) => {
            unary_instruction(id, AsmGenericOpcode::FPToSI, dest, read(v))
        }
        LirInstructionKind::UIToFP(v, _) => {
            unary_instruction(id, AsmGenericOpcode::UIToFP, dest, read(v))
        }
        LirInstructionKind::SIToFP(v, _) => {
            unary_instruction(id, AsmGenericOpcode::SIToFP, dest, read(v))
        }
        LirInstructionKind::SextOrTrunc(v, _) => {
            unary_instruction(id, AsmGenericOpcode::SextOrTrunc, dest, read(v))
        }
        LirInstructionKind::Load {
            address,
            alignment,
            volatile,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(address));
            if let Some(align) = alignment {
                operands.push(AsmOperand::Attr(AsmAttr::Alignment(*align)));
            }
            if *volatile {
                operands.push(AsmOperand::Attr(AsmAttr::Volatile));
            }
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Load), operands)
        }
        LirInstructionKind::Store {
            value,
            address,
            alignment,
            volatile,
        } => {
            let mut operands = vec![read(value), read(address)];
            if let Some(align) = alignment {
                operands.push(AsmOperand::Attr(AsmAttr::Alignment(*align)));
            }
            if *volatile {
                operands.push(AsmOperand::Attr(AsmAttr::Volatile));
            }
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Store), operands)
        }
        LirInstructionKind::Alloca { size, alignment } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(size));
            operands.push(AsmOperand::Attr(AsmAttr::Alignment(*alignment)));
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Alloca), operands)
        }
        LirInstructionKind::GetElementPtr {
            ptr,
            indices,
            inbounds,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(ptr));
            if *inbounds {
                operands.push(AsmOperand::Attr(AsmAttr::Inbounds));
            }
            operands.extend(indices.iter().map(&read));
            AsmInstruction::new(
                id,
                AsmOpcode::Generic(AsmGenericOpcode::GetElementPtr),
                operands,
            )
        }
        LirInstructionKind::ExtractValue { aggregate, indices } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(aggregate));
            operands.extend(indices.iter().map(|index| AsmOperand::Immediate(*index as i128)));
            AsmInstruction::new(
                id,
                AsmOpcode::Generic(AsmGenericOpcode::ExtractValue),
                operands,
            )
        }
        LirInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(aggregate));
            operands.push(read(element));
            operands.extend(indices.iter().map(|index| AsmOperand::Immediate(*index as i128)));
            AsmInstruction::new(
                id,
                AsmOpcode::Generic(AsmGenericOpcode::InsertValue),
                operands,
            )
        }
        LirInstructionKind::Call {
            function,
            args,
            calling_convention,
            tail_call,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(AsmOperand::Attr(AsmAttr::CallingConv(
                calling_convention.clone(),
            )));
            if *tail_call {
                operands.push(AsmOperand::Attr(AsmAttr::TailCall));
            }
            operands.push(read(function));
            operands.extend(args.iter().map(&read));
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Call), operands)
        }
        LirInstructionKind::ExecQuery(_) => {
            panic!("LIR ExecQuery is only supported by pxc whole-file lowering")
        }
        LirInstructionKind::IntrinsicCall { kind, format, args } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(AsmOperand::Attr(AsmAttr::Format(format.clone())));
            operands.push(AsmOperand::Attr(AsmAttr::Intrinsic(map_intrinsic(kind))));
            operands.extend(args.iter().map(&read));
            AsmInstruction::new(
                id,
                AsmOpcode::Generic(AsmGenericOpcode::IntrinsicCall),
                operands,
            )
        }
        LirInstructionKind::Phi { incoming } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            for (value, block) in incoming {
                operands.push(read(value));
                operands.push(AsmOperand::Block(*block));
            }
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Phi), operands)
        }
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(read(condition));
            operands.push(read(if_true));
            operands.push(read(if_false));
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Select), operands)
        }
        LirInstructionKind::InlineAsm {
            asm_string,
            constraints,
            inputs,
            output_type: _,
            side_effects,
            align_stack,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            operands.push(AsmOperand::Attr(AsmAttr::AsmText(asm_string.clone())));
            operands.push(AsmOperand::Attr(AsmAttr::Constraints(constraints.clone())));
            if *side_effects {
                operands.push(AsmOperand::Attr(AsmAttr::SideEffects));
            }
            if *align_stack {
                operands.push(AsmOperand::Attr(AsmAttr::AlignStack));
            }
            operands.extend(inputs.iter().map(&read));
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::InlineAsm), operands)
        }
        LirInstructionKind::LandingPad {
            result_type: _,
            personality,
            cleanup,
            clauses,
        } => {
            let mut operands = Vec::new();
            operands.extend(dest);
            if *cleanup {
                operands.push(AsmOperand::Attr(AsmAttr::Cleanup));
            }
            if let Some(personality) = personality {
                operands.push(read(personality));
            }
            for clause in clauses {
                match clause {
                    LandingPadClause::Catch(value) => {
                        operands.push(AsmOperand::Attr(AsmAttr::LandingPadCatch));
                        operands.push(read(value));
                    }
                    LandingPadClause::Filter(values) => {
                        operands.push(AsmOperand::Attr(AsmAttr::LandingPadFilter(
                            values.len() as u32
                        )));
                        operands.extend(values.iter().map(&read));
                    }
                }
            }
            AsmInstruction::new(
                id,
                AsmOpcode::Generic(AsmGenericOpcode::LandingPad),
                operands,
            )
        }
        LirInstructionKind::Unreachable => {
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Unreachable), Vec::new())
        }
        LirInstructionKind::ComptimeOp(_) => {
            AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
        }
    }
}

fn select_terminator(
    term: &LirTerminator,
    reg_map: &HashMap<RegisterId, AsmVirtualRegId>,
) -> AsmTerminator {
    let read = |value: &LirValue| lir_value_operand(value, OperandAccess::Read, reg_map);
    match term {
        LirTerminator::Return(value) => AsmTerminator::Return(value.as_ref().map(&read)),
        LirTerminator::Br(target) => AsmTerminator::Br(*target),
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => AsmTerminator::CondBr {
            condition: read(condition),
            if_true: *if_true,
            if_false: *if_false,
        },
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => AsmTerminator::Switch {
            value: read(value),
            default: *default,
            cases: cases.clone(),
        },
        LirTerminator::IndirectBr {
            address,
            destinations,
        } => AsmTerminator::IndirectBr {
            address: read(address),
            destinations: destinations.clone(),
        },
        LirTerminator::Invoke {
            function,
            args,
            normal_dest,
            unwind_dest,
            calling_convention,
        } => AsmTerminator::Invoke {
            function: read(function),
            args: args.iter().map(&read).collect(),
            normal_dest: *normal_dest,
            unwind_dest: *unwind_dest,
            calling_convention: calling_convention.clone(),
        },
        LirTerminator::Resume(value) => AsmTerminator::Resume(read(value)),
        LirTerminator::Unreachable => AsmTerminator::Unreachable,
        LirTerminator::CleanupRet {
            cleanup_pad,
            unwind_dest,
        } => AsmTerminator::CleanupRet {
            cleanup_pad: read(cleanup_pad),
            unwind_dest: *unwind_dest,
        },
        LirTerminator::CatchRet {
            catch_pad,
            successor,
        } => AsmTerminator::CatchRet {
            catch_pad: read(catch_pad),
            successor: *successor,
        },
        LirTerminator::CatchSwitch {
            parent_pad,
            handlers,
            unwind_dest,
        } => AsmTerminator::CatchSwitch {
            parent_pad: parent_pad.as_ref().map(&read),
            handlers: handlers.clone(),
            unwind_dest: *unwind_dest,
        },
    }
}

fn register_bank_id(bank: AsmRegisterBank) -> u8 {
    match bank {
        AsmRegisterBank::General => 0,
        AsmRegisterBank::Float => 1,
        AsmRegisterBank::Vector => 2,
        AsmRegisterBank::Predicate => 3,
        AsmRegisterBank::Special => 4,
        AsmRegisterBank::Custom(_) => 5,
    }
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
                let mut ctx = PhysicalRegisterLoweringContext::new(
                    next_virtual_id,
                    build_operand_type_map(function),
                );

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
                                .map(|instruction| {
                                    x86_detail_from_instruction(instruction, &mut ctx)
                                })
                                .collect(),
                            terminator: x86_terminator_detail(
                                &block.terminator,
                                &block.instructions,
                            ),
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
                let mut ctx = PhysicalRegisterLoweringContext::new(
                    next_virtual_id,
                    build_operand_type_map(function),
                );

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
                                .map(|instruction| {
                                    aarch64_detail_from_instruction(instruction, &mut ctx)
                                })
                                .collect(),
                            terminator: aarch64_terminator_detail(
                                &block.terminator,
                                &block.instructions,
                            ),
                        })
                        .collect(),
                }
            })
            .collect(),
    }
}

fn canonicalize_physical_registers(program: &mut AsmProgram) {
    let mut next_virtual_id = max_virtual_register_id(program)
        .unwrap_or(0)
        .saturating_add(1);
    let mut map: std::collections::HashMap<(String, u16, u8), u32> =
        std::collections::HashMap::new();

    for function in &mut program.functions {
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                canonicalize_instruction_registers(instruction, &mut map, &mut next_virtual_id);
            }
            canonicalize_terminator_registers(
                &mut block.terminator,
                &mut map,
                &mut next_virtual_id,
            );
        }
    }
}

fn max_virtual_register_id(program: &AsmProgram) -> Option<u32> {
    let mut max_id: Option<u32> = None;
    for function in &program.functions {
        for block in &function.basic_blocks {
            for instruction in &block.instructions {
                for operand in &instruction.operands {
                    let AsmOperand::Register {
                        reg: AsmRegister::Virtual { id, .. },
                        ..
                    } = operand
                    else {
                        continue;
                    };
                    max_id = Some(max_id.map_or(*id, |current| current.max(*id)));
                }
            }
        }
    }
    max_id
}

fn canonicalize_instruction_registers(
    instruction: &mut AsmInstruction,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    canonicalize_instruction_kind_registers(&mut instruction.kind, map, next_virtual_id);
    instruction
        .operands
        .iter_mut()
        .for_each(|operand| canonicalize_operand_registers(operand, map, next_virtual_id));
    instruction
        .implicit_uses
        .iter_mut()
        .for_each(|reg| canonicalize_register(reg, map, next_virtual_id));
    instruction
        .implicit_defs
        .iter_mut()
        .for_each(|reg| canonicalize_register(reg, map, next_virtual_id));
}

fn canonicalize_instruction_kind_registers(
    kind: &mut AsmInstructionKind,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    match kind {
        AsmInstructionKind::Nop => {}
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
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => {
            canonicalize_value(lhs, map, next_virtual_id);
            canonicalize_value(rhs, map, next_virtual_id);
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            canonicalize_value(lhs, map, next_virtual_id);
            canonicalize_value(rhs, map, next_virtual_id);
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
        | AsmInstructionKind::SextOrTrunc(value, _) => {
            canonicalize_value(value, map, next_virtual_id);
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => {
            canonicalize_value(value, map, next_virtual_id);
        }
        AsmInstructionKind::Load { address, .. } => {
            canonicalize_value(address, map, next_virtual_id);
        }
        AsmInstructionKind::Store { value, address, .. } => {
            canonicalize_value(value, map, next_virtual_id);
            canonicalize_value(address, map, next_virtual_id);
        }
        AsmInstructionKind::Alloca { size, .. } => {
            canonicalize_value(size, map, next_virtual_id);
        }
        AsmInstructionKind::SymbolAddress { .. } => {}
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            canonicalize_value(ptr, map, next_virtual_id);
            for index in indices {
                canonicalize_value(index, map, next_virtual_id);
            }
        }
        AsmInstructionKind::ExtractValue { aggregate, .. } => {
            canonicalize_value(aggregate, map, next_virtual_id);
        }
        AsmInstructionKind::InsertValue {
            aggregate, element, ..
        } => {
            canonicalize_value(aggregate, map, next_virtual_id);
            canonicalize_value(element, map, next_virtual_id);
        }
        AsmInstructionKind::Call { function, args, .. } => {
            canonicalize_value(function, map, next_virtual_id);
            for arg in args {
                canonicalize_value(arg, map, next_virtual_id);
            }
        }
        AsmInstructionKind::IntrinsicCall { args, .. } => {
            for arg in args {
                canonicalize_value(arg, map, next_virtual_id);
            }
        }
        AsmInstructionKind::Syscall { number, args, .. } => {
            canonicalize_value(number, map, next_virtual_id);
            for arg in args {
                canonicalize_value(arg, map, next_virtual_id);
            }
        }
        AsmInstructionKind::SysOp(op) => match op {
            fp_core::asmir::AsmSysOp::Exit { code } => {
                canonicalize_value(code, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::GetPid | fp_core::asmir::AsmSysOp::GetTid => {}
            fp_core::asmir::AsmSysOp::Dlopen { path, flags } => {
                canonicalize_value(path, map, next_virtual_id);
                canonicalize_value(flags, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Dlsym { handle, symbol } => {
                canonicalize_value(handle, map, next_virtual_id);
                canonicalize_value(symbol, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Dlclose { handle } => {
                canonicalize_value(handle, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Unlink { path }
            | fp_core::asmir::AsmSysOp::Rmdir { path } => {
                canonicalize_value(path, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Mkdir { path, mode } => {
                canonicalize_value(path, map, next_virtual_id);
                canonicalize_value(mode, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Rename { from, to } => {
                canonicalize_value(from, map, next_virtual_id);
                canonicalize_value(to, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Access { path, mode } => {
                canonicalize_value(path, map, next_virtual_id);
                canonicalize_value(mode, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Write { fd, buffer, len }
            | fp_core::asmir::AsmSysOp::Read { fd, buffer, len } => {
                canonicalize_value(fd, map, next_virtual_id);
                canonicalize_value(buffer, map, next_virtual_id);
                canonicalize_value(len, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Close { fd } => {
                canonicalize_value(fd, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Open {
                path, flags, mode, ..
            } => {
                canonicalize_value(path, map, next_virtual_id);
                canonicalize_value(flags, map, next_virtual_id);
                canonicalize_value(mode, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Seek { fd, offset, whence } => {
                canonicalize_value(fd, map, next_virtual_id);
                canonicalize_value(offset, map, next_virtual_id);
                canonicalize_value(whence, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Mmap {
                addr,
                len,
                prot,
                flags,
                fd,
                offset,
            } => {
                canonicalize_value(addr, map, next_virtual_id);
                canonicalize_value(len, map, next_virtual_id);
                canonicalize_value(prot, map, next_virtual_id);
                canonicalize_value(flags, map, next_virtual_id);
                canonicalize_value(fd, map, next_virtual_id);
                canonicalize_value(offset, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Munmap { addr, len } => {
                canonicalize_value(addr, map, next_virtual_id);
                canonicalize_value(len, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Opendir { path } => {
                canonicalize_value(path, map, next_virtual_id);
            }
            fp_core::asmir::AsmSysOp::Readdir { dir, .. }
            | fp_core::asmir::AsmSysOp::Closedir { dir } => {
                canonicalize_value(dir, map, next_virtual_id);
            }
        },
        AsmInstructionKind::Splat { value, .. } => {
            canonicalize_value(value, map, next_virtual_id);
        }
        AsmInstructionKind::BuildVector { elements } => {
            for element in elements {
                canonicalize_value(element, map, next_virtual_id);
            }
        }
        AsmInstructionKind::ExtractLane { vector, .. } => {
            canonicalize_value(vector, map, next_virtual_id);
        }
        AsmInstructionKind::InsertLane { vector, value, .. } => {
            canonicalize_value(vector, map, next_virtual_id);
            canonicalize_value(value, map, next_virtual_id);
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, _) in incoming {
                canonicalize_value(value, map, next_virtual_id);
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            canonicalize_value(condition, map, next_virtual_id);
            canonicalize_value(if_true, map, next_virtual_id);
            canonicalize_value(if_false, map, next_virtual_id);
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            for input in inputs {
                canonicalize_value(input, map, next_virtual_id);
            }
        }
        AsmInstructionKind::LandingPad {
            personality,
            clauses,
            ..
        } => {
            if let Some(personality) = personality {
                canonicalize_value(personality, map, next_virtual_id);
            }
            for clause in clauses {
                match clause {
                    fp_core::asmir::AsmLandingPadClause::Catch(value) => {
                        canonicalize_value(value, map, next_virtual_id);
                    }
                    fp_core::asmir::AsmLandingPadClause::Filter(values) => {
                        for value in values {
                            canonicalize_value(value, map, next_virtual_id);
                        }
                    }
                }
            }
        }
        AsmInstructionKind::Unreachable => {}
    }
}

fn canonicalize_terminator_registers(
    terminator: &mut AsmTerminator,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    match terminator {
        AsmTerminator::Return(Some(value)) => canonicalize_value(value, map, next_virtual_id),
        AsmTerminator::CondBr { condition, .. } => {
            canonicalize_value(condition, map, next_virtual_id)
        }
        AsmTerminator::Switch { value, .. } => canonicalize_value(value, map, next_virtual_id),
        AsmTerminator::IndirectBr { address, .. } => {
            canonicalize_value(address, map, next_virtual_id)
        }
        AsmTerminator::Invoke { function, args, .. } => {
            canonicalize_value(function, map, next_virtual_id);
            for arg in args {
                canonicalize_value(arg, map, next_virtual_id);
            }
        }
        AsmTerminator::Resume(value) => canonicalize_value(value, map, next_virtual_id),
        AsmTerminator::CleanupRet { cleanup_pad, .. } => {
            canonicalize_value(cleanup_pad, map, next_virtual_id);
        }
        AsmTerminator::CatchRet { catch_pad, .. } => {
            canonicalize_value(catch_pad, map, next_virtual_id)
        }
        AsmTerminator::CatchSwitch { parent_pad, .. } => {
            if let Some(parent) = parent_pad {
                canonicalize_value(parent, map, next_virtual_id);
            }
        }
        AsmTerminator::Return(None) | AsmTerminator::Br(_) | AsmTerminator::Unreachable => {}
    }
}

fn canonicalize_operand_registers(
    operand: &mut AsmOperand,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    match operand {
        AsmOperand::Register { reg, .. } => canonicalize_register(reg, map, next_virtual_id),
        AsmOperand::Memory(memory) => {
            if let Some(base) = &mut memory.base {
                canonicalize_register(base, map, next_virtual_id);
            }
            if let Some(index) = &mut memory.index {
                canonicalize_register(index, map, next_virtual_id);
            }
            if let Some(segment) = &mut memory.segment {
                canonicalize_register(segment, map, next_virtual_id);
            }
        }
        _ => {}
    }
}

fn canonicalize_register(
    reg: &mut AsmRegister,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    let AsmRegister::Physical(physical) = reg else {
        return;
    };
    let key = (
        physical.name.to_ascii_lowercase(),
        physical.size_bits,
        register_bank_id(physical.bank.clone()),
    );
    let id = *map.entry(key).or_insert_with(|| {
        let id = *next_virtual_id;
        *next_virtual_id = next_virtual_id.saturating_add(1);
        id
    });
    *reg = AsmRegister::Virtual {
        id,
        bank: physical.bank.clone(),
        size_bits: physical.size_bits,
    };
}

fn canonicalize_value(
    value: &mut AsmValue,
    map: &mut std::collections::HashMap<(String, u16, u8), u32>,
    next_virtual_id: &mut u32,
) {
    match value {
        AsmValue::PhysicalRegister(register) => {
            let key = (
                register.name.to_ascii_lowercase(),
                register.size_bits,
                register_bank_id(register.bank.clone()),
            );
            let id = *map.entry(key).or_insert_with(|| {
                let id = *next_virtual_id;
                *next_virtual_id = next_virtual_id.saturating_add(1);
                id
            });
            *value = AsmValue::Register(id);
        }
        AsmValue::Address(address) => {
            if let Some(base) = &mut address.base {
                canonicalize_value(base, map, next_virtual_id);
            }
            if let Some(index) = &mut address.index {
                canonicalize_value(index, map, next_virtual_id);
            }
            if let Some(segment) = &mut address.segment {
                canonicalize_value(segment, map, next_virtual_id);
            }
        }
        AsmValue::Comparison(comparison) => {
            canonicalize_value(&mut comparison.lhs, map, next_virtual_id);
            canonicalize_value(&mut comparison.rhs, map, next_virtual_id);
        }
        _ => {}
    }
}

pub fn lift_from_x86_64(program: &x86_64_asm::AsmX86_64Program) -> Result<AsmProgram> {
    let mut next_instruction_id = 0u32;
    let target = AsmTarget {
        architecture: AsmArchitecture::X86_64,
        object_format: AsmObjectFormat::Raw,
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    };
    let mut lifted = AsmProgram {
        target: target.clone(),
        data_layout: target.data_layout(),
        lifted_from: Some(target.clone()),
        container: None,
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
            .map(|function| -> Result<AsmFunction> {
                Ok(AsmFunction {
                    name: function.name.clone(),
                    signature: AsmFunctionSignature {
                        params: Vec::new(),
                        return_type: AsmType::Void,
                        is_variadic: false,
                    },
                    basic_blocks: function
                        .blocks
                        .iter()
                        .map(|block| -> Result<AsmBlock> {
                            let instructions = block
                                .instructions
                                .iter()
                                .map(|instruction| -> Result<AsmInstruction> {
                                    let lifted =
                                        lift_x86_instruction(instruction, next_instruction_id)?;
                                    next_instruction_id += 1;
                                    Ok(lifted)
                                })
                                .collect::<Result<Vec<_>>>()?;
                            let terminator = relink_comparison_condition(
                                instructions.as_slice(),
                                lift_x86_terminator(&block.terminator)?,
                            );
                            Ok(AsmBlock {
                                id: block.id,
                                label: Some(Name::new(format!("bb{}", block.id))),
                                instructions,
                                terminator,
                                terminator_encoding: None,
                                predecessors: Vec::new(),
                                successors: block.terminator.targets.clone(),
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                    locals: Vec::new(),
                    stack_slots: Vec::new(),
                    frame: None,
                    linkage: fp_core::lir::Linkage::External,
                    visibility: Visibility::Default,
                    calling_convention: None,
                    section: Some(".text".to_string()),
                    is_declaration: false,
                })
            })
            .collect::<Result<Vec<_>>>()?,
    };
    if let Some(abi) = crate::abi::default_abi_for_target(
        &lifted.target.architecture,
        &lifted.target.object_format,
    ) {
        for function in &mut lifted.functions {
            crate::abi::raise_implicit_call_arguments(function, abi);
            crate::abi::raise_implicit_return_value(function, abi);
        }
    }
    canonicalize_physical_registers(&mut lifted);
    Ok(lifted)
}

pub fn lift_from_aarch64(program: &aarch64_asm::AsmAarch64Program) -> Result<AsmProgram> {
    let mut next_instruction_id = 0u32;
    let target = AsmTarget {
        architecture: AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::Raw,
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    };
    let mut lifted = AsmProgram {
        target: target.clone(),
        data_layout: target.data_layout(),
        lifted_from: Some(target.clone()),
        container: None,
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
            .map(|function| -> Result<AsmFunction> {
                Ok(AsmFunction {
                    name: function.name.clone(),
                    signature: AsmFunctionSignature {
                        params: Vec::new(),
                        return_type: AsmType::Void,
                        is_variadic: false,
                    },
                    basic_blocks: function
                        .blocks
                        .iter()
                        .map(|block| -> Result<AsmBlock> {
                            let instructions = block
                                .instructions
                                .iter()
                                .map(|instruction| -> Result<AsmInstruction> {
                                    let lifted =
                                        lift_aarch64_instruction(instruction, next_instruction_id)?;
                                    next_instruction_id += 1;
                                    Ok(lifted)
                                })
                                .collect::<Result<Vec<_>>>()?;
                            let terminator = relink_comparison_condition(
                                instructions.as_slice(),
                                lift_aarch64_terminator(&block.terminator)?,
                            );
                            Ok(AsmBlock {
                                id: block.id,
                                label: Some(Name::new(format!("bb{}", block.id))),
                                instructions,
                                terminator,
                                terminator_encoding: None,
                                predecessors: Vec::new(),
                                successors: block.terminator.targets.clone(),
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                    locals: Vec::new(),
                    stack_slots: Vec::new(),
                    frame: None,
                    linkage: fp_core::lir::Linkage::External,
                    visibility: Visibility::Default,
                    calling_convention: None,
                    section: Some(".text".to_string()),
                    is_declaration: false,
                })
            })
            .collect::<Result<Vec<_>>>()?,
    };
    if let Some(abi) = crate::abi::default_abi_for_target(
        &lifted.target.architecture,
        &lifted.target.object_format,
    ) {
        for function in &mut lifted.functions {
            crate::abi::raise_implicit_call_arguments(function, abi);
            crate::abi::raise_implicit_return_value(function, abi);
        }
    }
    canonicalize_physical_registers(&mut lifted);
    Ok(lifted)
}


fn relink_comparison_condition(
    instructions: &[AsmInstruction],
    terminator: AsmTerminator,
) -> AsmTerminator {
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
    instructions.iter().rev().find_map(|instruction| {
        comparison_code_from_kind(&instruction.kind).map(|code| (instruction.id, code))
    })
}

fn comparison_code_from_kind(kind: &AsmInstructionKind) -> Option<AsmConditionCode> {
    match kind {
        AsmInstructionKind::Eq(..) => Some(AsmConditionCode::Eq),
        AsmInstructionKind::Ne(..) => Some(AsmConditionCode::Ne),
        AsmInstructionKind::Lt(..) => Some(AsmConditionCode::Lt),
        AsmInstructionKind::Le(..) => Some(AsmConditionCode::Le),
        AsmInstructionKind::Gt(..) => Some(AsmConditionCode::Gt),
        AsmInstructionKind::Ge(..) => Some(AsmConditionCode::Ge),
        AsmInstructionKind::Ult(..) => Some(AsmConditionCode::Ult),
        AsmInstructionKind::Ule(..) => Some(AsmConditionCode::Ule),
        AsmInstructionKind::Ugt(..) => Some(AsmConditionCode::Ugt),
        AsmInstructionKind::Uge(..) => Some(AsmConditionCode::Uge),
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
            let mut detail = x86_detail(
                instruction.id,
                &instruction.kind,
                Some(&instruction.ty),
                ctx,
            );
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
                Some(&instruction.ty),
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
                    detail.call_target = detail
                        .operands
                        .first()
                        .map(aarch64_call_target_from_operand);
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
        AsmOperand::Register {
            access: OperandAccess::Write | OperandAccess::ReadWrite,
            ..
        } => Some(asm_operand_to_x86(operand, ctx)),
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
        AsmOperand::Label(name) | AsmOperand::Symbol(name) => {
            Some(X86Operand::Symbol(name.clone()))
        }
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
        AsmRegister::Physical(register) if is_x86_physical_register_name(&register.name) => {
            Some(X86Register::Physical {
                name: register.name.clone(),
                size_bits: register.size_bits,
            })
        }
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
        AsmOperand::Register {
            access: OperandAccess::Write | OperandAccess::ReadWrite,
            ..
        } => Some(asm_operand_to_aarch64(operand, ctx)),
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
        AsmOperand::Memory(memory) => {
            Some(Aarch64Operand::Memory(aarch64_memory_from_asm(memory)?))
        }
        AsmOperand::Label(name) | AsmOperand::Symbol(name) => {
            Some(Aarch64Operand::Symbol(name.clone()))
        }
        AsmOperand::Block(id) => Some(Aarch64Operand::Block(*id)),
        AsmOperand::Relocation(relocation) => {
            Some(Aarch64Operand::Symbol(relocation.symbol.clone()))
        }
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
            "ax" | "bx"
                | "cx"
                | "dx"
                | "si"
                | "di"
                | "sp"
                | "bp"
                | "al"
                | "ah"
                | "bl"
                | "bh"
                | "cl"
                | "ch"
                | "dl"
                | "dh"
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
        "ult" => Some(X86ConditionCode::Below),
        "ule" => Some(X86ConditionCode::BelowEqual),
        "ugt" => Some(X86ConditionCode::Above),
        "uge" => Some(X86ConditionCode::AboveEqual),
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
        "ult" => Some(Aarch64ConditionCode::Lo),
        "ule" => Some(Aarch64ConditionCode::Ls),
        "ugt" => Some(Aarch64ConditionCode::Hi),
        "uge" => Some(Aarch64ConditionCode::Hs),
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
        AsmInstructionKind::Nop => {}
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
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => {
            operands.push(x86_operand(lhs, ctx));
            operands.push(x86_operand(rhs, ctx));
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            operands.push(x86_operand(lhs, ctx));
            operands.push(x86_operand(rhs, ctx));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(x86_operand(value, ctx)),
        AsmInstructionKind::Load { address, .. } => {
            operands.push(x86_address_operand(address, ty, ctx))
        }
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
            operands.extend(
                indices
                    .iter()
                    .map(|index| X86Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(x86_operand(aggregate, ctx));
            operands.push(x86_operand(element, ctx));
            operands.extend(
                indices
                    .iter()
                    .map(|index| X86Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::Call { function, .. } => {
            operands.push(match x86_call_target_from_value(function, ctx) {
                X86CallTarget::Symbol(name) => X86Operand::Symbol(name),
                X86CallTarget::Register(reg) => X86Operand::Register {
                    reg,
                    access: OperandAccess::Read,
                },
            });
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(X86Operand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(args.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(x86_operand(value, ctx));
                operands.push(X86Operand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
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
        AsmInstructionKind::Syscall { .. } | AsmInstructionKind::SysOp(_) => {}
        AsmInstructionKind::Splat { value, .. } => operands.push(x86_operand(value, ctx)),
        AsmInstructionKind::BuildVector { elements } => {
            operands.extend(elements.iter().map(|value| x86_operand(value, ctx)));
        }
        AsmInstructionKind::ExtractLane { vector, lane } => {
            operands.push(x86_operand(vector, ctx));
            operands.push(X86Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::InsertLane {
            vector,
            value,
            lane,
        } => {
            operands.push(x86_operand(vector, ctx));
            operands.push(x86_operand(value, ctx));
            operands.push(X86Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::SymbolAddress { symbol, .. } => {
            operands.push(X86Operand::Symbol(Name::new(symbol.clone())));
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
        AsmInstructionKind::Ult(..) => Some(X86ConditionCode::Below),
        AsmInstructionKind::Ule(..) => Some(X86ConditionCode::BelowEqual),
        AsmInstructionKind::Ugt(..) => Some(X86ConditionCode::Above),
        AsmInstructionKind::Uge(..) => Some(X86ConditionCode::AboveEqual),
        AsmInstructionKind::Select { .. } => Some(X86ConditionCode::NonZero),
        _ => None,
    }
}

fn x86_call_target(
    kind: &AsmInstructionKind,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<X86CallTarget> {
    match kind {
        AsmInstructionKind::Call { function, .. } => {
            Some(x86_call_target_from_value(function, ctx))
        }
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
    register_types: HashMap<u32, AsmType>,
}

impl PhysicalRegisterLoweringContext {
    fn new(next_virtual_id: u32, register_types: HashMap<u32, AsmType>) -> Self {
        Self {
            next_virtual_id,
            virtual_ids: std::collections::HashMap::new(),
            register_types,
        }
    }

    fn register_type(&self, id: u32) -> AsmType {
        self.register_types
            .get(&id)
            .map(backend_operand_type)
            .unwrap_or_else(|| panic!("missing type for virtual register {id}"))
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

fn x86_call_target_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86CallTarget {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => {
            X86CallTarget::Symbol(Name::new(name.clone()))
        }
        AsmValue::Register(id) => X86CallTarget::Register(x86_virtual_register(*id, &AsmType::I64)),
        AsmValue::PhysicalRegister(register) => {
            X86CallTarget::Register(map_physical_register_to_x86(register, ctx))
        }
        _ => X86CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn x86_terminator_detail(
    term: &AsmTerminator,
    instructions: &[AsmInstruction],
) -> X86TerminatorDetail {
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
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Jcc,
            condition: resolve_x86_branch_condition(condition, instructions)
                .or(Some(X86ConditionCode::NonZero)),
            targets: vec![*if_true, *if_false],
        },
        AsmTerminator::Switch { default, cases, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Switch,
            condition: None,
            targets: cases
                .iter()
                .map(|(_, target)| *target)
                .chain(std::iter::once(*default))
                .collect(),
        },
        AsmTerminator::IndirectBr { destinations, .. } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::IndirectJmp,
            condition: None,
            targets: destinations.clone(),
        },
        AsmTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Invoke,
            condition: None,
            targets: vec![*normal_dest, *unwind_dest],
        },
        AsmTerminator::Resume(_) => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Resume,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Unreachable => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::Ud2,
            condition: None,
            targets: Vec::new(),
        },
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
        AsmTerminator::CatchSwitch {
            handlers,
            unwind_dest,
            ..
        } => X86TerminatorDetail {
            opcode: X86TerminatorOpcode::CatchSwitch,
            condition: None,
            targets: handlers
                .iter()
                .copied()
                .chain(unwind_dest.iter().copied())
                .collect(),
        },
    }
}

fn resolve_x86_branch_condition(
    condition: &AsmValue,
    instructions: &[AsmInstruction],
) -> Option<X86ConditionCode> {
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
        AsmInstructionKind::Nop => "nop",
        AsmInstructionKind::Add(..) => "add",
        AsmInstructionKind::Sub(..) => "sub",
        AsmInstructionKind::Mul(..) if is_float_type_opt(ty) => {
            if matches!(ty, Some(AsmType::F32)) {
                "fmul.s"
            } else {
                "fmul.d"
            }
        }
        AsmInstructionKind::Mul(..) => "mul",
        AsmInstructionKind::Div(..) | AsmInstructionKind::Rem(..) if is_float_type_opt(ty) => {
            if matches!(ty, Some(AsmType::F32)) {
                "fdiv.s"
            } else {
                "fdiv.d"
            }
        }
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
        | AsmInstructionKind::Ge(..)
        | AsmInstructionKind::Ult(..)
        | AsmInstructionKind::Ule(..)
        | AsmInstructionKind::Ugt(..)
        | AsmInstructionKind::Uge(..) => "cmp",
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
        AsmInstructionKind::Syscall { .. } => "svc",
        AsmInstructionKind::Splat { .. } => "dup",
        AsmInstructionKind::BuildVector { .. } => "build_vector",
        AsmInstructionKind::ExtractLane { .. } => "extract_lane",
        AsmInstructionKind::InsertLane { .. } => "insert_lane",
        AsmInstructionKind::ZipLow { .. } => "zip1",
        AsmInstructionKind::SymbolAddress { kind, .. } => match kind {
            fp_core::asmir::AsmSymbolAddressKind::Direct => "symaddr.direct",
            fp_core::asmir::AsmSymbolAddressKind::Got => "symaddr.got",
        },
        AsmInstructionKind::SysOp(_) => "sysop",
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
        AsmInstructionKind::Nop => {}
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
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => {
            operands.push(aarch64_operand(lhs, ctx));
            operands.push(aarch64_operand(rhs, ctx));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(aarch64_operand(value, ctx)),
        AsmInstructionKind::Load { address, .. } => {
            operands.push(aarch64_address_operand(address, ty, ctx))
        }
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
            operands.extend(
                indices
                    .iter()
                    .map(|index| Aarch64Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(aarch64_operand(aggregate, ctx));
            operands.push(aarch64_operand(element, ctx));
            operands.extend(
                indices
                    .iter()
                    .map(|index| Aarch64Operand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::Call { function, .. } => {
            operands.push(match aarch64_call_target_from_value(function, ctx) {
                Aarch64CallTarget::Symbol(name) => Aarch64Operand::Symbol(name),
                Aarch64CallTarget::Register(reg) => Aarch64Operand::Register {
                    reg,
                    access: OperandAccess::Read,
                },
            });
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(Aarch64Operand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(args.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(aarch64_operand(value, ctx));
                operands.push(Aarch64Operand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
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
        AsmInstructionKind::Syscall { convention, .. } => {
            let imm = match convention {
                AsmSyscallConvention::LinuxAarch64 => 0,
                AsmSyscallConvention::DarwinAarch64 => 0x80,
                _ => 0,
            };
            operands.push(Aarch64Operand::Immediate(imm));
        }
        AsmInstructionKind::SysOp(_) => {}
        AsmInstructionKind::Splat { value, .. } => {
            operands.push(aarch64_operand(value, ctx));
        }
        AsmInstructionKind::BuildVector { elements } => {
            operands.extend(elements.iter().map(|value| aarch64_operand(value, ctx)));
        }
        AsmInstructionKind::ExtractLane { vector, lane } => {
            operands.push(aarch64_operand(vector, ctx));
            operands.push(Aarch64Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::InsertLane {
            vector,
            value,
            lane,
        } => {
            operands.push(aarch64_operand(vector, ctx));
            operands.push(aarch64_operand(value, ctx));
            operands.push(Aarch64Operand::Immediate((*lane).into()));
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            operands.push(aarch64_operand(lhs, ctx));
            operands.push(aarch64_operand(rhs, ctx));
        }
        AsmInstructionKind::SymbolAddress { symbol, .. } => {
            operands.push(Aarch64Operand::Symbol(Name::new(symbol.clone())));
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
        AsmInstructionKind::Ult(..) => Some(Aarch64ConditionCode::Lo),
        AsmInstructionKind::Ule(..) => Some(Aarch64ConditionCode::Ls),
        AsmInstructionKind::Ugt(..) => Some(Aarch64ConditionCode::Hi),
        AsmInstructionKind::Uge(..) => Some(Aarch64ConditionCode::Hs),
        AsmInstructionKind::Select { .. } => Some(Aarch64ConditionCode::NonZero),
        _ => None,
    }
}

fn aarch64_call_target(
    kind: &AsmInstructionKind,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64CallTarget> {
    match kind {
        AsmInstructionKind::Call { function, .. } => {
            Some(aarch64_call_target_from_value(function, ctx))
        }
        AsmInstructionKind::IntrinsicCall { kind, .. } => Some(Aarch64CallTarget::Symbol(
            Name::new(format!("intrinsic.{kind:?}").to_ascii_lowercase()),
        )),
        _ => None,
    }
}

fn aarch64_call_target_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64CallTarget {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => {
            Aarch64CallTarget::Symbol(Name::new(name.clone()))
        }
        AsmValue::Register(id) => {
            Aarch64CallTarget::Register(aarch64_virtual_register(*id, &AsmType::I64))
        }
        AsmValue::PhysicalRegister(register) => {
            Aarch64CallTarget::Register(map_physical_register_to_aarch64(register, ctx))
        }
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

fn aarch64_terminator_detail(
    term: &AsmTerminator,
    instructions: &[AsmInstruction],
) -> Aarch64TerminatorDetail {
    match term {
        AsmTerminator::Return(_) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Ret,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Br(target) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::B,
            condition: None,
            targets: vec![*target],
        },
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::BCond,
            condition: resolve_aarch64_branch_condition(condition, instructions)
                .or(Some(Aarch64ConditionCode::NonZero)),
            targets: vec![*if_true, *if_false],
        },
        AsmTerminator::Switch { default, cases, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Switch,
            condition: None,
            targets: cases
                .iter()
                .map(|(_, target)| *target)
                .chain(std::iter::once(*default))
                .collect(),
        },
        AsmTerminator::IndirectBr { destinations, .. } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Br,
            condition: None,
            targets: destinations.clone(),
        },
        AsmTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Invoke,
            condition: None,
            targets: vec![*normal_dest, *unwind_dest],
        },
        AsmTerminator::Resume(_) => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Resume,
            condition: None,
            targets: Vec::new(),
        },
        AsmTerminator::Unreachable => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::Brk,
            condition: None,
            targets: Vec::new(),
        },
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
        AsmTerminator::CatchSwitch {
            handlers,
            unwind_dest,
            ..
        } => Aarch64TerminatorDetail {
            opcode: Aarch64TerminatorOpcode::CatchSwitch,
            condition: None,
            targets: handlers
                .iter()
                .copied()
                .chain(unwind_dest.iter().copied())
                .collect(),
        },
    }
}

fn resolve_aarch64_branch_condition(
    condition: &AsmValue,
    instructions: &[AsmInstruction],
) -> Option<Aarch64ConditionCode> {
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
        AsmInstructionKind::Nop => X86Opcode::Nop,
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
        | AsmInstructionKind::Ge(..)
        | AsmInstructionKind::Ult(..)
        | AsmInstructionKind::Ule(..)
        | AsmInstructionKind::Ugt(..)
        | AsmInstructionKind::Uge(..) => X86Opcode::Cmp,
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
        AsmInstructionKind::Call { .. } | AsmInstructionKind::IntrinsicCall { .. } => {
            X86Opcode::Call
        }
        AsmInstructionKind::Phi { .. } => X86Opcode::PhiCopy,
        AsmInstructionKind::Select { .. } => X86Opcode::CMov,
        AsmInstructionKind::InlineAsm { .. } => X86Opcode::InlineAsm,
        AsmInstructionKind::LandingPad { .. } => X86Opcode::LandingPad,
        AsmInstructionKind::Syscall { .. } => X86Opcode::Syscall,
        AsmInstructionKind::SysOp(_) => X86Opcode::InlineAsm,
        AsmInstructionKind::Splat { .. } => X86Opcode::Mov,
        AsmInstructionKind::BuildVector { .. }
        | AsmInstructionKind::ExtractLane { .. }
        | AsmInstructionKind::InsertLane { .. }
        | AsmInstructionKind::ZipLow { .. } => X86Opcode::Mov,
        AsmInstructionKind::SymbolAddress { .. } => X86Opcode::Mov,
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

fn x86_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> Vec<AsmOperand> {
    let mut operands = Vec::new();
    if instruction_produces_value(kind) {
        if let Some(ty) = ty {
            operands.push(register_operand(
                virtual_register(id, &backend_operand_type(ty)),
                OperandAccess::Write,
            ));
        }
    }

    match kind {
        AsmInstructionKind::Nop => {}
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
        | AsmInstructionKind::Ge(lhs, rhs)
        | AsmInstructionKind::Ult(lhs, rhs)
        | AsmInstructionKind::Ule(lhs, rhs)
        | AsmInstructionKind::Ugt(lhs, rhs)
        | AsmInstructionKind::Uge(lhs, rhs) => {
            operands.push(value_operand(lhs, register_types));
            operands.push(value_operand(rhs, register_types));
        }
        AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
            operands.push(value_operand(lhs, register_types));
            operands.push(value_operand(rhs, register_types));
        }
        AsmInstructionKind::Not(value)
        | AsmInstructionKind::PtrToInt(value)
        | AsmInstructionKind::IntToPtr(value)
        | AsmInstructionKind::Freeze(value) => operands.push(value_operand(value, register_types)),
        AsmInstructionKind::Load { address, .. } => {
            operands.push(address_operand(address, ty, register_types))
        }
        AsmInstructionKind::Store { value, address, .. } => {
            operands.push(address_operand(address, None, register_types));
            operands.push(value_operand(value, register_types));
        }
        AsmInstructionKind::Alloca { size, .. } => {
            operands.push(value_operand(size, register_types))
        }
        AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
            operands.push(value_operand(ptr, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
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
        | AsmInstructionKind::SextOrTrunc(value, _) => {
            operands.push(value_operand(value, register_types))
        }
        AsmInstructionKind::ExtractValue { aggregate, indices } => {
            operands.push(value_operand(aggregate, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|index| AsmOperand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            operands.push(value_operand(aggregate, register_types));
            operands.push(value_operand(element, register_types));
            operands.extend(
                indices
                    .iter()
                    .map(|index| AsmOperand::Immediate(*index as i128)),
            );
        }
        AsmInstructionKind::Call { function, .. } => {
            // Call arguments are semantic (ABI-lowered), not textual operands.
            operands.push(call_target_operand(function, register_types));
        }
        AsmInstructionKind::IntrinsicCall { kind, args, .. } => {
            operands.push(AsmOperand::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )));
            operands.extend(
                args.iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::Phi { incoming } => {
            for (value, block) in incoming {
                operands.push(value_operand(value, register_types));
                operands.push(AsmOperand::Block(*block));
            }
        }
        AsmInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            operands.push(value_operand(condition, register_types));
            operands.push(value_operand(if_true, register_types));
            operands.push(value_operand(if_false, register_types));
        }
        AsmInstructionKind::InlineAsm { inputs, .. } => {
            operands.extend(
                inputs
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::LandingPad { personality, .. } => {
            if let Some(personality) = personality {
                operands.push(value_operand(personality, register_types));
            }
        }
        AsmInstructionKind::Syscall { .. } => {}
        AsmInstructionKind::SysOp(_) => {}
        AsmInstructionKind::Splat { value, .. } => {
            operands.push(value_operand(value, register_types))
        }
        AsmInstructionKind::BuildVector { elements } => {
            operands.extend(
                elements
                    .iter()
                    .map(|value| value_operand(value, register_types)),
            );
        }
        AsmInstructionKind::ExtractLane { vector, lane } => {
            operands.push(value_operand(vector, register_types));
            operands.push(AsmOperand::Immediate((*lane).into()));
        }
        AsmInstructionKind::InsertLane {
            vector,
            lane,
            value,
        } => {
            operands.push(value_operand(vector, register_types));
            operands.push(value_operand(value, register_types));
            operands.push(AsmOperand::Immediate((*lane).into()));
        }
        AsmInstructionKind::SymbolAddress { symbol, .. } => {
            operands.push(AsmOperand::Symbol(Name::new(symbol.clone())));
        }
        AsmInstructionKind::Unreachable => {}
    }

    operands
}

fn generic_operands(
    id: u32,
    kind: &AsmInstructionKind,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> Vec<AsmOperand> {
    x86_operands(id, kind, ty, register_types)
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

fn value_operand(value: &AsmValue, register_types: &HashMap<u32, AsmType>) -> AsmOperand {
    match value {
        AsmValue::Register(id) => register_operand(
            virtual_register(
                *id,
                &register_types
                    .get(id)
                    .map(backend_operand_type)
                    .unwrap_or_else(|| panic!("missing type for virtual register {id}")),
            ),
            OperandAccess::Read,
        ),
        AsmValue::PhysicalRegister(register) => {
            register_operand(AsmRegister::Physical(register.clone()), OperandAccess::Read)
        }
        AsmValue::Address(address) => AsmOperand::Memory(memory_from_address_value(address)),
        AsmValue::Condition(condition) => {
            AsmOperand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => AsmOperand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => AsmOperand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("stack.{id}"))),
        AsmValue::Undef(_) => AsmOperand::Immediate(0),
        AsmValue::Null(_) => AsmOperand::Immediate(0),
    }
}

fn x86_operand(value: &AsmValue, ctx: &mut PhysicalRegisterLoweringContext) -> X86Operand {
    match value {
        AsmValue::Register(id) => X86Operand::Register {
            reg: x86_virtual_register(*id, &ctx.register_type(*id)),
            access: OperandAccess::Read,
        },
        AsmValue::PhysicalRegister(register) => X86Operand::Register {
            reg: map_physical_register_to_x86(register, ctx),
            access: OperandAccess::Read,
        },
        AsmValue::Address(address) => x86_address_value_operand(address, ctx),
        AsmValue::Condition(condition) => {
            X86Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => X86Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => X86Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => x86_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            X86Operand::Symbol(Name::new(name.clone()))
        }
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
            base: Some(x86_virtual_register(
                *id,
                &AsmType::Ptr(Box::new(AsmType::I8)),
            )),
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
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            X86Operand::Symbol(Name::new(name.clone()))
        }
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
        AsmConstant::String(value) => {
            X86Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => X86Operand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            X86Operand::Symbol(name.clone())
        }
        AsmConstant::Array(..) => X86Operand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => X86Operand::Symbol(Name::new("const.struct")),
    }
}

fn call_target_operand(value: &AsmValue, register_types: &HashMap<u32, AsmType>) -> AsmOperand {
    match value {
        AsmValue::Function(name) | AsmValue::Global(name, _) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        _ => value_operand(value, register_types),
    }
}

fn address_operand(
    address: &AsmValue,
    ty: Option<&AsmType>,
    register_types: &HashMap<u32, AsmType>,
) -> AsmOperand {
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
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            AsmOperand::Symbol(Name::new(name.clone()))
        }
        AsmValue::Local(id) => AsmOperand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmValue::StackSlot(id) => AsmOperand::Symbol(Name::new(format!("frame.slot.{id}"))),
        _ => value_operand(address, register_types),
    }
}

fn address_value_from_memory(memory: &AsmMemoryOperand) -> AsmAddressValue {
    AsmAddressValue {
        base: memory
            .base
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        index: memory
            .index
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        scale: memory.scale,
        displacement: memory.displacement,
        segment: memory
            .segment
            .as_ref()
            .map(|register| Box::new(register_value_from_asm(register))),
        size_bytes: memory.size_bytes,
        address_space: memory.address_space,
        pre_indexed: memory.pre_indexed,
        post_indexed: memory.post_indexed,
    }
}

fn memory_from_address_value(address: &AsmAddressValue) -> AsmMemoryOperand {
    AsmMemoryOperand {
        base: address.base.as_deref().and_then(address_component_register),
        index: address
            .index
            .as_deref()
            .and_then(address_component_register),
        scale: address.scale,
        displacement: address.displacement,
        segment: address
            .segment
            .as_deref()
            .and_then(address_component_register),
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
        base: address
            .base
            .as_deref()
            .and_then(|value| x86_register_from_value(value, ctx)),
        index: address
            .index
            .as_deref()
            .and_then(|value| x86_register_from_value(value, ctx)),
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
        base: address
            .base
            .as_deref()
            .and_then(|value| aarch64_register_from_value(value, ctx)),
        index: address
            .index
            .as_deref()
            .and_then(|value| aarch64_register_from_value(value, ctx)),
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
        AsmValue::Register(id) => Some(x86_virtual_register(
            *id,
            &AsmType::Ptr(Box::new(AsmType::I8)),
        )),
        AsmValue::PhysicalRegister(register) => Some(map_physical_register_to_x86(register, ctx)),
        _ => None,
    }
}

fn aarch64_register_from_value(
    value: &AsmValue,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64Register> {
    match value {
        AsmValue::Register(id) => Some(aarch64_virtual_register(
            *id,
            &AsmType::Ptr(Box::new(AsmType::I8)),
        )),
        AsmValue::PhysicalRegister(register) => {
            Some(map_physical_register_to_aarch64(register, ctx))
        }
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
        AsmConstant::String(value) => {
            AsmOperand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => AsmOperand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            AsmOperand::Symbol(name.clone())
        }
        AsmConstant::Array(..) => AsmOperand::Symbol(Name::new("const.array")),
        AsmConstant::Struct(..) => AsmOperand::Symbol(Name::new("const.struct")),
    }
}

fn register_operand(reg: AsmRegister, access: OperandAccess) -> AsmOperand {
    AsmOperand::Register { reg, access }
}

/// Native aggregate values are represented by addresses of their storage.
/// ABI expansion is handled later by the target-specific call lowering.
fn backend_operand_type(ty: &AsmType) -> AsmType {
    match ty {
        AsmType::Struct { .. } | AsmType::Array(_, _) => AsmType::Ptr(Box::new(ty.clone())),
        _ => ty.clone(),
    }
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
        AsmConditionCode::Ult => X86ConditionCode::Below,
        AsmConditionCode::Ule => X86ConditionCode::BelowEqual,
        AsmConditionCode::Ugt => X86ConditionCode::Above,
        AsmConditionCode::Uge => X86ConditionCode::AboveEqual,
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
        AsmConditionCode::Ult => Aarch64ConditionCode::Lo,
        AsmConditionCode::Ule => Aarch64ConditionCode::Ls,
        AsmConditionCode::Ugt => Aarch64ConditionCode::Hi,
        AsmConditionCode::Uge => Aarch64ConditionCode::Hs,
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
        X86ConditionCode::Below => AsmConditionCode::Ult,
        X86ConditionCode::BelowEqual => AsmConditionCode::Ule,
        X86ConditionCode::Above => AsmConditionCode::Ugt,
        X86ConditionCode::AboveEqual => AsmConditionCode::Uge,
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
        Aarch64ConditionCode::Lo => AsmConditionCode::Ult,
        Aarch64ConditionCode::Ls => AsmConditionCode::Ule,
        Aarch64ConditionCode::Hi => AsmConditionCode::Ugt,
        Aarch64ConditionCode::Hs => AsmConditionCode::Uge,
        Aarch64ConditionCode::NonZero => AsmConditionCode::Nz,
    }
}

fn lift_x86_instruction(instruction: &X86InstructionDetail, id: u32) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(x86_operand_to_asm)
        .collect::<Vec<_>>();
    let ty = output_type_from_asm_operands(&operands).unwrap_or(AsmType::Void);
    let kind = semanticize_x86_detail(instruction, &operands)?;
    Ok(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        ty,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    })
}

fn lift_aarch64_instruction(
    instruction: &Aarch64InstructionDetail,
    id: u32,
) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(aarch64_operand_to_asm)
        .collect::<Vec<_>>();
    let ty = output_type_from_asm_operands(&operands).unwrap_or(AsmType::Void);
    let kind = semanticize_aarch64_detail(instruction, &operands)?;
    Ok(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(generic_opcode(&kind)),
        kind,
        ty,
        operands,
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    })
}

fn output_type_from_asm_operands(operands: &[AsmOperand]) -> Option<AsmType> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Register {
            reg: AsmRegister::Virtual { size_bits, .. },
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => {
            Some(type_from_bits(*size_bits))
        }
        AsmOperand::Register {
            reg: AsmRegister::Physical(register),
            access,
        } if matches!(access, OperandAccess::Write | OperandAccess::ReadWrite) => {
            Some(type_from_bits(register.size_bits))
        }
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

fn x86_custom_opcode_name(instruction: &X86InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode, X86Opcode::Cmp | X86Opcode::CMov) => {
            format!(
                "{}.{}",
                instruction.opcode.mnemonic(),
                x86_condition_suffix(condition)
            )
        }
        _ => instruction.opcode.mnemonic().to_string(),
    }
}

fn aarch64_custom_opcode_name(instruction: &Aarch64InstructionDetail) -> String {
    match instruction.condition.as_ref() {
        Some(condition) if matches!(instruction.opcode.as_str(), "cmp" | "csel") => {
            format!(
                "{}.{}",
                instruction.opcode,
                aarch64_condition_suffix(condition)
            )
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
        X86ConditionCode::Below => "ult",
        X86ConditionCode::BelowEqual => "ule",
        X86ConditionCode::Above => "ugt",
        X86ConditionCode::AboveEqual => "uge",
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
        Aarch64ConditionCode::Lo => "ult",
        Aarch64ConditionCode::Ls => "ule",
        Aarch64ConditionCode::Hi => "ugt",
        Aarch64ConditionCode::Hs => "uge",
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
        AsmConditionCode::Ult => "ult",
        AsmConditionCode::Ule => "ule",
        AsmConditionCode::Ugt => "ugt",
        AsmConditionCode::Uge => "uge",
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
        X86Register::Physical { name, size_bits } => {
            AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
                name: name.clone(),
                bank: if name.starts_with("xmm") {
                    AsmRegisterBank::Float
                } else {
                    AsmRegisterBank::General
                },
                size_bits: *size_bits,
            })
        }
        X86Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

fn aarch64_register_to_asm(register: &Aarch64Register) -> AsmRegister {
    match register {
        Aarch64Register::Physical { name, size_bits } => {
            AsmRegister::Physical(fp_core::asmir::AsmPhysicalRegister {
                name: name.clone(),
                bank: if matches!(name.chars().next(), Some('s' | 'd' | 'q' | 'v')) {
                    AsmRegisterBank::Float
                } else {
                    AsmRegisterBank::General
                },
                size_bits: *size_bits,
            })
        }
        Aarch64Register::Virtual { id, size_bits } => AsmRegister::Virtual {
            id: *id,
            bank: AsmRegisterBank::General,
            size_bits: *size_bits,
        },
    }
}

fn asm_operand_to_x86(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match operand {
        AsmOperand::Register { reg, access } => X86Operand::Register {
            reg: asm_register_to_x86(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => X86Operand::Immediate(*value),
        AsmOperand::Memory(mem) => X86Operand::Memory(X86MemoryOperand {
            base: mem
                .base
                .as_ref()
                .map(|register| asm_register_to_x86(register, ctx)),
            index: mem
                .index
                .as_ref()
                .map(|register| asm_register_to_x86(register, ctx)),
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

fn asm_operand_to_aarch64(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match operand {
        AsmOperand::Register { reg, access } => Aarch64Operand::Register {
            reg: asm_register_to_aarch64(reg, ctx),
            access: access.clone(),
        },
        AsmOperand::Immediate(value) => Aarch64Operand::Immediate(*value),
        AsmOperand::Memory(mem) => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: mem
                .base
                .as_ref()
                .map(|register| asm_register_to_aarch64(register, ctx)),
            index: mem
                .index
                .as_ref()
                .map(|register| asm_register_to_aarch64(register, ctx)),
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

fn asm_register_to_x86(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Register {
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
    let digits = name
        .chars()
        .skip_while(|ch| !ch.is_ascii_digit())
        .collect::<String>();
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
    let index = physical_register_index(name)
        .unwrap_or_else(|| x86_general_register_index(name).unwrap_or(0));
    x86_general_register_name(index, size_bits)
}

fn map_general_register_name_to_aarch64(name: &str, size_bits: u16) -> String {
    if matches!(name, "rsp" | "esp" | "sp") {
        return "sp".to_string();
    }
    if matches!(name, "rbp" | "ebp" | "bp") {
        return if size_bits <= 32 {
            "w29".to_string()
        } else {
            "x29".to_string()
        };
    }
    let index = x86_general_register_index(name)
        .or_else(|| physical_register_index(name))
        .unwrap_or(0);
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

fn lift_x86_terminator(terminator: &X86TerminatorDetail) -> Result<AsmTerminator> {
    match terminator.opcode {
        X86TerminatorOpcode::Ret => Ok(AsmTerminator::Return(None)),
        X86TerminatorOpcode::Jmp => Ok(AsmTerminator::Br(
            terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("direct branch is missing its target"))?,
        )),
        X86TerminatorOpcode::Jcc => {
            let condition = terminator
                .condition
                .as_ref()
                .ok_or_else(|| Error::from("conditional branch is missing its condition"))?;
            let if_true = terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its true target"))?;
            let if_false = terminator
                .targets
                .get(1)
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its false target"))?;
            Ok(AsmTerminator::CondBr {
                condition: AsmValue::Condition(asm_condition_from_x86(condition)),
                if_true,
                if_false,
            })
        }
        X86TerminatorOpcode::Ud2 => Ok(AsmTerminator::Unreachable),
        X86TerminatorOpcode::Switch
        | X86TerminatorOpcode::IndirectJmp
        | X86TerminatorOpcode::Invoke
        | X86TerminatorOpcode::Resume
        | X86TerminatorOpcode::CleanupRet
        | X86TerminatorOpcode::CatchRet
        | X86TerminatorOpcode::CatchSwitch => Err(Error::from(
            "raw x86 terminator lacks typed operands required by AsmIR",
        )),
    }
}

fn lift_aarch64_terminator(terminator: &Aarch64TerminatorDetail) -> Result<AsmTerminator> {
    match terminator.opcode {
        Aarch64TerminatorOpcode::Ret => Ok(AsmTerminator::Return(None)),
        Aarch64TerminatorOpcode::B => Ok(AsmTerminator::Br(
            terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("direct branch is missing its target"))?,
        )),
        Aarch64TerminatorOpcode::BCond => {
            let condition = terminator
                .condition
                .as_ref()
                .ok_or_else(|| Error::from("conditional branch is missing its condition"))?;
            let if_true = terminator
                .targets
                .first()
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its true target"))?;
            let if_false = terminator
                .targets
                .get(1)
                .copied()
                .ok_or_else(|| Error::from("conditional branch is missing its false target"))?;
            Ok(AsmTerminator::CondBr {
                condition: AsmValue::Condition(asm_condition_from_aarch64(condition)),
                if_true,
                if_false,
            })
        }
        Aarch64TerminatorOpcode::Brk => Ok(AsmTerminator::Unreachable),
        Aarch64TerminatorOpcode::Br
        | Aarch64TerminatorOpcode::Switch
        | Aarch64TerminatorOpcode::Invoke
        | Aarch64TerminatorOpcode::Resume
        | Aarch64TerminatorOpcode::CleanupRet
        | Aarch64TerminatorOpcode::CatchRet
        | Aarch64TerminatorOpcode::CatchSwitch => Err(Error::from(
            "raw AArch64 terminator lacks typed operands required by AsmIR",
        )),
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
        "syscall" => Ok(AsmInstructionKind::Syscall {
            convention: AsmSyscallConvention::LinuxX86_64,
            number: AsmValue::PhysicalRegister(AsmPhysicalRegister {
                name: "rax".to_string(),
                bank: AsmRegisterBank::General,
                size_bits: 64,
            }),
            args: vec![
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rdi".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rsi".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "rdx".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r10".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r8".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: "r9".to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
            ],
        }),
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
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported x86 opcode for transpile: {base}"
        ))),
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
        "svc" => {
            let imm = operands
                .iter()
                .find_map(|operand| match operand {
                    AsmOperand::Immediate(value) => Some(*value),
                    _ => None,
                })
                .unwrap_or(0);
            let convention = match imm {
                0 => AsmSyscallConvention::LinuxAarch64,
                0x80 => AsmSyscallConvention::DarwinAarch64,
                _ => AsmSyscallConvention::LinuxAarch64,
            };
            let number_reg = match convention {
                AsmSyscallConvention::DarwinAarch64 => "x16",
                _ => "x8",
            };
            Ok(AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::PhysicalRegister(AsmPhysicalRegister {
                    name: number_reg.to_string(),
                    bank: AsmRegisterBank::General,
                    size_bits: 64,
                }),
                args: (0..6)
                    .map(|idx| {
                        AsmValue::PhysicalRegister(AsmPhysicalRegister {
                            name: format!("x{idx}"),
                            bank: AsmRegisterBank::General,
                            size_bits: 64,
                        })
                    })
                    .collect(),
            })
        }
        "add" => binary_value_kind(operands, &values, AsmInstructionKind::Add),
        "sub" => binary_value_kind(operands, &values, AsmInstructionKind::Sub),
        "mul" | "fmul.s" | "fmul.d" => {
            binary_value_kind(operands, &values, AsmInstructionKind::Mul)
        }
        "sdiv" | "fdiv.s" | "fdiv.d" => {
            binary_value_kind(operands, &values, AsmInstructionKind::Div)
        }
        "and" => binary_value_kind(operands, &values, AsmInstructionKind::And),
        "orr" => binary_value_kind(operands, &values, AsmInstructionKind::Or),
        "eor" => binary_value_kind(operands, &values, AsmInstructionKind::Xor),
        "lsl" => binary_value_kind(operands, &values, AsmInstructionKind::Shl),
        "asr" => binary_value_kind(operands, &values, AsmInstructionKind::Shr),
        "mvn" => unary_value_kind(operands, &values, AsmInstructionKind::Not),
        "cmp" => compare_value_kind(
            operands,
            &values,
            condition.map(aarch64_condition_to_x86_equivalent),
        ),
        "ldr" => load_kind(operands),
        "str" => store_kind(operands),
        "add.addr" | "add.sp" => address_kind(operands),
        "bl" => call_value_kind(operands, &values),
        "csel" => select_value_kind(operands, &values),
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported aarch64 opcode for transpile: {base}"
        ))),
    }
}

fn collect_machine_values(operands: &[AsmOperand]) -> Result<Vec<AsmValue>> {
    operands.iter().map(machine_operand_to_value).collect()
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
        AsmOperand::Immediate(value) => Ok(AsmValue::Constant(AsmConstant::Int(
            *value as i64,
            AsmType::I64,
        ))),
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => {
            Ok(AsmValue::Function(name.to_string()))
        }
        AsmOperand::Block(id) => Ok(AsmValue::Constant(AsmConstant::UInt(
            *id as u64,
            AsmType::I32,
        ))),
        AsmOperand::Memory(memory) => memory_address_value(memory),
        _ => Err(fp_core::error::Error::from(
            "machine transpile currently supports only register, immediate, symbol, block, and memory operands",
        )),
    }
}

fn memory_address_value(memory: &AsmMemoryOperand) -> Result<AsmValue> {
    Ok(AsmValue::Address(Box::new(address_value_from_memory(
        memory,
    ))))
}

fn binary_value_kind<F>(
    operands: &[AsmOperand],
    values: &[AsmValue],
    build: F,
) -> Result<AsmInstructionKind>
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

fn unary_value_kind<F>(
    operands: &[AsmOperand],
    values: &[AsmValue],
    build: F,
) -> Result<AsmInstructionKind>
where
    F: Fn(AsmValue) -> AsmInstructionKind,
{
    let first_read = first_read_operand_index(operands);
    Ok(build(values.get(first_read).cloned().ok_or_else(|| {
        fp_core::error::Error::from("missing operand")
    })?))
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
        X86ConditionCode::Below => AsmInstructionKind::Ult(lhs, rhs),
        X86ConditionCode::BelowEqual => AsmInstructionKind::Ule(lhs, rhs),
        X86ConditionCode::Above => AsmInstructionKind::Ugt(lhs, rhs),
        X86ConditionCode::AboveEqual => AsmInstructionKind::Uge(lhs, rhs),
        X86ConditionCode::NonZero => {
            AsmInstructionKind::Ne(lhs, AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)))
        }
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
    let condition = values
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing select condition"))?;
    Ok(AsmInstructionKind::Select {
        condition,
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
        Aarch64ConditionCode::Lo => X86ConditionCode::Below,
        Aarch64ConditionCode::Ls => X86ConditionCode::BelowEqual,
        Aarch64ConditionCode::Hi => X86ConditionCode::Above,
        Aarch64ConditionCode::Hs => X86ConditionCode::AboveEqual,
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
        AsmValue::Condition(condition) => {
            Aarch64Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmValue::Comparison(comparison) => Aarch64Operand::Symbol(Name::new(format!(
            "cmp.{}",
            asm_condition_suffix(&comparison.condition)
        ))),
        AsmValue::Flags(id) => Aarch64Operand::Symbol(Name::new(format!("flags.{id}"))),
        AsmValue::Constant(constant) => aarch64_constant_operand(constant),
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            Aarch64Operand::Symbol(Name::new(name.clone()))
        }
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
            base: Some(aarch64_virtual_register(
                *id,
                &AsmType::Ptr(Box::new(AsmType::I8)),
            )),
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
        AsmValue::Global(name, _) | AsmValue::Function(name) => {
            Aarch64Operand::Symbol(Name::new(name.clone()))
        }
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
        AsmConstant::String(value) => {
            Aarch64Operand::Symbol(Name::new(format!("str.{}", sanitize_symbol(value))))
        }
        AsmConstant::Bytes(..) => Aarch64Operand::Symbol(Name::new("const.bytes")),
        AsmConstant::GlobalRef(name, _, _) | AsmConstant::FunctionRef(name, _) => {
            Aarch64Operand::Symbol(name.clone())
        }
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
    if bytes == 0 {
        64
    } else {
        bytes.saturating_mul(8)
    }
}

fn type_size_bytes(ty: &AsmType) -> u16 {
    let size = match ty {
        AsmType::I1 | AsmType::I8 => 1,
        AsmType::I16 => 2,
        AsmType::I32 | AsmType::F32 => 4,
        AsmType::I64 | AsmType::F64 | AsmType::Ptr(_) | AsmType::Function { .. } => 8,
        AsmType::I128 => 16,
        AsmType::Integer(width) => u64::from(width.div_ceil(8)),
        AsmType::Array(element, count) => u64::from(type_size_bytes(element)) * *count,
        AsmType::Vector(element, count) => u64::from(type_size_bytes(element)) * u64::from(*count),
        AsmType::Struct { fields, .. } => fields.iter().map(type_size_bytes).map(u64::from).sum(),
        AsmType::Void | AsmType::Label | AsmType::Token | AsmType::Metadata => 0,
        AsmType::Error => 0,
    };
    size.min(u64::from(u16::MAX)) as u16
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

fn map_constant(constant: &LirConstant) -> AsmConstant {
    map_constant_kind(&constant.kind, &constant.ty)
}

fn function_name(function: &fp_core::lir::LirFunctionRef) -> String {
    match function {
        fp_core::lir::LirFunctionRef::Name(name) => name.to_string(),
        fp_core::lir::LirFunctionRef::Package { name, .. } => name.to_string(),
        fp_core::lir::LirFunctionRef::Definition(def_id) => def_id.to_string(),
    }
}

fn map_constant_kind(kind: &LirConstantKind, ty: &fp_core::lir::LirType) -> AsmConstant {
    match kind {
        LirConstantKind::Data(LirConstantData::Integer(integer)) => match integer {
            LirInteger::I1(value) => AsmConstant::Bool(*value),
            LirInteger::I8(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I16(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I32(value) => AsmConstant::UInt(u64::from(*value), ty.clone()),
            LirInteger::I64(value) => AsmConstant::Int(*value as i64, ty.clone()),
            LirInteger::I128(value) => AsmConstant::UInt(*value as u64, ty.clone()),
            LirInteger::Arbitrary(_) => panic!("arbitrary-width native constant is unsupported"),
        },
        LirConstantKind::Data(LirConstantData::Float(float)) => match float {
            LirFloat::F32(value) => AsmConstant::Float(f32::from_bits(*value) as f64, ty.clone()),
            LirFloat::F64(value) => AsmConstant::Float(f64::from_bits(*value), ty.clone()),
        },
        LirConstantKind::Data(LirConstantData::Bytes(bytes)) => AsmConstant::Bytes(bytes.clone()),
        LirConstantKind::Aggregate(LirConstantAggregate::Array(values)) => {
            AsmConstant::Array(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Struct(values)) => {
            AsmConstant::Struct(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Vector(values)) => {
            AsmConstant::Array(values.iter().map(map_constant).collect(), ty.clone())
        }
        LirConstantKind::GlobalAddress { global } => {
            AsmConstant::GlobalRef(global.clone(), ty.clone(), Vec::new())
        }
        LirConstantKind::FunctionAddress(function) => {
            AsmConstant::FunctionRef(Name::new(function_name(function)), ty.clone())
        }
        LirConstantKind::Null => AsmConstant::Null(ty.clone()),
        LirConstantKind::Undef | LirConstantKind::Poison => AsmConstant::Undef(ty.clone()),
        LirConstantKind::Expr(LirConstantExpr::GetElementPtr { base, indices, .. }) => {
            let (global, mut base_indices) = global_ref_components(base)
                .unwrap_or_else(|| panic!("constant GEP requires a global-address base"));
            for index in indices {
                let value = constant_integer(index)
                    .unwrap_or_else(|| panic!("constant GEP index must be an integer"));
                base_indices.push(value);
            }
            AsmConstant::GlobalRef(global, ty.clone(), base_indices)
        }
    }
}

fn global_ref_components(constant: &LirConstant) -> Option<(Name, Vec<u64>)> {
    match &constant.kind {
        LirConstantKind::GlobalAddress { global } => Some((global.clone(), Vec::new())),
        LirConstantKind::Expr(LirConstantExpr::GetElementPtr { base, indices, .. }) => {
            let (global, mut base_indices) = global_ref_components(base)?;
            for index in indices {
                base_indices.push(constant_integer(index)?);
            }
            Some((global, base_indices))
        }
        _ => None,
    }
}

fn constant_integer(constant: &LirConstant) -> Option<u64> {
    let LirConstantKind::Data(LirConstantData::Integer(integer)) = &constant.kind else {
        return None;
    };
    Some(match integer {
        LirInteger::I1(value) => u64::from(*value),
        LirInteger::I8(value) => u64::from(*value),
        LirInteger::I16(value) => u64::from(*value),
        LirInteger::I32(value) => u64::from(*value),
        LirInteger::I64(value) => *value,
        LirInteger::I128(value) => *value as u64,
        LirInteger::Arbitrary(_) => {
            return None;
        }
    })
}

fn map_global(global: &fp_core::lir::LirGlobal) -> AsmGlobal {
    AsmGlobal {
        name: global.name.clone(),
        ty: global.ty.clone(),
        initializer: global.initializer.as_ref().map(map_constant),
        relocations: global
            .relocations
            .iter()
            .filter_map(|reloc| {
                let symbol = match &reloc.target {
                    fp_core::lir::LirRelocationTarget::Global(name)
                    | fp_core::lir::LirRelocationTarget::Function(name) => name.clone(),
                };
                Some(fp_core::asmir::AsmGlobalRelocation {
                    offset: reloc.offset,
                    kind: match reloc.kind {
                        fp_core::lir::LirRelocationKind::Abs64 => {
                            fp_core::asmir::AsmRelocationKind::Abs64
                        }
                        fp_core::lir::LirRelocationKind::PcRel32 => {
                            fp_core::asmir::AsmRelocationKind::PcRel32
                        }
                    },
                    symbol,
                    addend: reloc.addend,
                })
            })
            .collect(),
        section: global.section.clone(),
        linkage: global.linkage.clone(),
        visibility: global.visibility.clone(),
        alignment: global.alignment,
        is_constant: global.is_constant,
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
        lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64, map_constant_kind,
        select_program,
    };
    use crate::asm::aarch64::{Aarch64CallTarget, Aarch64ConditionCode, Aarch64TerminatorOpcode};
    use crate::asm::aarch64::{
        Aarch64InstructionDetail, Aarch64Operand, Aarch64Register, Aarch64TerminatorDetail,
        AsmAarch64Block, AsmAarch64Function, AsmAarch64Program,
    };
    use crate::asm::x86_64::{
        AsmX86_64Block, AsmX86_64Function, AsmX86_64Program, X86InstructionDetail, X86Operand,
        X86Register, X86TerminatorDetail,
    };
    use crate::asm::x86_64::{X86CallTarget, X86ConditionCode, X86Opcode, X86TerminatorOpcode};
    use crate::emit::{TargetArch, TargetFormat};
    use fp_core::asmir::{
        AsmConditionCode, AsmGenericOpcode, AsmInstructionKind, AsmOpcode, AsmOperand,
        AsmTerminator, AsmValue, OperandAccess,
    };
    use fp_core::lir::{
        CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
        LirInstruction, LirInstructionKind, LirInteger, LirProgram, LirRegister, LirTerminator,
        LirType, LirValue, Name,
    };

    fn layout() -> fp_core::lir::LirDataLayout {
        fp_core::lir::LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .unwrap()
    }

    fn i32_value(value: u32) -> LirValue {
        LirValue::constant(LirConstant::integer(LirType::I32, LirInteger::I32(value)).unwrap())
    }

    #[test]
    fn maps_constant_global_gep_to_global_reference() {
        let ptr_ty = LirType::Ptr(Box::new(LirType::I8));
        let constant = LirConstant::get_element_ptr(
            ptr_ty.clone(),
            LirConstant::global_address(ptr_ty.clone(), Name::new("message")),
            vec![LirConstant::integer(LirType::I64, LirInteger::I64(1)).unwrap()],
            true,
        );

        let mapped = map_constant_kind(&constant.kind, &constant.ty);
        assert!(matches!(
            mapped,
            fp_core::asmir::AsmConstant::GlobalRef(name, _, indices)
                if name == Name::new("message") && indices == vec![1]
        ));
    }

    fn reg(id: u32, ty: LirType) -> LirValue {
        LirValue::register(id, ty)
    }

    #[test]
    fn select_program_builds_semantic_asmir() {
        let lir = LirProgram {
            data_layout: layout(),
            functions: vec![LirFunction {
                def_id: None,
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
                        kind: LirInstructionKind::Freeze(LirValue::constant(LirConstant::undef(
                            LirType::I32,
                        ))),
                        result: Some(LirRegister {
                            id: 1,
                            ty: LirType::I32,
                        }),
                        debug_info: None,
                    }],
                    terminator: LirTerminator::Return(Some(reg(1, LirType::I32))),
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
            comptime_entries: Vec::new(),

            queries: Vec::new(),
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
            data_layout: layout(),
            functions: vec![LirFunction {
                def_id: None,
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
                        kind: LirInstructionKind::Add(reg(1, LirType::I32), i32_value(4)),
                        result: Some(LirRegister {
                            id: 7,
                            ty: LirType::I32,
                        }),
                        debug_info: None,
                    }],
                    terminator: LirTerminator::Return(Some(reg(7, LirType::I32))),
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
            comptime_entries: Vec::new(),

            queries: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let inst = &program.functions[0].basic_blocks[0].instructions[0];

        assert_eq!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Add));
        assert_eq!(inst.operands.len(), 3);
        assert!(matches!(
            &inst.operands[0],
            AsmOperand::Register {
                access: OperandAccess::Write,
                ..
            }
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
            data_layout: layout(),
            functions: vec![LirFunction {
                def_id: None,
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
                            kind: LirInstructionKind::Eq(i32_value(1), i32_value(2)),
                            result: Some(LirRegister {
                                id: 1,
                                ty: LirType::I1,
                            }),
                            debug_info: None,
                        },
                        LirInstruction {
                            id: 2,
                            kind: LirInstructionKind::Call {
                                function: LirValue::function(
                                    fp_core::lir::LirFunctionRef::Name(Name::new("callee")),
                                    LirType::Ptr(Box::new(LirType::I8)),
                                ),
                                args: Vec::new(),
                                calling_convention: CallingConvention::C,
                                tail_call: false,
                            },
                            result: Some(LirRegister {
                                id: 2,
                                ty: LirType::I32,
                            }),
                            debug_info: None,
                        },
                    ],
                    terminator: LirTerminator::Return(Some(reg(2, LirType::I32))),
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
            comptime_entries: Vec::new(),

            queries: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let x86 = lower_to_x86_64(&program);
        let eq_inst = &x86.functions[0].blocks[0].instructions[0];
        let call_inst = &x86.functions[0].blocks[0].instructions[1];

        assert!(matches!(eq_inst.condition, Some(X86ConditionCode::Equal)));
        assert!(matches!(
            call_inst.call_target,
            Some(X86CallTarget::Symbol(ref name)) if *name == Name::new("callee")
        ));
    }

    #[test]
    fn lower_to_aarch64_preserves_concrete_branch_and_call_metadata() {
        let lir = LirProgram {
            data_layout: layout(),
            functions: vec![LirFunction {
                def_id: None,
                name: Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![
                    LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions: vec![
                            LirInstruction {
                                id: 1,
                                kind: LirInstructionKind::Eq(i32_value(1), i32_value(2)),
                                result: Some(LirRegister {
                                    id: 1,
                                    ty: LirType::I1,
                                }),
                                debug_info: None,
                            },
                            LirInstruction {
                                id: 2,
                                kind: LirInstructionKind::Call {
                                    function: LirValue::function(
                                        fp_core::lir::LirFunctionRef::Name(Name::new("callee")),
                                        LirType::Ptr(Box::new(LirType::I8)),
                                    ),
                                    args: Vec::new(),
                                    calling_convention: CallingConvention::C,
                                    tail_call: false,
                                },
                                result: Some(LirRegister {
                                    id: 2,
                                    ty: LirType::I32,
                                }),
                                debug_info: None,
                            },
                        ],
                        terminator: LirTerminator::Br(1),
                        predecessors: Vec::new(),
                        successors: vec![1],
                    },
                    LirBasicBlock {
                        id: 1,
                        label: Some(Name::new("exit")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Return(Some(reg(2, LirType::I32))),
                        predecessors: vec![0],
                        successors: Vec::new(),
                    },
                ],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: fp_core::lir::Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            comptime_entries: Vec::new(),

            queries: Vec::new(),
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
            data_layout: layout(),
            functions: vec![
                LirFunction {
                    def_id: None,
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
                    def_id: None,
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
                            condition: reg(1, LirType::I1),
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
            comptime_entries: Vec::new(),

            queries: Vec::new(),
        };

        let program = select_program(&lir, TargetFormat::Elf, TargetArch::X86_64).unwrap();
        let x86 = lower_to_x86_64(&program);

        assert_eq!(x86.functions.len(), 1);
        assert_eq!(x86.functions[0].name, Name::new("main"));
        assert_eq!(
            x86.functions[0].blocks[0].terminator.opcode,
            X86TerminatorOpcode::Jcc
        );
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
                                reg: X86Register::Virtual {
                                    id: 1,
                                    size_bits: 64,
                                },
                                access: OperandAccess::Write,
                            },
                            X86Operand::Register {
                                reg: X86Register::Virtual {
                                    id: 2,
                                    size_bits: 64,
                                },
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

        let asmir = lift_from_x86_64(&x86).unwrap();
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
        assert_eq!(
            lowered.functions[0].blocks[0].terminator,
            x86.functions[0].blocks[0].terminator
        );
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
                                reg: Aarch64Register::Virtual {
                                    id: 1,
                                    size_bits: 64,
                                },
                                access: OperandAccess::Write,
                            },
                            Aarch64Operand::Register {
                                reg: Aarch64Register::Virtual {
                                    id: 2,
                                    size_bits: 64,
                                },
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

        let asmir = lift_from_aarch64(&aarch64).unwrap();
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
        assert_eq!(
            lowered.functions[0].blocks[0].terminator,
            aarch64.functions[0].blocks[0].terminator
        );
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
                                reg: X86Register::Virtual {
                                    id: 2,
                                    size_bits: 64,
                                },
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

        let asmir = lift_from_x86_64(&x86).unwrap();
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
                                reg: Aarch64Register::Virtual {
                                    id: 2,
                                    size_bits: 64,
                                },
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

        let asmir = lift_from_aarch64(&aarch64).unwrap();
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

        let asmir = lift_from_x86_64(&x86).unwrap();
        let lowered = lower_to_x86_64(&asmir);

        let instructions = &lowered.functions[0].blocks[0].instructions;
        assert_eq!(instructions.len(), 2);

        let X86InstructionDetail {
            operands: mov_operands,
            ..
        } = &instructions[0];
        let [
            X86Operand::Register {
                reg: X86Register::Virtual { id: dst_id, .. },
                ..
            },
            X86Operand::Memory(mem),
        ] = mov_operands.as_slice()
        else {
            panic!("unexpected x86 mov operands: {mov_operands:?}");
        };
        assert_eq!(mem.scale, 2);
        assert_eq!(mem.displacement, 8);
        assert_eq!(mem.size_bytes, Some(8));
        assert!(matches!(mem.base, Some(X86Register::Virtual { .. })));
        assert!(matches!(mem.index, Some(X86Register::Virtual { .. })));

        let X86InstructionDetail {
            operands: call_operands,
            call_target,
            ..
        } = &instructions[1];
        let [
            X86Operand::Register {
                reg: X86Register::Virtual { id: call_id, .. },
                ..
            },
        ] = call_operands.as_slice()
        else {
            panic!("unexpected x86 call operands: {call_operands:?}");
        };
        assert_eq!(dst_id, call_id);
        assert!(matches!(
            call_target,
            Some(X86CallTarget::Register(X86Register::Virtual { id, .. })) if id == call_id
        ));
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
                            call_target: Some(Aarch64CallTarget::Register(
                                Aarch64Register::Physical {
                                    name: "x0".to_string(),
                                    size_bits: 64,
                                },
                            )),
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

        let asmir = lift_from_aarch64(&aarch64).unwrap();
        let lowered = lower_to_aarch64(&asmir);

        let instructions = &lowered.functions[0].blocks[0].instructions;
        assert_eq!(instructions.len(), 2);

        let Aarch64InstructionDetail {
            operands: store_operands,
            ..
        } = &instructions[0];
        let [
            Aarch64Operand::Register {
                reg: Aarch64Register::Virtual { .. },
                ..
            },
            Aarch64Operand::Memory(mem),
        ] = store_operands.as_slice()
        else {
            panic!("unexpected aarch64 store operands: {store_operands:?}");
        };
        assert_eq!(mem.scale, 3);
        assert_eq!(mem.displacement, 16);
        assert_eq!(mem.size_bytes, Some(8));
        assert!(matches!(mem.base, Some(Aarch64Register::Virtual { .. })));
        assert!(matches!(mem.index, Some(Aarch64Register::Virtual { .. })));

        let Aarch64InstructionDetail {
            operands: call_operands,
            call_target,
            ..
        } = &instructions[1];
        let [
            Aarch64Operand::Register {
                reg: Aarch64Register::Virtual { id: call_id, .. },
                ..
            },
        ] = call_operands.as_slice()
        else {
            panic!("unexpected aarch64 call operands: {call_operands:?}");
        };
        assert!(matches!(
            call_target,
            Some(Aarch64CallTarget::Register(Aarch64Register::Virtual { id, .. })) if id == call_id
        ));
    }
}
