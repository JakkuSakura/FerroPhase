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
    AsmArchitecture, AsmAttr, AsmBlock, AsmConditionCode, AsmConstant, AsmEndianness, AsmFunction,
    AsmFunctionSignature, AsmGenericOpcode, AsmGlobal, AsmInstrId, AsmInstruction,
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
        AsmRegisterBank::Flags => 4,
        AsmRegisterBank::Special => 5,
        AsmRegisterBank::Custom(_) => 6,
    }
}

/// Canonical type for every virtual register this function declares. The
/// `AsmFunction::virtual_registers` table is the sole source of truth for
/// this now that `AsmRegister::Virtual` carries only a bare id.
fn build_operand_type_map(function: &AsmFunction) -> HashMap<AsmVirtualRegId, AsmType> {
    function
        .virtual_registers
        .iter()
        .map(|(id, reg)| (*id, reg.ty.clone()))
        .collect()
}

/// A fresh id space for virtual registers synthesized while mapping named
/// physical registers (e.g. `sp`/`fp`) into the machine-specific virtual
/// register space, bounded above every id already declared by this
/// function so newly minted ids can't collide with real ones.
fn next_synthetic_virtual_id(function: &AsmFunction) -> u32 {
    function
        .virtual_registers
        .keys()
        .max()
        .copied()
        .unwrap_or(0)
        .saturating_add(1)
}

pub fn lower_to_x86_64(program: &AsmProgram) -> x86_64_asm::AsmX86_64Program {
    x86_64_asm::AsmX86_64Program {
        functions: program
            .functions
            .iter()
            .filter(|function| !function.is_declaration)
            .map(|function| {
                let mut ctx = PhysicalRegisterLoweringContext::new(
                    next_synthetic_virtual_id(function),
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
                let mut ctx = PhysicalRegisterLoweringContext::new(
                    next_synthetic_virtual_id(function),
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

/// Converts every `AsmRegister::Physical` occurrence in `program` into a
/// fresh, function-scoped virtual register, deduplicated by
/// `(name, size_bits, bank)` so repeated uses of the same physical register
/// within one function map to the same virtual id. New ids are minted via
/// `AsmFunction::alloc_virtual_register` (never hand-picked), so this runs
/// per function in three passes to avoid holding a mutable borrow of
/// `basic_blocks` at the same time as the `&mut AsmFunction` needed to
/// allocate: (1) scan for distinct physical registers, (2) allocate a vreg
/// for each, (3) rewrite every occurrence using the resulting map.
fn canonicalize_physical_registers(program: &mut AsmProgram) {
    for function in &mut program.functions {
        let mut order: Vec<AsmPhysicalRegister> = Vec::new();
        let mut seen: std::collections::HashSet<(String, u16, u8)> =
            std::collections::HashSet::new();
        for_each_function_register(function, &mut |reg| {
            if let AsmRegister::Physical(physical) = reg {
                let key = (
                    physical.name.to_ascii_lowercase(),
                    physical.size_bits,
                    register_bank_id(physical.bank.clone()),
                );
                if seen.insert(key) {
                    order.push(physical.clone());
                }
            }
        });

        let mut map: HashMap<(String, u16, u8), AsmVirtualRegId> = HashMap::new();
        for physical in &order {
            let key = (
                physical.name.to_ascii_lowercase(),
                physical.size_bits,
                register_bank_id(physical.bank.clone()),
            );
            let vreg = function.alloc_virtual_register(
                physical_register_asm_type(physical),
                physical.bank.clone(),
                physical.size_bits,
            );
            map.insert(key, vreg);
        }

        for_each_function_register(function, &mut |reg| {
            let AsmRegister::Physical(physical) = reg else {
                return;
            };
            let key = (
                physical.name.to_ascii_lowercase(),
                physical.size_bits,
                register_bank_id(physical.bank.clone()),
            );
            if let Some(id) = map.get(&key) {
                *reg = AsmRegister::Virtual(*id);
            }
        });
    }
}

/// A reasonable canonical type for a physical register, used only to
/// populate the virtual-register table when canonicalizing physical
/// registers into virtual ones (the exact type is otherwise not load
/// bearing for this pass).
fn physical_register_asm_type(register: &AsmPhysicalRegister) -> AsmType {
    match register.bank {
        AsmRegisterBank::Float if register.size_bits <= 32 => AsmType::F32,
        AsmRegisterBank::Float => AsmType::F64,
        _ => type_from_bits(register.size_bits),
    }
}

/// Visits every `AsmRegister` reachable from `function`: instruction
/// operands (including nested `SysOp` operands and memory base/index/
/// segment), implicit uses/defs, and terminator operands.
fn for_each_function_register(function: &mut AsmFunction, f: &mut dyn FnMut(&mut AsmRegister)) {
    for block in &mut function.basic_blocks {
        for instruction in &mut block.instructions {
            for operand in &mut instruction.operands {
                for_each_operand_register(operand, f);
            }
            for reg in instruction
                .implicit_uses
                .iter_mut()
                .chain(instruction.implicit_defs.iter_mut())
            {
                f(reg);
            }
        }
        for operand in terminator_operands_mut(&mut block.terminator) {
            for_each_operand_register(operand, f);
        }
    }
}

fn for_each_operand_register(operand: &mut AsmOperand, f: &mut dyn FnMut(&mut AsmRegister)) {
    match operand {
        AsmOperand::Register { reg, .. } | AsmOperand::Predicate { reg, .. } => f(reg),
        AsmOperand::Memory(memory) => {
            if let Some(reg) = &mut memory.base {
                f(reg);
            }
            if let Some(reg) = &mut memory.index {
                f(reg);
            }
            if let Some(reg) = &mut memory.segment {
                f(reg);
            }
        }
        AsmOperand::SysOp(op) => {
            for nested in sysop_operands_mut(op) {
                for_each_operand_register(nested, f);
            }
        }
        AsmOperand::Immediate(_)
        | AsmOperand::Constant(_)
        | AsmOperand::Label(_)
        | AsmOperand::Symbol(_)
        | AsmOperand::Block(_)
        | AsmOperand::Relocation(_)
        | AsmOperand::Local(_)
        | AsmOperand::StackSlot(_)
        | AsmOperand::Condition(_)
        | AsmOperand::Attr(_) => {}
    }
}

/// Every `AsmOperand` field nested inside an `AsmSysOp`, in an order stable
/// enough for generic recursive traversal (register canonicalization,
/// constant interning, ...).
fn sysop_operands_mut(op: &mut fp_core::asmir::AsmSysOp) -> Vec<&mut AsmOperand> {
    use fp_core::asmir::AsmSysOp;
    match op {
        AsmSysOp::Exit { code } => vec![code],
        AsmSysOp::GetPid | AsmSysOp::GetTid => vec![],
        AsmSysOp::Dlopen { path, flags } => vec![path, flags],
        AsmSysOp::Dlsym { handle, symbol } => vec![handle, symbol],
        AsmSysOp::Dlclose { handle } => vec![handle],
        AsmSysOp::Unlink { path } | AsmSysOp::Rmdir { path } | AsmSysOp::Opendir { path } => {
            vec![path]
        }
        AsmSysOp::Mkdir { path, mode } => vec![path, mode],
        AsmSysOp::Rename { from, to } => vec![from, to],
        AsmSysOp::Access { path, mode } => vec![path, mode],
        AsmSysOp::Write { fd, buffer, len } | AsmSysOp::Read { fd, buffer, len } => {
            vec![fd, buffer, len]
        }
        AsmSysOp::Close { fd } => vec![fd],
        AsmSysOp::Open {
            path, flags, mode, ..
        } => vec![path, flags, mode],
        AsmSysOp::Seek { fd, offset, whence } => vec![fd, offset, whence],
        AsmSysOp::Mmap {
            addr,
            len,
            prot,
            flags,
            fd,
            offset,
        } => vec![addr, len, prot, flags, fd, offset],
        AsmSysOp::Munmap { addr, len } => vec![addr, len],
        AsmSysOp::Readdir { dir, .. } | AsmSysOp::Closedir { dir } => vec![dir],
    }
}

/// Every `AsmOperand` field carried directly by an `AsmTerminator`.
fn terminator_operands_mut(terminator: &mut AsmTerminator) -> Vec<&mut AsmOperand> {
    match terminator {
        AsmTerminator::Return(Some(value)) => vec![value],
        AsmTerminator::CondBr { condition, .. } => vec![condition],
        AsmTerminator::Switch { value, .. } => vec![value],
        AsmTerminator::IndirectBr { address, .. } => vec![address],
        AsmTerminator::Invoke { function, args, .. } => {
            let mut out = vec![function];
            out.extend(args.iter_mut());
            out
        }
        AsmTerminator::Resume(value) => vec![value],
        AsmTerminator::CleanupRet { cleanup_pad, .. } => vec![cleanup_pad],
        AsmTerminator::CatchRet { catch_pad, .. } => vec![catch_pad],
        AsmTerminator::CatchSwitch { parent_pad, .. } => parent_pad.iter_mut().collect(),
        AsmTerminator::Return(None) | AsmTerminator::Br(_) | AsmTerminator::Unreachable => {
            vec![]
        }
    }
}

pub fn lift_from_x86_64(program: &x86_64_asm::AsmX86_64Program) -> Result<AsmProgram> {
    let target = AsmTarget {
        architecture: AsmArchitecture::X86_64,
        object_format: AsmObjectFormat::Raw,
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    };
    let mut functions = Vec::with_capacity(program.functions.len());
    for raw_function in &program.functions {
        let mut function = AsmFunction::new(
            raw_function.name.clone(),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        function.section = Some(".text".to_string());
        let mut ctx = LiftContext::new(&mut function);
        let mut next_instruction_id: AsmInstrId = 0;
        let mut basic_blocks = Vec::with_capacity(raw_function.blocks.len());
        for block in &raw_function.blocks {
            let mut instructions = Vec::with_capacity(block.instructions.len());
            for raw_instruction in &block.instructions {
                instructions.push(lift_x86_instruction(
                    raw_instruction,
                    next_instruction_id,
                    &mut ctx,
                )?);
                next_instruction_id += 1;
            }
            let terminator = relink_comparison_condition(
                instructions.as_slice(),
                lift_x86_terminator(&block.terminator)?,
            );
            basic_blocks.push(AsmBlock {
                id: block.id,
                label: Some(Name::new(format!("bb{}", block.id))),
                instructions,
                terminator,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: block.terminator.targets.clone(),
            });
        }
        function.basic_blocks = basic_blocks;
        functions.push(function);
    }

    let mut lifted = AsmProgram {
        target: target.clone(),
        data_layout: target.data_layout(),
        lifted_from: Some(target),
        container: None,
        sections: vec![AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        functions,
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
    let target = AsmTarget {
        architecture: AsmArchitecture::Aarch64,
        object_format: AsmObjectFormat::Raw,
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    };
    let mut functions = Vec::with_capacity(program.functions.len());
    for raw_function in &program.functions {
        let mut function = AsmFunction::new(
            raw_function.name.clone(),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        function.section = Some(".text".to_string());
        let mut ctx = LiftContext::new(&mut function);
        let mut next_instruction_id: AsmInstrId = 0;
        let mut basic_blocks = Vec::with_capacity(raw_function.blocks.len());
        for block in &raw_function.blocks {
            let mut instructions = Vec::with_capacity(block.instructions.len());
            for raw_instruction in &block.instructions {
                instructions.push(lift_aarch64_instruction(
                    raw_instruction,
                    next_instruction_id,
                    &mut ctx,
                )?);
                next_instruction_id += 1;
            }
            let terminator = relink_comparison_condition(
                instructions.as_slice(),
                lift_aarch64_terminator(&block.terminator)?,
            );
            basic_blocks.push(AsmBlock {
                id: block.id,
                label: Some(Name::new(format!("bb{}", block.id))),
                instructions,
                terminator,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: block.terminator.targets.clone(),
            });
        }
        function.basic_blocks = basic_blocks;
        functions.push(function);
    }

    let mut lifted = AsmProgram {
        target: target.clone(),
        data_layout: target.data_layout(),
        lifted_from: Some(target),
        container: None,
        sections: vec![AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        functions,
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


/// If `terminator`'s condition is a bare `AsmOperand::Condition(cc)` and the
/// last comparison-shaped instruction in the block computes that same
/// condition, rewrites the condition to reference that instruction's own
/// result register instead. This lets a later `lower_to_*` regenerate the
/// exact original `cmp`/`jcc` pair (or `cmp`/`b.cc`) rather than falling
/// back to a generic compare-with-zero, without needing a separate "flags"
/// concept: the comparison's result register already *is* the condition
/// value in the canonical schema.
fn relink_comparison_condition(
    instructions: &[AsmInstruction],
    terminator: AsmTerminator,
) -> AsmTerminator {
    match terminator {
        AsmTerminator::CondBr {
            condition: AsmOperand::Condition(condition),
            if_true,
            if_false,
        } => {
            let condition = last_comparison_instruction(instructions)
                .filter(|(_, comparison)| comparison == &condition)
                .and_then(|(instruction, _)| instruction.result_register())
                .map(|reg| AsmOperand::Register {
                    reg: reg.clone(),
                    access: OperandAccess::Read,
                })
                .unwrap_or(AsmOperand::Condition(condition));
            AsmTerminator::CondBr {
                condition,
                if_true,
                if_false,
            }
        }
        other => other,
    }
}

fn last_comparison_instruction(
    instructions: &[AsmInstruction],
) -> Option<(&AsmInstruction, AsmConditionCode)> {
    instructions.iter().rev().find_map(|instruction| {
        comparison_code_from_opcode(&instruction.opcode).map(|code| (instruction, code))
    })
}

fn comparison_code_from_opcode(opcode: &AsmOpcode) -> Option<AsmConditionCode> {
    let AsmOpcode::Generic(generic) = opcode else {
        return None;
    };
    Some(match generic {
        AsmGenericOpcode::Eq => AsmConditionCode::Eq,
        AsmGenericOpcode::Ne => AsmConditionCode::Ne,
        AsmGenericOpcode::Lt => AsmConditionCode::Lt,
        AsmGenericOpcode::Le => AsmConditionCode::Le,
        AsmGenericOpcode::Gt => AsmConditionCode::Gt,
        AsmGenericOpcode::Ge => AsmConditionCode::Ge,
        AsmGenericOpcode::Ult => AsmConditionCode::Ult,
        AsmGenericOpcode::Ule => AsmConditionCode::Ule,
        AsmGenericOpcode::Ugt => AsmConditionCode::Ugt,
        AsmGenericOpcode::Uge => AsmConditionCode::Uge,
        _ => return None,
    })
}

fn x86_detail_from_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86InstructionDetail {
    match &instruction.opcode {
        AsmOpcode::Custom(opcode) => x86_detail_from_custom(opcode, &instruction.operands, ctx),
        AsmOpcode::Generic(generic) => {
            let opcode = x86_opcode_for_generic(generic, &instruction.operands, ctx);
            let condition = x86_condition_for_generic(generic);
            let operands = x86_machine_operands(generic, instruction, ctx);
            let call_target = x86_call_target_for_instruction(instruction, ctx);
            X86InstructionDetail {
                opcode,
                operands,
                condition,
                call_target,
            }
        }
    }
}

fn aarch64_detail_from_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64InstructionDetail {
    match &instruction.opcode {
        AsmOpcode::Custom(opcode) => aarch64_detail_from_custom(opcode, &instruction.operands, ctx),
        AsmOpcode::Generic(generic) => {
            let opcode = aarch64_opcode_for_generic(generic, &instruction.operands, ctx);
            let condition = aarch64_condition_for_generic(generic);
            let operands = aarch64_machine_operands(generic, instruction, ctx);
            let call_target = aarch64_call_target_for_instruction(instruction, ctx);
            Aarch64InstructionDetail {
                opcode: opcode.to_string(),
                operands,
                condition,
                call_target,
            }
        }
    }
}

/// The sole `Write`/`ReadWrite` register operand, if any, as a whole
/// operand (so its access marker is preserved when converting).
fn write_operand(operands: &[AsmOperand]) -> Option<&AsmOperand> {
    operands.iter().find(|operand| {
        matches!(
            operand,
            AsmOperand::Register {
                access: OperandAccess::Write | OperandAccess::ReadWrite,
                ..
            }
        )
    })
}

fn find_attr(operands: &[AsmOperand], pred: impl Fn(&AsmAttr) -> bool) -> Option<&AsmAttr> {
    operands.iter().find_map(|operand| match operand {
        AsmOperand::Attr(attr) if pred(attr) => Some(attr),
        _ => None,
    })
}

/// The canonical type of the instruction's result register, if it has one
/// and its type is known (physical registers carry only a bank + width, so
/// this approximates float-ness from the bank in that case).
fn result_asm_type(operands: &[AsmOperand], ctx: &PhysicalRegisterLoweringContext) -> Option<AsmType> {
    let reg = match write_operand(operands)? {
        AsmOperand::Register { reg, .. } => reg,
        _ => return None,
    };
    Some(match reg {
        AsmRegister::Physical(physical) => {
            if matches!(physical.bank, AsmRegisterBank::Float) {
                if physical.size_bits <= 32 {
                    AsmType::F32
                } else {
                    AsmType::F64
                }
            } else {
                type_from_bits(physical.size_bits)
            }
        }
        AsmRegister::Virtual(id) => ctx
            .register_types
            .get(id)
            .cloned()
            .unwrap_or(AsmType::I64),
    })
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

fn x86_opcode_for_generic(
    opcode: &AsmGenericOpcode,
    operands: &[AsmOperand],
    ctx: &PhysicalRegisterLoweringContext,
) -> X86Opcode {
    let ty = result_asm_type(operands, ctx);
    match opcode {
        AsmGenericOpcode::Nop => X86Opcode::Nop,
        AsmGenericOpcode::Add => X86Opcode::Add,
        AsmGenericOpcode::Sub => X86Opcode::Sub,
        AsmGenericOpcode::Mul if is_float_type_opt(ty.as_ref()) => {
            float_binop_opcode("mul", ty.as_ref())
        }
        AsmGenericOpcode::Mul => X86Opcode::IMul,
        AsmGenericOpcode::Div | AsmGenericOpcode::Rem if is_float_type_opt(ty.as_ref()) => {
            float_binop_opcode("div", ty.as_ref())
        }
        AsmGenericOpcode::Div | AsmGenericOpcode::Rem => X86Opcode::IDiv,
        AsmGenericOpcode::And => X86Opcode::And,
        AsmGenericOpcode::Or => X86Opcode::Or,
        AsmGenericOpcode::Xor => X86Opcode::Xor,
        AsmGenericOpcode::Shl => X86Opcode::Shl,
        AsmGenericOpcode::Shr => X86Opcode::Sar,
        AsmGenericOpcode::Not => X86Opcode::Not,
        AsmGenericOpcode::Eq
        | AsmGenericOpcode::Ne
        | AsmGenericOpcode::Lt
        | AsmGenericOpcode::Le
        | AsmGenericOpcode::Gt
        | AsmGenericOpcode::Ge
        | AsmGenericOpcode::Ult
        | AsmGenericOpcode::Ule
        | AsmGenericOpcode::Ugt
        | AsmGenericOpcode::Uge => X86Opcode::Cmp,
        AsmGenericOpcode::Load | AsmGenericOpcode::Store => X86Opcode::Mov,
        AsmGenericOpcode::Alloca => X86Opcode::LeaFrame,
        AsmGenericOpcode::GetElementPtr => X86Opcode::Lea,
        AsmGenericOpcode::Bitcast
        | AsmGenericOpcode::PtrToInt
        | AsmGenericOpcode::IntToPtr
        | AsmGenericOpcode::Trunc
        | AsmGenericOpcode::ZExt
        | AsmGenericOpcode::SExt
        | AsmGenericOpcode::SextOrTrunc
        | AsmGenericOpcode::Freeze => X86Opcode::Mov,
        AsmGenericOpcode::FPExt => X86Opcode::Cvtss2sd,
        AsmGenericOpcode::FPTrunc => X86Opcode::Cvtsd2ss,
        AsmGenericOpcode::FPToUI | AsmGenericOpcode::FPToSI => X86Opcode::Cvttsd2si,
        AsmGenericOpcode::UIToFP | AsmGenericOpcode::SIToFP => X86Opcode::Cvtsi2sd,
        AsmGenericOpcode::ExtractValue => X86Opcode::MovExtract,
        AsmGenericOpcode::InsertValue => X86Opcode::MovInsert,
        AsmGenericOpcode::Call | AsmGenericOpcode::IntrinsicCall => X86Opcode::Call,
        AsmGenericOpcode::Phi => X86Opcode::PhiCopy,
        AsmGenericOpcode::Select => X86Opcode::CMov,
        AsmGenericOpcode::InlineAsm => X86Opcode::InlineAsm,
        AsmGenericOpcode::LandingPad => X86Opcode::LandingPad,
        AsmGenericOpcode::Syscall => X86Opcode::Syscall,
        AsmGenericOpcode::SysOp => X86Opcode::InlineAsm,
        AsmGenericOpcode::Splat
        | AsmGenericOpcode::BuildVector
        | AsmGenericOpcode::ExtractLane
        | AsmGenericOpcode::InsertLane
        | AsmGenericOpcode::ZipLow => X86Opcode::Mov,
        AsmGenericOpcode::SymbolAddress => X86Opcode::Mov,
        AsmGenericOpcode::Unreachable => X86Opcode::Ud2,
    }
}

fn x86_condition_for_generic(opcode: &AsmGenericOpcode) -> Option<X86ConditionCode> {
    match opcode {
        AsmGenericOpcode::Eq => Some(X86ConditionCode::Equal),
        AsmGenericOpcode::Ne => Some(X86ConditionCode::NotEqual),
        AsmGenericOpcode::Lt => Some(X86ConditionCode::Less),
        AsmGenericOpcode::Le => Some(X86ConditionCode::LessEqual),
        AsmGenericOpcode::Gt => Some(X86ConditionCode::Greater),
        AsmGenericOpcode::Ge => Some(X86ConditionCode::GreaterEqual),
        AsmGenericOpcode::Ult => Some(X86ConditionCode::Below),
        AsmGenericOpcode::Ule => Some(X86ConditionCode::BelowEqual),
        AsmGenericOpcode::Ugt => Some(X86ConditionCode::Above),
        AsmGenericOpcode::Uge => Some(X86ConditionCode::AboveEqual),
        AsmGenericOpcode::Select => Some(X86ConditionCode::NonZero),
        _ => None,
    }
}

fn x86_call_target_for_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<X86CallTarget> {
    match &instruction.opcode {
        AsmOpcode::Generic(AsmGenericOpcode::Call) => {
            let (target, _args) = instruction.call_target_and_args()?;
            Some(x86_call_target_from_operand_generic(target, ctx))
        }
        AsmOpcode::Generic(AsmGenericOpcode::IntrinsicCall) => {
            let AsmAttr::Intrinsic(kind) =
                find_attr(&instruction.operands, |attr| matches!(attr, AsmAttr::Intrinsic(_)))?
            else {
                return None;
            };
            Some(X86CallTarget::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )))
        }
        _ => None,
    }
}

fn x86_call_target_from_operand_generic(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86CallTarget {
    match operand {
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => X86CallTarget::Symbol(name.clone()),
        AsmOperand::Register { reg, .. } => X86CallTarget::Register(asm_register_to_x86(reg, ctx)),
        AsmOperand::Constant(AsmConstant::FunctionRef(name, _))
        | AsmOperand::Constant(AsmConstant::GlobalRef(name, _, _)) => {
            X86CallTarget::Symbol(name.clone())
        }
        _ => X86CallTarget::Symbol(Name::new("indirect.call")),
    }
}

/// Builds the concrete x86 operand list for a generically-opcoded
/// instruction from its canonical operand schema. Most opcodes carry
/// exactly their canonical `Read`/`Write` operands with any `Attr` tags
/// filtered out (attrs are metadata consumed elsewhere, e.g. calling
/// convention or alignment); a handful of opcodes need bespoke handling
/// because the concrete detail's shape historically differs from the
/// canonical schema (memory-operand synthesis for `Load`/`Store`, dropped
/// argument/clause/lane operands for `Call`/`LandingPad`/vector ops, ...).
fn x86_machine_operands(
    opcode: &AsmGenericOpcode,
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Vec<X86Operand> {
    let operands = &instruction.operands;
    match opcode {
        AsmGenericOpcode::Syscall | AsmGenericOpcode::SysOp => Vec::new(),
        AsmGenericOpcode::Call => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            if let Some((target, _args)) = instruction.call_target_and_args() {
                out.push(asm_operand_to_x86(target, ctx));
            }
            out
        }
        AsmGenericOpcode::IntrinsicCall => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            if let Some(AsmAttr::Intrinsic(kind)) =
                find_attr(operands, |attr| matches!(attr, AsmAttr::Intrinsic(_)))
            {
                out.push(X86Operand::Symbol(Name::new(
                    format!("intrinsic.{kind:?}").to_ascii_lowercase(),
                )));
            }
            for operand in operands {
                if matches!(operand, AsmOperand::Attr(_))
                    || matches!(
                        operand,
                        AsmOperand::Register {
                            access: OperandAccess::Write,
                            ..
                        }
                    )
                {
                    continue;
                }
                out.push(asm_operand_to_x86(operand, ctx));
            }
            out
        }
        AsmGenericOpcode::Load => {
            let mut out = Vec::new();
            let ty = result_asm_type(operands, ctx);
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            if let Some(address) = operands.get(if write_operand(operands).is_some() { 1 } else { 0 })
            {
                out.push(x86_address_like_operand(address, ty.as_ref(), ctx));
            }
            out
        }
        AsmGenericOpcode::Store => {
            // Canonical schema: [Read value, Read address, Attr...]. The
            // concrete detail keeps the historical [address, value] order.
            let mut out = Vec::new();
            if let Some(address) = operands.get(1) {
                out.push(x86_address_like_operand(address, None, ctx));
            }
            if let Some(value) = operands.first() {
                out.push(asm_operand_to_x86(value, ctx));
            }
            out
        }
        AsmGenericOpcode::LandingPad => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            for operand in operands {
                match operand {
                    AsmOperand::Register {
                        access: OperandAccess::Write,
                        ..
                    } => continue,
                    AsmOperand::Attr(AsmAttr::Cleanup) => continue,
                    AsmOperand::Attr(AsmAttr::LandingPadCatch)
                    | AsmOperand::Attr(AsmAttr::LandingPadFilter(_)) => break,
                    AsmOperand::Attr(_) => continue,
                    other => {
                        out.push(asm_operand_to_x86(other, ctx));
                        break;
                    }
                }
            }
            out
        }
        AsmGenericOpcode::Splat => {
            // Canonical schema also carries lane_bits/lanes immediates the
            // concrete detail has no room for; keep just dest + value.
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            if let Some(value) = operands.get(1) {
                out.push(asm_operand_to_x86(value, ctx));
            }
            out
        }
        AsmGenericOpcode::InsertLane => {
            // Canonical schema: [dest, vector, Immediate(lane), value].
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            if let Some(vector) = operands.get(1) {
                out.push(asm_operand_to_x86(vector, ctx));
            }
            if let Some(value) = operands.get(3) {
                out.push(asm_operand_to_x86(value, ctx));
            }
            if let Some(lane) = operands.get(2) {
                out.push(asm_operand_to_x86(lane, ctx));
            }
            out
        }
        AsmGenericOpcode::ZipLow => {
            // Canonical schema also carries a trailing lane_bits immediate.
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_x86(dest, ctx));
            }
            for operand in operands.iter().skip(1).take(2) {
                out.push(asm_operand_to_x86(operand, ctx));
            }
            out
        }
        _ => operands
            .iter()
            .filter(|operand| !matches!(operand, AsmOperand::Attr(_)))
            .map(|operand| asm_operand_to_x86(operand, ctx))
            .collect(),
    }
}

/// Converts a canonical `Load`/`Store` address operand into a concrete x86
/// operand, synthesizing a `[base]` memory operand when the address is a
/// bare register (i.e. a pointer value computed by an earlier instruction)
/// rather than an already-folded `AsmOperand::Memory`.
fn x86_address_like_operand(
    operand: &AsmOperand,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Operand {
    match operand {
        AsmOperand::Memory(_) | AsmOperand::Symbol(_) | AsmOperand::Label(_) => {
            asm_operand_to_x86(operand, ctx)
        }
        AsmOperand::Register { reg, .. } => X86Operand::Memory(X86MemoryOperand {
            base: Some(asm_register_to_x86(reg, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmOperand::Local(id) => X86Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmOperand::StackSlot(id) => X86Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        AsmOperand::Constant(AsmConstant::GlobalRef(name, _, _))
        | AsmOperand::Constant(AsmConstant::FunctionRef(name, _)) => {
            X86Operand::Symbol(name.clone())
        }
        other => asm_operand_to_x86(other, ctx),
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
    condition: &AsmOperand,
    instructions: &[AsmInstruction],
) -> Option<X86ConditionCode> {
    match condition {
        AsmOperand::Register {
            reg: reg @ AsmRegister::Virtual(_),
            ..
        } => instructions
            .iter()
            .find(|instruction| instruction.result_register() == Some(reg))
            .and_then(|instruction| comparison_code_from_opcode(&instruction.opcode))
            .map(|code| x86_condition_from_asm(&code)),
        other => x86_branch_condition(other),
    }
}

fn aarch64_opcode_for_generic(
    opcode: &AsmGenericOpcode,
    operands: &[AsmOperand],
    ctx: &PhysicalRegisterLoweringContext,
) -> &'static str {
    let ty = result_asm_type(operands, ctx);
    let is_f32 = matches!(ty, Some(AsmType::F32));
    match opcode {
        AsmGenericOpcode::Nop => "nop",
        AsmGenericOpcode::Add => "add",
        AsmGenericOpcode::Sub => "sub",
        AsmGenericOpcode::Mul if is_float_type_opt(ty.as_ref()) => {
            if is_f32 {
                "fmul.s"
            } else {
                "fmul.d"
            }
        }
        AsmGenericOpcode::Mul => "mul",
        AsmGenericOpcode::Div | AsmGenericOpcode::Rem if is_float_type_opt(ty.as_ref()) => {
            if is_f32 {
                "fdiv.s"
            } else {
                "fdiv.d"
            }
        }
        AsmGenericOpcode::Div => "sdiv",
        AsmGenericOpcode::Rem => "msub.rem",
        AsmGenericOpcode::And => "and",
        AsmGenericOpcode::Or => "orr",
        AsmGenericOpcode::Xor => "eor",
        AsmGenericOpcode::Shl => "lsl",
        AsmGenericOpcode::Shr => "asr",
        AsmGenericOpcode::Not => "mvn",
        AsmGenericOpcode::Eq
        | AsmGenericOpcode::Ne
        | AsmGenericOpcode::Lt
        | AsmGenericOpcode::Le
        | AsmGenericOpcode::Gt
        | AsmGenericOpcode::Ge
        | AsmGenericOpcode::Ult
        | AsmGenericOpcode::Ule
        | AsmGenericOpcode::Ugt
        | AsmGenericOpcode::Uge => "cmp",
        AsmGenericOpcode::Load => "ldr",
        AsmGenericOpcode::Store => "str",
        AsmGenericOpcode::Alloca => "add.sp",
        AsmGenericOpcode::GetElementPtr => "add.addr",
        AsmGenericOpcode::Bitcast
        | AsmGenericOpcode::PtrToInt
        | AsmGenericOpcode::IntToPtr
        | AsmGenericOpcode::Trunc
        | AsmGenericOpcode::ZExt
        | AsmGenericOpcode::SExt
        | AsmGenericOpcode::SextOrTrunc
        | AsmGenericOpcode::Freeze => "mov",
        AsmGenericOpcode::FPExt => "fcvt.d.s",
        AsmGenericOpcode::FPTrunc => "fcvt.s.d",
        AsmGenericOpcode::FPToUI | AsmGenericOpcode::FPToSI => "fcvtzs",
        AsmGenericOpcode::UIToFP | AsmGenericOpcode::SIToFP => "scvtf",
        AsmGenericOpcode::ExtractValue => "ldr.extract",
        AsmGenericOpcode::InsertValue => "str.insert",
        AsmGenericOpcode::Call | AsmGenericOpcode::IntrinsicCall => "bl",
        AsmGenericOpcode::Phi => "phi.copy",
        AsmGenericOpcode::Select => "csel",
        AsmGenericOpcode::InlineAsm => "inlineasm",
        AsmGenericOpcode::LandingPad => "landingpad",
        AsmGenericOpcode::Syscall => "svc",
        AsmGenericOpcode::Splat => "dup",
        AsmGenericOpcode::BuildVector => "build_vector",
        AsmGenericOpcode::ExtractLane => "extract_lane",
        AsmGenericOpcode::InsertLane => "insert_lane",
        AsmGenericOpcode::ZipLow => "zip1",
        AsmGenericOpcode::SymbolAddress => match find_attr(operands, |attr| {
            matches!(attr, AsmAttr::SymbolAddressKind(_))
        }) {
            Some(AsmAttr::SymbolAddressKind(fp_core::asmir::AsmSymbolAddressKind::Got)) => {
                "symaddr.got"
            }
            _ => "symaddr.direct",
        },
        AsmGenericOpcode::SysOp => "sysop",
        AsmGenericOpcode::Unreachable => "brk",
    }
}

fn aarch64_condition_for_generic(opcode: &AsmGenericOpcode) -> Option<Aarch64ConditionCode> {
    match opcode {
        AsmGenericOpcode::Eq => Some(Aarch64ConditionCode::Eq),
        AsmGenericOpcode::Ne => Some(Aarch64ConditionCode::Ne),
        AsmGenericOpcode::Lt => Some(Aarch64ConditionCode::Lt),
        AsmGenericOpcode::Le => Some(Aarch64ConditionCode::Le),
        AsmGenericOpcode::Gt => Some(Aarch64ConditionCode::Gt),
        AsmGenericOpcode::Ge => Some(Aarch64ConditionCode::Ge),
        AsmGenericOpcode::Ult => Some(Aarch64ConditionCode::Lo),
        AsmGenericOpcode::Ule => Some(Aarch64ConditionCode::Ls),
        AsmGenericOpcode::Ugt => Some(Aarch64ConditionCode::Hi),
        AsmGenericOpcode::Uge => Some(Aarch64ConditionCode::Hs),
        AsmGenericOpcode::Select => Some(Aarch64ConditionCode::NonZero),
        _ => None,
    }
}

fn aarch64_call_target_for_instruction(
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Option<Aarch64CallTarget> {
    match &instruction.opcode {
        AsmOpcode::Generic(AsmGenericOpcode::Call) => {
            let (target, _args) = instruction.call_target_and_args()?;
            Some(aarch64_call_target_from_operand_generic(target, ctx))
        }
        AsmOpcode::Generic(AsmGenericOpcode::IntrinsicCall) => {
            let AsmAttr::Intrinsic(kind) =
                find_attr(&instruction.operands, |attr| matches!(attr, AsmAttr::Intrinsic(_)))?
            else {
                return None;
            };
            Some(Aarch64CallTarget::Symbol(Name::new(
                format!("intrinsic.{kind:?}").to_ascii_lowercase(),
            )))
        }
        _ => None,
    }
}

fn aarch64_call_target_from_operand_generic(
    operand: &AsmOperand,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64CallTarget {
    match operand {
        AsmOperand::Symbol(name) | AsmOperand::Label(name) => {
            Aarch64CallTarget::Symbol(name.clone())
        }
        AsmOperand::Register { reg, .. } => {
            Aarch64CallTarget::Register(asm_register_to_aarch64(reg, ctx))
        }
        AsmOperand::Constant(AsmConstant::FunctionRef(name, _))
        | AsmOperand::Constant(AsmConstant::GlobalRef(name, _, _)) => {
            Aarch64CallTarget::Symbol(name.clone())
        }
        _ => Aarch64CallTarget::Symbol(Name::new("indirect.call")),
    }
}

/// Aarch64 counterpart of `x86_machine_operands`; see its documentation.
fn aarch64_machine_operands(
    opcode: &AsmGenericOpcode,
    instruction: &AsmInstruction,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Vec<Aarch64Operand> {
    let operands = &instruction.operands;
    match opcode {
        AsmGenericOpcode::SysOp => Vec::new(),
        AsmGenericOpcode::Syscall => {
            let imm = match find_attr(operands, |attr| {
                matches!(attr, AsmAttr::SyscallConvention(_))
            }) {
                Some(AsmAttr::SyscallConvention(AsmSyscallConvention::DarwinAarch64)) => 0x80,
                _ => 0,
            };
            vec![Aarch64Operand::Immediate(imm)]
        }
        AsmGenericOpcode::Call => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            if let Some((target, _args)) = instruction.call_target_and_args() {
                out.push(asm_operand_to_aarch64(target, ctx));
            }
            out
        }
        AsmGenericOpcode::IntrinsicCall => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            if let Some(AsmAttr::Intrinsic(kind)) =
                find_attr(operands, |attr| matches!(attr, AsmAttr::Intrinsic(_)))
            {
                out.push(Aarch64Operand::Symbol(Name::new(
                    format!("intrinsic.{kind:?}").to_ascii_lowercase(),
                )));
            }
            for operand in operands {
                if matches!(operand, AsmOperand::Attr(_))
                    || matches!(
                        operand,
                        AsmOperand::Register {
                            access: OperandAccess::Write,
                            ..
                        }
                    )
                {
                    continue;
                }
                out.push(asm_operand_to_aarch64(operand, ctx));
            }
            out
        }
        AsmGenericOpcode::Load => {
            let mut out = Vec::new();
            let ty = result_asm_type(operands, ctx);
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            if let Some(address) = operands.get(if write_operand(operands).is_some() { 1 } else { 0 })
            {
                out.push(aarch64_address_like_operand(address, ty.as_ref(), ctx));
            }
            out
        }
        AsmGenericOpcode::Store => {
            let mut out = Vec::new();
            if let Some(value) = operands.first() {
                out.push(asm_operand_to_aarch64(value, ctx));
            }
            if let Some(address) = operands.get(1) {
                out.push(aarch64_address_like_operand(address, None, ctx));
            }
            out
        }
        AsmGenericOpcode::LandingPad => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            for operand in operands {
                match operand {
                    AsmOperand::Register {
                        access: OperandAccess::Write,
                        ..
                    } => continue,
                    AsmOperand::Attr(AsmAttr::Cleanup) => continue,
                    AsmOperand::Attr(AsmAttr::LandingPadCatch)
                    | AsmOperand::Attr(AsmAttr::LandingPadFilter(_)) => break,
                    AsmOperand::Attr(_) => continue,
                    other => {
                        out.push(asm_operand_to_aarch64(other, ctx));
                        break;
                    }
                }
            }
            out
        }
        AsmGenericOpcode::Splat => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            if let Some(value) = operands.get(1) {
                out.push(asm_operand_to_aarch64(value, ctx));
            }
            out
        }
        AsmGenericOpcode::InsertLane => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            if let Some(vector) = operands.get(1) {
                out.push(asm_operand_to_aarch64(vector, ctx));
            }
            if let Some(value) = operands.get(3) {
                out.push(asm_operand_to_aarch64(value, ctx));
            }
            if let Some(lane) = operands.get(2) {
                out.push(asm_operand_to_aarch64(lane, ctx));
            }
            out
        }
        AsmGenericOpcode::ZipLow => {
            let mut out = Vec::new();
            if let Some(dest) = write_operand(operands) {
                out.push(asm_operand_to_aarch64(dest, ctx));
            }
            for operand in operands.iter().skip(1).take(2) {
                out.push(asm_operand_to_aarch64(operand, ctx));
            }
            out
        }
        _ => operands
            .iter()
            .filter(|operand| !matches!(operand, AsmOperand::Attr(_)))
            .map(|operand| asm_operand_to_aarch64(operand, ctx))
            .collect(),
    }
}

fn aarch64_address_like_operand(
    operand: &AsmOperand,
    ty: Option<&AsmType>,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Operand {
    match operand {
        AsmOperand::Memory(_) | AsmOperand::Symbol(_) | AsmOperand::Label(_) => {
            asm_operand_to_aarch64(operand, ctx)
        }
        AsmOperand::Register { reg, .. } => Aarch64Operand::Memory(Aarch64MemoryOperand {
            base: Some(asm_register_to_aarch64(reg, ctx)),
            index: None,
            scale: 1,
            displacement: 0,
            size_bytes: ty.map(type_size_bytes),
        }),
        AsmOperand::Local(id) => Aarch64Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmOperand::StackSlot(id) => Aarch64Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        AsmOperand::Constant(AsmConstant::GlobalRef(name, _, _))
        | AsmOperand::Constant(AsmConstant::FunctionRef(name, _)) => {
            Aarch64Operand::Symbol(name.clone())
        }
        other => asm_operand_to_aarch64(other, ctx),
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
    condition: &AsmOperand,
    instructions: &[AsmInstruction],
) -> Option<Aarch64ConditionCode> {
    match condition {
        AsmOperand::Register {
            reg: reg @ AsmRegister::Virtual(_),
            ..
        } => instructions
            .iter()
            .find(|instruction| instruction.result_register() == Some(reg))
            .and_then(|instruction| comparison_code_from_opcode(&instruction.opcode))
            .map(|code| aarch64_condition_from_asm(&code)),
        other => aarch64_branch_condition(other),
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

/// Native aggregate values are represented by addresses of their storage.
/// ABI expansion is handled later by the target-specific call lowering.
fn backend_operand_type(ty: &AsmType) -> AsmType {
    match ty {
        AsmType::Struct { .. } | AsmType::Array(_, _) => AsmType::Ptr(Box::new(ty.clone())),
        _ => ty.clone(),
    }
}

fn x86_branch_condition(operand: &AsmOperand) -> Option<X86ConditionCode> {
    match operand {
        AsmOperand::Condition(condition) => Some(x86_condition_from_asm(condition)),
        _ => None,
    }
}

fn aarch64_branch_condition(operand: &AsmOperand) -> Option<Aarch64ConditionCode> {
    match operand {
        AsmOperand::Condition(condition) => Some(aarch64_condition_from_asm(condition)),
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

/// Threads a `&mut AsmFunction` through disassembly lifting so that (a)
/// machine-level virtual register ids (which live in their own id space,
/// independent of and not necessarily contiguous with canonical
/// `AsmVirtualRegId`s) get mapped to real canonical ids registered in the
/// function's virtual-register table, and (b) instructions whose machine
/// form has no explicit destination (e.g. a flags-only `cmp`) can be given
/// one via `AsmFunction::alloc_virtual_register`.
struct LiftContext<'a> {
    function: &'a mut AsmFunction,
    reg_map: HashMap<u32, AsmVirtualRegId>,
}

impl<'a> LiftContext<'a> {
    fn new(function: &'a mut AsmFunction) -> Self {
        Self {
            function,
            reg_map: HashMap::new(),
        }
    }

    /// The canonical vreg id for a machine-level virtual register id,
    /// allocating and registering a fresh one (best-effort typed from its
    /// width) the first time this id is seen.
    fn canonical_reg(&mut self, machine_id: u32, size_bits: u16) -> AsmVirtualRegId {
        if let Some(id) = self.reg_map.get(&machine_id) {
            return *id;
        }
        let id = self
            .function
            .alloc_virtual_register(type_from_bits(size_bits), AsmRegisterBank::General, size_bits);
        self.reg_map.insert(machine_id, id);
        id
    }

    /// Allocates a brand-new canonical vreg not tied to any machine-level
    /// id, for instructions that define a value with no explicit machine
    /// destination operand (e.g. a `cmp`, which only sets flags).
    fn fresh_reg(&mut self, ty: AsmType, bank: AsmRegisterBank, bits: u16) -> AsmVirtualRegId {
        self.function.alloc_virtual_register(ty, bank, bits)
    }
}

fn lift_x86_instruction(
    instruction: &X86InstructionDetail,
    id: AsmInstrId,
    ctx: &mut LiftContext,
) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(|operand| x86_operand_to_asm(operand, ctx))
        .collect::<Vec<_>>();
    semanticize_x86_detail(id, instruction, &operands, ctx)
}

fn lift_aarch64_instruction(
    instruction: &Aarch64InstructionDetail,
    id: AsmInstrId,
    ctx: &mut LiftContext,
) -> Result<AsmInstruction> {
    let operands = instruction
        .operands
        .iter()
        .map(|operand| aarch64_operand_to_asm(operand, ctx))
        .collect::<Vec<_>>();
    semanticize_aarch64_detail(id, instruction, &operands, ctx)
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

/// Machine (x86) operand -> canonical operand, used while lifting. Virtual
/// register ids are translated through `ctx` into real canonical ids (see
/// `LiftContext`); physical registers are kept as-is (a later
/// `canonicalize_physical_registers` pass turns them into virtual ones).
fn x86_operand_to_asm(operand: &X86Operand, ctx: &mut LiftContext) -> AsmOperand {
    match operand {
        X86Operand::Register { reg, access } => AsmOperand::Register {
            reg: x86_register_to_asm(reg, ctx),
            access: access.clone(),
        },
        X86Operand::Immediate(value) => AsmOperand::Immediate(*value),
        X86Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(|reg| x86_register_to_asm(reg, ctx)),
            index: mem.index.as_ref().map(|reg| x86_register_to_asm(reg, ctx)),
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

fn aarch64_operand_to_asm(operand: &Aarch64Operand, ctx: &mut LiftContext) -> AsmOperand {
    match operand {
        Aarch64Operand::Register { reg, access } => AsmOperand::Register {
            reg: aarch64_register_to_asm(reg, ctx),
            access: access.clone(),
        },
        Aarch64Operand::Immediate(value) => AsmOperand::Immediate(*value),
        Aarch64Operand::Memory(mem) => AsmOperand::Memory(AsmMemoryOperand {
            base: mem.base.as_ref().map(|reg| aarch64_register_to_asm(reg, ctx)),
            index: mem
                .index
                .as_ref()
                .map(|reg| aarch64_register_to_asm(reg, ctx)),
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

fn x86_register_to_asm(register: &X86Register, ctx: &mut LiftContext) -> AsmRegister {
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
        X86Register::Virtual { id, size_bits } => {
            AsmRegister::Virtual(ctx.canonical_reg(*id, *size_bits))
        }
    }
}

fn aarch64_register_to_asm(register: &Aarch64Register, ctx: &mut LiftContext) -> AsmRegister {
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
        Aarch64Register::Virtual { id, size_bits } => {
            AsmRegister::Virtual(ctx.canonical_reg(*id, *size_bits))
        }
    }
}

/// Canonical operand -> machine (x86) operand, used while lowering.
/// Virtual register widths come from `ctx.register_type` (the function's
/// virtual-register table), since `AsmRegister::Virtual` itself carries
/// only a bare id.
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
        AsmOperand::Local(id) => X86Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmOperand::StackSlot(id) => X86Operand::Symbol(Name::new(format!("frame.slot.{id}"))),
        AsmOperand::Constant(constant) => x86_constant_operand(constant),
        AsmOperand::Condition(condition) => {
            X86Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        // `SysOp`/`Attr` carry no direct machine-operand representation;
        // callers that need their contents (syscall lowering, alignment,
        // calling convention, ...) read them straight off the canonical
        // operand list rather than through this conversion.
        AsmOperand::SysOp(_) => X86Operand::Symbol(Name::new("sysop")),
        AsmOperand::Attr(_) => X86Operand::Symbol(Name::new("attr")),
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
        AsmOperand::Local(id) => Aarch64Operand::Symbol(Name::new(format!("frame.local.{id}"))),
        AsmOperand::StackSlot(id) => {
            Aarch64Operand::Symbol(Name::new(format!("frame.slot.{id}")))
        }
        AsmOperand::Constant(constant) => aarch64_constant_operand(constant),
        AsmOperand::Condition(condition) => {
            Aarch64Operand::Symbol(Name::new(format!("cc.{}", asm_condition_suffix(condition))))
        }
        AsmOperand::SysOp(_) => Aarch64Operand::Symbol(Name::new("sysop")),
        AsmOperand::Attr(_) => Aarch64Operand::Symbol(Name::new("attr")),
    }
}

fn asm_register_to_x86(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> X86Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_x86(physical, ctx),
        AsmRegister::Virtual(id) => X86Register::Virtual {
            id: *id,
            size_bits: type_size_bits(&ctx.register_type(*id)),
        },
    }
}

fn asm_register_to_aarch64(
    register: &AsmRegister,
    ctx: &mut PhysicalRegisterLoweringContext,
) -> Aarch64Register {
    match register {
        AsmRegister::Physical(physical) => map_physical_register_to_aarch64(physical, ctx),
        AsmRegister::Virtual(id) => Aarch64Register::Virtual {
            id: *id,
            size_bits: type_size_bits(&ctx.register_type(*id)),
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
                condition: AsmOperand::Condition(asm_condition_from_x86(condition)),
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
                condition: AsmOperand::Condition(asm_condition_from_aarch64(condition)),
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

/// Reconstructs a canonical `AsmInstruction` from a decoded x86 instruction
/// detail. `operands` is `instruction.operands` already converted to
/// canonical `AsmOperand`s (register ids translated via `ctx`); this
/// function reshapes them into the schema `select_instruction` builds for
/// each generic opcode, synthesizing a destination register via
/// `ctx.fresh_reg` for machine instructions that define a value with no
/// explicit destination operand (flags-only `cmp`).
fn semanticize_x86_detail(
    id: AsmInstrId,
    instruction: &X86InstructionDetail,
    operands: &[AsmOperand],
    ctx: &mut LiftContext,
) -> Result<AsmInstruction> {
    let opcode_name = x86_custom_opcode_name(instruction);
    let (base, condition) = parse_x86_custom_opcode(&opcode_name);
    match base {
        "syscall" => Ok(x86_syscall_instruction(id)),
        "add" => binary_instruction(id, AsmGenericOpcode::Add, operands),
        "sub" => binary_instruction(id, AsmGenericOpcode::Sub, operands),
        "imul" | "mulss" | "mulsd" => binary_instruction(id, AsmGenericOpcode::Mul, operands),
        "idiv" | "divss" | "divsd" => binary_instruction(id, AsmGenericOpcode::Div, operands),
        "and" => binary_instruction(id, AsmGenericOpcode::And, operands),
        "or" => binary_instruction(id, AsmGenericOpcode::Or, operands),
        "xor" => binary_instruction(id, AsmGenericOpcode::Xor, operands),
        "shl" => binary_instruction(id, AsmGenericOpcode::Shl, operands),
        "sar" => binary_instruction(id, AsmGenericOpcode::Shr, operands),
        "not" => unary_instruction_lifted(id, AsmGenericOpcode::Not, operands),
        "cmp" => compare_instruction(id, condition, operands, ctx),
        "mov" => x86_mov_instruction(id, operands),
        "lea" | "lea.frame" => address_instruction_lifted(id, operands),
        "call" => call_instruction_lifted(id, operands),
        "cmov" => select_instruction_lifted(id, operands),
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported x86 opcode for transpile: {base}"
        ))),
    }
}

/// Aarch64 counterpart of `semanticize_x86_detail`.
fn semanticize_aarch64_detail(
    id: AsmInstrId,
    instruction: &Aarch64InstructionDetail,
    operands: &[AsmOperand],
    ctx: &mut LiftContext,
) -> Result<AsmInstruction> {
    let opcode_name = aarch64_custom_opcode_name(instruction);
    let (base, condition) = parse_aarch64_custom_opcode(&opcode_name);
    match base {
        "svc" => {
            let imm = operands
                .iter()
                .find_map(|operand| match operand {
                    AsmOperand::Immediate(value) => Some(*value),
                    _ => None,
                })
                .unwrap_or(0);
            Ok(aarch64_syscall_instruction(id, imm))
        }
        "add" => binary_instruction(id, AsmGenericOpcode::Add, operands),
        "sub" => binary_instruction(id, AsmGenericOpcode::Sub, operands),
        "mul" | "fmul.s" | "fmul.d" => binary_instruction(id, AsmGenericOpcode::Mul, operands),
        "sdiv" | "fdiv.s" | "fdiv.d" => binary_instruction(id, AsmGenericOpcode::Div, operands),
        "and" => binary_instruction(id, AsmGenericOpcode::And, operands),
        "orr" => binary_instruction(id, AsmGenericOpcode::Or, operands),
        "eor" => binary_instruction(id, AsmGenericOpcode::Xor, operands),
        "lsl" => binary_instruction(id, AsmGenericOpcode::Shl, operands),
        "asr" => binary_instruction(id, AsmGenericOpcode::Shr, operands),
        "mvn" => unary_instruction_lifted(id, AsmGenericOpcode::Not, operands),
        "cmp" => compare_instruction(
            id,
            condition.map(aarch64_condition_to_x86_equivalent),
            operands,
            ctx,
        ),
        "ldr" => load_instruction_lifted(id, operands),
        "str" => store_instruction_lifted(id, operands),
        "add.addr" | "add.sp" => address_instruction_lifted(id, operands),
        "bl" => call_instruction_lifted(id, operands),
        "csel" => select_instruction_lifted(id, operands),
        _ => Err(fp_core::error::Error::from(format!(
            "unsupported aarch64 opcode for transpile: {base}"
        ))),
    }
}

fn x86_physical(name: &str, size_bits: u16) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Physical(AsmPhysicalRegister {
            name: name.to_string(),
            bank: AsmRegisterBank::General,
            size_bits,
        }),
        access: OperandAccess::Read,
    }
}

fn x86_syscall_instruction(id: AsmInstrId) -> AsmInstruction {
    let mut operands = vec![
        AsmOperand::Attr(AsmAttr::SyscallConvention(AsmSyscallConvention::LinuxX86_64)),
        x86_physical("rax", 64),
    ];
    for name in ["rdi", "rsi", "rdx", "r10", "r8", "r9"] {
        operands.push(x86_physical(name, 64));
    }
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Syscall), operands)
}

fn aarch64_syscall_instruction(id: AsmInstrId, imm: i128) -> AsmInstruction {
    let convention = match imm {
        0x80 => AsmSyscallConvention::DarwinAarch64,
        _ => AsmSyscallConvention::LinuxAarch64,
    };
    let number_reg = match convention {
        AsmSyscallConvention::DarwinAarch64 => "x16",
        _ => "x8",
    };
    let mut operands = vec![
        AsmOperand::Attr(AsmAttr::SyscallConvention(convention)),
        x86_physical(number_reg, 64),
    ];
    for idx in 0..6 {
        operands.push(x86_physical(&format!("x{idx}"), 64));
    }
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Syscall), operands)
}

/// Builds a `[Write dest?, Read lhs, Read rhs]` instruction from a machine
/// instruction's already-converted operand list, preserving any existing
/// destination and dropping any operands beyond `lhs`/`rhs`.
fn binary_instruction(
    id: AsmInstrId,
    opcode: AsmGenericOpcode,
    operands: &[AsmOperand],
) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let first_read = first_read_operand_index(operands);
    let lhs = operands
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing lhs operand"))?;
    let rhs = operands
        .get(first_read + 1)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing rhs operand"))?;
    let mut out = Vec::with_capacity(3);
    out.extend(dest);
    out.push(lhs);
    out.push(rhs);
    Ok(AsmInstruction::new(id, AsmOpcode::Generic(opcode), out))
}

fn unary_instruction_lifted(
    id: AsmInstrId,
    opcode: AsmGenericOpcode,
    operands: &[AsmOperand],
) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let first_read = first_read_operand_index(operands);
    let value = operands
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing operand"))?;
    Ok(unary_instruction(id, opcode, dest, value))
}

/// Reconstructs a comparison as a canonical `Eq`/`Ne`/.../`Uge` instruction.
/// Real `cmp` instructions only set flags, so if the machine detail carries
/// no explicit destination register a fresh one is synthesized (this is
/// the "flags-only instruction" case the destination-register-allocation
/// plumbing exists for): the comparison's own result register is what
/// lets a later branch on the same condition link back to it (see
/// `relink_comparison_condition`) instead of falling back to a generic
/// compare-with-zero.
fn compare_instruction(
    id: AsmInstrId,
    condition: Option<X86ConditionCode>,
    operands: &[AsmOperand],
    ctx: &mut LiftContext,
) -> Result<AsmInstruction> {
    let first_read = first_read_operand_index(operands);
    let lhs = operands
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare lhs"))?;
    let rhs = operands
        .get(first_read + 1)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing compare rhs"))?;
    let (opcode, rhs) = match condition.unwrap_or(X86ConditionCode::NonZero) {
        X86ConditionCode::Equal => (AsmGenericOpcode::Eq, rhs),
        X86ConditionCode::NotEqual => (AsmGenericOpcode::Ne, rhs),
        X86ConditionCode::Less => (AsmGenericOpcode::Lt, rhs),
        X86ConditionCode::LessEqual => (AsmGenericOpcode::Le, rhs),
        X86ConditionCode::Greater => (AsmGenericOpcode::Gt, rhs),
        X86ConditionCode::GreaterEqual => (AsmGenericOpcode::Ge, rhs),
        X86ConditionCode::Below => (AsmGenericOpcode::Ult, rhs),
        X86ConditionCode::BelowEqual => (AsmGenericOpcode::Ule, rhs),
        X86ConditionCode::Above => (AsmGenericOpcode::Ugt, rhs),
        X86ConditionCode::AboveEqual => (AsmGenericOpcode::Uge, rhs),
        X86ConditionCode::NonZero => (
            AsmGenericOpcode::Ne,
            AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
        ),
    };
    let dest = match write_operand(operands) {
        Some(dest) => dest.clone(),
        None => AsmOperand::Register {
            reg: AsmRegister::Virtual(ctx.fresh_reg(AsmType::I1, AsmRegisterBank::General, 1)),
            access: OperandAccess::Write,
        },
    };
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(opcode),
        vec![dest, lhs, rhs],
    ))
}

fn call_instruction_lifted(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let first_read = first_read_operand_index(operands);
    let target = operands
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing call target"))?;
    let args = operands.iter().skip(first_read + 1).cloned();
    let mut out = Vec::new();
    out.extend(dest);
    out.push(AsmOperand::Attr(AsmAttr::CallingConv(
        fp_core::lir::CallingConvention::C,
    )));
    out.push(target);
    out.extend(args);
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Call),
        out,
    ))
}

fn select_instruction_lifted(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let first_read = first_read_operand_index(operands);
    let condition = operands
        .get(first_read)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing select condition"))?;
    let if_true = operands
        .get(first_read + 1)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing select if_true"))?;
    let if_false = operands
        .get(first_read + 2)
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing select if_false"))?;
    let mut out = Vec::new();
    out.extend(dest);
    out.push(condition);
    out.push(if_true);
    out.push(if_false);
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Select),
        out,
    ))
}

fn x86_mov_instruction(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    match (operands.first(), operands.get(1)) {
        (Some(AsmOperand::Register { .. }), Some(AsmOperand::Memory(_))) => {
            load_instruction_lifted(id, operands)
        }
        (Some(AsmOperand::Memory(_)), Some(_)) => store_instruction_lifted(id, operands),
        _ => unary_instruction_lifted(id, AsmGenericOpcode::Freeze, operands),
    }
}

fn load_instruction_lifted(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let address = operands
        .iter()
        .find(|operand| matches!(operand, AsmOperand::Memory(_)))
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing load memory operand"))?;
    let mut out = Vec::new();
    out.extend(dest);
    out.push(address);
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Load),
        out,
    ))
}

fn store_instruction_lifted(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    let address = operands
        .iter()
        .find(|operand| matches!(operand, AsmOperand::Memory(_)))
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing store memory operand"))?;
    let value = operands
        .iter()
        .find(|operand| !matches!(operand, AsmOperand::Memory(_)))
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing store value operand"))?;
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Store),
        vec![value, address],
    ))
}

fn address_instruction_lifted(id: AsmInstrId, operands: &[AsmOperand]) -> Result<AsmInstruction> {
    let dest = write_operand(operands).cloned();
    let ptr = operands
        .iter()
        .find(|operand| {
            matches!(
                operand,
                AsmOperand::Memory(_)
                    | AsmOperand::Register {
                        access: OperandAccess::Read,
                        ..
                    }
                    | AsmOperand::Symbol(_)
                    | AsmOperand::Label(_)
            )
        })
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from("missing address operand"))?;
    let mut out = Vec::new();
    out.extend(dest);
    out.push(ptr);
    Ok(AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::GetElementPtr),
        out,
    ))
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
        AsmConditionCode, AsmConstant, AsmGenericOpcode, AsmOpcode, AsmOperand, AsmRegister,
        AsmTerminator, OperandAccess,
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
        assert_eq!(
            program.functions[0].basic_blocks[0].instructions[0].opcode,
            AsmOpcode::Generic(AsmGenericOpcode::Freeze)
        );
        assert!(matches!(
            program.functions[0].basic_blocks[0].terminator,
            AsmTerminator::Return(Some(AsmOperand::Register {
                reg: AsmRegister::Virtual(_),
                access: OperandAccess::Read,
            }))
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
                    instructions: vec![
                        LirInstruction {
                            id: 1,
                            kind: LirInstructionKind::Freeze(i32_value(10)),
                            result: Some(LirRegister {
                                id: 1,
                                ty: LirType::I32,
                            }),
                            debug_info: None,
                        },
                        LirInstruction {
                            id: 7,
                            kind: LirInstructionKind::Add(reg(1, LirType::I32), i32_value(4)),
                            result: Some(LirRegister {
                                id: 7,
                                ty: LirType::I32,
                            }),
                            debug_info: None,
                        },
                    ],
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
        let inst = &program.functions[0].basic_blocks[0].instructions[1];

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
        assert!(matches!(
            &inst.operands[2],
            AsmOperand::Constant(AsmConstant::UInt(4, _))
        ));

        let x86 = lower_to_x86_64(&program);
        let inst = &x86.functions[0].blocks[0].instructions[1];
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
                        instructions: vec![LirInstruction {
                            id: 1,
                            kind: LirInstructionKind::Freeze(LirValue::constant(
                                LirConstant::integer(LirType::I1, LirInteger::I1(true)).unwrap(),
                            )),
                            result: Some(LirRegister {
                                id: 1,
                                ty: LirType::I1,
                            }),
                            debug_info: None,
                        }],
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
        // Virtual register ids are reallocated through the canonical
        // function's own id space during lifting (rather than preserved
        // verbatim from the raw machine encoding), so compare shape rather
        // than exact ids; the immediate is unaffected and round-trips
        // exactly.
        assert!(matches!(
            lowered_inst.operands[1],
            X86Operand::Register {
                reg: X86Register::Virtual { .. },
                access: OperandAccess::Read,
            }
        ));
        assert_eq!(lowered_inst.operands[2], original_inst.operands[2]);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmOperand::Condition(AsmConditionCode::Ne),
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
        // See the x86 roundtrip test above: virtual register ids are
        // reallocated during lifting, so compare shape rather than exact
        // ids; the immediate round-trips exactly.
        assert!(matches!(
            lowered_inst.operands[1],
            Aarch64Operand::Register {
                reg: Aarch64Register::Virtual { .. },
                access: OperandAccess::Read,
            }
        ));
        assert_eq!(lowered_inst.operands[2], original_inst.operands[2]);
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmOperand::Condition(AsmConditionCode::Ge),
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
        // The block's sole instruction is the `cmp`; the branch condition
        // should be linked back to reference its own (synthesized) result
        // register rather than falling back to a bare condition code, so
        // `lower_to_x86_64` can regenerate the exact `cmp`/`jcc` pair.
        let cmp_result = asmir.functions[0].basic_blocks[0].instructions[0]
            .result_register()
            .cloned()
            .expect("cmp must define a result register in canonical AsmIR");
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmOperand::Register { reg, access: OperandAccess::Read },
                if_true: 1,
                if_false: 2,
            } if *reg == cmp_result
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
        let cmp_result = asmir.functions[0].basic_blocks[0].instructions[0]
            .result_register()
            .cloned()
            .expect("cmp must define a result register in canonical AsmIR");
        assert!(matches!(
            &asmir.functions[0].basic_blocks[0].terminator,
            AsmTerminator::CondBr {
                condition: AsmOperand::Register { reg, access: OperandAccess::Read },
                if_true: 1,
                if_false: 2,
            } if *reg == cmp_result
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
