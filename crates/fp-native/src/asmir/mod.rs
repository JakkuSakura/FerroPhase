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
    AsmAddressValue, AsmArchitecture, AsmBlock, AsmConditionCode, AsmConstant, AsmEndianness,
    AsmFunction, AsmFunctionSignature, AsmGenericOpcode, AsmGlobal, AsmInstruction,
    AsmInstructionKind, AsmIntrinsicKind, AsmLandingPadClause, AsmMemoryOperand, AsmObjectFormat,
    AsmOpcode, AsmOperand, AsmPhysicalRegister, AsmProgram, AsmRegister, AsmRegisterBank,
    AsmSection, AsmSectionFlag, AsmSectionKind, AsmSyscallConvention, AsmTarget, AsmTerminator,
    AsmType, AsmTypeDefinition, AsmValue, OperandAccess,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{
    Linkage, LirBlob, LirConstant, LirConstantAggregate, LirConstantData, LirConstantExpr,
    LirConstantKind, LirFloat, LirInstructionKind, LirInteger, LirIntrinsicKind, LirTerminator,
    LirValue, LirValueKind, Name, Visibility,
};
use std::collections::HashMap;

mod lir_mapping;
use lir_mapping::*;
mod aarch64_mapping;
use aarch64_mapping::*;
mod x86_mapping;
use x86_mapping::*;
mod lifting;
use lifting::*;
mod normalization;
pub use normalization::normalize_for_target;
use normalization::*;
mod operands;
pub(super) use operands::*;

pub fn select_program(
    lir_program: &LirBlob,
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
                    ty: instruction
                        .result
                        .as_ref()
                        .map(|result| result.ty.clone())
                        .unwrap_or(AsmType::Void),
                    opcode: AsmOpcode::Generic(generic_opcode(&map_instruction_kind(
                        &instruction.kind,
                    ))),
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
                terminator_encoding: None,
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
                    merged_register_types(program, function),
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

    // Every synthetic id just assigned above came from a real physical
    // register whose size/bank (embedded right in `map`'s key) is known
    // precisely — record the corresponding type now, while that
    // information still exists, so later passes that only know instruction
    // ids (never these) can still resolve a register operand's type by id
    // instead of finding no entry at all.
    program.physical_register_types.extend(map.into_iter().map(
        |((_name, size_bits, bank_id), id)| {
            (
                id,
                asm_type_for_register_identity(size_bits, bank_from_id(bank_id)),
            )
        },
    ));
}

/// A virtual register referenced directly in source syntax (e.g. assembly
/// text spelling a register as `v1:64`) carries its own known `size_bits`/
/// `bank` right on the `AsmRegister::Virtual` operand itself — but if
/// nothing in the function ever *produces* that id as an instruction
/// result (a live-in/parameter-like register, never defined locally), it
/// has no entry in `build_operand_type_map` either. Scan every operand
/// once and record a type for any such id, so later passes that only know
/// instruction ids still resolve it instead of finding nothing at all.
/// `.or_insert_with` so this never overrides a real, more precise
/// instruction-result type recorded elsewhere.
fn record_source_virtual_register_types(program: &mut AsmProgram) {
    let mut discovered = HashMap::new();
    for function in &program.functions {
        for block in &function.basic_blocks {
            for instruction in &block.instructions {
                for operand in &instruction.operands {
                    collect_operand_register_type(operand, &mut discovered);
                }
                for reg in instruction
                    .implicit_uses
                    .iter()
                    .chain(instruction.implicit_defs.iter())
                {
                    collect_register_type(reg, &mut discovered);
                }
            }
            collect_terminator_register_types(&block.terminator, &mut discovered);
        }
    }
    for (id, ty) in discovered {
        program.physical_register_types.entry(id).or_insert(ty);
    }
}

fn collect_terminator_register_types(
    terminator: &AsmTerminator,
    types: &mut HashMap<u32, AsmType>,
) {
    let mut values = Vec::new();
    match terminator {
        AsmTerminator::Return(value) => {
            if let Some(value) = value {
                values.push(value);
            }
        }
        AsmTerminator::Resume(value) => values.push(value),
        AsmTerminator::CondBr { condition, .. }
        | AsmTerminator::IndirectBr {
            address: condition, ..
        } => values.push(condition),
        AsmTerminator::Switch { value, .. } => values.push(value),
        AsmTerminator::Invoke { function, args, .. } => {
            values.push(function);
            values.extend(args);
        }
        AsmTerminator::CleanupRet { cleanup_pad, .. }
        | AsmTerminator::CatchRet {
            catch_pad: cleanup_pad,
            ..
        } => values.push(cleanup_pad),
        AsmTerminator::CatchSwitch { parent_pad, .. } => {
            if let Some(value) = parent_pad {
                values.push(value);
            }
        }
        AsmTerminator::Br(_) | AsmTerminator::Unreachable => {}
    }
    for value in values {
        collect_value_register_types(value, types);
    }
}

fn collect_value_register_types(value: &AsmValue, types: &mut HashMap<u32, AsmType>) {
    match value {
        AsmValue::Register(id) => {
            types.entry(*id).or_insert(AsmType::I64);
        }
        AsmValue::Address(address) => {
            if let Some(base) = &address.base {
                collect_value_register_types(base, types);
            }
            if let Some(index) = &address.index {
                collect_value_register_types(index, types);
            }
            if let Some(segment) = &address.segment {
                collect_value_register_types(segment, types);
            }
        }
        AsmValue::Comparison(comparison) => {
            collect_value_register_types(&comparison.lhs, types);
            collect_value_register_types(&comparison.rhs, types);
        }
        _ => {}
    }
}

fn collect_operand_register_type(operand: &AsmOperand, types: &mut HashMap<u32, AsmType>) {
    match operand {
        AsmOperand::Register { reg, .. } | AsmOperand::Predicate { reg, .. } => {
            collect_register_type(reg, types);
        }
        AsmOperand::Memory(memory) => {
            for reg in [&memory.base, &memory.index, &memory.segment]
                .into_iter()
                .filter_map(|reg| reg.as_ref())
            {
                collect_register_type(reg, types);
            }
        }
        _ => {}
    }
}

fn collect_register_type(reg: &AsmRegister, types: &mut HashMap<u32, AsmType>) {
    if let AsmRegister::Virtual {
        id,
        size_bits,
        bank,
    } = reg
    {
        types
            .entry(*id)
            .or_insert_with(|| asm_type_for_register_identity(*size_bits, bank.clone()));
    }
}

/// Inverse of `register_bank_id` for the purpose of reconstructing a
/// plausible `AsmType` below — `register_bank(ty: &AsmType)` (the direction
/// the rest of this file already relies on) only ever distinguishes
/// Float/Vector from everything else, so collapsing `Predicate`/`Special`/
/// `Custom` back to `General` here matches that existing limitation rather
/// than introducing a new one: no `AsmType` in this codebase round-trips
/// through those bank variants regardless.
fn bank_from_id(bank_id: u8) -> AsmRegisterBank {
    match bank_id {
        1 => AsmRegisterBank::Float,
        2 => AsmRegisterBank::Vector,
        _ => AsmRegisterBank::General,
    }
}

/// The `AsmType` a physical register of this size/bank should be treated as
/// by code that resolves a register operand's type from an `AsmType` (via
/// `register_bank`/`type_size_bits`) — chosen so it round-trips back to the
/// same bank and size.
fn asm_type_for_register_identity(size_bits: u16, bank: AsmRegisterBank) -> AsmType {
    match bank {
        AsmRegisterBank::Float => match size_bits {
            32 => AsmType::F32,
            64 => AsmType::F64,
            other => integer_type_for_size_bits(other),
        },
        AsmRegisterBank::Vector => {
            // Element type/count aren't recoverable from bank+size alone,
            // but `register_bank`/`type_size_bits` (the only consumers)
            // only need this to report bank=Vector and total `size_bits`
            // wide — any byte-element vector of the matching count does.
            AsmType::Vector(Box::new(AsmType::I8), u32::from(size_bits.div_ceil(8)))
        }
        AsmRegisterBank::General | AsmRegisterBank::Predicate | AsmRegisterBank::Special => {
            integer_type_for_size_bits(size_bits)
        }
        AsmRegisterBank::Custom(_) => integer_type_for_size_bits(size_bits),
    }
}

fn integer_type_for_size_bits(size_bits: u16) -> AsmType {
    match size_bits {
        1 => AsmType::I1,
        8 => AsmType::I8,
        16 => AsmType::I16,
        32 => AsmType::I32,
        64 => AsmType::I64,
        128 => AsmType::I128,
        other => AsmType::Integer(u32::from(other)),
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
        physical_register_types: HashMap::new(),
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
    record_source_virtual_register_types(&mut lifted);
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
        physical_register_types: HashMap::new(),
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
    record_source_virtual_register_types(&mut lifted);
    Ok(lifted)
}

pub(super) fn build_operand_type_map(function: &AsmFunction) -> HashMap<u32, AsmType> {
    function
        .basic_blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instruction| {
            (!matches!(instruction.ty, AsmType::Void))
                .then(|| (instruction.id, instruction.ty.clone()))
        })
        .collect()
}

/// `build_operand_type_map(function)` alone only ever has entries for real
/// instruction ids — it knows nothing about the synthetic ids
/// `canonicalize_physical_registers` assigns when lowering a raw physical
/// register reference, since those never correspond to any instruction.
/// Every caller that resolves a register operand's type by id needs both
/// merged, or it panics on any register that started out physical.
pub(crate) fn merged_register_types(
    program: &AsmProgram,
    function: &AsmFunction,
) -> HashMap<u32, AsmType> {
    // Reconstructed types go first so a real instruction-result type (more
    // precise than a size/bank-derived guess) wins if an id somehow ended
    // up in both.
    let mut types = program.physical_register_types.clone();
    types.extend(build_operand_type_map(function));
    types
}

pub(super) fn normalize_program_for_x86_64(program: &mut AsmProgram) {
    normalize_program_generic(program);
}

pub(super) fn normalize_program_for_aarch64(program: &mut AsmProgram) {
    normalize_program_generic(program);
}

pub(super) fn relink_comparison_condition(
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

pub(super) fn last_comparison_instruction(
    instructions: &[AsmInstruction],
) -> Option<(u32, AsmConditionCode)> {
    instructions.iter().rev().find_map(|instruction| {
        comparison_code_from_kind(&instruction.kind).map(|code| (instruction.id, code))
    })
}

pub(super) fn comparison_code_from_kind(kind: &AsmInstructionKind) -> Option<AsmConditionCode> {
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

pub(super) fn x86_detail_from_instruction(
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

pub(super) fn aarch64_detail_from_instruction(
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

pub(super) fn x86_operands_from_asm(operands: &[AsmOperand]) -> Option<Vec<X86Operand>> {
    operands.iter().map(x86_operand_from_asm).collect()
}

pub(super) fn mapped_x86_write_operand(
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

pub(super) fn x86_operand_from_asm(operand: &AsmOperand) -> Option<X86Operand> {
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

pub(super) fn x86_memory_from_asm(memory: &AsmMemoryOperand) -> Option<X86MemoryOperand> {
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

pub(super) fn x86_register_from_asm(register: &AsmRegister) -> Option<X86Register> {
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

pub(super) fn aarch64_operands_from_asm(operands: &[AsmOperand]) -> Option<Vec<Aarch64Operand>> {
    operands.iter().map(aarch64_operand_from_asm).collect()
}

pub(super) fn mapped_aarch64_write_operand(
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

pub(super) fn aarch64_operand_from_asm(operand: &AsmOperand) -> Option<Aarch64Operand> {
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

pub(super) fn aarch64_memory_from_asm(memory: &AsmMemoryOperand) -> Option<Aarch64MemoryOperand> {
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

pub(super) fn aarch64_register_from_asm(register: &AsmRegister) -> Option<Aarch64Register> {
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

pub(super) fn is_x86_physical_register_name(name: &str) -> bool {
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

pub(super) fn is_aarch64_physical_register_name(name: &str) -> bool {
    matches!(name.chars().next(), Some('x' | 'w' | 's' | 'd' | 'q'))
}

pub(super) fn x86_detail_from_custom(
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

pub(super) fn aarch64_detail_from_custom(
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
                    merged_register_types(program, function),
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
pub(super) struct PhysicalRegisterLoweringContext {
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

mod tests;
