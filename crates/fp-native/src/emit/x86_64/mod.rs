use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmBlockId as BasicBlockId, AsmConditionCode, AsmConstant,
    AsmFunction, AsmFunctionSignature, AsmInstructionKind, AsmIntrinsicKind, AsmProgram,
    AsmSyscallConvention, AsmTerminator, AsmType, AsmValue,
};
use fp_core::container::ContainerKind;
use fp_core::error::{Error, Result};
use fp_core::lir::{LirDataLayout, LirType};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

mod instructions;
use instructions::*;
mod control_flow;
use control_flow::*;
mod arguments;
mod blocks;
mod encoding;
use arguments::*;
use blocks::*;
use encoding::*;
mod memory;
use memory::*;
mod intrinsics;
use intrinsics::*;
mod calls;
use calls::*;
mod aggregate_values;
use aggregate_values::*;
mod aggregate_constants;
use aggregate_constants::*;
mod aggregate_memory;
use aggregate_memory::*;
mod addressing;
use addressing::*;
mod floating_point;
use floating_point::*;
mod globals;
use globals::*;
mod abi;
use abi::*;

const X86_CANON_NOP: [u8; 1] = [0x90];
const X86_CANON_RET: [u8; 1] = [0xC3];
const X86_CANON_SYSCALL: [u8; 2] = [0x0F, 0x05];

fn annotation_value<'a>(
    annotations: &'a [fp_core::asmir::AsmAnnotation],
    key: &str,
) -> Option<&'a str> {
    annotations
        .iter()
        .find(|annotation| annotation.key == key)
        .map(|annotation| annotation.value.as_str())
}

fn encode_x86_nop_sequence(len: usize) -> Vec<u8> {
    // Use standard Intel multi-byte NOP encodings.
    // These are deterministic and stable, and for compiler-produced code often
    // match the source encoding for padding regions.
    const NOP_2: [u8; 2] = [0x66, 0x90];
    const NOP_3: [u8; 3] = [0x0F, 0x1F, 0x00];
    const NOP_4: [u8; 4] = [0x0F, 0x1F, 0x40, 0x00];
    const NOP_5: [u8; 5] = [0x0F, 0x1F, 0x44, 0x00, 0x00];
    const NOP_6: [u8; 6] = [0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00];
    const NOP_7: [u8; 7] = [0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00];
    const NOP_8: [u8; 8] = [0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00];
    const NOP_9: [u8; 9] = [0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00];

    let mut remaining = len;
    let mut out = Vec::new();
    while remaining > 0 {
        match remaining {
            1 => {
                out.extend_from_slice(X86_CANON_NOP.as_slice());
                remaining = 0;
            }
            2 => {
                out.extend_from_slice(&NOP_2);
                remaining = 0;
            }
            3 => {
                out.extend_from_slice(&NOP_3);
                remaining = 0;
            }
            4 => {
                out.extend_from_slice(&NOP_4);
                remaining = 0;
            }
            5 => {
                out.extend_from_slice(&NOP_5);
                remaining = 0;
            }
            6 => {
                out.extend_from_slice(&NOP_6);
                remaining = 0;
            }
            7 => {
                out.extend_from_slice(&NOP_7);
                remaining = 0;
            }
            8 => {
                out.extend_from_slice(&NOP_8);
                remaining = 0;
            }
            9 => {
                out.extend_from_slice(&NOP_9);
                remaining = 0;
            }
            _ => {
                out.extend_from_slice(&NOP_9);
                remaining -= 9;
            }
        }
    }
    out
}

fn encode_x86_addsub_imm(
    dst_gpr: u8,
    imm: i64,
    imm_width_bits: u16,
    subopcode: u8,
) -> Option<Vec<u8>> {
    // Encodes: (REX.W) (81/83) /subopcode r/m64, imm{32|8}
    // This is sufficient for our current preserved subset.
    let rex = 0x48 | if dst_gpr >= 8 { 0x01 } else { 0x00 };
    let rm = dst_gpr & 7;
    let modrm = (0b11 << 6) | ((subopcode & 0b111) << 3) | rm;
    match imm_width_bits {
        8 => {
            let imm8 = i8::try_from(imm)
                .map_err(|e| {
                    eprintln!("[fp-native] preserved-instruction immediate error: {e}");
                    e
                })
                .ok()?;
            Some(vec![rex, 0x83, modrm, imm8 as u8])
        }
        32 => {
            let imm32 = i32::try_from(imm)
                .map_err(|e| {
                    eprintln!("[fp-native] preserved-instruction immediate error: {e}");
                    e
                })
                .ok()?;
            let mut out = vec![rex, 0x81, modrm];
            out.extend_from_slice(&imm32.to_le_bytes());
            Some(out)
        }
        _ => None,
    }
}

fn is_synthesized_instruction(inst: &fp_core::asmir::AsmInstruction) -> bool {
    inst.annotations
        .iter()
        .any(|annotation| annotation.key == "fp.synthesized")
}

#[allow(dead_code)]
fn terminator_encoding_matches_kind(block: &AsmBlock) -> bool {
    let Some(encoding) = &block.terminator_encoding else {
        return false;
    };

    match &block.terminator {
        AsmTerminator::Return(_) => encoding.as_slice() == X86_CANON_RET,
        _ => false,
    }
}

fn collect_preserved_single_block_bytes(
    _program: &AsmProgram,
    func: &AsmFunction,
) -> Option<Vec<u8>> {
    if func.basic_blocks.len() != 1 {
        return None;
    }
    let block = func.basic_blocks.first()?;
    let terminator_encoding: &[u8] = match &block.terminator {
        AsmTerminator::Return(_) => X86_CANON_RET.as_slice(),
        _ => return None,
    };

    let mut out = Vec::new();
    for inst in &block.instructions {
        if is_synthesized_instruction(inst) {
            continue;
        }

        match inst.kind {
            AsmInstructionKind::Nop => {
                let Some(len) = annotation_value(&inst.annotations, "fp.preserve.x86_64.nop_len")
                    .and_then(|value| {
                        value
                            .parse()
                            .map_err(|e| {
                                eprintln!("[fp-native] preserved-instruction parse error: {e}");
                                e
                            })
                            .ok()
                    })
                else {
                    return None;
                };
                out.extend_from_slice(&encode_x86_nop_sequence(len));
            }
            AsmInstructionKind::Syscall { .. } => {
                out.extend_from_slice(X86_CANON_SYSCALL.as_slice());
            }
            AsmInstructionKind::Add(_, _) | AsmInstructionKind::Sub(_, _) => {
                let dst = annotation_value(&inst.annotations, "fp.preserve.x86_64.dst_gpr")
                    .and_then(|value| {
                        value
                            .parse()
                            .map_err(|e| {
                                eprintln!("[fp-native] preserved-instruction parse error: {e}");
                                e
                            })
                            .ok()
                    });
                let imm_width_bits =
                    annotation_value(&inst.annotations, "fp.preserve.x86_64.imm_width_bits")
                        .and_then(|value| {
                            value
                                .parse()
                                .map_err(|e| {
                                    eprintln!("[fp-native] preserved-instruction parse error: {e}");
                                    e
                                })
                                .ok()
                        });

                if let (Some(dst), Some(imm_width_bits)) = (dst, imm_width_bits) {
                    let (subopcode, imm) = match &inst.kind {
                        AsmInstructionKind::Add(_, rhs) => (0, rhs),
                        AsmInstructionKind::Sub(_, rhs) => (5, rhs),
                        _ => return None,
                    };
                    let imm = match imm {
                        AsmValue::Constant(AsmConstant::Int(value, _)) => *value,
                        _ => return None,
                    };
                    let bytes = encode_x86_addsub_imm(dst, imm, imm_width_bits, subopcode)?;
                    out.extend_from_slice(&bytes);
                    continue;
                }

                return None;
            }
            _ => return None,
        }
    }
    out.extend_from_slice(terminator_encoding);
    Some(out)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    R8,
    R9,
    R10,
    R11,
    Rbp,
    Rsp,
}

fn emit_punpckldq_xmm_xmm(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x62);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_syscall(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    convention: AsmSyscallConvention,
    number: &AsmValue,
    args: &[AsmValue],
    ret_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    match (format, convention) {
        (TargetFormat::Elf, AsmSyscallConvention::LinuxX86_64)
        | (TargetFormat::MachO, AsmSyscallConvention::DarwinX86_64) => {}
        (TargetFormat::Coff, _) => {
            return Err(Error::from(
                "syscall emission is not supported for COFF targets",
            ));
        }
        _ => {
            return Err(Error::from(
                "syscall convention does not match output target",
            ));
        }
    }

    load_value(asm, layout, number, Reg::Rax, reg_types, local_types)?;

    for (idx, arg) in args.iter().take(SYSCALL_ARGS.len()).enumerate() {
        load_value(asm, layout, arg, SYSCALL_ARGS[idx], reg_types, local_types)?;
    }

    asm.extend(&[0x0F, 0x05]);

    if !matches!(ret_ty, AsmType::Void) {
        store_vreg(asm, layout, dst_id, Reg::Rax)?;
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FReg {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
}

impl FReg {
    fn id(self) -> u8 {
        match self {
            FReg::Xmm0 => 0,
            FReg::Xmm1 => 1,
            FReg::Xmm2 => 2,
            FReg::Xmm3 => 3,
            FReg::Xmm4 => 4,
            FReg::Xmm5 => 5,
            FReg::Xmm6 => 6,
            FReg::Xmm7 => 7,
        }
    }
}

impl Reg {
    fn id(self) -> u8 {
        match self {
            Reg::Rax => 0,
            Reg::Rcx => 1,
            Reg::Rdx => 2,
            Reg::Rdi => 7,
            Reg::Rsi => 6,
            Reg::R8 => 8,
            Reg::R9 => 9,
            Reg::R10 => 10,
            Reg::R11 => 11,
            Reg::Rsp => 4,
            Reg::Rbp => 5,
        }
    }
}

struct FrameLayout {
    data_layout: LirDataLayout,
    vreg_offsets: HashMap<u32, i32>,
    slot_offsets: HashMap<u32, i32>,
    local_offsets: HashMap<u32, i32>,
    agg_offsets: HashMap<u32, i32>,
    alloca_offsets: HashMap<u32, i32>,
    sret_offset: Option<i32>,
    aggregate_scratch_offset: Option<i32>,
    outgoing_size: i32,
    shadow_space: i32,
    frame_size: i32,
}

fn build_frame_layout(
    func: &AsmFunction,
    format: TargetFormat,
    reg_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut max_aggregate_scratch = 0i32;
    let mut has_calls = false;
    let mut alloca_info = Vec::new();
    let local_types = build_local_types(func);

    // Include live-in and synthetic virtual registers, which do not have a
    // defining instruction but still require spill slots when referenced by
    // lifted assembly operands.
    vreg_ids.extend(reg_types.keys().copied());

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let AsmInstructionKind::Call { args, .. } = &inst.kind {
                has_calls = true;
                let mut count = 0usize;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types, data_layout)?;
                }
                max_call_args = max_call_args.max(count);
                max_aggregate_scratch = max_aggregate_scratch.max(aggregate_constant_scratch_size(
                    args,
                    reg_types,
                    &local_types,
                    data_layout,
                )?);
            } else if let AsmInstructionKind::IntrinsicCall { kind, args, .. } = &inst.kind {
                has_calls = true;
                let fixed = if matches!(kind, AsmIntrinsicKind::Format) {
                    3
                } else {
                    1
                };
                let mut count = fixed;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types, data_layout)?;
                }
                max_call_args = max_call_args.max(count);
                max_aggregate_scratch = max_aggregate_scratch.max(aggregate_constant_scratch_size(
                    args,
                    reg_types,
                    &local_types,
                    data_layout,
                )?);
            } else if matches!(
                inst.kind,
                AsmInstructionKind::Mul(_, _)
                    | AsmInstructionKind::Div(_, _)
                    | AsmInstructionKind::Rem(_, _)
                    | AsmInstructionKind::Shl(_, _)
                    | AsmInstructionKind::Shr(_, _)
            ) {
                if matches!(inst.ty, AsmType::I128) {
                    has_calls = true;
                    let args = match inst.kind {
                        AsmInstructionKind::Shl(_, _) | AsmInstructionKind::Shr(_, _) => 3,
                        _ => 4,
                    };
                    max_call_args = max_call_args.max(args);
                }
            } else if let AsmInstructionKind::Alloca { size, alignment } = &inst.kind {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("alloca requires a concrete type"));
                }
                let AsmType::Ptr(inner) = ty else {
                    return Err(Error::from("alloca expects pointer type"));
                };
                let count = match size {
                    AsmValue::Constant(constant) => constant_to_i64(constant, data_layout)?,
                    _ => return Err(Error::from("alloca size must be constant")),
                };
                if count < 0 {
                    return Err(Error::from("alloca size must be non-negative"));
                }
                let elem_size = data_layout
                    .size_of(&inner)
                    .map_err(|error| Error::from(error.to_string()))?
                    as i64;
                let bytes = elem_size
                    .checked_mul(count)
                    .ok_or_else(|| Error::from("alloca size overflow"))?;
                let bytes = i32::try_from(bytes)
                    .map_err(|_| Error::from(format!("alloca size too large: {bytes}")))?;
                let align = (*alignment).max(1) as i32;
                alloca_info.push((inst.id, bytes, align));
            }
        }
    }

    let mut vreg_offsets = HashMap::new();
    let mut slot_offsets = HashMap::new();
    let mut local_offsets = HashMap::new();
    let mut agg_offsets = HashMap::new();
    let mut alloca_offsets = HashMap::new();
    let mut sret_offset = None;
    let mut offset = 0i32;

    for id in &vreg_ids {
        let (size, align) = vreg_slot_spec(*id, reg_types, data_layout);
        offset = align_to(offset, align);
        offset += size;
        vreg_offsets.insert(*id, -offset);
    }

    for slot in &func.stack_slots {
        let align = slot.alignment.max(1) as i32;
        let size = align8(slot.size as i32).max(8);
        let slot_align = align.max(8);
        offset = align_to(offset, slot_align);
        offset += size;
        slot_offsets.insert(slot.id, -offset);
    }

    for local in &func.locals {
        let size = align8(
            data_layout
                .size_of(&local.ty)
                .map_err(|error| Error::from(error.to_string()))? as i32,
        )
        .max(8);
        offset = align_to(offset, 8);
        offset += size;
        local_offsets.insert(local.id, -offset);
    }

    if matches!(
        abi_pass_mode(&func.signature.return_type, data_layout)?,
        AbiPassMode::Indirect
    ) {
        offset += 8;
        sret_offset = Some(-offset);
    }

    for id in &vreg_ids {
        if let Some(ty) = reg_types.get(id) {
            if is_aggregate_storage(ty, data_layout) {
                let size = align8(
                    data_layout
                        .size_of(ty)
                        .map_err(|error| Error::from(error.to_string()))?
                        as i32,
                );
                if size > 0 {
                    offset += size;
                    agg_offsets.insert(*id, -offset);
                }
            }
        }
    }

    for (id, size, align) in alloca_info {
        let size = align8(size).max(8);
        let align = align.max(8);
        offset = align_to(offset, align);
        offset += size;
        alloca_offsets.insert(id, -offset);
    }

    let aggregate_scratch_offset = if max_aggregate_scratch == 0 {
        None
    } else {
        let size = align8(max_aggregate_scratch);
        offset += size;
        Some(-offset)
    };

    let local_size = offset;
    let reg_arg_limit = match format {
        TargetFormat::Coff => 4,
        _ => 6,
    };
    let extra_stack_args = max_call_args.saturating_sub(reg_arg_limit);
    let shadow_space = if matches!(format, TargetFormat::Coff) && has_calls {
        32
    } else {
        0
    };
    let outgoing_size = shadow_space + (extra_stack_args as i32) * 8;
    let base = local_size + outgoing_size;
    // The prologue has already pushed `rbp`, leaving `rsp` 16-byte aligned.
    // Keep the alignment invariant after reserving locals and outgoing args;
    // variadic libc calls may use aligned SIMD stores even when no float is
    // present in the source-level signature.
    let frame_size = align16(base);

    Ok(FrameLayout {
        data_layout: data_layout.clone(),
        vreg_offsets,
        slot_offsets,
        local_offsets,
        agg_offsets,
        alloca_offsets,
        sret_offset,
        aggregate_scratch_offset,
        outgoing_size,
        shadow_space,
        frame_size,
    })
}

fn build_local_types(func: &AsmFunction) -> HashMap<u32, AsmType> {
    let mut map = HashMap::new();
    for local in &func.locals {
        map.insert(local.id, local.ty.clone());
    }
    map
}

fn call_arg_units(
    arg: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> Result<usize> {
    let ty = value_type(arg, reg_types, local_types)?;
    Ok(match abi_pass_mode(&ty, data_layout)? {
        AbiPassMode::Ignore => 0,
        AbiPassMode::Direct | AbiPassMode::Indirect => 1,
        AbiPassMode::Pair => 2,
    })
}

fn aggregate_constant_scratch_size(
    args: &[AsmValue],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> Result<i32> {
    let mut size = 0i32;
    for arg in args {
        let ty = value_type(arg, reg_types, local_types)?;
        if !is_aggregate_storage(&ty, data_layout)
            || !matches!(
                arg,
                AsmValue::Constant(AsmConstant::Struct(_, _) | AsmConstant::Array(_, _))
            )
        {
            continue;
        }
        let arg_size = data_layout
            .size_of(&ty)
            .map_err(|error| Error::from(error.to_string()))? as i32;
        size = size
            .checked_add(align8(arg_size))
            .ok_or_else(|| Error::from("aggregate constant scratch size overflow"))?;
    }
    Ok(size)
}

fn vreg_slot_spec(
    id: u32,
    reg_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> (i32, i32) {
    let Some(ty) = reg_types.get(&id) else {
        return (8, 8);
    };
    if is_aggregate_storage(ty, data_layout) {
        return (8, 8);
    }
    if matches!(ty, AsmType::I128) {
        let align = data_layout
            .align_of(ty)
            .expect("integer type must have alignment") as i32;
        return (16, align.max(16));
    }
    (8, 8)
}

fn build_reg_types(
    func: &AsmFunction,
    signatures: &HashMap<String, AsmFunctionSignature>,
) -> HashMap<u32, AsmType> {
    let mut map = HashMap::new();
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if !matches!(inst.ty, AsmType::Void) {
                map.insert(inst.id, inst.ty.clone());
            }
        }
    }

    let mut local_types = HashMap::new();
    for local in &func.locals {
        local_types.insert(local.id, local.ty.clone());
    }

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if let AsmInstructionKind::Call {
                function: AsmValue::Function(name),
                ..
            } = &inst.kind
            {
                if let Some(signature) = signatures.get(name) {
                    map.insert(inst.id, signature.return_type.clone());
                }
            }
            if map.contains_key(&inst.id) {
                continue;
            }
            if let AsmInstructionKind::ExtractValue { aggregate, indices } = &inst.kind {
                match value_type(aggregate, &map, &local_types) {
                    Ok(agg_ty) => match extract_value_type(&agg_ty, indices) {
                        Ok(field_ty) => {
                            map.insert(inst.id, field_ty);
                        }
                        Err(e) => {
                            eprintln!("[fp-native] x86_64 extract_value_type error: {e}");
                        }
                    },
                    Err(e) => {
                        eprintln!("[fp-native] x86_64 type inference error: {e}");
                    }
                }
            }
        }
    }
    map
}

fn align16(value: i32) -> i32 {
    ((value + 15) / 16) * 16
}

fn align8(value: i32) -> i32 {
    ((value + 7) / 8) * 8
}

fn align_to(value: i32, align: i32) -> i32 {
    if align <= 1 {
        return value;
    }
    ((value + align - 1) / align) * align
}

pub fn emit_text_from_asmir(program: &AsmProgram, format: TargetFormat) -> Result<CodegenOutput> {
    if !matches!(program.target.architecture, AsmArchitecture::X86_64) {
        return Err(Error::from("x86_64 emitter requires x86_64 AsmIR input"));
    }

    let mut func_map = build_function_map(program)?;
    let signatures: HashMap<String, AsmFunctionSignature> = program
        .functions
        .iter()
        .map(|func| (func.name.to_string(), func.signature.clone()))
        .collect();
    let needs_panic_stub = program_uses_fp_panic(program) && !func_map.contains_key("fp_panic");
    let panic_id = if needs_panic_stub {
        let id = func_map.len() as u32;
        func_map.insert("fp_panic".to_string(), id);
        Some(id)
    } else {
        None
    };
    let mut asm = Assembler::new();
    asm.entry_returns_exit = matches!(format, TargetFormat::Elf)
        && program
            .container
            .as_ref()
            .is_some_and(|container| container.kind == ContainerKind::Executable);
    let mut rodata = Vec::new();
    let mut data = Vec::new();
    let mut rodata_pool = HashMap::new();
    let mut rodata_symbols = HashMap::new();
    let mut data_symbols = HashMap::new();
    let mut global_relocs = Vec::new();
    let mut entry_offset = None;

    emit_const_globals(
        program,
        &program.data_layout,
        &mut rodata,
        &mut rodata_symbols,
        &mut data,
        &mut data_symbols,
        &mut global_relocs,
    )?;

    let defined_functions: Vec<&AsmFunction> = program
        .functions
        .iter()
        .filter(|func| !func.is_declaration)
        .collect();

    for (index, func) in defined_functions.iter().copied().enumerate() {
        asm.bind(Label::Function(index as u32));
        if entry_offset.is_none() && func.name.as_str() == "main" {
            entry_offset = Some(asm.buf.len() as u64);
        }

        if let Some(preserved) = collect_preserved_single_block_bytes(program, func) {
            let block_id = func.basic_blocks.first().map(|block| block.id).unwrap_or(0);
            asm.bind(Label::Block(index as u32, block_id));
            asm.extend(&preserved);
            continue;
        }

        let mut reg_types = build_reg_types(func, &signatures);
        let source_types = crate::asmir::merged_register_types(program, func);
        reg_types.extend(source_types);
        let layout = build_frame_layout(func, format, &reg_types, &program.data_layout)?;
        let local_types = build_local_types(func);
        asm.needs_frame = layout.frame_size > 0;
        if layout.frame_size > 0 {
            emit_prologue(&mut asm, &layout)?;
            spill_arguments(&mut asm, &layout, func, format, &local_types)?;
        }
        for block in &func.basic_blocks {
            asm.bind(Label::Block(index as u32, block.id));
            emit_block(
                &mut asm,
                block,
                format,
                &func_map,
                &signatures,
                &layout,
                &reg_types,
                &local_types,
                &func.signature.return_type,
                &mut rodata,
                &mut rodata_pool,
            )?;
        }
    }

    if let Some(id) = panic_id {
        emit_panic_stub(&mut asm, id);
    }

    // `None` when `program` has no `main` (e.g. a plain object/archive
    // transpile roundtrip) — only producing an executable or JIT-executing
    // this plan actually needs a real entrypoint; both check for that
    // explicitly at that point instead of it being required unconditionally
    // here for every plan.
    let func_offsets = asm.function_offsets();
    let mut symbols = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        if let Some(offset) = func_offsets.get(&(idx as u32)) {
            symbols.insert(func.name.to_string(), *offset);
        }
    }
    if let Some(id) = panic_id {
        if let Some(offset) = func_offsets.get(&id) {
            symbols.insert("fp_panic".to_string(), *offset);
        }
    }
    let (text, relocs) = asm.finish()?;
    Ok(CodegenOutput {
        text,
        rodata,
        data,
        relocs,
        section_relocs: global_relocs,
        symbols,
        rodata_symbols,
        data_symbols,
        entry_offset,
    })
}

fn spill_arguments(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
    format: TargetFormat,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let (arg_regs, float_regs, _) = call_abi(format);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let stack_base = if matches!(format, TargetFormat::Coff) {
        48
    } else {
        16
    };

    if let Some(offset) = layout.sret_offset {
        emit_mov_mr64(asm, Reg::Rbp, offset, arg_regs[0]);
        int_idx = 1;
    }

    for local in func.locals.iter().filter(|local| local.is_argument) {
        let ty = local_types
            .get(&local.id)
            .ok_or_else(|| Error::from("missing local type"))?;
        if matches!(ty, AsmType::Void) {
            continue;
        }
        let offset = local_offset(layout, local.id)?;
        if matches!(abi_pass_mode(ty, &layout.data_layout)?, AbiPassMode::Pair) {
            let load_unit = |asm: &mut Assembler,
                             dst: Reg,
                             int_idx: &mut usize,
                             stack_idx: &mut usize|
             -> Result<()> {
                if *int_idx < arg_regs.len() {
                    emit_mov_rr(asm, dst, arg_regs[*int_idx]);
                    *int_idx += 1;
                } else {
                    let incoming = stack_base + (*stack_idx as i32) * 8;
                    emit_mov_rm64(asm, dst, Reg::Rbp, incoming);
                    *stack_idx += 1;
                }
                Ok(())
            };
            load_unit(asm, Reg::R10, &mut int_idx, &mut stack_idx)?;
            load_unit(asm, Reg::R11, &mut int_idx, &mut stack_idx)?;
            emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
            emit_mov_mr64(asm, Reg::Rbp, offset + 8, Reg::R11);
            continue;
        }
        if is_aggregate_storage(ty, &layout.data_layout) {
            let size = size_of(ty) as i32;
            if int_idx < arg_regs.len() {
                copy_reg_to_sp(asm, arg_regs[int_idx], offset, size)?;
                int_idx += 1;
            } else {
                let incoming = stack_base + (stack_idx as i32) * 8;
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, incoming);
                copy_reg_to_sp(asm, Reg::R10, offset, size)?;
                stack_idx += 1;
            }
            continue;
        }
        if is_float_type(ty) {
            if float_idx < float_regs.len() {
                emit_movsd_m64x(asm, Reg::Rbp, offset, float_regs[float_idx], ty);
                float_idx += 1;
            } else {
                let incoming = stack_base + (stack_idx as i32) * 8;
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, incoming, ty);
                emit_movsd_m64x(asm, Reg::Rbp, offset, FReg::Xmm0, ty);
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            match size_of(ty) {
                1 => emit_mov_mr8(asm, Reg::Rbp, offset, arg_regs[int_idx]),
                2 => emit_mov_mr16(asm, Reg::Rbp, offset, arg_regs[int_idx]),
                4 => emit_mov_mr32(asm, Reg::Rbp, offset, arg_regs[int_idx]),
                _ => emit_mov_mr64(asm, Reg::Rbp, offset, arg_regs[int_idx]),
            }
            int_idx += 1;
        } else {
            let incoming = stack_base + (stack_idx as i32) * 8;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, incoming);
            match size_of(ty) {
                1 => emit_mov_mr8(asm, Reg::Rbp, offset, Reg::R10),
                2 => emit_mov_mr16(asm, Reg::Rbp, offset, Reg::R10),
                4 => emit_mov_mr32(asm, Reg::Rbp, offset, Reg::R10),
                _ => emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10),
            }
            stack_idx += 1;
        }
    }
    Ok(())
}

enum BinOp {
    Add,
    Sub,
    Mul,
}

enum BitOp {
    And,
    Or,
    Xor,
}

enum ShiftKind {
    Left,
    Right,
}

fn emit_bitwise_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BitOp,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
        load_i128_value(asm, layout, rhs, Reg::R8, Reg::R9, reg_types, local_types)?;
        match op {
            BitOp::And => {
                emit_and_rr(asm, Reg::R10, Reg::R8);
                emit_and_rr(asm, Reg::R11, Reg::R9);
            }
            BitOp::Or => {
                emit_or_rr(asm, Reg::R10, Reg::R8);
                emit_or_rr(asm, Reg::R11, Reg::R9);
            }
            BitOp::Xor => {
                emit_xor_rr(asm, Reg::R10, Reg::R8);
                emit_xor_rr(asm, Reg::R11, Reg::R9);
            }
        }
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
    match op {
        BitOp::And => emit_and_rr(asm, Reg::R10, Reg::R11),
        BitOp::Or => emit_or_rr(asm, Reg::R10, Reg::R11),
        BitOp::Xor => emit_xor_rr(asm, Reg::R10, Reg::R11),
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_not(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let ty = value_type(value, reg_types, local_types)?;
    if matches!(ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::R10,
            Reg::R11,
            reg_types,
            local_types,
        )?;
        emit_not_r64(asm, Reg::R10);
        emit_not_r64(asm, Reg::R11);
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    emit_not_r64(asm, Reg::R10);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_zext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits > dst_bits {
        return Err(Error::from("zext expects wider destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
        if src_bits < 64 {
            let mask = (1u64 << src_bits) - 1;
            emit_mov_imm64(asm, Reg::R11, mask);
            emit_and_rr(asm, Reg::R10, Reg::R11);
        }
        emit_mov_imm64(asm, Reg::R11, 0);
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if src_bits < 64 {
        let mask = if src_bits == 64 {
            u64::MAX
        } else {
            (1u64 << src_bits) - 1
        };
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let dst_bits = int_bits(dst_ty)?;
    let src_ty = value_type(value, reg_types, local_types)?;
    if matches!(src_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::R10,
            Reg::R11,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    }
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_shift(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            kind,
            reg_types,
            local_types,
            format,
        );
    }
    if !is_integer_type(&lhs_ty) {
        return Err(Error::from("shift expects integer operands"));
    }
    let bits = int_bits(&lhs_ty)?;
    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    if bits < 64 {
        let mask = (1u64 << bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }

    match rhs {
        AsmValue::Constant(constant) => {
            let imm = constant_to_i64(constant, &layout.data_layout)?;
            if imm < 0 {
                return Err(Error::from("shift amount must be non-negative"));
            }
            let shift = if bits < 64 {
                (imm as u64 % bits as u64) as u8
            } else {
                let masked = (imm as u64) & 0x3F;
                masked as u8
            };
            match kind {
                ShiftKind::Left => emit_shl_imm8(asm, Reg::R10, shift),
                ShiftKind::Right => emit_shr_imm8(asm, Reg::R10, shift),
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::Rcx, reg_types, local_types)?;
            if bits < 64 {
                emit_and_ri32(asm, Reg::Rcx, (bits - 1) as i32);
            }
            match kind {
                ShiftKind::Left => emit_shl_cl(asm, Reg::R10),
                ShiftKind::Right => emit_shr_cl(asm, Reg::R10),
            }
        }
    }

    if bits < 64 {
        let mask = (1u64 << bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_sext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_integer_type(&src_ty) {
        return Err(Error::from("sext expects integer source"));
    }
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if dst_bits < src_bits {
        return Err(Error::from("sext expects wider destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
        emit_mov_rr(asm, Reg::R11, Reg::R10);
        emit_sar_imm8(asm, Reg::R11, 63);
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if src_bits < 64 {
        let shift = (64 - src_bits) as u8;
        emit_shl_imm8(asm, Reg::R10, shift);
        emit_sar_imm8(asm, Reg::R10, shift);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_sext_or_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if matches!(dst_ty, AsmType::I128) {
        return emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
    if src_bits == dst_bits {
        if matches!(src_ty, AsmType::I128) {
            load_i128_value(
                asm,
                layout,
                value,
                Reg::R10,
                Reg::R11,
                reg_types,
                local_types,
            )?;
            store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        } else {
            load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
        return Ok(());
    }
    if src_bits < dst_bits {
        return emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
    emit_trunc(asm, layout, dst_id, value, dst_ty, reg_types, local_types)
}

fn emit_ptr_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let dst_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for ptrtoint"))?;
    if !is_integer_type(&dst_ty) {
        return Err(Error::from("ptrtoint expects integer destination"));
    }
    let dst_bits = int_bits(&dst_ty)?;
    if matches!(dst_ty, AsmType::I128) {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
        emit_mov_imm64(asm, Reg::R11, 0);
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_int_to_ptr(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let dst_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for inttoptr"))?;
    if !matches!(dst_ty, AsmType::Ptr(_)) {
        return Err(Error::from("inttoptr expects pointer destination"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if matches!(src_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::R10,
            Reg::R11,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_freeze(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let value_ty = reg_types
        .get(&dst_id)
        .cloned()
        .ok_or_else(|| Error::from("missing result type for freeze"))?;
    if is_float_type(&value_ty) {
        load_value_float(
            asm,
            layout,
            value,
            FReg::Xmm0,
            &value_ty,
            reg_types,
            local_types,
        )?;
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &value_ty)?;
        return Ok(());
    }
    if matches!(value_ty, AsmType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::R10,
            Reg::R11,
            reg_types,
            local_types,
        )?;
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    if is_aggregate_storage(&value_ty, &layout.data_layout) {
        return Err(Error::from("freeze does not support aggregate values"));
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_inline_asm(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    output_type: &AsmType,
) -> Result<()> {
    match output_type {
        AsmType::Void => Ok(()),
        ty if is_float_type(ty) => {
            emit_mov_imm64(asm, Reg::R10, 0);
            emit_movq_xmm_r64(asm, FReg::Xmm0, Reg::R10);
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            Ok(())
        }
        AsmType::I128 => {
            emit_mov_imm64(asm, Reg::R10, 0);
            emit_mov_imm64(asm, Reg::R11, 0);
            store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
            Ok(())
        }
        ty if is_aggregate_storage(ty, &layout.data_layout) => {
            Err(Error::from("inline asm does not support aggregate outputs"))
        }
        _ => {
            emit_mov_imm64(asm, Reg::R10, 0);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
            Ok(())
        }
    }
}

fn emit_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BinOp,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    if is_float_type(ty) {
        emit_float_binop(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            op,
            ty,
            reg_types,
            local_types,
        )?;
        return Ok(());
    }
    if matches!(ty, AsmType::I128) {
        return emit_i128_binop(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            op,
            reg_types,
            local_types,
            format,
        );
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        AsmValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
            }
        }
        AsmValue::Constant(constant) => {
            let imm = constant_to_i64(constant, &layout.data_layout)?;
            if let Ok(imm32) = i32::try_from(imm) {
                match op {
                    BinOp::Add => emit_add_ri32(asm, Reg::R10, imm32),
                    BinOp::Sub => emit_sub_ri32(asm, Reg::R10, imm32),
                    BinOp::Mul => {
                        emit_mov_imm64(asm, Reg::R11, imm as u64);
                        emit_imul_rr(asm, Reg::R10, Reg::R11);
                    }
                }
            } else {
                emit_mov_imm64(asm, Reg::R11, imm as u64);
                match op {
                    BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                    BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                    BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
                }
            }
        }
        other => {
            load_value(asm, layout, other, Reg::R11, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
            }
        }
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn load_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    dst: Reg,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) {
                emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                return Ok(());
            }
            if matches!(ty, AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                AsmType::I1 => emit_movzx_rm8(asm, dst, Reg::Rbp, offset),
                AsmType::I8 => emit_movsx_rm8(asm, dst, Reg::Rbp, offset),
                AsmType::I16 => emit_movsx_rm16(asm, dst, Reg::Rbp, offset),
                AsmType::I32 => emit_movsxd_rm32(asm, dst, Reg::Rbp, offset),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for x86_64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) {
                emit_mov_rr(asm, dst, Reg::Rbp);
                emit_add_ri32(asm, dst, offset);
                return Ok(());
            }
            if matches!(ty, AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                AsmType::I1 => emit_movzx_rm8(asm, dst, Reg::Rbp, offset),
                AsmType::I8 => emit_movsx_rm8(asm, dst, Reg::Rbp, offset),
                AsmType::I16 => emit_movsx_rm16(asm, dst, Reg::Rbp, offset),
                AsmType::I32 => emit_movsxd_rm32(asm, dst, Reg::Rbp, offset),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for x86_64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        AsmValue::Constant(constant) => {
            if size_of(&constant_type(constant)) == 0 {
                emit_mov_imm64(asm, dst, 0);
                return Ok(());
            }
            if matches!(constant_type(constant), AsmType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            if let AsmConstant::GlobalRef(name, _, indices) = constant {
                let addend = indices.iter().map(|idx| *idx as i64).sum();
                emit_mov_symbol_addr(asm, dst, name.as_str(), addend)?;
                return Ok(());
            }
            let imm = constant_to_i64(constant, &layout.data_layout)?;
            emit_mov_imm64(asm, dst, imm as u64);
            Ok(())
        }
        AsmValue::Condition(condition) => {
            emit_setcc(asm, x86_setcc_code(condition)?, dst);
            emit_movzx_r64_rm8(asm, dst, dst);
            Ok(())
        }
        AsmValue::Flags(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_movzx_rm8(asm, dst, Reg::Rbp, offset);
            Ok(())
        }
        AsmValue::Global(name, ty) => {
            // A pointer-typed global is the native representation of a
            // host-global address.  Loading it as a scalar would dereference
            // the host symbol once before the explicit host-pointer load.
            if matches!(ty, AsmType::Ptr(_)) {
                emit_mov_symbol_addr(asm, dst, name, 0)?;
                return Ok(());
            }
            if is_aggregate_type(ty) {
                emit_mov_symbol_addr(asm, dst, name, 0)?;
                return Ok(());
            }
            emit_mov_symbol_addr(asm, Reg::R11, name, 0)?;
            match ty {
                AsmType::I1 => emit_movzx_rm8(asm, dst, Reg::R11, 0),
                AsmType::I8 => emit_movsx_rm8(asm, dst, Reg::R11, 0),
                AsmType::I16 => emit_movsx_rm16(asm, dst, Reg::R11, 0),
                AsmType::I32 => emit_movsxd_rm32(asm, dst, Reg::R11, 0),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_rm64(asm, dst, Reg::R11, 0)
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported global value type for x86_64: {ty:?}"
                    )));
                }
            }
            Ok(())
        }
        AsmValue::Null(_) | AsmValue::Undef(_) => {
            emit_mov_imm64(asm, dst, 0);
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported LIR value for x86_64: value={value:?}, type={ty:?}"
            )))
        }
    }
}

fn x86_setcc_code(condition: &AsmConditionCode) -> Result<u8> {
    match condition {
        AsmConditionCode::Eq => Ok(0x4),
        AsmConditionCode::Ne | AsmConditionCode::Nz => Ok(0x5),
        AsmConditionCode::Lt => Ok(0xC),
        AsmConditionCode::Le => Ok(0xE),
        AsmConditionCode::Gt => Ok(0xF),
        AsmConditionCode::Ge => Ok(0xD),
        AsmConditionCode::Ult => Ok(0x2),
        AsmConditionCode::Ule => Ok(0x6),
        AsmConditionCode::Ugt => Ok(0x7),
        AsmConditionCode::Uge => Ok(0x3),
    }
}

fn i128_parts_from_const(constant: &AsmConstant) -> Result<(u64, u64)> {
    match constant {
        AsmConstant::Int(value, ty) if matches!(ty, AsmType::I128) => {
            let lo = *value as u64;
            let hi = if *value < 0 { u64::MAX } else { 0 };
            Ok((lo, hi))
        }
        AsmConstant::UInt(value, ty) if matches!(ty, AsmType::I128) => Ok((*value as u64, 0)),
        AsmConstant::Bool(value) => Ok((if *value { 1 } else { 0 }, 0)),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok((0, 0)),
        other => Err(Error::from(format!(
            "unsupported i128 constant: {:?}",
            other
        ))),
    }
}

fn load_i128_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    lo: Reg,
    hi: Reg,
    _reg_types: &HashMap<u32, AsmType>,
    _local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, lo, Reg::Rbp, offset);
            emit_mov_rm64(asm, hi, Reg::Rbp, offset + 8);
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, lo, Reg::Rbp, offset);
            emit_mov_rm64(asm, hi, Reg::Rbp, offset + 8);
            Ok(())
        }
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_mov_rm64(asm, lo, Reg::Rbp, offset);
            emit_mov_rm64(asm, hi, Reg::Rbp, offset + 8);
            Ok(())
        }
        AsmValue::Constant(constant) => {
            let (lo_val, hi_val) = i128_parts_from_const(constant)?;
            emit_mov_imm64(asm, lo, lo_val);
            emit_mov_imm64(asm, hi, hi_val);
            Ok(())
        }
        AsmValue::Null(_) | AsmValue::Undef(_) => {
            emit_mov_imm64(asm, lo, 0);
            emit_mov_imm64(asm, hi, 0);
            Ok(())
        }
        _ => Err(Error::from("unsupported i128 value")),
    }
}

fn store_i128_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lo: Reg,
    hi: Reg,
) -> Result<()> {
    let offset = vreg_offset(layout, dst_id)?;
    emit_mov_mr64(asm, Reg::Rbp, offset, lo);
    emit_mov_mr64(asm, Reg::Rbp, offset + 8, hi);
    Ok(())
}

fn load_aggregate_pair(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    lo: Reg,
    hi: Reg,
) -> Result<()> {
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            emit_mov_rm64(asm, lo, Reg::R11, 0);
            emit_mov_rm64(asm, hi, Reg::R11, 8);
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, lo, Reg::Rbp, offset);
            emit_mov_rm64(asm, hi, Reg::Rbp, offset + 8);
        }
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_mov_rm64(asm, lo, Reg::Rbp, offset);
            emit_mov_rm64(asm, hi, Reg::Rbp, offset + 8);
        }
        _ => return Err(Error::from("unsupported aggregate pair value")),
    }
    Ok(())
}

fn store_aggregate_pair(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lo: Reg,
    hi: Reg,
) -> Result<()> {
    let offset = agg_offset(layout, dst_id)?;
    emit_mov_mr64(asm, Reg::Rbp, offset, lo);
    emit_mov_mr64(asm, Reg::Rbp, offset + 8, hi);
    emit_mov_rr(asm, Reg::R10, Reg::Rbp);
    emit_add_ri32(asm, Reg::R10, offset);
    store_vreg(asm, layout, dst_id, Reg::R10)
}

fn emit_i128_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BinOp,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    match op {
        BinOp::Add => {
            load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::R8, Reg::R9, reg_types, local_types)?;
            emit_add_rr(asm, Reg::R10, Reg::R8);
            emit_adc_rr(asm, Reg::R11, Reg::R9);
            store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        }
        BinOp::Sub => {
            load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::R8, Reg::R9, reg_types, local_types)?;
            emit_sub_rr(asm, Reg::R10, Reg::R8);
            emit_sbb_rr(asm, Reg::R11, Reg::R9);
            store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        }
        BinOp::Mul => {
            emit_i128_libcall(
                asm,
                layout,
                dst_id,
                "__multi3",
                lhs,
                Some(rhs),
                None,
                reg_types,
                local_types,
                format,
            )?;
        }
    }
    Ok(())
}

fn emit_i128_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let symbol = match kind {
        ShiftKind::Left => "__ashlti3",
        ShiftKind::Right => "__lshrti3",
    };
    emit_i128_libcall(
        asm,
        layout,
        dst_id,
        symbol,
        lhs,
        None,
        Some(rhs),
        reg_types,
        local_types,
        format,
    )
}

fn emit_i128_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    want_rem: bool,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let symbol = if want_rem { "__modti3" } else { "__divti3" };
    emit_i128_libcall(
        asm,
        layout,
        dst_id,
        symbol,
        lhs,
        Some(rhs),
        None,
        reg_types,
        local_types,
        format,
    )
}

fn emit_i128_libcall(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    symbol: &str,
    lhs: &AsmValue,
    rhs: Option<&AsmValue>,
    shift: Option<&AsmValue>,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let (arg_regs, _, _) = call_abi(format);
    let mut int_idx = 0usize;
    let mut stack_idx = 0usize;

    load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
    push_reg_arg(
        asm,
        layout,
        Reg::R10,
        &mut int_idx,
        &mut stack_idx,
        arg_regs,
    )?;
    push_reg_arg(
        asm,
        layout,
        Reg::R11,
        &mut int_idx,
        &mut stack_idx,
        arg_regs,
    )?;

    if let Some(rhs) = rhs {
        load_i128_value(asm, layout, rhs, Reg::R8, Reg::R9, reg_types, local_types)?;
        push_reg_arg(asm, layout, Reg::R8, &mut int_idx, &mut stack_idx, arg_regs)?;
        push_reg_arg(asm, layout, Reg::R9, &mut int_idx, &mut stack_idx, arg_regs)?;
    }

    if let Some(shift) = shift {
        load_value(asm, layout, shift, Reg::Rax, reg_types, local_types)?;
        push_reg_arg(
            asm,
            layout,
            Reg::Rax,
            &mut int_idx,
            &mut stack_idx,
            arg_regs,
        )?;
    }

    asm.emit_call_external(symbol);
    store_i128_value(asm, layout, dst_id, Reg::Rax, Reg::Rdx)?;
    Ok(())
}

fn load_value_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    dst: FReg,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            if matches!(ty, AsmType::Vector(_, _) if size_of(ty) == 16) {
                emit_movdqu_xm128(asm, dst, Reg::Rbp, offset);
            } else {
                emit_movsd_xm64(asm, dst, Reg::Rbp, offset, ty);
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            if matches!(ty, AsmType::Vector(_, _) if size_of(ty) == 16) {
                emit_movdqu_xm128(asm, dst, Reg::Rbp, offset);
            } else {
                emit_movsd_xm64(asm, dst, Reg::Rbp, offset, ty);
            }
            Ok(())
        }
        AsmValue::Constant(AsmConstant::Float(value, _)) => {
            let bits = if matches!(ty, AsmType::F32) {
                (*value as f32).to_bits() as u64
            } else {
                value.to_bits()
            };
            emit_mov_imm64(asm, Reg::R10, bits);
            emit_movq_xmm_r64(asm, dst, Reg::R10);
            Ok(())
        }
        _ => {
            let actual_ty = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported float value for x86_64: {:?}",
                actual_ty
            )))
        }
    }
}

fn emit_float_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BinOp,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types, local_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types, local_types)?;
    match op {
        BinOp::Add => emit_addsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
        BinOp::Sub => emit_subsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
        BinOp::Mul => emit_mulsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
    }
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
    Ok(())
}

fn emit_float_div(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types, local_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types, local_types)?;
    emit_divsd(asm, FReg::Xmm0, FReg::Xmm1, ty);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
    Ok(())
}

fn emit_float_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: CmpKind,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types, local_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types, local_types)?;
    emit_ucomisd(asm, FReg::Xmm0, FReg::Xmm1, ty);
    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0x2,
        CmpKind::Le => 0x6,
        CmpKind::Gt => 0x7,
        CmpKind::Ge => 0x3,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, Reg::R10, Reg::R11);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_mov_mr64(asm, Reg::Rbp, offset, src);
    Ok(())
}

fn store_vreg_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    id: u32,
    src: FReg,
    ty: &AsmType,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let offset = vreg_offset(layout, id)?;
    if matches!(ty, AsmType::Vector(_, _) if size_of(ty) == 16) {
        emit_movdqu_m128x(asm, Reg::Rbp, offset, src);
    } else {
        emit_movsd_m64x(asm, Reg::Rbp, offset, src, ty);
    }
    Ok(())
}

struct Fixup {
    pos: usize,
    target: Label,
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<Label, usize>,
    fixups: Vec<Fixup>,
    needs_frame: bool,
    current_function: u32,
    relocs: Vec<Relocation>,
    entry_returns_exit: bool,
}

fn emit_prologue(asm: &mut Assembler, layout: &FrameLayout) -> Result<()> {
    asm.push(0x55);
    emit_mov_rr(asm, Reg::Rbp, Reg::Rsp);

    if layout.frame_size > 0 {
        emit_sub_ri32(asm, Reg::Rsp, layout.frame_size);
    }
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler) {
    emit_mov_rr(asm, Reg::Rsp, Reg::Rbp);
    asm.push(0x5D);
}

fn emit_panic_stub(asm: &mut Assembler, id: u32) {
    asm.needs_frame = false;
    asm.bind(Label::Function(id));
    asm.emit_call_external("abort");
    emit_ret(asm);
}

impl Assembler {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            needs_frame: false,
            current_function: 0,
            relocs: Vec::new(),
            entry_returns_exit: false,
        }
    }

    fn bind(&mut self, label: Label) {
        if let Label::Function(id) = label {
            self.current_function = id;
        }
        self.labels.insert(label, self.buf.len());
    }

    fn function_offsets(&self) -> HashMap<u32, u64> {
        let mut out = HashMap::new();
        for (label, pos) in &self.labels {
            if let Label::Function(id) = label {
                out.insert(*id, *pos as u64);
            }
        }
        out
    }

    fn is_entry(&self) -> bool {
        self.current_function == 0
    }

    fn emit_jmp(&mut self, target: Label) {
        self.buf.push(0xE9);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup { pos, target });
    }

    fn emit_jcc(&mut self, opcode: u8, target: Label) {
        self.buf.push(0x0F);
        self.buf.push(opcode);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup { pos, target });
    }

    fn emit_call(&mut self, target: Label) {
        self.buf.push(0xE8);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup { pos, target });
    }

    fn emit_call_reg(&mut self, reg: Reg) {
        emit_rex(self, true, 2, reg.id());
        self.buf.push(0xFF);
        emit_modrm(self, 0b11, 2, reg.id());
    }

    fn emit_call_external(&mut self, symbol: &str) {
        self.buf.push(0xE8);
        let offset = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::CallRel32,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend: 0,
        });
    }

    fn emit_mov_imm64_reloc(&mut self, dst: Reg, symbol: &str, addend: i64) {
        emit_rex(self, true, 0, dst.id());
        self.buf.push(0xB8 + (dst.id() & 0x7));
        let offset = self.buf.len();
        self.buf.extend_from_slice(&0u64.to_le_bytes());
        self.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::Abs64,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend,
        });
    }

    fn push(&mut self, byte: u8) {
        self.buf.push(byte);
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<(Vec<u8>, Vec<Relocation>)> {
        for fixup in &self.fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown jump target"))?;
            let origin = fixup.pos;
            let rel = (*target as i64) - (origin as i64 + 4);
            let rel32 =
                i32::try_from(rel).map_err(|_| Error::from(format!("jump out of range: {rel}")))?;
            self.buf[origin..origin + 4].copy_from_slice(&rel32.to_le_bytes());
        }
        Ok((self.buf, self.relocs))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Label {
    Function(u32),
    Block(u32, BasicBlockId),
}

fn build_function_map(program: &AsmProgram) -> Result<HashMap<String, u32>> {
    let mut map = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        if func.is_declaration {
            continue;
        }
        let name = String::from(func.name.clone());
        map.insert(name, idx as u32);
    }
    Ok(map)
}

fn program_uses_fp_panic(program: &AsmProgram) -> bool {
    for func in &program.functions {
        for block in &func.basic_blocks {
            for inst in &block.instructions {
                if let AsmInstructionKind::Call { function, .. } = &inst.kind {
                    if matches!(function, AsmValue::Function(name) if name == "fp_panic") {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn extract_value_type(ty: &AsmType, indices: &[u32]) -> Result<AsmType> {
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                current_ty = fields
                    .get(*idx as usize)
                    .cloned()
                    .ok_or_else(|| Error::from("ExtractValue field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                current_ty = *elem.clone();
            }
            _ => return Err(Error::from("ExtractValue expects aggregate type")),
        }
    }
    Ok(current_ty)
}

fn aggregate_field_offset(
    ty: &AsmType,
    indices: &[u32],
    data_layout: &LirDataLayout,
) -> Result<(i64, AsmType)> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    let mut offset = 0i64;
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                let layout = struct_layout(&current_ty)
                    .ok_or_else(|| Error::from("missing struct layout for aggregate"))?;
                let field_offset = *layout
                    .field_offsets
                    .get(*idx as usize)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                offset += field_offset as i64;
                current_ty = fields
                    .get(*idx as usize)
                    .cloned()
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                let elem_size = size_of(elem) as i64;
                offset += elem_size * (*idx as i64);
                current_ty = *elem.clone();
            }
            _ => return Err(Error::from("unsupported aggregate type for indices")),
        }
    }
    Ok((offset, current_ty))
}

fn emit_landingpad(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    result_ty: &AsmType,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let size = size_of(result_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    if is_aggregate_storage(result_ty, &layout.data_layout) {
        let dst_offset = agg_offset(layout, dst_id)?;
        zero_sp_range(asm, dst_offset, size)?;
        emit_mov_rr(asm, Reg::R10, Reg::Rbp);
        emit_add_ri32(asm, Reg::R10, dst_offset);
        store_vreg(asm, layout, dst_id, Reg::R10)?;
        return Ok(());
    }
    emit_mov_imm64(asm, Reg::R10, 0);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_float_prefix(asm: &mut Assembler, ty: &AsmType) {
    match ty {
        AsmType::F32 => asm.push(0xF3),
        AsmType::F64 => asm.push(0xF2),
        _ => {}
    }
}

fn emit_modrm_disp32(asm: &mut Assembler, reg: u8, rm: u8, disp: i32) {
    emit_modrm(asm, 0b10, reg, rm);
    asm.extend(&disp.to_le_bytes());
}

fn emit_sib(asm: &mut Assembler, scale: u8, index: u8, base: u8) {
    let byte = ((scale & 0x3) << 6) | ((index & 0x7) << 3) | (base & 0x7);
    asm.push(byte);
}

fn emit_cqo(asm: &mut Assembler) {
    emit_rex(asm, true, 0, 0);
    asm.push(0x99);
}

fn emit_idiv_reg(asm: &mut Assembler, divisor: Reg) {
    emit_rex(asm, true, 7, divisor.id());
    asm.push(0xF7);
    emit_modrm(asm, 0b11, 7, divisor.id());
}

#[cfg(test)]
mod tests;
