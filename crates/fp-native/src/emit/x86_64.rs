use fp_core::asmir::{
    AsmArchitecture, AsmBlock, AsmBlockId as BasicBlockId, AsmConstant, AsmFunction,
    AsmInstructionKind, AsmIntrinsicKind, AsmProgram, AsmSyscallConvention, AsmTerminator, AsmType,
    AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::layout::{align_of, size_of, struct_layout};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

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
    vreg_offsets: HashMap<u32, i32>,
    slot_offsets: HashMap<u32, i32>,
    local_offsets: HashMap<u32, i32>,
    agg_offsets: HashMap<u32, i32>,
    alloca_offsets: HashMap<u32, i32>,
    sret_offset: Option<i32>,
    outgoing_size: i32,
    shadow_space: i32,
    frame_size: i32,
}

fn build_frame_layout(
    func: &AsmFunction,
    format: TargetFormat,
    reg_types: &HashMap<u32, AsmType>,
) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut has_calls = false;
    let mut alloca_info = Vec::new();
    let local_types = build_local_types(func);

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let AsmInstructionKind::Call { args, .. } = &inst.kind {
                has_calls = true;
                let mut count = 0usize;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types);
                }
                max_call_args = max_call_args.max(count);
            } else if let AsmInstructionKind::IntrinsicCall { kind, args, .. } = &inst.kind {
                has_calls = true;
                let fixed = if matches!(kind, AsmIntrinsicKind::Format) {
                    3
                } else {
                    1
                };
                let mut count = fixed;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types);
                }
                max_call_args = max_call_args.max(count);
            } else if matches!(
                inst.kind,
                AsmInstructionKind::Mul(_, _)
                    | AsmInstructionKind::Div(_, _)
                    | AsmInstructionKind::Rem(_, _)
                    | AsmInstructionKind::Shl(_, _)
                    | AsmInstructionKind::Shr(_, _)
            ) {
                if matches!(inst.type_hint, Some(AsmType::I128)) {
                    has_calls = true;
                    let args = match inst.kind {
                        AsmInstructionKind::Shl(_, _) | AsmInstructionKind::Shr(_, _) => 3,
                        _ => 4,
                    };
                    max_call_args = max_call_args.max(args);
                }
            } else if let AsmInstructionKind::Alloca { size, alignment } = &inst.kind {
                let ty = inst
                    .type_hint
                    .clone()
                    .ok_or_else(|| Error::from("missing type for alloca"))?;
                let AsmType::Ptr(inner) = ty else {
                    return Err(Error::from("alloca expects pointer type"));
                };
                let count = match size {
                    AsmValue::Constant(constant) => constant_to_i64(constant)?,
                    _ => return Err(Error::from("alloca size must be constant")),
                };
                if count < 0 {
                    return Err(Error::from("alloca size must be non-negative"));
                }
                let elem_size = size_of(&inner) as i64;
                let bytes = elem_size
                    .checked_mul(count)
                    .ok_or_else(|| Error::from("alloca size overflow"))?;
                let bytes =
                    i32::try_from(bytes).map_err(|_| Error::from("alloca size too large"))?;
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
        let (size, align) = vreg_slot_spec(*id, reg_types);
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
        let size = align8(size_of(&local.ty) as i32).max(8);
        offset = align_to(offset, 8);
        offset += size;
        local_offsets.insert(local.id, -offset);
    }

    if returns_aggregate(&func.signature.return_type) {
        offset += 8;
        sret_offset = Some(-offset);
    }

    for id in &vreg_ids {
        if let Some(ty) = reg_types.get(id) {
            if is_large_aggregate(ty) {
                let size = align8(size_of(ty) as i32);
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
        alloca_offsets.insert(id, -offset);
        offset += size;
    }

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
    let frame_size = if base == 0 {
        if has_calls { 8 } else { 0 }
    } else {
        align16(base + 8) - 8
    };

    Ok(FrameLayout {
        vreg_offsets,
        slot_offsets,
        local_offsets,
        agg_offsets,
        alloca_offsets,
        sret_offset,
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
) -> usize {
    let ty = value_type(arg, reg_types, local_types).unwrap_or(AsmType::I64);
    if matches!(ty, AsmType::I128) { 2 } else { 1 }
}

fn vreg_slot_spec(id: u32, reg_types: &HashMap<u32, AsmType>) -> (i32, i32) {
    let Some(ty) = reg_types.get(&id) else {
        return (8, 8);
    };
    if is_large_aggregate(ty) {
        return (8, 8);
    }
    if matches!(ty, AsmType::I128) {
        let align = align_of(ty) as i32;
        return (16, align.max(16));
    }
    (8, 8)
}

fn build_reg_types(func: &AsmFunction) -> HashMap<u32, AsmType> {
    let mut map = HashMap::new();
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if let Some(ty) = inst.type_hint.as_ref() {
                map.insert(inst.id, ty.clone());
            }
        }
    }

    let mut local_types = HashMap::new();
    for local in &func.locals {
        local_types.insert(local.id, local.ty.clone());
    }

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if map.contains_key(&inst.id) {
                continue;
            }
            if let AsmInstructionKind::ExtractValue { aggregate, indices } = &inst.kind {
                if let Ok(agg_ty) = value_type(aggregate, &map, &local_types) {
                    if let Ok(field_ty) = extract_value_type(&agg_ty, indices) {
                        map.insert(inst.id, field_ty);
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
    let needs_panic_stub = program_uses_fp_panic(program) && !func_map.contains_key("fp_panic");
    let panic_id = if needs_panic_stub {
        let id = func_map.len() as u32;
        func_map.insert("fp_panic".to_string(), id);
        Some(id)
    } else {
        None
    };
    let mut asm = Assembler::new();
    let mut rodata = Vec::new();
    let mut data = Vec::new();
    let mut rodata_pool = HashMap::new();
    let mut rodata_symbols = HashMap::new();
    let mut data_symbols = HashMap::new();
    let mut global_relocs = Vec::new();
    let mut entry_offset = None;

    emit_const_globals(
        program,
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
        let reg_types = build_reg_types(func);
        let layout = build_frame_layout(func, format, &reg_types)?;
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

    let entry_offset = entry_offset.unwrap_or(0);
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
    let (text, mut relocs) = asm.finish()?;
    relocs.extend(global_relocs);
    Ok(CodegenOutput {
        text,
        rodata,
        data,
        relocs,
        symbols,
        rodata_symbols,
        data_symbols,
        entry_offset,
    })
}

fn emit_const_globals(
    program: &AsmProgram,
    rodata: &mut Vec<u8>,
    rodata_symbols: &mut HashMap<String, u64>,
    data: &mut Vec<u8>,
    data_symbols: &mut HashMap<String, u64>,
    relocs_out: &mut Vec<crate::emit::Relocation>,
) -> Result<()> {
    let mut emit_global =
        |global: &fp_core::asmir::AsmGlobal,
         initializer: &AsmConstant,
         bytes_out: &mut Vec<u8>,
         symbols_out: &mut HashMap<String, u64>,
         reloc_section: crate::emit::RelocSection|
         -> Result<()> {
            let align = global
                .alignment
                .map(|value| value as i32)
                .unwrap_or_else(|| align_of(&global.ty) as i32);
            let offset = align_to(bytes_out.len() as i32, align) as usize;
            if offset > bytes_out.len() {
                bytes_out.resize(offset, 0);
            }
            let bytes = encode_const_bytes(initializer, &global.ty)?;
            bytes_out.extend_from_slice(&bytes);
            symbols_out.insert(global.name.to_string(), offset as u64);

            for reloc in &global.relocations {
                let kind = match reloc.kind {
                    fp_core::asmir::AsmRelocationKind::Abs64 => crate::emit::RelocKind::Abs64,
                };
                relocs_out.push(crate::emit::Relocation {
                    offset: offset as u64 + reloc.offset,
                    kind,
                    section: reloc_section,
                    symbol: reloc.symbol.to_string(),
                    addend: reloc.addend,
                });
            }
            Ok(())
        };

    for global in &program.globals {
        let Some(initializer) = &global.initializer else {
            continue;
        };

        let section_kind = global
            .section
            .as_deref()
            .and_then(|name| {
                program
                    .sections
                    .iter()
                    .find(|section| section.name == name)
                    .map(|section| section.kind.clone())
            })
            .unwrap_or_else(|| {
                if global.is_constant {
                    fp_core::asmir::AsmSectionKind::ReadOnlyData
                } else {
                    fp_core::asmir::AsmSectionKind::Data
                }
            });

        match section_kind {
            fp_core::asmir::AsmSectionKind::Data | fp_core::asmir::AsmSectionKind::Bss => {
                emit_global(
                    global,
                    initializer,
                    data,
                    data_symbols,
                    crate::emit::RelocSection::Data,
                )?;
            }
            _ => {
                emit_global(
                    global,
                    initializer,
                    rodata,
                    rodata_symbols,
                    crate::emit::RelocSection::Rdata,
                )?;
            }
        }
    }
    Ok(())
}

fn encode_const_bytes(constant: &AsmConstant, ty: &AsmType) -> Result<Vec<u8>> {
    match (constant, ty) {
        (AsmConstant::UInt(value, _), AsmType::I8) => Ok(vec![*value as u8]),
        (AsmConstant::Int(value, _), AsmType::I8) => Ok(vec![*value as u8]),
        (AsmConstant::UInt(value, _), AsmType::I16) => Ok((*value as u16).to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I16) => Ok((*value as i16).to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::I32) => Ok((*value as u32).to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I32) => Ok((*value as i32).to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::I64) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I64) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::Ptr(_)) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::Ptr(_)) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I8) => Ok(vec![0u8]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I16) => Ok(vec![0u8; 2]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I32) => Ok(vec![0u8; 4]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I64) => Ok(vec![0u8; 8]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::Ptr(_)) => Ok(vec![0u8; 8]),
        (AsmConstant::Bytes(bytes), AsmType::Array(elem, _)) if **elem == AsmType::I8 => {
            Ok(bytes.clone())
        }
        (AsmConstant::Array(values, _), AsmType::Array(_, len))
            if values.is_empty() || *len == 0 =>
        {
            Ok(Vec::new())
        }
        (AsmConstant::Array(values, elem_ty), AsmType::Array(elem, _))
            if **elem == AsmType::I8 && *elem_ty == AsmType::I8 =>
        {
            let mut out = Vec::with_capacity(values.len());
            for value in values {
                out.push(const_to_u8(value)?);
            }
            Ok(out)
        }
        _ => Err(Error::from(
            "unsupported global initializer for native rodata",
        )),
    }
}

fn const_to_u8(constant: &AsmConstant) -> Result<u8> {
    match constant {
        AsmConstant::Int(value, _) => Ok(*value as u8),
        AsmConstant::UInt(value, _) => Ok(*value as u8),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        _ => Err(Error::from(
            "unsupported global array element for native rodata",
        )),
    }
}

fn spill_arguments(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
    format: TargetFormat,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
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
        if is_large_aggregate(ty) {
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
            let imm = constant_to_i64(constant)?;
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
    let value_ty =
        reg_types
            .get(&dst_id)
            .cloned()
            .unwrap_or(value_type(value, reg_types, local_types)?);
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
    if is_large_aggregate(&value_ty) {
        return Err(Error::from("freeze does not support large aggregates"));
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
        ty if is_large_aggregate(ty) => Err(Error::from("inline asm output too large")),
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
            let imm = constant_to_i64(constant)?;
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
        _ => return Err(Error::from("unsupported RHS for x86_64")),
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
    match value {
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
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
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
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
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
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
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
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
                if indices.iter().any(|idx| *idx != 0) {
                    return Err(Error::from(
                        "unsupported non-zero GlobalRef indices for x86_64",
                    ));
                }
                emit_mov_symbol_addr(asm, dst, name.as_str(), 0)?;
                return Ok(());
            }
            let imm = constant_to_i64(constant)?;
            emit_mov_imm64(asm, dst, imm as u64);
            Ok(())
        }
        AsmValue::Null(_) | AsmValue::Undef(_) => {
            emit_mov_imm64(asm, dst, 0);
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported LIR value for x86_64: {:?}",
                ty
            )))
        }
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
    let offset = vreg_offset(layout, id)?;
    if matches!(ty, AsmType::Vector(_, _) if size_of(ty) == 16) {
        emit_movdqu_m128x(asm, Reg::Rbp, offset, src);
    } else {
        emit_movsd_m64x(asm, Reg::Rbp, offset, src, ty);
    }
    Ok(())
}

fn is_float_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::F32 | AsmType::F64)
}

fn is_aggregate_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::Struct { .. } | AsmType::Array(_, _))
}

fn is_vector_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::Vector(_, _))
}

fn is_large_aggregate(ty: &AsmType) -> bool {
    is_aggregate_type(ty) && size_of(ty) > 8
}

fn returns_aggregate(ty: &AsmType) -> bool {
    is_large_aggregate(ty)
}

fn is_integer_type(ty: &AsmType) -> bool {
    matches!(
        ty,
        AsmType::I1 | AsmType::I8 | AsmType::I16 | AsmType::I32 | AsmType::I64 | AsmType::I128
    )
}

fn int_bits(ty: &AsmType) -> Result<u32> {
    match ty {
        AsmType::I1 => Ok(1),
        AsmType::I8 => Ok(8),
        AsmType::I16 => Ok(16),
        AsmType::I32 => Ok(32),
        AsmType::I64 => Ok(64),
        AsmType::I128 => Ok(128),
        _ => Err(Error::from("expected integer type")),
    }
}

fn constant_type(constant: &AsmConstant) -> AsmType {
    match constant {
        AsmConstant::Int(_, ty) => ty.clone(),
        AsmConstant::UInt(_, ty) => ty.clone(),
        AsmConstant::Float(_, ty) => ty.clone(),
        AsmConstant::Bool(_) => AsmType::I1,
        AsmConstant::String(_) => AsmType::Ptr(Box::new(AsmType::I8)),
        AsmConstant::Bytes(bytes) => AsmType::Array(Box::new(AsmType::I8), bytes.len() as u64),
        AsmConstant::Null(ty) => ty.clone(),
        AsmConstant::Undef(ty) => ty.clone(),
        AsmConstant::Array(_, ty) => ty.clone(),
        AsmConstant::Struct(_, ty) => ty.clone(),
        AsmConstant::GlobalRef(_, ty, _) => ty.clone(),
        AsmConstant::FunctionRef(_, ty) => ty.clone(),
    }
}

fn value_type(
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<AsmType> {
    match value {
        AsmValue::Register(id) => reg_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing register type")),
        AsmValue::PhysicalRegister(register) => Ok(match register.size_bits {
            0..=8 => AsmType::I8,
            9..=16 => AsmType::I16,
            17..=32 => AsmType::I32,
            33..=64 => AsmType::I64,
            _ => AsmType::I128,
        }),
        AsmValue::Address(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
        AsmValue::Condition(_) => Ok(AsmType::I1),
        AsmValue::Comparison(_) => Ok(AsmType::I1),
        AsmValue::Flags(_) => Ok(AsmType::I1),
        AsmValue::Constant(constant) => Ok(constant_type(constant)),
        AsmValue::Null(ty) | AsmValue::Undef(ty) => Ok(ty.clone()),
        AsmValue::StackSlot(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
        AsmValue::Local(id) => local_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing local type")),
        AsmValue::Global(_, ty) => Ok(ty.clone()),
        AsmValue::Function(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
    }
}

fn constant_to_i64(constant: &AsmConstant) -> Result<i64> {
    match constant {
        AsmConstant::Int(value, _) => Ok(*value),
        AsmConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        AsmConstant::Array(values, _) if values.is_empty() => Ok(0),
        AsmConstant::Struct(values, ty) if values.is_empty() || size_of(ty) == 0 => Ok(0),
        _ => Err(Error::from(format!(
            "unsupported constant for x86_64: {:?}",
            constant
        ))),
    }
}

fn constant_to_u64_bits(constant: &AsmConstant) -> Result<u64> {
    match constant {
        AsmConstant::Int(value, _) => Ok(*value as u64),
        AsmConstant::UInt(value, _) => Ok(*value),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Float(value, _) => Ok(value.to_bits()),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        _ => Err(Error::from("unsupported constant for aggregate store")),
    }
}

fn pack_small_aggregate(constant: &AsmConstant, ty: &AsmType) -> Result<u64> {
    if size_of(ty) > 8 {
        return Err(Error::from("aggregate too large to pack"));
    }
    match (constant, ty) {
        (AsmConstant::Struct(values, _), AsmType::Struct { fields, .. }) => {
            let layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            let mut packed = 0u64;
            for (idx, field) in values.iter().enumerate() {
                let field_ty = fields
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_size = size_of(field_ty) as u64;
                if field_size == 0 {
                    continue;
                }
                if field_size > 8 {
                    return Err(Error::from("unsupported aggregate field size"));
                }
                let mut bits = constant_to_u64_bits(field)?;
                let mask = if field_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (field_size * 8)) - 1
                };
                bits &= mask;
                let offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                packed |= bits << (offset as u64 * 8);
            }
            Ok(packed)
        }
        (AsmConstant::Array(values, _), AsmType::Array(elem, len)) => {
            let elem_ty = elem.as_ref();
            let elem_size = size_of(elem_ty) as u64;
            if elem_size == 0 {
                return Ok(0);
            }
            if elem_size > 8 {
                return Err(Error::from("unsupported array element size"));
            }
            let mut packed = 0u64;
            for idx in 0..(*len as usize).min(values.len()) {
                let mut bits = constant_to_u64_bits(&values[idx])?;
                let mask = if elem_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (elem_size * 8)) - 1
                };
                bits &= mask;
                let offset = (idx as u64) * elem_size;
                packed |= bits << (offset * 8);
            }
            Ok(packed)
        }
        (AsmConstant::Array(values, _), other_ty) => {
            let elem_size = size_of(other_ty) as u64;
            if elem_size == 0 {
                return Ok(0);
            }
            if elem_size > 8 {
                return Err(Error::from("unsupported array element size"));
            }
            let mut packed = 0u64;
            for (idx, value) in values.iter().enumerate() {
                let mut bits = constant_to_u64_bits(value)?;
                let mask = if elem_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (elem_size * 8)) - 1
                };
                bits &= mask;
                let offset = (idx as u64) * elem_size;
                if offset >= 8 {
                    break;
                }
                packed |= bits << (offset * 8);
            }
            Ok(packed)
        }
        _ => Err(Error::from("unsupported aggregate packing")),
    }
}

fn vreg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .vreg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing vreg slot"))
}

fn stack_slot_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .slot_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing stack slot"))
}

fn local_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout.local_offsets.get(&id).copied().ok_or_else(|| {
        let mut known = layout.local_offsets.keys().copied().collect::<Vec<_>>();
        known.sort_unstable();
        Error::from(format!(
            "missing local slot: id={} known_local_ids={:?}",
            id, known
        ))
    })
}

fn agg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .agg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from(format!("missing aggregate slot for vreg {}", id)))
}

fn alloca_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .alloca_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing alloca slot"))
}

fn emit_ret(asm: &mut Assembler) {
    asm.push(0xC3);
}

fn emit_trap(asm: &mut Assembler) {
    asm.extend(&[0x0F, 0x0B]);
}

fn emit_exit_syscall(asm: &mut Assembler, code: u32) -> Result<()> {
    if code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    emit_mov_imm64(asm, Reg::Rdi, code as u64);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_rr(asm, Reg::Rdi, reg);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_mov_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xB8 + (dst.id() & 0x7));
    asm.extend(&imm.to_le_bytes());
}

fn emit_mov_symbol_addr(asm: &mut Assembler, dst: Reg, symbol: &str, addend: i64) -> Result<()> {
    asm.emit_mov_imm64_reloc(dst, symbol, addend);
    Ok(())
}

fn emit_add_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x01);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_adc_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x11);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_sub_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x29);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_sbb_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x19);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_imul_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.extend(&[0x0F, 0xAF]);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_and_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x21);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_or_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x09);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_xor_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x31);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_not_r64(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xF7);
    emit_modrm(asm, 0b11, 2, dst.id());
}

fn emit_add_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 0, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_sub_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_and_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 4, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_shl_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 4, dst.id());
    asm.push(imm);
}

fn emit_pextrq_r64_xmm_imm8(asm: &mut Assembler, dst: Reg, src: FReg, imm: u8) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x3A);
    asm.push(0x16);
    emit_modrm(asm, 0b11, dst.id(), src.id());
    asm.push(imm);
}

fn emit_shr_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.push(imm);
}

fn emit_sar_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 7, dst.id());
    asm.push(imm);
}

fn emit_shl_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 4, dst.id());
}

fn emit_shr_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 5, dst.id());
}

#[allow(dead_code)]
fn emit_sar_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 7, dst.id());
}

fn emit_rex(asm: &mut Assembler, w: bool, reg: u8, rm: u8) {
    let mut rex = 0x40;
    if w {
        rex |= 0x08;
    }
    if (reg & 0x8) != 0 {
        rex |= 0x04;
    }
    if (rm & 0x8) != 0 {
        rex |= 0x01;
    }
    asm.push(rex);
}

fn emit_modrm(asm: &mut Assembler, mode: u8, reg: u8, rm: u8) {
    let byte = ((mode & 0x3) << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
    asm.push(byte);
}

fn emit_cmp_rr(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    emit_rex(asm, true, rhs.id(), lhs.id());
    asm.push(0x39);
    emit_modrm(asm, 0b11, rhs.id(), lhs.id());
}

fn emit_cmp_imm32(asm: &mut Assembler, lhs: Reg, imm: i32) {
    emit_rex(asm, true, 0, lhs.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 7, lhs.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_setcc(asm: &mut Assembler, cc: u8, dst: Reg) {
    emit_rex(asm, false, 0, dst.id());
    asm.push(0x0F);
    asm.push(0x90 + cc);
    emit_modrm(asm, 0b11, 0, dst.id());
}

fn emit_cmovcc(asm: &mut Assembler, cc: u8, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x40 + cc);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_movzx_r64_rm8(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_block(
    asm: &mut Assembler,
    block: &AsmBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    layout: &FrameLayout,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    return_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    for inst in &block.instructions {
        match &inst.kind {
            AsmInstructionKind::Add(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for add"))?;
                if matches!(ty, AsmType::Ptr(_)) {
                    if let (
                        AsmValue::Constant(AsmConstant::String(lhs_text)),
                        AsmValue::Constant(AsmConstant::String(rhs_text)),
                    ) = (lhs, rhs)
                    {
                        let mut combined = String::with_capacity(lhs_text.len() + rhs_text.len());
                        combined.push_str(lhs_text);
                        combined.push_str(rhs_text);
                        let offset = intern_cstring(rodata, rodata_pool, &combined);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                        store_vreg(asm, layout, inst.id, Reg::R10)?;
                        continue;
                    }
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Add,
                    ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Sub(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for sub"))?;
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Sub,
                    ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Mul(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for mul"))?;
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Mul,
                    ty,
                    reg_types,
                    local_types,
                    format,
                )?
            }
            AsmInstructionKind::Splat {
                value,
                lane_bits,
                lanes,
            } => {
                let result_ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for splat"))?;
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(result_ty) == 16) {
                    return Err(Error::from("splat expects 128-bit vector result"));
                }

                if *lane_bits != 64 || *lanes != 2 {
                    return Err(Error::from("x86_64 splat only supports 2x64 lanes for now"));
                }

                load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
                emit_movq_xmm_r64(asm, FReg::Xmm0, Reg::R10);
                emit_punpcklqdq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm0);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, result_ty)?;
            }
            AsmInstructionKind::BuildVector { elements } => {
                let result_ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for build_vector"))?;
                let AsmType::Vector(elem_ty, lanes) = result_ty else {
                    return Err(Error::from("build_vector expects vector result type"));
                };
                if size_of(result_ty) != 16 {
                    return Err(Error::from("build_vector only supports 128-bit vectors"));
                }
                if *elem_ty.as_ref() != AsmType::I64 || *lanes != 2 {
                    return Err(Error::from(
                        "build_vector currently only supports <2 x i64> on x86_64",
                    ));
                }
                if elements.len() != 2 {
                    return Err(Error::from("build_vector lane count mismatch"));
                }
                if !matches!(
                    elements[1],
                    AsmValue::Constant(AsmConstant::Int(0, _))
                        | AsmValue::Constant(AsmConstant::UInt(0, _))
                        | AsmValue::Null(_)
                ) {
                    return Err(Error::from(
                        "build_vector currently requires lane1=0 for x86_64",
                    ));
                }

                load_value(asm, layout, &elements[0], Reg::R10, reg_types, local_types)?;
                emit_movq_xmm_r64(asm, FReg::Xmm0, Reg::R10);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, result_ty)?;
            }
            AsmInstructionKind::ExtractLane { vector, lane } => {
                let result_ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for extract_lane"))?;
                if *result_ty != AsmType::I64 {
                    return Err(Error::from("extract_lane only supports i64 for now"));
                }

                let vector_ty = value_type(vector, reg_types, local_types)?;
                if !matches!(vector_ty, AsmType::Vector(_, _) if size_of(&vector_ty) == 16) {
                    return Err(Error::from("extract_lane expects 128-bit vector input"));
                }
                if *lane > 1 {
                    return Err(Error::from("extract_lane lane out of range"));
                }

                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::Xmm0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                if *lane == 0 {
                    emit_movq_r64_xmm(asm, Reg::R10, FReg::Xmm0);
                } else {
                    emit_pextrq_r64_xmm_imm8(asm, Reg::R10, FReg::Xmm0, *lane as u8);
                }
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            AsmInstructionKind::InsertLane { vector, lane, value } => {
                let result_ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for insert_lane"))?;
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(result_ty) == 16) {
                    return Err(Error::from("insert_lane expects 128-bit vector result"));
                }
                if *lane > 1 {
                    return Err(Error::from("insert_lane lane out of range"));
                }

                let vector_ty = value_type(vector, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::Xmm0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
                emit_pinsrq_xmm_r64_imm8(asm, FReg::Xmm0, Reg::R10, *lane as u8);
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, result_ty)?;
            }
            AsmInstructionKind::ZipLow { lhs, rhs, lane_bits } => {
                let result_ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for zip_low"))?;
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(result_ty) == 16) {
                    return Err(Error::from("zip_low expects 128-bit vector result"));
                }
                if !matches!(*lane_bits, 16 | 32 | 64) {
                    return Err(Error::from(
                        "x86_64 zip_low only supports 16/32/64-bit lanes for now",
                    ));
                }

                let lhs_ty = value_type(lhs, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    lhs,
                    FReg::Xmm0,
                    &lhs_ty,
                    reg_types,
                    local_types,
                )?;
                let rhs_ty = value_type(rhs, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    rhs,
                    FReg::Xmm1,
                    &rhs_ty,
                    reg_types,
                    local_types,
                )?;
                match *lane_bits {
                    16 => emit_punpcklwd_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                    32 => emit_punpckldq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                    _ => emit_punpcklqdq_xmm_xmm(asm, FReg::Xmm0, FReg::Xmm1),
                }
                store_vreg_float(asm, layout, inst.id, FReg::Xmm0, result_ty)?;
            }
            AsmInstructionKind::And(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::And,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Or(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Or,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Xor(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Xor,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Shl(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Left,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Shr(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Right,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Eq(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Eq,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ne(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ne,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Lt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Lt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Le(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Le,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Gt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Gt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ge(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ge,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Div(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                false,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Rem(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                true,
                reg_types,
                local_types,
                format,
            )?,
            AsmInstructionKind::Not(value) => {
                emit_not(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::Alloca { .. } => {
                let offset = alloca_offset(layout, inst.id)?;
                emit_mov_rr(asm, Reg::R10, Reg::Rbp);
                emit_add_ri32(asm, Reg::R10, offset);
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            AsmInstructionKind::Load { address, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for load"))?;
                emit_load(asm, layout, inst.id, address, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::Store { value, address, .. } => {
                emit_store(
                    asm,
                    layout,
                    value,
                    address,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
                emit_gep(asm, layout, inst.id, ptr, indices, reg_types, local_types)?;
            }
            AsmInstructionKind::Call { function, args, .. } => {
                let ty = inst.type_hint.as_ref().cloned().unwrap_or(AsmType::Void);
                emit_call(
                    asm,
                    layout,
                    inst.id,
                    function,
                    args,
                    func_map,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::Syscall {
                convention,
                number,
                args,
            } => {
                let ty = inst.type_hint.as_ref().cloned().unwrap_or(AsmType::I64);
                emit_syscall(
                    asm,
                    layout,
                    inst.id,
                    *convention,
                    number,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?;
            }
            AsmInstructionKind::IntrinsicCall {
                kind,
                format: format_str,
                args,
            } => {
                let ty = inst.type_hint.as_ref().cloned().unwrap_or(AsmType::Void);
                emit_intrinsic_call(
                    asm,
                    layout,
                    inst.id,
                    kind,
                    format_str,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                    format,
                )?;
            }
            AsmInstructionKind::SIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::UIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::Trunc(value, ty) => {
                emit_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::ZExt(value, ty) => {
                emit_zext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPToSI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::FPToUI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::FPTrunc(value, ty) => {
                emit_fp_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPExt(value, ty) => {
                emit_fp_ext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SExt(value, ty) => {
                emit_sext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SextOrTrunc(value, ty) => {
                emit_sext_or_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::Bitcast(value, ty) => {
                emit_bitcast(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::PtrToInt(value) => {
                emit_ptr_to_int(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::IntToPtr(value) => {
                emit_int_to_ptr(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                emit_insert_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    element,
                    indices,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::ExtractValue { aggregate, indices } => {
                emit_extract_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    indices,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                emit_select(
                    asm,
                    layout,
                    inst.id,
                    condition,
                    if_true,
                    if_false,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::LandingPad { result_type, .. } => {
                emit_landingpad(asm, layout, inst.id, result_type)?;
            }
            AsmInstructionKind::Freeze(value) => {
                emit_freeze(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InlineAsm { output_type, .. } => {
                emit_inline_asm(asm, layout, inst.id, output_type)?;
            }
            AsmInstructionKind::Unreachable => {
                emit_trap(asm);
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported LIR instruction for x86_64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        AsmTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm64(asm, Reg::Rax, 0);
                emit_ret(asm);
            }
        }
        AsmTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if returns_aggregate(return_ty) {
                let sret_offset = layout
                    .sret_offset
                    .ok_or_else(|| Error::from("missing sret pointer for aggregate return"))?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, sret_offset);
                match value {
                    AsmValue::Register(id) => {
                        let src_offset = agg_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::R11, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Local(id) => {
                        let src_offset = local_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::R11, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Constant(constant) => {
                        store_constant_aggregate_to_reg(
                            asm,
                            Reg::R11,
                            constant,
                            return_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                    _ => return Err(Error::from("unsupported aggregate return value")),
                }
                if asm.needs_frame {
                    emit_epilogue(asm);
                }
                emit_ret(asm);
                return Ok(());
            }
            if matches!(return_ty, AsmType::I128) {
                load_i128_value(
                    asm,
                    layout,
                    value,
                    Reg::Rax,
                    Reg::Rdx,
                    reg_types,
                    local_types,
                )?;
                exit_reg = Some(Reg::Rax);
            } else if is_float_type(return_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::Xmm0,
                    return_ty,
                    reg_types,
                    local_types,
                )?;
            } else {
                load_value(asm, layout, value, Reg::Rax, reg_types, local_types)?;
                exit_reg = Some(Reg::Rax);
            }
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                if let Some(reg) = exit_reg {
                    emit_exit_syscall_reg(asm, reg)?;
                } else {
                    emit_exit_syscall(asm, 0)?;
                }
            } else {
                emit_ret(asm);
            }
        }
        AsmTerminator::Br(target) => {
            asm.emit_jmp(Label::Block(asm.current_function, *target));
        }
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            emit_cond_branch(
                asm,
                layout,
                condition,
                Label::Block(asm.current_function, *if_true),
                Label::Block(asm.current_function, *if_false),
            )?;
        }
        AsmTerminator::Invoke {
            function,
            args,
            normal_dest,
            ..
        } => {
            emit_call(
                asm,
                layout,
                0,
                function,
                args,
                func_map,
                &AsmType::Void,
                reg_types,
                local_types,
                format,
                rodata,
                rodata_pool,
            )?;
            asm.emit_jmp(Label::Block(asm.current_function, *normal_dest));
        }
        AsmTerminator::Switch {
            value,
            default,
            cases,
        } => {
            emit_switch(asm, layout, value, *default, cases, reg_types, local_types)?;
        }
        AsmTerminator::Unreachable => {
            emit_trap(asm);
        }
        _ => {
            return Err(Error::from("unsupported terminator for x86_64"));
        }
    }

    Ok(())
}

enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

fn emit_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_cmp(asm, layout, dst_id, lhs, rhs, kind, reg_types, local_types);
    }
    if is_float_type(&lhs_ty) {
        emit_float_cmp(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            kind,
            &lhs_ty,
            reg_types,
            local_types,
        )?;
        return Ok(());
    }
    match (lhs, rhs) {
        (AsmValue::Register(_), AsmValue::Register(_))
        | (AsmValue::Register(_), AsmValue::Constant(_))
        | (AsmValue::Constant(_), AsmValue::Register(_))
        | (AsmValue::Constant(_), AsmValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        AsmValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
        AsmValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            let imm32 =
                i32::try_from(imm).map_err(|_| Error::from("cmp immediate out of range"))?;
            emit_cmp_imm32(asm, Reg::R10, imm32);
        }
        _ => {}
    }

    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0xC,
        CmpKind::Le => 0xE,
        CmpKind::Gt => 0xF,
        CmpKind::Ge => 0xD,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, Reg::R10, Reg::R11);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_i128_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_i128_value(asm, layout, lhs, Reg::R10, Reg::R11, reg_types, local_types)?;
    load_i128_value(asm, layout, rhs, Reg::Rax, Reg::Rdx, reg_types, local_types)?;

    emit_cmp_rr(asm, Reg::R11, Reg::Rdx);
    emit_setcc(asm, 0xC, Reg::R8); // signed lt
    emit_setcc(asm, 0xF, Reg::R9); // signed gt
    emit_setcc(asm, 0x4, Reg::Rcx); // eq

    emit_cmp_rr(asm, Reg::R10, Reg::Rax);
    emit_setcc(asm, 0x2, Reg::R10); // unsigned lt
    emit_setcc(asm, 0x7, Reg::R11); // unsigned gt
    emit_setcc(asm, 0x4, Reg::Rdx); // eq

    match kind {
        CmpKind::Eq => {
            emit_and_rr(asm, Reg::Rcx, Reg::Rdx);
            store_vreg(asm, layout, dst_id, Reg::Rcx)?;
        }
        CmpKind::Ne => {
            emit_and_rr(asm, Reg::Rcx, Reg::Rdx);
            emit_mov_imm64(asm, Reg::R8, 1);
            emit_xor_rr(asm, Reg::Rcx, Reg::R8);
            store_vreg(asm, layout, dst_id, Reg::Rcx)?;
        }
        CmpKind::Lt => {
            emit_and_rr(asm, Reg::Rcx, Reg::R10);
            emit_or_rr(asm, Reg::R8, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R8)?;
        }
        CmpKind::Gt => {
            emit_and_rr(asm, Reg::Rcx, Reg::R11);
            emit_or_rr(asm, Reg::R9, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R9)?;
        }
        CmpKind::Le => {
            emit_or_rr(asm, Reg::R10, Reg::Rdx);
            emit_and_rr(asm, Reg::Rcx, Reg::R10);
            emit_or_rr(asm, Reg::R8, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R8)?;
        }
        CmpKind::Ge => {
            emit_or_rr(asm, Reg::R11, Reg::Rdx);
            emit_and_rr(asm, Reg::Rcx, Reg::R11);
            emit_or_rr(asm, Reg::R9, Reg::Rcx);
            store_vreg(asm, layout, dst_id, Reg::R9)?;
        }
    }
    Ok(())
}

fn emit_select(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    condition: &AsmValue,
    if_true: &AsmValue,
    if_false: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let result_ty =
        reg_types
            .get(&dst_id)
            .cloned()
            .unwrap_or(value_type(if_true, reg_types, local_types)?);
    if is_float_type(&result_ty) {
        return Err(Error::from(
            "Select does not support float values on x86_64",
        ));
    }

    load_value(asm, layout, condition, Reg::R11, reg_types, local_types)?;
    emit_cmp_imm32(asm, Reg::R11, 0);
    load_value(asm, layout, if_true, Reg::R10, reg_types, local_types)?;
    load_value(asm, layout, if_false, Reg::Rax, reg_types, local_types)?;
    emit_cmovcc(asm, 0x4, Reg::R10, Reg::Rax);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_cond_branch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    condition: &AsmValue,
    if_true: Label,
    if_false: Label,
) -> Result<()> {
    match condition {
        AsmValue::Constant(AsmConstant::Bool(value)) => {
            if *value {
                asm.emit_jmp(if_true);
            } else {
                asm.emit_jmp(if_false);
            }
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            emit_cmp_imm32(asm, Reg::R10, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        AsmValue::Flags(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            emit_cmp_imm32(asm, Reg::R10, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}

fn emit_switch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    default: BasicBlockId,
    cases: &[(u64, BasicBlockId)],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    for (case_val, target) in cases {
        if *case_val <= i32::MAX as u64 {
            emit_cmp_imm32(asm, Reg::R10, *case_val as i32);
        } else {
            emit_mov_imm64(asm, Reg::R11, *case_val);
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
        asm.emit_jcc(0x84, Label::Block(asm.current_function, *target));
    }
    asm.emit_jmp(Label::Block(asm.current_function, default));
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
}

fn emit_prologue(asm: &mut Assembler, layout: &FrameLayout) -> Result<()> {
    asm.push(0x55);
    emit_rex(asm, true, Reg::Rbp.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, Reg::Rbp.id(), Reg::Rsp.id());

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
            let rel32 = i32::try_from(rel).map_err(|_| Error::from("jump out of range"))?;
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

fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &AsmValue,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if matches!(ty, AsmType::I128) {
        match address {
            AsmValue::StackSlot(id) => {
                let offset = stack_slot_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset + 8);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                emit_mov_rm64(asm, Reg::R11, Reg::R11, 8);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                emit_mov_rm64(asm, Reg::R11, Reg::R11, 8);
            }
            _ => return Err(Error::from("unsupported load address for i128 on x86_64")),
        }
        store_i128_value(asm, layout, dst_id, Reg::R10, Reg::R11)?;
        return Ok(());
    }
    if is_large_aggregate(ty) {
        let size = size_of(ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let dst_offset = agg_offset(layout, dst_id)?;
        match address {
            AsmValue::StackSlot(id) => {
                let src_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_reg_to_sp(asm, Reg::R11, dst_offset, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_reg_to_sp(asm, Reg::R11, dst_offset, size)?;
            }
            _ => return Err(Error::from("unsupported load address for x86_64")),
        }
        emit_mov_rr(asm, Reg::R10, Reg::Rbp);
        emit_add_ri32(asm, Reg::R10, dst_offset);
        store_vreg(asm, layout, dst_id, Reg::R10)?;
        return Ok(());
    }
    match address {
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, offset),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        _ => {
            let addr_ty = value_type(address, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported load address for x86_64: {:?}",
                addr_ty
            )))
        }
    }
}

fn emit_divrem(
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
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if is_float_type(&lhs_ty) {
        if want_rem {
            load_value_float(
                asm,
                layout,
                lhs,
                FReg::Xmm0,
                &lhs_ty,
                reg_types,
                local_types,
            )?;
            load_value_float(
                asm,
                layout,
                rhs,
                FReg::Xmm1,
                &lhs_ty,
                reg_types,
                local_types,
            )?;
            let symbol = if matches!(lhs_ty, AsmType::F32) {
                "fmodf"
            } else {
                "fmod"
            };
            asm.emit_call_external(symbol);
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &lhs_ty)?;
            return Ok(());
        }
        emit_float_div(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            &lhs_ty,
            reg_types,
            local_types,
        )?;
        return Ok(());
    }
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_divrem(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            want_rem,
            reg_types,
            local_types,
            format,
        );
    }

    load_value(asm, layout, lhs, Reg::Rax, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;

    emit_cqo(asm);
    emit_idiv_reg(asm, Reg::R11);

    let src = if want_rem { Reg::Rdx } else { Reg::Rax };
    store_vreg(asm, layout, dst_id, src)?;

    Ok(())
}

enum CallTarget {
    Internal(u32),
    External(String),
    Indirect,
}

const SYSV_INT_ARGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];
const SYSV_FLOAT_ARGS: [FReg; 8] = [
    FReg::Xmm0,
    FReg::Xmm1,
    FReg::Xmm2,
    FReg::Xmm3,
    FReg::Xmm4,
    FReg::Xmm5,
    FReg::Xmm6,
    FReg::Xmm7,
];
const WIN_INT_ARGS: [Reg; 4] = [Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9];
const WIN_FLOAT_ARGS: [FReg; 4] = [FReg::Xmm0, FReg::Xmm1, FReg::Xmm2, FReg::Xmm3];

const SYSCALL_ARGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::R10, Reg::R8, Reg::R9];

fn call_abi(format: TargetFormat) -> (&'static [Reg], &'static [FReg], bool) {
    match format {
        TargetFormat::Coff => (&WIN_INT_ARGS, &WIN_FLOAT_ARGS, false),
        _ => (&SYSV_INT_ARGS, &SYSV_FLOAT_ARGS, true),
    }
}

fn emit_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    function: &AsmValue,
    args: &[AsmValue],
    func_map: &HashMap<String, u32>,
    ret_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let target = match function {
        AsmValue::Function(name) => func_map
            .get(name)
            .copied()
            .map(CallTarget::Internal)
            .unwrap_or_else(|| CallTarget::External(name.clone())),
        AsmValue::Register(_) | AsmValue::Local(_) | AsmValue::StackSlot(_) => CallTarget::Indirect,
        _ => return Err(Error::from("unsupported callee for x86_64")),
    };

    let (arg_regs, float_regs, use_al) = call_abi(format);

    let needs_sret = returns_aggregate(ret_ty);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    let mut sret_offset = None;
    if needs_sret {
        let agg_off = agg_offset(layout, dst_id)?;
        emit_mov_rr(asm, arg_regs[0], Reg::Rbp);
        emit_add_ri32(asm, arg_regs[0], agg_off);
        int_idx = 1;
        sret_offset = Some(agg_off);
    }

    for arg in args {
        if let AsmValue::Constant(AsmConstant::String(text)) = arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            if int_idx < arg_regs.len() {
                asm.emit_mov_imm64_reloc(arg_regs[int_idx], ".rodata", offset as i64);
                int_idx += 1;
            } else {
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                emit_mov_mr64_sp(asm, offset, Reg::R10);
                stack_idx += 1;
            }
            continue;
        }
        let arg_ty = value_type(arg, reg_types, local_types)?;
        if matches!(arg_ty, AsmType::I128) {
            load_i128_value(asm, layout, arg, Reg::R10, Reg::R11, reg_types, local_types)?;
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
            continue;
        }
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(
                    asm,
                    layout,
                    arg,
                    float_regs[float_idx],
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                float_idx += 1;
            } else {
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
            int_idx += 1;
        } else {
            let offset = layout.shadow_space + (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
            stack_idx += 1;
        }
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    match target {
        CallTarget::Internal(target) => asm.emit_call(Label::Function(target)),
        CallTarget::External(symbol) => asm.emit_call_external(&symbol),
        CallTarget::Indirect => {
            load_value(asm, layout, function, Reg::R11, reg_types, local_types)?;
            asm.emit_call_reg(Reg::R11);
        }
    }

    if needs_sret {
        if let Some(agg_off) = sret_offset {
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, agg_off);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
    } else if matches!(ret_ty, AsmType::I128) {
        store_i128_value(asm, layout, dst_id, Reg::Rax, Reg::Rdx)?;
    } else if !matches!(ret_ty, AsmType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::Rax)?;
        }
    }

    Ok(())
}

fn emit_intrinsic_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    kind: &AsmIntrinsicKind,
    format: &str,
    args: &[AsmValue],
    result_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
    target_format: TargetFormat,
) -> Result<()> {
    match kind {
        AsmIntrinsicKind::Print | AsmIntrinsicKind::Println => {}
        AsmIntrinsicKind::Format => {
            if !matches!(result_ty, AsmType::Ptr(_)) {
                return Err(Error::from("Format expects pointer result"));
            }
            let format_offset = intern_cstring(rodata, rodata_pool, format);
            let (arg_regs, float_regs, use_al) = call_abi(target_format);

            let mut int_idx = 0usize;
            let mut float_idx = 0usize;
            let mut stack_idx = 0usize;

            push_int_arg(asm, layout, 0, &mut int_idx, &mut stack_idx, arg_regs)?;
            push_int_arg(asm, layout, 0, &mut int_idx, &mut stack_idx, arg_regs)?;
            push_rodata_arg(
                asm,
                layout,
                format_offset,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            for arg in args {
                push_value_arg(
                    asm,
                    layout,
                    arg,
                    &mut int_idx,
                    &mut float_idx,
                    &mut stack_idx,
                    arg_regs,
                    float_regs,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }

            if use_al {
                emit_mov_al_imm8(asm, float_idx as u8);
            }

            asm.emit_call_external("snprintf");

            store_vreg(asm, layout, dst_id, Reg::Rax)?;
            emit_mov_rr(asm, Reg::R10, Reg::Rax);
            emit_add_ri32(asm, Reg::R10, 1);
            if arg_regs[0] != Reg::R10 {
                emit_mov_rr(asm, arg_regs[0], Reg::R10);
            }
            asm.emit_call_external("malloc");

            let len_offset = vreg_offset(layout, dst_id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, len_offset);
            emit_add_ri32(asm, Reg::R10, 1);
            store_vreg(asm, layout, dst_id, Reg::Rax)?;

            int_idx = 0usize;
            float_idx = 0usize;
            stack_idx = 0usize;
            push_value_arg(
                asm,
                layout,
                &AsmValue::Register(dst_id),
                &mut int_idx,
                &mut float_idx,
                &mut stack_idx,
                arg_regs,
                float_regs,
                reg_types,
                local_types,
                rodata,
                rodata_pool,
            )?;
            push_reg_arg(
                asm,
                layout,
                Reg::R10,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            push_rodata_arg(
                asm,
                layout,
                format_offset,
                &mut int_idx,
                &mut stack_idx,
                arg_regs,
            )?;
            for arg in args {
                push_value_arg(
                    asm,
                    layout,
                    arg,
                    &mut int_idx,
                    &mut float_idx,
                    &mut stack_idx,
                    arg_regs,
                    float_regs,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }

            if use_al {
                emit_mov_al_imm8(asm, float_idx as u8);
            }

            asm.emit_call_external("snprintf");
            return Ok(());
        }
        AsmIntrinsicKind::TimeNow => {
            if !is_float_type(result_ty) {
                return Err(Error::from("TimeNow expects floating-point result"));
            }
            let (arg_regs, _, _) = call_abi(target_format);
            emit_mov_imm64(asm, arg_regs[0], 0);
            asm.emit_call_external("time");
            emit_cvtsi2sd(asm, FReg::Xmm0, Reg::Rax, result_ty);
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, result_ty)?;
            return Ok(());
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    let (arg_regs, float_regs, use_al) = call_abi(target_format);

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    push_rodata_arg(
        asm,
        layout,
        format_offset,
        &mut int_idx,
        &mut stack_idx,
        arg_regs,
    )?;
    for arg in args {
        push_value_arg(
            asm,
            layout,
            arg,
            &mut int_idx,
            &mut float_idx,
            &mut stack_idx,
            arg_regs,
            float_regs,
            reg_types,
            local_types,
            rodata,
            rodata_pool,
        )?;
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    asm.emit_call_external("printf");
    Ok(())
}

fn push_stack_qword(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    src: Reg,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    emit_mov_mr64_sp(asm, offset, src);
    Ok(())
}

fn push_int_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: i64,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        emit_mov_imm64(asm, arg_regs[*int_idx], value as u64);
        *int_idx += 1;
    } else {
        emit_mov_imm64(asm, Reg::R10, value as u64);
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, Reg::R10)?;
        *stack_idx += 1;
    }
    Ok(())
}

fn push_rodata_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: u64,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        asm.emit_mov_imm64_reloc(arg_regs[*int_idx], ".rodata", offset as i64);
        *int_idx += 1;
    } else {
        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, Reg::R10)?;
        *stack_idx += 1;
    }
    Ok(())
}

fn push_reg_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    reg: Reg,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        if arg_regs[*int_idx] != reg {
            emit_mov_rr(asm, arg_regs[*int_idx], reg);
        }
        *int_idx += 1;
    } else {
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        push_stack_qword(asm, layout, offset, reg)?;
        *stack_idx += 1;
    }
    Ok(())
}

fn push_value_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    arg: &AsmValue,
    int_idx: &mut usize,
    float_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
    float_regs: &[FReg],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    if let AsmValue::Constant(AsmConstant::String(text)) = arg {
        let offset = intern_cstring(rodata, rodata_pool, text);
        return push_rodata_arg(asm, layout, offset, int_idx, stack_idx, arg_regs);
    }

    let arg_ty = value_type(arg, reg_types, local_types)?;
    if is_float_type(&arg_ty) {
        if *float_idx < float_regs.len() {
            load_value_float(
                asm,
                layout,
                arg,
                float_regs[*float_idx],
                &arg_ty,
                reg_types,
                local_types,
            )?;
            *float_idx += 1;
        } else {
            let offset = layout.shadow_space + (*stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
            *stack_idx += 1;
        }
    } else if *int_idx < arg_regs.len() {
        load_value(asm, layout, arg, arg_regs[*int_idx], reg_types, local_types)?;
        *int_idx += 1;
    } else {
        let offset = layout.shadow_space + (*stack_idx as i32) * 8;
        store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
        *stack_idx += 1;
    }

    Ok(())
}

fn store_outgoing_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    let ty = value_type(value, reg_types, local_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &ty, reg_types, local_types)?;
        emit_movsd_m64x_sp(asm, offset, FReg::Xmm0, &ty);
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
        emit_mov_mr64_sp(asm, offset, Reg::R10);
    }
    Ok(())
}

fn intern_cstring(rodata: &mut Vec<u8>, pool: &mut HashMap<String, u64>, text: &str) -> u64 {
    if let Some(offset) = pool.get(text) {
        return *offset;
    }
    align_rodata(rodata, 8);
    let offset = rodata.len() as u64;
    rodata.extend_from_slice(text.as_bytes());
    rodata.push(0);
    pool.insert(text.to_string(), offset);
    offset
}

fn align_rodata(rodata: &mut Vec<u8>, align: usize) {
    while rodata.len() % align != 0 {
        rodata.push(0);
    }
}

fn emit_store(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    address: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    if let AsmValue::Constant(AsmConstant::String(text)) = value {
        let offset = intern_cstring(rodata, rodata_pool, text);
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if let AsmValue::Constant(AsmConstant::Array(values, elem_ty)) = value {
        if values.is_empty() {
            return Ok(());
        }
        let elem_ty = match elem_ty {
            AsmType::Array(elem, _) => elem.as_ref(),
            other => other,
        };
        let elem_size = size_of(elem_ty) as i32;
        let store_elem = |asm: &mut Assembler, base: Reg, offset: i32| -> Result<()> {
            match elem_size {
                1 => emit_mov_mr8(asm, base, offset, Reg::R10),
                2 => emit_mov_mr16(asm, base, offset, Reg::R10),
                4 => emit_mov_mr32(asm, base, offset, Reg::R10),
                8 => emit_mov_mr64(asm, base, offset, Reg::R10),
                _ => {
                    return Err(Error::from(
                        "unsupported array element size in constant store",
                    ));
                }
            }
            Ok(())
        };
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                for (idx, elem) in values.iter().enumerate() {
                    let offset = dst_offset + (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    store_elem(asm, Reg::Rbp, offset)?;
                }
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    store_elem(asm, Reg::Rax, 0)?;
                }
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        AsmConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    store_elem(asm, Reg::Rax, 0)?;
                }
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if matches!(value, AsmValue::Constant(AsmConstant::Array(values, _)) if values.is_empty()) {
        return Ok(());
    }
    let value_ty = value_type(value, reg_types, local_types)?;
    if size_of(&value_ty) == 0 {
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
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
                emit_mov_mr64(asm, Reg::Rbp, dst_offset + 8, Reg::R11);
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::Rcx, Reg::Rbp, addr_offset);
                emit_mov_mr64(asm, Reg::Rcx, 0, Reg::R10);
                emit_mov_mr64(asm, Reg::Rcx, 8, Reg::R11);
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::Rcx, Reg::Rbp, addr_offset);
                emit_mov_mr64(asm, Reg::Rcx, 0, Reg::R10);
                emit_mov_mr64(asm, Reg::Rcx, 8, Reg::R11);
            }
            _ => return Err(Error::from("unsupported store address for i128 on x86_64")),
        }
        return Ok(());
    }
    if let AsmValue::Constant(constant) = value {
        if matches!(
            constant,
            AsmConstant::Struct(_, _) | AsmConstant::Array(_, _)
        ) && size_of(&value_ty) <= 8
        {
            let bits = pack_small_aggregate(constant, &value_ty)?;
            emit_mov_imm64(asm, Reg::R10, bits);
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
    }
    if is_large_aggregate(&value_ty) {
        let size = size_of(&value_ty) as i32;
        if let AsmValue::Constant(AsmConstant::Struct(values, ty)) = value {
            let fields = match ty {
                AsmType::Struct { fields, .. } => fields,
                _ => return Err(Error::from("expected struct type for constant store")),
            };
            let struct_layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        match field {
                            AsmConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                                emit_mov_imm64(asm, Reg::R10, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::R10, bits);
                            }
                        }
                        let store_offset = dst_offset + field_offset as i32;
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        match field {
                            AsmConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                                emit_mov_imm64(asm, Reg::R10, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::R10, bits);
                            }
                        }
                        emit_mov_rr(asm, Reg::Rax, Reg::R11);
                        emit_add_ri32(asm, Reg::Rax, field_offset as i32);
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rax, 0, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rax, 0, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rax, 0, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    for (idx, field) in values.iter().enumerate() {
                        let field_offset = *struct_layout
                            .field_offsets
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_ty = fields
                            .get(idx)
                            .ok_or_else(|| Error::from("aggregate field out of range"))?;
                        let field_size = size_of(field_ty);
                        match field {
                            AsmConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                                emit_mov_imm64(asm, Reg::R10, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::R10, bits);
                            }
                        }
                        emit_mov_rr(asm, Reg::Rax, Reg::R11);
                        emit_add_ri32(asm, Reg::Rax, field_offset as i32);
                        match field_size {
                            1 => emit_mov_mr8(asm, Reg::Rax, 0, Reg::R10),
                            2 => emit_mov_mr16(asm, Reg::Rax, 0, Reg::R10),
                            4 => emit_mov_mr32(asm, Reg::Rax, 0, Reg::R10),
                            8 => emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        if matches!(value, AsmValue::Constant(AsmConstant::Undef(_))) {
            match address {
                AsmValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    zero_sp_range(asm, dst_offset, size)?;
                }
                AsmValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                AsmValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        let src_offset = match value {
            AsmValue::Register(id) => agg_offset(layout, *id)?,
            AsmValue::Local(id) => local_offset(layout, *id)?,
            _ => {
                return Err(Error::from(format!(
                    "unsupported aggregate store value: {:?}",
                    value
                )));
            }
        };
        match address {
            AsmValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            AsmValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            AsmValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
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
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    }

    match address {
        AsmValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::Rbp, offset, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, offset, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Register(id) => {
            let addr_offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        AsmValue::Local(id) => {
            let addr_offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for x86_64")),
    }
}

fn emit_mov_rm64(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x8B);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movzx_rm8(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsx_rm8(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xBE);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsx_rm16(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xBF);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsxd_rm32(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x63);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_mov_mr64(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_mov_mr32(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_mov_mr16(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    asm.push(0x66);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_mov_mr8(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x88);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_mov_mr64_sp(asm: &mut Assembler, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

fn emit_movsd_m64x_sp(asm: &mut Assembler, disp: i32, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), Reg::Rsp.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

fn emit_movsd_xm64(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x10);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsd_m64x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_movq_xmm_r64(asm: &mut Assembler, dst: FReg, src: Reg) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x6E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_movq_r64_xmm(asm: &mut Assembler, dst: Reg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x0F);
    asm.push(0x7E);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_movdqu_xm128(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32) {
    asm.push(0xF3);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x6F);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movdqu_m128x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg) {
    asm.push(0xF3);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x0F);
    asm.push(0x7F);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_punpcklqdq_xmm_xmm(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x6C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_punpcklwd_xmm_xmm(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x61);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_pinsrq_xmm_r64_imm8(asm: &mut Assembler, dst: FReg, src: Reg, imm: u8) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x3A);
    asm.push(0x22);
    emit_modrm(asm, 0b11, dst.id(), src.id());
    asm.push(imm);
}

fn emit_addsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x58);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_subsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_mulsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x59);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_divsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_ucomisd(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &AsmType) {
    if matches!(ty, AsmType::F64) {
        asm.push(0x66);
    }
    emit_rex(asm, false, lhs.id(), rhs.id());
    asm.push(0x0F);
    asm.push(0x2E);
    emit_modrm(asm, 0b11, lhs.id(), rhs.id());
}

fn emit_cvtsi2sd(asm: &mut Assembler, dst: FReg, src: Reg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvttsd2si(asm: &mut Assembler, dst: Reg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvtsd2ss(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF2);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvtss2sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF3);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_mov_al_imm8(asm: &mut Assembler, imm: u8) {
    asm.push(0xB0);
    asm.push(imm);
}

fn emit_int_to_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    signed: bool,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_integer_type(&src_ty) {
        return Err(Error::from("int to float expects integer source"));
    }
    if matches!(src_ty, AsmType::I128) {
        return Err(Error::from("i128 to float is not supported on x86_64"));
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if signed {
        emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R10, dst_ty);
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
        return Ok(());
    }
    emit_uint_to_float(asm, Reg::R10, dst_ty)?;
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_uint_to_float(asm: &mut Assembler, src: Reg, dst_ty: &AsmType) -> Result<()> {
    emit_mov_rr(asm, Reg::R11, src);
    emit_shr_imm8(asm, Reg::R11, 1);
    emit_and_ri32(asm, src, 1);
    emit_or_rr(asm, Reg::R11, src);
    emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R11, &AsmType::F64);
    emit_addsd(asm, FReg::Xmm0, FReg::Xmm0, &AsmType::F64);
    if matches!(dst_ty, AsmType::F32) {
        emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    }
    Ok(())
}

fn emit_float_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    _signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    if matches!(dst_ty, AsmType::I128) {
        return Err(Error::from("i128 from float is not supported on x86_64"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvttsd2si(asm, Reg::R10, FReg::Xmm0, &src_ty);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_fp_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (AsmType::F64, AsmType::F32)) {
        return Err(Error::from("unsupported FPTrunc on x86_64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_fp_ext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (AsmType::F32, AsmType::F64)) {
        return Err(Error::from("unsupported FPExt on x86_64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::Xmm0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_cvtss2sd(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_gep(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    ptr: &AsmValue,
    indices: &[AsmValue],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let ptr_ty = value_type(ptr, reg_types, local_types)?;
    let mut current_ty = match ptr_ty {
        AsmType::Ptr(inner) => *inner,
        _ => return Err(Error::from("GEP expects pointer base type")),
    };

    load_value(asm, layout, ptr, Reg::R11, reg_types, local_types)?;
    for index in indices {
        match &current_ty {
            AsmType::Struct { fields, .. } => {
                let idx = match index {
                    AsmValue::Constant(constant) => {
                        let raw = constant_to_i64(constant)?;
                        usize::try_from(raw)
                            .map_err(|_| Error::from("GEP struct index out of range"))?
                    }
                    _ => return Err(Error::from("GEP struct index must be constant")),
                };
                let layout = struct_layout(&current_ty)
                    .ok_or_else(|| Error::from("missing struct layout for GEP"))?;
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
                add_immediate_offset(asm, Reg::R11, field_offset as i64)?;
                current_ty = fields
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
            }
            AsmType::Array(elem, _) | AsmType::Vector(elem, _) => {
                emit_scaled_index(
                    asm,
                    layout,
                    index,
                    size_of(elem) as u64,
                    reg_types,
                    local_types,
                )?;
                current_ty = *elem.clone();
            }
            _ => {
                emit_scaled_index(
                    asm,
                    layout,
                    index,
                    size_of(&current_ty) as u64,
                    reg_types,
                    local_types,
                )?;
            }
        }
    }

    store_vreg(asm, layout, dst_id, Reg::R11)?;
    Ok(())
}

fn emit_scaled_index(
    asm: &mut Assembler,
    layout: &FrameLayout,
    index: &AsmValue,
    elem_size: u64,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if elem_size == 0 {
        return Ok(());
    }
    load_value(asm, layout, index, Reg::R10, reg_types, local_types)?;
    if elem_size != 1 {
        emit_mov_imm64(asm, Reg::Rax, elem_size);
        emit_imul_rr(asm, Reg::R10, Reg::Rax);
    }
    emit_add_rr(asm, Reg::R11, Reg::R10);
    Ok(())
}

fn add_immediate_offset(asm: &mut Assembler, base: Reg, offset: i64) -> Result<()> {
    if offset == 0 {
        return Ok(());
    }
    let imm = i32::try_from(offset).map_err(|_| Error::from("GEP offset too large for x86_64"))?;
    emit_add_ri32(asm, base, imm);
    Ok(())
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

fn aggregate_field_offset(ty: &AsmType, indices: &[u32]) -> Result<(i64, AsmType)> {
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

fn copy_sp_to_sp(asm: &mut Assembler, src: i32, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_mr32(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_mr16(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_mr8(asm, Reg::Rbp, dst + offset, Reg::R10);
    }
    Ok(())
}

fn copy_sp_to_reg(asm: &mut Assembler, src: i32, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr32(asm, Reg::R11, 0, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr16(asm, Reg::R11, 0, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr8(asm, Reg::R11, 0, Reg::R10);
    }
    Ok(())
}

fn copy_reg_to_sp(asm: &mut Assembler, src: Reg, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0);
        emit_mov_mr32(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0);
        emit_mov_mr16(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0);
        emit_mov_mr8(asm, Reg::Rbp, dst + offset, Reg::R10);
    }
    Ok(())
}

#[allow(dead_code)]
fn copy_reg_to_reg(asm: &mut Assembler, src: Reg, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr32(asm, Reg::R11, 0, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr16(asm, Reg::R11, 0, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr8(asm, Reg::R11, 0, Reg::R10);
    }
    Ok(())
}

fn zero_sp_range(asm: &mut Assembler, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset + 8 <= size {
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_mr32(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_mr16(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_mr8(asm, Reg::Rbp, dst + offset, Reg::R10);
    }
    Ok(())
}

fn zero_reg_range(asm: &mut Assembler, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset + 8 <= size {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr32(asm, Reg::R11, 0, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr16(asm, Reg::R11, 0, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr8(asm, Reg::R11, 0, Reg::R10);
    }
    Ok(())
}

fn store_constant_aggregate_to_reg(
    asm: &mut Assembler,
    base: Reg,
    constant: &AsmConstant,
    agg_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let size = size_of(agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    match constant {
        AsmConstant::Undef(_) | AsmConstant::Null(_) => return zero_reg_range(asm, base, size),
        AsmConstant::Int(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::UInt(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::Struct(values, _) => {
            let AsmType::Struct { fields, .. } = agg_ty else {
                return Err(Error::from("expected struct type for aggregate return"));
            };
            let layout = struct_layout(agg_ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate return"))?;
            for (idx, field) in values.iter().enumerate() {
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_ty = fields
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_size = size_of(field_ty);
                match field {
                    AsmConstant::String(text) => {
                        let offset = intern_cstring(rodata, rodata_pool, text);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                    }
                    AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                        emit_mov_imm64(asm, Reg::R10, 0);
                    }
                    other => {
                        let bits = constant_to_u64_bits(other)?;
                        emit_mov_imm64(asm, Reg::R10, bits);
                    }
                }
                let dst = field_offset as i32;
                match field_size {
                    1 => emit_mov_mr8(asm, base, dst, Reg::R10),
                    2 => emit_mov_mr16(asm, base, dst, Reg::R10),
                    4 => emit_mov_mr32(asm, base, dst, Reg::R10),
                    8 => emit_mov_mr64(asm, base, dst, Reg::R10),
                    _ => {
                        return Err(Error::from("unsupported aggregate field size in return"));
                    }
                }
            }
            Ok(())
        }
        AsmConstant::Array(values, elem_ty) => {
            let elem_ty = match agg_ty {
                AsmType::Array(elem, _) => elem.as_ref(),
                _ => elem_ty,
            };
            let elem_size = size_of(elem_ty) as i32;
            if elem_size == 0 {
                return Ok(());
            }
            if elem_size > 8 {
                return Err(Error::from("unsupported array element size in return"));
            }
            for (idx, elem) in values.iter().enumerate() {
                let offset = (idx as i32) * elem_size;
                match elem {
                    AsmConstant::String(text) => {
                        let ro_offset = intern_cstring(rodata, rodata_pool, text);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                    }
                    AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                        emit_mov_imm64(asm, Reg::R10, 0);
                    }
                    other => {
                        let bits = constant_to_u64_bits(other)?;
                        emit_mov_imm64(asm, Reg::R10, bits);
                    }
                }
                match elem_size {
                    1 => emit_mov_mr8(asm, base, offset, Reg::R10),
                    2 => emit_mov_mr16(asm, base, offset, Reg::R10),
                    4 => emit_mov_mr32(asm, base, offset, Reg::R10),
                    8 => emit_mov_mr64(asm, base, offset, Reg::R10),
                    _ => {
                        return Err(Error::from("unsupported array element size in return"));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from(format!(
            "unsupported aggregate constant for return: constant={:?} ty={:?}",
            constant, agg_ty
        ))),
    }
}

fn emit_bitcast(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &AsmValue,
    dst_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_size = size_of(&src_ty);
    let dst_size = size_of(dst_ty);
    if src_size != dst_size || src_size > 8 {
        return Err(Error::from("unsupported bitcast size for x86_64"));
    }
    if src_size == 0 {
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_insert_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    aggregate: &AsmValue,
    element: &AsmValue,
    indices: &[u32],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let agg_ty = value_type(aggregate, reg_types, local_types)?;
    if !is_large_aggregate(&agg_ty) {
        return Err(Error::from("InsertValue expects aggregate"));
    }
    let size = size_of(&agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    let dst_offset = agg_offset(layout, dst_id)?;

    match aggregate {
        AsmValue::Register(id) => {
            let src_offset = agg_offset(layout, *id)?;
            copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
        }
        AsmValue::Constant(AsmConstant::Undef(_)) => {
            zero_sp_range(asm, dst_offset, size)?;
        }
        _ => return Err(Error::from("unsupported InsertValue aggregate source")),
    }

    let (field_offset, field_ty) = aggregate_field_offset(&agg_ty, indices)?;
    let store_offset = dst_offset + field_offset as i32;
    if is_large_aggregate(&field_ty) {
        let field_size = size_of(&field_ty) as i32;
        if field_size == 0 {
            return Ok(());
        }
        match element {
            AsmValue::Register(id) => {
                let src_offset = agg_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            AsmValue::Local(id) => {
                let src_offset = local_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            AsmValue::Constant(AsmConstant::Struct(values, ty)) => {
                let fields = match ty {
                    AsmType::Struct { fields, .. } => fields,
                    _ => return Err(Error::from("expected struct type for InsertValue")),
                };
                let struct_layout = struct_layout(ty)
                    .ok_or_else(|| Error::from("missing struct layout for InsertValue"))?;
                for (idx, field) in values.iter().enumerate() {
                    let field_offset = *struct_layout
                        .field_offsets
                        .get(idx)
                        .ok_or_else(|| Error::from("aggregate field out of range"))?;
                    let field_ty = fields
                        .get(idx)
                        .ok_or_else(|| Error::from("aggregate field out of range"))?;
                    let field_size = size_of(field_ty);
                    match field {
                        AsmConstant::String(text) => {
                            let offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                        }
                        AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    let dst = store_offset + field_offset as i32;
                    match field_size {
                        1 => emit_mov_mr8(asm, Reg::Rbp, dst, Reg::R10),
                        2 => emit_mov_mr16(asm, Reg::Rbp, dst, Reg::R10),
                        4 => emit_mov_mr32(asm, Reg::Rbp, dst, Reg::R10),
                        8 => emit_mov_mr64(asm, Reg::Rbp, dst, Reg::R10),
                        _ => {
                            return Err(Error::from(
                                "unsupported aggregate field size in InsertValue",
                            ));
                        }
                    }
                }
            }
            AsmValue::Constant(AsmConstant::Undef(_)) => {
                zero_sp_range(asm, store_offset, field_size)?;
            }
            _ => return Err(Error::from("unsupported InsertValue aggregate element")),
        }
        emit_mov_rr(asm, Reg::R10, Reg::Rbp);
        emit_add_ri32(asm, Reg::R10, dst_offset);
        store_vreg(asm, layout, dst_id, Reg::R10)?;
        return Ok(());
    }
    if is_float_type(&field_ty) {
        load_value_float(
            asm,
            layout,
            element,
            FReg::Xmm0,
            &field_ty,
            reg_types,
            local_types,
        )?;
        emit_movsd_m64x(asm, Reg::Rbp, store_offset, FReg::Xmm0, &field_ty);
    } else {
        if let AsmValue::Constant(AsmConstant::String(text)) = element {
            let offset = intern_cstring(rodata, rodata_pool, text);
            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )));
                }
            }
        } else {
            load_value(asm, layout, element, Reg::R10, reg_types, local_types)?;
            match field_ty {
                AsmType::I1 | AsmType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )));
                }
            }
        }
    }

    emit_mov_rr(asm, Reg::R10, Reg::Rbp);
    emit_add_ri32(asm, Reg::R10, dst_offset);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_extract_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    aggregate: &AsmValue,
    indices: &[u32],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let agg_ty = value_type(aggregate, reg_types, local_types)?;
    if !is_large_aggregate(&agg_ty) {
        return Err(Error::from("ExtractValue expects aggregate"));
    }
    let size = size_of(&agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    let src_offset = match aggregate {
        AsmValue::Register(id) => agg_offset(layout, *id)?,
        _ => return Err(Error::from("unsupported ExtractValue aggregate source")),
    };
    let (field_offset, field_ty) = aggregate_field_offset(&agg_ty, indices)?;
    let load_offset = src_offset + field_offset as i32;
    let result_ty = reg_types.get(&dst_id).cloned().unwrap_or(field_ty.clone());
    if is_large_aggregate(&result_ty) {
        let field_size = size_of(&result_ty) as i32;
        if field_size == 0 {
            return Ok(());
        }
        if let Ok(dst_offset) = agg_offset(layout, dst_id) {
            copy_sp_to_sp(asm, load_offset, dst_offset, field_size)?;
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, dst_offset);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        } else {
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, load_offset);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
        return Ok(());
    }
    if is_float_type(&result_ty) {
        emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, load_offset, &result_ty);
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &result_ty)?;
    } else {
        match result_ty {
            AsmType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, load_offset),
            AsmType::I64 | AsmType::Ptr(_) | AsmType::Function { .. } => {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, load_offset);
            }
            _ if is_aggregate_type(&result_ty) && size_of(&result_ty) <= 8 => {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, load_offset);
            }
            _ => {
                return Err(Error::from(format!(
                    "unsupported ExtractValue element type for x86_64: {:?}",
                    result_ty
                )));
            }
        }
        store_vreg(asm, layout, dst_id, Reg::R10)?;
    }
    Ok(())
}

fn emit_landingpad(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    result_ty: &AsmType,
) -> Result<()> {
    let size = size_of(result_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    if is_large_aggregate(result_ty) {
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
mod tests {
    use super::emit_text_from_asmir;
    use crate::emit::TargetFormat;
    use fp_core::asmir::{
        AsmArchitecture, AsmBlock, AsmEndianness, AsmFunction, AsmFunctionSignature,
        AsmObjectFormat, AsmProgram, AsmTarget, AsmTerminator, AsmType,
    };
    use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

    #[test]
    fn x86_64_emitter_rejects_mismatched_asmir_architecture() {
        let error = emit_text_from_asmir(
            &AsmProgram::new(AsmTarget {
                architecture: AsmArchitecture::Aarch64,
                object_format: AsmObjectFormat::Elf,
                endianness: AsmEndianness::Little,
                pointer_width: 64,
                default_calling_convention: None,
            }),
            TargetFormat::Elf,
        )
        .err()
        .expect("expected architecture mismatch to fail");

        assert!(
            error.to_string().contains("requires x86_64 AsmIR input"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn x86_64_emitter_accepts_minimal_asmir_program() {
        let output = emit_text_from_asmir(&minimal_program(), TargetFormat::Elf).unwrap();
        assert!(!output.text.is_empty());
    }

    fn minimal_program() -> AsmProgram {
        AsmProgram {
            target: AsmTarget {
                architecture: AsmArchitecture::X86_64,
                object_format: AsmObjectFormat::Elf,
                endianness: AsmEndianness::Little,
                pointer_width: 64,
                default_calling_convention: None,
            },
            container: None,
            sections: Vec::new(),
            globals: Vec::new(),
            type_definitions: Vec::new(),
            functions: vec![AsmFunction {
                name: Name::new("main"),
                signature: AsmFunctionSignature {
                    params: Vec::new(),
                    return_type: AsmType::I32,
                    is_variadic: false,
                },
                basic_blocks: vec![AsmBlock {
                    id: 0,
                    label: Some(Name::new("entry")),
                    instructions: Vec::new(),
                    terminator: AsmTerminator::Return(None),
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
            }],
        }
    }
}
