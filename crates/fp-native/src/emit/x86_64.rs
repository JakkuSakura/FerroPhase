use fp_core::error::{Error, Result};
use fp_core::lir::layout::{size_of, struct_layout};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirIntrinsicKind,
    LirProgram, LirTerminator, LirType, LirValue,
};
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
    func: &LirFunction,
    format: TargetFormat,
    reg_types: &HashMap<u32, LirType>,
) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut has_calls = false;
    let mut alloca_info = Vec::new();

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let LirInstructionKind::Call { args, .. } = &inst.kind {
                has_calls = true;
                max_call_args = max_call_args.max(args.len());
            } else if let LirInstructionKind::IntrinsicCall { kind, args, .. } = &inst.kind {
                has_calls = true;
                let fixed = if matches!(kind, LirIntrinsicKind::Format) {
                    3
                } else {
                    1
                };
                max_call_args = max_call_args.max(args.len() + fixed);
            } else if let LirInstructionKind::Alloca { size, alignment } = &inst.kind {
                let ty = inst
                    .type_hint
                    .clone()
                    .ok_or_else(|| Error::from("missing type for alloca"))?;
                let LirType::Ptr(inner) = ty else {
                    return Err(Error::from("alloca expects pointer type"));
                };
                let count = match size {
                    LirValue::Constant(constant) => constant_to_i64(constant)?,
                    _ => return Err(Error::from("alloca size must be constant")),
                };
                if count < 0 {
                    return Err(Error::from("alloca size must be non-negative"));
                }
                let elem_size = size_of(&inner) as i64;
                let bytes = elem_size
                    .checked_mul(count)
                    .ok_or_else(|| Error::from("alloca size overflow"))?;
                let bytes = i32::try_from(bytes).map_err(|_| Error::from("alloca size too large"))?;
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
        offset += 8;
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
        if matches!(local.ty, LirType::Void) {
            continue;
        }
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

fn build_reg_types(func: &LirFunction) -> HashMap<u32, LirType> {
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
            if let LirInstructionKind::ExtractValue { aggregate, indices } = &inst.kind {
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

fn build_local_types(func: &LirFunction) -> HashMap<u32, LirType> {
    let mut map = HashMap::new();
    for local in &func.locals {
        map.insert(local.id, local.ty.clone());
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

pub fn emit_text(program: &LirProgram, format: TargetFormat) -> Result<CodegenOutput> {
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
    let mut rodata_pool = HashMap::new();
    let mut entry_offset = None;

    for (index, func) in program.functions.iter().enumerate() {
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
    let (text, relocs) = asm.finish()?;
    Ok(CodegenOutput {
        text,
        rodata,
        relocs,
        symbols,
        entry_offset,
    })
}

fn spill_arguments(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &LirFunction,
    format: TargetFormat,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let (arg_regs, float_regs, _) = call_abi(format);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let stack_base = if matches!(format, TargetFormat::Coff) { 48 } else { 16 };

    if let Some(offset) = layout.sret_offset {
        emit_mov_mr64(asm, Reg::Rbp, offset, arg_regs[0]);
        int_idx = 1;
    }

    for local in func.locals.iter().filter(|local| local.is_argument) {
        let ty = local_types
            .get(&local.id)
            .ok_or_else(|| Error::from("missing local type"))?;
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
    lhs: &LirValue,
    rhs: &LirValue,
    op: BitOp,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
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

fn emit_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        LirValue::Constant(constant) => {
            let mut shift = constant_to_i64(constant)?;
            if shift < 0 {
                return Err(Error::from("shift count must be non-negative"));
            }
            shift &= 0x3f;
            let imm = u8::try_from(shift).unwrap_or(0);
            match kind {
                ShiftKind::Left => emit_shl_imm8(asm, Reg::R10, imm),
                ShiftKind::Right => emit_shr_imm8(asm, Reg::R10, imm),
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            emit_mov_rr(asm, Reg::Rcx, Reg::R11);
            match kind {
                ShiftKind::Left => emit_shl_cl(asm, Reg::R10),
                ShiftKind::Right => emit_shr_cl(asm, Reg::R10),
            }
        }
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_not(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    emit_not_r64(asm, Reg::R10);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_sext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits > dst_bits {
        return Err(Error::from("sext expects wider destination"));
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if src_bits < 64 {
        let shift = 64 - src_bits;
        emit_shl_imm8(asm, Reg::R10, shift as u8);
        emit_sar_imm8(asm, Reg::R10, shift as u8);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_sext_or_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits >= dst_bits {
        return emit_trunc(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
    emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types)
}

fn emit_ptr_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    let dst_ty = reg_types
        .get(&dst_id)
        .ok_or_else(|| Error::from("missing type for ptrtoint"))?;
    let dst_bits = int_bits(dst_ty)?;
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
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if src_bits < 64 {
        let mask = (1u64 << src_bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_freeze(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let ty = value_type(value, reg_types, local_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &ty, reg_types, local_types)?;
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, &ty)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_inline_asm(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    output_ty: &LirType,
) -> Result<()> {
    if matches!(output_ty, LirType::Void) {
        return Ok(());
    }
    let size = size_of(output_ty) as i32;
    let dst_offset = vreg_offset(layout, dst_id)?;
    if is_aggregate_type(output_ty) && size > 8 {
        zero_sp_range(asm, dst_offset, size)?;
        return Ok(());
    }
    emit_mov_imm64(asm, Reg::R10, 0);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_zext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    let src_bits = int_bits(&src_ty)?;
    let dst_bits = int_bits(dst_ty)?;
    if src_bits > dst_bits {
        return Err(Error::from("zext expects wider destination"));
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
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let dst_bits = int_bits(dst_ty)?;
    load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::R11, mask);
        emit_and_rr(asm, Reg::R10, Reg::R11);
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    if is_float_type(ty) {
        emit_float_binop(asm, layout, dst_id, lhs, rhs, op, ty, reg_types, local_types)?;
        return Ok(());
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
            }
        }
        LirValue::Constant(constant) => {
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
    value: &LirValue,
    dst: Reg,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
                emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                return Ok(());
            }
            match ty {
                LirType::I1 => emit_movzx_rm8(asm, dst, Reg::Rbp, offset),
                LirType::I8 => emit_movsx_rm8(asm, dst, Reg::Rbp, offset),
                LirType::I16 => emit_movsx_rm16(asm, dst, Reg::Rbp, offset),
                LirType::I32 => emit_movsxd_rm32(asm, dst, Reg::Rbp, offset),
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for x86_64 load: {:?}",
                        ty
                    )))
                }
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
                emit_mov_rr(asm, dst, Reg::Rbp);
                emit_add_ri32(asm, dst, offset);
                return Ok(());
            }
            match ty {
                LirType::I1 => emit_movzx_rm8(asm, dst, Reg::Rbp, offset),
                LirType::I8 => emit_movsx_rm8(asm, dst, Reg::Rbp, offset),
                LirType::I16 => emit_movsx_rm16(asm, dst, Reg::Rbp, offset),
                LirType::I32 => emit_movsxd_rm32(asm, dst, Reg::Rbp, offset),
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_mov_rm64(asm, dst, Reg::Rbp, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for x86_64 load: {:?}",
                        ty
                    )))
                }
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            if size_of(&constant_type(constant)) == 0 {
                emit_mov_imm64(asm, dst, 0);
                return Ok(());
            }
            let imm = constant_to_i64(constant)?;
            emit_mov_imm64(asm, dst, imm as u64);
            Ok(())
        }
        LirValue::Null(_) | LirValue::Undef(_) => {
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

fn load_value_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    dst: FReg,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_movsd_xm64(asm, dst, Reg::Rbp, offset, ty);
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_movsd_xm64(asm, dst, Reg::Rbp, offset, ty);
            Ok(())
        }
        LirValue::Constant(LirConstant::Float(value, _)) => {
            let bits = if matches!(ty, LirType::F32) {
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
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    lhs: &LirValue,
    rhs: &LirValue,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    ty: &LirType,
) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_movsd_m64x(asm, Reg::Rbp, offset, src, ty);
    Ok(())
}

fn is_float_type(ty: &LirType) -> bool {
    matches!(ty, LirType::F32 | LirType::F64)
}

fn is_aggregate_type(ty: &LirType) -> bool {
    matches!(ty, LirType::Struct { .. } | LirType::Array(_, _) | LirType::Vector(_, _))
}

fn is_large_aggregate(ty: &LirType) -> bool {
    is_aggregate_type(ty) && size_of(ty) > 8
}

fn returns_aggregate(ty: &LirType) -> bool {
    is_large_aggregate(ty)
}

fn is_integer_type(ty: &LirType) -> bool {
    matches!(
        ty,
        LirType::I1
            | LirType::I8
            | LirType::I16
            | LirType::I32
            | LirType::I64
            | LirType::I128
    )
}

fn int_bits(ty: &LirType) -> Result<u32> {
    match ty {
        LirType::I1 => Ok(1),
        LirType::I8 => Ok(8),
        LirType::I16 => Ok(16),
        LirType::I32 => Ok(32),
        LirType::I64 => Ok(64),
        LirType::I128 => Err(Error::from("i128 not supported in native emitter")),
        _ => Err(Error::from("expected integer type")),
    }
}

fn constant_type(constant: &LirConstant) -> LirType {
    match constant {
        LirConstant::Int(_, ty) => ty.clone(),
        LirConstant::UInt(_, ty) => ty.clone(),
        LirConstant::Float(_, ty) => ty.clone(),
        LirConstant::Bool(_) => LirType::I1,
        LirConstant::String(_) => LirType::Ptr(Box::new(LirType::I8)),
        LirConstant::Null(ty) => ty.clone(),
        LirConstant::Undef(ty) => ty.clone(),
        LirConstant::Array(_, ty) => ty.clone(),
        LirConstant::Struct(_, ty) => ty.clone(),
        LirConstant::GlobalRef(_, ty, _) => ty.clone(),
        LirConstant::FunctionRef(_, ty) => ty.clone(),
    }
}

fn value_type(
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<LirType> {
    match value {
        LirValue::Register(id) => reg_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing register type")),
        LirValue::Constant(constant) => Ok(constant_type(constant)),
        LirValue::Null(ty) | LirValue::Undef(ty) => Ok(ty.clone()),
        LirValue::StackSlot(_) => Ok(LirType::Ptr(Box::new(LirType::I8))),
        LirValue::Local(id) => local_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing local type")),
        LirValue::Global(_, ty) => Ok(ty.clone()),
        LirValue::Function(_) => Ok(LirType::Ptr(Box::new(LirType::I8))),
    }
}

fn constant_to_i64(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        LirConstant::Null(_) | LirConstant::Undef(_) => Ok(0),
        LirConstant::Array(values, _) if values.is_empty() => Ok(0),
        LirConstant::Struct(values, ty) if values.is_empty() || size_of(ty) == 0 => Ok(0),
        _ => Err(Error::from(format!(
            "unsupported constant for x86_64: {:?}",
            constant
        ))),
    }
}

fn constant_to_u64_bits(constant: &LirConstant) -> Result<u64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value as u64),
        LirConstant::UInt(value, _) => Ok(*value),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        LirConstant::Float(value, _) => Ok(value.to_bits()),
        LirConstant::Null(_) | LirConstant::Undef(_) => Ok(0),
        _ => Err(Error::from("unsupported constant for aggregate store")),
    }
}

fn pack_small_aggregate(constant: &LirConstant, ty: &LirType) -> Result<u64> {
    if size_of(ty) > 8 {
        return Err(Error::from("aggregate too large to pack"));
    }
    match (constant, ty) {
        (LirConstant::Struct(values, _), LirType::Struct { fields, .. }) => {
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
        (LirConstant::Array(values, _), LirType::Array(elem, len)) => {
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
        (LirConstant::Array(values, _), other_ty) => {
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
    layout
        .local_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing local slot"))
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

fn emit_add_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x01);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_sub_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x29);
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
    block: &LirBasicBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    layout: &FrameLayout,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    return_ty: &LirType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    for inst in &block.instructions {
        match &inst.kind {
            LirInstructionKind::Add(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for add"))?;
                if matches!(ty, LirType::Ptr(_)) {
                    if let (LirValue::Constant(LirConstant::String(lhs_text)),
                        LirValue::Constant(LirConstant::String(rhs_text))) = (lhs, rhs)
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
                )?
            }
            LirInstructionKind::Sub(lhs, rhs) => {
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
                )?
            }
            LirInstructionKind::Mul(lhs, rhs) => {
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
                )?
            }
            LirInstructionKind::And(lhs, rhs) => {
                emit_bitwise_binop(asm, layout, inst.id, lhs, rhs, BitOp::And, reg_types, local_types)?
            }
            LirInstructionKind::Or(lhs, rhs) => {
                emit_bitwise_binop(asm, layout, inst.id, lhs, rhs, BitOp::Or, reg_types, local_types)?
            }
            LirInstructionKind::Xor(lhs, rhs) => {
                emit_bitwise_binop(asm, layout, inst.id, lhs, rhs, BitOp::Xor, reg_types, local_types)?
            }
            LirInstructionKind::Shl(lhs, rhs) => {
                emit_shift(asm, layout, inst.id, lhs, rhs, ShiftKind::Left, reg_types, local_types)?
            }
            LirInstructionKind::Shr(lhs, rhs) => {
                emit_shift(asm, layout, inst.id, lhs, rhs, ShiftKind::Right, reg_types, local_types)?
            }
            LirInstructionKind::Eq(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Eq,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Ne(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Ne,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Lt(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Lt,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Le(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Le,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Gt(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Gt,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Ge(lhs, rhs) => {
                emit_cmp(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    CmpKind::Ge,
                    reg_types,
                    local_types,
                )?
            }
            LirInstructionKind::Div(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, false, reg_types, local_types)?
            }
            LirInstructionKind::Rem(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, true, reg_types, local_types)?
            }
            LirInstructionKind::Not(value) => {
                emit_not(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            LirInstructionKind::Alloca { .. } => {
                let offset = alloca_offset(layout, inst.id)?;
                emit_mov_rr(asm, Reg::R10, Reg::Rbp);
                emit_add_ri32(asm, Reg::R10, offset);
                store_vreg(asm, layout, inst.id, Reg::R10)?;
            }
            LirInstructionKind::Load { address, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for load"))?;
                emit_load(asm, layout, inst.id, address, ty, reg_types, local_types)?;
            }
            LirInstructionKind::Store { value, address, .. } => {
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
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                emit_gep(asm, layout, inst.id, ptr, indices, reg_types, local_types)?;
            }
            LirInstructionKind::Call { function, args, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .cloned()
                    .unwrap_or(LirType::Void);
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
            LirInstructionKind::IntrinsicCall { kind, format: format_str, args } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .cloned()
                    .unwrap_or(LirType::Void);
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
            LirInstructionKind::SIToFP(value, ty) => {
                emit_int_to_float(asm, layout, inst.id, value, ty, reg_types, local_types, true)?;
            }
            LirInstructionKind::UIToFP(value, ty) => {
                emit_int_to_float(asm, layout, inst.id, value, ty, reg_types, local_types, false)?;
            }
            LirInstructionKind::Trunc(value, ty) => {
                emit_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::ZExt(value, ty) => {
                emit_zext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::FPToSI(value, ty) => {
                emit_float_to_int(asm, layout, inst.id, value, ty, reg_types, local_types, true)?;
            }
            LirInstructionKind::FPToUI(value, ty) => {
                emit_float_to_int(asm, layout, inst.id, value, ty, reg_types, local_types, false)?;
            }
            LirInstructionKind::FPTrunc(value, ty) => {
                emit_fp_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::FPExt(value, ty) => {
                emit_fp_ext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::SExt(value, ty) => {
                emit_sext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::SextOrTrunc(value, ty) => {
                emit_sext_or_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::Bitcast(value, ty) => {
                emit_bitcast(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::PtrToInt(value) => {
                emit_ptr_to_int(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            LirInstructionKind::IntToPtr(value) => {
                emit_int_to_ptr(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            LirInstructionKind::InsertValue { aggregate, element, indices } => {
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
            LirInstructionKind::ExtractValue { aggregate, indices } => {
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
            LirInstructionKind::Select {
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
            LirInstructionKind::LandingPad { result_type, .. } => {
                emit_landingpad(asm, layout, inst.id, result_type)?;
            }
            LirInstructionKind::Freeze(value) => {
                emit_freeze(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            LirInstructionKind::InlineAsm { output_type, .. } => {
                emit_inline_asm(asm, layout, inst.id, output_type)?;
            }
            LirInstructionKind::Unreachable => {
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
        LirTerminator::Return(None) => {
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
        LirTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if returns_aggregate(return_ty) {
                let src_offset = match value {
                    LirValue::Register(id) => agg_offset(layout, *id)?,
                    _ => return Err(Error::from("unsupported aggregate return value")),
                };
                let sret_offset = layout
                    .sret_offset
                    .ok_or_else(|| Error::from("missing sret pointer for aggregate return"))?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, sret_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size_of(return_ty) as i32)?;
                if asm.needs_frame {
                    emit_epilogue(asm);
                }
                emit_ret(asm);
                return Ok(());
            }
            if is_float_type(return_ty) {
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
        LirTerminator::Br(target) => {
            asm.emit_jmp(Label::Block(asm.current_function, *target));
        }
        LirTerminator::CondBr {
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
        LirTerminator::Invoke {
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
                &LirType::Void,
                reg_types,
                local_types,
                format,
                rodata,
                rodata_pool,
            )?;
            asm.emit_jmp(Label::Block(asm.current_function, *normal_dest));
        }
        LirTerminator::Switch { value, default, cases } => {
            emit_switch(
                asm,
                layout,
                value,
                *default,
                cases,
                reg_types,
                local_types,
            )?;
        }
        LirTerminator::Unreachable => {
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
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
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
        (LirValue::Register(_), LirValue::Register(_))
        | (LirValue::Register(_), LirValue::Constant(_))
        | (LirValue::Constant(_), LirValue::Register(_))
        | (LirValue::Constant(_), LirValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types, local_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types, local_types)?;
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            let imm32 = i32::try_from(imm).map_err(|_| Error::from("cmp immediate out of range"))?;
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

fn emit_select(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    condition: &LirValue,
    if_true: &LirValue,
    if_false: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let result_ty = reg_types
        .get(&dst_id)
        .cloned()
        .unwrap_or(value_type(if_true, reg_types, local_types)?);
    if is_float_type(&result_ty) {
        return Err(Error::from("Select does not support float values on x86_64"));
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
    condition: &LirValue,
    if_true: Label,
    if_false: Label,
) -> Result<()> {
    match condition {
        LirValue::Constant(LirConstant::Bool(value)) => {
            if *value {
                asm.emit_jmp(if_true);
            } else {
                asm.emit_jmp(if_false);
            }
        }
        LirValue::Register(id) => {
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
    value: &LirValue,
    default: BasicBlockId,
    cases: &[(u64, BasicBlockId)],
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn emit_jcc(&mut self, opcode: u8, target: Label) {
        self.buf.push(0x0F);
        self.buf.push(opcode);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn emit_call(&mut self, target: Label) {
        self.buf.push(0xE8);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup { pos, target });
    }

    fn emit_call_external(&mut self, symbol: &str) {
        self.buf.push(0xE8);
        let offset = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::CallRel32,
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

fn build_function_map(program: &LirProgram) -> Result<HashMap<String, u32>> {
    let mut map = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        let name = String::from(func.name.clone());
        map.insert(name, idx as u32);
    }
    Ok(map)
}

fn program_uses_fp_panic(program: &LirProgram) -> bool {
    for func in &program.functions {
        for block in &func.basic_blocks {
            for inst in &block.instructions {
                if let LirInstructionKind::Call { function, .. } = &inst.kind {
                    if matches!(function, LirValue::Function(name) if name == "fp_panic") {
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
    address: &LirValue,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    if is_large_aggregate(ty) {
        let size = size_of(ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let dst_offset = agg_offset(layout, dst_id)?;
        match address {
            LirValue::StackSlot(id) => {
                let src_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_reg_to_sp(asm, Reg::R11, dst_offset, size)?;
            }
            LirValue::Local(id) => {
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
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    LirType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, offset),
                    LirType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, offset),
                    LirType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, offset),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )))
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    LirType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    LirType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    LirType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )))
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, offset);
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::R11, 0, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::R11, 0),
                    LirType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::R11, 0),
                    LirType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::R11, 0),
                    LirType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::R11, 0),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for x86_64: {:?}",
                            ty
                        )))
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
    lhs: &LirValue,
    rhs: &LirValue,
    want_rem: bool,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if is_float_type(&lhs_ty) {
        if want_rem {
            return Err(Error::from("float remainder is not supported on x86_64"));
        }
        emit_float_div(asm, layout, dst_id, lhs, rhs, &lhs_ty, reg_types, local_types)?;
        return Ok(());
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
    function: &LirValue,
    args: &[LirValue],
    func_map: &HashMap<String, u32>,
    ret_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    format: TargetFormat,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let target = match function {
        LirValue::Function(name) => func_map
            .get(name)
            .copied()
            .map(CallTarget::Internal)
            .unwrap_or_else(|| CallTarget::External(name.clone())),
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
        if let LirValue::Constant(LirConstant::String(text)) = arg {
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
    }

    if needs_sret {
        if let Some(agg_off) = sret_offset {
            emit_mov_rr(asm, Reg::R10, Reg::Rbp);
            emit_add_ri32(asm, Reg::R10, agg_off);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
        }
    } else if !matches!(ret_ty, LirType::Void) {
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
    kind: &LirIntrinsicKind,
    format: &str,
    args: &[LirValue],
    result_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
    target_format: TargetFormat,
) -> Result<()> {
    match kind {
        LirIntrinsicKind::Print | LirIntrinsicKind::Println => {}
        LirIntrinsicKind::Format => {
            if !matches!(result_ty, LirType::Ptr(_)) {
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
                &LirValue::Register(dst_id),
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
        LirIntrinsicKind::TimeNow => {
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
    arg: &LirValue,
    int_idx: &mut usize,
    float_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
    float_regs: &[FReg],
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    if let LirValue::Constant(LirConstant::String(text)) = arg {
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
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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

fn intern_cstring(
    rodata: &mut Vec<u8>,
    pool: &mut HashMap<String, u64>,
    text: &str,
) -> u64 {
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
    value: &LirValue,
    address: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    if let LirValue::Constant(LirConstant::String(text)) = value {
        let offset = intern_cstring(rodata, rodata_pool, text);
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if let LirValue::Constant(LirConstant::Array(values, elem_ty)) = value {
        if values.is_empty() {
            return Ok(());
        }
        let elem_ty = match elem_ty {
            LirType::Array(elem, _) => elem.as_ref(),
            other => other,
        };
        let elem_size = size_of(elem_ty) as i32;
        if elem_size != 8 {
            return Err(Error::from("unsupported array element size in constant store"));
        }
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                for (idx, elem) in values.iter().enumerate() {
                    let offset = dst_offset + (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                }
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10);
                }
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm64(asm, Reg::R10, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::R10, bits);
                        }
                    }
                    emit_mov_rr(asm, Reg::Rax, Reg::R11);
                    emit_add_ri32(asm, Reg::Rax, offset);
                    emit_mov_mr64(asm, Reg::Rax, 0, Reg::R10);
                }
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if matches!(value, LirValue::Constant(LirConstant::Array(values, _)) if values.is_empty()) {
        return Ok(());
    }
    let value_ty = value_type(value, reg_types, local_types)?;
    if size_of(&value_ty) == 0 {
        return Ok(());
    }
    if let LirValue::Constant(constant) = value {
        if matches!(constant, LirConstant::Struct(_, _) | LirConstant::Array(_, _))
            && size_of(&value_ty) <= 8
        {
            let bits = pack_small_aggregate(constant, &value_ty)?;
            emit_mov_imm64(asm, Reg::R10, bits);
            match address {
                LirValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    emit_mov_mr64(asm, Reg::Rbp, dst_offset, Reg::R10);
                }
                LirValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                }
                LirValue::Local(id) => {
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
        if let LirValue::Constant(LirConstant::Struct(values, ty)) = value {
            let fields = match ty {
                LirType::Struct { fields, .. } => fields,
                _ => return Err(Error::from("expected struct type for constant store")),
            };
            let struct_layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            match address {
                LirValue::StackSlot(id) => {
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
                            LirConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
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
                                ))
                            }
                        }
                    }
                }
                LirValue::Register(id) => {
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
                            LirConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
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
                                ))
                            }
                        }
                    }
                }
                LirValue::Local(id) => {
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
                            LirConstant::String(text) => {
                                let offset = intern_cstring(rodata, rodata_pool, text);
                                asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
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
                                ))
                            }
                        }
                    }
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        if matches!(value, LirValue::Constant(LirConstant::Undef(_))) {
            match address {
                LirValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    zero_sp_range(asm, dst_offset, size)?;
                }
                LirValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                LirValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                    zero_reg_range(asm, Reg::R11, size)?;
                }
                _ => return Err(Error::from("unsupported store address for x86_64")),
            }
            return Ok(());
        }
        let src_offset = match value {
            LirValue::Register(id) => agg_offset(layout, *id)?,
            LirValue::Local(id) => local_offset(layout, *id)?,
            _ => {
                return Err(Error::from(format!(
                    "unsupported aggregate store value: {:?}",
                    value
                )))
            }
        };
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::R11, size)?;
            }
            _ => return Err(Error::from("unsupported store address for x86_64")),
        }
        return Ok(());
    }
    if is_float_type(&value_ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &value_ty, reg_types, local_types)?;
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types, local_types)?;
    }

    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::Rbp, offset, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_mov_mr8(asm, Reg::Rbp, offset, Reg::R10),
                    LirType::I16 => emit_mov_mr16(asm, Reg::Rbp, offset, Reg::R10),
                    LirType::I32 => emit_mov_mr32(asm, Reg::Rbp, offset, Reg::R10),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )))
                    }
                }
            }
            Ok(())
        }
        LirValue::Register(id) => {
            let addr_offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    LirType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    LirType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )))
                    }
                }
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let addr_offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::R11, 0, FReg::Xmm0, &value_ty);
            } else {
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_mov_mr8(asm, Reg::R11, 0, Reg::R10),
                    LirType::I16 => emit_mov_mr16(asm, Reg::R11, 0, Reg::R10),
                    LirType::I32 => emit_mov_mr32(asm, Reg::R11, 0, Reg::R10),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for x86_64: {:?}",
                            value_ty
                        )))
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

fn emit_movsd_m64x_sp(asm: &mut Assembler, disp: i32, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), Reg::Rsp.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

fn emit_movsd_xm64(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x10);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsd_m64x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg, ty: &LirType) {
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

fn emit_addsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x58);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_subsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_mulsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x59);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_divsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_ucomisd(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &LirType) {
    if matches!(ty, LirType::F64) {
        asm.push(0x66);
    }
    emit_rex(asm, false, lhs.id(), rhs.id());
    asm.push(0x0F);
    asm.push(0x2E);
    emit_modrm(asm, 0b11, lhs.id(), rhs.id());
}

fn emit_cvtsi2sd(asm: &mut Assembler, dst: FReg, src: Reg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvttsd2si(asm: &mut Assembler, dst: Reg, src: FReg, ty: &LirType) {
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
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    signed: bool,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_integer_type(&src_ty) {
        return Err(Error::from("int to float expects integer source"));
    }
    if matches!(src_ty, LirType::I128) {
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

fn emit_uint_to_float(asm: &mut Assembler, src: Reg, dst_ty: &LirType) -> Result<()> {
    emit_mov_rr(asm, Reg::R11, src);
    emit_shr_imm8(asm, Reg::R11, 1);
    emit_and_ri32(asm, src, 1);
    emit_or_rr(asm, Reg::R11, src);
    emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R11, &LirType::F64);
    emit_addsd(asm, FReg::Xmm0, FReg::Xmm0, &LirType::F64);
    if matches!(dst_ty, LirType::F32) {
        emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    }
    Ok(())
}

fn emit_float_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
    _signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    if matches!(dst_ty, LirType::I128) {
        return Err(Error::from("i128 from float is not supported on x86_64"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types, local_types)?;
    emit_cvttsd2si(asm, Reg::R10, FReg::Xmm0, &src_ty);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_fp_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (LirType::F64, LirType::F32)) {
        return Err(Error::from("unsupported FPTrunc on x86_64"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types, local_types)?;
    emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_fp_ext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types, local_types)?;
    if !matches!((&src_ty, dst_ty), (LirType::F32, LirType::F64)) {
        return Err(Error::from("unsupported FPExt on x86_64"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types, local_types)?;
    emit_cvtss2sd(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_gep(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    ptr: &LirValue,
    indices: &[LirValue],
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let ptr_ty = value_type(ptr, reg_types, local_types)?;
    let mut current_ty = match ptr_ty {
        LirType::Ptr(inner) => *inner,
        _ => return Err(Error::from("GEP expects pointer base type")),
    };

    load_value(asm, layout, ptr, Reg::R11, reg_types, local_types)?;
    for index in indices {
        match &current_ty {
            LirType::Struct { fields, .. } => {
                let idx = match index {
                    LirValue::Constant(constant) => {
                        let raw = constant_to_i64(constant)?;
                        usize::try_from(raw).map_err(|_| Error::from("GEP struct index out of range"))?
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
            LirType::Array(elem, _) | LirType::Vector(elem, _) => {
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
    index: &LirValue,
    elem_size: u64,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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

fn extract_value_type(ty: &LirType, indices: &[u32]) -> Result<LirType> {
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            LirType::Struct { fields, .. } => {
                current_ty = fields
                    .get(*idx as usize)
                    .cloned()
                    .ok_or_else(|| Error::from("ExtractValue field out of range"))?;
            }
            LirType::Array(elem, _) | LirType::Vector(elem, _) => {
                current_ty = *elem.clone();
            }
            _ => return Err(Error::from("ExtractValue expects aggregate type")),
        }
    }
    Ok(current_ty)
}

fn aggregate_field_offset(ty: &LirType, indices: &[u32]) -> Result<(i64, LirType)> {
    let mut offset = 0i64;
    let mut current_ty = ty.clone();
    for idx in indices {
        match &current_ty {
            LirType::Struct { fields, .. } => {
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
            LirType::Array(elem, _) | LirType::Vector(elem, _) => {
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
    if size % 8 != 0 {
        return Err(Error::from("aggregate copy size must be 8-byte aligned"));
    }
    let mut offset = 0;
    while offset < size {
        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn copy_sp_to_reg(asm: &mut Assembler, src: i32, dst: Reg, size: i32) -> Result<()> {
    if size % 8 != 0 {
        return Err(Error::from("aggregate copy size must be 8-byte aligned"));
    }
    let mut offset = 0;
    while offset < size {
        emit_mov_rm64(asm, Reg::R10, Reg::Rbp, src + offset);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn copy_reg_to_sp(asm: &mut Assembler, src: Reg, dst: i32, size: i32) -> Result<()> {
    if size % 8 != 0 {
        return Err(Error::from("aggregate copy size must be 8-byte aligned"));
    }
    let mut offset = 0;
    while offset < size {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn copy_reg_to_reg(asm: &mut Assembler, src: Reg, dst: Reg, size: i32) -> Result<()> {
    if size % 8 != 0 {
        return Err(Error::from("aggregate copy size must be 8-byte aligned"));
    }
    let mut offset = 0;
    while offset < size {
        emit_mov_rr(asm, Reg::R11, src);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_rm64(asm, Reg::R10, Reg::R11, 0);
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn zero_sp_range(asm: &mut Assembler, dst: i32, size: i32) -> Result<()> {
    if size % 8 != 0 {
        return Err(Error::from("aggregate zero size must be 8-byte aligned"));
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset < size {
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn zero_reg_range(asm: &mut Assembler, dst: Reg, size: i32) -> Result<()> {
    if size % 8 != 0 {
        return Err(Error::from("aggregate zero size must be 8-byte aligned"));
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset < size {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    Ok(())
}

fn emit_bitcast(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    aggregate: &LirValue,
    element: &LirValue,
    indices: &[u32],
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
        LirValue::Register(id) => {
            let src_offset = agg_offset(layout, *id)?;
            copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
        }
        LirValue::Constant(LirConstant::Undef(_)) => {
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
            LirValue::Register(id) => {
                let src_offset = agg_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            LirValue::Local(id) => {
                let src_offset = local_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, store_offset, field_size)?;
            }
            LirValue::Constant(LirConstant::Struct(values, ty)) => {
                let fields = match ty {
                    LirType::Struct { fields, .. } => fields,
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
                        LirConstant::String(text) => {
                            let offset = intern_cstring(rodata, rodata_pool, text);
                            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
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
                            ))
                        }
                    }
                }
            }
            LirValue::Constant(LirConstant::Undef(_)) => {
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
        load_value_float(asm, layout, element, FReg::Xmm0, &field_ty, reg_types, local_types)?;
        emit_movsd_m64x(asm, Reg::Rbp, store_offset, FReg::Xmm0, &field_ty);
    } else {
        if let LirValue::Constant(LirConstant::String(text)) = element {
            let offset = intern_cstring(rodata, rodata_pool, text);
            asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
            match field_ty {
                LirType::I1 | LirType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )))
                }
            }
        } else {
            load_value(asm, layout, element, Reg::R10, reg_types, local_types)?;
            match field_ty {
                LirType::I1 | LirType::I8 => emit_mov_mr8(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I16 => emit_mov_mr16(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I32 => emit_mov_mr32(asm, Reg::Rbp, store_offset, Reg::R10),
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_mov_mr64(asm, Reg::Rbp, store_offset, Reg::R10);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for x86_64: {:?}",
                        field_ty
                    )))
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
    aggregate: &LirValue,
    indices: &[u32],
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
        LirValue::Register(id) => agg_offset(layout, *id)?,
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
            LirType::I1 => emit_movzx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            LirType::I8 => emit_movsx_rm8(asm, Reg::R10, Reg::Rbp, load_offset),
            LirType::I16 => emit_movsx_rm16(asm, Reg::R10, Reg::Rbp, load_offset),
            LirType::I32 => emit_movsxd_rm32(asm, Reg::R10, Reg::Rbp, load_offset),
            LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, load_offset);
            }
            _ if is_aggregate_type(&result_ty) && size_of(&result_ty) <= 8 => {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, load_offset);
            }
            _ => {
                return Err(Error::from(format!(
                    "unsupported ExtractValue element type for x86_64: {:?}",
                    result_ty
                )))
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
    result_ty: &LirType,
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

fn emit_float_prefix(asm: &mut Assembler, ty: &LirType) {
    match ty {
        LirType::F32 => asm.push(0xF3),
        LirType::F64 => asm.push(0xF2),
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
