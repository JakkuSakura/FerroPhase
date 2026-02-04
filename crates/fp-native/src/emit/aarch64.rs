use fp_core::error::{Error, Result};
use fp_core::lir::layout::{align_of, size_of, struct_layout};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirIntrinsicKind,
    LirProgram, LirTerminator, LirType, LirValue,
};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X29,
    X30,
    X31,
}

impl Reg {
    fn id(self) -> u32 {
        match self {
            Reg::X0 => 0,
            Reg::X1 => 1,
            Reg::X2 => 2,
            Reg::X3 => 3,
            Reg::X4 => 4,
            Reg::X5 => 5,
            Reg::X6 => 6,
            Reg::X7 => 7,
            Reg::X8 => 8,
            Reg::X9 => 9,
            Reg::X10 => 10,
            Reg::X11 => 11,
            Reg::X12 => 12,
            Reg::X13 => 13,
            Reg::X14 => 14,
            Reg::X15 => 15,
            Reg::X16 => 16,
            Reg::X17 => 17,
            Reg::X29 => 29,
            Reg::X30 => 30,
            Reg::X31 => 31,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FReg {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
}

impl FReg {
    fn id(self) -> u32 {
        match self {
            FReg::V0 => 0,
            FReg::V1 => 1,
            FReg::V2 => 2,
            FReg::V3 => 3,
            FReg::V4 => 4,
            FReg::V5 => 5,
            FReg::V6 => 6,
            FReg::V7 => 7,
        }
    }
}

fn build_frame_layout(
    func: &LirFunction,
    reg_types: &HashMap<u32, LirType>,
) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut max_vararg_stack = 0usize;
    let mut has_calls = false;
    let local_types = build_local_types(func);
    let mut alloca_info = Vec::new();
    let mut alloca_debug = Vec::new();
    let mut local_debug = Vec::new();
    let mut agg_debug = Vec::new();

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let LirInstructionKind::Call { function, args, .. } = &inst.kind {
                has_calls = true;
                let mut count = 0usize;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types);
                }
                max_call_args = max_call_args.max(count);
                if matches!(function, LirValue::Function(name) if name == "printf") {
                    let bytes = vararg_outgoing_size(args, true, reg_types, &local_types)?;
                    max_vararg_stack = max_vararg_stack.max(bytes);
                }
            } else if let LirInstructionKind::IntrinsicCall { kind, args, .. } = &inst.kind {
                has_calls = true;
                let fixed = if matches!(kind, LirIntrinsicKind::Format) {
                    3
                } else {
                    1
                };
                let mut count = fixed;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types);
                }
                max_call_args = max_call_args.max(count);
                let bytes = vararg_outgoing_size(args, false, reg_types, &local_types)?;
                max_vararg_stack = max_vararg_stack.max(bytes);
            } else if matches!(
                inst.kind,
                LirInstructionKind::Mul(_, _)
                    | LirInstructionKind::Div(_, _)
                    | LirInstructionKind::Rem(_, _)
                    | LirInstructionKind::Shl(_, _)
                    | LirInstructionKind::Shr(_, _)
            ) {
                if matches!(inst.type_hint, Some(LirType::I128)) {
                    has_calls = true;
                    let args = match inst.kind {
                        LirInstructionKind::Shl(_, _) | LirInstructionKind::Shr(_, _) => 3,
                        _ => 4,
                    };
                    max_call_args = max_call_args.max(args);
                }
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
                let bytes =
                    i32::try_from(bytes).map_err(|_| Error::from("alloca size too large"))?;
                let align = (*alignment).max(1) as i32;
                alloca_info.push((inst.id, bytes, align));
            }
        }
    }

    let reg_spill_size = (max_call_args.saturating_sub(8) * 8) as i32;
    let vararg_stack_size = max_vararg_stack as i32;
    let outgoing_size = align16(reg_spill_size.max(vararg_stack_size));
    let mut vreg_offsets = HashMap::new();
    let mut slot_offsets = HashMap::new();
    let mut local_offsets = HashMap::new();
    let mut agg_offsets = HashMap::new();
    let mut alloca_offsets = HashMap::new();
    let mut sret_offset = None;
    let mut offset = outgoing_size;

    for id in &vreg_ids {
        let (size, align) = vreg_slot_spec(*id, reg_types);
        offset = align_to(offset, align);
        vreg_offsets.insert(*id, offset);
        offset += size;
    }

    for slot in &func.stack_slots {
        let align = slot.alignment.max(1) as i32;
        let size = align8(slot.size as i32).max(8);
        let slot_align = align.max(8);
        offset = align_to(offset, slot_align);
        slot_offsets.insert(slot.id, offset);
        offset += size;
    }

    for local in &func.locals {
        if matches!(local.ty, LirType::Void) {
            continue;
        }
        let size = align8(size_of(&local.ty) as i32).max(8);
        offset = align_to(offset, 8);
        local_offsets.insert(local.id, offset);
        local_debug.push((local.id, offset, size));
        offset += size;
    }

    if returns_aggregate(&func.signature.return_type) {
        sret_offset = Some(offset);
        offset += 8;
    }

    for id in &vreg_ids {
        if let Some(ty) = reg_types.get(id) {
            if is_large_aggregate(ty) {
                let size = align8(size_of(ty) as i32);
                if size > 0 {
                    agg_offsets.insert(*id, offset);
                    agg_debug.push((*id, offset, size));
                    offset += size;
                }
            }
        }
    }

    for (id, size, align) in alloca_info {
        let size = align8(size).max(8);
        let align = align.max(8);
        offset = align_to(offset, align);
        alloca_offsets.insert(id, offset);
        alloca_debug.push((id, offset, size));
        offset += size;
    }

    let local_size = offset - outgoing_size;
    let base = outgoing_size + local_size;
    let frame_size = if base == 0 && !has_calls {
        0
    } else {
        align16(base + 16)
    };

    if layout_debug_enabled() {
        let save_offset = frame_size - 16;
        layout_log(&format!(
            "{} frame_size={} outgoing_size={} save_offset={}",
            func.name, frame_size, outgoing_size, save_offset
        ));
        for (id, offset, size) in &alloca_debug {
            if *offset + *size > save_offset {
                layout_log(&format!(
                    "alloca id={} offset={} size={} overlaps save_area",
                    id, offset, size
                ));
            }
        }
        for (id, offset, size) in &local_debug {
            if *offset + *size > save_offset {
                layout_log(&format!(
                    "local id={} offset={} size={} overlaps save_area",
                    id, offset, size
                ));
            }
        }
        for (id, offset, size) in &agg_debug {
            if *offset + *size > save_offset {
                layout_log(&format!(
                    "agg id={} offset={} size={} overlaps save_area",
                    id, offset, size
                ));
            }
        }
    }

    Ok(FrameLayout {
        vreg_offsets,
        slot_offsets,
        local_offsets,
        agg_offsets,
        alloca_offsets,
        sret_offset,
        outgoing_size,
        frame_size,
    })
}

fn call_arg_units(
    arg: &LirValue,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> usize {
    let ty = value_type(arg, reg_types, local_types).unwrap_or(LirType::I64);
    if matches!(ty, LirType::I128) { 2 } else { 1 }
}

fn vreg_slot_spec(id: u32, reg_types: &HashMap<u32, LirType>) -> (i32, i32) {
    let Some(ty) = reg_types.get(&id) else {
        return (8, 8);
    };
    if is_large_aggregate(ty) {
        return (8, 8);
    }
    if matches!(ty, LirType::I128) {
        let align = align_of(ty) as i32;
        return (16, align.max(16));
    }
    (8, 8)
}

fn vararg_outgoing_size(
    args: &[LirValue],
    format_in_args: bool,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<usize> {
    let start = if format_in_args { 1 } else { 0 };
    let mut stack_bytes = 0i32;
    for arg in args.iter().skip(start) {
        let ty = value_type(arg, reg_types, local_types)?;
        stack_bytes += align8(size_of(&ty) as i32);
    }
    Ok(stack_bytes as usize)
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

fn abi_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_ABI_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

fn abi_log(msg: &str) {
    if abi_debug_enabled() {
        eprintln!("[fp-native][abi] {}", msg);
    }
}

fn stack_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_STACK_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

fn layout_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_LAYOUT_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

fn layout_log(msg: &str) {
    if layout_debug_enabled() {
        eprintln!("[fp-native][layout] {}", msg);
    }
}

fn reg_name(reg: Reg) -> &'static str {
    match reg {
        Reg::X0 => "x0",
        Reg::X1 => "x1",
        Reg::X2 => "x2",
        Reg::X3 => "x3",
        Reg::X4 => "x4",
        Reg::X5 => "x5",
        Reg::X6 => "x6",
        Reg::X7 => "x7",
        Reg::X8 => "x8",
        Reg::X9 => "x9",
        Reg::X10 => "x10",
        Reg::X11 => "x11",
        Reg::X12 => "x12",
        Reg::X13 => "x13",
        Reg::X14 => "x14",
        Reg::X15 => "x15",
        Reg::X16 => "x16",
        Reg::X17 => "x17",
        Reg::X29 => "x29",
        Reg::X30 => "x30",
        Reg::X31 => "sp",
    }
}

fn freg_name(reg: FReg) -> &'static str {
    match reg {
        FReg::V0 => "v0",
        FReg::V1 => "v1",
        FReg::V2 => "v2",
        FReg::V3 => "v3",
        FReg::V4 => "v4",
        FReg::V5 => "v5",
        FReg::V6 => "v6",
        FReg::V7 => "v7",
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

struct FrameLayout {
    vreg_offsets: HashMap<u32, i32>,
    slot_offsets: HashMap<u32, i32>,
    local_offsets: HashMap<u32, i32>,
    agg_offsets: HashMap<u32, i32>,
    alloca_offsets: HashMap<u32, i32>,
    sret_offset: Option<i32>,
    outgoing_size: i32,
    frame_size: i32,
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
    let mut asm = Assembler::new(format);
    let mut rodata = Vec::new();
    let mut rodata_pool = HashMap::new();
    let mut entry_offset = None;

    for (index, func) in program.functions.iter().enumerate() {
        asm.bind(Label::Function(index as u32));
        if entry_offset.is_none() && func.name.as_str() == "main" {
            entry_offset = Some(asm.buf.len() as u64);
        }
        let reg_types = build_reg_types(func);
        let layout = build_frame_layout(func, &reg_types)?;
        let local_types = build_local_types(func);
        asm.set_layout_context(func.name.as_str(), layout.frame_size);
        asm.needs_frame = layout.frame_size > 0;
        if layout.frame_size > 0 {
            emit_prologue(&mut asm, &layout)?;
            spill_arguments(&mut asm, &layout, func, &local_types)?;
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
        asm.clear_layout_context();
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
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, LirType::I128) {
        load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
        load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
        match op {
            BitOp::And => {
                emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_and_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
            BitOp::Or => {
                emit_or_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_or_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
            BitOp::Xor => {
                emit_eor_reg(asm, Reg::X16, Reg::X16, Reg::X9);
                emit_eor_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            }
        }
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
    match op {
        BitOp::And => emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17),
        BitOp::Or => emit_or_reg(asm, Reg::X16, Reg::X16, Reg::X17),
        BitOp::Xor => emit_eor_reg(asm, Reg::X16, Reg::X16, Reg::X17),
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if matches!(lhs_ty, LirType::I128) {
        return emit_i128_shift(asm, layout, dst_id, lhs, rhs, kind, reg_types, local_types);
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
    match kind {
        ShiftKind::Left => emit_lslv(asm, Reg::X16, Reg::X16, Reg::X17),
        ShiftKind::Right => emit_lsrv(asm, Reg::X16, Reg::X16, Reg::X17),
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    let ty = value_type(value, reg_types, local_types)?;
    if matches!(ty, LirType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        emit_mov_imm16(asm, Reg::X9, 0);
        emit_sub_imm12(asm, Reg::X9, Reg::X9, 1);
        emit_sub_reg(asm, Reg::X16, Reg::X9, Reg::X16);
        emit_sub_reg(asm, Reg::X17, Reg::X9, Reg::X17);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    emit_mov_imm16(asm, Reg::X17, 0);
    emit_sub_imm12(asm, Reg::X17, Reg::X17, 1);
    emit_sub_reg(asm, Reg::X16, Reg::X17, Reg::X16);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    if matches!(dst_ty, LirType::I128) {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        if src_bits < 64 {
            let mask = (1u64 << src_bits) - 1;
            emit_mov_imm64(asm, Reg::X17, mask);
            emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
        }
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    if src_bits < 64 {
        let mask = if src_bits == 64 {
            u64::MAX
        } else {
            (1u64 << src_bits) - 1
        };
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    let src_ty = value_type(value, reg_types, local_types)?;
    if matches!(src_ty, LirType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    }
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    if matches!(dst_ty, LirType::I128) {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        emit_mov_reg(asm, Reg::X17, Reg::X16);
        emit_mov_imm16(asm, Reg::X9, 63);
        emit_asrv(asm, Reg::X17, Reg::X17, Reg::X9);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    if src_bits < 64 {
        let shift = 64 - src_bits;
        emit_mov_imm16(asm, Reg::X17, shift as u16);
        emit_lslv(asm, Reg::X16, Reg::X16, Reg::X17);
        emit_asrv(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    if matches!(dst_ty, LirType::I128) {
        return emit_sext(asm, layout, dst_id, value, dst_ty, reg_types, local_types);
    }
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
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    let dst_ty = reg_types
        .get(&dst_id)
        .ok_or_else(|| Error::from("missing type for ptrtoint"))?;
    let dst_bits = int_bits(dst_ty)?;
    if matches!(dst_ty, LirType::I128) {
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    if dst_bits < 64 {
        let mask = (1u64 << dst_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    if matches!(src_ty, LirType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    }
    if src_bits < 64 {
        let mask = (1u64 << src_bits) - 1;
        emit_mov_imm64(asm, Reg::X17, mask);
        emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        load_value_float(asm, layout, value, FReg::V0, &ty, reg_types, local_types)?;
        store_vreg_float(asm, layout, dst_id, FReg::V0, &ty)?;
        return Ok(());
    }
    if matches!(ty, LirType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
    if matches!(output_ty, LirType::I128) {
        emit_mov_imm16(asm, Reg::X16, 0);
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    emit_mov_imm16(asm, Reg::X16, 0);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        load_value_float(asm, layout, lhs, FReg::V0, ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, ty, reg_types, local_types)?;
        match op {
            BinOp::Add => emit_fadd(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Sub => emit_fsub(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Mul => emit_fmul(asm, FReg::V0, FReg::V0, FReg::V1, ty),
        }
        store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
        return Ok(());
    }
    if matches!(ty, LirType::I128) {
        return emit_i128_binop(asm, layout, dst_id, lhs, rhs, op, reg_types, local_types);
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
            }
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                emit_mov_imm16(asm, Reg::X17, (imm as u64 & 0xffff) as u16);
                match op {
                    BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                }
            } else {
                match op {
                    BinOp::Add => emit_add_imm12(asm, Reg::X16, Reg::X16, imm as u32),
                    BinOp::Sub => emit_sub_imm12(asm, Reg::X16, Reg::X16, imm as u32),
                    BinOp::Mul => {
                        emit_mov_imm16(asm, Reg::X17, imm as u16);
                        emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                    }
                }
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
            }
        }
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
                emit_load_from_sp(asm, dst, offset);
                return Ok(());
            }
            if matches!(ty, LirType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                LirType::I1 => emit_load8u_from_sp(asm, dst, offset)?,
                LirType::I8 => emit_load8s_from_sp(asm, dst, offset)?,
                LirType::I16 => emit_load16s_from_sp(asm, dst, offset)?,
                LirType::I32 => emit_load32s_from_sp(asm, dst, offset)?,
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for aarch64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            let ty = value_type(value, reg_types, local_types)?;
            if is_aggregate_type(&ty) && size_of(&ty) > 8 {
                emit_mov_reg(asm, dst, Reg::X31);
                add_immediate_offset(asm, dst, offset as i64)?;
                return Ok(());
            }
            if matches!(ty, LirType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            match ty {
                LirType::I1 => emit_load8u_from_sp(asm, dst, offset)?,
                LirType::I8 => emit_load8s_from_sp(asm, dst, offset)?,
                LirType::I16 => emit_load16s_from_sp(asm, dst, offset)?,
                LirType::I32 => emit_load32s_from_sp(asm, dst, offset)?,
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ if is_aggregate_type(&ty) && size_of(&ty) <= 8 => {
                    emit_load_from_sp(asm, dst, offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported value type for aarch64 load: {:?}",
                        ty
                    )));
                }
            }
            Ok(())
        }
        LirValue::Constant(constant) => {
            if size_of(&constant_type(constant)) == 0 {
                emit_mov_imm16(asm, dst, 0);
                return Ok(());
            }
            if matches!(constant_type(constant), LirType::I128) {
                return Err(Error::from("use i128 helper to load 128-bit values"));
            }
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                emit_mov_imm64(asm, dst, imm as u64);
            } else {
                emit_mov_imm16(asm, dst, imm as u16);
            }
            Ok(())
        }
        LirValue::Null(_) | LirValue::Undef(_) => {
            emit_mov_imm16(asm, dst, 0);
            Ok(())
        }
        LirValue::Global(name, _) => {
            emit_load_symbol_addr(asm, dst, name, 0)?;
            Ok(())
        }
        LirValue::Function(name) => {
            emit_load_symbol_addr(asm, dst, name, 0)?;
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported value for aarch64: {:?}",
                ty
            )))
        }
    }
}

fn i128_parts_from_const(constant: &LirConstant) -> Result<(u64, u64)> {
    match constant {
        LirConstant::Int(value, ty) if matches!(ty, LirType::I128) => {
            let lo = *value as u64;
            let hi = if *value < 0 { u64::MAX } else { 0 };
            Ok((lo, hi))
        }
        LirConstant::UInt(value, ty) if matches!(ty, LirType::I128) => Ok((*value as u64, 0)),
        LirConstant::Bool(value) => Ok((if *value { 1 } else { 0 }, 0)),
        LirConstant::Null(_) | LirConstant::Undef(_) => Ok((0, 0)),
        other => Err(Error::from(format!(
            "unsupported i128 constant: {:?}",
            other
        ))),
    }
}

fn load_i128_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    lo: Reg,
    hi: Reg,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, lo, offset);
            emit_load_from_sp(asm, hi, offset + 8);
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_from_sp(asm, lo, offset);
            emit_load_from_sp(asm, hi, offset + 8);
            Ok(())
        }
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_load_from_sp(asm, lo, offset);
            emit_load_from_sp(asm, hi, offset + 8);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let (lo_val, hi_val) = i128_parts_from_const(constant)?;
            emit_mov_imm64(asm, lo, lo_val);
            emit_mov_imm64(asm, hi, hi_val);
            Ok(())
        }
        LirValue::Null(_) | LirValue::Undef(_) => {
            emit_mov_imm16(asm, lo, 0);
            emit_mov_imm16(asm, hi, 0);
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
    emit_store_to_sp(asm, lo, offset);
    emit_store_to_sp(asm, hi, offset + 8);
    Ok(())
}

fn emit_i128_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match op {
        BinOp::Add => {
            load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
            emit_adds_reg(asm, Reg::X16, Reg::X16, Reg::X9);
            emit_adc_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        }
        BinOp::Sub => {
            load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
            load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
            emit_subs_reg(asm, Reg::X16, Reg::X16, Reg::X9);
            emit_sbc_reg(asm, Reg::X17, Reg::X17, Reg::X10);
            store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
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
            )?;
        }
    }
    Ok(())
}

fn emit_i128_shift(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: ShiftKind,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    )
}

fn emit_i128_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    want_rem: bool,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
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
    )
}

fn emit_i128_libcall(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    symbol: &str,
    lhs: &LirValue,
    rhs: Option<&LirValue>,
    shift: Option<&LirValue>,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let mut int_idx = 0usize;
    let mut stack_idx = 0usize;

    load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
    push_int_arg(
        asm,
        layout,
        Reg::X16,
        &mut int_idx,
        &mut stack_idx,
        &arg_regs,
    )?;
    push_int_arg(
        asm,
        layout,
        Reg::X17,
        &mut int_idx,
        &mut stack_idx,
        &arg_regs,
    )?;

    if let Some(rhs) = rhs {
        load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;
        push_int_arg(
            asm,
            layout,
            Reg::X9,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
        push_int_arg(
            asm,
            layout,
            Reg::X10,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
    }

    if let Some(shift) = shift {
        load_value(asm, layout, shift, Reg::X9, reg_types, local_types)?;
        push_int_arg(
            asm,
            layout,
            Reg::X9,
            &mut int_idx,
            &mut stack_idx,
            &arg_regs,
        )?;
    }

    asm.emit_bl_external(symbol);
    store_i128_value(asm, layout, dst_id, Reg::X0, Reg::X1)?;
    Ok(())
}

fn push_int_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: Reg,
    int_idx: &mut usize,
    stack_idx: &mut usize,
    arg_regs: &[Reg],
) -> Result<()> {
    if *int_idx < arg_regs.len() {
        emit_mov_reg(asm, arg_regs[*int_idx], value);
        *int_idx += 1;
    } else {
        let offset = (*stack_idx as i32) * 8;
        if offset + 8 > layout.outgoing_size {
            return Err(Error::from("outgoing arg offset out of range"));
        }
        emit_store_to_sp(asm, value, offset);
        *stack_idx += 1;
    }
    Ok(())
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
            emit_load_float_from_sp(asm, dst, offset, ty);
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_float_from_sp(asm, dst, offset, ty);
            Ok(())
        }
        LirValue::Constant(LirConstant::Float(value, _)) => {
            if matches!(ty, LirType::F32) {
                let bits = (*value as f32).to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_fmov_s_from_w(asm, dst, Reg::X16);
            } else {
                let bits = value.to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 32) & 0xffff) as u16, 32);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 48) & 0xffff) as u16, 48);
                emit_fmov_d_from_x(asm, dst, Reg::X16);
            }
            Ok(())
        }
        _ => {
            let actual = value_type(value, reg_types, local_types)?;
            Err(Error::from(format!(
                "unsupported float value for aarch64: {:?}",
                actual
            )))
        }
    }
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_store_to_sp(asm, src, offset);
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
    emit_store_float_to_sp(asm, src, offset, ty);
    Ok(())
}

fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &LirValue,
    ty: &LirType,
) -> Result<()> {
    if matches!(ty, LirType::I128) {
        match address {
            LirValue::StackSlot(id) => {
                let offset = stack_slot_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X16, offset);
                emit_load_from_sp(asm, Reg::X17, offset + 8);
            }
            LirValue::Register(id) => {
                let offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, offset);
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8)?;
                emit_load_from_reg(asm, Reg::X17, Reg::X9);
            }
            LirValue::Local(id) => {
                let offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, offset);
                emit_load_from_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8)?;
                emit_load_from_reg(asm, Reg::X17, Reg::X9);
            }
            _ => return Err(Error::from("unsupported load address for i128 on aarch64")),
        }
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
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
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_reg_to_sp(asm, Reg::X17, dst_offset, size)?;
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_reg_to_sp(asm, Reg::X17, dst_offset, size)?;
            }
            _ => return Err(Error::from("unsupported load address for aarch64")),
        }
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64)?;
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        asm.record_vreg_sp_offset(dst_id, dst_offset);
        return Ok(());
    }
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_load_float_from_sp(asm, FReg::V0, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_load8u_from_sp(asm, Reg::X16, offset)?,
                    LirType::I8 => emit_load8s_from_sp(asm, Reg::X16, offset)?,
                    LirType::I16 => emit_load16s_from_sp(asm, Reg::X16, offset)?,
                    LirType::I32 => emit_load32s_from_sp(asm, Reg::X16, offset)?,
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_load_from_sp(asm, Reg::X16, offset);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_sp(asm, Reg::X16, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X16)?;
            }
            Ok(())
        }
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            if is_float_type(ty) {
                emit_load_float_from_reg(asm, FReg::V0, Reg::X16, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_load8u_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I8 => emit_load8s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I16 => emit_load16s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I32 => emit_load32s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X17)?;
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let offset = local_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            if is_float_type(ty) {
                emit_load_float_from_reg(asm, FReg::V0, Reg::X16, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                match ty {
                    LirType::I1 => emit_load8u_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I8 => emit_load8s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I16 => emit_load16s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I32 => emit_load32s_from_reg(asm, Reg::X17, Reg::X16),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ if is_aggregate_type(ty) && size_of(ty) <= 8 => {
                        emit_load_from_reg(asm, Reg::X17, Reg::X16);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported load type for aarch64: {:?}",
                            ty
                        )));
                    }
                }
                store_vreg(asm, layout, dst_id, Reg::X17)?;
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for aarch64")),
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
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_sp(asm, Reg::X16, dst_offset);
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X17);
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_reg(asm, Reg::X16, Reg::X17);
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
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
        let store_elem_sp = |asm: &mut Assembler, offset: i32| -> Result<()> {
            match elem_size {
                1 => emit_store8_to_sp(asm, Reg::X16, offset),
                2 => emit_store16_to_sp(asm, Reg::X16, offset),
                4 => emit_store32_to_sp(asm, Reg::X16, offset),
                8 => {
                    emit_store_to_sp(asm, Reg::X16, offset);
                    Ok(())
                }
                _ => Err(Error::from("unsupported array element size in constant store")),
            }
        };
        let store_elem_reg = |asm: &mut Assembler| -> Result<()> {
            match elem_size {
                1 => {
                    emit_store8_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                2 => {
                    emit_store16_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                4 => {
                    emit_store32_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                8 => {
                    emit_store_to_reg(asm, Reg::X16, Reg::X9);
                    Ok(())
                }
                _ => Err(Error::from("unsupported array element size in constant store")),
            }
        };
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                for (idx, elem) in values.iter().enumerate() {
                    let offset = dst_offset + (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    store_elem_sp(asm, offset)?;
                }
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    emit_mov_reg(asm, Reg::X9, Reg::X17);
                    add_immediate_offset(asm, Reg::X9, offset as i64)?;
                    store_elem_reg(asm)?;
                }
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                for (idx, elem) in values.iter().enumerate() {
                    let offset = (idx as i32) * elem_size;
                    match elem {
                        LirConstant::String(text) => {
                            let ro_offset = intern_cstring(rodata, rodata_pool, text);
                            emit_load_rodata_addr(asm, Reg::X16, ro_offset as i64)?;
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    emit_mov_reg(asm, Reg::X9, Reg::X17);
                    add_immediate_offset(asm, Reg::X9, offset as i64)?;
                    store_elem_reg(asm)?;
                }
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
        }
        return Ok(());
    }
    let value_ty = value_type(value, reg_types, local_types)?;
    if matches!(value_ty, LirType::I128) {
        load_i128_value(
            asm,
            layout,
            value,
            Reg::X16,
            Reg::X17,
            reg_types,
            local_types,
        )?;
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                emit_store_to_sp(asm, Reg::X16, dst_offset);
                emit_store_to_sp(asm, Reg::X17, dst_offset + 8);
            }
            LirValue::Register(id) => {
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, addr_offset);
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8)?;
                emit_store_to_reg(asm, Reg::X17, Reg::X9);
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X9, addr_offset);
                emit_store_to_reg(asm, Reg::X16, Reg::X9);
                add_immediate_offset(asm, Reg::X9, 8)?;
                emit_store_to_reg(asm, Reg::X17, Reg::X9);
            }
            _ => return Err(Error::from("unsupported store address for i128 on aarch64")),
        }
        return Ok(());
    }
    if matches!(value, LirValue::Constant(LirConstant::Array(values, _)) if values.is_empty()) {
        return Ok(());
    }
    if size_of(&value_ty) == 0 {
        return Ok(());
    }
    if let LirValue::Constant(constant) = value {
        if matches!(
            constant,
            LirConstant::Struct(_, _) | LirConstant::Array(_, _)
        ) && size_of(&value_ty) <= 8
        {
            let bits = pack_small_aggregate(constant, &value_ty)?;
            emit_mov_imm64(asm, Reg::X16, bits);
            match address {
                LirValue::StackSlot(id) => {
                    let dst_offset = stack_slot_offset(layout, *id)?;
                    emit_store_to_sp(asm, Reg::X16, dst_offset);
                }
                LirValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_store_to_reg(asm, Reg::X16, Reg::X17);
                }
                LirValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    emit_store_to_reg(asm, Reg::X16, Reg::X17);
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
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
                                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
                                emit_mov_imm16(asm, Reg::X16, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::X16, bits);
                            }
                        }
                        let store_offset = dst_offset + field_offset as i32;
                        match field_size {
                            1 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                            2 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                            4 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                            8 => emit_store_to_sp(asm, Reg::X16, store_offset),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                LirValue::Register(id) => {
                    let addr_offset = vreg_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
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
                                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
                                emit_mov_imm16(asm, Reg::X16, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::X16, bits);
                            }
                        }
                        emit_mov_reg(asm, Reg::X9, Reg::X17);
                        add_immediate_offset(asm, Reg::X9, field_offset as i64)?;
                        match field_size {
                            1 => emit_store8_to_reg(asm, Reg::X16, Reg::X9),
                            2 => emit_store16_to_reg(asm, Reg::X16, Reg::X9),
                            4 => emit_store32_to_reg(asm, Reg::X16, Reg::X9),
                            8 => emit_store_to_reg(asm, Reg::X16, Reg::X9),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                LirValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
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
                                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                            }
                            LirConstant::Null(_) | LirConstant::Undef(_) => {
                                emit_mov_imm16(asm, Reg::X16, 0);
                            }
                            other => {
                                let bits = constant_to_u64_bits(other)?;
                                emit_mov_imm64(asm, Reg::X16, bits);
                            }
                        }
                        emit_mov_reg(asm, Reg::X9, Reg::X17);
                        add_immediate_offset(asm, Reg::X9, field_offset as i64)?;
                        match field_size {
                            1 => emit_store8_to_reg(asm, Reg::X16, Reg::X9),
                            2 => emit_store16_to_reg(asm, Reg::X16, Reg::X9),
                            4 => emit_store32_to_reg(asm, Reg::X16, Reg::X9),
                            8 => emit_store_to_reg(asm, Reg::X16, Reg::X9),
                            _ => {
                                return Err(Error::from(
                                    "unsupported aggregate field size in constant store",
                                ));
                            }
                        }
                    }
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
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
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    zero_reg_range(asm, Reg::X17, size)?;
                }
                LirValue::Local(id) => {
                    let addr_offset = local_offset(layout, *id)?;
                    emit_load_from_sp(asm, Reg::X17, addr_offset);
                    zero_reg_range(asm, Reg::X17, size)?;
                }
                _ => return Err(Error::from("unsupported store address for aarch64")),
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
                )));
            }
        };
        match address {
            LirValue::StackSlot(id) => {
                let dst_offset = stack_slot_offset(layout, *id)?;
                copy_sp_to_sp(asm, src_offset, dst_offset, size)?;
            }
            LirValue::Register(id) => {
                if let Some(offset) = asm.vreg_sp_offset(*id) {
                    asm.log_stack_write(offset, size, "store-agg-via-reg");
                }
                let addr_offset = vreg_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::X17, size)?;
            }
            LirValue::Local(id) => {
                let addr_offset = local_offset(layout, *id)?;
                asm.log_stack_write(addr_offset, size, "store-agg-via-local");
                let addr_offset = local_offset(layout, *id)?;
                emit_load_from_sp(asm, Reg::X17, addr_offset);
                copy_sp_to_reg(asm, src_offset, Reg::X17, size)?;
            }
            _ => return Err(Error::from("unsupported store address for aarch64")),
        }
        return Ok(());
    }
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_sp(asm, FReg::V0, offset, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_store8_to_sp(asm, Reg::X16, offset)?,
                    LirType::I16 => emit_store16_to_sp(asm, Reg::X16, offset)?,
                    LirType::I32 => emit_store32_to_sp(asm, Reg::X16, offset)?,
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_store_to_sp(asm, Reg::X16, offset);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_sp(asm, Reg::X16, offset);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        LirValue::Register(id) => {
            let store_size = if is_float_type(&value_ty) {
                if matches!(value_ty, LirType::F32) {
                    4
                } else {
                    8
                }
            } else {
                size_of(&value_ty) as i32
            };
            if let Some(offset) = asm.vreg_sp_offset(*id) {
                asm.log_stack_write(offset, store_size, "store-via-reg");
            }
            let addr_offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X17, addr_offset);
            if is_float_type(&value_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_reg(asm, FReg::V0, Reg::X17, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_store8_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I16 => emit_store16_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I32 => emit_store32_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        LirValue::Local(id) => {
            let store_size = if is_float_type(&value_ty) {
                if matches!(value_ty, LirType::F32) {
                    4
                } else {
                    8
                }
            } else {
                size_of(&value_ty) as i32
            };
            let addr_offset = local_offset(layout, *id)?;
            asm.log_stack_write(addr_offset, store_size, "store-via-local");
            emit_load_from_sp(asm, Reg::X17, addr_offset);
            if is_float_type(&value_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    &value_ty,
                    reg_types,
                    local_types,
                )?;
                emit_store_float_to_reg(asm, FReg::V0, Reg::X17, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                match value_ty {
                    LirType::I1 | LirType::I8 => emit_store8_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I16 => emit_store16_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I32 => emit_store32_to_reg(asm, Reg::X16, Reg::X17),
                    LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ if is_aggregate_type(&value_ty) && size_of(&value_ty) <= 8 => {
                        emit_store_to_reg(asm, Reg::X16, Reg::X17);
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported store type for aarch64: {:?}",
                            value_ty
                        )));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for aarch64")),
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
            load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
            load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
            let symbol = if matches!(lhs_ty, LirType::F32) {
                "fmodf"
            } else {
                "fmod"
            };
            asm.emit_bl_external(symbol);
            store_vreg_float(asm, layout, dst_id, FReg::V0, &lhs_ty)?;
            return Ok(());
        }
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
        emit_fdiv(asm, FReg::V0, FReg::V0, FReg::V1, &lhs_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &lhs_ty)?;
        return Ok(());
    }
    if matches!(lhs_ty, LirType::I128) {
        return emit_i128_divrem(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            want_rem,
            reg_types,
            local_types,
        );
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;

    emit_sdiv(asm, Reg::X9, Reg::X16, Reg::X17);

    if want_rem {
        emit_msub(asm, Reg::X16, Reg::X9, Reg::X17, Reg::X16);
    } else {
        emit_mov_reg(asm, Reg::X16, Reg::X9);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;

    Ok(())
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
        _ => CallTarget::Indirect,
    };

    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let float_regs = [
        FReg::V0,
        FReg::V1,
        FReg::V2,
        FReg::V3,
        FReg::V4,
        FReg::V5,
        FReg::V6,
        FReg::V7,
    ];

    let needs_sret = returns_aggregate(ret_ty);
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;
    let mut stack_bytes = 0i32;

    let mut sret_offset = None;
    if needs_sret {
        let agg_off = agg_offset(layout, dst_id)?;
        emit_mov_reg(asm, arg_regs[0], Reg::X31);
        add_immediate_offset(asm, arg_regs[0], agg_off as i64)?;
        int_idx = 1;
        sret_offset = Some(agg_off);
    }

    let is_printf_varargs = matches!(function, LirValue::Function(name) if name == "printf");

    if abi_debug_enabled() {
        let name = match function {
            LirValue::Function(name) => name.as_str(),
            _ => "<unknown>",
        };
        let sp_aligned = layout.frame_size % 16 == 0;
        abi_log(&format!(
            "call {}: sp_align16={} frame_size={} outgoing_size={} needs_sret={}",
            name, sp_aligned, layout.frame_size, layout.outgoing_size, needs_sret
        ));
        if !sp_aligned {
            abi_log("warning: SP is not 16-byte aligned at call boundary");
        }
    }

    if is_printf_varargs {
        if args.is_empty() {
            return Err(Error::from(
                "printf call expects at least a format argument",
            ));
        }
        let format_arg = &args[0];
        if let LirValue::Constant(LirConstant::String(text)) = format_arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            emit_load_rodata_addr(asm, Reg::X0, offset as i64)?;
            if abi_debug_enabled() {
                abi_log(&format!("  printf format -> x0 (rodata+{})", offset));
            }
        } else {
            load_value(asm, layout, format_arg, Reg::X0, reg_types, local_types)?;
            if abi_debug_enabled() {
                abi_log("  printf format -> x0");
            }
        }
        let mut stack_offset = 0i32;
        int_idx = 1;
        float_idx = 0;
        for arg in args.iter().skip(1) {
            let arg_ty = value_type(arg, reg_types, local_types)?;
            if is_large_aggregate(&arg_ty) {
                let size = store_vararg_value(
                    asm,
                    layout,
                    stack_offset,
                    arg,
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                if abi_debug_enabled() {
                    abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
                }
                stack_offset += size;
                continue;
            }
            if let LirValue::Constant(LirConstant::String(text)) = arg {
                let offset = intern_cstring(rodata, rodata_pool, text);
                if int_idx < arg_regs.len() {
                    emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                    int_idx += 1;
                }
                emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                emit_store_to_sp(asm, Reg::X16, stack_offset);
                if abi_debug_enabled() {
                    abi_log(&format!("  vararg string -> [sp+{}]", stack_offset));
                }
                stack_offset += 8;
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
                }
            } else if int_idx < arg_regs.len() {
                load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                int_idx += 1;
            }
            let size = store_vararg_value(
                asm,
                layout,
                stack_offset,
                arg,
                &arg_ty,
                reg_types,
                local_types,
            )?;
            if abi_debug_enabled() {
                abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
            }
            stack_offset += size;
        }
        stack_bytes = stack_offset;
    } else {
        for arg in args {
            if let LirValue::Constant(LirConstant::String(text)) = arg {
                let offset = intern_cstring(rodata, rodata_pool, text);
                if int_idx < arg_regs.len() {
                    emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                    if abi_debug_enabled() {
                        abi_log(&format!(
                            "  arg string -> {} (rodata+{})",
                            reg_name(arg_regs[int_idx]),
                            offset
                        ));
                    }
                    int_idx += 1;
                } else {
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    let offset = (stack_idx as i32) * 8;
                    emit_store_to_sp(asm, Reg::X16, offset);
                    if abi_debug_enabled() {
                        abi_log(&format!("  arg string -> [sp+{}]", offset));
                    }
                    stack_idx += 1;
                }
                continue;
            }
            let arg_ty = value_type(arg, reg_types, local_types)?;
            if matches!(arg_ty, LirType::I128) {
                load_i128_value(asm, layout, arg, Reg::X16, Reg::X17, reg_types, local_types)?;
                push_int_arg(
                    asm,
                    layout,
                    Reg::X16,
                    &mut int_idx,
                    &mut stack_idx,
                    &arg_regs,
                )?;
                push_int_arg(
                    asm,
                    layout,
                    Reg::X17,
                    &mut int_idx,
                    &mut stack_idx,
                    &arg_regs,
                )?;
                if abi_debug_enabled() {
                    abi_log("  arg i128 -> pair");
                }
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
                    if abi_debug_enabled() {
                        abi_log(&format!(
                            "  arg {:?} -> {}",
                            arg_ty,
                            freg_name(float_regs[float_idx])
                        ));
                    }
                    float_idx += 1;
                } else {
                    let offset = (stack_idx as i32) * 8;
                    store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                    if abi_debug_enabled() {
                        abi_log(&format!("  arg {:?} -> [sp+{}]", arg_ty, offset));
                    }
                    stack_idx += 1;
                }
            } else if int_idx < arg_regs.len() {
                load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                if abi_debug_enabled() {
                    abi_log(&format!(
                        "  arg {:?} -> {}",
                        arg_ty,
                        reg_name(arg_regs[int_idx])
                    ));
                }
                int_idx += 1;
            } else {
                let offset = (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types, local_types)?;
                if abi_debug_enabled() {
                    abi_log(&format!("  arg {:?} -> [sp+{}]", arg_ty, offset));
                }
                stack_idx += 1;
            }
        }
    }

    if abi_debug_enabled() {
        if !is_printf_varargs {
            stack_bytes = (stack_idx as i32) * 8;
        }
        abi_log(&format!(
            "  stack_args={} bytes={} outgoing_cap={}",
            stack_idx, stack_bytes, layout.outgoing_size
        ));
        if stack_bytes > layout.outgoing_size {
            abi_log("warning: outgoing stack arguments exceed reserved frame size");
        }
        if stack_bytes % 16 != 0 {
            abi_log("note: stack argument area size is not 16-byte aligned");
        }
    }

    match target {
        CallTarget::Internal(id) => asm.emit_bl(Label::Function(id)),
        CallTarget::External(name) => {
            if matches!(format, TargetFormat::Coff) {
                asm.emit_bl_external(&name);
            } else {
                asm.emit_bl_external(&name);
            }
        }
        CallTarget::Indirect => {
            load_value(asm, layout, function, Reg::X16, reg_types, local_types)?;
            emit_bl_reg(asm, Reg::X16);
        }
    }

    if needs_sret {
        if let Some(agg_off) = sret_offset {
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            add_immediate_offset(asm, Reg::X16, agg_off as i64)?;
            store_vreg(asm, layout, dst_id, Reg::X16)?;
        }
    } else if matches!(ret_ty, LirType::I128) {
        store_i128_value(asm, layout, dst_id, Reg::X0, Reg::X1)?;
    } else if !matches!(ret_ty, LirType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::V0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::X0)?;
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
            let arg_regs = [
                Reg::X0,
                Reg::X1,
                Reg::X2,
                Reg::X3,
                Reg::X4,
                Reg::X5,
                Reg::X6,
                Reg::X7,
            ];
            let float_regs = [
                FReg::V0,
                FReg::V1,
                FReg::V2,
                FReg::V3,
                FReg::V4,
                FReg::V5,
                FReg::V6,
                FReg::V7,
            ];

            emit_mov_imm16(asm, Reg::X0, 0);
            emit_mov_imm16(asm, Reg::X1, 0);
            emit_load_rodata_addr(asm, Reg::X2, format_offset as i64)?;

            let mut int_idx = 3usize;
            let mut float_idx = 0usize;
            let mut stack_offset = 0i32;
            for arg in args {
                let arg_ty = value_type(arg, reg_types, local_types)?;
                if is_large_aggregate(&arg_ty) {
                    let size = store_vararg_value(
                        asm,
                        layout,
                        stack_offset,
                        arg,
                        &arg_ty,
                        reg_types,
                        local_types,
                    )?;
                    stack_offset += size;
                    continue;
                }
                if let LirValue::Constant(LirConstant::String(text)) = arg {
                    let offset = intern_cstring(rodata, rodata_pool, text);
                    if int_idx < arg_regs.len() {
                        emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                        int_idx += 1;
                    }
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    emit_store_to_sp(asm, Reg::X16, stack_offset);
                    stack_offset += 8;
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
                    }
                } else if int_idx < arg_regs.len() {
                    load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                    int_idx += 1;
                }
                let size = store_vararg_value(
                    asm,
                    layout,
                    stack_offset,
                    arg,
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                stack_offset += size;
            }

            asm.emit_bl_external("snprintf");

            emit_mov_reg(asm, Reg::X16, Reg::X0);
            emit_add_imm12(asm, Reg::X16, Reg::X16, 1);
            emit_mov_reg(asm, Reg::X0, Reg::X16);
            asm.emit_bl_external("malloc");
            emit_mov_reg(asm, Reg::X9, Reg::X0);

            emit_mov_reg(asm, Reg::X0, Reg::X9);
            emit_mov_reg(asm, Reg::X1, Reg::X16);
            emit_load_rodata_addr(asm, Reg::X2, format_offset as i64)?;

            int_idx = 3usize;
            float_idx = 0usize;
            stack_offset = 0i32;
            for arg in args {
                let arg_ty = value_type(arg, reg_types, local_types)?;
                if is_large_aggregate(&arg_ty) {
                    let size = store_vararg_value(
                        asm,
                        layout,
                        stack_offset,
                        arg,
                        &arg_ty,
                        reg_types,
                        local_types,
                    )?;
                    stack_offset += size;
                    continue;
                }
                if let LirValue::Constant(LirConstant::String(text)) = arg {
                    let offset = intern_cstring(rodata, rodata_pool, text);
                    if int_idx < arg_regs.len() {
                        emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                        int_idx += 1;
                    }
                    emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                    emit_store_to_sp(asm, Reg::X16, stack_offset);
                    stack_offset += 8;
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
                    }
                } else if int_idx < arg_regs.len() {
                    load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
                    int_idx += 1;
                }
                let size = store_vararg_value(
                    asm,
                    layout,
                    stack_offset,
                    arg,
                    &arg_ty,
                    reg_types,
                    local_types,
                )?;
                stack_offset += size;
            }

            asm.emit_bl_external("snprintf");
            emit_mov_reg(asm, Reg::X0, Reg::X9);
            store_vreg(asm, layout, dst_id, Reg::X0)?;
            return Ok(());
        }
        LirIntrinsicKind::TimeNow => {
            if !is_float_type(result_ty) {
                return Err(Error::from("TimeNow expects floating-point result"));
            }
            emit_mov_imm16(asm, Reg::X0, 0);
            asm.emit_bl_external("time");
            emit_scvtf(asm, FReg::V0, Reg::X0, result_ty, true);
            store_vreg_float(asm, layout, dst_id, FReg::V0, result_ty)?;
            return Ok(());
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    emit_load_rodata_addr(asm, Reg::X0, format_offset as i64)?;
    if abi_debug_enabled() {
        let sp_aligned = layout.frame_size % 16 == 0;
        abi_log(&format!(
            "call printf (varargs): sp_align16={} frame_size={} outgoing_size={} format=rodata+{}",
            sp_aligned, layout.frame_size, layout.outgoing_size, format_offset
        ));
        if !sp_aligned {
            abi_log("warning: SP is not 16-byte aligned at call boundary");
        }
        abi_log("  arg format -> x0");
    }

    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let float_regs = [
        FReg::V0,
        FReg::V1,
        FReg::V2,
        FReg::V3,
        FReg::V4,
        FReg::V5,
        FReg::V6,
        FReg::V7,
    ];

    let mut int_idx = 1usize;
    let mut float_idx = 0usize;
    let mut stack_offset = 0i32;
    for arg in args {
        let arg_ty = value_type(arg, reg_types, local_types)?;
        if is_large_aggregate(&arg_ty) {
            let size = store_vararg_value(
                asm,
                layout,
                stack_offset,
                arg,
                &arg_ty,
                reg_types,
                local_types,
            )?;
            if abi_debug_enabled() {
                abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
            }
            stack_offset += size;
            continue;
        }
        if let LirValue::Constant(LirConstant::String(text)) = arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            if int_idx < arg_regs.len() {
                emit_load_rodata_addr(asm, arg_regs[int_idx], offset as i64)?;
                int_idx += 1;
            }
            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
            emit_store_to_sp(asm, Reg::X16, stack_offset);
            if abi_debug_enabled() {
                abi_log(&format!("  vararg string -> [sp+{}]", stack_offset));
            }
            stack_offset += 8;
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
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types, local_types)?;
            int_idx += 1;
        }
        let size = store_vararg_value(
            asm,
            layout,
            stack_offset,
            arg,
            &arg_ty,
            reg_types,
            local_types,
        )?;
        if abi_debug_enabled() {
            abi_log(&format!("  vararg {:?} -> [sp+{}]", arg_ty, stack_offset));
        }
        stack_offset += size;
    }
    let stack_bytes = stack_offset;

    if abi_debug_enabled() {
        abi_log(&format!(
            "  stack_args={} bytes={} outgoing_cap={}",
            (stack_bytes / 8),
            stack_bytes,
            layout.outgoing_size
        ));
        if stack_bytes > layout.outgoing_size {
            abi_log("warning: outgoing stack arguments exceed reserved frame size");
        }
        if stack_bytes % 16 != 0 {
            abi_log("note: stack argument area size is not 16-byte aligned");
        }
    }

    if matches!(target_format, TargetFormat::Coff) {
        asm.emit_bl_external("printf");
    } else {
        asm.emit_bl_external("printf");
    }
    Ok(())
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
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    emit_scvtf(asm, FReg::V0, Reg::X16, dst_ty, signed);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
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
    signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    let src_ty = value_type(value, reg_types, local_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvtzs(asm, Reg::X16, FReg::V0, &src_ty, signed);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        return Err(Error::from("unsupported FPTrunc on aarch64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvt_sd(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
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
        return Err(Error::from("unsupported FPExt on aarch64"));
    }
    load_value_float(
        asm,
        layout,
        value,
        FReg::V0,
        &src_ty,
        reg_types,
        local_types,
    )?;
    emit_fcvt_ds(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
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
    let mut const_offset = if let LirValue::Register(id) = ptr {
        asm.vreg_sp_offset(*id).map(|offset| offset as i64)
    } else {
        None
    };

    load_value(asm, layout, ptr, Reg::X16, reg_types, local_types)?;
    for index in indices {
        match &current_ty {
            LirType::Struct { fields, .. } => {
                let idx = match index {
                    LirValue::Constant(constant) => {
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
                add_immediate_offset(asm, Reg::X16, field_offset as i64)?;
                if let Some(base) = const_offset.as_mut() {
                    *base += field_offset as i64;
                }
                current_ty = fields
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| Error::from("GEP struct field out of range"))?;
            }
            LirType::Array(elem, _) | LirType::Vector(elem, _) => {
                match index {
                    LirValue::Constant(constant) => {
                        if let Some(base) = const_offset.as_mut() {
                            let idx = constant_to_i64(constant)?;
                            *base += idx * size_of(elem) as i64;
                        }
                    }
                    _ => const_offset = None,
                }
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
                match index {
                    LirValue::Constant(constant) => {
                        if let Some(base) = const_offset.as_mut() {
                            let idx = constant_to_i64(constant)?;
                            *base += idx * size_of(&current_ty) as i64;
                        }
                    }
                    _ => const_offset = None,
                }
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

    store_vreg(asm, layout, dst_id, Reg::X16)?;
    if let Some(offset) = const_offset {
        if let Ok(offset) = i32::try_from(offset) {
            asm.record_vreg_sp_offset(dst_id, offset);
        }
    }
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
    load_value(asm, layout, index, Reg::X17, reg_types, local_types)?;
    if elem_size != 1 {
        let imm = u16::try_from(elem_size)
            .map_err(|_| Error::from("GEP element size too large for aarch64"))?;
        emit_mov_imm16(asm, Reg::X9, imm);
        emit_mul_reg(asm, Reg::X17, Reg::X17, Reg::X9);
    }
    emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    Ok(())
}

fn add_immediate_offset(asm: &mut Assembler, base: Reg, offset: i64) -> Result<()> {
    if offset == 0 {
        return Ok(());
    }
    if offset < 0 {
        let abs = (-offset) as u64;
        if abs <= 4095 {
            emit_sub_imm12(asm, base, base, abs as u32);
            return Ok(());
        }
        if let Ok(imm) = u16::try_from(abs) {
            emit_mov_imm16(asm, Reg::X17, imm);
            emit_sub_reg(asm, base, base, Reg::X17);
        } else {
            emit_mov_imm64(asm, Reg::X9, abs as u64);
            emit_sub_reg(asm, base, base, Reg::X9);
        }
        return Ok(());
    }
    if offset <= 4095 {
        emit_add_imm12(asm, base, base, offset as u32);
        return Ok(());
    }
    if let Ok(imm) = u16::try_from(offset) {
        emit_mov_imm16(asm, Reg::X17, imm);
        emit_add_reg(asm, base, base, Reg::X17);
    } else {
        emit_mov_imm64(asm, Reg::X9, offset as u64);
        emit_add_reg(asm, base, base, Reg::X9);
    }
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
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_load_from_sp(asm, Reg::X16, src + offset);
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_load32u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_load16u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_load8u_from_sp(asm, Reg::X16, src + offset)?;
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

fn copy_sp_to_reg(asm: &mut Assembler, src: i32, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_load_from_sp(asm, Reg::X16, src + offset);
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_store_to_reg(asm, Reg::X16, Reg::X9);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_load32u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_store32_to_reg(asm, Reg::X16, Reg::X9);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_load16u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_store16_to_reg(asm, Reg::X16, Reg::X9);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_load8u_from_sp(asm, Reg::X16, src + offset)?;
        emit_mov_reg(asm, Reg::X9, dst);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_store8_to_reg(asm, Reg::X16, Reg::X9);
    }
    Ok(())
}

fn copy_reg_to_sp(asm: &mut Assembler, src: Reg, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load_from_reg(asm, Reg::X16, Reg::X9);
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load32u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load16u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load8u_from_reg(asm, Reg::X16, Reg::X9);
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

fn copy_reg_to_reg(asm: &mut Assembler, src: Reg, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store_to_reg(asm, Reg::X16, Reg::X17);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load32u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store32_to_reg(asm, Reg::X16, Reg::X17);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load16u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store16_to_reg(asm, Reg::X16, Reg::X17);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X9, src);
        add_immediate_offset(asm, Reg::X9, offset as i64)?;
        emit_load8u_from_reg(asm, Reg::X16, Reg::X9);
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store8_to_reg(asm, Reg::X16, Reg::X17);
    }
    Ok(())
}

fn zero_sp_range(asm: &mut Assembler, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm16(asm, Reg::X16, 0);
    while offset + 8 <= size {
        emit_store_to_sp(asm, Reg::X16, dst + offset);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_store32_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_store16_to_sp(asm, Reg::X16, dst + offset)?;
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_store8_to_sp(asm, Reg::X16, dst + offset)?;
    }
    Ok(())
}

fn zero_reg_range(asm: &mut Assembler, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm16(asm, Reg::X16, 0);
    while offset + 8 <= size {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store_to_reg(asm, Reg::X16, Reg::X17);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store32_to_reg(asm, Reg::X16, Reg::X17);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store16_to_reg(asm, Reg::X16, Reg::X17);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_reg(asm, Reg::X17, dst);
        add_immediate_offset(asm, Reg::X17, offset as i64)?;
        emit_store8_to_reg(asm, Reg::X16, Reg::X17);
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
        return Err(Error::from("unsupported bitcast size for aarch64"));
    }
    if src_size == 0 {
        return Ok(());
    }
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        let size = size_of(&agg_ty) as i32;
        if size == 0 {
            return Ok(());
        }
        let (field_offset, field_ty) = aggregate_field_offset(&agg_ty, indices)?;
        if field_offset != 0 || size_of(&field_ty) as i32 != size {
            return Err(Error::from("unsupported InsertValue for small aggregate"));
        }
        if is_float_type(&field_ty) {
            return Err(Error::from(
                "unsupported float InsertValue for small aggregate",
            ));
        }
        load_value(asm, layout, element, Reg::X16, reg_types, local_types)?;
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
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
                            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                        }
                        LirConstant::Null(_) | LirConstant::Undef(_) => {
                            emit_mov_imm16(asm, Reg::X16, 0);
                        }
                        other => {
                            let bits = constant_to_u64_bits(other)?;
                            emit_mov_imm64(asm, Reg::X16, bits);
                        }
                    }
                    let dst = store_offset + field_offset as i32;
                    match field_size {
                        1 => emit_store8_to_sp(asm, Reg::X16, dst)?,
                        2 => emit_store16_to_sp(asm, Reg::X16, dst)?,
                        4 => emit_store32_to_sp(asm, Reg::X16, dst)?,
                        8 => emit_store_to_sp(asm, Reg::X16, dst),
                        _ => {
                            return Err(Error::from(
                                "unsupported aggregate field size in InsertValue",
                            ));
                        }
                    }
                }
            }
            LirValue::Constant(LirConstant::Undef(_)) => {
                zero_sp_range(asm, store_offset, field_size)?;
            }
            _ => return Err(Error::from("unsupported InsertValue aggregate element")),
        }
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64)?;
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    if is_float_type(&field_ty) {
        load_value_float(
            asm,
            layout,
            element,
            FReg::V0,
            &field_ty,
            reg_types,
            local_types,
        )?;
        emit_store_float_to_sp(asm, FReg::V0, store_offset, &field_ty);
    } else {
        if let LirValue::Constant(LirConstant::String(text)) = element {
            let offset = intern_cstring(rodata, rodata_pool, text);
            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
            match field_ty {
                LirType::I1 | LirType::I8 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I16 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I32 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for aarch64: {:?}",
                        field_ty
                    )));
                }
            }
        } else {
            load_value(asm, layout, element, Reg::X16, reg_types, local_types)?;
            match field_ty {
                LirType::I1 | LirType::I8 => emit_store8_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I16 => emit_store16_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I32 => emit_store32_to_sp(asm, Reg::X16, store_offset)?,
                LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ if is_aggregate_type(&field_ty) && size_of(&field_ty) <= 8 => {
                    emit_store_to_sp(asm, Reg::X16, store_offset);
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported InsertValue element type for aarch64: {:?}",
                        field_ty
                    )));
                }
            }
        }
    }

    emit_mov_reg(asm, Reg::X16, Reg::X31);
    add_immediate_offset(asm, Reg::X16, dst_offset as i64)?;
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    asm.record_vreg_sp_offset(dst_id, dst_offset);
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
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            add_immediate_offset(asm, Reg::X16, dst_offset as i64)?;
            store_vreg(asm, layout, dst_id, Reg::X16)?;
            asm.record_vreg_sp_offset(dst_id, dst_offset);
        } else {
            emit_mov_reg(asm, Reg::X16, Reg::X31);
            add_immediate_offset(asm, Reg::X16, load_offset as i64)?;
            store_vreg(asm, layout, dst_id, Reg::X16)?;
            asm.record_vreg_sp_offset(dst_id, load_offset);
        }
        return Ok(());
    }
    if is_float_type(&result_ty) {
        emit_load_float_from_sp(asm, FReg::V0, load_offset, &result_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &result_ty)?;
    } else {
        match result_ty {
            LirType::I1 => emit_load8u_from_sp(asm, Reg::X16, load_offset)?,
            LirType::I8 => emit_load8s_from_sp(asm, Reg::X16, load_offset)?,
            LirType::I16 => emit_load16s_from_sp(asm, Reg::X16, load_offset)?,
            LirType::I32 => emit_load32s_from_sp(asm, Reg::X16, load_offset)?,
            LirType::I64 | LirType::Ptr(_) | LirType::Function { .. } => {
                emit_load_from_sp(asm, Reg::X16, load_offset);
            }
            _ if is_aggregate_type(&result_ty) && size_of(&result_ty) <= 8 => {
                emit_load_from_sp(asm, Reg::X16, load_offset);
            }
            _ => {
                return Err(Error::from(format!(
                    "unsupported ExtractValue element type for aarch64: {:?}",
                    result_ty
                )));
            }
        }
        store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64)?;
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    emit_mov_imm16(asm, Reg::X16, 0);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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

fn emit_load_rodata_addr(asm: &mut Assembler, dst: Reg, addend: i64) -> Result<()> {
    if matches!(asm.target_format, TargetFormat::MachO) {
        let adrp = 0x9000_0000u32 | dst.id();
        let add = 0x9100_0000u32 | (dst.id() << 5) | dst.id();
        let offset = asm.buf.len();
        asm.emit_u32(adrp);
        asm.emit_u32(add);
        asm.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::Aarch64AdrpAdd,
            section: crate::emit::RelocSection::Text,
            symbol: ".rodata".to_string(),
            addend,
        });
        return Ok(());
    }

    if asm.buf.len() % 8 != 0 {
        emit_nop(asm);
    }
    let ldr_instr = 0x5800_0000u32 | ((2u32 & 0x7ffff) << 5) | dst.id();
    asm.emit_u32(ldr_instr);
    let b_instr = 0x1400_0000u32 | 3;
    asm.emit_u32(b_instr);
    let literal_offset = asm.buf.len();
    asm.relocs.push(Relocation {
        offset: literal_offset as u64,
        kind: RelocKind::Abs64,
        section: crate::emit::RelocSection::Text,
        symbol: ".rodata".to_string(),
        addend,
    });
    asm.extend(&0u64.to_le_bytes());
    Ok(())
}

fn emit_load_symbol_addr(asm: &mut Assembler, dst: Reg, symbol: &str, addend: i64) -> Result<()> {
    if matches!(asm.target_format, TargetFormat::MachO) {
        let adrp = 0x9000_0000u32 | dst.id();
        let add = 0x9100_0000u32 | (dst.id() << 5) | dst.id();
        let offset = asm.buf.len();
        asm.emit_u32(adrp);
        asm.emit_u32(add);
        asm.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::Aarch64AdrpAdd,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend,
        });
        return Ok(());
    }

    if asm.buf.len() % 8 != 0 {
        emit_nop(asm);
    }
    let ldr_instr = 0x5800_0000u32 | ((2u32 & 0x7ffff) << 5) | dst.id();
    asm.emit_u32(ldr_instr);
    let b_instr = 0x1400_0000u32 | 3;
    asm.emit_u32(b_instr);
    let literal_offset = asm.buf.len();
    asm.relocs.push(Relocation {
        offset: literal_offset as u64,
        kind: RelocKind::Abs64,
        section: crate::emit::RelocSection::Text,
        symbol: symbol.to_string(),
        addend,
    });
    asm.extend(&0u64.to_le_bytes());
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
        load_value_float(asm, layout, value, FReg::V0, &ty, reg_types, local_types)?;
        emit_store_float_to_sp(asm, FReg::V0, offset, &ty);
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        emit_store_to_sp(asm, Reg::X16, offset);
    }
    Ok(())
}

fn store_vararg_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &LirValue,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<i32> {
    let size = align8(size_of(ty) as i32);
    if size == 0 {
        return Ok(0);
    }
    if offset < 0 || offset + size > layout.outgoing_size {
        return Err(Error::from("outgoing vararg offset out of range"));
    }
    if size <= 8 {
        store_outgoing_arg(asm, layout, offset, value, reg_types, local_types)?;
        return Ok(size);
    }
    match value {
        LirValue::Register(id) => {
            let src_offset = agg_offset(layout, *id)?;
            copy_sp_to_sp(asm, src_offset, offset, size)?;
            Ok(size)
        }
        _ => Err(Error::from(
            "unsupported large aggregate for varargs on aarch64",
        )),
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
            "unsupported constant for aarch64: {:?}",
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

fn is_float_type(ty: &LirType) -> bool {
    matches!(ty, LirType::F32 | LirType::F64)
}

fn is_aggregate_type(ty: &LirType) -> bool {
    matches!(
        ty,
        LirType::Struct { .. } | LirType::Array(_, _) | LirType::Vector(_, _)
    )
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
        LirType::I1 | LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64 | LirType::I128
    )
}

fn int_bits(ty: &LirType) -> Result<u32> {
    match ty {
        LirType::I1 => Ok(1),
        LirType::I8 => Ok(8),
        LirType::I16 => Ok(16),
        LirType::I32 => Ok(32),
        LirType::I64 => Ok(64),
        LirType::I128 => Ok(128),
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

fn emit_mov_reg(asm: &mut Assembler, dst: Reg, src: Reg) {
    if dst.id() == src.id() {
        return;
    }
    let instr = if dst.is_sp() || src.is_sp() {
        0x9100_0000u32 | (src.id() << 5) | dst.id()
    } else {
        0xAA00_03E0u32 | (src.id() << 16) | dst.id()
    };
    asm.extend(&instr.to_le_bytes());
}

fn spill_arguments(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &LirFunction,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let float_regs = [
        FReg::V0,
        FReg::V1,
        FReg::V2,
        FReg::V3,
        FReg::V4,
        FReg::V5,
        FReg::V6,
        FReg::V7,
    ];

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    if let Some(offset) = layout.sret_offset {
        emit_store_to_sp(asm, arg_regs[0], offset);
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
                let incoming = layout.frame_size + (stack_idx as i32) * 8;
                emit_load_from_sp(asm, Reg::X16, incoming);
                copy_reg_to_sp(asm, Reg::X16, offset, size)?;
                stack_idx += 1;
            }
            continue;
        }
        if is_float_type(ty) {
            if float_idx < float_regs.len() {
                emit_store_float_to_sp(asm, float_regs[float_idx], offset, ty);
                float_idx += 1;
            } else {
                let incoming = layout.frame_size + (stack_idx as i32) * 8;
                emit_load_float_from_sp(asm, FReg::V0, incoming, ty);
                emit_store_float_to_sp(asm, FReg::V0, offset, ty);
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            match size_of(ty) {
                1 => emit_store8_to_sp(asm, arg_regs[int_idx], offset)?,
                2 => emit_store16_to_sp(asm, arg_regs[int_idx], offset)?,
                4 => emit_store32_to_sp(asm, arg_regs[int_idx], offset)?,
                _ => emit_store_to_sp(asm, arg_regs[int_idx], offset),
            }
            int_idx += 1;
        } else {
            let incoming = layout.frame_size + (stack_idx as i32) * 8;
            emit_load_from_sp(asm, Reg::X16, incoming);
            match size_of(ty) {
                1 => emit_store8_to_sp(asm, Reg::X16, offset)?,
                2 => emit_store16_to_sp(asm, Reg::X16, offset)?,
                4 => emit_store32_to_sp(asm, Reg::X16, offset)?,
                _ => emit_store_to_sp(asm, Reg::X16, offset),
            }
            stack_idx += 1;
        }
    }
    Ok(())
}

fn emit_mov_imm16(asm: &mut Assembler, dst: Reg, imm: u16) {
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_mov_imm16(asm, dst, (imm & 0xffff) as u16);
    emit_movk_imm16(asm, dst, ((imm >> 16) & 0xffff) as u16, 16);
    emit_movk_imm16(asm, dst, ((imm >> 32) & 0xffff) as u16, 32);
    emit_movk_imm16(asm, dst, ((imm >> 48) & 0xffff) as u16, 48);
}

fn emit_movk_imm16(asm: &mut Assembler, dst: Reg, imm: u16, shift: u32) {
    let hw = (shift / 16) & 0x3;
    let instr = 0xF280_0000u32 | ((imm as u32) << 5) | (hw << 21) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmov_d_from_x(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x9E67_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmov_s_from_w(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x1E27_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8B00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_adds_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xAB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_and_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8A00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_or_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xAA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_eor_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_lslv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_lsrv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2400u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_asrv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2800u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_subs_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_adc_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9A00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sbc_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xDA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_mul_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9B00_7C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0x9100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0xD100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_ret(asm: &mut Assembler) {
    asm.extend(&0xD65F_03C0u32.to_le_bytes());
}

fn emit_trap(asm: &mut Assembler) {
    asm.extend(&0xD420_0000u32.to_le_bytes());
}

fn emit_exit_syscall(asm: &mut Assembler, code: u16) -> Result<()> {
    emit_mov_imm16(asm, Reg::X0, code);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_reg(asm, Reg::X0, reg);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_svc(asm: &mut Assembler) {
    asm.extend(&0xD400_0001u32.to_le_bytes());
}

fn emit_sub_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0xD100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0x9100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_adjust_sp(asm: &mut Assembler, imm: i32, add: bool) -> Result<()> {
    if imm <= 0 {
        return Ok(());
    }
    let imm = imm as u32;
    if imm <= 0xfff {
        if add {
            emit_add_sp(asm, imm);
        } else {
            emit_sub_sp(asm, imm);
        }
        return Ok(());
    }
    emit_mov_reg(asm, Reg::X16, Reg::X31);
    emit_mov_imm64(asm, Reg::X17, imm as u64);
    if add {
        emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    } else {
        emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    emit_mov_reg(asm, Reg::X31, Reg::X16);
    Ok(())
}

fn emit_sdiv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9AC0_0C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_msub(asm: &mut Assembler, dst: Reg, mul_lhs: Reg, mul_rhs: Reg, add: Reg) {
    let instr =
        0x9B00_8000u32 | (mul_rhs.id() << 16) | (mul_lhs.id() << 5) | dst.id() | (add.id() << 10);
    asm.extend(&instr.to_le_bytes());
}
fn emit_load_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) {
    if offset >= 0 && (offset % 8) == 0 {
        let imm12 = (offset / 8) as u32;
        if imm12 <= 0xfff {
            let instr = 0xF940_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return;
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    let _ = add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load_from_reg(asm, dst, Reg::X17);
}

fn emit_load8u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x3940_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load8u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_load8s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x39C0_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load8s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_load16u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x7940_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load16u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_load16s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x79C0_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load16s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_load32u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB940_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load32u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_load32s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB980_03E0u32 | (imm12 << 10) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_load32s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

fn emit_store_to_sp(asm: &mut Assembler, src: Reg, offset: i32) {
    asm.log_stack_write(offset, 8, "str");
    if offset >= 0 && (offset % 8) == 0 {
        let imm12 = (offset / 8) as u32;
        if imm12 <= 0xfff {
            let instr = 0xF900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return;
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    let _ = add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store_to_reg(asm, src, Reg::X17);
}

fn emit_store8_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    asm.log_stack_write(offset, 1, "strb");
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x3900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_store8_to_reg(asm, src, Reg::X17);
    Ok(())
}

fn emit_store16_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit store on aarch64"));
    }
    asm.log_stack_write(offset, 2, "strh");
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x7900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_store16_to_reg(asm, src, Reg::X17);
    Ok(())
}

fn emit_store32_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit store on aarch64"));
    }
    asm.log_stack_write(offset, 4, "strw");
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64)?;
    emit_store32_to_reg(asm, src, Reg::X17);
    Ok(())
}

fn emit_load_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xF940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load8u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x3940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load8s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x39C0_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load16u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x7940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load16s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x79C0_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load32u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xB940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load32s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xB980_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0xF900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store8_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0x3900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store16_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0x7900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store32_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0xB900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_float_from_sp(asm: &mut Assembler, dst: FReg, offset: i32, ty: &LirType) {
    let scale = if matches!(ty, LirType::F32) { 4 } else { 8 };
    let imm12 = (offset / scale) as u32;
    if imm12 <= 0xfff {
        let base = if matches!(ty, LirType::F32) {
            0xBD40_03E0u32
        } else {
            0xFD40_03E0u32
        };
        let instr = base | (imm12 << 10) | dst.id();
        asm.extend(&instr.to_le_bytes());
    } else {
        emit_mov_reg(asm, Reg::X17, Reg::X31);
        let _ = add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_load_float_from_reg(asm, dst, Reg::X17, ty);
    }
}

fn emit_store_float_to_sp(asm: &mut Assembler, src: FReg, offset: i32, ty: &LirType) {
    let scale = if matches!(ty, LirType::F32) { 4 } else { 8 };
    asm.log_stack_write(offset, scale, "strf");
    let imm12 = (offset / scale) as u32;
    if imm12 <= 0xfff {
        let base = if matches!(ty, LirType::F32) {
            0xBD00_03E0u32
        } else {
            0xFD00_03E0u32
        };
        let instr = base | (imm12 << 10) | src.id();
        asm.extend(&instr.to_le_bytes());
    } else {
        emit_mov_reg(asm, Reg::X17, Reg::X31);
        let _ = add_immediate_offset(asm, Reg::X17, offset as i64);
        emit_store_float_to_reg(asm, src, Reg::X17, ty);
    }
}

fn emit_load_float_from_reg(asm: &mut Assembler, dst: FReg, base: Reg, ty: &LirType) {
    let base_opcode = if matches!(ty, LirType::F32) {
        0xBD40_0000u32
    } else {
        0xFD40_0000u32
    };
    let instr = base_opcode | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_float_to_reg(asm: &mut Assembler, src: FReg, base: Reg, ty: &LirType) {
    let base_opcode = if matches!(ty, LirType::F32) {
        0xBD00_0000u32
    } else {
        0xFD00_0000u32
    };
    let instr = base_opcode | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (31 << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (31 << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_pair_base(asm: &mut Assembler, base: Reg, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (base.id() << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_pair_base(asm: &mut Assembler, base: Reg, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (base.id() << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_reg(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_001F | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_imm12(asm: &mut Assembler, lhs: Reg, imm12: u32) {
    let instr = 0xF100_001F | ((imm12 & 0xfff) << 10) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_bl_reg(asm: &mut Assembler, reg: Reg) {
    let instr = 0xD63F_0000u32 | (reg.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cset(asm: &mut Assembler, dst: Reg, cond: u32) {
    let inv = cond ^ 1;
    let instr = 0x9A9F_07E0u32 | ((inv & 0xF) << 12) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_csel(asm: &mut Assembler, dst: Reg, if_true: Reg, if_false: Reg, cond: u32) {
    let instr = 0x9A80_0000u32
        | (if_false.id() << 16)
        | ((cond & 0xF) << 12)
        | (if_true.id() << 5)
        | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fadd(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_2800u32
    } else {
        0x1E60_2800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fsub(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_3800u32
    } else {
        0x1E60_3800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmul(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_0800u32
    } else {
        0x1E60_0800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fdiv(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_1800u32
    } else {
        0x1E60_1800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcmp(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E21_2000u32
    } else {
        0x1E60_2000u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvt_sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E62_4000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvt_ds(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E22_C000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_scvtf(asm: &mut Assembler, dst: FReg, src: Reg, ty: &LirType, signed: bool) {
    let base = match (ty, signed) {
        (LirType::F32, true) => 0x1E22_0000u32,
        (LirType::F32, false) => 0x1E23_0000u32,
        (LirType::F64, true) => 0x9E62_0000u32,
        (LirType::F64, false) => 0x9E63_0000u32,
        _ => 0x9E62_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvtzs(asm: &mut Assembler, dst: Reg, src: FReg, ty: &LirType, signed: bool) {
    let base = match (ty, signed) {
        (LirType::F32, true) => 0x1E38_0000u32,
        (LirType::F32, false) => 0x1E39_0000u32,
        (LirType::F64, true) => 0x9E78_0000u32,
        (LirType::F64, false) => 0x9E79_0000u32,
        _ => 0x9E78_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_nop(asm: &mut Assembler) {
    asm.extend(&0xD503_201Fu32.to_le_bytes());
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
                    if let (
                        LirValue::Constant(LirConstant::String(lhs_text)),
                        LirValue::Constant(LirConstant::String(rhs_text)),
                    ) = (lhs, rhs)
                    {
                        let mut combined = String::with_capacity(lhs_text.len() + rhs_text.len());
                        combined.push_str(lhs_text);
                        combined.push_str(rhs_text);
                        let offset = intern_cstring(rodata, rodata_pool, &combined);
                        emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                        store_vreg(asm, layout, inst.id, Reg::X16)?;
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
            LirInstructionKind::And(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::And,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Or(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Or,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Xor(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Xor,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Shl(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Left,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Shr(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Right,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Eq(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Eq,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Ne(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ne,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Lt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Lt,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Le(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Le,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Gt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Gt,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Ge(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ge,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Div(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                false,
                reg_types,
                local_types,
            )?,
            LirInstructionKind::Rem(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, true, reg_types, local_types)?
            }
            LirInstructionKind::Not(value) => {
                emit_not(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            LirInstructionKind::Alloca { .. } => {
                let offset = alloca_offset(layout, inst.id)?;
                emit_mov_reg(asm, Reg::X16, Reg::X31);
                add_immediate_offset(asm, Reg::X16, offset as i64)?;
                store_vreg(asm, layout, inst.id, Reg::X16)?;
                asm.record_vreg_sp_offset(inst.id, offset);
            }
            LirInstructionKind::Load { address, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for load"))?;
                emit_load(asm, layout, inst.id, address, ty)?;
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
                let ty = inst.type_hint.as_ref().cloned().unwrap_or(LirType::Void);
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
            LirInstructionKind::IntrinsicCall {
                kind,
                format: format_str,
                args,
            } => {
                let ty = inst.type_hint.as_ref().cloned().unwrap_or(LirType::Void);
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
            LirInstructionKind::UIToFP(value, ty) => {
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
            LirInstructionKind::Trunc(value, ty) => {
                emit_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::ZExt(value, ty) => {
                emit_zext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            LirInstructionKind::FPToSI(value, ty) => {
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
            LirInstructionKind::FPToUI(value, ty) => {
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
            LirInstructionKind::InsertValue {
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
                    "unsupported LIR instruction for aarch64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm, layout);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm16(asm, Reg::X0, 0);
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
                emit_load_from_sp(asm, Reg::X17, sret_offset);
                copy_sp_to_reg(asm, src_offset, Reg::X17, size_of(return_ty) as i32)?;
                if asm.needs_frame {
                    emit_epilogue(asm, layout);
                }
                emit_ret(asm);
                return Ok(());
            }
            if matches!(return_ty, LirType::I128) {
                load_i128_value(asm, layout, value, Reg::X0, Reg::X1, reg_types, local_types)?;
                exit_reg = Some(Reg::X0);
            } else if is_float_type(return_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    return_ty,
                    reg_types,
                    local_types,
                )?;
            } else {
                load_value(asm, layout, value, Reg::X0, reg_types, local_types)?;
                exit_reg = Some(Reg::X0);
            }
            if asm.needs_frame {
                emit_epilogue(asm, layout);
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
            asm.emit_b(Label::Block(asm.current_function, *target));
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
            asm.emit_b(Label::Block(asm.current_function, *normal_dest));
        }
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => {
            emit_switch(asm, layout, value, *default, cases, reg_types, local_types)?;
        }
        LirTerminator::Unreachable => {
            emit_trap(asm);
        }
        _ => {
            return Err(Error::from("unsupported terminator for aarch64"));
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
    if matches!(lhs_ty, LirType::I128) {
        return emit_i128_cmp(asm, layout, dst_id, lhs, rhs, kind, reg_types, local_types);
    }
    if is_float_type(&lhs_ty) {
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
        emit_fcmp(asm, FReg::V0, FReg::V1, &lhs_ty);
        let cond = match kind {
            CmpKind::Eq => 0,
            CmpKind::Ne => 1,
            CmpKind::Lt => 11,
            CmpKind::Le => 13,
            CmpKind::Gt => 12,
            CmpKind::Ge => 10,
        };
        emit_cset(asm, Reg::X16, cond);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    match rhs {
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > 4095 {
                return Err(Error::from("cmp immediate out of range"));
            }
            emit_cmp_imm12(asm, Reg::X16, imm as u32);
        }
        _ => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            emit_cmp_reg(asm, Reg::X16, Reg::X17);
        }
    }

    let cond = match kind {
        CmpKind::Eq => 0,
        CmpKind::Ne => 1,
        CmpKind::Lt => 11,
        CmpKind::Le => 13,
        CmpKind::Gt => 12,
        CmpKind::Ge => 10,
    };
    emit_cset(asm, Reg::X16, cond);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

fn emit_i128_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, LirType>,
    local_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_i128_value(asm, layout, lhs, Reg::X16, Reg::X17, reg_types, local_types)?;
    load_i128_value(asm, layout, rhs, Reg::X9, Reg::X10, reg_types, local_types)?;

    emit_cmp_reg(asm, Reg::X17, Reg::X10);
    emit_cset(asm, Reg::X11, 11); // signed lt
    emit_cset(asm, Reg::X12, 12); // signed gt
    emit_cset(asm, Reg::X13, 0); // eq

    emit_cmp_reg(asm, Reg::X16, Reg::X9);
    emit_cset(asm, Reg::X14, 3); // unsigned lt (LO)
    emit_cset(asm, Reg::X15, 8); // unsigned gt (HI)
    emit_cset(asm, Reg::X9, 0); // eq

    match kind {
        CmpKind::Eq => {
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X9);
            store_vreg(asm, layout, dst_id, Reg::X13)?;
        }
        CmpKind::Ne => {
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X9);
            emit_mov_imm16(asm, Reg::X10, 1);
            emit_eor_reg(asm, Reg::X13, Reg::X13, Reg::X10);
            store_vreg(asm, layout, dst_id, Reg::X13)?;
        }
        CmpKind::Lt => {
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X14);
            emit_or_reg(asm, Reg::X11, Reg::X11, Reg::X13);
            store_vreg(asm, layout, dst_id, Reg::X11)?;
        }
        CmpKind::Gt => {
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X15);
            emit_or_reg(asm, Reg::X12, Reg::X12, Reg::X13);
            store_vreg(asm, layout, dst_id, Reg::X12)?;
        }
        CmpKind::Le => {
            emit_or_reg(asm, Reg::X14, Reg::X14, Reg::X9);
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X14);
            emit_or_reg(asm, Reg::X11, Reg::X11, Reg::X13);
            store_vreg(asm, layout, dst_id, Reg::X11)?;
        }
        CmpKind::Ge => {
            emit_or_reg(asm, Reg::X15, Reg::X15, Reg::X9);
            emit_and_reg(asm, Reg::X13, Reg::X13, Reg::X15);
            emit_or_reg(asm, Reg::X12, Reg::X12, Reg::X13);
            store_vreg(asm, layout, dst_id, Reg::X12)?;
        }
    }
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
    let result_ty =
        reg_types
            .get(&dst_id)
            .cloned()
            .unwrap_or(value_type(if_true, reg_types, local_types)?);
    if is_float_type(&result_ty) {
        return Err(Error::from(
            "Select does not support float values on aarch64",
        ));
    }

    load_value(asm, layout, condition, Reg::X16, reg_types, local_types)?;
    emit_cmp_imm12(asm, Reg::X16, 0);
    load_value(asm, layout, if_true, Reg::X17, reg_types, local_types)?;
    load_value(asm, layout, if_false, Reg::X9, reg_types, local_types)?;
    emit_csel(asm, Reg::X16, Reg::X17, Reg::X9, 1);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
                asm.emit_b(if_true);
            } else {
                asm.emit_b(if_false);
            }
        }
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            emit_cmp_imm12(asm, Reg::X16, 0);
            asm.emit_b_cond(1, if_true);
            asm.emit_b(if_false);
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
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    for (case_val, target) in cases {
        if *case_val <= 4095 {
            emit_cmp_imm12(asm, Reg::X16, *case_val as u32);
        } else {
            emit_mov_imm64(asm, Reg::X17, *case_val);
            emit_cmp_reg(asm, Reg::X16, Reg::X17);
        }
        asm.emit_b_cond(0, Label::Block(asm.current_function, *target));
    }
    asm.emit_b(Label::Block(asm.current_function, default));
    Ok(())
}

#[derive(Clone, Copy, Debug)]
struct Fixup {
    pos: usize,
    target: Label,
    kind: FixupKind,
}

#[derive(Clone, Copy, Debug)]
enum FixupKind {
    B,
    BCond(u32),
    Bl,
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<Label, usize>,
    fixups: Vec<Fixup>,
    needs_frame: bool,
    current_function: u32,
    relocs: Vec<Relocation>,
    target_format: TargetFormat,
    current_layout: Option<LayoutContext>,
    vreg_sp_offsets: HashMap<u32, i32>,
}

struct LayoutContext {
    func: String,
    frame_size: i32,
    save_offset: i32,
}

fn emit_prologue(asm: &mut Assembler, layout: &FrameLayout) -> Result<()> {
    let frame = layout.frame_size;
    if frame > 0 {
        emit_adjust_sp(asm, frame, false)?;
    }
    let save_offset = layout.frame_size - 16;
    if save_offset > 504 {
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, save_offset as i64)?;
        emit_store_pair_base(asm, Reg::X16, Reg::X29, Reg::X30, 0);
    } else {
        emit_store_pair(asm, Reg::X29, Reg::X30, save_offset);
    }
    emit_mov_reg(asm, Reg::X29, Reg::X31);
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler, layout: &FrameLayout) {
    emit_mov_reg(asm, Reg::X31, Reg::X29);
    let save_offset = layout.frame_size - 16;
    if save_offset > 504 {
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        let _ = add_immediate_offset(asm, Reg::X16, save_offset as i64);
        emit_load_pair_base(asm, Reg::X16, Reg::X29, Reg::X30, 0);
    } else {
        emit_load_pair(asm, Reg::X29, Reg::X30, save_offset);
    }
    if layout.frame_size > 0 {
        let _ = emit_adjust_sp(asm, layout.frame_size, true);
    }
}

fn emit_panic_stub(asm: &mut Assembler, id: u32) {
    asm.needs_frame = false;
    asm.bind(Label::Function(id));
    asm.emit_bl_external("abort");
    emit_ret(asm);
}

impl Assembler {
    fn new(target_format: TargetFormat) -> Self {
        Self {
            buf: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            needs_frame: false,
            current_function: 0,
            relocs: Vec::new(),
            target_format,
            current_layout: None,
            vreg_sp_offsets: HashMap::new(),
        }
    }

    fn bind(&mut self, label: Label) {
        if let Label::Function(id) = label {
            self.current_function = id;
        }
        self.labels.insert(label, self.buf.len());
    }

    fn set_layout_context(&mut self, func: &str, frame_size: i32) {
        let save_offset = if frame_size > 0 { frame_size - 16 } else { -1 };
        self.current_layout = Some(LayoutContext {
            func: func.to_string(),
            frame_size,
            save_offset,
        });
        self.vreg_sp_offsets.clear();
    }

    fn clear_layout_context(&mut self) {
        self.current_layout = None;
        self.vreg_sp_offsets.clear();
    }

    fn record_vreg_sp_offset(&mut self, id: u32, offset: i32) {
        self.vreg_sp_offsets.insert(id, offset);
    }

    fn vreg_sp_offset(&self, id: u32) -> Option<i32> {
        self.vreg_sp_offsets.get(&id).copied()
    }

    fn log_stack_write(&self, offset: i32, size: i32, kind: &str) {
        if !stack_debug_enabled() || size <= 0 {
            return;
        }
        let Some(ctx) = self.current_layout.as_ref() else {
            return;
        };
        if ctx.save_offset < 0 {
            return;
        }
        let start = offset;
        let end = offset + size;
        let save_start = ctx.save_offset;
        let save_end = ctx.save_offset + 16;
        if start < save_end && end > save_start {
            eprintln!(
                "[fp-native][stack] {} write {} bytes at sp+{} overlaps save area [{}, {}) ({})",
                ctx.func, size, offset, save_start, save_end, kind
            );
        }
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

    fn emit_b(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x1400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::B,
        });
    }

    fn emit_b_cond(&mut self, cond: u32, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x5400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::BCond(cond),
        });
    }

    fn emit_bl(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::Bl,
        });
    }

    fn emit_bl_external(&mut self, symbol: &str) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.relocs.push(Relocation {
            offset: pos as u64,
            kind: RelocKind::CallRel32,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend: 0,
        });
    }

    fn emit_u32(&mut self, word: u32) {
        self.buf.extend_from_slice(&word.to_le_bytes());
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<(Vec<u8>, Vec<Relocation>)> {
        let fixups = self.fixups.clone();
        for fixup in fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown branch target"))?;
            let origin = fixup.pos;
            let delta = (*target as i64) - (origin as i64);
            let imm = delta / 4;
            match fixup.kind {
                FixupKind::B => {
                    let imm26 =
                        i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("branch out of range"));
                    }
                    let encoded = 0x1400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::BCond(cond) => {
                    let imm19 =
                        i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm19 < -(1 << 18) || imm19 > (1 << 18) - 1 {
                        return Err(Error::from("conditional branch out of range"));
                    }
                    let encoded = 0x5400_0000u32 | (((imm19 as u32) & 0x7FFFF) << 5) | (cond & 0xF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::Bl => {
                    let imm26 =
                        i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("call target out of range"));
                    }
                    let encoded = 0x9400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
            }
        }
        Ok((self.buf, self.relocs))
    }

    fn patch_u32(&mut self, pos: usize, word: u32) {
        self.buf[pos..pos + 4].copy_from_slice(&word.to_le_bytes());
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Label {
    Function(u32),
    Block(u32, BasicBlockId),
}

enum CallTarget {
    Internal(u32),
    External(String),
    Indirect,
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
impl Reg {
    fn is_sp(self) -> bool {
        matches!(self, Reg::X31)
    }
}
