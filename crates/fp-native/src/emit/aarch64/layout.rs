use super::*;

pub(super) fn build_frame_layout(
    func: &AsmFunction,
    reg_types: &HashMap<u32, AsmType>,
    use_x86_regfile: bool,
    data_layout: &LirDataLayout,
) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut max_vararg_stack = 0usize;
    let mut has_calls = false;
    let mut max_const_agg_args = 0i32;
    let local_types = build_local_types(func);
    let mut alloca_info = Vec::new();
    let mut alloca_debug = Vec::new();
    let mut local_debug = Vec::new();
    let mut agg_debug = Vec::new();
    // A bare aggregate constant (e.g. a `&str`'s `{ptr, len}` slice) passed
    // as a call argument has no backing Local/Register stack slot the way
    // every other >8-byte aggregate value in this backend does — the whole
    // calling convention here passes such aggregates *by address*, so one
    // has to exist somewhere. Reserve a single scratch slot per function,
    // sized to the largest such constant seen, and materialize into it
    // immediately before each call that needs it (see `emit_call`).
    let mut const_agg_scratch_size = 0i32;
    vreg_ids.extend(reg_types.keys().copied());

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let AsmInstructionKind::Call { function, args, .. } = &inst.kind {
                has_calls = true;
                let mut const_agg_args = 0i32;
                let mut count = 0usize;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types)?;
                    if let AsmValue::Constant(
                        AsmConstant::Struct(_, _) | AsmConstant::Array(_, _),
                    ) = arg
                    {
                        let ty = value_type(arg, reg_types, &local_types)?;
                        if is_large_aggregate(&ty, data_layout) {
                            let size = data_layout
                                .size_of(&ty)
                                .map_err(|error| Error::from(error.to_string()))?
                                as i32;
                            const_agg_scratch_size = const_agg_scratch_size.max(align8(size));
                            const_agg_args += 1;
                        }
                    }
                }
                max_const_agg_args = max_const_agg_args.max(const_agg_args);
                max_call_args = max_call_args.max(count);
                if let Some(start) = darwin_variadic_format_start(function, args) {
                    let bytes =
                        vararg_outgoing_size(args, start, reg_types, &local_types, data_layout)?;
                    max_vararg_stack = max_vararg_stack.max(bytes);
                }
            } else if let AsmInstructionKind::IntrinsicCall { kind, args, .. } = &inst.kind {
                has_calls = true;
                let fixed = if matches!(kind, AsmIntrinsicKind::Format) {
                    3
                } else {
                    1
                };
                let mut count = fixed;
                for arg in args {
                    count += call_arg_units(arg, reg_types, &local_types)?;
                }
                max_call_args = max_call_args.max(count);
                let bytes = vararg_outgoing_size(args, 0, reg_types, &local_types, data_layout)?;
                max_vararg_stack = max_vararg_stack.max(bytes);
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
                    .map_err(|e| Error::from(format!("alloca size too large: {bytes}: {e}")))?;
                let align = (*alignment).max(1) as i32;
                alloca_info.push((inst.id, bytes, align));
            }
        }
    }

    // Calls can contain aggregate constants indirectly through formatting or
    // const-propagated expressions, so keep materialization scratch available
    // for those values as well as direct aggregate arguments.
    if has_calls {
        const_agg_scratch_size = const_agg_scratch_size.max(16);
        const_agg_scratch_size *= max_const_agg_args.max(1);
    }

    let reg_spill_size = (max_call_args.saturating_sub(8) * 8) as i32;
    let vararg_stack_size = max_vararg_stack as i32;
    let mut outgoing_size = align16(reg_spill_size.max(vararg_stack_size));
    if use_x86_regfile {
        // Reserve a small scratch area for internal lifted-call preservation.
        outgoing_size = outgoing_size.max(64);
    }
    outgoing_size = align16(outgoing_size);
    let mut vreg_offsets = HashMap::new();
    let mut slot_offsets = HashMap::new();
    let mut x86_regfile_offsets = HashMap::new();
    let mut local_offsets = HashMap::new();
    let mut agg_offsets = HashMap::new();
    let mut alloca_offsets = HashMap::new();
    let mut sret_offset = None;
    let mut offset = outgoing_size;

    for id in &vreg_ids {
        let (size, align) = vreg_slot_spec(*id, reg_types, data_layout);
        offset = align_to(offset, align);
        vreg_offsets.insert(*id, offset);
        offset += size;
    }

    for slot in &func.stack_slots {
        if use_x86_regfile {
            if slot
                .name
                .as_deref()
                .is_some_and(|name| name.starts_with("x86."))
            {
                let offset = (slot.id as i32)
                    .checked_mul(8)
                    .ok_or_else(|| Error::from("x86 regfile slot offset overflow"))?;
                x86_regfile_offsets.insert(slot.id, offset);
                continue;
            }
        }
        let align = slot.alignment.max(1) as i32;
        let size = align8(slot.size as i32).max(8);
        let slot_align = align.max(8);
        offset = align_to(offset, slot_align);
        slot_offsets.insert(slot.id, offset);
        offset += size;
    }

    for local in &func.locals {
        let size = align8(
            data_layout
                .size_of(&local.ty)
                .map_err(|error| Error::from(error.to_string()))? as i32,
        )
        .max(8);
        offset = align_to(offset, 8);
        local_offsets.insert(local.id, offset);
        local_debug.push((local.id, offset, size));
        offset += size;
    }

    if returns_aggregate(&func.signature.return_type, data_layout) {
        sret_offset = Some(offset);
        offset += 8;
    }

    let const_agg_scratch_offset = if const_agg_scratch_size > 0 {
        let scratch_offset = offset;
        offset += const_agg_scratch_size;
        Some(scratch_offset)
    } else {
        None
    };

    for id in &vreg_ids {
        if let Some(ty) = reg_types.get(id) {
            if is_large_aggregate(ty, data_layout) {
                let size = align8(
                    data_layout
                        .size_of(ty)
                        .map_err(|error| Error::from(error.to_string()))?
                        as i32,
                );
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
        let watch_offsets = layout_watch_offsets();
        let watch_function = layout_watch_function();
        let watch_enabled = !watch_offsets.is_empty()
            && match watch_function.as_ref() {
                None => true,
                Some(name) => name.as_str() == func.name.as_str(),
            };
        if watch_enabled {
            for watch in &watch_offsets {
                for (id, offset) in &vreg_offsets {
                    if offset == watch {
                        layout_log(&format!(
                            "{} watch_offset={} vreg={} kind=vreg",
                            func.name, watch, id
                        ));
                    }
                }
                for (id, offset) in &local_offsets {
                    if offset == watch {
                        layout_log(&format!(
                            "{} watch_offset={} local={} kind=local",
                            func.name, watch, id
                        ));
                    }
                }
                for (id, offset) in &slot_offsets {
                    if offset == watch {
                        layout_log(&format!(
                            "{} watch_offset={} slot={} kind=stack_slot",
                            func.name, watch, id
                        ));
                    }
                }
            }
        }
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
        data_layout: data_layout.clone(),
        vreg_offsets,
        slot_offsets,
        x86_regfile_offsets,
        local_offsets,
        agg_offsets,
        alloca_offsets,
        sret_offset,
        const_agg_scratch_offset,
        const_agg_scratch_stride: if max_const_agg_args > 0 {
            const_agg_scratch_size / max_const_agg_args
        } else {
            16
        },
        outgoing_size,
        frame_size,
    })
}

pub(super) fn call_arg_units(
    arg: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<usize> {
    let ty = value_type(arg, reg_types, local_types)?;
    if matches!(ty, AsmType::I128) {
        Ok(2)
    } else {
        Ok(1)
    }
}

pub(super) fn vreg_slot_spec(
    id: u32,
    reg_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> (i32, i32) {
    let Some(ty) = reg_types.get(&id) else {
        return (8, 8);
    };
    if is_large_aggregate(ty, data_layout) {
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

pub(super) fn vararg_outgoing_size(
    args: &[AsmValue],
    start: usize,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    data_layout: &LirDataLayout,
) -> Result<usize> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let mut stack_bytes = 0i32;
    for arg in args.iter().skip(start) {
        let ty = value_type(arg, reg_types, local_types)?;
        stack_bytes += align8(size_of(&ty) as i32);
    }
    Ok(stack_bytes as usize)
}

pub(super) fn darwin_variadic_format_start(
    function: &AsmValue,
    args: &[AsmValue],
) -> Option<usize> {
    let AsmValue::Function(name) = function else {
        return None;
    };
    match name.as_str() {
        "printf" if !args.is_empty() => Some(1),
        "fprintf" if args.len() >= 2 => Some(2),
        "dprintf" if args.len() >= 2 => Some(2),
        _ => None,
    }
}

pub(super) fn build_reg_types(func: &AsmFunction) -> HashMap<u32, AsmType> {
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
            if map.contains_key(&inst.id) {
                continue;
            }
            if let AsmInstructionKind::ExtractValue { aggregate, indices } = &inst.kind {
                if let Ok(aggregate_ty) = value_type(aggregate, &map, &local_types) {
                    if let Ok(field_ty) = extract_value_type(&aggregate_ty, indices) {
                        map.insert(inst.id, field_ty);
                    }
                }
            }
        }
    }
    map
}

pub(super) fn build_local_types(func: &AsmFunction) -> HashMap<u32, AsmType> {
    let mut map = HashMap::new();
    for local in &func.locals {
        map.insert(local.id, local.ty.clone());
    }
    map
}

pub(super) fn align16(value: i32) -> i32 {
    ((value + 15) / 16) * 16
}

pub(super) fn align8(value: i32) -> i32 {
    ((value + 7) / 8) * 8
}

pub(super) fn align_to(value: i32, align: i32) -> i32 {
    if align <= 1 {
        return value;
    }
    ((value + align - 1) / align) * align
}

pub(super) fn abi_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_ABI_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

pub(super) fn abi_log(msg: &str) {
    if abi_debug_enabled() {
        eprintln!("[fp-native][abi] {}", msg);
    }
}

pub(super) fn stack_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_STACK_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

pub(super) fn layout_debug_enabled() -> bool {
    matches!(
        std::env::var("FP_NATIVE_LAYOUT_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    )
}

pub(super) fn layout_log(msg: &str) {
    if layout_debug_enabled() {
        eprintln!("[fp-native][layout] {}", msg);
    }
}

pub(super) fn layout_watch_offsets() -> Vec<i32> {
    let Ok(value) = std::env::var("FP_NATIVE_LAYOUT_WATCH_OFFSETS") else {
        return Vec::new();
    };
    value
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .filter_map(|item| {
            if let Some(hex) = item.strip_prefix("0x") {
                i32::from_str_radix(hex, 16)
                    .map_err(|e| {
                        eprintln!("[fp-native] preserved-instruction parse error: {e}");
                        e
                    })
                    .ok()
            } else {
                item.parse()
                    .map_err(|e| {
                        eprintln!("[fp-native] preserved-instruction parse error: {e}");
                        e
                    })
                    .ok()
            }
        })
        .collect()
}

pub(super) fn layout_watch_function() -> Option<String> {
    std::env::var("FP_NATIVE_LAYOUT_WATCH_FUNCTION").ok()
}

pub(super) fn reg_name(reg: Reg) -> &'static str {
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
        Reg::X19 => "x19",
        Reg::X29 => "x29",
        Reg::X30 => "x30",
        Reg::X31 => "sp",
    }
}

pub(super) fn freg_name(reg: FReg) -> &'static str {
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

pub(super) fn vreg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .vreg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing vreg slot"))
}

pub(super) fn stack_slot_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .slot_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing stack slot"))
}

pub(super) fn x86_regfile_slot_offset(layout: &FrameLayout, id: u32) -> Option<i32> {
    layout.x86_regfile_offsets.get(&id).copied()
}

pub(super) fn stack_slot_base_and_offset(layout: &FrameLayout, id: u32) -> Result<(Reg, i32)> {
    if let Some(offset) = x86_regfile_slot_offset(layout, id) {
        return Ok((Reg::X19, offset));
    }
    Ok((Reg::X31, stack_slot_offset(layout, id)?))
}

pub(super) fn local_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout.local_offsets.get(&id).copied().ok_or_else(|| {
        let mut known = layout.local_offsets.keys().copied().collect::<Vec<_>>();
        known.sort_unstable();
        Error::from(format!(
            "missing local slot: id={} known_local_ids={:?}",
            id, known
        ))
    })
}

pub(super) fn agg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .agg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from(format!("missing aggregate slot for vreg {}", id)))
}

pub(super) fn alloca_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .alloca_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing alloca slot"))
}

pub(super) struct FrameLayout {
    pub(super) data_layout: LirDataLayout,
    pub(super) vreg_offsets: HashMap<u32, i32>,
    pub(super) slot_offsets: HashMap<u32, i32>,
    pub(super) x86_regfile_offsets: HashMap<u32, i32>,
    pub(super) local_offsets: HashMap<u32, i32>,
    pub(super) agg_offsets: HashMap<u32, i32>,
    pub(super) alloca_offsets: HashMap<u32, i32>,
    pub(super) sret_offset: Option<i32>,
    pub(super) const_agg_scratch_offset: Option<i32>,
    pub(super) const_agg_scratch_stride: i32,
    pub(super) outgoing_size: i32,
    pub(super) frame_size: i32,
}
