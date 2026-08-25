use super::*;

#[derive(Clone, Debug)]
pub(super) struct JumpTable {
    pub(super) jmp_offset: u64,
    pub(super) capture_offset: u64,
    pub(super) index_reg: u8,
    pub(super) default_offset: Option<u64>,
    pub(super) target_offsets: Vec<u64>,
}

pub(super) fn x86_opcode_after_prefixes(bytes: &[u8], inst: &DecodedInstruction) -> Result<u8> {
    let start = inst.offset as usize;
    let end = (inst.offset + inst.len as u64) as usize;
    let raw = bytes
        .get(start..end)
        .ok_or_else(|| Error::from("x86 opcode slice out of range"))?;
    let mut idx = 0usize;
    while idx < raw.len() {
        let byte = raw[idx];
        let is_prefix = matches!(
            byte,
            0x66 | 0x67 | 0xF2 | 0xF3 | 0x2E | 0x36 | 0x3E | 0x26 | 0x64 | 0x65
        ) || (0x40..=0x4F).contains(&byte);
        if !is_prefix {
            break;
        }
        idx += 1;
    }
    raw.get(idx)
        .copied()
        .ok_or_else(|| Error::from("missing x86 opcode"))
}

pub(super) fn discover_jump_tables(
    decoded: &[DecodedInstruction],
    code_bytes: &[u8],
    code_base_address: u64,
    data_regions: Option<&[DataRegion]>,
    bytes_len: u64,
    reachable_bounds: Option<(u64, u64)>,
) -> Vec<JumpTable> {
    let Some(data_regions) = data_regions else {
        return Vec::new();
    };

    let inst_map = decoded
        .iter()
        .map(|inst| (inst.offset, inst))
        .collect::<std::collections::HashMap<_, _>>();

    let find_region = |address: u64| -> Option<(&[u8], usize)> {
        if let Some((region, offset)) = data_regions.iter().find_map(|region| {
            if address >= region.start && address < region.end {
                Some((region, (address - region.start) as usize))
            } else {
                None
            }
        }) {
            return Some((region.bytes.as_slice(), offset));
        }

        if address >= code_base_address && address < code_base_address.saturating_add(bytes_len) {
            let offset = address.saturating_sub(code_base_address) as usize;
            return Some((code_bytes, offset));
        }

        None
    };

    let trace = std::env::var_os("FP_JUMPTABLE_TRACE").is_some();
    let mut trace_count = 0usize;
    let mut trace_detail = 0usize;

    fn kind_tag(kind: &Decoded) -> &'static str {
        match kind {
            Decoded::Lea { .. } => "lea",
            Decoded::MovSxd { .. } => "movsxd",
            Decoded::MovRmToReg { .. } => "mov",
            Decoded::AddRegRm { .. } | Decoded::AddRmReg { .. } => "add",
            Decoded::JmpRm { .. } => "jmp_rm",
            Decoded::JmpRel { .. } => "jmp",
            Decoded::JccRel { .. } => "jcc",
            Decoded::Cmp { .. } => "cmp",
            _ => "other",
        }
    }

    let mut tables = Vec::new();
    let mut i = 0usize;

    fn resolve_reg_const_address(
        decoded: &[DecodedInstruction],
        code_base_address: u64,
        search_start: usize,
        search_end_exclusive: usize,
        reg: u8,
    ) -> Option<u64> {
        // Very small constant-prop for jump-table base discovery.
        // We only handle "address-like" patterns that commonly occur for
        // switch jump tables in PIC/PIE:
        // - lea reg, [rip + disp]
        // - mov reg, imm64
        // - mov reg, other_reg
        // - add/sub reg, imm
        let mut cur_reg = reg;
        let mut cur_limit = search_end_exclusive;
        let mut addend: i64 = 0;

        for _ in 0..8 {
            let mut found = None;
            for pos in (search_start..cur_limit).rev() {
                let inst = &decoded[pos];
                match &inst.kind {
                    Decoded::Lea {
                        dst,
                        src,
                        width_bits,
                    } if *dst == cur_reg && *width_bits == 64 => {
                        if src.base == Some(16) && src.index.is_none() {
                            let pc = (code_base_address as i64)
                                .saturating_add(inst.offset as i64)
                                .saturating_add(inst.len as i64);
                            let base = pc.saturating_add(src.displacement);
                            return Some((base.saturating_add(addend)) as u64);
                        }
                    }
                    Decoded::MovImm64 { dst, imm, .. } if *dst == cur_reg => {
                        return Some(((*imm as i64).saturating_add(addend)) as u64);
                    }
                    Decoded::MovRmToReg {
                        dst,
                        src: RmOperand::Reg(src_reg),
                        width_bits,
                    } if *dst == cur_reg && *width_bits == 64 => {
                        found = Some((*src_reg, pos));
                        break;
                    }
                    Decoded::AddImm {
                        dst,
                        imm,
                        width_bits,
                    } if *dst == cur_reg && *width_bits == 64 => {
                        addend = addend.saturating_add(*imm as i64);
                        found = Some((cur_reg, pos));
                        break;
                    }
                    Decoded::SubImm {
                        dst,
                        imm,
                        width_bits,
                    } if *dst == cur_reg && *width_bits == 64 => {
                        addend = addend.saturating_sub(*imm as i64);
                        found = Some((cur_reg, pos));
                        break;
                    }
                    _ => {}
                }
            }

            match found {
                Some((next_reg, next_limit)) => {
                    cur_reg = next_reg;
                    cur_limit = next_limit;
                }
                None => return None,
            }
        }

        None
    }
    while i < decoded.len() {
        let inst = &decoded[i];
        let Decoded::JmpRm {
            target: RmOperand::Reg(jmp_reg),
        } = inst.kind
        else {
            i += 1;
            continue;
        };

        // Heuristic: restrict jump-table targets to a neighborhood around the
        // indirect jump instruction. When lifting whole `.text` slices (for
        // example, when starting from a SysV `main`), scanning the entire
        // address space can match unrelated tables and accidentally create CFG
        // edges into unrelated code.
        let local_lo = inst.offset.saturating_sub(0x20_000);
        let local_hi = inst.offset.saturating_add(0x20_000);
        let effective_bounds = match reachable_bounds {
            Some((lo, hi)) => {
                let lo = lo.max(local_lo);
                let hi = hi.min(local_hi);
                (lo <= hi).then_some((lo, hi))
            }
            None => Some((local_lo, local_hi)),
        };

        if trace && trace_count < 20 {
            let window_start = i.saturating_sub(8);
            let tags = (window_start..=i)
                .map(|idx| kind_tag(&decoded[idx].kind))
                .collect::<Vec<_>>();
            eprintln!(
                "[fp-native] jmp_rm reg={} at 0x{:x} prev={:?}",
                jmp_reg, inst.offset, tags
            );

            if trace_count == 0 {
                for idx in window_start..=i {
                    let di = &decoded[idx];
                    match &di.kind {
                        Decoded::Lea {
                            dst,
                            src,
                            width_bits,
                        } => {
                            eprintln!(
                                "[fp-native]   0x{:x}: lea r{} <= [base={:?} idx={:?} scale={} disp={}] w{}",
                                di.offset,
                                dst,
                                src.base,
                                src.index,
                                src.scale,
                                src.displacement,
                                width_bits
                            );
                        }
                        Decoded::MovSxd { dst, src } => {
                            eprintln!(
                                "[fp-native]   0x{:x}: movsxd r{} <= {:?}",
                                di.offset, dst, src
                            );
                        }
                        Decoded::MovRmToReg {
                            dst,
                            src,
                            width_bits,
                        } => {
                            eprintln!(
                                "[fp-native]   0x{:x}: mov r{} <= {:?} w{}",
                                di.offset, dst, src, width_bits
                            );
                        }
                        Decoded::AddRegRm { dst, src } => {
                            eprintln!("[fp-native]   0x{:x}: add r{} += {:?}", di.offset, dst, src);
                        }
                        Decoded::AddRmReg {
                            dst,
                            src,
                            width_bits,
                        } => {
                            eprintln!(
                                "[fp-native]   0x{:x}: add {:?} += r{} w{}",
                                di.offset, dst, src, width_bits
                            );
                        }
                        Decoded::JmpRm { target } => {
                            eprintln!("[fp-native]   0x{:x}: jmp {:?}", di.offset, target);
                        }
                        _ => {
                            eprintln!("[fp-native]   0x{:x}: {}", di.offset, kind_tag(&di.kind));
                        }
                    }
                }
            }
            trace_count += 1;
        }

        // Match common PIC jump-table pattern:
        //   lea base, [rip + disp]
        //   movsxd jmp_reg, dword ptr [base + index*4]
        //   add jmp_reg, base
        //   jmp jmp_reg
        let search_window_start = i.saturating_sub(1024);

        // (base_reg, index_reg, entry_disp, lea_inst, capture_offset, default_offset)
        let mut matched: Option<(u8, u8, i64, Option<&DecodedInstruction>, u64, Option<u64>)> =
            None;

        // Find the `movsxd` (or `mov` + `movsxd`) that loads the jump-table entry.
        'search: for mov_pos in (search_window_start..i).rev() {
            let off_reg: u8;
            let base_reg: u8;
            let index_reg: u8;
            let table_entry_disp: i64;

            match &decoded[mov_pos].kind {
                Decoded::MovSxd {
                    dst,
                    src:
                        RmOperand::Mem(X86Memory {
                            base: Some(mem_base),
                            index: Some(mem_index),
                            scale: 4,
                            displacement: mem_disp,
                            ..
                        }),
                } => {
                    off_reg = *dst;
                    base_reg = *mem_base;
                    index_reg = *mem_index;
                    table_entry_disp = *mem_disp;
                }
                Decoded::MovSxd {
                    dst,
                    src: RmOperand::Reg(tmp_reg),
                } => {
                    if mov_pos == 0 {
                        continue;
                    }
                    let Decoded::MovRmToReg {
                        dst: prev_dst,
                        src:
                            RmOperand::Mem(X86Memory {
                                base: Some(mem_base),
                                index: Some(mem_index),
                                scale: 4,
                                displacement: mem_disp,
                                ..
                            }),
                        width_bits,
                    } = &decoded[mov_pos - 1].kind
                    else {
                        continue;
                    };
                    if *prev_dst != *tmp_reg || *width_bits != 32 {
                        continue;
                    }
                    off_reg = *dst;
                    base_reg = *mem_base;
                    index_reg = *mem_index;
                    table_entry_disp = *mem_disp;
                }
                _ => continue,
            }

            // Find an `add` between `movsxd` and `jmp` that combines the base and offset.
            let mut add_ok = false;
            for add_pos in mov_pos + 1..i {
                match &decoded[add_pos].kind {
                    Decoded::AddRegRm {
                        dst,
                        src: RmOperand::Reg(src_reg),
                    } if (*dst == off_reg && *src_reg == base_reg && jmp_reg == off_reg)
                        || (*dst == base_reg && *src_reg == off_reg && jmp_reg == base_reg) =>
                    {
                        add_ok = true;
                        break;
                    }
                    Decoded::AddRmReg {
                        dst: RmOperand::Reg(dst),
                        src,
                        width_bits,
                    } if *width_bits == 64
                        && ((*dst == off_reg && *src == base_reg && jmp_reg == off_reg)
                            || (*dst == base_reg && *src == off_reg && jmp_reg == base_reg)) =>
                    {
                        add_ok = true;
                        break;
                    }
                    _ => {}
                }
            }
            if !add_ok {
                continue;
            }

            // Find the `lea` that defines the base register.
            for lea_pos in (search_window_start..mov_pos).rev() {
                let Decoded::Lea {
                    dst,
                    src,
                    width_bits,
                } = &decoded[lea_pos].kind
                else {
                    continue;
                };
                if *dst != base_reg || *width_bits != 64 {
                    continue;
                }
                if src.base != Some(16) || src.index.is_some() {
                    continue;
                }
                let default_offset = if mov_pos > 0 {
                    let prev = &decoded[mov_pos - 1];
                    match prev.kind {
                        Decoded::JccRel { target, .. }
                            if prev.offset + prev.len as u64 == decoded[mov_pos].offset =>
                        {
                            Some(target)
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                matched = Some((
                    base_reg,
                    index_reg,
                    table_entry_disp,
                    Some(&decoded[lea_pos]),
                    decoded[mov_pos].offset,
                    default_offset,
                ));
                break 'search;
            }

            // Some compilers keep the jump-table base address in a callee-saved
            // register (often via a prologue `lea` or a copy from another reg).
            // In that case we still accept the pattern, and resolve the base
            // address via local constant-prop.
            let default_offset = if mov_pos > 0 {
                let prev = &decoded[mov_pos - 1];
                match prev.kind {
                    Decoded::JccRel { target, .. }
                        if prev.offset + prev.len as u64 == decoded[mov_pos].offset =>
                    {
                        Some(target)
                    }
                    _ => None,
                }
            } else {
                None
            };
            matched = Some((
                base_reg,
                index_reg,
                table_entry_disp,
                None,
                decoded[mov_pos].offset,
                default_offset,
            ));
            break;
        }

        let Some((base_reg, index_reg, table_entry_disp, lea_inst, capture_offset, default_offset)) =
            matched
        else {
            if trace && trace_detail < 5 {
                eprintln!("[fp-native] jt no-match at 0x{:x}", inst.offset);
                trace_detail += 1;
            }
            i += 1;
            continue;
        };

        // Compute the table's absolute VA within the input image.
        fn parse_jump_table_targets(
            inst_map: &std::collections::HashMap<u64, &DecodedInstruction>,
            code_base_address: u64,
            bytes_len: u64,
            table_bytes: &[u8],
            region_offset: usize,
            table_label_va: i64,
            reachable_bounds: Option<(u64, u64)>,
        ) -> Vec<u64> {
            let mut targets = Vec::new();
            let mut entry_index = 0usize;
            while targets.len() < 4096 {
                let entry_offset = region_offset.saturating_add(entry_index * 4);
                if entry_offset.saturating_add(4) > table_bytes.len() {
                    break;
                }
                let raw = &table_bytes[entry_offset..entry_offset + 4];
                let rel = i32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]]) as i64;
                let target_va = table_label_va.saturating_add(rel) as u64;
                if target_va < code_base_address
                    || target_va >= code_base_address.saturating_add(bytes_len)
                {
                    break;
                }
                let target_offset = target_va.saturating_sub(code_base_address);
                if let Some((lo, hi)) = reachable_bounds {
                    if target_offset < lo || target_offset > hi {
                        break;
                    }
                }
                if !inst_map.contains_key(&target_offset) {
                    break;
                }
                targets.push(target_offset);
                entry_index += 1;
            }
            targets
        }

        fn scan_for_jump_table_base(
            inst_map: &std::collections::HashMap<u64, &DecodedInstruction>,
            data_regions: &[DataRegion],
            code_bytes: &[u8],
            code_base_address: u64,
            bytes_len: u64,
            table_entry_disp: i64,
            reachable_bounds: Option<(u64, u64)>,
        ) -> Option<(i64, Vec<u64>)> {
            let scan_region = |start_va: u64, bytes: &[u8]| -> Option<(i64, Vec<u64>)> {
                let disp = table_entry_disp;
                if disp.abs() > (bytes.len() as i64) {
                    return None;
                }

                // Scan 4-byte aligned candidates.
                let max_off = bytes.len().saturating_sub(16);
                for base_off in (0..max_off).step_by(4) {
                    let entry_off_i = (base_off as i64).saturating_add(disp);
                    if entry_off_i < 0 {
                        continue;
                    }
                    let entry_off = entry_off_i as usize;
                    if entry_off.saturating_add(12) > bytes.len() {
                        continue;
                    }

                    let label_va = (start_va as i64).saturating_add(base_off as i64);

                    // Quick plausibility: first 3 entries must land on decoded instruction boundaries.
                    let mut ok = true;
                    for idx in 0..3 {
                        let raw = &bytes[entry_off + idx * 4..entry_off + idx * 4 + 4];
                        let rel = i32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]]) as i64;
                        let target_va = label_va.saturating_add(rel) as u64;
                        if target_va < code_base_address
                            || target_va >= code_base_address.saturating_add(bytes_len)
                        {
                            ok = false;
                            break;
                        }
                        let target_offset = target_va.saturating_sub(code_base_address);
                        if let Some((lo, hi)) = reachable_bounds {
                            if target_offset < lo || target_offset > hi {
                                ok = false;
                                break;
                            }
                        }
                        if !inst_map.contains_key(&target_offset) {
                            ok = false;
                            break;
                        }
                    }
                    if !ok {
                        continue;
                    }

                    let targets = parse_jump_table_targets(
                        inst_map,
                        code_base_address,
                        bytes_len,
                        bytes,
                        entry_off,
                        label_va,
                        reachable_bounds,
                    );
                    if targets.len() >= 2 {
                        return Some((label_va, targets));
                    }
                }

                None
            };

            for region in data_regions {
                if let Some(found) = scan_region(region.start, &region.bytes) {
                    return Some(found);
                }
            }

            // Some binaries place small jump tables into the code section.
            scan_region(code_base_address, code_bytes)
        }

        let (table_label_va, targets) = if let Some(lea_inst) = lea_inst {
            let Decoded::Lea { src, .. } = &lea_inst.kind else {
                i += 1;
                continue;
            };
            let pc = (code_base_address as i64)
                .saturating_add(lea_inst.offset as i64)
                .saturating_add(lea_inst.len as i64);
            let label_va = pc.saturating_add(src.displacement);

            let table_entry_va = label_va.saturating_add(table_entry_disp) as u64;
            let Some((table_bytes, region_offset)) = find_region(table_entry_va) else {
                i += 1;
                continue;
            };

            let targets = parse_jump_table_targets(
                &inst_map,
                code_base_address,
                bytes_len,
                table_bytes,
                region_offset,
                label_va,
                effective_bounds,
            );
            (label_va, targets)
        } else {
            let resolved = resolve_reg_const_address(
                decoded,
                code_base_address,
                search_window_start,
                i,
                base_reg,
            );

            match resolved {
                Some(resolved) => {
                    let label_va = resolved as i64;
                    let table_entry_va = label_va.saturating_add(table_entry_disp) as u64;
                    let Some((table_bytes, region_offset)) = find_region(table_entry_va) else {
                        i += 1;
                        continue;
                    };
                    let targets = parse_jump_table_targets(
                        &inst_map,
                        code_base_address,
                        bytes_len,
                        table_bytes,
                        region_offset,
                        label_va,
                        effective_bounds,
                    );
                    (label_va, targets)
                }
                None => {
                    if let Some(found) = scan_for_jump_table_base(
                        &inst_map,
                        data_regions,
                        code_bytes,
                        code_base_address,
                        bytes_len,
                        table_entry_disp,
                        effective_bounds,
                    ) {
                        found
                    } else {
                        i += 1;
                        continue;
                    }
                }
            }
        };

        if trace && trace_detail < 5 {
            eprintln!(
                "[fp-native] jt probe jmp=0x{:x} label_va=0x{:x} entry_va=0x{:x} region_off=0x{:x}",
                inst.offset,
                table_label_va as u64,
                (table_label_va.saturating_add(table_entry_disp) as u64),
                0
            );
            trace_detail += 1;
        }

        if targets.len() >= 2 {
            if trace {
                eprintln!(
                    "[fp-native] matched jump table at 0x{:x} entries={}",
                    inst.offset,
                    targets.len()
                );
            }
            tables.push(JumpTable {
                jmp_offset: inst.offset,
                capture_offset,
                index_reg,
                default_offset,
                target_offsets: targets,
            });
        }

        i += 1;
    }

    tables
}
