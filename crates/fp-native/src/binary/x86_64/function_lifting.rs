use super::*;

pub fn lift_function_bytes(
    bytes: &[u8],
    relocs: &[TextRelocation],
    syscall_convention: Option<AsmSyscallConvention>,
) -> Result<LiftedFunction> {
    lift_function_bytes_with_symbols(
        bytes,
        relocs,
        syscall_convention,
        0,
        None,
        None,
        None,
        None,
        None,
        0,
        true,
        false,
    )
}

pub fn lift_function_bytes_with_symbols(
    bytes: &[u8],
    relocs: &[TextRelocation],
    syscall_convention: Option<AsmSyscallConvention>,
    code_base_address: u64,
    rip_symbols: Option<&HashMap<u64, RipSymbol>>,
    plt_targets: Option<&HashMap<u64, String>>,
    rodata_cstrings: Option<&HashMap<String, String>>,
    rodata_cstrings_by_addr: Option<&HashMap<u64, String>>,
    data_regions: Option<&[DataRegion]>,
    entry_offset: u64,
    initialize_reg_file_from_locals: bool,
    use_lifted_regfile_calls: bool,
) -> Result<LiftedFunction> {
    let decoded = decode_stream(bytes)?;

    let jump_tables = discover_jump_tables(
        &decoded,
        bytes,
        code_base_address,
        data_regions,
        bytes.len() as u64,
        None,
    );
    let sorted_starts =
        determine_block_starts(&decoded, bytes.len() as u64, entry_offset, &jump_tables)?;
    let offset_to_sorted_index = sorted_starts
        .iter()
        .enumerate()
        .map(|(idx, offset)| (*offset, idx))
        .collect::<std::collections::HashMap<_, _>>();

    // Codegen expects the first basic block to be the function entry.
    // Keep block boundaries in ascending address order, but assign IDs and
    // emit order with the entry block first.
    let mut block_starts = Vec::with_capacity(sorted_starts.len());
    block_starts.push(entry_offset);
    for &offset in &sorted_starts {
        if offset != entry_offset {
            block_starts.push(offset);
        }
    }

    let offset_to_block = block_starts
        .iter()
        .enumerate()
        .map(|(idx, offset)| (*offset, idx as u32))
        .collect::<std::collections::HashMap<_, _>>();

    let jump_table_by_jmp_offset = jump_tables
        .into_iter()
        .map(|table| (table.jmp_offset, table))
        .collect::<std::collections::HashMap<_, _>>();
    let jump_table_by_capture_offset = jump_table_by_jmp_offset
        .values()
        .map(|table| (table.capture_offset, (table.jmp_offset, table.index_reg)))
        .collect::<std::collections::HashMap<_, _>>();
    let switch_default_block = block_starts.len() as u32;
    let mut saw_switch = false;

    let mut ctx = RegisterLiftContext::new(
        code_base_address,
        rip_symbols,
        plt_targets,
        rodata_cstrings,
        rodata_cstrings_by_addr,
        data_regions,
        initialize_reg_file_from_locals,
        use_lifted_regfile_calls,
    );
    let mut next_id = 0u32;
    let mut basic_blocks = Vec::new();

    let stack_slots = ctx.initialize_reg_file_slots();

    for (block_index, &block_offset) in block_starts.iter().enumerate() {
        let block_id = block_index as u32;
        let sorted_index = *offset_to_sorted_index
            .get(&block_offset)
            .ok_or_else(|| Error::from("missing block offset index"))?;
        let next_block_offset = sorted_starts
            .get(sorted_index + 1)
            .copied()
            .unwrap_or(bytes.len() as u64);
        let mut instructions = Vec::new();

        if block_offset == entry_offset && initialize_reg_file_from_locals {
            ctx.emit_reg_file_init_stores(&mut instructions, &mut next_id)?;
        }
        ctx.begin_block(&mut instructions, &mut next_id)?;
        let mut terminated = false;
        let mut last_compare: Option<LastCompare> = None;

        let mut cursor = block_offset;
        while cursor < next_block_offset {
            let inst = decoded
                .iter()
                .find(|inst| inst.offset == cursor)
                .ok_or_else(|| Error::from("missing decoded instruction"))?;

            if is_terminator(&inst.kind) {
                let terminator = lift_terminator(
                    &mut ctx,
                    inst,
                    bytes,
                    &mut instructions,
                    &mut next_id,
                    &offset_to_block,
                    &jump_table_by_jmp_offset,
                    switch_default_block,
                    &mut saw_switch,
                    &mut last_compare,
                )?;

                ctx.end_block(&mut instructions, &mut next_id)?;
                basic_blocks.push(fp_core::asmir::AsmBlock {
                    id: block_id,
                    label: None,
                    instructions: std::mem::take(&mut instructions),
                    terminator,
                    terminator_encoding: None,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
                terminated = true;
                break;
            }

            lift_non_terminator(
                &mut ctx,
                inst,
                bytes,
                relocs,
                &mut instructions,
                &mut next_id,
                &mut last_compare,
                syscall_convention,
                &jump_table_by_capture_offset,
            )?;
            cursor = cursor
                .checked_add(inst.len as u64)
                .ok_or_else(|| Error::from("x86_64 lift overflow"))?;
        }

        if !terminated {
            let terminator = if let Some(&fallthrough_offset) = sorted_starts.get(sorted_index + 1)
            {
                let fallthrough = *offset_to_block
                    .get(&fallthrough_offset)
                    .ok_or_else(|| Error::from("missing fallthrough block"))?;
                fp_core::asmir::AsmTerminator::Br(fallthrough)
            } else {
                fp_core::asmir::AsmTerminator::Return(None)
            };

            ctx.end_block(&mut instructions, &mut next_id)?;
            basic_blocks.push(fp_core::asmir::AsmBlock {
                id: block_id,
                label: None,
                instructions,
                terminator,
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }
    }

    if saw_switch {
        basic_blocks.push(fp_core::asmir::AsmBlock {
            id: switch_default_block,
            label: None,
            instructions: Vec::new(),
            terminator: fp_core::asmir::AsmTerminator::Unreachable,
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
    }

    wire_block_edges(&mut basic_blocks);

    Ok(LiftedFunction {
        basic_blocks,
        locals: ctx.locals,
        stack_slots,
        direct_call_targets: ctx.direct_call_targets,
    })
}
