use super::*;

pub(super) fn determine_block_starts(
    decoded: &[DecodedInstruction],
    bytes_len: u64,
    entry_offset: u64,
    jump_tables: &[JumpTable],
) -> Result<Vec<u64>> {
    let inst_map = decoded
        .iter()
        .map(|inst| (inst.offset, inst))
        .collect::<std::collections::HashMap<_, _>>();

    if !inst_map.contains_key(&entry_offset) {
        return Err(Error::from(
            "x86_64 entrypoint is not on an instruction boundary",
        ));
    }

    let mut starts = std::collections::HashSet::new();
    starts.insert(entry_offset);

    let mut queue = std::collections::VecDeque::new();
    queue.push_back(entry_offset);

    for table in jump_tables {
        for &target in &table.target_offsets {
            starts.insert(target);
            queue.push_back(target);
        }

        if let Some(default_offset) = table.default_offset {
            starts.insert(default_offset);
            queue.push_back(default_offset);
        }
    }
    let mut visited = std::collections::HashSet::new();

    while let Some(entry) = queue.pop_front() {
        if !visited.insert(entry) {
            continue;
        }
        let mut cursor = entry;
        loop {
            let inst = inst_map
                .get(&cursor)
                .ok_or_else(|| Error::from("missing decoded instruction"))?;

            match inst.kind {
                Decoded::JccRel { target, .. } => {
                    if !inst_map.contains_key(&target) {
                        return Err(Error::from("x86_64 jcc target not on instruction boundary"));
                    }
                    let fallthrough = cursor + inst.len as u64;
                    starts.insert(target);
                    starts.insert(fallthrough);
                    queue.push_back(target);
                    queue.push_back(fallthrough);
                    break;
                }
                Decoded::JmpRel { target } => {
                    if !inst_map.contains_key(&target) {
                        return Err(Error::from("x86_64 jmp target not on instruction boundary"));
                    }
                    starts.insert(target);
                    queue.push_back(target);
                    break;
                }
                Decoded::Ret | Decoded::Hlt | Decoded::JmpRm { .. } => {
                    break;
                }
                _ => {
                    let next = cursor + inst.len as u64;
                    if next >= bytes_len {
                        break;
                    }
                    if starts.contains(&next) {
                        queue.push_back(next);
                        break;
                    }
                    cursor = next;
                }
            }
        }
    }

    let mut starts: Vec<u64> = starts.into_iter().collect();
    starts.sort_unstable();
    Ok(starts)
}

pub(super) fn is_terminator(kind: &Decoded) -> bool {
    matches!(
        kind,
        Decoded::Ret
            | Decoded::Hlt
            | Decoded::JmpRel { .. }
            | Decoded::JmpRm { .. }
            | Decoded::JccRel { .. }
    )
}

pub(super) fn synthesize_fallthrough_compare(
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> LastCompare {
    let id = *next_id;
    instructions.push(AsmInstruction {
        id,
        opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Eq),
        kind: AsmInstructionKind::Eq(
            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
            AsmValue::Constant(AsmConstant::Int(1, AsmType::I64)),
        ),
        ty: AsmType::Void,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    });
    *next_id += 1;
    LastCompare {
        id,
        index: instructions.len() - 1,
        is_float: false,
    }
}

pub(super) fn lift_terminator(
    ctx: &mut RegisterLiftContext,
    inst: &DecodedInstruction,
    _bytes: &[u8],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    offset_to_block: &std::collections::HashMap<u64, u32>,
    jump_tables: &std::collections::HashMap<u64, JumpTable>,
    switch_default_block: u32,
    saw_switch: &mut bool,
    last_compare: &mut Option<LastCompare>,
) -> Result<fp_core::asmir::AsmTerminator> {
    match inst.kind {
        Decoded::Ret => Ok(fp_core::asmir::AsmTerminator::Return(
            ctx.read_return_value(),
        )),
        Decoded::Hlt => Ok(fp_core::asmir::AsmTerminator::Unreachable),
        Decoded::JccRel { condition, target } => {
            let if_true = offset_to_block
                .get(&target)
                .copied()
                .ok_or_else(|| Error::from("missing conditional jump target block"))?;
            let fallthrough = inst.offset + inst.len as u64;
            let if_false = offset_to_block
                .get(&fallthrough)
                .copied()
                .ok_or_else(|| Error::from("missing conditional jump fallthrough block"))?;

            let compare = if let Some(compare) = last_compare.as_ref() {
                match patch_compare_kind(instructions, compare, condition) {
                    Ok(()) => *compare,
                    Err(_) => synthesize_fallthrough_compare(instructions, next_id),
                }
            } else {
                synthesize_fallthrough_compare(instructions, next_id)
            };

            // The branch consumes the flags produced by the compare.
            // This keeps the terminator independent from ISA-specific condition codes.
            Ok(fp_core::asmir::AsmTerminator::CondBr {
                condition: AsmValue::Flags(compare.id),
                if_true,
                if_false,
            })
        }
        Decoded::JmpRel { target } => {
            let dest = offset_to_block
                .get(&target)
                .copied()
                .ok_or_else(|| Error::from("missing jump target block"))?;
            Ok(fp_core::asmir::AsmTerminator::Br(dest))
        }
        Decoded::JmpRm { target } => {
            if let Some(table) = jump_tables.get(&inst.offset) {
                let value = ctx
                    .pending_jump_table_index
                    .remove(&inst.offset)
                    .or_else(|| {
                        // JUSTIFY: if the index register can't be read, fall
                        // back to case 0; the log makes the failure visible.
                        ctx.read_gpr(table.index_reg)
                            .map_err(|e| {
                                eprintln!("[fp-native] jump-table index register read error: {e}");
                                e
                            })
                            .ok()
                    })
                    .unwrap_or_else(|| AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)));

                let mut cases = Vec::with_capacity(table.target_offsets.len());
                for (idx, target) in table.target_offsets.iter().enumerate() {
                    if let Some(&dest) = offset_to_block.get(target) {
                        cases.push((idx as u64, dest));
                    }
                }
                if cases.len() >= 2 {
                    *saw_switch = true;
                    let default = table
                        .default_offset
                        .and_then(|offset| offset_to_block.get(&offset).copied())
                        .unwrap_or(switch_default_block);
                    return Ok(fp_core::asmir::AsmTerminator::Switch {
                        value,
                        default,
                        cases,
                    });
                }
            }

            // Modern ELF binaries (notably those built with `-fno-plt`) may use
            // `jmp *<import>@GOTPCREL(%rip)` as a tail call into an imported
            // function. Treat these as an external call followed by a return
            // so that we marshal arguments through the target ABI instead of
            // emitting a raw indirect branch into the host's function pointer.
            if let RmOperand::Mem(memory) = &target {
                let symbol = ctx
                    .resolve_rip_symbol(memory, inst.offset, inst.len)
                    .or_else(|| ctx.resolve_disp32_symbol(memory, inst.offset, inst.len));
                if let Some(symbol) = symbol {
                    if let Some(import) = symbol.import.clone() {
                        let args = x86_64_sysv_call_args(ctx)?;
                        let id = *next_id;
                        instructions.push(AsmInstruction {
                            id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call),
                            kind: AsmInstructionKind::Call {
                                function: AsmValue::Function(import),
                                args,
                                calling_convention: CallingConvention::X86_64SysV,
                                tail_call: false,
                            },
                            ty: AsmType::I64,
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        ctx.write_gpr(0, AsmValue::Register(id));
                        return Ok(fp_core::asmir::AsmTerminator::Return(Some(
                            AsmValue::Register(id),
                        )));
                    }
                }
            }

            // Indirect branches show up in jump tables / PLT stubs.
            let address = match target {
                RmOperand::Reg(reg) => ctx.read_gpr(reg)?,
                RmOperand::Mem(memory) => {
                    if memory.segment.is_some() {
                        AsmValue::Constant(AsmConstant::Int(0, AsmType::I64))
                    } else {
                        let addr = compute_address(
                            ctx,
                            memory,
                            inst.offset,
                            inst.len,
                            &[],
                            instructions,
                            next_id,
                        )?;
                        let load_id = *next_id;
                        instructions.push(AsmInstruction {
                            id: load_id,
                            opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                            kind: AsmInstructionKind::Load {
                                address: addr,
                                alignment: None,
                                volatile: true,
                            },
                            ty: AsmType::I64,
                            operands: Vec::new(),
                            implicit_uses: Vec::new(),
                            implicit_defs: Vec::new(),
                            encoding: None,
                            debug_info: None,
                            annotations: Vec::new(),
                        });
                        *next_id += 1;
                        AsmValue::Register(load_id)
                    }
                }
            };
            Ok(fp_core::asmir::AsmTerminator::IndirectBr {
                address,
                destinations: Vec::new(),
            })
        }
        _ => Err(Error::from("internal error: expected terminator")),
    }
}
