use std::collections::{HashMap, HashSet, VecDeque};

use fp_core::error::Error;
use fp_core::lir::{
    BasicBlockId, LirFunction, LirId, LirInstruction, LirInstructionKind, LirProgram,
    LirTerminator, LirValue,
};

use crate::error::optimization_error;

#[derive(Debug, Clone)]
struct PromoteCandidate {
    alloca_id: LirId,
    value_type: fp_core::lir::LirType,
}

#[derive(Debug, Clone)]
struct BlockFacts {
    replaceable_loads: HashMap<LirId, LirValue>,
}

#[derive(Debug, Clone)]
struct PromotionAnalysis {
    block_facts: Vec<BlockFacts>,
    predecessor_copies: HashMap<usize, Vec<LirInstruction>>,
}

pub fn promote_stack_to_register(program: &mut LirProgram) -> Result<usize, Error> {
    let mut total_changes = 0usize;
    for function in &mut program.functions {
        total_changes += promote_stack_in_function(function)?;
    }
    Ok(total_changes)
}

fn promote_stack_in_function(function: &mut LirFunction) -> Result<usize, Error> {
    let candidates = collect_promote_candidates(function);
    if candidates.is_empty() {
        return Ok(0);
    }

    let PromotionAnalysis {
        block_facts,
        predecessor_copies,
    } = analyze_stack_values(function, &candidates)?;
    let mut changes = 0usize;

    for (block_idx, block) in function.basic_blocks.iter_mut().enumerate() {
        let facts = &block_facts[block_idx];
        let replacements = facts.replaceable_loads.clone();
        let mut instructions = Vec::with_capacity(block.instructions.len());

        for instruction in &block.instructions {
            if facts.replaceable_loads.contains_key(&instruction.id) {
                changes += 1;
                continue;
            }

            let mut rewritten = instruction.clone();
            rewrite_instruction_values(&mut rewritten, &replacements);
            instructions.push(rewritten);
        }

        let mut terminator = block.terminator.clone();
        rewrite_terminator_values(&mut terminator, &replacements);
        block.instructions = instructions;
        block.terminator = terminator;
    }

    for (block_idx, mut copies) in predecessor_copies {
        changes += copies.len();
        function.basic_blocks[block_idx]
            .instructions
            .append(&mut copies);
    }

    let dead_allocas = collect_dead_promoted_allocas(function, &candidates);
    if !dead_allocas.is_empty() {
        for block in &mut function.basic_blocks {
            let before = block.instructions.len();
            block.instructions.retain(|instruction| {
                !matches!(
                    &instruction.kind,
                    LirInstructionKind::Alloca { .. } if dead_allocas.contains(&instruction.id)
                ) && !matches!(
                    &instruction.kind,
                    LirInstructionKind::Store { address: LirValue::Register(id), .. }
                        if dead_allocas.contains(id)
                )
            });
            changes += before - block.instructions.len();
        }
    }

    Ok(changes)
}

fn collect_promote_candidates(function: &LirFunction) -> Vec<PromoteCandidate> {
    let mut allocas = HashMap::new();
    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            if let LirInstructionKind::Alloca { .. } = &instruction.kind {
                let Some(fp_core::lir::LirType::Ptr(value_type)) = instruction.type_hint.clone()
                else {
                    continue;
                };
                allocas.insert(
                    instruction.id,
                    PromoteCandidate {
                        alloca_id: instruction.id,
                        value_type: *value_type,
                    },
                );
            }
        }
    }

    if allocas.is_empty() {
        return Vec::new();
    }

    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            match &instruction.kind {
                LirInstructionKind::Load { address, .. } => {
                    if let Some(id) = direct_register(address) {
                        continue_candidate_address_use(&mut allocas, id);
                    }
                }
                LirInstructionKind::Store { value, address, .. } => {
                    if let Some(id) = direct_register(address) {
                        continue_candidate_address_use(&mut allocas, id);
                    }
                    remove_candidate_value_use(&mut allocas, value);
                }
                other => remove_candidate_instruction_uses(&mut allocas, other),
            }
        }
        remove_candidate_terminator_uses(&mut allocas, &block.terminator);
    }

    let mut candidates: Vec<_> = allocas.into_values().collect();
    candidates.sort_by_key(|candidate| candidate.alloca_id);
    candidates
}

fn continue_candidate_address_use(
    allocas: &mut HashMap<LirId, PromoteCandidate>,
    address_id: LirId,
) {
    if allocas.contains_key(&address_id) {
        return;
    }
}

fn remove_candidate_value_use(allocas: &mut HashMap<LirId, PromoteCandidate>, value: &LirValue) {
    if let Some(id) = direct_register(value) {
        allocas.remove(&id);
    }
}

fn remove_candidate_instruction_uses(
    allocas: &mut HashMap<LirId, PromoteCandidate>,
    kind: &LirInstructionKind,
) {
    for value in instruction_values(kind) {
        remove_candidate_value_use(allocas, value);
    }
}

fn remove_candidate_terminator_uses(
    allocas: &mut HashMap<LirId, PromoteCandidate>,
    terminator: &LirTerminator,
) {
    for value in terminator_values(terminator) {
        remove_candidate_value_use(allocas, value);
    }
}

fn analyze_stack_values(
    function: &LirFunction,
    candidates: &[PromoteCandidate],
) -> Result<PromotionAnalysis, Error> {
    let mut block_index = HashMap::new();
    for (idx, block) in function.basic_blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    let slot_index: HashMap<LirId, usize> = candidates
        .iter()
        .enumerate()
        .map(|(idx, candidate)| (candidate.alloca_id, idx))
        .collect();
    let slot_count = candidates.len();

    let mut in_states = vec![vec![None; slot_count]; function.basic_blocks.len()];
    let mut out_states = vec![vec![None; slot_count]; function.basic_blocks.len()];
    let mut replaceable_loads = vec![HashMap::new(); function.basic_blocks.len()];
    let merge_reg_preferences = collect_merge_reg_preferences(function, &slot_index);
    let mut merge_regs = HashMap::new();
    let mut next_virtual_id = next_function_instruction_id(function);
    let mut worklist: VecDeque<usize> = (0..function.basic_blocks.len()).collect();
    let mut queued: HashSet<usize> = (0..function.basic_blocks.len()).collect();

    while let Some(block_idx) = worklist.pop_front() {
        queued.remove(&block_idx);
        let block = &function.basic_blocks[block_idx];
        let incoming = meet_predecessor_states(
            block_idx,
            block,
            &block_index,
            &out_states,
            slot_count,
            &mut merge_regs,
            &merge_reg_preferences,
            &mut next_virtual_id,
        )?;
        let (outgoing, loads) = simulate_block(block, &incoming, &slot_index);

        let in_changed = in_states[block_idx] != incoming;
        let out_changed = out_states[block_idx] != outgoing;
        let load_changed = replaceable_loads[block_idx] != loads;

        if in_changed {
            in_states[block_idx] = incoming;
        }
        if out_changed {
            out_states[block_idx] = outgoing;
        }
        if load_changed {
            replaceable_loads[block_idx] = loads;
        }

        if in_changed || out_changed || load_changed {
            for successor in &block.successors {
                let Some(successor_idx) = block_index.get(successor).copied() else {
                    return Err(optimization_error(format!(
                        "missing successor block {successor}"
                    )));
                };
                if queued.insert(successor_idx) {
                    worklist.push_back(successor_idx);
                }
            }
        }
    }

    let predecessor_copies = build_predecessor_copies(
        function,
        candidates,
        &block_index,
        &in_states,
        &out_states,
        &merge_regs,
    )?;

    Ok(PromotionAnalysis {
        block_facts: replaceable_loads
            .into_iter()
            .map(|loads| BlockFacts {
                replaceable_loads: loads,
            })
            .collect(),
        predecessor_copies,
    })
}

fn meet_predecessor_states(
    block_idx: usize,
    block: &fp_core::lir::LirBasicBlock,
    block_index: &HashMap<BasicBlockId, usize>,
    out_states: &[Vec<Option<LirValue>>],
    slot_count: usize,
    merge_regs: &mut HashMap<(usize, usize), LirId>,
    merge_reg_preferences: &HashMap<(usize, usize), LirId>,
    next_virtual_id: &mut LirId,
) -> Result<Vec<Option<LirValue>>, Error> {
    if block.predecessors.is_empty() {
        return Ok(vec![None; slot_count]);
    }

    let mut predecessor_states = Vec::with_capacity(block.predecessors.len());
    for predecessor in &block.predecessors {
        let Some(pred_idx) = block_index.get(predecessor).copied() else {
            return Err(optimization_error(format!(
                "missing predecessor block {predecessor}"
            )));
        };
        predecessor_states.push(&out_states[pred_idx]);
    }

    let mut merged = vec![None; slot_count];
    for slot_idx in 0..slot_count {
        merged[slot_idx] = merge_slot_value(
            block_idx,
            slot_idx,
            &predecessor_states,
            merge_regs,
            merge_reg_preferences,
            next_virtual_id,
        );
    }
    Ok(merged)
}

fn merge_slot_value(
    block_idx: usize,
    slot_idx: usize,
    predecessor_states: &[&Vec<Option<LirValue>>],
    merge_regs: &mut HashMap<(usize, usize), LirId>,
    merge_reg_preferences: &HashMap<(usize, usize), LirId>,
    next_virtual_id: &mut LirId,
) -> Option<LirValue> {
    let mut values = predecessor_states
        .iter()
        .map(|state| state[slot_idx].clone());
    let first = values.next()??;
    let mut all_equal = true;
    for value in values {
        let value = value?;
        if value != first {
            all_equal = false;
        }
    }
    if all_equal {
        return Some(first);
    }

    let reg_id = *merge_regs.entry((block_idx, slot_idx)).or_insert_with(|| {
        if let Some(preferred) = merge_reg_preferences.get(&(block_idx, slot_idx)).copied() {
            return preferred;
        }
        let reg_id = *next_virtual_id;
        *next_virtual_id += 1;
        reg_id
    });
    Some(LirValue::Register(reg_id))
}

fn build_predecessor_copies(
    function: &LirFunction,
    candidates: &[PromoteCandidate],
    block_index: &HashMap<BasicBlockId, usize>,
    in_states: &[Vec<Option<LirValue>>],
    out_states: &[Vec<Option<LirValue>>],
    merge_regs: &HashMap<(usize, usize), LirId>,
) -> Result<HashMap<usize, Vec<LirInstruction>>, Error> {
    let mut copies_per_block: HashMap<usize, Vec<LirInstruction>> = HashMap::new();

    for (&(block_idx, slot_idx), &reg_id) in merge_regs {
        if in_states[block_idx][slot_idx] != Some(LirValue::Register(reg_id)) {
            continue;
        }
        let block = &function.basic_blocks[block_idx];
        let candidate = &candidates[slot_idx];
        for predecessor in &block.predecessors {
            let Some(pred_idx) = block_index.get(predecessor).copied() else {
                return Err(optimization_error(format!(
                    "missing predecessor block {predecessor}"
                )));
            };
            let Some(value) = out_states[pred_idx][slot_idx].clone() else {
                return Err(optimization_error(format!(
                    "missing promoted predecessor value for block {} slot {}",
                    block.id, slot_idx
                )));
            };
            if value == LirValue::Register(reg_id) {
                continue;
            }
            copies_per_block
                .entry(pred_idx)
                .or_default()
                .push(LirInstruction {
                    id: reg_id,
                    kind: LirInstructionKind::Freeze(value),
                    type_hint: Some(candidate.value_type.clone()),
                    debug_info: None,
                });
        }
    }

    Ok(copies_per_block)
}

fn next_function_instruction_id(function: &LirFunction) -> LirId {
    let mut next_id = 0;
    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            next_id = next_id.max(instruction.id.saturating_add(1));
        }
    }
    next_id
}

fn collect_merge_reg_preferences(
    function: &LirFunction,
    slot_index: &HashMap<LirId, usize>,
) -> HashMap<(usize, usize), LirId> {
    let mut preferences = HashMap::new();
    for (block_idx, block) in function.basic_blocks.iter().enumerate() {
        let mut seen_store = vec![false; slot_index.len()];
        for instruction in &block.instructions {
            match &instruction.kind {
                LirInstructionKind::Load { address, .. } => {
                    let Some(slot_idx) = slot_for_address(slot_index, address) else {
                        continue;
                    };
                    if seen_store[slot_idx] {
                        continue;
                    }
                    preferences
                        .entry((block_idx, slot_idx))
                        .or_insert(instruction.id);
                }
                LirInstructionKind::Store { address, .. } => {
                    if let Some(slot_idx) = slot_for_address(slot_index, address) {
                        seen_store[slot_idx] = true;
                    }
                }
                _ => {}
            }
        }
    }
    preferences
}

fn simulate_block(
    block: &fp_core::lir::LirBasicBlock,
    incoming: &[Option<LirValue>],
    slot_index: &HashMap<LirId, usize>,
) -> (Vec<Option<LirValue>>, HashMap<LirId, LirValue>) {
    let mut state = incoming.to_vec();
    let mut replacements = HashMap::new();

    for instruction in &block.instructions {
        match &instruction.kind {
            LirInstructionKind::Load { address, .. } => {
                if let Some(slot) = slot_for_address(slot_index, address) {
                    if let Some(value) = state[slot].clone() {
                        replacements.insert(instruction.id, value);
                    } else {
                        state[slot] = Some(LirValue::Register(instruction.id));
                    }
                }
            }
            LirInstructionKind::Store { value, address, .. } => {
                if let Some(slot) = slot_for_address(slot_index, address) {
                    state[slot] = Some(resolve_value(value, &replacements));
                }
            }
            _ => {}
        }
    }

    (state, replacements)
}

fn collect_dead_promoted_allocas(
    function: &LirFunction,
    candidates: &[PromoteCandidate],
) -> HashSet<LirId> {
    let candidate_ids: HashSet<LirId> = candidates
        .iter()
        .map(|candidate| candidate.alloca_id)
        .collect();
    let mut live_loads = HashSet::new();

    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            if let LirInstructionKind::Load { address, .. } = &instruction.kind {
                if let Some(id) = direct_register(address) {
                    if candidate_ids.contains(&id) {
                        live_loads.insert(id);
                    }
                }
            }
        }
    }

    candidate_ids
        .into_iter()
        .filter(|id| !live_loads.contains(id))
        .collect()
}

#[cfg(test)]
fn lower_phis_in_function(function: &mut LirFunction) -> Result<usize, Error> {
    let mut block_index = HashMap::new();
    for (idx, block) in function.basic_blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    let mut copies_per_block: HashMap<usize, Vec<LirInstruction>> = HashMap::new();
    let mut changes = 0usize;

    for block in &mut function.basic_blocks {
        let mut retained = Vec::with_capacity(block.instructions.len());
        for instruction in &block.instructions {
            let LirInstructionKind::Phi { incoming } = &instruction.kind else {
                retained.push(instruction.clone());
                continue;
            };

            for (value, predecessor) in incoming {
                let Some(pred_idx) = block_index.get(predecessor).copied() else {
                    return Err(optimization_error(format!(
                        "missing phi predecessor block {predecessor}"
                    )));
                };
                copies_per_block
                    .entry(pred_idx)
                    .or_default()
                    .push(LirInstruction {
                        id: instruction.id,
                        kind: LirInstructionKind::Freeze(value.clone()),
                        type_hint: instruction.type_hint.clone(),
                        debug_info: instruction.debug_info.clone(),
                    });
            }
            changes += 1;
        }
        block.instructions = retained;
    }

    for (block_idx, mut copies) in copies_per_block {
        changes += copies.len();
        function.basic_blocks[block_idx]
            .instructions
            .append(&mut copies);
    }

    Ok(changes)
}

fn rewrite_instruction_values(
    instruction: &mut LirInstruction,
    replacements: &HashMap<LirId, LirValue>,
) {
    match &mut instruction.kind {
        LirInstructionKind::Add(lhs, rhs)
        | LirInstructionKind::Sub(lhs, rhs)
        | LirInstructionKind::Mul(lhs, rhs)
        | LirInstructionKind::Div(lhs, rhs)
        | LirInstructionKind::Rem(lhs, rhs)
        | LirInstructionKind::And(lhs, rhs)
        | LirInstructionKind::Or(lhs, rhs)
        | LirInstructionKind::Xor(lhs, rhs)
        | LirInstructionKind::Shl(lhs, rhs)
        | LirInstructionKind::Shr(lhs, rhs)
        | LirInstructionKind::Eq(lhs, rhs)
        | LirInstructionKind::Ne(lhs, rhs)
        | LirInstructionKind::Lt(lhs, rhs)
        | LirInstructionKind::Le(lhs, rhs)
        | LirInstructionKind::Gt(lhs, rhs)
        | LirInstructionKind::Ge(lhs, rhs) => {
            *lhs = resolve_value(lhs, replacements);
            *rhs = resolve_value(rhs, replacements);
        }
        LirInstructionKind::Not(value)
        | LirInstructionKind::PtrToInt(value)
        | LirInstructionKind::IntToPtr(value)
        | LirInstructionKind::Trunc(value, _)
        | LirInstructionKind::ZExt(value, _)
        | LirInstructionKind::SExt(value, _)
        | LirInstructionKind::FPTrunc(value, _)
        | LirInstructionKind::FPExt(value, _)
        | LirInstructionKind::FPToUI(value, _)
        | LirInstructionKind::FPToSI(value, _)
        | LirInstructionKind::UIToFP(value, _)
        | LirInstructionKind::SIToFP(value, _)
        | LirInstructionKind::Bitcast(value, _)
        | LirInstructionKind::SextOrTrunc(value, _)
        | LirInstructionKind::Freeze(value) => {
            *value = resolve_value(value, replacements);
        }
        LirInstructionKind::Load { address, .. } => {
            *address = resolve_value(address, replacements);
        }
        LirInstructionKind::Store { value, address, .. } => {
            *value = resolve_value(value, replacements);
            *address = resolve_value(address, replacements);
        }
        LirInstructionKind::Alloca { size, .. } => {
            *size = resolve_value(size, replacements);
        }
        LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
            *ptr = resolve_value(ptr, replacements);
            for index in indices {
                *index = resolve_value(index, replacements);
            }
        }
        LirInstructionKind::ExtractValue { aggregate, .. } => {
            *aggregate = resolve_value(aggregate, replacements);
        }
        LirInstructionKind::InsertValue {
            aggregate, element, ..
        } => {
            *aggregate = resolve_value(aggregate, replacements);
            *element = resolve_value(element, replacements);
        }
        LirInstructionKind::Call { function, args, .. } => {
            *function = resolve_value(function, replacements);
            for arg in args {
                *arg = resolve_value(arg, replacements);
            }
        }
        LirInstructionKind::ExecQuery(_) => {}
        LirInstructionKind::IntrinsicCall { args, .. } => {
            for arg in args {
                *arg = resolve_value(arg, replacements);
            }
        }
        LirInstructionKind::Phi { incoming } => {
            for (value, _) in incoming {
                *value = resolve_value(value, replacements);
            }
        }
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            *condition = resolve_value(condition, replacements);
            *if_true = resolve_value(if_true, replacements);
            *if_false = resolve_value(if_false, replacements);
        }
        LirInstructionKind::InlineAsm { inputs, .. } => {
            for input in inputs {
                *input = resolve_value(input, replacements);
            }
        }
        LirInstructionKind::LandingPad { personality, .. } => {
            if let Some(value) = personality {
                *value = resolve_value(value, replacements);
            }
        }
        LirInstructionKind::Unreachable => {}
    }
}

fn rewrite_terminator_values(
    terminator: &mut LirTerminator,
    replacements: &HashMap<LirId, LirValue>,
) {
    match terminator {
        LirTerminator::Return(Some(value))
        | LirTerminator::Resume(value)
        | LirTerminator::IndirectBr { address: value, .. } => {
            *value = resolve_value(value, replacements);
        }
        LirTerminator::CondBr { condition, .. } => {
            *condition = resolve_value(condition, replacements);
        }
        LirTerminator::Switch { value, .. } => {
            *value = resolve_value(value, replacements);
        }
        LirTerminator::Invoke { function, args, .. } => {
            *function = resolve_value(function, replacements);
            for arg in args {
                *arg = resolve_value(arg, replacements);
            }
        }
        LirTerminator::CleanupRet { cleanup_pad, .. }
        | LirTerminator::CatchRet {
            catch_pad: cleanup_pad,
            ..
        } => {
            *cleanup_pad = resolve_value(cleanup_pad, replacements);
        }
        LirTerminator::CatchSwitch { parent_pad, .. } => {
            if let Some(value) = parent_pad {
                *value = resolve_value(value, replacements);
            }
        }
        LirTerminator::Return(None) | LirTerminator::Br(_) | LirTerminator::Unreachable => {}
    }
}

fn resolve_value(value: &LirValue, replacements: &HashMap<LirId, LirValue>) -> LirValue {
    let mut current = value.clone();
    while let LirValue::Register(id) = current.clone() {
        let Some(next) = replacements.get(&id) else {
            break;
        };
        if *next == current {
            break;
        }
        current = next.clone();
    }
    current
}

fn slot_for_address(slot_index: &HashMap<LirId, usize>, address: &LirValue) -> Option<usize> {
    direct_register(address).and_then(|id| slot_index.get(&id).copied())
}

fn direct_register(value: &LirValue) -> Option<LirId> {
    match value {
        LirValue::Register(id) => Some(*id),
        _ => None,
    }
}

fn instruction_values(kind: &LirInstructionKind) -> Vec<&LirValue> {
    match kind {
        LirInstructionKind::Add(lhs, rhs)
        | LirInstructionKind::Sub(lhs, rhs)
        | LirInstructionKind::Mul(lhs, rhs)
        | LirInstructionKind::Div(lhs, rhs)
        | LirInstructionKind::Rem(lhs, rhs)
        | LirInstructionKind::And(lhs, rhs)
        | LirInstructionKind::Or(lhs, rhs)
        | LirInstructionKind::Xor(lhs, rhs)
        | LirInstructionKind::Shl(lhs, rhs)
        | LirInstructionKind::Shr(lhs, rhs)
        | LirInstructionKind::Eq(lhs, rhs)
        | LirInstructionKind::Ne(lhs, rhs)
        | LirInstructionKind::Lt(lhs, rhs)
        | LirInstructionKind::Le(lhs, rhs)
        | LirInstructionKind::Gt(lhs, rhs)
        | LirInstructionKind::Ge(lhs, rhs) => vec![lhs, rhs],
        LirInstructionKind::Not(value)
        | LirInstructionKind::PtrToInt(value)
        | LirInstructionKind::IntToPtr(value)
        | LirInstructionKind::Trunc(value, _)
        | LirInstructionKind::ZExt(value, _)
        | LirInstructionKind::SExt(value, _)
        | LirInstructionKind::FPTrunc(value, _)
        | LirInstructionKind::FPExt(value, _)
        | LirInstructionKind::FPToUI(value, _)
        | LirInstructionKind::FPToSI(value, _)
        | LirInstructionKind::UIToFP(value, _)
        | LirInstructionKind::SIToFP(value, _)
        | LirInstructionKind::Bitcast(value, _)
        | LirInstructionKind::SextOrTrunc(value, _)
        | LirInstructionKind::Freeze(value) => vec![value],
        LirInstructionKind::Load { address, .. } => vec![address],
        LirInstructionKind::Store { value, address, .. } => vec![value, address],
        LirInstructionKind::Alloca { size, .. } => vec![size],
        LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
            let mut values = vec![ptr];
            values.extend(indices.iter());
            values
        }
        LirInstructionKind::ExtractValue { aggregate, .. } => vec![aggregate],
        LirInstructionKind::InsertValue {
            aggregate, element, ..
        } => vec![aggregate, element],
        LirInstructionKind::Call { function, args, .. } => {
            let mut values = vec![function];
            values.extend(args.iter());
            values
        }
        LirInstructionKind::ExecQuery(_) => Vec::new(),
        LirInstructionKind::IntrinsicCall { args, .. } => args.iter().collect(),
        LirInstructionKind::Phi { incoming } => incoming.iter().map(|(value, _)| value).collect(),
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => vec![condition, if_true, if_false],
        LirInstructionKind::InlineAsm { inputs, .. } => inputs.iter().collect(),
        LirInstructionKind::LandingPad { personality, .. } => personality.iter().collect(),
        LirInstructionKind::Unreachable => Vec::new(),
    }
}

fn terminator_values(terminator: &LirTerminator) -> Vec<&LirValue> {
    match terminator {
        LirTerminator::Return(Some(value))
        | LirTerminator::Resume(value)
        | LirTerminator::IndirectBr { address: value, .. } => vec![value],
        LirTerminator::CondBr { condition, .. } => vec![condition],
        LirTerminator::Switch { value, .. } => vec![value],
        LirTerminator::Invoke { function, args, .. } => {
            let mut values = vec![function];
            values.extend(args.iter());
            values
        }
        LirTerminator::CleanupRet { cleanup_pad, .. }
        | LirTerminator::CatchRet {
            catch_pad: cleanup_pad,
            ..
        } => vec![cleanup_pad],
        LirTerminator::CatchSwitch { parent_pad, .. } => parent_pad.iter().collect(),
        LirTerminator::Return(None) | LirTerminator::Br(_) | LirTerminator::Unreachable => {
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirFunction, LirFunctionSignature, LirType, Name,
    };

    fn test_function(blocks: Vec<LirBasicBlock>) -> LirFunction {
        LirFunction {
            name: Name::new("test"),
            signature: LirFunctionSignature {
                params: vec![LirType::I64],
                return_type: LirType::I64,
                is_variadic: false,
            },
            basic_blocks: blocks,
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: Linkage::Internal,
            is_declaration: false,
        }
    }

    #[test]
    fn promotes_simple_stack_value_into_return() {
        let mut function = test_function(vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("bb0")),
            instructions: vec![
                LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Alloca {
                        size: LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I32)),
                        alignment: 8,
                    },
                    type_hint: Some(LirType::Ptr(Box::new(LirType::I64))),
                    debug_info: None,
                },
                LirInstruction {
                    id: 2,
                    kind: LirInstructionKind::Store {
                        value: LirValue::Local(1),
                        address: LirValue::Register(1),
                        alignment: Some(8),
                        volatile: false,
                    },
                    type_hint: None,
                    debug_info: None,
                },
                LirInstruction {
                    id: 3,
                    kind: LirInstructionKind::Load {
                        address: LirValue::Register(1),
                        alignment: Some(8),
                        volatile: false,
                    },
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                },
            ],
            terminator: LirTerminator::Return(Some(LirValue::Register(3))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }]);

        let changes = promote_stack_in_function(&mut function).expect("promotion should succeed");

        assert!(changes >= 3);
        assert!(function.basic_blocks[0].instructions.is_empty());
        assert_eq!(
            function.basic_blocks[0].terminator,
            LirTerminator::Return(Some(LirValue::Local(1)))
        );
    }

    #[test]
    fn promotes_merge_values_into_predecessor_freeze_copies() {
        let mut function = test_function(vec![
            LirBasicBlock {
                id: 0,
                label: Some(Name::new("bb0")),
                instructions: vec![LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Alloca {
                        size: LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I32)),
                        alignment: 8,
                    },
                    type_hint: Some(LirType::Ptr(Box::new(LirType::I64))),
                    debug_info: None,
                }],
                terminator: LirTerminator::CondBr {
                    condition: LirValue::Constant(fp_core::lir::LirConstant::Bool(true)),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            },
            LirBasicBlock {
                id: 1,
                label: Some(Name::new("bb1")),
                instructions: vec![LirInstruction {
                    id: 2,
                    kind: LirInstructionKind::Store {
                        value: LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I64)),
                        address: LirValue::Register(1),
                        alignment: Some(8),
                        volatile: false,
                    },
                    type_hint: None,
                    debug_info: None,
                }],
                terminator: LirTerminator::Br(3),
                predecessors: vec![0],
                successors: vec![3],
            },
            LirBasicBlock {
                id: 2,
                label: Some(Name::new("bb2")),
                instructions: vec![LirInstruction {
                    id: 3,
                    kind: LirInstructionKind::Store {
                        value: LirValue::Constant(fp_core::lir::LirConstant::Int(2, LirType::I64)),
                        address: LirValue::Register(1),
                        alignment: Some(8),
                        volatile: false,
                    },
                    type_hint: None,
                    debug_info: None,
                }],
                terminator: LirTerminator::Br(3),
                predecessors: vec![0],
                successors: vec![3],
            },
            LirBasicBlock {
                id: 3,
                label: Some(Name::new("bb3")),
                instructions: vec![LirInstruction {
                    id: 4,
                    kind: LirInstructionKind::Load {
                        address: LirValue::Register(1),
                        alignment: Some(8),
                        volatile: false,
                    },
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                }],
                terminator: LirTerminator::Return(Some(LirValue::Register(4))),
                predecessors: vec![1, 2],
                successors: Vec::new(),
            },
        ]);

        let changes = promote_stack_in_function(&mut function).expect("promotion should succeed");

        assert!(changes >= 5);
        assert!(function.basic_blocks[3].instructions.is_empty());
        assert!(matches!(
            function.basic_blocks[1].instructions[0].kind,
            LirInstructionKind::Freeze(_)
        ));
        assert!(matches!(
            function.basic_blocks[2].instructions[0].kind,
            LirInstructionKind::Freeze(_)
        ));
        assert_eq!(
            function.basic_blocks[3].terminator,
            LirTerminator::Return(Some(LirValue::Register(
                function.basic_blocks[1].instructions[0].id
            )))
        );
    }

    #[test]
    fn promotes_loop_carried_stack_value_with_header_freeze() {
        let mut function = test_function(vec![
            LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::Alloca {
                            size: LirValue::Constant(fp_core::lir::LirConstant::Int(
                                1,
                                LirType::I32,
                            )),
                            alignment: 8,
                        },
                        type_hint: Some(LirType::Ptr(Box::new(LirType::I64))),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 2,
                        kind: LirInstructionKind::Store {
                            value: LirValue::Constant(fp_core::lir::LirConstant::Int(
                                0,
                                LirType::I64,
                            )),
                            address: LirValue::Register(1),
                            alignment: Some(8),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    },
                ],
                terminator: LirTerminator::Br(1),
                predecessors: Vec::new(),
                successors: vec![1],
            },
            LirBasicBlock {
                id: 1,
                label: Some(Name::new("loop")),
                instructions: vec![
                    LirInstruction {
                        id: 3,
                        kind: LirInstructionKind::Load {
                            address: LirValue::Register(1),
                            alignment: Some(8),
                            volatile: false,
                        },
                        type_hint: Some(LirType::I64),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 4,
                        kind: LirInstructionKind::Add(
                            LirValue::Register(3),
                            LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I64)),
                        ),
                        type_hint: Some(LirType::I64),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 5,
                        kind: LirInstructionKind::Store {
                            value: LirValue::Register(4),
                            address: LirValue::Register(1),
                            alignment: Some(8),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 6,
                        kind: LirInstructionKind::Lt(
                            LirValue::Register(4),
                            LirValue::Constant(fp_core::lir::LirConstant::Int(3, LirType::I64)),
                        ),
                        type_hint: Some(LirType::I1),
                        debug_info: None,
                    },
                ],
                terminator: LirTerminator::CondBr {
                    condition: LirValue::Register(6),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: vec![0, 1],
                successors: vec![1, 2],
            },
            LirBasicBlock {
                id: 2,
                label: Some(Name::new("exit")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(Some(LirValue::Register(4))),
                predecessors: vec![1],
                successors: Vec::new(),
            },
        ]);

        let changes = promote_stack_in_function(&mut function).expect("promotion should succeed");

        assert!(changes >= 5);
        assert!(function.basic_blocks[1]
            .instructions
            .iter()
            .any(|inst| matches!(inst.kind, LirInstructionKind::Add(_, _))));
        assert!(!function.basic_blocks[1]
            .instructions
            .iter()
            .any(|inst| matches!(inst.kind, LirInstructionKind::Load { .. })));
        assert!(!function.basic_blocks[1]
            .instructions
            .iter()
            .any(|inst| matches!(inst.kind, LirInstructionKind::Store { .. })));
        assert!(function.basic_blocks[0]
            .instructions
            .iter()
            .any(|inst| matches!(inst.kind, LirInstructionKind::Freeze(_))));
        assert!(function.basic_blocks[1]
            .instructions
            .iter()
            .any(|inst| matches!(inst.kind, LirInstructionKind::Freeze(_))));
    }

    #[test]
    fn lowers_phi_into_predecessor_freeze_copies() {
        let mut function = test_function(vec![
            LirBasicBlock {
                id: 0,
                label: Some(Name::new("bb0")),
                instructions: Vec::new(),
                terminator: LirTerminator::CondBr {
                    condition: LirValue::Constant(fp_core::lir::LirConstant::Bool(true)),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            },
            LirBasicBlock {
                id: 1,
                label: Some(Name::new("bb1")),
                instructions: Vec::new(),
                terminator: LirTerminator::Br(3),
                predecessors: vec![0],
                successors: vec![3],
            },
            LirBasicBlock {
                id: 2,
                label: Some(Name::new("bb2")),
                instructions: Vec::new(),
                terminator: LirTerminator::Br(3),
                predecessors: vec![0],
                successors: vec![3],
            },
            LirBasicBlock {
                id: 3,
                label: Some(Name::new("bb3")),
                instructions: vec![LirInstruction {
                    id: 7,
                    kind: LirInstructionKind::Phi {
                        incoming: vec![
                            (
                                LirValue::Constant(fp_core::lir::LirConstant::Int(1, LirType::I64)),
                                1,
                            ),
                            (
                                LirValue::Constant(fp_core::lir::LirConstant::Int(2, LirType::I64)),
                                2,
                            ),
                        ],
                    },
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                }],
                terminator: LirTerminator::Return(Some(LirValue::Register(7))),
                predecessors: vec![1, 2],
                successors: Vec::new(),
            },
        ]);

        let changes = lower_phis_in_function(&mut function).expect("phi lowering should succeed");

        assert_eq!(changes, 3);
        assert!(function.basic_blocks[3].instructions.is_empty());
        assert!(matches!(
            function.basic_blocks[1].instructions[0].kind,
            LirInstructionKind::Freeze(_)
        ));
        assert_eq!(function.basic_blocks[1].instructions[0].id, 7);
        assert_eq!(function.basic_blocks[2].instructions[0].id, 7);
    }
}
