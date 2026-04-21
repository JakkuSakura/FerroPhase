use std::collections::{HashMap, HashSet, VecDeque};

use fp_core::error::Error;
use fp_core::lir::{BasicBlockId, LirFunction, LirProgram, LirTerminator};

pub fn remove_unreachable_blocks(program: &mut LirProgram) -> Result<usize, Error> {
    let mut total_changes = 0usize;
    for function in &mut program.functions {
        total_changes += remove_unreachable_blocks_in_function(function)?;
    }
    Ok(total_changes)
}

fn remove_unreachable_blocks_in_function(function: &mut LirFunction) -> Result<usize, Error> {
    let Some(entry_block) = function.basic_blocks.first().map(|block| block.id) else {
        return Ok(0);
    };

    let block_ids: HashSet<BasicBlockId> =
        function.basic_blocks.iter().map(|block| block.id).collect();
    let mut reachable = HashSet::new();
    let mut queue = VecDeque::from([entry_block]);

    while let Some(block_id) = queue.pop_front() {
        if !reachable.insert(block_id) {
            continue;
        }
        let Some(block) = function
            .basic_blocks
            .iter()
            .find(|block| block.id == block_id)
        else {
            continue;
        };
        for successor in &block.successors {
            if block_ids.contains(successor) && !reachable.contains(successor) {
                queue.push_back(*successor);
            }
        }
    }

    let removed = function.basic_blocks.len() - reachable.len();
    if removed != 0 {
        function
            .basic_blocks
            .retain(|block| reachable.contains(&block.id));
        rebuild_cfg_edges(function);
    }

    Ok(removed + collapse_trivial_jump_blocks(function))
}

fn collapse_trivial_jump_blocks(function: &mut LirFunction) -> usize {
    let Some(entry_id) = function.basic_blocks.first().map(|block| block.id) else {
        return 0;
    };
    let mut total_changes = 0usize;

    loop {
        let redirect_map = collect_trivial_jump_redirects(function, entry_id);
        if redirect_map.is_empty() {
            break;
        }

        for block in &mut function.basic_blocks {
            rewrite_terminator_targets(&mut block.terminator, &redirect_map);
        }
        let removed_ids: HashSet<_> = redirect_map.keys().copied().collect();
        let before = function.basic_blocks.len();
        function
            .basic_blocks
            .retain(|block| !removed_ids.contains(&block.id));
        total_changes += before - function.basic_blocks.len();
        rebuild_cfg_edges(function);
    }

    total_changes
}

fn collect_trivial_jump_redirects(
    function: &LirFunction,
    entry_id: BasicBlockId,
) -> HashMap<BasicBlockId, BasicBlockId> {
    let jump_targets: HashMap<BasicBlockId, BasicBlockId> = function
        .basic_blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            LirTerminator::Br(target) if block.id != entry_id && block.instructions.is_empty() => {
                Some((block.id, *target))
            }
            _ => None,
        })
        .collect();

    jump_targets
        .iter()
        .filter_map(|(&block_id, &target)| {
            let resolved = resolve_redirect_target(target, &jump_targets);
            (resolved != block_id).then_some((block_id, resolved))
        })
        .collect()
}

fn resolve_redirect_target(
    mut target: BasicBlockId,
    redirect_map: &HashMap<BasicBlockId, BasicBlockId>,
) -> BasicBlockId {
    let mut seen = HashSet::new();
    while let Some(next) = redirect_map.get(&target).copied() {
        if !seen.insert(target) {
            break;
        }
        target = next;
    }
    target
}

fn rewrite_terminator_targets(
    terminator: &mut LirTerminator,
    redirect_map: &HashMap<BasicBlockId, BasicBlockId>,
) {
    match terminator {
        LirTerminator::Br(target) => {
            *target = resolve_redirect_target(*target, redirect_map);
        }
        LirTerminator::CondBr {
            if_true, if_false, ..
        } => {
            *if_true = resolve_redirect_target(*if_true, redirect_map);
            *if_false = resolve_redirect_target(*if_false, redirect_map);
        }
        LirTerminator::Switch { default, cases, .. } => {
            *default = resolve_redirect_target(*default, redirect_map);
            for (_, target) in cases {
                *target = resolve_redirect_target(*target, redirect_map);
            }
        }
        LirTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => {
            *normal_dest = resolve_redirect_target(*normal_dest, redirect_map);
            *unwind_dest = resolve_redirect_target(*unwind_dest, redirect_map);
        }
        LirTerminator::CleanupRet {
            unwind_dest: Some(target),
            ..
        } => {
            *target = resolve_redirect_target(*target, redirect_map);
        }
        LirTerminator::CatchRet { successor, .. } => {
            *successor = resolve_redirect_target(*successor, redirect_map);
        }
        LirTerminator::CatchSwitch {
            handlers,
            unwind_dest,
            ..
        } => {
            for handler in handlers {
                *handler = resolve_redirect_target(*handler, redirect_map);
            }
            if let Some(target) = unwind_dest {
                *target = resolve_redirect_target(*target, redirect_map);
            }
        }
        LirTerminator::CleanupRet {
            unwind_dest: None, ..
        }
        | LirTerminator::Return(_)
        | LirTerminator::Resume(_)
        | LirTerminator::IndirectBr { .. }
        | LirTerminator::Unreachable => {}
    }
}

fn rebuild_cfg_edges(function: &mut LirFunction) {
    let block_ids: HashSet<BasicBlockId> =
        function.basic_blocks.iter().map(|block| block.id).collect();
    let mut predecessors: HashMap<BasicBlockId, Vec<BasicBlockId>> = function
        .basic_blocks
        .iter()
        .map(|block| (block.id, Vec::new()))
        .collect();

    for block in &mut function.basic_blocks {
        block.successors = terminator_successors(&block.terminator)
            .into_iter()
            .filter(|target| block_ids.contains(target))
            .collect();
        for successor in &block.successors {
            predecessors.entry(*successor).or_default().push(block.id);
        }
    }

    for block in &mut function.basic_blocks {
        block.predecessors = predecessors.remove(&block.id).unwrap_or_default();
    }
}

fn terminator_successors(terminator: &LirTerminator) -> Vec<BasicBlockId> {
    match terminator {
        LirTerminator::Br(target) => vec![*target],
        LirTerminator::CondBr {
            if_true, if_false, ..
        } => vec![*if_true, *if_false],
        LirTerminator::Switch { default, cases, .. } => {
            let mut out = Vec::with_capacity(cases.len() + 1);
            out.push(*default);
            out.extend(cases.iter().map(|(_, target)| *target));
            out
        }
        LirTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => vec![*normal_dest, *unwind_dest],
        LirTerminator::CleanupRet {
            unwind_dest: Some(target),
            ..
        } => vec![*target],
        LirTerminator::CatchRet { successor, .. } => vec![*successor],
        LirTerminator::CatchSwitch {
            handlers,
            unwind_dest,
            ..
        } => {
            let mut out = handlers.clone();
            if let Some(target) = unwind_dest {
                out.push(*target);
            }
            out
        }
        LirTerminator::CleanupRet {
            unwind_dest: None, ..
        }
        | LirTerminator::Return(_)
        | LirTerminator::Resume(_)
        | LirTerminator::IndirectBr { .. }
        | LirTerminator::Unreachable => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram,
        LirTerminator, LirType, Name,
    };

    #[test]
    fn removes_unreachable_blocks_and_rewrites_preds() {
        let mut program = LirProgram {
            globals: Vec::new(),
            functions: vec![LirFunction {
                name: Name::new("test"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::Void,
                    is_variadic: false,
                },
                basic_blocks: vec![
                    LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Br(1),
                        predecessors: Vec::new(),
                        successors: vec![1],
                    },
                    LirBasicBlock {
                        id: 1,
                        label: Some(Name::new("live")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Return(None),
                        predecessors: vec![0, 2],
                        successors: Vec::new(),
                    },
                    LirBasicBlock {
                        id: 2,
                        label: Some(Name::new("dead")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Br(1),
                        predecessors: Vec::new(),
                        successors: vec![1],
                    },
                ],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: Linkage::Internal,
                is_declaration: false,
            }],
            type_definitions: Vec::new(),
            queries: Vec::new(),
        };

        let changes = remove_unreachable_blocks(&mut program).expect("cfg cleanup should succeed");

        assert_eq!(changes, 1);
        let blocks = &program.functions[0].basic_blocks;
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[1].predecessors, vec![0]);
    }

    #[test]
    fn collapses_trivial_jump_blocks() {
        let mut program = LirProgram {
            globals: Vec::new(),
            functions: vec![LirFunction {
                name: Name::new("test"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::Void,
                    is_variadic: false,
                },
                basic_blocks: vec![
                    LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Br(1),
                        predecessors: Vec::new(),
                        successors: vec![1],
                    },
                    LirBasicBlock {
                        id: 1,
                        label: Some(Name::new("redir1")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Br(2),
                        predecessors: vec![0],
                        successors: vec![2],
                    },
                    LirBasicBlock {
                        id: 2,
                        label: Some(Name::new("redir2")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Br(3),
                        predecessors: vec![1],
                        successors: vec![3],
                    },
                    LirBasicBlock {
                        id: 3,
                        label: Some(Name::new("live")),
                        instructions: Vec::new(),
                        terminator: LirTerminator::Return(None),
                        predecessors: vec![2],
                        successors: Vec::new(),
                    },
                ],
                locals: Vec::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: Linkage::Internal,
                is_declaration: false,
            }],
            type_definitions: Vec::new(),
            queries: Vec::new(),
        };

        let changes = remove_unreachable_blocks(&mut program).expect("cfg cleanup should succeed");

        assert_eq!(changes, 2);
        let blocks = &program.functions[0].basic_blocks;
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].terminator, LirTerminator::Br(3));
        assert_eq!(blocks[1].predecessors, vec![0]);
    }
}
