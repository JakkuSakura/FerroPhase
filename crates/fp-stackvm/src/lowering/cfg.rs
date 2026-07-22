//! Control-flow graph computation.
//!
//! After all basic blocks have been lowered, this pass fills in the
//! `predecessors` and `successors` fields of each [`LirBasicBlock`].

use fp_core::lir::{BasicBlockId, LirFunction, LirTerminator};
use std::collections::HashMap;

/// Compute predecessor and successor sets for every basic block in
/// `func`, based on its terminators.
pub(crate) fn compute_cfg(func: &mut LirFunction) {
    let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    let mut succs: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();

    for block in &func.basic_blocks {
        let targets = terminator_targets(&block.terminator);
        succs.insert(block.id, targets.clone());
        for target in &targets {
            preds.entry(*target).or_default().push(block.id);
        }
    }

    for block in &mut func.basic_blocks {
        block.predecessors = preds.remove(&block.id).unwrap_or_default();
        block.successors = succs.remove(&block.id).unwrap_or_default();
    }
}

/// Collect the successor block IDs from a terminator.
fn terminator_targets(term: &LirTerminator) -> Vec<BasicBlockId> {
    match term {
        LirTerminator::Br(dest) => vec![*dest],
        LirTerminator::CondBr {
            if_true, if_false, ..
        } => vec![*if_true, *if_false],
        LirTerminator::Switch {
            default, cases, ..
        } => {
            let mut v: Vec<BasicBlockId> = cases.iter().map(|(_, t)| *t).collect();
            v.push(*default);
            v
        }
        LirTerminator::Invoke {
            normal_dest,
            unwind_dest,
            ..
        } => vec![*normal_dest, *unwind_dest],
        _ => vec![],
    }
}
