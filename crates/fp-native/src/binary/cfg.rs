use fp_core::asmir::{AsmBlock, AsmTerminator};

pub(super) fn wire_block_edges(blocks: &mut [AsmBlock]) {
    let mut predecessors: std::collections::HashMap<u32, Vec<u32>> =
        std::collections::HashMap::new();

    for block in blocks.iter_mut() {
        block.successors.clear();

        match &block.terminator {
            AsmTerminator::Br(dest) => {
                block.successors.push(*dest);
                predecessors.entry(*dest).or_default().push(block.id);
            }
            AsmTerminator::CondBr {
                if_true, if_false, ..
            } => {
                block.successors.push(*if_true);
                block.successors.push(*if_false);
                predecessors.entry(*if_true).or_default().push(block.id);
                predecessors.entry(*if_false).or_default().push(block.id);
            }
            AsmTerminator::Switch { default, cases, .. } => {
                block.successors.push(*default);
                predecessors.entry(*default).or_default().push(block.id);
                for (_, dest) in cases {
                    block.successors.push(*dest);
                    predecessors.entry(*dest).or_default().push(block.id);
                }
            }
            _ => {}
        }
    }

    for block in blocks.iter_mut() {
        block.predecessors = predecessors.remove(&block.id).unwrap_or_default();
    }
}
