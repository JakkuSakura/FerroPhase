//! Program-level lowering context and entry point.
//!
//! [`LoweringContext`] owns the output [`LirBlob`] and iterates over
//! bytecode functions, delegating per-function lowering to
//! [`FunctionLowering`][super::function::FunctionLowering].

use fp_bytecode::{BytecodeBlock, BytecodeFunction, BytecodeProgram};
use fp_core::lir::{LirBlob, LirDataLayout};
use std::collections::{HashMap, HashSet};

use super::LowerResult;
use super::function::FunctionLowering;

/// Top-level entry point: convert a complete bytecode program into LIR.
pub fn lower_program(program: &BytecodeProgram) -> LowerResult<LirBlob> {
    validate_program(program)?;
    let mut ctx = LoweringContext::new(program);
    for func in &program.functions {
        let lir_func = ctx.lower_function(func)?;
        ctx.program.add_function(lir_func);
    }
    Ok(ctx.program)
}

fn validate_program(program: &BytecodeProgram) -> LowerResult<()> {
    if let Some(entry) = &program.entry {
        if !program
            .functions
            .iter()
            .any(|function| function.name == *entry)
        {
            return Err(super::LowerError::Internal(format!(
                "entry function `{entry}` is not present in the program"
            )));
        }
    }
    for function in &program.functions {
        let block_ids: HashSet<u32> = function.blocks.iter().map(|block| block.id).collect();
        if block_ids.len() != function.blocks.len() {
            return Err(super::LowerError::Internal(format!(
                "function `{}` contains duplicate basic-block IDs",
                function.name
            )));
        }
        for block in &function.blocks {
            for instruction in &block.code {
                match instruction {
                    fp_bytecode::BytecodeInstr::LoadConst(id) => {
                        if (*id as usize) >= program.const_pool.len() {
                            return Err(super::LowerError::Internal(format!(
                                "function `{}` block {} references missing constant {}",
                                function.name, block.id, id
                            )));
                        }
                    }
                    fp_bytecode::BytecodeInstr::LoadLocal(local)
                    | fp_bytecode::BytecodeInstr::StoreLocal(local) => {
                        validate_local(function, block.id, *local)?;
                    }
                    fp_bytecode::BytecodeInstr::LoadPlace(place)
                    | fp_bytecode::BytecodeInstr::StorePlace(place) => {
                        validate_place(function, block.id, place)?;
                    }
                    fp_bytecode::BytecodeInstr::IntrinsicCall { .. }
                    | fp_bytecode::BytecodeInstr::BinaryOp(_)
                    | fp_bytecode::BytecodeInstr::UnaryOp(_)
                    | fp_bytecode::BytecodeInstr::MakeTuple(_)
                    | fp_bytecode::BytecodeInstr::MakeArray(_)
                    | fp_bytecode::BytecodeInstr::MakeList(_)
                    | fp_bytecode::BytecodeInstr::MakeMap(_)
                    | fp_bytecode::BytecodeInstr::ContainerGet
                    | fp_bytecode::BytecodeInstr::ContainerLen
                    | fp_bytecode::BytecodeInstr::Pop => {}
                }
            }
            let targets = match &block.terminator {
                fp_bytecode::BytecodeTerminator::Jump { target } => vec![*target],
                fp_bytecode::BytecodeTerminator::JumpIfTrue { target, otherwise }
                | fp_bytecode::BytecodeTerminator::JumpIfFalse { target, otherwise } => {
                    vec![*target, *otherwise]
                }
                fp_bytecode::BytecodeTerminator::SwitchInt {
                    targets, otherwise, ..
                } => {
                    if let fp_bytecode::BytecodeTerminator::SwitchInt { values, .. } =
                        &block.terminator
                    {
                        if values.len() != targets.len() {
                            return Err(super::LowerError::Internal(format!(
                                "function `{}` block {} has {} switch values but {} targets",
                                function.name,
                                block.id,
                                values.len(),
                                targets.len()
                            )));
                        }
                    }
                    targets
                        .iter()
                        .copied()
                        .chain(std::iter::once(*otherwise))
                        .collect()
                }
                fp_bytecode::BytecodeTerminator::Call { target, .. } => vec![*target],
                fp_bytecode::BytecodeTerminator::Return
                | fp_bytecode::BytecodeTerminator::Abort
                | fp_bytecode::BytecodeTerminator::Unreachable => Vec::new(),
            };
            if let fp_bytecode::BytecodeTerminator::Call { callee, .. } = &block.terminator {
                if let fp_bytecode::BytecodeCallee::Local(place) = callee {
                    validate_place(function, block.id, place)?;
                }
            }
            for target in targets {
                if !block_ids.contains(&target) {
                    return Err(super::LowerError::Internal(format!(
                        "function `{}` block {} targets missing block {}",
                        function.name, block.id, target
                    )));
                }
            }
        }
    }
    Ok(())
}

fn validate_local(function: &BytecodeFunction, block_id: u32, local: u32) -> LowerResult<()> {
    if (local as usize) >= function.local_types.len() {
        return Err(super::LowerError::Internal(format!(
            "function `{}` block {} references missing local {}",
            function.name, block_id, local
        )));
    }
    Ok(())
}

fn validate_place(
    function: &BytecodeFunction,
    block_id: u32,
    place: &fp_bytecode::BytecodePlace,
) -> LowerResult<()> {
    validate_local(function, block_id, place.local)?;
    for projection in &place.projection {
        if let fp_bytecode::BytecodePlaceElem::Index(local) = projection {
            validate_local(function, block_id, *local)?;
        }
    }
    Ok(())
}

/// Accumulates the output [`LirBlob`] while lowering each bytecode
/// function in turn.
pub(crate) struct LoweringContext<'a> {
    pub program: LirBlob,
    pub bytecode: &'a BytecodeProgram,
}

impl<'a> LoweringContext<'a> {
    pub fn new(bytecode: &'a BytecodeProgram) -> Self {
        Self {
            program: LirBlob::new(
                LirDataLayout::new(
                    64,
                    8,
                    vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
                )
                .expect("valid stack VM data layout"),
            ),
            bytecode,
        }
    }

    /// Lower a single bytecode function into an [`LirFunction`].
    pub fn lower_function(
        &mut self,
        func: &BytecodeFunction,
    ) -> LowerResult<fp_core::lir::LirFunction> {
        let entry_block_id = func.blocks.first().map(|b| b.id).unwrap_or(0);
        let sig = fp_core::lir::LirFunctionSignature {
            params: func.param_types.clone(),
            return_type: func.return_type.clone(),
            is_variadic: false,
        };

        let locals: Vec<fp_core::lir::LirLocal> = func
            .local_types
            .iter()
            .enumerate()
            .map(|(i, ty)| fp_core::lir::LirLocal {
                id: i as u32,
                ty: ty.clone(),
                name: Some(format!("local_{i}")),
                is_argument: i > 0 && i <= func.param_types.len(),
            })
            .collect();

        let mut lir_func = fp_core::lir::LirFunction::new(
            fp_core::lir::Name::new(func.name.clone()),
            sig,
            fp_core::lir::CallingConvention::C,
            fp_core::lir::Linkage::Internal,
        );
        lir_func.locals = locals;

        let local_types = lir_func
            .locals
            .iter()
            .map(|local| local.ty.clone())
            .collect();
        let mut fl = FunctionLowering::new(
            self.bytecode,
            &mut lir_func,
            entry_block_id,
            local_types,
            bytecode_predecessors(func),
        );

        // The LirInterpreter pre-allocates stack slots for all locals
        // declared in func.locals during function execution. We reference
        // them by local index rather than emitting Alloca
        // instructions, which would permanently consume stack space.

        // Lower reachable blocks in reverse postorder. This makes every
        // acyclic predecessor available before a join, independent of the
        // block order used by the bytecode serializer.
        let blocks_by_id: HashMap<u32, &BytecodeBlock> =
            func.blocks.iter().map(|block| (block.id, block)).collect();
        for block_id in lowering_order(func, entry_block_id) {
            if let Some(block) = blocks_by_id.get(&block_id) {
                fl.lower_block(block)?;
            }
        }
        // Preserve diagnostics for disconnected blocks rather than silently
        // dropping malformed or intentionally unreachable bytecode blocks.
        for block in &func.blocks {
            if !fl.func.basic_blocks.iter().any(|lir| lir.id == block.id) {
                fl.lower_block(block)?;
            }
        }

        // Compute predecessor/successor sets for each block.
        super::cfg::compute_cfg(&mut lir_func);

        Ok(lir_func)
    }
}

fn lowering_order(func: &BytecodeFunction, entry: u32) -> Vec<u32> {
    let known: HashSet<u32> = func.blocks.iter().map(|block| block.id).collect();
    let blocks = func.blocks.iter().map(|block| (block.id, block)).collect();
    let reachable = reachable_blocks(entry, &known, &blocks);
    let mut predecessors: HashMap<u32, HashSet<u32>> = HashMap::new();
    for id in &reachable {
        for successor in bytecode_successors(&blocks[id].terminator) {
            if reachable.contains(&successor) {
                predecessors.entry(successor).or_default().insert(*id);
            }
        }
    }
    let mut order = Vec::with_capacity(reachable.len());
    let mut emitted = HashSet::new();
    while order.len() < reachable.len() {
        let next = reachable.iter().copied().find(|id| {
            !emitted.contains(id)
                && (*id == entry
                    || predecessors
                        .get(id)
                        .is_none_or(|preds| preds.iter().all(|pred| emitted.contains(pred))))
        });
        let Some(next) = next else { break };
        emitted.insert(next);
        order.push(next);
    }
    // A remaining cycle is intentionally left in declaration order; the
    // lowering pass reports its unresolved carried stack explicitly.
    order.extend(
        func.blocks
            .iter()
            .map(|block| block.id)
            .filter(|id| reachable.contains(id) && emitted.insert(*id)),
    );
    order
}

fn reachable_blocks(
    entry: u32,
    known: &HashSet<u32>,
    blocks: &HashMap<u32, &BytecodeBlock>,
) -> HashSet<u32> {
    let mut reachable = HashSet::new();
    let mut pending = vec![entry];
    while let Some(id) = pending.pop() {
        if !known.contains(&id) || !reachable.insert(id) {
            continue;
        }
        if let Some(block) = blocks.get(&id) {
            pending.extend(bytecode_successors(&block.terminator));
        }
    }
    reachable
}

fn bytecode_successors(terminator: &fp_bytecode::BytecodeTerminator) -> Vec<u32> {
    match terminator {
        fp_bytecode::BytecodeTerminator::Jump { target } => vec![*target],
        fp_bytecode::BytecodeTerminator::JumpIfTrue { target, otherwise }
        | fp_bytecode::BytecodeTerminator::JumpIfFalse { target, otherwise } => {
            vec![*target, *otherwise]
        }
        fp_bytecode::BytecodeTerminator::SwitchInt {
            targets, otherwise, ..
        } => targets
            .iter()
            .copied()
            .chain(std::iter::once(*otherwise))
            .collect(),
        fp_bytecode::BytecodeTerminator::Call { target, .. } => vec![*target],
        _ => Vec::new(),
    }
}

fn bytecode_predecessors(func: &BytecodeFunction) -> HashMap<u32, Vec<u32>> {
    let mut predecessors: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in &func.blocks {
        let targets: Vec<u32> = match &block.terminator {
            fp_bytecode::BytecodeTerminator::Jump { target } => vec![*target],
            fp_bytecode::BytecodeTerminator::JumpIfTrue { target, otherwise }
            | fp_bytecode::BytecodeTerminator::JumpIfFalse { target, otherwise } => {
                vec![*target, *otherwise]
            }
            fp_bytecode::BytecodeTerminator::SwitchInt {
                targets, otherwise, ..
            } => targets
                .iter()
                .copied()
                .chain(std::iter::once(*otherwise))
                .collect(),
            fp_bytecode::BytecodeTerminator::Call { target, .. } => vec![*target],
            _ => Vec::new(),
        };
        for target in targets {
            predecessors.entry(target).or_default().push(block.id);
        }
    }
    predecessors
}
