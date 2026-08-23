use fp_core::error::Error;
use fp_core::lir::{
    LirBasicBlock, LirConstantData, LirConstantKind, LirFunction, LirInstructionKind, LirBlob,
    LirTerminator, LirValue, LirValueKind,
};

pub fn simplify_bool_conditions(program: &mut LirBlob) -> Result<usize, Error> {
    let mut total_changes = 0usize;
    for function in &mut program.functions {
        total_changes += simplify_bool_conditions_in_function(function);
    }
    Ok(total_changes)
}

fn simplify_bool_conditions_in_function(function: &mut LirFunction) -> usize {
    let mut changes = 0usize;
    for block in &mut function.basic_blocks {
        changes += simplify_block_condbr(block);
        changes += simplify_block_return(block);
    }
    changes
}

fn simplify_block_condbr(block: &mut LirBasicBlock) -> usize {
    let (condition, if_true, if_false) = match &mut block.terminator {
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => (condition, if_true, if_false),
        _ => return 0,
    };
    let Some(last) = block.instructions.last() else {
        return 0;
    };
    let LirValueKind::Register(condition_id) = &condition.kind else {
        return 0;
    };
    if last.id != *condition_id {
        return 0;
    }
    let Some((rewritten, invert)) = bool_condition_alias(&last.kind) else {
        return 0;
    };
    *condition = rewritten;
    if invert {
        std::mem::swap(if_true, if_false);
    }
    block.instructions.pop();
    1
}

fn simplify_block_return(block: &mut LirBasicBlock) -> usize {
    let value = match &mut block.terminator {
        LirTerminator::Return(Some(value)) => value,
        _ => return 0,
    };
    let Some(last) = block.instructions.last() else {
        return 0;
    };
    let LirValueKind::Register(return_id) = &value.kind else {
        return 0;
    };
    if last.id != *return_id {
        return 0;
    }
    let rewritten = match &last.kind {
        LirInstructionKind::Freeze(inner) => inner.clone(),
        _ => return 0,
    };
    *value = rewritten;
    block.instructions.pop();
    1
}

fn bool_condition_alias(kind: &LirInstructionKind) -> Option<(LirValue, bool)> {
    match kind {
        LirInstructionKind::Eq(lhs, rhs) => {
            bool_alias_pair(lhs, rhs).map(|(value, truth)| (value, !truth))
        }
        LirInstructionKind::Ne(lhs, rhs) => {
            bool_alias_pair(lhs, rhs).map(|(value, truth)| (value, truth))
        }
        _ => None,
    }
}

fn bool_alias_pair(lhs: &LirValue, rhs: &LirValue) -> Option<(LirValue, bool)> {
    if let Some(truth) = bool_constant(rhs) {
        return Some((lhs.clone(), truth));
    }
    bool_constant(lhs).map(|truth| (rhs.clone(), truth))
}

fn bool_constant(value: &LirValue) -> Option<bool> {
    match value {
        LirValue {
            kind:
                LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(
                    fp_core::lir::LirInteger::I1(value),
                ))),
            ..
        } => Some(*value),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{
        CallingConvention, Linkage, LirConstant, LirDataLayout, LirFunctionSignature,
        LirInstruction, LirInteger, LirRegister, LirType, Name,
    };

    fn layout() -> LirDataLayout {
        LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid test layout")
    }

    fn bool_value(value: bool) -> LirValue {
        LirValue::constant(
            LirConstant::integer(LirType::I1, LirInteger::I1(value)).expect("valid bool"),
        )
    }

    fn local(id: u32) -> LirValue {
        LirValue::local(id, LirType::I1)
    }

    fn register(id: u32) -> LirValue {
        LirValue::register(id, LirType::I1)
    }

    fn result(id: u32) -> LirRegister {
        LirRegister {
            id,
            ty: LirType::I1,
        }
    }

    fn test_function(blocks: Vec<LirBasicBlock>) -> LirFunction {
        LirFunction {
            def_id: None,
            name: Name::new("test"),
            signature: LirFunctionSignature {
                params: vec![LirType::I1],
                return_type: LirType::I1,
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
    fn simplifies_bool_eq_true_before_condbr() {
        let mut program = LirBlob {
            data_layout: layout(),
            functions: vec![test_function(vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::Freeze(local(1)),
                        result: Some(result(1)),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 2,
                        kind: LirInstructionKind::Eq(register(1), bool_value(true)),
                        result: Some(result(2)),
                        debug_info: None,
                    },
                ],
                terminator: LirTerminator::CondBr {
                    condition: register(2),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            }])],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            comptime_entries: Vec::new(),
            queries: Vec::new(),
        };

        let changes = simplify_bool_conditions(&mut program).expect("bool simplify should work");

        assert_eq!(changes, 1);
        let block = &program.functions[0].basic_blocks[0];
        assert_eq!(block.instructions.len(), 1);
        assert_eq!(
            block.terminator,
            LirTerminator::CondBr {
                condition: register(1),
                if_true: 1,
                if_false: 2,
            }
        );
    }

    #[test]
    fn simplifies_bool_eq_false_by_inverting_branch() {
        let mut program = LirBlob {
            data_layout: layout(),
            functions: vec![test_function(vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![LirInstruction {
                    id: 2,
                    kind: LirInstructionKind::Eq(local(1), bool_value(false)),
                    result: Some(result(2)),
                    debug_info: None,
                }],
                terminator: LirTerminator::CondBr {
                    condition: register(2),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            }])],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            comptime_entries: Vec::new(),
            queries: Vec::new(),
        };

        let changes = simplify_bool_conditions(&mut program).expect("bool simplify should work");

        assert_eq!(changes, 1);
        let block = &program.functions[0].basic_blocks[0];
        assert!(block.instructions.is_empty());
        assert_eq!(
            block.terminator,
            LirTerminator::CondBr {
                condition: local(1),
                if_true: 2,
                if_false: 1,
            }
        );
    }

    #[test]
    fn simplifies_return_of_trailing_freeze() {
        let mut program = LirBlob {
            data_layout: layout(),
            functions: vec![test_function(vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![LirInstruction {
                    id: 3,
                    kind: LirInstructionKind::Freeze(bool_value(true)),
                    result: Some(result(3)),
                    debug_info: None,
                }],
                terminator: LirTerminator::Return(Some(register(3))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }])],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            comptime_entries: Vec::new(),
            queries: Vec::new(),
        };

        let changes = simplify_bool_conditions(&mut program).expect("bool simplify should work");

        assert_eq!(changes, 1);
        let block = &program.functions[0].basic_blocks[0];
        assert!(block.instructions.is_empty());
        assert_eq!(
            block.terminator,
            LirTerminator::Return(Some(bool_value(true)))
        );
    }
}
