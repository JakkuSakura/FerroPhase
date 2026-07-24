#[cfg(test)]
mod tests {
    use fp_bytecode::{
        BytecodeBinOp, BytecodeBlock, BytecodeCallee, BytecodeConst, BytecodeFunction,
        BytecodeInstr, BytecodePlace, BytecodeProgram, BytecodeTerminator,
    };
    use fp_core::ast::Value;
    use fp_core::lir::LirTerminator;
    use fp_interpret::LirInterpreter;

    use crate::lowering::lower_program;

    #[test]
    fn lowers_simple_arithmetic() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Add),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        assert_eq!(lir.functions.len(), 1);
        let main = &lir.functions[0];
        assert_eq!(main.name.as_str(), "main");
        assert_eq!(main.basic_blocks.len(), 1);
        let block = &main.basic_blocks[0];
        assert!(!block.instructions.is_empty());
        assert!(matches!(block.terminator, LirTerminator::Return(_)));
    }

    #[test]
    fn lowers_control_flow() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![
                    BytecodeBlock {
                        id: 0,
                        code: vec![BytecodeInstr::LoadConst(0)],
                        terminator: BytecodeTerminator::JumpIfTrue {
                            target: 1,
                            otherwise: 2,
                        },
                    },
                    BytecodeBlock {
                        id: 1,
                        code: vec![],
                        terminator: BytecodeTerminator::Jump { target: 2 },
                    },
                    BytecodeBlock {
                        id: 2,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    },
                ],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let main = &lir.functions[0];
        assert_eq!(main.basic_blocks.len(), 3);

        let bb0 = main.get_basic_block(0).unwrap();
        assert!(matches!(bb0.terminator, LirTerminator::CondBr { .. }));

        let bb1 = main.get_basic_block(1).unwrap();
        assert!(matches!(bb1.terminator, LirTerminator::Br(2)));
        assert_eq!(bb1.predecessors, vec![0]);
        assert_eq!(bb1.successors, vec![2]);
    }

    #[test]
    fn lowers_function_call() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![
                BytecodeFunction {
                    name: "helper".to_string(),
                    params: 0,
                    locals: 1,
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    }],
                },
                BytecodeFunction {
                    name: "main".to_string(),
                    params: 0,
                    locals: 1,
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![],
                        terminator: BytecodeTerminator::Call {
                            callee: BytecodeCallee::Function("helper".into()),
                            arg_count: 0,
                            destination: Some(BytecodePlace {
                                local: 0,
                                projection: vec![],
                            }),
                            target: 1,
                        },
                    }],
                },
            ],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        assert_eq!(lir.functions.len(), 2);
    }

    #[test]
    fn end_to_end_arithmetic() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Add),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("interpretation should succeed");
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn end_to_end_control_flow() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![
                    BytecodeBlock {
                        id: 0,
                        code: vec![BytecodeInstr::LoadConst(0)],
                        terminator: BytecodeTerminator::JumpIfTrue {
                            target: 1,
                            otherwise: 0,
                        },
                    },
                    BytecodeBlock {
                        id: 1,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    },
                ],
            }],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("interpretation should succeed");
        assert_eq!(result, Value::int(0));
    }
}
