#[cfg(test)]
mod tests {
    use fp_bytecode::{
        BytecodeBinOp, BytecodeBlock, BytecodeCallee, BytecodeConst, BytecodeFunction,
        BytecodeInstr, BytecodePlace, BytecodeProgram, BytecodeTerminator,
    };
    use fp_core::ast::Value;
    use fp_core::intrinsics::IntrinsicKind;
    use fp_core::lir::LirTerminator;
    use fp_interpret::LirInterpreter;

    use crate::lowering::lower_program;

    #[test]
    fn lowers_simple_arithmetic() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
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
    fn rejects_missing_branch_target() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![],
                    terminator: BytecodeTerminator::Jump { target: 99 },
                }],
            }],
            entry: Some("main".into()),
        };

        let error = lower_program(&program).expect_err("invalid target must be rejected");
        assert!(error.to_string().contains("missing block 99"));
    }

    #[test]
    fn rejects_duplicate_block_ids() {
        let block = || BytecodeBlock {
            id: 0,
            code: vec![],
            terminator: BytecodeTerminator::Return,
        };
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
                blocks: vec![block(), block()],
            }],
            entry: Some("main".into()),
        };

        let error = lower_program(&program).expect_err("duplicate IDs must be rejected");
        assert!(error.to_string().contains("duplicate basic-block IDs"));
    }

    #[test]
    fn rejects_mismatched_switch_tables() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![],
                    terminator: BytecodeTerminator::SwitchInt {
                        values: vec![1, 2],
                        targets: vec![0],
                        otherwise: 0,
                    },
                }],
            }],
            entry: Some("main".into()),
        };

        let error = lower_program(&program).expect_err("mismatched switch tables must fail");
        assert!(error.to_string().contains("switch values but 1 targets"));
    }

    #[test]
    fn rejects_out_of_range_bytecode_references() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![BytecodeInstr::LoadConst(4)],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let error = lower_program(&program).expect_err("invalid constant must fail");
        assert!(error.to_string().contains("missing constant 4"));
    }

    #[test]
    fn lowers_control_flow() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I1],
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
                    param_types: vec![],
                    return_type: fp_core::lir::LirType::I64,
                    local_types: vec![fp_core::lir::LirType::I64],
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![],
                        terminator: BytecodeTerminator::Return,
                    }],
                },
                BytecodeFunction {
                    name: "main".to_string(),
                    param_types: vec![],
                    return_type: fp_core::lir::LirType::I64,
                    local_types: vec![fp_core::lir::LirType::I64],
                    blocks: vec![
                        BytecodeBlock {
                            id: 0,
                            code: vec![],
                            terminator: BytecodeTerminator::Call {
                                callee: BytecodeCallee::Function("helper".into()),
                                arg_count: 0,
                                destination: Some(BytecodePlace {
                                    local: 0,
                                    projection: vec![],
                                }),
                                result_type: fp_core::lir::LirType::I64,
                                target: 1,
                            },
                        },
                        BytecodeBlock {
                            id: 1,
                            code: vec![],
                            terminator: BytecodeTerminator::Return,
                        },
                    ],
                },
            ],
            entry: Some("main".to_string()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        assert_eq!(lir.functions.len(), 2);
    }

    #[test]
    fn lowers_string_constant_with_contents() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str("hello".into())],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8)),
                local_types: vec![fp_core::lir::LirType::Ptr(Box::new(
                    fp_core::lir::LirType::I8,
                ))],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![BytecodeInstr::LoadConst(0), BytecodeInstr::StoreLocal(0)],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let instructions = &lir.functions[0].basic_blocks[0].instructions;
        let call = instructions.iter().find(|instruction| {
            matches!(
                &instruction.kind,
                fp_core::lir::LirInstructionKind::Call { function, .. }
                    if matches!(
                        &function.kind,
                        fp_core::lir::LirValueKind::Function(
                            fp_core::lir::LirFunctionRef::Name(name)
                        ) if name.as_str() == "__bc_str_const"
                    )
            )
        });
        let Some(call) = call else {
            panic!("string constant did not lower to __bc_str_const");
        };
        let fp_core::lir::LirInstructionKind::Call { args, .. } = &call.kind else {
            unreachable!();
        };
        assert_eq!(args.len(), 5);
    }

    #[test]
    fn end_to_end_arithmetic() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
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
    fn end_to_end_array_projection_and_store() {
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Int(10),
                BytecodeConst::Int(20),
                BytecodeConst::Int(1),
                BytecodeConst::Int(99),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![
                    fp_core::lir::LirType::I64,
                    fp_core::lir::LirType::I64,
                    fp_core::lir::LirType::I64,
                ],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::MakeArray(2),
                        BytecodeInstr::StoreLocal(0),
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadPlace(BytecodePlace {
                            local: 0,
                            projection: vec![fp_bytecode::BytecodePlaceElem::Index(1)],
                        }),
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadConst(3),
                        BytecodeInstr::StorePlace(BytecodePlace {
                            local: 0,
                            projection: vec![fp_bytecode::BytecodePlaceElem::Index(1)],
                        }),
                        BytecodeInstr::LoadPlace(BytecodePlace {
                            local: 0,
                            projection: vec![fp_bytecode::BytecodePlaceElem::Index(1)],
                        }),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("interpretation should succeed");
        assert_eq!(result, Value::int(99));
    }

    #[test]
    fn end_to_end_function_value_indirect_call() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Int(42),
                BytecodeConst::Function("helper".into()),
            ],
            functions: vec![
                BytecodeFunction {
                    name: "helper".into(),
                    param_types: vec![],
                    return_type: i64_ty.clone(),
                    local_types: vec![i64_ty.clone()],
                    blocks: vec![BytecodeBlock {
                        id: 0,
                        code: vec![BytecodeInstr::LoadConst(0), BytecodeInstr::StoreLocal(0)],
                        terminator: BytecodeTerminator::Return,
                    }],
                },
                BytecodeFunction {
                    name: "main".into(),
                    param_types: vec![],
                    return_type: i64_ty.clone(),
                    local_types: vec![i64_ty.clone(), ptr_ty],
                    blocks: vec![
                        BytecodeBlock {
                            id: 0,
                            code: vec![BytecodeInstr::LoadConst(1), BytecodeInstr::StoreLocal(1)],
                            terminator: BytecodeTerminator::Call {
                                callee: BytecodeCallee::Local(BytecodePlace {
                                    local: 1,
                                    projection: vec![],
                                }),
                                arg_count: 0,
                                destination: Some(BytecodePlace {
                                    local: 0,
                                    projection: vec![],
                                }),
                                result_type: i64_ty,
                                target: 1,
                            },
                        },
                        BytecodeBlock {
                            id: 1,
                            code: vec![],
                            terminator: BytecodeTerminator::Return,
                        },
                    ],
                },
            ],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("indirect interpretation should succeed");
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn end_to_end_map_lookup() {
        let i64_ty = fp_core::lir::LirType::I64;
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(7), BytecodeConst::Int(42)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone(), i64_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::MakeMap(1),
                        BytecodeInstr::StoreLocal(0),
                        BytecodeInstr::LoadLocal(0),
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::ContainerGet,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("map lookup should succeed");
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn end_to_end_string_slice() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Str("hello".into()),
                BytecodeConst::Int(1),
                BytecodeConst::Int(4),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone(), ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::Slice,
                            arg_count: 3,
                            format: None,
                            result_type: ptr_ty,
                        },
                        BytecodeInstr::ContainerLen,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("slice lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("slice interpretation should succeed");
        assert_eq!(result, Value::int(3));
    }

    #[test]
    fn end_to_end_fs_exists() {
        let i1_ty = fp_core::lir::LirType::I1;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str(".".into())],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i1_ty.clone(),
                local_types: vec![i1_ty, ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FsExists,
                            arg_count: 1,
                            format: None,
                            result_type: fp_core::lir::LirType::I1,
                        },
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("filesystem predicate lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("filesystem predicate interpretation should succeed");
        assert_eq!(result, Value::bool(true));
    }

    #[test]
    fn end_to_end_variadic_path_join() {
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Str("root".into()),
                BytecodeConst::Str("child".into()),
                BytecodeConst::Str("file.txt".into()),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: ptr_ty.clone(),
                local_types: vec![
                    ptr_ty.clone(),
                    ptr_ty.clone(),
                    ptr_ty.clone(),
                    ptr_ty.clone(),
                ],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::StoreLocal(3),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadLocal(2),
                        BytecodeInstr::LoadLocal(3),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::PathJoin,
                            arg_count: 3,
                            format: None,
                            result_type: ptr_ty.clone(),
                        },
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("path join lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("path join interpretation should succeed");
        assert!(matches!(result, Value::Pointer(_)));
    }

    #[test]
    fn end_to_end_relative_glob_match() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str("Cargo.toml".into())],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty, ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FsGlob,
                            arg_count: 1,
                            format: None,
                            result_type: ptr_ty,
                        },
                        BytecodeInstr::ContainerLen,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("glob lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("glob interpretation should succeed");
        assert!(matches!(result, Value::Int(value) if value.value >= 1));
    }

    #[test]
    fn debug_assertions_raise_runtime_error() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(false)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::DebugAssertions,
                            arg_count: 1,
                            format: None,
                            result_type: fp_core::lir::LirType::Void,
                        },
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("assertion lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let error = interpreter
            .run_main(&lir)
            .expect_err("false debug assertion must fail");
        assert!(
            error.to_string().contains("debug assertion failed"),
            "unexpected assertion error: {error}"
        );
    }

    #[test]
    fn end_to_end_loop_carried_stack_value() {
        let i64_ty = fp_core::lir::LirType::I64;
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(7), BytecodeConst::Bool(false)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone(), fp_core::lir::LirType::I1],
                blocks: vec![
                    BytecodeBlock {
                        id: 0,
                        code: vec![
                            BytecodeInstr::LoadConst(0),
                            BytecodeInstr::LoadConst(1),
                            BytecodeInstr::StoreLocal(1),
                        ],
                        terminator: BytecodeTerminator::Jump { target: 1 },
                    },
                    BytecodeBlock {
                        id: 1,
                        code: vec![BytecodeInstr::LoadLocal(1)],
                        terminator: BytecodeTerminator::JumpIfTrue {
                            target: 3,
                            otherwise: 2,
                        },
                    },
                    BytecodeBlock {
                        id: 3,
                        code: vec![BytecodeInstr::LoadConst(1), BytecodeInstr::StoreLocal(1)],
                        terminator: BytecodeTerminator::Jump { target: 1 },
                    },
                    BytecodeBlock {
                        id: 2,
                        code: vec![BytecodeInstr::StoreLocal(0)],
                        terminator: BytecodeTerminator::Return,
                    },
                ],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("loop stack lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("loop stack interpretation should succeed");
        assert_eq!(result, Value::int(7));
    }

    #[test]
    fn end_to_end_fs_read_to_string() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str("Cargo.toml".into())],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty, ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FsReadToString,
                            arg_count: 1,
                            format: None,
                            result_type: ptr_ty,
                        },
                        BytecodeInstr::ContainerLen,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("file read lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("file read interpretation should succeed");
        assert!(matches!(result, Value::Int(value) if value.value > 0));
    }

    #[test]
    fn end_to_end_type_name() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(42)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty, ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::TypeName,
                            arg_count: 1,
                            format: None,
                            result_type: ptr_ty,
                        },
                        BytecodeInstr::ContainerLen,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("type name lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("type name interpretation should succeed");
        assert_eq!(result, Value::int(3));
    }

    #[test]
    fn end_to_end_size_of_uses_lir_type() {
        let i64_ty = fp_core::lir::LirType::I64;
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(42)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::SizeOf,
                            arg_count: 1,
                            format: None,
                            result_type: i64_ty,
                        },
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("size_of lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("size_of interpretation should succeed");
        assert_eq!(result, Value::int(8));
    }

    #[test]
    fn end_to_end_dynamic_struct_reflection() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str("answer".into()), BytecodeConst::Int(42)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone(), ptr_ty.clone(), ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::CreateStruct,
                            arg_count: 2,
                            format: None,
                            result_type: ptr_ty.clone(),
                        },
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadLocal(2),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FieldCount,
                            arg_count: 1,
                            format: None,
                            result_type: i64_ty.clone(),
                        },
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("struct reflection lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("struct reflection interpretation should succeed");
        assert_eq!(result, Value::int(1));
    }

    #[test]
    fn end_to_end_dynamic_struct_field_access() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Str("answer".into()), BytecodeConst::Int(42)],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty.clone(), ptr_ty.clone(), ptr_ty.clone()],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::CreateStruct,
                            arg_count: 2,
                            format: None,
                            result_type: ptr_ty.clone(),
                        },
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadLocal(2),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::ContainerGet,
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("field access lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("field access interpretation should succeed");
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn end_to_end_fs_write_string() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let path =
            std::env::temp_dir().join(format!("fp-stackvm-write-test-{}", std::process::id()));
        let _ = std::fs::remove_file(&path);
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Str(path.to_string_lossy().into_owned()),
                BytecodeConst::Str("stackvm write".into()),
                BytecodeConst::Int(1),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty, ptr_ty.clone(), ptr_ty],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadLocal(2),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FsWriteString,
                            arg_count: 2,
                            format: None,
                            result_type: fp_core::lir::LirType::Void,
                        },
                        BytecodeInstr::Pop,
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("file write lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("file write interpretation should succeed");
        assert_eq!(result, Value::int(1));
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "stackvm write");
        std::fs::remove_file(path).expect("test file cleanup should succeed");
    }

    #[test]
    fn end_to_end_fs_append_string() {
        let i64_ty = fp_core::lir::LirType::I64;
        let ptr_ty = fp_core::lir::LirType::Ptr(Box::new(fp_core::lir::LirType::I8));
        let path =
            std::env::temp_dir().join(format!("fp-stackvm-append-test-{}", std::process::id()));
        std::fs::write(&path, "a").expect("test file setup should succeed");
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Str(path.to_string_lossy().into_owned()),
                BytecodeConst::Str("b".into()),
                BytecodeConst::Int(1),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: i64_ty.clone(),
                local_types: vec![i64_ty, ptr_ty.clone(), ptr_ty],
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::StoreLocal(1),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::StoreLocal(2),
                        BytecodeInstr::LoadLocal(1),
                        BytecodeInstr::LoadLocal(2),
                        BytecodeInstr::IntrinsicCall {
                            kind: IntrinsicKind::FsAppendString,
                            arg_count: 2,
                            format: None,
                            result_type: fp_core::lir::LirType::Void,
                        },
                        BytecodeInstr::Pop,
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("file append lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("file append interpretation should succeed");
        assert_eq!(result, Value::int(1));
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "ab");
        std::fs::remove_file(path).expect("test file cleanup should succeed");
    }

    #[test]
    fn end_to_end_control_flow() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
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

    #[test]
    fn end_to_end_forward_stack_join() {
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::Bool(false),
                BytecodeConst::Int(11),
                BytecodeConst::Int(22),
            ],
            functions: vec![BytecodeFunction {
                name: "main".into(),
                param_types: vec![],
                return_type: fp_core::lir::LirType::I64,
                local_types: vec![fp_core::lir::LirType::I64],
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
                        id: 2,
                        code: vec![BytecodeInstr::LoadConst(2)],
                        terminator: BytecodeTerminator::Jump { target: 3 },
                    },
                    BytecodeBlock {
                        id: 1,
                        code: vec![BytecodeInstr::LoadConst(1)],
                        terminator: BytecodeTerminator::Jump { target: 3 },
                    },
                    BytecodeBlock {
                        id: 3,
                        code: vec![BytecodeInstr::StoreLocal(0)],
                        terminator: BytecodeTerminator::Return,
                    },
                ],
            }],
            entry: Some("main".into()),
        };

        let lir = lower_program(&program).expect("stack join lowering should succeed");
        let mut interpreter = LirInterpreter::new();
        let result = interpreter
            .run_main(&lir)
            .expect("stack join interpretation should succeed");
        assert_eq!(result, Value::int(22));
    }
}
