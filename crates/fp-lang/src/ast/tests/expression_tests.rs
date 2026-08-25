use super::*;

#[test]
fn parse_expr_ast_handles_bench_report_struct_literal_shorthand() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("BenchReport { total, passed, failed, }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Struct(_)));
}

#[test]
fn parse_items_ast_handles_tail_struct_literal() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn read_insn(bytes: &[u8]) -> RawInsn {
                RawInsn {
                    code: bytes[0],
                    dst: bytes[1] & 0x0f,
                    src: (bytes[1] >> 4) & 0x0f,
                    off: i16::from_le_bytes([bytes[2], bytes[3]]),
                    imm: i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
                }
            }
            "#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_tail_struct_literal_after_impl() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            impl Runtime {
                fn run(&mut self) {
                    loop {
                        match instruction {
                            DecodedInstruction::Exit => break,
                        }
                    }
                }
            }

            fn read_insn(bytes: &[u8]) -> RawInsn {
                RawInsn {
                    code: bytes[0],
                    dst: bytes[1] & 0x0f,
                    src: (bytes[1] >> 4) & 0x0f,
                    off: i16::from_le_bytes([bytes[2], bytes[3]]),
                    imm: i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
                }
            }
            "#,
        )
        .unwrap();
    assert_eq!(items.len(), 2);
}

#[test]
fn parse_items_ast_handles_nested_fn_with_tuple_variant_match() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn outer() {
                fn rewrite_terminator(terminator: &mut AsmTerminator) {
                    match terminator {
                        AsmTerminator::Return(value) => {
                            rewrite_value(value);
                        }
                        AsmTerminator::CondBr { condition, .. } => rewrite_value(condition),
                        AsmTerminator::Resume(value)
                        | AsmTerminator::CleanupRet {
                            cleanup_pad: value, ..
                        }
                        | AsmTerminator::CatchRet {
                            catch_pad: value, ..
                        } => rewrite_value(value),
                        AsmTerminator::Br(..) | AsmTerminator::Unreachable => {}
                    }
                }
                fn mapped_x86_write_operand(operands: &[AsmOperand]) {
                    operands.iter().find_map(|operand| match operand {
                        AsmOperand::Register {
                            access: OperandAccess::Write | OperandAccess::ReadWrite,
                            ..
                        } => Some(operand),
                        _ => None,
                    });
                }
            }
            "#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_tuple_variant_negative_literal_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn std_handle(handle_code: Option<i64>) -> Result<Option<u64>> {
                let fd = match handle_code {
                    Some(-10) => 0u64,
                    Some(-11) => 1u64,
                    Some(-12) => 2u64,
                    _ => return Ok(None),
                };
                Ok(Some(fd))
            }
            "#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_nested_or_pattern_inside_tuple() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn default_abi_for_target(arch: &AsmArchitecture, format: &AsmObjectFormat) -> Option<Abi> {
                match (arch, format) {
                    (AsmArchitecture::X86_64, AsmObjectFormat::Coff | AsmObjectFormat::Pe) => {
                        Some(Abi::X86_64Win64)
                    }
                    (AsmArchitecture::X86_64, _) => Some(Abi::X86_64SysV),
                    (AsmArchitecture::Aarch64, _) => Some(Abi::Aarch64Aapcs64),
                    _ => None,
                }
            }
            "#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_expr_ast_handles_multiline_println_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"println(
                "  {} ... ok (iters: {}, time: {:.6}s, ns/iter: {:.2})",
                bench.name,
                measure_iters,
                elapsed,
                ns_per_iter
            )"#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_field_arg_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("catch_unwind(bench.run)").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_bench_run_body_prefix_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let benches: Vec<BenchCase> = REGISTRY;
            let mut passed = 0;
            let mut failed = 0;
            let mut idx = 0;
            while idx < benches.len() {
                let bench: BenchCase = benches[idx];
                let mut ok = true;
                let warmup_secs = 5.0f64;
                let measure_secs = 15.0f64;

                let warmup_start = std::time::now();
                let warmup_deadline = warmup_start + warmup_secs;
                let mut warmup_iters = 0;
                while std::time::now() < warmup_deadline {
                    let warm_ok = catch_unwind(bench.run);
                    if !warm_ok {
                        ok = false;
                        break;
                    }
                    warmup_iters = warmup_iters + 1;
                }

                let measure_start = std::time::now();
                let measure_deadline = measure_start + measure_secs;
                let mut measure_iters = 0;
                if ok {
                    while std::time::now() < measure_deadline || measure_iters == 0 {
                        let run_ok = catch_unwind(bench.run);
                        if !run_ok {
                            ok = false;
                            break;
                        }
                        measure_iters = measure_iters + 1;
                    }
                }
                idx = idx + 1;
            }
            passed
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_run_body_suffix_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let measure_end = std::time::now();
            let elapsed = measure_end - measure_start;
            if ok {
                passed = passed + 1;
                let iters_f = measure_iters as f64;
                let ns_per_iter = if iters_f > 0.0 {
                    (elapsed / iters_f) * 1000000000.0
                } else {
                    0.0
                };
                println(
                    "  {} ... ok (iters: {}, time: {:.6}s, ns/iter: {:.2})",
                    bench.name,
                    measure_iters,
                    elapsed,
                    ns_per_iter
                );
            } else {
                failed = failed + 1;
                println("  {} ... FAILED", bench.name);
            }
            let total = passed + failed;
            println(
                "bench result: {} passed; {} failed; {} total",
                passed,
                failed,
                total
            );
            BenchReport {
                total,
                passed,
                failed,
            }
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_outer_while_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let benches: Vec<BenchCase> = REGISTRY;
            let mut idx = 0;
            while idx < benches.len() {
                let bench: BenchCase = benches[idx];
                let mut ok = true;
                let warmup_secs = 5.0f64;
                let measure_secs = 15.0f64;
                idx = idx + 1;
            }
            idx
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_warmup_loop_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let warmup_start = std::time::now();
            let warmup_deadline = warmup_start + warmup_secs;
            let mut warmup_iters = 0;
            while std::time::now() < warmup_deadline {
                let warm_ok = catch_unwind(bench.run);
                if !warm_ok {
                    ok = false;
                    break;
                }
                warmup_iters = warmup_iters + 1;
            }
            warmup_iters
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_measure_loop_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let measure_start = std::time::now();
            let measure_deadline = measure_start + measure_secs;
            let mut measure_iters = 0;
            if ok {
                while std::time::now() < measure_deadline || measure_iters == 0 {
                    let run_ok = catch_unwind(bench.run);
                    if !run_ok {
                        ok = false;
                        break;
                    }
                    measure_iters = measure_iters + 1;
                }
            }
            measure_iters
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_typed_generic_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let benches: Vec<BenchCase> = REGISTRY; benches }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_typed_index_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let bench: BenchCase = benches[idx]; bench }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_outer_while_minimal_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ let benches: Vec<BenchCase> = REGISTRY; let mut idx = 0; while idx < benches.len() { idx = idx + 1; } idx }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_len_call_in_comparison() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("idx < benches.len()").unwrap();
    assert!(matches!(expr.kind(), ExprKind::BinOp(_)));
}

#[test]
fn parse_expr_ast_handles_nonfinal_while_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ while idx < benches.len() { idx = idx + 1; } idx }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_match_with_path_tuple_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match self { Result::Ok(_) => true, Result::Err(_) => false }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_match_guard_with_ref_tuple_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "match iter.peek() { Some(&(idx, ch)) if ch.is_ascii_digit() => idx, _ => 0 }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_wildcard_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ let _ = self; _ }");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_uninitialized_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ let mut end; end }").unwrap();
    let ExprKind::Block(block) = expr.kind() else {
        panic!("expected block expr");
    };
    let Some(BlockStmt::Let(stmt)) = block.stmts.first() else {
        panic!("expected let stmt");
    };
    assert!(stmt.init.is_none());
}

#[test]
fn parse_expr_ast_handles_unit_literal() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("()");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_result_ok_unit_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("std::result::Result::Ok(())");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_reference_prefix() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("&self.inner");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_array_literal_call_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("first_gt([1, 2, 5], [0, 1, 3])");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_try_operator_on_identifier() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("x?").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Try(_)));
}

#[test]
fn parse_expr_ast_operator_precedence_smoke() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("1 + 2 * 3").unwrap();
    match expr.kind() {
        ExprKind::BinOp(op) => {
            assert_eq!(op.kind, BinOpKind::Add);
        }
        other => panic!("expected binop, got {:?}", other),
    }
}

#[test]
fn direct_parser_handles_basic_call_chain() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("foo.bar(1)[0]?").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Try(_)));
}

#[test]
fn direct_parser_handles_cast_and_await() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("await foo as i64 + 1").unwrap();
    let ExprKind::BinOp(bin) = expr.kind() else {
        panic!("expected binop");
    };
    assert!(matches!(bin.kind, BinOpKind::Add));
}
