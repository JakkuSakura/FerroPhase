use std::path::PathBuf;
use std::thread;

use fp_cli::languages;
use fp_cli::pipeline::{BackendKind, Pipeline, PipelineOptions};
use fp_core::{hir, lir};

fn examples_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../..")
        .join("examples")
}

fn hir_contains_query_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Query(_) => true,
        hir::ExprKind::Binary(_, lhs, rhs) | hir::ExprKind::Assign(lhs, rhs) => {
            hir_contains_query_expr(lhs) || hir_contains_query_expr(rhs)
        }
        hir::ExprKind::Unary(_, value)
        | hir::ExprKind::FieldAccess(value, _)
        | hir::ExprKind::Cast(value, _)
        | hir::ExprKind::Return(Some(value))
        | hir::ExprKind::Break(Some(value)) => hir_contains_query_expr(value),
        hir::ExprKind::Reference(reference) => hir_contains_query_expr(&reference.expr),
        hir::ExprKind::Call(callee, args) => {
            hir_contains_query_expr(callee)
                || args.iter().any(|arg| hir_contains_query_expr(&arg.value))
        }
        hir::ExprKind::MethodCall(receiver, _, args) => {
            hir_contains_query_expr(receiver)
                || args.iter().any(|arg| hir_contains_query_expr(&arg.value))
        }
        hir::ExprKind::Index(base, index) => {
            hir_contains_query_expr(base) || hir_contains_query_expr(index)
        }
        hir::ExprKind::Slice(slice) => {
            hir_contains_query_expr(&slice.base)
                || slice
                    .start
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
                || slice
                    .end
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
        }
        hir::ExprKind::Struct(_, fields) => fields
            .iter()
            .any(|field| hir_contains_query_expr(&field.expr)),
        hir::ExprKind::If(cond, then_branch, else_branch) => {
            hir_contains_query_expr(cond)
                || hir_contains_query_expr(then_branch)
                || else_branch
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            hir_contains_query_expr(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(hir_contains_query_expr)
                        || hir_contains_query_expr(&arm.body)
                })
        }
        hir::ExprKind::Try(expr_try) => {
            hir_contains_query_expr(&expr_try.expr)
                || expr_try
                    .catches
                    .iter()
                    .any(|catch| hir_contains_query_expr(&catch.body))
                || expr_try
                    .elze
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
                || expr_try
                    .finally
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            block.stmts.iter().any(|stmt| match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    local.init.as_ref().is_some_and(hir_contains_query_expr)
                }
                hir::StmtKind::Item(_) => false,
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    hir_contains_query_expr(expr)
                }
            }) || block
                .expr
                .as_ref()
                .is_some_and(|expr| hir_contains_query_expr(expr))
        }
        hir::ExprKind::While(cond, block) => {
            hir_contains_query_expr(cond)
                || block.stmts.iter().any(|stmt| match &stmt.kind {
                    hir::StmtKind::Local(local) => {
                        local.init.as_ref().is_some_and(hir_contains_query_expr)
                    }
                    hir::StmtKind::Item(_) => false,
                    hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                        hir_contains_query_expr(expr)
                    }
                })
                || block
                    .expr
                    .as_ref()
                    .is_some_and(|expr| hir_contains_query_expr(expr))
        }
        hir::ExprKind::With(context, body) => {
            hir_contains_query_expr(context) || hir_contains_query_expr(body)
        }
        hir::ExprKind::IntrinsicCall(call) => call
            .callargs
            .iter()
            .any(|arg| hir_contains_query_expr(&arg.value)),
        hir::ExprKind::Let(_, _, init) => init
            .as_ref()
            .is_some_and(|expr| hir_contains_query_expr(expr)),
        hir::ExprKind::Array(elements) => elements.iter().any(hir_contains_query_expr),
        hir::ExprKind::ArrayRepeat { elem, len } => {
            hir_contains_query_expr(elem) || hir_contains_query_expr(len)
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::FormatString(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Return(None)
        | hir::ExprKind::Break(None) => false,
    }
}

fn hir_program_contains_query_expr(program: &hir::Program) -> bool {
    program.items.iter().any(hir_item_contains_query_expr)
}

fn hir_item_contains_query_expr(item: &hir::Item) -> bool {
    match &item.kind {
        hir::ItemKind::Function(function) => function
            .body
            .as_ref()
            .is_some_and(|body| hir_contains_query_expr(&body.value)),
        hir::ItemKind::Expr(expr) => hir_contains_query_expr(expr),
        hir::ItemKind::Impl(impl_block) => impl_block.items.iter().any(|impl_item| {
            matches!(
                &impl_item.kind,
                hir::ImplItemKind::Method(function)
                    if function
                        .body
                        .as_ref()
                        .is_some_and(|body| hir_contains_query_expr(&body.value))
            )
        }),
        _ => false,
    }
}

fn lir_program_contains_exec_query(program: &lir::LirProgram) -> bool {
    program.functions.iter().any(|function| {
        function.basic_blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(instruction.kind, lir::LirInstructionKind::ExecQuery(_))
            })
        })
    })
}

fn assert_query_bundle(name: &'static str, expected_language: &'static str) {
    thread::Builder::new()
        .name(format!("fp-cli-mir-{name}"))
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let path = examples_root().join(name);
            let mut pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = BackendKind::Bytecode;
            let bundle = pipeline
                .compile_file_to_lir(path.as_path(), options)
                .expect("compile example to lir");
            assert_eq!(bundle.frontend.source_language, expected_language);
            assert!(
                !bundle.lir_program.queries.is_empty(),
                "expected query item in LIR for {name}"
            );
        })
        .expect("spawn compile thread")
        .join()
        .expect("join compile thread");
}

fn assert_host_query_bundle(name: &'static str) {
    thread::Builder::new()
        .name(format!("fp-cli-host-lir-{name}"))
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let path = examples_root().join(name);
            let mut pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = BackendKind::Bytecode;
            let bundle = pipeline
                .compile_file_to_lir(path.as_path(), options)
                .expect("compile host example to lir");
            assert_eq!(bundle.frontend.source_language, languages::FERROPHASE);
            assert!(
                bundle
                    .hir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, hir::ItemKind::Function(_))),
                "expected function item in HIR for {name}"
            );
            assert!(
                hir_program_contains_query_expr(&bundle.hir_program),
                "expected query expr in HIR for {name}"
            );
            assert!(
                bundle
                    .lir_program
                    .functions
                    .iter()
                    .any(|function| !function.is_declaration),
                "expected function item in LIR for {name}"
            );
            assert!(
                lir_program_contains_exec_query(&bundle.lir_program),
                "expected exec-query instruction in LIR for {name}"
            );
        })
        .expect("spawn compile thread")
        .join()
        .expect("join compile thread");
}

#[test]
fn compiles_fp_example_to_mir_bundle() {
    assert_query_bundle("query.fp", languages::FERROPHASE);
}

#[test]
fn compiles_sql_example_to_mir_bundle() {
    assert_query_bundle("query.sql", languages::SQL);
}

#[test]
fn compiles_prql_example_to_mir_bundle() {
    assert_query_bundle("query.prql", languages::PRQL);
}

#[test]
fn compiles_host_query_main_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_main.fp");
}

#[test]
fn compiles_host_query_functions_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_functions.fp");
}

#[test]
fn compiles_host_query_struct_methods_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_struct_methods.fp");
}
