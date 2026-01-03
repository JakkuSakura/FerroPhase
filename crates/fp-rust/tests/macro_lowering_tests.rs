use std::sync::Arc;

use eyre::Result;
use fp_core::ast::*;
use fp_core::ast::{ExprKind as AstExprKind, Node, NodeKind};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_rust::normalization::lower_macro_for_ast;
use fp_rust::printer::RustPrinter;
use fp_rust::shll_parse_expr;

fn normalize_expr_tree(expr: fp_core::ast::Expr) -> Result<fp_core::ast::Expr> {
    // First lower the top-level macro if present (shortcut).
    let expr = match expr.kind() {
        AstExprKind::Macro(mac) => lower_macro_for_ast(mac, None),
        _ => expr,
    };
    // Then run the shared intrinsic normalization pass over a Node wrapper to recursively
    // lower nested macros and normalize intrinsic forms.
    let mut node = Node::new(NodeKind::Expr(expr.clone()));
    fp_optimize::transformations::normalize_intrinsics_with(
        &mut node,
        &fp_rust::normalization::RustIntrinsicNormalizer,
    )?;
    match node.kind() {
        NodeKind::Expr(e) => Ok(e.clone()),
        _ => Ok(expr),
    }
}

#[test]
fn emit_macro_lowers_to_splice_of_quote_without_kind() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { emit! { let x = 1; } })?;
    let ExprKind::Splice(splice) = expr.kind() else {
        panic!("emit! should lower to a splice expression");
    };
    let ExprKind::Quote(q) = splice.token.kind() else {
        panic!("emit! splice should contain a quote token");
    };
    assert!(
        q.kind.is_none(),
        "quote kind should be None after refactor to quote-only"
    );
    Ok(())
}

fn expect_println_template<'a>(stmt: &'a BlockStmt, expected_prefix: &str) -> &'a ExprFormatString {
    let BlockStmt::Expr(expr_stmt) = stmt else {
        panic!("expected expression statement, found {:?}", stmt);
    };
    let expr = expr_stmt.expr.as_ref();
    let ExprKind::IntrinsicCall(call) = expr.kind() else {
        panic!("expected intrinsic call, found {:?}", expr.kind());
    };
    assert_eq!(
        call.kind,
        IntrinsicCallKind::Println,
        "expected println! intrinsic"
    );
    let IntrinsicCallPayload::Format { template } = &call.payload else {
        panic!("expected println! to use format payload");
    };
    assert!(
        matches!(
            template.parts.first(),
            Some(FormatTemplatePart::Literal(text)) if text.starts_with(expected_prefix)
        ),
        "unexpected first literal in println!: {:?}",
        template.parts
    );
    assert!(
        template.kwargs.is_empty(),
        "println! lowering should not produce keyword args"
    );
    template
}

fn expect_abort(stmt: &BlockStmt) {
    let BlockStmt::Expr(expr_stmt) = stmt else {
        panic!("expected expression statement, found {:?}", stmt);
    };
    let expr = expr_stmt.expr.as_ref();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!(
            "expected std::process::abort() invoke, found {:?}",
            expr.kind()
        );
    };
    assert!(
        invoke.args.is_empty(),
        "abort call should not contain arguments"
    );
    let ExprInvokeTarget::Function(locator) = &invoke.target else {
        panic!("abort call should resolve to function target");
    };
    assert_eq!(
        locator.to_string(),
        "std::process::abort",
        "unexpected abort target"
    );
}

fn expect_assert_failure_block<'a>(
    block: &'a ExprBlock,
    expected_prefix: &str,
) -> &'a ExprFormatString {
    let stmts = &block.stmts;
    assert_eq!(
        stmts.len(),
        2,
        "expected failure path to contain println! + abort"
    );
    let template = expect_println_template(&stmts[0], expected_prefix);
    expect_abort(&stmts[1]);
    template
}

#[test]
fn panic_macro_lowering_emits_message_and_abort() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { panic!() })?;

    let ExprKind::Block(block) = expr.kind() else {
        panic!("panic! should lower to a block expression");
    };
    assert_eq!(block.stmts.len(), 2);
    let template = expect_println_template(&block.stmts[0], "panic! macro triggered");
    assert_eq!(
        template.args.len(),
        0,
        "panic! without message should not forward arguments"
    );
    assert_eq!(
        template.parts.len(),
        1,
        "panic! default template should be a single literal"
    );
    expect_abort(&block.stmts[1]);
    Ok(())
}

#[test]
fn assert_macro_uses_default_message() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { assert!(1 == 2); })?;

    let ExprKind::Block(block) = expr.kind() else {
        panic!("assert! lowering should produce a block");
    };
    assert_eq!(
        block.stmts.len(),
        1,
        "assert! should produce single if stmt"
    );

    let BlockStmt::Expr(stmt) = &block.stmts[0] else {
        panic!("expected assert! lowering to yield expression statement");
    };
    let ExprKind::If(if_expr) = stmt.expr.kind() else {
        panic!("assert! lowering should produce an if expression");
    };

    // Condition should negate the assertion predicate.
    let ExprKind::UnOp(un_op) = if_expr.cond.as_ref().kind() else {
        panic!("expected assertion condition to lower via logical negation");
    };
    assert_eq!(un_op.op, UnOpKind::Not);

    let ExprKind::Block(failure_block) = if_expr.then.as_ref().kind() else {
        panic!("assert! failure path should stay as a block");
    };
    let template = expect_println_template(&failure_block.stmts[0], "assertion failed");
    assert_eq!(
        template.args.len(),
        0,
        "plain assert! should not forward interpolation arguments"
    );
    expect_abort(&failure_block.stmts[1]);

    Ok(())
}

#[test]
fn assert_eq_lowering_binds_operands_once() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { assert_eq!(foo(), bar()); })?;

    let ExprKind::Block(block) = expr.kind() else {
        panic!("assert_eq! lowering should produce a block");
    };
    assert_eq!(
        block.stmts.len(),
        3,
        "assert_eq! should produce operand bindings and guard"
    );

    let first_binding = match &block.stmts[0] {
        BlockStmt::Let(stmt) => stmt,
        other => panic!("expected first stmt to be let-binding, found {:?}", other),
    };
    let first_ident = first_binding
        .pat
        .as_ident()
        .expect("expected pattern to be identifier");
    assert!(
        first_ident.as_str().starts_with("__fp_assert_left_"),
        "unexpected left binding name {first_ident}"
    );

    let second_binding = match &block.stmts[1] {
        BlockStmt::Let(stmt) => stmt,
        other => panic!("expected second stmt to be let-binding, found {:?}", other),
    };
    let second_ident = second_binding
        .pat
        .as_ident()
        .expect("expected pattern to be identifier");
    assert!(
        second_ident.as_str().starts_with("__fp_assert_right_"),
        "unexpected right binding name {second_ident}"
    );

    let BlockStmt::Expr(if_stmt) = &block.stmts[2] else {
        panic!("expected assert_eq! guard to be an if statement");
    };
    let ExprKind::If(if_expr) = if_stmt.expr.kind() else {
        panic!("expected if guard for assert_eq!");
    };
    let ExprKind::BinOp(bin_op) = if_expr.cond.as_ref().kind() else {
        panic!("assert_eq! guard should be a binary op");
    };
    assert_eq!(
        bin_op.kind,
        BinOpKind::Ne,
        "assert_eq! should branch when operands differ"
    );

    let ExprKind::Block(failure_block) = if_expr.then.as_ref().kind() else {
        panic!("expected assert_eq! failure block");
    };
    let template = expect_assert_failure_block(failure_block, "assertion failed");
    assert_eq!(
        template.args.len(),
        2,
        "assert_eq! should forward operands for formatting"
    );

    Ok(())
}

#[test]
fn assert_with_side_effect_arguments_preserves_order() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { assert!(1 == 2, log_error()); })?;

    let ExprKind::Block(block) = expr.kind() else {
        panic!("assert! lowering should produce a block");
    };
    assert_eq!(
        block.stmts.len(),
        1,
        "assert! with extra message should remain a single if statement"
    );
    let BlockStmt::Expr(if_stmt) = &block.stmts[0] else {
        panic!("expected expression statement from assert! lowering");
    };
    let ExprKind::If(if_expr) = if_stmt.expr.kind() else {
        panic!("expected if guard for assert!");
    };
    let ExprKind::Block(failure_block) = if_expr.then.as_ref().kind() else {
        panic!("expected failure arm to remain block");
    };

    let stmts = &failure_block.stmts;
    assert_eq!(
        stmts.len(),
        3,
        "side-effecting message should produce evaluation + println! + abort"
    );

    // First statement executes the user-provided expression.
    let BlockStmt::Expr(side_effect_stmt) = &stmts[0] else {
        panic!("expected side-effect expression statement");
    };
    let ExprKind::Invoke(side_effect_call) = side_effect_stmt.expr.kind() else {
        panic!("expected log_error() invocation");
    };
    let ExprInvokeTarget::Function(locator) = &side_effect_call.target else {
        panic!("expected side-effect call to resolve to function");
    };
    assert_eq!(locator.to_string(), "log_error");

    let template = expect_println_template(&stmts[1], "assertion failed");
    assert!(
        template.args.is_empty(),
        "default assert! failure should not interpolate values"
    );
    expect_abort(&stmts[2]);

    Ok(())
}

#[test]
fn debug_assert_wrapped_in_runtime_guard() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let expr = normalize_expr_tree(shll_parse_expr! { debug_assert!(1 == 2); })?;

    let ExprKind::If(wrapper) = expr.kind() else {
        panic!("debug_assert! should produce guard if expression");
    };

    let cond = wrapper.cond.as_ref();
    let ExprKind::IntrinsicCall(guard_call) = cond.kind() else {
        panic!("debug_assert! guard should call debug_assertions()");
    };
    assert_eq!(
        guard_call.kind,
        IntrinsicCallKind::DebugAssertions,
        "unexpected intrinsic guarding debug_assert!"
    );

    let ExprKind::Block(inner_block) = wrapper.then.as_ref().kind() else {
        panic!("debug_assert! body should remain a block expression");
    };
    assert_eq!(
        inner_block.stmts.len(),
        1,
        "assert! body should contain guard"
    );
    Ok(())
}

#[test]
fn env_macros_resolve_at_parse_time() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    const KEY_SOME: &str = "FP_RUST_TEST_ENV_SOME";
    const KEY_NONE: &str = "FP_RUST_TEST_ENV_NONE";

    std::env::set_var(KEY_SOME, "alpha");
    std::env::remove_var(KEY_NONE);

    let expr_some = normalize_expr_tree(shll_parse_expr! { env!("FP_RUST_TEST_ENV_SOME") })?;
    let value = match expr_some.kind() {
        ExprKind::Value(val) => val.as_ref(),
        other => panic!("env! should lower to literal value, found {:?}", other),
    };
    let Value::String(s) = value else {
        panic!("env! should produce string literal value");
    };
    assert_eq!(s.value, "alpha");

    let expr_option_some =
        normalize_expr_tree(shll_parse_expr! { option_env!("FP_RUST_TEST_ENV_SOME") })?;
    let value = match expr_option_some.kind() {
        ExprKind::Value(val) => val.as_ref(),
        other => panic!(
            "option_env! should lower to runtime value, found {:?}",
            other
        ),
    };
    let Value::Option(opt) = value else {
        panic!("option_env! should produce option");
    };
    assert!(opt.value.is_some(), "expected option_env! to capture value");

    let expr_option_none =
        normalize_expr_tree(shll_parse_expr! { option_env!("FP_RUST_TEST_ENV_NONE") })?;
    let value = match expr_option_none.kind() {
        ExprKind::Value(val) => val.as_ref(),
        other => panic!(
            "option_env! should lower to runtime value, found {:?}",
            other
        ),
    };
    let Value::Option(opt) = value else {
        panic!("option_env! should produce option");
    };
    assert!(
        opt.value.is_none(),
        "expected missing env var to yield None"
    );

    std::env::remove_var(KEY_SOME);
    Ok(())
}
