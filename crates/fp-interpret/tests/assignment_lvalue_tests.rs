use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprAssign, ExprBlock, ExprDereference, ExprIndex, ExprKind,
    ExprLet, ExprRange, ExprRangeLimit, ExprReference, ExprSelect, ExprSelectType, Ident, Pattern,
    PatternIdent, PatternKind, Value, ValueField, ValueList, ValueStructural,
};
use fp_core::context::SharedScopedContext;
use fp_core::span::Span;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};

fn mut_let_expr(name: &str, value: Value) -> Expr {
    let pat = Pattern::new(PatternKind::Ident(PatternIdent {
        ident: Ident::new(name),
        mutability: Some(true),
    }));
    Expr::new(ExprKind::Let(ExprLet {
        span: Span::null(),
        pat: Box::new(pat),
        expr: Box::new(Expr::value(value)),
    }))
}

fn select_expr(obj: Expr, field: &str) -> Expr {
    Expr::new(ExprKind::Select(ExprSelect {
        span: Span::null(),
        obj: Box::new(obj),
        field: Ident::new(field),
        select: ExprSelectType::Unknown,
    }))
}

fn index_expr(obj: Expr, idx: i64) -> Expr {
    Expr::new(ExprKind::Index(ExprIndex {
        span: Span::null(),
        obj: Box::new(obj),
        index: Box::new(Expr::value(Value::int(idx))),
    }))
}

fn assign_expr(target: Expr, value: Value) -> Expr {
    Expr::new(ExprKind::Assign(ExprAssign {
        span: Span::null(),
        target: Box::new(target),
        value: Box::new(Expr::value(value)),
    }))
}

fn range_index_expr(obj: Expr, start: Option<i64>, end: Option<i64>) -> Expr {
    Expr::new(ExprKind::Index(ExprIndex {
        span: Span::null(),
        obj: Box::new(obj),
        index: Box::new(Expr::new(ExprKind::Range(ExprRange {
            span: Span::null(),
            start: start.map(|value| Expr::value(Value::int(value)).into()),
            limit: ExprRangeLimit::Exclusive,
            end: end.map(|value| Expr::value(Value::int(value)).into()),
            step: None,
        }))),
    }))
}

fn mut_ref_expr(expr: Expr) -> Expr {
    Expr::new(ExprKind::Reference(ExprReference {
        span: Span::null(),
        referee: Box::new(expr),
        mutable: Some(true),
    }))
}

fn deref_expr(expr: Expr) -> Expr {
    Expr::new(ExprKind::Dereference(ExprDereference {
        span: Span::null(),
        referee: Box::new(expr),
    }))
}

#[test]
fn nested_field_assignment_updates_inner_structural_value() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let initial = Value::Structural(ValueStructural::new(vec![ValueField::new(
        Ident::new("b"),
        Value::Structural(ValueStructural::new(vec![ValueField::new(
            Ident::new("c"),
            Value::int(0),
        )])),
    )]));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", initial)).with_semicolon(true),
    ));

    let target = select_expr(select_expr(Expr::ident(Ident::new("a")), "b"), "c");
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(target, Value::int(42))).with_semicolon(true),
    ));

    let result_expr = select_expr(select_expr(Expr::ident(Ident::new("a")), "b"), "c");
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(result_expr)));
    let mut expr = Expr::new(ExprKind::Block(block));

    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(42));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}

#[test]
fn nested_index_assignment_updates_inner_list() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let initial = Value::List(ValueList::new(vec![
        Value::List(ValueList::new(vec![Value::int(0), Value::int(1)])),
        Value::List(ValueList::new(vec![Value::int(2), Value::int(3)])),
    ]));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", initial)).with_semicolon(true),
    ));

    let target = index_expr(index_expr(Expr::ident(Ident::new("a")), 1), 0);
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(target, Value::int(9))).with_semicolon(true),
    ));

    let result_expr = index_expr(index_expr(Expr::ident(Ident::new("a")), 1), 0);
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(result_expr)));
    let mut expr = Expr::new(ExprKind::Block(block));

    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(9));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}

#[test]
fn field_then_index_assignment_updates_list_inside_structural_value() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let initial = Value::Structural(ValueStructural::new(vec![ValueField::new(
        Ident::new("b"),
        Value::List(ValueList::new(vec![Value::int(1), Value::int(2)])),
    )]));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", initial)).with_semicolon(true),
    ));

    let target = index_expr(select_expr(Expr::ident(Ident::new("a")), "b"), 1);
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(target, Value::int(5))).with_semicolon(true),
    ));

    let result_expr = index_expr(select_expr(Expr::ident(Ident::new("a")), "b"), 1);
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(result_expr)));
    let mut expr = Expr::new(ExprKind::Block(block));

    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(5));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}

#[test]
fn deref_assignment_updates_referenced_storage() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", Value::int(1))).with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr(
            "p",
            Value::expr(mut_ref_expr(Expr::ident(Ident::new("a")))),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(
            deref_expr(Expr::ident(Ident::new("p"))),
            Value::int(7),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(Expr::ident(
        Ident::new("a"),
    ))));

    let mut expr = Expr::new(ExprKind::Block(block));
    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(7));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}

#[test]
fn mutable_reference_to_field_supports_followup_assignment() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let initial = Value::Structural(ValueStructural::new(vec![ValueField::new(
        Ident::new("b"),
        Value::int(3),
    )]));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", initial)).with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr(
            "field_ref",
            Value::expr(mut_ref_expr(select_expr(Expr::ident(Ident::new("a")), "b"))),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(
            deref_expr(Expr::ident(Ident::new("field_ref"))),
            Value::int(11),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(select_expr(
        Expr::ident(Ident::new("a")),
        "b",
    ))));

    let mut expr = Expr::new(ExprKind::Block(block));
    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(11));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}

#[test]
fn mutable_reference_to_slice_without_explicit_end_uses_container_length() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::Runtime,
            ..InterpreterOptions::default()
        },
    );

    let initial = Value::List(ValueList::new(vec![
        Value::int(10),
        Value::int(20),
        Value::int(30),
        Value::int(40),
    ]));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr("a", initial)).with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(mut_let_expr(
            "slice_ref",
            Value::expr(mut_ref_expr(range_index_expr(
                Expr::ident(Ident::new("a")),
                Some(1),
                None,
            ))),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(assign_expr(
            index_expr(Expr::ident(Ident::new("slice_ref")), 1),
            Value::int(99),
        ))
        .with_semicolon(true),
    ));
    block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(index_expr(
        Expr::ident(Ident::new("a")),
        2,
    ))));

    let mut expr = Expr::new(ExprKind::Block(block));
    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(value, Value::int(99));

    let outcome = interpreter.take_outcome();
    assert!(!outcome.has_errors);
}
