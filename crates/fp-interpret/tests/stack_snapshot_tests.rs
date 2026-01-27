use fp_core::ast::{Expr, ExprKind, Value};
use fp_core::context::SharedScopedContext;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};

#[test]
fn stack_snapshot_tracks_const_values() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut expr = Expr::new(ExprKind::Value(Value::int(1)));
    interpreter.evaluate_expression(&mut expr);

    let snapshot = interpreter.stack_snapshot();
    assert_eq!(snapshot.call_frames, 0);
    assert_eq!(snapshot.expr_frames, 0);
    assert_eq!(snapshot.const_values, 1);
    assert_eq!(snapshot.runtime_values, 0);
}

#[test]
fn stack_snapshot_tracks_runtime_values() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::RunTime,
            ..InterpreterOptions::default()
        },
    );

    let mut expr = Expr::new(ExprKind::Value(Value::int(2)));
    interpreter.evaluate_expression(&mut expr);

    let snapshot = interpreter.stack_snapshot();
    assert_eq!(snapshot.call_frames, 0);
    assert_eq!(snapshot.expr_frames, 0);
    assert_eq!(snapshot.const_values, 0);
    assert_eq!(snapshot.runtime_values, 1);
}
