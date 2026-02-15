use fp_core::ast::{Expr, ExprBinOp, ExprKind, ExprRange, ExprRangeLimit, Value};
use fp_core::context::SharedScopedContext;
use fp_core::ops::BinOpKind;
use fp_interpret::engine::{
    AstInterpreter, EvalStepOutcome, InterpreterMode, InterpreterOptions, RuntimeStepOutcome,
};

fn binop_expr(lhs: i64, rhs: i64, op: BinOpKind) -> Expr {
    Expr::new(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: op,
        lhs: Box::new(Expr::value(Value::int(lhs))),
        rhs: Box::new(Expr::value(Value::int(rhs))),
    }))
}

#[test]
fn const_step_eval_yields_then_completes() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut expr = binop_expr(1, 2, BinOpKind::Add);
    interpreter
        .begin_const_eval(&mut expr)
        .expect("begin const eval");

    let first = interpreter.step_const_eval(1).expect("step const eval");
    assert!(matches!(first, EvalStepOutcome::Yielded));

    let mut final_value = None;
    for _ in 0..8 {
        match interpreter.step_const_eval(10).expect("step const eval") {
            EvalStepOutcome::Yielded => continue,
            EvalStepOutcome::Complete(value) => {
                final_value = Some(value);
                break;
            }
        }
    }

    let final_value = final_value.expect("const eval completion");
    assert_eq!(final_value, Value::int(3));
}

#[test]
fn runtime_step_eval_yields_then_completes() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(
        &ctx,
        InterpreterOptions {
            mode: InterpreterMode::RunTime,
            ..InterpreterOptions::default()
        },
    );

    let mut expr = binop_expr(4, 5, BinOpKind::Mul);
    interpreter
        .begin_runtime_eval(&mut expr)
        .expect("begin runtime eval");

    let first = interpreter.step_runtime_eval(1).expect("step runtime eval");
    assert!(matches!(first, RuntimeStepOutcome::Yielded));

    let mut final_flow = None;
    for _ in 0..8 {
        match interpreter
            .step_runtime_eval(10)
            .expect("step runtime eval")
        {
            RuntimeStepOutcome::Yielded => continue,
            RuntimeStepOutcome::Complete(flow) => {
                final_flow = Some(flow);
                break;
            }
        }
    }

    let final_flow = final_flow.expect("runtime eval completion");
    match final_flow {
        fp_interpret::engine::RuntimeFlow::Value(value) => {
            assert_eq!(value, Value::int(20));
        }
        other => panic!("unexpected runtime flow: {other:?}"),
    }
}

#[test]
fn const_range_eval_uses_stack_tasks() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut expr = Expr::new(ExprKind::Range(ExprRange {
        span: fp_core::span::Span::null(),
        start: Some(Box::new(Expr::value(Value::int(1)))),
        limit: ExprRangeLimit::Inclusive,
        end: Some(Box::new(Expr::value(Value::int(3)))),
        step: None,
    }));

    let value = interpreter.evaluate_expression(&mut expr);
    assert_eq!(
        value,
        Value::List(fp_core::ast::ValueList::new(vec![
            Value::int(1),
            Value::int(2),
            Value::int(3),
        ]))
    );
}
