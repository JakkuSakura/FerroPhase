use fp_core::ast::{Expr, ExprBinOp, ExprKind, Value};
use fp_core::context::SharedScopedContext;
use fp_core::ops::BinOpKind;
use fp_interpret::engine::{AstInterpreter, EvalStepOutcome, InterpreterOptions};

fn main() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut expr = Expr::new(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(Expr::value(Value::int(10))),
        rhs: Box::new(Expr::value(Value::int(32))),
    }));

    interpreter.begin_const_eval(&mut expr).expect("begin const eval");

    loop {
        match interpreter.step_const_eval(4).expect("step const eval") {
            EvalStepOutcome::Yielded => continue,
            EvalStepOutcome::Complete(value) => {
                println!("result={value:?}");
                break;
            }
        }
    }
}
