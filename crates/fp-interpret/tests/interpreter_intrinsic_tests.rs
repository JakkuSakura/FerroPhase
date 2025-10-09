use fp_core::ast::{
    Expr, ExprFormatString, ExprKind, FormatArgRef, FormatPlaceholder, FormatTemplatePart, Value,
};
use fp_core::context::SharedScopedContext;
use fp_core::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
use fp_interpret::ast::{AstInterpreter, InterpreterOptions};

fn intrinsic_args_expr(kind: IntrinsicCallKind, values: Vec<Value>) -> Expr {
    let args = values.into_iter().map(Expr::value).collect();
    let call = IntrinsicCall::new(kind, IntrinsicCallPayload::Args { args });
    Expr::new(ExprKind::IntrinsicCall(call))
}

fn intrinsic_format_expr(kind: IntrinsicCallKind, template: ExprFormatString) -> Expr {
    let call = IntrinsicCall::new(kind, IntrinsicCallPayload::Format { template });
    Expr::new(ExprKind::IntrinsicCall(call))
}

#[test]
fn print_appends_without_newline_and_println_pushes_new_entry() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut first = intrinsic_args_expr(
        IntrinsicCallKind::Print,
        vec![Value::string("hello".to_owned())],
    );
    let value = interpreter.evaluate_expression(&mut first);
    assert!(matches!(value, Value::Unit(_)));

    let mut second = intrinsic_args_expr(
        IntrinsicCallKind::Print,
        vec![Value::string(" world".to_owned())],
    );
    interpreter.evaluate_expression(&mut second);

    let mut third = intrinsic_args_expr(
        IntrinsicCallKind::Println,
        vec![Value::string("!".to_owned())],
    );
    interpreter.evaluate_expression(&mut third);

    let outcome = interpreter.take_outcome();
    assert_eq!(
        outcome.stdout,
        vec!["hello world".to_owned(), "!".to_owned()]
    );
}

#[test]
fn println_renders_format_template_placeholders() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let template = ExprFormatString {
        parts: vec![
            FormatTemplatePart::Literal("total=".to_owned()),
            FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: None,
            }),
        ],
        args: vec![Expr::value(Value::int(42))],
        kwargs: vec![],
    };

    let mut expr = intrinsic_format_expr(IntrinsicCallKind::Println, template);
    interpreter.evaluate_expression(&mut expr);

    let outcome = interpreter.take_outcome();
    assert_eq!(outcome.stdout, vec!["total=42".to_owned()]);
    assert!(!outcome.has_errors);
}

#[test]
fn println_args_payload_joins_values_with_space() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut expr = intrinsic_args_expr(
        IntrinsicCallKind::Println,
        vec![Value::int(1), Value::int(2), Value::int(3)],
    );
    interpreter.evaluate_expression(&mut expr);

    let outcome = interpreter.take_outcome();
    assert_eq!(outcome.stdout, vec!["1 2 3".to_owned()]);
}

#[test]
fn println_with_missing_placeholder_argument_emits_diagnostic() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let template = ExprFormatString {
        parts: vec![FormatTemplatePart::Placeholder(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        })],
        args: vec![],
        kwargs: vec![],
    };

    let mut expr = intrinsic_format_expr(IntrinsicCallKind::Println, template);
    interpreter.evaluate_expression(&mut expr);

    let outcome = interpreter.take_outcome();
    assert!(outcome.has_errors);
    assert!(outcome.stdout.is_empty());
    assert!(outcome
        .diagnostics
        .iter()
        .any(|diag| diag.message.contains("has no argument")));
}
