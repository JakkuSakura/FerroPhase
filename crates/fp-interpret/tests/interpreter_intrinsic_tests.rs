use fp_core::ast::{
    Expr, ExprIntrinsicCall, ExprKind, ExprStringTemplate, FormatArgRef, FormatPlaceholder,
    FormatTemplatePart, Ty, TypeInt, TypePrimitive, Value,
};
use fp_core::context::SharedScopedContext;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_interpret::engine::{AstInterpreter, InterpreterOptions};

fn intrinsic_args_expr(kind: IntrinsicCallKind, values: Vec<Value>) -> Expr {
    let args = values.into_iter().map(Expr::value).collect();
    let call = ExprIntrinsicCall::new(kind, args, Vec::new());
    Expr::new(ExprKind::IntrinsicCall(call))
}

fn intrinsic_format_expr(
    kind: IntrinsicCallKind,
    template: ExprStringTemplate,
    args: Vec<Expr>,
) -> Expr {
    let mut call_args = Vec::with_capacity(1 + args.len());
    call_args.push(Expr::new(ExprKind::FormatString(template)));
    call_args.extend(args);
    let call = ExprIntrinsicCall::new(kind, call_args, Vec::new());
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
        vec!["hello world".to_owned(), "!\n".to_owned()]
    );
}

#[test]
fn println_renders_format_template_placeholders() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let template = ExprStringTemplate {
        parts: vec![
            FormatTemplatePart::Literal("total=".to_owned()),
            FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: None,
            }),
        ],
    };

    let mut expr = intrinsic_format_expr(
        IntrinsicCallKind::Println,
        template,
        vec![Expr::value(Value::int(42))],
    );
    interpreter.evaluate_expression(&mut expr);

    let outcome = interpreter.take_outcome();
    assert_eq!(outcome.stdout, vec!["total=42\n".to_owned()]);
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
    assert_eq!(outcome.stdout, vec!["1 2 3\n".to_owned()]);
}

#[test]
fn println_with_missing_placeholder_argument_emits_diagnostic() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let template = ExprStringTemplate {
        parts: vec![FormatTemplatePart::Placeholder(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        })],
    };

    let mut expr = intrinsic_format_expr(IntrinsicCallKind::Println, template, Vec::new());
    interpreter.evaluate_expression(&mut expr);

    let outcome = interpreter.take_outcome();
    assert!(outcome.has_errors);
    assert!(outcome.stdout.is_empty());
    assert!(outcome
        .diagnostics
        .iter()
        .any(|diag| diag.message.contains("has no argument")));
}

#[test]
fn type_of_returns_typed_expression_type() {
    let ctx = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&ctx, InterpreterOptions::default());

    let mut arg = Expr::value(Value::int(42));
    arg.set_ty(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
    let call = ExprIntrinsicCall::new(IntrinsicCallKind::TypeOf, vec![arg], Vec::new());
    let mut expr = Expr::new(ExprKind::IntrinsicCall(call));

    let value = interpreter.evaluate_expression(&mut expr);
    match value {
        Value::Type(ty) => {
            assert!(matches!(
                ty,
                Ty::Primitive(TypePrimitive::Int(TypeInt::I64))
            ));
        }
        other => panic!("expected type value, got {:?}", other),
    }
}
