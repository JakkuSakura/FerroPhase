use fp_core::ast::AstValue;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::interpreter::Interpreter;
use pretty_assertions::assert_eq;
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::{shll_parse_expr, shll_parse_value};
use std::sync::Arc;

fn interpret_const_expr(expr: AstExpr) -> Result<AstValue> {
    let interpreter = Interpreter::new(Arc::new(RustPrinter::new()));
    let ctx = SharedScopedContext::new();
    interpreter.interpret_expr(expr, &ctx)
}

#[test]
fn test_const_eval_literals() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test integer literal
    let code = shll_parse_expr!(42);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    // Test boolean literal
    let code = shll_parse_expr!(true);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(true);
    assert_eq!(value, expected);

    // Test string literal
    let code = shll_parse_expr!("hello");
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!("hello");
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_simple_arithmetic() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test addition
    let code = shll_parse_expr!(2 + 3);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(5);
    assert_eq!(value, expected);

    // Test multiplication
    let code = shll_parse_expr!(4 * 5);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(20);
    assert_eq!(value, expected);

    // Test complex expression
    let code = shll_parse_expr!(2 + 3 * 4);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(14);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_boolean_operations() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test equality
    let code = shll_parse_expr!(5 == 5);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(true);
    assert_eq!(value, expected);

    // Test inequality
    let code = shll_parse_expr!(5 != 3);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(true);
    assert_eq!(value, expected);

    // Test comparison
    let code = shll_parse_expr!(10 > 5);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(true);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_if_expression() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test if with true condition
    let code = shll_parse_expr!(if true { 42 } else { 24 });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    // Test if with false condition
    let code = shll_parse_expr!(if false { 42 } else { 24 });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(24);
    assert_eq!(value, expected);

    // Test if without else (should return unit)
    let code = shll_parse_expr!(if false { 42 });
    let value = interpret_const_expr(code)?;
    assert!(matches!(value, AstValue::Unit(_)));

    Ok(())
}

#[test]
fn test_const_eval_variable_binding() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test simple let binding
    let code = shll_parse_expr!({
        let x = 42;
        x
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    // Test let with expression
    let code = shll_parse_expr!({
        let x = 2 + 3;
        x * 2
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(10);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_block_expression() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test block with multiple statements
    let code = shll_parse_expr!({
        let x = 10;
        let y = 20;
        x + y
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(30);
    assert_eq!(value, expected);

    // Test nested blocks
    let code = shll_parse_expr!({
        let a = {
            let b = 5;
            b * 2
        };
        a + 3
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(13);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_string_operations() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test string concatenation
    let code = shll_parse_expr!("hello" + " " + "world");
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!("hello world");
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_complex_expression() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test complex nested expression
    let code = shll_parse_expr!({
        let condition = 5 > 3;
        let base = if condition { 10 } else { 5 };
        let multiplier = 2;
        base * multiplier + 1
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(21); // (10 * 2) + 1
    assert_eq!(value, expected);

    Ok(())
}