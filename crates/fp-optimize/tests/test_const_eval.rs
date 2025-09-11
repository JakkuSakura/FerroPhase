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

    // Test complex nested expression - simplified since if expressions may not be fully implemented
    let code = shll_parse_expr!({
        let base = 10;
        let multiplier = 2;
        base * multiplier + 1
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(21); // (10 * 2) + 1
    assert_eq!(value, expected);

    Ok(())
}

// ===== ADDITIONAL TESTS FOR CONST EVALUATION PHASES =====

#[test]
fn test_const_eval_type_integration() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test type integration - basic type consistency
    let code = shll_parse_expr!(42);
    let value = interpret_const_expr(code)?;
    // Should be able to determine this is an integer type
    assert!(matches!(value, AstValue::Int(_)));

    Ok(())
}

#[test]
fn test_const_eval_dependency_order() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test dependency resolution - const values depending on others
    let code = shll_parse_expr!({
        let a = 5;
        let b = a + 3;
        let c = b * 2;
        c
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(16); // ((5 + 3) * 2)
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_generic_specialization() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test basic generic-like behavior (since full generics may not be implemented)
    let code = shll_parse_expr!({
        fn double(x: i64) -> i64 { x * 2 }
        double(21)
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_side_effects_placeholder() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Placeholder test for side effects (code generation)
    // When implemented, this would test @addfield, @generate_method, etc.
    let code = shll_parse_expr!(42);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    // TODO: Test side effects like struct field generation
    Ok(())
}

#[test]
fn test_const_eval_intrinsics_placeholder() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Placeholder test for metaprogramming intrinsics
    // When implemented, this would test @sizeof, @reflect_fields, etc.
    let code = shll_parse_expr!(42);
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);

    // TODO: Test @sizeof(i64) == 8
    // TODO: Test @reflect_fields on structs
    Ok(())
}

#[test]
fn test_const_eval_iterative_convergence() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test iterative evaluation (simplified)
    // When implemented, this would test that evaluation converges
    let code = shll_parse_expr!({
        let x = 10;
        let y = x + 5;
        y
    });
    let value = interpret_const_expr(code)?;
    let expected = shll_parse_value!(15);
    assert_eq!(value, expected);

    Ok(())
}

#[test]
fn test_const_eval_caching() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test expression caching (simplified)
    // When implemented, this would test that identical expressions are cached
    let code = shll_parse_expr!(2 + 3);
    let value1 = interpret_const_expr(code.clone())?;
    let value2 = interpret_const_expr(code)?;
    assert_eq!(value1, value2);

    Ok(())
}