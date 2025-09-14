// Interpreter tests in the context of optimization passes
// Tests how the interpreter integrates with optimization and const evaluation systems

use fp_core::ast::AstValue;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::interpreter::Interpreter;
use pretty_assertions::assert_eq;
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::{shll_parse_expr, shll_parse_value};
use std::sync::Arc;

fn interpret_expr(expr: AstExpr) -> Result<AstValue> {
    let interpreter = Interpreter::new(Arc::new(RustPrinter::new()));
    let ctx = SharedScopedContext::new();
    interpreter.interpret_expr(expr, &ctx)
}

// ===== INTERPRETER FOR OPTIMIZATION PASSES =====

#[test]
fn test_interpreter_in_optimization_context() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter used within optimization passes
    let code = shll_parse_expr!(1 + 2 * 3);
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(7);
    assert_eq!(value, expected);
    // This kind of expression would be a candidate for const folding
    Ok(())
}

#[test]
#[ignore] // Function calls don't work yet in interpreter
fn test_interpreter_function_specialization_context() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter role in function specialization optimization
    let code = shll_parse_expr!({
        fn add(a: i64, b: i64) -> i64 { a + b }
        add(1, 2)
    });
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(3);
    assert_eq!(value, expected);
    Ok(())
}

// ===== INTERPRETER INTEGRATION WITH CONST EVALUATION =====

#[test]
fn test_interpreter_with_const_values() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter handling of const values
    let code = shll_parse_expr!({
        let max_value = 100;
        fn get_max() -> i64 {
            max_value
        }
        get_max()
    });
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(100);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
#[ignore] // Function calls don't work yet in interpreter  
fn test_interpreter_identity_function_specialization() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter in context of function specialization
    let code = shll_parse_expr!({
        fn identity(x: i64) -> i64 { x }
        identity(42)
    });
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_iterative_const_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter role in iterative const evaluation
    let code = shll_parse_expr!({
        fn compute() -> i64 {
            let a = 10;
            let b = a * 2;
            b + 5
        }
        compute()
    });
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(25);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_expression_caching_support() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter support for expression caching
    let expr1 = shll_parse_expr!(7 * 2);
    let expr2 = shll_parse_expr!(7 * 2); // Identical expression
    
    let value1 = interpret_expr(expr1)?;
    let value2 = interpret_expr(expr2)?;
    
    assert_eq!(value1, shll_parse_value!(14));
    assert_eq!(value2, shll_parse_value!(14));
    assert_eq!(value1, value2);
    
    Ok(())
}

#[test]
fn test_interpreter_main_function_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test interpreter handling of main function patterns
    let code = shll_parse_expr!({
        fn helper(a: i64, b: i64) -> i64 { a + b }
        fn main() -> i64 { helper(1, 2) }
        main()
    });
    let value = interpret_expr(code)?;
    let expected = shll_parse_value!(3);
    assert_eq!(value, expected);
    Ok(())
}
