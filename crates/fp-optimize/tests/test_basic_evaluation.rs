// Basic language evaluation through optimization infrastructure
// Tests interpreter functionality as part of the optimization system
// Focus: Basic language features evaluated through fp-optimize interpreter

use fp_core::ast::AstValue;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust::printer::RustPrinter;
use fp_rust::{shll_parse_expr, shll_parse_value};
use pretty_assertions::assert_eq;
use std::sync::Arc;

// TODO: Fix interpreter API - use InterpretationOrchestrator instead of Interpreter
fn create_interpreter() -> InterpretationOrchestrator {
    todo!("Update to use InterpretationOrchestrator API")
}

fn interpret_expr(_expr: AstExpr) -> Result<AstValue> {
    // TODO: Fix interpreter API calls
    todo!("Update interpreter.interpret_expr call to use new API")
}

// ===== BASIC EVALUATION THROUGH OPTIMIZATION SYSTEM =====

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_basic_expression_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test basic expression evaluation through optimizer
    let expr = shll_parse_expr!(42);
    let result = interpreter.interpret_expr(expr, &ctx)?;
    assert_eq!(result, shll_parse_value!(42));

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_arithmetic_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test arithmetic evaluation capabilities
    let test_cases = vec![
        (shll_parse_expr!(5 + 3), shll_parse_value!(8)),
        (shll_parse_expr!(10 - 4), shll_parse_value!(6)),
        (shll_parse_expr!(6 * 7), shll_parse_value!(42)),
        (shll_parse_expr!(2 + 3 * 4), shll_parse_value!(14)), // precedence
    ];

    for (expr, expected) in test_cases {
        let result = interpreter.interpret_expr(expr, &ctx)?;
        assert_eq!(result, expected);
    }

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_comparison_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test comparison capabilities
    let test_cases = vec![
        (shll_parse_expr!(5 == 5), shll_parse_value!(true)),
        (shll_parse_expr!(5 != 3), shll_parse_value!(true)),
        (shll_parse_expr!(10 > 5), shll_parse_value!(true)),
        (shll_parse_expr!(3 < 7), shll_parse_value!(true)),
        (shll_parse_expr!(5 >= 5), shll_parse_value!(true)),
        (shll_parse_expr!(3 <= 7), shll_parse_value!(true)),
    ];

    for (expr, expected) in test_cases {
        let result = interpreter.interpret_expr(expr, &ctx)?;
        assert_eq!(result, expected);
    }

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_context_management() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test that interpreter properly manages context
    let expr1 = shll_parse_expr!(true);
    let expr2 = shll_parse_expr!(false);

    let result1 = interpreter.interpret_expr(expr1, &ctx)?;
    let result2 = interpreter.interpret_expr(expr2, &ctx)?;

    assert_eq!(result1, shll_parse_value!(true));
    assert_eq!(result2, shll_parse_value!(false));

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_error_handling() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test that interpreter properly handles unsupported operations
    let unsupported_expr = shll_parse_expr!(20 / 4); // Division not supported
    let result = interpreter.interpret_expr(unsupported_expr, &ctx);

    // Should return an error, not crash
    assert!(result.is_err());

    Ok(())
}

// ===== SUPPORTED VS UNSUPPORTED OPERATIONS =====

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_supported_operations() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Document what operations the interpreter currently supports
    let supported_operations = vec![
        "Addition (+)",
        "Subtraction (-)",
        "Multiplication (*)",
        "Equality (==)",
        "Inequality (!=)",
        "Greater than (>)",
        "Less than (<)",
        "Greater than or equal (>=)",
        "Less than or equal (<=)",
    ];

    let test_expressions = vec![
        shll_parse_expr!(1 + 1),
        shll_parse_expr!(5 - 2),
        shll_parse_expr!(3 * 4),
        shll_parse_expr!(5 == 5),
        shll_parse_expr!(5 != 3),
        shll_parse_expr!(10 > 5),
        shll_parse_expr!(3 < 7),
        shll_parse_expr!(5 >= 5),
        shll_parse_expr!(3 <= 7),
    ];

    // All of these should evaluate successfully
    for (i, expr) in test_expressions.into_iter().enumerate() {
        let result = interpreter.interpret_expr(expr, &ctx);
        assert!(
            result.is_ok(),
            "Operation '{}' should be supported",
            supported_operations[i]
        );
    }

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_unsupported_operations() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Document what operations the interpreter does NOT support yet
    let unsupported_expressions = vec![
        ("Division", shll_parse_expr!(20 / 4)),
        ("Logical AND", shll_parse_expr!(true && false)),
        ("Logical OR", shll_parse_expr!(true || false)),
        ("Logical NOT", shll_parse_expr!(!true)),
        (
            "If expressions",
            shll_parse_expr!(if true { 42 } else { 24 }),
        ),
        // Note: Basic block expressions now work, but complex scoping may still fail
    ];

    for (operation_name, expr) in unsupported_expressions {
        let result = interpreter.interpret_expr(expr, &ctx);
        assert!(
            result.is_err(),
            "Operation '{}' should fail (not yet supported)",
            operation_name
        );
    }

    Ok(())
}

// ===== COMPLEX EXPRESSIONS =====

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_complex_expressions() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test interpreter with more complex expressions
    let expr = shll_parse_expr!(2 + 3 * 4 - 1 + 5);
    let result = interpreter.interpret_expr(expr, &ctx)?;
    assert_eq!(result, shll_parse_value!(18)); // 2 + 12 - 1 + 5

    let expr = shll_parse_expr!(1 + 2 + 3 + 4 + 5);
    let result = interpreter.interpret_expr(expr, &ctx)?;
    assert_eq!(result, shll_parse_value!(15));

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_edge_case_values() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test edge cases
    let expr = shll_parse_expr!(0 * 1000000);
    let result = interpreter.interpret_expr(expr, &ctx)?;
    assert_eq!(result, shll_parse_value!(0));

    let expr = shll_parse_expr!(5 - 10); // Results in negative
    let result = interpreter.interpret_expr(expr, &ctx)?;
    match result {
        AstValue::Int(val) => assert_eq!(val.value, -5),
        _ => panic!("Expected integer result"),
    }

    Ok(())
}

// ===== CONST EVALUATION READINESS =====

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_const_evaluation_readiness() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let interpreter = create_interpreter();
    let ctx = SharedScopedContext::new();

    // Test that interpreter can handle expressions that would be good candidates
    // for const evaluation (compile-time computation)
    let const_evaluable_exprs = vec![
        shll_parse_expr!(2 + 3),     // Simple arithmetic
        shll_parse_expr!(10 * 5),    // Multiplication
        shll_parse_expr!(1 + 2 * 3), // With precedence
        shll_parse_expr!(5 == 5),    // Boolean comparisons
        shll_parse_expr!(100 > 50),  // Ordering
    ];

    for expr in const_evaluable_exprs {
        let result = interpreter.interpret_expr(expr, &ctx);
        assert!(
            result.is_ok(),
            "Expression should be evaluable (const evaluation candidate)"
        );
    }

    Ok(())
}
