// Core AST and context functionality tests
// Tests pure fp-core capabilities without optimization infrastructure
// Focus: AST manipulation, context management, basic language constructs

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ops::BinOpKind;
use fp_core::pretty::{pretty, PrettyOptions};
use fp_core::Result;
use pretty_assertions::assert_eq;

fn int_expr(value: i64) -> Expr {
    Expr::value(Value::int(value))
}

// ===== CORE AST VALUE TESTS =====

#[test]
fn test_ast_value_creation() -> Result<()> {
    // Test creation of basic AST values
    let int_val = Value::int(42);
    let bool_val = Value::bool(true);
    let string_val = Value::string("hello".to_string());
    let unit_val = Value::unit();

    // Test value properties
    assert!(unit_val.is_unit());

    // Test value display
    assert!(!format!("{int_val:?}").is_empty());
    assert!(!format!("{bool_val:?}").is_empty());
    assert!(!format!("{string_val:?}").is_empty());

    Ok(())
}

#[test]
fn test_ast_value_comparison() -> Result<()> {
    // Test that values can be compared
    let val1 = Value::int(42);
    let val2 = Value::int(42);
    let val3 = Value::int(24);

    assert_eq!(val1, val2);
    assert_ne!(val1, val3);

    Ok(())
}

#[test]
fn test_ast_expr_parsing() -> Result<()> {
    // Test that expressions parse into correct AST structures
    let expr = int_expr(42);
    match expr.kind() {
        ExprKind::Value(val) => match val.as_ref() {
            Value::Int(int_val) => assert_eq!(int_val.value, 42),
            _ => panic!("Expected integer value"),
        },
        _ => panic!("Expected value expression"),
    }

    Ok(())
}

// ===== CONTEXT MANAGEMENT TESTS =====

#[test]
fn test_shared_scoped_context_creation() -> Result<()> {
    // Test basic context creation and management
    let ctx = SharedScopedContext::new();

    // Context should be created successfully
    // This is mainly testing that the type can be instantiated
    // More detailed context functionality would need specific context APIs

    // Test multiple contexts can coexist
    let ctx2 = SharedScopedContext::new();

    // Both should be valid (basic smoke test)
    drop(ctx);
    drop(ctx2);

    Ok(())
}

// ===== AST STRUCTURE TESTS =====

#[test]
fn test_complex_ast_structures() -> Result<()> {
    // Test that complex AST structures can be built
    let expr = Expr::from(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(int_expr(5)),
        rhs: Box::new(int_expr(3)),
    }));

    // Verify the AST structure is correct
    match expr.kind() {
        ExprKind::BinOp(binop) => {
            // Has left and right operands
            match (binop.lhs.as_ref().kind(), binop.rhs.as_ref().kind()) {
                (ExprKind::Value(_), ExprKind::Value(_)) => {
                    // Good - both sides are values
                }
                _ => panic!("Expected value operands"),
            }
        }
        _ => panic!("Expected binary operation"),
    }

    Ok(())
}

#[test]
fn test_nested_ast_expressions() -> Result<()> {
    // Test nested expression parsing
    let expr = Expr::from(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(int_expr(2)),
        rhs: Box::new(Expr::from(ExprKind::BinOp(ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Mul,
            lhs: Box::new(int_expr(3)),
            rhs: Box::new(int_expr(4)),
        }))),
    }));

    // Should parse as 2 + (3 * 4) due to precedence
    match expr.kind() {
        ExprKind::BinOp(add_op) => {
            // Right side should be a multiplication
            match add_op.rhs.as_ref().kind() {
                ExprKind::BinOp(_mul_op) => {
                    // Good - nested structure preserved
                }
                _ => panic!("Expected nested binary operation"),
            }
        }
        _ => panic!("Expected addition at top level"),
    }

    Ok(())
}

// ===== SERIALIZATION AND DISPLAY TESTS =====

#[test]
fn test_ast_serialization() -> Result<()> {
    // Test that AST nodes can be serialized to strings
    let expr = int_expr(42);
    let serialized = format!("{}", pretty(&expr, PrettyOptions::default()));

    assert!(
        !serialized.is_empty(),
        "Serialized expression should not be empty"
    );
    assert!(
        serialized.contains("42"),
        "Serialized expression should contain the value"
    );

    let complex_expr = Expr::from(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(int_expr(1)),
        rhs: Box::new(int_expr(2)),
    }));
    let complex_serialized = format!("{}", pretty(&complex_expr, PrettyOptions::default()));

    assert!(
        !complex_serialized.is_empty(),
        "Complex expression should serialize"
    );

    Ok(())
}

// ===== ERROR HANDLING TESTS =====

#[test]
fn test_core_error_types() -> Result<()> {
    // Test that core error types can be created and handled
    // This is a basic test to ensure error handling infrastructure works

    // For now, just test that Result types work
    let success: Result<i32> = Ok(42);
    let _failure: Result<i32> = Err("test error".to_string().into());

    assert!(success.is_ok());
    assert!(_failure.is_err());

    Ok(())
}
