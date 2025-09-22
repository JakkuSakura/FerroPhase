// Core AST and context functionality tests
// Tests pure fp-core capabilities without optimization infrastructure
// Focus: AST manipulation, context management, basic language constructs

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_rust::printer::RustPrinter;
use fp_rust::{shll_parse_expr, shll_parse_value};
use pretty_assertions::assert_eq;
use std::sync::Arc;

// ===== CORE AST VALUE TESTS =====

#[test]
fn test_ast_value_creation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test creation of basic AST values
    let int_val = AstValue::int(42);
    let bool_val = AstValue::bool(true);
    let string_val = AstValue::string("hello".to_string());
    let unit_val = AstValue::unit();

    // Test value properties
    assert!(unit_val.is_unit());

    // Test value display
    assert!(!int_val.to_string().is_empty());
    assert!(!bool_val.to_string().is_empty());
    assert!(!string_val.to_string().is_empty());

    Ok(())
}

#[test]
fn test_ast_value_comparison() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that values can be compared
    let val1 = shll_parse_value!(42);
    let val2 = shll_parse_value!(42);
    let val3 = shll_parse_value!(24);

    assert_eq!(val1, val2);
    assert_ne!(val1, val3);

    Ok(())
}

#[test]
fn test_ast_expr_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that expressions parse into correct AST structures
    let expr = shll_parse_expr!(42);
    match expr {
        AstExpr::Value(val) => match val.as_ref() {
            AstValue::Int(int_val) => assert_eq!(int_val.value, 42),
            _ => panic!("Expected integer value"),
        },
        _ => panic!("Expected value expression"),
    }

    Ok(())
}

// ===== CONTEXT MANAGEMENT TESTS =====

#[test]
fn test_shared_scoped_context_creation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

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
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that complex AST structures can be built
    let expr = shll_parse_expr!(5 + 3);

    // Verify the AST structure is correct
    match expr {
        AstExpr::BinOp(binop) => {
            // Has left and right operands
            match (binop.lhs.as_ref(), binop.rhs.as_ref()) {
                (AstExpr::Value(_), AstExpr::Value(_)) => {
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
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test nested expression parsing
    let expr = shll_parse_expr!(2 + 3 * 4);

    // Should parse as 2 + (3 * 4) due to precedence
    match expr {
        AstExpr::BinOp(add_op) => {
            // Right side should be a multiplication
            match add_op.rhs.as_ref() {
                AstExpr::BinOp(_mul_op) => {
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
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that AST nodes can be serialized to strings
    let expr = shll_parse_expr!(42);
    let serialized = expr.to_string();

    assert!(
        !serialized.is_empty(),
        "Serialized expression should not be empty"
    );
    assert!(
        serialized.contains("42"),
        "Serialized expression should contain the value"
    );

    let complex_expr = shll_parse_expr!(1 + 2);
    let complex_serialized = complex_expr.to_string();

    assert!(
        !complex_serialized.is_empty(),
        "Complex expression should serialize"
    );

    Ok(())
}

// ===== ERROR HANDLING TESTS =====

#[test]
fn test_core_error_types() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that core error types can be created and handled
    // This is a basic test to ensure error handling infrastructure works

    // For now, just test that Result types work
    let success: Result<i32> = Ok(42);
    let _failure: Result<i32> = Err("test error".to_string().into());

    assert!(success.is_ok());
    assert!(_failure.is_err());

    Ok(())
}
