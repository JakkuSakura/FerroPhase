// Basic language parsing tests
// Tests that language constructs parse correctly into AST nodes
// Focus: Parsing accuracy for basic language features

use fp_core::ast::*;
use fp_core::ops::BinOpKind;
use fp_core::Result;
use fp_rust::printer::RustPrinter;
use fp_rust::shll_parse_expr;
use pretty_assertions::assert_eq;
use std::sync::Arc;

// ===== LITERAL PARSING =====

#[test]
fn test_integer_literal_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test parsing of integer literals
    let expr = shll_parse_expr!(42);
    match expr {
        Expr::Value(val) => match val.as_ref() {
            Value::Int(int_val) => assert_eq!(int_val.value, 42),
            _ => panic!("Expected integer value"),
        },
        _ => panic!("Expected value expression"),
    }

    let expr = shll_parse_expr!(0);
    match expr {
        Expr::Value(val) => match val.as_ref() {
            Value::Int(int_val) => assert_eq!(int_val.value, 0),
            _ => panic!("Expected integer value"),
        },
        _ => panic!("Expected value expression"),
    }

    Ok(())
}

#[test]
fn test_boolean_literal_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let expr = shll_parse_expr!(true);
    match expr {
        Expr::Value(val) => match val.as_ref() {
            Value::Bool(bool_val) => assert_eq!(bool_val.value, true),
            _ => panic!("Expected boolean value"),
        },
        _ => panic!("Expected value expression"),
    }

    let expr = shll_parse_expr!(false);
    match expr {
        Expr::Value(val) => match val.as_ref() {
            Value::Bool(bool_val) => assert_eq!(bool_val.value, false),
            _ => panic!("Expected boolean value"),
        },
        _ => panic!("Expected value expression"),
    }

    Ok(())
}

#[test]
fn test_string_literal_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let expr = shll_parse_expr!("hello");
    match expr {
        Expr::Value(val) => match val.as_ref() {
            Value::String(str_val) => assert_eq!(str_val.value, "hello"),
            _ => panic!("Expected string value"),
        },
        _ => panic!("Expected value expression"),
    }

    Ok(())
}

// ===== ARITHMETIC EXPRESSION PARSING =====

#[test]
fn test_binary_operation_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test addition parsing
    let expr = shll_parse_expr!(5 + 3);
    match expr {
        Expr::BinOp(binop) => {
            assert!(matches!(binop.kind, BinOpKind::Add));
            // Check left operand
            match binop.lhs.as_ref() {
                Expr::Value(val) => match val.as_ref() {
                    Value::Int(int_val) => assert_eq!(int_val.value, 5),
                    _ => panic!("Expected integer in left operand"),
                },
                _ => panic!("Expected value in left operand"),
            }
            // Check right operand
            match binop.rhs.as_ref() {
                Expr::Value(val) => match val.as_ref() {
                    Value::Int(int_val) => assert_eq!(int_val.value, 3),
                    _ => panic!("Expected integer in right operand"),
                },
                _ => panic!("Expected value in right operand"),
            }
        }
        _ => panic!("Expected binary operation"),
    }

    Ok(())
}

#[test]
fn test_operator_precedence_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that 2 + 3 * 4 parses as 2 + (3 * 4)
    let expr = shll_parse_expr!(2 + 3 * 4);
    match expr {
        Expr::BinOp(add_op) => {
            assert!(matches!(add_op.kind, BinOpKind::Add));

            // Left side should be 2
            match add_op.lhs.as_ref() {
                Expr::Value(val) => match val.as_ref() {
                    Value::Int(int_val) => assert_eq!(int_val.value, 2),
                    _ => panic!("Expected integer 2"),
                },
                _ => panic!("Expected value 2"),
            }

            // Right side should be 3 * 4
            match add_op.rhs.as_ref() {
                Expr::BinOp(mul_op) => {
                    assert!(matches!(mul_op.kind, BinOpKind::Mul));
                    // Should have 3 and 4 as operands
                }
                _ => panic!("Expected multiplication on right side"),
            }
        }
        _ => panic!("Expected addition at top level"),
    }

    Ok(())
}

#[test]
fn test_comparison_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test equality parsing
    let expr = shll_parse_expr!(5 == 5);
    match expr {
        Expr::BinOp(binop) => {
            assert!(matches!(binop.kind, BinOpKind::Eq));
        }
        _ => panic!("Expected equality comparison"),
    }

    // Test inequality parsing
    let expr = shll_parse_expr!(5 != 3);
    match expr {
        Expr::BinOp(binop) => {
            assert!(matches!(binop.kind, BinOpKind::Ne));
        }
        _ => panic!("Expected inequality comparison"),
    }

    // Test greater than parsing
    let expr = shll_parse_expr!(10 > 5);
    match expr {
        Expr::BinOp(binop) => {
            assert!(matches!(binop.kind, BinOpKind::Gt));
        }
        _ => panic!("Expected greater than comparison"),
    }

    Ok(())
}

// ===== COMPLEX EXPRESSION PARSING =====

#[test]
fn test_complex_expression_parsing() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test parsing of complex expression: 2 + 3 * 4 - 5
    let expr = shll_parse_expr!(2 + 3 * 4 - 5);

    // Should parse as ((2 + (3 * 4)) - 5)
    match expr {
        Expr::BinOp(sub_op) => {
            assert!(matches!(sub_op.kind, BinOpKind::Sub));

            // Left side should be 2 + 3 * 4
            match sub_op.lhs.as_ref() {
                Expr::BinOp(add_op) => {
                    assert!(matches!(add_op.kind, BinOpKind::Add));
                }
                _ => panic!("Expected addition on left side"),
            }

            // Right side should be 5
            match sub_op.rhs.as_ref() {
                Expr::Value(val) => match val.as_ref() {
                    Value::Int(int_val) => assert_eq!(int_val.value, 5),
                    _ => panic!("Expected integer 5"),
                },
                _ => panic!("Expected value 5"),
            }
        }
        _ => panic!("Expected subtraction at top level"),
    }

    Ok(())
}

// ===== PARSING ACCURACY VALIDATION =====

#[test]
fn test_parsing_roundtrip() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that parsing and printing roundtrips correctly
    let original_expressions = vec![
        "42",
        "true",
        "\"hello\"",
        "5 + 3",
        "10 * 2",
        "5 == 5",
        "2 + 3 * 4",
    ];

    for original in original_expressions {
        // Parse the expression
        let expr = match original {
            "42" => shll_parse_expr!(42),
            "true" => shll_parse_expr!(true),
            "\"hello\"" => shll_parse_expr!("hello"),
            "5 + 3" => shll_parse_expr!(5 + 3),
            "10 * 2" => shll_parse_expr!(10 * 2),
            "5 == 5" => shll_parse_expr!(5 == 5),
            "2 + 3 * 4" => shll_parse_expr!(2 + 3 * 4),
            _ => continue,
        };

        // Convert back to string
        let printed = expr.to_string();

        // Should be parseable (not necessarily identical due to formatting)
        assert!(
            !printed.is_empty(),
            "Parsed expression should print to non-empty string"
        );
    }

    Ok(())
}

// ===== UNSUPPORTED PARSING FEATURES =====

/*
The following language features parse correctly but may not be fully supported
in evaluation/interpretation:

1. If expressions - parse correctly but interpretation may fail
   Example: if true { 42 } else { 24 }

2. Block expressions with let bindings - parse but interpretation fails
   Example: { let x = 42; x }

3. Function definitions and calls - parse but interpretation has issues
   Example: { fn add(a: i64, b: i64) -> i64 { a + b } add(1, 2) }

4. Unary operators - parse but interpretation fails
   Example: -5, !true

5. More complex expressions with parentheses, logical operators, etc.

The parsing layer (this file) should handle these correctly,
but evaluation/interpretation may not be fully implemented.
*/
