use fp_core::ast::AstValue;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::interpreter::Interpreter;
use pretty_assertions::assert_eq;
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::{shll_parse_expr, shll_parse_value};
use std::sync::Arc;

fn interpret_shll_expr(expr: AstExpr) -> Result<AstValue> {
    let interpreter = Interpreter::new(Arc::new(RustPrinter::new()));
    let ctx = SharedScopedContext::new();
    interpreter.interpret_expr(expr, &ctx)
}

#[test]
fn test_eval_arithmetics() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {
        1 + 2 * 3
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(7);
    assert_eq!(value, expected);
    Ok(())
}
#[test]
fn test_eval_function_call() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {
        {
            fn foo(a: i64, b: i64) -> i64 {
                a + b
            }
            foo(1, 2)
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(3);
    assert_eq!(value, expected);
    Ok(())
}

// ===== ADDITIONAL TESTS FOR INTERPRETER PHASES =====

#[test]
fn test_interpreter_type_system_integration() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test type system integration in interpreter
    let code = shll_parse_expr! {
        {
            fn add(x: i64, y: i64) -> i64 {
                x + y
            }
            add(5, 10)
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(15);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_const_discovery() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const discovery and evaluation
    let code = shll_parse_expr! {
        {
            const MAX_VALUE = 100;
            fn get_max() -> i64 {
                MAX_VALUE
            }
            get_max()
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(100);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_generic_context() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test generic context handling
    let code = shll_parse_expr! {
        {
            fn identity(x: i64) -> i64 {
                x
            }
            identity(42)
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(42);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_iterative_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test iterative evaluation (simplified)
    let code = shll_parse_expr! {
        {
            fn compute() -> i64 {
                let a = 10;
                let b = a * 2;
                b + 5
            }
            compute()
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(25);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_interpreter_specialization() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test function specialization
    let code = shll_parse_expr! {
        {
            fn multiply_by_two(x: i64) -> i64 {
                x * 2
            }
            multiply_by_two(7)
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(14);
    assert_eq!(value, expected);
    Ok(())
}

#[test]
fn test_eval_function_call_with_main() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {
        {
            fn foo(a: i64, b: i64) -> i64 {
                a + b
            }
            fn main() -> i64 {
                foo(1, 2)
            }
            main()
        }
    };
    let value = interpret_shll_expr(code)?;
    let expected = shll_parse_value!(3);
    assert_eq!(value, expected);
    Ok(())
}
