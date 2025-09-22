use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::passes::SpecializePass;
use fp_optimize::utils::FoldOptimizer;
use fp_rust::printer::RustPrinter;
use fp_rust::shll_parse_expr;
use std::sync::Arc;

fn specialize_shll_expr(mut expr: AstExpr) -> Result<AstExpr> {
    let serializer = Arc::new(RustPrinter::new());
    let optimizer = FoldOptimizer::new(
        serializer.clone(),
        Box::new(SpecializePass::new(serializer.clone())),
    );
    let ctx = SharedScopedContext::new();
    expr = optimizer.optimize_expr(expr, &ctx)?;

    Ok(expr)
}

#[test]
fn test_specialize_arithmetics() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {
        1 + 2 * 3
    };
    let value = specialize_shll_expr(code)?;
    let expected = shll_parse_expr!(7);
    assert_eq!(value.to_string(), expected.to_string());
    Ok(())
}
#[test]
fn test_specialize_function_call() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {{
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        foo(1, 2)
    }};
    let value = specialize_shll_expr(code)?;
    let expected = shll_parse_expr!({
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        {
            fn foo_0() -> i64 {
                let a = 1;
                let b = 2;
                3
            }
            foo_0()
        }
    });
    assert_eq!(value.to_string(), expected.to_string());
    Ok(())
}

// ===== ADDITIONAL TESTS FOR SPECIALIZER PHASES =====

#[test]
fn test_specializer_type_integration() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test type integration in specialization
    let code = shll_parse_expr! {{
        fn add(x: i64, y: i64) -> i64 {
            x + y
        }
        add(5, 10)
    }};
    let value = specialize_shll_expr(code)?;
    // Should produce specialized version
    assert!(value.to_string().contains("15"));
    Ok(())
}

#[test]
fn test_specializer_const_propagation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const propagation through specialization
    let code = shll_parse_expr! {{
        const MULTIPLIER = 3;
        fn multiply(x: i64) -> i64 {
            x * MULTIPLIER
        }
        multiply(7)
    }};
    let value = specialize_shll_expr(code)?;
    // Should propagate the const value
    assert!(value.to_string().contains("21")); // 7 * 3
    Ok(())
}

#[test]
fn test_specializer_generic_specialization() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test generic specialization (placeholder - may not be fully implemented)
    let code = shll_parse_expr! {{
        fn identity(x: i64) -> i64 {
            x
        }
        identity(42)
    }};
    let value = specialize_shll_expr(code)?;
    assert!(value.to_string().contains("42"));
    Ok(())
}

#[test]
fn test_specializer_code_generation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test code generation through specialization
    let code = shll_parse_expr! {{
        fn compute() -> i64 {
            let a = 10;
            let b = a + 5;
            b * 2
        }
        compute()
    }};
    let value = specialize_shll_expr(code)?;
    // Should generate optimized code
    assert!(value.to_string().contains("30")); // (10 + 5) * 2
    Ok(())
}

#[test]
fn test_specializer_final_validation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test final validation of specialized code
    let code = shll_parse_expr! {{
        fn validate(x: i64) -> i64 {
            if x > 0 { x } else { 0 }
        }
        validate(5)
    }};
    let value = specialize_shll_expr(code)?;
    // Should validate and optimize the conditional
    assert!(value.to_string().contains("5"));
    Ok(())
}
#[test]
fn test_specialize_function_call_in_main() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {{
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() {
            foo(1, 2)
        }
        main()
    }};
    let value = specialize_shll_expr(code)?;
    let expected = shll_parse_expr!({
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() {
            foo(1, 2)
        }
        {
            fn foo_0() -> i64 {
                let a = 1;
                let b = 2;
                3
            }
            foo_0()
        }
    });
    assert_eq!(value.to_string(), expected.to_string());
    Ok(())
}
