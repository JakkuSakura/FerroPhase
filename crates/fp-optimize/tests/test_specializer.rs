use common::*;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_optimize::pass::{FoldOptimizer, SpecializePass};
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::shll_parse_expr;
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
