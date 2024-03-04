use common::assert_eq;
use common::*;
use common_lang::context::ScopedContext;
use common_lang::expr::*;
use common_lang::optimizer::FoldOptimizer;
use common_lang::passes::SpecializePass;
use common_lang::register_threadlocal_serializer;
use rust_lang::{shll_parse_expr, RustSerde};
use std::rc::Rc;

fn specialize_shll_expr(mut expr: Expr) -> Result<Expr> {
    let serializer = Rc::new(RustSerde::new());
    let optimizer = FoldOptimizer::new(
        serializer.clone(),
        Box::new(SpecializePass::new(serializer.clone())),
    );
    let ctx = ScopedContext::new().into_shared();
    expr = optimizer.optimize_expr(expr, &ctx)?;

    Ok(expr)
}

#[test]
fn test_specialize_arithmetics() -> Result<()> {
    register_threadlocal_serializer(Rc::new(RustSerde::new()));

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
    setup_logs(LogLevel::Debug)?;

    register_threadlocal_serializer(Rc::new(RustSerde::new()));

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
                a + b
            }
            foo_0()
        }
    });
    assert_eq!(value.to_string(), expected.to_string());
    Ok(())
}
