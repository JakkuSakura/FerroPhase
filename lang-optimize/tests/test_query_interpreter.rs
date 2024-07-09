use std::sync::Arc;

use common::*;
use pretty_assertions::assert_eq;

use lang_core::ast::*;
use lang_core::ctx::Context;
use lang_core::register_threadlocal_serializer;
use lang_optimize::pass::InterpreterPass;
use rust_lang::{shll_parse_expr, shll_parse_value, RustSerde};

fn interpret_shll_expr(expr: AstExpr) -> Result<Value> {
    let interpreter = InterpreterPass::new(Arc::new(RustSerde::new()));
    let mut ctx = Context::new();
    ctx.value = Arc::new(interpreter.clone());
    ctx.ty = Arc::new(interpreter.clone());
    ctx.value.get_value_from_expr(&ctx, &expr)
}

#[test]
fn test_eval_arithmetics() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

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
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

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

#[test]
fn test_eval_function_call_with_main() -> Result<()> {
    setup_logs(LogLevel::Debug)?;
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

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
