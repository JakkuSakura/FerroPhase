use common::*;
use lang_core::context::SharedScopedContext;
use lang_core::expr::*;
use lang_core::register_threadlocal_serializer;
use mips::emitter::MipsEmitter;
use proc_macro2::TokenStream;
use rust_lang::{shll_parse_expr, RustSerde};
use std::sync::Arc;

fn emit_mips_shll_expr(mut expr: Expr) -> Result<TokenStream> {
    let ctx = SharedScopedContext::new();
    let emitter = MipsEmitter::new();

    let expr = emitter.emit_expr(expr, &ctx)?;

    Ok(expr)
}

#[test]
fn test_specialize_arithmetics() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        1 + 2 * 3
    };
    let value = emit_mips_shll_expr(code)?;
    let expected = shll_parse_expr!(7);
    assert_eq!(value.to_string(), expected.to_string());
    Ok(())
}
#[test]
fn test_specialize_function_call() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {{
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        foo(1, 2)
    }};
    let value = emit_mips_shll_expr(code)?;
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
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {{
        fn foo(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() {
            foo(1, 2)
        }
        main()
    }};
    let value = emit_mips_shll_expr(code)?;
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
