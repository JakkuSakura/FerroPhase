use common::*;
use lang_core::ast::*;
use lang_core::context::SharedScopedContext;
use lang_core::register_threadlocal_serializer;
use lang_mips::emitter::MipsEmitter;
use lang_mips::instruction::MipsInstruction;
use rust_lang::{shll_parse_expr, RustSerde};
use std::sync::Arc;

fn emit_mips_shll_expr(expr: Expr) -> Result<Vec<MipsInstruction>> {
    let ctx = SharedScopedContext::new();
    let mut emitter = MipsEmitter::new();

    let ret = emitter.emit_expr(&expr, &ctx)?;
    for ins in &ret.instructions {
        println!("{}", ins);
    }
    println!("ret: {}", ret.ret);
    Ok(ret.instructions)
}

#[test]
fn test_mips_emit_add() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        1 + 2 * 3
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}

#[test]
fn test_mips_emit_loop() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        loop {}
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}
#[test]
fn test_mips_emit_if() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        if 1 {
            2
        } else {
            3
        }
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}

#[test]
fn test_mips_emit_loop_if() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        loop {
            if 1 {
                2
            } else {
                3
            }
        }
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}

#[test]
fn test_mips_emit_comp() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        1 < 2
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}

#[test]
fn test_mips_emit_func() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustSerde::new()));

    let code = shll_parse_expr! {
        {
            fn foo() -> i32 {
                1
            }
        }
    };
    let _value = emit_mips_shll_expr(code)?;

    Ok(())
}
