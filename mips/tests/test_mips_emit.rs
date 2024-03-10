use common::*;
use lang_core::ast::*;
use lang_core::context::SharedScopedContext;
use lang_core::register_threadlocal_serializer;
use mips::emitter::MipsEmitter;
use mips::instruction::MipsInstruction;
use rust_lang::{shll_parse_expr, RustSerde};
use std::sync::Arc;

fn emit_mips_shll_expr(expr: Expr) -> Result<Vec<MipsInstruction>> {
    let ctx = SharedScopedContext::new();
    let emitter = MipsEmitter::new();

    let ret = emitter.emit_expr(&expr, &ctx)?;
    for ins in &ret.instructions {
        println!("{}", ins);
    }
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
