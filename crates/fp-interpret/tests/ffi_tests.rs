use std::path::Path;

use fp_core::ast::Value;
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;

fn interpret_and_run(source: &str) -> Result<Value> {
    let frontend = FerroFrontend::new();
    let result = frontend.parse(source, Some(Path::new("<ffi-test>")))?;
    let mut ast = result.ast;

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::RunTime,
        macro_parser: result.macro_parser,
        intrinsic_normalizer: result.intrinsic_normalizer,
        ..InterpreterOptions::default()
    };

    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);

    let value = interpreter
        .execute_main()
        .expect("expected main to be available");
    let outcome = interpreter.take_outcome();
    assert!(
        !outcome.has_errors,
        "unexpected interpreter errors: {:#?}",
        outcome.diagnostics
    );

    Ok(value)
}

#[test]
fn calls_libc_strlen_via_ffi() -> Result<()> {
    let source = r#"
extern "C" fn strlen(s: &std::ffi::CStr) -> i64;

fn main() {
    strlen("hello")
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, Value::int(5));
    Ok(())
}

#[test]
fn allocates_and_frees_with_libc_malloc_free() -> Result<()> {
    let source = r#"
extern "C" fn malloc(size: usize) -> &u8;
extern "C" fn free(ptr: &u8);

fn main() {
    let ptr = malloc(16);
    free(ptr);
    0
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, Value::int(0));
    Ok(())
}
