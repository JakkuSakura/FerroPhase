use std::path::Path;

use fp_core::ast::Value;
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;

fn interpret_and_run(source: &str) -> Result<Value> {
    let frontend = FerroFrontend::new();
    let result = frontend.parse(source, Some(Path::new("<json-test>")))?;
    let mut ast = result.ast;

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::Runtime,
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
fn json_value_accessors() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"n\":1,\"f\":1.5,\"s\":\"ok\",\"b\":true,\"a\":[1,2],\"o\":{\"k\":3}}");
    if !value.is_object() {
        return 0;
    }
    let n = value.get("n").unwrap().as_number().unwrap().as_i64().unwrap();
    let f = value.get("f").unwrap().as_number().unwrap().as_f64().unwrap();
    let s = value.get("s").unwrap().as_str().unwrap();
    let b = value.get("b").unwrap().as_bool().unwrap();
    let a0 = value.get("a").unwrap().get_index(0).unwrap().as_number().unwrap().as_i64().unwrap();
    let ok = b && s == "ok" && n == 1 && a0 == 1 && f > 1.4 && f < 1.6;
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, Value::int(1));
    Ok(())
}

#[test]
fn json_number_api() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2,\"i\":-3,\"f\":2.25}");
    let u = value.get("u").unwrap().as_number().unwrap();
    let i = value.get("i").unwrap().as_number().unwrap();
    let f = value.get("f").unwrap().as_number().unwrap();
    let ok = u.is_u64()
        && u.as_u64().unwrap() == 2
        && i.is_i64()
        && i.as_i64().unwrap() == -3
        && f.is_f64()
        && f.to_string() == "2.25";
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, Value::int(1));
    Ok(())
}

#[test]
fn json_object_helpers() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"a\":1}");
    let a = json::get_object_field(value, "a").as_number().unwrap().as_i64().unwrap();
    let missing = json::find_object_field(value, "missing");
    let ok = a == 1 && missing.is_null();
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, Value::int(1));
    Ok(())
}
