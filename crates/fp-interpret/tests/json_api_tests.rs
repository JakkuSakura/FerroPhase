use std::path::Path;
use std::fs;

use fp_core::ast::{Ident, Item, ItemKind, Module, Node, NodeKind, Value, Visibility};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;

fn interpret_and_run(source: &str) -> Result<i64> {
    // The interpreter (and JSON lowering) can be stack-intensive; run each test with a larger
    // stack to avoid platform-default test thread limits.
    let source = source.to_string();
    let handle = std::thread::Builder::new()
        .name("fp-json-api-test".to_string())
        .stack_size(32 * 1024 * 1024)
        .spawn(move || -> Result<i64> {
            let frontend = FerroFrontend::new();
            let result = frontend.parse(source.as_str(), Some(Path::new("<json-test>")))?;
            let mut ast = result.ast;
            inject_core_std(&frontend, &mut ast)?;

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

            match value {
                Value::Int(int_value) => Ok(int_value.value),
                other => Err(fp_core::error::Error::from(format!(
                    "expected main to return int, got {other}"
                ))),
            }
        })
        .map_err(|err| fp_core::error::Error::from(format!("spawn test thread: {err}")))?;

    handle.join().map_err(|_| {
        fp_core::error::Error::from("json api test thread panicked")
    })?
}

fn inject_core_std(frontend: &FerroFrontend, ast: &mut Node) -> Result<()> {
    let NodeKind::File(file) = ast.kind_mut() else {
        return Ok(());
    };
    ensure_std_module(file);
    let std_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../fp-lang/src/std");
    let std_files = ["json/mod.fp", "option/mod.fp"];
    for relative in std_files {
        let source_path = std_root.join(relative);
        let module_path = module_path_for_core_std(relative)?;
        merge_source_items(frontend, file, &module_path, &source_path)?;
    }
    Ok(())
}

fn module_path_for_core_std(embedded_path: &str) -> Result<Vec<&str>> {
    let without_prefix = embedded_path
        .strip_prefix("std/")
        .unwrap_or(embedded_path);
    if without_prefix == "mod.fp" {
        return Ok(Vec::new());
    }
    let without_mod = without_prefix
        .strip_suffix("/mod.fp")
        .or_else(|| without_prefix.strip_suffix(".fp"))
        .ok_or_else(|| {
            fp_core::error::Error::from(format!(
                "embedded core std path must end in .fp: {embedded_path}"
            ))
        })?;
    let segments = without_mod
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    Ok(segments)
}

fn merge_source_items(
    frontend: &FerroFrontend,
    user_file: &mut fp_core::ast::File,
    module_path: &[&str],
    source_path: &Path,
) -> Result<()> {
    let source = fs::read_to_string(source_path)?;
    let parsed = frontend
        .parse(source.as_str(), Some(source_path))
        .map_err(|err| {
            fp_core::error::Error::from(format!(
                "failed to parse std source {}: {}",
                source_path.display(),
                err
            ))
        })?;
    let NodeKind::File(file) = parsed.ast.kind else {
        return Err(fp_core::error::Error::from(
            "embedded std must be a file document",
        ));
    };
    let target = ensure_nested_module(user_file, module_path)?;
    target.items.extend(file.items);
    Ok(())
}

fn ensure_std_module(user_file: &mut fp_core::ast::File) {
    let has_std = user_file.items.iter().any(
        |item| matches!(item.kind(), ItemKind::Module(module) if module.name.as_str() == "std"),
    );
    if !has_std {
        user_file.items.insert(
            0,
            Item::from(ItemKind::Module(Module {
                attrs: Vec::new(),
                name: Ident::new("std"),
                items: Vec::new(),
                visibility: Visibility::Public,
                is_external: false,
            })),
        );
    }
}

fn ensure_nested_module<'a>(
    user_file: &'a mut fp_core::ast::File,
    module_path: &[&str],
) -> Result<&'a mut Module> {
    let std_module = user_file
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::Module(module) if module.name.as_str() == "std" => Some(module),
            _ => None,
        })
        .ok_or_else(|| fp_core::error::Error::from("missing std module"))?;
    let mut current = std_module;
    for segment in module_path {
        let existing_index = current.items.iter().position(|item| {
            matches!(item.kind(), ItemKind::Module(module) if module.name.as_str() == *segment)
        });
        let index = match existing_index {
            Some(index) => index,
            None => {
                current.items.push(Item::from(ItemKind::Module(Module {
                    attrs: Vec::new(),
                    name: Ident::new(*segment),
                    items: Vec::new(),
                    visibility: Visibility::Public,
                    is_external: false,
                })));
                current.items.len() - 1
            }
        };
        current = match current.items[index].kind_mut() {
            ItemKind::Module(module) => module,
            _ => unreachable!(),
        };
    }
    Ok(current)
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
    let ok_b = b;
    let ok_s = s == "ok";
    let ok_n = n == 1;
    let ok_a0 = a0 == 1;
    let ok_f_low = f > 1.4;
    let ok_f_high = f < 1.6;
    let ok = ok_b && ok_s && ok_n && ok_a0 && ok_f_low && ok_f_high;
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_value_simple() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"n\":1}");
    let n = value.get("n").unwrap().as_number().unwrap().as_i64().unwrap();
    if n == 1 { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_value_array_index() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"a\":[1,2]}");
    let a0 = value.get("a").unwrap().get_index(0).unwrap().as_number().unwrap().as_i64().unwrap();
    if a0 == 1 { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_parse_only() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let _value = json::parse("{\"n\":1}");
    1
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_number_api() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2,\"i\":-3,\"f\":2.25,\"big\":18446744073709551615}");
    let u = value.get("u").unwrap().as_number().unwrap();
    let i = value.get("i").unwrap().as_number().unwrap();
    let f = value.get("f").unwrap().as_number().unwrap();
    let big = value.get("big").unwrap().as_number().unwrap();
    let ok_u_type = u.is_u64();
    let ok_u_value = u.as_u64().unwrap() == 2;
    let ok_i_type = i.is_i64();
    let ok_i_value = i.as_i64().unwrap() == -3;
    let ok_f_type = f.is_f64();
    let ok_f_value = f.to_string() == "2.25";
    let ok_big_type = big.is_u64();
    let ok_big_value = big.as_u64().is_some();
    let ok_big_text = big.to_string() == "18446744073709551615";
    let ok = ok_u_type
        && ok_u_value
        && ok_i_type
        && ok_i_value
        && ok_f_type
        && ok_f_value
        && ok_big_type
        && ok_big_value
        && ok_big_text;
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_big_number_is_number() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"big\":18446744073709551615}");
    let ok = value.get("big").unwrap().is_number();
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_big_number_to_string() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"big\":18446744073709551615}");
    let big = value.get("big").unwrap().as_number().unwrap();
    let ok = big.to_string() == "18446744073709551615";
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_big_number_as_u64() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"big\":18446744073709551615}");
    let big = value.get("big").unwrap().as_number().unwrap();
    let ok = big.as_u64().is_some();
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_number_small() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2}");
    let u = value.get("u").unwrap().as_number().unwrap();
    let ok = u.as_u64().unwrap() == 2;
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_as_number_only() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2}");
    let _number = value.get("u").unwrap().as_number().unwrap();
    1
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_get_only() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2}");
    let _maybe = value.get("u");
    1
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
    Ok(())
}

#[test]
fn json_is_number_only() -> Result<()> {
    let source = r#"
use std::json;

fn main() {
    let value = json::parse("{\"u\":2}");
    let ok = value.get("u").unwrap().is_number();
    if ok { 1 } else { 0 }
}
"#;

    let value = interpret_and_run(source)?;
    assert_eq!(value, 1);
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
    assert_eq!(value, 1);
    Ok(())
}
