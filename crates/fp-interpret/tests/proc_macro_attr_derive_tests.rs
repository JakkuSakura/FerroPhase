use std::path::Path;

use fp_core::ast::{ItemKind, Node, NodeKind};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;

fn interpret_source(source: &str) -> Result<Node> {
    let frontend = FerroFrontend::new();
    let result = frontend.parse(source, Some(Path::new("<proc-macro-test>")))?;
    let mut ast = result.ast;
    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        macro_parser: result.macro_parser,
        ..InterpreterOptions::default()
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);
    Ok(ast)
}

#[test]
fn expands_attribute_proc_macro_on_struct() -> Result<()> {
    let source = r#"
#[proc_macro_attribute]
const fn tag(
    _args: std::proc_macro::TokenStream,
    input: std::proc_macro::TokenStream,
) -> std::proc_macro::TokenStream {
    input
}

#[tag]
struct Demo {
    value: i64,
}
"#;

    let ast = interpret_source(source)?;
    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("expected file node"),
    };

    let count = file
        .items
        .iter()
        .filter(|item| matches!(item.kind(), ItemKind::DefStruct(def) if def.name.as_str() == "Demo"))
        .count();
    assert_eq!(count, 1, "expected a single Demo struct after expansion");

    Ok(())
}

#[test]
fn expands_derive_proc_macro_on_struct() -> Result<()> {
    let source = r#"
#[proc_macro_derive(MakeDemo)]
const fn make_demo(_input: std::proc_macro::TokenStream) -> std::proc_macro::TokenStream {
    std::proc_macro::token_stream_from_str("fn generated() {}")
}

#[derive(MakeDemo)]
struct Demo {}
"#;

    let ast = interpret_source(source)?;
    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("expected file node"),
    };

    let has_demo = file
        .items
        .iter()
        .any(|item| matches!(item.kind(), ItemKind::DefStruct(def) if def.name.as_str() == "Demo"));
    let has_generated = file.items.iter().any(|item| {
        matches!(item.kind(), ItemKind::DefFunction(def) if def.name.as_str() == "generated")
    });

    assert!(has_demo, "expected Demo struct to remain after derive expansion");
    assert!(has_generated, "expected generated function from derive macro");

    Ok(())
}
