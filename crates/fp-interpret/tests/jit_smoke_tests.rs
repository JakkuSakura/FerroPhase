use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{ItemKind, NodeKind, Value};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_jit::{JitConfig, JitKey, JitSession};
use fp_lang::FerroFrontend;

fn find_function_sig(ast: &fp_core::ast::Node, name: &str) -> fp_core::ast::FunctionSignature {
    let NodeKind::File(file) = &ast.kind else {
        panic!("expected file node");
    };
    for item in &file.items {
        if let ItemKind::DefFunction(func) = &item.kind {
            if func.name.as_str() == name {
                return func.sig.clone();
            }
        }
    }
    panic!("function '{}' not found in AST", name);
}

#[test]
fn jit_compiles_hot_function() -> Result<()> {
    let source = r#"
fn hot_add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let mut sum = 0;
    sum = hot_add(1, 2);
    sum
}
"#;

    let frontend = FerroFrontend::new();
    let result = frontend.parse(source, Some(Path::new("<jit-smoke>")))?;
    let mut ast = result.ast;
    let hot_sig = find_function_sig(&ast, "hot_add");

    let ctx = SharedScopedContext::new();
    let jit = Arc::new(JitSession::new(JitConfig {
        hot_threshold: 1,
        ..JitConfig::default()
    })?);
    let options = InterpreterOptions {
        mode: InterpreterMode::Runtime,
        macro_parser: result.macro_parser,
        intrinsic_normalizer: result.intrinsic_normalizer,
        jit: Some(Arc::clone(&jit)),
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
    assert_eq!(value, Value::int(3));

    let jit_key = JitKey::for_function("hot_add".to_string(), &hot_sig);
    assert!(
        jit.registry().contains(&jit_key),
        "expected jit registry to contain compiled hot_add"
    );

    Ok(())
}
