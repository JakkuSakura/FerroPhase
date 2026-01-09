use fp_core::error::Result as OptimizeResult;
use fp_core::hir::{self, FormatTemplatePart, ItemKind, StmtKind};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_optimize::transformations::HirGenerator;
use fp_rust::parser::RustParser;
use std::path::PathBuf;

mod support;

#[test]
fn transforms_literal_expression_into_main_function() -> OptimizeResult<()> {
    let ast_expr = support::ast::literal_expr(42);
    let mut generator = HirGenerator::new();

    let program = generator.transform_expr(&ast_expr)?;

    assert_eq!(program.items.len(), 1);
    let item = &program.items[0];
    match &item.kind {
        ItemKind::Function(func) => {
            assert_eq!(func.sig.name, "main");
            assert_eq!(func.sig.inputs.len(), 0);
            assert!(!func.is_const);

            let body = func.body.as_ref().expect("main should have a body");
            assert_eq!(body.params.len(), 0);
            support::assertions::assert_hir_integer(&body.value, 42);
        }
        kind => panic!("expected function item, found {:?}", kind),
    }

    Ok(())
}

#[test]
fn propagates_unimplemented_expression_error() {
    use fp_core::ast::{Expr, ExprKind, ExprTry};

    let mut generator = HirGenerator::new();
    let unsupported: Expr = ExprKind::Try(ExprTry {
        expr: Box::new(Expr::unit()),
    })
    .into();
    let result = generator.transform_expr(&unsupported);
    assert!(result.is_err());
}

#[test]
fn lowers_module_exports_and_use_aliases() -> OptimizeResult<()> {
    let source = r#"
        pub mod math {
            pub fn add(x: i32, y: i32) -> i32 { x + y }
        }

        use math::add as sum;

        pub fn call_sum(a: i32, b: i32) -> i32 {
            sum(a, b)
        }
    "#;

    let parser = RustParser::new();
    let syn_file = syn::parse_file(source).expect("valid Rust source");
    let ast_file = parser
        .parse_file_content(PathBuf::from("<memory>"), syn_file)
        .expect("AST parsing succeeds");

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    let add_item = program
        .items
        .iter()
        .find(|item| {
            matches!(
                &item.kind,
                ItemKind::Function(func) if func.sig.name == "math::add"
            )
        })
        .expect("math::add present in program");

    let call_item = program
        .items
        .iter()
        .find(|item| {
            matches!(
                &item.kind,
                ItemKind::Function(func) if func.sig.name == "call_sum"
            )
        })
        .expect("call_sum present in program");

    assert_eq!(add_item.visibility, hir::Visibility::Public);
    assert_eq!(call_item.visibility, hir::Visibility::Public);

    let add_def_id = add_item.def_id;

    if let ItemKind::Function(func) = &call_item.kind {
        let body = func.body.as_ref().expect("call_sum has a body");
        match &body.value.kind {
            hir::ExprKind::Call(callee, args) => {
                assert_eq!(args.len(), 2);
                if let hir::ExprKind::Path(path) = &callee.kind {
                    assert_eq!(path.segments.len(), 1);
                    assert_eq!(path.segments[0].name, "sum");
                    assert_eq!(path.res, Some(hir::Res::Def(add_def_id)));
                } else {
                    panic!("expected alias to lower into a path expression");
                }
            }
            other => panic!("expected call expression, found {:?}", other),
        }
    } else {
        panic!("call_sum should remain a function item");
    }

    Ok(())
}

#[test]
fn reexports_visible_to_child_modules() -> OptimizeResult<()> {
    let source = r#"
        pub mod math {
            pub fn add(x: i32, y: i32) -> i32 { x + y }
        }

        pub use math::add;

        pub mod callers {
            pub fn call(a: i32, b: i32) -> i32 {
                super::add(a, b)
            }
        }
    "#;

    let parser = RustParser::new();
    let syn_file = syn::parse_file(source).expect("valid Rust source");
    let ast_file = parser
        .parse_file_content(PathBuf::from("<memory>"), syn_file)
        .expect("AST parsing succeeds");

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    let add_item = program
        .items
        .iter()
        .find(|item| {
            matches!(
                &item.kind,
                ItemKind::Function(func) if func.sig.name == "math::add"
            )
        })
        .expect("math::add present in program");

    let callers_item = program
        .items
        .iter()
        .find(|item| {
            matches!(
                &item.kind,
                ItemKind::Function(func) if func.sig.name == "callers::call"
            )
        })
        .expect("callers::call present in program");

    let add_def_id = add_item.def_id;

    if let ItemKind::Function(func) = &callers_item.kind {
        let body = func.body.as_ref().expect("call has a body");
        match &body.value.kind {
            hir::ExprKind::Call(callee, args) => {
                assert_eq!(args.len(), 2);
                if let hir::ExprKind::Path(path) = &callee.kind {
                    assert_eq!(path.segments.last().unwrap().name, "add");
                    assert_eq!(path.res, Some(hir::Res::Def(add_def_id)));
                } else {
                    panic!("expected call to lower with a path callee");
                }
            }
            other => panic!("expected call expression, found {:?}", other),
        }
    } else {
        panic!("callers::call should remain a function item");
    }

    Ok(())
}

fn transform_source(source: &str) -> OptimizeResult<hir::Program> {
    let parser = RustParser::new();
    let syn_file = syn::parse_file(source).expect("valid Rust source");
    let mut ast_file = parser
        .parse_file_content(PathBuf::from("<memory>"), syn_file)
        .expect("AST parsing succeeds");

    // Lower builtin macros and normalize intrinsic forms before HIR generation.
    let mut node = fp_core::ast::Node::from(fp_core::ast::NodeKind::File(ast_file));
    fp_core::intrinsics::normalize_intrinsics_with(
        &mut node,
        &fp_rust::normalization::RustIntrinsicNormalizer,
    )
    .expect("intrinsic normalization");
    let ast_file = match node.kind() {
        fp_core::ast::NodeKind::File(f) => f.clone(),
        _ => unreachable!("expected file node after normalization"),
    };

    let mut generator = HirGenerator::new();
    generator.transform_file(&ast_file)
}

fn find_function<'a>(program: &'a hir::Program, name: &str) -> &'a hir::Function {
    program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            ItemKind::Function(func) if func.sig.name == name => Some(func),
            _ => None,
        })
        .unwrap_or_else(|| panic!("function {} should exist", name))
}

#[test]
fn lowers_println_macro_into_intrinsic_call() -> OptimizeResult<()> {
    let program = transform_source(
        r#"
        fn main() {
            println!("value={} and count={} ", 42, 7);
        }
    "#,
    )?;

    let main_fn = find_function(&program, "main");
    let body = main_fn.body.as_ref().expect("main has body");
    let block = match &body.value.kind {
        hir::ExprKind::Block(block) => block,
        other => panic!("expected function body block, found {:?}", other),
    };
    assert_eq!(block.stmts.len(), 1, "println expands to one statement");

    let stmt = &block.stmts[0];
    let expr = match &stmt.kind {
        StmtKind::Semi(expr) => expr,
        other => panic!(
            "expected println to lower into a semi statement, found {:?}",
            other
        ),
    };

    let call = match &expr.kind {
        hir::ExprKind::IntrinsicCall(call) => call,
        other => panic!("expected intrinsic call, found {:?}", other),
    };

    assert_eq!(call.kind, IntrinsicCallKind::Println);
    let template = match &call.payload {
        IntrinsicCallPayload::Format { template } => template,
        other => panic!("println payload should be format template, got {:?}", other),
    };

    assert!(
        matches!(template.parts.first(), Some(FormatTemplatePart::Literal(lit)) if lit.contains("value=")),
        "expected literal prefix in println template"
    );
    assert_eq!(
        template.args.len(),
        2,
        "println forwards positional arguments"
    );

    Ok(())
}

#[test]
fn lowers_print_macro_into_intrinsic_call() -> OptimizeResult<()> {
    let program = transform_source(
        r#"
        fn main() {
            print!("prefix: {}", 3.14);
        }
    "#,
    )?;

    let main_fn = find_function(&program, "main");
    let body = main_fn.body.as_ref().expect("main has body");
    let block = match &body.value.kind {
        hir::ExprKind::Block(block) => block,
        other => panic!("expected block in function body, found {:?}", other),
    };
    assert_eq!(block.stmts.len(), 1);

    let expr = match &block.stmts[0].kind {
        StmtKind::Semi(expr) => expr,
        other => panic!("expected statement expression, got {:?}", other),
    };

    let call = match &expr.kind {
        hir::ExprKind::IntrinsicCall(call) => call,
        other => panic!("expected intrinsic call, found {:?}", other),
    };

    assert_eq!(call.kind, IntrinsicCallKind::Print);
    match &call.payload {
        IntrinsicCallPayload::Format { template } => {
            assert_eq!(template.args.len(), 1);
            assert!(
                matches!(
                    template.parts.first(),
                    Some(FormatTemplatePart::Literal(lit)) if lit == "prefix: "
                ),
                "expected literal prefix in print template"
            );
        }
        other => panic!("print payload should be format template, got {:?}", other),
    }

    Ok(())
}

#[test]
fn lowers_sizeof_and_field_count_intrinsics() -> OptimizeResult<()> {
    let program = transform_source(
        r#"
        struct Point { x: i32, y: i32 }

        const SIZE: usize = sizeof!(Point);
        const FIELDS: usize = field_count!(Point);
    "#,
    )?;

    let mut saw_sizeof = false;
    let mut saw_field_count = false;

    for item in &program.items {
        if let ItemKind::Const(konst) = &item.kind {
            let expr = &konst.body.value;
            if let hir::ExprKind::IntrinsicCall(call) = &expr.kind {
                match call.kind {
                    IntrinsicCallKind::SizeOf => {
                        saw_sizeof = true;
                        assert!(matches!(call.payload, IntrinsicCallPayload::Args { .. }));
                    }
                    IntrinsicCallKind::FieldCount => {
                        saw_field_count = true;
                        assert!(matches!(call.payload, IntrinsicCallPayload::Args { .. }));
                    }
                    _ => {}
                }
            }
        }
    }

    assert!(saw_sizeof, "expected sizeof! to lower into intrinsic call");
    assert!(
        saw_field_count,
        "expected field_count! to lower into intrinsic call"
    );

    Ok(())
}
