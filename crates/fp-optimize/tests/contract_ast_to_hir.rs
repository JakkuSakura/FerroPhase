use fp_core::error::Result as OptimizeResult;
use fp_core::hir::{self, ItemKind};
use fp_optimize::transformations::{HirGenerator, IrTransform};
use fp_rust::parser::RustParser;
use std::path::PathBuf;

mod support;

#[test]
fn transforms_literal_expression_into_main_function() -> OptimizeResult<()> {
    let ast_expr = support::ast::literal_expr(42);
    let mut generator = HirGenerator::new();

    let program = generator.transform(&ast_expr)?;

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
    use fp_core::ast::{Expr, ExprTry};

    let mut generator = HirGenerator::new();
    let unsupported = Expr::Try(ExprTry {
        expr: Box::new(Expr::unit()),
    });
    let result = generator.transform(&unsupported);
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
    let program = generator.transform(&ast_file)?;

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
    let program = generator.transform(&ast_file)?;

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
