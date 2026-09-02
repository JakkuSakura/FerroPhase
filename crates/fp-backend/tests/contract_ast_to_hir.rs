use fp_backend::transformations::AstToHirLowerer;
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{FixedPackageProvider, PackageProvider};
use fp_core::ast::package::{AstPackage, PackageId, PackageItem};
use fp_core::ast::path::InPackagePath;
use fp_core::ast::program::AstProgram;
use fp_core::error::Result as OptimizeResult;
use fp_core::hir::{self, FormatTemplatePart, ItemKind, StmtKind};
use fp_core::intrinsics::{CallKind, IntrinsicKind};
use fp_core::lir::LirDataLayout;
use fp_core::ops::BinOpKind;

mod support;

fn ident(name: &str) -> fp_core::ast::Ident {
    fp_core::ast::Ident::new(name)
}

fn int_ty() -> fp_core::ast::Ty {
    fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I32))
}

fn ty_ident(name: &str) -> fp_core::ast::Ty {
    fp_core::ast::Ty::ident(ident(name))
}

fn call_expr(path: &[&str], args: Vec<fp_core::ast::Expr>) -> fp_core::ast::Expr {
    let segments = path.iter().map(|s| ident(s)).collect();
    let target = fp_core::ast::ExprInvokeTarget::Function(fp_core::ast::Name::path(
        fp_core::ast::Path::plain(segments),
    ));
    fp_core::ast::Expr::from(fp_core::ast::ExprKind::Invoke(fp_core::ast::ExprInvoke {
        span: fp_core::span::Span::null(),
        target,
        args,
        kwargs: Vec::new(),
    }))
}

fn make_fn(
    name: &str,
    params: Vec<(fp_core::ast::Ident, fp_core::ast::Ty)>,
    ret: fp_core::ast::Ty,
    body: fp_core::ast::Expr,
) -> fp_core::ast::Item {
    // A body that's already block-shaped (`{ stmt; stmt; ... }`) keeps its own
    // statements; wrapping it again via `ExprBlock::new_expr` would nest it as
    // a tail expression instead, losing its statements from the function's
    // own block.
    let block = match body.kind() {
        fp_core::ast::ExprKind::Block(block) => block.clone(),
        _ => fp_core::ast::ExprBlock::new_expr(body),
    };
    let func = fp_core::ast::ItemDefFunction::new_simple(ident(name), block)
        .with_params(params)
        .with_ret_ty(ret);
    fp_core::ast::Item::from(fp_core::ast::ItemKind::DefFunction(func))
}

/// Wraps a file's items as a one-member package, obtained via a real
/// `PackageProvider` (`FixedPackageProvider`, wrapping an already-built
/// `AstPackage` — no real frontend/disk parsing involved here) followed
/// by `AstProgram::begin_package`, then lowers it with
/// `transform_package` — `ast::File`'s `path` field carries no information
/// `transform_package` needs.
fn transform_file(file: fp_core::ast::File) -> OptimizeResult<hir::HirPackage> {
    let package_id = PackageId::new("test");
    let source = AstPackage::new(
        package_id.clone(),
        "test",
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), "test"),
        vec![fp_core::ast::Module {
            attrs: Vec::new(),
            name: fp_core::ast::Ident::new(""),
            items: file,
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }],
    );
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    let workspace = AstProgram::new(std::sync::Arc::new(provider));
    let data_layout = LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid test data layout");
    let package = workspace.begin_package(package_id, loaded, data_layout);
    let package = package.borrow();
    let mut generator = AstToHirLowerer::new(
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    generator.transform_package(&package)
}

#[test]
fn transforms_literal_expression_into_main_function() -> OptimizeResult<()> {
    let ast_expr = support::ast::literal_expr(42);
    let mut generator = AstToHirLowerer::new(
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );

    let program = generator.transform_expr(&ast_expr)?;

    assert_eq!(program.items.len(), 1);
    let item = &program.items[0];
    match &item.kind {
        ItemKind::Function(func) => {
            assert_eq!(func.sig.name, "main");
            assert_eq!(func.sig.inputs.len(), 0);
            assert!(!func.is_const);

            let body = func.body.as_ref().expect("main should have a body");
            support::assertions::assert_hir_integer(
                body.expr.as_deref().expect("body has a tail expression"),
                42,
            );
        }
        kind => panic!("expected function item, found {:?}", kind),
    }

    Ok(())
}

#[test]
fn preserves_try_expression_for_backend_lowering() -> OptimizeResult<()> {
    use fp_core::ast::{Expr, ExprKind, ExprTry};

    let mut generator = AstToHirLowerer::new(
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let try_expr: Expr = ExprKind::Try(ExprTry {
        span: fp_core::span::Span::null(),
        expr: Box::new(Expr::unit()),
        catches: Vec::new(),
        elze: None,
        finally: None,
    })
    .into();

    let program = generator.transform_expr(&try_expr)?;

    let main_item = program
        .items
        .iter()
        .find(|item| matches!(&item.kind, ItemKind::Function(func) if func.sig.name == "main"))
        .expect("main function present");

    let ItemKind::Function(main_fn) = &main_item.kind else {
        panic!("main item should be a function");
    };
    let body = main_fn.body.as_ref().expect("main should have a body");
    let tail = body.expr.as_deref().expect("body has a tail expression");
    assert!(
        matches!(tail.kind, hir::ExprKind::Try(_)),
        "try should remain a first-class HIR node for MIR lowering",
    );

    Ok(())
}

#[test]
fn lowers_defer_statement_before_hir_conversion() -> OptimizeResult<()> {
    use fp_core::ast::{BlockStmt, Expr, ExprBlock, ExprKind, StmtDefer};

    let expr = Expr::new(ExprKind::Block(ExprBlock::new_stmts(vec![
        BlockStmt::Defer(StmtDefer {
            span: fp_core::span::Span::null(),
            expr: Box::new(call_expr(&["cleanup"], Vec::new())),
        }),
        BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(Expr::unit()).with_semicolon(false)),
    ])));
    let file = fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![
            make_fn(
                "cleanup",
                Vec::new(),
                fp_core::ast::Ty::unit(),
                Expr::unit(),
            ),
            fp_core::ast::Item::from(fp_core::ast::ItemKind::Expr(expr)),
        ],
    };

    let program = transform_file(file)?;
    assert!(
        program.items.iter().any(|item| {
            matches!(&item.kind, ItemKind::Function(func) if func.sig.name == "cleanup")
        }),
        "cleanup function should still lower",
    );

    Ok(())
}

#[test]
fn lowers_module_exports_and_use_aliases() -> OptimizeResult<()> {
    let add_body =
        fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Add,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("x"))),
            rhs: Box::new(fp_core::ast::Expr::ident(ident("y"))),
        }));
    let add_fn = make_fn(
        "add",
        vec![(ident("x"), int_ty()), (ident("y"), int_ty())],
        int_ty(),
        fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_expr(add_body)),
    );
    let math_module =
        fp_core::ast::Item::from(fp_core::ast::ItemKind::Module(fp_core::ast::Module {
            attrs: Vec::new(),
            name: ident("math"),
            items: vec![add_fn],
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }));

    let import_tree = fp_core::ast::ItemImportTree::Path(fp_core::ast::ItemImportPath {
        segments: vec![
            fp_core::ast::ItemImportTree::Ident(ident("math")),
            fp_core::ast::ItemImportTree::Rename(fp_core::ast::ItemImportRename {
                from: ident("add"),
                to: ident("sum"),
            }),
        ],
    });
    let import =
        fp_core::ast::Item::from(fp_core::ast::ItemKind::Import(fp_core::ast::ItemImport {
            attrs: Vec::new(),
            visibility: fp_core::ast::Visibility::Private,
            style: fp_core::ast::ItemImportStyle::Plain,
            tree: import_tree,
        }));

    let call_sum_body = fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_expr(call_expr(
        &["sum"],
        vec![
            fp_core::ast::Expr::ident(ident("a")),
            fp_core::ast::Expr::ident(ident("b")),
        ],
    )));
    let call_sum = make_fn(
        "call_sum",
        vec![(ident("a"), int_ty()), (ident("b"), int_ty())],
        int_ty(),
        call_sum_body,
    );

    let program = transform_file(fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![math_module, import, call_sum],
    })?;

    let add_item = program
        .items
        .iter()
        .find(|item| matches!(&item.kind, ItemKind::Function(func) if func.sig.name == "add"))
        .expect("math::add present in program");
    assert_eq!(
        program
            .def_paths
            .get(&add_item.def_id)
            .map(|p| p.to_segments()),
        Some(vec!["math".to_string(), "add".to_string()]),
        "add's qualified path should be recorded in def_paths, not baked into sig.name"
    );

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

    let add_def_id = add_item.def_id.clone();

    if let ItemKind::Function(func) = &call_item.kind {
        let body = func.body.as_ref().expect("call_sum has a body");
        let tail = body
            .expr
            .as_deref()
            .expect("call_sum body has a tail expression");
        let expr = match &tail.kind {
            hir::ExprKind::Call(_, _) => tail,
            hir::ExprKind::Block(block) => block
                .expr
                .as_deref()
                .expect("block should hold call expression"),
            other => panic!("expected call expression, found {:?}", other),
        };
        match &expr.kind {
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
    let add_body =
        fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Add,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("x"))),
            rhs: Box::new(fp_core::ast::Expr::ident(ident("y"))),
        }));
    let add_fn = make_fn(
        "add",
        vec![(ident("x"), int_ty()), (ident("y"), int_ty())],
        int_ty(),
        fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_expr(add_body)),
    );
    let math_module =
        fp_core::ast::Item::from(fp_core::ast::ItemKind::Module(fp_core::ast::Module {
            attrs: Vec::new(),
            name: ident("math"),
            items: vec![add_fn],
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }));

    let reexport_tree = fp_core::ast::ItemImportTree::Path(fp_core::ast::ItemImportPath {
        segments: vec![
            fp_core::ast::ItemImportTree::Ident(ident("math")),
            fp_core::ast::ItemImportTree::Ident(ident("add")),
        ],
    });
    let reexport =
        fp_core::ast::Item::from(fp_core::ast::ItemKind::Import(fp_core::ast::ItemImport {
            attrs: Vec::new(),
            visibility: fp_core::ast::Visibility::Public,
            style: fp_core::ast::ItemImportStyle::Plain,
            tree: reexport_tree,
        }));

    let call_body = fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_expr(call_expr(
        &["super", "add"],
        vec![
            fp_core::ast::Expr::ident(ident("a")),
            fp_core::ast::Expr::ident(ident("b")),
        ],
    )));
    let call_fn = make_fn(
        "call",
        vec![(ident("a"), int_ty()), (ident("b"), int_ty())],
        int_ty(),
        call_body,
    );
    let callers_module =
        fp_core::ast::Item::from(fp_core::ast::ItemKind::Module(fp_core::ast::Module {
            attrs: Vec::new(),
            name: ident("callers"),
            items: vec![call_fn],
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }));

    let program = transform_file(fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![math_module, reexport, callers_module],
    })?;

    let add_item = program
        .items
        .iter()
        .find(|item| matches!(&item.kind, ItemKind::Function(func) if func.sig.name == "add"))
        .expect("math::add present in program");
    assert_eq!(
        program
            .def_paths
            .get(&add_item.def_id)
            .map(|p| p.to_segments()),
        Some(vec!["math".to_string(), "add".to_string()]),
        "add's qualified path should be recorded in def_paths, not baked into sig.name"
    );

    let callers_item = program
        .items
        .iter()
        .find(|item| matches!(&item.kind, ItemKind::Function(func) if func.sig.name == "call"))
        .expect("callers::call present in program");
    assert_eq!(
        program
            .def_paths
            .get(&callers_item.def_id)
            .map(|p| p.to_segments()),
        Some(vec!["callers".to_string(), "call".to_string()]),
        "call's qualified path should be recorded in def_paths, not baked into sig.name"
    );

    let add_def_id = add_item.def_id.clone();

    if let ItemKind::Function(func) = &callers_item.kind {
        let body = func.body.as_ref().expect("call has a body");
        let tail = body
            .expr
            .as_deref()
            .expect("call body has a tail expression");
        let expr = match &tail.kind {
            hir::ExprKind::Call(_, _) => tail,
            hir::ExprKind::Block(block) => block
                .expr
                .as_deref()
                .expect("block should hold call expression"),
            other => panic!("expected call expression, found {:?}", other),
        };
        match &expr.kind {
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

fn find_function<'a>(program: &'a hir::HirPackage, name: &str) -> &'a hir::Function {
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
    let template = fp_core::ast::Expr::from(fp_core::ast::ExprKind::FormatString(
        fp_core::ast::ExprStringTemplate {
            parts: vec![
                fp_core::ast::FormatTemplatePart::Literal("value=".to_string()),
                fp_core::ast::FormatTemplatePart::Placeholder(fp_core::ast::FormatPlaceholder {
                    arg_ref: fp_core::ast::FormatArgRef::Implicit,
                    format_spec: None,
                }),
                fp_core::ast::FormatTemplatePart::Literal(" and count=".to_string()),
                fp_core::ast::FormatTemplatePart::Placeholder(fp_core::ast::FormatPlaceholder {
                    arg_ref: fp_core::ast::FormatArgRef::Implicit,
                    format_spec: None,
                }),
                fp_core::ast::FormatTemplatePart::Literal(" ".to_string()),
            ],
        },
    ));
    let call = fp_core::ast::Expr::from(fp_core::ast::ExprKind::IntrinsicCall(
        fp_core::ast::ExprIntrinsicCall::new(
            CallKind::Println,
            vec![
                template,
                fp_core::ast::Expr::value(fp_core::ast::Value::int(42)),
                fp_core::ast::Expr::value(fp_core::ast::Value::int(7)),
            ],
            Vec::new(),
        ),
    ));
    let body = fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_stmts(vec![
        fp_core::ast::BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(call).with_semicolon(true)),
    ]));
    let main_fn = make_fn("main", vec![], fp_core::ast::Ty::unit(), body);
    let program = transform_file(fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![main_fn],
    })?;

    let main_fn = find_function(&program, "main");
    let block = main_fn.body.as_ref().expect("main has body");
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

    assert_eq!(call.kind, IntrinsicKind::Println);
    let template = match call.callargs.first().map(|arg| &arg.value.kind) {
        Some(hir::ExprKind::FormatString(template)) => template,
        other => panic!("println expects format template argument, got {:?}", other),
    };

    assert!(
        matches!(template.parts.first(), Some(FormatTemplatePart::Literal(lit)) if lit.contains("value=")),
        "expected literal prefix in println template"
    );
    assert_eq!(
        call.callargs.len() - 1,
        2,
        "println forwards positional arguments"
    );

    Ok(())
}

#[test]
fn lowers_print_macro_into_intrinsic_call() -> OptimizeResult<()> {
    let template = fp_core::ast::Expr::from(fp_core::ast::ExprKind::FormatString(
        fp_core::ast::ExprStringTemplate {
            parts: vec![
                fp_core::ast::FormatTemplatePart::Literal("prefix: ".to_string()),
                fp_core::ast::FormatTemplatePart::Placeholder(fp_core::ast::FormatPlaceholder {
                    arg_ref: fp_core::ast::FormatArgRef::Implicit,
                    format_spec: None,
                }),
            ],
        },
    ));
    let call = fp_core::ast::Expr::from(fp_core::ast::ExprKind::IntrinsicCall(
        fp_core::ast::ExprIntrinsicCall::new(
            CallKind::Print,
            vec![
                template,
                fp_core::ast::Expr::value(fp_core::ast::Value::decimal(3.14)),
            ],
            Vec::new(),
        ),
    ));
    let body = fp_core::ast::Expr::block(fp_core::ast::ExprBlock::new_stmts(vec![
        fp_core::ast::BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(call).with_semicolon(true)),
    ]));
    let main_fn = make_fn("main", vec![], fp_core::ast::Ty::unit(), body);
    let program = transform_file(fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![main_fn],
    })?;

    let main_fn = find_function(&program, "main");
    let block = main_fn.body.as_ref().expect("main has body");
    assert_eq!(block.stmts.len(), 1);

    let expr = match &block.stmts[0].kind {
        StmtKind::Semi(expr) => expr,
        other => panic!("expected statement expression, got {:?}", other),
    };

    let call = match &expr.kind {
        hir::ExprKind::IntrinsicCall(call) => call,
        other => panic!("expected intrinsic call, found {:?}", other),
    };

    assert_eq!(call.kind, IntrinsicKind::Print);
    let template = match call.callargs.first().map(|arg| &arg.value.kind) {
        Some(hir::ExprKind::FormatString(template)) => template,
        other => panic!("print expects format template argument, got {:?}", other),
    };
    assert_eq!(call.callargs.len() - 1, 1);
    assert!(
        matches!(
            template.parts.first(),
            Some(FormatTemplatePart::Literal(lit)) if lit == "prefix: "
        ),
        "expected literal prefix in print template"
    );

    Ok(())
}

#[test]
fn lowers_sizeof_and_field_count_intrinsics() -> OptimizeResult<()> {
    let point = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefStruct(
        fp_core::ast::ItemDefStruct::new(
            ident("Point"),
            vec![
                fp_core::ast::StructuralField::new(ident("x"), int_ty()),
                fp_core::ast::StructuralField::new(ident("y"), int_ty()),
            ],
        ),
    ));

    let sizeof_call = fp_core::ast::Expr::from(fp_core::ast::ExprKind::IntrinsicCall(
        fp_core::ast::ExprIntrinsicCall::new(
            CallKind::SizeOf,
            vec![fp_core::ast::Expr::ident(ident("Point"))],
            Vec::new(),
        ),
    ));
    let field_count_call = fp_core::ast::Expr::from(fp_core::ast::ExprKind::IntrinsicCall(
        fp_core::ast::ExprIntrinsicCall::new(
            CallKind::FieldCount,
            vec![fp_core::ast::Expr::ident(ident("Point"))],
            Vec::new(),
        ),
    ));
    let size_const = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("SIZE"),
            ty: Some(ty_ident("usize")),
            value: Box::new(sizeof_call),
        },
    ));
    let fields_const = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("FIELDS"),
            ty: Some(ty_ident("usize")),
            value: Box::new(field_count_call),
        },
    ));

    let program = transform_file(fp_core::ast::File {
        path: "<memory>".into(),
        attrs: Vec::new(),
        items: vec![point, size_const, fields_const],
    })?;

    let mut saw_sizeof = false;
    let mut saw_field_count = false;

    for item in &program.items {
        if let ItemKind::Const(konst) = &item.kind {
            let expr = &konst.body.value;
            if let hir::ExprKind::IntrinsicCall(call) = &expr.kind {
                match call.kind {
                    IntrinsicKind::SizeOf => {
                        saw_sizeof = true;
                        assert_eq!(call.callargs.len(), 1);
                    }
                    IntrinsicKind::FieldCount => {
                        saw_field_count = true;
                        assert_eq!(call.callargs.len(), 1);
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
