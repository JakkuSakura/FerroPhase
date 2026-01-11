use super::*;
use fp_core::ast::register_threadlocal_serializer;
use fp_rust::printer::RustPrinter;
use fp_rust::shll_parse_items;
use std::collections::HashMap;
use std::sync::Arc;

#[test]
fn test_hir_generator_creation() {
    let generator = HirGenerator::new();
    assert_eq!(generator.next_hir_id, 0);
    assert_eq!(generator.next_def_id, 0);
}

#[test]
fn test_simple_literal_creation() -> Result<()> {
    let mut generator = HirGenerator::new();
    let expr = generator.create_simple_literal(42);

    match expr.kind {
        hir::ExprKind::Literal(hir::Lit::Integer(value)) => {
            assert_eq!(value, 42);
        }
        _ => {
            return Err(crate::error::optimization_error(
                "Expected integer literal".to_string(),
            ));
        }
    }
    Ok(())
}

#[test]
fn test_simple_type_creation() -> Result<()> {
    let mut generator = HirGenerator::new();
    let ty = generator.create_simple_type("i32");

    match ty.kind {
        hir::TypeExprKind::Path(path) => {
            assert_eq!(path.segments[0].name.as_str(), "i32");
        }
        _ => {
            return Err(crate::error::optimization_error(
                "Expected path type".to_string(),
            ));
        }
    }
    Ok(())
}

#[test]
fn transform_slice_type_to_hir() -> Result<()> {
    let mut generator = HirGenerator::new();
    let slice_ty = ast::Ty::Slice(ast::TypeSlice {
        elem: Box::new(ast::Ty::Primitive(ast::TypePrimitive::Int(
            ast::TypeInt::I64,
        ))),
    });
    let lowered = generator.transform_type_to_hir(&slice_ty)?;
    assert!(matches!(lowered.kind, hir::TypeExprKind::Slice(_)));

    Ok(())
}

#[test]
fn transform_index_expression_to_hir() -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());

    let items = shll_parse_items! {
        fn pick(values: [i64; 3], idx: usize) -> i64 {
            values[idx]
        }
    };

    let ast_file = ast::File {
        path: "index.fp".into(),
        items,
    };

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    let pick = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "pick" => Some(func),
            _ => None,
        })
        .expect("pick function present");

    let body_expr = &pick.body.as_ref().expect("body present").value;
    let target_expr = match &body_expr.kind {
        hir::ExprKind::Block(block) => block.expr.as_deref().expect("expression present in block"),
        _ => body_expr,
    };

    assert!(matches!(target_expr.kind, hir::ExprKind::Index(_, _)));

    Ok(())
}

#[test]
fn transform_file_with_function_and_struct() -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());

    let items = shll_parse_items! {
        struct Point {
            x: i64,
            y: i64,
        }

        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
    };

    let ast_file = ast::File {
        path: "test.fp".into(),
        items,
    };

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    assert_eq!(program.items.len(), 2);
    let names: Vec<String> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) => Some(def.name.as_str().to_owned()),
            hir::ItemKind::Function(func) => Some(func.sig.name.as_str().to_owned()),
            _ => None,
        })
        .collect();

    assert!(names.contains(&"Point".to_string()));
    assert!(names.contains(&"add".to_string()));

    Ok(())
}

#[test]
fn transform_generic_function_and_method() -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());

    let items = shll_parse_items! {
        struct Container {
            value: i64,
        }

        impl Container {
            fn get(&self) -> i64 {
                self.value
            }
        }

        fn identity<T>(x: T) -> T {
            x
        }
    };

    let ast_file = ast::File {
        path: "generics.fp".into(),
        items,
    };

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    let identity = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "identity" => Some(func),
            _ => None,
        })
        .expect("identity function present");
    assert_eq!(identity.sig.generics.params.len(), 1);
    if let hir::TypeExprKind::Path(path) = &identity.sig.output.kind {
        assert!(
            matches!(path.res, Some(hir::Res::Local(_))),
            "generic return type should resolve to local generic param"
        );
    } else {
        panic!("expected path return type for identity function");
    }
    let param_ty = &identity.sig.inputs[0].ty;
    if let hir::TypeExprKind::Path(path) = &param_ty.kind {
        assert!(
            matches!(path.res, Some(hir::Res::Local(_))),
            "generic parameter type should resolve to local generic param"
        );
    } else {
        panic!("expected path param type for identity function parameter");
    }

    let impl_item = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
        .expect("impl block present");
    assert!(impl_item.trait_ty.is_none());

    let method = impl_item
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ImplItemKind::Method(func) => Some(func),
            _ => None,
        })
        .expect("method present");
    assert_eq!(method.sig.inputs.len(), 1);
    match &method.sig.inputs[0].pat.kind {
        hir::PatKind::Binding { name, .. } => assert_eq!(name.as_str(), "self"),
        other => panic!("expected self binding, got {other:?}"),
    }

    Ok(())
}

#[test]
fn transform_scoped_block_name_resolution() -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());

    let items = shll_parse_items! {
        fn outer(a: i64) -> i64 {
            let b = a;
            {
                let c = b;
                c + a
            }
        }
    };

    let ast_file = ast::File {
        path: "scopes.fp".into(),
        items,
    };

    let mut generator = HirGenerator::new();
    let program = generator.transform_file(&ast_file)?;

    let outer = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "outer" => Some(func),
            _ => None,
        })
        .expect("outer function present");

    let body = outer.body.as_ref().expect("outer function has body");

    let mut collected_paths: Vec<&hir::Path> = Vec::new();

    fn collect_paths<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a hir::Path>) {
        match &expr.kind {
            hir::ExprKind::Path(path) => out.push(path),
            hir::ExprKind::Binary(_, lhs, rhs) => {
                collect_paths(lhs, out);
                collect_paths(rhs, out);
            }
            hir::ExprKind::Unary(_, inner) => collect_paths(inner, out),
            hir::ExprKind::Call(func, args) => {
                collect_paths(func, out);
                for arg in args {
                    collect_paths(arg, out);
                }
            }
            hir::ExprKind::MethodCall(receiver, _, args) => {
                collect_paths(receiver, out);
                for arg in args {
                    collect_paths(arg, out);
                }
            }
            hir::ExprKind::FieldAccess(inner, _) => collect_paths(inner, out),
            hir::ExprKind::Struct(_, fields) => {
                for field in fields {
                    collect_paths(&field.expr, out);
                }
            }
            hir::ExprKind::If(cond, then_branch, else_branch) => {
                collect_paths(cond, out);
                collect_paths(then_branch, out);
                if let Some(else_expr) = else_branch {
                    collect_paths(else_expr, out);
                }
            }
            hir::ExprKind::Block(block) => collect_paths_from_block(block, out),
            hir::ExprKind::Let(_, _, Some(init)) => collect_paths(init, out),
            hir::ExprKind::Let(_, _, None) => {}
            hir::ExprKind::Assign(lhs, rhs) => {
                collect_paths(lhs, out);
                collect_paths(rhs, out);
            }
            hir::ExprKind::Return(expr_opt) | hir::ExprKind::Break(expr_opt) => {
                if let Some(expr) = expr_opt {
                    collect_paths(expr, out);
                }
            }
            hir::ExprKind::Loop(block) => collect_paths_from_block(block, out),
            hir::ExprKind::While(cond, block) => {
                collect_paths(cond, out);
                collect_paths_from_block(block, out);
            }
            hir::ExprKind::IntrinsicCall(call) => {
                for arg in &call.callargs {
                    collect_paths(&arg.value, out);
                }
            }
            hir::ExprKind::Cast(expr, _) => collect_paths(expr, out),
            hir::ExprKind::Array(elements) => {
                for elem in elements {
                    collect_paths(elem, out);
                }
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                collect_paths(elem, out);
                collect_paths(len, out);
            }
            hir::ExprKind::Index(base, index) => {
                collect_paths(base, out);
                collect_paths(index, out);
            }
            hir::ExprKind::Literal(_) | hir::ExprKind::Continue => {}
        }
    }

    fn collect_paths_from_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a hir::Path>) {
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    if let Some(init) = &local.init {
                        collect_paths(init, out);
                    }
                }
                hir::StmtKind::Item(item) => collect_paths_from_item(item, out),
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    collect_paths(expr, out);
                }
            }
        }
        if let Some(expr) = &block.expr {
            collect_paths(expr, out);
        }
    }

    fn collect_paths_from_item<'a>(item: &'a hir::Item, out: &mut Vec<&'a hir::Path>) {
        match &item.kind {
            hir::ItemKind::Function(func) => {
                if let Some(body) = &func.body {
                    collect_paths(&body.value, out);
                }
            }
            hir::ItemKind::Const(const_item) => collect_paths(&const_item.body.value, out),
            hir::ItemKind::Impl(impl_block) => {
                for impl_item in &impl_block.items {
                    if let hir::ImplItemKind::Method(method) = &impl_item.kind {
                        if let Some(body) = &method.body {
                            collect_paths(&body.value, out);
                        }
                    }
                }
            }
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => {}
        }
    }

    collect_paths(&body.value, &mut collected_paths);

    let mut name_to_paths: HashMap<String, Vec<&hir::Path>> = HashMap::new();

    for path in collected_paths {
        if let Some(segment) = path.segments.last() {
            name_to_paths
                .entry(segment.name.as_str().to_owned())
                .or_default()
                .push(path);
        }
    }

    for name in ["a", "b", "c"] {
        let paths = name_to_paths
            .get(name)
            .unwrap_or_else(|| panic!("expected paths for {name}"));
        assert!(
            paths
                .iter()
                .all(|path| matches!(path.res, Some(hir::Res::Local(_)))),
            "expected {name} to resolve to a local"
        );
    }

    Ok(())
}
