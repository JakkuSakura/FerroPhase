use super::*;
use fp_core::ast;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use std::collections::HashMap;

fn ident(name: &str) -> ast::Ident {
    ast::Ident::new(name)
}

fn int_ty() -> ast::Ty {
    ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64))
}

fn ty_ident(name: &str) -> ast::Ty {
    ast::Ty::ident(ident(name))
}

fn make_struct(name: &str, fields: Vec<(&str, ast::Ty)>) -> ast::Item {
    let fields = fields
        .into_iter()
        .map(|(name, ty)| ast::StructuralField::new(ident(name), ty))
        .collect();
    ast::Item::from(ast::ItemKind::DefStruct(ast::ItemDefStruct::new(
        ident(name),
        fields,
    )))
}

fn make_fn(
    name: &str,
    params: Vec<(ast::Ident, ast::Ty)>,
    ret: ast::Ty,
    body: ast::Expr,
) -> ast::Item {
    let func = ast::ItemDefFunction::new_simple(ident(name), body.into())
        .with_params(params)
        .with_ret_ty(ret);
    ast::Item::from(ast::ItemKind::DefFunction(func))
}

fn cfg_target_os_attr(value: &str) -> ast::Attribute {
    let cfg_name = ast::Path::from_ident(ident("cfg"));
    let target_name = ast::Path::from_ident(ident("target_os"));
    let value_expr = ast::Expr::value(ast::Value::string(value.to_string()));
    let meta = ast::AttrMeta::List(ast::AttrMetaList {
        name: cfg_name,
        items: vec![ast::AttrMeta::NameValue(ast::AttrMetaNameValue {
            name: target_name,
            value: value_expr.into(),
        })],
    });
    ast::Attribute {
        style: ast::AttrStyle::Outer,
        meta,
    }
}

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
    let array_ty = ast::Ty::Array(ast::TypeArray {
        elem: Box::new(int_ty()),
        len: Box::new(ast::Expr::value(ast::Value::int(3))),
    });
    let index_expr = ast::Expr::from(ast::ExprKind::Index(ast::ExprIndex {
        span: Span::null(),
        obj: Box::new(ast::Expr::ident(ident("values"))),
        index: Box::new(ast::Expr::ident(ident("idx"))),
    }));
    let body = ast::Expr::block(ast::ExprBlock::new_expr(index_expr));
    let items = vec![make_fn(
        "pick",
        vec![(ident("values"), array_ty), (ident("idx"), ty_ident("usize"))],
        int_ty(),
        body,
    )];

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
fn cfg_filters_items_by_target_os() -> Result<()> {
    let mut linux_fn = make_fn(
        "linux_only",
        Vec::new(),
        int_ty(),
        ast::Expr::value(ast::Value::int(1)),
    );
    let mut mac_fn = make_fn(
        "mac_only",
        Vec::new(),
        int_ty(),
        ast::Expr::value(ast::Value::int(2)),
    );

    if let ast::ItemKind::DefFunction(def) = linux_fn.kind_mut() {
        def.attrs.push(cfg_target_os_attr("linux"));
    }
    if let ast::ItemKind::DefFunction(def) = mac_fn.kind_mut() {
        def.attrs.push(cfg_target_os_attr("macos"));
    }

    let ast_file = ast::File {
        path: "cfg.fp".into(),
        items: vec![linux_fn, mac_fn],
    };

    let mut generator = HirGenerator::new();
    generator.set_target_triple(Some("x86_64-apple-darwin"));
    let program = generator.transform_file(&ast_file)?;

    let names = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::ItemKind::Function(func) => Some(func.sig.name.as_str().to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert!(names.contains(&"mac_only".to_string()));
    assert!(!names.contains(&"linux_only".to_string()));
    Ok(())
}

#[test]
fn transform_type_expr_invoke_to_hir_path() -> Result<()> {
    let mut generator = HirGenerator::new();
    let target = ast::ExprInvokeTarget::Function(ast::Name::Ident(ident("Result")));
    let arg = ast::Expr::path(ast::Path::plain(vec![ident("hir"), ident("GenericArgs")]));
    let invoke = ast::ExprInvoke {
        span: Span::null(),
        target,
        args: vec![arg],
        kwargs: Vec::new(),
    };
    let ty = ast::Ty::expr(ast::Expr::from(ast::ExprKind::Invoke(invoke)));
    let lowered = generator.transform_type_to_hir(&ty)?;

    let hir::TypeExprKind::Path(path) = &lowered.kind else {
        return Err(crate::error::optimization_error(
            "expected type path from invoke expression".to_string(),
        ));
    };
    assert_eq!(path.segments.len(), 1);
    let seg = &path.segments[0];
    assert_eq!(seg.name.as_str(), "Result");
    let args = seg.args.as_ref().ok_or_else(|| {
        crate::error::optimization_error("expected generic args on Result".to_string())
    })?;
    assert_eq!(args.args.len(), 1);
    let hir::GenericArg::Type(arg_ty) = &args.args[0] else {
        return Err(crate::error::optimization_error(
            "expected type generic arg".to_string(),
        ));
    };
    let hir::TypeExprKind::Path(arg_path) = &arg_ty.kind else {
        return Err(crate::error::optimization_error(
            "expected type path for generic arg".to_string(),
        ));
    };
    assert_eq!(arg_path.segments.len(), 2);
    assert_eq!(arg_path.segments[0].name.as_str(), "hir");
    assert_eq!(arg_path.segments[1].name.as_str(), "GenericArgs");

    Ok(())
}

#[test]
fn transform_intrinsic_container_to_hir() -> Result<()> {
    let mut generator = HirGenerator::new();
    let container = ast::ExprIntrinsicContainer::VecElements {
        elements: vec![
            ast::Expr::value(ast::Value::int(1)),
            ast::Expr::value(ast::Value::int(2)),
        ],
    };
    let expr = ast::Expr::from(ast::ExprKind::IntrinsicContainer(container));
    let lowered = generator.transform_expr_to_hir(&expr)?;

    let hir::ExprKind::Array(elements) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "expected array from intrinsic container".to_string(),
        ));
    };
    assert_eq!(elements.len(), 2);

    Ok(())
}

#[test]
fn transform_file_with_function_and_struct() -> Result<()> {
    let point = make_struct(
        "Point",
        vec![("x", int_ty()), ("y", int_ty())],
    );
    let add_body = ast::Expr::from(ast::ExprKind::BinOp(ast::ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(ast::Expr::ident(ident("a"))),
        rhs: Box::new(ast::Expr::ident(ident("b"))),
    }));
    let add = make_fn(
        "add",
        vec![(ident("a"), int_ty()), (ident("b"), int_ty())],
        int_ty(),
        ast::Expr::block(ast::ExprBlock::new_expr(add_body)),
    );
    let items = vec![point, add];

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
    let container = make_struct("Container", vec![("value", int_ty())]);
    let mut method = ast::ItemDefFunction::new_simple(
        ident("get"),
        ast::Expr::block(ast::ExprBlock::new_expr(ast::Expr::from(
            ast::ExprKind::Select(ast::ExprSelect {
                span: Span::null(),
                obj: Box::new(ast::Expr::ident(ident("self"))),
                field: ident("value"),
                select: ast::ExprSelectType::Field,
            }),
        )))
        .into(),
    );
    method.sig.receiver = Some(ast::FunctionParamReceiver::Ref);
    method.sig.ret_ty = Some(int_ty());
    let impl_block = ast::ItemImpl::new_ident(
        ident("Container"),
        vec![ast::Item::from(ast::ItemKind::DefFunction(method))],
    );

    let mut identity = ast::ItemDefFunction::new_simple(
        ident("identity"),
        ast::Expr::block(ast::ExprBlock::new_expr(ast::Expr::ident(ident("x")))).into(),
    );
    identity.sig.generics_params = vec![ast::GenericParam {
        name: ident("T"),
        bounds: ast::TypeBounds::any(),
    }];
    identity.sig.params = vec![ast::FunctionParam::new(ident("x"), ty_ident("T"))];
    identity.sig.ret_ty = Some(ty_ident("T"));

    let items = vec![
        container,
        ast::Item::from(ast::ItemKind::Impl(impl_block)),
        ast::Item::from(ast::ItemKind::DefFunction(identity)),
    ];

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
    let stmt_b = ast::BlockStmt::Let(ast::StmtLet::new_simple(
        ident("b"),
        ast::Expr::ident(ident("a")),
    ));
    let stmt_c = ast::BlockStmt::Let(ast::StmtLet::new_simple(
        ident("c"),
        ast::Expr::ident(ident("b")),
    ));
    let sum_expr = ast::Expr::from(ast::ExprKind::BinOp(ast::ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: BinOpKind::Add,
        lhs: Box::new(ast::Expr::ident(ident("c"))),
        rhs: Box::new(ast::Expr::ident(ident("a"))),
    }));
    let inner_block = ast::Expr::block(ast::ExprBlock::new_stmts_expr(
        vec![stmt_c],
        sum_expr,
    ));
    let outer_body = ast::Expr::block(ast::ExprBlock::new_stmts_expr(
        vec![stmt_b],
        inner_block,
    ));
    let items = vec![make_fn(
        "outer",
        vec![(ident("a"), int_ty())],
        int_ty(),
        outer_body,
    )];

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
                    collect_paths(&arg.value, out);
                }
            }
            hir::ExprKind::MethodCall(receiver, _, args) => {
                collect_paths(receiver, out);
                for arg in args {
                    collect_paths(&arg.value, out);
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
            hir::ExprKind::Match(scrutinee, arms) => {
                collect_paths(scrutinee, out);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        collect_paths(guard, out);
                    }
                    collect_paths(&arm.body, out);
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
            hir::ExprKind::Reference(reference) => collect_paths(&reference.expr, out),
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
            hir::ExprKind::FormatString(_) => {}
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
