use super::*;
use fp_core::ast;
use fp_core::frontend::LanguageFrontend;
use fp_core::intrinsics::IntrinsicNormalizationMode;
use fp_core::ast::path::QualifiedPath;
use fp_core::lir::LirDataLayout;
use fp_core::ops::BinOpKind;
use fp_core::package::graph::PackageGraph;
use fp_core::package::provider::{FixedPackageProvider, PackageProvider};
use fp_core::package::{PackageId, PackageSource};
use fp_core::span::Span;
use fp_core::workspace::WorkspaceContext;
use fp_typing::{ResolvedName, ResolvedNameNamespace, ResolvedNameTable};
use std::collections::HashMap;

fn test_data_layout() -> LirDataLayout {
    LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid test data layout")
}

/// Wraps bare `ast::Item`s (no real file/frontend involved) as a
/// one-member package, obtained the same way every real package is: via a
/// `PackageProvider` (`FixedPackageProvider`, which just hands back an
/// already-built `PackageSource`) followed by
/// `WorkspaceContext::begin_package` — never a hand-rolled
/// `CompiledPackage`.
fn package_from_items(items: Vec<ast::Item>) -> Result<fp_core::package::CompiledPackage> {
    let package_id = PackageId::new("test");
    let mut source = PackageSource::new(package_id.clone(), "test", PackageGraph::new(Vec::new()));
    source.items = items
        .into_iter()
        .map(|item| fp_core::package::PackageItem {
            path: QualifiedPath::new(Vec::new()),
            item,
        })
        .collect();
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| crate::error::optimization_error(e.to_string()))?;
    let workspace = WorkspaceContext::new(std::sync::Arc::new(provider));
    let package = workspace.begin_package(package_id, loaded, test_data_layout());
    let package = package.borrow().clone();
    Ok(package)
}

/// Like `package_from_items`, but tags every item with a nested
/// `module_path` (e.g. `["std", "sys", "stdio"]`) instead of the package
/// root — exercises `transform_package`'s per-`PackageItem`
/// `with_module_scope` push/pop, which `package_from_items`'s always-empty
/// path never does.
fn package_from_module_items(
    module_path: Vec<String>,
    items: Vec<ast::Item>,
) -> Result<fp_core::package::CompiledPackage> {
    let package_id = PackageId::new("test");
    let mut source = PackageSource::new(package_id.clone(), "test", PackageGraph::new(Vec::new()));
    source.items = items
        .into_iter()
        .map(|item| fp_core::package::PackageItem {
            path: QualifiedPath::new(module_path.clone()),
            item,
        })
        .collect();
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| crate::error::optimization_error(e.to_string()))?;
    let workspace = WorkspaceContext::new(std::sync::Arc::new(provider));
    let package = workspace.begin_package(package_id, loaded, test_data_layout());
    let package = package.borrow().clone();
    Ok(package)
}

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
    let func = ast::ItemDefFunction::new_simple(ident(name), ast::ExprBlock::new_expr(body))
        .with_params(params)
        .with_ret_ty(ret);
    ast::Item::from(ast::ItemKind::DefFunction(func))
}

#[test]
fn transform_expr_uses_typing_resolved_name_table() -> Result<()> {
    let expr = ast::Expr::name(ast::Name::from_ident(ident("VALUE")));

    let mut resolved_names = ResolvedNameTable::new();
    resolved_names.insert(
        expr.id(),
        ResolvedName {
            namespace: ResolvedNameNamespace::Value,
            path: QualifiedPath::new(vec!["module".to_string(), "VALUE".to_string()]),
        },
    );

    let mut generator = HirGenerator::new().with_resolved_names(resolved_names);
    let hir_expr = generator.transform_expr_to_hir(&expr)?;
    let hir::ExprKind::Path(path) = hir_expr.kind else {
        return Err(crate::error::optimization_error(
            "expected path expression".to_string(),
        ));
    };

    assert_eq!(path.segments.len(), 2);
    assert_eq!(path.segments[0].name.as_str(), "module");
    assert_eq!(path.segments[1].name.as_str(), "VALUE");
    Ok(())
}

#[test]
fn unqualified_lookup_does_not_scan_global_paths_by_suffix() {
    // Resolving a bare name against the *current* module's own qualified
    // entries (module_path + name) is intentional (lets a module's own
    // items reference each other unqualified). What this guards against is
    // resolving it against an unrelated *foreign* module's entries by
    // matching just the name's suffix.
    let mut generator = HirGenerator::new();
    generator.module_path = QualifiedPath::new(vec!["dependency".to_string()]);
    generator.record_type_symbol(
        "SharedType",
        hir::Res::Def(hir::DefId::new(hir::PackageId(7), 1)),
        &ast::Visibility::Public,
    );

    generator.module_path = QualifiedPath::new(vec!["consumer".to_string()]);
    assert_eq!(generator.resolve_value_symbol("SharedType"), None);
    assert_eq!(generator.resolve_type_symbol("SharedType"), None);
}

#[test]
fn compile_normalization_runs_during_ast_to_hir_lowering() -> Result<()> {
    let frontend = fp_lang::FerroFrontend::new();
    let parsed = frontend.parse_expr("println!(\"hello\")")?;
    let ast::ItemKind::Expr(expr) = parsed.ast.items[0].kind() else {
        return Err(crate::error::optimization_error(
            "expected parsed expression item".to_string(),
        ));
    };
    assert!(matches!(expr.kind(), ast::ExprKind::Macro(_)));

    let mut generator = HirGenerator::new().with_intrinsic_normalizer(
        fp_lang::FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile),
    );
    let lowered = generator.transform_expr_to_hir(expr)?;
    // `println!`/`print!`/`format!` are compiler intrinsics; unlike other
    // std-surfaced macros, they stay as a first-class `IntrinsicCall` node
    // rather than degrading to an ordinary call (see
    // `fp_lang::normalization`'s `compile_mode_std_path`).
    let hir::ExprKind::IntrinsicCall(call) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "expected println! to lower to an intrinsic call".to_string(),
        ));
    };
    // `println!` may now surface as either `CallKind::Intrinsic(Println)`
    // (a genuine low-level intrinsic) or `CallKind::Op(OpKind::Println)`
    // (the portable `#[op(...)]` tag) -- both mean the same thing here, so
    // compare via `intrinsic_kind()` rather than the raw `CallKind`.
    assert_eq!(
        call.kind.intrinsic_kind(),
        Some(fp_core::intrinsics::IntrinsicKind::Println)
    );
    Ok(())
}

#[test]
fn const_block_expr_lowers_to_dedicated_hir_node() -> Result<()> {
    let frontend = fp_lang::FerroFrontend::new();
    let parsed = frontend.parse_expr("const { 1 + 1 }")?;
    let ast::ItemKind::Expr(expr) = parsed.ast.items[0].kind() else {
        return Err(crate::error::optimization_error(
            "expected parsed expression item".to_string(),
        ));
    };
    assert!(matches!(expr.kind(), ast::ExprKind::ConstBlock(_)));

    let mut generator = HirGenerator::new();
    let lowered = generator.transform_expr_to_hir(expr)?;
    let hir::ExprKind::ConstBlock(const_block) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "const block must lower to a dedicated ConstBlock node, not a synthetic item"
                .to_string(),
        ));
    };
    let hir::ExprKind::Block(block) = const_block.body.kind else {
        return Err(crate::error::optimization_error(
            "expected const block body to lower its `{ ... }` to a HIR block".to_string(),
        ));
    };
    let tail = block.expr.expect("const block has a tail expression");
    assert!(matches!(
        tail.kind,
        hir::ExprKind::Binary(hir::BinOp::Add, _, _)
    ));
    Ok(())
}

#[test]
fn const_block_type_alias_produces_no_synthetic_item() -> Result<()> {
    let const_block_ty = ast::Ty::ConstBlock(ast::ExprConstBlock {
        span: Span::null(),
        collected_items: Vec::new(),
        expr: Box::new(ast::Expr::value(ast::Value::int(1))),
    });
    let type_item = ast::Item::from(ast::ItemKind::DefType(ast::ItemDefType {
        attrs: Vec::new(),
        visibility: ast::Visibility::Private,
        name: ident("X"),
        generics_params: Vec::new(),
        value: const_block_ty,
    }));

    let package = package_from_items(vec![type_item])?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

    assert!(
        program.items.is_empty(),
        "`type X = const {{ ... }};` must not synthesize a fake HIR item; uses of X \
         resolve via type_aliases substitution instead: {:?}",
        program.items
    );
    Ok(())
}

#[test]
fn nested_type_position_const_block_lowers_to_dedicated_hir_node() -> Result<()> {
    let const_block_ty = ast::Ty::ConstBlock(ast::ExprConstBlock {
        span: Span::null(),
        collected_items: Vec::new(),
        expr: Box::new(ast::Expr::value(ast::Value::int(2))),
    });

    let mut generator = HirGenerator::new();
    let lowered = generator.transform_type_to_hir(&const_block_ty)?;
    let hir::TypeExprKind::ConstBlock(body) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "nested type-position const block must lower to a dedicated ConstBlock node"
                .to_string(),
        ));
    };
    assert!(matches!(
        body.kind,
        hir::ExprKind::Literal(hir::Lit::Integer(2))
    ));
    Ok(())
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
        vec![
            (ident("values"), array_ty),
            (ident("idx"), ty_ident("usize")),
        ],
        int_ty(),
        body,
    )];

    let package = package_from_items(items)?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

    let pick = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "pick" => Some(func),
            _ => None,
        })
        .expect("pick function present");

    let body = pick.body.as_ref().expect("body present");
    let target_expr = body.expr.as_deref().expect("expression present in block");

    assert!(matches!(target_expr.kind, hir::ExprKind::Index(_, _)));

    Ok(())
}

fn range_expr(
    start: Option<ast::Expr>,
    limit: ast::ExprRangeLimit,
    end: Option<ast::Expr>,
) -> ast::Expr {
    ast::Expr::from(ast::ExprKind::Range(ast::ExprRange {
        span: Span::null(),
        start: start.map(Box::new),
        limit,
        end: end.map(Box::new),
        step: None,
    }))
}

#[test]
fn transform_slice_syntax_to_hir_slice_expr_preserves_bounds() -> Result<()> {
    let mut generator = HirGenerator::new();

    let base = ast::Expr::ident(ident("values"));
    let start = ast::Expr::ident(ident("i"));
    let end = ast::Expr::ident(ident("j"));

    let cases = vec![
        (None, ast::ExprRangeLimit::Exclusive, None, false, false),
        (
            Some(start.clone()),
            ast::ExprRangeLimit::Exclusive,
            None,
            true,
            false,
        ),
        (
            None,
            ast::ExprRangeLimit::Exclusive,
            Some(end.clone()),
            false,
            true,
        ),
        (
            Some(start.clone()),
            ast::ExprRangeLimit::Exclusive,
            Some(end.clone()),
            true,
            true,
        ),
        (
            None,
            ast::ExprRangeLimit::Inclusive,
            Some(end.clone()),
            false,
            true,
        ),
        (
            Some(start.clone()),
            ast::ExprRangeLimit::Inclusive,
            Some(end.clone()),
            true,
            true,
        ),
    ];

    for (range_start, limit, range_end, expect_start, expect_end) in cases {
        let inclusive = matches!(limit, ast::ExprRangeLimit::Inclusive);
        let slice_index = ast::Expr::from(ast::ExprKind::Index(ast::ExprIndex {
            span: Span::null(),
            obj: Box::new(base.clone()),
            index: Box::new(range_expr(range_start, limit, range_end)),
        }));
        let lowered = generator.transform_expr_to_hir(&slice_index)?;

        let hir::ExprKind::Slice(slice) = lowered.kind else {
            return Err(crate::error::optimization_error(format!(
                "expected HIR slice expr, got {:?}",
                lowered.kind
            )));
        };

        assert_eq!(slice.start.is_some(), expect_start);
        assert_eq!(slice.end.is_some(), expect_end);
        assert_eq!(slice.inclusive, inclusive);
    }

    Ok(())
}

#[test]
fn transform_await_expression_to_hir_passthrough() -> Result<()> {
    let mut generator = HirGenerator::new();
    let await_expr = ast::Expr::from(ast::ExprKind::Await(ast::ExprAwait {
        span: Span::null(),
        base: Box::new(ast::Expr::ident(ident("future"))),
    }));

    let lowered = generator.transform_expr_to_hir(&await_expr)?;
    match lowered.kind {
        hir::ExprKind::Path(path) => {
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].name.as_str(), "future");
        }
        other => {
            return Err(crate::error::optimization_error(format!(
                "expected await passthrough to path, got {:?}",
                other
            )));
        }
    }

    Ok(())
}

#[test]
fn transform_async_await_expression_to_hir_passthrough() -> Result<()> {
    let mut generator = HirGenerator::new();
    let await_expr = ast::Expr::from(ast::ExprKind::Await(ast::ExprAwait {
        span: Span::null(),
        base: Box::new(ast::Expr::ident(ident("future"))),
    }));
    let async_expr = ast::Expr::from(ast::ExprKind::Async(ast::ExprAsync {
        span: Span::null(),
        expr: Box::new(await_expr),
    }));

    let lowered = generator.transform_expr_to_hir(&async_expr)?;
    match lowered.kind {
        hir::ExprKind::Path(path) => {
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].name.as_str(), "future");
        }
        other => {
            return Err(crate::error::optimization_error(format!(
                "expected async/await passthrough to path, got {:?}",
                other
            )));
        }
    }

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

    let package = package_from_items(vec![linux_fn, mac_fn])?;
    let mut generator = HirGenerator::new();
    generator.set_target_triple(Some("x86_64-apple-darwin"));
    let program = generator.transform_package(&package)?;

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
    let result_def_id = hir::DefId::new(hir::PackageId(0), 1);
    // `Result` is defined in `std::result` and re-exported through the
    // prelude; only the prelude alias entry is needed here for the bare
    // `Result` reference below to resolve.
    generator.global_type_defs.insert(
        "std::prelude::Result".to_string(),
        SymbolEntry {
            res: hir::Res::Def(result_def_id),
            export: SymbolExport::Public,
            path: None,
        },
    );
    generator.load_default_prelude_defs();

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
    // Resolved via the prelude alias to the real `Result` def — the path
    // stays unqualified (matching how it was written), only `res` needs to
    // point at the correct definition.
    assert_eq!(path.segments.len(), 1);
    let seg = &path.segments[0];
    assert_eq!(seg.name.as_str(), "Result");
    assert_eq!(path.res, Some(hir::Res::Def(result_def_id)));
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
fn transform_package_with_function_and_struct() -> Result<()> {
    let point = make_struct("Point", vec![("x", int_ty()), ("y", int_ty())]);
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

    let package = package_from_items(items)?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

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
        ast::ExprBlock::new_expr(ast::Expr::from(ast::ExprKind::Select(ast::ExprSelect {
            span: Span::null(),
            obj: Box::new(ast::Expr::ident(ident("self"))),
            field: ident("value"),
            select: ast::ExprSelectType::Field,
        }))),
    );
    method.sig.receiver = Some(ast::FunctionParamReceiver::Ref);
    method.sig.ret_ty = Some(int_ty());
    let impl_block = ast::ItemImpl::new_ident(
        ident("Container"),
        vec![ast::Item::from(ast::ItemKind::DefFunction(method))],
    );

    let mut identity = ast::ItemDefFunction::new_simple(
        ident("identity"),
        ast::ExprBlock::new_expr(ast::Expr::ident(ident("x"))),
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

    let package = package_from_items(items)?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

    let identity = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "identity" => Some(func),
            _ => None,
        })
        .expect("identity function present");
    assert_eq!(identity.sig.generics.params.len(), 1);
    let generic_def_id = identity.sig.generics.params[0].def_id;
    if let hir::TypeExprKind::Path(path) = &identity.sig.output.kind {
        assert!(
            matches!(path.res, Some(hir::Res::Def(def_id)) if def_id == generic_def_id),
            "generic return type should resolve to its declared generic definition"
        );
    } else {
        panic!("expected path return type for identity function");
    }
    let param_ty = &identity.sig.inputs[0].ty;
    if let hir::TypeExprKind::Path(path) = &param_ty.kind {
        assert!(
            matches!(path.res, Some(hir::Res::Def(def_id)) if def_id == generic_def_id),
            "generic parameter type should resolve to its declared generic definition"
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

/// Same shape as `transform_generic_function_and_method`'s struct+impl
/// case, but the items live in a *nested* module path (e.g.
/// `["std", "sys", "stdio"]`, mirroring how `fp-rust`'s vendored real-std
/// provider tags every item) instead of the package root. `transform_package`
/// processes each `PackageItem` through its own independent
/// `with_module_scope` call — this must not lose a struct's own-module
/// registration by the time its `impl` (a separate top-level item) is
/// processed.
#[test]
fn transform_package_resolves_impl_self_type_in_nested_module_path() -> Result<()> {
    let container = make_struct("Container", vec![("value", int_ty())]);
    let mut method = ast::ItemDefFunction::new_simple(
        ident("get"),
        ast::ExprBlock::new_expr(ast::Expr::from(ast::ExprKind::Select(ast::ExprSelect {
            span: Span::null(),
            obj: Box::new(ast::Expr::ident(ident("self"))),
            field: ident("value"),
            select: ast::ExprSelectType::Field,
        }))),
    );
    method.sig.receiver = Some(ast::FunctionParamReceiver::Ref);
    method.sig.ret_ty = Some(int_ty());
    let impl_block = ast::ItemImpl::new_ident(
        ident("Container"),
        vec![ast::Item::from(ast::ItemKind::DefFunction(method))],
    );

    let items = vec![container, ast::Item::from(ast::ItemKind::Impl(impl_block))];

    let module_path = vec!["std".to_string(), "sys".to_string(), "stdio".to_string()];
    let package = package_from_module_items(module_path, items)?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

    let impl_item = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
        .expect("impl block present — self-type `Container` must resolve even in a nested module");

    let method = impl_item
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ImplItemKind::Method(func) => Some(func),
            _ => None,
        })
        .expect("method present");
    assert_eq!(method.sig.name.as_str(), "get");

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
    let inner_block = ast::Expr::block(ast::ExprBlock::new_stmts_expr(vec![stmt_c], sum_expr));
    let outer_body = ast::Expr::block(ast::ExprBlock::new_stmts_expr(vec![stmt_b], inner_block));
    let items = vec![make_fn(
        "outer",
        vec![(ident("a"), int_ty())],
        int_ty(),
        outer_body,
    )];

    let package = package_from_items(items)?;
    let mut generator = HirGenerator::new();
    let program = generator.transform_package(&package)?;

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
            hir::ExprKind::Query(_) => {}
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
            hir::ExprKind::Try(expr_try) => {
                collect_paths(&expr_try.expr, out);
                for catch in &expr_try.catches {
                    collect_paths(&catch.body, out);
                }
                if let Some(elze) = &expr_try.elze {
                    collect_paths(elze, out);
                }
                if let Some(finally) = &expr_try.finally {
                    collect_paths(finally, out);
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
            hir::ExprKind::For(_pat, iter, block) => {
                collect_paths(iter, out);
                collect_paths_from_block(block, out);
            }
            hir::ExprKind::With(context, body) => {
                collect_paths(context, out);
                collect_paths(body, out);
            }
            hir::ExprKind::IntrinsicCall(call) => {
                for arg in &call.callargs {
                    collect_paths(&arg.value, out);
                }
            }
            hir::ExprKind::Reference(reference) => collect_paths(&reference.expr, out),
            hir::ExprKind::Cast(expr, _) => collect_paths(expr, out),
            hir::ExprKind::Array(elements) | hir::ExprKind::Tuple(elements) => {
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
            hir::ExprKind::Slice(slice) => {
                collect_paths(&slice.base, out);
                if let Some(start) = &slice.start {
                    collect_paths(start, out);
                }
                if let Some(end) = &slice.end {
                    collect_paths(end, out);
                }
            }
            hir::ExprKind::FormatString(_) => {}
            hir::ExprKind::ConstBlock(const_block) => collect_paths(&const_block.body, out),
            hir::ExprKind::Closure(closure) => collect_paths(&closure.body, out),
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
                    collect_paths_from_block(body, out);
                }
            }
            hir::ItemKind::Const(const_item) => collect_paths(&const_item.body.value, out),
            hir::ItemKind::Impl(impl_block) => {
                for impl_item in &impl_block.items {
                    if let hir::ImplItemKind::Method(method) = &impl_item.kind {
                        if let Some(body) = &method.body {
                            collect_paths_from_block(body, out);
                        }
                    }
                }
            }
            hir::ItemKind::Query(_) => {}
            hir::ItemKind::Expr(expr) => collect_paths(expr, out),
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) | hir::ItemKind::Trait(_) => {}
        }
    }

    collect_paths_from_block(body, &mut collected_paths);

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

fn expect_lowering_error<T: std::fmt::Debug>(result: Result<T>, expected: &str) {
    let err = result.expect_err("lowering should fail");
    let message = err.to_string();
    assert!(
        message.contains(expected),
        "expected error containing `{expected}`, got `{message}`"
    );
}

/// Some unsupported constructs are rejected non-fatally: lowering still
/// succeeds (producing a placeholder node, `HirGenerator::
/// error_placeholder_expr_kind`) so one unsupported construct doesn't
/// poison the whole surrounding item, but a real error diagnostic is
/// recorded via `fp_core::diagnostics::diagnostic_manager()` — this
/// checks for that recorded diagnostic instead of a hard `Err`.
fn expect_lowering_diagnostic<T: std::fmt::Debug>(
    call: impl FnOnce() -> Result<T>,
    expected: &str,
) {
    use fp_core::diagnostics::{DiagnosticLevel, diagnostic_manager};
    let mgr = diagnostic_manager();
    let start = mgr.snapshot();
    let result = call();
    result.expect("lowering should recover with a placeholder, not fail outright");
    let diagnostics = mgr.diagnostics_since(start);
    assert!(
        diagnostics.iter().any(|d| d.level == DiagnosticLevel::Error
            && d.message.to_string().contains(expected)),
        "expected an error diagnostic containing `{expected}`, got {diagnostics:?}"
    );
}

#[test]
fn transform_expr_rejects_dynamic_import() {
    let mut generator = HirGenerator::new();
    let expr = ast::Expr::from(ast::ExprKind::Invoke(ast::ExprInvoke {
        span: Span::null(),
        target: ast::ExprInvokeTarget::Function(ast::Name::Ident(ident("import"))),
        args: Vec::new(),
        kwargs: Vec::new(),
    }));

    expect_lowering_diagnostic(
        || generator.transform_expr_to_hir(&expr),
        "dynamic import is only supported in interpret mode",
    );
}

#[test]
fn transform_expr_rejects_match_without_scrutinee() {
    let mut generator = HirGenerator::new();
    let expr = ast::Expr::from(ast::ExprKind::Match(ast::ExprMatch {
        span: Span::null(),
        scrutinee: None,
        cases: vec![ast::ExprMatchCase {
            span: Span::null(),
            pat: None,
            cond: Box::new(ast::Expr::value(ast::Value::bool(true))),
            guard: None,
            body: Box::new(ast::Expr::value(ast::Value::int(1))),
        }],
    }));

    expect_lowering_error(
        generator.transform_expr_to_hir(&expr),
        "match expressions without scrutinee are not supported",
    );
}

#[test]
fn transform_expr_rejects_for_loop_non_binding_pattern() {
    let mut generator = HirGenerator::new();
    let pat = ast::Pattern::new(ast::PatternKind::Tuple(ast::PatternTuple {
        patterns: vec![ast::Pattern::new(ast::PatternKind::Ident(
            ast::PatternIdent::new(ident("i")),
        ))],
    }));
    let iter = range_expr(
        Some(ast::Expr::value(ast::Value::int(0))),
        ast::ExprRangeLimit::Exclusive,
        Some(ast::Expr::value(ast::Value::int(4))),
    );
    let body = ast::Expr::block(ast::ExprBlock::new_expr(ast::Expr::value(
        ast::Value::unit(),
    )));
    let expr = ast::Expr::from(ast::ExprKind::For(ast::ExprFor {
        span: Span::null(),
        pat: Box::new(pat),
        iter: Box::new(iter),
        body: Box::new(body),
    }));

    expect_lowering_diagnostic(
        || generator.transform_expr_to_hir(&expr),
        "`for` loop pattern must be a simple binding",
    );
}

#[test]
fn transform_block_rejects_unsupported_statement_kind() {
    let mut generator = HirGenerator::new();
    let expr = ast::Expr::block(ast::ExprBlock::new_stmts(vec![ast::BlockStmt::any(
        "unsupported statement payload".to_string(),
    )]));

    expect_lowering_diagnostic(
        || generator.transform_expr_to_hir(&expr),
        "unimplemented block statement type for HIR transformation",
    );
}
