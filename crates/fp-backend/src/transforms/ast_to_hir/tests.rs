use super::*;
use crate::transforms::HirToAstLifter;
use fp_core::ast;
use fp_core::ast::package::PackageDescriptor;
use fp_core::ast::package::provider::{FixedPackageProvider, PackageProvider};
use fp_core::ast::package::{AstPackage, PackageId};
use fp_core::ast::path::InPackagePath;
use fp_core::ast::program::AstProgram;
use fp_core::frontend::LanguageFrontend;
use fp_core::lir::LirDataLayout;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use fp_lang::ast::FerroPhaseParser;
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
/// already-built `AstPackage`) followed by
/// `AstProgram::begin_package` — never a hand-rolled
/// `AstPackage`.
fn package_from_items(items: Vec<ast::Item>) -> Result<fp_core::ast::package::AstPackage> {
    package_from_items_as(PackageId::new("test"), items)
}

fn package_from_items_as(
    package_id: PackageId,
    items: Vec<ast::Item>,
) -> Result<fp_core::ast::package::AstPackage> {
    let source = AstPackage::new(
        package_id.clone(),
        "test",
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), "test"),
        vec![ast::Module {
            attrs: Vec::new(),
            name: ast::Ident::new(""),
            items,
            visibility: ast::Visibility::Public,
            is_external: false,
        }],
    );
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| crate::error::optimization_error(e.to_string()))?;
    let workspace = AstProgram::new(std::sync::Arc::new(provider));
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
) -> Result<fp_core::ast::package::AstPackage> {
    let package_id = PackageId::new("test");
    let source = AstPackage::new(
        package_id.clone(),
        "test",
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), "test"),
        vec![ast::Module {
            attrs: Vec::new(),
            name: ast::Ident::new(module_path.last().cloned().unwrap_or_default()),
            items,
            visibility: ast::Visibility::Public,
            is_external: false,
        }],
    );
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| crate::error::optimization_error(e.to_string()))?;
    let workspace = AstProgram::new(std::sync::Arc::new(provider));
    let package = workspace.begin_package(package_id, loaded, test_data_layout());
    let package = package.borrow().clone();
    Ok(package)
}

/// Like `package_from_module_items`, but each item carries its own
/// module path — mirrors how a real multi-file source tree actually loads
/// (`RustPackageProvider` gives every `.rs` file's own top-level items the
/// module path matching that file's location; `mod foo { .. }` written
/// inline in one file is the only case that instead nests as a literal
/// `ast::ItemKind::Module`). Needed for repros of file-boundary-crossing
/// behavior (e.g. `use`/prelude resolution) that a single uniform
/// `module_path` can't exercise.
fn package_from_items_with_paths(
    items: Vec<(Vec<String>, ast::Item)>,
) -> Result<fp_core::ast::package::AstPackage> {
    package_from_items_with_paths_as(PackageId::new("test"), items)
}

fn package_from_items_with_paths_as(
    package_id: PackageId,
    items: Vec<(Vec<String>, ast::Item)>,
) -> Result<fp_core::ast::package::AstPackage> {
    fn insert_item(module: &mut ast::Module, path: &[String], item: ast::Item) {
        let Some((head, tail)) = path.split_first() else {
            module.items.push(item);
            return;
        };
        let child = module.items.iter_mut().find_map(|existing| {
            let ast::ItemKind::Module(child) = existing.kind_mut() else {
                return None;
            };
            (child.name.as_str() == head).then_some(child)
        });
        if let Some(child) = child {
            insert_item(child, tail, item);
            return;
        }
        let mut child = ast::Module {
            attrs: Vec::new(),
            name: ast::Ident::new(head),
            items: Vec::new(),
            visibility: ast::Visibility::Public,
            is_external: false,
        };
        insert_item(&mut child, tail, item);
        module
            .items
            .push(ast::Item::from(ast::ItemKind::Module(child)));
    }
    let mut root = ast::Module {
        attrs: Vec::new(),
        name: ast::Ident::new(""),
        items: Vec::new(),
        visibility: ast::Visibility::Public,
        is_external: false,
    };
    for (path, item) in items {
        insert_item(&mut root, &path, item);
    }
    let mut source = AstPackage::new(
        package_id.clone(),
        "test",
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), "test"),
        vec![root],
    );
    fn has_module_path(module: &ast::Module, path: &[&str]) -> bool {
        let Some((head, tail)) = path.split_first() else {
            return true;
        };
        module.items.iter().any(|item| {
            let ast::ItemKind::Module(child) = item.kind() else {
                return false;
            };
            child.name.as_str() == *head && has_module_path(child, tail)
        })
    }
    if has_module_path(&source.module, &["prelude", "v1"]) {
        source
            .prelude_modules
            .push(fp_core::ast::package::PackagePath::new(
                package_id.clone(),
                fp_core::ast::path::InPackagePath::new(vec!["prelude".into(), "v1".into()]),
            ));
    }
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let loaded = provider
        .load_package_source(&package_id)
        .map_err(|e| crate::error::optimization_error(e.to_string()))?;
    let workspace = AstProgram::new(std::sync::Arc::new(provider));
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
fn user_type_named_like_primitive_shadows_builtin_fallback() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let user_type = hir::DefId::new(hir::PackageId::new("test"), 7);
    generator.package_mut().module_data.add_child(
        fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("test")),
        "u8",
        fp_core::hir::resolve::Namespace::Type,
        hir::Res::Def(user_type.clone()),
    );

    generator
        .hir_program
        .add_package(generator.hir_package_handle());
    let expr = ast::Expr::new(ast::ExprKind::Name(ast::Name::ident(ident("u8"))));
    let path =
        generator.ast_expr_to_hir_path(&expr, PathResolutionScope::Type, ParamMode::Explicit)?;
    assert_eq!(path.res(), hir::Res::Def(user_type));
    Ok(())
}

#[test]
fn unresolved_primitive_name_resolves_to_builtin() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let expr = ast::Expr::new(ast::ExprKind::Name(ast::Name::ident(ident("f128"))));
    let path =
        generator.ast_expr_to_hir_path(&expr, PathResolutionScope::Type, ParamMode::Explicit)?;
    assert_eq!(
        path.res(),
        hir::Res::Builtin(hir::BuiltinSelfType::Primitive("f128".to_owned()))
    );
    let associated = ast::Expr::new(ast::ExprKind::Name(ast::Name::path(ast::Path::new(
        fp_core::ast::path::PathPrefix::Plain,
        vec!["f128".into(), "MAX".into()],
    ))));
    let associated_path = generator.ast_expr_to_hir_path(
        &associated,
        PathResolutionScope::Value,
        ParamMode::Optional,
    )?;
    let hir::QPath::TypeRelative(receiver, segment) = associated_path else {
        panic!("expected primitive type-relative path");
    };
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected primitive receiver path");
    };
    assert_eq!(
        receiver_path.res(),
        hir::Res::Builtin(hir::BuiltinSelfType::Primitive("f128".to_owned()))
    );
    assert_eq!(segment.ident.as_str(), "MAX");
    Ok(())
}

#[test]
fn unresolved_item_does_not_fall_back_to_resolved_module_prefix() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let root = fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("test"));
    let module = hir::DefId::new(hir::PackageId::new("test"), 7);
    generator
        .package_mut()
        .module_data
        .set_children(module.clone(), Vec::new());
    generator.package_mut().module_data.add_child(
        root,
        "missing",
        fp_core::hir::resolve::Namespace::Type,
        hir::Res::Module(module),
    );
    let path = ast::Path::new(
        fp_core::ast::path::PathPrefix::Plain,
        vec![ident("missing").into(), ident("Item").into()],
    );
    let expr = ast::Expr::new(ast::ExprKind::Name(ast::Name::path(path)));
    let lowered =
        generator.ast_expr_to_hir_path(&expr, PathResolutionScope::Type, ParamMode::Explicit)?;
    assert_eq!(lowered.res(), hir::Res::Error);
    Ok(())
}

#[test]
fn optional_path_parameters_infer_when_arguments_have_no_types_or_consts() {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let segment = generator.make_path_segment(
        "Trait",
        Some(hir::GenericArgs::default()),
        ParamMode::Optional,
    );
    assert!(segment.infer_args);

    let output = hir::TypeExpr::new(
        generator.next_id(),
        hir::TypeExprKind::Primitive(ast::TypePrimitive::Bool),
        Span::null(),
    );
    let constrained = hir::GenericArgs {
        args: Vec::new(),
        constraints: vec![hir::AssocItemConstraint {
            hir_id: generator.next_id(),
            ident: hir::Symbol::new("Output"),
            gen_args: hir::GenericArgs::default(),
            kind: hir::AssocItemConstraintKind::Equality {
                term: hir::Term::Ty(Box::new(output)),
            },
            span: Span::null(),
        }],
        parenthesized: hir::GenericArgsParentheses::No,
        span_ext: Span::null(),
    };
    let segment = generator.make_path_segment("Trait", Some(constrained), ParamMode::Optional);
    assert!(segment.infer_args);
}

#[test]
fn qualified_generic_paths_remain_types_even_when_value_lookup_succeeds() -> Result<()> {
    let package_id = hir::PackageId::new("test");
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        package_id.clone(),
    );
    let root = hir::resolve::ModuleData::virtual_root_for(package_id.clone());
    generator.package_mut().module_data.add_child(
        root,
        "VALUE",
        hir::resolve::Namespace::Value,
        hir::Res::Def(hir::DefId::new(package_id.clone(), 7)),
    );
    generator
        .hir_program
        .add_package(generator.hir_package_handle());

    let qualified = ast::Ty::Expr(Box::new(ast::Expr::name(ast::Name {
        qself: Some(ast::QSelf {
            ty: Box::new(ast::Ty::Primitive(ast::TypePrimitive::Int(
                ast::TypeInt::U8,
            ))),
            path_span: Span::null(),
            position: 0,
        }),
        path: ast::Path::from_ident(ast::Ident::new("VALUE")),
    })));
    let arguments = ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs {
        span: Span::null(),
        args: vec![ast::AngleBracketedArg::Arg(ast::GenericArg::Type(Box::new(
            qualified.clone(),
        )))],
    });
    let lowered = generator.convert_path_arguments(&arguments)?;
    assert!(matches!(
        lowered.args.as_slice(),
        [hir::GenericArg::Type(ty)]
            if matches!(ty.kind, hir::TypeExprKind::Path(hir::QPath::TypeRelative(_, _)))
    ));

    let lowered = generator.convert_generic_args(&[qualified])?;
    assert!(matches!(
        lowered.args.as_slice(),
        [hir::GenericArg::Type(ty)]
            if matches!(ty.kind, hir::TypeExprKind::Path(hir::QPath::TypeRelative(_, _)))
    ));
    Ok(())
}

#[test]
fn impl_self_keys_use_definition_identity_and_generic_arguments() {
    let package = hir::PackageId::new("impl-key-test");
    let adt = |index| ImplSelfKey::Adt {
        def_id: hir::DefId::new(package.clone(), index),
        args: Vec::new(),
    };
    assert_ne!(adt(1), adt(2));

    let concrete = |arg| ImplSelfKey::Adt {
        def_id: hir::DefId::new(package.clone(), 3),
        args: vec![ImplGenericArgKey::Type(Box::new(arg))],
    };
    assert_ne!(
        concrete(ImplSelfKey::Primitive(ast::TypePrimitive::Bool.to_string())),
        concrete(ImplSelfKey::Primitive(ast::TypePrimitive::Char.to_string()))
    );
}

#[test]
fn impl_self_keys_preserve_structural_outer_shape() {
    let inner = ImplSelfKey::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64).to_string());
    assert_ne!(
        ImplSelfKey::Reference {
            mutable: false,
            inner: Box::new(inner.clone()),
        },
        ImplSelfKey::RawPointer {
            mutable: false,
            inner: Box::new(inner),
        }
    );
}

#[test]
fn unqualified_lookup_does_not_scan_global_paths_by_suffix() {
    // Resolving a bare name against the *current* module's own qualified
    // entries (module_path + name) is intentional (lets a module's own
    // items reference each other unqualified). What this guards against is
    // resolving it against an unrelated *foreign* module's entries by
    // matching just the name's suffix.
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    generator.module_path = InPackagePath::new(vec!["dependency".to_string()]);
    generator.record_type_symbol(
        "SharedType",
        hir::Res::Def(hir::DefId::new(hir::PackageId::new("7"), 1)),
        &ast::Visibility::Public,
    );

    generator.module_path = InPackagePath::new(vec!["consumer".to_string()]);
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

    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    )
    .with_intrinsic_normalizer(fp_lang::FerroIntrinsicNormalizer::new());
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
    // `println!` may now surface as either `Println`
    // (a genuine low-level intrinsic) or `CallKind::Op(OpKind::Println)`
    // (the portable `#[op(...)]` tag) -- both mean the same thing here, so
    // compare the intrinsic kind directly.
    assert_eq!(call.kind, fp_core::intrinsics::IntrinsicKind::Println);
    Ok(())
}

#[test]
fn suffixed_numeric_literal_lowers_to_explicit_cast() -> Result<()> {
    let frontend = fp_lang::FerroFrontend::new();
    let parsed = frontend.parse_expr("1_usize")?;
    let ast::ItemKind::Expr(expr) = parsed.ast.items[0].kind() else {
        return Err(crate::error::optimization_error(
            "expected parsed expression item".to_string(),
        ));
    };

    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let lowered = generator.transform_expr_to_hir(expr)?;
    let hir::ExprKind::Cast(value, target) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "expected suffixed numeric literal to lower to a cast".to_string(),
        ));
    };
    assert!(matches!(value.kind, hir::ExprKind::Literal(_)));
    let hir::TypeExprKind::Path(path) = target.kind else {
        return Err(crate::error::optimization_error(
            "expected usize cast target path".to_string(),
        ));
    };
    assert_eq!(path.segments().len(), 1);
    assert_eq!(path.segments()[0].ident.as_str(), "usize");
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

    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    assert!(
        program
            .items
            .iter()
            .filter(|item| !matches!(item.kind, hir::ItemKind::Impl(_)))
            .count()
            == 0,
        "`type X = const {{ ... }};` must not synthesize a fake HIR item: {:?}",
        program.items
    );
    let root = fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("test"));
    assert!(matches!(
        generator
            .hir_package_handle()
            .borrow()
            .module_data
            .resolve_child(&root, "X", fp_core::hir::resolve::Namespace::Value),
        fp_core::hir::resolve::ResolutionResult::NotFound(_)
    ));
    Ok(())
}

#[test]
fn nested_type_position_const_block_lowers_to_dedicated_hir_node() -> Result<()> {
    let const_block_ty = ast::Ty::ConstBlock(ast::ExprConstBlock {
        span: Span::null(),
        expr: Box::new(ast::Expr::value(ast::Value::int(2))),
    });

    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let lowered = generator.transform_type_to_hir(&const_block_ty)?;
    let hir::TypeExprKind::ConstBlock(_, body) = lowered.kind else {
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
    let generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    assert_eq!(generator.local_id, 0);
    assert_eq!(generator.package().next_def_id, 1);
}

#[test]
fn test_simple_literal_creation() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let ty = generator.create_simple_type("i32");

    match ty.kind {
        hir::TypeExprKind::Path(path) => {
            assert_eq!(path.segments()[0].ident.as_str(), "i32");
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );

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
fn transform_range_value_to_standard_range_struct() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let range = range_expr(
        None,
        ast::ExprRangeLimit::Exclusive,
        Some(ast::Expr::ident(ident("end"))),
    );
    let lowered = generator.transform_expr_to_hir(&range)?;
    let hir::ExprKind::Struct(path, fields) = lowered.kind else {
        return Err(crate::error::optimization_error(
            "expected RangeTo struct literal",
        ));
    };
    assert_eq!(path.segments().last().unwrap().ident.as_str(), "RangeTo");
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.as_str(), "end");
    Ok(())
}

#[test]
fn transform_raw_reference_preserves_pointer_kind() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let raw_ref = ast::Expr::from(ast::ExprKind::Reference(ast::ExprReference {
        span: Span::null(),
        referee: Box::new(ast::Expr::ident(ident("value"))),
        mutable: None,
        raw: true,
    }));
    let lowered = generator.transform_expr_to_hir(&raw_ref)?;
    let hir::ExprKind::Reference(reference) = lowered.kind else {
        return Err(crate::error::optimization_error("expected HIR reference"));
    };
    assert!(reference.raw);
    assert_eq!(reference.mutable, hir::ty::Mutability::Not);
    Ok(())
}

#[test]
fn transform_await_expression_to_hir_passthrough() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let await_expr = ast::Expr::from(ast::ExprKind::Await(ast::ExprAwait {
        span: Span::null(),
        base: Box::new(ast::Expr::ident(ident("future"))),
    }));

    let lowered = generator.transform_expr_to_hir(&await_expr)?;
    match lowered.kind {
        hir::ExprKind::Path(path) => {
            assert_eq!(path.segments().len(), 1);
            assert_eq!(path.segments()[0].ident.as_str(), "future");
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
            assert_eq!(path.segments().len(), 1);
            assert_eq!(path.segments()[0].ident.as_str(), "future");
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let result_def_id = hir::DefId::new(hir::PackageId::new("test"), 1);
    // `Result` is defined in `std::result` and re-exported through the
    // prelude; only the prelude alias entry is needed here for the bare
    // `Result` reference below to resolve.
    generator.package_mut().module_data.add_child(
        fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("test")),
        "Result",
        fp_core::hir::resolve::Namespace::Type,
        hir::Res::Def(result_def_id.clone()),
    );

    let target = ast::ExprInvokeTarget::Function(ast::Name::ident(ident("Result")));
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
    // Resolved via the prelude alias to the real `Result` def. Generic
    // arguments are retained on the corresponding path segment.
    assert_eq!(path.res(), hir::Res::Def(result_def_id));
    let args = path
        .segments()
        .last()
        .and_then(|segment| segment.args.as_ref())
        .ok_or_else(|| {
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
    assert_eq!(arg_path.segments().len(), 2);
    assert_eq!(arg_path.segments()[0].ident.as_str(), "hir");
    assert_eq!(arg_path.segments()[1].ident.as_str(), "GenericArgs");

    Ok(())
}

#[test]
fn transform_package_resolves_pub_super_type_from_sibling_module() -> Result<()> {
    let frontend = fp_lang::FerroFrontend::new();
    let parsed = frontend.parse_file(
        "mod node { pub(super) struct NodeRef {} }\nmod map { use super::node::NodeRef; }",
        std::path::Path::new("sibling.rs"),
    )?;
    let package = package_from_items(parsed.ast.items.into_iter().collect())?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    generator.transform_package(&package)?;
    let binding = generator.tree_lookup_raw(
        &InPackagePath::new(vec!["map".into(), "NodeRef".into()]),
        fp_core::hir::resolve::Namespace::Type,
    );
    assert!(matches!(binding, Some(path) if matches!(path.res(), hir::Res::Def(_))));
    Ok(())
}

#[test]
fn transform_intrinsic_container_to_hir() -> Result<()> {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    assert_eq!(
        program
            .items
            .iter()
            .filter(|item| !matches!(item.kind, hir::ItemKind::Impl(_)))
            .count(),
        2
    );
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
        ast::ExprBlock::new_expr(ast::Expr::from(ast::ExprKind::FieldAccess(
            ast::ExprFieldAccess {
                span: Span::null(),
                obj: Box::new(ast::Expr::ident(ident("self"))),
                field: ident("value"),
                generic_args: None,
            },
        ))),
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
        kind: ast::GenericParamKind::Type,
        default: None,
        projection_bounds: Vec::new(),
    }];
    identity.sig.params = vec![ast::FunctionParam::new(ident("x"), ty_ident("T"))];
    identity.sig.ret_ty = Some(ty_ident("T"));

    let items = vec![
        container,
        ast::Item::from(ast::ItemKind::Impl(impl_block)),
        ast::Item::from(ast::ItemKind::DefFunction(identity)),
    ];

    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let generic_def_id = identity.sig.generics.params[0].def_id.clone();
    if let hir::TypeExprKind::Path(path) = &identity.sig.output.kind {
        assert!(
            matches!(&path.res(), hir::Res::Generic(def_id) if *def_id == generic_def_id),
            "generic return type should resolve to its declared generic definition"
        );
    } else {
        panic!("expected path return type for identity function");
    }
    let param_ty = &identity.sig.inputs[0].ty;
    if let hir::TypeExprKind::Path(path) = &param_ty.kind {
        assert!(
            matches!(&path.res(), hir::Res::Generic(def_id) if *def_id == generic_def_id),
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

#[test]
fn transform_parsed_mut_self_receiver_into_one_hir_input() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast("struct Buffer; impl Buffer { fn clear(&mut self) {} }")
        .expect("parse inherent method with mutable receiver");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let method = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(impl_item) => {
                impl_item.items.iter().find_map(|item| match &item.kind {
                    hir::ImplItemKind::Method(function)
                        if function.sig.name.as_str() == "clear" =>
                    {
                        Some(function)
                    }
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("lowered clear method");

    assert_eq!(method.sig.inputs.len(), 1);
    assert!(matches!(
        method.sig.inputs[0].ty.kind,
        hir::TypeExprKind::Ref(_)
    ));
    Ok(())
}

#[test]
fn transform_explicit_boxed_self_receiver_preserves_wrapper() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "struct Box<T> { value: T } struct S; impl S { fn take(self: Box<Self>) {} }",
        )
        .expect("parse explicit boxed receiver");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let receiver = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(impl_item) => {
                impl_item.items.iter().find_map(|item| match &item.kind {
                    hir::ImplItemKind::Method(function) => function.sig.inputs.first(),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("lowered receiver");
    let hir::TypeExprKind::Path(path) = &receiver.ty.kind else {
        panic!("expected Box path receiver");
    };

    assert_eq!(
        path.segments()
            .last()
            .and_then(|segment| segment.args.as_ref())
            .unwrap()
            .args
            .len(),
        1
    );
    Ok(())
}

#[test]
fn transform_trait_associated_type_bounds() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "trait Borrow<T> { fn borrow(&self) -> &T; } \
             trait ToOwned { type Owned: Borrow<Self>; }",
        )
        .expect("parse associated type bound");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let assoc_type = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::ItemKind::Trait(trait_def) => Some(trait_def),
            _ => None,
        })
        .flat_map(|trait_def| &trait_def.items)
        .find_map(|item| match &item.kind {
            hir::TraitItemKind::AssocType(assoc_type) => Some(assoc_type),
            _ => None,
        })
        .expect("lowered associated type");

    assert_eq!(assoc_type.bounds.len(), 1);
    let hir::TypeExprKind::Path(bound) = &assoc_type.bounds[0].kind else {
        panic!("expected trait path bound");
    };
    assert!(matches!(bound.res(), hir::Res::Def(_)));
    assert_eq!(bound.segments().len(), 1);
    assert_eq!(bound.segments()[0].ident.as_str(), "Borrow");
    Ok(())
}

#[test]
fn transform_dynamic_type_preserves_all_bounds() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "trait Error {} trait Send {} trait Sync {} \
             struct Holder { value: dyn Error + Send + Sync }",
        )
        .expect("parse dynamic type bounds");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let holder = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    let hir::TypeExprKind::Dynamic(bounds) = &holder.fields[0].ty.kind else {
        panic!("expected dynamic field type");
    };

    assert_eq!(bounds.len(), 3);
    assert!(
        bounds
            .iter()
            .all(|bound| matches!(bound.res(), hir::Res::Def(_)))
    );
    Ok(())
}

#[test]
fn transform_dynamic_type_prefers_trait_from_prelude_collision() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let mut nominal = parser
        .parse_items_ast("pub struct Error {}")
        .expect("parse nominal Error");
    let mut trait_item = parser
        .parse_items_ast("pub trait Error {}")
        .expect("parse trait Error");
    let mut nominal_import = parser
        .parse_items_ast("pub use crate::nominal::Error;")
        .expect("parse nominal Error import");
    let mut trait_import = parser
        .parse_items_ast("pub use crate::error_trait::Error;")
        .expect("parse trait Error import");
    let mut holder = parser
        .parse_items_ast("pub struct Holder { value: dyn Error }")
        .expect("parse Holder");

    let items = vec![
        (vec!["nominal".to_string()], nominal.remove(0)),
        (vec!["error_trait".to_string()], trait_item.remove(0)),
        (
            vec!["prelude".to_string(), "v1".to_string()],
            nominal_import.remove(0),
        ),
        (
            vec!["prelude".to_string(), "v1".to_string()],
            trait_import.remove(0),
        ),
        (vec!["consumer".to_string()], holder.remove(0)),
    ];
    let package = package_from_items_with_paths_as(PackageId::new("test"), items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let holder = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    let hir::TypeExprKind::Dynamic(bounds) = &holder.fields[0].ty.kind else {
        panic!("expected dynamic field type");
    };
    assert_eq!(bounds.len(), 1);
    let hir::Res::Def(trait_def_id) = bounds[0].res().clone() else {
        panic!("expected trait definition");
    };
    assert!(!program.placeholder_defs.contains(&trait_def_id));
    Ok(())
}

#[test]
fn transform_dynamic_type_resolves_foreign_trait_from_prelude() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser.parse_items_ast("pub trait Error {}")?;
    let dependency_package = package_from_items_with_paths_as(
        PackageId::new("dependency"),
        dependency_items
            .into_iter()
            .map(|item| (vec!["prelude".to_string(), "v1".to_string()], item))
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("dependency"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_package)?;
    let error_id = dependency
        .items
        .iter()
        .find_map(|item| match item.kind {
            hir::ItemKind::Trait(_) => Some(item.def_id.clone()),
            _ => None,
        })
        .expect("dependency trait");
    dependency.hir_exports.insert(
        "dependency::prelude::v1::Error".to_string(),
        hir::Res::Def(error_id.clone()),
    );

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items = parser.parse_items_ast("pub struct Holder { value: dyn Error }")?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    let hir::TypeExprKind::Dynamic(bounds) = &holder.fields[0].ty.kind else {
        panic!("expected dynamic field type");
    };
    let hir::Res::Def(error_id) = bounds[0].res().clone() else {
        panic!("expected foreign trait definition");
    };
    assert_eq!(error_id.package_id, hir::PackageId::new("dependency"));
    assert!(consumer_lowerer.is_trait_definition(&error_id));
    Ok(())
}

#[test]
fn enum_attributes_survive_hir_roundtrip() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items =
        parser.parse_items_ast("#[derive(Debug, Error)] pub enum Problem { Broken(String) }")?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let hir = lowerer.transform_package(&package)?;
    let item = hir
        .items
        .iter()
        .find(|item| matches!(item.kind, hir::ItemKind::Enum(_)))
        .expect("enum HIR item");
    let hir::ItemKind::Enum(def) = &item.kind else {
        unreachable!();
    };
    assert!(def.attrs.iter().any(|attr| {
        matches!(
            &attr.meta,
            ast::AttrMeta::List(list)
                if list.name.last().as_str() == "derive"
                    && list.items.iter().any(|item| {
                        matches!(item, ast::AttrMeta::Path(path) if path.last().as_str() == "Error")
                    })
        )
    }));

    let mut workspace = hir::HirProgram::new();
    workspace.publish_package(hir.clone());
    let lifted = HirToAstLifter::new(&hir, &workspace).lift_items()?;
    let ast::ItemKind::DefEnum(def) = lifted[0].kind() else {
        panic!("expected lifted enum");
    };
    assert!(def.attrs.iter().any(|attr| {
        matches!(
            &attr.meta,
            ast::AttrMeta::List(list)
                if list.name.last().as_str() == "derive"
                    && list.items.iter().any(|item| {
                        matches!(item, ast::AttrMeta::Path(path) if path.last().as_str() == "Error")
                    })
        )
    }));
    Ok(())
}

#[test]
fn enum_constructor_keeps_variant_identity_with_generic_arguments() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "enum Boxed<T> { Value(T) } fn make() -> Boxed<i64> { Boxed::Value(1) }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = lowerer.transform_package(&package)?;
    let (enum_id, variant_id) = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Enum(def) => Some((item.def_id.clone(), def.variants[0].def_id.clone())),
            _ => None,
        })
        .expect("generic enum and variant are present");
    let make = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "make" => {
                Some(function)
            }
            _ => None,
        })
        .expect("constructor function is present");
    let body = make.body.as_ref().expect("constructor has a body");
    let hir::ExprKind::Call(callee, _) = &body.expr.as_ref().expect("body expression").kind else {
        panic!("expected enum variant constructor call");
    };
    let hir::ExprKind::Path(path) = &callee.kind else {
        panic!("expected path callee");
    };
    let hir::QPath::TypeRelative(receiver, segment) = path else {
        panic!("expected type-relative enum constructor path: {path:?}");
    };
    assert_eq!(segment.ident.as_str(), "Value");
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected enum constructor receiver path: {receiver:?}");
    };
    assert_eq!(receiver_path.res(), hir::Res::Def(enum_id.clone()));
    assert_ne!(variant_id, enum_id);
    Ok(())
}

#[test]
fn enum_constructor_through_alias_keeps_nominal_variant_identity() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "enum Original { Value(i64) } type Alias = Original; fn make() -> Alias { Alias::Value(1) }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = lowerer.transform_package(&package)?;
    let _variant_id = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Enum(def) => Some(def.variants[0].def_id.clone()),
            _ => None,
        })
        .expect("enum variant is present");
    let make = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "make" => {
                Some(function)
            }
            _ => None,
        })
        .expect("constructor function is present");
    let body = make.body.as_ref().expect("constructor has a body");
    let hir::ExprKind::Call(callee, _) = &body.expr.as_ref().expect("body expression").kind else {
        panic!("expected enum variant constructor call");
    };
    let hir::ExprKind::Path(path) = &callee.kind else {
        panic!("expected path callee");
    };
    let hir::QPath::TypeRelative(receiver, segment) = path else {
        panic!("expected type-relative aliased enum constructor path: {path:?}");
    };
    assert_eq!(segment.ident.as_str(), "Value");
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected aliased enum receiver path: {receiver:?}");
    };
    assert!(matches!(receiver_path.res(), hir::Res::Def(_)));
    Ok(())
}

#[test]
fn self_enum_constructor_preserves_type_relative_identity() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "enum Message { Text(i64) } impl Message { fn make() -> Self { Self::Text(1) } }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = lowerer.transform_package(&package)?;
    let _variant_id = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Enum(def) => Some(def.variants[0].def_id.clone()),
            _ => None,
        })
        .expect("enum variant is present");
    let method = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(imp) => imp.items.iter().find_map(|member| match &member.kind {
                hir::ImplItemKind::Method(function) if function.sig.name.as_str() == "make" => {
                    Some(function)
                }
                _ => None,
            }),
            _ => None,
        })
        .expect("lowered constructor method");
    let body = method.body.as_ref().expect("constructor body");
    let hir::ExprKind::Call(callee, _) = &body.expr.as_ref().expect("body expression").kind else {
        panic!("expected constructor call");
    };
    let hir::ExprKind::Path(path) = &callee.kind else {
        panic!("expected type-relative constructor path");
    };
    let hir::QPath::TypeRelative(receiver, segment) = path else {
        panic!("expected type-relative constructor path: {path:?}");
    };
    assert_eq!(segment.ident.as_str(), "Text");
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected Self receiver path: {receiver:?}");
    };
    assert!(matches!(
        receiver_path.res(),
        hir::Res::SelfTy | hir::Res::Def(_)
    ));
    Ok(())
}

#[test]
fn transparent_type_alias_has_a_hir_definition_identity() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items =
        parser.parse_items_ast("type Alias = i64; fn read(value: Alias) -> Alias { value }")?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = lowerer.transform_package(&package)?;
    let alias = program
        .items
        .iter()
        .find(|item| matches!(item.kind, hir::ItemKind::TypeAlias(_)))
        .expect("ordinary aliases must be published as HIR items");
    assert_eq!(alias.def_id.package_id.as_str(), "test");
    assert!(program.def_map.contains_key(&alias.def_id));
    assert!(
        !program
            .module_data
            .resolve_module(
                &fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("test")),
                &["Alias".to_string()],
                fp_core::hir::resolve::Namespace::Type,
            )
            .is_not_found()
    );
    Ok(())
}

#[test]
fn generic_transparent_type_alias_resolves_rhs_parameter() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items =
        parser.parse_items_ast("type Alias<T> = T; fn read<T>(value: Alias<T>) -> T { value }")?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "generic alias RHS should resolve in its parameter scope: {diagnostics:?}"
    );
    let alias = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
            _ => None,
        })
        .expect("generic alias should be published as a HIR item");
    let hir::TypeExprKind::Path(path) = &alias.target.kind else {
        panic!(
            "expected generic alias target path: {:?}",
            alias.target.kind
        );
    };
    assert!(matches!(path.res(), hir::Res::Generic(_)), "path: {path:?}");
    Ok(())
}

#[test]
fn lifetime_path_arguments_are_erased_without_resolution_diagnostics() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "struct Wrapper<'a, T>(T); fn read<'a, T: 'a>(value: Wrapper<'a, T>) -> Wrapper<'a, T> { value }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let lowered = lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| !diagnostic.message.contains("'a")),
        "lifetime arguments must not be resolved as type paths: {diagnostics:?}"
    );
    let wrapper = lowered
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Wrapper" => Some(def),
            _ => None,
        })
        .expect("Wrapper should be lowered");
    let lifetime = wrapper
        .generics
        .params
        .first()
        .expect("Wrapper should retain its lifetime parameter");
    assert!(matches!(
        lifetime.kind,
        hir::GenericParamKind::Lifetime {
            kind: hir::LifetimeParamKind::Explicit,
        }
    ));
    assert!(lifetime.is_lifetime());
    assert!(!lifetime.is_elided_lifetime());
    assert!(matches!(
        wrapper.generics.params.get(1).map(|param| &param.kind),
        Some(hir::GenericParamKind::Type {
            synthetic: false,
            ..
        })
    ));
    let read = lowered
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "read" => {
                Some(function)
            }
            _ => None,
        })
        .expect("read should be lowered");
    let hir::TypeExprKind::Path(path) = &read.sig.inputs[0].ty.kind else {
        return Err(crate::error::optimization_error(
            "expected Wrapper parameter type path".to_owned(),
        ));
    };
    let Some(hir::GenericArgs { args, .. }) = path.segments()[0].args.as_ref() else {
        return Err(crate::error::optimization_error(
            "expected lifetime generic arguments".to_owned(),
        ));
    };
    assert!(args.iter().any(|arg| matches!(
        arg,
        hir::GenericArg::Lifetime(lifetime) if !lifetime.span().is_null()
    )));
    Ok(())
}

#[test]
fn const_generic_identifier_arguments_use_value_namespace() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "struct Array<T, const N: usize>([T; N]); fn take<T, const N: usize>(value: Array<T, N>) -> Array<T, N> { value }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let lowered = lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| !diagnostic.message.contains("unresolved Type path `N`")),
        "const generic identifiers must resolve in the value namespace: {diagnostics:?}"
    );
    let take = lowered
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "take" => {
                Some(function)
            }
            _ => None,
        })
        .expect("take function should be lowered");
    let hir::TypeExprKind::Path(path) = &take.sig.inputs[0].ty.kind else {
        panic!("expected Array path in take parameter");
    };
    let args = path
        .segments()
        .first()
        .and_then(|segment| segment.args.as_ref())
        .expect("Array's generic arguments should be retained on its resolved base");
    assert!(matches!(args.args.get(1), Some(hir::GenericArg::Const(_))));
    Ok(())
}

#[test]
fn transform_package_resolves_foreign_glob_reexport_through_selected_prelude() -> Result<()> {
    let parser = FerroPhaseParser::new();

    let core_items = parser.parse_items_ast("pub struct Ok;")?;
    let core_source = package_from_items_with_paths_as(
        PackageId::new("core"),
        core_items
            .into_iter()
            .map(|item| {
                (
                    vec![
                        "core".to_string(),
                        "prelude".to_string(),
                        "rust_2024".to_string(),
                    ],
                    item,
                )
            })
            .collect(),
    )?;
    let mut core_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("core"),
    );
    let mut core = core_lowerer.transform_package(&core_source)?;
    core.hir_exports = core_lowerer.exported_symbols();
    let ok_def_id = core
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Ok" => Some(item.def_id.clone()),
            _ => None,
        })
        .expect("core Ok definition");

    let mut core_workspace = hir::HirProgram::new();
    core_workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(core)));
    let std_items = parser.parse_items_ast("pub use core::prelude::rust_2024::*;")?;
    let std_source = package_from_items_with_paths_as(
        PackageId::new("std"),
        std_items
            .into_iter()
            .map(|item| {
                (
                    vec![
                        "std".to_string(),
                        "prelude".to_string(),
                        "rust_2024".to_string(),
                    ],
                    item,
                )
            })
            .collect(),
    )?;
    let mut std_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(core_workspace),
        hir::PackageId::new("std"),
    );
    let mut std = std_lowerer.transform_package(&std_source)?;
    std.hir_exports = std_lowerer.exported_symbols();
    assert_eq!(
        std.module_data.resolve_module(
            &fp_core::hir::resolve::ModuleData::virtual_root_for(hir::PackageId::new("std")),
            &["Ok".to_string()],
            fp_core::hir::resolve::Namespace::Type,
        ),
        fp_core::hir::resolve::ResolutionResult::Found(hir::Path {
            span: Default::default(),
            res: hir::Res::Def(ok_def_id.clone()),
            segments: Vec::new(),
        }),
    );

    let workspace = std_lowerer.hir_program.clone();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(std)));
    let consumer_items = parser.parse_items_ast("pub struct Holder { value: Ok }")?;
    let consumer_source = package_from_items_as(PackageId::new("consumer"), consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        workspace,
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_source)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("consumer Holder definition");
    let hir::TypeExprKind::Path(path) = &holder.fields[0].ty.kind else {
        panic!("expected Holder field to be a path");
    };
    assert_eq!(path.res(), hir::Res::Def(ok_def_id));
    Ok(())
}

#[test]
fn transform_package_resolves_sysroot_io_result_reexport_chain() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let result_items = parser.parse_items_ast("pub struct Result<T, E>; pub struct Error;")?;
    let core_io_items = parser
        .parse_items_ast("use crate::result; pub type Result<T> = result::Result<T, Error>;")?;
    let alloc_io_items = parser.parse_items_ast("pub use core::io::{Error, Result};")?;
    let std_root_items = parser.parse_items_ast("extern crate alloc as alloc_crate;")?;
    let std_io_items = parser.parse_items_ast("pub use alloc_crate::io::{Error, Result};")?;
    let consumer_items =
        parser.parse_items_ast("pub fn load() -> std::io::Result<i64> { loop {} }")?;
    let source = package_from_items_with_paths_as(
        PackageId::new("std"),
        result_items
            .into_iter()
            .map(|item| (vec!["core".to_string(), "result".to_string()], item))
            .chain(
                parser
                    .parse_items_ast("pub struct Error;")?
                    .into_iter()
                    .map(|item| (vec!["core".to_string(), "io".to_string()], item)),
            )
            .chain(
                core_io_items
                    .into_iter()
                    .map(|item| (vec!["core".to_string(), "io".to_string()], item)),
            )
            .chain(
                alloc_io_items
                    .into_iter()
                    .map(|item| (vec!["alloc".to_string(), "io".to_string()], item)),
            )
            .chain(
                std_root_items
                    .into_iter()
                    .map(|item| (vec!["std".to_string()], item)),
            )
            .chain(
                std_io_items
                    .into_iter()
                    .map(|item| (vec!["std".to_string(), "io".to_string()], item)),
            )
            .chain(
                consumer_items
                    .into_iter()
                    .map(|item| (vec!["consumer".to_string()], item)),
            )
            .collect(),
    )?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("std"),
    );
    let program = lowerer.transform_package(&source)?;
    let function = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "load" => {
                Some(function)
            }
            _ => None,
        })
        .expect("consumer function");
    let hir::TypeExprKind::Path(path) = &function.sig.output.kind else {
        panic!("expected transparent Result alias to lower as a path");
    };
    let hir::Res::Def(def_id) = &path.res() else {
        panic!("std::io::Result must resolve through std and alloc re-exports: {path:?}");
    };
    let result = program
        .items
        .iter()
        .find(|item| item.def_id == *def_id)
        .expect("underlying Result definition");
    assert!(matches!(result.kind, hir::ItemKind::Struct(_)));
    Ok(())
}

#[test]
fn transform_qualified_dependency_type_uses_exported_module_path() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser.parse_items_ast("pub struct PublicType;")?;
    let dependency_package = package_from_items_with_paths_as(
        PackageId::new("dependency"),
        dependency_items
            .into_iter()
            .map(|item| (vec!["api".to_string()], item))
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("dependency"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_package)?;
    dependency.hir_exports = dependency_lowerer.exported_symbols();
    let dependency_exports = dependency_lowerer.exported_symbols();
    let mut dependency = dependency;
    dependency.hir_exports = dependency_exports;
    let public_type_id = dependency
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "PublicType" => {
                Some(item.def_id.clone())
            }
            _ => None,
        })
        .expect("dependency type");

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items =
        parser.parse_items_ast("pub struct Holder { value: dependency::api::PublicType }")?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("consumer type");
    let hir::TypeExprKind::Path(path) = &holder.fields[0].ty.kind else {
        panic!("expected qualified dependency type path");
    };
    assert_eq!(path.res(), hir::Res::Def(public_type_id.clone()));
    assert_eq!(
        path.segments()
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["dependency", "api", "PublicType"]
    );
    assert!(!consumer.def_map.contains_key(&public_type_id));
    Ok(())
}

#[test]
fn lift_cross_package_intrinsic_call_from_its_resolved_definition() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser.parse_items_ast(
        "#[intrinsic = \"fs_read_to_string\"] pub fn read_to_string(path: String) -> String { path }",
    )?;
    let dependency_source = package_from_items_with_paths_as(
        PackageId::new("dependency"),
        dependency_items
            .into_iter()
            .map(|item| (vec!["fs".to_string()], item))
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("dependency"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_source)?;
    dependency.hir_exports = dependency_lowerer.exported_symbols();

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items = parser.parse_items_ast(
        "pub fn load(path: String) -> String { dependency::fs::read_to_string(path) }",
    )?;
    let consumer_source = package_from_items_as(PackageId::new("consumer"), consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace.clone()),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_source)?;

    workspace.publish_package(consumer.clone());
    let lifted = HirToAstLifter::new(&consumer, &workspace).lift_items()?;
    let ast::ItemKind::DefFunction(function) = lifted[0].kind() else {
        panic!("expected consumer function");
    };
    let ast::BlockStmt::Expr(expr_stmt) = &function.body.stmts[0] else {
        panic!("expected function expression");
    };
    let ast::ExprKind::IntrinsicCall(call) = expr_stmt.expr.kind() else {
        panic!("expected dependency call to lift as an intrinsic");
    };
    assert_eq!(call.kind, fp_core::intrinsics::CallKind::FsReadToString);
    Ok(())
}

#[test]
fn transform_normalizes_bundled_std_external_crate_root() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser.parse_items_ast("pub struct Formatter; pub struct Result;")?;
    let dependency_package = package_from_items_with_paths_as(
        PackageId::new("dependency"),
        dependency_items
            .into_iter()
            .map(|item| {
                (
                    vec!["std".to_string(), "std".to_string(), "fmt".to_string()],
                    item,
                )
            })
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("std"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_package)?;
    dependency.hir_exports = dependency_lowerer.exported_symbols();

    let formatter_id = dependency
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Formatter" => {
                Some(item.def_id.clone())
            }
            _ => None,
        })
        .expect("bundled std Formatter");
    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));

    let consumer_items = parser.parse_items_ast(
        "pub struct Holder { formatter: std::fmt::Formatter, result: std::fmt::Result }",
    )?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("consumer Holder");
    let hir::TypeExprKind::Path(formatter_path) = &holder.fields[0].ty.kind else {
        panic!("expected Formatter path");
    };
    assert_eq!(formatter_path.res(), hir::Res::Def(formatter_id));
    assert_eq!(
        formatter_path
            .segments()
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["std", "fmt", "Formatter"]
    );
    Ok(())
}

#[test]
fn transform_dependency_reexport_uses_defining_package_item_kind() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let alloc_items = parser.parse_items_ast("pub struct Arc<T>(T);")?;
    let alloc_package = package_from_items(alloc_items)?;
    let mut alloc_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("alloc"),
    );
    let alloc = alloc_lowerer.transform_package(&alloc_package)?;
    let arc_id = alloc
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Arc" => Some(item.def_id.clone()),
            _ => None,
        })
        .expect("alloc::sync::Arc definition");

    let mut std = hir::HirPackage::new(hir::PackageId::new("std"));
    std.hir_exports
        .insert("std::sync::Arc".to_string(), hir::Res::Def(arc_id.clone()));
    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(alloc)));
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(std)));

    let consumer_items =
        parser.parse_items_ast("pub struct Holder { value: std::sync::Arc<u8> }")?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    assert!(!matches!(
        holder.fields[0].ty.kind,
        hir::TypeExprKind::Error
    ));
    Ok(())
}

#[test]
fn transform_hyphenated_dependency_exports_use_rust_crate_root() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items =
        parser.parse_items_ast("pub struct CoreError; pub struct ChangesResult;")?;
    let dependency_package = package_from_items_with_paths_as(
        PackageId::new("skln-core"),
        dependency_items
            .into_iter()
            .enumerate()
            .map(|(index, item)| {
                let module = if index == 0 { "error" } else { "types" };
                (vec![module.to_string()], item)
            })
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("skln-core"),
    );
    let dependency = dependency_lowerer.transform_package(&dependency_package)?;
    let dependency_exports = dependency_lowerer.exported_symbols();
    assert!(dependency_exports.contains_key("skln_core::error::CoreError"));
    assert!(dependency_exports.contains_key("skln_core::types::ChangesResult"));
    let mut dependency = dependency;
    dependency.hir_exports = dependency_exports;

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items = parser.parse_items_ast(
        "pub struct Holder { error: skln_core::error::CoreError, result: skln_core::types::ChangesResult }",
    )?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    for field in &holder.fields {
        assert!(
            !matches!(field.ty.kind, hir::TypeExprKind::Error),
            "dependency export remained unresolved for {}",
            field.name
        );
    }
    Ok(())
}

#[test]
fn transform_imported_dependency_enum_variant_uses_defining_identity() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser
        .parse_items_ast("pub mod types { pub enum RefNode { WorkingTree, Branch(String) } }")?;
    let dependency_package = package_from_items_as(PackageId::new("skln-core"), dependency_items)?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("skln-core"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_package)?;
    dependency.hir_exports = dependency_lowerer.exported_symbols();

    let mut workspace = hir::HirProgram::new();
    workspace.publish_package(dependency);
    let consumer_items = parser.parse_items_ast(
        "use skln_core::types::RefNode; pub fn make() -> RefNode { RefNode::WorkingTree }",
    )?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let consumer_diagnostics = consumer_lowerer.take_diagnostics().get_diagnostics();
    assert!(
        consumer_diagnostics.is_empty(),
        "imported dependency enum variant should resolve without diagnostics: {consumer_diagnostics:?}"
    );
    let function = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "make" => {
                Some(function)
            }
            _ => None,
        })
        .expect("lowered make function");
    let hir::TypeExprKind::Path(path) = &function.sig.output.kind else {
        panic!("expected RefNode output path")
    };
    assert!(matches!(path.res(), hir::Res::Def(_)));
    Ok(())
}

#[test]
fn transform_bare_imported_enum_variant_pattern_uses_enum_identity() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "enum RefNode { WorkingTree, Branch(String) } use RefNode::*; fn classify(node: RefNode) -> bool { match node { WorkingTree => true, Branch(_) => false } }",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("consumer"),
    );
    let _package = lowerer.transform_package(&package)?;
    assert!(
        lowerer.take_diagnostics().get_diagnostics().is_empty(),
        "bare enum variants imported into pattern scope should resolve"
    );
    Ok(())
}

#[test]
fn unresolved_import_diagnostic_points_at_import_span() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast("use missing::Thing; fn main() {}")?;
    let import_span = items.iter().find_map(|item| match item.kind() {
        ast::ItemKind::Import(import) => Some(import.span()),
        _ => None,
    });
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.message.to_string().contains("unresolved import"))
        .expect("unresolved import diagnostic");
    let span = diagnostic
        .span
        .expect("diagnostic should carry import span");
    // `parse_items_ast` intentionally uses the synthetic null file span;
    // real package providers attach source offsets. The important invariant
    // here is that the resolver preserves a span instead of manufacturing
    // `None`/a lowerer's current-file fallback.
    assert_eq!(Some(span), import_span);
    Ok(())
}

#[test]
fn conflicting_glob_imports_are_reported_as_ambiguous() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "mod first { pub struct Choice; } mod second { pub struct Choice; } use first::*; use second::*; fn take(_: Choice) {}",
    )?;
    let conflicting_import_span = items
        .iter()
        .filter_map(|item| match item.kind() {
            ast::ItemKind::Import(import) => Some(import.span()),
            _ => None,
        })
        .nth(1);
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    let ambiguity = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.message.to_string().contains("ambiguous import"))
        .unwrap_or_else(|| {
            panic!("expected an ambiguous glob import diagnostic, got {diagnostics:?}")
        });
    assert_eq!(ambiguity.span, conflicting_import_span);
    assert!(
        diagnostics.iter().all(|diagnostic| {
            !diagnostic
                .message
                .to_string()
                .contains("duplicate definition")
        }),
        "an import collision should not also be reported as a definition collision: {diagnostics:?}"
    );
    Ok(())
}

#[test]
fn local_definition_wins_over_glob_import() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "mod source { pub struct Choice; } use source::*; struct Choice; fn take(_: Choice) {}",
    )?;
    let package = package_from_items(items)?;
    let mut lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    lowerer.transform_package(&package)?;
    let diagnostics = lowerer.take_diagnostics().get_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "a local definition should shadow a glob import without diagnostics: {diagnostics:?}"
    );
    Ok(())
}

#[test]
fn transform_hyphenated_dependency_root_reexport_uses_rust_crate_root() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items = parser.parse_items_ast(
        "pub mod error { pub struct CoreError; } pub mod types { pub struct ChangesResult; pub struct RefNode; } pub use error::CoreError;",
    )?;
    let dependency_package = package_from_items_as(PackageId::new("skln-core"), dependency_items)?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("skln-core"),
    );
    let dependency = dependency_lowerer.transform_package(&dependency_package)?;
    let dependency_exports = dependency_lowerer.exported_symbols();
    assert!(dependency_exports.contains_key("skln_core::CoreError"));
    assert!(dependency_exports.contains_key("skln_core::error::CoreError"));
    assert!(dependency_exports.contains_key("skln_core::types::ChangesResult"));
    assert!(dependency_exports.contains_key("skln_core::types::RefNode"));
    let mut dependency = dependency;
    dependency.hir_exports = dependency_exports;

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items = parser.parse_items_ast(
        "pub struct Holder { error: skln_core::CoreError, result: skln_core::types::ChangesResult, node: skln_core::types::RefNode }",
    )?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("lowered Holder");
    assert!(
        holder
            .fields
            .iter()
            .all(|field| { !matches!(field.ty.kind, hir::TypeExprKind::Error) })
    );
    Ok(())
}

#[test]
fn transform_provider_rooted_hyphenated_exports_replace_cargo_root() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items =
        parser.parse_items_ast("pub struct CoreError; pub struct ChangesResult;")?;
    let dependency_package = package_from_items_with_paths(
        dependency_items
            .into_iter()
            .enumerate()
            .map(|(index, item)| {
                let module = if index == 0 { "error" } else { "types" };
                (vec!["skln-core".to_string(), module.to_string()], item)
            })
            .collect(),
    )?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("skln-core"),
    );
    let mut dependency = dependency_lowerer.transform_package(&dependency_package)?;
    dependency.hir_exports = dependency_lowerer.exported_symbols();

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let consumer_items = parser.parse_items_ast(
        "pub struct Holder { error: skln_core::error::CoreError, result: skln_core::types::ChangesResult }",
    )?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(workspace),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    let holder = consumer
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Holder" => Some(def),
            _ => None,
        })
        .expect("consumer Holder");
    let expected = ["CoreError", "ChangesResult"];
    for (field, name) in holder.fields.iter().zip(expected) {
        let hir::TypeExprKind::Path(path) = &field.ty.kind else {
            panic!("expected dependency path for {name}");
        };
        assert!(matches!(
            path.res().as_ref(),
            Some(hir::Res::Def(def_id))
                if def_id.package_id == hir::PackageId::new("skln-core")
        ));
    }
    Ok(())
}

#[test]
fn indexes_function_local_trait_impl_by_local_type() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast("trait Marker {} fn make() { struct Local; impl Marker for Local {} }")
        .expect("parse function-local trait impl");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let package = generator.transform_package(&package)?;
    let local_id = package
        .def_map
        .values()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "Local" => Some(item.def_id.clone()),
            _ => None,
        })
        .expect("materialized local struct");

    assert_eq!(
        package.impls_by_self_did.get(&local_id).map(Vec::len),
        Some(1)
    );
    Ok(())
}

#[test]
fn resolves_local_struct_constructor_inside_impl_method() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "struct Box; impl<'a> From<String> for Box { fn from(err: String) { struct StringError(String); impl From<String> for StringError { fn from(value: String) {} } let value = StringError(err); } }",
    )?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let package = generator.transform_package(&package)?;
    let string_error = package
        .def_map
        .values()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Struct(def) if def.name.as_str() == "StringError" => {
                Some(item.def_id.clone())
            }
            _ => None,
        })
        .expect("materialized local struct");
    let method = package
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Impl(impl_def) => {
                impl_def.items.iter().find_map(|item| match &item.kind {
                    hir::ImplItemKind::Method(function) if function.sig.name.as_str() == "from" => {
                        Some(function)
                    }
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("lowered impl method");
    let body = method.body.as_ref().expect("method body");
    let local = body
        .stmts
        .iter()
        .find_map(|stmt| {
            let hir::StmtKind::Local(local) = &stmt.kind else {
                return None;
            };
            let init = local.init.as_ref()?;
            let hir::ExprKind::Call(callee, _) = &init.kind else {
                return None;
            };
            let hir::ExprKind::Path(path) = &callee.kind else {
                return None;
            };
            (path.res() == hir::Res::Def(string_error.clone())).then_some(local)
        })
        .expect("local constructor call");
    assert!(local.init.is_some());
    Ok(())
}

#[test]
fn transform_type_relative_call_without_a_method_receiver() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "struct Buffer; impl Buffer { fn new() -> Buffer { Buffer } } fn make() { Buffer::new(); }",
        )
        .expect("parse type-relative associated function call");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let make = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "make" => {
                Some(function)
            }
            _ => None,
        })
        .expect("lowered make function");
    let call = make
        .body
        .as_ref()
        .and_then(|body| body.stmts.first())
        .expect("make call statement");

    assert!(matches!(
        call.kind,
        hir::StmtKind::Semi(hir::Expr {
            kind: hir::ExprKind::Call(_, _),
            ..
        })
    ));
    Ok(())
}

#[test]
fn transform_type_relative_call_through_reexport_keeps_type_resolution() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "mod facade { \
                 mod inner { pub struct Buffer; impl Buffer { pub fn new() -> Buffer { Buffer } } } \
                 pub use inner::Buffer; \
             } \
             fn make() { facade::Buffer::new(); }",
        )
        .expect("parse re-exported type-relative associated function call");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let reexport = generator.lookup_global_res(
        &InPackagePath::new(vec!["facade".to_string(), "Buffer".to_string()]),
        PathResolutionScope::Type,
    );
    assert!(
        matches!(reexport, Some(ref path) if matches!(path.res(), hir::Res::Def(_))),
        "re-export resolution: {reexport:?}"
    );
    let make = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "make" => {
                Some(function)
            }
            _ => None,
        })
        .expect("lowered make function");
    let hir::StmtKind::Semi(hir::Expr {
        kind: hir::ExprKind::Call(callee, _),
        ..
    }) = &make
        .body
        .as_ref()
        .and_then(|body| body.stmts.first())
        .expect("make call statement")
        .kind
    else {
        panic!("expected associated function call");
    };
    let hir::ExprKind::Path(path) = &callee.kind else {
        panic!("expected associated function path");
    };
    match path {
        hir::QPath::TypeRelative(receiver, segment) => {
            let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
                panic!("expected nominal receiver path");
            };
            assert!(matches!(receiver_path.res(), hir::Res::Def(_)));
            assert_eq!(segment.ident.as_str(), "new");
        }
        hir::QPath::Resolved(_, resolved) => {
            assert!(matches!(resolved.res(), hir::Res::Def(_)));
        }
    }
    Ok(())
}

#[test]
fn transform_method_call_on_runtime_field_keeps_receiver_chain() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast(
            "struct Buffer; impl Buffer { fn clear(&mut self) {} } \
             struct Holder { data: Buffer } \
             fn clear(holder: Holder) { holder.data.clear(); }",
        )
        .expect("parse method call on runtime field");
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let clear = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "clear" => {
                Some(function)
            }
            _ => None,
        })
        .expect("lowered clear function");
    let stmt = clear
        .body
        .as_ref()
        .and_then(|body| body.stmts.first())
        .expect("clear call statement");
    assert!(matches!(
        stmt.kind,
        hir::StmtKind::Semi(hir::Expr {
            kind: hir::ExprKind::MethodCall(_, _, _, _),
            ..
        })
    ));
    Ok(())
}

#[test]
fn transform_generic_associated_const_keeps_type_relative_base() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "trait Layout { const IS_ZST: bool; } \
         fn read<T: Layout>() -> bool { T::IS_ZST }",
    )?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let read = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "read" => {
                Some(function)
            }
            _ => None,
        })
        .expect("read function present");
    let generic_def_id = read.sig.generics.params[0].def_id.clone();
    let body_expr = read
        .body
        .as_ref()
        .and_then(|body| body.expr.as_deref())
        .expect("read body expression");
    let hir::ExprKind::Path(path) = &body_expr.kind else {
        panic!(
            "expected associated constant path, got {:?}",
            body_expr.kind
        );
    };
    let hir::QPath::TypeRelative(receiver, segment) = path else {
        panic!("expected type-relative associated constant path, got {path:?}");
    };
    assert_eq!(segment.ident.as_str(), "IS_ZST");
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected generic receiver path, got {:?}", receiver.kind);
    };
    assert_eq!(
        receiver_path.res(),
        hir::Res::Generic(generic_def_id),
        "type-relative associated constants resolve their base type in AST→HIR"
    );
    Ok(())
}

#[test]
fn transform_trait_associated_consts_preserves_declaration_and_default() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast("trait Layout { const ABSTRACT: bool; const DEFAULTED: bool = true; }")?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let trait_def = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Trait(trait_def) => Some(trait_def),
            _ => None,
        })
        .expect("lowered trait");
    let consts = trait_def
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::TraitItemKind::AssocConst(konst) => Some(konst),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(consts.len(), 2);
    assert_eq!(consts[0].name.as_str(), "ABSTRACT");
    assert!(consts[0].body.is_none());
    assert_eq!(consts[1].name.as_str(), "DEFAULTED");
    assert!(consts[1].body.is_some());
    assert!(matches!(
        consts[1].ty.kind,
        hir::TypeExprKind::Path(_) | hir::TypeExprKind::Primitive(_)
    ));
    Ok(())
}

#[test]
fn transform_trait_associated_consts_preserves_ids_and_body_owner() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast("trait Layout { const ABSTRACT: bool; const DEFAULTED: bool = true; }")?;
    let package = package_from_items(items)?;
    let ast_trait = package
        .module
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ast::ItemKind::DefTrait(trait_def) => Some(trait_def),
            _ => None,
        })
        .expect("trait in AST package");

    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let trait_def = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Trait(trait_def) => Some((item, trait_def)),
            _ => None,
        })
        .expect("lowered trait");

    assert_eq!(trait_def.0.def_id.package_id, generator.package_id);

    let expected_member_ids = ast_trait
        .items
        .iter()
        .filter_map(|item| match item.kind() {
            ast::ItemKind::DeclConst(const_item) => Some(const_item.name.name.as_str()),
            ast::ItemKind::DefConst(const_item) => Some(const_item.name.name.as_str()),
            _ => None,
        })
        .map(|name| (name, ()))
        .collect::<Vec<_>>();

    let consts = trait_def
        .1
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::TraitItemKind::AssocConst(konst) => Some((item, konst)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(consts.len(), expected_member_ids.len());
    assert_eq!(
        consts
            .iter()
            .map(|(item, _)| item.name.as_str())
            .collect::<Vec<_>>(),
        expected_member_ids
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
    );

    let owner = hir::OwnerId(trait_def.0.def_id.clone());
    for ((item, konst), _) in consts.iter().zip(expected_member_ids.iter()) {
        assert_eq!(item.def_id.package_id, generator.package_id);
        assert_eq!(item.hir_id.owner, owner);
        assert_eq!(konst.ty.hir_id.owner, owner);
        if let Some(body) = &konst.body {
            assert_eq!(body.hir_id.owner, owner);
            assert_eq!(body.value.hir_id.owner, owner);
            assert!(matches!(body.value.kind, hir::ExprKind::Literal(_)));
        }
    }

    assert!(consts[0].1.body.is_none());
    assert!(consts[1].1.body.is_some());
    Ok(())
}

#[test]
fn transform_trait_associated_const_default_is_owned_by_dependency_package() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let dependency_items =
        parser.parse_items_ast("pub trait Layout { const DEFAULTED: bool = true; }")?;
    let dependency_package = package_from_items(dependency_items)?;
    let mut dependency_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("dependency"),
    );
    let dependency = dependency_lowerer.transform_package(&dependency_package)?;
    let (trait_def_id, dependency_trait) = dependency
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Trait(trait_def) => Some((item.def_id.clone(), trait_def)),
            _ => None,
        })
        .expect("dependency trait");
    let default_const_id = dependency_trait
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::TraitItemKind::AssocConst(constant) if item.name.as_str() == "DEFAULTED" => {
                constant.body.as_ref().map(|_| item.def_id.clone())
            }
            _ => None,
        })
        .expect("dependency trait associated constant");

    let mut workspace = hir::HirProgram::new();
    workspace.add_package(std::rc::Rc::new(std::cell::RefCell::new(dependency)));
    let workspace = hir::SharedHirProgram::new(workspace);
    let consumer_items = parser
        .parse_items_ast("use dependency::Layout; fn read<T: Layout>() -> bool { T::DEFAULTED }")?;
    let consumer_package = package_from_items(consumer_items)?;
    let mut consumer_lowerer = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        workspace.clone(),
        hir::PackageId::new("consumer"),
    );
    let consumer = consumer_lowerer.transform_package(&consumer_package)?;
    assert!(!consumer.def_map.contains_key(&trait_def_id));
    let dependency_item = workspace
        .item(trait_def_id)
        .expect("dependency trait remains in its owning package");
    let hir::ItemKind::Trait(dependency_trait) = &dependency_item.kind else {
        panic!("dependency definition is not a trait");
    };
    let dependency_const = dependency_trait
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::TraitItemKind::AssocConst(constant) if item.def_id == default_const_id => {
                Some(constant)
            }
            _ => None,
        })
        .expect("dependency trait associated constant");
    assert!(dependency_const.body.is_some());
    Ok(())
}

#[test]
fn transform_associated_const_uses_type_namespace_for_base() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "trait Layout { const IS_ZST: bool; } \
         fn read<T: Layout>() -> bool { let T = false; T::IS_ZST }",
    )?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let read = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "read" => {
                Some(function)
            }
            _ => None,
        })
        .expect("read function present");
    let generic_def_id = read.sig.generics.params[0].def_id.clone();
    let body_expr = read
        .body
        .as_ref()
        .and_then(|body| body.expr.as_deref())
        .expect("read body expression");
    let hir::ExprKind::Path(path) = &body_expr.kind else {
        panic!(
            "expected associated constant path, got {:?}",
            body_expr.kind
        );
    };
    let hir::QPath::TypeRelative(receiver, segment) = path else {
        panic!("expected type-relative associated constant path, got {path:?}");
    };
    assert_eq!(segment.ident.as_str(), "IS_ZST");
    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
        panic!("expected generic receiver path, got {:?}", receiver.kind);
    };
    assert_eq!(receiver_path.res(), hir::Res::Generic(generic_def_id));
    Ok(())
}

#[test]
fn transform_generic_associated_type_path_keeps_qpath_base() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "trait Iterator { type Item; } \
         fn item<I: Iterator>() -> I::Item; \
         fn alias<A: Iterator>() -> A::Item; \
         fn nested<I: Iterator>() -> I::Item::IntoIter;",
    )?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    for (name, expected_segments) in [
        ("item", vec!["Item"]),
        ("alias", vec!["Item"]),
        ("nested", vec!["Item", "IntoIter"]),
    ] {
        let function = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Function(function) if function.sig.name.as_str() == name => {
                    Some(function)
                }
                _ => None,
            })
            .expect("lowered function");
        let hir::TypeExprKind::Path(path) = &function.sig.output.kind else {
            panic!("expected associated type path for {name}");
        };
        match path {
            hir::QPath::TypeRelative(receiver, segment) => {
                assert_eq!(
                    segment.ident.as_str(),
                    expected_segments.last().copied().unwrap()
                );
                if expected_segments.len() > 1 {
                    let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
                        panic!("expected receiver path for nested associated type");
                    };
                    assert!(receiver_path.segments().len() >= 1);
                }
            }
            hir::QPath::Resolved(_, path) => panic!("expected type-relative path, got {path:?}"),
        }
    }
    Ok(())
}

#[test]
fn transform_module_const_keeps_value_namespace_for_base() -> Result<()> {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(
        "mod values { pub const FLAG: bool = true; } \
         fn read() -> bool { values::FLAG }",
    )?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;
    let read = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "read" => {
                Some(function)
            }
            _ => None,
        })
        .expect("read function present");
    let body_expr = read
        .body
        .as_ref()
        .and_then(|body| body.expr.as_deref())
        .expect("read body expression");
    let hir::ExprKind::Path(path) = &body_expr.kind else {
        panic!("expected module constant path, got {:?}", body_expr.kind);
    };
    assert_eq!(
        path.segments()
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["values", "FLAG"]
    );
    assert!(
        matches!(path.res(), hir::Res::Def(_)),
        "module-qualified constants resolve to the module's value definition"
    );
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
        ast::ExprBlock::new_expr(ast::Expr::from(ast::ExprKind::FieldAccess(
            ast::ExprFieldAccess {
                span: Span::null(),
                obj: Box::new(ast::Expr::ident(ident("self"))),
                field: ident("value"),
                generic_args: None,
            },
        ))),
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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

/// Fast, targeted repro for the "unresolved type path `Vec`/`Option`/
/// `Arc`/..." bugs found typechecking the real vendored std (`fp compile`
/// against it takes ~30 minutes; this test exercises the exact same
/// resolution machinery — the resolver's implicit-prelude fallback
/// scan and `resolve_global_type_symbol`'s consultation of it — in
/// milliseconds). Mirrors real std's own *file* layout, not an inline `mod
/// foo { .. }` block: `inner.rs` (module path `["inner"]`) declares `pub
/// struct Foo`, `prelude/v1.rs` (module path `["prelude", "v1"]`) has a
/// top-level `pub use crate::inner::Foo;` (matching `std::prelude::v1`'s own
/// top-level `pub use crate::vec::Vec;`), and `other.rs` (module path
/// `["other"]`) references the bare, unqualified name with no explicit
/// `use` of its own — exactly how every real std/core/alloc source file
/// relies on the compiler's implicit per-module prelude import.
#[test]
fn transform_package_resolves_bare_prelude_reexport_from_sibling_module() -> Result<()> {
    let inner_item = make_struct("Foo", vec![("value", int_ty())]);

    let prelude_use = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("inner")),
                ast::ItemImportTree::Ident(ident("Foo")),
            ],
        }),
    }));

    // References the bare name `Foo` with no `use` of its own — relies
    // entirely on the AST resolver having picked it up from
    // `prelude::v1`'s re-export.
    let make_fn_item = make_fn(
        "make",
        Vec::new(),
        ty_ident("Foo"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Foo"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    // A second-hop re-export (`crate::b::Foo` re-exporting `crate::inner
    // ::Foo`, then `prelude::v1` re-exporting `crate::b::Foo`) — mirrors
    // real std's own multi-hop chains (e.g. `std::prelude::v1` re-exports
    // `crate::vec::Vec`, and `crate::vec` module re-exports `alloc::vec::
    // Vec` from a *different* real crate merged into the same package).
    let b_reexport = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("inner")),
                ast::ItemImportTree::Ident(ident("Foo")),
            ],
        }),
    }));
    let prelude_reexports_b = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("b")),
                ast::ItemImportTree::Ident(ident("Foo")),
            ],
        }),
    }));
    let _ = prelude_use;

    let items = vec![
        (vec!["inner".to_string()], inner_item),
        (vec!["b".to_string()], b_reexport),
        (
            vec!["prelude".to_string(), "v1".to_string()],
            prelude_reexports_b,
        ),
        (vec!["other".to_string()], make_fn_item),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    fn find_fn<'a>(items: &'a [hir::Item], name: &str) -> Option<&'a hir::Function> {
        items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == name => Some(func),
            _ => None,
        })
    }
    let make_fn_hir = find_fn(&program.items, "make").expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "bare `Foo` return type in a sibling module must resolve via the \
         prelude re-export — got unresolved path {ret_path:?}"
    );
    Ok(())
}

/// Companion to the flat-file prelude repro above — real std's own
/// `std::prelude::v1` (`crates/fp-rust/std/std/prelude/v1.rs`) writes a
/// `use` inline inside a *nested* `mod ambiguous_macros_only { pub use
/// crate::*; }` block rather than only at its own file's top level.
/// The resolver must scan beyond `package.items`' own
/// top-level entries, never recursing into `ast::ItemKind::Module`, so an
/// import written this way was silently never collected as pending at all
/// — this constructs that exact shape (`use` nested inside an inline
/// `mod`, itself inside the module the import needs to end up visible in)
/// and asserts the re-exported name still resolves.
#[test]
fn transform_package_resolves_import_nested_inside_inline_module() -> Result<()> {
    let inner_item = make_struct("Foo", vec![("value", int_ty())]);

    let prelude_use = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("inner")),
                ast::ItemImportTree::Ident(ident("Foo")),
            ],
        }),
    }));
    // The function that needs to see the re-export lives *inside* the same
    // inline module the `use` is nested in — matching real std's own
    // shape, where `ambiguous_macros_only`'s whole point is scoping its
    // own re-export locally rather than leaking it to `prelude::v1`
    // itself (that requires its own further explicit re-export one level
    // up, a separate, unrelated concern from whether the nested `use`
    // resolves at all).
    let make_fn_item = make_fn(
        "make",
        Vec::new(),
        ty_ident("Foo"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Foo"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );
    let nested_inline_module = ast::Item::from(ast::ItemKind::Module(ast::Module {
        attrs: Vec::new(),
        name: ident("ambiguous_macros_only"),
        items: vec![prelude_use, make_fn_item],
        visibility: ast::Visibility::Public,
        is_external: false,
    }));

    let items = vec![
        (vec!["inner".to_string()], inner_item),
        (
            vec!["prelude".to_string(), "v1".to_string()],
            nested_inline_module,
        ),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "bare `Foo` return type must resolve via a `use` nested inside an \
         inline `mod` block — got unresolved path {ret_path:?}"
    );

    Ok(())
}

/// Real `core::prelude::v1` re-exports `Option`/`Result` via `pub use
/// crate::option::Option::{self, None, Some};` / `pub use crate::result::
/// Result::{self, Err, Ok};` — the `self` inside the group means "the
/// enclosing path itself" (bind `Option`, not just its variants), a
/// completely different meaning from `self::` as a path's own first
/// segment ("current module"). `collect_imports` treated every `SelfMod`
/// node as a no-op regardless of position, so `Option`/`Result`
/// themselves were silently never imported by this exact (extremely
/// common) idiom — only their variants were. This is the actual root
/// cause behind the huge "unresolved type path `Option`"/`Result`" counts
/// seen typechecking real std (hundreds of thousands of occurrences,
/// since nearly every function signature in std touches one of them).
#[test]
fn transform_package_resolves_self_plus_variants_group_import() -> Result<()> {
    let inner_item = make_struct("Foo", vec![("value", int_ty())]);

    // `use crate::inner::Foo::{self, Variant};` — a simplified stand-in
    // for real core::prelude::v1's own `pub use crate::option::Option::
    // {self, None, Some};`/`crate::result::Result::{self, Err, Ok};`. Only
    // the `self` member matters for this repro (binding `Foo` itself);
    // `Variant` is just a second, unrelated group member establishing
    // that this is genuinely a multi-item group, not a single-item one.
    let prelude_use = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("inner")),
                ast::ItemImportTree::Ident(ident("Foo")),
                ast::ItemImportTree::Group(ast::ItemImportGroup {
                    items: vec![
                        ast::ItemImportTree::SelfMod,
                        ast::ItemImportTree::Ident(ident("Variant")),
                    ],
                }),
            ],
        }),
    }));

    // References the bare name `Foo` with no `use` of its own — relies
    // entirely on the prelude re-export's `self` member actually binding
    // `Foo` itself.
    let make_fn_item = make_fn(
        "make",
        Vec::new(),
        ty_ident("Foo"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Foo"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    let items = vec![
        (vec!["inner".to_string()], inner_item),
        (vec!["prelude".to_string(), "v1".to_string()], prelude_use),
        (vec!["other".to_string()], make_fn_item),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "bare `Foo` return type must resolve via the `Foo::{{self, \
         Variant}}` group import's own `self` member — got unresolved \
         path {ret_path:?}"
    );

    Ok(())
}

/// Real `std/lib.rs` writes `extern crate alloc as alloc_crate;` then
/// re-exports through it (`pub use alloc_crate::vec;`, `pub use
/// alloc_crate::boxed;`, ...) — the vendored std source merges the real
/// `core`/`alloc`/`std` crates into one FerroPhase package, so this
/// "extern crate" is really a *whole-module* alias within the same
/// package, not a cross-package dependency. Mirrors that exact shape:
/// `alloc::vec` defines `Vec`, a top-level `alloc` module path is
/// registered (matching `alloc/lib.rs`'s own crate-root file), `std`
/// aliases `alloc` as `alloc_crate` then re-exports `alloc_crate::vec::
/// Vec`, and a third module references the bare name.
#[test]
fn transform_package_resolves_extern_crate_alias_reexport_chain() -> Result<()> {
    let vec_item = make_struct("Vec", vec![("value", int_ty())]);

    // A stand-in for `alloc/lib.rs`'s own top-level items — content
    // doesn't matter, only that *some* item exists at module path
    // `["alloc"]` so that path gets registered (mirroring a real
    // crate-root file's module path).
    let alloc_crate_root_marker = ast::Item::from(ast::ItemKind::DefConst(ast::ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: ast::Visibility::Public,
        name: ident("ALLOC_MARKER"),
        ty: None,
        value: Box::new(ast::Expr::value(ast::Value::int(0))),
    }));

    // `extern crate alloc as alloc_crate;`
    let extern_crate_alloc = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Rename(ast::ItemImportRename {
            from: ident("alloc"),
            to: ident("alloc_crate"),
        }),
    }));
    // `pub use alloc_crate::vec::Vec;`
    let reexport_vec = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Ident(ident("alloc_crate")),
                ast::ItemImportTree::Ident(ident("vec")),
                ast::ItemImportTree::Ident(ident("Vec")),
            ],
        }),
    }));

    let make_fn_item = make_fn(
        "make",
        Vec::new(),
        ty_ident("Vec"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Vec"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    // `make` lives in `std` too (not a third, unrelated module) —
    // `alloc_crate::vec::Vec`'s re-export binds `Vec` into `std`'s own
    // module scope directly; whether an *unrelated sibling* module can
    // also see it bare (without its own `use`) is the separate prelude
    // mechanism already covered by
    // `transform_package_resolves_bare_prelude_reexport_from_sibling_module`,
    // not what this test is isolating.
    let items = vec![
        (vec!["alloc".to_string()], alloc_crate_root_marker),
        (vec!["alloc".to_string(), "vec".to_string()], vec_item),
        (vec!["std".to_string()], extern_crate_alloc),
        (vec!["std".to_string()], reexport_vec),
        (vec!["std".to_string()], make_fn_item),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "bare `Vec` return type must resolve via `extern crate alloc as \
         alloc_crate;` + `pub use alloc_crate::vec::Vec;` — got \
         unresolved path {ret_path:?}"
    );

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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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

    let mut collected_paths: Vec<&hir::QPath> = Vec::new();

    fn collect_paths<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a hir::QPath>) {
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
            hir::ExprKind::MethodCall(receiver, _, _, args) => {
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

    fn collect_paths_from_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a hir::QPath>) {
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

    fn collect_paths_from_item<'a>(item: &'a hir::Item, out: &mut Vec<&'a hir::QPath>) {
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
            hir::ItemKind::Struct(_)
            | hir::ItemKind::Enum(_)
            | hir::ItemKind::Trait(_)
            | hir::ItemKind::TypeAlias(_) => {}
        }
    }

    collect_paths_from_block(body, &mut collected_paths);

    let mut name_to_paths: HashMap<String, Vec<&hir::QPath>> = HashMap::new();

    for path in collected_paths {
        if let Some(segment) = path.segments().last() {
            name_to_paths
                .entry(segment.ident.as_str().to_owned())
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
                .all(|path| matches!(path.res(), hir::Res::Local(_))),
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
/// succeeds (producing a placeholder node, `AstToHirLowerer::
/// error_placeholder_expr_kind`) so one unsupported construct doesn't
/// poison the whole surrounding item, but a real error diagnostic is
/// recorded on the lowerer's own `DiagnosticManager` — this checks for
/// that recorded diagnostic instead of a hard `Err`.
fn expect_lowering_diagnostic<T: std::fmt::Debug>(
    generator: &mut AstToHirLowerer,
    call: impl FnOnce(&mut AstToHirLowerer) -> Result<T>,
    expected: &str,
) {
    use fp_core::diagnostics::DiagnosticLevel;
    let result = call(generator);
    result.expect("lowering should recover with a placeholder, not fail outright");
    let diagnostics = generator.take_diagnostics().get_diagnostics();
    assert!(
        diagnostics
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error && d.message.to_string().contains(expected)),
        "expected an error diagnostic containing `{expected}`, got {diagnostics:?}"
    );
}

/// Function-body path tests stay separate from package/module-resolution
/// tests.  They exercise the lexical scope that the lowerer installs while
/// transforming a function, so an unresolved body path cannot be mistaken
/// for a missing module binding.
mod function_body_resolution {
    use super::*;

    fn lower(source: &str) -> (hir::HirPackage, Vec<Diagnostic>) {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast(source)
            .expect("function-body fixture should parse");
        let mut package = package_from_items(items).expect("function-body fixture package");
        fn has_module_path(module: &ast::Module, path: &[&str]) -> bool {
            let Some((head, tail)) = path.split_first() else {
                return true;
            };
            module.items.iter().any(|item| {
                let ast::ItemKind::Module(child) = item.kind() else {
                    return false;
                };
                child.name.as_str() == *head && has_module_path(child, tail)
            })
        }
        if has_module_path(&package.module, &["prelude", "v1"]) {
            package
                .prelude_modules
                .push(fp_core::ast::package::PackagePath::new(
                    PackageId::new("test"),
                    InPackagePath::new(vec!["prelude".into(), "v1".into()]),
                ));
        }
        let mut lowerer = AstToHirLowerer::new(
            std::rc::Rc::new(AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
            hir::SharedHirProgram::new(hir::HirProgram::new()),
            PackageId::new("test"),
        );
        let lowered = lowerer
            .transform_package(&package)
            .expect("function-body fixture should lower");
        (lowered, lowerer.take_diagnostics().get_diagnostics())
    }

    fn function<'a>(package: &'a hir::HirPackage, name: &str) -> &'a hir::Function {
        package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Function(function) if function.sig.name.as_str() == name => {
                    Some(function)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("function `{name}` should be present"))
    }

    #[test]
    fn lowers_qualified_ast_type_to_qpath() {
        let parser = FerroPhaseParser::new();
        let mut items = parser
            .parse_items_ast("trait Trait { type Item; } struct Value; type Alias = Value;")
            .expect("projection fixture should parse");
        let alias = items
            .iter_mut()
            .find(|item| matches!(item.kind(), ast::ItemKind::DefType(_)))
            .expect("type alias should be present");
        let ast::ItemKind::DefType(def) = alias.kind_mut() else {
            unreachable!();
        };
        def.value = ast::Ty::Expr(Box::new(ast::Expr::name(ast::Name {
            qself: Some(ast::QSelf {
                ty: Box::new(ast::Ty::Expr(Box::new(ast::Expr::name(
                    ast::Name::ident("Value"),
                )))),
                path_span: Span::null(),
                position: 1,
            }),
            path: ast::Path::plain(vec![
                ast::Ident::new("Trait").into(),
                ast::Ident::new("Item").into(),
            ]),
        })));

        let package = package_from_items(items).expect("projection fixture package");
        let package_id = PackageId::new("test");
        let mut lowerer = AstToHirLowerer::new(
            std::rc::Rc::new(AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
            hir::SharedHirProgram::new(hir::HirProgram::new()),
            package_id,
        );
        let lowered = lowerer
            .transform_package(&package)
            .expect("projection fixture should lower");
        assert!(
            lowerer.take_diagnostics().get_diagnostics().is_empty(),
            "qualified path lowering emitted diagnostics"
        );
        let alias = lowered
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("lowered type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(Some(_), path)) = &alias.target.kind
        else {
            panic!("qualified AST type should lower to a resolved QPath");
        };
        assert_eq!(path.segments.len(), 2);
        assert_eq!(path.segments[0].ident.as_str(), "Trait");
        assert_eq!(path.segments[1].ident.as_str(), "Item");
        assert!(matches!(path.segments[1].res, hir::Res::Error));
    }

    #[test]
    fn resolves_explicit_qself_type_path() {
        let (package, diagnostics) =
            lower("trait Trait { type Item; } struct Value; type Alias = <Value as Trait>::Item;");
        assert!(
            diagnostics.is_empty(),
            "explicit QSelf type path emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(Some(_), path)) = &alias.target.kind
        else {
            panic!(
                "expected explicitly qualified QPath, got {:?}",
                alias.target.kind
            );
        };
        assert_eq!(path.segments.len(), 2);
        assert!(matches!(path.res, hir::Res::Def(_)));
        assert!(
            !path.span().is_null(),
            "explicitly qualified HIR paths must retain their source span"
        );
        assert!(matches!(path.segments[0].res, hir::Res::Def(_)));
        assert_eq!(path.segments[0].ident.as_str(), "Trait");
        assert!(!path.segments[0].infer_args);
        assert_eq!(path.segments[1].ident.as_str(), "Item");
        assert!(matches!(path.segments[1].res, hir::Res::Error));
        assert!(!path.segments[1].infer_args);
    }

    #[test]
    fn preserves_generic_arguments_on_explicit_qself_segments() {
        let (package, diagnostics) = lower(
            "trait Trait<T> { type Item; } struct Value; type Alias = <Value as Trait<u8>>::Item<u16>;",
        );
        assert!(
            diagnostics.is_empty(),
            "explicit QSelf generic path emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(Some(_), trait_path)) = &alias.target.kind
        else {
            panic!(
                "expected explicitly qualified QPath, got {:?}",
                alias.target.kind
            );
        };
        assert_eq!(
            trait_path.segments[0]
                .args
                .as_ref()
                .expect("trait arguments")
                .args
                .len(),
            1
        );
        assert_eq!(
            trait_path.segments[1]
                .args
                .as_ref()
                .expect("associated-type arguments")
                .args
                .len(),
            1
        );
        assert!(!trait_path.segments[0].infer_args);
        assert!(!trait_path.segments[1].infer_args);
    }

    #[test]
    fn infers_omitted_qself_associated_arguments_in_expression_paths() {
        let (package, diagnostics) = lower(
            "trait Trait<T> { fn make() -> T; } struct Value<T>(T); fn call() -> i64 { <Value<i64> as Trait<i64>>::make() }",
        );
        assert!(
            diagnostics.is_empty(),
            "expression QPath emitted diagnostics: {diagnostics:?}"
        );
        let call = function(&package, "call");
        let hir::ExprKind::Call(callee, _) = &body_expr(call).kind else {
            panic!("expected qualified associated function call");
        };
        let hir::ExprKind::Path(hir::QPath::Resolved(Some(_), path)) = &callee.kind else {
            panic!("expected resolved qualified QPath, got {:?}", callee.kind);
        };
        assert_eq!(path.segments.len(), 2);
        assert!(!path.segments[0].infer_args);
        assert!(path.segments[1].infer_args);
    }

    #[test]
    fn preserves_generic_arguments_on_nested_type_relative_segments() {
        let (package, diagnostics) = lower("type Alias<T> = <T>::First<u8>::Second<u16>;");
        assert!(
            diagnostics.is_empty(),
            "nested type-relative path emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::TypeRelative(first_receiver, second)) =
            &alias.target.kind
        else {
            panic!(
                "expected outer type-relative path, got {:?}",
                alias.target.kind
            );
        };
        assert_eq!(second.ident.as_str(), "Second");
        assert_eq!(
            second.args.as_ref().expect("Second arguments").args.len(),
            1
        );
        assert!(!second.infer_args);
        let hir::TypeExprKind::Path(hir::QPath::TypeRelative(base_receiver, first)) =
            &first_receiver.kind
        else {
            panic!(
                "expected nested type-relative receiver, got {:?}",
                first_receiver.kind
            );
        };
        assert_eq!(first.ident.as_str(), "First");
        assert_eq!(first.args.as_ref().expect("First arguments").args.len(), 1);
        assert!(!first.infer_args);
        let hir::TypeExprKind::Path(hir::QPath::Resolved(None, base)) = &base_receiver.kind else {
            panic!(
                "expected resolved generic base, got {:?}",
                base_receiver.kind
            );
        };
        assert!(matches!(base.res, hir::Res::Generic(_)));
        assert_eq!(base.segments.len(), 1);
        assert_eq!(base.segments[0].ident.as_str(), "T");
    }

    #[test]
    fn preserves_associated_type_constraints_on_path_segments() {
        let (package, diagnostics) =
            lower("trait Trait { type Item; } type Alias<T> = Trait<Item = T>;");
        assert!(
            diagnostics.is_empty(),
            "associated-type constraint emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(_, path)) = &alias.target.kind else {
            panic!("expected resolved trait path, got {:?}", alias.target.kind);
        };
        let args = path.segments[0]
            .args
            .as_ref()
            .expect("trait generic arguments");
        assert!(matches!(
            args.constraints.as_slice(),
            [hir::AssocItemConstraint {
                ident,
                kind: hir::AssocItemConstraintKind::Equality { .. },
                ..
            }] if ident.as_str() == "Item"
        ));
    }

    #[test]
    fn preserves_generic_arguments_on_associated_constraints() {
        let (package, diagnostics) =
            lower("trait Trait { type Item; } type Alias<T> = Trait<Item<'a> = T>;");
        assert!(
            diagnostics.is_empty(),
            "generic associated constraint emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(_, path)) = &alias.target.kind else {
            panic!("expected resolved trait path, got {:?}", alias.target.kind);
        };
        let args = path.segments[0]
            .args
            .as_ref()
            .expect("trait generic arguments");
        let [
            hir::AssocItemConstraint {
                ident,
                gen_args,
                kind: hir::AssocItemConstraintKind::Equality { .. },
                ..
            },
        ] = args.constraints.as_slice()
        else {
            panic!("expected one generic associated-type equality constraint");
        };
        assert_eq!(ident.as_str(), "Item");
        assert!(matches!(
            gen_args.args.as_slice(),
            [hir::GenericArg::Lifetime(lifetime)] if lifetime.as_str() == "'a"
        ));
    }

    #[test]
    fn preserves_associated_const_constraint_terms() {
        let (package, diagnostics) =
            lower("trait Trait { const VALUE: i32; } type Alias = Trait<VALUE = 3>;");
        assert!(
            diagnostics.is_empty(),
            "associated-const constraint emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(_, path)) = &alias.target.kind else {
            panic!("expected resolved trait path, got {:?}", alias.target.kind);
        };
        let args = path.segments[0]
            .args
            .as_ref()
            .expect("trait generic arguments");
        assert!(matches!(
            args.constraints.as_slice(),
            [hir::AssocItemConstraint {
                ident,
                kind: hir::AssocItemConstraintKind::Equality {
                    term: hir::Term::Const(_),
                },
                ..
            }] if ident.as_str() == "VALUE"
        ));
    }

    #[test]
    fn lowers_parenthesized_path_arguments_like_rustc() {
        let (package, diagnostics) =
            lower("trait Callable<T> {} type Alias = Callable(u8) -> bool;");
        assert!(
            diagnostics.is_empty(),
            "parenthesized path arguments emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(_, path)) = &alias.target.kind else {
            panic!(
                "expected resolved callable path, got {:?}",
                alias.target.kind
            );
        };
        let args = path.segments[0]
            .args
            .as_ref()
            .expect("callable path arguments");
        assert_eq!(args.parenthesized, hir::GenericArgsParentheses::ParenSugar);
        assert!(matches!(
            args.args.as_slice(),
            [hir::GenericArg::Type(input)]
                if matches!(input.kind, hir::TypeExprKind::Tuple(ref inputs) if inputs.len() == 1)
        ));
        assert!(matches!(
            args.constraints.as_slice(),
            [hir::AssocItemConstraint {
                ident,
                kind: hir::AssocItemConstraintKind::Equality {
                    term: hir::Term::Ty(ty),
                },
                ..
            }] if ident.as_str() == "Output" && matches!(ty.kind, hir::TypeExprKind::Primitive(_))
        ));
    }

    #[test]
    fn lowers_return_type_notation_like_rustc() {
        let (package, diagnostics) = lower("trait Trait {} type Alias = Trait(..);");
        assert!(
            diagnostics.is_empty(),
            "return-type notation emitted diagnostics: {diagnostics:?}"
        );
        let alias = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::TypeAlias(alias) if alias.name.as_str() == "Alias" => Some(alias),
                _ => None,
            })
            .expect("type alias should be present");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(_, path)) = &alias.target.kind else {
            panic!("expected resolved trait path, got {:?}", alias.target.kind);
        };
        let args = path.segments[0]
            .args
            .as_ref()
            .expect("trait path arguments");
        assert_eq!(
            args.parenthesized,
            hir::GenericArgsParentheses::ReturnTypeNotation
        );
        assert!(args.args.is_empty());
        assert!(args.constraints.is_empty());
    }

    fn body_expr(function: &hir::Function) -> &hir::Expr {
        function
            .body
            .as_ref()
            .and_then(|body| body.expr.as_deref())
            .expect("fixture function should have a final expression")
    }

    fn type_path(ty: &hir::TypeExpr) -> &hir::QPath {
        match &ty.kind {
            hir::TypeExprKind::Path(path) => path,
            hir::TypeExprKind::Ref(inner) => type_path(inner),
            other => panic!("expected a path-like type, got {other:?}"),
        }
    }

    #[test]
    fn resolves_function_parameter_in_body() {
        let (package, diagnostics) = lower("fn identity(value: i64) -> i64 { value }");
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "identity")).kind else {
            panic!("expected parameter body to lower to a path");
        };
        assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `value`")),
            "parameter resolution emitted diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn resolves_local_binding_in_body() {
        let (package, diagnostics) = lower("fn local() -> i64 { let value = 1; value }");
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "local")).kind else {
            panic!("expected local body to lower to a path");
        };
        assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `value`")),
            "local resolution emitted diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn resolves_generic_type_parameter_in_body_signature() {
        let (package, diagnostics) = lower("fn identity<T>(value: T) -> T { value }");
        let function = function(&package, "identity");
        assert!(matches!(
            function.sig.inputs[0].ty.kind,
            hir::TypeExprKind::Path(_)
        ));
        assert!(matches!(
            function.sig.output.kind,
            hir::TypeExprKind::Path(_)
        ));
        let hir::ExprKind::Path(path) = &body_expr(function).kind else {
            panic!("expected generic function body to lower to a path");
        };
        assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved Type path `T`")),
            "generic type resolution emitted diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn preserves_generic_arguments_on_resolved_path_base() {
        let (package, diagnostics) =
            lower("struct Wrapper<T>(T); fn take(value: Wrapper<u8>) -> Wrapper<u8> { value }");
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "take");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let args = input_path
            .segments()
            .first()
            .and_then(|segment| segment.args.as_ref())
            .expect("resolved Wrapper base should retain generic arguments");
        assert_eq!(args.args.len(), 1);
        assert!(matches!(args.args[0], hir::GenericArg::Type(_)));
        assert_eq!(input_path.segments().len(), 1);
        assert!(!input_path.segments()[0].infer_args);
    }

    #[test]
    fn explicit_empty_generic_arguments_disable_inference() {
        let (package, diagnostics) =
            lower("struct Wrapper<T>(T); fn take(value: Wrapper<>) -> Wrapper<> { value }");
        assert!(
            diagnostics.is_empty(),
            "explicit empty arguments emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "take");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let segment = input_path.segments().first().expect("Wrapper path segment");
        assert!(segment.args.is_some(), "explicit <> must be retained");
        assert!(
            segment
                .args
                .as_ref()
                .is_some_and(|args| args.args.is_empty()),
            "explicit <> must remain empty"
        );
        assert!(!segment.infer_args);
    }

    #[test]
    fn classifies_named_const_generic_arguments_as_constants() {
        let (package, diagnostics) = lower(
            "struct Array<T, const N: usize> { value: T } fn use_array<T, const N: usize>(value: Array<T, N>) -> Array<T, N> { value }",
        );
        assert!(
            diagnostics.is_empty(),
            "named const generic arguments emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "use_array");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let args = input_path
            .segments()
            .first()
            .and_then(|segment| segment.args.as_ref())
            .expect("Array generic arguments");
        assert!(matches!(args.args[0], hir::GenericArg::Type(_)));
        assert!(matches!(args.args[1], hir::GenericArg::Const(_)));
    }

    #[test]
    fn preserves_const_generic_default_as_const_argument() {
        let (package, diagnostics) = lower("struct Buffer<const N: usize = 4>;");
        assert!(
            diagnostics.is_empty(),
            "const generic default emitted diagnostics: {diagnostics:?}"
        );
        let buffer = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Struct(def) if def.name.as_str() == "Buffer" => Some(def),
                _ => None,
            })
            .expect("Buffer struct");
        let hir::GenericParamKind::Const { default, .. } = &buffer.generics.params[0].kind else {
            panic!("expected const generic parameter");
        };
        let default = default.as_ref().expect("const generic default");
        assert!(matches!(
            default.kind,
            hir::ConstArgKind::Literal {
                lit: hir::Lit::Integer(4),
                negated: false,
            }
        ));
    }

    #[test]
    fn type_namespace_wins_for_ambiguous_single_segment_generic_arguments() {
        let (package, diagnostics) = lower(
            "struct N; const N: usize = 1; struct Array<T, const M: usize> { value: T } fn use_array(value: Array<N, 1>) -> Array<N, 1> { value }",
        );
        assert!(
            diagnostics.is_empty(),
            "ambiguous generic arguments emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "use_array");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let args = input_path
            .segments()
            .first()
            .and_then(|segment| segment.args.as_ref())
            .expect("Array generic arguments");
        assert!(matches!(args.args[0], hir::GenericArg::Type(_)));
        assert!(matches!(args.args[1], hir::GenericArg::Const(_)));
    }

    #[test]
    fn preserves_infer_argument_metadata_on_path_segments() {
        let (package, diagnostics) =
            lower("struct Wrapper<T>(T); fn take(value: Wrapper::<_>) -> Wrapper::<_> { value }");
        assert!(
            diagnostics.is_empty(),
            "inferred generic arguments emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "take");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let args = input_path
            .segments()
            .first()
            .and_then(|segment| segment.args.as_ref())
            .expect("Wrapper generic arguments");
        let hir::GenericArg::Infer(infer) = &args.args[0] else {
            panic!(
                "expected an inferred generic argument, got {:?}",
                args.args[0]
            );
        };
        assert_eq!(infer.kind, hir::InferArgKind::TypeOrConst);
        assert_eq!(
            infer.hir_id.package_id(),
            function.sig.inputs[0].hir_id.package_id()
        );
    }

    #[test]
    fn classifies_braced_const_inference_as_const() {
        let (package, diagnostics) = lower(
            "struct Array<const N: usize>([u8; N]); fn take(value: Array<{ _ }>) -> Array<{ _ }> { value }",
        );
        assert!(
            diagnostics.is_empty(),
            "const inference emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "take");
        let input_path = type_path(&function.sig.inputs[0].ty);
        let args = input_path
            .segments()
            .first()
            .and_then(|segment| segment.args.as_ref())
            .expect("Array generic arguments");
        let hir::GenericArg::Infer(infer) = &args.args[0] else {
            panic!(
                "expected an inferred generic argument, got {:?}",
                args.args[0]
            );
        };
        assert_eq!(infer.kind, hir::InferArgKind::Const);
    }

    #[test]
    fn preserves_generic_arguments_on_type_relative_segments() {
        let (package, diagnostics) = lower(
            "struct Outer<T>(T); impl<T> Outer<T> { fn inner<U>(value: U) -> U { value } } fn call(value: u16) -> u16 { Outer::<u8>::inner::<u16>(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "call")).kind else {
            panic!("expected associated-function call");
        };
        let hir::ExprKind::Path(hir::QPath::TypeRelative(receiver, method)) = &callee.kind else {
            panic!("expected type-relative callee, got {:?}", callee.kind);
        };
        assert_eq!(method.ident.as_str(), "inner");
        assert_eq!(
            method.args.as_ref().expect("method arguments").args.len(),
            1
        );
        assert!(!method.infer_args);
        let hir::TypeExprKind::Path(hir::QPath::Resolved(None, receiver)) = &receiver.kind else {
            panic!("expected resolved receiver path, got {:?}", receiver.kind);
        };
        assert_eq!(receiver.segments.len(), 1);
        assert_eq!(receiver.segments[0].ident.as_str(), "Outer");
        assert!(!receiver.segments[0].infer_args);
        assert_eq!(
            receiver.segments[0]
                .args
                .as_ref()
                .expect("receiver arguments")
                .args
                .len(),
            1
        );
    }

    #[test]
    fn infers_omitted_type_arguments_on_type_relative_receivers() {
        let (package, diagnostics) = lower(
            "struct Outer<T>(T); impl<T> Outer<T> { fn inner(value: T) -> T { value } } fn call(value: u8) -> u8 { Outer::inner(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "omitted receiver arguments emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "call")).kind else {
            panic!("expected associated-function call");
        };
        let hir::ExprKind::Path(hir::QPath::TypeRelative(receiver, method)) = &callee.kind
        else {
            panic!("expected type-relative callee, got {:?}", callee.kind);
        };
        assert_eq!(method.ident.as_str(), "inner");
        let hir::TypeExprKind::Path(hir::QPath::Resolved(None, receiver)) = &receiver.kind else {
            panic!("expected resolved receiver path, got {:?}", receiver.kind);
        };
        let receiver = receiver.segments.first().expect("receiver path segment");
        assert_eq!(receiver.ident.as_str(), "Outer");
        assert!(
            receiver.infer_args,
            "omitted generic arguments on a type-relative receiver should be inferred"
        );
        assert!(receiver.args.is_none());
    }

    #[test]
    fn preserves_lifetime_and_const_arguments_on_runtime_method_calls() {
        let (package, diagnostics) =
            lower("fn call(value: i64) -> i64 { value.method::<'a, 3>() }");
        assert!(
            diagnostics.is_empty(),
            "method generic arguments emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::MethodCall(_, _, Some(args), _) =
            &body_expr(function(&package, "call")).kind
        else {
            panic!("expected runtime method call with generic arguments");
        };
        assert!(matches!(
            args.args.as_slice(),
            [
                hir::GenericArg::Lifetime(lifetime),
                hir::GenericArg::Const(_),
            ] if lifetime.as_str() == "'a"
        ));
    }

    #[test]
    fn preserves_generic_arguments_on_method_target_paths() -> Result<()> {
        let package_id = hir::PackageId::new("test");
        let mut generator = AstToHirLowerer::new(
            std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
            hir::SharedHirProgram::new(hir::HirProgram::new()),
            package_id.clone(),
        );
        let receiver_def = hir::DefId::new(package_id.clone(), 7);
        generator.package_mut().module_data.add_child(
            hir::resolve::ModuleData::virtual_root_for(package_id.clone()),
            "Receiver",
            hir::resolve::Namespace::Type,
            hir::Res::Def(receiver_def),
        );
        generator
            .hir_program
            .add_package(generator.hir_package_handle());

        let select = ast::ExprFieldAccess {
            span: Span::null(),
            obj: Box::new(ast::Expr::name(ast::Name::ident("Receiver"))),
            field: ast::Ident::new("method"),
            generic_args: Some(ast::GenericArgs::AngleBracketed(
                ast::AngleBracketedArgs {
                    span: Span::null(),
                    args: vec![ast::AngleBracketedArg::Arg(ast::GenericArg::Type(
                        Box::new(ast::Ty::Primitive(ast::TypePrimitive::Int(
                            ast::TypeInt::U8,
                        ))),
                    ))],
                },
            )),
        };
        let expr = ast::Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
            span: Span::null(),
            target: ast::ExprInvokeTarget::Method(select),
            args: Vec::new(),
            kwargs: Vec::new(),
        }));
        let path = generator.ast_expr_to_hir_path(
            &expr,
            PathResolutionScope::Type,
            ParamMode::Explicit,
        )?;
        let hir::QPath::Resolved(_, path) = path else {
            panic!("expected a resolved method path");
        };
        let method = path.segments.last().expect("method path segment");
        assert_eq!(method.ident.as_str(), "method");
        let args = method.args.as_ref().expect("method generic arguments");
        assert_eq!(args.args.len(), 1);
        assert!(!method.infer_args);
        Ok(())
    }

    #[test]
    fn resolves_const_generic_parameter_in_body() {
        let (package, diagnostics) = lower("fn constant<const N: usize>() -> usize { N }");
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "constant")).kind else {
            panic!("expected const-generic body to lower to a path");
        };
        assert!(matches!(path.res(), hir::Res::Generic(_)), "path: {path:?}");
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `N`")),
            "const-generic resolution emitted diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn resolves_nested_function_body_scopes() {
        let (package, diagnostics) = lower(
            "fn nested(input: i64) -> i64 { let outer = input; { let inner = outer; inner } }",
        );
        let function = function(&package, "nested");
        let body = function.body.as_ref().expect("nested function body");
        let hir::StmtKind::Local(local) = &body.stmts[0].kind else {
            panic!("expected outer local declaration");
        };
        let hir::ExprKind::Path(outer_path) = &local.init.as_ref().expect("outer initializer").kind
        else {
            panic!("expected outer initializer path");
        };
        assert!(
            matches!(outer_path.res(), hir::Res::Local(_)),
            "path: {outer_path:?}"
        );
        let hir::ExprKind::Block(inner) = &body_expr(function).kind else {
            panic!("expected nested block body");
        };
        let Some(hir::Stmt {
            kind: hir::StmtKind::Local(inner_local),
            ..
        }) = inner.stmts.first()
        else {
            panic!("expected inner local declaration");
        };
        let hir::ExprKind::Path(inner_path) =
            &inner_local.init.as_ref().expect("inner initializer").kind
        else {
            panic!("expected inner initializer path");
        };
        assert!(
            matches!(inner_path.res(), hir::Res::Local(_)),
            "path: {inner_path:?}"
        );
        let hir::ExprKind::Path(result_path) = &inner.expr.as_deref().expect("inner result").kind
        else {
            panic!("expected inner result path");
        };
        assert!(
            matches!(result_path.res(), hir::Res::Local(_)),
            "path: {result_path:?}"
        );
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path")),
            "nested-scope resolution emitted diagnostics: {diagnostics:?}"
        );
    }

    #[test]
    fn resolves_block_local_function_in_body() {
        let (package, diagnostics) = lower("fn outer() -> i64 { fn inner() -> i64 { 1 } inner() }");
        assert!(
            diagnostics.is_empty(),
            "block-local function resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "outer")).kind else {
            panic!("expected block-local function call");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected block-local function path: {callee:?}");
        };
        assert!(matches!(path.res(), hir::Res::Def(_)), "path: {path:?}");
        assert_eq!(path.segments().len(), 1, "fully resolved path: {path:?}");
        assert_eq!(path.segments()[0].ident.as_str(), "inner");
    }

    #[test]
    fn resolves_closure_body_parameter_and_capture() {
        let (package, diagnostics) = lower(
            "fn apply(input: i64) -> i64 { let closure = |value: i64| input + value; closure(1) }",
        );
        assert!(
            diagnostics.iter().all(|diagnostic| {
                let message = diagnostic.message.to_string();
                !message.contains("unresolved value path `input`")
                    && !message.contains("unresolved value path `value`")
            }),
            "closure resolution emitted diagnostics: {diagnostics:?}"
        );
        let closure_function = package.items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(function)
                if function.sig.inputs.len() == 2
                    && function
                        .body
                        .as_ref()
                        .and_then(|body| body.expr.as_deref())
                        .is_some_and(|expr| matches!(expr.kind, hir::ExprKind::Binary(..))) =>
            {
                Some(function)
            }
            _ => None,
        });
        let closure_function =
            closure_function.expect("closure lowering should publish its environment function");
        let body = closure_function
            .body
            .as_ref()
            .expect("closure function body");
        let hir::ExprKind::Binary(_, capture, parameter) =
            &body.expr.as_deref().expect("closure function result").kind
        else {
            panic!("expected closure body to add capture and parameter: {body:?}");
        };
        let hir::ExprKind::Path(capture_path) = &capture.kind else {
            panic!("expected captured input path: {capture:?}");
        };
        assert_eq!(
            capture_path
                .segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["input"]
        );
        let hir::ExprKind::Path(parameter_path) = &parameter.kind else {
            panic!("expected closure parameter path: {parameter:?}");
        };
        assert!(
            matches!(
                parameter_path.res(),
                hir::Res::Local(_) | hir::Res::Parameter(_)
            ),
            "closure parameter should resolve to its local binding: {parameter_path:?}"
        );
    }

    #[test]
    fn resolves_generic_callable_parameter_in_body() {
        let (package, diagnostics) = lower(
            "trait FnOnce<Args> {} fn apply<F: FnOnce(i64) -> i64>(f: F, value: i64) -> i64 { f(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "generic callable parameter resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "apply")).kind else {
            panic!("expected generic callable body to lower to a call");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected generic callable callee to lower to a path");
        };
        assert!(
            matches!(path.res(), hir::Res::Local(_) | hir::Res::Parameter(_)),
            "generic callable should resolve to its parameter: {path:?}"
        );
    }

    #[test]
    fn resolves_callable_capture_in_generated_closure_body() {
        let (package, diagnostics) = lower(
            "trait FnOnce<Args> {} fn apply<F: FnOnce(i64) -> i64>(f: F, value: i64) -> i64 { let closure = |x: i64| f(x); closure(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "callable capture resolution emitted diagnostics: {diagnostics:?}"
        );
        let closure_function = package.items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(function)
                if function.sig.inputs.len() == 2
                    && function
                        .body
                        .as_ref()
                        .and_then(|body| body.expr.as_deref())
                        .is_some_and(|expr| matches!(expr.kind, hir::ExprKind::Call(..))) =>
            {
                Some(function)
            }
            _ => None,
        });
        let closure_function =
            closure_function.expect("callable capture should produce a closure function");
        let hir::ExprKind::Call(callee, _) = &body_expr(closure_function).kind else {
            panic!("expected generated closure call body");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected captured callable path: {callee:?}");
        };
        assert!(
            matches!(path.res(), hir::Res::Local(_) | hir::Res::Parameter(_)),
            "captured callable should retain the environment binding: {path:?}"
        );
        assert_eq!(
            path.segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["f"]
        );
    }

    #[test]
    fn resolves_capture_field_base_in_generated_closure_body() {
        let (package, diagnostics) = lower(
            "struct Holder { value: i64 } fn read(holder: Holder) -> i64 { let closure = || holder.value; closure() }",
        );
        assert!(
            diagnostics.is_empty(),
            "capture field resolution emitted diagnostics: {diagnostics:?}"
        );
        let closure_function = package.items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(function)
                if function.sig.inputs.len() == 1
                    && function
                        .body
                        .as_ref()
                        .and_then(|body| body.expr.as_deref())
                        .is_some_and(|expr| {
                            matches!(expr.kind, hir::ExprKind::FieldAccess(..))
                        }) =>
            {
                Some(function)
            }
            _ => None,
        });
        let closure_function =
            closure_function.expect("field capture should produce a closure function");
        let hir::ExprKind::FieldAccess(base, field) = &body_expr(closure_function).kind else {
            panic!("expected generated closure field path");
        };
        let hir::ExprKind::Path(path) = &base.kind else {
            panic!("expected generated closure field base path: {base:?}");
        };
        assert!(
            matches!(path.res(), hir::Res::Local(_) | hir::Res::Parameter(_)),
            "capture field should retain the environment binding: {path:?}"
        );
        assert_eq!(
            path.segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["__env", "holder"]
        );
        assert_eq!(field.as_str(), "value");
    }

    #[test]
    fn resolves_capture_in_match_scrutinee() {
        let (package, diagnostics) = lower(
            "trait FnOnce<Args> {} fn apply<F: FnOnce(i64) -> i64>(f: F, value: i64) -> i64 { let closure = |x: i64| match f(x) { _ => 1 }; closure(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "match scrutinee capture emitted diagnostics: {diagnostics:?}"
        );
        let closure_function = package.items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(function)
                if function.sig.inputs.len() == 2
                    && function
                        .body
                        .as_ref()
                        .and_then(|body| body.expr.as_deref())
                        .is_some_and(|expr| matches!(expr.kind, hir::ExprKind::Match(..))) =>
            {
                Some(function)
            }
            _ => None,
        });
        let closure_function =
            closure_function.expect("match closure should publish its environment function");
        let hir::ExprKind::Match(scrutinee, _) = &body_expr(closure_function).kind else {
            panic!("expected generated closure match body");
        };
        let hir::ExprKind::Call(callee, _) = &scrutinee.kind else {
            panic!("expected match scrutinee call: {scrutinee:?}");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected captured callable path: {callee:?}");
        };
        assert!(
            matches!(path.res(), hir::Res::Local(_) | hir::Res::Parameter(_)),
            "captured callable should retain the environment binding: {path:?}"
        );
        assert_eq!(
            path.segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["f"]
        );
    }

    #[test]
    fn resolves_nested_closure_body() {
        let (package, diagnostics) =
            lower("fn nested() -> i64 { let outer = || { let inner = || 1; inner() }; outer() }");
        assert!(
            diagnostics.is_empty(),
            "nested closure lowering emitted diagnostics: {diagnostics:?}"
        );
        let generated_functions = package
            .items
            .iter()
            .filter(|item| matches!(item.kind, hir::ItemKind::Function(_)))
            .count();
        assert_eq!(
            generated_functions, 3,
            "nested closure functions: {package:?}"
        );
    }

    #[test]
    fn resolves_module_qualified_function_in_body() {
        let (package, diagnostics) = lower(
            "mod helpers { fn answer() -> i64 { 1 } } fn call() -> i64 { helpers::answer() }",
        );
        assert!(
            diagnostics.is_empty(),
            "module-qualified body resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "call")).kind else {
            panic!("expected module-qualified call");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected module-qualified callee path: {callee:?}");
        };
        let hir::QPath::Resolved(_, path) = path else {
            panic!("expected ordinary resolved function path: {path:?}");
        };
        assert!(matches!(path.res(), hir::Res::Def(_)), "path: {path:?}");
        assert_eq!(
            path.segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["helpers", "answer"]
        );
    }

    #[test]
    fn resolves_enum_variant_path_in_body() {
        let (package, diagnostics) =
            lower("enum Choice { Yes, No } fn pick() -> Choice { Choice::Yes }");
        assert!(
            diagnostics.is_empty(),
            "enum variant body resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "pick")).kind else {
            panic!("expected enum variant path");
        };
        let hir::QPath::TypeRelative(receiver, segment) = path else {
            panic!("expected type-relative enum variant path: {path:?}");
        };
        assert_eq!(segment.ident.as_str(), "Yes");
        let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
            panic!("expected enum receiver path: {receiver:?}");
        };
        assert!(matches!(receiver_path.res(), hir::Res::Def(_)));
    }

    #[test]
    fn resolves_nested_enum_variant_path_in_body() {
        let (package, diagnostics) = lower(
            "mod values { enum Choice { Yes, No } } fn pick() -> values::Choice { values::Choice::Yes }",
        );
        assert!(
            diagnostics.is_empty(),
            "nested enum variant body resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "pick")).kind else {
            panic!("expected nested enum variant path");
        };
        let hir::QPath::TypeRelative(receiver, segment) = path else {
            panic!("expected type-relative nested enum variant path: {path:?}");
        };
        assert_eq!(segment.ident.as_str(), "Yes");
        let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
            panic!("expected nested enum receiver path: {receiver:?}");
        };
        assert_eq!(
            receiver_path
                .segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            ["values", "Choice"]
        );
    }

    #[test]
    fn resolves_self_in_impl_method_signature() {
        let (package, diagnostics) =
            lower("struct Value; impl Value { fn identity(&self) -> Self { self } }");
        assert!(
            diagnostics.is_empty(),
            "impl Self resolution emitted diagnostics: {diagnostics:?}"
        );
        let impl_block = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl item should be present");
        let method = impl_block
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(method) if item.name.as_str() == "identity" => {
                    Some(method)
                }
                _ => None,
            })
            .expect("impl method should be present");
        assert_eq!(type_path(&method.sig.output).res(), hir::Res::SelfTy);
    }

    #[test]
    fn resolves_associated_projection_from_generic_body() {
        let (package, diagnostics) = lower("fn project<T>(value: T) -> i64 { T::VALUE }");
        assert!(
            diagnostics.is_empty(),
            "generic projection body resolution emitted diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Path(path) = &body_expr(function(&package, "project")).kind else {
            panic!("expected associated projection path");
        };
        let hir::QPath::TypeRelative(receiver, segment) = path else {
            panic!("expected type-relative associated projection path: {path:?}");
        };
        assert!(matches!(
            receiver.kind,
            hir::TypeExprKind::Path(hir::QPath::Resolved(_, ref path))
                if matches!(path.res(), hir::Res::Generic(_))
        ));
        assert_eq!(segment.ident.as_str(), "VALUE");
    }

    #[test]
    fn resolves_trait_generic_in_synthesized_default_method() {
        let (package, diagnostics) = lower(
            "trait Pair<A, B = Self> { fn compare(&self, a: &A, b: &B) {} } struct Foo; struct Bar; struct Value; impl Pair<Foo, Bar> for Value {}",
        );
        assert!(
            diagnostics.is_empty(),
            "trait default generic resolution emitted diagnostics: {diagnostics:?}"
        );
        let def_id = |package: &hir::HirPackage, name: &str| {
            package
                .items
                .iter()
                .find_map(|item| match &item.kind {
                    hir::ItemKind::Struct(def) if def.name.as_str() == name => {
                        Some(item.def_id.clone())
                    }
                    _ => None,
                })
                .unwrap_or_else(|| panic!("missing struct `{name}`"))
        };
        let assert_method = |package: &hir::HirPackage, expected_b: hir::Res| {
            let foo = def_id(package, "Foo");
            let impl_item = package
                .items
                .iter()
                .find_map(|item| match &item.kind {
                    hir::ItemKind::Impl(impl_block) => Some(impl_block),
                    _ => None,
                })
                .expect("missing trait impl");
            let method = impl_item
                .items
                .iter()
                .find_map(|item| match &item.kind {
                    hir::ImplItemKind::Method(method) if item.name.as_str() == "compare" => {
                        Some(method)
                    }
                    _ => None,
                })
                .unwrap_or_else(|| panic!("missing synthesized trait method"));
            assert_eq!(
                type_path(&method.sig.inputs[1].ty).res(),
                hir::Res::Def(foo.clone())
            );
            assert_eq!(type_path(&method.sig.inputs[2].ty).res(), expected_b);
        };
        let bar = def_id(&package, "Bar");
        assert_method(&package, hir::Res::Def(bar));

        let (default_package, default_diagnostics) = lower(
            "trait Pair<A, B = Self> { fn compare(&self, a: &A, b: &B) {} } struct Foo; struct Other; impl Pair<Foo> for Other {}",
        );
        assert!(
            default_diagnostics.is_empty(),
            "defaulted trait generic resolution emitted diagnostics: {default_diagnostics:?}"
        );
        assert_method(&default_package, hir::Res::SelfTy);
    }

    #[test]
    fn resolves_paths_in_if_branches() {
        let (package, diagnostics) = lower(
            "fn choose(flag: bool, left: i64, right: i64) -> i64 { if flag { left } else { right } }",
        );
        assert!(
            diagnostics.is_empty(),
            "if branch resolution diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::If(condition, then_branch, Some(else_branch)) =
            &body_expr(function(&package, "choose")).kind
        else {
            panic!("expected if expression");
        };
        fn branch_path(expression: &hir::Expr) -> &hir::QPath {
            let expression = match &expression.kind {
                hir::ExprKind::Block(block) => {
                    block.expr.as_deref().expect("branch block expression")
                }
                _ => expression,
            };
            let hir::ExprKind::Path(path) = &expression.kind else {
                panic!("expected branch path: {expression:?}");
            };
            path
        }
        for expression in [
            condition.as_ref(),
            then_branch.as_ref(),
            else_branch.as_ref(),
        ] {
            let path = branch_path(expression);
            assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        }
    }

    #[test]
    fn resolves_match_binding_in_guard_and_body() {
        let (package, diagnostics) = lower(
            "fn classify(value: i64) -> i64 { match value { candidate if candidate > 0 => candidate, _ => 0 } }",
        );
        assert!(
            diagnostics.is_empty(),
            "match binding resolution diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Match(_, arms) = &body_expr(function(&package, "classify")).kind else {
            panic!("expected match expression");
        };
        let first = &arms[0];
        let hir::ExprKind::Binary(_, guard_lhs, _) =
            &first.guard.as_ref().expect("match guard").kind
        else {
            panic!("expected match guard comparison: {first:?}");
        };
        let hir::ExprKind::Path(guard_path) = &guard_lhs.kind else {
            panic!("expected guard binding path: {guard_lhs:?}");
        };
        assert!(
            matches!(guard_path.res(), hir::Res::Local(_)),
            "path: {guard_path:?}"
        );
        let hir::ExprKind::Path(body_path) = &first.body.kind else {
            panic!("expected match body binding path: {:?}", first.body);
        };
        assert!(
            matches!(body_path.res(), hir::Res::Local(_)),
            "path: {body_path:?}"
        );
    }

    #[test]
    fn resolves_for_loop_binding_and_capture() {
        let (package, diagnostics) = lower(
            "fn sum(limit: i64) -> i64 { let mut total = 0; for index in 0..limit { total = total + index; } total }",
        );
        assert!(
            diagnostics.is_empty(),
            "for-loop resolution diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "sum");
        let body = function.body.as_ref().expect("sum body");
        let loop_block = body
            .stmts
            .iter()
            .find_map(|stmt| match &stmt.kind {
                hir::StmtKind::Expr(hir::Expr {
                    kind: hir::ExprKind::Block(block),
                    ..
                }) => Some(block),
                _ => None,
            })
            .expect("desugared for-loop block");
        let index_local = loop_block
            .stmts
            .iter()
            .find_map(|stmt| match &stmt.kind {
                hir::StmtKind::Local(local)
                    if matches!(
                        local.pat.kind,
                        hir::PatKind::Binding { ref name, .. } if name.as_str() == "index"
                    ) =>
                {
                    Some(local)
                }
                _ => None,
            })
            .expect("for-loop binding declaration");
        assert!(matches!(index_local.pat.kind, hir::PatKind::Binding { .. }));
        let loop_body = loop_block
            .stmts
            .iter()
            .find_map(|stmt| match &stmt.kind {
                hir::StmtKind::Expr(hir::Expr {
                    kind: hir::ExprKind::While(_, body),
                    ..
                }) => Some(body),
                _ => None,
            })
            .expect("desugared for-loop body");
        let hir::StmtKind::Semi(assignment) = &loop_body.stmts[0].kind else {
            panic!("expected loop assignment: {loop_body:?}");
        };
        let hir::ExprKind::Assign(lhs, rhs) = &assignment.kind else {
            panic!("expected loop assignment expression: {assignment:?}");
        };
        let hir::ExprKind::Path(total_lhs) = &lhs.kind else {
            panic!("expected total assignment target: {lhs:?}");
        };
        assert!(matches!(total_lhs.res(), hir::Res::Local(_)));
        let hir::ExprKind::Binary(_, total_rhs, index_rhs) = &rhs.kind else {
            panic!("expected total plus index expression: {rhs:?}");
        };
        for expression in [total_rhs.as_ref(), index_rhs.as_ref()] {
            let hir::ExprKind::Path(path) = &expression.kind else {
                panic!("expected local path in loop body: {expression:?}");
            };
            assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        }
        let hir::ExprKind::Path(path) = &body_expr(function).kind else {
            panic!("expected final total path");
        };
        assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
    }

    #[test]
    fn resolves_self_field_access_in_impl_body() {
        let (package, diagnostics) =
            lower("struct Value { field: i64 } impl Value { fn get(&self) -> i64 { self.field } }");
        assert!(
            diagnostics.is_empty(),
            "impl body resolution diagnostics: {diagnostics:?}"
        );
        let impl_block = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl item");
        let method = impl_block
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(method) if item.name.as_str() == "get" => Some(method),
                _ => None,
            })
            .expect("get method");
        let hir::ExprKind::Path(path) = &body_expr(method).kind else {
            panic!("expected self field path: {:?}", body_expr(method));
        };
        assert!(matches!(path.res(), hir::Res::Local(_)), "path: {path:?}");
        assert_eq!(
            path.segments()
                .last()
                .expect("self field path segment")
                .ident
                .as_str(),
            "field"
        );
    }

    #[test]
    fn resolves_imported_module_function_in_body() {
        let (package, diagnostics) = lower(
            "mod helpers { fn identity(value: i64) -> i64 { value } } mod nested { use crate::helpers; fn call(value: i64) -> i64 { helpers::identity(value) } }",
        );
        assert!(
            diagnostics.is_empty(),
            "imported module call resolution diagnostics: {diagnostics:?}"
        );
        let hir::ExprKind::Call(callee, _) = &body_expr(function(&package, "call")).kind else {
            panic!("expected imported module call");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected imported module callee path: {callee:?}");
        };
        let hir::QPath::Resolved(_, resolved) = path else {
            panic!("expected ordinary resolved imported function path: {path:?}");
        };
        assert!(matches!(resolved.res(), hir::Res::Def(_)), "path: {path:?}");
        assert_eq!(
            resolved
                .segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            vec!["helpers", "identity"]
        );
    }

    #[test]
    fn resolves_impl_const_generic_in_method_body() {
        let (package, diagnostics) = lower(
            "struct Array<T, const N: usize>([T; N]); impl<T, const N: usize> Array<T, N> { fn length(&self) -> usize { N } }",
        );
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `N`")),
            "impl const-generic body resolution emitted diagnostics: {diagnostics:?}"
        );
        let impl_block = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl item");
        let method = impl_block
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(method) if item.name.as_str() == "length" => Some(method),
                _ => None,
            })
            .expect("length method");
        let hir::ExprKind::Path(path) = &body_expr(method).kind else {
            panic!("expected const generic path in impl body");
        };
        assert!(matches!(path.res(), hir::Res::Generic(_)), "path: {path:?}");
    }

    #[test]
    fn method_generic_body_uses_method_not_impl_parameter() {
        let (package, diagnostics) = lower(
            "struct Holder<T>(T); impl<T> Holder<T> { fn convert<U>(value: U) -> U { value } }",
        );
        assert!(
            diagnostics.is_empty(),
            "method-generic body resolution emitted diagnostics: {diagnostics:?}"
        );
        let impl_block = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl item");
        let impl_generic = &impl_block.generics.params[0].def_id;
        let method = impl_block
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(method) if item.name.as_str() == "convert" => {
                    Some(method)
                }
                _ => None,
            })
            .expect("convert method");
        let output = type_path(&method.sig.output);
        let hir::Res::Generic(method_generic) = &output.res() else {
            panic!("expected method generic output path: {output:?}");
        };
        assert_ne!(
            method_generic, impl_generic,
            "method generic reused the enclosing impl generic identity"
        );
    }

    #[test]
    fn resolves_const_generic_in_nested_impl_header_and_body() {
        let (package, diagnostics) = lower(
            "struct MaybeUninit<T>(T); trait AsRef<T> {} impl<T, const N: usize> AsRef<[MaybeUninit<T>; N]> for MaybeUninit<[T; N]> { fn as_ref(&self) -> &[MaybeUninit<T>; N] { self } }",
        );
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `N`")),
            "nested impl const-generic resolution emitted diagnostics: {diagnostics:?}"
        );
        assert!(
            package
                .items
                .iter()
                .any(|item| matches!(item.kind, hir::ItemKind::Impl(_))),
            "nested const-generic impl should be lowered"
        );
    }

    #[test]
    fn resolves_const_generic_in_array_body_expression() {
        let (package, diagnostics) = lower("fn make<const N: usize>() -> [u8; N] { [0; N] }");
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved value path `N`")),
            "array const-generic body resolution emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "make");
        let hir::ExprKind::ArrayRepeat { len, .. } = &body_expr(function).kind else {
            panic!("expected repeat array expression");
        };
        let hir::ExprKind::Path(path) = &len.kind else {
            panic!("expected const generic repeat count path: {len:?}");
        };
        assert!(matches!(path.res(), hir::Res::Generic(_)), "path: {path:?}");
    }

    #[test]
    fn resolves_trait_generic_in_default_method_body() {
        let (package, diagnostics) = lower(
            "trait Compare<Rhs> { fn compare(&self, other: &Rhs) -> bool { true } } struct Value; impl Compare<Value> for Value {}",
        );
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved Type path `Rhs`")),
            "trait-generic body resolution emitted diagnostics: {diagnostics:?}"
        );
        let trait_item = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Trait(trait_def) => Some(trait_def),
                _ => None,
            })
            .expect("trait item");
        let method = trait_item
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::TraitItemKind::Method(method) if item.name.as_str() == "compare" => {
                    Some(method)
                }
                _ => None,
            })
            .expect("compare method");
        let ty = type_path(&method.sig.inputs[1].ty);
        assert!(matches!(ty.res(), hir::Res::Generic(_)), "path: {ty:?}");
    }

    #[test]
    fn resolves_prelude_type_in_function_body_signature() {
        let (package, diagnostics) = lower(
            "mod prelude { pub mod v1 { pub struct PhantomData<T>; } } fn marker<T>() -> PhantomData<T> { PhantomData }",
        );
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_string()
                .contains("unresolved Type path `PhantomData`")),
            "imported prelude type resolution emitted diagnostics: {diagnostics:?}"
        );
        let output = type_path(&function(&package, "marker").sig.output);
        assert!(matches!(output.res(), hir::Res::Def(_)), "path: {output:?}");
    }

    #[test]
    fn resolves_qualified_module_type_in_function_body() {
        let (package, diagnostics) =
            lower("mod types { pub struct Token; } fn make() -> types::Token { types::Token }");
        assert!(
            diagnostics.is_empty(),
            "qualified module body resolution emitted diagnostics: {diagnostics:?}"
        );
        let function = function(&package, "make");
        let output = type_path(&function.sig.output);
        assert!(matches!(output.res(), hir::Res::Def(_)), "path: {output:?}");
        let hir::ExprKind::Path(path) = &body_expr(function).kind else {
            panic!("expected qualified module path body");
        };
        assert!(matches!(path.res(), hir::Res::Def(_)), "path: {path:?}");
    }

    #[test]
    fn resolves_parent_module_import_in_nested_function_body() {
        let (package, diagnostics) = lower(
            "mod helpers { pub fn answer() -> i64 { 1 } } mod outer { use crate::helpers; mod inner { fn call() -> i64 { helpers::answer() } } }",
        );
        assert!(
            diagnostics.is_empty(),
            "nested child should inherit the parent import: {diagnostics:?}"
        );
        let call = function(&package, "call");
        let hir::ExprKind::Call(callee, _) = &body_expr(call).kind else {
            panic!(
                "expected nested imported function call: {:?}",
                body_expr(call)
            );
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected nested imported callee path: {callee:?}");
        };
        assert!(matches!(path.res(), hir::Res::Def(_)), "path: {path:?}");
        assert_eq!(
            path.segments()
                .iter()
                .map(|segment| segment.ident.as_str())
                .collect::<Vec<_>>(),
            vec!["helpers", "answer"]
        );
    }

    #[test]
    fn resolves_self_in_trait_default_function_body() {
        let (package, diagnostics) = lower(
            "trait Identity { fn make(&self) -> Self { Self {} } } struct Value; impl Identity for Value {}",
        );
        assert!(
            diagnostics.is_empty(),
            "trait default body should resolve `Self`: {diagnostics:?}"
        );
        let trait_item = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Trait(def) => Some(def),
                _ => None,
            })
            .expect("trait item");
        let method = trait_item
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::TraitItemKind::Method(method) if item.name.as_str() == "make" => Some(method),
                _ => None,
            })
            .expect("trait default method");
        let hir::ExprKind::Struct(path, _) = &body_expr(method).kind else {
            panic!(
                "expected trait default body constructor: {:?}",
                body_expr(method)
            );
        };
        assert!(matches!(path.res(), hir::Res::Def(_) | hir::Res::SelfTy));
    }

    #[test]
    fn resolves_const_impl_array_parameter_in_function_body() {
        let (package, diagnostics) = lower(
            "trait Len { fn len(&self) -> usize; } impl<T, const N: usize> Len for [T; N] { fn len(&self) -> usize { N } }",
        );
        assert!(
            diagnostics.is_empty(),
            "array const parameter should resolve in impl method body: {diagnostics:?}"
        );
        let impl_block = package
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl item");
        let method = impl_block
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(method) if item.name.as_str() == "len" => Some(method),
                _ => None,
            })
            .expect("len method");
        let hir::ExprKind::Path(path) = &body_expr(method).kind else {
            panic!("expected const parameter path: {:?}", body_expr(method));
        };
        assert!(matches!(path.res(), hir::Res::Generic(_)), "path: {path:?}");
    }

    #[test]
    fn resolves_associated_function_from_generic_type_in_body() {
        let (package, diagnostics) = lower(
            "struct Wrapper<T>(T); impl<T> Wrapper<T> { fn new(value: T) -> Self { Self(value) } } fn call(value: i64) -> Wrapper<i64> { Wrapper::new(value) }",
        );
        assert!(
            diagnostics.is_empty(),
            "associated function path should resolve its type base: {diagnostics:?}"
        );
        let call = function(&package, "call");
        let hir::ExprKind::Call(callee, _) = &body_expr(call).kind else {
            panic!("expected associated function call");
        };
        let hir::ExprKind::Path(path) = &callee.kind else {
            panic!("expected associated function path: {callee:?}");
        };
        let hir::QPath::TypeRelative(receiver, segment) = path else {
            panic!("expected type-relative associated function path: {path:?}");
        };
        assert_eq!(segment.ident.as_str(), "new");
        let hir::TypeExprKind::Path(receiver_path) = &receiver.kind else {
            panic!("expected associated function receiver path: {receiver:?}");
        };
        assert!(matches!(receiver_path.res(), hir::Res::Def(_)));
    }

    #[test]
    fn resolves_imported_type_constructor_in_function_body() {
        let (package, diagnostics) = lower(
            "mod types { pub struct Token; } mod consumer { use crate::types::Token; fn make() -> Token { Token {} } }",
        );
        assert!(
            diagnostics.is_empty(),
            "imported type constructor should resolve in function body: {diagnostics:?}"
        );
        let make = function(&package, "make");
        let hir::ExprKind::Struct(path, _) = &body_expr(make).kind else {
            panic!(
                "expected imported constructor expression: {:?}",
                body_expr(make)
            );
        };
        assert!(matches!(path.res(), hir::Res::Def(_)), "path: {path:?}");
    }
}

#[test]
fn transform_expr_rejects_dynamic_import() {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let expr = ast::Expr::from(ast::ExprKind::Invoke(ast::ExprInvoke {
        span: Span::null(),
        target: ast::ExprInvokeTarget::Function(ast::Name::ident(ident("import"))),
        args: Vec::new(),
        kwargs: Vec::new(),
    }));

    expect_lowering_diagnostic(
        &mut generator,
        |g| g.transform_expr_to_hir(&expr),
        "dynamic import is only supported in interpret mode",
    );
}

#[test]
fn transform_expr_rejects_match_without_scrutinee() {
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
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
        &mut generator,
        |g| g.transform_expr_to_hir(&expr),
        "`for` loop pattern must be a simple binding",
    );
}

/// Fast, targeted repro for the "unresolved type path `std::std::os::raw::
/// c_int`"-shaped diagnostics seen in the full std typecheck run
/// (`/tmp/typecheck7.log`) — a literal doubled package/sub-crate prefix
/// baked into the *reported* path segments. Mirrors real vendored std's own
/// two-segment-root convention (`rs_relative_to_module_segments`: `std/os/
/// raw.rs` -> `["std", "std", "os", "raw"]`) and a sibling module
/// referencing it via a plain absolute path spelled with the crate's own
/// name (`std::os::raw::c_int`, as real `std/sys/pal/itron/abi.rs` does via
/// `crate::os::raw::c_int` — `crate::` and a literal `std::` prefix
/// normalize to the same absolute reference once the crate's own name is
/// `std`). Before any fix, this asserts on whatever actually comes out
/// (documenting the real, traced behavior) rather than guessing.
#[test]
fn transform_package_plain_absolute_path_into_vendored_subcrate() -> Result<()> {
    let c_int_item = make_struct("c_int", vec![("value", int_ty())]);

    let f_item = make_fn(
        "f",
        Vec::new(),
        ast::Ty::path(ast::Path::plain(vec![
            ident("std"),
            ident("os"),
            ident("raw"),
            ident("c_int"),
        ])),
        ast::Expr::value(ast::Value::unit()),
    );

    let items = vec![
        (
            vec![
                "std".to_string(),
                "std".to_string(),
                "os".to_string(),
                "raw".to_string(),
            ],
            c_int_item,
        ),
        (
            vec![
                "std".to_string(),
                "std".to_string(),
                "sys".to_string(),
                "pal".to_string(),
                "itron".to_string(),
            ],
            f_item,
        ),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let f_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "f" => Some(func),
            _ => None,
        })
        .expect("`f` function present");
    let hir::TypeExprKind::Path(ret_path) = &f_hir.sig.output.kind else {
        panic!(
            "expected `f`'s return type to lower to a path, got {:?}",
            f_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "plain absolute reference `std::os::raw::c_int` (written from \
         within the vendored `std` sub-crate itself) must resolve to the \
         real `c_int` struct at [\"std\",\"std\",\"os\",\"raw\",\"c_int\"] — \
         got unresolved path {ret_path:?}"
    );

    Ok(())
}

/// Combines the two previously-isolated shapes
/// (`transform_package_resolves_import_nested_inside_inline_module`'s
/// nested-`mod`-scoped `use`, and
/// `transform_package_resolves_self_plus_variants_group_import`'s
/// `Foo::{self, Variant}` group import) with the one thing neither tested
/// alone: a *third*, unrelated sibling module referencing the bare name
/// with no `use` of its own — real std's actual consumer shape, which
/// relies entirely on the AST resolver's automatic
/// prelude-injection tier, not an explicit import. If `Option`/`Result`
/// are still showing up unresolved in the full std run after both prior
/// fixes landed, this is the next shape to isolate — this repro exists to
/// find out whether it does or doesn't, not to assume either way.
#[test]
fn transform_package_resolves_self_group_import_nested_in_module_via_default_prelude() -> Result<()>
{
    let inner_item = make_struct("Foo", vec![("value", int_ty())]);

    // `pub use crate::inner::Foo::{self, Variant};` nested inside `mod
    // ambiguous_macros_only { .. }`, itself nested inside `prelude::v1` —
    // exactly `core::prelude::v1`'s real shape for `Option`/`Result`.
    let prelude_use = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("inner")),
                ast::ItemImportTree::Ident(ident("Foo")),
                ast::ItemImportTree::Group(ast::ItemImportGroup {
                    items: vec![
                        ast::ItemImportTree::SelfMod,
                        ast::ItemImportTree::Ident(ident("Variant")),
                    ],
                }),
            ],
        }),
    }));

    // A third, unrelated module — no `use` of its own, relying entirely on
    // the default-prelude mechanism to see the bare name.
    let make_fn_item = make_fn(
        "make",
        Vec::new(),
        ty_ident("Foo"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Foo"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    let items = vec![
        (vec!["inner".to_string()], inner_item),
        (vec!["prelude".to_string(), "v1".to_string()], prelude_use),
        (vec!["other".to_string()], make_fn_item),
    ];
    let package = package_from_items_with_paths_as(PackageId::new("test"), items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "bare `Foo` return type in a third, unrelated module must resolve \
         via the nested-`mod`-scoped `Foo::{{self, Variant}}` group import's \
         `self` member, through the default-prelude tier — got unresolved \
         path {ret_path:?}"
    );

    Ok(())
}

/// Real vendored std generates a batch of C-FFI type aliases via an
/// item-position `macro_rules!` invocation (`std/os/raw/mod.rs`'s
/// `alias_core_ffi! { c_char c_int .. }`, expanding to `pub type c_int =
/// core::ffi::c_int;` per name). `predeclare_items`'s `ItemKind::Macro` arm
/// previously just warned and dropped any such invocation unconditionally
/// — meaning every item it would have generated was never defined at all
/// (not a name-resolution gap, a missing-expansion one). This confirms the
/// real fp-lang macro engine (wired in via `IntrinsicNormalizer::
/// expand_item_macro`/`collect_macro_rules_defs`, mirroring how
/// expression-position macros already flow through the same normalizer)
/// now actually expands it into a real, resolvable item.
#[test]
fn transform_package_expands_item_position_macro_rules_invocation() -> Result<()> {
    let parser = fp_lang::ast::FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"
        struct Marker { value: i64 }

        macro_rules! alias_marker {
            ($($t:ident)*) => {$(
                pub type $t = Marker;
            )*}
        }

        alias_marker! { c_int }

        fn make() -> c_int {
            Marker { value: 1 }
        }
    "#;
    let items = parser
        .parse_items_ast(source)
        .map_err(|e| crate::error::optimization_error(format!("{e:?}")))?;

    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    )
    .with_intrinsic_normalizer(fp_lang::FerroIntrinsicNormalizer::new());
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "`c_int` generated by `alias_marker! {{ c_int }}` must resolve to \
         the real `Marker` struct via the macro's own `pub type $t = \
         Marker;` expansion — got unresolved path {ret_path:?}"
    );

    Ok(())
}

#[test]
fn transform_package_expands_item_macro_in_nested_module_before_resolution() -> Result<()> {
    let parser = fp_lang::ast::FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            mod num {
                mod niche_types {
                    macro_rules! define_valid_range_type {
                        ($($(#[$m:meta])*$vis:vis struct$name:ident($int:ident is$pat:pat);)+) => {
                            $($(#[$m])* $vis struct$name($int);)+
                        };
                    }

                    define_valid_range_type! {
                        pub struct Nanoseconds(u32 is ..0 | 1..);
                    }

                    impl Nanoseconds {}
                }
            }
            "#,
        )
        .map_err(|e| crate::error::optimization_error(format!("{e:?}")))?;
    let package = package_from_items(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    )
    .with_intrinsic_normalizer(fp_lang::FerroIntrinsicNormalizer::new());
    let program = generator.transform_package(&package)?;

    assert!(program.items.iter().any(|item| {
        matches!(
            &item.kind,
            hir::ItemKind::Struct(def) if def.name.as_str() == "Nanoseconds"
        )
    }));
    assert!(
        generator
            .take_diagnostics()
            .get_diagnostics()
            .iter()
            .all(|diagnostic| !diagnostic.message.contains("Nanoseconds"))
    );
    Ok(())
}

/// Macro definitions have module scope independent of the provider's
/// flattened file order. An invocation from an earlier PackageItem must see
/// a definition discovered in a later PackageItem, just as rustc's resolver
/// does after building the macro scope.
#[test]
fn transform_package_expands_macro_invocation_before_definition() -> Result<()> {
    let parser = fp_lang::ast::FerroPhaseParser::new();
    parser.clear_diagnostics();
    let invocation = parser
        .parse_items_ast("make_alias! { c_int }")
        .map_err(|e| crate::error::optimization_error(format!("{e:?}")))?
        .into_iter()
        .next()
        .expect("macro invocation");
    let definitions = parser
        .parse_items_ast(
            r#"
            macro_rules! make_alias {
                ($name:ident) => { pub type $name = i64; }
            }
            "#,
        )
        .map_err(|e| crate::error::optimization_error(format!("{e:?}")))?;

    let package = package_from_items_with_paths(
        vec![(vec!["use_site".to_string()], invocation)]
            .into_iter()
            .chain(
                definitions
                    .into_iter()
                    .map(|item| (vec!["definition_site".to_string()], item)),
            )
            .collect(),
    )?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    )
    .with_intrinsic_normalizer(fp_lang::FerroIntrinsicNormalizer::new());
    let program = generator.transform_package(&package)?;

    let _ = program;
    Ok(())
}

/// Real `core::panic::Location` shape, inside the vendored real-`std`
/// package's own two-segment sub-crate module paths (`["std", "core", ...]`
/// — see `transform_package_plain_absolute_path_into_vendored_subcrate`'s
/// doc comment for why the bundled package needs this): `panic.rs` (module
/// path `["std", "core", "panic"]`) has `mod location;` (a private
/// submodule, `["std", "core", "panic", "location"]`) plus `pub use
/// self::location::Location;` re-exporting it one level up, and `cell.rs`
/// (module path `["std", "core", "cell"]`) references the type via the
/// fully-qualified absolute path `crate::panic::Location` with no `use` of
/// its own — real `core/cell.rs`'s actual style (`borrowed_at: Cell<Option
/// <&'static crate::panic::Location<'static>>>`). Isolates whether the
/// `crate::`-prefixed absolute path's crate-root candidate walk
/// (`name_to_hir_path_with_scope`'s `crate_root_candidates`) resolves
/// against the correct two-segment sub-crate root before falling back to
/// the wrong one-segment root, for a path that depends on a same-file-level
/// re-export chain rather than a direct definition.
#[test]
fn transform_package_resolves_crate_absolute_path_to_self_reexport_in_vendored_subcrate()
-> Result<()> {
    let location_item = make_struct("Location", vec![("value", int_ty())]);

    let panic_self_reexport = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Public,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::SelfMod,
                ast::ItemImportTree::Ident(ident("location")),
                ast::ItemImportTree::Ident(ident("Location")),
            ],
        }),
    }));

    let cell_fn_item = make_fn(
        "make",
        Vec::new(),
        ast::Ty::path(ast::Path::plain(vec![
            ident("crate"),
            ident("panic"),
            ident("Location"),
        ])),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Location"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    let items = vec![
        (
            vec![
                "std".to_string(),
                "core".to_string(),
                "panic".to_string(),
                "location".to_string(),
            ],
            location_item,
        ),
        (
            vec!["std".to_string(), "core".to_string(), "panic".to_string()],
            panic_self_reexport,
        ),
        (
            vec!["std".to_string(), "core".to_string(), "cell".to_string()],
            cell_fn_item,
        ),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "`crate::panic::Location` (referenced from `core::cell`, resolving \
         through `core::panic`'s own `pub use self::location::Location;` \
         re-export) must resolve — got unresolved path {ret_path:?}"
    );

    Ok(())
}

/// Real `core::fmt`'s own shape: `use crate::{result};` (a whole-*module*
/// import, brought in via a braced group with a single plain `Ident`
/// member — not a glob, not a named-item import) followed by a bare
/// relative reference to that module's own item (`result::Result`, no
/// further `use` of `Result` itself) — mirrors `fmt/mod.rs`'s real `use
/// crate::{iter, mem, result, str};` plus its own `pub type Result =
/// result::Result<(), Error>;`. Inside the vendored real-`std` package's
/// two-segment sub-crate module paths, so this also exercises whichever
/// resolution path a *relative*, non-`crate::`-prefixed multi-segment name
/// takes (distinct from the `crate::`-prefixed absolute-path fix above).
#[test]
fn transform_package_resolves_whole_module_import_then_relative_item_reference() -> Result<()> {
    let result_item = make_struct("Result", vec![("value", int_ty())]);

    let fmt_module_import = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Private,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Group(ast::ItemImportGroup {
                    items: vec![ast::ItemImportTree::Ident(ident("result"))],
                }),
            ],
        }),
    }));

    let fmt_fn_item = make_fn(
        "make",
        Vec::new(),
        ast::Ty::path(ast::Path::plain(vec![ident("result"), ident("Result")])),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Result"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    let items = vec![
        (
            vec!["std".to_string(), "core".to_string(), "result".to_string()],
            result_item,
        ),
        (
            vec!["std".to_string(), "core".to_string(), "fmt".to_string()],
            fmt_module_import,
        ),
        (
            vec!["std".to_string(), "core".to_string(), "fmt".to_string()],
            fmt_fn_item,
        ),
    ];
    let package = package_from_items_with_paths_as(PackageId::new("std"), items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("std"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "`result::Result` (referenced from `core::fmt` after `use crate::\
         {{result}};` brings the `result` module itself into scope) must \
         resolve — got unresolved path {ret_path:?}"
    );

    Ok(())
}

/// Real `core::any`'s own shape: `use crate::intrinsics::{self,
/// type_id_vtable};` — a *module*-alias `self` combined, in the same
/// group, with a specific named sibling item (`type_id_vtable`), unlike
/// `transform_package_resolves_self_plus_variants_group_import`'s
/// enum-variant shape (`Foo::{self, Variant}`, where `self` means "the
/// enclosing type", not "the enclosing module") or the whole-module-only
/// case above (`use crate::{result};`, no combined named sibling).
/// Followed by a relative call through the module alias
/// (`intrinsics::make_value()`), matching `core::any`'s own `intrinsics::
/// type_id::<T>()`.
#[test]
fn transform_package_resolves_module_self_plus_named_item_group_import() -> Result<()> {
    let helper_item = make_struct("Helper", vec![("value", int_ty())]);

    let make_value_fn = make_fn(
        "make_value",
        Vec::new(),
        ty_ident("Helper"),
        ast::Expr::from(ast::ExprKind::Struct(ast::ExprStruct::new_ident(
            ident("Helper"),
            vec![ast::ExprField::new(
                ident("value"),
                ast::Expr::value(ast::Value::int(1)),
            )],
        ))),
    );

    let import_item = ast::Item::from(ast::ItemKind::Import(ast::ItemImport {
        attrs: Vec::new(),
        visibility: ast::Visibility::Private,
        style: ast::ItemImportStyle::Plain,
        tree: ast::ItemImportTree::Path(ast::ItemImportPath {
            segments: vec![
                ast::ItemImportTree::Crate,
                ast::ItemImportTree::Ident(ident("intrinsics")),
                ast::ItemImportTree::Group(ast::ItemImportGroup {
                    items: vec![
                        ast::ItemImportTree::SelfMod,
                        ast::ItemImportTree::Ident(ident("Helper")),
                    ],
                }),
            ],
        }),
    }));

    let caller_fn = make_fn(
        "make",
        Vec::new(),
        ast::Ty::path(ast::Path::plain(vec![ident("intrinsics"), ident("Helper")])),
        ast::Expr::from(ast::ExprKind::Invoke(ast::ExprInvoke {
            target: ast::ExprInvokeTarget::Function(ast::Name::path(ast::Path::new(
                fp_core::ast::path::PathPrefix::Plain,
                vec![ident("intrinsics").into(), ident("make_value").into()],
            ))),
            args: Vec::new(),
            kwargs: Vec::new(),
            span: Span::default(),
        })),
    );

    let items = vec![
        (vec!["intrinsics".to_string()], helper_item),
        (vec!["intrinsics".to_string()], make_value_fn),
        (vec!["caller".to_string()], import_item),
        (vec!["caller".to_string()], caller_fn),
    ];
    let package = package_from_items_with_paths(items)?;
    let mut generator = AstToHirLowerer::new(
        std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
            fp_core::ast::package::provider::EmptyProvider,
        ))),
        hir::SharedHirProgram::new(hir::HirProgram::new()),
        hir::PackageId::new("test"),
    );
    let program = generator.transform_package(&package)?;

    let make_fn_hir = program
        .items
        .iter()
        .find_map(|item| match &item.kind {
            hir::ItemKind::Function(func) if func.sig.name.as_str() == "make" => Some(func),
            _ => None,
        })
        .expect("`make` function present");
    let hir::TypeExprKind::Path(ret_path) = &make_fn_hir.sig.output.kind else {
        panic!(
            "expected `make`'s return type to lower to a path, got {:?}",
            make_fn_hir.sig.output.kind
        );
    };
    assert!(
        ret_path.res().is_some(),
        "`intrinsics::Helper` (referenced from `caller` after `use crate::\
         intrinsics::{{self, Helper}};` brings the `intrinsics` module \
         itself into scope alongside a named sibling) must resolve — got \
         unresolved path {ret_path:?}"
    );

    Ok(())
}
