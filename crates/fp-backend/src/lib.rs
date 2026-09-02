// fp-backend: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - transforms: Focused AST/IR rewrites and lowering helpers
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod abi;
pub mod error;
pub mod lir_optimizer;
pub mod optimizer;
pub mod transforms;

pub use transforms as transformations;

/// Wraps a single already-parsed file's items as a one-member package,
/// obtained via a real `PackageProvider` (`FixedPackageProvider`, which
/// just hands back an already-built `AstPackage` — no filesystem
/// access), then `AstProgram::begin_package`. Deliberately does not
/// use `fp-lang`'s disk-resolving `single_file_provider`: callers here
/// (`roundtrip_items_via_hir`/`_dce`) receive already-fully-assembled
/// `File`s (e.g. `fp-shell` splices in its embedded std tree directly,
/// leaving behind `mod foo;` markers whose content already lives
/// elsewhere in the tree) that must not be re-resolved against a real
/// filesystem — `AstToHirLowerer::append_item` already walks nested `Module`
/// items structurally on its own, so no flattening is needed here either.
fn package_from_file(
    file: &fp_core::ast::File,
) -> fp_core::Result<(
    std::rc::Rc<fp_core::ast::program::AstProgram>,
    fp_core::ast::package::AstPackage,
)> {
    use fp_core::ast::package::provider::{FixedPackageProvider, PackageProvider};

    let package_id = fp_core::ast::package::PackageId::new("roundtrip");
    let mut items = file.items.clone();
    transforms::ast_to_hir::strip_doc_attrs_in_items(&mut items);
    let source = fp_core::ast::package::AstPackage::new(
        package_id.clone(),
        "roundtrip",
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), "roundtrip"),
        vec![fp_core::ast::Module {
            attrs: Vec::new(),
            name: fp_core::ast::Ident::new(""),
            collected_items: Vec::new(),
            items,
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }],
    );
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let source = provider
        .load_package_source(&package_id)
        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    let workspace = std::rc::Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
        provider,
    )));
    let data_layout = fp_core::lir::LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    let package = workspace.begin_package(package_id, source, data_layout);
    let package = package.borrow().clone();
    Ok((workspace, package))
}

pub fn roundtrip_items_via_hir(
    file: &fp_core::ast::File,
) -> fp_core::Result<Vec<fp_core::ast::Item>> {
    let (ast_program, package) = package_from_file(file)?;
    let mut generator = transforms::ast_to_hir::AstToHirLowerer::new(
        ast_program,
        fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new()),
        package.package_id.clone(),
    );
    generator.set_cfg_filtering(false);
    let program = generator.transform_package(&package)?;
    let hir_program = fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new());
    hir_program.publish_package(program.clone());
    let workspace = hir_program.borrow();
    transforms::hir_to_ast::HirToAstLifter::new(&program, &workspace).lift_items()
}

pub fn roundtrip_items_via_hir_target(
    file: &fp_core::ast::File,
    target_lang: &str,
) -> fp_core::Result<Vec<fp_core::ast::Item>> {
    let mut filtered = file.clone();
    let mut target_env = fp_core::cfg::TargetEnv::host();
    target_env.lang = Some(target_lang.to_owned());
    fp_core::cfg::filter_items_in_file(&mut filtered, &target_env);
    let (ast_program, package) = package_from_file(&filtered)?;
    let mut generator = transforms::ast_to_hir::AstToHirLowerer::new(
        ast_program,
        fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new()),
        package.package_id.clone(),
    );
    generator.set_target_lang(Some(target_lang));
    generator.set_cfg_filtering(true);
    let program = generator.transform_package(&package)?;
    let hir_program = fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new());
    hir_program.publish_package(program.clone());
    let workspace = hir_program.borrow();
    transforms::hir_to_ast::HirToAstLifter::new(&program, &workspace).lift_items()
}

pub fn roundtrip_items_via_hir_dce(
    file: &fp_core::ast::File,
) -> fp_core::Result<Vec<fp_core::ast::Item>> {
    let (ast_program, package) = package_from_file(file)?;
    let mut generator = transforms::ast_to_hir::AstToHirLowerer::new(
        ast_program,
        fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new()),
        package.package_id.clone(),
    );
    generator.set_cfg_filtering(false);
    let mut program = generator.transform_package(&package)?;
    optimizer::hir::eliminate_dead_code(&mut program, None);
    let hir_program = fp_core::hir::SharedHirProgram::new(fp_core::hir::HirProgram::new());
    hir_program.publish_package(program.clone());
    let workspace = hir_program.borrow();
    transforms::hir_to_ast::HirToAstLifter::new(&program, &workspace).lift_items()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{
        self, BlockStmt, Expr, ExprInvoke, ExprInvokeTarget, File, Ident, Item, ItemKind, Name, Ty,
    };
    use fp_core::span::Span;
    use std::path::PathBuf;

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn invoke(name: &str) -> Expr {
        Expr::from(ast::ExprKind::Invoke(ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(Name::from_ident(ident(name))),
            args: Vec::new(),
            kwargs: Vec::new(),
        }))
    }

    fn function_item(name: &str) -> Item {
        Item::from(ItemKind::DefFunction(
            ast::ItemDefFunction::new_simple(
                ident(name),
                ast::ExprBlock::new_expr(Expr::value(ast::Value::unit())),
            )
            .with_ret_ty(Ty::unit()),
        ))
    }

    #[test]
    fn roundtrip_via_hir_prunes_unused_functions_for_top_level_expr_roots() {
        let file = File {
            path: PathBuf::from("dce_example.fp"),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![
                function_item("used_helper"),
                function_item("unused_helper"),
                Item::from(ItemKind::Expr(invoke("used_helper"))),
            ],
        };

        let items = roundtrip_items_via_hir_dce(&file).expect("roundtrip should succeed");

        assert!(items.iter().any(|item| matches!(
            item.kind(),
            ItemKind::DefFunction(function) if function.name.as_str() == "used_helper"
        )));
        assert!(!items.iter().any(|item| matches!(
            item.kind(),
            ItemKind::DefFunction(function) if function.name.as_str() == "unused_helper"
        )));
        assert!(
            items
                .iter()
                .any(|item| matches!(item.kind(), ItemKind::Expr(_)))
        );
    }

    /// `let x = 1; let x = x + 1; x` is valid Rust shadowing (each `let`
    /// introduces a distinct binding), but re-emitting both as literal
    /// `val x = ...` would be a "conflicting declarations" error in Kotlin
    /// and similar targets. `HirToAstLifter` must rename the second binding
    /// (and any later reference to it) instead of reusing the same-block
    /// stale name.
    #[test]
    fn roundtrip_via_hir_renames_shadowed_let_bindings_for_target_emission() {
        let x_plus_one = Expr::from(ast::ExprKind::BinOp(ast::ExprBinOp {
            span: Span::null(),
            kind: fp_core::ops::BinOpKind::Add,
            lhs: Box::new(Expr::ident(ident("x"))),
            rhs: Box::new(Expr::value(ast::Value::int(1))),
        }));
        let block = ast::ExprBlock::new_stmts_expr(
            vec![
                ast::BlockStmt::Let(ast::StmtLet::new_simple(
                    ident("x"),
                    Expr::value(ast::Value::int(1)),
                )),
                ast::BlockStmt::Let(ast::StmtLet::new_simple(ident("x"), x_plus_one)),
            ],
            Expr::ident(ident("x")),
        );
        let func = ast::ItemDefFunction::new_simple(ident("shadow_test"), block)
            .with_ret_ty(Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)));
        let file = File {
            path: PathBuf::from("shadow.fp"),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![Item::from(ItemKind::DefFunction(func))],
        };

        let items = roundtrip_items_via_hir(&file).expect("roundtrip should succeed");
        let ItemKind::DefFunction(function) = items[0].kind() else {
            panic!("expected a function item, got {:?}", items[0].kind());
        };
        let stmts = &function.body.stmts;
        assert_eq!(stmts.len(), 3, "two lets + a tail expression");

        let first_name = let_binding_name(&stmts[0]);
        let second_name = let_binding_name(&stmts[1]);
        assert_eq!(first_name, "x", "first `let` keeps its source name");
        assert_ne!(
            second_name, "x",
            "second (shadowing) `let x` must be renamed to avoid a same-block redeclaration"
        );

        let tail_name = tail_expr_name(&stmts[2]);
        assert_eq!(
            tail_name, second_name,
            "the trailing `x` must resolve to the renamed second binding, not the stale first one"
        );
    }

    /// Rust allows a `mut` (or `&mut`-by-value-lowered) parameter to be
    /// reassigned directly in the body (`x = x + 1;`), but Kotlin parameters
    /// are always an implicit `val` — reassigning one is a compile error.
    /// `HirToAstLifter` must give such a parameter a renamed mutable local
    /// shadow instead of emitting a direct reassignment of the parameter.
    #[test]
    fn roundtrip_via_hir_renames_reassigned_parameters_for_target_emission() {
        let x_plus_one = Expr::from(ast::ExprKind::BinOp(ast::ExprBinOp {
            span: Span::null(),
            kind: fp_core::ops::BinOpKind::Add,
            lhs: Box::new(Expr::ident(ident("x"))),
            rhs: Box::new(Expr::value(ast::Value::int(1))),
        }));
        let assign = Expr::from(ast::ExprKind::Assign(ast::ExprAssign {
            span: Span::null(),
            target: Box::new(Expr::ident(ident("x"))),
            value: Box::new(x_plus_one),
        }));
        let block = ast::ExprBlock::new_stmts_expr(
            vec![BlockStmt::Expr(
                ast::BlockStmtExpr::new(assign).with_semicolon(true),
            )],
            Expr::ident(ident("x")),
        );
        let mut func = ast::ItemDefFunction::new_simple(ident("reassign_param"), block)
            .with_ret_ty(Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)));
        func.sig.params.push(ast::FunctionParam::new(
            ident("x"),
            Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)),
        ));
        let file = File {
            path: PathBuf::from("reassign_param.fp"),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![Item::from(ItemKind::DefFunction(func))],
        };

        let items = roundtrip_items_via_hir(&file).expect("roundtrip should succeed");
        let ItemKind::DefFunction(function) = items[0].kind() else {
            panic!("expected a function item, got {:?}", items[0].kind());
        };
        assert_eq!(
            function.sig.params.len(),
            1,
            "the parameter itself must not be duplicated/removed"
        );
        let param_name = function.sig.params[0].name.name.clone();
        assert_eq!(
            param_name, "x",
            "the parameter itself keeps its source name"
        );

        let stmts = &function.body.stmts;
        assert_eq!(
            stmts.len(),
            3,
            "a synthetic shadow `let` + the reassignment + a tail expression"
        );

        let shadow_name = let_binding_name(&stmts[0]);
        assert_ne!(
            shadow_name, "x",
            "the shadow local must be renamed to avoid colliding with the parameter"
        );

        let BlockStmt::Expr(assign_stmt) = &stmts[1] else {
            panic!("expected an expression statement, got {:?}", stmts[1]);
        };
        let ast::ExprKind::Assign(assign) = assign_stmt.expr.kind() else {
            panic!("expected an assignment, got {:?}", assign_stmt.expr.kind());
        };
        let ast::ExprKind::Name(Name::Ident(assign_target)) = assign.target.kind() else {
            panic!(
                "expected a bare name target, got {:?}",
                assign.target.kind()
            );
        };
        assert_eq!(
            assign_target.name, shadow_name,
            "the reassignment must target the renamed shadow, not the (unassignable) parameter"
        );

        let tail_name = tail_expr_name(&stmts[2]);
        assert_eq!(
            tail_name, shadow_name,
            "the trailing `x` must resolve to the renamed shadow local"
        );
    }

    fn let_binding_name(stmt: &BlockStmt) -> String {
        let BlockStmt::Let(stmt_let) = stmt else {
            panic!("expected a let statement, got {stmt:?}");
        };
        match stmt_let.pat.kind() {
            ast::PatternKind::Ident(ident_pat) => ident_pat.ident.name.clone(),
            other => panic!("expected a simple ident pattern, got {other:?}"),
        }
    }

    fn tail_expr_name(stmt: &BlockStmt) -> String {
        let BlockStmt::Expr(expr_stmt) = stmt else {
            panic!("expected a trailing expression statement, got {stmt:?}");
        };
        match expr_stmt.expr.kind() {
            ast::ExprKind::Name(Name::Path(path)) => {
                path.segments.last().expect("non-empty path").name.clone()
            }
            ast::ExprKind::Name(Name::Ident(ident)) => ident.name.clone(),
            other => panic!("expected a name/path expression, got {other:?}"),
        }
    }
}
