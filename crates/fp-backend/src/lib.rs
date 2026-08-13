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
/// just hands back an already-built `PackageSource` — no filesystem
/// access), then `WorkspaceContext::begin_package`. Deliberately does not
/// use `fp-lang`'s disk-resolving `single_file_provider`: callers here
/// (`roundtrip_items_via_hir`/`_dce`) receive already-fully-assembled
/// `File`s (e.g. `fp-shell` splices in its embedded std tree directly,
/// leaving behind `mod foo;` markers whose content already lives
/// elsewhere in the tree) that must not be re-resolved against a real
/// filesystem — `HirGenerator::append_item` already walks nested `Module`
/// items structurally on its own, so no flattening is needed here either.
fn package_from_file(
    file: &fp_core::ast::File,
) -> fp_core::Result<fp_core::package::CompiledPackage> {
    use fp_core::package::provider::{FixedPackageProvider, PackageProvider};

    let package_id = fp_core::package::PackageId::new("roundtrip");
    let mut items = file.items.clone();
    transforms::ast_to_hir::strip_doc_attrs_in_items(&mut items);
    let mut source = fp_core::package::PackageSource::new(
        package_id.clone(),
        "roundtrip",
        fp_core::package::graph::PackageGraph::new(Vec::new()),
    );
    source.items = items
        .into_iter()
        .map(|item| fp_core::package::PackageItem {
            path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            item,
        })
        .collect();
    let provider = FixedPackageProvider::for_source(package_id.clone(), source);
    let source = provider
        .load_package_source(&package_id)
        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    let workspace = fp_core::workspace::WorkspaceContext::new();
    let data_layout = fp_core::lir::LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    let package = workspace.begin_package(package_id, source, data_layout);
    let package = package.borrow().clone();
    Ok(package)
}

pub fn roundtrip_items_via_hir(
    file: &fp_core::ast::File,
) -> fp_core::Result<Vec<fp_core::ast::Item>> {
    let package = package_from_file(file)?;
    let mut generator = transforms::ast_to_hir::HirGenerator::new();
    generator.set_cfg_filtering(false);
    let program = generator.transform_package(&package)?;
    transforms::hir_to_ast::HirToAstLifter::new(&program, None).lift_items()
}

pub fn roundtrip_items_via_hir_dce(
    file: &fp_core::ast::File,
) -> fp_core::Result<Vec<fp_core::ast::Item>> {
    let package = package_from_file(file)?;
    let mut generator = transforms::ast_to_hir::HirGenerator::new();
    generator.set_cfg_filtering(false);
    let mut program = generator.transform_package(&package)?;
    optimizer::hir::eliminate_dead_code(&mut program, None);
    transforms::hir_to_ast::HirToAstLifter::new(&program, None).lift_items()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{
        self, Expr, ExprInvoke, ExprInvokeTarget, File, Ident, Item, ItemKind, Name, Ty,
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
}
