// fp-backend: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - transforms: Focused AST/IR rewrites and lowering helpers
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod abi;
pub mod error;
pub mod optimizer;
pub mod transforms;

pub use transforms as transformations;

pub fn roundtrip_ast_file_via_hir(
    file: &fp_core::ast::File,
) -> fp_core::Result<fp_core::ast::Node> {
    let mut generator = transforms::ast_to_hir::HirGenerator::with_file(&file.path);
    generator.set_cfg_filtering(false);
    let mut program = generator.transform_file(file)?;
    optimizer::hir::eliminate_dead_code(&mut program);
    transforms::hir_to_ast::lift_program(&program, file.path.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{
        self, Expr, ExprInvoke, ExprInvokeTarget, File, Ident, Item, ItemKind, Name, NodeKind, Ty,
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
                Expr::block(ast::ExprBlock::new_expr(Expr::value(ast::Value::unit()))).into(),
            )
            .with_ret_ty(Ty::unit()),
        ))
    }

    #[test]
    fn roundtrip_via_hir_prunes_unused_functions_for_top_level_expr_roots() {
        let file = File {
            path: PathBuf::from("dce_example.fp"),
            attrs: Vec::new(),
            items: vec![
                function_item("used_helper"),
                function_item("unused_helper"),
                Item::from(ItemKind::Expr(invoke("used_helper"))),
            ],
        };

        let lowered = roundtrip_ast_file_via_hir(&file).expect("roundtrip should succeed");
        let NodeKind::File(file) = lowered.kind() else {
            panic!("expected file node");
        };

        assert!(file.items.iter().any(|item| matches!(
            item.kind(),
            ItemKind::DefFunction(function) if function.name.as_str() == "used_helper"
        )));
        assert!(!file.items.iter().any(|item| matches!(
            item.kind(),
            ItemKind::DefFunction(function) if function.name.as_str() == "unused_helper"
        )));
        assert!(file
            .items
            .iter()
            .any(|item| matches!(item.kind(), ItemKind::Expr(_))));
    }
}
