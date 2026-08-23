#![allow(dead_code)]
use fp_core::hir::{
    self, Block, Expr, ExprKind, Function, FunctionSig, Generics, HirPackage, Item, ItemKind,
    TypeExpr, TypeExprKind,
};
use fp_core::span::Span;

fn test_pkg() -> hir::PackageId { hir::PackageId::new("test") }

pub fn literal_expr(value: i64) -> Expr {
    Expr::new(
        hir::HirId::new(test_pkg(), 0),
        ExprKind::Literal(hir::Lit::Integer(value)),
        Span::new(0, 0, 0),
    )
}

pub fn unit_type() -> TypeExpr {
    TypeExpr {
        hir_id: hir::HirId::new(test_pkg(), 0),
        kind: TypeExprKind::Tuple(Vec::new()),
        span: Span::new(0, 0, 0),
    }
}

pub fn function_item(name: &str, body: Expr) -> Item {
    let func_body = Block {
        hir_id: hir::HirId::new(test_pkg(), 1),
        stmts: Vec::new(),
        expr: Some(Box::new(body)),
    };

    let sig = FunctionSig {
        name: hir::Symbol::new(name),
        inputs: Vec::new(),
        output: unit_type(),
        generics: Generics {
            params: Vec::new(),
            where_clause: None,
        },
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(func_body), false, false);

    Item {
        hir_id: hir::HirId::new(test_pkg(), 0),
        def_id: hir::DefId::new(test_pkg(), 0),
        visibility: hir::Visibility::Public,
        kind: ItemKind::Function(function),
        span: Span::new(0, 0, 0),
    }
}

pub fn program_with_items(items: Vec<Item>) -> HirPackage {
    let mut program = HirPackage::new(test_pkg());
    program.items = items;
    program.next_hir_id = program.items.len() as u32;
    program
}
