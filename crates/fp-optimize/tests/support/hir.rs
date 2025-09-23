use fp_core::hir::{self, HirBody, HirExpr, HirExprKind, HirFunction, HirFunctionSig, HirGenerics, HirItem, HirItemKind, HirProgram, HirTy, HirTyKind};
use fp_core::span::Span;

pub fn literal_expr(value: i64) -> HirExpr {
    HirExpr::new(0, HirExprKind::Literal(hir::HirLit::Integer(value)), Span::new(0, 0, 0))
}

pub fn unit_type() -> HirTy {
    HirTy::new(0, HirTyKind::Tuple(Vec::new()), Span::new(0, 0, 0))
}

pub fn function_item(name: &str, body: HirExpr) -> HirItem {
    let func_body = HirBody {
        hir_id: 1,
        params: Vec::new(),
        value: body,
    };

    let sig = HirFunctionSig {
        name: name.to_string(),
        inputs: Vec::new(),
        output: unit_type(),
        generics: HirGenerics {
            params: Vec::new(),
            where_clause: None,
        },
    };

    let function = HirFunction::new(sig, Some(func_body), false);

    HirItem {
        hir_id: 0,
        def_id: 0,
        kind: HirItemKind::Function(function),
        span: Span::new(0, 0, 0),
    }
}

pub fn program_with_items(items: Vec<HirItem>) -> HirProgram {
    let mut program = HirProgram::new();
    program.items = items;
    program.next_hir_id = program.items.len() as u32;
    program
}
