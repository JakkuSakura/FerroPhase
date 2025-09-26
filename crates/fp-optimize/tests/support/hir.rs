use fp_core::hir::{
    self, Body, Expr, ExprKind, Function, FunctionSig, Generics, Item, ItemKind, Program, Ty,
    TyKind,
};
use fp_core::span::Span;

pub fn literal_expr(value: i64) -> Expr {
    Expr::new(
        0,
        ExprKind::Literal(hir::Lit::Integer(value)),
        Span::new(0, 0, 0),
    )
}

pub fn unit_type() -> Ty {
    Ty { kind: TyKind::Tuple(Vec::new()) }
}

pub fn function_item(name: &str, body: Expr) -> Item {
    let func_body = Body {
        hir_id: 1,
        params: Vec::new(),
        value: body,
    };

    let sig = FunctionSig {
        name: name.to_string(),
        inputs: Vec::new(),
        output: unit_type(),
        generics: Generics {
            params: Vec::new(),
            where_clause: None,
        },
    };

    let function = Function::new(sig, Some(func_body), false);

    Item {
        hir_id: 0,
        def_id: 0,
        visibility: hir::Visibility::Public,
        kind: ItemKind::Function(function),
        span: Span::new(0, 0, 0),
    }
}

pub fn program_with_items(items: Vec<Item>) -> Program {
    let mut program = Program::new();
    program.items = items;
    program.next_hir_id = program.items.len() as u32;
    program
}
