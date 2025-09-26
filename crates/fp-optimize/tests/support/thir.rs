use fp_core::hir::ty::IntTy;
use fp_core::hir::typed::{
    self as thir, Body, BodyId, Expr, ExprKind, Function, FunctionSig, Item, ItemKind, Lit, Ty,
};
use fp_core::span::Span;

fn int_ty() -> Ty {
    Ty::int(IntTy::I32)
}

pub fn literal_expr(value: i64) -> Expr {
    Expr::new(
        0,
        ExprKind::Literal(Lit::Int(value.into(), thir::IntTy::I32)),
        int_ty(),
        Span::new(0, 0, 0),
    )
}

pub fn body_with_expr(expr: Expr) -> (BodyId, Body) {
    let body_id = BodyId::new(0);
    let body = Body {
        params: Vec::new(),
        value: expr,
        locals: Vec::new(),
    };
    (body_id, body)
}

pub fn function_item(name: &str, body_id: Option<BodyId>) -> Item {
    let sig = FunctionSig {
        inputs: Vec::new(),
        output: int_ty(),
        c_variadic: false,
    };

    let function = Function {
        sig,
        body_id,
        is_const: false,
    };

    Item {
        thir_id: 0,
        kind: ItemKind::Function(function),
        ty: int_ty(),
        span: Span::new(0, 0, 0),
    }
}
