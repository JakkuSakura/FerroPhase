use fp_core::span::Span;
use fp_core::thir::{
    self, Body, BodyId, Expr, ExprKind, Function, FunctionSig, Item, ItemKind, Lit,
};
use fp_core::types::{IntTy, Ty, TyKind, TypeFlags};

fn int_ty() -> Ty {
    Ty {
        kind: TyKind::Int(IntTy::I32),
        flags: TypeFlags::empty(),
    }
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
