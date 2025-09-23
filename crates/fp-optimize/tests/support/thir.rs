use fp_core::span::Span;
use fp_core::thir::{
    self, BodyId, ThirBody, ThirExpr, ThirExprKind, ThirFunction, ThirFunctionSig, ThirItem,
    ThirItemKind, ThirLit,
};
use fp_core::types::{IntTy, Ty, TyKind, TypeFlags};

fn int_ty() -> Ty {
    Ty {
        kind: TyKind::Int(IntTy::I32),
        flags: TypeFlags::empty(),
    }
}

pub fn literal_expr(value: i64) -> ThirExpr {
    ThirExpr::new(
        0,
        ThirExprKind::Literal(ThirLit::Int(value, thir::IntTy::I32)),
        int_ty(),
        Span::new(0, 0, 0),
    )
}

pub fn body_with_expr(expr: ThirExpr) -> (BodyId, ThirBody) {
    let body_id = BodyId::new(0);
    let body = ThirBody {
        params: Vec::new(),
        value: expr,
        locals: Vec::new(),
    };
    (body_id, body)
}

pub fn function_item(name: &str, body_id: Option<BodyId>) -> ThirItem {
    let sig = ThirFunctionSig {
        inputs: Vec::new(),
        output: int_ty(),
        c_variadic: false,
    };

    let function = ThirFunction {
        sig,
        body_id,
        is_const: false,
    };

    ThirItem {
        thir_id: 0,
        kind: ThirItemKind::Function(function),
        ty: int_ty(),
        span: Span::new(0, 0, 0),
    }
}
