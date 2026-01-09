use fp_core::ast::{TypeInt, TypePrimitive};
use fp_core::hir::{
    self, Expr, ExprKind, Function, FunctionSig, Generics, Item, ItemKind, Lit, Pat, PatKind, Path,
    PathSegment, Program, Res, Symbol, TypeExpr, TypeExprKind, Visibility,
};
use fp_core::mir::{
    self,
    ty::{IntTy as MirIntTy, TyKind as MirTyKind},
    ConstantKind, ItemKind as MirItemKind, Operand, Rvalue, StatementKind, TerminatorKind,
};
use fp_core::span::Span;
use fp_optimize::transformations::MirLowering;

fn span() -> Span {
    Span::new(0, 0, 0)
}

fn primitive_type(kind: TypePrimitive) -> TypeExpr {
    TypeExpr {
        hir_id: 0,
        kind: TypeExprKind::Primitive(kind),
        span: span(),
    }
}

fn path_type(name: &str) -> TypeExpr {
    TypeExpr {
        hir_id: 0,
        kind: TypeExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
            }],
            res: None,
        }),
        span: span(),
    }
}

fn literal_expr(hir_id: u32, value: i64) -> Expr {
    Expr::new(hir_id, ExprKind::Literal(Lit::Integer(value)), span())
}

fn program_with_items(items: Vec<Item>) -> Program {
    let mut program = Program::new();
    program.items = items.clone();
    for item in items {
        program.def_map.insert(item.def_id, item);
    }
    program
}

fn mir_lowering() -> MirLowering {
    MirLowering::new()
}

#[test]
fn lowers_constant_return_function_into_mir_assign_and_return() {
    let body_expr = literal_expr(1, 5);
    let body = hir::Body {
        hir_id: 2,
        params: Vec::new(),
        value: body_expr.clone(),
    };

    let sig = FunctionSig {
        name: hir::Symbol::new("main"),
        inputs: Vec::new(),
        output: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        generics: Generics::default(),
    };

    let function = Function::new(sig, Some(body), false);
    let item = Item {
        hir_id: 3,
        def_id: 10,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program)
        .expect("HIR→MIR lowering should succeed");
    let (diagnostics, has_errors) = lowering.take_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(!has_errors);

    assert_eq!(mir_program.items.len(), 1);
    let mir_item = &mir_program.items[0];
    let mir_function = match &mir_item.kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };

    assert_eq!(mir_function.name.as_str(), "main");
    assert!(mir_function.sig.inputs.is_empty());
    assert!(matches!(
        mir_function.sig.output.kind,
        mir::ty::TyKind::Int(_)
    ));

    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    assert_eq!(body.basic_blocks.len(), 1);
    let block = &body.basic_blocks[0];
    assert_eq!(block.statements.len(), 1);
    match &block.statements[0].kind {
        StatementKind::Assign(place, Rvalue::Use(Operand::Constant(constant))) => {
            assert_eq!(place.local, 0);
            assert!(matches!(constant.literal, ConstantKind::Int(5)));
        }
        other => panic!("unexpected statement: {other:?}"),
    }
    match block.terminator.as_ref().expect("terminator").kind {
        TerminatorKind::Return => {}
        ref other => panic!("expected return terminator, found {other:?}"),
    }
}

#[test]
fn lowers_identity_function_with_parameter() {
    // Parameter binding `x: i32`
    let param_pat = Pat {
        hir_id: 5,
        kind: PatKind::Binding {
            name: hir::Symbol::new("x"),
            mutable: false,
        },
    };
    let param_ty = primitive_type(TypePrimitive::Int(TypeInt::I32));
    let param = hir::Param {
        hir_id: 6,
        pat: param_pat.clone(),
        ty: param_ty.clone(),
    };

    let path = hir::Path {
        segments: vec![hir::PathSegment {
            name: hir::Symbol::new("x"),
            args: None,
        }],
        res: Some(hir::Res::Local(param_pat.hir_id)),
    };
    let body_expr = Expr::new(7, ExprKind::Path(path), span());
    let body = hir::Body {
        hir_id: 8,
        params: vec![param.clone()],
        value: body_expr,
    };

    let sig = FunctionSig {
        name: hir::Symbol::new("identity"),
        inputs: vec![param],
        output: param_ty.clone(),
        generics: Generics::default(),
    };

    let function = Function::new(sig, Some(body), false);
    let item = Item {
        hir_id: 9,
        def_id: 11,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program)
        .expect("HIR→MIR lowering should succeed");
    let (diagnostics, has_errors) = lowering.take_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(!has_errors);

    let mir_item = &mir_program.items[0];
    let mir_function = match &mir_item.kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    assert_eq!(mir_function.sig.inputs.len(), 1);
    assert_eq!(mir_function.sig.output.kind, MirTyKind::Int(MirIntTy::I32));

    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    assert_eq!(body.arg_count, 1);
    assert!(body.locals.len() >= 2, "expected return + argument locals");
    assert!(matches!(
        body.basic_blocks[0].terminator.as_ref().unwrap().kind,
        TerminatorKind::Return
    ));
}

#[test]
fn lowers_const_item_to_mir_static_with_integer_initializer() {
    let const_body = hir::Body {
        hir_id: 12,
        params: Vec::new(),
        value: literal_expr(13, 7),
    };
    let konst = hir::Const {
        name: hir::Symbol::new("VALUE"),
        ty: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        body: const_body,
    };
    let item = Item {
        hir_id: 14,
        def_id: 42,
        visibility: Visibility::Public,
        kind: ItemKind::Const(konst),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program)
        .expect("HIR→MIR lowering should succeed");
    let (diagnostics, has_errors) = lowering.take_diagnostics();
    assert!(diagnostics.is_empty());
    assert!(!has_errors);

    assert_eq!(mir_program.items.len(), 1);
    let mir_item = &mir_program.items[0];
    match &mir_item.kind {
        MirItemKind::Static(mir_static) => {
            assert!(matches!(mir_static.ty.kind, MirTyKind::Int(MirIntTy::I32)));
            match &mir_static.init {
                Operand::Constant(constant) => match constant.literal {
                    ConstantKind::Int(value) => assert_eq!(value, 7),
                    ref other => panic!("expected integer literal, got {other:?}"),
                },
                other => panic!("expected constant operand, got {other:?}"),
            }
        }
        other => panic!("expected MIR static item, found {other:?}"),
    }
}

#[test]
fn lowers_index_expression_into_place_projection() {
    let values_pat = Pat {
        hir_id: 20,
        kind: PatKind::Binding {
            name: Symbol::new("values"),
            mutable: false,
        },
    };
    let idx_pat = Pat {
        hir_id: 21,
        kind: PatKind::Binding {
            name: Symbol::new("idx"),
            mutable: false,
        },
    };

    let array_len = Expr::new(22, ExprKind::Literal(Lit::Integer(3)), span());
    let values_ty = TypeExpr {
        hir_id: 23,
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };
    let idx_ty = path_type("usize");

    let values_param = hir::Param {
        hir_id: 24,
        pat: values_pat.clone(),
        ty: values_ty,
    };
    let idx_param = hir::Param {
        hir_id: 25,
        pat: idx_pat.clone(),
        ty: idx_ty,
    };

    let values_path = Expr::new(
        26,
        ExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new("values"),
                args: None,
            }],
            res: Some(Res::Local(values_pat.hir_id)),
        }),
        span(),
    );
    let idx_path = Expr::new(
        27,
        ExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new("idx"),
                args: None,
            }],
            res: Some(Res::Local(idx_pat.hir_id)),
        }),
        span(),
    );

    let body_expr = Expr::new(
        28,
        ExprKind::Index(Box::new(values_path), Box::new(idx_path)),
        span(),
    );
    let body = hir::Body {
        hir_id: 29,
        params: vec![values_param.clone(), idx_param.clone()],
        value: body_expr,
    };

    let sig = FunctionSig {
        name: Symbol::new("pick"),
        inputs: vec![values_param, idx_param],
        output: primitive_type(TypePrimitive::Int(TypeInt::I64)),
        generics: Generics::default(),
    };

    let function = Function::new(sig, Some(body), false);
    let item = Item {
        hir_id: 30,
        def_id: 40,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program)
        .expect("HIR→MIR lowering should succeed");

    let mir_item = &mir_program.items[0];
    let mir_function = match &mir_item.kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };

    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    let block = &body.basic_blocks[0];
    let has_index_projection = block.statements.iter().any(|stmt| match &stmt.kind {
        StatementKind::Assign(_, Rvalue::Use(Operand::Copy(place))) => {
            matches!(place.projection.last(), Some(mir::PlaceElem::Index(_)))
        }
        _ => false,
    });

    assert!(has_index_projection, "expected index projection in MIR");
}
