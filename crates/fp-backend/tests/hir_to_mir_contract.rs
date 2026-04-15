use fp_backend::transformations::MirLowering;
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

fn binding_pat(hir_id: u32, name: &str, mutable: bool) -> Pat {
    Pat {
        hir_id,
        kind: PatKind::Binding {
            name: Symbol::new(name),
            mutable,
        },
    }
}

fn local_path(hir_id: u32, name: &str, local_id: u32) -> Expr {
    Expr::new(
        hir_id,
        ExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
            }],
            res: Some(Res::Local(local_id)),
        }),
        span(),
    )
}

fn slice_expr(hir_id: u32, base: Expr, start: Expr, end: Expr) -> Expr {
    Expr::new(
        hir_id,
        ExprKind::Slice(hir::SliceExpr {
            hir_id: hir_id + 10_000,
            base: Box::new(base),
            start: Some(Box::new(start)),
            end: Some(Box::new(end)),
            inclusive: false,
        }),
        span(),
    )
}

fn local_stmt(hir_id: u32, pat: Pat, ty: TypeExpr, init: Expr) -> hir::Stmt {
    hir::Stmt {
        hir_id,
        kind: hir::StmtKind::Local(hir::Local {
            hir_id,
            pat,
            ty: Some(ty),
            init: Some(init),
        }),
    }
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
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
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
        is_context: false,
        default: None,
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
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
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
        is_context: false,
        default: None,
    };
    let idx_param = hir::Param {
        hir_id: 25,
        pat: idx_pat.clone(),
        ty: idx_ty,
        is_context: false,
        default: None,
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
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
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

#[test]
fn lowers_index_on_static_slice_into_subslice_then_index_projection() {
    let values_pat = Pat {
        hir_id: 41,
        kind: PatKind::Binding {
            name: Symbol::new("values"),
            mutable: false,
        },
    };

    let array_len = Expr::new(42, ExprKind::Literal(Lit::Integer(4)), span());
    let values_ty = TypeExpr {
        hir_id: 43,
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };

    let values_param = hir::Param {
        hir_id: 44,
        pat: values_pat.clone(),
        ty: values_ty,
        is_context: false,
        default: None,
    };

    let values_path = local_path(45, "values", values_pat.hir_id);
    let start = literal_expr(46, 1);
    let end = literal_expr(47, 3);
    let slice = slice_expr(48, values_path, start, end);
    let body_expr = Expr::new(
        49,
        ExprKind::Index(Box::new(slice), Box::new(literal_expr(50, 0))),
        span(),
    );
    let body = hir::Body {
        hir_id: 51,
        params: vec![values_param.clone()],
        value: body_expr,
    };

    let sig = FunctionSig {
        name: Symbol::new("slice_pick"),
        inputs: vec![values_param],
        output: primitive_type(TypePrimitive::Int(TypeInt::I64)),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
    let item = Item {
        hir_id: 52,
        def_id: 53,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program_with_items(vec![item]))
        .expect("HIR→MIR lowering should succeed");
    let (diagnostics, has_errors) = lowering.take_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(!has_errors);

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    let block = &body.basic_blocks[0];
    let has_subslice_container_get = block.statements.iter().any(|stmt| match &stmt.kind {
        StatementKind::Assign(
            _,
            Rvalue::ContainerGet {
                container: Operand::Copy(place),
                ..
            },
        ) => matches!(
            place.projection.last(),
            Some(mir::PlaceElem::Subslice {
                from: 1,
                to: 3,
                from_end: false,
            })
        ),
        _ => false,
    });

    assert!(
        has_subslice_container_get,
        "expected static slice indexing to preserve MIR subslice projection"
    );
}

#[test]
fn lowers_index_on_dynamic_slice_into_explicit_slice_value_then_index_projection() {
    let values_pat = binding_pat(60, "values", false);
    let start_pat = binding_pat(61, "start", false);
    let end_pat = binding_pat(62, "end", false);

    let array_len = Expr::new(63, ExprKind::Literal(Lit::Integer(4)), span());
    let values_ty = TypeExpr {
        hir_id: 64,
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };

    let usize_ty = path_type("usize");
    let values_param = hir::Param {
        hir_id: 65,
        pat: values_pat.clone(),
        ty: values_ty,
        is_context: false,
        default: None,
    };
    let start_param = hir::Param {
        hir_id: 66,
        pat: start_pat.clone(),
        ty: usize_ty.clone(),
        is_context: false,
        default: None,
    };
    let end_param = hir::Param {
        hir_id: 67,
        pat: end_pat.clone(),
        ty: usize_ty,
        is_context: false,
        default: None,
    };

    let values_path = local_path(68, "values", values_pat.hir_id);
    let start_path = local_path(69, "start", start_pat.hir_id);
    let end_path = local_path(70, "end", end_pat.hir_id);
    let slice = slice_expr(71, values_path, start_path, end_path);
    let body_expr = Expr::new(
        72,
        ExprKind::Index(Box::new(slice), Box::new(literal_expr(73, 0))),
        span(),
    );
    let body = hir::Body {
        hir_id: 74,
        params: vec![values_param.clone(), start_param.clone(), end_param.clone()],
        value: body_expr,
    };

    let sig = FunctionSig {
        name: Symbol::new("slice_pick_dynamic"),
        inputs: vec![values_param, start_param, end_param],
        output: primitive_type(TypePrimitive::Int(TypeInt::I64)),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
    let item = Item {
        hir_id: 75,
        def_id: 76,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program_with_items(vec![item]))
        .expect("HIR→MIR lowering should succeed");
    let (diagnostics, has_errors) = lowering.take_diagnostics();
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(!has_errors);

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    let block = &body.basic_blocks[0];

    let slice_value_local = block.statements.iter().find_map(|stmt| match &stmt.kind {
        StatementKind::Assign(place, Rvalue::IntrinsicCall { kind, .. })
            if matches!(kind, fp_core::intrinsics::IntrinsicCallKind::Slice) =>
        {
            Some(place.local)
        }
        _ => None,
    });
    let slice_value_local = slice_value_local.expect("expected MIR slice value intrinsic");

    let has_index_from_slice_value = block.statements.iter().any(|stmt| match &stmt.kind {
        StatementKind::Assign(
            _,
            Rvalue::ContainerGet {
                container: Operand::Copy(place),
                ..
            },
        ) => {
            place.local == slice_value_local
                && !place
                    .projection
                    .iter()
                    .any(|elem| matches!(elem, mir::PlaceElem::Subslice { .. }))
        }
        _ => false,
    });

    assert!(
        has_index_from_slice_value,
        "expected dynamic slice to lower via explicit slice value, not a subslice place"
    );
}

#[test]
fn return_value_is_materialized_before_finally_runs() {
    let x_pat = binding_pat(100, "x", true);
    let x_init = literal_expr(101, 1);
    let x_stmt = local_stmt(
        102,
        x_pat.clone(),
        primitive_type(TypePrimitive::Int(TypeInt::I32)),
        x_init,
    );

    let return_expr = Expr::new(
        103,
        ExprKind::Return(Some(Box::new(local_path(104, "x", x_pat.hir_id)))),
        span(),
    );
    let finally_expr = Expr::new(
        105,
        ExprKind::Assign(
            Box::new(local_path(106, "x", x_pat.hir_id)),
            Box::new(literal_expr(107, 2)),
        ),
        span(),
    );
    let try_expr = Expr::new(
        108,
        ExprKind::Try(hir::TryExpr {
            expr: Box::new(return_expr),
            catches: Vec::new(),
            elze: None,
            finally: Some(Box::new(finally_expr)),
        }),
        span(),
    );

    let body = hir::Body {
        hir_id: 109,
        params: Vec::new(),
        value: Expr::new(
            110,
            ExprKind::Block(hir::Block {
                hir_id: 111,
                stmts: vec![x_stmt],
                expr: Some(Box::new(try_expr)),
            }),
            span(),
        ),
    };

    let sig = FunctionSig {
        name: Symbol::new("main"),
        inputs: Vec::new(),
        output: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };
    let function = Function::new(sig, Some(body), false, false);
    let item = Item {
        hir_id: 112,
        def_id: 113,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program_with_items(vec![item]))
        .expect("HIR→MIR lowering should succeed");

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");

    let mut saw_copy_from_x = false;
    let mut saw_finally_assign = false;
    let mut saw_return_copy_after_finally = false;

    for block in &body.basic_blocks {
        for stmt in &block.statements {
            match &stmt.kind {
                StatementKind::Assign(place, Rvalue::Use(Operand::Copy(src)))
                    if src.local == 1 && place.local != 0 =>
                {
                    saw_copy_from_x = true;
                }
                StatementKind::Assign(place, Rvalue::Use(Operand::Constant(constant)))
                    if place.local == 1 && matches!(constant.literal, ConstantKind::Int(2)) =>
                {
                    assert!(
                        saw_copy_from_x,
                        "finally ran before return value was captured"
                    );
                    saw_finally_assign = true;
                }
                StatementKind::Assign(place, Rvalue::Use(Operand::Copy(src)))
                    if place.local == 0 && src.local != 1 =>
                {
                    assert!(
                        saw_finally_assign,
                        "return local should be written after finally completes"
                    );
                    saw_return_copy_after_finally = true;
                }
                _ => {}
            }
        }
    }

    assert!(saw_copy_from_x, "expected temp copy of return value");
    assert!(saw_finally_assign, "expected finally assignment in MIR");
    assert!(
        saw_return_copy_after_finally,
        "expected return local assignment after finally"
    );
}

#[test]
fn break_value_is_materialized_before_finally_runs() {
    let x_pat = binding_pat(120, "x", true);
    let x_stmt = local_stmt(
        121,
        x_pat.clone(),
        primitive_type(TypePrimitive::Int(TypeInt::I32)),
        literal_expr(122, 1),
    );

    let break_expr = Expr::new(
        123,
        ExprKind::Break(Some(Box::new(local_path(124, "x", x_pat.hir_id)))),
        span(),
    );
    let finally_expr = Expr::new(
        125,
        ExprKind::Assign(
            Box::new(local_path(126, "x", x_pat.hir_id)),
            Box::new(literal_expr(127, 2)),
        ),
        span(),
    );
    let try_expr = Expr::new(
        128,
        ExprKind::Try(hir::TryExpr {
            expr: Box::new(break_expr),
            catches: Vec::new(),
            elze: None,
            finally: Some(Box::new(finally_expr)),
        }),
        span(),
    );
    let loop_expr = Expr::new(
        129,
        ExprKind::Loop(hir::Block {
            hir_id: 130,
            stmts: vec![x_stmt],
            expr: Some(Box::new(try_expr)),
        }),
        span(),
    );

    let body = hir::Body {
        hir_id: 131,
        params: Vec::new(),
        value: loop_expr,
    };
    let sig = FunctionSig {
        name: Symbol::new("main"),
        inputs: Vec::new(),
        output: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };
    let function = Function::new(sig, Some(body), false, false);
    let item = Item {
        hir_id: 132,
        def_id: 133,
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let mut lowering = mir_lowering();
    let mir_program = lowering
        .transform(program_with_items(vec![item]))
        .expect("HIR→MIR lowering should succeed");

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");

    let mut saw_break_copy = false;
    let mut saw_finally_assign = false;
    let mut saw_return_copy = false;

    for block in &body.basic_blocks {
        for stmt in &block.statements {
            match &stmt.kind {
                StatementKind::Assign(place, Rvalue::Use(Operand::Copy(src)))
                    if src.local == 1 && place.local != 0 =>
                {
                    saw_break_copy = true;
                }
                StatementKind::Assign(place, Rvalue::Use(Operand::Constant(constant)))
                    if place.local == 1 && matches!(constant.literal, ConstantKind::Int(2)) =>
                {
                    assert!(
                        saw_break_copy,
                        "finally ran before break value was captured"
                    );
                    saw_finally_assign = true;
                }
                StatementKind::Assign(place, Rvalue::Use(Operand::Copy(src)))
                    if place.local == 0 && src.local != 1 =>
                {
                    assert!(
                        saw_finally_assign,
                        "loop result should be assigned after finally"
                    );
                    saw_return_copy = true;
                }
                _ => {}
            }
        }
    }

    assert!(saw_break_copy, "expected temp copy of break value");
    assert!(
        saw_finally_assign,
        "expected finally assignment before loop exit"
    );
    assert!(
        saw_return_copy,
        "expected loop result propagated to return local"
    );
}
