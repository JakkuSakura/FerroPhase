use fp_backend::transformations::HirToMirLowerer;
use fp_core::ast::{TypeInt, TypePrimitive};
use fp_core::hir::{
    self, Expr, ExprKind, Function, FunctionSig, Generics, HirPackage, Item, ItemKind, Lit, Pat,
    PatKind, Path, PathSegment, Res, Symbol, TypeExpr, TypeExprKind, Visibility,
};
use fp_core::mir::{
    self, ConstantKind, ItemKind as MirItemKind, Operand, Rvalue, StatementKind, TerminatorKind,
    ty::{IntTy as MirIntTy, TyKind as MirTyKind},
};
use fp_core::span::Span;

fn span() -> Span {
    Span::new(0, 0, 0)
}

fn def_id(index: u32) -> hir::DefId {
    hir::DefId::new(test_pkg(), index)
}

fn test_pkg() -> hir::PackageId {
    hir::PackageId::new("test")
}

fn hid(index: u32) -> hir::HirId {
    hir::HirId::new(hir::OwnerId::root(test_pkg()), index)
}

fn primitive_type(kind: TypePrimitive) -> TypeExpr {
    TypeExpr {
        hir_id: hid(0),
        kind: TypeExprKind::Primitive(kind),
        span: span(),
    }
}

fn path_type(name: &str) -> TypeExpr {
    TypeExpr {
        hir_id: hid(0),
        kind: TypeExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
                infer_args: true,
                res: Res::Error,
            }],
            res: Res::Error,
        })),
        span: span(),
    }
}

fn literal_expr(hir_id: u32, value: i64) -> Expr {
    Expr::new(hid(hir_id), ExprKind::Literal(Lit::Integer(value)), span())
}

fn program_with_items(items: Vec<Item>) -> HirPackage {
    let mut program = HirPackage::new(test_pkg());
    program.items = items.clone();
    for item in items {
        program.def_map.insert(item.def_id.clone(), item);
    }
    program
}

fn mir_lowering(package: HirPackage) -> HirToMirLowerer {
    let package_id = package.id.clone();
    let mut program = fp_core::hir::HirProgram::new();
    program.publish_package(package);
    HirToMirLowerer::new(
        fp_core::hir::SharedHirProgram::new(program),
        package_id,
        std::rc::Rc::new(std::cell::RefCell::new(fp_core::mir::MirPackage::default())),
    )
}

fn transform(program: HirPackage) -> (HirToMirLowerer, fp_core::Result<mir::MirCodeUnit>) {
    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let result = lowering.transform(package_id);
    (lowering, result)
}

fn binding_pat(hir_id: u32, name: &str, mutable: bool) -> Pat {
    Pat {
        hir_id: hid(hir_id),
        kind: PatKind::Binding {
            name: Symbol::new(name),
            mutable,
        },
    }
}

fn local_path(hir_id: u32, name: &str, local_id: hir::HirId) -> Expr {
    Expr::new(
        hid(hir_id),
        ExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
                infer_args: true,
                res: Res::Local(local_id.clone()),
            }],
            res: Res::Local(local_id),
        })),
        span(),
    )
}

fn slice_expr(hir_id: u32, base: Expr, start: Expr, end: Expr) -> Expr {
    Expr::new(
        hid(hir_id),
        ExprKind::Slice(hir::SliceExpr {
            hir_id: hid(hir_id + 10_000),
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
        hir_id: hid(hir_id),
        kind: hir::StmtKind::Local(hir::Local {
            hir_id: hid(hir_id),
            pat,
            ty: Some(ty),
            init: Some(init),
        }),
    }
}

#[test]
fn lowers_constant_return_function_into_mir_assign_and_return() {
    let body_expr = literal_expr(1, 5);
    let body = hir::Block {
        hir_id: hid(2),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr.clone())),
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
        hir_id: hid(3),
        def_id: def_id(10),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let mir_program = lowering
        .transform(package_id)
        .expect("HIR→MIR lowering should succeed");
    let diagnostics = lowering.take_diagnostics();
    assert!(
        diagnostics.get_diagnostics().is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics.get_diagnostics()
    );
    assert!(!diagnostics.has_errors());

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
        hir_id: hid(5),
        kind: PatKind::Binding {
            name: hir::Symbol::new("x"),
            mutable: false,
        },
    };
    let param_ty = primitive_type(TypePrimitive::Int(TypeInt::I32));
    let param = hir::Param {
        hir_id: hid(6),
        pat: param_pat.clone(),
        ty: param_ty.clone(),
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };

    let path = hir::Path {
        segments: vec![hir::PathSegment {
            name: hir::Symbol::new("x"),
            args: None,
            infer_args: true,
            res: hir::Res::Local(param_pat.hir_id.clone()),
        }],
        res: hir::Res::Local(param_pat.hir_id),
    };
    let body_expr = Expr::new(hid(7), ExprKind::Path(hir::QPath::resolved(path)), span());
    let body = hir::Block {
        hir_id: hid(8),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
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
        hir_id: hid(9),
        def_id: def_id(11),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let mir_program = lowering
        .transform(package_id)
        .expect("HIR→MIR lowering should succeed");
    let diagnostics = lowering.take_diagnostics();
    assert!(
        diagnostics.get_diagnostics().is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics.get_diagnostics()
    );
    assert!(!diagnostics.has_errors());

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
fn rejects_unresolved_value_path_in_function_body() {
    let body_expr = Expr::new(
        hid(21),
        ExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new("missing_value"),
                args: None,
                infer_args: true,
                res: Res::Error,
            }],
            res: Res::Error,
        })),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(22),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
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
        hir_id: hid(23),
        def_id: def_id(42),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let (mut lowering, result) = transform(program);
    let err = result.expect_err("HIR→MIR lowering should reject unresolved value paths");
    let message = err.to_string();
    assert!(
        message.contains("unresolved value path during MIR lowering: `missing_value`"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_binary_operations_with_unit_operands() {
    let unit_expr = Expr::new(
        hid(30),
        ExprKind::Block(hir::Block {
            hir_id: hid(31),
            stmts: Vec::new(),
            expr: None,
        }),
        span(),
    );
    let body_expr = Expr::new(
        hid(32),
        ExprKind::Binary(
            hir::BinOp::Eq,
            Box::new(unit_expr),
            Box::new(literal_expr(33, 1)),
        ),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(34),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
    };

    let sig = FunctionSig {
        name: hir::Symbol::new("main"),
        inputs: Vec::new(),
        output: primitive_type(TypePrimitive::Bool),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, Some(body), false, false);
    let item = Item {
        hir_id: hid(35),
        def_id: def_id(43),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let (mut lowering, result) = transform(program);
    let err =
        result.expect_err("HIR→MIR lowering should reject unit operands in binary operations");
    let message = err.to_string();
    assert!(
        message.contains("binary operation `Eq` received unit operand(s)"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_enum_variant_call_with_missing_payload_values() {
    let enum_def_id = def_id(100);
    let variant_def_id = def_id(101);

    let enum_item = Item {
        hir_id: hid(40),
        def_id: enum_def_id.clone(),
        visibility: Visibility::Public,
        kind: ItemKind::Enum(hir::Enum {
            attrs: Vec::new(),
            name: Symbol::new("MaybeInt"),
            variants: vec![hir::EnumVariant {
                attrs: Vec::new(),
                hir_id: hid(41),
                def_id: variant_def_id.clone(),
                name: Symbol::new("Some"),
                discriminant: None,
                payload: Some(primitive_type(TypePrimitive::Int(TypeInt::I32))),
            }],
            generics: Generics::default(),
            repr: Default::default(),
        }),
        span: span(),
    };

    let body_expr = Expr::new(
        hid(42),
        ExprKind::Call(
            Box::new(Expr::new(
                hid(43),
                ExprKind::Path(hir::QPath::resolved(Path {
                    segments: vec![PathSegment {
                        name: Symbol::new("Some"),
                        args: None,
                        infer_args: true,
                        res: Res::Def(variant_def_id.clone()),
                    }],
                    res: Res::Def(variant_def_id),
                })),
                span(),
            )),
            Vec::new(),
        ),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(44),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
    };

    let function = Function::new(
        FunctionSig {
            name: hir::Symbol::new("main"),
            inputs: Vec::new(),
            output: TypeExpr {
                hir_id: hid(45),
                kind: TypeExprKind::Path(hir::QPath::resolved(Path {
                    segments: vec![PathSegment {
                        name: Symbol::new("MaybeInt"),
                        args: None,
                        infer_args: true,
                        res: Res::Def(enum_def_id.clone()),
                    }],
                    res: Res::Def(enum_def_id),
                })),
                span: span(),
            },
            generics: Generics::default(),
            abi: hir::Abi::Rust,
        },
        Some(body),
        false,
        false,
    );
    let function_item = Item {
        hir_id: hid(46),
        def_id: def_id(102),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![enum_item, function_item]);

    let (mut lowering, result) = transform(program);
    let err =
        result.expect_err("HIR→MIR lowering should reject missing enum variant payload values");
    let message = err.to_string();
    assert!(
        message.contains("enum variant expected 1 payload values, got 0"),
        "unexpected error: {message}"
    );
}

#[test]
fn rejects_struct_like_enum_variant_with_missing_fields() {
    let payload_struct_def_id = def_id(110);
    let enum_def_id = def_id(111);
    let variant_def_id = def_id(112);

    let payload_struct_item = Item {
        hir_id: hid(50),
        def_id: payload_struct_def_id.clone(),
        visibility: Visibility::Public,
        kind: ItemKind::Struct(hir::Struct {
            name: Symbol::new("Some"),
            fields: vec![hir::StructField {
                hir_id: hid(51),
                name: Symbol::new("value"),
                ty: primitive_type(TypePrimitive::Int(TypeInt::I32)),
                vis: Visibility::Public,
            }],
            generics: Generics::default(),
            repr: Default::default(),
        }),
        span: span(),
    };

    let enum_item = Item {
        hir_id: hid(52),
        def_id: enum_def_id.clone(),
        visibility: Visibility::Public,
        kind: ItemKind::Enum(hir::Enum {
            attrs: Vec::new(),
            name: Symbol::new("MaybeInt"),
            variants: vec![hir::EnumVariant {
                attrs: Vec::new(),
                hir_id: hid(53),
                def_id: variant_def_id.clone(),
                name: Symbol::new("Some"),
                discriminant: None,
                payload: Some(TypeExpr {
                    hir_id: hid(54),
                    kind: TypeExprKind::Path(hir::QPath::resolved(Path {
                        segments: vec![PathSegment {
                            name: Symbol::new("Some"),
                            args: None,
                            infer_args: true,
                            res: Res::Def(payload_struct_def_id.clone()),
                        }],
                        res: Res::Def(payload_struct_def_id),
                    })),
                    span: span(),
                }),
            }],
            generics: Generics::default(),
            repr: Default::default(),
        }),
        span: span(),
    };

    let body_expr = Expr::new(
        hid(55),
        ExprKind::Struct(
            hir::QPath::resolved(Path {
                segments: vec![PathSegment {
                    name: Symbol::new("Some"),
                    args: None,
                    infer_args: true,
                    res: Res::Def(variant_def_id.clone()),
                }],
                res: Res::Def(variant_def_id),
            }),
            Vec::new(),
        ),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(56),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
    };

    let function = Function::new(
        FunctionSig {
            name: hir::Symbol::new("main"),
            inputs: Vec::new(),
            output: TypeExpr {
                hir_id: hid(57),
                kind: TypeExprKind::Path(hir::QPath::resolved(Path {
                    segments: vec![PathSegment {
                        name: Symbol::new("MaybeInt"),
                        args: None,
                        infer_args: true,
                        res: Res::Def(enum_def_id.clone()),
                    }],
                    res: Res::Def(enum_def_id),
                })),
                span: span(),
            },
            generics: Generics::default(),
            abi: hir::Abi::Rust,
        },
        Some(body),
        false,
        false,
    );
    let function_item = Item {
        hir_id: hid(58),
        def_id: def_id(113),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![payload_struct_item, enum_item, function_item]);

    let (mut lowering, result) = transform(program);
    let err =
        result.expect_err("HIR→MIR lowering should reject missing struct-like enum variant fields");
    let message = err.to_string();
    assert!(
        message.contains("enum variant expected 1 payload values, got 0")
            || message.contains("missing field `value` in enum variant struct literal"),
        "unexpected error: {message}"
    );
}

#[test]
fn stubs_bodyless_functions_as_unreachable() {
    let sig = FunctionSig {
        name: hir::Symbol::new("extern_like"),
        inputs: Vec::new(),
        output: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        generics: Generics::default(),
        abi: hir::Abi::Rust,
    };

    let function = Function::new(sig, None, false, false);
    let item = Item {
        hir_id: hid(60),
        def_id: def_id(120),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let mir_program = lowering
        .transform(package_id)
        .expect("HIR→MIR lowering should succeed for bodyless stubs");

    let mir_item = &mir_program.items[0];
    let mir_function = match &mir_item.kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    assert_eq!(body.basic_blocks.len(), 1);
    let block = &body.basic_blocks[0];
    assert!(
        block.statements.is_empty(),
        "unexpected stub statements: {:?}",
        block.statements
    );
    assert!(matches!(
        block.terminator.as_ref().expect("terminator").kind,
        TerminatorKind::Unreachable
    ));
}

#[test]
fn lowers_const_item_to_mir_static_with_integer_initializer() {
    let const_body = hir::Body {
        hir_id: hid(12),
        params: Vec::new(),
        value: literal_expr(13, 7),
    };
    let konst = hir::Const {
        name: hir::Symbol::new("VALUE"),
        mutable: false,
        is_host: false,
        ty: primitive_type(TypePrimitive::Int(TypeInt::I32)),
        body: const_body,
    };
    let item = Item {
        hir_id: hid(14),
        def_id: def_id(42),
        visibility: Visibility::Public,
        kind: ItemKind::Const(konst),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let mir_program = lowering
        .transform(package_id)
        .expect("HIR→MIR lowering should succeed");
    let diagnostics = lowering.take_diagnostics();
    assert!(diagnostics.get_diagnostics().is_empty());
    assert!(!diagnostics.has_errors());

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
fn lowers_function_local_const_before_its_declaration_without_a_global() {
    let local_const_id = def_id(43);
    let local_const = Item {
        hir_id: hid(43),
        def_id: local_const_id.clone(),
        visibility: Visibility::Private,
        kind: ItemKind::Const(hir::Const {
            name: Symbol::new("VALUE"),
            ty: primitive_type(TypePrimitive::Int(TypeInt::I64)),
            body: hir::Body {
                hir_id: hid(44),
                params: Vec::new(),
                value: literal_expr(45, 7),
            },
            mutable: false,
            is_host: false,
        }),
        span: span(),
    };
    let use_before_declaration = Expr::new(
        hid(46),
        ExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new("VALUE"),
                args: None,
                infer_args: true,
                res: Res::Def(local_const_id.clone()),
            }],
            res: Res::Def(local_const_id),
        })),
        span(),
    );
    let function = Function::new(
        FunctionSig {
            name: Symbol::new("read"),
            inputs: Vec::new(),
            output: primitive_type(TypePrimitive::Int(TypeInt::I64)),
            generics: Generics::default(),
            abi: hir::Abi::Rust,
        },
        Some(hir::Block {
            hir_id: hid(47),
            stmts: vec![
                hir::Stmt {
                    hir_id: hid(48),
                    kind: hir::StmtKind::Expr(use_before_declaration),
                },
                hir::Stmt {
                    hir_id: hid(49),
                    kind: hir::StmtKind::Item(local_const),
                },
            ],
            expr: Some(Box::new(literal_expr(50, 0))),
        }),
        false,
        false,
    );
    let program = program_with_items(vec![Item {
        hir_id: hid(51),
        def_id: def_id(44),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    }]);

    let (_, result) = transform(program);
    let mir_program = result.expect("HIR-to-MIR lowering should succeed");
    assert!(
        !mir_program
            .items
            .iter()
            .any(|item| matches!(item.kind, MirItemKind::ExecutableConst(_))),
        "function-local consts must be materialized in their body, not emitted as globals"
    );
}

#[test]
fn lowers_index_expression_into_place_projection() {
    let values_pat = Pat {
        hir_id: hid(20),
        kind: PatKind::Binding {
            name: Symbol::new("values"),
            mutable: false,
        },
    };
    let idx_pat = Pat {
        hir_id: hid(21),
        kind: PatKind::Binding {
            name: Symbol::new("idx"),
            mutable: false,
        },
    };

    let array_len = Expr::new(hid(22), ExprKind::Literal(Lit::Integer(3)), span());
    let values_ty = TypeExpr {
        hir_id: hid(23),
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };
    let idx_ty = path_type("usize");

    let values_param = hir::Param {
        hir_id: hid(24),
        pat: values_pat.clone(),
        ty: values_ty,
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };
    let idx_param = hir::Param {
        hir_id: hid(25),
        pat: idx_pat.clone(),
        ty: idx_ty,
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };

    let values_path = Expr::new(
        hid(26),
        ExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new("values"),
                args: None,
                infer_args: true,
                res: Res::Local(values_pat.hir_id.clone()),
            }],
            res: Res::Local(values_pat.hir_id),
        })),
        span(),
    );
    let idx_path = Expr::new(
        hid(27),
        ExprKind::Path(hir::QPath::resolved(Path {
            segments: vec![PathSegment {
                name: Symbol::new("idx"),
                args: None,
                infer_args: true,
                res: Res::Local(idx_pat.hir_id.clone()),
            }],
            res: Res::Local(idx_pat.hir_id),
        })),
        span(),
    );

    let body_expr = Expr::new(
        hid(28),
        ExprKind::Index(Box::new(values_path), Box::new(idx_path)),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(29),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
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
        hir_id: hid(30),
        def_id: def_id(40),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let program = program_with_items(vec![item]);

    let package_id = program.id.clone();
    let mut lowering = mir_lowering(program);
    let mir_program = lowering
        .transform(package_id)
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
        hir_id: hid(41),
        kind: PatKind::Binding {
            name: Symbol::new("values"),
            mutable: false,
        },
    };

    let array_len = Expr::new(hid(42), ExprKind::Literal(Lit::Integer(4)), span());
    let values_ty = TypeExpr {
        hir_id: hid(43),
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };

    let values_param = hir::Param {
        hir_id: hid(44),
        pat: values_pat.clone(),
        ty: values_ty,
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };

    let values_path = local_path(45, "values", values_pat.hir_id);
    let start = literal_expr(46, 1);
    let end = literal_expr(47, 3);
    let slice = slice_expr(48, values_path, start, end);
    let body_expr = Expr::new(
        hid(49),
        ExprKind::Index(Box::new(slice), Box::new(literal_expr(50, 0))),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(51),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
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
        hir_id: hid(52),
        def_id: def_id(53),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let (mut lowering, result) = transform(program_with_items(vec![item]));
    let mir_program = result.expect("HIR→MIR lowering should succeed");
    let diagnostics = lowering.take_diagnostics();
    assert!(
        diagnostics.get_diagnostics().is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics.get_diagnostics()
    );
    assert!(!diagnostics.has_errors());

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    let block = &body.basic_blocks[0];
    let has_subslice_index = block.statements.iter().any(|stmt| match &stmt.kind {
        StatementKind::Assign(_, Rvalue::Use(Operand::Copy(place))) => matches!(
            place.projection.as_slice(),
            [
                mir::PlaceElem::Subslice {
                    from: 1,
                    to: 3,
                    from_end: false,
                },
                mir::PlaceElem::Index(_)
            ]
        ),
        _ => false,
    });

    assert!(
        has_subslice_index,
        "expected static slice indexing to preserve the constant subslice projection"
    );
}

#[test]
fn lowers_index_on_dynamic_slice_into_explicit_slice_value_then_index_projection() {
    let values_pat = binding_pat(60, "values", false);
    let start_pat = binding_pat(61, "start", false);
    let end_pat = binding_pat(62, "end", false);

    let array_len = Expr::new(hid(63), ExprKind::Literal(Lit::Integer(4)), span());
    let values_ty = TypeExpr {
        hir_id: hid(64),
        kind: TypeExprKind::Array(
            Box::new(primitive_type(TypePrimitive::Int(TypeInt::I64))),
            Some(Box::new(array_len)),
        ),
        span: span(),
    };

    let usize_ty = path_type("usize");
    let values_param = hir::Param {
        hir_id: hid(65),
        pat: values_pat.clone(),
        ty: values_ty,
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };
    let start_param = hir::Param {
        hir_id: hid(66),
        pat: start_pat.clone(),
        ty: usize_ty.clone(),
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };
    let end_param = hir::Param {
        hir_id: hid(67),
        pat: end_pat.clone(),
        ty: usize_ty,
        is_context: false,
        as_tuple: false,
        as_dict: false,
        default: None,
    };

    let values_path = local_path(68, "values", values_pat.hir_id);
    let start_path = local_path(69, "start", start_pat.hir_id);
    let end_path = local_path(70, "end", end_pat.hir_id);
    let slice = slice_expr(71, values_path, start_path, end_path);
    let body_expr = Expr::new(
        hid(72),
        ExprKind::Index(Box::new(slice), Box::new(literal_expr(73, 0))),
        span(),
    );
    let body = hir::Block {
        hir_id: hid(74),
        stmts: Vec::new(),
        expr: Some(Box::new(body_expr)),
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
        hir_id: hid(75),
        def_id: def_id(76),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let (mut lowering, result) = transform(program_with_items(vec![item]));
    let mir_program = result.expect("HIR→MIR lowering should succeed");
    let diagnostics = lowering.take_diagnostics();
    assert!(
        diagnostics.get_diagnostics().is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics.get_diagnostics()
    );
    assert!(!diagnostics.has_errors());

    let mir_function = match &mir_program.items[0].kind {
        MirItemKind::Function(func) => func,
        other => panic!("expected MIR function item, found {other:?}"),
    };
    let body = mir_program
        .bodies
        .get(&mir_function.body_id)
        .expect("function body present");
    let block = &body.basic_blocks[0];

    let slice_value_locals: Vec<_> = block
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            StatementKind::Assign(place, Rvalue::IntrinsicCall { kind, .. })
                if matches!(kind, fp_core::intrinsics::IntrinsicKind::Slice) =>
            {
                Some(place.local)
            }
            _ => None,
        })
        .collect();
    assert!(
        !slice_value_locals.is_empty(),
        "expected MIR slice value intrinsic"
    );

    let slice_place_locals: Vec<_> = block
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            StatementKind::Assign(place, Rvalue::Use(Operand::Copy(source)))
                if slice_value_locals.contains(&source.local) && source.projection.is_empty() =>
            {
                Some(place.local)
            }
            _ => None,
        })
        .collect();
    assert!(
        !slice_place_locals.is_empty(),
        "expected materialized slice place"
    );
    let has_index_from_slice_value = block.statements.iter().any(|stmt| match &stmt.kind {
        StatementKind::Assign(_, Rvalue::Use(Operand::Copy(place))) => {
            slice_place_locals.contains(&place.local)
                && matches!(place.projection.last(), Some(mir::PlaceElem::Index(_)))
        }
        _ => false,
    });

    assert!(
        has_index_from_slice_value,
        "expected dynamic slice indexing through its materialized slice value"
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
        hid(103),
        ExprKind::Return(Some(Box::new(local_path(104, "x", x_pat.hir_id.clone())))),
        span(),
    );
    let finally_expr = Expr::new(
        hid(105),
        ExprKind::Assign(
            Box::new(local_path(106, "x", x_pat.hir_id.clone())),
            Box::new(literal_expr(107, 2)),
        ),
        span(),
    );
    let try_expr = Expr::new(
        hid(108),
        ExprKind::Try(hir::TryExpr {
            expr: Box::new(return_expr),
            catches: Vec::new(),
            elze: None,
            finally: Some(Box::new(finally_expr)),
        }),
        span(),
    );

    let body = hir::Block {
        hir_id: hid(109),
        stmts: Vec::new(),
        expr: Some(Box::new(Expr::new(
            hid(110),
            ExprKind::Block(hir::Block {
                hir_id: hid(111),
                stmts: vec![x_stmt],
                expr: Some(Box::new(try_expr)),
            }),
            span(),
        ))),
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
        hir_id: hid(112),
        def_id: def_id(113),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let (mut lowering, result) = transform(program_with_items(vec![item]));
    let mir_program = result.expect("HIR→MIR lowering should succeed");

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
        hid(123),
        ExprKind::Break(Some(Box::new(local_path(124, "x", x_pat.hir_id.clone())))),
        span(),
    );
    let finally_expr = Expr::new(
        hid(125),
        ExprKind::Assign(
            Box::new(local_path(126, "x", x_pat.hir_id.clone())),
            Box::new(literal_expr(127, 2)),
        ),
        span(),
    );
    let try_expr = Expr::new(
        hid(128),
        ExprKind::Try(hir::TryExpr {
            expr: Box::new(break_expr),
            catches: Vec::new(),
            elze: None,
            finally: Some(Box::new(finally_expr)),
        }),
        span(),
    );
    let loop_expr = Expr::new(
        hid(129),
        ExprKind::Loop(hir::Block {
            hir_id: hid(130),
            stmts: vec![x_stmt],
            expr: Some(Box::new(try_expr)),
        }),
        span(),
    );

    let body = hir::Block {
        hir_id: hid(131),
        stmts: Vec::new(),
        expr: Some(Box::new(loop_expr)),
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
        hir_id: hid(132),
        def_id: def_id(133),
        visibility: Visibility::Public,
        kind: ItemKind::Function(function),
        span: span(),
    };

    let (mut lowering, result) = transform(program_with_items(vec![item]));
    let mir_program = result.expect("HIR→MIR lowering should succeed");

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
