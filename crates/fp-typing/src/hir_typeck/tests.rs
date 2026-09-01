use super::*;

fn test_pkg() -> hir::PackageId {
    hir::PackageId::new("test")
}

fn hid(index: u32) -> hir::HirId {
    hir::HirId::new(hir::OwnerId::root(test_pkg()), index)
}

/// Test-only stand-in for the old `HirTypeChecker::new(program).check()`
/// single-future entry point — spawns one task per top-level item (see
/// `HirTypeChecker::spawn_item_task`) and awaits them all directly, the
/// same way `fp_compiler::driver::type_check_program` does (no
/// driver-specific setup, no comptime requests expected in these
/// tests). `async` all the way through, rather than hand-rolling a
/// poll/tick loop: the caller drives it to completion via
/// `ExecutorHandle::run` (see call sites below), on the same executor
/// the item tasks are spawned on.
async fn typecheck_program(
    package: hir::HirPackage,
    executor: ExecutorHandle,
) -> Result<Rc<RefCell<hir::HirPackage>>> {
    let checker = HirTypeChecker::new(Rc::new(RefCell::new(package)), None, None, executor);
    let item_ids: Vec<_> = checker
        .borrow()
        .package()
        .items
        .iter()
        .map(|item| item.def_id.clone())
        .collect();
    let handles: Vec<_> = item_ids
        .into_iter()
        .map(|def_id| HirTypeChecker::spawn_item_task(&checker, def_id))
        .collect();
    for handle in handles {
        handle.await;
    }
    Ok(checker.borrow().finish())
}

#[test]
fn typechecker_stages_current_package_without_publishing_it() {
    let dependency_id = hir::PackageId::new("dependency");
    let dependency = Rc::new(RefCell::new(hir::HirPackage::new(dependency_id.clone())));
    let mut dependency_program = hir::HirProgram::new();
    dependency_program.add_package(dependency.clone());
    let dependency_program = Rc::new(dependency_program);

    let current_id = test_pkg();
    let current = Rc::new(RefCell::new(hir::HirPackage::new(current_id.clone())));
    let checker = HirTypeChecker::new(
        current.clone(),
        Some(dependency_program.clone()),
        None,
        fp_core::executor::CompilerExecutor::new().handle(),
    );
    let typing_program = checker.borrow().program_handle();

    assert!(Rc::ptr_eq(
        &dependency,
        &typing_program
            .package_rc(&dependency_id)
            .expect("dependency package should remain visible"),
    ));
    assert!(Rc::ptr_eq(
        &current,
        &typing_program
            .package_rc(&current_id)
            .expect("current package should be visible while checking"),
    ));
    assert!(dependency_program.package_rc(&current_id).is_none());
}

/// The core same-package ordering fix: `const A` (checked first, per
/// `program.items`' textual order) references `const B`, declared
/// *later* in the same list. Before `expr_path_ty`'s `Const` arm
/// awaited `B`'s own task on demand, this silently fell back to
/// "constant type was not recorded" instead of resolving `B`'s real
/// type.
#[test]
fn forward_referenced_const_resolves_regardless_of_item_order() {
    let b_def_id = hir::DefId::local(2);
    let a_def_id = hir::DefId::local(1);

    let b_item = hir::Item {
        hir_id: hid(10),
        def_id: b_def_id.clone(),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Const(hir::Const {
            name: "B".into(),
            mutable: false,
            is_host: false,
            ty: hir::TypeExpr {
                hir_id: hid(11),
                kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
                span: fp_core::span::Span::null(),
            },
            body: hir::Body {
                hir_id: hid(12),
                params: Vec::new(),
                value: hir::Expr {
                    hir_id: hid(13),
                    kind: hir::ExprKind::Literal(hir::Lit::Integer(41)),
                    span: fp_core::span::Span::null(),
                },
            },
        }),
        span: fp_core::span::Span::null(),
    };

    let a_item = hir::Item {
        hir_id: hid(20),
        def_id: a_def_id.clone(),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Const(hir::Const {
            name: "A".into(),
            mutable: false,
            is_host: false,
            ty: hir::TypeExpr {
                hir_id: hid(21),
                kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
                span: fp_core::span::Span::null(),
            },
            body: hir::Body {
                hir_id: hid(22),
                params: Vec::new(),
                value: hir::Expr {
                    hir_id: hid(23),
                    kind: hir::ExprKind::Binary(
                        hir::BinOp::Add,
                        Box::new(hir::Expr {
                            hir_id: hid(24),
                            kind: hir::ExprKind::Path(hir::Path {
                                segments: vec![hir::PathSegment {
                                    name: "B".into(),
                                    args: None,
                                }],
                                res: Some(hir::Res::Def(b_def_id.clone())),
                            }),
                            span: fp_core::span::Span::null(),
                        }),
                        Box::new(hir::Expr {
                            hir_id: hid(25),
                            kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
                            span: fp_core::span::Span::null(),
                        }),
                    ),
                    span: fp_core::span::Span::null(),
                },
            },
        }),
        span: fp_core::span::Span::null(),
    };

    let mut program = hir::HirPackage::new(test_pkg());
    // Textual order: A first, B second -- A's own task must await B's
    // on demand rather than assuming it's already been checked.
    program.items.push(a_item.clone());
    program.items.push(b_item.clone());
    program.def_map.insert(a_def_id.clone(), a_item);
    program.def_map.insert(b_def_id.clone(), b_item);

    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(
        results.borrow().const_type(a_def_id),
        Some(Ty::int(ty::IntTy::I64)),
        "forward-referenced const B's type must resolve, not fall back to error_ty"
    );
    assert_eq!(
        results.borrow().const_type(b_def_id),
        Some(Ty::int(ty::IntTy::I64))
    );
}

#[test]
fn records_literal_type_by_hir_id() {
    let expr = hir::Expr {
        hir_id: hid(7),
        kind: hir::ExprKind::Literal(hir::Lit::Integer(4)),
        span: fp_core::span::Span::null(),
    };
    let mut program = hir::HirPackage::new(test_pkg());
    let item = hir::Item {
        hir_id: hid(1),
        def_id: hir::DefId::local(1),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Expr(expr),
        span: fp_core::span::Span::null(),
    };
    program.items.push(item.clone());
    // Real HIR lowering always populates `def_map` before typing begins
    // (see `ast_to_hir::transform_package`'s last step) — per-item tasks
    // look items up by `DefId` through it (needed so a cross-reference
    // to an item spawned only by `def_id`, not handed the `Item`
    // directly, can still find it), so a hand-built test program needs
    // to mirror that.
    program.def_map.insert(item.def_id.clone(), item);

    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(
        results.borrow().expr_type(hid(7)),
        Some(Ty::int(ty::IntTy::I64))
    );
}

#[test]
fn records_binding_pattern_type() {
    let pattern = hir::Pat {
        hir_id: hid(8),
        kind: hir::PatKind::Binding {
            name: "value".into(),
            mutable: false,
        },
    };
    let expr = hir::Expr {
        hir_id: hid(9),
        kind: hir::ExprKind::Let(
            pattern,
            Box::new(hir::TypeExpr {
                hir_id: hid(10),
                kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
                span: fp_core::span::Span::null(),
            }),
            None,
        ),
        span: fp_core::span::Span::null(),
    };
    let mut program = hir::HirPackage::new(test_pkg());
    let item = hir::Item {
        hir_id: hid(1),
        def_id: hir::DefId::local(1),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Expr(expr),
        span: fp_core::span::Span::null(),
    };
    program.items.push(item.clone());
    program.def_map.insert(item.def_id.clone(), item);

    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(
        results.borrow().pat_type(hid(8)),
        Some(Ty::int(ty::IntTy::I64))
    );
}

fn str_shaped_ty() -> Ty {
    Ty { kind: TyKind::Str }
}

#[test]
fn string_and_byte_slice_use_distinct_method_lookup_buckets() {
    let string_keys =
        type_shapes::ty_shape_keys(&TyKind::Str).expect("str has a method lookup shape");
    let byte_slice_keys =
        type_shapes::ty_shape_keys(&TyKind::Slice(Box::new(Ty::uint(ty::UintTy::U8))))
            .expect("[u8] has a method lookup shape");

    assert_eq!(string_keys, vec!["str"]);
    assert_eq!(byte_slice_keys, vec!["[]"]);
}

#[test]
fn error_types_are_invalid_for_lookup_even_when_nested() {
    let invalid = Ty {
        kind: TyKind::Ref(
            ty::Region::ReStatic,
            Box::new(Ty {
                kind: TyKind::Tuple(vec![Box::new(Ty::error())]),
            }),
            ty::Mutability::Not,
        ),
    };

    assert!(ty_contains_error(&invalid));
    assert!(!ty_contains_error(&Ty {
        kind: TyKind::Param(ty::ParamTy {
            index: 0,
            name: "T".into(),
        }),
    }));
}

#[test]
fn impl_header_obligation_is_keyed_by_impl_def_id() {
    let package_id = test_pkg();
    let shared_self_ty = hir::TypeExpr {
        hir_id: hid(901),
        kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
        span: fp_core::span::Span::null(),
    };
    let first_impl = hir::DefId::new(package_id.clone(), 901);
    let second_impl = hir::DefId::new(package_id.clone(), 902);
    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let checker = HirTypeChecker::new(
        Rc::new(RefCell::new(hir::HirPackage::new(package_id))),
        None,
        None,
        executor.clone(),
    );
    executor.run(async move {
        let mut checker = checker.borrow_mut();
        let first = checker
            .checked_impl_self_ty(&first_impl, &shared_self_ty)
            .await
            .expect("first impl header should resolve");
        let second = checker
            .checked_impl_self_ty(&second_impl, &shared_self_ty)
            .await
            .expect("distinct impl header should resolve");
        assert_eq!(first, second);
        assert!(checker.resolving_impl_headers.is_empty());
    });
}

/// Wraps a bare `hir::TypeExpr` in `let value: <ty>;` (no initializer)
/// the same way `f16_and_f128_type_paths_resolve_as_primitive_floats`
/// does, so `check_type_expr`'s handling of a single `TypeExprKind` can
/// be exercised in isolation via `results.pat_type(hid(8))`.
fn let_with_type(ty_kind: hir::TypeExprKind) -> hir::HirPackage {
    let pattern = hir::Pat {
        hir_id: hid(8),
        kind: hir::PatKind::Binding {
            name: "value".into(),
            mutable: false,
        },
    };
    let expr = hir::Expr {
        hir_id: hid(9),
        kind: hir::ExprKind::Let(
            pattern,
            Box::new(hir::TypeExpr {
                hir_id: hid(10),
                kind: ty_kind,
                span: fp_core::span::Span::null(),
            }),
            None,
        ),
        span: fp_core::span::Span::null(),
    };
    let mut program = hir::HirPackage::new(test_pkg());
    let item = hir::Item {
        hir_id: hid(1),
        def_id: hir::DefId::local(1),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Expr(expr),
        span: fp_core::span::Span::null(),
    };
    program.items.push(item.clone());
    program.def_map.insert(item.def_id.clone(), item);
    program
}

#[test]
fn string_literal_type_resolves_to_str() {
    let program = let_with_type(hir::TypeExprKind::LiteralString("foo".into()));
    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(results.borrow().pat_type(hid(8)), Some(str_shaped_ty()));
}

#[test]
fn union_of_string_literal_types_resolves_to_str() {
    let program = let_with_type(hir::TypeExprKind::TypeBinaryOp(hir::TypeBinaryOp {
        kind: fp_core::ast::TypeBinaryOpKind::Union,
        lhs: Box::new(hir::TypeExpr {
            hir_id: hid(11),
            kind: hir::TypeExprKind::LiteralString("a".into()),
            span: fp_core::span::Span::null(),
        }),
        rhs: Box::new(hir::TypeExpr {
            hir_id: hid(12),
            kind: hir::TypeExprKind::LiteralString("b".into()),
            span: fp_core::span::Span::null(),
        }),
    }));
    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(results.borrow().pat_type(hid(8)), Some(str_shaped_ty()));
}

/// A union of two *non*-literal types (e.g. two primitives) must keep
/// erroring exactly as it did before literal-union support was added —
/// only a union where every operand is itself a literal-string type (or
/// a nested union of them) is accepted.
#[test]
fn union_of_non_literal_types_still_errors() {
    let program = let_with_type(hir::TypeExprKind::TypeBinaryOp(hir::TypeBinaryOp {
        kind: fp_core::ast::TypeBinaryOpKind::Union,
        lhs: Box::new(hir::TypeExpr {
            hir_id: hid(11),
            kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
            span: fp_core::span::Span::null(),
        }),
        rhs: Box::new(hir::TypeExpr {
            hir_id: hid(12),
            kind: hir::TypeExprKind::Primitive(TypePrimitive::Bool),
            span: fp_core::span::Span::null(),
        }),
    }));
    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(results.borrow().pat_type(hid(8)), Some(Ty::error()));
}

/// `f16`/`f128` are real, stabilized Rust primitive float types (same
/// family as `f32`/`f64`), not name-resolution gaps — a bare `f16`/
/// `f128` type path must resolve straight to `Ty::Float`, never fall
/// through to `path_ty`'s "unresolved type path" `error_ty` branch the
/// way an actually-undeclared name would.
#[test]
fn f16_and_f128_type_paths_resolve_as_primitive_floats() {
    // `let value: f16/f128;` with no initializer — `ExprKind::Let`'s
    // declared-type slot (`check_type_expr(target)`) is recorded into
    // `pat_types` verbatim, unlike a `Const`'s slot (which gets
    // overwritten by the body's own inferred type), so this isolates
    // exactly what `path_ty`/`primitive_path_ty` resolve a bare
    // `f16`/`f128` path to.
    fn let_item(
        def_id: hir::DefId,
        hir_id_base: u32,
        pat_name: &str,
        path_name: &str,
    ) -> hir::Item {
        let pattern = hir::Pat {
            hir_id: hid(hir_id_base + 1),
            kind: hir::PatKind::Binding {
                name: pat_name.into(),
                mutable: false,
            },
        };
        let expr = hir::Expr {
            hir_id: hid(hir_id_base + 2),
            kind: hir::ExprKind::Let(
                pattern,
                Box::new(hir::TypeExpr {
                    hir_id: hid(hir_id_base + 3),
                    kind: hir::TypeExprKind::Path(hir::Path {
                        segments: vec![hir::PathSegment {
                            name: path_name.into(),
                            args: None,
                        }],
                        res: None,
                    }),
                    span: fp_core::span::Span::null(),
                }),
                None,
            ),
            span: fp_core::span::Span::null(),
        };
        hir::Item {
            hir_id: hid(hir_id_base),
            def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Expr(expr),
            span: fp_core::span::Span::null(),
        }
    }

    let f16_def_id = hir::DefId::local(1);
    let f128_def_id = hir::DefId::local(2);
    let f16_item = let_item(f16_def_id.clone(), 10, "f16_value", "f16");
    let f128_item = let_item(f128_def_id.clone(), 20, "f128_value", "f128");

    let mut program = hir::HirPackage::new(test_pkg());
    program.items.push(f16_item.clone());
    program.items.push(f128_item.clone());
    program.def_map.insert(f16_def_id, f16_item);
    program.def_map.insert(f128_def_id, f128_item);

    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let results = executor
        .run(typecheck_program(program, executor.clone()))
        .expect("HIR type check");
    assert_eq!(
        results.borrow().pat_type(hid(11)),
        Some(Ty::float(ty::FloatTy::F16)),
        "bare `f16` type path must resolve to the f16 primitive, not an unresolved-path error type"
    );
    assert_eq!(
        results.borrow().pat_type(hid(21)),
        Some(Ty::float(ty::FloatTy::F128)),
        "bare `f128` type path must resolve to the f128 primitive, not an unresolved-path error type"
    );
}

#[test]
fn typed_command_helper_local_preserves_method_def_identity() {
    let package_id = test_pkg();
    let command_id = hir::DefId::new(package_id.clone(), 1);
    let helper_id = hir::DefId::new(package_id.clone(), 2);
    let impl_id = hir::DefId::new(package_id.clone(), 3);
    let output_id = hir::DefId::new(package_id.clone(), 4);
    let new_id = hir::DefId::new(package_id.clone(), 6);
    let caller_id = hir::DefId::new(package_id.clone(), 5);
    let output_call_hir_id = hid(50);
    let command_path = || hir::TypeExpr {
        hir_id: hid(60),
        kind: hir::TypeExprKind::Path(hir::Path {
            segments: vec![hir::PathSegment {
                name: "Command".into(),
                args: None,
            }],
            res: Some(hir::Res::Def(command_id.clone())),
        }),
        span: fp_core::span::Span::null(),
    };
    let unit_ty = || hir::TypeExpr {
        hir_id: hid(61),
        kind: hir::TypeExprKind::Tuple(Vec::new()),
        span: fp_core::span::Span::null(),
    };
    let function = |name: &str, output: hir::TypeExpr, body: hir::Block| hir::Function {
        sig: hir::FunctionSig {
            name: name.into(),
            inputs: Vec::new(),
            output,
            generics: hir::Generics::default(),
            abi: ty::Abi::Rust,
        },
        body: Some(body),
        is_const: false,
        is_extern: false,
        is_async: false,
        attrs: Vec::new(),
    };
    let command = hir::Item {
        hir_id: hid(1),
        def_id: command_id.clone(),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Struct(hir::Struct {
            name: "Command".into(),
            fields: Vec::new(),
            generics: hir::Generics::default(),
            repr: fp_core::ast::ReprOptions::default(),
        }),
        span: fp_core::span::Span::null(),
    };
    let helper = hir::Item {
        hir_id: hid(2),
        def_id: helper_id.clone(),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Function(function(
            "helper",
            command_path(),
            hir::Block {
                hir_id: hid(20),
                stmts: Vec::new(),
                expr: Some(Box::new(hir::Expr {
                    hir_id: hid(21),
                    kind: hir::ExprKind::Struct(
                        hir::Path {
                            segments: vec![hir::PathSegment {
                                name: "Command".into(),
                                args: None,
                            }],
                            res: Some(hir::Res::Def(command_id.clone())),
                        },
                        Vec::new(),
                    ),
                    span: fp_core::span::Span::null(),
                })),
            },
        )),
        span: fp_core::span::Span::null(),
    };
    let impl_item = hir::Item {
        hir_id: hid(3),
        def_id: impl_id,
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Impl(hir::Impl {
            generics: hir::Generics::default(),
            trait_ty: None,
            self_ty: command_path(),
            items: vec![
                hir::ImplItem {
                    def_id: output_id.clone(),
                    hir_id: hid(30),
                    name: "output".into(),
                    kind: hir::ImplItemKind::Method({
                        let mut method = function(
                            "output",
                            unit_ty(),
                            hir::Block {
                                hir_id: hid(31),
                                stmts: Vec::new(),
                                expr: Some(Box::new(hir::Expr {
                                    hir_id: hid(32),
                                    kind: hir::ExprKind::Tuple(Vec::new()),
                                    span: fp_core::span::Span::null(),
                                })),
                            },
                        );
                        method.sig.inputs.push(hir::Param {
                            hir_id: hid(33),
                            pat: hir::Pat {
                                hir_id: hid(34),
                                kind: hir::PatKind::Binding {
                                    name: "self".into(),
                                    mutable: true,
                                },
                            },
                            ty: hir::TypeExpr {
                                hir_id: hid(35),
                                kind: hir::TypeExprKind::Ref(Box::new(command_path())),
                                span: fp_core::span::Span::null(),
                            },
                            is_context: false,
                            as_tuple: false,
                            as_dict: false,
                            default: None,
                        });
                        method
                    }),
                },
                hir::ImplItem {
                    def_id: new_id.clone(),
                    hir_id: hid(36),
                    name: "new".into(),
                    kind: hir::ImplItemKind::Method(function(
                        "new",
                        command_path(),
                        hir::Block {
                            hir_id: hid(37),
                            stmts: Vec::new(),
                            expr: Some(Box::new(hir::Expr {
                                hir_id: hid(38),
                                kind: hir::ExprKind::Struct(
                                    hir::Path {
                                        segments: vec![hir::PathSegment {
                                            name: "Command".into(),
                                            args: None,
                                        }],
                                        res: Some(hir::Res::Def(command_id.clone())),
                                    },
                                    Vec::new(),
                                ),
                                span: fp_core::span::Span::null(),
                            })),
                        },
                    )),
                },
            ],
        }),
        span: fp_core::span::Span::null(),
    };
    let caller = hir::Item {
        hir_id: hid(4),
        def_id: caller_id.clone(),
        visibility: hir::Visibility::Private,
        kind: hir::ItemKind::Function(function(
            "caller",
            unit_ty(),
            hir::Block {
                hir_id: hid(40),
                stmts: vec![
                    hir::Stmt {
                        hir_id: hid(41),
                        kind: hir::StmtKind::Local(hir::Local {
                            hir_id: hid(42),
                            pat: hir::Pat {
                                hir_id: hid(43),
                                kind: hir::PatKind::Binding {
                                    name: "cmd".into(),
                                    mutable: true,
                                },
                            },
                            ty: Some(command_path()),
                            init: Some(hir::Expr {
                                hir_id: hid(44),
                                kind: hir::ExprKind::Call(
                                    Box::new(hir::Expr {
                                        hir_id: hid(45),
                                        kind: hir::ExprKind::Path(hir::Path {
                                            segments: vec![hir::PathSegment {
                                                name: "helper".into(),
                                                args: None,
                                            }],
                                            res: Some(hir::Res::Def(helper_id)),
                                        }),
                                        span: fp_core::span::Span::null(),
                                    }),
                                    Vec::new(),
                                ),
                                span: fp_core::span::Span::null(),
                            }),
                        }),
                    },
                    hir::Stmt {
                        hir_id: hid(46),
                        kind: hir::StmtKind::Local(hir::Local {
                            hir_id: hid(47),
                            pat: hir::Pat {
                                hir_id: hid(48),
                                kind: hir::PatKind::Binding {
                                    name: "created".into(),
                                    mutable: false,
                                },
                            },
                            ty: None,
                            init: Some(hir::Expr {
                                hir_id: hid(49),
                                kind: hir::ExprKind::Call(
                                    Box::new(hir::Expr {
                                        hir_id: hid(52),
                                        kind: hir::ExprKind::Path(hir::Path {
                                            segments: vec![
                                                hir::PathSegment {
                                                    name: "Command".into(),
                                                    args: None,
                                                },
                                                hir::PathSegment {
                                                    name: "new".into(),
                                                    args: None,
                                                },
                                            ],
                                            // Type-relative resolution is deliberately the type;
                                            // typeck must record `new_id` on the enclosing call.
                                            res: Some(hir::Res::Def(command_id.clone())),
                                        }),
                                        span: fp_core::span::Span::null(),
                                    }),
                                    Vec::new(),
                                ),
                                span: fp_core::span::Span::null(),
                            }),
                        }),
                    },
                ],
                expr: Some(Box::new(hir::Expr {
                    hir_id: output_call_hir_id.clone(),
                    kind: hir::ExprKind::MethodCall(
                        Box::new(hir::Expr {
                            hir_id: hid(51),
                            kind: hir::ExprKind::Path(hir::Path {
                                segments: vec![hir::PathSegment {
                                    name: "cmd".into(),
                                    args: None,
                                }],
                                res: Some(hir::Res::Local(hid(43))),
                            }),
                            span: fp_core::span::Span::null(),
                        }),
                        "output".into(),
                        None,
                        Vec::new(),
                    ),
                    span: fp_core::span::Span::null(),
                })),
            },
        )),
        span: fp_core::span::Span::null(),
    };

    let mut package = hir::HirPackage::new(package_id);
    for item in [command, helper, impl_item, caller] {
        package.def_map.insert(item.def_id.clone(), item.clone());
        package.items.push(item);
    }
    let executor = fp_core::executor::CompilerExecutor::new().handle();
    let result = executor
        .run(typecheck_program(package, executor.clone()))
        .expect("HIR type check");
    assert_eq!(
        result.borrow().method_resolution(output_call_hir_id),
        Some(output_id),
        "a typed helper local must retain its Command DefId for method resolution"
    );
    assert_eq!(
        result.borrow().method_resolution(hid(49)),
        Some(new_id),
        "a type-relative associated call must retain its selected impl member DefId"
    );
}

#[test]
fn comptime_request_returns_resolver_value_directly() {
    let resolver: ComptimeResolver = Rc::new(|program, request| {
        assert!(program.package_rc(&request.package_id).is_some());
        Box::pin(async { Ok(fp_core::ast::Value::unit()) })
    });
    let package = Rc::new(RefCell::new(hir::HirPackage::new(test_pkg())));
    let checker = HirTypeChecker::new(
        package,
        None,
        Some(resolver),
        fp_core::executor::CompilerExecutor::new().handle(),
    );
    let request = ComptimeRequest {
        package_id: test_pkg(),
        def_id: hir::DefId::new(test_pkg(), 0),
    };
    let mut future = Box::pin(async move { checker.borrow().request_comptime(request).await });
    let waker = std::task::Waker::noop();
    let mut cx = std::task::Context::from_waker(waker);
    let value = match future.as_mut().poll(&mut cx) {
        std::task::Poll::Ready(result) => result.expect("comptime value"),
        std::task::Poll::Pending => {
            panic!("resolver-backed comptime request should resolve immediately")
        }
    };
    assert!(value.is_unit());
}
