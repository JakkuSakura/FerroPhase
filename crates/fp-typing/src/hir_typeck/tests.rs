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
) -> Result<Rc<hir::HirPackage>> {
    let current_package = Rc::new(package);
    let mut program = hir::HirProgram::new();
    program.add_package(current_package.clone());
    let checker = HirTypeChecker::new(Rc::new(program), current_package, None, executor);
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
        results.const_type(a_def_id),
        Some(Ty::int(ty::IntTy::I64)),
        "forward-referenced const B's type must resolve, not fall back to error_ty"
    );
    assert_eq!(results.const_type(b_def_id), Some(Ty::int(ty::IntTy::I64)));
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
    assert_eq!(results.expr_type(hid(7)), Some(Ty::int(ty::IntTy::I64)));
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
    assert_eq!(results.pat_type(hid(8)), Some(Ty::int(ty::IntTy::I64)));
}

fn str_shaped_ty() -> Ty {
    Ty {
        kind: TyKind::Slice(Box::new(Ty::uint(ty::UintTy::U8))),
    }
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
    assert_eq!(results.pat_type(hid(8)), Some(str_shaped_ty()));
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
    assert_eq!(results.pat_type(hid(8)), Some(str_shaped_ty()));
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
    assert_eq!(results.pat_type(hid(8)), Some(Ty::error()));
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
        results.pat_type(hid(11)),
        Some(Ty::float(ty::FloatTy::F16)),
        "bare `f16` type path must resolve to the f16 primitive, not an unresolved-path error type"
    );
    assert_eq!(
        results.pat_type(hid(21)),
        Some(Ty::float(ty::FloatTy::F128)),
        "bare `f128` type path must resolve to the f128 primitive, not an unresolved-path error type"
    );
}

#[test]
fn comptime_request_returns_resolver_value_directly() {
    let resolver: ComptimeResolver =
        Rc::new(|_request| Box::pin(async { Ok(fp_core::ast::Value::unit()) }));
    let package = Rc::new(hir::HirPackage::new(test_pkg()));
    let mut program = hir::HirProgram::new();
    program.add_package(package.clone());
    let checker = HirTypeChecker::new(
        Rc::new(program),
        package,
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
