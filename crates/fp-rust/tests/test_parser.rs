use std::sync::Arc;

use eyre::Result;
use pretty_assertions::assert_eq;

use fp_core::ast::Locator;
use fp_core::ast::*;
use fp_core::ast::{FunctionParam, FunctionSignature, Ty, TypePrimitive};
use fp_core::ast::{ItemDefFunction, ItemImpl, Visibility};
use fp_rust::printer::RustPrinter;
use fp_rust::{shll_parse_expr, shll_parse_item};

#[test]
fn test_parse_fn() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let code = shll_parse_item! {
        fn foo(a: i64) -> i64 {
            a + 1
        }
    };
    let block = shll_parse_expr! {
        a + 1
    };
    assert_eq!(
        code,
        ItemKind::DefFunction(ItemDefFunction {
            ty_annotation: None,
            attrs: vec![],
            name: "foo".into(),
            ty: None,
            sig: FunctionSignature {
                name: Some("foo".into()),
                receiver: None,
                params: vec![FunctionParam::new(
                    "a".into(),
                    Ty::Primitive(TypePrimitive::i64())
                )],
                generics_params: vec![],
                ret_ty: Some(Ty::Primitive(TypePrimitive::i64()))
            },
            body: block.into(),
            visibility: Visibility::Private,
        })
        .into()
    );
    Ok(())
}
#[test]
fn test_parse_block_noop() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let code = shll_parse_expr! {
        {
            ;;;
        }
    };

    // println!("{:?} => {}", code, code);
    assert_eq!(
        code,
        Expr::block(ExprBlock {
            stmts: vec![BlockStmt::Noop, BlockStmt::Noop, BlockStmt::Noop],
        })
    );
    Ok(())
}
#[test]
fn test_parse_if() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let code = shll_parse_expr! {
        if true {

        }
    };
    assert_eq!(
        code,
        Expr::from(ExprIf {
            cond: Expr::value(Value::bool(true)).into(),
            then: Expr::block(ExprBlock::new()).into(),
            elze: None,
        })
    );
    let code = shll_parse_expr! {
        if true {

        } else {

        }
    };
    assert_eq!(
        code,
        Expr::from(ExprIf {
            cond: Expr::value(Value::bool(true)).into(),
            then: Expr::block(ExprBlock::new()).into(),
            elze: Some(Expr::block(ExprBlock::new()).into()),
        }),
    );
    let code = shll_parse_expr! {
        if true {

        } else if false {

        }
    };
    assert_eq!(
        code,
        Expr::from(ExprIf {
            cond: Expr::value(Value::bool(true)).into(),
            then: Expr::block(ExprBlock::new()).into(),
            elze: Some(
                Expr::from(ExprIf {
                    cond: Expr::value(Value::bool(false)).into(),
                    then: Expr::block(ExprBlock::new()).into(),
                    elze: None,
                })
                .into()
            ),
        }),
    );
    let code = shll_parse_expr! {
        if true {
            ()
        } else if false {
             ; {}
        }
    };
    let nested_then_block = ExprBlock::new_stmts(vec![
        BlockStmt::Noop,
        BlockStmt::Expr(BlockStmtExpr::new(Expr::block(ExprBlock::new())).with_semicolon(false)),
    ]);

    assert_eq!(
        code,
        Expr::from(ExprIf {
            cond: Expr::value(Value::bool(true)).into(),
            then: Expr::unit().into(),
            elze: Some(
                Expr::from(ExprIf {
                    cond: Expr::value(Value::bool(false)).into(),
                    then: Expr::block(nested_then_block).into(),
                    elze: None,
                })
                .into()
            ),
        }),
    );
    Ok(())
}
#[test]
fn test_parse_block_if() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_expr! {
        {
            if true {
            }
            if true {
            };

        }
    };

    // println!("{:?} => {}", code, code);
    assert_eq!(
        code,
        Expr::block(ExprBlock::new_stmts(vec![
            BlockStmt::Expr(
                BlockStmtExpr::new(Expr::from(ExprIf {
                    cond: Expr::value(Value::bool(true)).into(),
                    then: Expr::block(ExprBlock::new()).into(),
                    elze: None,
                }))
                .with_semicolon(false)
            ),
            BlockStmt::Expr(
                BlockStmtExpr::new(Expr::from(ExprIf {
                    cond: Expr::value(Value::bool(true)).into(),
                    then: Expr::block(ExprBlock::new()).into(),
                    elze: None,
                }))
                .with_semicolon(true)
            ),
        ],))
    );
    Ok(())
}
#[test]
fn test_parse_impl_for() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_item! {
        impl Foo for Bar {
            fn foo(a: i64) -> i64 {
                a + 1
            }
        }
    };
    assert_eq!(
        code,
        ItemKind::Impl(ItemImpl {
            trait_ty: Some(Locator::Ident("Foo".into())),
            self_ty: Expr::ident("Bar".into()),
            items: vec![shll_parse_item! {
                fn foo(a: i64) -> i64 {
                    a + 1
                }
            }],
        })
        .into()
    );
    Ok(())
}
#[test]
fn test_parse_static() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let code = shll_parse_item! {
        static FOO: i64 = 1;
    };
    assert_eq!(
        code,
        ItemKind::DefStatic(ItemDefStatic {
            ty_annotation: None,
            name: "FOO".into(),
            ty: Ty::Primitive(TypePrimitive::i64()),
            value: Expr::value(Value::int(1)).into(),
            visibility: Visibility::Private,
        })
        .into()
    );
    println!("{}", code);
    Ok(())
}
#[test]
fn test_parse_fn_self() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    let code = shll_parse_item! {
        fn foo(&self) {}
    };

    assert_eq!(
        code,
        ItemKind::DefFunction(ItemDefFunction {
            ty_annotation: None,
            attrs: vec![],
            name: "foo".into(),
            ty: None,
            sig: FunctionSignature {
                name: Some("foo".into()),
                receiver: Some(FunctionParamReceiver::Ref),
                params: vec![],
                generics_params: vec![],
                ret_ty: None
            },
            body: Expr::block(ExprBlock::new()).into(),
            visibility: Visibility::Private,
        })
        .into()
    );
    let code = shll_parse_item! {
        fn foo(&'static self) {}
    };

    assert_eq!(
        code,
        ItemKind::DefFunction(ItemDefFunction {
            ty_annotation: None,
            attrs: vec![],
            name: "foo".into(),
            ty: None,
            sig: FunctionSignature {
                name: Some("foo".into()),
                receiver: Some(FunctionParamReceiver::RefStatic),
                params: vec![],
                generics_params: vec![],
                ret_ty: None
            },
            body: Expr::block(ExprBlock::new()).into(),
            visibility: Visibility::Private,
        })
        .into()
    );
    Ok(())
}
