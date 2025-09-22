use std::sync::Arc;

use eyre::Result;
use fp_core::bail;
use fp_core::info;
use fp_core::warn;
use pretty_assertions::assert_eq;

use fp_core::ast::*;
use fp_core::ast::{AstItem, ItemDefFunction, ItemImpl, Visibility};
use fp_core::ast::{AstType, FunctionParam, FunctionSignature, TypePrimitive};
use fp_core::id::Locator;
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
        AstItem::DefFunction(ItemDefFunction {
            attrs: vec![],
            name: "foo".into(),
            ty: None,
            sig: FunctionSignature {
                name: Some("foo".into()),
                receiver: None,
                params: vec![FunctionParam::new(
                    "a".into(),
                    AstType::Primitive(TypePrimitive::i64())
                )],
                generics_params: vec![],
                ret_ty: Some(AstType::Primitive(TypePrimitive::i64()))
            },
            body: block.into(),
            visibility: Visibility::Private,
        })
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
        AstExpr::Block(ExprBlock {
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
        AstExpr::If(ExprIf {
            cond: AstExpr::value(AstValue::bool(true)).into(),
            then: AstExpr::Block(ExprBlock::new()).into(),
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
        AstExpr::If(ExprIf {
            cond: AstExpr::value(AstValue::bool(true)).into(),
            then: AstExpr::Block(ExprBlock::new()).into(),
            elze: Some(AstExpr::Block(ExprBlock::new()).into()),
        }),
    );
    let code = shll_parse_expr! {
        if true {

        } else if false {

        }
    };
    assert_eq!(
        code,
        AstExpr::If(ExprIf {
            cond: AstExpr::value(AstValue::bool(true)).into(),
            then: AstExpr::Block(ExprBlock::new()).into(),
            elze: Some(
                AstExpr::If(ExprIf {
                    cond: AstExpr::value(AstValue::bool(false)).into(),
                    then: AstExpr::Block(ExprBlock::new()).into(),
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
    assert_eq!(
        code,
        AstExpr::If(ExprIf {
            cond: AstExpr::value(AstValue::bool(true)).into(),
            then: AstExpr::unit().into(),
            elze: Some(
                AstExpr::If(ExprIf {
                    cond: AstExpr::value(AstValue::bool(false)).into(),
                    then: AstExpr::Block(ExprBlock::new_stmts_expr(
                        vec![BlockStmt::Noop,],
                        AstExpr::Block(ExprBlock::new())
                    ))
                    .into(),
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
        AstExpr::Block(ExprBlock::new_stmts(vec![
            BlockStmt::Expr(
                BlockStmtExpr::new(AstExpr::If(ExprIf {
                    cond: AstExpr::value(AstValue::bool(true)).into(),
                    then: AstExpr::Block(ExprBlock::new()).into(),
                    elze: None,
                }))
                .with_semicolon(false)
            ),
            BlockStmt::Expr(
                BlockStmtExpr::new(AstExpr::If(ExprIf {
                    cond: AstExpr::value(AstValue::bool(true)).into(),
                    then: AstExpr::Block(ExprBlock::new()).into(),
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
        AstItem::Impl(ItemImpl {
            trait_ty: Some(Locator::Ident("Foo".into())),
            self_ty: AstExpr::ident("Bar".into()),
            items: vec![shll_parse_item! {
                fn foo(a: i64) -> i64 {
                    a + 1
                }
            }],
        })
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
        AstItem::DefStatic(ItemDefStatic {
            name: "FOO".into(),
            ty: AstType::Primitive(TypePrimitive::i64()),
            value: AstExpr::value(AstValue::int(1)).into(),
            visibility: Visibility::Private,
        })
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
        AstItem::DefFunction(ItemDefFunction {
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
            body: AstExpr::Block(ExprBlock::new()).into(),
            visibility: Visibility::Private,
        })
    );
    let code = shll_parse_item! {
        fn foo(&'static self) {}
    };

    assert_eq!(
        code,
        AstItem::DefFunction(ItemDefFunction {
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
            body: AstExpr::Block(ExprBlock::new()).into(),
            visibility: Visibility::Private,
        })
    );
    Ok(())
}
