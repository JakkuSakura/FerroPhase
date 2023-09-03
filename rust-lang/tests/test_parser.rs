use common::*;
use common_lang::ast::*;
use common_lang::value::{FunctionParam, FunctionValue, PrimitiveType, TypeValue};
macro_rules! shll_parse_item {
    ($($tt:tt)*) => {{
        let code: syn::Item = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_item(code)?
    }};
}
macro_rules! shll_parse_expr {
    ($($tt:tt)*) => {{
        let code: syn::Expr = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_expr(code)?
    }};
}
#[test]
fn test_parse_fn() -> Result<()> {
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
        Item::Define(Define {
            name: "foo".into(),
            kind: DefKind::Function,
            ty: None,
            value: DefValue::Function(FunctionValue {
                name: Some("foo".into()),
                params: vec![FunctionParam {
                    name: "a".into(),
                    ty: TypeValue::Primitive(PrimitiveType::i64())
                }],
                generics_params: vec![],
                body: block.into(),
                ret: TypeValue::Primitive(PrimitiveType::i64())
            }),
            visibility: Visibility::Private,
        })
    );
    Ok(())
}
#[test]
fn test_parse_impl_for() -> Result<()> {
    let code = shll_parse_item! {
        impl Foo for Bar {
            fn foo(a: i64) -> i64 {
                a + 1
            }
        }
    };
    assert_eq!(
        code,
        Item::Impl(Impl {
            trait_ty: Some(Pat::Ident("Foo".into())),
            self_ty: TypeExpr::ident("Bar".into()),
            items: vec![Item::Define(Define {
                name: "foo".into(),
                kind: DefKind::Function,
                ty: None,
                value: DefValue::Function(FunctionValue {
                    name: Some("foo".into()),
                    params: vec![FunctionParam {
                        name: "a".into(),
                        ty: TypeValue::Primitive(PrimitiveType::i64())
                    }],
                    generics_params: vec![],
                    body: shll_parse_expr! {
                        a + 1
                    }
                    .into(),
                    ret: TypeValue::Primitive(PrimitiveType::i64())
                }),
                visibility: Visibility::Private,
            })],
        })
    );
    Ok(())
}
