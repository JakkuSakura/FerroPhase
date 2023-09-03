use common::*;
use common_lang::ast::*;
use common_lang::value::{
    FunctionParam, FunctionSignature, FunctionValue, PrimitiveType, TypeValue,
};
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
            kind: DefineKind::Function,
            ty: None,
            value: DefineValue::Function(FunctionValue {
                sig: FunctionSignature {
                    name: Some("foo".into()),
                    params: vec![FunctionParam {
                        name: "a".into(),
                        ty: TypeValue::Primitive(PrimitiveType::i64())
                    }],
                    generics_params: vec![],
                    ret: TypeValue::Primitive(PrimitiveType::i64())
                },
                body: block.into(),
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
            trait_ty: Some(Locator::Ident("Foo".into())),
            self_ty: TypeExpr::ident("Bar".into()),
            items: vec![shll_parse_item! {
                fn foo(a: i64) -> i64 {
                    a + 1
                }
            }],
        })
    );
    Ok(())
}
