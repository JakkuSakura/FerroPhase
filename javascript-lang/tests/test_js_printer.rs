use common::assert_eq;
use javascript_lang::ts::printer::TsPrinter;
use lang_core::expr::*;
use lang_core::value::{EnumTypeVariant, Type, TypeEnum, TypeUnit};
use lang_core::Serializer;
#[test]
fn test_print_enum_declaration() {
    let printer = TsPrinter::new();
    let def = TypeEnum {
        name: crate::id::Ident::new("Test"),
        variants: vec![
            EnumTypeVariant {
                name: crate::id::Ident::new("A"),
                value: Type::Unit(TypeUnit),
            },
            EnumTypeVariant {
                name: crate::id::Ident::new("B"),
                value: Type::Unit(TypeUnit),
            },
        ],
    };
    let s = printer.serialize_type(&Type::Enum(def)).unwrap();
    assert_eq!(
        s,
        r#"declare const enum Test {
    A,
    B
}
"#
    );
}
