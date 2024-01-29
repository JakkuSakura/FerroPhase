use common::assert_eq;
use common_lang::expr::*;
use common_lang::value::{EnumTypeVariant, TypeEnum, TypeUnit, TypeValue};
use common_lang::Serializer;
use javascript_lang::ts::printer::TsPrinter;
#[test]
fn test_print_enum_declaration() {
    let printer = TsPrinter::new();
    let def = TypeEnum {
        name: crate::id::Ident::new("Test"),
        variants: vec![
            EnumTypeVariant {
                name: crate::id::Ident::new("A"),
                value: TypeValue::Unit(TypeUnit),
            },
            EnumTypeVariant {
                name: crate::id::Ident::new("B"),
                value: TypeValue::Unit(TypeUnit),
            },
        ],
    };
    let s = printer.serialize_type(&TypeValue::Enum(def)).unwrap();
    assert_eq!(
        s,
        r#"declare const enum Test {
    A,
    B
}
"#
    );
}
