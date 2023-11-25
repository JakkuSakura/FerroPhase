use common::assert_eq;
use common_lang::ast::*;
use common_lang::value::{EnumTypeVariant, TypeEnum, TypeUnit, TypeValue};
use common_lang::Serializer;
use javascript_lang::ts::printer::TsPrinter;
#[test]
fn test_print_enum_declaration() {
    let printer = TsPrinter::new();
    let def = TypeEnum {
        name: Ident::new("Test"),
        variants: vec![
            EnumTypeVariant {
                name: Ident::new("A"),
                value: TypeValue::Unit(TypeUnit),
            },
            EnumTypeVariant {
                name: Ident::new("B"),
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
