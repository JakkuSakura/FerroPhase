use common::assert_eq;
use common_lang::ast::*;
use common_lang::value::{EnumType, EnumTypeVariant, TypeValue, UnitType};
use common_lang::Serializer;
use javascript_lang::ts::printer::TsPrinter;
#[test]
fn test_print_enum_declaration() {
    let printer = TsPrinter::new();
    let def = EnumType {
        name: Ident::new("Test"),
        variants: vec![
            EnumTypeVariant {
                name: Ident::new("A"),
                value: TypeValue::Unit(UnitType),
            },
            EnumTypeVariant {
                name: Ident::new("B"),
                value: TypeValue::Unit(UnitType),
            },
        ],
    };
    let s = printer.serialize_type(&TypeValue::Enum(def)).unwrap();
    assert_eq!(
        s,
        r#"enum Test {
    A,
    B,
}
"#
    );
}
