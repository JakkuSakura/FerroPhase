use fp_lang::ast::FerroPhaseParser;

#[test]
fn parses_assoc_constraint_in_generic_bound() {
    let parser = FerroPhaseParser::new();
    let code = r#"
        use std::ops::Add;
        fn apply<T: Add<Output = T>>(a: T, b: T, op: fn(T, T) -> T) {
            op(a, b);
        }
    "#;
    let res = parser.parse_items_ast(code);
    assert!(res.is_ok(), "expected parse success, got {:?}", res);
}
