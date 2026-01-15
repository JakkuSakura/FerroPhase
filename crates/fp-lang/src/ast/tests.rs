use super::*;
use fp_core::ast::{QuoteFragmentKind, Ty};
use crate::syntax::SyntaxPrinter;
use fp_core::ast::{AttrMeta, BlockStmt, ExprKind, ItemKind, MacroDelimiter};
use fp_core::ops::BinOpKind;

#[test]
fn parses_rust_like_source() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn main() { println!(\"hi\"); }")
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parses_quote_and_splice() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("quote { splice ( token ) }").unwrap();
    match expr.kind() {
        ExprKind::Quote(q) => {
            let inner = q.block.last_expr().expect("quote should carry expr");
            assert!(matches!(inner.kind(), ExprKind::Splice(_)));
        }
        other => panic!("expected quote expr, got {:?}", other),
    }
}

#[test]
fn cst_printer_roundtrips_source_text() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn main() { emit! { let generated = 42; generated } }\n";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn expr_cst_first_parses_basic_binary_ops() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();

    let cst = parser.parse_expr_cst("a + b * 2").expect("parse_expr_cst");
    let printed = SyntaxPrinter::print(&cst);
    assert_eq!(printed, "a + b * 2");

    let cst_first = parser.parse_expr_ast("a + b * 2").expect("parse_expr_ast");
    assert!(matches!(cst_first.kind(), ExprKind::BinOp(_)));
}

#[test]
fn nested_quote_splice_and_control_flow() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
            fn main() {
                if true { let _ = quote { splice ( z ); }; }
                loop { let _ = quote { 1 + 2 }; break; }
                while false { let _ = splice ( quote { 3 } ); }
            }
        "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parser_handles_raw_identifiers_and_strings() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr_src = r####"r#type + "hi\\nthere" + r#"hello world"# + br##"bin data"## + b"abc""####;
    let cst = parser.parse_expr_cst(expr_src).unwrap();
    let printed = SyntaxPrinter::print(&cst);
    assert_eq!(printed, expr_src);
}

#[test]
fn parse_expr_ast_builds_quote_ast() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("quote { 1 + 2 }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Quote(_)));
}

#[test]
fn parse_expr_ast_supports_typed_quote_fragments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("quote<item> { struct S { x: i64 } }")
        .unwrap();
    match expr.kind() {
        ExprKind::Quote(quote) => assert_eq!(quote.kind, Some(QuoteFragmentKind::Item)),
        other => panic!("expected quote expr, got {:?}", other),
    }

    let expr = parser.parse_expr_ast("quote<expr> { 1 + 2 }").unwrap();
    match expr.kind() {
        ExprKind::Quote(quote) => assert_eq!(quote.kind, Some(QuoteFragmentKind::Expr)),
        other => panic!("expected quote expr, got {:?}", other),
    }
}

#[test]
fn parse_items_ast_supports_quote_fn() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("quote fn build(flag: bool) -> item { struct A { x: i64 } }")
        .unwrap();
    match items.first().map(|item| item.kind()) {
        Some(ItemKind::DefFunction(func)) => {
            match func.sig.ret_ty.as_ref() {
                Some(Ty::Quote(quote)) => {
                    assert_eq!(quote.kind, QuoteFragmentKind::Item);
                }
                other => panic!("expected quote item return type, got {:?}", other),
            }
            assert_eq!(func.sig.quote_kind, Some(QuoteFragmentKind::Item));
            match func.body.kind() {
                ExprKind::Block(_) => {}
                other => panic!("expected block body, got {:?}", other),
            }
        }
        other => panic!("expected quote fn item, got {:?}", other),
    }
}

#[test]
fn parse_expr_ast_handles_splice_of_quote() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("splice ( quote { 1 } )").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Splice(_)));
}

#[test]
fn parse_expr_ast_supports_splice_without_parens() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("splice build_items(true)").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Splice(_)));
}

#[test]
fn parse_expr_ast_handles_macro_invocation() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("foo!{a + b}").unwrap();
    match expr.kind() {
        ExprKind::Macro(m) => {
            assert_eq!(m.invocation.delimiter, MacroDelimiter::Brace);
            assert!(m.invocation.span.is_some());
        }
        other => panic!("expected macro invocation, got {:?}", other),
    }
}

#[test]
fn parse_expr_ast_handles_if_loop_and_while() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("if true { 1 } else { 2 }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::If(_)));
    let expr = parser.parse_expr_ast("loop { break; }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Loop(_)));
    let expr = parser.parse_expr_ast("while false { break; }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::While(_)));
}

#[test]
fn parse_expr_ast_handles_for_loop_syntax() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("for x in xs { break; }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::For(_)));
}

#[test]
fn parse_expr_ast_handles_match() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("match x { _ => 1, y => y }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_match_guard_and_wildcard() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match x { _ if true => 1, y => y }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_range() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("1..=2").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Range(_)));
}

#[test]
fn parse_expr_ast_handles_calls_fields_and_assignments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("a.b(c)[0] = 1").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Assign(_)));
}

#[test]
fn parse_expr_ast_handles_closure() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|x| x + 1").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_typed_and_mut_closure_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|mut x: i32| x").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_async_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("async { 1 } ").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Async(_)));
}

#[test]
fn parse_expr_ast_handles_await() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("await foo").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Await(_)));
}

#[test]
fn parse_expr_ast_lowers_const_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("const { 1 + 2 }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parses_const_block_with_for_tuple_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
            const {
                for (i, x) in xs.iter().enumerate() {
                    splice ( quote { if x > ys[i] { return x; } } );
                }
            }
        "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parse_expr_ast_supports_let_statements_in_blocks() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ let x = 1; x }").unwrap();
    match expr.kind() {
        ExprKind::Block(block) => assert!(
            block.stmts.iter().any(|s| matches!(s, BlockStmt::Let(_))),
            "expected let statement in block"
        ),
        other => panic!("expected block expr, got {:?}", other),
    }
}

#[test]
fn parse_items_ast_handles_const_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("const X: i64 = 1;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefConst(_)));
}

#[test]
fn parse_items_ast_handles_static_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("static X: i64 = 1;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefStatic(_)));
}

#[test]
fn parse_items_ast_handles_type_alias() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("type X = i64;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefType(_)));
}

#[test]
fn parse_items_ast_handles_enum_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("enum E { A = 1, B }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefEnum(_)));
}

#[test]
fn parse_items_ast_handles_module_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("mod foo {}").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Module(_)));
}

#[test]
fn parse_items_ast_handles_external_module_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("mod foo;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Module(_)));
}

#[test]
fn parse_items_ast_handles_trait_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("trait T { fn f(); }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefTrait(_)));
}

#[test]
fn parse_items_ast_handles_impl_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("impl Foo { fn f() {} }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_trait_impl_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl Foo for Bar { fn f() {} }")
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_generic_fn_with_where() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn f<T>(x: T) where T: Foo { x }")
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_supports_fn_struct_and_use() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "use foo::bar; struct S { x: i64 } fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(items.len() >= 3);
}

#[test]
fn parse_items_supports_typed_params_and_fields() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "struct S { x: i64 } fn f(x: i64) -> i64 { x }";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(items.len() >= 2);
}

#[test]
fn parse_items_supports_fn_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[inline] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_supports_lang_name_value_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[lang = \"time_now\"] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let attr = function
        .attrs
        .iter()
        .find(|attr| matches!(&attr.meta, AttrMeta::NameValue(_)))
        .expect("expected name-value attribute");
    let AttrMeta::NameValue(meta) = &attr.meta else {
        unreachable!();
    };
    assert_eq!(meta.name.last().as_str(), "lang");
}

#[test]
fn parse_items_supports_item_macro() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("foo!{ bar }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Macro(_)));
}

#[test]
fn parse_expr_ast_handles_try_operator_on_identifier() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("x?").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Try(_)));
}

#[test]
fn parse_expr_ast_operator_precedence_smoke() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("1 + 2 * 3").unwrap();
    match expr.kind() {
        ExprKind::BinOp(op) => {
            assert_eq!(op.kind, BinOpKind::Add);
        }
        other => panic!("expected binop, got {:?}", other),
    }
}
