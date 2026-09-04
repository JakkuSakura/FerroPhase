use super::*;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::{
    AttrMeta, AttrStyle, BlockStmt, ExprInvokeTarget, ExprKind, ItemKind, MacroDelimiter, Name,
    PatternKind, QuoteItemKind, Value,
};
use fp_core::ast::{QuoteFragmentKind, Ty};
use fp_core::ops::BinOpKind;

mod expression_tests;
mod item_tests;

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
fn direct_parser_accepts_emit_macro_source() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn main() { emit! { let generated = 42; generated } }\n";
    let items = parser.parse_items_ast(src);
    eprintln!(
        "parse result: {items:?}, diagnostics: {:?}",
        parser.diagnostics()
    );
    let items = items.unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_expr_ast_parses_basic_binary_ops() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("a + b * 2").expect("parse_expr_ast");
    assert!(matches!(expr.kind(), ExprKind::BinOp(_)));
}

#[test]
fn parse_raw_references_preserves_pointer_kind_and_mutability() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let raw_const = parser.parse_expr_ast("&raw const value").unwrap();
    let ExprKind::Reference(reference) = raw_const.kind() else {
        panic!("expected raw const reference");
    };
    assert!(reference.raw);
    assert_eq!(reference.mutable, None);
    let raw_mut = parser.parse_expr_ast("&raw mut value").unwrap();
    let ExprKind::Reference(reference) = raw_mut.kind() else {
        panic!("expected raw mut reference");
    };
    assert!(reference.raw);
    assert_eq!(reference.mutable, Some(true));
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
    let expr = parser.parse_expr_ast(expr_src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::BinOp(_)));
}

#[test]
fn parse_expr_ast_strips_prefix_from_parameter_path_segments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("super::super::Foo<Bar>").unwrap();
    match expr.kind() {
        ExprKind::Name(Name { path: path, .. }) => {
            assert_eq!(path.prefix, PathPrefix::Super(2));
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].ident.as_str(), "Foo");
            assert!(matches!(
                path.segments[0].args.as_deref(),
                Some(fp_core::ast::GenericArgs::AngleBracketed(args)) if args.args.len() == 1
            ));
        }
        other => panic!("expected parameter path expr, got {:?}", other),
    }
}

#[test]
fn parse_type_alias_strips_prefix_from_parameter_path_segments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = crate::module::Foo<Bar>;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected parameterized name, got {:?}", def.value);
    };
    let ExprKind::Name(Name { path: path, .. }) = expr.kind() else {
        panic!("expected parameterized name expr, got {:?}", expr);
    };
    assert_eq!(path.prefix, PathPrefix::Crate);
    assert_eq!(
        path.segments
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["module", "Foo"]
    );
    assert!(matches!(
        path.segments[1].args.as_deref(),
        Some(fp_core::ast::GenericArgs::AngleBracketed(args)) if args.args.len() == 1
    ));
}

#[test]
fn parse_type_args_accept_trailing_comma_before_close_angle() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "type Alias = std::collections::HashMap<String, Option<CallingConvention>,>;",
        )
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected parameterized name, got {:?}", def.value);
    };
    let ExprKind::Name(Name { path: path, .. }) = expr.kind() else {
        panic!("expected parameterized name expr, got {:?}", expr);
    };
    assert!(matches!(
        path.segments.last().unwrap().args.as_deref(),
        Some(fp_core::ast::GenericArgs::AngleBracketed(args)) if args.args.len() == 2
    ));
}

#[test]
fn parse_path_arguments_preserve_lifetime_const_and_binding_kinds() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = Foo<'a, 3, Item = T>;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected named path");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        path.segments[0].args.as_deref()
    else {
        panic!("expected angle-bracketed arguments");
    };
    assert!(matches!(
        args.args[0],
        fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Lifetime(_))
    ));
    assert!(matches!(
        args.args[1],
        fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Const(_))
    ));
    assert!(matches!(
        args.args[2],
        fp_core::ast::AngleBracketedArg::Constraint(fp_core::ast::AssocItemConstraint {
            kind: fp_core::ast::AssocItemConstraintKind::Equality { .. },
            ..
        })
    ));
}

#[test]
fn parse_assoc_constraint_preserves_item_generic_arguments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = Trait<Item<'a> = T, Bound<'b>: Display>;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected named path");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        path.segments[0].args.as_deref()
    else {
        panic!("expected angle-bracketed arguments");
    };
    let fp_core::ast::AngleBracketedArg::Constraint(fp_core::ast::AssocItemConstraint {
        gen_args: Some(fp_core::ast::GenericArgs::AngleBracketed(item_args)),
        kind: fp_core::ast::AssocItemConstraintKind::Equality { .. },
        ..
    }) = &args.args[0]
    else {
        panic!("expected generic associated-type equality constraint");
    };
    assert_eq!(item_args.args.len(), 1);
    let fp_core::ast::AngleBracketedArg::Constraint(fp_core::ast::AssocItemConstraint {
        gen_args: Some(fp_core::ast::GenericArgs::AngleBracketed(item_args)),
        kind: fp_core::ast::AssocItemConstraintKind::Bound { .. },
        ..
    }) = &args.args[1]
    else {
        panic!("expected generic associated-type bound constraint");
    };
    assert_eq!(item_args.args.len(), 1);
}

#[test]
fn parse_assoc_constraint_preserves_const_terms() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = Trait<OTHER = { 1 + 2 }>;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected named path");
    };
    let Some(GenericArgs::AngleBracketed(args)) = path.segments[0].args.as_deref() else {
        panic!("expected angle-bracketed arguments");
    };
    assert!(matches!(
        args.args[0],
        AngleBracketedArg::Constraint(AssocItemConstraint {
            kind: AssocItemConstraintKind::Equality {
                term: fp_core::ast::Term::Const(_),
            },
            ..
        })
    ));
}

#[test]
fn parse_generic_parameter_default_is_retained() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("struct Wrapper<T = u8> { value: T }")
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct");
    };
    assert!(def.value.generics_params[0].default.is_some());
}

#[test]
fn parse_turbofish_arguments_on_path_segment() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("foo::<u8>(1)").unwrap();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation");
    };
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        panic!("expected function path");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        name.path.segments[0].args.as_deref()
    else {
        panic!("expected turbofish arguments");
    };
    assert!(matches!(
        args.args[0],
        fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(_))
    ));
}

#[test]
fn parse_wildcard_generic_argument_as_type_variant() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("Vec::<_>::new()").unwrap();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation");
    };
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        panic!("expected function path");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        name.path.segments[0].args.as_deref()
    else {
        panic!("expected turbofish arguments");
    };
    assert!(matches!(
        args.args.as_slice(),
        [fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(ty))]
            if matches!(ty.as_ref(), fp_core::ast::Ty::Wildcard(_))
    ));
}

#[test]
fn parse_turbofish_arguments_are_retained_per_path_segment() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("Outer::<u8>::inner::<u16>(value)")
        .unwrap();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation");
    };
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        panic!("expected function path");
    };
    assert_eq!(name.path.segments.len(), 2);
    let Some(fp_core::ast::GenericArgs::AngleBracketed(outer_args)) =
        name.path.segments[0].args.as_deref()
    else {
        panic!("expected generic arguments on the receiver segment");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(method_args)) =
        name.path.segments[1].args.as_deref()
    else {
        panic!("expected generic arguments on the associated segment");
    };
    assert_eq!(outer_args.args.len(), 1);
    assert_eq!(method_args.args.len(), 1);
}

#[test]
fn parse_path_distinguishes_omitted_and_explicit_empty_arguments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();

    let bare = parser.parse_expr_ast("Foo").unwrap();
    let ExprKind::Name(Name {
        path: bare_path, ..
    }) = bare.kind()
    else {
        panic!("expected bare path");
    };
    assert!(bare_path.segments[0].args.is_none());

    let explicit = parser.parse_expr_ast("Foo<>").unwrap();
    let ExprKind::Name(Name {
        path: explicit_path,
        ..
    }) = explicit.kind()
    else {
        panic!("expected explicitly parameterized path");
    };
    assert!(matches!(
        explicit_path.segments[0].args.as_deref(),
        Some(fp_core::ast::GenericArgs::AngleBracketed(args)) if args.args.is_empty()
    ));
}

#[test]
fn parse_path_retains_source_span_and_rustc_shape_helpers() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();

    let expr = parser.parse_expr_ast("Outer::<u8>::inner").unwrap();
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected path expression");
    };
    assert!(!path.span().is_null());
    assert!(!path.is_global());
    assert!(!path.is_single_argless_ident());

    let ident = fp_core::ast::Ident::new("value");
    let bare = fp_core::ast::Path::from_ident(ident.clone());
    assert!(bare.is_single_argless_ident());
    assert_eq!(bare.as_single_argless_ident(), Some(ident));
}

#[test]
fn parse_qualified_path_keeps_qself_and_trait_segments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("<Vec<u8> as a::Trait>::Item")
        .unwrap();
    let ExprKind::Name(Name { qself, path }) = expr.kind() else {
        panic!("expected qualified path");
    };
    let qself = qself.as_ref().expect("qualified self");
    assert_eq!(qself.position, 2);
    assert!(!qself.path_span.is_null());
    assert_eq!(path.join("::"), "a::Trait::Item");
}

#[test]
fn parse_traitless_expression_qpath_keeps_associated_tail() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("<T>::Assoc::Nested").unwrap();
    let ExprKind::Name(Name { qself, path }) = expr.kind() else {
        panic!("expected traitless qualified path");
    };
    let qself = qself.as_ref().expect("qualified self");
    assert_eq!(qself.position, 0);
    assert_eq!(path.join("::"), "Assoc::Nested");
}

#[test]
fn parse_qualified_type_path_keeps_qself_and_nested_segments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = <Vec<u8> as a::Trait>::Item::Nested;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected qualified path type");
    };
    let ExprKind::Name(Name { qself, path }) = expr.kind() else {
        panic!("expected name path type");
    };
    let qself = qself.as_ref().expect("qualified self");
    assert_eq!(qself.position, 2);
    assert_eq!(path.join("::"), "a::Trait::Item::Nested");
}

#[test]
fn parse_type_relative_path_keeps_qself() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = <T>::Assoc::Nested;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected type-relative path type");
    };
    let ExprKind::Name(Name { qself, path }) = expr.kind() else {
        panic!("expected name path type");
    };
    let qself = qself.as_ref().expect("qualified self");
    assert_eq!(qself.path_span, fp_core::span::Span::null());
    assert_eq!(qself.position, 0);
    assert_eq!(path.join("::"), "Assoc::Nested");
}

#[test]
fn parse_parenthesized_path_arguments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("type Alias = Fn(u8) -> bool;")
        .unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected path type");
    };
    assert!(matches!(
        path.last().args.as_deref(),
        Some(fp_core::ast::GenericArgs::Parenthesized(
            fp_core::ast::ParenthesizedArgs {
                inputs_span,
                output: fp_core::ast::FnRetTy::Ty(_),
                ..
            }
        )) if !inputs_span.is_null()
    ));
}

#[test]
fn parse_parenthesized_path_arguments_without_return_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("type Alias = Fn(u8);").unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected path type");
    };
    let Some(fp_core::ast::GenericArgs::Parenthesized(args)) = path.last().args.as_deref()
    else {
        panic!("expected parenthesized arguments");
    };
    assert_eq!(args.inputs.len(), 1);
    assert!(matches!(args.output, fp_core::ast::FnRetTy::Default(_)));
    assert!(!args.inputs_span.is_null());
    assert_eq!(path.span.hi, args.span.hi);
    assert!(path.span.lo <= args.span.lo);
}

#[test]
fn parse_return_type_notation_as_elided_path_arguments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("type Alias = Trait(..);").unwrap();
    let ItemKind::DefType(def) = items[0].kind() else {
        panic!("expected type alias");
    };
    let Ty::Expr(expr) = &def.value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!("expected named path");
    };
    assert!(matches!(
        path.last().args.as_deref(),
        Some(fp_core::ast::GenericArgs::ParenthesizedElided(_))
    ));
}

#[test]
fn parse_byte_string_literal_as_bytes_value() {
    // `b"..."` is a byte string (`&[u8; N]` in real Rust), not a `String` —
    // it can contain non-UTF-8 bytes, so it must not be conflated with a
    // string literal.
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("b\"hello\"").unwrap();
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::Bytes(bytes_val) => assert_eq!(&bytes_val.value[..], b"hello"),
            other => panic!("expected bytes value, got {:?}", other),
        },
        other => panic!("expected value expr, got {:?}", other),
    }
}

#[test]
fn parse_expr_ast_builds_quote_ast() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("quote { 1 + 2 }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Quote(_)));
}

#[test]
fn parse_expr_ast_supports_with_context() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("with \"web-1\" { std::ops::server::shell(\"uptime\"); }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::With(_)));
}

#[test]
fn parse_items_ast_supports_context_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn run(context hosts: str) { hosts }")
        .unwrap();
    let function = items
        .into_iter()
        .find_map(|item| match item.kind() {
            ItemKind::DefFunction(function) => Some(function.clone()),
            _ => None,
        })
        .expect("function should exist");
    assert!(function.sig.params[0].is_context);
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
fn parse_match_quote_fn_splice_binds_name() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match token { quote<fn> => name, _ => \"none\" }")
        .unwrap();

    let ExprKind::Match(match_expr) = expr.kind() else {
        panic!("expected match expr, got {:?}", expr.kind());
    };
    let first_case = match_expr
        .cases
        .first()
        .expect("match should have at least one case");
    let pattern = first_case.pat.as_ref().expect("match case pattern");
    let PatternKind::Quote(quote) = pattern.kind() else {
        panic!("expected quote pattern, got {:?}", pattern.kind());
    };
    assert_eq!(quote.item, Some(QuoteItemKind::Function));
    assert_eq!(quote.fields.len(), 0);
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
        }
        other => panic!("expected quote fn item, got {:?}", other),
    }
}

#[test]
fn parse_items_ast_handles_struct_field_attrs_and_visibility() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            #[derive(Clone)]
            pub struct Cli {
                #[arg(default_value = ".")]
                pub repo: String,
                /// help text
                port: Option<u16>,
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefStruct(def) = items.first().expect("struct item").kind() else {
        panic!("expected struct item");
    };
    assert_eq!(def.value.fields.len(), 2);
    assert_eq!(def.value.fields[0].name.as_str(), "repo");
    assert_eq!(def.value.fields[1].name.as_str(), "port");
}

#[test]
fn parse_items_ast_handles_pub_struct_fields() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("pub struct GraphData { pub nodes: Vec<Node>, pub edges: Vec<Edge> }")
        .unwrap();
    let ItemKind::DefStruct(def) = items.first().expect("struct item").kind() else {
        panic!("expected struct item");
    };
    assert_eq!(def.value.fields.len(), 2);
}

#[test]
fn parse_items_ast_handles_async_move_block_statement() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn run() {
                let value = async move {
                    task().await?;
                    result()
                };
                value
            }
            "#,
        )
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_outer_attrs_on_block_statement() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn run() {
                #[cfg(feature = "x")]
                {
                    do_work();
                }
            }
            "#,
        )
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_emit_method_body_snippet() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub fn emit(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
                let _ = source_file;

                if let Some(parent) = self.config.output_path.parent() {
                    std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
                }

                self.emit_impl(&lir_program)
            }
            "#,
        )
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_emit_impl_tuple_let_snippet() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            fn emit_impl(&self, lir_program: &LirBlob) -> Result<PathBuf> {
                let out = self.config.output_path.clone();
                resolve_native_target(
                    self.config.native_target,
                    self.config.target_triple.as_deref(),
                )?;

                let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

                let plan = emit::emit_plan(lir_program, format, arch)?;
                if let Some(path) = self.config.asm_dump.as_ref() {
                    emit::dump_asm(path, &plan)?;
                }

                Ok(out)
            }
            "#,
        )
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_expr_ast_handles_tuple_let_with_try_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"
            {
                resolve_native_target(
                    self.config.native_target,
                    self.config.target_triple.as_deref(),
                )?;

                let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

                let plan = emit::emit_plan(lir_program, format, arch)?;
                if let Some(path) = self.config.asm_dump.as_ref() {
                    emit::dump_asm(path, &plan)?;
                }

                Ok(out)
            }
            "#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_tuple_let_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let (format, arch) = detect_target(); format }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_field_call_let_initializer() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let out = self.config.output_path.clone(); out }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_try_statement_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"
            {
                resolve_native_target(
                    self.config.native_target,
                    self.config.target_triple.as_deref(),
                )?;
                Ok(out)
            }
            "#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_try_then_tuple_let_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"
            {
                resolve_native_target(
                    self.config.native_target,
                    self.config.target_triple.as_deref(),
                )?;

                let (format, arch) = detect_target(self.config.target_triple.as_deref())?;
                format
            }
            "#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_items_ast_handles_native_emitter_impl_snippet() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            impl NativeEmitter {
                pub fn new(config: NativeConfig) -> Self {
                    Self { config }
                }

                /// Emit LIR into an object or executable.
                pub fn emit(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
                    let _ = source_file;

                    // Ensure output directory exists.
                    if let Some(parent) = self.config.output_path.parent() {
                        std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
                    }

                    self.emit_impl(&lir_program)
                }

                /// Back-compat for older callers.
                pub fn compile(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
                    self.emit(lir_program, source_file)
                }

                fn emit_impl(&self, lir_program: &LirBlob) -> Result<PathBuf> {
                    let out = self.config.output_path.clone();
                    resolve_native_target(
                        self.config.native_target,
                        self.config.target_triple.as_deref(),
                    )?;

                    let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

                    let plan = emit::emit_plan(lir_program, format, arch)?;
                    if let Some(path) = self.config.asm_dump.as_ref() {
                        emit::dump_asm(path, &plan)?;
                    }

                    match self.config.emit {
                        EmitKind::Object => emit::write_object(&out, &plan)?,
                        EmitKind::Executable => emit::write_executable(&out, &plan)?,
                        EmitKind::AssemblyText => {
                            return Err(fp_core::error::Error::from(
                                "fp-native does not support textual assembly emission",
                            ));
                        }
                    }
                    Ok(out)
                }
            }
            "#,
        )
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_enum_variant_field_attrs() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub enum GraphError {
                #[error("database error: {0}")]
                Db(#[from] rusqlite::Error),
                Other(String),
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefEnum(def) = items.first().expect("enum item").kind() else {
        panic!("expected enum item");
    };
    assert_eq!(def.value.variants.len(), 2);
}

#[test]
fn parse_expr_ast_handles_let_else_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let Ok(mut stream) = connect().await else { return false; }; stream }")
        .unwrap();
    let ExprKind::Block(block) = expr.kind() else {
        panic!("expected block expr");
    };
    let Some(BlockStmt::Let(stmt)) = block.stmts.first() else {
        panic!("expected let stmt");
    };
    assert!(stmt.init.is_some());
    assert!(stmt.diverge.is_some());
}

#[test]
fn parse_expr_ast_handles_nested_mut_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let Ok(mut stream) = connect() else { return false; }; stream }")
        .unwrap();
    let ExprKind::Block(block) = expr.kind() else {
        panic!("expected block expr");
    };
    let Some(BlockStmt::Let(stmt)) = block.stmts.first() else {
        panic!("expected let stmt");
    };
    assert!(stmt.diverge.is_some());
}

#[test]
fn parse_items_ast_handles_attr_literal_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            #[error("database error: {0}")]
            pub enum GraphError {
                Db(String),
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefEnum(def) = items.first().expect("enum item").kind() else {
        panic!("expected enum item");
    };
    assert_eq!(def.value.variants.len(), 1);
}

#[test]
fn parse_expr_ast_handles_block_use_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ use tokio::io::AsyncWriteExt; true }")
        .unwrap();
    let ExprKind::Block(block) = expr.kind() else {
        panic!("expected block expr");
    };
    assert!(matches!(block.stmts.first(), Some(BlockStmt::Item(_))));
}

#[test]
fn parse_expr_ast_handles_char_literal() {
    // `'\n'` is a `char` literal, not a single-character `String`.
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("'\\n'").unwrap();
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::Char(char_val) => assert_eq!(char_val.value, '\n'),
            other => panic!("expected char literal value, got {:?}", other),
        },
        other => panic!("expected literal expr, got {:?}", other),
    }
}

#[test]
fn parse_expr_ast_handles_reference_pattern_in_for_loop() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ for &b in data { b } }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_tuple_pattern_closure_param() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ values.map(|(id, name)| { id; name }) }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_items_ast_handles_impl_trait_return_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn f() -> impl std::future::Future<Output = Result<T, E>> + Send { x }")
        .unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_expr_ast_handles_struct_update_syntax() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ PrettyOptions { show_spans: false, show_types: false, ..PrettyOptions::default() } }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
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
fn parse_items_ast_supports_visible_struct_fields() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub struct ProjectConfig {
                pub root: PathBuf,
                pub kind: ProjectKind,
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct item");
    };
    assert_eq!(def.value.fields.len(), 2);
    assert_eq!(def.value.fields[0].name.as_str(), "root");
    assert_eq!(def.value.fields[1].name.as_str(), "kind");
}

#[test]
fn parse_items_ast_supports_reference_lifetimes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub struct Indexer<'a> {
                db: &'a DataFlowDb,
                project_scope: String,
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct item");
    };
    let Ty::Reference(reference) = &def.value.fields[0].value else {
        panic!("expected reference type");
    };
    assert_eq!(reference.lifetime.as_ref().map(Ident::as_str), Some("'a"));
}

#[test]
fn parse_items_ast_supports_static_reference_return_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("pub(crate) fn as_str(self) -> &'static str { value }")
        .unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function item");
    };
    let Some(Ty::Reference(reference)) = function.sig.ret_ty.as_ref() else {
        panic!("expected reference return type");
    };
    assert_eq!(
        reference.lifetime.as_ref().map(Ident::as_str),
        Some("'static")
    );
}

#[test]
fn parse_items_ast_supports_dyn_trait_object_type_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("pub struct FrontendHolder { inner: Option<Box<dyn LanguageFrontend>>, }")
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct item");
    };
    let Ty::Expr(expr) = &def.value.fields[0].value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path: path, .. }) = expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        path.segments[0].args.as_deref()
    else {
        panic!("expected Option type arg");
    };
    let Some(fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(box_arg))) =
        args.args.first()
    else {
        panic!("expected Option type arg");
    };
    let Ty::Expr(box_expr) = box_arg.as_ref() else {
        panic!("expected Box path type");
    };
    let ExprKind::Name(Name { path: box_path, .. }) = box_expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(box_args)) =
        box_path.segments[0].args.as_deref()
    else {
        panic!("expected dyn trait bounds");
    };
    let Some(fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(box_arg))) =
        box_args.args.first()
    else {
        panic!("expected dyn trait bounds");
    };
    let Ty::TypeBounds(bounds) = box_arg.as_ref() else {
        panic!("expected dyn trait bounds");
    };
    assert_eq!(bounds.bounds.len(), 1);
}

#[test]
fn parse_items_ast_supports_dyn_trait_object_with_multiple_bounds() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "pub struct ErrorHolder { inner: Box<dyn std::error::Error + Send + Sync>, }",
        )
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct item");
    };
    let Ty::Expr(expr) = &def.value.fields[0].value else {
        panic!("expected path type");
    };
    let ExprKind::Name(Name { path: path, .. }) = expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) =
        path.segments[0].args.as_deref()
    else {
        panic!("expected dyn trait bounds");
    };
    let Some(fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(arg))) =
        args.args.first()
    else {
        panic!("expected dyn trait bounds");
    };
    let Ty::TypeBounds(bounds) = arg.as_ref() else {
        panic!("expected dyn trait bounds");
    };
    assert_eq!(bounds.bounds.len(), 3);
}

#[test]
fn parse_items_ast_supports_struct_field_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub struct GraphNodeData {
                #[serde(rename = "edgeCount")]
                pub edge_count: i64,
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefStruct(def) = items[0].kind() else {
        panic!("expected struct item");
    };
    assert_eq!(def.value.fields.len(), 1);
    assert_eq!(def.value.fields[0].name.as_str(), "edge_count");
}

#[test]
fn parse_items_ast_supports_destructured_function_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "async fn graph_data(State(state): State<Arc<GraphState>>) -> Result<Json<GraphData>, HttpStatus> { state }",
        )
        .unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function item");
    };
    assert_eq!(function.sig.params.len(), 1);
    assert_eq!(function.sig.params[0].name.as_str(), "state");
}

#[test]
fn parse_expr_ast_supports_if_let_condition() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("if let Some(cfg) = Self::try_detect(root) { cfg } else { other }")
        .unwrap();
    let ExprKind::Match(match_expr) = expr.kind() else {
        panic!("expected match expr");
    };
    assert!(match_expr.cases.len() >= 2);
}

#[test]
fn parse_items_ast_supports_enum_variant_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
            pub enum GraphError {
                #[error("database error: {0}")]
                Db(#[from] rusqlite::Error),
            }
            "#,
        )
        .unwrap();
    let ItemKind::DefEnum(def) = items[0].kind() else {
        panic!("expected enum item");
    };
    assert_eq!(def.value.variants.len(), 1);
    assert_eq!(def.value.variants[0].name.as_str(), "Db");
}

#[test]
fn parse_expr_ast_supports_literal_match_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match ext { \"rs\" => rust, _ => other }")
        .unwrap();
    let ExprKind::Match(match_expr) = expr.kind() else {
        panic!("expected match expr");
    };
    assert_eq!(match_expr.cases.len(), 2);
}

#[test]
fn parse_expr_ast_supports_destructured_closure_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|Json(body)| body.path").unwrap();
    let ExprKind::Closure(closure) = expr.kind() else {
        panic!("expected closure expr");
    };
    assert_eq!(closure.params.len(), 1);
    assert_eq!(
        closure.params[0].as_ident().map(Ident::as_str),
        Some("body")
    );
}

#[test]
fn parse_expr_ast_supports_while_let_slice_bind_rest_pattern() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("while let [first, second, tail @ ..] = rest { first }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Loop(_)));
}

#[test]
fn parse_expr_ast_supports_match_or_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match ext { \"rs\" | \"rust\" => rust, _ => other }")
        .unwrap();
    let ExprKind::Match(match_expr) = expr.kind() else {
        panic!("expected match expr");
    };
    // `"rs" | "rust" => rust` desugars into one case per alternative
    // (both sharing the `rust` body), plus the `_ => other` case: 3 total.
    // Previously the parser silently discarded all but the first
    // alternative, making `"rust"` unreachable dead code in every backend.
    assert_eq!(match_expr.cases.len(), 3);
    let patterns: Vec<_> = match_expr
        .cases
        .iter()
        .map(|case| format!("{:?}", case.pat))
        .collect();
    assert!(patterns[0].contains("rs"));
    assert!(patterns[1].contains("rust"));
}

#[test]
fn parse_expr_ast_expands_nested_or_pattern_cartesian_product() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match pair { (Some(1) | Some(2), y) => y, (None, y) => y, _ => 0 }")
        .unwrap();
    let ExprKind::Match(match_expr) = expr.kind() else {
        panic!("expected match expr");
    };
    // `(Some(1) | Some(2), y)` is a nested or-pattern (the `|` sits
    // inside a tuple element, not at the arm's top level) — it must
    // expand into the cartesian product of `Or`-free cases: `(Some(1), y)`
    // and `(Some(2), y)`, plus the two other arms: 4 total. Before this
    // fix, `parse_pattern_alternatives` silently discarded every
    // alternative but the first, making `Some(2)` unreachable dead code.
    assert_eq!(match_expr.cases.len(), 4);
    let patterns: Vec<_> = match_expr
        .cases
        .iter()
        .map(|case| format!("{:?}", case.pat))
        .collect();
    assert!(!patterns[0].contains("PatternKind::Or") && !patterns[0].contains("\"Or\""));
    assert!(patterns[0].contains('1'));
    assert!(patterns[1].contains('2'));
}

#[test]
fn parse_items_ast_supports_impl_trait_bounds_in_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "fn run<T>(f: impl FnOnce(&DataFlowDb) -> Result<T, GraphError> + Send + 'static) -> bool { true }",
        )
        .unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function item");
    };
    assert!(matches!(function.sig.params[0].ty, Ty::ImplTraits(_)));
}

#[test]
fn parse_expr_ast_rejects_dotted_macro_path_as_module_path() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    assert!(parser.parse_expr_ast("self.foo!()").is_err());
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
fn parse_expr_ast_handles_if_with_comparison_and_block_branches() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("if a > b { a } else { b }").unwrap();
    assert!(matches!(expr.kind(), ExprKind::If(_)));
}

#[test]
fn parse_expr_ast_handles_if_condition_with_casts_and_or_chain() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "if start < 0 || end < 0 || end as usize >= STACK_SIZE || start as usize >= STACK_SIZE { return Err(err); }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::If(_)));
}

#[test]
fn parse_expr_ast_handles_for_iter_before_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("for stmt in statements { out.push(stmt); }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::For(_)));
}

#[test]
fn parse_expr_ast_handles_while_condition_before_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("while cursor < end { cursor += 1; }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::While(_)));
}

#[test]
fn parse_expr_ast_handles_shift_assignment_in_loop() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("loop { value >>= 7; if value == 0 { break; } }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Loop(_)));
}

#[test]
fn parse_expr_ast_handles_labeled_for_loop() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("'search: for item in items { if done { break 'search; } }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::For(_)));
}

#[test]
fn parse_expr_ast_handles_with_identifier_context() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("with host { std::ops::server::shell(\"uptime\"); }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::With(_)));
}

#[test]
fn parse_expr_ast_handles_slice_pattern_with_rest_prefix() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match target.as_slice() { [.., owner, method] if owner == \"HashMap\" && method == \"from\" => {}, _ => {} }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_while_let_with_char_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"while let Some(ch) = chars.next() {
                if let Some(active) = quote {
                    if ch == active {
                        quote = None;
                    } else if ch == '\\' {
                        if let Some(next) = chars.next() {
                            current.push(next);
                        }
                    } else {
                        current.push(ch);
                    }
                    continue;
                }

                match ch {
                    '"' | '\'' => quote = Some(ch),
                    '\\' => {
                        if let Some(next) = chars.next() {
                            current.push(next);
                        }
                    }
                    ch if ch.is_ascii_whitespace() => {
                        if !current.is_empty() {
                            args.push(std::mem::take(&mut current));
                        }
                    }
                    _ => current.push(ch),
                }
            }"#;
    let expr = parser.parse_expr_ast(source).unwrap_or_else(|err| {
        panic!(
            "{err:?}\ndiagnostics: {:?}",
            parser.diagnostics().get_diagnostics()
        )
    });
    assert!(matches!(
        expr.kind(),
        ExprKind::Loop(_) | ExprKind::While(_)
    ));
}

#[test]
fn parse_expr_ast_handles_if_let_with_else_if_and_continue() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"if let Some(active) = quote {
                if ch == active {
                    quote = None;
                } else if ch == '\\' {
                    if let Some(next) = chars.next() {
                        current.push(next);
                    }
                } else {
                    current.push(ch);
                }
                continue;
            }"#;
    let expr = parser.parse_expr_ast(source).unwrap_or_else(|err| {
        panic!(
            "{err:?}\ndiagnostics: {:?}",
            parser.diagnostics().get_diagnostics()
        )
    });
    assert!(matches!(expr.kind(), ExprKind::Match(_) | ExprKind::If(_)));
}

#[test]
fn parse_expr_ast_handles_if_else_if_chain_with_nested_if_let() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"if ch == active {
                quote = None;
            } else if ch == '\\' {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
            } else {
                current.push(ch);
            }"#;
    let expr = parser.parse_expr_ast(source).unwrap_or_else(|err| {
        panic!(
            "{err:?}\ndiagnostics: {:?}",
            parser.diagnostics().get_diagnostics()
        )
    });
    assert!(matches!(expr.kind(), ExprKind::If(_)));
}

#[test]
fn parse_expr_ast_handles_if_let_with_continue_only() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"if let Some(active) = quote {
                continue;
            }"#;
    let expr = parser.parse_expr_ast(source).unwrap_or_else(|err| {
        panic!(
            "{err:?}\ndiagnostics: {:?}",
            parser.diagnostics().get_diagnostics()
        )
    });
    assert!(matches!(expr.kind(), ExprKind::Match(_) | ExprKind::If(_)));
}

#[test]
fn parse_items_ast_handles_command_attribute_parser_function() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"fn parse_command_attribute(input: &str) -> std::result::Result<Vec<String>, String> {
                let mut args = Vec::new();
                let mut current = String::new();
                let mut chars = input.chars();
                let mut quote: Option<char> = None;

                while let Some(ch) = chars.next() {
                    if let Some(active) = quote {
                        if ch == active {
                            quote = None;
                        } else if ch == '\\' {
                            if let Some(next) = chars.next() {
                                current.push(next);
                            }
                        } else {
                            current.push(ch);
                        }
                        continue;
                    }

                    match ch {
                        '"' | '\'' => quote = Some(ch),
                        '\\' => {
                            if let Some(next) = chars.next() {
                                current.push(next);
                            }
                        }
                        ch if ch.is_ascii_whitespace() => {
                            if !current.is_empty() {
                                args.push(std::mem::take(&mut current));
                            }
                        }
                        _ => current.push(ch),
                    }
                }

                if quote.is_some() {
                    return Err("unterminated quoted segment".to_string());
                }
                if !current.is_empty() {
                    args.push(current);
                }
                Ok(args)
            }"#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_command_attribute_signature_only() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"fn parse_command_attribute(input: &str) -> std::result::Result<Vec<String>, String> {
                Ok(Vec::new())
            }"#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_command_attribute_body_only() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let source = r#"fn parse_command_attribute(input: &str) -> Result<(), String> {
                let mut args = Vec::new();
                let mut current = String::new();
                let mut chars = input.chars();
                let mut quote: Option<char> = None;

                while let Some(ch) = chars.next() {
                    if let Some(active) = quote {
                        if ch == active {
                            quote = None;
                        } else if ch == '\\' {
                            if let Some(next) = chars.next() {
                                current.push(next);
                            }
                        } else {
                            current.push(ch);
                        }
                        continue;
                    }

                    match ch {
                        '"' | '\'' => quote = Some(ch),
                        '\\' => {
                            if let Some(next) = chars.next() {
                                current.push(next);
                            }
                        }
                        ch if ch.is_ascii_whitespace() => {
                            if !current.is_empty() {
                                args.push(std::mem::take(&mut current));
                            }
                        }
                        _ => current.push(ch),
                    }
                }

                if quote.is_some() {
                    return Err("unterminated quoted segment".to_string());
                }
                if !current.is_empty() {
                    args.push(current);
                }
                Ok(())
            }"#;
    let items = parser.parse_items_ast(source).unwrap_or_else(|err| {
        panic!(
            "{err:?}\ndiagnostics: {:?}",
            parser.diagnostics().get_diagnostics()
        )
    });
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_expr_ast_handles_struct_literal_fields_with_cfg_attrs() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"Self {
                #[cfg(feature = "llvm")]
                strategy: Box::new(LlvmRuntimeIntrinsicMaterializer),
                #[cfg(not(feature = "llvm"))]
                strategy: Box::new(NoopIntrinsicMaterializer),
            }"#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Struct(_)));
}

#[test]
fn parse_items_ast_handles_never_return_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"fn fail_with_error(&self, stage: &str, err: CliError) -> ! {
                panic!("{} must succeed: {:?}", stage, err);
            }"#,
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_expr_ast_handles_match_arms_with_cfg_attrs() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"match ext.as_str() {
                #[cfg(feature = "lang-typescript")]
                "ts" | "tsx" | "mts" | "cts" => Some(LanguageSource::TypeScript),
                "rs" => Some(LanguageSource::Rust),
                _ => None,
            }"#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_array_elements_with_cfg_attrs() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"[
                Language {
                    name: RUST,
                    extensions: &["rs"],
                    ast_target_supported: true,
                },
                #[cfg(feature = "lang-typescript")]
                Language {
                    name: TYPESCRIPT,
                    extensions: &["ts", "tsx"],
                    ast_target_supported: true,
                },
            ]"#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Array(_)));
}

#[test]
fn parse_items_ast_handles_unsafe_impl() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"unsafe impl Send for TsPrinter {}
               unsafe impl Sync for TsPrinter {}"#,
        )
        .unwrap();
    assert_eq!(items.len(), 2);
    assert!(
        items
            .iter()
            .all(|item| matches!(item.kind(), ItemKind::Impl(_)))
    );
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
fn parse_expr_ast_handles_match_tuple_and_range_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match pair { (Mode::Ssh { host }, Backend::Tmux) => host, _ => fallback }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));

    let expr = parser
        .parse_expr_ast("match b { b'A'..=b'Z' | b'a'..=b'z' => 1, _ => 0 }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_range() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("1..=2").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Range(_)));

    let expr = parser.parse_expr_ast("buf[..n]").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Index(_)));
}

#[test]
fn parse_expr_ast_handles_calls_fields_and_assignments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("a.b(c)[0] = 1").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Assign(_)));
}

#[test]
fn parse_expr_ast_handles_keyword_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("foo(bar=1, baz=2)").unwrap();
    match expr.kind() {
        ExprKind::Invoke(invoke) => {
            assert!(invoke.args.is_empty());
            assert_eq!(invoke.kwargs.len(), 2);
            assert_eq!(invoke.kwargs[0].name, "bar");
            assert_eq!(invoke.kwargs[1].name, "baz");
        }
        other => panic!("expected invoke, got {:?}", other),
    }
}

#[test]
fn parse_expr_ast_rejects_positional_after_keyword_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    assert!(parser.parse_expr_ast("foo(bar=1, 2)").is_err());
}

#[test]
fn parse_expr_ast_handles_closure() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|x| x + 1").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_move_closure() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("move || 1").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_tuple_field_access() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("self.0").unwrap();
    assert!(matches!(expr.kind(), ExprKind::FieldAccess(_)));
}

#[test]
fn parse_expr_ast_bare_self_is_a_plain_identifier() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("self").unwrap();
    let ExprKind::Name(Name { path, .. }) = expr.kind() else {
        panic!(
            "expected bare `self` to parse as a one-segment path, got {:?}",
            expr.kind()
        );
    };
    assert_eq!(path.last().as_str(), "self");
}

#[test]
fn parse_expr_ast_self_field_access_targets_self_ident() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("self.x").unwrap();
    let ExprKind::FieldAccess(select) = expr.kind() else {
        panic!("expected field access, got {:?}", expr.kind());
    };
    let ExprKind::Name(Name { path, .. }) = select.obj.kind() else {
        panic!(
            "expected receiver to be Name::ident(\"self\"), got {:?}",
            select.obj.kind()
        );
    };
    assert_eq!(path.last().as_str(), "self");
}

#[test]
fn parse_expr_ast_handles_raw_ref_identifier_binding() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let r#ref = value; &r#ref }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_function_accepts_raw_ref_parameter_name() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn read(r#ref: &str) { let _ = r#ref; }")
        .unwrap();
    assert_eq!(items.len(), 1);
}

#[test]
fn parse_expr_ast_supports_turbofish_method_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("ap.arg::<u64>()").unwrap();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation, got {:?}", expr.kind());
    };
    let ExprInvokeTarget::Method(select) = &invoke.target else {
        panic!("expected method invocation, got {:?}", invoke.target);
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) = &select.generic_args else {
        panic!("expected angle-bracketed method arguments");
    };
    assert_eq!(args.args.len(), 1);
}

#[test]
fn parse_expr_ast_preserves_structured_method_arguments() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("ap.arg::<'a, 3, _>()").unwrap();
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation, got {:?}", expr.kind());
    };
    let ExprInvokeTarget::Method(select) = &invoke.target else {
        panic!("expected method invocation, got {:?}", invoke.target);
    };
    let Some(fp_core::ast::GenericArgs::AngleBracketed(args)) = &select.generic_args else {
        panic!("expected angle-bracketed method arguments");
    };
    assert!(matches!(
        args.args.as_slice(),
        [
            fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Lifetime(lifetime)),
            fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Const(_)),
            fp_core::ast::AngleBracketedArg::Arg(fp_core::ast::GenericArg::Type(infer)),
        ] if lifetime == "'a" && matches!(infer.as_ref(), fp_core::ast::Ty::Wildcard(_))
    ));
}

#[test]
fn parse_expr_ast_handles_typed_and_mut_closure_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|mut x: i32| x").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_ref_str_closure_params() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("|s: &str| s.len()").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Closure(_)));
}

#[test]
fn parse_expr_ast_handles_call_with_typed_closure_arg() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("foo(|s: &str| s.len() >= 7 && s.len() <= 40)")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_method_chain_in_closure_body() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("self.inner.get(key).map(|entry| entry.value().clone())")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_self_new_internal_with_turbofish_arg() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("Some(Self::new_internal(true, Vec::<String>::new()))")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_return_if_expression() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ return if self.absolute { Some(Self::new_internal(true, Vec::<String>::new())) } else { None }; }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
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
fn parse_block_ast_handles_defer_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ defer close(handle); 1 }").unwrap();
    let ExprKind::Block(block) = expr.kind() else {
        panic!("expected block expr, got {:?}", expr.kind());
    };
    assert!(matches!(block.stmts.first(), Some(BlockStmt::Defer(_))));
}

#[test]
fn parse_expr_ast_handles_structured_try() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("try { run() } catch err { recover(err) } else { ok() } finally { done() }")
        .unwrap();
    let ExprKind::Try(expr_try) = expr.kind() else {
        panic!("expected try expr, got {:?}", expr.kind());
    };
    assert_eq!(expr_try.catches.len(), 1);
    assert!(expr_try.elze.is_some());
    assert!(expr_try.finally.is_some());
}

#[test]
fn parse_items_ast_handles_opaque_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("opaque type Session;").unwrap();
    match items.first().map(|item| item.kind()) {
        Some(ItemKind::OpaqueType(item)) => assert_eq!(item.name.as_str(), "Session"),
        other => panic!("expected opaque type item, got {:?}", other),
    }
}

#[test]
fn parse_items_ast_handles_unit_and_tuple_structs() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("struct FrontendAssets; struct TempDir(std::path::PathBuf, i32);")
        .unwrap();

    let ItemKind::DefStruct(unit) = items[0].kind() else {
        panic!("expected unit struct");
    };
    assert!(unit.value.fields.is_empty());

    let ItemKind::DefStruct(tuple) = items[1].kind() else {
        panic!("expected tuple struct");
    };
    assert_eq!(tuple.value.fields.len(), 2);
    assert_eq!(tuple.value.fields[0].name.as_str(), "0");
    assert_eq!(tuple.value.fields[1].name.as_str(), "1");
}

#[test]
fn parse_expr_ast_handles_local_item_with_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ #[derive(Clone)] struct TempDir(std::path::PathBuf); TempDir(path) }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_unsafe_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("unsafe { libc::openpty(a, b, c, d, e) }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_unsafe_block_with_pre_exec_closure() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "unsafe { command.pre_exec(|| { if libc::setsid() == -1 { return Err(std::io::Error::last_os_error()); } if libc::ioctl(libc::STDIN_FILENO, libc::TIOCSCTTY.into(), 0) == -1 { return Err(std::io::Error::last_os_error()); } Ok(()) }); }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
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
fn parse_block_ast_handles_let_const_block_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let optimized_size = const { BUFFER_SIZE * 2 }; optimized_size }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_block_ast_handles_multiline_let_const_block_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ let cache_strategy = const { if BUFFER_SIZE > 2048 { \"large\" } else { \"small\" } }; cache_strategy }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_block_ast_handles_local_const_and_struct_items() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ const BUFFER_SIZE: i64 = 1024 * 4; struct Config { buffer_size: i64, max_connections: i64, } let optimized_size = const { BUFFER_SIZE * 2 }; optimized_size }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_const_block_with_for_splice_quote_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "const { for (i, x) in xs.iter().enumerate() { splice ( quote { if x > ys[i] { return x; } } ); } }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parse_items_ast_handles_quote_splice_example_function() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            const {
                for (i, op) in ops.iter().enumerate() {
                    if op % 2 == 0 {
                        splice(quote<expr> { x = x + op; });
                    } else {
                        emit! { x = x + op; }
                    }
                    splice(quote<expr> { println!("step {}: {}", i, x); });
                    splice(quote<expr> { if x >= limit { return x; } });
                }
            }
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_expr_ast_handles_const_block_emit_macro_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("const { emit! { x = x + op; } }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parse_expr_ast_handles_const_block_splice_quote_assign_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("const { splice(quote<expr> { x = x + op; }); }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parse_expr_ast_handles_const_block_splice_quote_if_stmt() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("const { splice(quote<expr> { if x >= limit { return x; } }); }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::ConstBlock(_)));
}

#[test]
fn parse_items_ast_handles_quote_splice_example_function_min_body() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            const {
                for (i, op) in ops.iter().enumerate() {
                    splice(quote<expr> { x = x + op; });
                }
            }
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_quote_splice_example_function_with_emit_else() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            const {
                for (i, op) in ops.iter().enumerate() {
                    if op % 2 == 0 {
                        splice(quote<expr> { x = x + op; });
                    } else {
                        emit! { x = x + op; }
                    }
                }
            }
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_quote_splice_example_function_with_println_splice() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            const {
                for (i, op) in ops.iter().enumerate() {
                    if op % 2 == 0 {
                        splice(quote<expr> { x = x + op; });
                    } else {
                        emit! { x = x + op; }
                    }
                    splice(quote<expr> { println!("step {}: {}", i, x); });
                }
            }
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_apply_ops_signature_with_simple_body() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_apply_ops_signature_with_const_then_tail() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
            const {}
            x
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_bench_quote_item_function() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        const fn bench(item: quote<item>) -> quote<item> {
            let name = item.name;
            REGISTRY.push(BenchCase { name, run: item.value });
            item
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_type_alias_const_block_expr() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        type Config = const {
            TypeBuilder::new("Config")
                .with_field("id", i64)
                .build()
        };
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_type_alias_rhs_forms() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            r#"
                type IntLiteral = 1;
                type StringLiteral = "xxx";
                type Direct = Bar;
                type Structural = struct { a: i32 };
                type Computed = comptime_fn(1);
                type Generic = Vec<i32>;
                type ExplicitConst = const { comptime_fn(1) };
            "#,
        )
        .expect("type alias RHS forms parse");

    let aliases = items
        .iter()
        .map(|item| match item.kind() {
            ItemKind::DefType(alias) => alias,
            kind => panic!("expected type alias, found {kind:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(aliases.len(), 7);
    assert!(matches!(aliases[0].value, Ty::Expr(_)));
    assert!(matches!(aliases[1].value, Ty::Literal(_)));
    assert!(matches!(aliases[2].value, Ty::Expr(_)));
    assert!(matches!(aliases[3].value, Ty::Structural(_)));
    let Ty::Expr(computed_expr) = &aliases[4].value else {
        panic!("expected parenthesized path for computed alias");
    };
    let ExprKind::Name(Name { path, .. }) = computed_expr.kind() else {
        panic!("expected parenthesized path for computed alias");
    };
    assert!(matches!(
        path.last().args.as_deref(),
        Some(fp_core::ast::GenericArgs::Parenthesized(
            fp_core::ast::ParenthesizedArgs {
                output: fp_core::ast::FnRetTy::Default(_),
                ..
            }
        ))
    ));
    assert!(matches!(aliases[5].value, Ty::Expr(_)));
    assert!(matches!(aliases[6].value, Ty::ConstBlock(_)));
}

#[test]
fn parse_items_ast_handles_type_alias_macro_expr() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        type Foo = t! {
            struct {
                a: i64,
            }
        };
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_quote_fn_with_if_generated_items() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        quote fn build_items_2(flag: bool) -> item {
            if flag {
                struct Alpha {
                    id: i64
                }
            } else {
                struct Beta {
                    id: i64
                }
            }
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_local_type_alias_const_block_expr() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn main() {
            type Base = const {
                TypeBuilder::new("Base")
                    .with_field("id", i64)
                    .build()
            };
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_bare_quote_fragment_types() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        const SCORE_EXPR: expr = quote<expr> { (2 + 3) * 4 };
        const FN_GROUP: [item] = quote<[item]> {
            fn alpha() {}
            fn beta(x: i64) -> i64 { x + 1 }
        };
        const STEP_STMT: stmt = quote<stmt> {
            let step = 7 * 3;
        };
        const BANNER_ITEM: item = quote<item> {
            struct Banner {
                title: &'static str,
                rank: i64,
            }
        };
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_top_level_splice_statements() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        const fn build_items(flag: bool) -> item {
            quote<item> {
                if flag {
                    struct Alpha {
                        id: i64
                    }
                } else {
                    struct Beta {
                        id: i64
                    }
                }
            }
        }
        splice build_items(true);
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_negative_trait_bound() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        use std::fmt::Display;
        fn print_display<T: Display + !Clone>(value: T) {}
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_clone_struct_macro_in_const_type_alias() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn main() {
            type ConfigClone = const { clone_struct!(Config) };
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_items_ast_handles_type_metadata_contains_calls() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        fn main() {
            println!("{}", type(Config).fields.contains("mode"));
        }
    "#;
    let items = parser.parse_items_ast(src).unwrap();
    assert!(!items.is_empty());
}

#[test]
fn parse_expr_ast_handles_type_value_call_arg_static_str_ref() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(r#"TypeBuilder::new("Base").with_field("name", &'static str).build()"#)
        .unwrap();
    assert!(matches!(
        expr.kind(),
        ExprKind::Invoke(_) | ExprKind::FieldAccess(_)
    ));
}

#[test]
fn parse_expr_ast_handles_struct_update_before_explicit_fields() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("FooPlusBar { ..base_foo, bar: 6 }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Struct(_)));
}

#[test]
fn parse_expr_ast_handles_bench_run_body_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let benches: Vec<BenchCase> = REGISTRY;
            let mut passed = 0;
            let mut failed = 0;
            let mut idx = 0;
            while idx < benches.len() {
                let bench: BenchCase = benches[idx];
                let mut ok = true;
                let warmup_secs = 5.0f64;
                let measure_secs = 15.0f64;

                let warmup_start = std::time::now();
                let warmup_deadline = warmup_start + warmup_secs;
                let mut warmup_iters = 0;
                while std::time::now() < warmup_deadline {
                    let warm_ok = catch_unwind(bench.run);
                    if !warm_ok {
                        ok = false;
                        break;
                    }
                    warmup_iters = warmup_iters + 1;
                }

                let measure_start = std::time::now();
                let measure_deadline = measure_start + measure_secs;
                let mut measure_iters = 0;
                if ok {
                    while std::time::now() < measure_deadline || measure_iters == 0 {
                        let run_ok = catch_unwind(bench.run);
                        if !run_ok {
                            ok = false;
                            break;
                        }
                        measure_iters = measure_iters + 1;
                    }
                }
                let measure_end = std::time::now();
                let elapsed = measure_end - measure_start;
                if ok {
                    passed = passed + 1;
                    let iters_f = measure_iters as f64;
                    let ns_per_iter = if iters_f > 0.0 {
                        (elapsed / iters_f) * 1000000000.0
                    } else {
                        0.0
                    };
                    println(
                        "  {} ... ok (iters: {}, time: {:.6}s, ns/iter: {:.2})",
                        bench.name,
                        measure_iters,
                        elapsed,
                        ns_per_iter
                    );
                } else {
                    failed = failed + 1;
                    println("  {} ... FAILED", bench.name);
                }
                idx = idx + 1;
            }
            let total = passed + failed;
            println(
                "bench result: {} passed; {} failed; {} total",
                passed,
                failed,
                total
            );
            BenchReport {
                total,
                passed,
                failed,
            }
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}
