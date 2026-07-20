use super::*;
use fp_core::ast::{
    AttrMeta, AttrStyle, BlockStmt, ExprKind, ItemKind, MacroDelimiter, Name, PatternKind,
    QuoteItemKind, Value,
};
use fp_core::ast::{QuoteFragmentKind, Ty};
use fp_core::module::path::PathPrefix;
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
fn direct_parser_accepts_emit_macro_source() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn main() { emit! { let generated = 42; generated } }\n";
    let items = parser.parse_items_ast(src).unwrap();
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
        ExprKind::Name(Name::ParameterPath(path)) => {
            assert_eq!(path.prefix, PathPrefix::Super(2));
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].ident.as_str(), "Foo");
            assert_eq!(path.segments[0].args.len(), 1);
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
        panic!("expected parameterized locator, got {:?}", def.value);
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        panic!("expected parameterized locator expr, got {:?}", expr);
    };
    assert_eq!(path.prefix, PathPrefix::Crate);
    assert_eq!(
        path.segments
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["module", "Foo"]
    );
    assert_eq!(path.segments[1].args.len(), 1);
}

#[test]
fn parse_byte_string_literal_as_string_value() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("b\"hello\"").unwrap();
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(str_val) => assert_eq!(str_val.value, "hello"),
            other => panic!("expected string value, got {:?}", other),
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
            match func.body.kind() {
                ExprKind::Block(_) => {}
                other => panic!("expected block body, got {:?}", other),
            }
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
        .parse_expr_ast(
            "{ let Ok(mut stream) = connect().await else { return false; }; stream }",
        )
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
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("'\\n'").unwrap();
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(text) => assert_eq!(text.value, "\n"),
            other => panic!("expected string literal value, got {:?}", other),
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
        .parse_items_ast(
            "fn f() -> impl std::future::Future<Output = Result<T, E>> + Send { x }",
        )
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
    assert_eq!(
        reference
            .lifetime
            .as_ref()
            .map(Ident::as_str),
        Some("'a")
    );
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
        reference
            .lifetime
            .as_ref()
            .map(Ident::as_str),
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
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(box_arg) = path.segments[0].args.first() else {
        panic!("expected Option type arg");
    };
    let Ty::Expr(box_expr) = box_arg else {
        panic!("expected Box path type");
    };
    let ExprKind::Name(Name::ParameterPath(box_path)) = box_expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(Ty::TypeBounds(bounds)) = box_path.segments[0].args.first() else {
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
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        panic!("expected parameter path type");
    };
    let Some(Ty::TypeBounds(bounds)) = path.segments[0].args.first() else {
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
    assert_eq!(closure.params[0].as_ident().map(Ident::as_str), Some("body"));
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
    assert_eq!(match_expr.cases.len(), 2);
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
    assert!(matches!(expr.kind(), ExprKind::Select(_)));
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
fn parse_expr_ast_supports_turbofish_method_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    parser.parse_expr_ast("ap.arg::<u64>()").unwrap();
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

#[test]
fn parse_expr_ast_handles_bench_report_struct_literal_shorthand() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("BenchReport { total, passed, failed, }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Struct(_)));
}

#[test]
fn parse_expr_ast_handles_multiline_println_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            r#"println(
                "  {} ... ok (iters: {}, time: {:.6}s, ns/iter: {:.2})",
                bench.name,
                measure_iters,
                elapsed,
                ns_per_iter
            )"#,
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_field_arg_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("catch_unwind(bench.run)").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
}

#[test]
fn parse_expr_ast_handles_bench_run_body_prefix_block() {
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
                idx = idx + 1;
            }
            passed
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_run_body_suffix_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
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

#[test]
fn parse_expr_ast_handles_bench_outer_while_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
            let benches: Vec<BenchCase> = REGISTRY;
            let mut idx = 0;
            while idx < benches.len() {
                let bench: BenchCase = benches[idx];
                let mut ok = true;
                let warmup_secs = 5.0f64;
                let measure_secs = 15.0f64;
                idx = idx + 1;
            }
            idx
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_warmup_loop_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
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
            warmup_iters
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_bench_measure_loop_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = r#"
        {
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
            measure_iters
        }
    "#;
    let expr = parser.parse_expr_ast(src).unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_typed_generic_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let benches: Vec<BenchCase> = REGISTRY; benches }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_typed_index_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ let bench: BenchCase = benches[idx]; bench }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_outer_while_minimal_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast(
            "{ let benches: Vec<BenchCase> = REGISTRY; let mut idx = 0; while idx < benches.len() { idx = idx + 1; } idx }",
        )
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_len_call_in_comparison() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("idx < benches.len()").unwrap();
    assert!(matches!(expr.kind(), ExprKind::BinOp(_)));
}

#[test]
fn parse_expr_ast_handles_nonfinal_while_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("{ while idx < benches.len() { idx = idx + 1; } idx }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Block(_)));
}

#[test]
fn parse_expr_ast_handles_match_with_path_tuple_patterns() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser
        .parse_expr_ast("match self { Result::Ok(_) => true, Result::Err(_) => false }")
        .unwrap();
    assert!(matches!(expr.kind(), ExprKind::Match(_)));
}

#[test]
fn parse_expr_ast_handles_wildcard_let_stmt_in_block() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("{ let _ = self; _ }");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_unit_literal() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("()");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_result_ok_unit_call() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("std::result::Result::Ok(())");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_reference_prefix() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("&self.inner");
    assert!(expr.is_ok(), "{:?}", expr.err());
}

#[test]
fn parse_expr_ast_handles_array_literal_call_args() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("first_gt([1, 2, 5], [0, 1, 3])");
    assert!(expr.is_ok(), "{:?}", expr.err());
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
fn parse_items_ast_handles_enum_struct_variants() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("enum E { Named { path: String, code: i64 }, Unit }")
        .unwrap();
    let ItemKind::DefEnum(item) = items[0].kind() else {
        panic!("expected enum item");
    };
    let named = &item.value.variants[0];
    assert!(matches!(named.value, Ty::Structural(_)));
}

#[test]
fn parse_items_ast_handles_module_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("mod foo {}").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Module(_)));
}

#[test]
fn parse_items_ast_supports_inner_doc_include_str() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("mod m { #![doc = include_str!(\"foo\")] }")
        .unwrap();
    let ItemKind::Module(module) = items[0].kind() else {
        panic!("expected module item");
    };
    let attr = module
        .attrs
        .iter()
        .find(|attr| matches!(attr.style, AttrStyle::Inner))
        .expect("expected inner attribute");
    let AttrMeta::NameValue(meta) = &attr.meta else {
        panic!("expected name-value attribute");
    };
    assert_eq!(meta.name.last().as_str(), "doc");
    assert!(matches!(meta.value.kind, ExprKind::Macro(_)));
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
fn parse_items_ast_handles_impl_associated_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl FromStr for RefNode { type Err = CoreError; fn from_str() {} }")
        .unwrap();
    let ItemKind::Impl(item) = items[0].kind() else {
        panic!("expected impl item");
    };
    assert!(matches!(item.items[0].kind(), ItemKind::DefType(_)));
    assert!(matches!(item.items[1].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_lifetime_self_receiver() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl Foo { fn resolve<'a>(&'a self) -> &'a Self { self } }")
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
fn parse_items_ast_handles_octal_literals_in_impl_methods() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "impl OpenOptions { pub fn new() -> OpenOptions { OpenOptions { mode: 0o666 } } }",
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
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
fn parse_items_supports_bool_and_int_name_value_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[defaults(present = true, retries = 3)] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let defaults = function
        .attrs
        .iter()
        .find(|attr| matches!(&attr.meta, AttrMeta::List(list) if list.name.last().as_str() == "defaults"))
        .expect("expected defaults attribute");
    let AttrMeta::List(list) = &defaults.meta else {
        panic!("expected defaults list");
    };
    assert_eq!(list.items.len(), 2);

    let AttrMeta::NameValue(present) = &list.items[0] else {
        panic!("expected present default");
    };
    assert_eq!(present.name.last().as_str(), "present");
    assert!(
        matches!(present.value.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::Bool(flag) if flag.value))
    );

    let AttrMeta::NameValue(retries) = &list.items[1] else {
        panic!("expected retries default");
    };
    assert_eq!(retries.name.last().as_str(), "retries");
    assert!(
        matches!(retries.value.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::Int(number) if number.value == 3))
    );
}

#[test]
fn parse_items_supports_python_like_function_defaults() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn f(x: str = \"hi\", y: bool = true, z: i64 = 3) {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let params = &function.sig.params;
    assert_eq!(params.len(), 3);
    assert!(matches!(params[0].default, Some(Value::String(_))));
    assert!(matches!(params[1].default, Some(Value::Bool(_))));
    assert!(matches!(params[2].default, Some(Value::Int(_))));
}

#[test]
fn parse_items_supports_python_like_param_markers() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn f(a: str, /, b: str, *args: str, c: str, **kwargs: str) {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let params = &function.sig.params;
    assert_eq!(params.len(), 5);
    assert!(params[0].positional_only);
    assert!(!params[1].positional_only);
    assert!(params[2].as_tuple);
    assert!(params[3].keyword_only);
    assert!(params[4].as_dict);
    assert!(params[4].keyword_only);
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

#[test]
fn direct_parser_handles_basic_call_chain() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("foo.bar(1)[0]?").unwrap();
    assert!(matches!(expr.kind(), ExprKind::Try(_)));
}

#[test]
fn direct_parser_handles_cast_and_await() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let expr = parser.parse_expr_ast("await foo as i64 + 1").unwrap();
    let ExprKind::BinOp(bin) = expr.kind() else {
        panic!("expected binop");
    };
    assert!(matches!(bin.kind, BinOpKind::Add));
}
