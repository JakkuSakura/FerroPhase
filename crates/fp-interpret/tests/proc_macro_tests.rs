use fp_core::ast::{
    AttrMeta, AttrStyle, Attribute, Expr, ExprKind, FunctionParam, FunctionSignature, Ident,
    Item, ItemDefFunction, ItemKind, MacroDelimiter, MacroInvocation, MacroToken, MacroTokenTree,
    MacroExpansionParser, Path, Ty, TypeTokenStream, TypeFunction, Value, ValueTokenStream,
};
use fp_core::span::Span;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_interpret::engine::{AstInterpreter, InterpreterMode, InterpreterOptions, StdoutMode};

struct TokenStreamParser;

impl TokenStreamParser {
    fn tokens_to_string(tokens: &[MacroTokenTree]) -> String {
        fn flatten(tokens: &[MacroTokenTree], out: &mut Vec<String>) {
            for tree in tokens {
                match tree {
                    MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
                    MacroTokenTree::Group(group) => {
                        let (open, close) = match group.delimiter {
                            MacroDelimiter::Parenthesis => ("(", ")"),
                            MacroDelimiter::Bracket => ("[", "]"),
                            MacroDelimiter::Brace => ("{", "}"),
                        };
                        out.push(open.to_string());
                        flatten(&group.tokens, out);
                        out.push(close.to_string());
                    }
                }
            }
        }

        let mut out = Vec::new();
        flatten(tokens, &mut out);
        out.join(" ")
    }
}

impl MacroExpansionParser for TokenStreamParser {
    fn parse_items(&self, _tokens: &[MacroTokenTree]) -> Result<Vec<Item>> {
        Ok(Vec::new())
    }

    fn parse_expr(&self, tokens: &[MacroTokenTree]) -> Result<Expr> {
        let text = Self::tokens_to_string(tokens);
        let value: i64 = text.trim().parse().unwrap_or(0);
        Ok(Expr::value(Value::int(value)))
    }

    fn parse_type(&self, _tokens: &[MacroTokenTree]) -> Result<Ty> {
        Ok(Ty::TokenStream(TypeTokenStream))
    }
}

fn proc_macro_attribute() -> Attribute {
    Attribute {
        style: AttrStyle::Outer,
        meta: AttrMeta::Path(Path::from_ident(Ident::new("proc_macro"))),
    }
}

fn token(text: &str) -> MacroTokenTree {
    MacroTokenTree::Token(MacroToken {
        text: text.to_string(),
        span: Span::null(),
    })
}

#[test]
fn expands_function_like_proc_macro() -> Result<()> {
    let proc_macro_body = Expr::value(Value::TokenStream(ValueTokenStream {
        tokens: vec![token("123")],
    }));

    let mut proc_sig = FunctionSignature::unit();
    proc_sig.name = Some(Ident::new("echo"));
    proc_sig.params = vec![FunctionParam::new(
        Ident::new("input"),
        Ty::TokenStream(TypeTokenStream),
    )];
    proc_sig.ret_ty = Some(Ty::TokenStream(TypeTokenStream));

    let proc_macro_fn = ItemDefFunction {
        ty_annotation: Some(Ty::TokenStream(TypeTokenStream)),
        attrs: vec![proc_macro_attribute()],
        name: Ident::new("echo"),
        ty: Some(TypeFunction {
            params: vec![Ty::TokenStream(TypeTokenStream)],
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(Ty::TokenStream(TypeTokenStream))),
        }),
        sig: proc_sig,
        body: proc_macro_body.into(),
        visibility: fp_core::ast::Visibility::Public,
    };

    let invocation = MacroInvocation::new(
        Path::from_ident(Ident::new("echo")),
        MacroDelimiter::Parenthesis,
        "42",
    )
    .with_token_trees(vec![token("42")])
    .with_span(Span::null());
    let demo_body = Expr::from(ExprKind::Macro(fp_core::ast::ExprMacro::new(invocation)));

    let mut demo_sig = FunctionSignature::unit();
    demo_sig.name = Some(Ident::new("demo"));
    demo_sig.ret_ty = Some(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
        fp_core::ast::TypeInt::I64,
    )));

    let demo_fn = ItemDefFunction {
        ty_annotation: None,
        attrs: Vec::new(),
        name: Ident::new("demo"),
        ty: None,
        sig: demo_sig,
        body: demo_body.into(),
        visibility: fp_core::ast::Visibility::Public,
    };

    let file = fp_core::ast::File {
        path: "<test>".into(),
        items: vec![
            Item::from(ItemKind::DefFunction(proc_macro_fn)),
            Item::from(ItemKind::DefFunction(demo_fn)),
        ],
    };

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        debug_assertions: false,
        diagnostics: None,
        diagnostic_context: "proc-macro-test",
        module_resolution: None,
        macro_parser: Some(std::sync::Arc::new(TokenStreamParser)),
        intrinsic_normalizer: None,
        stdout_mode: StdoutMode::Capture,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    let mut node = fp_core::ast::Node::file(file);
    interpreter.interpret(&mut node);
    let fp_core::ast::NodeKind::File(updated_file) = node.kind() else {
        panic!("expected file node");
    };
    let ItemKind::DefFunction(updated) = &updated_file.items[1].kind else {
        panic!("expected demo function to remain");
    };
    let ExprKind::Value(value) = updated.body.kind() else {
        panic!("expected macro expansion to replace body with value");
    };
    assert!(matches!(value.as_ref(), Value::Int(i) if i.value == 123));

    Ok(())
}
