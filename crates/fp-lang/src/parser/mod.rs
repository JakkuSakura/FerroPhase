//! FerroPhase parser powered by the `winnow` parser combinator library.
//!
//! Instead of relying on tree-sitter and a bespoke preprocessor, the parser now
//! performs tokenization and syntax analysis directly using winnow. Source text
//! is lowered into the `fp_core::cst::CstNode` tree, which acts as the single
//! syntax representation used throughout the frontend before lowering into the
//! full fp-core AST.

use eyre::Result;
use fp_core::ast::Expr;
use fp_core::cst::{CstKind, CstNode};
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};

mod cst;
mod expr;
mod items;
pub mod lower;
mod winnow;
pub use fp_core::cst::{CstError, CstResult};

const FERRO_CONTEXT: &str = "ferrophase.parser";

/// Parser for the FerroPhase language backed by winnow.
pub struct FerroPhaseParser {
    diagnostics: std::sync::Arc<DiagnosticManager>,
}

impl Default for FerroPhaseParser {
    fn default() -> Self {
        Self {
            diagnostics: std::sync::Arc::new(DiagnosticManager::new()),
        }
    }
}

impl FerroPhaseParser {
    /// Create a new parser instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Access the diagnostics collected by the winnow-based parser.
    pub fn diagnostics(&self) -> std::sync::Arc<DiagnosticManager> {
        self.diagnostics.clone()
    }

    /// Clear any previously collected diagnostics.
    pub fn clear_diagnostics(&self) {
        self.diagnostics.clear();
    }

    fn record_diagnostic(&self, level: DiagnosticLevel, message: impl Into<String>) {
        let message = message.into();
        let diagnostic = match level {
            DiagnosticLevel::Error => Diagnostic::error(message),
            DiagnosticLevel::Warning => Diagnostic::warning(message),
            DiagnosticLevel::Info => Diagnostic::info(message),
        }
        .with_source_context(FERRO_CONTEXT.to_string());

        self.diagnostics.add_diagnostic(diagnostic);
    }

    fn record_error(&self, message: impl Into<String>) {
        self.record_diagnostic(DiagnosticLevel::Error, message);
    }

    /// Parse source and produce a CST.
    pub fn parse_to_cst(&self, source: &str) -> Result<CstNode> {
        match cst::parse(source) {
            Ok(cst) => Ok(cst),
            Err(err) => {
                self.record_error(format!("failed to parse CST: {}", err));
                Err(eyre::eyre!(err))
            }
        }
    }

    /// Parse a FerroPhase expression directly into an fp-core AST expression.
    pub fn parse_expr_ast(&self, source: &str) -> Result<Expr> {
        match expr::parse_expression(source) {
            Ok(expr) => Ok(expr),
            Err(err) => {
                self.record_error(format!("failed to parse expression: {}", err));
                Err(eyre::eyre!(err))
            }
        }
    }

    /// Parse source to CST and immediately lower into an fp-core AST expression.
    pub fn parse_cst_to_ast(&self, source: &str) -> Result<Expr> {
        let cst = self.parse_to_cst(source)?;
        if let Some(expr) = lower::lower_expr_from_cst(&cst) {
            Ok(expr)
        } else {
            let src = cst_to_source(&cst);
            match expr::parse_expression(&src) {
                Ok(expr) => Ok(expr),
                Err(err) => {
                    self.record_error(format!("failed to lower CST to expression: {}", err));
                    Err(eyre::eyre!(err))
                }
            }
        }
    }

    /// Rewrite source code into Rust-compatible macros.
    pub fn rewrite_to_rust(&self, source: &str) -> Result<String> {
        let cst = self.parse_to_cst(source)?;
        Ok(cst_to_rust(&cst))
    }

    /// Parse a sequence of items into fp-core AST items.
    pub fn parse_items_ast(&self, source: &str) -> Result<Vec<fp_core::ast::Item>> {
        match items::parse_items(source) {
            Ok(items) => Ok(items),
            Err(err) => {
                self.record_error(format!("failed to parse items: {}", err));
                Err(eyre::eyre!(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{BlockStmt, ExprKind, ItemKind, MacroDelimiter};
    use fp_core::intrinsics::IntrinsicCallKind;
    use fp_core::ops::BinOpKind;

    #[test]
    fn parses_rust_like_source() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let cst = parser
            .parse_to_cst("fn main() { println!(\"hi\"); }")
            .unwrap();
        assert!(matches!(cst.kind, CstKind::Root));
    }

    #[test]
    fn rewrites_quote_and_splice() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_cst_to_ast("quote { splice ( token ) }")
            .unwrap();
        match expr.kind() {
            ExprKind::Quote(q) => {
                let inner = q.block.last_expr().expect("quote should carry expr");
                assert!(matches!(inner.kind(), ExprKind::Splice(_)));
            }
            other => panic!("expected quote expr, got {:?}", other),
        }
    }

    #[test]
    fn rewrite_emit_macro_matches_expected_output() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let src = "fn main() { emit! { let generated = 42; generated } }";
        let rewritten = parser.rewrite_to_rust(src).unwrap();
        assert_eq!(
            rewritten,
            "fn main(){fp_splice!(fp_quote!({let generated=42;generated}))}"
        );
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
        let cst = parser.parse_to_cst(src).unwrap();
        let quotes = count_kind(&cst, CstKind::Quote);
        let splices = count_kind(&cst, CstKind::Splice);
        assert_eq!(quotes, 3, "all quote constructs should be captured");
        assert!(splices >= 2, "splice constructs should remain visible");
        assert!(
            cst_contains_splice_with_quote(&cst),
            "expected splice to contain nested quote"
        );
    }

    #[test]
    fn parser_handles_raw_identifiers_and_strings() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let src = r####"
            fn r#type() {
                let cooked = "hi\nthere";
                let raw = r#"hello world"#;
                let bytes = br##"bin data"##;
                let short = b"abc";
            }
        "####;
        let cst = parser.parse_to_cst(src).unwrap();
        let tokens = collect_tokens(&cst);
        assert!(
            tokens.iter().any(|t| t == "r#type"),
            "raw ident token missing"
        );
        assert!(
            tokens.iter().any(|t| t == "\"hi\\nthere\""),
            "escaped string literal not captured"
        );
        assert!(
            tokens.iter().any(|t| t == r##"r#"hello world"#"##),
            "raw string literal missing"
        );
        assert!(
            tokens.iter().any(|t| t == r####"br##"bin data"##"####),
            "raw byte string literal missing"
        );
        assert!(
            tokens.iter().any(|t| t == "b\"abc\""),
            "byte string literal missing"
        );
    }

    #[test]
    fn parse_expr_ast_builds_quote_ast() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser.parse_expr_ast("quote { 1 + 2 }").unwrap();
        match expr.kind() {
            ExprKind::Quote(quote) => {
                let stmts = &quote.block.stmts;
                assert_eq!(stmts.len(), 1);
                if let BlockStmt::Expr(block_expr) = &stmts[0] {
                    match block_expr.expr.kind() {
                        ExprKind::BinOp(bin) => {
                            assert!(matches!(bin.kind, BinOpKind::Add));
                        }
                        other => panic!("expected binary op, found {:?}", other),
                    }
                } else {
                    panic!("quote should contain expression statement");
                }
            }
            other => panic!("expected quote expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_splice_of_quote() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("splice ( quote { 3 } )")
            .expect("parse splice expr");
        match expr.kind() {
            ExprKind::Splice(splice) => match splice.token.kind() {
                ExprKind::Quote(_) => {}
                other => panic!("expected nested quote, found {:?}", other),
            },
            other => panic!("expected splice expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_match() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("match x { a => 1, _ => 2 }")
            .expect("parse match expr");
        match expr.kind() {
            ExprKind::Match(m) => {
                assert_eq!(m.cases.len(), 2);
            }
            other => panic!("expected match expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_match_guard_and_wildcard() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let src = "match x { y if cond => 1, _ => 2 }";
        let expr = parser
            .parse_expr_ast(src)
            .expect("parse guarded match expr");
        match expr.kind() {
            ExprKind::Match(m) => {
                assert_eq!(m.cases.len(), 2);
            }
            other => panic!("expected match expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_range() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser.parse_expr_ast("0..10").expect("parse range expr");
        match expr.kind() {
            ExprKind::Range(r) => {
                assert!(r.start.is_some());
                assert!(r.end.is_some());
            }
            other => panic!("expected range expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_closure() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("|x, y| x + y")
            .expect("parse closure expr");
        match expr.kind() {
            ExprKind::Closure(c) => {
                assert_eq!(c.params.len(), 2);
            }
            other => panic!("expected closure expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_typed_and_mut_closure_params() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("|mut x: Foo, _: crate::bar::Baz| x")
            .expect("parse typed closure expr");
        match expr.kind() {
            ExprKind::Closure(c) => {
                assert_eq!(c.params.len(), 2);
                // First param should be a mutable ident pattern possibly wrapped in a type.
                let first = &c.params[0];
                assert!(first.ty().is_some());
                if let fp_core::ast::PatternKind::Type(pattern_type) = first.kind() {
                    assert!(pattern_type.pat.ty().is_none());
                    assert_eq!(
                        pattern_type.pat.as_ident().expect("ident pattern").as_str(),
                        "x"
                    );
                } else {
                    panic!("expected typed pattern for first closure param");
                }
                // Second param is `_ : crate::bar::Baz`; we only check that a type is present.
                let second = &c.params[1];
                assert!(second.ty().is_some());
            }
            other => panic!("expected closure expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_await() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("await foo")
            .expect("parse await expr");
        match expr.kind() {
            ExprKind::Await(a) => {
                // base should be a locator for `foo`
                assert!(matches!(a.base.kind(), ExprKind::Locator(_)));
            }
            other => panic!("expected await expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_cst_to_ast_lowers_const_block() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_cst_to_ast("const { 1 + 2 }")
            .expect("parse const block expr");
        match expr.kind() {
            ExprKind::IntrinsicCall(call) => {
                assert!(matches!(call.kind, IntrinsicCallKind::ConstBlock));
                if let IntrinsicCallKind::ConstBlock = call.kind {
                    // We expect exactly one argument carrying the lowered block.
                    let args = match &call.payload {
                        fp_core::intrinsics::IntrinsicCallPayload::Args { args } => args,
                        _ => panic!("expected args payload for const block intrinsic"),
                    };
                    assert_eq!(args.len(), 1);
                }
            }
            other => panic!("expected intrinsic const block, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_async_block() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("async { 1 + 2 }")
            .expect("parse async block expr");
        match expr.kind() {
            ExprKind::Async(async_expr) => {
                if let ExprKind::Block(block) = async_expr.expr.kind() {
                    assert_eq!(block.stmts.len(), 1);
                    if let BlockStmt::Expr(block_expr) = &block.stmts[0] {
                        match block_expr.expr.kind() {
                            ExprKind::BinOp(bin) => {
                                assert!(matches!(bin.kind, BinOpKind::Add));
                            }
                            other => {
                                panic!("expected binary op in async block, found {:?}", other)
                            }
                        }
                    } else {
                        panic!("async block should contain expression statement");
                    }
                } else {
                    panic!("async should wrap a block expression");
                }
            }
            other => panic!("expected block expr from async, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_for_loop_syntax() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let expr = parser
            .parse_expr_ast("for i in 0..10 { i }")
            .expect("parse for loop expr");
        match expr.kind() {
            ExprKind::For(for_expr) => {
                assert!(for_expr.pat.as_ident().is_some());
                assert!(matches!(for_expr.iter.kind(), ExprKind::Range(_)));
            }
            other => panic!("expected block expr from for, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_const_item() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("const FOO: i64 = 42;")
            .expect("parse const item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefConst(def) => {
                assert_eq!(def.name.as_str(), "FOO");
                assert!(def.ty.is_some());
            }
            other => panic!("expected const item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_static_item() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("static BAR: i64 = 1;")
            .expect("parse static item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefStatic(def) => {
                assert_eq!(def.name.as_str(), "BAR");
            }
            other => panic!("expected static item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_type_alias() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("type MyInt = i64;")
            .expect("parse type alias item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefType(def) => {
                assert_eq!(def.name.as_str(), "MyInt");
            }
            other => panic!("expected type alias item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_enum_item() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("enum Color { Red = 1, Green(i64, i64), Blue: i64 = 3 }")
            .expect("parse enum item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefEnum(def) => {
                assert_eq!(def.name.as_str(), "Color");
                assert_eq!(def.value.variants.len(), 3);
            }
            other => panic!("expected enum item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_module_item() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("mod foo { fn bar() {} }")
            .expect("parse module item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::Module(module) => {
                assert_eq!(module.name.as_str(), "foo");
                assert!(!module.items.is_empty());
            }
            other => panic!("expected module item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_trait_item() {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast("trait Foo<T>: Bar + Baz { fn bar(x: T); type T; const N: i64; }")
            .expect("parse trait item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefTrait(def) => {
                assert_eq!(def.name.as_str(), "Foo");
                assert_eq!(def.bounds.bounds.len(), 2);
                assert_eq!(def.items.len(), 3);
            }
            other => panic!("expected trait item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_impl_item() {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast("impl MyType { fn foo() {} }")
            .expect("parse impl item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::Impl(impl_item) => {
                assert!(!impl_item.items.is_empty());
            }
            other => panic!("expected impl item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_ast_handles_trait_impl_item() {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast("impl<T> Foo<T> for Bar<T> { fn baz() {} }")
            .expect("parse trait impl item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::Impl(impl_item) => {
                assert!(impl_item.trait_ty.is_some());
                assert!(!impl_item.items.is_empty());
            }
            other => panic!("expected impl item, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_macro_invocation() {
        let parser = FerroPhaseParser::new();
        let expr = parser
            .parse_expr_ast("foo!(x, y)")
            .expect("parse macro expr");
        match expr.kind() {
            ExprKind::Macro(m) => {
                assert_eq!(m.invocation.delimiter, MacroDelimiter::Parenthesis);
                assert!(m.invocation.tokens.contains("x"));
            }
            other => panic!("expected macro expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_calls_fields_and_assignments() {
        let parser = FerroPhaseParser::new();
        let expr = parser
            .parse_expr_ast("foo.bar(1, 2)[i] = -baz")
            .expect("parse complex expr");
        match expr.kind() {
            ExprKind::Assign(assign) => {
                match assign.value.kind() {
                    ExprKind::UnOp(_) => {}
                    other => panic!("expected unary rhs, found {:?}", other),
                }
                match assign.target.kind() {
                    ExprKind::Index(index) => match index.obj.kind() {
                        ExprKind::Invoke(_) => {}
                        other => panic!("expected invoke on index base, got {:?}", other),
                    },
                    other => panic!("expected index as assignment target, got {:?}", other),
                }
            }
            other => panic!("expected assignment expr, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_supports_let_statements_in_blocks() {
        let parser = FerroPhaseParser::new();
        let expr = parser
            .parse_expr_ast("quote { let x = 1; let y = x; x + y }")
            .expect("parse quote with lets");
        let ExprKind::Quote(quote) = expr.kind() else {
            panic!("expected quote expr");
        };
        assert_eq!(quote.block.stmts.len(), 3, "two lets + trailing expr");
        assert!(matches!(quote.block.stmts[0], BlockStmt::Let(_)));
        assert!(matches!(quote.block.stmts[1], BlockStmt::Let(_)));
        match &quote.block.stmts[2] {
            BlockStmt::Expr(stmt) => match stmt.expr.kind() {
                ExprKind::BinOp(bin) => assert!(matches!(bin.kind, BinOpKind::Add)),
                other => panic!("expected addition tail expr, found {:?}", other),
            },
            other => panic!("expected final expr stmt, got {:?}", other),
        }
    }

    #[test]
    fn parse_expr_ast_handles_if_loop_and_while() {
        let parser = FerroPhaseParser::new();
        let expr = parser
            .parse_expr_ast("if cond { 1 } else { 2 }")
            .expect("parse if expr");
        assert!(matches!(expr.kind(), ExprKind::If(_)));

        let loop_expr = parser.parse_expr_ast("loop { break }").expect("parse loop");
        assert!(matches!(loop_expr.kind(), ExprKind::Loop(_)));

        let while_expr = parser
            .parse_expr_ast("while cond { continue }")
            .expect("parse while");
        assert!(matches!(while_expr.kind(), ExprKind::While(_)));
    }

    #[test]
    fn parse_items_supports_fn_struct_and_use() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("use foo::bar; struct S {} fn main() { return }")
            .expect("parse items");
        assert!(items
            .iter()
            .any(|it| matches!(it.kind(), ItemKind::Import(_))));
        assert!(items
            .iter()
            .any(|it| matches!(it.kind(), ItemKind::DefStruct(_))));
        assert!(items
            .iter()
            .any(|it| matches!(it.kind(), ItemKind::DefFunction(_))));
    }

    #[test]
    fn parse_items_ast_handles_generic_fn_with_where() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("fn foo<T>(x: T) -> i64 where T: Clone { 0 }")
            .expect("parse generic fn item");
        assert_eq!(items.len(), 1);
        match items[0].kind() {
            ItemKind::DefFunction(def) => {
                assert_eq!(def.sig.generics_params.len(), 1);
                assert_eq!(def.sig.params.len(), 1);
                assert!(def.sig.ret_ty.is_some());
            }
            other => panic!("expected function item, got {:?}", other),
        }
    }

    #[test]
    fn parse_items_supports_typed_params_and_fields() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast(
                "struct S { x: Foo, y: crate::bar::Baz } fn f(a: Foo, b: crate::bar::Baz) { return }",
            )
            .expect("parse typed items");
        let mut saw_struct = false;
        let mut saw_fn = false;
        for it in items {
            match it.kind() {
                ItemKind::DefStruct(def) => {
                    saw_struct = true;
                    assert_eq!(def.value.fields.len(), 2);
                }
                ItemKind::DefFunction(def) => {
                    saw_fn = true;
                    assert_eq!(def.sig.params.len(), 2);
                }
                _ => {}
            }
        }
        assert!(saw_struct && saw_fn);
    }

    #[test]
    fn parse_items_supports_fn_attributes() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("#[test_attr] fn f() { return }")
            .expect("parse attrs");
        let func = items
            .into_iter()
            .find_map(|it| match it.kind() {
                ItemKind::DefFunction(def) => Some(def.clone()),
                _ => None,
            })
            .expect("expected function item");
        assert_eq!(func.attrs.len(), 1);
    }

    #[test]
    fn parse_items_supports_item_macro() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast("foo! { x, y }")
            .expect("parse item macro");
        assert!(items
            .iter()
            .any(|it| matches!(it.kind(), ItemKind::Macro(_))));
    }

    fn count_kind(node: &CstNode, kind: CstKind) -> usize {
        (node.kind == kind) as usize
            + node
                .children
                .iter()
                .map(|child| count_kind(child, kind.clone()))
                .sum::<usize>()
    }

    fn collect_tokens(node: &CstNode) -> Vec<String> {
        let mut out = Vec::new();
        walk_tokens(node, &mut out);
        out
    }

    fn walk_tokens(node: &CstNode, out: &mut Vec<String>) {
        if node.kind == CstKind::Token {
            if let Some(text) = &node.text {
                out.push(text.clone());
            }
        }
        for child in &node.children {
            walk_tokens(child, out);
        }
    }

    fn cst_contains_splice_with_quote(node: &CstNode) -> bool {
        if node.kind == CstKind::Splice {
            return node
                .children
                .iter()
                .any(|child| count_kind(child, CstKind::Quote) > 0);
        }
        node.children.iter().any(cst_contains_splice_with_quote)
    }
}

fn cst_to_rust(node: &CstNode) -> String {
    let mut builder = RewriteBuilder::default();
    builder.write(node);
    builder.finish()
}

fn cst_to_source(node: &CstNode) -> String {
    let mut builder = SourceBuilder::default();
    builder.write(node);
    builder.finish()
}

#[derive(Default)]
struct RewriteBuilder {
    buf: String,
}

impl RewriteBuilder {
    fn write(&mut self, node: &CstNode) {
        match node.kind {
            CstKind::Root => {
                for child in &node.children {
                    self.write(child);
                }
            }
            CstKind::Block => {
                self.push_symbol("{");
                for child in &node.children {
                    self.write(child);
                }
                self.push_symbol("}");
            }
            CstKind::ConstBlock => {
                self.push_token("const");
                self.soft_space();
                for child in &node.children {
                    self.write(child);
                }
            }
            CstKind::Quote => {
                self.push_token("fp_quote!");
                self.push_symbol("(");
                for child in &node.children {
                    self.write(child);
                }
                self.push_symbol(")");
            }
            CstKind::Splice => {
                self.push_token("fp_splice!");
                self.push_symbol("(");
                for (idx, child) in node.children.iter().enumerate() {
                    if idx > 0 {
                        self.soft_space();
                    }
                    self.write(child);
                }
                self.push_symbol(")");
            }
            CstKind::Token => {
                if let Some(text) = &node.text {
                    self.push_token(text);
                }
            }
        }
    }

    fn push_token(&mut self, text: &str) {
        if self.needs_space(text) {
            self.buf.push(' ');
        }
        self.buf.push_str(text);
    }

    fn push_symbol(&mut self, symbol: &str) {
        self.buf.push_str(symbol);
    }

    fn soft_space(&mut self) {
        if self.buf.chars().last().map_or(true, |c| !c.is_whitespace()) {
            self.buf.push(' ');
        }
    }

    fn needs_space(&self, next: &str) -> bool {
        if self.buf.is_empty() {
            return false;
        }
        let prev = self.last_significant_char();
        let next_char = next.chars().next();
        match (prev, next_char) {
            (Some(p), Some(n)) => Self::is_ident_like(p) && Self::is_ident_char(n),
            _ => false,
        }
    }

    fn last_significant_char(&self) -> Option<char> {
        self.buf.chars().rev().find(|c| !c.is_whitespace())
    }

    fn is_ident_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    fn is_ident_like(ch: char) -> bool {
        Self::is_ident_char(ch) || matches!(ch, '}' | ')' | ']')
    }

    fn finish(self) -> String {
        self.buf
    }
}

#[derive(Default)]
struct SourceBuilder {
    buf: String,
}

impl SourceBuilder {
    fn write(&mut self, node: &CstNode) {
        match node.kind {
            CstKind::Root => {
                for child in &node.children {
                    self.write(child);
                }
            }
            CstKind::Block => {
                self.push_symbol("{");
                for child in &node.children {
                    self.write(child);
                }
                self.push_symbol("}");
            }
            CstKind::ConstBlock => {
                self.push_token("const");
                self.soft_space();
                for child in &node.children {
                    self.write(child);
                }
            }
            CstKind::Quote => {
                self.push_token("quote");
                self.soft_space();
                for child in &node.children {
                    self.write(child);
                }
            }
            CstKind::Splice => {
                self.push_token("splice");
                self.push_symbol(" (");
                for (idx, child) in node.children.iter().enumerate() {
                    if idx > 0 {
                        self.soft_space();
                    }
                    self.write(child);
                }
                self.push_symbol(")");
            }
            CstKind::Token => {
                if let Some(text) = &node.text {
                    self.push_token(text);
                }
            }
        }
    }

    fn push_token(&mut self, text: &str) {
        if self.needs_space(text) {
            self.buf.push(' ');
        }
        self.buf.push_str(text);
    }

    fn push_symbol(&mut self, symbol: &str) {
        self.buf.push_str(symbol);
    }

    fn soft_space(&mut self) {
        if self.buf.chars().last().map_or(true, |c| !c.is_whitespace()) {
            self.buf.push(' ');
        }
    }

    fn needs_space(&self, next: &str) -> bool {
        if self.buf.is_empty() {
            return false;
        }
        let prev = self.last_significant_char();
        let next_char = next.chars().next();
        match (prev, next_char) {
            (Some(p), Some(n)) => Self::is_ident_like(p) && Self::is_ident_char(n),
            _ => false,
        }
    }

    fn last_significant_char(&self) -> Option<char> {
        self.buf.chars().rev().find(|c| !c.is_whitespace())
    }

    fn is_ident_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    fn is_ident_like(ch: char) -> bool {
        Self::is_ident_char(ch) || matches!(ch, '}' | ')' | ']')
    }

    fn finish(self) -> String {
        self.buf
    }
}
