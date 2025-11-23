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

mod expr;
mod items;
mod cst;
mod winnow;
pub use fp_core::cst::{CstError, CstResult};

/// Parser for the FerroPhase language backed by winnow.
#[derive(Default)]
pub struct FerroPhaseParser;

impl FerroPhaseParser {
    /// Create a new parser instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse source and produce a CST.
    pub fn parse_to_cst(&self, source: &str) -> Result<CstNode> {
        cst::parse(source).map_err(|err| eyre::eyre!(err))
    }

    /// Parse a FerroPhase expression directly into an fp-core AST expression.
    pub fn parse_expr_ast(&self, source: &str) -> Result<Expr> {
        expr::parse_expression(source).map_err(|err| eyre::eyre!(err))
    }

    /// Parse source to CST and immediately lower into an fp-core AST expression.
    pub fn parse_cst_to_ast(&self, source: &str) -> Result<Expr> {
        let cst = self.parse_to_cst(source)?;
        let src = cst_to_source(&cst);
        expr::parse_expression(&src).map_err(|err| eyre::eyre!(err))
    }

    /// Rewrite source code into Rust-compatible macros.
    pub fn rewrite_to_rust(&self, source: &str) -> Result<String> {
        let cst = self.parse_to_cst(source)?;
        Ok(cst_to_rust(&cst))
    }

    /// Parse a sequence of items into fp-core AST items.
    pub fn parse_items_ast(&self, source: &str) -> Result<Vec<fp_core::ast::Item>> {
        items::parse_items(source).map_err(|err| eyre::eyre!(err))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{BlockStmt, ExprKind, ItemKind};
    use fp_core::ops::BinOpKind;

    #[test]
    fn parses_rust_like_source() {
        let parser = FerroPhaseParser::new();
        let cst = parser
            .parse_to_cst("fn main() { println!(\"hi\"); }")
            .unwrap();
        assert!(matches!(cst.kind, CstKind::Root));
    }

    #[test]
    fn rewrites_quote_and_splice() {
        let parser = FerroPhaseParser::new();
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
    fn parse_expr_ast_handles_range() {
        let parser = FerroPhaseParser::new();
        let expr = parser
            .parse_expr_ast("0..10")
            .expect("parse range expr");
        match expr.kind() {
            ExprKind::Range(r) => {
                assert!(r.start.is_some());
                assert!(r.end.is_some());
            }
            other => panic!("expected range expr, got {:?}", other),
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
