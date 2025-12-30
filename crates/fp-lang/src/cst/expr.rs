use crate::lexer::lexeme::{Lexeme, LexemeKind};
use crate::lexer::tokenizer::{classify_and_normalize_lexeme, TokenKind};
use crate::lexer::{Span as TokSpan, Token};
use crate::syntax::{
    span_for_children, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, SyntaxTokenKind,
};
use fp_core::cst::CstCategory;
use fp_core::span::{FileId, Span};

#[derive(Debug, Clone)]
pub struct ExprCstParseError {
    pub message: String,
}

impl std::fmt::Display for ExprCstParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ExprCstParseError {}

pub fn parse_expr_lexemes_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
) -> Result<SyntaxNode, ExprCstParseError> {
    let mut p = Parser::new(lexemes, file);
    p.parse_root_expr()
}

pub fn parse_expr_lexemes_prefix_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    let mut p = Parser::new(lexemes, file);
    let expr = p.parse_expr_bp(0)?;

    // Consume trailing trivia so callers can look at the next significant token.
    while p.idx < p.tokens.len() && p.tokens[p.idx].is_trivia() {
        p.idx += 1;
    }

    Ok((expr, p.idx))
}

pub fn parse_type_lexemes_prefix_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
    stops: &[&str],
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    let mut p = Parser::new(lexemes, file);
    let ty = p.parse_type_bp_until(0, stops)?;
    while p.idx < p.tokens.len() && p.tokens[p.idx].is_trivia() {
        p.idx += 1;
    }
    Ok((ty, p.idx))
}

#[derive(Clone, Debug)]
struct Classified {
    raw: String,
    normalized: String,
    kind: TokenKind,
    lexeme_kind: LexemeKind,
    span: Span,
}

impl Classified {
    fn is_trivia(&self) -> bool {
        self.lexeme_kind != LexemeKind::Token
    }
}

struct Parser {
    tokens: Vec<Classified>,
    idx: usize,
    file: FileId,
}

impl Parser {
    fn new(lexemes: &[Lexeme], file: FileId) -> Self {
        let tokens = lexemes
            .iter()
            .map(|l| {
                let span = Span::new(
                    file,
                    (l.span.start.min(u32::MAX as usize)) as u32,
                    (l.span.end.min(u32::MAX as usize)) as u32,
                );
                if l.kind != LexemeKind::Token {
                    return Classified {
                        raw: l.text.clone(),
                        normalized: l.text.clone(),
                        kind: TokenKind::Symbol,
                        lexeme_kind: l.kind,
                        span,
                    };
                }

                let (kind, normalized) = classify_and_normalize_lexeme(&l.text)
                    .unwrap_or((TokenKind::Symbol, l.text.clone()));

                Classified {
                    raw: l.text.clone(),
                    normalized,
                    kind,
                    lexeme_kind: l.kind,
                    span,
                }
            })
            .collect();

        Self {
            tokens,
            idx: 0,
            file,
        }
    }

    fn split_right_shift(&mut self) {
        let Some(token) = self.tokens.get(self.idx).cloned() else {
            return;
        };
        if token.raw != ">>" {
            return;
        }
        let first = Classified {
            raw: ">".to_string(),
            normalized: ">".to_string(),
            kind: TokenKind::Symbol,
            lexeme_kind: token.lexeme_kind,
            span: token.span,
        };
        let second = Classified {
            raw: ">".to_string(),
            normalized: ">".to_string(),
            kind: TokenKind::Symbol,
            lexeme_kind: token.lexeme_kind,
            span: token.span,
        };
        self.tokens[self.idx] = first;
        self.tokens.insert(self.idx + 1, second);
    }

    fn parse_root_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let lo = self.tokens.first().map(|t| t.span.lo).unwrap_or(0);
        let hi = self.tokens.last().map(|t| t.span.hi).unwrap_or(0);
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        let expr = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(expr)));
        self.bump_trivia_into(&mut children);

        if self.has_remaining_non_trivia() {
            return Err(self.error("unsupported trailing tokens in CST-first expr"));
        }

        Ok(SyntaxNode::new(
            SyntaxKind::Root,
            children,
            Span::new(self.file, lo, hi),
        ))
    }

    fn parse_expr_before_block(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_expr_bp_inner(0, false)
    }

    fn has_remaining_non_trivia(&self) -> bool {
        self.tokens[self.idx..].iter().any(|t| !t.is_trivia())
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_expr_bp_inner(min_bp, true)
    }

    fn parse_expr_bp_inner(
        &mut self,
        min_bp: u8,
        allow_struct_literal: bool,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut left = self.parse_prefix()?;
        left = self.parse_postfix(left, allow_struct_literal)?;

        loop {
            let Some(op) = self.peek_non_trivia_raw() else {
                break;
            };
            let Some((l_bp, r_bp, kind)) = infix_binding_power(op) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }

            let mut children = Vec::new();
            children.push(SyntaxElement::Node(Box::new(left)));
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // operator
            self.bump_trivia_into(&mut children);

            if kind == SyntaxKind::ExprCast {
                let ty = self.parse_type_node()?;
                children.push(SyntaxElement::Node(Box::new(ty)));
            } else {
                let right = self.parse_expr_bp_inner(r_bp, allow_struct_literal)?;
                children.push(SyntaxElement::Node(Box::new(right)));
            }

            let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
            left = SyntaxNode::new(kind, children, span);
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        if self.peek_non_trivia_normalized() == Some("await") {
            let mut children = Vec::new();
            let start_span = self
                .peek_any_span()
                .unwrap_or_else(|| Span::new(self.file, 0, 0));
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // `await`
            self.bump_trivia_into(&mut children);
            let expr = self.parse_expr_bp(255)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
            let span = span_for_children(&children).unwrap_or(start_span);
            return Ok(SyntaxNode::new(SyntaxKind::ExprAwait, children, span));
        }

        match self.peek_non_trivia_normalized() {
            Some("quote") => return self.parse_quote_expr(),
            Some("splice") => return self.parse_splice_expr(),
            Some("async") => return self.parse_async_expr(),
            Some("const") => {
                if self.peek_second_non_trivia_raw() == Some("{") {
                    return self.parse_const_block_expr();
                }
            }
            Some("move") => {
                if self.peek_second_non_trivia_raw() == Some("|") {
                    return self.parse_closure_expr();
                }
            }
            Some("if") => return self.parse_if_expr(),
            Some("loop") => return self.parse_loop_expr(),
            Some("while") => return self.parse_while_expr(),
            Some("for") => return self.parse_for_expr(),
            Some("match") => return self.parse_match_expr(),
            Some("struct") => {
                if self.peek_second_non_trivia_raw() == Some("{") {
                    return self.parse_structural_literal_expr();
                }
            }
            Some("return") => return self.parse_return_expr(),
            Some("break") => return self.parse_break_expr(),
            Some("continue") => return self.parse_continue_expr(),
            _ => {}
        }

        if self.peek_non_trivia_raw() == Some("|") {
            return self.parse_closure_expr();
        }

        match self.peek_non_trivia_raw() {
            Some("+") | Some("-") | Some("!") | Some("&") | Some("*") => {
                let mut children = Vec::new();
                let start_span = self
                    .peek_any_span()
                    .unwrap_or_else(|| Span::new(self.file, 0, 0));
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children); // operator
                self.bump_trivia_into(&mut children);

                // `&mut expr`
                if self.peek_non_trivia_normalized() == Some("mut") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
                let expr = self.parse_expr_bp(255)?;
                children.push(SyntaxElement::Node(Box::new(expr)));
                let span = span_for_children(&children).unwrap_or(start_span);
                Ok(SyntaxNode::new(SyntaxKind::ExprUnary, children, span))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_postfix(
        &mut self,
        mut base: SyntaxNode,
        allow_struct_literal: bool,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        loop {
            match self.peek_non_trivia_raw() {
                Some("{") => {
                    if allow_struct_literal
                        && matches!(base.kind, SyntaxKind::ExprName | SyntaxKind::ExprPath)
                    {
                        base = self.parse_struct_literal(base)?;
                        continue;
                    }
                    break;
                }
                Some("(") => {
                    base = self.parse_call(base)?;
                }
                Some("[") => {
                    base = self.parse_index(base)?;
                }
                Some("?") => {
                    let mut children = Vec::new();
                    children.push(SyntaxElement::Node(Box::new(base.clone())));
                    self.bump_trivia_into(&mut children);
                    self.bump_token_into(&mut children);
                    let span =
                        span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
                    base = SyntaxNode::new(SyntaxKind::ExprTry, children, span);
                }
                Some(".") => {
                    // `.await`
                    base = self.parse_dot_postfix(base)?;
                }
                Some("!") => {
                    base = self.parse_macro_call(base)?;
                }
                _ => break,
            }
        }

        Ok(base)
    }

    fn parse_primary(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        match self.peek_non_trivia_raw() {
            Some("(") => self.parse_paren_expr(),
            Some("[") => self.parse_array_literal_expr(),
            Some("{") => self.parse_block_expr(),
            Some(_tok) => {
                let kind = self.peek_non_trivia_token_kind();
                match kind {
                    Some(TokenKind::Number) => self.parse_number(),
                    Some(TokenKind::StringLiteral) => self.parse_string(),
                    Some(TokenKind::Ident) => self.parse_name_or_path(),
                    Some(TokenKind::Keyword(kw))
                        if matches!(
                            kw,
                            crate::lexer::Keyword::Crate | crate::lexer::Keyword::Super
                        ) =>
                    {
                        self.parse_name_or_path()
                    }
                    _ => Err(self.error("expected primary expression")),
                }
            }
            None => Err(self.error("unexpected end of input")),
        }
    }

    fn parse_block_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);

        while self.peek_non_trivia_raw() != Some("}") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated block"));
            }
            let stmt = self.parse_block_stmt()?;
            children.push(SyntaxElement::Node(Box::new(stmt)));
            self.bump_trivia_into(&mut children);
        }

        self.bump_trivia_into(&mut children);
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprBlock, children, span))
    }

    fn parse_block_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        match self.peek_non_trivia_normalized() {
            Some("let") => self.parse_let_stmt(),
            _ if self.is_item_stmt_start() => self.parse_item_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn is_item_stmt_start(&self) -> bool {
        match self.peek_non_trivia_normalized() {
            Some(
                "fn" | "struct" | "enum" | "type" | "trait" | "impl" | "use" | "extern" | "mod"
                | "static",
            ) => true,
            Some("const") => {
                // Disambiguate `const { ... }` (expression) vs `const NAME: Ty = ...;` (item).
                matches!(
                    self.peek_nth_non_trivia_token_kind(2),
                    Some(TokenKind::Ident)
                )
            }
            _ => false,
        }
    }

    fn parse_item_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        let mut toks: Vec<Token> = Vec::new();
        let mut idx_map: Vec<usize> = Vec::new();
        for (rel, t) in self.tokens[self.idx..].iter().enumerate() {
            if t.is_trivia() {
                continue;
            }
            let span = TokSpan {
                start: t.span.lo as usize,
                end: t.span.hi as usize,
            };
            toks.push(Token {
                kind: t.kind.clone(),
                lexeme: t.raw.clone(),
                span,
            });
            idx_map.push(self.idx + rel);
        }

        let (item, consumed) = crate::cst::items::parse_item_tokens_prefix_to_cst(&toks)
            .map_err(|err| self.error(&format!("failed to parse item statement: {err}")))?;
        if consumed == 0 {
            return Err(self.error("failed to parse item statement"));
        }
        let last_idx = idx_map
            .get(consumed - 1)
            .copied()
            .ok_or_else(|| self.error("failed to advance after item statement"))?;
        self.idx = last_idx + 1;

        children.push(SyntaxElement::Node(Box::new(item)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtItem, children, span))
    }

    fn parse_let_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `let`
        self.bump_trivia_into(&mut children);

        // Optional `mut`
        if self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        // ident or `_`
        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) => self.bump_token_into(&mut children),
            Some(TokenKind::Keyword(_)) if self.peek_non_trivia_raw() == Some("_") => {
                self.bump_token_into(&mut children)
            }
            _ => return Err(self.error("expected identifier after let")),
        }
        self.bump_trivia_into(&mut children);

        // Optional type annotation `: <type>`.
        if self.peek_non_trivia_raw() == Some(":") {
            self.bump_token_into(&mut children);
            let ty = self.parse_type_node_until(&["=", ";"])?;
            children.push(SyntaxElement::Node(Box::new(ty)));
            self.bump_trivia_into(&mut children);
        }

        // initializer
        self.expect_token_raw("=")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        let init = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(init)));
        self.bump_trivia_into(&mut children);

        self.expect_token_raw(";")?;
        self.bump_token_into(&mut children);

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtLet, children, span))
    }

    fn parse_expr_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        let expr = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(expr)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(";") {
            self.bump_token_into(&mut children);
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtExpr, children, span))
    }

    fn parse_quote_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `quote`
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("<") {
            self.expect_token_raw("<")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut children);
                }
                _ => return Err(self.error("expected quote fragment kind")),
            }
            self.bump_trivia_into(&mut children);
            self.expect_token_raw(">")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprQuote, children, span))
    }

    fn parse_splice_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `splice`
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("(") {
            self.expect_token_raw("(")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let token_expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(token_expr)));
            self.bump_trivia_into(&mut children);
            self.expect_token_raw(")")?;
            self.bump_token_into(&mut children);
        } else {
            let token_expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(token_expr)));
        }
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprSplice, children, span))
    }

    fn parse_async_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `async`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprAsync, children, span))
    }

    fn parse_const_block_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `const`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprConstBlock, children, span))
    }

    fn parse_if_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `if`
        self.bump_trivia_into(&mut children);
        let cond = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(cond)));
        self.bump_trivia_into(&mut children);
        let then_block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(then_block)));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_normalized() == Some("else") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_normalized() == Some("if") {
                let else_if = self.parse_if_expr()?;
                children.push(SyntaxElement::Node(Box::new(else_if)));
            } else {
                let else_block = self.parse_block_expr()?;
                children.push(SyntaxElement::Node(Box::new(else_block)));
            }
        }
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprIf, children, span))
    }

    fn parse_loop_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `loop`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprLoop, children, span))
    }

    fn parse_while_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `while`
        self.bump_trivia_into(&mut children);
        let cond = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(cond)));
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprWhile, children, span))
    }

    fn parse_pattern(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        match self.peek_non_trivia_raw() {
            Some("_") => {
                self.bump_token_into(&mut children);
                let span = span_for_children(&children).unwrap_or(start);
                Ok(SyntaxNode::new(SyntaxKind::PatternWildcard, children, span))
            }
            Some("(") => {
                self.bump_token_into(&mut children); // '('
                self.bump_trivia_into(&mut children);

                while self.peek_non_trivia_raw() != Some(")") {
                    let pat = self.parse_pattern()?;
                    children.push(SyntaxElement::Node(Box::new(pat)));
                    self.bump_trivia_into(&mut children);

                    if self.peek_non_trivia_raw() == Some(",") {
                        self.bump_token_into(&mut children);
                        self.bump_trivia_into(&mut children);
                        // Allow trailing comma.
                        if self.peek_non_trivia_raw() == Some(")") {
                            break;
                        }
                        continue;
                    }
                    break;
                }

                self.expect_token_raw(")")?;
                self.bump_token_into(&mut children);
                let span = span_for_children(&children).unwrap_or(start);
                Ok(SyntaxNode::new(SyntaxKind::PatternTuple, children, span))
            }
            _ => {
                self.expect_ident_token()?;
                self.bump_token_into(&mut children);
                let span = span_for_children(&children).unwrap_or(start);
                Ok(SyntaxNode::new(SyntaxKind::PatternIdent, children, span))
            }
        }
    }

    fn parse_for_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `for`
        self.bump_trivia_into(&mut children);

        let pat = self.parse_pattern()?;
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() != Some("in") {
            return Err(self.error("expected 'in' in for loop"));
        }
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let iter = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(iter)));
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprFor, children, span))
    }

    fn parse_match_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `match`
        self.bump_trivia_into(&mut children);
        let scrutinee = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(scrutinee)));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);

        loop {
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("}") {
                break;
            }
            let arm = self.parse_match_arm()?;
            children.push(SyntaxElement::Node(Box::new(arm)));
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some(",") {
                self.bump_token_into(&mut children);
            }
        }

        self.bump_trivia_into(&mut children);
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprMatch, children, span))
    }

    fn parse_match_arm(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        let pat = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);

        // Support struct-like match patterns like `Variant { .. }` by consuming the braced body.
        // The current match lowering does not destructure fields, but this at least keeps parsing
        // compatible with example sources.
        if self.peek_non_trivia_raw() == Some("{") {
            self.parse_balanced_group_into("{", "}", &mut children)?;
            self.bump_trivia_into(&mut children);
        }
        if self.peek_non_trivia_normalized() == Some("if") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let guard = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(guard)));
            self.bump_trivia_into(&mut children);
        }
        self.expect_token_raw("=>")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        let body = if self.peek_non_trivia_raw() == Some("{") {
            self.parse_block_expr()?
        } else {
            self.parse_expr_bp(0)?
        };
        children.push(SyntaxElement::Node(Box::new(body)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::MatchArm, children, span))
    }

    fn parse_closure_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("move") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }
        self.expect_token_raw("|")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        // params until '|'
        if self.peek_non_trivia_raw() != Some("|") {
            loop {
                self.parse_closure_param_into(&mut children)?;
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some("|") {
                    break;
                }
                self.expect_token_raw(",")?;
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }
        self.expect_token_raw("|")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let body = if self.peek_non_trivia_raw() == Some("{") {
            self.parse_block_expr()?
        } else {
            self.parse_expr_bp(0)?
        };
        children.push(SyntaxElement::Node(Box::new(body)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprClosure, children, span))
    }

    fn parse_closure_param_into(
        &mut self,
        out: &mut Vec<SyntaxElement>,
    ) -> Result<(), ExprCstParseError> {
        self.bump_trivia_into(out);

        let mut pat_node = self.parse_pattern()?;
        self.bump_trivia_into(out);

        if self.peek_non_trivia_raw() == Some(":") {
            let mut typed_children = Vec::new();
            typed_children.push(SyntaxElement::Node(Box::new(pat_node)));
            self.bump_trivia_into(&mut typed_children);
            self.bump_token_into(&mut typed_children); // ':'
            self.bump_trivia_into(&mut typed_children);
            let ty = self.parse_type_node_until(&[",", "|"])?;
            typed_children.push(SyntaxElement::Node(Box::new(ty)));
            let span =
                span_for_children(&typed_children).unwrap_or_else(|| Span::new(self.file, 0, 0));
            pat_node = SyntaxNode::new(SyntaxKind::PatternType, typed_children, span);
        }

        out.push(SyntaxElement::Node(Box::new(pat_node)));
        Ok(())
    }

    fn parse_return_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `return`
        self.bump_trivia_into(&mut children);
        if !matches!(self.peek_non_trivia_raw(), Some(";") | Some("}")) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprReturn, children, span))
    }

    fn parse_break_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `break`
        self.bump_trivia_into(&mut children);
        if !matches!(self.peek_non_trivia_raw(), Some(";") | Some("}")) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprBreak, children, span))
    }

    fn parse_continue_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `continue`
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprContinue, children, span))
    }

    fn parse_type_node_until(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_type_bp_until(0, stops)
    }

    fn expect_token_raw(&self, expected: &str) -> Result<(), ExprCstParseError> {
        if self.peek_non_trivia_raw() == Some(expected) {
            Ok(())
        } else {
            Err(self.error(&format!("expected token '{expected}'")))
        }
    }

    fn expect_ident_token(&self) -> Result<(), ExprCstParseError> {
        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) => Ok(()),
            _ => Err(self.error("expected identifier")),
        }
    }

    fn parse_dot_postfix(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let snapshot = self.idx;
        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(base.clone())));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '.'
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("await") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
            return Ok(SyntaxNode::new(SyntaxKind::ExprAwait, children, span));
        }

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) => {
                self.bump_token_into(&mut children);
                let span =
                    span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
                Ok(SyntaxNode::new(SyntaxKind::ExprSelect, children, span))
            }
            _ => {
                self.idx = snapshot;
                Err(self.error("expected field name after '.'"))
            }
        }
    }

    fn parse_call(&mut self, callee: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(callee)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() != Some(")") {
            loop {
                let arg = self.parse_expr_bp(0)?;
                children.push(SyntaxElement::Node(Box::new(arg)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    continue;
                }
                break;
            }
        }

        if self.peek_non_trivia_raw() != Some(")") {
            return Err(self.error("expected ')' to close call"));
        }
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
        Ok(SyntaxNode::new(SyntaxKind::ExprCall, children, span))
    }

    fn parse_index(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(base)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '['
        self.bump_trivia_into(&mut children);
        let index = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(index)));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() != Some("]") {
            return Err(self.error("expected ']' to close index"));
        }
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
        Ok(SyntaxNode::new(SyntaxKind::ExprIndex, children, span))
    }

    fn parse_macro_call(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        // Only support simple `ident!(...)` / `ident!{...}` / `ident![...]` forms for now.
        if base.kind != SyntaxKind::ExprName {
            return Err(self.error("unsupported macro callee in CST-first expr"));
        }

        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(base)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '!'
        self.bump_trivia_into(&mut children);

        let (open, close) = match self.peek_non_trivia_raw() {
            Some("(") => ("(", ")"),
            Some("{") => ("{", "}"),
            Some("[") => ("[", "]"),
            _ => return Err(self.error("expected delimiter after '!'")),
        };

        self.parse_balanced_group_into(open, close, &mut children)?;

        let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
        Ok(SyntaxNode::new(SyntaxKind::ExprMacroCall, children, span))
    }

    fn parse_type_node(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_type_bp_until(0, &[])
    }

    fn parse_type_bp_until(
        &mut self,
        min_bp: u8,
        stops: &[&str],
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut left = self.parse_type_atom(stops)?;

        loop {
            let Some(op) = self.peek_non_trivia_raw() else {
                break;
            };
            if stops.iter().any(|s| *s == op) {
                break;
            }
            if op == "=>" {
                break;
            }
            if is_type_boundary_token(op) {
                break;
            }

            let (l_bp, r_bp) = match op {
                "+" | "|" | "&" | "-" => (10u8, 11u8),
                _ => break,
            };
            if l_bp < min_bp {
                break;
            }

            let mut children = Vec::new();
            children.push(SyntaxElement::Node(Box::new(left)));
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // operator
            self.bump_trivia_into(&mut children);
            let right = self.parse_type_bp_until(r_bp, stops)?;
            children.push(SyntaxElement::Node(Box::new(right)));
            let span = span_for_children(&children).unwrap_or_else(|| Span::new(self.file, 0, 0));
            left = SyntaxNode::new(SyntaxKind::TyBinary, children, span);
        }

        Ok(left)
    }

    fn parse_type_atom(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        match self.peek_non_trivia_normalized() {
            Some("_") => {
                let mut children = Vec::new();
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children);
                let span = span_for_children(&children).unwrap_or(start);
                Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span))
            }
            Some("(") => self.parse_paren_type(stops),
            Some("&") => self.parse_ref_type(stops),
            Some("[") => self.parse_array_or_slice_type(),
            Some("fn") => self.parse_fn_type(),
            Some("impl") => self.parse_impl_traits_type(),
            Some("struct") => {
                if self.peek_second_non_trivia_raw() == Some("{") {
                    self.parse_structural_type()
                } else {
                    self.parse_path_type()
                }
            }
            Some("{") => self.parse_structural_type(),
            _ => self.parse_path_type(),
        }
    }

    fn parse_paren_type(&mut self, _stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("(")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(")") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::TyUnit, children, span));
        }

        let first = self.parse_type_bp_until(0, &[",", ")"])?;
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(",") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            while self.peek_non_trivia_raw() != Some(")") {
                let next = self.parse_type_bp_until(0, &[",", ")"])?;
                children.push(SyntaxElement::Node(Box::new(next)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    if self.peek_non_trivia_raw() == Some(")") {
                        break;
                    }
                    continue;
                }
                break;
            }
            self.expect_token_raw(")")?;
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::TyTuple, children, span));
        }

        self.expect_token_raw(")")?;
        self.bump_token_into(&mut children);

        // Parentheses are grouping for non-tuple types.
        let inner = children
            .iter()
            .find_map(|c| match c {
                SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
                    Some(n.as_ref())
                }
                _ => None,
            })
            .ok_or_else(|| self.error("expected type"))?;
        Ok(inner.clone())
    }

    fn parse_ref_type(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("&")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        if let Some(raw) = self.peek_non_trivia_raw() {
            if raw == "'" {
                // `&'lifetime T`
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                if matches!(
                    self.peek_non_trivia_token_kind(),
                    Some(TokenKind::Ident) | Some(TokenKind::Keyword(_))
                ) {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
            } else if raw.starts_with('\'') {
                // Lexer may provide a combined `'lifetime` token.
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }

        let inner = self.parse_type_bp_until(0, stops)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyRef, children, span))
    }

    fn parse_array_or_slice_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("[")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let elem = self.parse_type_bp_until(0, &[";", "]"])?;
        children.push(SyntaxElement::Node(Box::new(elem)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(";") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let len_expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(len_expr)));
            self.bump_trivia_into(&mut children);
            self.expect_token_raw("]")?;
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::TyArray, children, span));
        }

        self.expect_token_raw("]")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TySlice, children, span))
    }

    fn parse_fn_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `fn`
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("(")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() != Some(")") {
            loop {
                let ty = self.parse_type_bp_until(0, &[",", ")"])?;
                children.push(SyntaxElement::Node(Box::new(ty)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    if self.peek_non_trivia_raw() == Some(")") {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_token_raw(")")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("->") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let ret = self.parse_type_bp_until(0, &[])?;
            children.push(SyntaxElement::Node(Box::new(ret)));
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyFn, children, span))
    }

    fn parse_impl_traits_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `impl`
        self.bump_trivia_into(&mut children);

        let bound = self.parse_path_type()?;
        children.push(SyntaxElement::Node(Box::new(bound)));

        // Support Rust-like `impl Fn(T) -> U` syntax by consuming the signature tokens.
        // At the moment this is represented as additional children on `TyImplTraits`.
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("(") {
            self.bump_token_into(&mut children); // '('
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() != Some(")") {
                loop {
                    let arg = self.parse_type_bp_until(0, &[",", ")"])?;
                    children.push(SyntaxElement::Node(Box::new(arg)));
                    self.bump_trivia_into(&mut children);
                    if self.peek_non_trivia_raw() == Some(",") {
                        self.bump_token_into(&mut children);
                        self.bump_trivia_into(&mut children);
                        continue;
                    }
                    break;
                }
            }
            self.expect_token_raw(")")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);

            if self.peek_non_trivia_raw() == Some("->") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                let ret = self.parse_type_bp_until(0, &[])?;
                children.push(SyntaxElement::Node(Box::new(ret)));
            }
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyImplTraits, children, span))
    }

    fn parse_structural_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_normalized() == Some("struct") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        while self.peek_non_trivia_raw() != Some("}") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated structural type"));
            }
            let field = self.parse_structural_type_field()?;
            children.push(SyntaxElement::Node(Box::new(field)));
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some(",") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                continue;
            }
        }

        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyStructural, children, span))
    }

    fn parse_structural_type_field(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_ident_token()?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        self.expect_token_raw(":")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        let ty = self.parse_type_bp_until(0, &[",", "}"])?;
        children.push(SyntaxElement::Node(Box::new(ty)));
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyField, children, span))
    }

    fn parse_path_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) => self.bump_token_into(&mut children),
            Some(TokenKind::Keyword(_)) => self.bump_token_into(&mut children),
            _ => return Err(self.error("expected type path")),
        }
        self.bump_trivia_into(&mut children);

        while self.peek_non_trivia_raw() == Some("::") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) => self.bump_token_into(&mut children),
                Some(TokenKind::Keyword(_)) => self.bump_token_into(&mut children),
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
        }

        if self.peek_non_trivia_raw() == Some("<") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            while self.peek_non_trivia_raw() != Some(">") {
                if self.peek_non_trivia_raw().is_none() {
                    return Err(self.error("unterminated generic args"));
                }
                if self.peek_non_trivia_raw() == Some(">>") {
                    self.split_right_shift();
                    break;
                }

                let snapshot = self.idx;
                if self.peek_non_trivia_token_kind() == Some(TokenKind::Ident) {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    if self.peek_non_trivia_raw() == Some("=") {
                        self.bump_token_into(&mut children);
                        self.bump_trivia_into(&mut children);
                    } else {
                        self.idx = snapshot;
                    }
                }

                let arg_ty = self.parse_type_bp_until(0, &[",", ">"])?;
                children.push(SyntaxElement::Node(Box::new(arg_ty)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    continue;
                }
                break;
            }
            if self.peek_non_trivia_raw() == Some(">>") {
                self.split_right_shift();
            }
            self.expect_token_raw(">")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        if self.peek_non_trivia_raw() == Some("!") {
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // '!'
            self.bump_trivia_into(&mut children);

            let (open, close) = match self.peek_non_trivia_raw() {
                Some("(") => ("(", ")"),
                Some("{") => ("{", "}"),
                Some("[") => ("[", "]"),
                _ => return Err(self.error("expected delimiter after '!'")),
            };
            self.parse_balanced_group_into(open, close, &mut children)?;

            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::TyMacroCall, children, span));
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::TyPath, children, span))
    }

    fn parse_balanced_group_into(
        &mut self,
        open: &'static str,
        close: &'static str,
        out: &mut Vec<SyntaxElement>,
    ) -> Result<(), ExprCstParseError> {
        self.bump_trivia_into(out);
        if self.peek_non_trivia_raw() != Some(open) {
            return Err(self.error("expected group opener"));
        }

        // Consume the outer opener and initialize the delimiter stack.
        self.bump_token_into(out);
        let mut stack: Vec<&'static str> = Vec::new();
        stack.push(close);

        while !stack.is_empty() {
            self.bump_trivia_into(out);
            let Some(tok) = self.peek_non_trivia_raw() else {
                return Err(self.error("unexpected end of input in macro group"));
            };

            if tok == "(" {
                stack.push(")");
            } else if tok == "{" {
                stack.push("}");
            } else if tok == "[" {
                stack.push("]");
            } else if tok == ")" || tok == "}" || tok == "]" {
                let expected = stack.pop().unwrap();
                if tok != expected {
                    return Err(self.error("mismatched delimiter in macro group"));
                }
            }

            self.bump_token_into(out);
        }

        Ok(())
    }

    fn parse_paren_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(")") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::ExprUnit, children, span));
        }

        let first = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(",") {
            // Tuple
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);

            while self.peek_non_trivia_raw() != Some(")") {
                let next = self.parse_expr_bp(0)?;
                children.push(SyntaxElement::Node(Box::new(next)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    continue;
                }
                break;
            }

            self.expect_token_raw(")")?;
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::ExprTuple, children, span));
        }

        self.expect_token_raw(")")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprParen, children, span))
    }

    fn parse_array_literal_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '['
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("]") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::ExprArray, children, span));
        }

        let first = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(";") {
            // Repeat
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let len = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(len)));
            self.bump_trivia_into(&mut children);
            self.expect_token_raw("]")?;
            self.bump_token_into(&mut children);
            let span = span_for_children(&children).unwrap_or(start);
            return Ok(SyntaxNode::new(SyntaxKind::ExprArrayRepeat, children, span));
        }

        while self.peek_non_trivia_raw() == Some(",") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("]") {
                break;
            }
            let next = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(next)));
            self.bump_trivia_into(&mut children);
        }

        self.expect_token_raw("]")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprArray, children, span))
    }

    fn parse_structural_literal_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `struct`
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        self.parse_struct_fields_into(&mut children)?;
        self.bump_trivia_into(&mut children);

        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprStructural, children, span))
    }

    fn parse_struct_literal(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = base.span;
        children.push(SyntaxElement::Node(Box::new(base)));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        self.parse_struct_fields_into(&mut children)?;
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprStruct, children, span))
    }

    fn parse_struct_fields_into(
        &mut self,
        out: &mut Vec<SyntaxElement>,
    ) -> Result<(), ExprCstParseError> {
        if self.peek_non_trivia_raw() == Some("}") {
            return Ok(());
        }

        loop {
            if self.peek_non_trivia_raw() == Some("}") {
                break;
            }

            // Rust-like struct update / wildcard field pattern: `..` or `..expr`.
            if self.peek_non_trivia_raw() == Some("..") {
                self.bump_token_into(out);
                self.bump_trivia_into(out);
                if !matches!(self.peek_non_trivia_raw(), Some(",") | Some("}")) {
                    let expr = self.parse_expr_bp(0)?;
                    out.push(SyntaxElement::Node(Box::new(expr)));
                    self.bump_trivia_into(out);
                }
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(out);
                    self.bump_trivia_into(out);
                }
                // `..` must be the last field.
                break;
            }

            let field = self.parse_struct_field()?;
            out.push(SyntaxElement::Node(Box::new(field)));
            self.bump_trivia_into(out);
            if self.peek_non_trivia_raw() == Some("}") {
                break;
            }
            if self.peek_non_trivia_raw() == Some(",") {
                self.bump_token_into(out);
                self.bump_trivia_into(out);
                // allow trailing comma
                if self.peek_non_trivia_raw() == Some("}") {
                    break;
                }
                continue;
            }
            return Err(self.error("expected ',' or '}' after struct field"));
        }

        Ok(())
    }

    fn parse_struct_field(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        // name
        self.expect_ident_token()?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(":") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let value = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(value)));
        } else {
            // shorthand; ensure next token is `,` or `}`
            match self.peek_non_trivia_raw() {
                Some(",") | Some("}") => {}
                _ => return Err(self.error("expected ':' after field name")),
            }
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::StructField, children, span))
    }

    fn parse_name_or_path(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                self.bump_token_into(&mut children)
            }
            _ => return Err(self.error("expected identifier")),
        }
        self.bump_trivia_into(&mut children);

        let mut is_path = false;
        while self.peek_non_trivia_raw() == Some("::") {
            is_path = true;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut children)
                }
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
        }

        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(
            if is_path {
                SyntaxKind::ExprPath
            } else {
                SyntaxKind::ExprName
            },
            children,
            span,
        ))
    }

    fn parse_number(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprNumber, children, span))
    }

    fn parse_string(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children);
        let span = span_for_children(&children).unwrap_or(start);
        Ok(SyntaxNode::new(SyntaxKind::ExprString, children, span))
    }

    fn bump_trivia_into(&mut self, out: &mut Vec<SyntaxElement>) {
        while self.tokens.get(self.idx).is_some_and(|t| t.is_trivia()) {
            self.bump_any_into(out);
        }
    }

    fn bump_token_into(&mut self, out: &mut Vec<SyntaxElement>) {
        while self.tokens.get(self.idx).is_some_and(|t| t.is_trivia()) {
            self.bump_any_into(out);
        }
        self.bump_any_into(out);
    }

    fn bump_any_into(&mut self, out: &mut Vec<SyntaxElement>) {
        let Some(tok) = self.tokens.get(self.idx).cloned() else {
            return;
        };
        let kind = match tok.lexeme_kind {
            LexemeKind::Token => SyntaxTokenKind::Token,
            LexemeKind::TriviaWhitespace => SyntaxTokenKind::TriviaWhitespace,
            LexemeKind::TriviaLineComment => SyntaxTokenKind::TriviaLineComment,
            LexemeKind::TriviaBlockComment => SyntaxTokenKind::TriviaBlockComment,
        };
        out.push(SyntaxElement::Token(SyntaxToken {
            kind,
            text: tok.raw,
            span: tok.span,
        }));
        self.idx += 1;
    }

    fn peek_any_span(&self) -> Option<Span> {
        self.tokens.get(self.idx).map(|t| t.span)
    }

    fn peek_non_trivia_raw(&self) -> Option<&str> {
        self.peek_non_trivia().map(|t| t.raw.as_str())
    }

    fn peek_non_trivia_normalized(&self) -> Option<&str> {
        self.peek_non_trivia().map(|t| t.normalized.as_str())
    }

    fn peek_non_trivia_token_kind(&self) -> Option<TokenKind> {
        self.peek_non_trivia().map(|t| t.kind.clone())
    }

    fn peek_non_trivia(&self) -> Option<&Classified> {
        let mut i = self.idx;
        while let Some(t) = self.tokens.get(i) {
            if !t.is_trivia() {
                return Some(t);
            }
            i += 1;
        }
        None
    }

    fn peek_nth_non_trivia(&self, n: usize) -> Option<&Classified> {
        if n == 0 {
            return None;
        }
        let mut i = self.idx;
        let mut seen = 0usize;
        while let Some(t) = self.tokens.get(i) {
            if !t.is_trivia() {
                seen += 1;
                if seen == n {
                    return Some(t);
                }
            }
            i += 1;
        }
        None
    }

    fn peek_nth_non_trivia_token_kind(&self, n: usize) -> Option<TokenKind> {
        self.peek_nth_non_trivia(n).map(|t| t.kind.clone())
    }

    fn peek_second_non_trivia_raw(&self) -> Option<&str> {
        let mut i = self.idx;
        let mut seen = 0usize;
        while let Some(t) = self.tokens.get(i) {
            if !t.is_trivia() {
                seen += 1;
                if seen == 2 {
                    return Some(t.raw.as_str());
                }
            }
            i += 1;
        }
        None
    }

    fn error(&self, message: &str) -> ExprCstParseError {
        let near = self.peek_non_trivia_raw().unwrap_or("<eof>");
        ExprCstParseError {
            message: format!("{message} (near `{near}`)"),
        }
    }
}

fn infix_binding_power(op: &str) -> Option<(u8, u8, SyntaxKind)> {
    // Only a small subset for the initial CST-first experiment.
    // Higher number = tighter binding.
    let (l, r, kind) = match op {
        "*" | "/" | "%" => (50, 51, SyntaxKind::ExprBinary),
        "+" | "-" => (40, 41, SyntaxKind::ExprBinary),
        "<<" | ">>" => (35, 36, SyntaxKind::ExprBinary),
        ".." | "..=" => (33, 34, SyntaxKind::ExprRange),
        "as" => (55, 56, SyntaxKind::ExprCast),
        "<" | ">" | "<=" | ">=" => (30, 31, SyntaxKind::ExprBinary),
        "==" | "!=" => (25, 26, SyntaxKind::ExprBinary),
        "&" => (22, 23, SyntaxKind::ExprBinary),
        "^" => (21, 22, SyntaxKind::ExprBinary),
        "|" => (20, 21, SyntaxKind::ExprBinary),
        "&&" => (15, 16, SyntaxKind::ExprBinary),
        "||" => (10, 11, SyntaxKind::ExprBinary),
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=" => {
            (5, 5, SyntaxKind::ExprBinary)
        } // right-associative
        _ => return None,
    };
    Some((l, r, kind))
}

fn is_type_boundary_token(tok: &str) -> bool {
    matches!(
        tok,
        ")" | "]"
            | "}"
            | ","
            | ";"
            | "?"
            | "."
            | "!"
            | "<<"
            | ">>"
            | "<"
            | ">"
            | "<="
            | ">="
            | "=="
            | "!="
            | "&&"
            | "||"
            | "="
            | ".."
            | "..="
    )
}
