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
    pub span: Option<Span>,
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
    parse_prefix_with(lexemes, file, |p| p.parse_expr_bp(0))
}

pub fn parse_block_lexemes_prefix_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    parse_prefix_with(lexemes, file, Parser::parse_block_expr)
}

pub fn parse_type_lexemes_prefix_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
    stops: &[&str],
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    parse_prefix_with(lexemes, file, |p| p.parse_type_bp_until(0, stops))
}

pub fn parse_pattern_lexemes_prefix_to_cst(
    lexemes: &[Lexeme],
    file: FileId,
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    parse_prefix_with(lexemes, file, Parser::parse_pattern)
}

fn consume_trailing_trivia(parser: &mut Parser) {
    // Consume trailing trivia so callers can look at the next significant token.
    while parser.idx < parser.tokens.len() && parser.tokens[parser.idx].is_trivia() {
        parser.idx += 1;
    }
}

fn parse_prefix_with(
    lexemes: &[Lexeme],
    file: FileId,
    parse: impl FnOnce(&mut Parser) -> Result<SyntaxNode, ExprCstParseError>,
) -> Result<(SyntaxNode, usize), ExprCstParseError> {
    let mut parser = Parser::new(lexemes, file);
    let node = parse(&mut parser)?;
    consume_trailing_trivia(&mut parser);
    Ok((node, parser.idx.min(lexemes.len())))
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

    fn split_turbofish(&mut self) {
        self.split_turbofish_at(self.idx);
    }

    fn split_turbofish_at(&mut self, idx: usize) {
        let Some(token) = self.tokens.get(idx).cloned() else {
            return;
        };
        if token.raw != "::<" {
            return;
        }
        let colon_colon = Classified {
            raw: "::".to_string(),
            normalized: "::".to_string(),
            kind: TokenKind::Symbol,
            lexeme_kind: token.lexeme_kind,
            span: token.span,
        };
        let lt = Classified {
            raw: "<".to_string(),
            normalized: "<".to_string(),
            kind: TokenKind::Symbol,
            lexeme_kind: token.lexeme_kind,
            span: token.span,
        };
        self.tokens[idx] = colon_colon;
        self.tokens.insert(idx + 1, lt);
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
            while self.idx < self.tokens.len() {
                self.bump_token_into(&mut children);
            }
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

    fn parse_condition_expr_before_block(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        if self.peek_non_trivia_is("let") {
            return self.parse_let_expr();
        }

        let start_idx = self.idx;
        match self.parse_let_expr() {
            Ok(let_expr) => Ok(let_expr),
            Err(_) => {
                self.idx = start_idx;
                self.parse_expr_before_block()
            }
        }
    }

    fn parse_guard_condition_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        if self.peek_non_trivia_normalized() == Some("let") {
            self.parse_let_expr()
        } else {
            self.parse_expr_bp(0)
        }
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
        if min_bp == 0 {
            if let Some(assign) = self.try_parse_assign_pattern_expr(allow_struct_literal)? {
                return Ok(assign);
            }
        }
        let mut left = self.parse_prefix(allow_struct_literal)?;
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
            } else if kind == SyntaxKind::ExprRange && self.range_end_is_missing() {
                let span = span_for_children(&children);
                left = SyntaxNode::new(kind, children, span);
                continue;
            } else {
                let right = self.parse_expr_bp_inner(r_bp, allow_struct_literal)?;
                children.push(SyntaxElement::Node(Box::new(right)));
            }

            let span = span_for_children(&children);
            left = SyntaxNode::new(kind, children, span);
        }

        Ok(left)
    }

    fn try_parse_assign_pattern_expr(
        &mut self,
        allow_struct_literal: bool,
    ) -> Result<Option<SyntaxNode>, ExprCstParseError> {
        let start_idx = self.idx;
        let pat = match self.parse_pattern() {
            Ok(pat) => pat,
            Err(_) => {
                self.idx = start_idx;
                return Ok(None);
            }
        };
        let Some(op) = self.peek_non_trivia_raw() else {
            self.idx = start_idx;
            return Ok(None);
        };
        if !is_assignment_operator(op) {
            self.idx = start_idx;
            return Ok(None);
        }
        let Some((_, r_bp, kind)) = infix_binding_power(op) else {
            self.idx = start_idx;
            return Ok(None);
        };

        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // operator
        self.bump_trivia_into(&mut children);
        let right = self.parse_expr_bp_inner(r_bp, allow_struct_literal)?;
        children.push(SyntaxElement::Node(Box::new(right)));
        let span = span_for_children(&children);
        Ok(Some(SyntaxNode::new(kind, children, span)))
    }

    fn range_end_is_missing(&self) -> bool {
        matches!(
            self.peek_non_trivia_raw(),
            None | Some("]") | Some(")") | Some("}") | Some(",") | Some(";")
        )
    }

    fn parse_prefix(
        &mut self,
        allow_struct_literal: bool,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        if self.peek_non_trivia_raw() == Some("#")
            && self.peek_second_non_trivia_raw() != Some("!")
        {
            let attrs = self.parse_outer_attrs_expr()?;
            if !attrs.is_empty() {
                let mut children = Vec::new();
                for attr in attrs {
                    children.push(SyntaxElement::Node(Box::new(attr)));
                }
                self.bump_trivia_into(&mut children);
                let expr = self.parse_prefix(allow_struct_literal)?;
                children.push(SyntaxElement::Node(Box::new(expr)));
                let span = span_for_children(&children);
                return Ok(SyntaxNode::new(SyntaxKind::ExprAttr, children, span));
            }
        }
        if self.looks_like_ref_type_literal() {
            return self.parse_type_literal_expr();
        }
        if self.peek_non_trivia_normalized() == Some("await") {
            let mut children = Vec::new();
            let _start_span = self
                .peek_any_span()
                .unwrap_or_else(|| Span::new(self.file, 0, 0));
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // `await`
            self.bump_trivia_into(&mut children);
            let expr = self.parse_expr_bp(255)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::ExprAwait, children, span));
        }

        if let Some(result) = self.try_parse_keyword_prefix_expr() {
            return result;
        }

        if matches!(self.peek_non_trivia_raw(), Some("|") | Some("||")) {
            return self.parse_closure_expr();
        }

        match self.peek_non_trivia_raw() {
            Some("+") | Some("-") | Some("!") | Some("&") | Some("*") | Some("box") => {
                let mut children = Vec::new();
                let _start_span = self
                    .peek_any_span()
                    .unwrap_or_else(|| Span::new(self.file, 0, 0));
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children); // operator
                self.bump_trivia_into(&mut children);

                let op_is_ref = matches!(
                    children.last(),
                    Some(SyntaxElement::Token(token)) if token.text == "&"
                );
                if op_is_ref && self.peek_non_trivia_normalized() == Some("raw") {
                    let next = self
                        .peek_nth_non_trivia(2)
                        .map(|t| t.normalized.as_str());
                    if matches!(next, Some("mut") | Some("const")) {
                        self.bump_token_into(&mut children);
                        self.bump_trivia_into(&mut children);
                        self.bump_token_into(&mut children);
                        self.bump_trivia_into(&mut children);
                    }
                } else if op_is_ref && self.peek_non_trivia_normalized() == Some("mut") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
                let expr = self.parse_expr_bp_inner(255, allow_struct_literal)?;
                children.push(SyntaxElement::Node(Box::new(expr)));
                let span = span_for_children(&children);
                Ok(SyntaxNode::new(SyntaxKind::ExprUnary, children, span))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_type_literal_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let ty = self.parse_type_node()?;
        children.push(SyntaxElement::Node(Box::new(ty)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprType, children, span))
    }

    fn parse_postfix(
        &mut self,
        mut base: SyntaxNode,
        allow_struct_literal: bool,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        loop {
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            } else if self.peek_non_trivia_raw() == Some("::") {
                let next_idx = self.skip_trivia_from(self.idx + 1);
                self.split_turbofish_at(next_idx);
            }
            match self.peek_non_trivia_raw() {
                Some("{") => {
                    if allow_struct_literal
                        && matches!(
                            base.kind,
                            SyntaxKind::ExprName
                                | SyntaxKind::ExprPath
                                | SyntaxKind::ExprQuoteToken
                        )
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
                    let span = span_for_children(&children);
                    base = SyntaxNode::new(SyntaxKind::ExprTry, children, span);
                }
                Some(".") => {
                    // `.await`
                    base = self.parse_dot_postfix(base)?;
                }
                Some("::") => {
                    if self
                        .peek_nth_non_trivia(1)
                        .is_some_and(|tok| tok.raw.as_str() == "::<")
                    {
                        let next_idx = self.skip_trivia_from(self.idx + 1);
                        self.split_turbofish_at(next_idx);
                    }
                    if self
                        .peek_nth_non_trivia(1)
                        .is_some_and(|tok| tok.raw.as_str() == "<")
                        && matches!(
                            base.kind,
                            SyntaxKind::ExprSelect
                                | SyntaxKind::ExprPath
                                | SyntaxKind::ExprName
                                | SyntaxKind::ExprQuoteToken
                        )
                    {
                        base = self.parse_turbofish_postfix(base)?;
                        continue;
                    }
                    break;
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
        if self.peek_non_trivia_is("let") {
            return self.parse_let_expr();
        }
        if self.peek_non_trivia_normalized() == Some("unsafe")
            && self.peek_second_non_trivia_raw() == Some("{")
        {
            return self.parse_unsafe_block_expr();
        }
        match self.peek_non_trivia_raw() {
            Some("..") | Some("..=") => self.parse_range_prefix_expr(),
            Some("$") => self.parse_macro_var_expr(),
            Some("(") => self.parse_paren_expr(),
            Some("[") => self.parse_array_literal_expr(),
            Some("{") => self.parse_block_expr(),
            Some("<") => self.parse_qualified_path_expr(),
            Some(_tok) => {
                let kind = self.peek_non_trivia_token_kind();
                match kind {
                    Some(TokenKind::Number) => self.parse_number(),
                    Some(TokenKind::StringLiteral) => self.parse_string(),
                    Some(TokenKind::Ident) => self.parse_name_or_path(),
                    Some(TokenKind::Keyword(kw))
                        if matches!(
                            kw,
                            crate::lexer::Keyword::Crate
                                | crate::lexer::Keyword::Super
                                | crate::lexer::Keyword::Emit
                                | crate::lexer::Keyword::Fn
                                | crate::lexer::Keyword::Type
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
        let _start = self
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

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprBlock, children, span))
    }

    fn parse_unsafe_block_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("unsafe")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);

        while self.peek_non_trivia_raw() != Some("}") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated unsafe block"));
            }
            let stmt = self.parse_block_stmt()?;
            children.push(SyntaxElement::Node(Box::new(stmt)));
            self.bump_trivia_into(&mut children);
        }

        self.bump_trivia_into(&mut children);
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprBlock, children, span))
    }

    fn parse_block_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        if self.is_item_stmt_start() {
            return self.parse_item_stmt();
        }
        self.consume_stmt_attrs()?;
        if self.peek_non_trivia_is("let") {
            return self.parse_let_stmt();
        }
        match self.peek_non_trivia_normalized() {
            Some("defer") => self.parse_defer_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn is_item_stmt_start(&self) -> bool {
        let mut idx = self.idx;
        idx = self.skip_trivia_from(idx);
        idx = self.skip_outer_attrs_from(idx);
        idx = self.skip_trivia_from(idx);
        idx = self.skip_visibility_from(idx);
        idx = self.skip_trivia_from(idx);
        let Some(first) = self.nth_non_trivia_from(idx, 1) else {
            return false;
        };
        let first_norm = first.normalized.as_str();
        match first_norm {
            "fn" | "struct" | "enum" | "type" | "trait" | "impl" | "use" | "extern" | "mod"
                | "static" | "opaque"
            => true,
            "unsafe" => matches!(
                self.nth_non_trivia_from(idx, 2).map(|t| t.raw.as_str()),
                Some("extern" | "fn" | "trait" | "impl")
            ),
            "async" => self
                .nth_non_trivia_from(idx, 2)
                .map(|t| t.raw.as_str())
                == Some("fn"),
            "const" => {
                // Disambiguate `const { ... }` (expression) vs `const NAME: Ty = ...;` (item).
                matches!(self.nth_non_trivia_from(idx, 2).map(|t| t.kind.clone()), Some(TokenKind::Ident))
            }
            _ => false,
        }
    }

    fn skip_trivia_from(&self, mut idx: usize) -> usize {
        while self.tokens.get(idx).is_some_and(|t| t.is_trivia()) {
            idx += 1;
        }
        idx
    }

    fn skip_outer_attrs_from(&self, mut idx: usize) -> usize {
        loop {
            idx = self.skip_trivia_from(idx);
            let Some(tok) = self.tokens.get(idx) else {
                return idx;
            };
            let combined = tok.raw == "#[" || tok.raw == "#![";
            if !combined && tok.raw != "#" {
                return idx;
            }

            if combined {
                idx += 1;
            } else {
                idx += 1; // '#'
                idx = self.skip_trivia_from(idx);
                if self.tokens.get(idx).is_some_and(|t| t.raw == "!") {
                    idx += 1;
                    idx = self.skip_trivia_from(idx);
                }
                if self.tokens.get(idx).is_some_and(|t| t.raw == "[") {
                    idx += 1;
                } else {
                    return idx;
                }
            }

            let mut depth = 1usize;
            while idx < self.tokens.len() && depth > 0 {
                if self.tokens[idx].raw == "[" {
                    depth += 1;
                } else if self.tokens[idx].raw == "]" {
                    depth = depth.saturating_sub(1);
                }
                idx += 1;
            }
        }
    }

    fn skip_visibility_from(&self, mut idx: usize) -> usize {
        let Some(tok) = self.tokens.get(idx) else {
            return idx;
        };
        if tok.normalized != "pub" && tok.raw != "pub" {
            return idx;
        }
        idx += 1;
        idx = self.skip_trivia_from(idx);
        if self.tokens.get(idx).is_some_and(|t| t.raw == "(") {
            idx += 1;
            let mut depth = 1usize;
            while idx < self.tokens.len() && depth > 0 {
                if self.tokens[idx].raw == "(" {
                    depth += 1;
                } else if self.tokens[idx].raw == ")" {
                    depth = depth.saturating_sub(1);
                }
                idx += 1;
            }
        }
        idx
    }

    fn nth_non_trivia_from(&self, mut idx: usize, n: usize) -> Option<&Classified> {
        let mut count = 0usize;
        while idx < self.tokens.len() {
            let tok = &self.tokens[idx];
            if !tok.is_trivia() {
                count += 1;
                if count == n {
                    return Some(tok);
                }
            }
            idx += 1;
        }
        None
    }

    fn parse_item_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        let mut toks: Vec<Token> = Vec::new();
        let mut idx_map: Vec<usize> = Vec::new();
        let mut brace_depth = 0usize;
        let mut saw_brace = false;
        for (rel, t) in self.tokens[self.idx..].iter().enumerate() {
            if t.is_trivia() {
                continue;
            }
            let raw = t.raw.as_str();
            if raw == "{" {
                brace_depth = brace_depth.saturating_add(1);
                saw_brace = true;
            } else if raw == "}" {
                brace_depth = brace_depth.saturating_sub(1);
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

            if brace_depth == 0 && (raw == ";" || (raw == "}" && saw_brace)) {
                break;
            }
        }
        if toks.is_empty() || idx_map.is_empty() {
            return Err(self.error("failed to parse item statement"));
        }

        let (item, consumed) =
            crate::cst::items::parse_item_tokens_prefix_to_cst_with_file(&toks, self.file)
                .map_err(|err| self.error(&format!("failed to parse item statement: {err}")))?;
        if consumed == 0 || consumed != toks.len() {
            return Err(self.error("failed to parse item statement"));
        }
        let last_idx = idx_map
            .get(consumed - 1)
            .copied()
            .ok_or_else(|| self.error("failed to advance after item statement"))?;
        self.idx = last_idx + 1;

        children.push(SyntaxElement::Node(Box::new(item)));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some(";") {
            self.bump_token_into(&mut children);
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtItem, children, span))
    }

    fn parse_let_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `let`
        self.bump_trivia_into(&mut children);

        let pat = self.parse_pattern()?;
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);

        // Optional type annotation `: <type>`.
        if self.peek_non_trivia_raw() == Some(":") {
            self.bump_token_into(&mut children);
            let ty = self.parse_type_node_until(&["=", ";"])?;
            children.push(SyntaxElement::Node(Box::new(ty)));
            self.bump_trivia_into(&mut children);
        }

        // initializer (optional when ended by semicolon).
        if self.peek_non_trivia_raw() == Some("=") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let init = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(init)));
            self.bump_trivia_into(&mut children);
        }

        if self.peek_non_trivia_raw() == Some("else")
            || self.peek_non_trivia_normalized() == Some("else")
        {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let diverge = self.parse_block_expr()?;
            children.push(SyntaxElement::Node(Box::new(diverge)));
            self.bump_trivia_into(&mut children);
        }

        self.expect_token_raw(";")?;
        self.bump_token_into(&mut children);

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtLet, children, span))
    }

    fn parse_expr_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        let expr = match self.peek_non_trivia_normalized() {
            Some("if" | "for" | "while" | "loop" | "match" | "const") => {
                let expr = self.parse_prefix(true)?;
                self.parse_postfix(expr, true)?
            }
            _ => self.parse_expr_bp(0)?,
        };
        children.push(SyntaxElement::Node(Box::new(expr)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(";") {
            self.bump_token_into(&mut children);
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtExpr, children, span))
    }

    fn parse_defer_stmt(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `defer`
        self.bump_trivia_into(&mut children);
        let expr = self.parse_expr_bp(0)?;
        children.push(SyntaxElement::Node(Box::new(expr)));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw(";")?;
        self.bump_token_into(&mut children);

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::BlockStmtDefer, children, span))
    }

    fn parse_quote_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `quote`
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("<") {
            self.expect_token_raw("<")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("[") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                match self.peek_non_trivia_token_kind() {
                    Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                        self.bump_token_into(&mut children);
                    }
                    _ => return Err(self.error("expected quote fragment kind")),
                }
                self.bump_trivia_into(&mut children);
                self.expect_token_raw("]")?;
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                self.expect_token_raw(">")?;
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            } else {
                let kind_text = match self.peek_non_trivia_token_kind() {
                    Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                        let text = self.peek_non_trivia_raw().unwrap_or_default().to_string();
                        self.bump_token_into(&mut children);
                        text
                    }
                    _ => return Err(self.error("expected quote fragment kind")),
                };
                self.bump_trivia_into(&mut children);
                if kind_text == "expr" && self.peek_non_trivia_raw() == Some("<") {
                    self.bump_token_into(&mut children); // '<'
                    self.bump_trivia_into(&mut children);
                    let ty = self.parse_type_node_until(&[">"])?;
                    children.push(SyntaxElement::Node(Box::new(ty)));
                    self.bump_trivia_into(&mut children);
                    self.expect_token_raw(">")?;
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
                self.expect_token_raw(">")?;
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }
        if self.peek_non_trivia_raw() == Some("{") {
            let block = self.parse_block_expr()?;
            children.push(SyntaxElement::Node(Box::new(block)));
            let span = span_for_children(&children);
            Ok(SyntaxNode::new(SyntaxKind::ExprQuote, children, span))
        } else {
            let span = span_for_children(&children);
            Ok(SyntaxNode::new(SyntaxKind::ExprQuoteToken, children, span))
        }
    }

    fn parse_splice_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprSplice, children, span))
    }

    fn parse_async_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `async`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprAsync, children, span))
    }

    fn parse_gen_block_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `gen`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprBlock, children, span))
    }

    fn parse_const_block_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `const`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprConstBlock, children, span))
    }

    fn parse_if_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `if`
        self.bump_trivia_into(&mut children);
        let cond = self.parse_condition_expr_before_block()?;
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprIf, children, span))
    }

    fn parse_try_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `try`
        self.bump_trivia_into(&mut children);

        let body = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(body)));
        self.bump_trivia_into(&mut children);

        let mut saw_clause = false;
        loop {
            match self.peek_non_trivia_normalized() {
                Some("catch") => {
                    saw_clause = true;
                    let catch = self.parse_try_catch_clause()?;
                    children.push(SyntaxElement::Node(Box::new(catch)));
                    self.bump_trivia_into(&mut children);
                }
                Some("else") => {
                    saw_clause = true;
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    let elze = self.parse_block_expr()?;
                    children.push(SyntaxElement::Node(Box::new(elze)));
                    self.bump_trivia_into(&mut children);
                }
                Some("finally") => {
                    saw_clause = true;
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    let finally = self.parse_block_expr()?;
                    children.push(SyntaxElement::Node(Box::new(finally)));
                    self.bump_trivia_into(&mut children);
                }
                _ => break,
            }
        }

        if !saw_clause {
            return Err(self.error("expected catch, else, or finally after try block"));
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprTry, children, span))
    }

    fn parse_try_catch_clause(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_token_into(&mut children); // `catch`
        self.bump_trivia_into(&mut children);

        match self.peek_non_trivia_raw() {
            Some("{") => {}
            Some(_) => {
                let pat = self.parse_pattern()?;
                children.push(SyntaxElement::Node(Box::new(pat)));
                self.bump_trivia_into(&mut children);
            }
            None => return Err(self.error("unexpected end of input after catch")),
        }

        let body = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(body)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprTryCatch, children, span))
    }

    fn parse_loop_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `loop`
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprLoop, children, span))
    }

    fn parse_while_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `while`
        self.bump_trivia_into(&mut children);
        let cond = self.parse_condition_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(cond)));
        self.bump_trivia_into(&mut children);
        let block = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(block)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprWhile, children, span))
    }

    fn parse_with_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `with`
        self.bump_trivia_into(&mut children);
        let context = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(context)));
        self.bump_trivia_into(&mut children);
        let body = self.parse_block_expr()?;
        children.push(SyntaxElement::Node(Box::new(body)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprWith, children, span))
    }

    fn parse_let_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `let`
        self.bump_trivia_into(&mut children);

        let pat = self.parse_pattern()?;
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(":") {
            self.bump_token_into(&mut children);
            let ty = self.parse_type_node_until(&["="])?;
            children.push(SyntaxElement::Node(Box::new(ty)));
            self.bump_trivia_into(&mut children);
        }

        self.expect_token_raw("=")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        let init = self.parse_expr_before_block()?;
        children.push(SyntaxElement::Node(Box::new(init)));

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprLet, children, span))
    }

    fn parse_pattern(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        let mut pat = self.parse_pattern_atom()?;
        self.bump_trivia_into(&mut pat.children);

        let is_split_range = self.peek_non_trivia_raw() == Some("..")
            && self
                .peek_nth_non_trivia(1)
                .is_some_and(|tok| tok.raw.as_str() == "=");
        let is_split_dotdot = {
            let mut i = self.idx;
            while self.tokens.get(i).is_some_and(|t| t.is_trivia()) {
                i += 1;
            }
            self.tokens.get(i).is_some_and(|t| t.raw.as_str() == ".")
                && self
                    .tokens
                    .get(i + 1)
                    .is_some_and(|t| !t.is_trivia() && t.raw.as_str() == ".")
        };
        if matches!(self.peek_non_trivia_raw(), Some("..") | Some("..="))
            || is_split_range
            || is_split_dotdot
        {
            let start = self
                .coerce_pattern_range_start(pat)
                .ok_or_else(|| self.error("invalid range pattern start"))?;
            pat = self.parse_range_pattern(Some(start))?;
            self.bump_trivia_into(&mut pat.children);
        }

        if self.peek_non_trivia_raw() == Some("@") {
            let mut children = Vec::new();
            children.push(SyntaxElement::Node(Box::new(pat)));
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // '@'
            self.bump_trivia_into(&mut children);
            let rhs = self.parse_pattern()?;
            children.push(SyntaxElement::Node(Box::new(rhs)));
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::PatternBind, children, span));
        }

        Ok(pat)
    }

    fn parse_pattern_atom(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("&") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_normalized() == Some("mut") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
            let inner = self.parse_pattern()?;
            children.push(SyntaxElement::Node(Box::new(inner)));
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::PatternRef, children, span));
        }

        if self.peek_non_trivia_normalized() == Some("ref") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_normalized() == Some("mut") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        } else if self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_normalized() == Some("ref") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }

        if self.peek_non_trivia_normalized() == Some("box") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let inner = self.parse_pattern()?;
            children.push(SyntaxElement::Node(Box::new(inner)));
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::PatternBox, children, span));
        }

        match self.peek_non_trivia_raw() {
            Some("_") => {
                self.bump_token_into(&mut children);
                let span = span_for_children(&children);
                Ok(SyntaxNode::new(SyntaxKind::PatternWildcard, children, span))
            }
            Some("..") => {
                if matches!(
                    self.peek_nth_non_trivia(1).map(|tok| tok.raw.as_str()),
                    Some("," | ")" | "]" | "}")
                ) {
                    self.bump_token_into(&mut children);
                    let span = span_for_children(&children);
                    Ok(SyntaxNode::new(SyntaxKind::PatternRest, children, span))
                } else {
                    self.parse_range_pattern(None)
                }
            }
            Some("..=") => self.parse_range_pattern(None),
            Some("(") => self.parse_tuple_or_paren_pattern(children),
            Some("[") => self.parse_slice_pattern(children),
            _ => match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Number) => {
                    let node = self.parse_number()?;
                    Ok(node)
                }
                Some(TokenKind::StringLiteral) => {
                    let node = self.parse_string()?;
                    Ok(node)
                }
                _ => self.parse_path_or_ident_pattern(children),
            },
        }
    }

    fn parse_tuple_or_paren_pattern(
        &mut self,
        mut children: Vec<SyntaxElement>,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some(")") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::PatternTuple, children, span));
        }

        let first = self.parse_pattern()?;
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        let mut is_tuple = false;
        if self.peek_non_trivia_raw() == Some(",") {
            is_tuple = true;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            while self.peek_non_trivia_raw() != Some(")") {
                let next = self.parse_pattern()?;
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
        }

        if !is_tuple && self.peek_non_trivia_normalized() == Some("if") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let guard = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(guard)));
            self.bump_trivia_into(&mut children);
        }

        self.expect_token_raw(")")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        if is_tuple {
            Ok(SyntaxNode::new(SyntaxKind::PatternTuple, children, span))
        } else {
            Ok(SyntaxNode::new(SyntaxKind::PatternParen, children, span))
        }
    }

    fn parse_slice_pattern(
        &mut self,
        mut children: Vec<SyntaxElement>,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        self.bump_token_into(&mut children); // '['
        self.bump_trivia_into(&mut children);
        while self.peek_non_trivia_raw() != Some("]") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated slice pattern"));
            }
            let pat = self.parse_pattern()?;
            children.push(SyntaxElement::Node(Box::new(pat)));
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some(",") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some("]") {
                    break;
                }
                continue;
            }
            break;
        }
        self.expect_token_raw("]")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::PatternSlice, children, span))
    }

    fn parse_path_or_ident_pattern(
        &mut self,
        mut children: Vec<SyntaxElement>,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut path_children = Vec::new();
        path_children.append(&mut children);

        let mut saw_colon = false;
        if self.peek_non_trivia_raw() == Some("::") {
            self.bump_token_into(&mut path_children);
            self.bump_trivia_into(&mut path_children);
            saw_colon = true;
        }

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                self.bump_token_into(&mut path_children)
            }
            _ => return Err(self.error("expected pattern")),
        }
        self.bump_trivia_into(&mut path_children);

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        if self.peek_non_trivia_raw() == Some("::")
            && self
                .peek_nth_non_trivia(1)
                .is_some_and(|tok| tok.raw.as_str() == "<")
        {
            self.bump_token_into(&mut path_children);
            self.bump_trivia_into(&mut path_children);
            self.parse_generic_args_into(&mut path_children)?;
            self.bump_trivia_into(&mut path_children);
        }
        if self.peek_non_trivia_raw() == Some("<") {
            self.parse_generic_args_into(&mut path_children)?;
            self.bump_trivia_into(&mut path_children);
        }

        while self.peek_non_trivia_raw() == Some("::") {
            saw_colon = true;
            self.bump_token_into(&mut path_children);
            self.bump_trivia_into(&mut path_children);
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut path_children)?;
                self.bump_trivia_into(&mut path_children);
                continue;
            }
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut path_children)
                }
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut path_children);
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("::")
                && self
                    .peek_nth_non_trivia(1)
                    .is_some_and(|tok| tok.raw.as_str() == "<")
            {
                self.bump_token_into(&mut path_children);
                self.bump_trivia_into(&mut path_children);
                self.parse_generic_args_into(&mut path_children)?;
                self.bump_trivia_into(&mut path_children);
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut path_children)?;
                self.bump_trivia_into(&mut path_children);
            }
        }

        let path_span = span_for_children(&path_children);
        let path_node = SyntaxNode::new(SyntaxKind::PatternPath, path_children, path_span);

        match self.peek_non_trivia_raw() {
            Some("{") => self.parse_struct_pattern(path_node),
            Some("(") => self.parse_tuple_struct_pattern(path_node),
            _ => {
                if saw_colon {
                    Ok(path_node)
                } else {
                    // Single-segment path: treat as identifier pattern.
                    Ok(SyntaxNode::new(
                        SyntaxKind::PatternIdent,
                        path_node.children,
                        path_node.span,
                    ))
                }
            }
        }
    }

    fn coerce_pattern_range_start(&self, node: SyntaxNode) -> Option<SyntaxNode> {
        match node.kind {
            SyntaxKind::ExprNumber | SyntaxKind::ExprString | SyntaxKind::ExprName => Some(node),
            SyntaxKind::PatternIdent => Some(SyntaxNode::new(
                SyntaxKind::ExprName,
                node.children,
                node.span,
            )),
            SyntaxKind::PatternPath => Some(SyntaxNode::new(
                SyntaxKind::ExprPath,
                node.children,
                node.span,
            )),
            _ => None,
        }
    }

    fn parse_range_pattern(
        &mut self,
        start: Option<SyntaxNode>,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _span_start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));

        if let Some(start) = start {
            children.push(SyntaxElement::Node(Box::new(start)));
            self.bump_trivia_into(&mut children);
        }

        let split_dotdot = {
            let mut i = self.idx;
            while self.tokens.get(i).is_some_and(|t| t.is_trivia()) {
                i += 1;
            }
            if self.tokens.get(i).is_some_and(|t| t.raw.as_str() == ".") {
                let j = i + 1;
                if self
                    .tokens
                    .get(j)
                    .is_some_and(|t| !t.is_trivia() && t.raw.as_str() == ".")
                {
                    Some((i, j))
                } else {
                    None
                }
            } else {
                None
            }
        };

        if !matches!(self.peek_non_trivia_raw(), Some("..") | Some("..=")) && split_dotdot.is_none()
        {
            return Err(self.error("expected range operator"));
        }
        if let Some((first_idx, second_idx)) = split_dotdot {
            let mut op_text = "..";
            let mut last_idx = second_idx;
            let third_idx = second_idx + 1;
            if self
                .tokens
                .get(third_idx)
                .is_some_and(|t| !t.is_trivia() && t.raw.as_str() == "=")
            {
                op_text = "..=";
                last_idx = third_idx;
            }
            let start_span = self.tokens[first_idx].span;
            let end_span = self.tokens[last_idx].span;
            let span = Span::new(self.file, start_span.lo, end_span.hi);
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: op_text.to_string(),
                span,
            }));
            self.idx = last_idx + 1;
            self.bump_trivia_into(&mut children);
        } else if self.peek_non_trivia_raw() == Some("..") {
            self.bump_token_into(&mut children); // ".."
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("=") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        } else {
            self.bump_token_into(&mut children); // "..="
            self.bump_trivia_into(&mut children);
        }

        if let Some(raw) = self.peek_non_trivia_raw() {
            if !matches!(raw, "," | ")" | "]" | "}" | ";" | "if") {
                let end = self.parse_expr_bp(0)?;
                children.push(SyntaxElement::Node(Box::new(end)));
                self.bump_trivia_into(&mut children);
            }
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprRange, children, span))
    }

    fn parse_struct_pattern(
        &mut self,
        path_node: SyntaxNode,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = vec![SyntaxElement::Node(Box::new(path_node))];
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '{'
        self.bump_trivia_into(&mut children);
        while self.peek_non_trivia_raw() != Some("}") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated struct pattern"));
            }
            if self.peek_non_trivia_raw() == Some("..") {
                let mut rest_children = Vec::new();
                self.bump_token_into(&mut rest_children);
                let rest_span = span_for_children(&rest_children);
                let rest =
                    SyntaxNode::new(SyntaxKind::PatternRest, rest_children, rest_span);
                children.push(SyntaxElement::Node(Box::new(rest)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
                continue;
            }

            let mut field_children = Vec::new();
            self.bump_trivia_into(&mut field_children);
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) | Some(TokenKind::Number) => {
                    self.bump_token_into(&mut field_children)
                }
                _ => return Err(self.error("expected field name in struct pattern")),
            }
            self.bump_trivia_into(&mut field_children);
            if self.peek_non_trivia_raw() == Some(":") {
                self.bump_token_into(&mut field_children);
                self.bump_trivia_into(&mut field_children);
                let pat = self.parse_pattern()?;
                field_children.push(SyntaxElement::Node(Box::new(pat)));
            }
            let span = span_for_children(&field_children);
            children.push(SyntaxElement::Node(Box::new(SyntaxNode::new(
                SyntaxKind::StructField,
                field_children,
                span,
            ))));

            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some(",") {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some("}") {
                    break;
                }
                continue;
            }
            break;
        }
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::PatternStruct, children, span))
    }

    fn parse_tuple_struct_pattern(
        &mut self,
        path_node: SyntaxNode,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = vec![SyntaxElement::Node(Box::new(path_node))];
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);
        while self.peek_non_trivia_raw() != Some(")") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated tuple-struct pattern"));
            }
            let pat = self.parse_pattern()?;
            children.push(SyntaxElement::Node(Box::new(pat)));
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(
            SyntaxKind::PatternTupleStruct,
            children,
            span,
        ))
    }

    fn parse_for_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprFor, children, span))
    }

    fn parse_match_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprMatch, children, span))
    }

    fn parse_match_arm(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        let pat = self.parse_pattern()?;
        children.push(SyntaxElement::Node(Box::new(pat)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("if") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let guard = self.parse_guard_condition_expr()?;
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::MatchArm, children, span))
    }

    fn parse_closure_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        loop {
            match self.peek_non_trivia_normalized() {
                Some("async") | Some("gen") | Some("static") | Some("move") => {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                }
                _ => break,
            }
        }
        let is_double_bar = self.peek_non_trivia_raw() == Some("||");
        if is_double_bar {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        } else {
            self.expect_token_raw("|")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        // params until '|'
        if !is_double_bar && self.peek_non_trivia_raw() != Some("|") {
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
        if !is_double_bar {
            self.expect_token_raw("|")?;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        let body = if self.peek_non_trivia_raw() == Some("{") {
            self.parse_block_expr()?
        } else {
            self.parse_expr_bp(0)?
        };
        children.push(SyntaxElement::Node(Box::new(body)));
        let span = span_for_children(&children);
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
            let span = span_for_children(&typed_children);
            pat_node = SyntaxNode::new(SyntaxKind::PatternType, typed_children, span);
        }

        out.push(SyntaxElement::Node(Box::new(pat_node)));
        Ok(())
    }

    fn parse_return_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `return`
        self.bump_trivia_into(&mut children);
        if !matches!(
            self.peek_non_trivia_raw(),
            Some(";") | Some("}") | Some(")") | Some(",") | Some("=>")
        ) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprReturn, children, span))
    }

    fn parse_become_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `become`
        self.bump_trivia_into(&mut children);
        if !matches!(
            self.peek_non_trivia_raw(),
            Some(";") | Some("}") | Some(")") | Some(",") | Some("=>")
        ) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprReturn, children, span))
    }

    fn parse_yield_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `yield`
        self.bump_trivia_into(&mut children);
        if !matches!(
            self.peek_non_trivia_raw(),
            Some(";") | Some("}") | Some(")") | Some(",") | Some("=>")
        ) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprYield, children, span))
    }

    fn parse_break_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `break`
        self.bump_trivia_into(&mut children);
        if !matches!(
            self.peek_non_trivia_raw(),
            Some(";") | Some("}") | Some(")") | Some(",") | Some("=>")
        ) {
            let expr = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprBreak, children, span))
    }

    fn parse_continue_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `continue`
        let span = span_for_children(&children);
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
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::ExprAwait, children, span));
        }

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Number) | Some(TokenKind::Keyword(_)) => {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some("::<") {
                    self.split_turbofish();
                }
                if self.peek_non_trivia_raw() == Some("::")
                    && self
                        .peek_nth_non_trivia(1)
                        .is_some_and(|tok| tok.raw.as_str() == "<")
                {
                    let turbofish = self.parse_turbofish_args_node()?;
                    children.push(SyntaxElement::Node(Box::new(turbofish)));
                }
                let span = span_for_children(&children);
                Ok(SyntaxNode::new(SyntaxKind::ExprSelect, children, span))
            }
            _ => {
                self.idx = snapshot;
                Err(self.error("expected field name after '.'"))
            }
        }
    }

    fn parse_turbofish_args_node(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        self.expect_token_raw("::")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        self.parse_generic_args_into(&mut children)?;
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span))
    }

    fn parse_turbofish_postfix(
        &mut self,
        base: SyntaxNode,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = base.children.clone();
        let turbofish = self.parse_turbofish_args_node()?;
        children.push(SyntaxElement::Node(Box::new(turbofish)));
        let span = span_for_children(&children);
        let kind = if base.kind == SyntaxKind::ExprName {
            SyntaxKind::ExprPath
        } else {
            base.kind
        };
        Ok(SyntaxNode::new(kind, children, span))
    }

    fn parse_call(&mut self, callee: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(callee)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() != Some(")") {
            let mut saw_kwarg = false;
            loop {
                let (arg, is_kwarg) = self.parse_call_arg()?;
                if saw_kwarg && !is_kwarg {
                    return Err(self.error("positional argument after keyword argument"));
                }
                saw_kwarg |= is_kwarg;
                children.push(SyntaxElement::Node(Box::new(arg)));
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

        if self.peek_non_trivia_raw() != Some(")") {
            return Err(self.error("expected ')' to close call"));
        }
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprCall, children, span))
    }

    fn parse_call_arg(&mut self) -> Result<(SyntaxNode, bool), ExprCstParseError> {
        if self.peek_non_trivia_raw() == Some("**") {
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // '**'
            self.bump_trivia_into(&mut children);
            let value = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(value)));
            let span = span_for_children(&children);
            return Ok((SyntaxNode::new(SyntaxKind::ExprSplatDict, children, span), true));
        }
        if self.peek_non_trivia_raw() == Some("*") {
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // '*'
            self.bump_trivia_into(&mut children);
            let value = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(value)));
            let span = span_for_children(&children);
            return Ok((SyntaxNode::new(SyntaxKind::ExprSplat, children, span), false));
        }
        if self.peek_non_trivia_token_kind() == Some(TokenKind::Ident)
            && self
                .peek_nth_non_trivia(2)
                .is_some_and(|tok| tok.raw.as_str() == "=")
        {
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // name
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // '='
            self.bump_trivia_into(&mut children);
            let value = self.parse_expr_bp(0)?;
            children.push(SyntaxElement::Node(Box::new(value)));
            let span = span_for_children(&children);
            return Ok((SyntaxNode::new(SyntaxKind::ExprKwArg, children, span), true));
        }

        let expr = self.parse_expr_bp(0)?;
        Ok((expr, false))
    }

    fn parse_index(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        children.push(SyntaxElement::Node(Box::new(base)));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '['
        self.bump_trivia_into(&mut children);
        let index = if matches!(self.peek_non_trivia_raw(), Some("..") | Some("..=")) {
            self.parse_range_in_index(None)?
        } else {
            let start = self.parse_expr_bp(0)?;
            if matches!(self.peek_non_trivia_raw(), Some("..") | Some("..=")) {
                self.parse_range_in_index(Some(start))?
            } else {
                start
            }
        };
        children.push(SyntaxElement::Node(Box::new(index)));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() != Some("]") {
            return Err(self.error("expected ']' to close index"));
        }
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprIndex, children, span))
    }

    fn parse_range_in_index(
        &mut self,
        start: Option<SyntaxNode>,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_range_expr_core(start, &["],"], false)
    }

    fn parse_range_from_paren(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_range_expr_core(None, &[",", ")"], true)
    }

    fn parse_range_prefix_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        self.parse_range_expr_core(None, &[",", ")", "]", "}", ";"], true)
    }

    fn parse_range_expr_core(
        &mut self,
        start: Option<SyntaxNode>,
        end_stops: &[&str],
        include_leading_trivia: bool,
    ) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _span_start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));

        if let Some(start) = start {
            children.push(SyntaxElement::Node(Box::new(start)));
            self.bump_trivia_into(&mut children);
        } else if include_leading_trivia {
            self.bump_trivia_into(&mut children);
        }

        self.bump_token_into(&mut children); // ".." or "..="
        self.bump_trivia_into(&mut children);

        if let Some(raw) = self.peek_non_trivia_raw() {
            if !end_stops.iter().any(|stop| *stop == raw) {
                let end = self.parse_expr_bp(0)?;
                children.push(SyntaxElement::Node(Box::new(end)));
                self.bump_trivia_into(&mut children);
            }
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprRange, children, span))
    }

    fn try_parse_keyword_prefix_expr(
        &mut self,
    ) -> Option<Result<SyntaxNode, ExprCstParseError>> {
        let next = self.peek_second_non_trivia_raw();
        let result = match self.peek_non_trivia_normalized() {
            Some("gen") if next == Some("{") => self.parse_gen_block_expr(),
            Some("gen")
                if matches!(next, Some("|") | Some("||") | Some("move") | Some("static")) =>
            {
                self.parse_closure_expr()
            }
            Some("unsafe") if next == Some("{") => self.parse_unsafe_block_expr(),
            Some("quote") => self.parse_quote_expr(),
            Some("splice") => self.parse_splice_expr(),
            Some("async")
                if matches!(next, Some("|") | Some("||") | Some("move") | Some("static")) =>
            {
                self.parse_closure_expr()
            }
            Some("async") => self.parse_async_expr(),
            Some("const") if next == Some("{") => self.parse_const_block_expr(),
            Some("static")
                if matches!(next, Some("|") | Some("||") | Some("move") | Some("async") | Some("gen")) =>
            {
                self.parse_closure_expr()
            }
            Some("move") if matches!(next, Some("|") | Some("||")) => self.parse_closure_expr(),
            Some("if") => self.parse_if_expr(),
            Some("try") => self.parse_try_expr(),
            Some("loop") => self.parse_loop_expr(),
            Some("while") => self.parse_while_expr(),
            Some("with") => self.parse_with_expr(),
            Some("for") => self.parse_for_expr(),
            Some("match") => self.parse_match_expr(),
            Some("let") => self.parse_let_expr(),
            Some("struct") if next == Some("{") => self.parse_structural_literal_expr(),
            Some("return") => self.parse_return_expr(),
            Some("become") => self.parse_become_expr(),
            Some("yield") => self.parse_yield_expr(),
            Some("break") => self.parse_break_expr(),
            Some("continue") => self.parse_continue_expr(),
            _ => return None,
        };
        Some(result)
    }

    fn parse_macro_call(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        // Only support simple `ident!(...)` / `ident!{...}` / `ident![...]` forms for now.
        if !matches!(
            base.kind,
            SyntaxKind::ExprName | SyntaxKind::ExprPath | SyntaxKind::ExprSelect
        ) {
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

        let span = span_for_children(&children);
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
            if op == "::<" {
                self.split_turbofish();
                continue;
            }
            if op == ">>" && stops.iter().any(|s| *s == ">") {
                self.split_right_shift();
                continue;
            }
            if stops.iter().any(|s| *s == op) {
                break;
            }
            if op == "=>" {
                break;
            }
            if op == "?" {
                let mut children = Vec::new();
                children.push(SyntaxElement::Node(Box::new(left)));
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children);
                let span = span_for_children(&children);
                left = SyntaxNode::new(SyntaxKind::TyOptional, children, span);
                continue;
            }
            if op == "::"
                && matches!(left.kind, SyntaxKind::TyPath)
                && self
                    .peek_nth_non_trivia(1)
                    .is_some_and(|tok| tok.raw.as_str() == "<")
            {
                let mut children = left.children.clone();
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                self.parse_generic_args_into(&mut children)?;
                let span = span_for_children(&children);
                left = SyntaxNode::new(SyntaxKind::TyPath, children, span);
                continue;
            }
            if op == "<" && matches!(left.kind, SyntaxKind::TyPath) {
                let mut children = left.children.clone();
                self.parse_generic_args_into(&mut children)?;
                let span = span_for_children(&children);
                left = SyntaxNode::new(SyntaxKind::TyPath, children, span);
                continue;
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
            let span = span_for_children(&children);
            left = SyntaxNode::new(SyntaxKind::TyBinary, children, span);
        }

        Ok(left)
    }

    fn parse_type_atom(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        let raw = self.peek_non_trivia_raw();
        let normalized = self.peek_non_trivia_normalized();
        if raw.as_deref() == Some("!") {
            return self.parse_type_value(start);
        }
        if raw.as_deref() == Some("...") {
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children);
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span));
        }
        if let Some(tok) = raw.as_deref() {
            if tok.starts_with('"')
                || (tok.starts_with('\'') && tok.ends_with('\'') && tok.len() >= 2)
                || tok.chars().next().is_some_and(|c| c.is_ascii_digit())
            {
                return self.parse_type_value(start);
            }
        }
        if matches!(normalized.as_deref(), Some("true" | "false" | "null")) {
            return self.parse_type_value(start);
        }
        if matches!(normalized.as_deref(), Some("const"))
            && self.peek_second_non_trivia_raw() == Some("{")
        {
            let expr = self.parse_const_block_expr()?;
            let span = expr.span;
            return Ok(SyntaxNode::new(
                SyntaxKind::TyExpr,
                vec![SyntaxElement::Node(Box::new(expr))],
                span,
            ));
        }
        if matches!(normalized.as_deref(), Some("unsafe"))
            && self.peek_second_non_trivia_raw() == Some("<")
        {
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            self.bump_token_into(&mut children); // `unsafe`
            self.parse_balanced_group_into("<", ">", &mut children)?;
            self.bump_trivia_into(&mut children);
            let inner = self.parse_type_atom(stops)?;
            children.push(SyntaxElement::Node(Box::new(inner)));
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::TyUnsafeBinder, children, span));
        }
        if matches!(normalized.as_deref(), Some("for"))
            && self.peek_second_non_trivia_raw() == Some("<")
        {
            return self.parse_for_binder_type(stops);
        }
        if self.looks_like_fn_type() {
            return self.parse_fn_type();
        }

        match normalized.as_deref() {
            Some("_") => {
                let mut children = Vec::new();
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children);
                let span = span_for_children(&children);
                Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span))
            }
            Some("(") => self.parse_paren_type(stops),
            Some("&") => self.parse_ref_type(stops),
            Some("*") => self.parse_ptr_type(stops),
            Some("[") => self.parse_array_or_slice_type(),
            Some("<") => self.parse_qualified_path_type(),
            Some("fn") => {
                // Treat `fn` as a plain type name when it is not a function signature.
                let mut children = Vec::new();
                self.bump_trivia_into(&mut children);
                self.bump_token_into(&mut children);
                let span = span_for_children(&children);
                Ok(SyntaxNode::new(SyntaxKind::TyPath, children, span))
            }
            Some("dyn") => self.parse_dyn_traits_type(),
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

    fn parse_for_binder_type(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `for`
        self.parse_balanced_group_into("<", ">", &mut children)?;
        self.bump_trivia_into(&mut children);
        let inner = self.parse_type_atom(stops)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyUnsafeBinder, children, span))
    }

    fn parse_dyn_traits_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `dyn`
        self.bump_trivia_into(&mut children);

        let first = self.parse_relaxed_trait_bound()?;
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        while self.peek_non_trivia_raw() == Some("+") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let bound = self.parse_relaxed_trait_bound()?;
            children.push(SyntaxElement::Node(Box::new(bound)));
            self.bump_trivia_into(&mut children);
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyImplTraits, children, span))
    }

    fn parse_type_value(&mut self, _start: Span) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyValue, children, span))
    }

    fn parse_paren_type(&mut self, _stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("(")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(")") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children);
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
            let span = span_for_children(&children);
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
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("&")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let mut saw_mut = false;
        if self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            saw_mut = true;
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

        if !saw_mut && self.peek_non_trivia_normalized() == Some("mut") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        let inner = self.parse_type_atom(stops)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyRef, children, span))
    }

    fn parse_array_or_slice_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::TyArray, children, span));
        }

        self.expect_token_raw("]")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TySlice, children, span))
    }

    fn parse_ptr_type(&mut self, stops: &[&str]) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("*")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        match self.peek_non_trivia_normalized() {
            Some("mut") | Some("const") => {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
            _ => {
                return Err(self.error("raw pointer types require `*const` or `*mut`"));
            }
        }

        let inner = self.parse_type_bp_until(0, stops)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyPtr, children, span))
    }

    fn parse_fn_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_normalized() == Some("unsafe") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }
        if self.peek_non_trivia_normalized() == Some("extern") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_token_kind() == Some(TokenKind::StringLiteral) {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }
        self.expect_token_raw("fn")?;
        self.bump_token_into(&mut children); // `fn`
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("(")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() != Some(")") {
            loop {
                if self.peek_non_trivia_raw() == Some("...") {
                    let mut vararg_children = Vec::new();
                    self.bump_trivia_into(&mut vararg_children);
                    self.bump_token_into(&mut vararg_children);
                    let span = span_for_children(&vararg_children);
                    let vararg = SyntaxNode::new(SyntaxKind::TyUnknown, vararg_children, span);
                    children.push(SyntaxElement::Node(Box::new(vararg)));
                    self.bump_trivia_into(&mut children);
                    break;
                }

                let is_named_param = self.peek_non_trivia_token_kind() == Some(TokenKind::Ident)
                    || self.peek_non_trivia_normalized() == Some("_");
                if is_named_param
                    && self
                        .peek_nth_non_trivia(2)
                        .is_some_and(|t| t.raw.as_str() == ":")
                {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    self.bump_token_into(&mut children); // ':'
                    self.bump_trivia_into(&mut children);
                }

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

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyFn, children, span))
    }

    fn parse_impl_traits_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `impl`
        self.bump_trivia_into(&mut children);

        let bound = self.parse_relaxed_trait_bound()?;
        children.push(SyntaxElement::Node(Box::new(bound)));
        self.bump_trivia_into(&mut children);

        while self.peek_non_trivia_raw() == Some("+") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let bound = self.parse_relaxed_trait_bound()?;
            children.push(SyntaxElement::Node(Box::new(bound)));
            self.bump_trivia_into(&mut children);
        }

        // Support Rust-like `impl Fn(T) -> U` syntax by consuming the signature tokens.
        // At the moment this is represented as additional children on `TyImplTraits`.
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

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyImplTraits, children, span))
    }

    fn parse_relaxed_trait_bound(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        if matches!(self.peek_non_trivia_normalized().as_deref(), Some("for"))
            && self.peek_second_non_trivia_raw() == Some("<")
        {
            return self.parse_for_binder_type(&[]);
        }
        if self.peek_non_trivia_raw() != Some("?") {
            return self.parse_path_type();
        }
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // `?`
        self.bump_trivia_into(&mut children);
        let bound = self.parse_path_type()?;
        children.push(SyntaxElement::Node(Box::new(bound)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyOptional, children, span))
    }

    fn parse_structural_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
            if self.peek_non_trivia_raw() == Some("..") {
                // Support type-level structural updates: `struct { ..Other, field: Ty }`.
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                let ty = self.parse_type_bp_until(0, &[",", "}"])?;
                children.push(SyntaxElement::Node(Box::new(ty)));
                self.bump_trivia_into(&mut children);
                if self.peek_non_trivia_raw() == Some(",") {
                    self.bump_token_into(&mut children);
                    self.bump_trivia_into(&mut children);
                    continue;
                }
                continue;
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyStructural, children, span))
    }

    fn parse_structural_type_field(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_ident_token()?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        if self.peek_non_trivia_raw() == Some("?") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }
        self.expect_token_raw(":")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        let ty = self.parse_type_bp_until(0, &[",", "}"])?;
        children.push(SyntaxElement::Node(Box::new(ty)));
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyField, children, span))
    }

    fn parse_path_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("::") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) => self.bump_token_into(&mut children),
            Some(TokenKind::Keyword(_)) => self.bump_token_into(&mut children),
            _ => return Err(self.error("expected type path")),
        }
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        if self.peek_non_trivia_raw() == Some("::")
            && self
                .peek_nth_non_trivia(1)
                .is_some_and(|tok| tok.raw.as_str() == "<")
        {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            self.parse_generic_args_into(&mut children)?;
            self.bump_trivia_into(&mut children);
        }
        if self.peek_non_trivia_raw() == Some("<") {
            self.parse_generic_args_into(&mut children)?;
            self.bump_trivia_into(&mut children);
        }

        loop {
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() != Some("::") {
                break;
            }
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut children)?;
                continue;
            }
            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) => self.bump_token_into(&mut children),
                Some(TokenKind::Keyword(_)) => self.bump_token_into(&mut children),
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
        }

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        if self.peek_non_trivia_raw() == Some("::")
            && self
                .peek_nth_non_trivia(1)
                .is_some_and(|tok| tok.raw.as_str() == "<")
        {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            self.parse_generic_args_into(&mut children)?;
        }
        if self.peek_non_trivia_raw() == Some("<") {
            self.parse_generic_args_into(&mut children)?;
        }

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
                self.bump_trivia_into(&mut children);
            }
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

            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::TyMacroCall, children, span));
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyPath, children, span))
    }

    fn parse_macro_var_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("$")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => self.bump_token_into(&mut children),
            _ => return Err(self.error("expected identifier after '$'")),
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprName, children, span))
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
        let track_angle = open == "<" && close == ">";

        while !stack.is_empty() {
            self.bump_trivia_into(out);
            let Some(tok) = self.peek_non_trivia_raw() else {
                return Err(self.error("unexpected end of input in macro group"));
            };

            if track_angle && tok == ">>" {
                self.split_right_shift();
                continue;
            }

            if tok == "(" {
                stack.push(")");
            } else if tok == "{" {
                stack.push("}");
            } else if tok == "[" {
                stack.push("]");
            } else if track_angle && tok == "<" {
                stack.push(">");
            } else if tok == ")" || tok == "}" || tok == "]" || (track_angle && tok == ">") {
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
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '('
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(")") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::ExprUnit, children, span));
        }

        let first = if matches!(self.peek_non_trivia_raw(), Some("..") | Some("..=")) {
            self.parse_range_from_paren()?
        } else {
            self.parse_expr_bp(0)?
        };
        children.push(SyntaxElement::Node(Box::new(first)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some(",") {
            // Tuple
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);

            while self.peek_non_trivia_raw() != Some(")") {
                let next = if matches!(self.peek_non_trivia_raw(), Some("..") | Some("..=")) {
                    self.parse_range_from_paren()?
                } else {
                    self.parse_expr_bp(0)?
                };
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
            let span = span_for_children(&children);
            return Ok(SyntaxNode::new(SyntaxKind::ExprTuple, children, span));
        }

        self.expect_token_raw(")")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprParen, children, span))
    }

    fn parse_array_literal_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children); // '['
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("]") {
            self.bump_token_into(&mut children);
            let span = span_for_children(&children);
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
            let span = span_for_children(&children);
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprArray, children, span))
    }

    fn parse_structural_literal_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
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
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprStructural, children, span))
    }

    fn parse_struct_literal(&mut self, base: SyntaxNode) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = base.span;
        children.push(SyntaxElement::Node(Box::new(base)));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("{")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        self.parse_struct_fields_into(&mut children)?;
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("}")?;
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
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
                if self.peek_non_trivia_raw() == Some("}") {
                    break;
                }
                continue;
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
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        // name
        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) | Some(TokenKind::Number) => {}
            _ => return Err(self.error("expected identifier")),
        }
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

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::StructField, children, span))
    }

    fn parse_name_or_path(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);

        let mut is_path = false;
        if self.peek_non_trivia_raw() == Some("::") {
            is_path = true;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
        }

        match self.peek_non_trivia_token_kind() {
            Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                self.bump_token_into(&mut children)
            }
            _ => return Err(self.error("expected identifier")),
        }
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }

        loop {
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() != Some("::") {
                break;
            }
            is_path = true;
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut children)?;
                continue;
            }

            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut children)
                }
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
        }

        let span = span_for_children(&children);
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

    fn parse_qualified_path_expr(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("<")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let ty = self.parse_type_bp_until(0, &["as", ">"])?;
        children.push(SyntaxElement::Node(Box::new(ty)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("as") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let trait_ty = self.parse_type_bp_until(0, &[">"])?;
            children.push(SyntaxElement::Node(Box::new(trait_ty)));
            self.bump_trivia_into(&mut children);
        }

        if self.peek_non_trivia_raw() == Some(">>") {
            self.split_right_shift();
        }
        self.expect_token_raw(">")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        self.expect_token_raw("::")?;
        loop {
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() != Some("::") {
                break;
            }
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);

            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut children)?;
                continue;
            }

            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut children)
                }
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprPath, children, span))
    }

    fn parse_lifetime_type_arg(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);
        let raw = self.peek_non_trivia_raw().unwrap_or("").to_string();
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);
        if raw == "'" {
            if matches!(
                self.peek_non_trivia_token_kind(),
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_))
            ) {
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
            }
        }
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span))
    }

    fn parse_generic_args_into(
        &mut self,
        out: &mut Vec<SyntaxElement>,
    ) -> Result<(), ExprCstParseError> {
        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        self.expect_token_raw("<")?;
        self.bump_token_into(out);
        self.bump_trivia_into(out);

        while self.peek_non_trivia_raw() != Some(">") {
            if self.peek_non_trivia_raw().is_none() {
                return Err(self.error("unterminated generic args"));
            }
            if self.peek_non_trivia_raw() == Some(">>") {
                self.split_right_shift();
                break;
            }

            if self
                .peek_non_trivia_raw()
                .is_some_and(|raw| raw == "'" || raw.starts_with('\''))
            {
                let lifetime = self.parse_lifetime_type_arg()?;
                out.push(SyntaxElement::Node(Box::new(lifetime)));
                self.bump_trivia_into(out);
                if self.peek_non_trivia_raw() == Some(",") {
                    if self.comma_looks_like_outer_field_separator() {
                        break;
                    }
                    self.bump_token_into(out);
                    self.bump_trivia_into(out);
                    continue;
                }
                break;
            }

            if self.peek_non_trivia_normalized() == Some("const") || self.peek_non_trivia_raw() == Some("{") {
                let arg = self.parse_unknown_generic_arg()?;
                out.push(SyntaxElement::Node(Box::new(arg)));
                self.bump_trivia_into(out);
                if self.peek_non_trivia_raw() == Some(",") {
                    if self.comma_looks_like_outer_field_separator() {
                        break;
                    }
                    self.bump_token_into(out);
                    self.bump_trivia_into(out);
                    continue;
                }
                break;
            }

            let snapshot = self.idx;
            if self.peek_non_trivia_token_kind() == Some(TokenKind::Ident) {
                self.bump_token_into(out);
                self.bump_trivia_into(out);
                if self.peek_non_trivia_raw() == Some("=") {
                    self.bump_token_into(out);
                    self.bump_trivia_into(out);
                } else {
                    self.idx = snapshot;
                }
            }

            let arg_ty = self.parse_type_bp_until(0, &[",", ">"])?;
            out.push(SyntaxElement::Node(Box::new(arg_ty)));
            self.bump_trivia_into(out);
            if self.peek_non_trivia_raw() == Some(",") {
                if self.comma_looks_like_outer_field_separator() {
                    break;
                }
                self.bump_token_into(out);
                self.bump_trivia_into(out);
                continue;
            }
            break;
        }
        if self.peek_non_trivia_raw() == Some(">>") {
            self.split_right_shift();
        }
        self.expect_token_raw(">")?;
        self.bump_token_into(out);
        self.bump_trivia_into(out);
        Ok(())
    }

    fn parse_unknown_generic_arg(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        self.bump_trivia_into(&mut children);

        loop {
            let Some(raw) = self.peek_non_trivia_raw() else {
                return Err(self.error("unterminated generic arg"));
            };
            if raw == "," || raw == ">" {
                break;
            }
            if raw == ">>" {
                self.split_right_shift();
                break;
            }
            match raw {
                "(" => self.parse_balanced_group_into("(", ")", &mut children)?,
                "[" => self.parse_balanced_group_into("[", "]", &mut children)?,
                "{" => self.parse_balanced_group_into("{", "}", &mut children)?,
                _ => self.bump_token_into(&mut children),
            }
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyUnknown, children, span))
    }

    fn parse_qualified_path_type(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.expect_token_raw("<")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        let ty = self.parse_type_bp_until(0, &["as", ">"])?;
        children.push(SyntaxElement::Node(Box::new(ty)));
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_normalized() == Some("as") {
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);
            let trait_ty = self.parse_type_bp_until(0, &[">"])?;
            children.push(SyntaxElement::Node(Box::new(trait_ty)));
            self.bump_trivia_into(&mut children);
        }

        if self.peek_non_trivia_raw() == Some(">>") {
            self.split_right_shift();
        }
        self.expect_token_raw(">")?;
        self.bump_token_into(&mut children);
        self.bump_trivia_into(&mut children);

        if self.peek_non_trivia_raw() == Some("::<") {
            self.split_turbofish();
        }
        self.expect_token_raw("::")?;
        loop {
            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() != Some("::") {
                break;
            }
            self.bump_token_into(&mut children);
            self.bump_trivia_into(&mut children);

            if self.peek_non_trivia_raw() == Some("::<") {
                self.split_turbofish();
            }
            if self.peek_non_trivia_raw() == Some("<") {
                self.parse_generic_args_into(&mut children)?;
                continue;
            }

            match self.peek_non_trivia_token_kind() {
                Some(TokenKind::Ident) | Some(TokenKind::Keyword(_)) => {
                    self.bump_token_into(&mut children)
                }
                _ => return Err(self.error("expected path segment")),
            }
            self.bump_trivia_into(&mut children);
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::TyPath, children, span))
    }

    fn parse_number(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprNumber, children, span))
    }

    fn parse_string(&mut self) -> Result<SyntaxNode, ExprCstParseError> {
        let mut children = Vec::new();
        let _start = self
            .peek_any_span()
            .unwrap_or_else(|| Span::new(self.file, 0, 0));
        self.bump_trivia_into(&mut children);
        self.bump_token_into(&mut children);
        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ExprString, children, span))
    }

    fn parse_outer_attrs_expr(&mut self) -> Result<Vec<SyntaxNode>, ExprCstParseError> {
        let mut attrs = Vec::new();
        loop {
            let combined = matches!(self.peek_non_trivia_raw(), Some("#[") | Some("#!["));
            if !combined && self.peek_non_trivia_raw() != Some("#") {
                break;
            }
            if combined {
                if self.peek_non_trivia_raw() == Some("#![") {
                    break;
                }
            } else if self.peek_second_non_trivia_raw() == Some("!") {
                break;
            }
            let mut children = Vec::new();
            self.bump_trivia_into(&mut children);
            if combined {
                self.bump_token_into(&mut children);
            } else {
                self.expect_token_raw("#")?;
                self.bump_token_into(&mut children);
                self.bump_trivia_into(&mut children);
                self.expect_token_raw("[")?;
                self.bump_token_into(&mut children);
            }

            let mut depth = 1usize;
            while depth > 0 {
                let Some(tok) = self.peek_non_trivia_raw() else {
                    return Err(self.error("unterminated attribute"));
                };
                match tok {
                    "[" => depth += 1,
                    "]" => depth = depth.saturating_sub(1),
                    _ => {}
                }
                self.bump_token_into(&mut children);
            }

            let span = span_for_children(&children);
            attrs.push(SyntaxNode::new(SyntaxKind::AttrOuter, children, span));
        }
        Ok(attrs)
    }

    fn consume_stmt_attrs(&mut self) -> Result<(), ExprCstParseError> {
        let mut scratch = Vec::new();
        loop {
            self.bump_trivia_into(&mut scratch);
            let combined = matches!(self.peek_non_trivia_raw(), Some("#[") | Some("#!["));
            if !combined && self.peek_non_trivia_raw() != Some("#") {
                break;
            }
            if combined {
                self.bump_token_into(&mut scratch);
            } else {
                self.bump_token_into(&mut scratch); // '#'
                self.bump_trivia_into(&mut scratch);
                if self.peek_non_trivia_raw() == Some("!") {
                    self.bump_token_into(&mut scratch);
                    self.bump_trivia_into(&mut scratch);
                }
                self.expect_token_raw("[")?;
                self.bump_token_into(&mut scratch);
            }

            let mut depth = 1usize;
            while depth > 0 {
                let Some(tok) = self.peek_non_trivia_raw() else {
                    return Err(self.error("unterminated attribute"));
                };
                match tok {
                    "[" => depth += 1,
                    "]" => depth = depth.saturating_sub(1),
                    _ => {}
                }
                self.bump_token_into(&mut scratch);
            }
        }
        Ok(())
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

    fn peek_non_trivia_is(&self, text: &str) -> bool {
        matches!(self.peek_non_trivia_normalized(), Some(value) if value == text)
            || matches!(self.peek_non_trivia_raw(), Some(value) if value == text)
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

    fn looks_like_ref_type_literal(&self) -> bool {
        if self.peek_non_trivia_raw() != Some("&") {
            return false;
        }
        let Some(second) = self.peek_nth_non_trivia(2) else {
            return false;
        };
        if !second.raw.starts_with('\'') {
            return false;
        }
        let Some(third) = self.peek_nth_non_trivia(3) else {
            return false;
        };
        matches!(third.kind, TokenKind::Ident | TokenKind::Keyword(_))
            || matches!(third.raw.as_str(), "(" | "[" | "&")
    }

    fn peek_nth_non_trivia_token_kind(&self, n: usize) -> Option<TokenKind> {
        self.peek_nth_non_trivia(n).map(|t| t.kind.clone())
    }

    fn looks_like_fn_type(&self) -> bool {
        let Some(first) = self.peek_nth_non_trivia(1) else {
            return false;
        };
        match first.normalized.as_str() {
            "fn" => self
                .peek_nth_non_trivia(2)
                .is_some_and(|t| t.raw.as_str() == "("),
            "unsafe" => {
                let Some(second) = self.peek_nth_non_trivia(2) else {
                    return false;
                };
                match second.normalized.as_str() {
                    "fn" => self
                        .peek_nth_non_trivia(3)
                        .is_some_and(|t| t.raw.as_str() == "("),
                    "extern" => self.looks_like_fn_type_after_extern(3),
                    _ => false,
                }
            }
            "extern" => self.looks_like_fn_type_after_extern(2),
            _ => false,
        }
    }

    fn looks_like_fn_type_after_extern(&self, idx: usize) -> bool {
        let Some(next) = self.peek_nth_non_trivia(idx) else {
            return false;
        };
        if next.kind == TokenKind::StringLiteral {
            return self
                .peek_nth_non_trivia(idx + 1)
                .is_some_and(|t| t.normalized.as_str() == "fn")
                && self
                    .peek_nth_non_trivia(idx + 2)
                    .is_some_and(|t| t.raw.as_str() == "(");
        }
        if next.normalized.as_str() == "fn" {
            return self
                .peek_nth_non_trivia(idx + 1)
                .is_some_and(|t| t.raw.as_str() == "(");
        }
        false
    }

    fn comma_looks_like_outer_field_separator(&self) -> bool {
        if self.peek_non_trivia_raw() != Some(",") {
            return false;
        }
        let Some(second) = self.peek_nth_non_trivia(2) else {
            return false;
        };
        if !matches!(second.kind, TokenKind::Ident) {
            return false;
        }
        self.peek_nth_non_trivia(3)
            .is_some_and(|third| third.raw.as_str() == ":")
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
            span: self.peek_non_trivia_span(),
        }
    }

    fn peek_non_trivia_span(&self) -> Option<Span> {
        self.peek_non_trivia().map(|t| t.span)
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
        "@" => (12, 13, SyntaxKind::ExprBinary),
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=" => {
            (5, 5, SyntaxKind::ExprBinary)
        } // right-associative
        _ => return None,
    };
    Some((l, r, kind))
}

fn is_assignment_operator(op: &str) -> bool {
    matches!(
        op,
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^="
    )
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
