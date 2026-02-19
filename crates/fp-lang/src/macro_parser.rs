use fp_core::ast::{Expr, MacroExpansionParser, MacroTokenTree, Ty};
use fp_core::error::Result;

use crate::ast::{lower_expr_from_cst, lower_items_from_cst};
use crate::cst::{parse_expr_lexemes_prefix_to_cst, parse_type_lexemes_prefix_to_cst};
use crate::lexer::lexeme::{Lexeme, LexemeKind};
use crate::lexer::tokenizer::{classify_and_normalize_lexeme, Span as TokSpan, Token, TokenKind};

#[derive(Clone)]
pub struct FerroMacroExpansionParser {}

impl FerroMacroExpansionParser {
    pub fn new() -> Self {
        Self {}
    }
}

impl MacroExpansionParser for FerroMacroExpansionParser {
    fn parse_items(&self, tokens: &[MacroTokenTree]) -> Result<Vec<fp_core::ast::Item>> {
        let file_id = macro_tokens_file_id(tokens);
        let tokens = macro_token_trees_to_tokens(tokens);
        let cst = crate::cst::items::parse_items_tokens_to_cst_with_file(&tokens, file_id)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        lower_items_from_cst(&cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
    }

    fn parse_expr(&self, tokens: &[MacroTokenTree]) -> Result<Expr> {
        let file_id = macro_tokens_file_id(tokens);
        let lexemes = macro_token_trees_to_lexemes(tokens);
        let (cst, consumed) = parse_expr_lexemes_prefix_to_cst(&lexemes, file_id)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        if lexemes[consumed..].iter().any(|lex| !lex.is_trivia()) {
            return Err(fp_core::error::Error::from(
                "macro expression tokens contain trailing input",
            ));
        }
        lower_expr_from_cst(&cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
    }

    fn parse_type(&self, tokens: &[MacroTokenTree]) -> Result<Ty> {
        let file_id = macro_tokens_file_id(tokens);
        let lexemes = macro_token_trees_to_lexemes(tokens);
        let (cst, consumed) = parse_type_lexemes_prefix_to_cst(&lexemes, file_id, &[])
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        if lexemes[consumed..]
            .iter()
            .any(|lex| lex.kind == LexemeKind::Token)
        {
            return Err(fp_core::error::Error::from(
                "macro type expansion has trailing input",
            ));
        }
        crate::ast::expr::lower_type_from_cst(&cst)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))
    }
}

fn macro_token_trees_to_tokens(tokens: &[MacroTokenTree]) -> Vec<Token> {
    let mut out = Vec::new();
    append_macro_tokens(tokens, &mut out);
    out
}

fn append_macro_tokens(tokens: &[MacroTokenTree], out: &mut Vec<Token>) {
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => {
                let (kind, lexeme) = classify_and_normalize_lexeme(&tok.text)
                    .unwrap_or((TokenKind::Symbol, tok.text.clone()));
                out.push(Token {
                    kind,
                    lexeme,
                    span: token_span_from_span(tok.span),
                });
            }
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                    fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                    fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                };
                let (open_span, close_span) = token_spans_for_group(group.span);
                out.push(Token {
                    kind: TokenKind::Symbol,
                    lexeme: open.to_string(),
                    span: open_span,
                });
                append_macro_tokens(&group.tokens, out);
                out.push(Token {
                    kind: TokenKind::Symbol,
                    lexeme: close.to_string(),
                    span: close_span,
                });
            }
        }
    }
}

fn macro_token_trees_to_lexemes(tokens: &[MacroTokenTree]) -> Vec<Lexeme> {
    let mut out = Vec::new();
    append_macro_lexemes(tokens, &mut out);
    out
}

fn append_macro_lexemes(tokens: &[MacroTokenTree], out: &mut Vec<Lexeme>) {
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => {
                out.push(Lexeme::token(
                    tok.text.clone(),
                    token_span_from_span(tok.span),
                ));
            }
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                    fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                    fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                };
                let (open_span, close_span) = token_spans_for_group(group.span);
                out.push(Lexeme::token(open.to_string(), open_span));
                append_macro_lexemes(&group.tokens, out);
                out.push(Lexeme::token(close.to_string(), close_span));
            }
        }
    }
}

fn token_span_from_span(span: fp_core::span::Span) -> TokSpan {
    TokSpan {
        start: span.lo as usize,
        end: span.hi as usize,
    }
}

fn token_spans_for_group(span: fp_core::span::Span) -> (TokSpan, TokSpan) {
    let open_start = span.lo;
    let open_end = if span.hi > span.lo {
        span.lo.saturating_add(1)
    } else {
        span.lo
    };
    let close_start = span.hi.saturating_sub(1);
    let close_end = span.hi;
    (
        TokSpan {
            start: open_start as usize,
            end: open_end as usize,
        },
        TokSpan {
            start: close_start as usize,
            end: close_end as usize,
        },
    )
}

fn macro_tokens_file_id(tokens: &[MacroTokenTree]) -> u64 {
    for tree in tokens {
        if let Some(file) = token_tree_file(tree) {
            return file;
        }
    }
    0
}

fn token_tree_file(tree: &MacroTokenTree) -> Option<u64> {
    match tree {
        MacroTokenTree::Token(tok) => Some(tok.span.file),
        MacroTokenTree::Group(group) => {
            if group.span.file != 0 {
                return Some(group.span.file);
            }
            for inner in &group.tokens {
                if let Some(file) = token_tree_file(inner) {
                    return Some(file);
                }
            }
            None
        }
    }
}
