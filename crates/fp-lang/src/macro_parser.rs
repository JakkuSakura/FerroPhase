use fp_core::ast::{Expr, MacroExpansionParser, MacroTokenTree, Ty};
use fp_core::error::Result;
use fp_core::span::FileId;

use crate::ast::lower_common::{
    lex_span_from_span, lex_spans_for_group, macro_token_trees_to_lexemes,
    macro_tokens_file_id,
};
use crate::ast::{lower_expr_from_cst, lower_items_from_cst};
use crate::cst::{parse_expr_lexemes_prefix_to_cst, parse_type_lexemes_prefix_to_cst};
use crate::lexer::lexeme::{Lexeme, LexemeKind};
use crate::lexer::tokenizer::{classify_and_normalize_lexeme, Token, TokenKind};

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
        let cst = parse_macro_prefix_cst(
            &lexemes,
            file_id,
            |lexemes, file_id| parse_expr_lexemes_prefix_to_cst(lexemes, file_id),
            |lex| !lex.is_trivia(),
            "macro expression tokens contain trailing input",
        )?;
        lower_expr_from_cst(&cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
    }

    fn parse_type(&self, tokens: &[MacroTokenTree]) -> Result<Ty> {
        let file_id = macro_tokens_file_id(tokens);
        let lexemes = macro_token_trees_to_lexemes(tokens);
        let cst = parse_macro_prefix_cst(
            &lexemes,
            file_id,
            |lexemes, file_id| parse_type_lexemes_prefix_to_cst(lexemes, file_id, &[]),
            |lex| lex.kind == LexemeKind::Token,
            "macro type expansion has trailing input",
        )?;
        crate::ast::expr::lower_type_from_cst(&cst)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))
    }
}

fn parse_macro_prefix_cst(
    lexemes: &[Lexeme],
    file_id: FileId,
    parse: impl FnOnce(
        &[Lexeme],
        FileId,
    ) -> std::result::Result<(crate::syntax::SyntaxNode, usize), crate::cst::ExprCstParseError>,
    has_trailing: impl Fn(&Lexeme) -> bool,
    trailing_message: &'static str,
) -> Result<crate::syntax::SyntaxNode> {
    let (cst, consumed) =
        parse(lexemes, file_id).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    if lexemes[consumed..].iter().any(has_trailing) {
        return Err(fp_core::error::Error::from(trailing_message));
    }
    Ok(cst)
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
                    span: lex_span_from_span(tok.span),
                });
            }
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                    fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                    fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                };
                let (open_span, close_span) = lex_spans_for_group(group.span);
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
