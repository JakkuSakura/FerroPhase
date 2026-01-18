use std::sync::Arc;

use fp_core::ast::{Expr, MacroExpansionParser, MacroTokenTree, Ty};
use fp_core::error::Result;
use crate::ast::FerroPhaseParser;
use crate::lexer::tokenizer::lex_lexemes;

#[derive(Clone)]
pub struct FerroMacroExpansionParser {
    parser: Arc<FerroPhaseParser>,
}

impl FerroMacroExpansionParser {
    pub fn new(parser: Arc<FerroPhaseParser>) -> Self {
        Self { parser }
    }

    fn tokens_to_string(tokens: &[MacroTokenTree]) -> String {
        fn is_ident_like(text: &str) -> bool {
            text.chars()
                .next()
                .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        }
        fn needs_space(prev: &str, next: &str) -> bool {
            is_ident_like(prev) && is_ident_like(next)
        }

        let mut out = String::new();
        let mut prev_text: Option<String> = None;
        for tok in flatten_token_trees(tokens) {
            if let Some(prev) = prev_text.as_deref() {
                if needs_space(prev, tok.as_str()) {
                    out.push(' ');
                }
            }
            out.push_str(&tok);
            prev_text = Some(tok);
        }
        out
    }

    fn tokens_file_id(tokens: &[MacroTokenTree]) -> u64 {
        for tree in tokens {
            if let Some(file) = token_tree_file(tree) {
                return file;
            }
        }
        0
    }
}

impl MacroExpansionParser for FerroMacroExpansionParser {
    fn parse_items(&self, tokens: &[MacroTokenTree]) -> Result<Vec<fp_core::ast::Item>> {
        let source = Self::tokens_to_string(tokens);
        let file_id = Self::tokens_file_id(tokens);
        let items = self.parser.parse_items_ast_with_file(&source, file_id)?;
        Ok(items)
    }

    fn parse_expr(&self, tokens: &[MacroTokenTree]) -> Result<Expr> {
        let source = Self::tokens_to_string(tokens);
        let file_id = Self::tokens_file_id(tokens);
        let expr = self.parser.parse_expr_ast_with_file(&source, file_id)?;
        Ok(expr)
    }

    fn parse_type(&self, tokens: &[MacroTokenTree]) -> Result<Ty> {
        let source = Self::tokens_to_string(tokens);
        let file_id = Self::tokens_file_id(tokens);
        let lexemes = lex_lexemes(&source)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        let (cst, consumed) =
            crate::cst::parse_type_lexemes_prefix_to_cst(&lexemes, file_id, &[])
                .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        if lexemes[consumed..]
            .iter()
            .any(|lex| lex.kind == crate::lexer::lexeme::LexemeKind::Token)
        {
            return Err(fp_core::error::Error::from(
                "macro type expansion has trailing input",
            ));
        }
        crate::ast::expr::lower_type_from_cst(&cst)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))
    }
}

fn flatten_token_trees(tokens: &[MacroTokenTree]) -> Vec<String> {
    let mut out = Vec::new();
    for tree in tokens {
        match tree {
            MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                    fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                    fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                };
                out.push(open.to_string());
                out.extend(flatten_token_trees(&group.tokens));
                out.push(close.to_string());
            }
        }
    }
    out
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
