use super::tokenizer::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexemeKind {
    Token,
    TriviaWhitespace,
    TriviaLineComment,
    TriviaBlockComment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexeme {
    pub kind: LexemeKind,
    /// Raw source slice for this lexeme (no normalization).
    pub text: String,
    /// Byte offsets within the original source.
    pub span: Span,
}

impl Lexeme {
    pub fn token(text: String, span: Span) -> Self {
        Self {
            kind: LexemeKind::Token,
            text,
            span,
        }
    }

    pub fn trivia_whitespace(text: String, span: Span) -> Self {
        Self {
            kind: LexemeKind::TriviaWhitespace,
            text,
            span,
        }
    }

    pub fn trivia_line_comment(text: String, span: Span) -> Self {
        Self {
            kind: LexemeKind::TriviaLineComment,
            text,
            span,
        }
    }

    pub fn trivia_block_comment(text: String, span: Span) -> Self {
        Self {
            kind: LexemeKind::TriviaBlockComment,
            text,
            span,
        }
    }

    pub fn is_trivia(&self) -> bool {
        matches!(
            self.kind,
            LexemeKind::TriviaWhitespace
                | LexemeKind::TriviaLineComment
                | LexemeKind::TriviaBlockComment
        )
    }
}
