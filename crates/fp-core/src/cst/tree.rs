use thiserror::Error;

use crate::span::Span;

/// Result type produced by CST operations.
pub type CstResult<T> = Result<T, CstError>;

/// Error raised while building or transforming the concrete syntax tree.
#[derive(Debug, Error)]
pub enum CstError {
    #[error("unexpected closing brace")]
    UnexpectedClosingBrace,
    #[error("unexpected eof while parsing {context}")]
    UnexpectedEof { context: &'static str },
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("{0}")]
    Message(String),
}

/// Syntax tree kind emitted by the FerroPhase parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstKind {
    Root,
    Block,
    Quote,
    ConstBlock,
    Splice,
    Token,
}

/// A lightweight concrete syntax tree used as an intermediate representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CstNode {
    pub kind: CstKind,
    pub text: Option<String>,
    pub children: Vec<CstNode>,
    pub span: Option<Span>,
}

impl CstNode {
    pub fn root(children: Vec<CstNode>) -> Self {
        Self {
            kind: CstKind::Root,
            text: None,
            children,
            span: None,
        }
    }

    pub fn block(children: Vec<CstNode>) -> Self {
        Self {
            kind: CstKind::Block,
            text: None,
            children,
            span: None,
        }
    }

    pub fn quote(children: Vec<CstNode>) -> Self {
        Self {
            kind: CstKind::Quote,
            text: None,
            children,
            span: None,
        }
    }

    pub fn const_block(children: Vec<CstNode>) -> Self {
        Self {
            kind: CstKind::ConstBlock,
            text: None,
            children,
            span: None,
        }
    }

    pub fn splice(children: Vec<CstNode>) -> Self {
        Self {
            kind: CstKind::Splice,
            text: None,
            children,
            span: None,
        }
    }

    pub fn token(text: impl Into<String>) -> Self {
        Self {
            kind: CstKind::Token,
            text: Some(text.into()),
            children: Vec::new(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}
