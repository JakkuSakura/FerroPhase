use fp_core::cst::CstError;
use fp_core::span::Span;
use winnow::error::{ContextError, ErrMode};
use winnow::token::literal;
use winnow::{ModalResult, Parser};

use crate::lexer::winnow::is_ident_continue;

/// Helper to parse an exact keyword without allowing identifier continuation.
pub(super) fn keyword<'a>(kw: &'static str) -> impl Parser<&'a str, (), ContextError> {
    move |input: &mut &str| {
        literal(kw).parse_next(input)?;
        if input.chars().next().map_or(false, is_ident_continue) {
            Err(ErrMode::Backtrack(ContextError::new()))
        } else {
            Ok(())
        }
    }
}

/// Helper to parse a fixed symbol.
pub(super) fn symbol<'a>(sym: &'static str) -> impl Parser<&'a str, (), ContextError> {
    move |input: &mut &str| {
        literal(sym).parse_next(input)?;
        Ok(())
    }
}

pub(super) fn map_err(err: ContextError) -> CstError {
    CstError::Message(err.to_string())
}

pub(super) fn map_err_mode(err: ErrMode<ContextError>) -> CstError {
    match err {
        ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => map_err(ctx),
        ErrMode::Incomplete(_) => CstError::Message("incomplete input".to_string()),
    }
}

pub(super) fn try_parse<T, F>(input: &mut &str, mut f: F) -> Result<Option<T>, ErrMode<ContextError>>
where
    F: FnMut(&mut &str) -> ModalResult<T>,
{
    let mut clone = *input;
    match f(&mut clone) {
        Ok(val) => {
            *input = clone;
            Ok(Some(val))
        }
        Err(ErrMode::Backtrack(_)) => Ok(None),
        Err(other) => Err(other),
    }
}

pub(super) fn offset(input: &str, origin_len: usize) -> u32 {
    (origin_len - input.len()) as u32
}

pub(super) fn span(lo: u32, hi: u32) -> Span {
    Span::new(0, lo, hi)
}
