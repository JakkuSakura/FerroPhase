use crate::winnow::error::ErrMode;
use crate::winnow::Parser;

pub fn opt<I, O, P>(
    mut parser: P,
) -> impl Parser<I, Option<O>, crate::winnow::error::ContextError>
where
    P: Parser<I, O, crate::winnow::error::ContextError>,
{
    move |input: &mut I| match parser.parse_next(input) {
        Ok(value) => Ok(Some(value)),
        Err(ErrMode::Backtrack(_)) => Ok(None),
        Err(err) => Err(err),
    }
}

pub fn cut_err<I, O, P>(
    mut parser: P,
) -> impl Parser<I, O, crate::winnow::error::ContextError>
where
    P: Parser<I, O, crate::winnow::error::ContextError>,
{
    move |input: &mut I| match parser.parse_next(input) {
        Err(ErrMode::Backtrack(err)) => Err(ErrMode::Cut(err)),
        other => other,
    }
}

pub fn repeat<I, O, P, R>(
    range: R,
    mut parser: P,
) -> impl Parser<I, Vec<O>, crate::winnow::error::ContextError>
where
    P: Parser<I, O, crate::winnow::error::ContextError>,
    R: std::ops::RangeBounds<usize> + Clone,
{
    move |input: &mut I| {
        let min = match range.start_bound() {
            std::ops::Bound::Included(v) => *v,
            std::ops::Bound::Excluded(v) => v.saturating_add(1),
            std::ops::Bound::Unbounded => 0,
        };
        let max = match range.end_bound() {
            std::ops::Bound::Included(v) => Some(*v),
            std::ops::Bound::Excluded(v) => Some(v.saturating_sub(1)),
            std::ops::Bound::Unbounded => None,
        };
        let mut out = Vec::new();
        loop {
            if let Some(max) = max {
                if out.len() >= max {
                    break;
                }
            }
            match parser.parse_next(input) {
                Ok(value) => out.push(value),
                Err(ErrMode::Backtrack(_)) => break,
                Err(err) => return Err(err),
            }
        }
        if out.len() < min {
            return Err(ErrMode::Backtrack(crate::winnow::error::ContextError::new()));
        }
        Ok(out)
    }
}

pub fn alt<I, O, P>(
    mut parsers: P,
) -> impl Parser<I, O, crate::winnow::error::ContextError>
where
    P: Alt<I, O>,
{
    move |input: &mut I| parsers.parse_alt(input)
}

pub trait Alt<I, O> {
    fn parse_alt(
        &mut self,
        input: &mut I,
    ) -> Result<O, ErrMode<crate::winnow::error::ContextError>>;
}

macro_rules! impl_alt_tuple {
    ($($name:ident),+ $(,)?) => {
        impl<I, O, $($name),+> Alt<I, O> for ($($name,)+)
        where
            $($name: Parser<I, O, crate::winnow::error::ContextError>,)+
        {
            fn parse_alt(
                &mut self,
                input: &mut I,
            ) -> Result<O, ErrMode<crate::winnow::error::ContextError>> {
                let ($($name,)+) = self;
                $(
                    match $name.parse_next(input) {
                        Ok(value) => return Ok(value),
                        Err(ErrMode::Backtrack(_)) => {}
                        Err(err) => return Err(err),
                    }
                )+
                Err(ErrMode::Backtrack(crate::winnow::error::ContextError::new()))
            }
        }
    };
}

impl_alt_tuple!(P1, P2);
impl_alt_tuple!(P1, P2, P3);
impl_alt_tuple!(P1, P2, P3, P4);
impl_alt_tuple!(P1, P2, P3, P4, P5);
impl_alt_tuple!(P1, P2, P3, P4, P5, P6);
impl_alt_tuple!(P1, P2, P3, P4, P5, P6, P7);
impl_alt_tuple!(P1, P2, P3, P4, P5, P6, P7, P8);
impl_alt_tuple!(P1, P2, P3, P4, P5, P6, P7, P8, P9);
impl_alt_tuple!(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
