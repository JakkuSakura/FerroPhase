pub mod combinator;
pub mod error;
pub mod token;

pub use combinator::{alt, cut_err, opt, repeat};
pub use error::{ContextError, ErrMode, ErrorKind, FromExternalError};
pub use token::{literal, take_till, take_until, take_while};

pub type ModalResult<T> = Result<T, ErrMode<ContextError>>;

pub trait Parser<I, O, E> {
    fn parse_next(&mut self, input: &mut I) -> Result<O, ErrMode<E>>;

    fn map<F, O2>(self, f: F) -> Map<Self, F, I, O, E, O2>
    where
        Self: Sized,
        F: Fn(O) -> O2,
    {
        Map {
            parser: self,
            map: f,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<I, O, E, F> Parser<I, O, E> for F
where
    F: FnMut(&mut I) -> Result<O, ErrMode<E>>,
{
    fn parse_next(&mut self, input: &mut I) -> Result<O, ErrMode<E>> {
        (self)(input)
    }
}

impl<I, O1, O2, E, P1, P2> Parser<I, (O1, O2), E> for (P1, P2)
where
    I: Clone,
    P1: Parser<I, O1, E>,
    P2: Parser<I, O2, E>,
{
    fn parse_next(&mut self, input: &mut I) -> Result<(O1, O2), ErrMode<E>> {
        let checkpoint = input.clone();
        let (p1, p2) = self;
        let first = match p1.parse_next(input) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        match p2.parse_next(input) {
            Ok(second) => Ok((first, second)),
            Err(ErrMode::Backtrack(err)) => {
                *input = checkpoint;
                Err(ErrMode::Backtrack(err))
            }
            Err(err) => Err(err),
        }
    }
}

pub struct Map<P, F, I, O, E, O2> {
    parser: P,
    map: F,
    _marker: std::marker::PhantomData<(I, O, E, O2)>,
}

impl<I, O, E, O2, P, F> Parser<I, O2, E> for Map<P, F, I, O, E, O2>
where
    P: Parser<I, O, E>,
    F: Fn(O) -> O2,
{
    fn parse_next(&mut self, input: &mut I) -> Result<O2, ErrMode<E>> {
        let output = self.parser.parse_next(input)?;
        Ok((self.map)(output))
    }
}
