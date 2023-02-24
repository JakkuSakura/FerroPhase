#![feature(associated_type_defaults)]

pub use shell_macro::pipe;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub enum Stderr {
    Abort,
}

impl Display for Stderr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for Stderr {}

pub trait Actor<Stdin> {
    type Stdout;
    fn process(&self, item: Stdin) -> Result<Self::Stdout, Stderr>;
}

impl<'a, Stdin, T: Actor<Stdin>> Actor<Stdin> for &'a T {
    type Stdout = T::Stdout;

    fn process(&self, item: Stdin) -> Result<Self::Stdout, Stderr> {
        (**self).process(item)
    }
}

pub trait Source: Actor<()> {}

impl<T: Actor<()>> Source for T {}
#[must_use]
pub struct Pipe<Stdin, L: Actor<Stdin>, R: Actor<L::Stdout>> {
    l: L,
    r: R,
    stdin: PhantomData<Stdin>,
}

impl<Stdin, L: Actor<Stdin>, R: Actor<L::Stdout>> Pipe<Stdin, L, R> {
    pub fn new(l: L, r: R) -> Self {
        Self {
            l,
            r,
            stdin: Default::default(),
        }
    }
}

impl<L: Actor<Stdin>, R: Actor<L::Stdout>, Stdin> Actor<Stdin> for Pipe<Stdin, L, R> {
    type Stdout = R::Stdout;
    fn process(&self, item: Stdin) -> Result<Self::Stdout, Stderr> {
        let out = self.l.process(item)?;
        self.r.process(out)
    }
}
pub struct ActorFn<I, O, F: Fn(I) -> Result<O, Stderr>> {
    f: F,
    _p: PhantomData<(I, O)>,
}
impl<I, O, F: Fn(I) -> Result<O, Stderr>> ActorFn<I, O, F> {
    pub fn new(f: F) -> Self {
        Self {
            f,
            _p: Default::default(),
        }
    }
}
impl<I, O, F: Fn(I) -> Result<O, Stderr>> Actor<I> for ActorFn<I, O, F> {
    type Stdout = O;

    fn process(&self, item: I) -> Result<Self::Stdout, Stderr> {
        (self.f)(item)
    }
}
pub mod starter {
    use crate::Source;

    pub trait TryStarter {
        fn start(self);
    }

    impl<T: Source> TryStarter for T {
        fn start(self) {
            let _ = self.process(());
        }
    }
    pub struct TryStarter2 {}

    impl TryStarter2 {
        pub fn start<A: Source>(&self, a: A) {
            let _ = a.process(());
        }
    }
}
