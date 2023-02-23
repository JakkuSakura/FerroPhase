#![feature(associated_type_defaults)]

pub use shell_macro::shell;
use std::marker::PhantomData;
#[macro_export]
macro_rules! pipe {
    (inner $proc: tt) => {
        $proc
    };
    (inner $proc1: tt | $proc2: tt) => {
        $crate::Pipe::new($proc1, $proc2)
    };
    (inner $proc1: tt | $proc2: tt $(| $proc: tt)+) => {{
        let p = pipe!(inner $proc1 | $proc2);
        pipe!(inner p $(| $proc)+)
    }};

    ($($proc: tt) | +) => {{
        pipe!(inner $(($proc)) |+)
    }};
}
#[derive(Debug, Clone)]
pub enum Stderr {
    Abort,
}

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
}
