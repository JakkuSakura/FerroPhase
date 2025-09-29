use std::fmt;

/// Symbolic identifier for intrinsic calls recognised by the front-end.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum IntrinsicCallKind {
    /// `std::io::println` variants (newline appended).
    Println,
    /// `std::io::print` variants (no trailing newline).
    Print,
    /// Length query for collections/strings.
    Len,
}

/// Wrapper that carries the intrinsic kind plus stage-specific payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct IntrinsicCall<T> {
    pub kind: IntrinsicCallKind,
    pub payload: T,
}

impl<T> IntrinsicCall<T> {
    pub fn new(kind: IntrinsicCallKind, payload: T) -> Self {
        Self { kind, payload }
    }

    pub fn map_payload<U, F>(self, f: F) -> IntrinsicCall<U>
    where
        F: FnOnce(T) -> U,
    {
        IntrinsicCall {
            kind: self.kind,
            payload: f(self.payload),
        }
    }

    pub fn kind(&self) -> IntrinsicCallKind {
        self.kind
    }
}

/// Stage-generic payload representation for intrinsic calls.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum IntrinsicCallPayload<Expr, Format> {
    /// Call captured as a format template (used by print-style helpers).
    Format { template: Format },
    /// Plain positional arguments.
    Args { args: Vec<Expr> },
}

impl<E, F> IntrinsicCallPayload<E, F> {
    pub fn map_exprs<E2, FE>(self, mut f: FE) -> IntrinsicCallPayload<E2, F>
    where
        FE: FnMut(E) -> E2,
        F: Clone,
    {
        match self {
            IntrinsicCallPayload::Format { template } => IntrinsicCallPayload::Format { template },
            IntrinsicCallPayload::Args { args } => IntrinsicCallPayload::Args {
                args: args.into_iter().map(|value| f(value)).collect(),
            },
        }
    }

    pub fn map_format<F2, FF>(self, f: FF) -> IntrinsicCallPayload<E, F2>
    where
        FF: FnOnce(F) -> F2,
    {
        match self {
            IntrinsicCallPayload::Format { template } => IntrinsicCallPayload::Format {
                template: f(template),
            },
            IntrinsicCallPayload::Args { args } => IntrinsicCallPayload::Args { args },
        }
    }
}

impl<E, F> IntrinsicCallPayload<E, F>
where
    F: fmt::Debug,
    E: fmt::Debug,
{
    pub fn expect_args(&self, expected: usize, kind: IntrinsicCallKind) {
        if let IntrinsicCallPayload::Args { args } = self {
            debug_assert_eq!(
                args.len(),
                expected,
                "intrinsic call {:?} expected {} args, got {:?}",
                kind,
                expected,
                args
            );
        }
    }
}
